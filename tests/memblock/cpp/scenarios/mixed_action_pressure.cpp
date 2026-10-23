#include "mixed/scenario.hpp"

#include <cstdlib>

namespace memblock::scenarios::mixed {

bool MixedScenario::execute_set_pressure(ActionWindow &window)
{
    // Harness-only negative-path control. Unset in normal campaigns; when set,
    // exactly one ownership attempt is forced through rejected construction so
    // current-revision cleanup/accounting has runtime evidence.
    static bool force_reject_once =
        std::getenv("MEMBLOCK_FORCE_DIRTY_OWNERSHIP_REJECT_ONCE") != nullptr;

    auto &set_pressure_depth_index = window.set_pressure_depth_index;
    auto &set_pressure_width = window.set_pressure_width;
    auto &set_pressure_set = window.set_pressure_set;
    auto &set_pressure_dirty = window.set_pressure_dirty;
    auto &set_pressure_refill_overlap = window.set_pressure_refill_overlap;
    auto &set_pressure_release_backpressure = window.set_pressure_release_backpressure;
    auto &set_pressure_window_class = window.set_pressure_window_class;
    auto &translation = window.translation;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;

    phase = "random-set-pressure-drain";
    if (!environment.run_until_all_complete(
            constrained_completion_timeout) ||
        !environment.run_until_queues_retired(8192) ||
        !environment.run_until_sbuffer_empty(
            constrained_completion_timeout) ||
        !environment.run_until_dcache_release_idle(
            constrained_completion_timeout) ||
        !environment.run_until_dcache_idle(
            constrained_completion_timeout)) {
        return false;
    }
    // The L1 stride prefetcher is traffic-trained and may issue a
    // background A request into a target set while the held
    // responses are being armed. That eviction is legal but it is
    // not part of the pressure-owned stimulus, so quiesce the
    // prefetcher for the whole action and restore it afterwards.
    const bool restore_stride_prefetch =
        constraints.stride_stream_per_mille != 0;
    if (restore_stride_prefetch &&
        !environment.configure_stride_prefetch(false)) {
        return false;
    }
    if (restore_stride_prefetch &&
        (!environment.run_cycles(8) ||
            !environment.run_until_dcache_idle(
                constrained_completion_timeout))) {
        return false;
    }

    // Retry only construction; keep the selected profile tuple fixed.
    // Fresh address/identity sequences and actual traffic never rewind.
    constexpr unsigned ownership_attempt_limit = 4;
    bool pressure_action_accepted = false;
    for (unsigned ownership_attempt = 0;
         ownership_attempt < ownership_attempt_limit &&
         !pressure_action_accepted;
         ++ownership_attempt) {
        // The generated 256-set, four-way DCache maps this 8-KiB
        // stride to two sets. A window has eight total ways across
        // that pair; the low seven index bits identify disjoint pairs.
        // These windows do not claim single-set associativity coverage.
        constexpr unsigned pressure_index_count = 128;
        constexpr unsigned pressure_capacity_lines = 8;
        constexpr unsigned line_bytes = 64;
        constexpr unsigned indices_per_quartile = pressure_index_count / 4;
        constexpr std::uint64_t pressure_stride =
            pressure_index_count * line_bytes;
        constexpr std::uint64_t group_stride = 0x20000;
        constexpr std::uint64_t groups =
            set_pressure_span / group_stride;
        constexpr std::uint64_t sequences_per_quartile =
            groups * indices_per_quartile;
        struct SetPressureWindow {
            std::uint64_t sequence;
            std::uint64_t first_line;
            unsigned set_index;
        };
        const unsigned window_count =
            set_pressure_window_class + 1U;
        const unsigned depth = set_pressure_depth_index + 9U;
        const std::array<unsigned,
            RandomConstraints::set_pressure_window_class_count>
            window_quartiles{{
                set_pressure_set,
                (set_pressure_set + 2U) % 4U,
                (set_pressure_set + 1U) % 4U,
                (set_pressure_set + 3U) % 4U,
                set_pressure_set,
                (set_pressure_set + 2U) % 4U,
                (set_pressure_set + 1U) % 4U,
                (set_pressure_set + 3U) % 4U,
            }};
        std::vector<SetPressureWindow> pressure_windows;
        pressure_windows.reserve(window_count);
        for (unsigned window = 0; window < window_count; ++window) {
            const unsigned quartile = window_quartiles[window];
            std::optional<SetPressureWindow> fresh_window;
            while (set_pressure_sequences.at(quartile) <
                sequences_per_quartile) {
                const std::uint64_t sequence =
                    set_pressure_sequences.at(quartile)++;
                const std::uint64_t group =
                    sequence / indices_per_quartile;
                const unsigned set_index =
                    quartile * indices_per_quartile +
                    static_cast<unsigned>(
                        sequence % indices_per_quartile);
                const std::uint64_t first_line =
                    set_pressure_base + group * group_stride +
                    set_index * line_bytes;
                bool fresh = true;
                for (unsigned index = 0; index < depth; ++index) {
                    const std::uint64_t line =
                        first_line + index * pressure_stride;
                    if (environment.dcache_requests_covering_since(
                            line, 0) != 0) {
                        fresh = false;
                        break;
                    }
                }
                if (fresh) {
                    fresh_window = SetPressureWindow{
                        sequence, first_line, set_index};
                    break;
                }
            }
            if (!fresh_window) {
                phase =
                    "random-set-pressure-target-address-exhausted";
                return false;
            }
            pressure_windows.push_back(*fresh_window);
        }
        const std::uint64_t sequence =
            pressure_windows.front().sequence;
        const unsigned set_index =
            pressure_windows.front().set_index;
        std::vector<std::uint64_t> target_lines;
        target_lines.reserve(window_count * depth);
        for (const auto &window : pressure_windows) {
            for (unsigned index = 0; index < depth; ++index) {
                target_lines.push_back(
                    window.first_line + index * pressure_stride);
            }
        }
        const unsigned width_bytes = 1U << set_pressure_width;
        const unsigned bank = static_cast<unsigned>(random() % 8U);
        const unsigned offset = bank * 8U +
            static_cast<unsigned>(random() % (8U / width_bytes)) *
                width_bytes;

        environment.clear_release_line_expectations();
        environment.begin_dcache_release_observation();
        std::vector<memblock::LoadTransaction> overlap_loads;
        std::vector<std::uint64_t> overlap_lines;
        std::uint64_t overlap_requests_before = 0;
        std::uint64_t overlap_writebacks_before = 0;
        std::uint64_t overlap_lq_before = 0;
        std::uint64_t releases_while_overlap_pending = 0;
        constexpr unsigned forced_release_stall_cycles = 16;
        std::uint64_t target_stalled_releases_before = 0;
        std::uint64_t target_release_stall_cycles_before = 0;
        std::uint64_t c_stability_checks_before = 0;
        bool release_ready_window_started = false;
        std::array<std::uint64_t, 5> backpressure_manager_delta{};
        const unsigned set_pressure_completion_timeout =
            std::max(constrained_completion_timeout, 8192U);
        const auto window_target_releases = [&] (
                                                const SetPressureWindow &window, bool data_only) {
            std::uint64_t count = 0;
            for (unsigned index = 0; index < depth; ++index) {
                const std::uint64_t line =
                    window.first_line + index * pressure_stride;
                count += data_only
                    ? environment
                          .observed_dcache_release_data_covering(line)
                    : environment.observed_dcache_releases_covering(line);
            }
            return count;
        };
        const auto target_releases = [&](bool data_only) {
            std::uint64_t count = 0;
            for (const auto &window : pressure_windows) {
                count += window_target_releases(window, data_only);
            }
            return count;
        };
        const auto target_stalled_releases = [&](bool data_only) {
            std::uint64_t count = 0;
            for (const std::uint64_t line : target_lines) {
                count += environment
                             .observed_dcache_stalled_releases_covering(
                                 line, data_only);
            }
            return count;
        };
        const auto target_release_stall_cycles = [&](bool data_only) {
            std::uint64_t count = 0;
            for (const std::uint64_t line : target_lines) {
                count += environment
                             .observed_dcache_release_stall_cycles_covering(
                                 line, data_only);
            }
            return count;
        };
        const auto begin_release_backpressure = [&](bool data_only) {
            target_stalled_releases_before =
                target_stalled_releases(data_only);
            target_release_stall_cycles_before =
                target_release_stall_cycles(data_only);
            c_stability_checks_before =
                environment
                    .dcache_release_target_payload_stability_checks();
            release_ready_window_started =
                environment.begin_dcache_release_ready_window(
                    target_lines,
                    set_pressure_release_backpressure
                        ? forced_release_stall_cycles
                        : 0);
            return release_ready_window_started;
        };
        const auto finish_release_backpressure = [&](bool data_only) {
            if (!release_ready_window_started) {
                return false;
            }
            const std::uint64_t stalled_releases =
                target_stalled_releases(data_only) -
                target_stalled_releases_before;
            const std::uint64_t stall_cycles =
                target_release_stall_cycles(data_only) -
                target_release_stall_cycles_before;
            const std::uint64_t stability_checks =
                environment
                    .dcache_release_target_payload_stability_checks() -
                c_stability_checks_before;
            if (!environment.end_dcache_release_ready_window()) {
                return false;
            }
            const bool backpressure_observation_valid =
                memblock::dcache_release_backpressure_observation_valid(
                    set_pressure_release_backpressure,
                    stalled_releases, stall_cycles, stability_checks,
                    forced_release_stall_cycles);
            if (!backpressure_observation_valid) {
                std::ostringstream detail;
                detail << "random-set-pressure-release-backpressure"
                       << ":selected="
                       << set_pressure_release_backpressure
                       << ":stalled=" << stalled_releases
                       << ":cycles=" << stall_cycles
                       << ":stable=" << stability_checks;
                phase = detail.str();
                return false;
            }
            backpressure_manager_delta = {{
                set_pressure_release_backpressure ? 1U : 0U,
                stalled_releases,
                stall_cycles,
                set_pressure_release_backpressure
                    ? forced_release_stall_cycles : 0U,
                1,
            }};
            return true;
        };
        const auto start_refill_overlap = [&](bool load_at_rob_head) {
            if (!set_pressure_refill_overlap) {
                return true;
            }
            overlap_requests_before = environment.tilelink_requests();
            overlap_writebacks_before = environment.writebacks();
            overlap_lq_before = environment.lq_dequeued();
            overlap_loads.clear();
            overlap_lines.clear();
            overlap_loads.reserve(window_count);
            overlap_lines.reserve(window_count);
            std::vector<unsigned> overlap_sets;
            overlap_sets.reserve(window_count);
            for (const auto &window : pressure_windows) {
                const std::uint64_t group_base = window.first_line -
                    window.set_index * line_bytes;
                std::optional<std::uint64_t> fresh_overlap_line;
                std::optional<unsigned> fresh_overlap_set;
                for (unsigned delta = 1; delta < pressure_index_count &&
                     !fresh_overlap_line; ++delta) {
                    const unsigned candidate_set =
                        (window.set_index + delta) % pressure_index_count;
                    const bool aliases_target = std::any_of(
                        pressure_windows.begin(), pressure_windows.end(),
                        [&](const SetPressureWindow &target) {
                            return target.set_index == candidate_set;
                        });
                    if (aliases_target ||
                        std::find(
                            overlap_sets.begin(), overlap_sets.end(),
                            candidate_set) != overlap_sets.end()) {
                        continue;
                    }
                    for (unsigned tag = depth; tag < 16U; ++tag) {
                        const std::uint64_t candidate = group_base +
                            tag * pressure_stride +
                            candidate_set * line_bytes;
                        if (environment.dcache_requests_covering_since(
                                candidate, 0) == 0 &&
                            std::find(
                                overlap_lines.begin(),
                                overlap_lines.end(), candidate) ==
                                overlap_lines.end()) {
                            fresh_overlap_line = candidate;
                            fresh_overlap_set = candidate_set;
                            break;
                        }
                    }
                }
                if (!fresh_overlap_line || !fresh_overlap_set) {
                    phase =
                        "random-set-pressure-overlap-address-exhausted";
                    return false;
                }
                const std::uint64_t overlap_line = *fresh_overlap_line;
                overlap_sets.push_back(*fresh_overlap_set);
                environment.memory().fill_incrementing(
                    overlap_line, line_bytes,
                    static_cast<std::uint8_t>(
                        0xa7U + window.set_index));
                overlap_lines.push_back(overlap_line);
                overlap_loads.push_back(make_load(
                    overlap_line + 8U * (random() % 8U),
                    memblock::LoadOp::ld,
                    random() % memblock::kScalarLoadLanes));
                overlap_loads.back().oracle_address =
                    overlap_loads.back().address;
                environment.expect_load(overlap_loads.back());
                environment.hold_dcache_response_at(overlap_line);
            }
            if (load_at_rob_head &&
                !environment.set_rob_head(
                    overlap_loads.front().rob,
                    overlap_loads.front().rob_flag)) {
                return false;
            }
            for (const auto &load : overlap_loads) {
                if (!environment.enqueue_load(load) ||
                    !environment.issue_load(load, 4096)) {
                    return false;
                }
            }
            const auto all_overlap_responses_held = [&]() {
                for (unsigned window = 0; window < window_count;
                     ++window) {
                    if (environment.dcache_requests_covering_since(
                            overlap_lines[window],
                            overlap_requests_before) != 1 ||
                        !environment.dcache_response_held_at(
                            overlap_lines[window]) ||
                        !environment.scalar_load_pending(
                            overlap_loads[window])) {
                        return false;
                    }
                }
                return environment.dcache_held_response_count() ==
                    window_count;
            };
            for (unsigned cycle = 0;
                 cycle < set_pressure_completion_timeout &&
                 !all_overlap_responses_held();
                 ++cycle) {
                if (!environment.run_cycles(1)) {
                    return false;
                }
            }
            return all_overlap_responses_held();
        };
        const auto prove_release_overlap = [&](bool data_only,
                                               const auto &before) {
            if (!set_pressure_refill_overlap) {
                return true;
            }
            const std::uint64_t required = depth - pressure_capacity_lines;
            const auto overlap_failure = [&](const char *why) {
                std::ostringstream detail;
                detail << phase << ":reason=" << why
                       << ":cycle=" << environment.cycle()
                       << ":depth=" << depth
                       << ":windows=" << window_count
                       << ":required=" << required
                       << ":data_only=" << data_only
                       << ":backpressure=" << set_pressure_release_backpressure
                       << ":held_count=" << environment.dcache_held_response_count();
                for (unsigned w = 0; w < window_count; ++w) {
                    detail << ":w" << w << "=" << pressure_windows[w].set_index
                           << ",0x" << std::hex << pressure_windows[w].first_line
                           << std::dec << "," << before[w] << ","
                           << window_target_releases(pressure_windows[w], data_only)
                           << ",0x" << std::hex << overlap_lines[w]
                           << std::dec << "," << environment.dcache_response_held_at(overlap_lines[w])
                           << "," << environment.scalar_load_pending(overlap_loads[w]);
                }
                phase = detail.str();
                return false;
            };
            const auto all_windows_released = [&]() {
                for (unsigned window = 0; window < window_count;
                     ++window) {
                    if (window_target_releases(
                            pressure_windows[window], data_only) -
                            before[window] < required) {
                        return false;
                    }
                }
                return true;
            };
            for (unsigned cycle = 0;
                 cycle < set_pressure_completion_timeout &&
                 !all_windows_released();
                 ++cycle) {
                for (const auto &load : overlap_loads) {
                    if (!environment.scalar_load_pending(load)) {
                        return overlap_failure("overlap-load-not-pending");
                    }
                }
                if (!environment.run_cycles(1)) {
                    return false;
                }
            }
            releases_while_overlap_pending = 0;
            if (!all_windows_released() ||
                environment.dcache_held_response_count() !=
                    window_count) {
                return overlap_failure("release-minimum-or-held-count");
            }
            for (unsigned window = 0; window < window_count; ++window) {
                releases_while_overlap_pending +=
                    window_target_releases(
                        pressure_windows[window], data_only) -
                    before[window];
                if (!environment.dcache_response_held_at(
                        overlap_lines[window]) ||
                    !environment.scalar_load_pending(
                        overlap_loads[window])) {
                    return overlap_failure("overlap-address-or-identity");
                }
            }
            if (data_only &&
                !environment.set_rob_head(
                    overlap_loads.front().rob,
                    overlap_loads.front().rob_flag)) {
                return false;
            }
            for (const std::uint64_t line : overlap_lines) {
                if (!environment.release_held_dcache_response_at(line)) {
                    return false;
                }
            }
            return true;
        };
        const auto overlap_manager_delta = [&]() {
            if (!set_pressure_refill_overlap) {
                return std::array<std::uint64_t, 5>{};
            }
            std::uint64_t requests = 0;
            for (const std::uint64_t line : overlap_lines) {
                requests += environment.dcache_requests_covering_since(
                    line, overlap_requests_before);
            }
            return std::array<std::uint64_t, 5>{{
                window_count,
                requests,
                releases_while_overlap_pending,
                window_count,
                window_count,
            }};
        };

        if (!set_pressure_dirty) {
            const std::uint64_t requests_before_pressure =
                environment.tilelink_requests();
            const std::uint64_t all_releases_before_pressure =
                environment.tilelink_releases();
            const std::uint64_t releases_before_pressure =
                environment.tilelink_release_data();
            const std::uint64_t verified_before_pressure =
                environment.tilelink_release_data_verified();
            const std::uint64_t writebacks_before_pressure =
                environment.writebacks();
            const std::uint64_t lq_before_pressure =
                environment.lq_dequeued();
            constexpr std::array<memblock::LoadOp, 4>
                unsigned_load_ops{{
                    memblock::LoadOp::lbu, memblock::LoadOp::lhu,
                    memblock::LoadOp::lwu, memblock::LoadOp::ld,
                }};
            std::vector<memblock::LoadTransaction> pressure_loads;
            pressure_loads.reserve(target_lines.size());
            for (unsigned index = 0; index < target_lines.size();
                 ++index) {
                const std::uint64_t line = target_lines[index];
                const unsigned target_set = static_cast<unsigned>(
                    (line / line_bytes) % pressure_index_count);
                environment.memory().fill_incrementing(
                    line, line_bytes,
                    static_cast<std::uint8_t>(
                        0x31U + target_set + index * 17U));
                environment.expect_release_line(
                    line,
                    environment.memory().read_beat(line, line_bytes));
            }
            std::vector<std::uint64_t>
                target_releases_before_overlap;
            target_releases_before_overlap.reserve(window_count);
            const auto issue_pressure_load = [&](unsigned index,
                                                 bool defer_issue = false) {
                const std::uint64_t line = target_lines[index];
                auto load = make_load(
                    line + offset,
                    unsigned_load_ops[set_pressure_width],
                    (index + set_pressure_set) %
                        memblock::kScalarLoadLanes);
                load.oracle_address = load.address;
                bool issued = false;
                if (set_pressure_refill_overlap) {
                    environment.expect_load(load);
                    issued = environment.enqueue_load(load) &&
                        (defer_issue || environment.issue_load(load, 4096));
                } else {
                    issued = issue_load(load, std::nullopt, false);
                }
                if (!issued) {
                    phase = "random-set-pressure-clean-fill";
                    return false;
                }
                if (!defer_issue) {
                    coverage.sample(load);
                }
                pressure_loads.push_back(load);
                return true;
            };
            // Allocate an older prefix without sending target cache
            // traffic. It can retire ahead of the held younger loads.
            // At eight windows: 32 prefix + 8 held flows initially;
            // after prefix dequeue: 8 * (10 - 4 + 1) = 56 flows.
            constexpr unsigned older_targets_per_window = 4;
            static_assert(
                RandomConstraints::set_pressure_window_class_count *
                    (older_targets_per_window + 1U) <=
                memblock::kVirtualLoadQueueEntries -
                    memblock::kLqEnqueueHeadroom);
            static_assert(
                RandomConstraints::set_pressure_window_class_count *
                    (10U - older_targets_per_window + 1U) <=
                memblock::kVirtualLoadQueueEntries -
                    memblock::kLqEnqueueHeadroom);
            if (set_pressure_refill_overlap) {
                phase = "random-set-pressure-clean-prefix-enqueue";
                for (unsigned window = 0; window < window_count;
                     ++window) {
                    for (unsigned tag = 0;
                         tag < older_targets_per_window; ++tag) {
                        if (!issue_pressure_load(
                                window * depth + tag, true)) {
                            return false;
                        }
                    }
                }
                if (!environment.set_rob_head(
                        pressure_loads.front().rob,
                        pressure_loads.front().rob_flag)) {
                    return false;
                }
            }
            for (const auto &window : pressure_windows) {
                target_releases_before_overlap.push_back(
                    window_target_releases(window, false));
            }
            if (set_pressure_refill_overlap) {
                phase = "random-set-pressure-clean-refill-overlap";
                if (!start_refill_overlap(false)) {
                    return false;
                }
            }
            phase = "random-set-pressure-clean-release-backpressure";
            if (!begin_release_backpressure(false)) {
                return false;
            }
            if (set_pressure_refill_overlap) {
                phase = "random-set-pressure-clean-prefix-issue";
                const unsigned prefix_count =
                    static_cast<unsigned>(pressure_loads.size());
                for (const auto &load : pressure_loads) {
                    if (!environment.issue_load(load, 4096)) {
                        return false;
                    }
                    coverage.sample(load);
                }
                phase = "random-set-pressure-clean-prefix-retire";
                if (!environment.run_until_load_writebacks(
                        writebacks_before_pressure + prefix_count,
                        set_pressure_completion_timeout)) {
                    return false;
                }
                for (unsigned cycle = 0;
                     cycle < set_pressure_completion_timeout &&
                     environment.lq_dequeued() <
                         lq_before_pressure + prefix_count;
                     ++cycle) {
                    if (!environment.run_cycles(1)) {
                        return false;
                    }
                }
                if (environment.lq_dequeued() !=
                        lq_before_pressure + prefix_count ||
                    !environment.set_rob_head(
                        overlap_loads.front().rob,
                        overlap_loads.front().rob_flag)) {
                    return false;
                }
                phase = "random-set-pressure-clean-tail-issue";
                for (unsigned window = 0; window < window_count;
                     ++window) {
                    for (unsigned tag = older_targets_per_window;
                         tag < depth; ++tag) {
                        if (!issue_pressure_load(
                                window * depth + tag)) {
                            return false;
                        }
                    }
                }
            } else {
                for (unsigned index = 0; index < target_lines.size();
                     ++index) {
                    if (!issue_pressure_load(index)) {
                        return false;
                    }
                }
            }
            phase = "random-set-pressure-clean-fill-complete";
            if (set_pressure_refill_overlap
                    ? !environment.run_until_load_writebacks(
                          writebacks_before_pressure +
                              target_lines.size(),
                          set_pressure_completion_timeout)
                    : (!environment.run_until_queues_retired(8192) ||
                          !environment.run_until_dcache_idle(
                              constrained_completion_timeout))) {
                return false;
            }

            std::uint64_t initial_target_requests = 0;
            for (const auto &load : pressure_loads) {
                const std::uint64_t target_requests =
                    environment.dcache_requests_covering_since(
                        load.address & ~std::uint64_t{63},
                        requests_before_pressure);
                if (target_requests != 1) {
                    phase = "random-set-pressure-clean-initial-request";
                    return false;
                }
                initial_target_requests += target_requests;
            }
            phase = "random-set-pressure-clean-release-overlap";
            if (!prove_release_overlap(
                    false, target_releases_before_overlap)) {
                return false;
            }
            if (set_pressure_refill_overlap &&
                (!environment.run_until_all_complete(
                     set_pressure_completion_timeout) ||
                    !environment.run_until_queues_retired(8192) ||
                    !environment.run_until_dcache_idle(
                        set_pressure_completion_timeout))) {
                phase = "random-set-pressure-clean-fill-retire";
                return false;
            }
            const std::uint64_t requests_before_revisit =
                environment.tilelink_requests();
            for (unsigned reverse = pressure_loads.size();
                 reverse != 0; --reverse) {
                const unsigned index = reverse - 1U;
                auto revisit = make_load(
                    pressure_loads[index].address,
                    pressure_loads[index].op,
                    (index + set_pressure_set + 1U) %
                        memblock::kScalarLoadLanes);
                revisit.oracle_address = revisit.address;
                if (!issue_load(revisit, std::nullopt, false)) {
                    phase = "random-set-pressure-clean-revisit";
                    return false;
                }
                coverage.sample(revisit);
            }
            phase = "random-set-pressure-clean-revisit-complete";
            if (!environment.run_until_all_complete(
                    set_pressure_completion_timeout) ||
                !environment.run_until_queues_retired(8192) ||
                !environment.run_until_dcache_idle(
                    set_pressure_completion_timeout)) {
                return false;
            }
            phase = "random-set-pressure-clean-release-window";
            if (!finish_release_backpressure(false)) {
                return false;
            }

            std::uint64_t revisit_target_requests = 0;
            std::uint64_t target_releases = 0;
            std::uint64_t target_release_data = 0;
            for (const auto &load : pressure_loads) {
                revisit_target_requests +=
                    environment.dcache_requests_covering_since(
                        load.address & ~std::uint64_t{63},
                        requests_before_revisit);
                target_releases +=
                    environment.observed_dcache_releases_covering(
                        load.address & ~std::uint64_t{63});
                target_release_data +=
                    environment.observed_dcache_release_data_covering(
                        load.address & ~std::uint64_t{63});
            }
            for (const auto &window : pressure_windows) {
                if (window_target_releases(window, false) <
                    depth - pressure_capacity_lines) {
                    phase =
                        "random-set-pressure-clean-window-release";
                    return false;
                }
            }
            const std::array<std::uint64_t, 12> manager_delta{{
                1,
                target_lines.size(),
                initial_target_requests,
                target_lines.size(),
                revisit_target_requests,
                target_releases,
                target_release_data,
                environment.tilelink_releases() -
                    all_releases_before_pressure,
                environment.tilelink_release_data() -
                    releases_before_pressure,
                environment.tilelink_release_data_verified() -
                    verified_before_pressure,
                environment.writebacks() - writebacks_before_pressure,
                environment.lq_dequeued() - lq_before_pressure,
            }};
            const std::uint64_t minimum_target_releases =
                window_count * (depth - pressure_capacity_lines);
            const std::uint64_t overlap_load_count =
                set_pressure_refill_overlap ? window_count : 0U;
            if (manager_delta[2] != target_lines.size() ||
                manager_delta[4] < minimum_target_releases ||
                manager_delta[5] < minimum_target_releases ||
                manager_delta[6] != 0 ||
                manager_delta[7] < manager_delta[5] ||
                manager_delta[9] != manager_delta[8] ||
                manager_delta[10] !=
                    target_lines.size() * 2U + overlap_load_count ||
                manager_delta[11] !=
                    target_lines.size() * 2U + overlap_load_count) {
                std::ostringstream detail;
                detail << "random-set-pressure-clean-manager"
                       << ":depth=" << depth
                       << ":width=" << width_bytes
                       << ":set=" << set_index << ":manager=";
                for (unsigned index = 0; index < manager_delta.size();
                     ++index) {
                    detail << (index == 0 ? "" : ",")
                           << manager_delta[index];
                }
                phase = detail.str();
                return false;
            }
            environment.end_dcache_release_observation();
            environment.clear_release_line_expectations();
            constraint_coverage.sample_set_pressure(
                false, set_pressure_refill_overlap,
                set_pressure_release_backpressure,
                set_pressure_window_class,
                set_pressure_depth_index, set_pressure_width,
                translation.regime, set_pressure_set, {}, {},
                manager_delta, overlap_manager_delta(),
                backpressure_manager_delta);
            ordinary_leaf_addressed = true;
            ++actions;
            coverage.cacheable += target_lines.size() * 2U;
        } else {
            const std::uint64_t requests_before_pressure =
                environment.tilelink_requests();
            const bool qualify_ownership = set_pressure_refill_overlap;
            if (qualify_ownership) {
                ++constraint_coverage.set_pressure_ownership[0]; // attempts
                ++constraint_coverage.set_pressure_ownership[8]; // in flight
            }
            const std::array<std::uint64_t, 3> ownership_errors_before{{
                environment.dcache_error_response_requests(),
                environment.dcache_denied_d_beats(),
                environment.dcache_corrupt_d_beats(),
            }};
            const std::uint64_t all_requests_before_pressure =
                environment.tilelink_requests();
            const std::uint64_t expected_releases_before =
                environment.tilelink_expected_release_data_verified();
            const std::uint64_t releases_before_pressure =
                environment.tilelink_release_data();
            const std::uint64_t verified_before_pressure =
                environment.tilelink_release_data_verified();
            const std::uint64_t writebacks_before_pressure =
                environment.store_writebacks();
            const std::uint64_t sq_before_pressure =
                environment.sq_dequeued();
            std::vector<std::uint64_t> store_lines;
            store_lines.reserve(target_lines.size());
            for (unsigned index = 0; index < pressure_capacity_lines; ++index) {
                for (const auto &window : pressure_windows) {
                    store_lines.push_back(
                        window.first_line + index * pressure_stride);
                }
            }
            for (unsigned index = pressure_capacity_lines; index < depth; ++index) {
                for (const auto &window : pressure_windows) {
                    store_lines.push_back(
                        window.first_line + index * pressure_stride);
                }
            }
            std::vector<memblock::StoreTransaction> pressure_stores;
            std::vector<bool> pressure_store_data_first;
            pressure_stores.reserve(store_lines.size());
            pressure_store_data_first.reserve(store_lines.size());
            std::array<std::uint64_t, 2> issue_orders{};
            for (unsigned index = 0; index < store_lines.size();
                 ++index) {
                const std::uint64_t line = store_lines[index];
                const unsigned target_set = static_cast<unsigned>(
                    (line / line_bytes) % pressure_index_count);
                environment.memory().fill_incrementing(
                    line, line_bytes,
                    static_cast<std::uint8_t>(0x31U + target_set +
                        index * 17U));
                std::vector<unsigned char> expected_line =
                    environment.memory().read_beat(line, line_bytes);
                std::uint64_t data = random();
                for (unsigned byte = 0; byte < width_bytes; ++byte) {
                    const auto stored = static_cast<unsigned char>(
                        expected_line[offset + byte] ^ 0xffU);
                    data &= ~(std::uint64_t{0xff} << (8U * byte));
                    data |= std::uint64_t{stored} << (8U * byte);
                    expected_line[offset + byte] = stored;
                }
                pressure_stores.push_back(make_store(
                    line + offset, data,
                    static_cast<memblock::StoreOp>(set_pressure_width),
                    (index + set_pressure_set) %
                        memblock::kScalarStoreLanes,
                    (index + set_pressure_set + 1U) %
                        memblock::kScalarStoreLanes));
                const auto &store = pressure_stores.back();
                environment.expect_release_line(line, expected_line);
                const bool data_first =
                    ((index + (sequence & 1U)) & 1U) != 0;
                pressure_store_data_first.push_back(data_first);
                ++issue_orders[data_first ? 1U : 0U];
            }
            const auto issue_store_range = [&](std::size_t first,
                                               std::size_t last,
                                               bool already_enqueued = false) {
                for (std::size_t index = first; index < last; ++index) {
                    const auto &store = pressure_stores[index];
                    const bool data_first =
                        pressure_store_data_first[index];
                    if (!already_enqueued) {
                        environment.expect_store(store);
                    }
                    const bool issued =
                        (index != 0 ||
                            environment.set_rob_head(
                                store.rob, store.rob_flag)) &&
                        (already_enqueued || environment.enqueue_store(
                                                 store, memblock::lq_pointer_value(lq_offset))) &&
                        (data_first
                                ? environment.issue_store_data(store, 4096) &&
                                    environment
                                        .issue_store_address_until_tlb_hit(
                                            store,
                                            constrained_completion_timeout)
                                : environment
                                        .issue_store_address_until_tlb_hit(
                                            store,
                                            constrained_completion_timeout) &&
                                    environment.issue_store_data(store, 4096));
                    if (!issued) {
                        return false;
                    }
                    coverage.sample(store, data_first);
                }
                return true;
            };
            const std::size_t baseline_stores =
                window_count * pressure_capacity_lines;
            // Commit each four-way half before the next one so an
            // eight-window action stays below the SQ dispatch watermark.
            const std::size_t baseline_batch_stores = window_count * 4U;
            for (std::size_t first = 0; first < baseline_stores;
                 first += baseline_batch_stores) {
                const std::size_t last = std::min(
                    first + baseline_batch_stores, baseline_stores);
                phase = "random-set-pressure-dirty-baseline-issue";
                if (!issue_store_range(first, last) ||
                    !environment.run_until_store_complete(
                        constrained_completion_timeout)) {
                    return false;
                }
                phase = "random-set-pressure-dirty-baseline-commit";
                for (std::size_t index = first; index < last; ++index) {
                    if (!environment.commit_store(
                            pressure_stores[index], 8192)) {
                        return false;
                    }
                }
                if (!environment.run_until_queues_retired(8192) ||
                    !environment.run_until_sbuffer_empty(
                        set_pressure_completion_timeout)) {
                    return false;
                }
            }
            if (qualify_ownership) {
                phase = "random-set-pressure-dirty-baseline-response-drain";
                if (!environment.run_until_dcache_idle(
                        set_pressure_completion_timeout)) {
                    return false;
                }
            }
            // Overflow stores are older than the overlap loads.
            // Dispatch all of their SQ allocations first; defer address
            // and data issue until every younger D response is held.
            if (set_pressure_refill_overlap) {
                phase = "random-set-pressure-dirty-overflow-enqueue";
                static_assert(
                    RandomConstraints::set_pressure_window_class_count *
                        (10U - 8U) <=
                    memblock::kStoreQueueEntries - memblock::kSqEnqueueHeadroom);
                for (std::size_t index = baseline_stores;
                     index < pressure_stores.size(); ++index) {
                    environment.expect_store(pressure_stores[index]);
                    if (!environment.enqueue_store(
                            pressure_stores[index],
                            memblock::lq_pointer_value(lq_offset))) {
                        return false;
                    }
                }
            }
            std::vector<std::uint64_t>
                target_releases_before_overlap;
            target_releases_before_overlap.reserve(window_count);
            for (const auto &window : pressure_windows) {
                target_releases_before_overlap.push_back(
                    window_target_releases(window, true));
            }
            phase = "random-set-pressure-dirty-refill-overlap";
            if (!start_refill_overlap(false)) {
                return false;
            }
            if (qualify_ownership) {
                // A target ReleaseData that was already in flight when the
                // hold was established must complete before the external
                // ownership proof samples completed Releases. Otherwise an
                // early eviction would be invisible to target_releases().
                phase = "random-set-pressure-dirty-release-idle";
                if (!environment.run_until_dcache_release_idle(
                        set_pressure_completion_timeout)) {
                    return false;
                }
                // No DUT tick between this external proof and arming C
                // backpressure. Responsibility may reside in a writeback
                // queue; array residency is neither observed nor required.
                std::uint64_t bad_acquisitions = 0;
                for (std::size_t index = 0; index < baseline_stores; ++index) {
                    const auto line = pressure_stores[index].address &
                        ~std::uint64_t{63};
                    bad_acquisitions +=
                        environment.dcache_requests_covering_since(
                            line, requests_before_pressure) != 1 ||
                        environment.dcache_store_ntot_line_count_since(
                            line, requests_before_pressure) != 1;
                }
                const bool errors_changed = ownership_errors_before !=
                    std::array<std::uint64_t, 3>{{
                        environment.dcache_error_response_requests(),
                        environment.dcache_denied_d_beats(),
                        environment.dcache_corrupt_d_beats(),
                    }};
                const auto early_releases = target_releases(false);
                bool ownership_qualified = bad_acquisitions == 0 &&
                    !errors_changed && early_releases == 0;
                if (force_reject_once && qualify_ownership) {
                    // Harness-only negative-path injection: force the first
                    // ownership attempt to exercise rejected cleanup, then
                    // clear the flag so the retry follows the normal path.
                    force_reject_once = false;
                    ownership_qualified = false;
                }
                if (!ownership_qualified) {
                    ++constraint_coverage.set_pressure_ownership[3];
                    constraint_coverage.set_pressure_ownership[5] +=
                        bad_acquisitions != 0;
                    constraint_coverage.set_pressure_ownership[6] += errors_changed;
                    constraint_coverage.set_pressure_ownership[7] += early_releases != 0;
                    std::cerr << "MEMBLOCK_DIRTY_OWNERSHIP_REJECT"
                              << " seed=" << options.seed << " action=" << actions
                              << " attempt=" << ownership_attempt + 1U
                              << " cycle=" << environment.cycle()
                              << " bad_acquisitions=" << bad_acquisitions
                              << " errors_changed=" << errors_changed
                              << " early_releases=" << early_releases << '\n';
                    // Keep every byte, identity and protocol oracle. Only the
                    // unqualified overlap/stall minima are inapplicable.
                    phase = "random-set-pressure-dirty-rejected-release-held";
                    for (const auto line : overlap_lines) {
                        if (!environment.release_held_dcache_response_at(line)) {
                            return false;
                        }
                    }
                    phase = "random-set-pressure-dirty-rejected-overflow";
                    if (!issue_store_range(
                            baseline_stores, pressure_stores.size(), true) ||
                        !environment.run_until_store_complete(
                            set_pressure_completion_timeout)) {
                        return false;
                    }
                    for (std::size_t index = baseline_stores;
                         index < pressure_stores.size(); ++index) {
                        if (!environment.commit_store(pressure_stores[index], 8192)) {
                            return false;
                        }
                    }
                    phase = "random-set-pressure-dirty-rejected-drain";
                    if (!environment.set_rob_head(
                            overlap_loads.front().rob, overlap_loads.front().rob_flag) ||
                        !environment.run_until_all_complete(set_pressure_completion_timeout) ||
                        !environment.run_until_queues_retired(8192) ||
                        !environment.run_until_sbuffer_empty(set_pressure_completion_timeout) ||
                        !environment.run_until_dcache_idle(set_pressure_completion_timeout) ||
                        !environment.run_until_dcache_release_idle(set_pressure_completion_timeout) ||
                        !environment.run_until_dcache_idle(set_pressure_completion_timeout)) {
                        return false;
                    }
                    std::uint64_t target_requests = 0;
                    std::uint64_t held_requests = 0;
                    std::uint64_t preserved_stores = 0;
                    constexpr std::array<memblock::LoadOp, 4> read_ops{{
                        memblock::LoadOp::lbu, memblock::LoadOp::lhu,
                        memblock::LoadOp::lwu, memblock::LoadOp::ld,
                    }};
                    const auto value_mask = width_bytes == 8 ? ~std::uint64_t{0}
                                                             : (std::uint64_t{1} << (width_bytes * 8U)) - 1U;
                    for (const auto &store : pressure_stores) {
                        const auto line_requests = environment.dcache_requests_covering_since(
                            store.address & ~std::uint64_t{63}, requests_before_pressure);
                        // Even unqualified cold-store construction must fetch
                        // each target. Extra legal acquisitions cannot hide a
                        // missing target's request through aggregate accounting.
                        if (line_requests == 0) {
                            phase = "random-set-pressure-dirty-rejected-missing-target-request";
                            return false;
                        }
                        target_requests += line_requests;
                        preserved_stores += environment.bus_expected_load(
                                                store.address, read_ops[set_pressure_width]) ==
                            (store.data & value_mask);
                    }
                    for (const auto line : overlap_lines) {
                        held_requests += environment.dcache_requests_covering_since(
                            line, overlap_requests_before);
                    }
                    const std::array<std::uint64_t, 18> rejected_delta{{
                        1, pressure_stores.size(), baseline_stores,
                        pressure_stores.size() - baseline_stores, window_count,
                        target_requests, held_requests,
                        environment.store_writebacks() - writebacks_before_pressure,
                        environment.sq_dequeued() - sq_before_pressure,
                        environment.writebacks() - overlap_writebacks_before,
                        environment.lq_dequeued() - overlap_lq_before,
                        target_releases(false), target_releases(true),
                        environment.tilelink_release_data() - releases_before_pressure,
                        environment.tilelink_release_data_verified() - verified_before_pressure,
                        environment.tilelink_expected_release_data_verified() - expected_releases_before,
                        preserved_stores,
                        environment.tilelink_requests() - all_requests_before_pressure,
                    }};
                    if (!ConstraintCoverage::rejected_pressure_accounting_valid(
                            rejected_delta, issue_orders)) {
                        phase = "random-set-pressure-dirty-rejected-accounting";
                        return false;
                    }
                    for (unsigned i = 0; i < rejected_delta.size(); ++i) {
                        constraint_coverage.set_pressure_rejected_manager[i] += rejected_delta[i];
                    }
                    for (unsigned i = 0; i < issue_orders.size(); ++i) {
                        constraint_coverage.set_pressure_rejected_issue_orders[i] += issue_orders[i];
                    }
                    ++constraint_coverage.set_pressure_ownership[4]; // cleaned
                    --constraint_coverage.set_pressure_ownership[8];
                    coverage.cacheable += pressure_stores.size();
                    environment.end_dcache_release_observation();
                    environment.clear_release_line_expectations();
                    continue;
                }
                ++constraint_coverage.set_pressure_ownership[1]; // qualified
            }
            phase = "random-set-pressure-dirty-release-backpressure";
            if (!begin_release_backpressure(true)) {
                return false;
            }
            phase = "random-set-pressure-dirty-overflow-issue";
            if (!issue_store_range(
                    baseline_stores, pressure_stores.size(),
                    set_pressure_refill_overlap) ||
                !environment.run_until_store_complete(
                    constrained_completion_timeout)) {
                return false;
            }
            phase = "random-set-pressure-dirty-overflow-commit";
            for (std::size_t index = baseline_stores;
                 index < pressure_stores.size(); ++index) {
                if (!environment.commit_store(
                        pressure_stores[index], 8192)) {
                    return false;
                }
            }
            phase = "random-set-pressure-dirty-store-issue-complete";
            if (!environment.run_until_store_complete(
                    constrained_completion_timeout)) {
                return false;
            }
            phase = "random-set-pressure-dirty-release-overlap";
            if (!prove_release_overlap(
                    true, target_releases_before_overlap)) {
                return false;
            }
            phase = "random-set-pressure-drain-complete";
            if (!environment.run_until_all_complete(
                    set_pressure_completion_timeout) ||
                !environment.run_until_queues_retired(8192) ||
                !environment.run_until_sbuffer_empty(
                    set_pressure_completion_timeout) ||
                !environment.run_until_expected_release_data_count(
                    expected_releases_before +
                        window_count * (depth - pressure_capacity_lines),
                    set_pressure_completion_timeout) ||
                !environment.run_until_dcache_idle(
                    set_pressure_completion_timeout)) {
                return false;
            }
            phase = "random-set-pressure-dirty-release-window";
            if (!finish_release_backpressure(true)) {
                return false;
            }
            // Complete any release that was held by the selected C
            // backpressure before checking its opcode. A dirty target must
            // not escape as a clean Release.
            phase = "random-set-pressure-release-idle";
            if (!environment.run_until_dcache_release_idle(
                    set_pressure_completion_timeout)) {
                return false;
            }
            const auto target_release_total = target_releases(false);
            const auto target_release_data = target_releases(true);
            if ((set_pressure_dirty &&
                    target_release_total != target_release_data) ||
                (!set_pressure_dirty && target_release_data != 0)) {
                phase = "random-set-pressure-target-release-kind";
                return false;
            }

            std::uint64_t target_requests = 0;
            std::uint64_t preserved_stores = 0;
            constexpr std::array<memblock::LoadOp, 4> unsigned_load_ops{{
                memblock::LoadOp::lbu,
                memblock::LoadOp::lhu,
                memblock::LoadOp::lwu,
                memblock::LoadOp::ld,
            }};
            const std::uint64_t value_mask =
                width_bytes == 8
                ? ~std::uint64_t{0}
                : (std::uint64_t{1} << (width_bytes * 8U)) - 1U;
            for (const auto &store : pressure_stores) {
                const std::uint64_t store_requests =
                    environment.dcache_requests_covering_since(
                        store.address & ~std::uint64_t{63},
                        requests_before_pressure);
                if (store_requests != 1) {
                    phase = "random-set-pressure-dirty-target-request";
                    return false;
                }
                target_requests += store_requests;
                if (environment.bus_expected_load(
                        store.address,
                        unsigned_load_ops[set_pressure_width]) ==
                    (store.data & value_mask)) {
                    ++preserved_stores;
                }
            }
            for (const auto &window : pressure_windows) {
                if (window_target_releases(window, true) <
                    depth - pressure_capacity_lines) {
                    phase = "random-set-pressure-dirty-window-release";
                    return false;
                }
            }
            const std::array<std::uint64_t, 9> manager_delta{{
                1,
                target_lines.size(),
                target_requests,
                environment.tilelink_expected_release_data_verified() -
                    expected_releases_before,
                environment.tilelink_release_data() -
                    releases_before_pressure,
                environment.tilelink_release_data_verified() -
                    verified_before_pressure,
                environment.store_writebacks() -
                    writebacks_before_pressure,
                environment.sq_dequeued() - sq_before_pressure,
                preserved_stores,
            }};
            const auto overlap_delta = overlap_manager_delta();
            if (manager_delta[2] != target_lines.size() ||
                manager_delta[3] <
                    window_count * (depth - pressure_capacity_lines) ||
                manager_delta[4] < manager_delta[3] ||
                manager_delta[5] != manager_delta[4] ||
                manager_delta[6] != target_lines.size() ||
                manager_delta[7] != target_lines.size() ||
                manager_delta[8] != manager_delta[3] ||
                (set_pressure_refill_overlap &&
                    (environment.writebacks() - overlap_writebacks_before !=
                            window_count ||
                        environment.lq_dequeued() - overlap_lq_before !=
                            window_count))) {
                std::ostringstream detail;
                detail << "random-set-pressure-manager"
                       << ":depth=" << depth << ":width=" << width_bytes
                       << ":set=" << set_index << ":manager=";
                for (unsigned index = 0; index < manager_delta.size();
                     ++index) {
                    detail << (index == 0 ? "" : ",") << manager_delta[index];
                }
                phase = detail.str();
                return false;
            }
            environment.end_dcache_release_observation();
            environment.clear_release_line_expectations();
            constraint_coverage.sample_set_pressure(
                true, set_pressure_refill_overlap,
                set_pressure_release_backpressure,
                set_pressure_window_class,
                set_pressure_depth_index, set_pressure_width,
                translation.regime, set_pressure_set, issue_orders,
                manager_delta, {}, overlap_delta,
                backpressure_manager_delta);
            if (qualify_ownership) {
                ++constraint_coverage.set_pressure_ownership[2]; // accepted
                --constraint_coverage.set_pressure_ownership[8];
            }
            ordinary_leaf_addressed = true;
            ++actions;
            coverage.cacheable += target_lines.size();
        }
        if (restore_stride_prefetch &&
            !environment.configure_stride_prefetch(true)) {
            return false;
        }
        pressure_action_accepted = true;
    }
    if (!pressure_action_accepted) {
        phase = "stimulus_construction_exhausted:dirty-ownership:attempts=4";
        return false;
    }

    return true;
}

bool MixedScenario::execute_miss_burst(ActionWindow &window)
{
    auto &miss_burst_depth_index = window.miss_burst_depth_index;
    auto &miss_burst_issue_width_index = window.miss_burst_issue_width_index;
    auto &miss_burst_composition = window.miss_burst_composition;
    auto &translation = window.translation;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;

    phase = "random-miss-burst";
    const unsigned depth = miss_burst_depth_index + 2U;
    const unsigned issue_width =
        miss_burst_issue_width_index + 1U;
    const unsigned scalar_load_count =
        RandomConstraints::miss_burst_scalar_load_count(
            miss_burst_depth_index, miss_burst_composition);
    const bool include_vector_load =
        miss_burst_composition == RandomConstraints::
        miss_burst_scalar_vector_load;
    constexpr std::uint64_t line_bytes = 64;
    constexpr std::uint64_t lines = miss_burst_span / line_bytes;
    if (!environment.run_until_all_complete(
            constrained_completion_timeout) ||
        !environment.run_until_queues_retired(
            constrained_completion_timeout) ||
        !environment.run_until_dcache_idle(
            constrained_completion_timeout)) {
        return false;
    }
    std::vector<memblock::LoadTransaction> transactions;
    std::optional<memblock::VectorMemoryTransaction>
        vector_transaction;
    std::vector<std::uint64_t> burst_lines;
    transactions.reserve(scalar_load_count);
    burst_lines.reserve(depth);
    while (burst_lines.size() < depth) {
        if (miss_burst_line >= lines) {
            phase = "random-miss-burst-address-space-exhausted";
            return false;
        }
        const std::uint64_t line_index = miss_burst_line++;
        const std::uint64_t line =
            miss_burst_base + line_index * line_bytes;
        // A prior legal hardware prefetch may already have made a
        // sequential candidate resident. Freshness is established
        // only from the external manager history, then the hold is
        // armed before any new DUT cycle can fetch this line.
        if (environment.dcache_requests_covering_since(line, 0) != 0) {
            continue;
        }
        const unsigned index =
            static_cast<unsigned>(burst_lines.size());
        const std::uint64_t address = line +
            ((index & 1U) != 0 ? 8U : 40U);
        burst_lines.push_back(line);
        if (transactions.size() < scalar_load_count) {
            const auto op = static_cast<memblock::LoadOp>(
                (index + random()) % 4U);
            transactions.push_back(make_load(
                address, op,
                index % memblock::kScalarLoadLanes));
            environment.expect_load(transactions.back());
        } else {
            vector_transaction = make_vector(
                false, line + 16U, random() % 4U,
                random() % memblock::kVectorMemoryLanes);
            vector_transaction->vm = true;
            environment.expect_vector(*vector_transaction);
        }
        environment.hold_dcache_response_at(line);
    }
    if (include_vector_load != vector_transaction.has_value()) {
        phase = "random-miss-burst-composition";
        return false;
    }
    const std::uint64_t requests_before_burst =
        environment.tilelink_requests();
    const std::uint64_t refills_before_burst =
        environment.dcache_refills();
    const std::uint64_t acks_before_burst =
        environment.dcache_grant_acks();
    const std::uint64_t writebacks_before_burst =
        environment.writebacks();
    const std::uint64_t vector_writebacks_before_burst =
        environment.vector_load_writebacks();
    const std::uint64_t lq_before_burst =
        environment.lq_dequeued();
    if (!environment.set_rob_head(
            transactions.front().rob,
            transactions.front().rob_flag)) {
        return false;
    }
    for (unsigned begin = 0; begin < transactions.size();
         begin += memblock::kScalarLoadLanes) {
        const unsigned end = std::min<unsigned>(
            begin + memblock::kScalarLoadLanes,
            transactions.size());
        std::vector<memblock::LoadTransaction> batch(
            transactions.begin() + begin,
            transactions.begin() + end);
        std::vector<unsigned> dispatch_lanes(batch.size());
        std::iota(dispatch_lanes.begin(), dispatch_lanes.end(), 0U);
        if (!environment.enqueue_load_batch(batch, dispatch_lanes)) {
            return false;
        }
    }
    if (vector_transaction &&
        !environment.enqueue_vector(*vector_transaction)) {
        return false;
    }
    std::vector<memblock::LoadTransaction> first_batch(
        transactions.begin(), transactions.begin() + issue_width);
    if (!environment.issue_load_batch(first_batch, 4096, true)) {
        return false;
    }
    const unsigned pressure_depth = std::min<unsigned>(
        depth, memblock::kDcacheMissEntries);
    const bool capacity_plus_one = depth > pressure_depth;
    std::array<std::uint64_t, 3> saturation_delta{};
    if (vector_transaction && !capacity_plus_one &&
        !environment.issue_vector(*vector_transaction, 4096)) {
        return false;
    }
    const unsigned scalar_issue_limit = capacity_plus_one
        ? pressure_depth
        : static_cast<unsigned>(transactions.size());
    for (unsigned index = issue_width;
         index < scalar_issue_limit; ++index) {
        if (!environment.issue_load(transactions[index], 4096)) {
            return false;
        }
    }
    const auto held_target_count = [&]() {
        return static_cast<unsigned>(std::count_if(
            burst_lines.begin(),
            burst_lines.begin() + pressure_depth,
            [&](std::uint64_t line) {
                return environment.dcache_response_held_at(line);
            }));
    };
    const auto burst_pressure_reached = [&]() {
        return held_target_count() >= pressure_depth &&
            environment.dcache_held_response_count() >=
            pressure_depth &&
            environment.dcache_outstanding_requests() >=
            pressure_depth;
    };
    for (unsigned cycle = 0;
         cycle < constrained_completion_timeout &&
         !burst_pressure_reached(); ++cycle) {
        if (!environment.run_cycles(1)) {
            return false;
        }
    }
    if (!burst_pressure_reached()) {
        phase = "random-miss-burst-outstanding-depth";
        return false;
    }
    const std::uint64_t observed_outstanding =
        environment.dcache_outstanding_requests();
    if (capacity_plus_one) {
        saturation_delta[0] = 1;
        const auto pressure_end =
            burst_lines.begin() + pressure_depth;
        const auto held = std::find_if(
            burst_lines.begin(), pressure_end,
            [&](std::uint64_t line) {
                return environment.dcache_response_held_at(line);
            });
        if (held == pressure_end ||
            !environment.schedule_held_dcache_response_release_at(
                *held)) {
            phase = "random-miss-burst-saturation-release";
            return false;
        }
        saturation_delta[1] = 1;
        const bool overflow_issued = vector_transaction
            ? environment.issue_vector(*vector_transaction, 4096)
            : environment.issue_load(
                  transactions.at(pressure_depth), 4096);
        if (!overflow_issued) {
            phase = "random-miss-burst-saturation-issue";
            return false;
        }
        const auto every_target_requested = [&]() {
            return std::all_of(
                burst_lines.begin(), burst_lines.end(),
                [&](std::uint64_t line) {
                    return environment.dcache_requests_covering_since(
                               line, requests_before_burst) != 0;
                });
        };
        for (unsigned cycle = 0;
             cycle < constrained_completion_timeout &&
             !every_target_requested(); ++cycle) {
            if (!environment.run_cycles(1)) {
                return false;
            }
        }
        if (!every_target_requested()) {
            phase = "random-miss-burst-saturation-progress";
            return false;
        }
        saturation_delta[2] = 1;
    }
    for (const auto line : burst_lines) {
        if (environment.dcache_response_held_at(line) &&
            !environment.release_held_dcache_response_at(line)) {
            return false;
        }
    }
    if ((vector_transaction &&
            !environment.run_until_vector_complete_with_replays(
                *vector_transaction,
                constrained_completion_timeout)) ||
        !environment.run_until_all_complete(
            constrained_completion_timeout) ||
        !environment.run_until_queues_retired(
            constrained_completion_timeout) ||
        !environment.run_until_dcache_idle(
            constrained_completion_timeout)) {
        return false;
    }
    std::uint64_t target_requests = 0;
    for (const auto line : burst_lines) {
        const std::uint64_t line_requests =
            environment.dcache_requests_covering_since(
                line, requests_before_burst);
        if (line_requests == 0) {
            phase = "random-miss-burst-request-accounting";
            return false;
        }
        target_requests += line_requests;
    }
    const std::array<std::uint64_t, 10> manager_delta{{
        1,
        depth,
        scalar_load_count,
        include_vector_load ? 1U : 0U,
        target_requests,
        environment.dcache_refills() - refills_before_burst,
        environment.dcache_grant_acks() - acks_before_burst,
        environment.writebacks() - writebacks_before_burst,
        environment.vector_load_writebacks() -
            vector_writebacks_before_burst,
        environment.lq_dequeued() - lq_before_burst,
    }};
    if (manager_delta[4] < depth ||
        manager_delta[5] < manager_delta[4] ||
        manager_delta[6] != manager_delta[5] ||
        manager_delta[7] != scalar_load_count ||
        manager_delta[8] != (include_vector_load ? 1U : 0U) ||
        manager_delta[9] != scalar_load_count +
                (vector_transaction ? vector_transaction->flow_num : 0U)) {
        phase = "random-miss-burst-manager-accounting";
        return false;
    }
    constraint_coverage.sample_miss_burst(
        miss_burst_depth_index, miss_burst_issue_width_index,
        miss_burst_composition, translation.regime,
        observed_outstanding, manager_delta, saturation_delta);
    ordinary_leaf_addressed = true;
    ++actions;
    coverage.cacheable += depth;

    return true;
}

} // namespace memblock::scenarios::mixed
