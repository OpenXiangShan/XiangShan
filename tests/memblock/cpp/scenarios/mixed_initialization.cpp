#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

MixedScenario::MixedScenario(int argc, char **argv, const Options &options,
    const RandomConstraints &constraints,
    std::uint64_t progress_interval)
    : options(options), constraints(constraints), progress_interval(progress_interval),
      environment(argc, argv), random(options.seed ^ 0x510e527fade682d1ULL)
{}

void MixedScenario::initialize_memory()
{
    if (constraints.canonical_boundary_per_mille != 0) {
        environment.memory().fill_incrementing(
            canonical_boundary_physical, 0x4000,
            static_cast<unsigned char>(options.seed ^ 0x83U));
    }
    if (constraints.gstage_boundary_per_mille != 0) {
        environment.memory().fill_incrementing(
            gstage_boundary_physical, 0x4000,
            static_cast<unsigned char>(options.seed ^ 0xb7U));
    }
    environment.memory().fill_incrementing(bare_base, 0x20000, 0x19);
    environment.memory().fill_incrementing(cache0_base, 0x90000, 0x43);
    environment.memory().fill_incrementing(cache1_base, 0x10000, 0x71);
    for (unsigned pbmt = 0; pbmt < hypervisor_pbmt_mappings.size(); ++pbmt) {
        for (unsigned topology = 0; topology < 4; ++topology) {
            environment.memory().fill_incrementing(
                hypervisor_physical_base(pbmt, topology), 0x1000,
                static_cast<unsigned char>(
                    0x35 + pbmt * 0x17 + topology * 0x13));
        }
    }
    for (unsigned spvp = 0; spvp < 2; ++spvp) {
        for (unsigned topology = 0; topology < 4; ++topology) {
            environment.memory().fill_incrementing(
                hypervisor_pma_device_physical_base(spvp, topology), 0x1000,
                static_cast<unsigned char>(0xb5 + spvp * 0x29 +
                    topology * 0x13));
            environment.memory().fill_incrementing(
                hypervisor_pmp_physical_base(spvp, topology), 0x4000,
                static_cast<unsigned char>(0xd5 + spvp * 0x29 +
                    topology * 0x13));
        }
    }
    environment.memory().fill_incrementing(nc_base, 0x1000, 0xa3);
    environment.memory().fill_incrementing(mmio_physical, 0x1000, 0xb7);
    environment.memory().fill_incrementing(atomic_base, 0x1000, 0xd3);
    environment.memory().fill_incrementing(stage1_napot_base, 0x40000, 0xe1);
    environment.memory().fill_incrementing(
        vector_fof_physical_base, 0x1000, 0x95);
    environment.memory().fill_incrementing(
        ptw_error_base, ptw_error_span, 0xf1);
    environment.memory().fill_incrementing(host_physical, 0x1000, 0xc5);
    // Establish pools that lack a per-line external freshness check before
    // any traffic. Hardware prefetch may legally fetch future lines, so a
    // later manager rewrite could diverge from an already resident line.
    environment.memory().define_repeating_incrementing(
        load_merge_base, load_merge_span, 64, 0x41);
    environment.memory().define_repeating_incrementing(
        miss_burst_base, miss_burst_span, 64, 0x61);
    environment.memory().define_repeating_incrementing(
        bank_conflict_base, bank_conflict_span, 64, 0x71);
    environment.configure_backpressure(
        options.seed ^ 0x1f83d9abfb41bd6bULL, options.backpressure);
}

bool MixedScenario::prepare_tail()
{
    phase = "seeded-mixed-tail";
    if (options.transactions < actions + 2U) {
        phase = "short-mixed-prefix-exceeds-transaction-budget";
        return false;
    }
    target_before_redirect = options.transactions - 2;
    if (!environment.configure_stride_prefetch(
            constraints.stride_stream_per_mille != 0)) {
        return false;
    }
    environment.configure_backpressure(
        options.seed ^ 0x1f83d9abfb41bd6bULL, options.backpressure,
        constraints.response_latency);
    const unsigned available_tail_actions = target_before_redirect - actions;
    const unsigned requested_concurrent_actions = static_cast<unsigned>(
        std::uint64_t{available_tail_actions} *
        constraints.concurrent_actions_per_mille / 1000);
    const unsigned minimum_concurrent_actions =
        constraints.concurrent_actions_per_mille == 0
        ? 0
        : std::min(
              constraints.special_concurrent_per_mille != 0 &&
                      constraints.uses_concurrent_special_operations()
                  ? 64U : 56U,
              available_tail_actions);
    const unsigned concurrent_action_budget = std::max(
        requested_concurrent_actions, minimum_concurrent_actions);
    const unsigned constrained_floor_actions =
        constraints.minimum_serial_actions();
    const unsigned maximum_concurrent_limit =
        target_before_redirect -
        std::min(constrained_floor_actions, available_tail_actions);
    concurrent_action_limit = std::min(
        maximum_concurrent_limit, actions + concurrent_action_budget);

    for (std::size_t slot = 0; slot < atomic_values.size(); ++slot) {
        atomic_values[slot] = environment.memory().expected_load(
            atomic_base + slot * 64, memblock::LoadOp::ld);
    }

    for (unsigned addressing = 0; addressing < 4; ++addressing) {
        if (constraints.vector_addressing_weights[addressing] == 0) {
            continue;
        }
        for (unsigned eew = 0; eew < 4; ++eew) {
            if (constraints.vector_eew_weights[eew] == 0) {
                continue;
            }
            for (unsigned vsew = 0; vsew < 4; ++vsew) {
                if (constraints.vector_sew_weights[vsew] == 0) {
                    continue;
                }
                for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                    if (constraints.vector_lmul_weights[lmul_log2 + 3] == 0) {
                        continue;
                    }
                    const int emul_log2 = static_cast<int>(eew) -
                        static_cast<int>(vsew) + lmul_log2;
                    if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                        emul_log2 < -3 || emul_log2 > 3 ||
                        constraints.vector_emul_weights[
                            emul_log2 + 3] == 0) {
                        continue;
                    }
                    const unsigned vector_bytes = lmul_log2 < 0
                        ? 16U >> static_cast<unsigned>(-lmul_log2)
                        : 16U << static_cast<unsigned>(lmul_log2);
                    if ((vector_bytes >> vsew) <
                        constraints.vector_policy_minimum_vlmax()) {
                        continue;
                    }
                    long double weight =
                        constraints.vector_addressing_weights[addressing];
                    weight *= constraints.vector_eew_weights[eew];
                    weight *= constraints.vector_sew_weights[vsew];
                    weight *= constraints.vector_lmul_weights[lmul_log2 + 3];
                    weight *= constraints.vector_emul_weights[emul_log2 + 3];
                    vector_shapes.push_back({
                        addressing, eew, vsew, lmul_log2, emul_log2,
                        weight});
                }
            }
        }
    }

    for (unsigned addressing = 0; addressing < 4; ++addressing) {
        if (constraints.vector_segment_addressing_weights[addressing] == 0) {
            continue;
        }
        const bool indexed = addressing >= 2;
        for (unsigned eew = 0; eew < 4; ++eew) {
            if (constraints.vector_segment_eew_weights[eew] == 0) {
                continue;
            }
            for (unsigned vsew = 0; vsew < 4; ++vsew) {
                if (constraints.vector_segment_sew_weights[vsew] == 0) {
                    continue;
                }
                for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                    if (constraints.vector_segment_lmul_weights[
                            lmul_log2 + 3] == 0) {
                        continue;
                    }
                    const int emul_log2 = static_cast<int>(eew) -
                        static_cast<int>(vsew) + lmul_log2;
                    if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                        emul_log2 < -3 || emul_log2 > 3 ||
                        constraints.vector_segment_emul_weights[
                            emul_log2 + 3] == 0) {
                        continue;
                    }
                    const unsigned group_uops = 1U <<
                        static_cast<unsigned>(std::max(
                            indexed ? lmul_log2 : emul_log2, 0));
                    for (unsigned nf_index = 0; nf_index < 7; ++nf_index) {
                        if (constraints.vector_segment_nf_weights[
                                nf_index] == 0 ||
                            group_uops * (nf_index + 2U) > 8) {
                            continue;
                        }
                        long double weight =
                            constraints.vector_segment_addressing_weights[
                                addressing];
                        weight *= constraints.vector_segment_eew_weights[eew];
                        weight *= constraints.vector_segment_sew_weights[vsew];
                        weight *= constraints.vector_segment_lmul_weights[
                            lmul_log2 + 3];
                        weight *= constraints.vector_segment_emul_weights[
                            emul_log2 + 3];
                        weight *= constraints.vector_segment_nf_weights[
                            nf_index];
                        vector_segment_shapes.push_back({
                            addressing, eew, vsew, lmul_log2, emul_log2,
                            nf_index, weight});
                    }
                }
            }
        }
    }

    return true;
}

} // namespace memblock::scenarios::mixed

namespace memblock::scenarios {

int run_mixed_scenario(int argc, char **argv, const Options &options)
{
    const RandomConstraints constraints = resolve_random_constraints(options);
    // Optional diagnostics only: never consume random numbers or advance the
    // DUT here. Flushed checkpoints survive a controller timeout and expose
    // whether the replay is still advancing actions and simulation cycles.
    const char *const progress_text =
        std::getenv("MEMBLOCK_MIXED_PROGRESS_ACTIONS");
    const std::uint64_t progress_interval = progress_text
        ? parse_u64(progress_text, "MEMBLOCK_MIXED_PROGRESS_ACTIONS") : 0;
    // The constrained coverage prefix consumes most of a short run. Require
    // enough randomized windows afterwards to exercise producer overlap.
    constexpr unsigned minimum_actions = 6144;
    if (!options.allow_short_mixed && options.transactions < minimum_actions) {
        std::cerr << "MEMBLOCK_RANDOM_MIXED_FAIL seed=" << options.seed
                  << " transactions=0 cycle=0"
                  << " reason=random-mixed_requires_at_least_"
                  << minimum_actions << "_actions\n";
        return 1;
    }
    mixed::MixedScenario scenario(argc, argv, options, constraints, progress_interval);
    return scenario.run();
}

} // namespace memblock::scenarios
