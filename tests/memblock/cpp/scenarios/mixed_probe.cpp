#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::observe_action(ActionWindow &window)
{
    auto &kind = window.kind;
    auto &translation = window.translation;
    auto &leaf = window.leaf;
    auto &ptw_requests_before = window.ptw_requests_before;
    auto &requests_before = window.requests_before;
    auto &sample_dcache = window.sample_dcache;
    auto &sample_translation_action = window.sample_translation_action;
    auto &leaf_addressed = window.leaf_addressed;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;
    auto &probe_candidate = window.probe_candidate;
    constraint_coverage.sample_operation(kind);
    const bool ordinary_leaf_enabled =
        translation.regime == RandomConstraints::translation_stage1
        ? constraints.stage1_napot_per_mille != 1000
        : translation.regime == RandomConstraints::translation_nested
            ? constraints.nested_vs_napot_per_mille != 1000 &&
                constraints.nested_g_napot_per_mille != 1000
            : false;
    const bool sample_ordinary_leaf =
        ordinary_leaf_addressed && ordinary_leaf_enabled;
    const TranslationLeafTopology observed_leaf = leaf_addressed
        ? leaf : TranslationLeafTopology{};
    if (sample_translation_action) {
        constraint_coverage.sample_translation(
            translation, observed_leaf, ptw_requests_before,
            environment.ptw_requests(), 1,
            (leaf_addressed || sample_ordinary_leaf) ? 1 : 0);
    }
    if (sample_dcache) {
        constraint_coverage.sample_dcache(
            requests_before, environment.tilelink_requests());
    }
    if (probe_candidate && constraints.probes_per_mille != 0 &&
        random() % 1000 < constraints.probes_per_mille) {
        phase = "random-probe";
        const bool to_b =
            random() % 1000 < constraints.probe_to_b_per_mille;
        const bool need_data =
            random() % 1000 < constraints.probe_need_data_per_mille;
        const unsigned probe_depth_class =
            constraints.choose_probe_depth(random());
        const unsigned probe_depth = probe_depth_class + 1U;
        const bool probe_overlap = probe_depth > 1U;

        std::ostringstream probe_phase;
        probe_phase << "random-probe:address=0x" << std::hex
                    << *probe_candidate << std::dec
                    << ":cap=" << (to_b ? "toB" : "toN")
                    << ":need_data=" << need_data
                    << ":depth=" << probe_depth;
        phase = probe_phase.str();

        const auto expected_line = environment.memory().read_beat(
            *probe_candidate, 64);
        const std::uint64_t responses_before =
            environment.dcache_probe_responses();
        const unsigned probe_completion_timeout =
            std::max(constrained_completion_timeout, 8192U);
        // SQ retirement only moves the committed write into SBuffer.
        // Drain it before requiring a dirty Probe response.
        if (!environment.run_until_sbuffer_empty(
                probe_completion_timeout)) {
            return false;
        }

        std::optional<memblock::LoadTransaction> overlap_load;
        std::uint64_t overlap_writebacks_before = 0;
        if (probe_overlap) {
            phase = probe_phase.str() + ":prepare-overlap";
            constexpr std::uint64_t block_bytes = 0x200000;
            constexpr std::uint64_t half_block_bytes = 0x100000;
            constexpr std::uint64_t lines_per_half =
                half_block_bytes / 64;
            constexpr std::uint64_t overlap_blocks =
                (probe_overlap_span / 2U) / block_bytes;
            const std::uint64_t sequence = probe_overlap_line++;
            const std::uint64_t block =
                (sequence / lines_per_half) % overlap_blocks;
            const std::uint64_t line_index = sequence % lines_per_half;
            const unsigned candidate_set = static_cast<unsigned>(
                (*probe_candidate >> 6) & 0x7fU);
            std::vector<std::uint64_t> clean_lines;
            for (std::uint64_t offset = 0;
                 clean_lines.size() < probe_depth - 1U; ++offset) {
                const std::uint64_t clean_line_index =
                    (line_index + offset) % lines_per_half;
                if ((clean_line_index & 0x7fU) == candidate_set) {
                    continue;
                }
                clean_lines.push_back(
                    probe_overlap_base + block * block_bytes +
                    clean_line_index * 64);
            }
            for (const std::uint64_t clean_line : clean_lines) {
                const auto clean_load = make_load(
                    clean_line + 8 * (random() % 8),
                    memblock::LoadOp::ld,
                    random() % memblock::kScalarLoadLanes);
                environment.expect_load(clean_load);
                if (!environment.set_rob_head(
                        clean_load.rob, clean_load.rob_flag) ||
                    !environment.enqueue_load(clean_load) ||
                    !environment.issue_load(clean_load, 4096) ||
                    !environment.run_until_complete(
                        probe_completion_timeout) ||
                    !environment.run_until_lq_retired(8192)) {
                    return false;
                }
            }

            const std::uint64_t miss_line =
                probe_overlap_base + block * block_bytes +
                half_block_bytes + line_index * 64;
            overlap_load = make_load(
                miss_line + 8 * (random() % 8),
                memblock::LoadOp::ld,
                random() % memblock::kScalarLoadLanes);
            environment.expect_load(*overlap_load);
            const std::uint64_t requests_before_overlap =
                environment.tilelink_requests();
            overlap_writebacks_before = environment.writebacks();
            environment.force_next_dcache_response_delay(
                2048U + static_cast<unsigned>(random() % 2049U));
            if (!environment.set_rob_head(
                    overlap_load->rob, overlap_load->rob_flag) ||
                !environment.enqueue_load(*overlap_load) ||
                !environment.issue_load(*overlap_load, 4096) ||
                !environment.run_until_dcache_requests(
                    requests_before_overlap + 1, 8192) ||
                environment.writebacks() != overlap_writebacks_before) {
                return false;
            }
            if (!environment.hold_dcache_c_until_probes(
                    environment.dcache_probes() + probe_depth)) {
                return false;
            }
            for (const std::uint64_t clean_line : clean_lines) {
                if (!environment.request_dcache_probe(
                        clean_line, 2, false, 2)) {
                    return false;
                }
            }
        }
        if (!probe_overlap && !environment.hold_dcache_c_until_probes(
                                  environment.dcache_probes() + probe_depth)) {
            return false;
        }

        phase = probe_phase.str() + ":probe";
        const std::uint64_t probe_depth_before =
            environment.dcache_max_probe_outstanding();
        if (!environment.request_dcache_probe(
                *probe_candidate, to_b ? 1U : 2U, need_data,
                to_b ? 0U : 1U, expected_line)) {
            phase = probe_phase.str() + ":primary-request";
            return false;
        }
        if (!environment.run_until_probe_responses(
                responses_before + probe_depth,
                probe_completion_timeout)) {
            phase = probe_phase.str() + ":response-drain";
            return false;
        }
        if (probe_overlap &&
            environment.writebacks() != overlap_writebacks_before) {
            phase = probe_phase.str() + ":early-delayed-writeback";
            return false;
        }
        if (probe_overlap &&
            environment.dcache_max_probe_outstanding() <
                std::max<std::uint64_t>(probe_depth,
                    probe_depth_before)) {
            phase = probe_phase.str() + ":outstanding-depth";
            return false;
        }
        if (to_b) {
            if (!environment.request_dcache_probe(
                    *probe_candidate, 2, false, 2)) {
                phase = probe_phase.str() + ":cleanup-request";
                return false;
            }
            if (!environment.run_until_probe_responses(
                    responses_before + probe_depth + 1U,
                    probe_completion_timeout)) {
                phase = probe_phase.str() + ":cleanup-response";
                return false;
            }
        }
        if (overlap_load) {
            phase = probe_phase.str() + ":delayed-load-complete";
            if (!environment.run_until_complete(
                    probe_completion_timeout)) {
                return false;
            }
            phase = probe_phase.str() + ":delayed-load-retire";
            if (!environment.run_until_lq_retired(8192)) {
                return false;
            }
            phase = probe_phase.str() + ":delayed-manager-drain";
            if (!environment.run_until_dcache_idle(
                    probe_completion_timeout)) {
                return false;
            }
        }
        constraint_coverage.sample_probe(
            probe_depth_class, to_b, need_data);
        phase = "seeded-mixed-tail";
    }

    return true;
}

} // namespace memblock::scenarios::mixed
