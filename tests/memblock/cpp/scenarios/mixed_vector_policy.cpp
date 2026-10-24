#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

void MixedScenario::prepare_vector_policy(ActionWindow &window)
{
    auto &kind = window.kind;
    auto &vector_segment_store = window.vector_segment_store;
    auto &vector_segment_fof = window.vector_segment_fof;
    auto &vector_segment_fof_first_fault = window.vector_segment_fof_first_fault;
    auto &vector_segment_fof_stage1_mode = window.vector_segment_fof_stage1_mode;
    auto &vector_segment_fof_shape = window.vector_segment_fof_shape;
    auto &vector_segment_high_va_cross = window.vector_segment_high_va_cross;
    auto &vector_fof = window.vector_fof;
    auto &vector_fof_first_fault = window.vector_fof_first_fault;
    auto &vector_fof_stage1_mode = window.vector_fof_stage1_mode;
    auto &vector_fof_eew = window.vector_fof_eew;
    if (kind == RandomConstraints::vector_segment) {
        vector_segment_store = choose_binary_class(
            constraints.vector_segment_stores_per_mille,
            constraint_coverage.vector_segment_directions);
    }
    vector_segment_fof =
        kind == RandomConstraints::vector_segment &&
        !vector_segment_store &&
        choose_binary_class(
            constraints.vector_segment_fof_per_mille,
            constraint_coverage.vector_segment_fof_presence);
    vector_segment_fof_first_fault = false;
    vector_segment_fof_stage1_mode = 0;
    vector_segment_fof_shape = nullptr;
    if (vector_segment_fof) {
        vector_segment_fof_first_fault = choose_binary_class(
            constraints.vector_segment_fof_first_fault_per_mille,
            constraint_coverage.vector_segment_fof_fault_positions);
        vector_segment_fof_stage1_mode =
            constraints.choose_stage1_mode(random());
        vector_segment_fof_shape =
            &choose_vector_segment_fof_shape();
    }
    vector_segment_high_va_cross = {};
    if (kind == RandomConstraints::vector_segment &&
        constraints.vector_segment_high_va_per_mille != 0 &&
        !vector_segment_fof) {
        constexpr unsigned high_va_cross_count =
            RandomConstraints::vector_segment_high_va_cross_count;
        if (random() % 1000 < constraints.vector_segment_high_va_per_mille) {
            std::array<std::uint64_t, high_va_cross_count> weights{};
            std::uint64_t total = 0;
            for (unsigned cross = 0; cross < high_va_cross_count;
                 ++cross) {
                if (!constraints.vector_segment_high_va_cross_enabled(cross)) {
                    continue;
                }
                const bool nested = RandomConstraints::
                    vector_segment_high_va_cross_is_nested(cross);
                const unsigned cross_class = nested ? cross - 8U : cross;
                const bool store = (cross_class / 2U) % 2U != 0;
                const bool misaligned = cross % 2U != 0;
                std::uint64_t translation_weight = 0;
                if (nested) {
                    const unsigned pair = RandomConstraints::
                        vector_segment_high_va_nested_pair(cross);
                    translation_weight =
                        std::uint64_t{
                            constraints.translation_weights[
                                RandomConstraints::translation_nested]} *
                        constraints.vs_mode_weights[pair / 2U] *
                        constraints.g_mode_weights[pair % 2U];
                } else {
                    translation_weight =
                        std::uint64_t{
                            constraints.translation_weights[
                                RandomConstraints::translation_stage1]} *
                        constraints.stage1_mode_weights[cross / 4U];
                }
                weights[cross] = translation_weight *
                    (store ? constraints.vector_segment_stores_per_mille
                           : 1000U - constraints.vector_segment_stores_per_mille) *
                    (misaligned ? constraints.misaligned_per_mille
                                : 1000U - constraints.misaligned_per_mille);
                total += weights[cross];
            }
            if (total != 0) {
                std::uint64_t selection = random() % total;
                for (unsigned cross = 0; cross < high_va_cross_count;
                     ++cross) {
                    if (selection < weights[cross]) {
                        vector_segment_high_va_cross = cross;
                        break;
                    }
                    selection -= weights[cross];
                }
            }
        }
        if (vector_segment_high_va_cross) {
            const unsigned cross_class = RandomConstraints::
                vector_segment_high_va_cross_is_nested(
                    *vector_segment_high_va_cross)
                ? *vector_segment_high_va_cross - 8U
                : *vector_segment_high_va_cross;
            vector_segment_store = (cross_class / 2U) % 2U != 0;
            vector_segment_fof = false;
        }
    }
    vector_fof = kind == RandomConstraints::vector_load &&
        choose_binary_class(
            constraints.vector_fof_per_mille,
            constraint_coverage.vector_fof_presence);
    vector_fof_first_fault = false;
    vector_fof_stage1_mode = 0;
    vector_fof_eew = 0;
    if (vector_fof) {
        vector_fof_first_fault = choose_binary_class(
            constraints.vector_fof_first_fault_per_mille,
            constraint_coverage.vector_fof_fault_positions);
        vector_fof_stage1_mode =
            constraints.choose_stage1_mode(random());
        vector_fof_eew = constraints.choose_vector_eew(random());
    }
}

} // namespace memblock::scenarios::mixed
