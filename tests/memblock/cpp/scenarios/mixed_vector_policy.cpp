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
        if (constraints.vector_segment_stores_per_mille != 1000 &&
            constraint_coverage.vector_segment_directions[0] == 0) {
            vector_segment_store = false;
        } else if (
            constraints.vector_segment_stores_per_mille != 0 &&
            constraint_coverage.vector_segment_directions[1] == 0) {
            vector_segment_store = true;
        }
    }
    bool vector_segment_fof_cross_missing = false;
    bool missing_vector_segment_fof_first_fault = false;
    unsigned missing_vector_segment_fof_mode = 0;
    const RandomVectorSegmentShape *missing_vector_segment_fof_shape =
        nullptr;
    if (kind == RandomConstraints::vector_segment &&
        constraints.vector_segment_fof_per_mille != 0) {
        for (unsigned fault = 0;
             fault < 2 && !vector_segment_fof_cross_missing; ++fault) {
            for (unsigned mode = 0;
                 mode < 2 && !vector_segment_fof_cross_missing; ++mode) {
                for (const auto &shape : vector_segment_shapes) {
                    if (shape.addressing != 0 ||
                        !constraints.vector_segment_fof_cross_enabled(
                            fault, mode, shape.eew, shape.vsew,
                            shape.lmul_log2, shape.nf_index)) {
                        continue;
                    }
                    const unsigned cross = ConstraintCoverage::
                        vector_segment_fof_cross_index(
                            fault, mode, shape.eew, shape.vsew,
                            shape.lmul_log2, shape.nf_index);
                    if (constraint_coverage
                            .vector_segment_fof_crosses[cross] == 0) {
                        vector_segment_fof_cross_missing = true;
                        missing_vector_segment_fof_first_fault =
                            fault != 0;
                        missing_vector_segment_fof_mode = mode;
                        missing_vector_segment_fof_shape = &shape;
                        break;
                    }
                }
            }
        }
    }
    if (kind == RandomConstraints::vector_segment) {
        vector_segment_store =
            !vector_segment_fof_cross_missing && vector_segment_store;
    }
    vector_segment_fof =
        kind == RandomConstraints::vector_segment &&
        !vector_segment_store &&
        (vector_segment_fof_cross_missing || choose_binary_class(
                                                 constraints.vector_segment_fof_per_mille,
                                                 constraint_coverage.vector_segment_fof_presence));
    vector_segment_fof_first_fault = false;
    vector_segment_fof_stage1_mode = 0;
    vector_segment_fof_shape = nullptr;
    if (vector_segment_fof) {
        if (vector_segment_fof_cross_missing) {
            vector_segment_fof_first_fault =
                missing_vector_segment_fof_first_fault;
            vector_segment_fof_stage1_mode =
                missing_vector_segment_fof_mode;
            vector_segment_fof_shape =
                missing_vector_segment_fof_shape;
        } else {
            vector_segment_fof_first_fault = choose_binary_class(
                constraints.vector_segment_fof_first_fault_per_mille,
                constraint_coverage.vector_segment_fof_fault_positions);
            vector_segment_fof_stage1_mode =
                constraints.choose_stage1_mode(random());
            vector_segment_fof_shape =
                &choose_vector_segment_fof_shape();
        }
    }
    vector_segment_high_va_cross = {};
    if (kind == RandomConstraints::vector_segment &&
        constraints.vector_segment_high_va_per_mille != 0) {
        constexpr unsigned high_va_cross_count =
            RandomConstraints::vector_segment_high_va_cross_count;
        const unsigned first = random() % high_va_cross_count;
        for (unsigned offset = 0; offset < high_va_cross_count;
             ++offset) {
            const unsigned cross =
                (first + offset) % high_va_cross_count;
            if (constraints.vector_segment_high_va_cross_enabled(cross) &&
                constraint_coverage.vector_segment_high_va_crosses[cross] == 0) {
                vector_segment_high_va_cross = cross;
                break;
            }
        }
        if (!vector_segment_high_va_cross && !vector_segment_fof &&
            !ordinary_constraint_coverage_closed) {
            ordinary_constraint_coverage_closed = constraint_coverage.complete(
                constraints, options.backpressure,
                environment.dcache_response_latency_stats(),
                environment.ptw_response_latency_stats(),
                environment.uncache_response_latency_stats(),
                canonical_boundary_loads(), gstage_boundary_loads());
        }
        if (!vector_segment_high_va_cross && !vector_segment_fof &&
            ordinary_constraint_coverage_closed &&
            random() % 1000 < constraints.vector_segment_high_va_per_mille) {
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
    bool vector_fof_cross_missing = false;
    if (kind == RandomConstraints::vector_load &&
        constraints.vector_fof_per_mille != 0) {
        for (unsigned fault = 0;
             fault < 2 && !vector_fof_cross_missing; ++fault) {
            const bool fault_enabled = fault != 0
                ? constraints.vector_fof_first_fault_per_mille != 0
                : constraints.vector_fof_first_fault_per_mille != 1000;
            for (unsigned mode = 0;
                 mode < 2 && !vector_fof_cross_missing; ++mode) {
                for (unsigned eew = 0; eew < 4; ++eew) {
                    const unsigned cross =
                        (fault * 2 + mode) * 4 + eew;
                    vector_fof_cross_missing = fault_enabled &&
                        constraints.stage1_mode_weights[mode] != 0 &&
                        constraints.vector_eew_weights[eew] != 0 &&
                        constraint_coverage.vector_fof_crosses[cross] ==
                            0;
                    if (vector_fof_cross_missing) {
                        break;
                    }
                }
            }
        }
    }
    vector_fof = kind == RandomConstraints::vector_load &&
        (vector_fof_cross_missing || choose_binary_class(
                                         constraints.vector_fof_per_mille,
                                         constraint_coverage.vector_fof_presence));
    vector_fof_first_fault = false;
    vector_fof_stage1_mode = 0;
    vector_fof_eew = 0;
    if (vector_fof) {
        std::optional<unsigned> uncovered_cross;
        for (unsigned fault = 0; fault < 2 && !uncovered_cross;
             ++fault) {
            const bool fault_enabled = fault != 0
                ? constraints.vector_fof_first_fault_per_mille != 0
                : constraints.vector_fof_first_fault_per_mille != 1000;
            for (unsigned mode = 0; mode < 2 && !uncovered_cross;
                 ++mode) {
                for (unsigned eew = 0; eew < 4; ++eew) {
                    const unsigned cross =
                        (fault * 2 + mode) * 4 + eew;
                    if (fault_enabled &&
                        constraints.stage1_mode_weights[mode] != 0 &&
                        constraints.vector_eew_weights[eew] != 0 &&
                        constraint_coverage.vector_fof_crosses[cross] ==
                            0) {
                        uncovered_cross = cross;
                        break;
                    }
                }
            }
        }
        if (uncovered_cross) {
            vector_fof_first_fault = *uncovered_cross / 8U != 0;
            vector_fof_stage1_mode = (*uncovered_cross / 4U) % 2U;
            vector_fof_eew = *uncovered_cross % 4U;
        } else {
            vector_fof_first_fault = choose_binary_class(
                constraints.vector_fof_first_fault_per_mille,
                constraint_coverage.vector_fof_fault_positions);
            vector_fof_stage1_mode =
                constraints.choose_stage1_mode(random());
            vector_fof_eew = constraints.choose_vector_eew(random());
        }
    }
}

} // namespace memblock::scenarios::mixed
