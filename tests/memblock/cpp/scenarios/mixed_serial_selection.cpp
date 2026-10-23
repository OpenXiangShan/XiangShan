#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

ActionDisposition MixedScenario::select_action(ActionWindow &window)
{
    auto &closing_stride_stream = window.closing_stride_stream;
    auto &kind = window.kind;
    report_progress();
    closing_stride_stream =
        constraints.stride_stream_per_mille != 0 &&
        stride_stream_loads < stride_training_loads;
    kind = closing_stride_stream
        ? RandomConstraints::scalar_load
        : constraints.choose_operation(random());
    if (!closing_stride_stream && probe_coverage_incomplete()) {
        kind = RandomConstraints::scalar_store;
    } else if (!closing_stride_stream) {
        const auto &uncache_latency =
            environment.uncache_response_latency_stats();
        const bool force_uncache_latency =
            options.backpressure &&
            constraints.response_latency.uncache ==
                memblock::ResponseLatencyProfile::spec &&
            uncache_latency.samples < uncache_latency.buckets.size();
        bool selected = false;
        if (force_uncache_latency) {
            for (unsigned candidate = RandomConstraints::noncacheable;
                 candidate <= RandomConstraints::mmio; ++candidate) {
                if (constraints.operation_weights[candidate] != 0 &&
                    !constraint_coverage.operation_complete(
                        constraints, candidate)) {
                    kind = candidate;
                    selected = true;
                    break;
                }
            }
            if (!selected) {
                kind = constraints.choose_concurrent_special_operation(
                    random());
                selected = true;
            }
        }
        for (unsigned candidate = 0;
             candidate < RandomConstraints::operation_count &&
             !selected;
             ++candidate) {
            if (constraints.operation_weights[candidate] != 0 &&
                !constraint_coverage.operation_complete(
                    constraints, candidate)) {
                kind = candidate;
                selected = true;
            }
        }
    }
    std::optional<unsigned> canonical_cross;
    std::optional<unsigned> vector_canonical_cross;
    std::optional<unsigned> gstage_cross;
    std::optional<unsigned> vector_gstage_cross;
    const auto find_missing_cross = [&](const auto &counts, auto enabled) {
        std::optional<unsigned> missing;
        const unsigned first = random() % counts.size();
        for (unsigned offset = 0; offset < counts.size(); ++offset) {
            const unsigned cross = (first + offset) % counts.size();
            if (enabled(cross) && counts[cross] == 0) {
                missing = cross;
                break;
            }
        }
        return missing;
    };
    if (!closing_stride_stream) {
        // A finite randomized prefix closes all boundary classes.
        // Weighted replacement starts only after ordinary/Probe closure.
        canonical_cross = find_missing_cross(
            canonical_boundary_crosses, [&](unsigned cross) {
                return constraints.canonical_boundary_cross_enabled(cross);
            });
        if (!canonical_cross) {
            vector_canonical_cross = find_missing_cross(
                vector_canonical_boundary_crosses, [&](unsigned cross) {
                    return constraints.vector_canonical_boundary_cross_enabled(cross);
                });
        }
        if (!canonical_cross && !vector_canonical_cross) {
            gstage_cross = find_missing_cross(
                gstage_boundary_crosses, [&](unsigned cross) {
                    return constraints.gstage_boundary_cross_enabled(cross);
                });
            if (!gstage_cross) {
                vector_gstage_cross = find_missing_cross(
                    vector_gstage_boundary_crosses, [&](unsigned cross) {
                        return constraints.vector_gstage_boundary_cross_enabled(cross);
                    });
            }
        }
    }
    if (!closing_stride_stream && !canonical_cross && !vector_canonical_cross &&
        !gstage_cross && !vector_gstage_cross && !probe_coverage_incomplete()) {
        const bool scalar = kind == RandomConstraints::scalar_load ||
            kind == RandomConstraints::scalar_store;
        const bool vector = kind == RandomConstraints::vector_store;
        const bool canonical_enabled = scalar
            ? constraints.canonical_boundary_per_mille != 0 &&
                constraints.translation_weights[RandomConstraints::translation_stage1] != 0 &&
                std::accumulate(constraints.stage1_mode_weights.begin(),
                    constraints.stage1_mode_weights.end(), 0U) != 0
            : vector && constraints.enabled_vector_canonical_boundary_crosses() != 0;
        const bool gstage_enabled = scalar
            ? constraints.gstage_boundary_per_mille != 0 &&
                std::accumulate(constraints.g_mode_weights.begin(),
                    constraints.g_mode_weights.end(), 0U) != 0
            : vector && constraints.enabled_vector_gstage_boundary_crosses() != 0;
        if (canonical_enabled || gstage_enabled) {
            if (!ordinary_constraint_coverage_closed) {
                ordinary_constraint_coverage_closed = constraint_coverage.complete(
                    constraints, options.backpressure,
                    environment.dcache_response_latency_stats(),
                    environment.ptw_response_latency_stats(),
                    environment.uncache_response_latency_stats(),
                    canonical_boundary_loads(), gstage_boundary_loads());
            }
            if (ordinary_constraint_coverage_closed) {
                bool canonical_selected = canonical_enabled &&
                    random() % 1000 < constraints.canonical_boundary_per_mille;
                bool gstage_selected = gstage_enabled &&
                    random() % 1000 < constraints.gstage_boundary_per_mille;
                // Independent rate draws share one action slot. Random
                // arbitration gives both subclasses a nonzero chance
                // even when both targets are fixed at 1000.
                if (canonical_selected && gstage_selected) {
                    canonical_selected = (random() & 1U) != 0;
                    gstage_selected = !canonical_selected;
                }
                if (canonical_selected && scalar) {
                    const unsigned mode = constraints.choose_stage1_mode(random());
                    canonical_cross = ((random() % 2U * 2U + mode) * 2U +
                                          (kind == RandomConstraints::scalar_store ? 1U : 0U)) * 3U +
                        random() % 3U;
                } else if (canonical_selected) {
                    vector_canonical_cross =
                        constraints.choose_vector_canonical_boundary_cross(
                            random(), random(), random(), random());
                } else if (gstage_selected && scalar) {
                    const unsigned mode = constraints.choose_g_mode(random());
                    gstage_cross = (mode * 2U +
                                       (kind == RandomConstraints::scalar_store ? 1U : 0U)) * 3U +
                        random() % 3U;
                } else if (gstage_selected) {
                    vector_gstage_cross =
                        constraints.choose_vector_gstage_boundary_cross(
                            random(), random(), random());
                }
            }
        }
    }
    if (canonical_cross) {
        if (!issue_canonical_boundary(*canonical_cross)) {
            return ActionDisposition::failed;
        }
        return ActionDisposition::boundary_completed;
    }
    if (vector_canonical_cross) {
        if (!issue_vector_canonical_boundary(*vector_canonical_cross)) {
            return ActionDisposition::failed;
        }
        return ActionDisposition::boundary_completed;
    }
    if (gstage_cross || vector_gstage_cross) {
        if (!issue_gstage_boundary(
                gstage_cross ? *gstage_cross : *vector_gstage_cross,
                vector_gstage_cross.has_value())) {
            return ActionDisposition::failed;
        }
        return ActionDisposition::boundary_completed;
    }

    return ActionDisposition::ordinary;
}

} // namespace memblock::scenarios::mixed
