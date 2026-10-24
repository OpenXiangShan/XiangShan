#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

ActionDisposition MixedScenario::select_action(ActionWindow &window)
{
    auto &kind = window.kind;
    report_progress();
    kind = constraints.choose_operation(random());

    std::optional<unsigned> canonical_cross;
    std::optional<unsigned> vector_canonical_cross;
    std::optional<unsigned> gstage_cross;
    std::optional<unsigned> vector_gstage_cross;
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
        bool canonical_selected = canonical_enabled &&
            random() % 1000 < constraints.canonical_boundary_per_mille;
        bool gstage_selected = gstage_enabled &&
            random() % 1000 < constraints.gstage_boundary_per_mille;
        // Independent rate draws share one action slot. Random arbitration
        // gives both subclasses a nonzero chance even when both targets are
        // fixed at 1000.
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
