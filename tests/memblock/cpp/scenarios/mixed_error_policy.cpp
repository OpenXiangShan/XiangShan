#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::retire_random_nc_store_error(
    std::uint64_t lq_dequeued_before,
    std::uint64_t sq_dequeued_before,
    std::uint64_t lq_cancels_before,
    std::uint64_t sq_cancels_before)
{
    phase = "random-nc-store-error-retire";
    if (!environment.run_cycles(8) ||
        !environment.run_until_queues_retired(8192)) {
        return false;
    }
    return environment.lq_dequeued() == lq_dequeued_before &&
        environment.sq_dequeued() == sq_dequeued_before + 1 &&
        environment.lq_redirect_canceled_observed() ==
        lq_cancels_before &&
        environment.sq_redirect_canceled_observed() ==
        sq_cancels_before;
}

bool MixedScenario::ptw_error_outcome_enabled(unsigned outcome)
{
    return outcome == 0
        ? constraints.ptw_error_denied_per_mille != 0
        : outcome == 1
            ? constraints.ptw_error_denied_per_mille != 1000 &&
                constraints.ptw_error_corrupt_first_per_mille != 0
            : constraints.ptw_error_denied_per_mille != 1000 &&
                constraints.ptw_error_corrupt_first_per_mille != 1000;
}

bool MixedScenario::ptw_error_mode_slot_enabled(
    unsigned site, unsigned slot)
{
    if (site == RandomConstraints::ptw_error_stage1) {
        return slot < 2 && constraints.stage1_mode_weights[slot] != 0;
    }
    if (site == RandomConstraints::ptw_error_gstage) {
        return slot < 2 && constraints.g_mode_weights[slot] != 0;
    }
    return slot < 4 &&
        constraints.vs_mode_weights[slot / 2] != 0 &&
        constraints.g_mode_weights[slot % 2] != 0;
}

bool MixedScenario::ptw_error_target_level_enabled(
    unsigned site, unsigned target_level)
{
    if (constraints.ptw_error_site_weights[site] == 0) {
        return false;
    }
    for (unsigned mode = 0; mode < 2; ++mode) {
        const bool mode_enabled =
            site == RandomConstraints::ptw_error_stage1
            ? constraints.stage1_mode_weights[mode] != 0
            : site == RandomConstraints::ptw_error_nested_vs
                ? constraints.vs_mode_weights[mode] != 0
                : constraints.g_mode_weights[mode] != 0;
        const unsigned levels = mode == 0 ? 3U : 4U;
        if (!mode_enabled) {
            continue;
        }
        if ((target_level == 0 &&
                constraints.ptw_error_level_weights[
                    RandomConstraints::ptw_error_leaf] != 0) ||
            (target_level + 1 == levels &&
                constraints.ptw_error_level_weights[
                    RandomConstraints::ptw_error_root] != 0) ||
            (target_level > 0 && target_level + 1 < levels &&
                constraints.ptw_error_level_weights[
                    RandomConstraints::ptw_error_intermediate] != 0)) {
            return true;
        }
    }
    return false;
}

} // namespace memblock::scenarios::mixed
