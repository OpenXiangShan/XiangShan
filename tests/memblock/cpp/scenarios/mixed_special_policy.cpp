#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

void MixedScenario::prepare_special_policy(ActionWindow &window)
{
    auto &kind = window.kind;
    auto &cmo_operation = window.cmo_operation;
    auto &cmo_dirty = window.cmo_dirty;
    auto &cmo_younger_overlap = window.cmo_younger_overlap;
    auto &cmo_probe_depth_class = window.cmo_probe_depth_class;
    auto &cmo_error_denied = window.cmo_error_denied;
    auto &hypervisor_family = window.hypervisor_family;
    auto &hypervisor_spvp_user = window.hypervisor_spvp_user;
    auto &hypervisor_misaligned = window.hypervisor_misaligned;
    auto &hypervisor_pma_device = window.hypervisor_pma_device;
    auto &hypervisor_pmp_relation = window.hypervisor_pmp_relation;
    auto &hypervisor_pbmt_pair = window.hypervisor_pbmt_pair;
    auto &ptw_error_site = window.ptw_error_site;
    auto &ptw_error_level_class = window.ptw_error_level_class;
    auto &ptw_error_outcome = window.ptw_error_outcome;
    auto &ptw_error_stage1_mode = window.ptw_error_stage1_mode;
    auto &ptw_error_vs_mode = window.ptw_error_vs_mode;
    auto &ptw_error_g_mode = window.ptw_error_g_mode;
    auto &ptw_error_target_level = window.ptw_error_target_level;
    auto &ptw_error_store = window.ptw_error_store;
    auto &ptw_error_address = window.ptw_error_address;
    auto &ptw_error_guest_address = window.ptw_error_guest_address;
    auto &ptw_error_stage1_root = window.ptw_error_stage1_root;
    auto &ptw_error_vs_root = window.ptw_error_vs_root;
    auto &ptw_error_g_root = window.ptw_error_g_root;
    auto &set_pressure_depth_index = window.set_pressure_depth_index;
    auto &set_pressure_width = window.set_pressure_width;
    auto &set_pressure_set = window.set_pressure_set;
    auto &set_pressure_dirty = window.set_pressure_dirty;
    auto &set_pressure_refill_overlap = window.set_pressure_refill_overlap;
    auto &set_pressure_release_backpressure = window.set_pressure_release_backpressure;
    auto &set_pressure_window_class = window.set_pressure_window_class;
    auto &miss_burst_depth_index = window.miss_burst_depth_index;
    auto &miss_burst_issue_width_index = window.miss_burst_issue_width_index;
    auto &miss_burst_composition = window.miss_burst_composition;
    cmo_operation = 0;
    cmo_dirty = false;
    cmo_younger_overlap = false;
    cmo_probe_depth_class = 0;
    cmo_error_denied = {};
    if (kind == RandomConstraints::cmo) {
        cmo_operation = constraints.choose_cmo_operation(random());
        if (constraints.cmo_error_per_mille != 1000) {
            cmo_probe_depth_class =
                constraints.choose_cmo_probe_depth(random());
        }
        if (!cmo_error_denied) {
            const bool cmo_error = choose_binary_class(
                constraints.cmo_error_per_mille,
                constraint_coverage.cmo_errors);
            if (cmo_error) {
                cmo_error_denied = choose_binary_class(
                    constraints.cmo_error_denied_per_mille,
                    constraint_coverage.cmo_error_kinds);
            }
        }
        cmo_dirty = choose_binary_class(
            constraints.cmo_dirty_per_mille,
            constraint_coverage.cmo_line_states);
        cmo_younger_overlap = choose_binary_class(
            constraints.cmo_younger_overlap_per_mille,
            constraint_coverage.cmo_younger_overlaps);
    }
    hypervisor_family = {};
    hypervisor_spvp_user = false;
    hypervisor_misaligned = false;
    hypervisor_pma_device = false;
    hypervisor_pmp_relation =
        RandomConstraints::hypervisor_pmp_none;
    hypervisor_pbmt_pair =
        RandomConstraints::hypervisor_pbmt_pma_pma;
    if (kind == RandomConstraints::hypervisor) {
        hypervisor_family =
            constraints.choose_hypervisor_family(random());
        hypervisor_spvp_user = choose_binary_class(
            constraints.hypervisor_spvp_user_per_mille,
            constraint_coverage.hypervisor_spvp);
        hypervisor_misaligned = choose_binary_class(
            constraints.misaligned_per_mille,
            constraint_coverage.hypervisor_alignments);
        hypervisor_pma_device = choose_binary_class(
            constraints.hypervisor_pma_device_per_mille,
            constraint_coverage.hypervisor_pma_devices);
        hypervisor_pmp_relation =
            constraints.choose_hypervisor_pmp_relation(random());
        if (hypervisor_pmp_relation !=
            RandomConstraints::hypervisor_pmp_none) {
            hypervisor_pbmt_pair =
                RandomConstraints::hypervisor_pbmt_pma_pma;
            hypervisor_pma_device = false;
            hypervisor_misaligned =
                hypervisor_pmp_relation >= RandomConstraints::
                hypervisor_pmp_cross_lower;
        } else if (hypervisor_pma_device) {
            hypervisor_pbmt_pair =
                RandomConstraints::hypervisor_pbmt_pma_pma;
            hypervisor_misaligned = false;
        } else {
            hypervisor_pbmt_pair =
                constraints.choose_hypervisor_pbmt_pair(random());
            if (hypervisor_pbmt_pair !=
                RandomConstraints::hypervisor_pbmt_pma_pma) {
                hypervisor_misaligned = false;
            }
        }
    }
    ptw_error_site = RandomConstraints::ptw_error_stage1;
    ptw_error_level_class = RandomConstraints::ptw_error_root;
    ptw_error_outcome = 0;
    ptw_error_stage1_mode = 0;
    ptw_error_vs_mode = 0;
    ptw_error_g_mode = 0;
    ptw_error_target_level = 0;
    ptw_error_store = false;
    ptw_error_address = 0;
    ptw_error_guest_address = 0;
    ptw_error_stage1_root = 0;
    ptw_error_vs_root = 0;
    ptw_error_g_root = 0;
    set_pressure_depth_index = 0;
    set_pressure_width = 0;
    set_pressure_set = 0;
    set_pressure_dirty = false;
    set_pressure_refill_overlap = false;
    set_pressure_release_backpressure = false;
    set_pressure_window_class =
        RandomConstraints::set_pressure_single_window;
    miss_burst_depth_index = 0;
    miss_burst_issue_width_index = 0;
    miss_burst_composition =
        RandomConstraints::miss_burst_scalar_only;
    if (kind == RandomConstraints::set_pressure) {
        set_pressure_depth_index =
            constraints.choose_set_pressure_depth(random());
        set_pressure_width =
            constraints.choose_set_pressure_width(random());
        set_pressure_set =
            constraints.choose_set_pressure_set(random());
        set_pressure_dirty = choose_binary_class(
            constraints.set_pressure_dirty_per_mille,
            constraint_coverage.set_pressure_line_states);
        set_pressure_refill_overlap = choose_binary_class(
            constraints.set_pressure_refill_overlap_per_mille,
            constraint_coverage.set_pressure_refill_overlaps);
        set_pressure_release_backpressure = choose_binary_class(
            constraints.set_pressure_release_backpressure_per_mille,
            constraint_coverage.set_pressure_release_backpressures);
        set_pressure_window_class =
            constraints.choose_set_pressure_window(random());
    } else if (kind == RandomConstraints::miss_burst) {
        miss_burst_depth_index =
            constraints.choose_miss_burst_depth(random());
        miss_burst_composition =
            constraints.choose_miss_burst_composition(random());
        miss_burst_issue_width_index =
            constraints.choose_miss_burst_issue_width(
                miss_burst_depth_index, miss_burst_composition,
                random());
    }
    if (kind == RandomConstraints::ptw_error) {
        ptw_error_site = constraints.choose_ptw_error_site(random());
        ptw_error_level_class =
            constraints.choose_ptw_error_level(random());
        ptw_error_store = random() % 1000 <
            constraints.ptw_error_stores_per_mille;
        if (random() % 1000 < constraints.ptw_error_denied_per_mille) {
            ptw_error_outcome = 0;
        } else {
            ptw_error_outcome = random() % 1000 <
                    constraints.ptw_error_corrupt_first_per_mille
                ? 1U : 2U;
        }

        std::optional<unsigned> forced_mode_slot;
        std::optional<unsigned> forced_governing_mode;
        std::optional<unsigned> forced_target_level;

        if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
            ptw_error_stage1_mode =
                constraints.choose_stage1_mode(random());
        } else if (ptw_error_site ==
            RandomConstraints::ptw_error_gstage) {
            ptw_error_g_mode = constraints.choose_g_mode(random());
        } else {
            ptw_error_vs_mode = constraints.choose_vs_mode(random());
            ptw_error_g_mode = constraints.choose_g_mode(random());
        }
        if (forced_mode_slot) {
            if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                ptw_error_stage1_mode = *forced_mode_slot;
            } else if (ptw_error_site ==
                RandomConstraints::ptw_error_gstage) {
                ptw_error_g_mode = *forced_mode_slot;
            } else {
                ptw_error_vs_mode = *forced_mode_slot / 2;
                ptw_error_g_mode = *forced_mode_slot % 2;
            }
        }
        if (forced_governing_mode) {
            if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                ptw_error_stage1_mode = *forced_governing_mode;
            } else if (ptw_error_site ==
                RandomConstraints::ptw_error_nested_vs) {
                ptw_error_vs_mode = *forced_governing_mode;
            } else {
                ptw_error_g_mode = *forced_governing_mode;
            }
        }
        const unsigned governing_mode =
            ptw_error_site == RandomConstraints::ptw_error_stage1
            ? ptw_error_stage1_mode
            : ptw_error_site == RandomConstraints::ptw_error_nested_vs
                ? ptw_error_vs_mode
                : ptw_error_g_mode;
        const unsigned levels = governing_mode == 0 ? 3U : 4U;
        if (forced_target_level) {
            ptw_error_target_level = *forced_target_level;
        } else if (ptw_error_level_class ==
            RandomConstraints::ptw_error_leaf) {
            ptw_error_target_level = 0;
        } else if (ptw_error_level_class ==
            RandomConstraints::ptw_error_root) {
            ptw_error_target_level = levels - 1;
        } else {
            ptw_error_target_level = 1U + random() % (levels - 2U);
        }
        constexpr std::uint64_t ptw_error_lines =
            ptw_error_span / 64;
        const std::uint64_t sequence = ptw_error_line++;
        const std::uint64_t line =
            (sequence ^ (sequence >> 1)) % ptw_error_lines;
        ptw_error_address = ptw_error_base + line * 64 + 0x18;
        ptw_error_guest_address =
            ptw_error_guest_base + line * 64 + 0x18;
        const std::uint64_t root_offset =
            (sequence % ptw_error_root_slots) * ptw_error_root_stride;
        ptw_error_stage1_root =
            ptw_error_stage1_root_base + root_offset;
        ptw_error_vs_root = ptw_error_vs_root_base + root_offset;
        ptw_error_g_root = ptw_error_g_root_base + root_offset;
    }
}

} // namespace memblock::scenarios::mixed
