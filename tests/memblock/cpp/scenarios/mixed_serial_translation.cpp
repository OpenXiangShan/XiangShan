#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::prepare_action_translation(ActionWindow &window)
{
    auto &closing_stride_stream = window.closing_stride_stream;
    auto &kind = window.kind;
    auto &bank_conflict_action = window.bank_conflict_action;
    auto &bank_conflict_forced_regime = window.bank_conflict_forced_regime;
    auto &vector_segment_fof = window.vector_segment_fof;
    auto &vector_segment_fof_stage1_mode = window.vector_segment_fof_stage1_mode;
    auto &vector_segment_high_va_cross = window.vector_segment_high_va_cross;
    auto &vector_fof = window.vector_fof;
    auto &vector_fof_stage1_mode = window.vector_fof_stage1_mode;
    auto &ptw_error_site = window.ptw_error_site;
    auto &ptw_error_stage1_mode = window.ptw_error_stage1_mode;
    auto &ptw_error_vs_mode = window.ptw_error_vs_mode;
    auto &ptw_error_g_mode = window.ptw_error_g_mode;
    auto &ptw_error_address = window.ptw_error_address;
    auto &ptw_error_guest_address = window.ptw_error_guest_address;
    auto &ptw_error_stage1_root = window.ptw_error_stage1_root;
    auto &ptw_error_vs_root = window.ptw_error_vs_root;
    auto &ptw_error_g_root = window.ptw_error_g_root;
    auto &set_pressure_depth_index = window.set_pressure_depth_index;
    auto &set_pressure_width = window.set_pressure_width;
    auto &set_pressure_dirty = window.set_pressure_dirty;
    auto &set_pressure_refill_overlap = window.set_pressure_refill_overlap;
    auto &set_pressure_release_backpressure = window.set_pressure_release_backpressure;
    auto &set_pressure_window_class = window.set_pressure_window_class;
    auto &miss_burst_depth_index = window.miss_burst_depth_index;
    auto &miss_burst_issue_width_index = window.miss_burst_issue_width_index;
    auto &miss_burst_composition = window.miss_burst_composition;
    auto &translation = window.translation;
    auto &leaf = window.leaf;
    translation = closing_stride_stream
        ? dominant_translation_context()
        : choose_translation_context();
    if (kind == RandomConstraints::scalar_load &&
        bank_conflict_action && bank_conflict_forced_regime) {
        translation = TranslationContext{};
        translation.regime = *bank_conflict_forced_regime;
        if (translation.regime ==
            RandomConstraints::translation_stage1) {
            translation.stage1_mode =
                constraints.choose_stage1_mode(random());
        } else if (translation.regime ==
            RandomConstraints::translation_nested) {
            translation.vs_mode =
                constraints.choose_vs_mode(random());
            translation.g_mode =
                constraints.choose_g_mode(random());
        }
    } else if (kind == RandomConstraints::vector_load && vector_fof) {
        translation = TranslationContext{};
        translation.regime = RandomConstraints::translation_stage1;
        translation.stage1_mode = vector_fof_stage1_mode;
    } else if (vector_segment_high_va_cross) {
        translation = TranslationContext{};
        if (RandomConstraints::vector_segment_high_va_cross_is_nested(
                *vector_segment_high_va_cross)) {
            const unsigned pair =
                RandomConstraints::vector_segment_high_va_nested_pair(
                    *vector_segment_high_va_cross);
            translation.regime = RandomConstraints::translation_nested;
            translation.vs_mode = pair / 2U;
            translation.g_mode = pair % 2U;
        } else {
            translation.regime = RandomConstraints::translation_stage1;
            translation.stage1_mode =
                *vector_segment_high_va_cross / 4U;
        }
    } else if (kind == RandomConstraints::vector_segment &&
        vector_segment_fof) {
        translation = TranslationContext{};
        translation.regime = RandomConstraints::translation_stage1;
        translation.stage1_mode = vector_segment_fof_stage1_mode;
    } else if (kind == RandomConstraints::hypervisor) {
        translation.regime = RandomConstraints::translation_nested;
        translation.vs_mode = constraints.choose_vs_mode(random());
        translation.g_mode = constraints.choose_g_mode(random());
    } else if ((kind == RandomConstraints::noncacheable ||
                   kind == RandomConstraints::mmio) &&
        translation.regime == RandomConstraints::translation_bare) {
        translation = translated_context();
    } else if (kind == RandomConstraints::ptw_error) {
        if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
            translation.regime = RandomConstraints::translation_stage1;
            translation.stage1_mode = ptw_error_stage1_mode;
        } else if (ptw_error_site !=
            RandomConstraints::ptw_error_gstage) {
            translation.regime = RandomConstraints::translation_nested;
            translation.vs_mode = ptw_error_vs_mode;
            translation.g_mode = ptw_error_g_mode;
        }
    } else if (kind == RandomConstraints::load_merge) {
        for (unsigned regime = 0;
             regime < RandomConstraints::translation_regime_count;
             ++regime) {
            if (constraints.translation_weights[regime] == 0 ||
                constraint_coverage.load_merge_translations[regime] !=
                    0) {
                continue;
            }
            translation = TranslationContext{};
            translation.regime = regime;
            if (regime == RandomConstraints::translation_stage1) {
                translation.stage1_mode = first_enabled_mode(
                    constraints.stage1_mode_weights);
            } else if (regime ==
                RandomConstraints::translation_nested) {
                translation.vs_mode = first_enabled_mode(
                    constraints.vs_mode_weights);
                translation.g_mode = first_enabled_mode(
                    constraints.g_mode_weights);
            }
            break;
        }
    } else if (kind == RandomConstraints::set_pressure) {
        bool forced_cross = false;
        for (unsigned state = 0;
             state < constraint_coverage.set_pressure_crosses.size() &&
             !forced_cross;
             ++state) {
            const bool state_enabled = state == 0
                ? constraints.set_pressure_dirty_per_mille != 1000
                : constraints.set_pressure_dirty_per_mille != 0;
            if (!state_enabled) {
                continue;
            }
            for (unsigned overlap = 0;
                 overlap < constraint_coverage
                               .set_pressure_crosses[state].size() &&
                 !forced_cross;
                 ++overlap) {
                const bool overlap_enabled = overlap == 0
                    ? constraints
                            .set_pressure_refill_overlap_per_mille !=
                        1000
                    : constraints
                            .set_pressure_refill_overlap_per_mille != 0;
                if (!overlap_enabled) {
                    continue;
                }
                for (unsigned backpressure = 0;
                     backpressure < constraint_coverage
                                        .set_pressure_crosses[state]
                                                             [overlap]
                                        .size() &&
                     !forced_cross;
                     ++backpressure) {
                    const bool backpressure_enabled = backpressure == 0
                        ? constraints
                                .set_pressure_release_backpressure_per_mille !=
                            1000
                        : constraints
                                .set_pressure_release_backpressure_per_mille !=
                            0;
                    if (!backpressure_enabled) {
                        continue;
                    }
                    for (unsigned window_class = 0;
                         window_class < constraint_coverage
                                            .set_pressure_crosses[state]
                                                                 [overlap]
                                                                 [backpressure]
                                            .size() &&
                         !forced_cross;
                         ++window_class) {
                        if (!constraints.set_pressure_window_enabled(
                                window_class)) {
                            continue;
                        }
                        for (unsigned depth = 0;
                             depth < constraint_coverage
                                         .set_pressure_crosses[state]
                                                              [overlap]
                                                              [backpressure]
                                                              [window_class]
                                         .size() &&
                             !forced_cross;
                             ++depth) {
                            if (constraints.set_pressure_depth_weights
                                    [depth] == 0) {
                                continue;
                            }
                            for (unsigned width = 0;
                                 width < constraint_coverage
                                             .set_pressure_crosses[state]
                                                                  [overlap]
                                                                  [backpressure]
                                                                  [window_class]
                                                                  [depth]
                                             .size() &&
                                 !forced_cross;
                                 ++width) {
                                if (constraints
                                        .set_pressure_width_weights
                                            [width] == 0) {
                                    continue;
                                }
                                for (unsigned regime = 0;
                                     regime < RandomConstraints::
                                     translation_regime_count;
                                     ++regime) {
                                    if (constraints.translation_weights
                                                [regime] != 0 &&
                                        constraint_coverage
                                                .set_pressure_crosses
                                                    [state][overlap]
                                                    [backpressure]
                                                    [window_class]
                                                    [depth][width]
                                                    [regime] == 0) {
                                        set_pressure_dirty = state != 0;
                                        set_pressure_refill_overlap =
                                            overlap != 0;
                                        set_pressure_release_backpressure =
                                            backpressure != 0;
                                        set_pressure_window_class =
                                            window_class;
                                        set_pressure_depth_index = depth;
                                        set_pressure_width = width;
                                        translation =
                                            TranslationContext{};
                                        translation.regime = regime;
                                        if (regime == RandomConstraints::
                                            translation_stage1) {
                                            translation.stage1_mode =
                                                first_enabled_mode(
                                                    constraints.
                                                    stage1_mode_weights);
                                        } else if (regime ==
                                            RandomConstraints::
                                            translation_nested) {
                                            translation.vs_mode =
                                                first_enabled_mode(
                                                    constraints.
                                                    vs_mode_weights);
                                            translation.g_mode =
                                                first_enabled_mode(
                                                    constraints.
                                                    g_mode_weights);
                                        }
                                        forced_cross = true;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    } else if (kind == RandomConstraints::miss_burst) {
        bool forced_cross = false;
        for (unsigned depth = 0;
             depth < constraint_coverage.miss_burst_crosses.size() &&
             !forced_cross;
             ++depth) {
            for (unsigned width = 0;
                 width < constraint_coverage
                             .miss_burst_crosses[depth].size() &&
                 !forced_cross;
                 ++width) {
                for (unsigned composition = 0;
                     composition < constraint_coverage
                                       .miss_burst_crosses[depth][width]
                                       .size() &&
                     !forced_cross;
                     ++composition) {
                    if (!constraints.miss_burst_cross_enabled(
                            depth, width, composition)) {
                        continue;
                    }
                    for (unsigned regime = 0;
                         regime <
                         RandomConstraints::translation_regime_count;
                         ++regime) {
                        if (constraints.translation_weights[regime] == 0 ||
                            constraint_coverage.miss_burst_crosses
                                    [depth][width][composition][regime] != 0) {
                            continue;
                        }
                        miss_burst_depth_index = depth;
                        miss_burst_issue_width_index = width;
                        miss_burst_composition = composition;
                        translation = TranslationContext{};
                        translation.regime = regime;
                        if (regime ==
                            RandomConstraints::translation_stage1) {
                            translation.stage1_mode = first_enabled_mode(
                                constraints.stage1_mode_weights);
                        } else if (regime ==
                            RandomConstraints::translation_nested) {
                            translation.vs_mode = first_enabled_mode(
                                constraints.vs_mode_weights);
                            translation.g_mode = first_enabled_mode(
                                constraints.g_mode_weights);
                        }
                        forced_cross = true;
                        break;
                    }
                }
            }
        }
    }
    leaf =
        choose_translation_leaf(translation);
    // The conflict address pool is mapped as ordinary 2-MiB leaves
    // in every translated context. Keep this stress action out of
    // the separately located NAPOT pools so its failure mode remains
    // bank/replay behavior rather than an accidental page fault.
    if (bank_conflict_action || vector_fof || vector_segment_fof ||
        vector_segment_high_va_cross) {
        leaf = TranslationLeafTopology{};
    }
    if (kind != RandomConstraints::ptw_error) {
        std::ostringstream detail;
        detail << phase << ":regime=" << translation.regime
               << ":stage1_mode=" << translation.stage1_mode
               << ":vs_mode=" << translation.vs_mode
               << ":g_mode=" << translation.g_mode
               << ":leaf=" << leaf.nested_index();
        phase = detail.str();
    }
    if (kind == RandomConstraints::ptw_error) {
        phase = "random-ptw-error-context";
        if (!environment.run_until_all_complete(
                constrained_completion_timeout) ||
            !environment.run_until_queues_retired(
                constrained_completion_timeout) ||
            !environment.run_until_dcache_idle(
                constrained_completion_timeout) ||
            !flush_pending_translation_fences()) {
            return false;
        }
        const std::uint64_t ptw_error_page =
            ptw_error_address & ~std::uint64_t{0xfff};
        const std::uint64_t ptw_error_guest_page =
            ptw_error_guest_address & ~std::uint64_t{0xfff};
        bool mapped = false;
        if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
            mapped = map_stage_page(
                ptw_error_stage1_mode, ptw_error_stage1_root,
                ptw_error_page, ptw_error_page, false, false, true);
        } else if (ptw_error_site ==
            RandomConstraints::ptw_error_gstage) {
            mapped = map_g_page(
                ptw_error_g_mode, ptw_error_g_root, ptw_error_page,
                ptw_error_page, true);
        } else {
            mapped = map_stage_page(
                ptw_error_vs_mode, ptw_error_vs_root, ptw_error_page,
                ptw_error_guest_page, false, false, true);
            const unsigned vs_levels = ptw_error_vs_mode == 0 ? 3U : 4U;
            for (unsigned level = 0; mapped && level < vs_levels;
                 ++level) {
                const auto vs_pte =
                    memblock::reference_pte_address_at_level(
                        environment.memory(), ptw_error_vs_root,
                        ptw_error_address,
                        page_mode(ptw_error_vs_mode), level);
                mapped = vs_pte && map_g_page(
                                       ptw_error_g_mode, ptw_error_g_root,
                                       *vs_pte & ~std::uint64_t{0xfff},
                                       *vs_pte & ~std::uint64_t{0xfff});
            }
            mapped = mapped && map_g_page(
                                   ptw_error_g_mode, ptw_error_g_root,
                                   ptw_error_guest_page, ptw_error_page, true);
        }
        bool reference_valid = false;
        if (mapped && ptw_error_site ==
                RandomConstraints::ptw_error_stage1) {
            const auto reference = memblock::reference_page_walk(
                environment.memory(), ptw_error_stage1_root,
                ptw_error_address,
                page_mode(ptw_error_stage1_mode));
            reference_valid = reference.translated &&
                reference.physical_address == ptw_error_address;
        } else if (mapped && ptw_error_site ==
                RandomConstraints::ptw_error_gstage) {
            const auto reference = memblock::reference_page_walk(
                environment.memory(), ptw_error_g_root,
                ptw_error_address, page_mode(ptw_error_g_mode), true);
            reference_valid = reference.translated &&
                reference.physical_address == ptw_error_address;
        } else if (mapped) {
            const auto reference =
                memblock::reference_two_stage_walk(
                    environment.memory(), ptw_error_vs_root,
                    ptw_error_g_root, ptw_error_address,
                    page_mode(ptw_error_vs_mode),
                    page_mode(ptw_error_g_mode));
            reference_valid = reference.translated &&
                reference.physical_address == ptw_error_address;
        }
        if (!reference_valid) {
            std::ostringstream detail;
            detail << "random-ptw-error-reference"
                   << ":site=" << ptw_error_site
                   << ":stage1_mode=" << ptw_error_stage1_mode
                   << ":vs_mode=" << ptw_error_vs_mode
                   << ":g_mode=" << ptw_error_g_mode
                   << ":va=0x" << std::hex << ptw_error_address
                   << ":gpa=0x" << ptw_error_guest_address
                   << ":s1root=0x" << ptw_error_stage1_root
                   << ":vsroot=0x" << ptw_error_vs_root
                   << ":groot=0x" << ptw_error_g_root << std::dec;
            if (ptw_error_site !=
                    RandomConstraints::ptw_error_stage1 &&
                ptw_error_site !=
                    RandomConstraints::ptw_error_gstage) {
                const auto reference =
                    memblock::reference_two_stage_walk(
                        environment.memory(), ptw_error_vs_root,
                        ptw_error_g_root, ptw_error_address,
                        page_mode(ptw_error_vs_mode),
                        page_mode(ptw_error_g_mode));
                detail << ":translated=" << reference.translated
                       << ":stage1_pf="
                       << reference.stage1_page_fault
                       << ":gpf=" << reference.guest_page_fault
                       << ":af=" << reference.access_fault
                       << ":fault_gpa=0x" << std::hex
                       << reference.faulting_guest_physical_address
                       << ":pa=0x" << reference.physical_address
                       << std::dec;
            }
            phase = detail.str();
            return false;
        }
        bool activated = false;
        if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
            activated = ptw_error_stage1_mode == 0
                ? environment.activate_sv39(
                      ptw_error_stage1_root, 23)
                : environment.activate_sv48(
                      ptw_error_stage1_root, 23);
        } else if (ptw_error_site ==
            RandomConstraints::ptw_error_gstage) {
            activated = environment.activate_two_stage_modes(
                memblock::ReferencePageMode::bare,
                page_mode(ptw_error_g_mode), 0,
                ptw_error_g_root, 0, 31);
        } else {
            activated = environment.activate_two_stage_modes(
                page_mode(ptw_error_vs_mode),
                page_mode(ptw_error_g_mode),
                ptw_error_vs_root, ptw_error_g_root, 29, 31);
        }
        if (!mapped || !activated ||
            ((ptw_error_site == RandomConstraints::ptw_error_stage1)
                    ? !environment.issue_sfence(
                          0, 0, true, true, false, false)
                    : ptw_error_site == RandomConstraints::ptw_error_gstage
                        ? !environment.issue_sfence(
                              0, 0, true, true, false, true)
                        : (!environment.issue_sfence(
                               0, 0, true, true, true, false) ||
                              !environment.issue_sfence(
                                  0, 0, true, true, false, true)))) {
            return false;
        }
        translation_context_valid = false;
        pending_translation_fences =
            ptw_error_site == RandomConstraints::ptw_error_stage1
            ? pending_sfence
            : ptw_error_site == RandomConstraints::ptw_error_gstage
                ? pending_hfence_gvma
                : pending_hfence_vvma | pending_hfence_gvma;
    } else if (!enter_translation_context(translation) ||
        (!closing_stride_stream &&
            !issue_constrained_fence(kind))) {
        return false;
    }

    return true;
}

} // namespace memblock::scenarios::mixed
