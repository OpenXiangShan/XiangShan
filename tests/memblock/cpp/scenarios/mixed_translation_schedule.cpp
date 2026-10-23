#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

unsigned int MixedScenario::first_enabled_mode(const std::array<unsigned, 2> &weights)
{
    return weights[0] != 0 ? 0U : 1U;
}

TranslationContext MixedScenario::random_translation_context()
{
    TranslationContext context;
    context.regime = constraints.choose_translation_regime(random());
    if (context.regime == RandomConstraints::translation_stage1) {
        context.stage1_mode = constraints.choose_stage1_mode(random());
    } else if (context.regime == RandomConstraints::translation_nested) {
        context.vs_mode = constraints.choose_vs_mode(random());
        context.g_mode = constraints.choose_g_mode(random());
    }
    return context;
}

TranslationContext MixedScenario::dominant_translation_context()
{
    const auto dominant_index = [](const auto &weights) {
        return static_cast<unsigned>(std::distance(
            weights.begin(),
            std::max_element(weights.begin(), weights.end())));
    };
    TranslationContext context;
    context.regime = dominant_index(constraints.translation_weights);
    if (context.regime == RandomConstraints::translation_stage1) {
        context.stage1_mode = dominant_index(
            constraints.stage1_mode_weights);
    } else if (context.regime == RandomConstraints::translation_nested) {
        context.vs_mode = dominant_index(constraints.vs_mode_weights);
        context.g_mode = dominant_index(constraints.g_mode_weights);
    }
    return context;
}

TranslationContext MixedScenario::translated_context()
{
    TranslationContext context;
    const std::uint64_t stage1_weight = constraints.translation_weights[
        RandomConstraints::translation_stage1];
    const std::uint64_t nested_weight = constraints.translation_weights[
        RandomConstraints::translation_nested];
    if (random() % (stage1_weight + nested_weight) < stage1_weight) {
        context.regime = RandomConstraints::translation_stage1;
        context.stage1_mode = constraints.choose_stage1_mode(random());
    } else {
        context.regime = RandomConstraints::translation_nested;
        context.vs_mode = constraints.choose_vs_mode(random());
        context.g_mode = constraints.choose_g_mode(random());
    }
    return context;
}

bool MixedScenario::binary_class_enabled(unsigned true_per_mille,
    bool value)
{
    return value ? true_per_mille != 0 : true_per_mille != 1000;
}

TranslationLeafTopology MixedScenario::random_translation_leaf(const TranslationContext &context)
{
    TranslationLeafTopology leaf;
    if (context.regime == RandomConstraints::translation_stage1) {
        leaf.stage1_napot =
            constraints.choose_stage1_napot(random());
    } else if (context.regime == RandomConstraints::translation_nested) {
        leaf.vs_napot =
            constraints.choose_nested_vs_napot(random());
        leaf.g_napot = constraints.choose_nested_g_napot(random());
    }
    return leaf;
}

std::optional<TranslationLeafTopology> MixedScenario::required_translation_leaf(
    const TranslationContext &context)
{
    if (context.regime == RandomConstraints::translation_stage1) {
        for (unsigned napot = 0; napot < 2; ++napot) {
            if (binary_class_enabled(
                    constraints.stage1_napot_per_mille, napot != 0) &&
                constraint_coverage.stage1_leaf_types[napot] == 0) {
                TranslationLeafTopology leaf;
                leaf.stage1_napot = napot != 0;
                return leaf;
            }
        }
    } else if (context.regime == RandomConstraints::translation_nested) {
        for (unsigned vs_napot = 0; vs_napot < 2; ++vs_napot) {
            for (unsigned g_napot = 0; g_napot < 2; ++g_napot) {
                const unsigned topology = vs_napot * 2 + g_napot;
                if (binary_class_enabled(
                        constraints.nested_vs_napot_per_mille,
                        vs_napot != 0) &&
                    binary_class_enabled(
                        constraints.nested_g_napot_per_mille,
                        g_napot != 0) &&
                    constraint_coverage.nested_leaf_topologies[
                        topology] == 0) {
                    TranslationLeafTopology leaf;
                    leaf.vs_napot = vs_napot != 0;
                    leaf.g_napot = g_napot != 0;
                    return leaf;
                }
            }
        }
    }
    return std::nullopt;
}

memblock::scenarios::TranslationLeafTopology MixedScenario::choose_translation_leaf(const TranslationContext &context)
{
    if (const auto required = required_translation_leaf(context)) {
        return *required;
    }
    return random_translation_leaf(context);
}

std::optional<TranslationContext> MixedScenario::required_translation_context()
{
    if (constraints.translation_weights[
            RandomConstraints::translation_bare] != 0 &&
        constraint_coverage.translation_regimes[
            RandomConstraints::translation_bare] == 0) {
        return TranslationContext{};
    }
    if (constraints.translation_weights[
            RandomConstraints::translation_stage1] != 0) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (constraints.stage1_mode_weights[mode] != 0 &&
                constraint_coverage.stage1_modes[mode] == 0) {
                TranslationContext context;
                context.regime = RandomConstraints::translation_stage1;
                context.stage1_mode = mode;
                return context;
            }
        }
    }
    if (constraints.translation_weights[
            RandomConstraints::translation_nested] != 0) {
        for (unsigned vs_mode = 0; vs_mode < 2; ++vs_mode) {
            for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
                if (constraints.vs_mode_weights[vs_mode] != 0 &&
                    constraints.g_mode_weights[g_mode] != 0 &&
                    constraint_coverage.nested_mode_pairs[
                        vs_mode * 2 + g_mode] == 0) {
                    TranslationContext context;
                    context.regime =
                        RandomConstraints::translation_nested;
                    context.vs_mode = vs_mode;
                    context.g_mode = g_mode;
                    return context;
                }
            }
        }
    }
    if (constraints.translation_weights[
            RandomConstraints::translation_stage1] != 0) {
        TranslationContext context;
        context.regime = RandomConstraints::translation_stage1;
        context.stage1_mode = first_enabled_mode(
            constraints.stage1_mode_weights);
        if (required_translation_leaf(context)) {
            return context;
        }
    }
    if (constraints.translation_weights[
            RandomConstraints::translation_nested] != 0) {
        TranslationContext context;
        context.regime = RandomConstraints::translation_nested;
        context.vs_mode = first_enabled_mode(constraints.vs_mode_weights);
        context.g_mode = first_enabled_mode(constraints.g_mode_weights);
        if (required_translation_leaf(context)) {
            return context;
        }
    }
    if (constraints.uses_translation() &&
        constraint_coverage.translation_walk_windows != 0 &&
        constraint_coverage.translation_reuse_windows == 0 &&
        translation_context_valid &&
        current_translation.regime !=
            RandomConstraints::translation_bare) {
        return current_translation;
    }
    if (constraints.tlb_flushes_per_mille == 0) {
        return std::nullopt;
    }
    for (unsigned kind = 0;
         kind < RandomConstraints::fence_kind_count; ++kind) {
        const bool compatible = kind == RandomConstraints::fence_sfence
            ? constraints.translation_weights[
                  RandomConstraints::translation_stage1] != 0
            : constraints.translation_weights[
                  RandomConstraints::translation_nested] != 0;
        if (!compatible || constraints.fence_kind_weights[kind] == 0) {
            continue;
        }
        for (unsigned scope = 0; scope < 2; ++scope) {
            if (constraints.fence_scope_weights[scope] != 0 &&
                constraint_coverage.fences[kind][scope] == 0) {
                TranslationContext context;
                if (kind == RandomConstraints::fence_sfence) {
                    context.regime =
                        RandomConstraints::translation_stage1;
                    context.stage1_mode = first_enabled_mode(
                        constraints.stage1_mode_weights);
                } else {
                    context.regime =
                        RandomConstraints::translation_nested;
                    context.vs_mode = first_enabled_mode(
                        constraints.vs_mode_weights);
                    context.g_mode = first_enabled_mode(
                        constraints.g_mode_weights);
                }
                return context;
            }
        }
    }
    return std::nullopt;
}

memblock::scenarios::TranslationContext MixedScenario::choose_translation_context()
{
    if (const auto required = required_translation_context()) {
        return *required;
    }
    if (!translation_coverage_closed) {
        translation_coverage_closed = true;
        return dominant_translation_context();
    }
    if (translation_context_valid &&
        random() % 1000 >=
            constraints.translation_switches_per_mille) {
        return current_translation;
    }
    return random_translation_context();
}

bool MixedScenario::flush_pending_translation_fences()
{
    const bool fenced =
        ((pending_translation_fences & pending_sfence) == 0 ||
            environment.issue_sfence(
                0, 0, true, true, false, false)) &&
        ((pending_translation_fences & pending_hfence_vvma) == 0 ||
            environment.issue_sfence(
                0, 0, true, true, true, false)) &&
        ((pending_translation_fences & pending_hfence_gvma) == 0 ||
            environment.issue_sfence(
                0, 0, true, true, false, true));
    if (fenced) {
        pending_translation_fences = 0;
    }
    return fenced;
}

bool MixedScenario::enter_translation_context(const TranslationContext &context)
{
    if (translation_context_valid &&
        context == current_translation &&
        pending_translation_fences == 0) {
        return true;
    }
    if (!environment.run_until_all_complete(4096) ||
        !environment.run_until_queues_retired(4096) ||
        !flush_pending_translation_fences() ||
        !activate_translation_context(context)) {
        return false;
    }
    if (translation_context_valid) {
        ++constraint_coverage.translation_switches;
    }
    current_translation = context;
    translation_context_valid = true;
    return true;
}

bool MixedScenario::issue_constrained_fence(unsigned operation_kind)
{
    if (current_translation.regime ==
            RandomConstraints::translation_bare ||
        constraints.tlb_flushes_per_mille == 0 ||
        !constraints.has_compatible_fence_kind(
            current_translation.regime)) {
        return true;
    }
    std::optional<std::pair<unsigned, unsigned>> required;
    for (unsigned kind = 0;
         kind < RandomConstraints::fence_kind_count && !required;
         ++kind) {
        const bool compatible =
            (current_translation.regime ==
                    RandomConstraints::translation_stage1 &&
                kind == RandomConstraints::fence_sfence) ||
            (current_translation.regime ==
                    RandomConstraints::translation_nested &&
                kind != RandomConstraints::fence_sfence);
        if (!compatible ||
            constraints.fence_kind_weights[kind] == 0) {
            continue;
        }
        for (unsigned scope = 0; scope < 2; ++scope) {
            if (constraints.fence_scope_weights[scope] != 0 &&
                constraint_coverage.fences[kind][scope] == 0) {
                required = std::make_pair(kind, scope);
                break;
            }
        }
    }
    if (!required &&
        random() % 1000 >= constraints.tlb_flushes_per_mille) {
        return true;
    }
    const unsigned kind = required
        ? required->first
        : constraints.choose_fence_kind(
              current_translation.regime, random());
    const unsigned scope = required
        ? required->second
        : constraints.choose_fence_scope(random());
    const bool global = scope == 0;
    std::uint64_t virtual_address = cache0_base + 0x10000;
    if (operation_kind == RandomConstraints::noncacheable) {
        virtual_address = nc_base;
    } else if (operation_kind == RandomConstraints::mmio) {
        virtual_address = mmio_virtual;
    } else if (operation_kind == RandomConstraints::load_merge) {
        virtual_address = load_merge_base;
    } else if (operation_kind == RandomConstraints::set_pressure) {
        virtual_address = set_pressure_base;
    } else if (operation_kind == RandomConstraints::miss_burst) {
        virtual_address = miss_burst_base;
    }
    std::uint64_t fence_address = global ? 0 : virtual_address;
    std::uint16_t id = 0;
    bool hypervisor_virtual = false;
    bool hypervisor_guest = false;
    if (kind == RandomConstraints::fence_sfence) {
        id = 23;
    } else if (kind == RandomConstraints::fence_hfence_vvma) {
        id = 29;
        hypervisor_virtual = true;
    } else {
        id = 31;
        hypervisor_guest = true;
        const std::uint64_t guest_physical_address =
            operation_kind == RandomConstraints::mmio
            ? mmio_physical : virtual_address;
        fence_address = global ? 0 : guest_physical_address >> 2;
    }
    if (!environment.issue_sfence(
            fence_address, id, global, global,
            hypervisor_virtual, hypervisor_guest)) {
        return false;
    }
    ++constraint_coverage.fences[kind][scope];
    ++constraint_coverage.tlb_flushes;
    return true;
}

std::uint64_t MixedScenario::mmio_access_address()
{
    return current_translation.regime ==
            RandomConstraints::translation_bare
        ? mmio_physical : mmio_virtual;
}

} // namespace memblock::scenarios::mixed
