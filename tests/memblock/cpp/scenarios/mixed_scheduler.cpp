#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

void MixedScenario::snapshot_action(ActionWindow &window)
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
    ptw_requests_before =
        environment.ptw_requests();
    requests_before = environment.tilelink_requests();
    sample_dcache = true;
    sample_translation_action =
        kind != RandomConstraints::ptw_error;
    leaf_addressed = false;
    ordinary_leaf_addressed = false;
    probe_candidate = {};
}

bool MixedScenario::execute_action(ActionWindow &window)
{
    auto &kind = window.kind;
    if (kind == RandomConstraints::scalar_load) {
        return execute_scalar_load(window);
    } else if (kind == RandomConstraints::load_merge) {
        return execute_load_merge(window);
    } else if (kind == RandomConstraints::set_pressure) {
        return execute_set_pressure(window);
    } else if (kind == RandomConstraints::miss_burst) {
        return execute_miss_burst(window);
    } else if (kind == RandomConstraints::scalar_store) {
        return execute_scalar_store(window);
    } else if (kind == RandomConstraints::ptw_error) {
        return execute_ptw_error(window);
    } else if (kind == RandomConstraints::vector_load ||
        kind == RandomConstraints::vector_store) {
        return execute_vector_memory(window);
    } else if (kind == RandomConstraints::vector_segment) {
        return execute_vector_segment(window);
    } else if (kind == RandomConstraints::prefetch) {
        return execute_prefetch(window);
    } else if (kind == RandomConstraints::atomic) {
        return execute_atomic(window);
    } else if (kind == RandomConstraints::hypervisor) {
        return execute_hypervisor(window);
    } else if (kind == RandomConstraints::cmo) {
        return execute_cmo(window);
    } else if (kind == RandomConstraints::noncacheable) {
        return execute_noncacheable(window);
    } else {
        return execute_mmio(window);
    }
}

bool MixedScenario::run_serial_action()
{
    ActionWindow window;
    const ActionDisposition selected = select_action(window);
    if (selected == ActionDisposition::failed) return false;
    if (selected == ActionDisposition::boundary_completed) return true;
    prepare_scalar_policy(window);
    prepare_vector_policy(window);
    prepare_special_policy(window);
    if (!prepare_action_translation(window)) return false;
    snapshot_action(window);
    if (!execute_action(window)) return false;
    return observe_action(window);
}

bool MixedScenario::execute()
{
    if (!run_prefix()) return false;
    phase = "random-translation-map";
    if (!prepare_random_translation_contexts()) return false;
    if (!prepare_tail()) return false;
    if (!run_concurrent_windows()) return false;
    while (actions < target_before_redirect) {
        if (!run_serial_action()) return false;
    }
    return finish();
}

} // namespace memblock::scenarios::mixed
