#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

void MixedScenario::prepare_scalar_policy(ActionWindow &window)
{
    auto &kind = window.kind;
    auto &nc_store = window.nc_store;
    auto &mmio_store = window.mmio_store;
    auto &dcache_load_error_denied = window.dcache_load_error_denied;
    auto &bank_conflict_action = window.bank_conflict_action;
    auto &bank_conflict_depth_index = window.bank_conflict_depth_index;
    auto &bank_conflict_bank = window.bank_conflict_bank;
    auto &bank_conflict_forced_regime = window.bank_conflict_forced_regime;
    auto &atomic_family = window.atomic_family;
    auto &atomic_width = window.atomic_width;
    auto &atomic_probe_depth = window.atomic_probe_depth;
    auto &atomic_error_denied = window.atomic_error_denied;
    auto &uncache_error_denied = window.uncache_error_denied;
    constexpr std::array<const char *,
        RandomConstraints::operation_count>
        operation_names{{
            "scalar-load", "scalar-store", "vector-load",
            "vector-store", "vector-segment", "prefetch", "atomic",
            "nc", "mmio", "hypervisor", "cmo", "ptw-error",
            "load-merge", "set-pressure", "miss-burst",
        }};
    {
        std::ostringstream detail;
        detail << "random-action:" << operation_names[kind]
               << ":index=" << actions;
        phase = detail.str();
    }
    nc_store = kind == RandomConstraints::noncacheable &&
        choose_binary_class(
            constraints.nc_stores_per_mille,
            constraint_coverage.nc_directions);
    mmio_store = kind == RandomConstraints::mmio &&
        choose_binary_class(
            constraints.mmio_stores_per_mille,
            constraint_coverage.mmio_directions);
    dcache_load_error_denied = {};
    if (kind == RandomConstraints::scalar_load) {
        if (choose_binary_class(
                constraints.dcache_load_error_per_mille,
                constraint_coverage.dcache_load_errors)) {
            dcache_load_error_denied = choose_binary_class(
                constraints.dcache_load_error_denied_per_mille,
                constraint_coverage.dcache_load_error_kinds);
        }
    }
    bank_conflict_action = kind == RandomConstraints::scalar_load &&
        !dcache_load_error_denied &&
        random() % 1000U < constraints.bank_conflict_per_mille;
    bank_conflict_depth_index = random() % 2U;
    bank_conflict_bank = random() % 8U;
    bank_conflict_forced_regime = {};
    atomic_family = 0;
    atomic_width = 0;
    atomic_probe_depth = 0;
    atomic_error_denied = {};
    if (kind == RandomConstraints::atomic) {
        atomic_family = constraints.choose_atomic_family(random());
        atomic_width = constraints.choose_atomic_width(random());
        if (constraints.atomic_error_per_mille != 1000) {
            atomic_probe_depth =
                constraints.choose_atomic_probe_depth(random());
        }
        if (choose_binary_class(
                constraints.atomic_error_per_mille,
                constraint_coverage.atomic_errors)) {
            atomic_error_denied = choose_binary_class(
                constraints.atomic_error_denied_per_mille,
                constraint_coverage.atomic_error_kinds);
        }
    }
    uncache_error_denied = {};
    if (kind == RandomConstraints::noncacheable ||
        kind == RandomConstraints::mmio) {
        const bool store = kind == RandomConstraints::mmio
            ? mmio_store : nc_store;
        if (choose_binary_class(
                constraints.uncache_error_per_mille,
                constraint_coverage.uncache_errors)) {
            uncache_error_denied = store ? true :
                choose_binary_class(
                    constraints.uncache_load_error_denied_per_mille,
                    constraint_coverage.uncache_error_kinds);
        }
    }
}

} // namespace memblock::scenarios::mixed
