#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

void MixedScenario::prepare_scalar_policy(ActionWindow &window)
{
    auto &closing_stride_stream = window.closing_stride_stream;
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
    auto &vector_segment_store = window.vector_segment_store;
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
        random() % 1000 < constraints.nc_stores_per_mille;
    if (kind == RandomConstraints::noncacheable) {
        if (constraints.nc_stores_per_mille != 1000 &&
            constraint_coverage.nc_directions[0] == 0) {
            nc_store = false;
        } else if (constraints.nc_stores_per_mille != 0 &&
            constraint_coverage.nc_directions[1] == 0) {
            nc_store = true;
        }
    }
    mmio_store = kind == RandomConstraints::mmio &&
        random() % 1000 < constraints.mmio_stores_per_mille;
    if (kind == RandomConstraints::mmio) {
        if (constraints.mmio_stores_per_mille != 1000 &&
            constraint_coverage.mmio_directions[0] == 0) {
            mmio_store = false;
        } else if (constraints.mmio_stores_per_mille != 0 &&
            constraint_coverage.mmio_directions[1] == 0) {
            mmio_store = true;
        }
    }
    dcache_load_error_denied = {};
    if (kind == RandomConstraints::scalar_load &&
        !closing_stride_stream) {
        for (unsigned outcome = 0; outcome < 3; ++outcome) {
            const bool outcome_enabled = outcome == 0
                ? constraints.dcache_load_error_per_mille != 1000
                : outcome == 1
                    ? constraints.dcache_load_error_per_mille != 0 &&
                        constraints.dcache_load_error_denied_per_mille !=
                            1000
                    : constraints.dcache_load_error_per_mille != 0 &&
                        constraints.dcache_load_error_denied_per_mille != 0;
            if (outcome_enabled &&
                constraint_coverage.dcache_load_outcomes[outcome] == 0) {
                if (outcome != 0) {
                    dcache_load_error_denied = outcome == 2;
                }
                break;
            }
        }
        const bool missing_forced_outcome =
            (!dcache_load_error_denied &&
                constraint_coverage.dcache_load_outcomes[0] == 0 &&
                constraints.dcache_load_error_per_mille != 1000) ||
            dcache_load_error_denied.has_value();
        if (!missing_forced_outcome) {
            const bool error = choose_binary_class(
                constraints.dcache_load_error_per_mille,
                constraint_coverage.dcache_load_errors);
            if (error) {
                dcache_load_error_denied = choose_binary_class(
                    constraints.dcache_load_error_denied_per_mille,
                    constraint_coverage.dcache_load_error_kinds);
            }
        }
    }
    bank_conflict_action = false;
    bank_conflict_depth_index = random() % 2U;
    bank_conflict_bank = random() % 8U;
    bank_conflict_forced_regime = {};
    if (kind == RandomConstraints::scalar_load &&
        !closing_stride_stream && !dcache_load_error_denied &&
        constraints.bank_conflict_per_mille != 0) {
        for (unsigned depth = 0;
             depth < constraint_coverage.bank_conflict_crosses.size() &&
             !bank_conflict_forced_regime;
             ++depth) {
            for (unsigned bank = 0;
                 bank < constraint_coverage
                            .bank_conflict_crosses[depth].size() &&
                 !bank_conflict_forced_regime;
                 ++bank) {
                for (unsigned regime = 0;
                     regime < RandomConstraints::translation_regime_count;
                     ++regime) {
                    if (constraints.translation_weights[regime] != 0 &&
                        constraint_coverage.bank_conflict_crosses
                                [depth][bank][regime] == 0) {
                        bank_conflict_action = true;
                        bank_conflict_depth_index = depth;
                        bank_conflict_bank = bank;
                        bank_conflict_forced_regime = regime;
                        break;
                    }
                }
            }
        }
        if (!bank_conflict_action) {
            bank_conflict_action = random() % 1000U <
                constraints.bank_conflict_per_mille;
        }
    }
    atomic_family = 0;
    atomic_width = 0;
    atomic_probe_depth = 0;
    atomic_error_denied = {};
    bool atomic_outcome_forced = false;
    if (kind == RandomConstraints::atomic) {
        atomic_family = constraints.choose_atomic_family(random());
        atomic_width = constraints.choose_atomic_width(random());
        if (constraints.atomic_error_per_mille != 1000) {
            atomic_probe_depth =
                constraints.choose_atomic_probe_depth(random());
        }
        for (unsigned family = 0;
             family < RandomConstraints::atomic_family_count &&
             !atomic_outcome_forced;
             ++family) {
            if (constraints.atomic_family_weights[family] == 0) {
                continue;
            }
            for (unsigned width = 0;
                 width < 2 && !atomic_outcome_forced; ++width) {
                if (constraints.atomic_width_weights[width] == 0) {
                    continue;
                }
                for (unsigned outcome = 0; outcome < 3; ++outcome) {
                    const bool enabled = outcome == 0
                        ? constraints.atomic_error_per_mille != 1000
                        : outcome == 1
                            ? constraints.atomic_error_per_mille != 0 &&
                                constraints.
                                    atomic_error_denied_per_mille != 1000
                            : constraints.atomic_error_per_mille != 0 &&
                                constraints.
                                    atomic_error_denied_per_mille != 0;
                    if (enabled &&
                        constraint_coverage.atomic_outcomes[
                            family][width][outcome] == 0) {
                        atomic_family = family;
                        atomic_width = width;
                        atomic_outcome_forced = true;
                        if (outcome != 0) {
                            atomic_error_denied = outcome == 2;
                        }
                        break;
                    }
                }
            }
        }
        const std::optional<unsigned> missing_atomic_probe_cross =
            constraint_coverage.first_missing_atomic_probe_cross(
                constraints);
        if (missing_atomic_probe_cross) {
            constexpr unsigned depth_count =
                RandomConstraints::atomic_probe_depth_count;
            constexpr unsigned family_stride = 2U * depth_count;
            atomic_family =
                *missing_atomic_probe_cross / family_stride;
            const unsigned remainder =
                *missing_atomic_probe_cross % family_stride;
            atomic_width = remainder / depth_count;
            atomic_probe_depth = remainder % depth_count;
            atomic_error_denied.reset();
            atomic_outcome_forced = true;
        }
        if (!atomic_outcome_forced && choose_binary_class(
                                          constraints.atomic_error_per_mille,
                                          constraint_coverage.atomic_errors)) {
            atomic_error_denied = choose_binary_class(
                constraints.atomic_error_denied_per_mille,
                constraint_coverage.atomic_error_kinds);
        }
    }
    uncache_error_denied = {};
    bool uncache_outcome_forced = false;
    if (kind == RandomConstraints::noncacheable ||
        kind == RandomConstraints::mmio) {
        const unsigned memory_type =
            kind == RandomConstraints::mmio ? 1U : 0U;
        bool &store = kind == RandomConstraints::mmio
            ? mmio_store : nc_store;
        const unsigned store_per_mille =
            kind == RandomConstraints::mmio
            ? constraints.mmio_stores_per_mille
            : constraints.nc_stores_per_mille;
        for (unsigned direction = 0;
             direction < 2 && !uncache_outcome_forced;
             ++direction) {
            const bool direction_enabled = direction == 0
                ? store_per_mille != 1000 : store_per_mille != 0;
            if (!direction_enabled) {
                continue;
            }
            for (unsigned outcome = 0; outcome < 3; ++outcome) {
                const bool outcome_enabled =
                    outcome == 0
                    ? constraints.uncache_error_per_mille != 1000
                    : outcome == 1
                        ? constraints.uncache_error_per_mille != 0 &&
                            direction == 0 &&
                            constraints.
                                uncache_load_error_denied_per_mille != 1000
                        : constraints.uncache_error_per_mille != 0 &&
                            (direction == 1 ||
                                constraints.
                                    uncache_load_error_denied_per_mille != 0);
                if (outcome_enabled &&
                    constraint_coverage.uncache_outcomes[
                        memory_type][direction][outcome] == 0) {
                    store = direction == 1;
                    uncache_outcome_forced = true;
                    if (outcome != 0) {
                        uncache_error_denied = outcome == 2;
                    }
                    break;
                }
            }
        }
        if (!uncache_outcome_forced) {
            const bool error = choose_binary_class(
                constraints.uncache_error_per_mille,
                constraint_coverage.uncache_errors);
            if (error) {
                uncache_error_denied = store ? true :
                    choose_binary_class(
                        constraints.
                        uncache_load_error_denied_per_mille,
                        constraint_coverage.uncache_error_kinds);
            }
        }
    }
    vector_segment_store = kind == RandomConstraints::vector_segment &&
        random() % 1000 <
            constraints.vector_segment_stores_per_mille;
}

} // namespace memblock::scenarios::mixed
