#include "memblock_env.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <iostream>
#include <limits>
#include <numeric>
#include <optional>
#include <random>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace {

struct Options {
    std::string_view test = "single-load";
    std::uint64_t seed = 1;
    unsigned transactions = 200;
    bool backpressure = true;
    bool hunt_boundaries = false;
    bool allow_short_mixed = false;
    std::string constraint_profile = "coverage";
    std::vector<std::string> constraint_overrides;
};

// Keep long stress runs reproducible while avoiding accidental correlation
// between transaction shape, payload bytes, and issue scheduling.  The
// derived seeds are part of the scenario contract: changing one stream does
// not silently perturb all other dimensions.
struct StressRandom {
    static std::uint64_t splitmix64(std::uint64_t value)
    {
        value += 0x9e3779b97f4a7c15ULL;
        value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
        value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
        return value ^ (value >> 31);
    }

    explicit StressRandom(std::uint64_t seed)
        : traffic(splitmix64(seed ^ 0x243f6a8885a308d3ULL)),
          shape(splitmix64(seed ^ 0x13198a2e03707344ULL)),
          payload(splitmix64(seed ^ 0xa4093822299f31d0ULL)),
          scheduler(splitmix64(seed ^ 0x082efa98ec4e6c89ULL))
    {}

    std::uint64_t operator()() { return traffic(); }
    std::uint64_t next_shape() { return shape(); }
    std::uint64_t next_payload() { return payload(); }
    std::uint64_t next_schedule() { return scheduler(); }

    std::mt19937_64 traffic;
    std::mt19937_64 shape;
    std::mt19937_64 payload;
    std::mt19937_64 scheduler;
};

std::uint64_t parse_u64(std::string_view text, const char *option)
{
    std::size_t consumed = 0;
    const std::string copy(text);
    const std::uint64_t value = std::stoull(copy, &consumed, 0);
    if (consumed != copy.size()) {
        throw std::invalid_argument(std::string("invalid value for ") + option);
    }
    return value;
}

#include "random/constraints.inc"

struct TranslationContext {
    unsigned regime = RandomConstraints::translation_bare;
    unsigned stage1_mode = 0;
    unsigned vs_mode = 0;
    unsigned g_mode = 0;

    bool operator==(const TranslationContext &other) const
    {
        return regime == other.regime && stage1_mode == other.stage1_mode &&
            vs_mode == other.vs_mode && g_mode == other.g_mode;
    }

    bool operator!=(const TranslationContext &other) const
    {
        return !(*this == other);
    }
};

struct TranslationLeafTopology {
    bool stage1_napot = false;
    bool vs_napot = false;
    bool g_napot = false;

    unsigned nested_index() const
    {
        return static_cast<unsigned>(vs_napot) * 2U +
            static_cast<unsigned>(g_napot);
    }
};

Options parse_options(int argc, char **argv)
{
    Options options;
    for (int index = 1; index < argc; ++index) {
        const std::string_view argument(argv[index]);
        if (argument == "--test" && index + 1 < argc) {
            options.test = argv[++index];
        } else if (argument == "--seed" && index + 1 < argc) {
            options.seed = parse_u64(argv[++index], "--seed");
        } else if (argument == "--transactions" && index + 1 < argc) {
            options.transactions = static_cast<unsigned>(
                parse_u64(argv[++index], "--transactions"));
        } else if (argument == "--no-backpressure") {
            options.backpressure = false;
        } else if (argument == "--hunt-boundaries") {
            options.hunt_boundaries = true;
        } else if (argument == "--allow-short-mixed") {
            options.allow_short_mixed = true;
        } else if (argument == "--constraints" && index + 1 < argc) {
            options.constraint_profile = argv[++index];
        } else if (argument == "--constraint" && index + 1 < argc) {
            options.constraint_overrides.emplace_back(argv[++index]);
        }
    }
    return options;
}

#include "random/coverage.inc"

#include "scenarios/control.inc"
#include "scenarios/load_feedback.inc"
#include "scenarios/manager_errors.inc"
#include "scenarios/maintenance_reset.inc"
#include "scenarios/vector_memory.inc"
#include "scenarios/cache_queue.inc"
#include "scenarios/protection.inc"
#include "scenarios/translation_context.inc"
#include "scenarios/translation_faults.inc"
#include "scenarios/translation_permissions.inc"
#include "scenarios/guest_faults.inc"
#include "scenarios/random_legacy.inc"
#include "scenarios/random_mixed.inc"
#include "scenarios/random_stress.inc"
} // namespace

int main(int argc, char **argv)
{
    try {
        const Options options = parse_options(argc, argv);
        if (options.test == "smoke") {
            return run_smoke(argc, argv);
        }
        if (options.test == "l2-flush-contracts") {
            return run_l2_flush_contracts(argc, argv);
        }
        if (options.test == "top-control-contracts") {
            return run_top_control_contracts(argc, argv);
        }
        if (options.test == "trace-bridge-contracts") {
            return run_trace_bridge_contracts(argc, argv);
        }
        if (options.test == "dft-bridge-contracts") {
            return run_dft_bridge_contracts(argc, argv);
        }
        if (options.test == "pin-space") {
            return run_pin_space(argc, argv);
        }
        if (options.test == "frontend-bridge") {
            return run_frontend_bridge(argc, argv, options);
        }
        if (options.test == "frontend-reset-recovery") {
            return run_frontend_reset_recovery(argc, argv);
        }
        if (options.test == "single-load") {
            return run_single_load(argc, argv);
        }
        if (options.test == "load-feedback") {
            return run_load_feedback(argc, argv);
        }
        if (options.test == "topdown-contracts") {
            return run_topdown_contracts(argc, argv);
        }
        if (options.test == "memory-violation") {
            return run_memory_violation(argc, argv);
        }
        if (options.test == "rar-violation") {
            return run_rar_violation(argc, argv);
        }
        if (options.test == "ifetch-prefetch") {
            return run_ifetch_prefetch(argc, argv);
        }
        if (options.test == "hardware-prefetch") {
            return run_hardware_prefetch(argc, argv);
        }
        if (options.test == "fp-loads") {
            return run_fp_loads(argc, argv);
        }
        if (options.test == "trigger-contracts") {
            return run_trigger_contracts(argc, argv);
        }
        if (options.test == "metadata-contracts") {
            return run_metadata_contracts(argc, argv);
        }
        if (options.test == "dcache-errors") {
            return run_dcache_errors(argc, argv);
        }
        if (options.test == "atomic-contracts") {
            return run_atomic_contracts(argc, argv);
        }
        if (options.test == "uncache-errors") {
            return run_uncache_errors(argc, argv);
        }
        if (options.test == "atomic-dchannel-errors") {
            return run_atomic_dchannel_errors(argc, argv);
        }
        if (options.test == "uncache-widths") {
            return run_uncache_widths(argc, argv);
        }
        if (options.test == "uncache-outstanding") {
            return run_uncache_outstanding(argc, argv);
        }
        if (options.test == "sbuffer-flush") {
            return run_sbuffer_flush(argc, argv);
        }
        if (options.test == "sbuffer-timeout") {
            return run_sbuffer_timeout(argc, argv);
        }
        if (options.test == "mbmc-contracts") {
            return run_mbmc_contracts(argc, argv);
        }
        if (options.test == "mmio-contracts") {
            return run_mmio_contracts(argc, argv);
        }
        if (options.test == "cbo-zero-contracts") {
            return run_cbo_zero_contracts(argc, argv);
        }
        if (options.test == "cmo-contracts") {
            return run_cmo_contracts(argc, argv);
        }
        if (options.test == "wfi-safety") {
            return run_wfi_safety(argc, argv);
        }
        if (options.test == "reset-recovery") {
            return run_reset_recovery(argc, argv);
        }
        if (options.test == "reset-tree-contracts") {
            return run_reset_tree_contracts(argc, argv);
        }
        if (options.test == "vector-load") {
            return run_vector_load(argc, argv);
        }
        if (options.test == "vector-split-load") {
            return run_vector_split_load(argc, argv);
        }
        if (options.test == "vector-fof") {
            return run_vector_fault_only_first(argc, argv);
        }
        if (options.test == "vector-segment") {
            return run_vector_segment(argc, argv);
        }
        if (options.test == "vector-segment-fof") {
            return run_vector_segment_fault_only_first(argc, argv);
        }
        if (options.test == "vector-store-forwarding") {
            return run_vector_store_forwarding(argc, argv);
        }
        if (options.test == "random-loads") {
            return run_random_loads(argc, argv, options);
        }
        if (options.test == "random-vector-loads") {
            return run_random_vector_loads(argc, argv, options);
        }
        if (options.test == "random-vector-forwarding") {
            return run_random_vector_forwarding(argc, argv, options);
        }
        if (options.test == "store-forwarding") {
            return run_store_forwarding(argc, argv);
        }
        if (options.test == "dcache-release") {
            return run_dcache_release(argc, argv);
        }
        if (options.test == "dcache-coherence") {
            return run_dcache_coherence(argc, argv);
        }
        if (options.test == "store-rdata-order") {
            return run_store_rdata_order(argc, argv);
        }
        if (options.test == "store-tlb-miss-preserve") {
            return run_store_tlb_miss_preserve(argc, argv);
        }
        if (options.test == "iq-slow-feedback") {
            return run_iq_slow_feedback(argc, argv);
        }
        if (options.test == "redirect") {
            return run_redirect(argc, argv);
        }
        if (options.test == "queue-pressure") {
            return run_queue_pressure(argc, argv);
        }
        if (options.test == "scalar-misaligned") {
            return run_scalar_misaligned(argc, argv);
        }
        if (options.test == "misaligned-stores") {
            return run_misaligned_stores(argc, argv);
        }
        if (options.test == "vector-addressing") {
            return run_vector_addressing(argc, argv);
        }
        if (options.test == "vector-issue-order") {
            return run_vector_issue_order(argc, argv);
        }
        if (options.test == "exception-contracts") {
            return run_exception_contracts(argc, argv);
        }
        if (options.test == "pmp-contracts") {
            return run_pmp_contracts(argc, argv);
        }
        if (options.test == "hypervisor-contracts") {
            return run_hypervisor_contracts(argc, argv);
        }
        if (options.test == "pointer-masking-contracts") {
            return run_pointer_masking_contracts(argc, argv);
        }
        if (options.test == "l2-tlb-contracts") {
            return run_l2_tlb_contracts(argc, argv);
        }
        if (options.test == "ifetch-ptw-bridge") {
            return run_ifetch_ptw_bridge(argc, argv);
        }
        if (options.test == "two-stage-translation") {
            return run_two_stage_translation(argc, argv);
        }
        if (options.test == "translation-matrix") {
            return run_translation_matrix(argc, argv);
        }
        if (options.test == "translation-fence") {
            return run_translation_fence(argc, argv, false, false, false);
        }
        if (options.test == "translation-fence-selective") {
            return run_translation_fence(argc, argv, true, false, false);
        }
        if (options.test == "translation-fence-sv48") {
            return run_translation_fence(argc, argv, false, true, true);
        }
        if (options.test == "translation-fence-sv48-selective") {
            return run_translation_fence(argc, argv, true, true, true);
        }
        if (options.test == "translation-fence-sv39-sv48x4") {
            return run_translation_fence(argc, argv, false, false, true);
        }
        if (options.test == "translation-fence-sv39-sv48x4-selective") {
            return run_translation_fence(argc, argv, true, false, true);
        }
        if (options.test == "translation-fence-sv48-sv39x4") {
            return run_translation_fence(argc, argv, false, true, false);
        }
        if (options.test == "translation-fence-sv48-sv39x4-selective") {
            return run_translation_fence(argc, argv, true, true, false);
        }
        if (options.test == "translation-inflight-context") {
            return run_translation_inflight_context(argc, argv, false, false);
        }
        if (options.test == "translation-inflight-context-sv48") {
            return run_translation_inflight_context(argc, argv, true, true);
        }
        if (options.test == "translation-inflight-context-sv39-sv48x4") {
            return run_translation_inflight_context(argc, argv, false, true);
        }
        if (options.test == "translation-inflight-context-sv48-sv39x4") {
            return run_translation_inflight_context(argc, argv, true, false);
        }
        if (options.test == "translation-context") {
            return run_translation_context(argc, argv);
        }
        if (options.test == "translation-bare") {
            return run_translation_bare(argc, argv);
        }
        if (options.test == "ptw-errors") {
            return run_ptw_errors(argc, argv);
        }
        if (options.test == "translation-faults") {
            return run_translation_faults(argc, argv);
        }
        if (options.test == "translation-permissions") {
            return run_translation_permissions(argc, argv);
        }
        if (options.test == "translation-pbmt") {
            return run_translation_pbmt(argc, argv);
        }
        if (options.test == "translation-superpages") {
            return run_translation_superpages(argc, argv);
        }
        if (options.test == "vector-guest-fault") {
            return run_vector_guest_fault(argc, argv);
        }
        if (options.test == "vector-guest-fault-split") {
            return run_vector_guest_fault(argc, argv, true);
        }
        if (options.test == "scalar-guest-fault") {
            return run_scalar_guest_fault(argc, argv);
        }
        if (options.test == "random-forwarding") {
            return run_random_forwarding(argc, argv, options);
        }
        if (options.test == "random-mixed") {
            return run_random_mixed(argc, argv, options);
        }
        if (options.test == "random-stress") {
            return run_random_stress(argc, argv, options);
        }
        if (options.test == "random-boundary-hunt") {
            return run_random_boundary_hunt(argc, argv, options);
        }
        std::cerr << "unknown test: " << options.test << '\n';
        return 2;
    } catch (const std::exception &error) {
        std::cerr << "MEMBLOCK_ARGUMENT_ERROR reason=" << error.what() << '\n';
        return 2;
    }
}
