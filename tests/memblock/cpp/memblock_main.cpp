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

struct RandomConstraints {
    enum Operation : unsigned {
        scalar_load,
        scalar_store,
        vector_load,
        vector_store,
        vector_segment,
        prefetch,
        atomic,
        noncacheable,
        mmio,
        hypervisor,
        cmo,
        ptw_error,
        load_merge,
        set_pressure,
        operation_count,
    };

    enum AtomicFamily : unsigned {
        atomic_amo,
        atomic_lrsc,
        atomic_cas,
        atomic_family_count,
    };

    enum HypervisorFamily : unsigned {
        hypervisor_hlv,
        hypervisor_hlvx,
        hypervisor_hsv,
        hypervisor_family_count,
    };

    enum HypervisorPbmtPair : unsigned {
        hypervisor_pbmt_pma_pma,
        hypervisor_pbmt_pma_nc,
        hypervisor_pbmt_pma_io,
        hypervisor_pbmt_nc_io,
        hypervisor_pbmt_io_nc,
        hypervisor_pbmt_pair_count,
    };

    enum HypervisorPmpRelation : unsigned {
        hypervisor_pmp_none,
        hypervisor_pmp_first,
        hypervisor_pmp_last,
        hypervisor_pmp_below,
        hypervisor_pmp_above,
        hypervisor_pmp_cross_lower,
        hypervisor_pmp_cross_upper,
        hypervisor_pmp_relation_count,
    };

    enum CmoOperation : unsigned {
        cmo_clean,
        cmo_flush,
        cmo_inval,
        cmo_operation_count,
    };

    static constexpr unsigned cmo_probe_depth_count =
        memblock::kDcacheProbeEntries;
    static constexpr unsigned atomic_probe_depth_count =
        memblock::kDcacheProbeEntries + 1U;

    enum TranslationRegime : unsigned {
        translation_bare,
        translation_stage1,
        translation_nested,
        translation_regime_count,
    };

    enum FenceKind : unsigned {
        fence_sfence,
        fence_hfence_vvma,
        fence_hfence_gvma,
        fence_kind_count,
    };

    enum PtwErrorSite : unsigned {
        ptw_error_stage1,
        ptw_error_gstage,
        ptw_error_nested_g_implicit,
        ptw_error_nested_vs,
        ptw_error_nested_g_final,
        ptw_error_site_count,
    };

    enum PtwErrorLevel : unsigned {
        ptw_error_root,
        ptw_error_intermediate,
        ptw_error_leaf,
        ptw_error_level_count,
    };

    enum LoadMergePattern : unsigned {
        load_merge_same_address,
        load_merge_same_beat,
        load_merge_cross_beat,
        load_merge_pattern_count,
    };

    enum SetPressureWindowClass : unsigned {
        set_pressure_single_window,
        set_pressure_dual_window,
        set_pressure_triple_window,
        set_pressure_quad_window,
        set_pressure_window_class_count,
    };

    std::string name;
    std::array<unsigned, operation_count> operation_weights{};
    std::array<unsigned, 3> locality_weights{};
    std::array<unsigned, atomic_family_count> atomic_family_weights{};
    std::array<unsigned, 2> atomic_width_weights{};
    unsigned atomic_error_per_mille = 0;
    unsigned atomic_error_denied_per_mille = 0;
    std::array<unsigned, atomic_probe_depth_count> atomic_probe_depth_weights{};
    std::array<unsigned, hypervisor_family_count> hypervisor_family_weights{};
    unsigned hypervisor_spvp_user_per_mille = 0;
    std::array<unsigned, hypervisor_pbmt_pair_count>
        hypervisor_pbmt_pair_weights{};
    unsigned hypervisor_pma_device_per_mille = 0;
    std::array<unsigned, hypervisor_pmp_relation_count>
        hypervisor_pmp_relation_weights{};
    std::array<unsigned, cmo_operation_count> cmo_operation_weights{};
    unsigned cmo_dirty_per_mille = 0;
    unsigned cmo_younger_overlap_per_mille = 0;
    unsigned cmo_error_per_mille = 0;
    unsigned cmo_error_denied_per_mille = 0;
    std::array<unsigned, cmo_probe_depth_count> cmo_probe_depth_weights{};
    unsigned dcache_load_error_per_mille = 0;
    unsigned dcache_load_error_denied_per_mille = 0;
    std::array<unsigned, ptw_error_site_count> ptw_error_site_weights{};
    std::array<unsigned, ptw_error_level_count> ptw_error_level_weights{};
    unsigned ptw_error_stores_per_mille = 0;
    unsigned ptw_error_denied_per_mille = 0;
    unsigned ptw_error_corrupt_first_per_mille = 0;
    std::array<unsigned, 2> load_merge_depth_weights{};
    std::array<unsigned, load_merge_pattern_count>
        load_merge_pattern_weights{};
    std::array<unsigned, 2> set_pressure_depth_weights{};
    std::array<unsigned, 4> set_pressure_width_weights{};
    std::array<unsigned, 4> set_pressure_set_weights{};
    unsigned set_pressure_dirty_per_mille = 0;
    unsigned set_pressure_refill_overlap_per_mille = 0;
    unsigned set_pressure_release_backpressure_per_mille = 0;
    unsigned set_pressure_dual_window_per_mille = 0;
    unsigned set_pressure_triple_window_per_mille = 0;
    unsigned set_pressure_quad_window_per_mille = 0;
    std::array<unsigned, translation_regime_count> translation_weights{};
    std::array<unsigned, 2> stage1_mode_weights{};
    std::array<unsigned, 2> vs_mode_weights{};
    std::array<unsigned, 2> g_mode_weights{};
    unsigned stage1_napot_per_mille = 0;
    unsigned nested_vs_napot_per_mille = 0;
    unsigned nested_g_napot_per_mille = 0;
    std::array<unsigned, fence_kind_count> fence_kind_weights{};
    std::array<unsigned, 2> fence_scope_weights{};
    unsigned concurrent_actions_per_mille = 1000;
    unsigned special_concurrent_per_mille = 0;
    unsigned translation_switches_per_mille = 0;
    unsigned tlb_flushes_per_mille = 0;
    unsigned misaligned_per_mille = 0;
    unsigned vector_corner_per_mille = 0;
    unsigned vector_masked_per_mille = 0;
    unsigned vector_vma_per_mille = 0;
    unsigned vector_vta_per_mille = 0;
    unsigned vector_partial_vl_per_mille = 0;
    unsigned vector_nonzero_vstart_per_mille = 0;
    std::array<unsigned, 4> vector_addressing_weights{};
    std::array<unsigned, 4> vector_eew_weights{};
    std::array<unsigned, 4> vector_sew_weights{};
    std::array<unsigned, 7> vector_lmul_weights{};
    std::array<unsigned, 7> vector_emul_weights{};
    std::array<unsigned, 4> vector_segment_addressing_weights{};
    std::array<unsigned, 4> vector_segment_eew_weights{};
    std::array<unsigned, 4> vector_segment_sew_weights{};
    std::array<unsigned, 7> vector_segment_lmul_weights{};
    std::array<unsigned, 7> vector_segment_emul_weights{};
    std::array<unsigned, 7> vector_segment_nf_weights{};
    unsigned vector_segment_stores_per_mille = 0;
    unsigned probes_per_mille = 0;
    unsigned probe_to_b_per_mille = 0;
    unsigned probe_need_data_per_mille = 0;
    unsigned probe_overlap_per_mille = 0;
    unsigned probe_triple_overlap_per_mille = 0;
    std::array<unsigned, memblock::kDcacheProbeEntries - 2>
        probe_deep_depth_weights{};
    unsigned nc_stores_per_mille = 0;
    unsigned mmio_stores_per_mille = 0;
    unsigned uncache_error_per_mille = 0;
    unsigned uncache_load_error_denied_per_mille = 0;
    unsigned stride_stream_per_mille = 0;
    memblock::ResponseLatencyProfiles response_latency{};

    static RandomConstraints preset(std::string_view name)
    {
        if (name == "coverage") {
            return RandomConstraints{
                .name = "coverage",
                .operation_weights = {
                    200, 150, 150, 150, 75, 100, 100, 75, 75, 75, 75, 75,
                    100, 75},
                .locality_weights = {250, 250, 500},
                .atomic_family_weights = {8, 2, 2},
                .atomic_width_weights = {1, 1},
                .atomic_error_per_mille = 100,
                .atomic_error_denied_per_mille = 500,
                .atomic_probe_depth_weights = {1, 1, 1, 1, 1, 1, 1, 1, 1},
                .hypervisor_family_weights = {1, 1, 1},
                .hypervisor_spvp_user_per_mille = 500,
                .hypervisor_pbmt_pair_weights = {1, 1, 1, 1, 1},
                .hypervisor_pma_device_per_mille = 500,
                .hypervisor_pmp_relation_weights = {1, 1, 1, 1, 1, 1, 1},
                .cmo_operation_weights = {1, 1, 1},
                .cmo_dirty_per_mille = 500,
                .cmo_younger_overlap_per_mille = 500,
                .cmo_error_per_mille = 100,
                .cmo_error_denied_per_mille = 500,
                .cmo_probe_depth_weights = {1, 1, 1, 1, 1, 1, 1, 1},
                .dcache_load_error_per_mille = 100,
                .dcache_load_error_denied_per_mille = 500,
                .ptw_error_site_weights = {1, 1, 1, 1, 1},
                .ptw_error_level_weights = {1, 1, 1},
                .ptw_error_stores_per_mille = 500,
                .ptw_error_denied_per_mille = 500,
                .ptw_error_corrupt_first_per_mille = 500,
                .load_merge_depth_weights = {1, 1},
                .load_merge_pattern_weights = {1, 1, 1},
                .set_pressure_depth_weights = {1, 1},
                .set_pressure_width_weights = {1, 1, 1, 1},
                .set_pressure_set_weights = {1, 1, 1, 1},
                .set_pressure_dirty_per_mille = 500,
                .set_pressure_refill_overlap_per_mille = 500,
                .set_pressure_release_backpressure_per_mille = 500,
                .set_pressure_dual_window_per_mille = 500,
                .set_pressure_triple_window_per_mille = 333,
                .set_pressure_quad_window_per_mille = 250,
                .translation_weights = {1, 1, 1},
                .stage1_mode_weights = {1, 1},
                .vs_mode_weights = {1, 1},
                .g_mode_weights = {1, 1},
                .stage1_napot_per_mille = 500,
                .nested_vs_napot_per_mille = 500,
                .nested_g_napot_per_mille = 500,
                .fence_kind_weights = {1, 1, 1},
                .fence_scope_weights = {1, 1},
                .concurrent_actions_per_mille = 1000,
                .special_concurrent_per_mille = 500,
                .translation_switches_per_mille = 500,
                .tlb_flushes_per_mille = 50,
                .misaligned_per_mille = 500,
                .vector_corner_per_mille = 1000,
                .vector_masked_per_mille = 500,
                .vector_vma_per_mille = 500,
                .vector_vta_per_mille = 500,
                .vector_partial_vl_per_mille = 500,
                .vector_nonzero_vstart_per_mille = 500,
                .vector_addressing_weights = {1, 1, 1, 1},
                .vector_eew_weights = {1, 1, 1, 1},
                .vector_sew_weights = {1, 1, 1, 1},
                .vector_lmul_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_emul_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_segment_addressing_weights = {1, 1, 1, 1},
                .vector_segment_eew_weights = {1, 1, 1, 1},
                .vector_segment_sew_weights = {1, 1, 1, 1},
                .vector_segment_lmul_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_segment_emul_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_segment_nf_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_segment_stores_per_mille = 500,
                .probes_per_mille = 20,
                .probe_to_b_per_mille = 500,
                .probe_need_data_per_mille = 500,
                .probe_overlap_per_mille = 500,
                .probe_triple_overlap_per_mille = 500,
                .probe_deep_depth_weights = {1, 1, 1, 1, 1, 1},
                .nc_stores_per_mille = 500,
                .mmio_stores_per_mille = 500,
                .uncache_error_per_mille = 100,
                .uncache_load_error_denied_per_mille = 500,
                .stride_stream_per_mille = 500,
                .response_latency = {},
            };
        }
        if (name == "spec") {
            // The ordinary load/store split comes from the aggregate final
            // measurement counters. Rare operations are verification floors,
            // not claims about their exact SPEC frequency.
            return RandomConstraints{
                .name = "spec",
                .operation_weights = {
                    648, 270, 20, 10, 1, 35, 5, 5, 5, 1, 1, 0, 10, 2},
                .locality_weights = {800, 150, 50},
                .atomic_family_weights = {90, 5, 5},
                .atomic_width_weights = {1, 1},
                .atomic_error_per_mille = 0,
                .atomic_error_denied_per_mille = 500,
                .atomic_probe_depth_weights = {1000, 10, 5, 2, 1, 1, 1, 1, 1},
                .hypervisor_family_weights = {90, 5, 5},
                .hypervisor_spvp_user_per_mille = 1,
                .hypervisor_pbmt_pair_weights = {996, 1, 1, 1, 1},
                .hypervisor_pma_device_per_mille = 1,
                .hypervisor_pmp_relation_weights = {
                    999994, 1, 1, 1, 1, 1, 1},
                .cmo_operation_weights = {1, 1, 1},
                .cmo_dirty_per_mille = 50,
                .cmo_younger_overlap_per_mille = 10,
                .cmo_error_per_mille = 0,
                .cmo_error_denied_per_mille = 500,
                .cmo_probe_depth_weights = {1000, 10, 5, 2, 1, 1, 1, 1},
                .dcache_load_error_per_mille = 0,
                .dcache_load_error_denied_per_mille = 500,
                .ptw_error_site_weights = {1, 1, 1, 1, 1},
                .ptw_error_level_weights = {1, 1, 1},
                .ptw_error_stores_per_mille = 500,
                .ptw_error_denied_per_mille = 500,
                .ptw_error_corrupt_first_per_mille = 500,
                .load_merge_depth_weights = {19, 1},
                .load_merge_pattern_weights = {1, 8, 1},
                .set_pressure_depth_weights = {9, 1},
                .set_pressure_width_weights = {1, 2, 6, 20},
                .set_pressure_set_weights = {1, 1, 1, 1},
                .set_pressure_dirty_per_mille = 50,
                .set_pressure_refill_overlap_per_mille = 10,
                .set_pressure_release_backpressure_per_mille = 10,
                .set_pressure_dual_window_per_mille = 10,
                .set_pressure_triple_window_per_mille = 1,
                .set_pressure_quad_window_per_mille = 1,
                .translation_weights = {5, 990, 5},
                .stage1_mode_weights = {95, 5},
                .vs_mode_weights = {1, 1},
                .g_mode_weights = {1, 1},
                .stage1_napot_per_mille = 1,
                .nested_vs_napot_per_mille = 1,
                .nested_g_napot_per_mille = 1,
                .fence_kind_weights = {98, 1, 1},
                .fence_scope_weights = {95, 5},
                .concurrent_actions_per_mille = 100,
                .special_concurrent_per_mille = 20,
                .translation_switches_per_mille = 1,
                .tlb_flushes_per_mille = 20,
                .misaligned_per_mille = 5,
                .vector_corner_per_mille = 100,
                .vector_masked_per_mille = 100,
                .vector_vma_per_mille = 500,
                .vector_vta_per_mille = 500,
                .vector_partial_vl_per_mille = 100,
                .vector_nonzero_vstart_per_mille = 20,
                .vector_addressing_weights = {980, 10, 5, 5},
                .vector_eew_weights = {5, 3, 2, 1},
                .vector_sew_weights = {1, 2, 5, 2},
                .vector_lmul_weights = {1, 1, 2, 16, 8, 2, 1},
                .vector_emul_weights = {1, 1, 2, 16, 8, 2, 1},
                .vector_segment_addressing_weights = {980, 10, 5, 5},
                .vector_segment_eew_weights = {5, 3, 2, 1},
                .vector_segment_sew_weights = {1, 2, 5, 2},
                .vector_segment_lmul_weights = {1, 1, 2, 16, 8, 2, 1},
                .vector_segment_emul_weights = {1, 1, 2, 16, 8, 2, 1},
                .vector_segment_nf_weights = {32, 16, 8, 4, 2, 1, 1},
                .vector_segment_stores_per_mille = 300,
                .probes_per_mille = 1,
                .probe_to_b_per_mille = 500,
                .probe_need_data_per_mille = 500,
                .probe_overlap_per_mille = 10,
                .probe_triple_overlap_per_mille = 10,
                .probe_deep_depth_weights = {32, 16, 8, 4, 2, 1},
                .nc_stores_per_mille = 300,
                .mmio_stores_per_mille = 300,
                .uncache_error_per_mille = 0,
                .uncache_load_error_denied_per_mille = 500,
                .stride_stream_per_mille = 100,
                .response_latency = {
                    memblock::ResponseLatencyProfile::spec,
                    memblock::ResponseLatencyProfile::spec,
                    memblock::ResponseLatencyProfile::spec},
            };
        }
        if (name == "corner") {
            return RandomConstraints{
                .name = "corner",
                .operation_weights = {
                    125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125,
                    125, 125, 125},
                .locality_weights = {100, 200, 700},
                .atomic_family_weights = {1, 1, 1},
                .atomic_width_weights = {1, 1},
                .atomic_error_per_mille = 500,
                .atomic_error_denied_per_mille = 500,
                .atomic_probe_depth_weights = {1, 1, 1, 1, 2, 4, 8, 16, 32},
                .hypervisor_family_weights = {1, 1, 1},
                .hypervisor_spvp_user_per_mille = 500,
                .hypervisor_pbmt_pair_weights = {1, 1, 1, 1, 1},
                .hypervisor_pma_device_per_mille = 500,
                .hypervisor_pmp_relation_weights = {1, 1, 1, 1, 1, 1, 1},
                .cmo_operation_weights = {1, 1, 1},
                .cmo_dirty_per_mille = 500,
                .cmo_younger_overlap_per_mille = 750,
                .cmo_error_per_mille = 500,
                .cmo_error_denied_per_mille = 500,
                .cmo_probe_depth_weights = {1, 1, 1, 2, 4, 8, 16, 32},
                .dcache_load_error_per_mille = 500,
                .dcache_load_error_denied_per_mille = 500,
                .ptw_error_site_weights = {1, 1, 1, 1, 1},
                .ptw_error_level_weights = {1, 1, 1},
                .ptw_error_stores_per_mille = 500,
                .ptw_error_denied_per_mille = 500,
                .ptw_error_corrupt_first_per_mille = 500,
                .load_merge_depth_weights = {1, 1},
                .load_merge_pattern_weights = {1, 1, 1},
                .set_pressure_depth_weights = {1, 1},
                .set_pressure_width_weights = {1, 1, 1, 1},
                .set_pressure_set_weights = {1, 1, 1, 1},
                .set_pressure_dirty_per_mille = 500,
                .set_pressure_refill_overlap_per_mille = 750,
                .set_pressure_release_backpressure_per_mille = 750,
                .set_pressure_dual_window_per_mille = 750,
                .set_pressure_triple_window_per_mille = 500,
                .set_pressure_quad_window_per_mille = 750,
                .translation_weights = {1, 1, 1},
                .stage1_mode_weights = {1, 1},
                .vs_mode_weights = {1, 1},
                .g_mode_weights = {1, 1},
                .stage1_napot_per_mille = 500,
                .nested_vs_napot_per_mille = 500,
                .nested_g_napot_per_mille = 500,
                .fence_kind_weights = {1, 1, 1},
                .fence_scope_weights = {1, 1},
                .concurrent_actions_per_mille = 500,
                .special_concurrent_per_mille = 750,
                .translation_switches_per_mille = 750,
                .tlb_flushes_per_mille = 100,
                .misaligned_per_mille = 500,
                .vector_corner_per_mille = 1000,
                .vector_masked_per_mille = 500,
                .vector_vma_per_mille = 500,
                .vector_vta_per_mille = 500,
                .vector_partial_vl_per_mille = 500,
                .vector_nonzero_vstart_per_mille = 500,
                .vector_addressing_weights = {1, 1, 1, 1},
                .vector_eew_weights = {1, 1, 1, 1},
                .vector_sew_weights = {1, 1, 1, 1},
                .vector_lmul_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_emul_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_segment_addressing_weights = {1, 1, 1, 1},
                .vector_segment_eew_weights = {1, 1, 1, 1},
                .vector_segment_sew_weights = {1, 1, 1, 1},
                .vector_segment_lmul_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_segment_emul_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_segment_nf_weights = {1, 1, 1, 1, 1, 1, 1},
                .vector_segment_stores_per_mille = 500,
                .probes_per_mille = 100,
                .probe_to_b_per_mille = 500,
                .probe_need_data_per_mille = 500,
                .probe_overlap_per_mille = 750,
                .probe_triple_overlap_per_mille = 750,
                .probe_deep_depth_weights = {1, 1, 1, 1, 1, 1},
                .nc_stores_per_mille = 500,
                .mmio_stores_per_mille = 500,
                .uncache_error_per_mille = 500,
                .uncache_load_error_denied_per_mille = 500,
                .stride_stream_per_mille = 750,
                .response_latency = {
                    memblock::ResponseLatencyProfile::spec,
                    memblock::ResponseLatencyProfile::spec,
                    memblock::ResponseLatencyProfile::spec},
            };
        }
        throw std::invalid_argument(
            "unknown random constraint preset: " + std::string(name));
    }

    void apply(std::string_view assignment)
    {
        const std::size_t separator = assignment.find('=');
        if (separator == std::string_view::npos || separator == 0 ||
            separator + 1 == assignment.size()) {
            throw std::invalid_argument(
                "--constraint expects key=value, got: " +
                std::string(assignment));
        }
        const std::string_view key = assignment.substr(0, separator);
        const std::string_view value = assignment.substr(separator + 1);
        auto parse_latency = [&](std::string_view candidate) {
            if (candidate == "compact") {
                return memblock::ResponseLatencyProfile::compact;
            }
            if (candidate == "spec") {
                return memblock::ResponseLatencyProfile::spec;
            }
            throw std::invalid_argument(
                "constraint latency must be compact or spec");
        };
        if (key == "latency") {
            const auto profile = parse_latency(value);
            response_latency = {profile, profile, profile};
            return;
        }
        if (key == "dcache-latency") {
            response_latency.dcache = parse_latency(value);
            return;
        }
        if (key == "ptw-latency") {
            response_latency.ptw = parse_latency(value);
            return;
        }
        if (key == "uncache-latency") {
            response_latency.uncache = parse_latency(value);
            return;
        }

        const std::uint64_t parsed_value = parse_u64(value, "--constraint");
        if (parsed_value > std::numeric_limits<unsigned>::max()) {
            throw std::invalid_argument(
                "constraint weight exceeds the supported unsigned range");
        }
        const unsigned parsed = static_cast<unsigned>(parsed_value);
        const std::array<std::pair<std::string_view, Operation>, operation_count>
            operation_keys{{
                {"scalar-load", scalar_load},
                {"scalar-store", scalar_store},
                {"vector-load", vector_load},
                {"vector-store", vector_store},
                {"vector-segment", vector_segment},
                {"prefetch", prefetch},
                {"atomic", atomic},
                {"nc", noncacheable},
                {"mmio", mmio},
                {"hypervisor", hypervisor},
                {"cmo", cmo},
                {"ptw-error", ptw_error},
                {"load-merge", load_merge},
                {"set-pressure", set_pressure},
            }};
        for (const auto &[candidate, operation] : operation_keys) {
            if (key == candidate) {
                operation_weights[operation] = parsed;
                return;
            }
        }
        const std::array<std::pair<std::string_view, AtomicFamily>,
                         atomic_family_count> atomic_family_keys{{
            {"atomic-amo", atomic_amo},
            {"atomic-lrsc", atomic_lrsc},
            {"atomic-cas", atomic_cas},
        }};
        for (const auto &[candidate, family] : atomic_family_keys) {
            if (key == candidate) {
                atomic_family_weights[family] = parsed;
                return;
            }
        }
        if (key == "atomic-w") {
            atomic_width_weights[0] = parsed;
            return;
        }
        if (key == "atomic-d") {
            atomic_width_weights[1] = parsed;
            return;
        }
        if (key == "atomic-error") {
            atomic_error_per_mille = parsed;
            return;
        }
        if (key == "atomic-error-denied") {
            atomic_error_denied_per_mille = parsed;
            return;
        }
        const std::array<std::pair<std::string_view, HypervisorFamily>,
                         hypervisor_family_count> hypervisor_family_keys{{
            {"hypervisor-hlv", hypervisor_hlv},
            {"hypervisor-hlvx", hypervisor_hlvx},
            {"hypervisor-hsv", hypervisor_hsv},
        }};
        for (const auto &[candidate, family] : hypervisor_family_keys) {
            if (key == candidate) {
                hypervisor_family_weights[family] = parsed;
                return;
            }
        }
        if (key == "hypervisor-spvp-user") {
            hypervisor_spvp_user_per_mille = parsed;
            return;
        }
        if (key == "hypervisor-pma-device") {
            hypervisor_pma_device_per_mille = parsed;
            return;
        }
        const auto assign_weight = [&]<std::size_t N>(
            const std::array<std::string_view, N> &keys,
            std::array<unsigned, N> &weights) {
            for (unsigned index = 0; index < keys.size(); ++index) {
                if (key == keys[index]) {
                    weights[index] = parsed;
                    return true;
                }
            }
            return false;
        };
        constexpr std::array<std::string_view, atomic_probe_depth_count>
            atomic_probe_depth_keys{{
                "atomic-probe-depth0", "atomic-probe-depth1",
                "atomic-probe-depth2", "atomic-probe-depth3",
                "atomic-probe-depth4", "atomic-probe-depth5",
                "atomic-probe-depth6", "atomic-probe-depth7",
                "atomic-probe-depth8",
            }};
        if (assign_weight(
                atomic_probe_depth_keys, atomic_probe_depth_weights)) {
            return;
        }
        constexpr std::array<std::string_view, hypervisor_pbmt_pair_count>
            hypervisor_pbmt_pair_keys{{
                "hypervisor-pbmt-pma-pma",
                "hypervisor-pbmt-pma-nc",
                "hypervisor-pbmt-pma-io",
                "hypervisor-pbmt-nc-io",
                "hypervisor-pbmt-io-nc",
            }};
        if (assign_weight(
                hypervisor_pbmt_pair_keys, hypervisor_pbmt_pair_weights)) {
            return;
        }
        constexpr std::array<std::string_view, hypervisor_pmp_relation_count>
            hypervisor_pmp_relation_keys{{
                "hypervisor-pmp-none",
                "hypervisor-pmp-first",
                "hypervisor-pmp-last",
                "hypervisor-pmp-below",
                "hypervisor-pmp-above",
                "hypervisor-pmp-cross-lower",
                "hypervisor-pmp-cross-upper",
            }};
        if (assign_weight(
                hypervisor_pmp_relation_keys,
                hypervisor_pmp_relation_weights)) {
            return;
        }
        constexpr std::array<std::string_view, cmo_operation_count>
            cmo_operation_keys{{"cmo-clean", "cmo-flush", "cmo-inval"}};
        if (assign_weight(cmo_operation_keys, cmo_operation_weights)) {
            return;
        }
        constexpr std::array<std::string_view, cmo_probe_depth_count>
            cmo_probe_depth_keys{{
                "cmo-probe-depth1", "cmo-probe-depth2",
                "cmo-probe-depth3", "cmo-probe-depth4",
                "cmo-probe-depth5", "cmo-probe-depth6",
                "cmo-probe-depth7", "cmo-probe-depth8",
            }};
        if (assign_weight(cmo_probe_depth_keys, cmo_probe_depth_weights)) {
            return;
        }
        constexpr std::array<std::string_view, ptw_error_site_count>
            ptw_error_site_keys{{
                "ptw-error-stage1",
                "ptw-error-gstage",
                "ptw-error-nested-g-implicit",
                "ptw-error-nested-vs",
                "ptw-error-nested-g-final",
            }};
        constexpr std::array<std::string_view, ptw_error_level_count>
            ptw_error_level_keys{{
                "ptw-error-root",
                "ptw-error-intermediate",
                "ptw-error-leaf",
            }};
        if (assign_weight(ptw_error_site_keys, ptw_error_site_weights) ||
            assign_weight(ptw_error_level_keys, ptw_error_level_weights)) {
            return;
        }
        constexpr std::array<std::string_view, 2> load_merge_depth_keys{{
            "load-merge-depth2", "load-merge-depth3",
        }};
        constexpr std::array<std::string_view, load_merge_pattern_count>
            load_merge_pattern_keys{{
                "load-merge-same-address",
                "load-merge-same-beat",
                "load-merge-cross-beat",
            }};
        if (assign_weight(
                load_merge_depth_keys, load_merge_depth_weights) ||
            assign_weight(
                load_merge_pattern_keys, load_merge_pattern_weights)) {
            return;
        }
        constexpr std::array<std::string_view, 2> set_pressure_depth_keys{{
            "set-pressure-depth9", "set-pressure-depth10",
        }};
        constexpr std::array<std::string_view, 4> set_pressure_width_keys{{
            "set-pressure-sb", "set-pressure-sh", "set-pressure-sw",
            "set-pressure-sd",
        }};
        constexpr std::array<std::string_view, 4> set_pressure_set_keys{{
            "set-pressure-set-q0", "set-pressure-set-q1",
            "set-pressure-set-q2", "set-pressure-set-q3",
        }};
        if (assign_weight(
                set_pressure_depth_keys, set_pressure_depth_weights) ||
            assign_weight(
                set_pressure_width_keys, set_pressure_width_weights) ||
            assign_weight(set_pressure_set_keys, set_pressure_set_weights)) {
            return;
        }
        constexpr std::array<std::string_view,
                             memblock::kDcacheProbeEntries - 2>
            probe_deep_depth_keys{{
                "probe-depth3", "probe-depth4", "probe-depth5",
                "probe-depth6", "probe-depth7", "probe-depth8",
            }};
        if (assign_weight(probe_deep_depth_keys, probe_deep_depth_weights)) {
            return;
        }
        constexpr std::array<std::string_view, 4> vector_addressing_keys{{
            "vector-unit-stride", "vector-strided",
            "vector-indexed-unordered", "vector-indexed-ordered",
        }};
        constexpr std::array<std::string_view, 4> vector_eew_keys{{
            "vector-eew8", "vector-eew16", "vector-eew32", "vector-eew64",
        }};
        constexpr std::array<std::string_view, 4> vector_sew_keys{{
            "vector-sew8", "vector-sew16", "vector-sew32", "vector-sew64",
        }};
        constexpr std::array<std::string_view, 7> vector_lmul_keys{{
            "vector-lmul-mf8", "vector-lmul-mf4", "vector-lmul-mf2",
            "vector-lmul-m1", "vector-lmul-m2", "vector-lmul-m4",
            "vector-lmul-m8",
        }};
        constexpr std::array<std::string_view, 7> vector_emul_keys{{
            "vector-emul-mf8", "vector-emul-mf4", "vector-emul-mf2",
            "vector-emul-m1", "vector-emul-m2", "vector-emul-m4",
            "vector-emul-m8",
        }};
        if (assign_weight(vector_addressing_keys, vector_addressing_weights) ||
            assign_weight(vector_eew_keys, vector_eew_weights) ||
            assign_weight(vector_sew_keys, vector_sew_weights) ||
            assign_weight(vector_lmul_keys, vector_lmul_weights) ||
            assign_weight(vector_emul_keys, vector_emul_weights)) {
            return;
        }
        constexpr std::array<std::string_view, 4> segment_addressing_keys{{
            "vector-segment-unit-stride",
            "vector-segment-strided",
            "vector-segment-indexed-unordered",
            "vector-segment-indexed-ordered",
        }};
        constexpr std::array<std::string_view, 4> segment_eew_keys{{
            "vector-segment-eew8", "vector-segment-eew16",
            "vector-segment-eew32", "vector-segment-eew64",
        }};
        constexpr std::array<std::string_view, 4> segment_sew_keys{{
            "vector-segment-sew8", "vector-segment-sew16",
            "vector-segment-sew32", "vector-segment-sew64",
        }};
        constexpr std::array<std::string_view, 7> segment_lmul_keys{{
            "vector-segment-lmul-mf8", "vector-segment-lmul-mf4",
            "vector-segment-lmul-mf2", "vector-segment-lmul-m1",
            "vector-segment-lmul-m2", "vector-segment-lmul-m4",
            "vector-segment-lmul-m8",
        }};
        constexpr std::array<std::string_view, 7> segment_emul_keys{{
            "vector-segment-emul-mf8", "vector-segment-emul-mf4",
            "vector-segment-emul-mf2", "vector-segment-emul-m1",
            "vector-segment-emul-m2", "vector-segment-emul-m4",
            "vector-segment-emul-m8",
        }};
        constexpr std::array<std::string_view, 7> segment_nf_keys{{
            "vector-segment-nf2", "vector-segment-nf3",
            "vector-segment-nf4", "vector-segment-nf5",
            "vector-segment-nf6", "vector-segment-nf7",
            "vector-segment-nf8",
        }};
        if (assign_weight(
                segment_addressing_keys,
                vector_segment_addressing_weights) ||
            assign_weight(segment_eew_keys, vector_segment_eew_weights) ||
            assign_weight(segment_sew_keys, vector_segment_sew_weights) ||
            assign_weight(segment_lmul_keys, vector_segment_lmul_weights) ||
            assign_weight(segment_emul_keys, vector_segment_emul_weights) ||
            assign_weight(segment_nf_keys, vector_segment_nf_weights)) {
            return;
        }
        const std::array<std::pair<std::string_view, TranslationRegime>,
                         translation_regime_count> translation_keys{{
            {"translation-bare", translation_bare},
            {"translation-stage1", translation_stage1},
            {"translation-nested", translation_nested},
        }};
        for (const auto &[candidate, regime] : translation_keys) {
            if (key == candidate) {
                translation_weights[regime] = parsed;
                return;
            }
        }
        if (key == "stage1-sv39") {
            stage1_mode_weights[0] = parsed;
            return;
        }
        if (key == "stage1-sv48") {
            stage1_mode_weights[1] = parsed;
            return;
        }
        if (key == "vs-sv39") {
            vs_mode_weights[0] = parsed;
            return;
        }
        if (key == "vs-sv48") {
            vs_mode_weights[1] = parsed;
            return;
        }
        if (key == "g-sv39x4") {
            g_mode_weights[0] = parsed;
            return;
        }
        if (key == "g-sv48x4") {
            g_mode_weights[1] = parsed;
            return;
        }
        if (key == "translation-stage1-napot") {
            stage1_napot_per_mille = parsed;
            return;
        }
        if (key == "translation-vs-napot") {
            nested_vs_napot_per_mille = parsed;
            return;
        }
        if (key == "translation-g-napot") {
            nested_g_napot_per_mille = parsed;
            return;
        }
        const std::array<std::pair<std::string_view, FenceKind>,
                         fence_kind_count> fence_keys{{
            {"fence-sfence", fence_sfence},
            {"fence-hfence-vvma", fence_hfence_vvma},
            {"fence-hfence-gvma", fence_hfence_gvma},
        }};
        for (const auto &[candidate, kind] : fence_keys) {
            if (key == candidate) {
                fence_kind_weights[kind] = parsed;
                return;
            }
        }
        if (key == "fence-global") {
            fence_scope_weights[0] = parsed;
            return;
        }
        if (key == "fence-selective") {
            fence_scope_weights[1] = parsed;
            return;
        }
        if (key == "locality-hot") {
            locality_weights[0] = parsed;
        } else if (key == "locality-warm") {
            locality_weights[1] = parsed;
        } else if (key == "locality-cold") {
            locality_weights[2] = parsed;
        } else if (key == "concurrent") {
            concurrent_actions_per_mille = parsed;
        } else if (key == "special-concurrent") {
            special_concurrent_per_mille = parsed;
        } else if (key == "translation-switch") {
            translation_switches_per_mille = parsed;
        } else if (key == "tlb-flush") {
            tlb_flushes_per_mille = parsed;
        } else if (key == "misaligned") {
            misaligned_per_mille = parsed;
        } else if (key == "vector-corner") {
            vector_corner_per_mille = parsed;
        } else if (key == "vector-masked") {
            vector_masked_per_mille = parsed;
        } else if (key == "vector-vma") {
            vector_vma_per_mille = parsed;
        } else if (key == "vector-vta") {
            vector_vta_per_mille = parsed;
        } else if (key == "vector-partial-vl") {
            vector_partial_vl_per_mille = parsed;
        } else if (key == "vector-nonzero-vstart") {
            vector_nonzero_vstart_per_mille = parsed;
        } else if (key == "vector-segment-store") {
            vector_segment_stores_per_mille = parsed;
        } else if (key == "cmo-dirty") {
            cmo_dirty_per_mille = parsed;
        } else if (key == "cmo-younger-overlap") {
            cmo_younger_overlap_per_mille = parsed;
        } else if (key == "cmo-error") {
            cmo_error_per_mille = parsed;
        } else if (key == "cmo-error-denied") {
            cmo_error_denied_per_mille = parsed;
        } else if (key == "dcache-load-error") {
            dcache_load_error_per_mille = parsed;
        } else if (key == "dcache-load-error-denied") {
            dcache_load_error_denied_per_mille = parsed;
        } else if (key == "ptw-error-store") {
            ptw_error_stores_per_mille = parsed;
        } else if (key == "ptw-error-denied") {
            ptw_error_denied_per_mille = parsed;
        } else if (key == "ptw-error-corrupt-first") {
            ptw_error_corrupt_first_per_mille = parsed;
        } else if (key == "set-pressure-dirty") {
            set_pressure_dirty_per_mille = parsed;
        } else if (key == "set-pressure-refill-overlap") {
            set_pressure_refill_overlap_per_mille = parsed;
        } else if (key == "set-pressure-release-backpressure") {
            set_pressure_release_backpressure_per_mille = parsed;
        } else if (key == "set-pressure-dual-window") {
            set_pressure_dual_window_per_mille = parsed;
        } else if (key == "set-pressure-triple-window") {
            set_pressure_triple_window_per_mille = parsed;
        } else if (key == "set-pressure-quad-window") {
            set_pressure_quad_window_per_mille = parsed;
        } else if (key == "probe") {
            probes_per_mille = parsed;
        } else if (key == "probe-to-b") {
            probe_to_b_per_mille = parsed;
        } else if (key == "probe-need-data") {
            probe_need_data_per_mille = parsed;
        } else if (key == "probe-overlap") {
            probe_overlap_per_mille = parsed;
        } else if (key == "probe-triple-overlap") {
            probe_triple_overlap_per_mille = parsed;
        } else if (key == "nc-store") {
            nc_stores_per_mille = parsed;
        } else if (key == "mmio-store") {
            mmio_stores_per_mille = parsed;
        } else if (key == "uncache-error") {
            uncache_error_per_mille = parsed;
        } else if (key == "uncache-load-error-denied") {
            uncache_load_error_denied_per_mille = parsed;
        } else if (key == "stride-stream") {
            stride_stream_per_mille = parsed;
        } else {
            throw std::invalid_argument(
                "unknown random constraint key: " + std::string(key));
        }
    }

    void validate() const
    {
        if (std::accumulate(
                operation_weights.begin(), operation_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "random operation constraint weights cannot all be zero");
        }
        if (std::accumulate(
                locality_weights.begin(), locality_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "random locality constraint weights cannot all be zero");
        }
        if (operation_weights[atomic] != 0 &&
            std::accumulate(
                atomic_family_weights.begin(), atomic_family_weights.end(),
                0ULL) == 0) {
            throw std::invalid_argument(
                "atomic family constraint weights cannot all be zero");
        }
        if (operation_weights[atomic] != 0 &&
            std::accumulate(
                atomic_width_weights.begin(), atomic_width_weights.end(),
                0ULL) == 0) {
            throw std::invalid_argument(
                "atomic width constraint weights cannot all be zero");
        }
        if (operation_weights[hypervisor] != 0 &&
            std::accumulate(
                hypervisor_family_weights.begin(),
                hypervisor_family_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "hypervisor family constraint weights cannot all be zero");
        }
        if (operation_weights[hypervisor] != 0 &&
            std::accumulate(
                hypervisor_pbmt_pair_weights.begin(),
                hypervisor_pbmt_pair_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "hypervisor PBMT pair constraint weights cannot all be zero");
        }
        if (operation_weights[hypervisor] != 0 &&
            std::accumulate(
                hypervisor_pmp_relation_weights.begin(),
                hypervisor_pmp_relation_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "hypervisor PMP relation constraint weights cannot all be zero");
        }
        const bool hypervisor_non_pma_enabled = std::any_of(
            hypervisor_pbmt_pair_weights.begin() + 1,
            hypervisor_pbmt_pair_weights.end(),
            [](unsigned weight) { return weight != 0; });
        if (operation_weights[hypervisor] != 0 &&
            misaligned_per_mille != 0 &&
            hypervisor_pbmt_pair_weights[hypervisor_pbmt_pma_pma] == 0) {
            throw std::invalid_argument(
                "misaligned hypervisor traffic requires the PMA/PMA PBMT pair");
        }
        if (operation_weights[hypervisor] != 0 &&
            misaligned_per_mille == 1000 && hypervisor_non_pma_enabled) {
            throw std::invalid_argument(
                "non-PMA hypervisor PBMT pairs require aligned traffic");
        }
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pma_device_per_mille != 0 &&
            hypervisor_pbmt_pair_weights[hypervisor_pbmt_pma_pma] == 0) {
            throw std::invalid_argument(
                "fixed-PMA hypervisor traffic requires the PMA/PMA PBMT pair");
        }
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pma_device_per_mille == 1000 &&
            hypervisor_non_pma_enabled) {
            throw std::invalid_argument(
                "non-PMA hypervisor PBMT pairs require the DDR address class");
        }
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pma_device_per_mille == 1000 &&
            misaligned_per_mille != 0) {
            throw std::invalid_argument(
                "fixed-PMA hypervisor device traffic requires aligned accesses");
        }
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pma_device_per_mille != 0 &&
            misaligned_per_mille == 1000) {
            throw std::invalid_argument(
                "fixed-PMA hypervisor device traffic requires an aligned class");
        }
        const bool hypervisor_pmp_edge_enabled = std::any_of(
            hypervisor_pmp_relation_weights.begin() + 1,
            hypervisor_pmp_relation_weights.end(),
            [](unsigned weight) { return weight != 0; });
        const bool hypervisor_pmp_natural_edge_enabled = std::any_of(
            hypervisor_pmp_relation_weights.begin() + hypervisor_pmp_first,
            hypervisor_pmp_relation_weights.begin() +
                hypervisor_pmp_cross_lower,
            [](unsigned weight) { return weight != 0; });
        const bool hypervisor_pmp_cross_edge_enabled = std::any_of(
            hypervisor_pmp_relation_weights.begin() +
                hypervisor_pmp_cross_lower,
            hypervisor_pmp_relation_weights.end(),
            [](unsigned weight) { return weight != 0; });
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pmp_edge_enabled &&
            hypervisor_pmp_relation_weights[hypervisor_pmp_none] == 0) {
            throw std::invalid_argument(
                "hypervisor PMP edge relations require the no-PMP control");
        }
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pmp_edge_enabled &&
            hypervisor_pbmt_pair_weights[hypervisor_pbmt_pma_pma] == 0) {
            throw std::invalid_argument(
                "hypervisor PMP edge relations require the PMA/PMA PBMT pair");
        }
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pmp_edge_enabled &&
            hypervisor_pma_device_per_mille == 1000) {
            throw std::invalid_argument(
                "hypervisor PMP edge relations require the DDR address class");
        }
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pmp_natural_edge_enabled &&
            misaligned_per_mille == 1000) {
            throw std::invalid_argument(
                "natural hypervisor PMP edges require an aligned class");
        }
        if (operation_weights[hypervisor] != 0 &&
            hypervisor_pmp_cross_edge_enabled &&
            misaligned_per_mille == 0) {
            throw std::invalid_argument(
                "crossing hypervisor PMP edges require a misaligned class");
        }
        if (operation_weights[cmo] != 0 &&
            std::accumulate(
                cmo_operation_weights.begin(), cmo_operation_weights.end(),
                0ULL) == 0) {
            throw std::invalid_argument(
                "CMO operation constraint weights cannot all be zero");
        }
        if (operation_weights[ptw_error] != 0 &&
            std::accumulate(
                ptw_error_site_weights.begin(), ptw_error_site_weights.end(),
                0ULL) == 0) {
            throw std::invalid_argument(
                "PTW error site constraint weights cannot all be zero");
        }
        if (operation_weights[ptw_error] != 0 &&
            std::accumulate(
                ptw_error_level_weights.begin(), ptw_error_level_weights.end(),
                0ULL) == 0) {
            throw std::invalid_argument(
                "PTW error level constraint weights cannot all be zero");
        }
        if (operation_weights[load_merge] != 0 &&
            std::accumulate(
                load_merge_depth_weights.begin(),
                load_merge_depth_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "load-merge depth constraint weights cannot all be zero");
        }
        if (operation_weights[load_merge] != 0 &&
            std::accumulate(
                load_merge_pattern_weights.begin(),
                load_merge_pattern_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "load-merge pattern constraint weights cannot all be zero");
        }
        if (operation_weights[set_pressure] != 0 &&
            std::accumulate(
                set_pressure_depth_weights.begin(),
                set_pressure_depth_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "set-pressure depth constraint weights cannot all be zero");
        }
        if (operation_weights[set_pressure] != 0 &&
            std::accumulate(
                set_pressure_width_weights.begin(),
                set_pressure_width_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "set-pressure width constraint weights cannot all be zero");
        }
        if (operation_weights[set_pressure] != 0 &&
            std::accumulate(
                set_pressure_set_weights.begin(),
                set_pressure_set_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "set-pressure set constraint weights cannot all be zero");
        }
        if (operation_weights[ptw_error] != 0 &&
            ptw_error_site_weights[ptw_error_stage1] != 0 &&
            std::accumulate(
                stage1_mode_weights.begin(), stage1_mode_weights.end(),
                0ULL) == 0) {
            throw std::invalid_argument(
                "stage-1 PTW errors require a nonzero stage-1 mode weight");
        }
        if (operation_weights[ptw_error] != 0 &&
            std::any_of(
                ptw_error_site_weights.begin() + ptw_error_gstage,
                ptw_error_site_weights.end(),
                [](unsigned weight) { return weight != 0; }) &&
            std::accumulate(
                g_mode_weights.begin(), g_mode_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "G-stage PTW errors require a nonzero G-stage mode weight");
        }
        if (operation_weights[ptw_error] != 0 &&
            std::any_of(
                ptw_error_site_weights.begin() + ptw_error_nested_g_implicit,
                ptw_error_site_weights.end(),
                [](unsigned weight) { return weight != 0; }) &&
            std::accumulate(
                vs_mode_weights.begin(), vs_mode_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "nested PTW errors require a nonzero VS-stage mode weight");
        }
        if (operation_weights[vector_load] != 0 ||
            operation_weights[vector_store] != 0) {
            const auto require_vector_weights = [](const auto &weights,
                                                   const char *dimension) {
                if (std::accumulate(
                        weights.begin(), weights.end(), std::uint64_t{0}) == 0) {
                    throw std::invalid_argument(
                        std::string("vector ") + dimension +
                        " constraint weights cannot all be zero");
                }
            };
            require_vector_weights(vector_addressing_weights, "addressing");
            require_vector_weights(vector_eew_weights, "EEW");
            require_vector_weights(vector_sew_weights, "SEW");
            require_vector_weights(vector_lmul_weights, "LMUL");
            require_vector_weights(vector_emul_weights, "EMUL");

            const unsigned minimum_vlmax = vector_policy_minimum_vlmax();
            std::array<bool, 4> reachable_addressing{};
            std::array<bool, 4> reachable_eew{};
            std::array<bool, 4> reachable_sew{};
            std::array<bool, 7> reachable_lmul{};
            std::array<bool, 7> reachable_emul{};
            bool reachable_shape = false;
            for (unsigned addressing = 0; addressing < 4; ++addressing) {
                if (vector_addressing_weights[addressing] == 0) {
                    continue;
                }
                for (unsigned eew = 0; eew < 4; ++eew) {
                    if (vector_eew_weights[eew] == 0) {
                        continue;
                    }
                    for (unsigned vsew = 0; vsew < 4; ++vsew) {
                        if (vector_sew_weights[vsew] == 0) {
                            continue;
                        }
                        for (int lmul_log2 = -3; lmul_log2 <= 3;
                             ++lmul_log2) {
                            if (vector_lmul_weights[lmul_log2 + 3] == 0) {
                                continue;
                            }
                            const int emul_log2 = static_cast<int>(eew) -
                                static_cast<int>(vsew) + lmul_log2;
                            if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                                emul_log2 < -3 || emul_log2 > 3 ||
                                vector_emul_weights[emul_log2 + 3] == 0) {
                                continue;
                            }
                            const unsigned vector_bytes = lmul_log2 < 0
                                ? 16U >> static_cast<unsigned>(-lmul_log2)
                                : 16U << static_cast<unsigned>(lmul_log2);
                            if ((vector_bytes >> vsew) < minimum_vlmax) {
                                continue;
                            }
                            reachable_shape = true;
                            reachable_addressing[addressing] = true;
                            reachable_eew[eew] = true;
                            reachable_sew[vsew] = true;
                            reachable_lmul[lmul_log2 + 3] = true;
                            reachable_emul[emul_log2 + 3] = true;
                        }
                    }
                }
            }
            const auto require_reachable = [](const auto &weights,
                                              const auto &reachable,
                                              const char *dimension) {
                for (unsigned index = 0; index < weights.size(); ++index) {
                    if (weights[index] != 0 && !reachable[index]) {
                        throw std::invalid_argument(
                            std::string("vector ") + dimension +
                            " constraint enables an unreachable class");
                    }
                }
            };
            if (!reachable_shape) {
                throw std::invalid_argument(
                    "vector policy constraints are incompatible with enabled "
                    "shape classes");
            }
            require_reachable(
                vector_addressing_weights, reachable_addressing, "addressing");
            require_reachable(vector_eew_weights, reachable_eew, "EEW");
            require_reachable(vector_sew_weights, reachable_sew, "SEW");
            require_reachable(vector_lmul_weights, reachable_lmul, "LMUL");
            require_reachable(vector_emul_weights, reachable_emul, "EMUL");
        }
        if (operation_weights[vector_segment] != 0) {
            const auto require_segment_weights = [](const auto &weights,
                                                     const char *dimension) {
                if (std::accumulate(
                        weights.begin(), weights.end(), std::uint64_t{0}) == 0) {
                    throw std::invalid_argument(
                        std::string("vector segment ") + dimension +
                        " constraint weights cannot all be zero");
                }
            };
            require_segment_weights(
                vector_segment_addressing_weights, "addressing");
            require_segment_weights(vector_segment_eew_weights, "EEW");
            require_segment_weights(vector_segment_sew_weights, "SEW");
            require_segment_weights(vector_segment_lmul_weights, "LMUL");
            require_segment_weights(vector_segment_emul_weights, "EMUL");
            require_segment_weights(vector_segment_nf_weights, "NF");

            std::array<bool, 4> reachable_addressing{};
            std::array<bool, 4> reachable_eew{};
            std::array<bool, 4> reachable_sew{};
            std::array<bool, 7> reachable_lmul{};
            std::array<bool, 7> reachable_emul{};
            std::array<bool, 7> reachable_nf{};
            for (unsigned addressing = 0; addressing < 4; ++addressing) {
                if (vector_segment_addressing_weights[addressing] == 0) {
                    continue;
                }
                const bool indexed = addressing >= 2;
                for (unsigned eew = 0; eew < 4; ++eew) {
                    if (vector_segment_eew_weights[eew] == 0) {
                        continue;
                    }
                    for (unsigned vsew = 0; vsew < 4; ++vsew) {
                        if (vector_segment_sew_weights[vsew] == 0) {
                            continue;
                        }
                        for (int lmul_log2 = -3; lmul_log2 <= 3;
                             ++lmul_log2) {
                            if (vector_segment_lmul_weights[
                                    lmul_log2 + 3] == 0) {
                                continue;
                            }
                            const int emul_log2 = static_cast<int>(eew) -
                                static_cast<int>(vsew) + lmul_log2;
                            if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                                emul_log2 < -3 || emul_log2 > 3 ||
                                vector_segment_emul_weights[
                                    emul_log2 + 3] == 0) {
                                continue;
                            }
                            const unsigned group_uops = 1U <<
                                static_cast<unsigned>(std::max(
                                    indexed ? lmul_log2 : emul_log2, 0));
                            for (unsigned nf = 0; nf < 7; ++nf) {
                                if (vector_segment_nf_weights[nf] != 0 &&
                                    group_uops * (nf + 2U) <= 8) {
                                    reachable_addressing[addressing] = true;
                                    reachable_eew[eew] = true;
                                    reachable_sew[vsew] = true;
                                    reachable_lmul[lmul_log2 + 3] = true;
                                    reachable_emul[emul_log2 + 3] = true;
                                    reachable_nf[nf] = true;
                                }
                            }
                        }
                    }
                }
            }
            const auto require_reachable = [](const auto &weights,
                                              const auto &reachable,
                                              const char *dimension) {
                for (unsigned index = 0; index < weights.size(); ++index) {
                    if (weights[index] != 0 && !reachable[index]) {
                        throw std::invalid_argument(
                            std::string("vector segment ") + dimension +
                            " constraint enables an unreachable class");
                    }
                }
            };
            require_reachable(
                vector_segment_addressing_weights, reachable_addressing,
                "addressing");
            require_reachable(
                vector_segment_eew_weights, reachable_eew, "EEW");
            require_reachable(
                vector_segment_sew_weights, reachable_sew, "SEW");
            require_reachable(
                vector_segment_lmul_weights, reachable_lmul, "LMUL");
            require_reachable(
                vector_segment_emul_weights, reachable_emul, "EMUL");
            require_reachable(
                vector_segment_nf_weights, reachable_nf, "NF");
        }
        if (std::accumulate(
                translation_weights.begin(), translation_weights.end(),
                0ULL) == 0) {
            throw std::invalid_argument(
                "translation regime constraint weights cannot all be zero");
        }
        if (translation_weights[translation_stage1] != 0 &&
            std::accumulate(
                stage1_mode_weights.begin(), stage1_mode_weights.end(),
                0ULL) == 0) {
            throw std::invalid_argument(
                "stage-1 mode constraint weights cannot all be zero");
        }
        if (translation_weights[translation_nested] != 0 &&
            (std::accumulate(vs_mode_weights.begin(), vs_mode_weights.end(),
                             0ULL) == 0 ||
             std::accumulate(g_mode_weights.begin(), g_mode_weights.end(),
                             0ULL) == 0)) {
            throw std::invalid_argument(
                "nested VS/G mode constraint weights cannot all be zero");
        }
        if (concurrent_actions_per_mille > 1000 ||
            special_concurrent_per_mille > 1000 ||
            translation_switches_per_mille > 1000 ||
            stage1_napot_per_mille > 1000 ||
            nested_vs_napot_per_mille > 1000 ||
            nested_g_napot_per_mille > 1000 ||
            tlb_flushes_per_mille > 1000 || misaligned_per_mille > 1000 ||
            vector_corner_per_mille > 1000 || probes_per_mille > 1000 ||
            vector_masked_per_mille > 1000 || vector_vma_per_mille > 1000 ||
            vector_vta_per_mille > 1000 ||
            vector_partial_vl_per_mille > 1000 ||
            vector_nonzero_vstart_per_mille > 1000 ||
            vector_segment_stores_per_mille > 1000 ||
            hypervisor_spvp_user_per_mille > 1000 ||
            hypervisor_pma_device_per_mille > 1000 ||
            atomic_error_per_mille > 1000 ||
            atomic_error_denied_per_mille > 1000 ||
            cmo_dirty_per_mille > 1000 ||
            cmo_younger_overlap_per_mille > 1000 ||
            cmo_error_per_mille > 1000 ||
            cmo_error_denied_per_mille > 1000 ||
            dcache_load_error_per_mille > 1000 ||
            dcache_load_error_denied_per_mille > 1000 ||
            ptw_error_stores_per_mille > 1000 ||
            ptw_error_denied_per_mille > 1000 ||
            ptw_error_corrupt_first_per_mille > 1000 ||
            set_pressure_dirty_per_mille > 1000 ||
            set_pressure_refill_overlap_per_mille > 1000 ||
            set_pressure_release_backpressure_per_mille > 1000 ||
            set_pressure_dual_window_per_mille > 1000 ||
            set_pressure_triple_window_per_mille > 1000 ||
            set_pressure_quad_window_per_mille > 1000 ||
            probe_to_b_per_mille > 1000 ||
            probe_need_data_per_mille > 1000 ||
            probe_overlap_per_mille > 1000 ||
            probe_triple_overlap_per_mille > 1000 ||
            nc_stores_per_mille > 1000 ||
            mmio_stores_per_mille > 1000 ||
            uncache_error_per_mille > 1000 ||
            uncache_load_error_denied_per_mille > 1000 ||
            stride_stream_per_mille > 1000) {
            throw std::invalid_argument(
                "per-mille random constraints must be in 0..1000");
        }
        if (response_latency.dcache == memblock::ResponseLatencyProfile::spec &&
            !uses_dcache()) {
            throw std::invalid_argument(
                "dcache-latency=spec requires a cacheable operation weight");
        }
        if (response_latency.ptw == memblock::ResponseLatencyProfile::spec &&
            (tlb_flushes_per_mille == 0 || !uses_translation())) {
            throw std::invalid_argument(
                "ptw-latency=spec requires translated traffic and a nonzero "
                "tlb-flush rate");
        }
        if (response_latency.uncache == memblock::ResponseLatencyProfile::spec &&
            !uses_uncache()) {
            throw std::invalid_argument(
                "uncache-latency=spec requires a nonzero nc or mmio weight");
        }
        if (special_concurrent_per_mille != 0 &&
            concurrent_actions_per_mille == 0) {
            throw std::invalid_argument(
                "special-concurrent requires a nonzero concurrent rate");
        }
        if (special_concurrent_per_mille != 0 &&
            !uses_concurrent_special_operations()) {
            throw std::invalid_argument(
                "special-concurrent requires nc or mmio traffic");
        }
        if (probes_per_mille != 0 && operation_weights[scalar_store] == 0) {
            throw std::invalid_argument(
                "probe requires a nonzero scalar-store weight");
        }
        if (probes_per_mille != 0 && probe_overlap_per_mille != 0 &&
            probe_triple_overlap_per_mille != 0 &&
            std::accumulate(
                probe_deep_depth_weights.begin(),
                probe_deep_depth_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "deep Probe depth constraint weights cannot all be zero");
        }
        if (cmo_error_per_mille != 0 && operation_weights[cmo] == 0) {
            throw std::invalid_argument(
                "cmo-error requires a nonzero CMO operation weight");
        }
        if (operation_weights[cmo] != 0 && cmo_error_per_mille != 1000 &&
            std::accumulate(
                cmo_probe_depth_weights.begin(),
                cmo_probe_depth_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "CMO Probe depth constraint weights cannot all be zero");
        }
        if (atomic_error_per_mille != 0 && operation_weights[atomic] == 0) {
            throw std::invalid_argument(
                "atomic-error requires a nonzero atomic operation weight");
        }
        if (operation_weights[atomic] != 0 && atomic_error_per_mille != 1000 &&
            std::accumulate(
                atomic_probe_depth_weights.begin(),
                atomic_probe_depth_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "atomic Probe depth constraint weights cannot all be zero");
        }
        if (dcache_load_error_per_mille != 0 &&
            operation_weights[scalar_load] == 0) {
            throw std::invalid_argument(
                "dcache-load-error requires a nonzero scalar-load weight");
        }
        if (dcache_load_error_per_mille == 1000 &&
            stride_stream_per_mille != 0) {
            throw std::invalid_argument(
                "dcache-load-error=1000 requires stride-stream=0 because "
                "stride-prefetch training needs clean scalar loads");
        }
        if (dcache_load_error_per_mille == 1000 &&
            concurrent_actions_per_mille != 0) {
            throw std::invalid_argument(
                "dcache-load-error=1000 requires concurrent=0 because "
                "faulting scalar loads redirect the mixed issue window");
        }
        if (uncache_error_per_mille != 0 && !uses_uncache()) {
            throw std::invalid_argument(
                "uncache-error requires a nonzero NC or MMIO operation weight");
        }
        if (uncache_error_per_mille == 1000 &&
            special_concurrent_per_mille != 0) {
            throw std::invalid_argument(
                "uncache-error=1000 requires special-concurrent=0 because "
                "faulting special accesses redirect the mixed issue window");
        }
        if (stride_stream_per_mille != 0 &&
            (operation_weights[scalar_load] == 0 ||
             locality_weights[2] == 0)) {
            throw std::invalid_argument(
                "stride-stream requires nonzero scalar-load and locality-cold "
                "weights");
        }
        if (tlb_flushes_per_mille != 0) {
            const bool has_stage1_fence =
                translation_weights[translation_stage1] != 0 &&
                fence_kind_weights[fence_sfence] != 0;
            const bool has_nested_fence =
                translation_weights[translation_nested] != 0 &&
                (fence_kind_weights[fence_hfence_vvma] != 0 ||
                 fence_kind_weights[fence_hfence_gvma] != 0);
            if (!has_stage1_fence && !has_nested_fence) {
                throw std::invalid_argument(
                    "tlb-flush requires a fence kind compatible with an "
                    "enabled translated regime");
            }
            if (std::accumulate(
                    fence_scope_weights.begin(), fence_scope_weights.end(),
                    0ULL) == 0) {
                throw std::invalid_argument(
                    "fence scope constraint weights cannot all be zero");
            }
        }
        if ((operation_weights[noncacheable] != 0 ||
             operation_weights[mmio] != 0) &&
            !uses_translation()) {
            throw std::invalid_argument(
                "NC/MMIO traffic requires stage-1 or nested PBMT translation "
                "at the MemBlock UT boundary");
        }
        if (operation_weights[hypervisor] != 0 &&
            translation_weights[translation_nested] == 0) {
            throw std::invalid_argument(
                "hypervisor traffic requires nested translation");
        }
        const bool non_hypervisor_napot_access = std::any_of(
            operation_weights.begin(), operation_weights.begin() + atomic,
            [](unsigned weight) { return weight != 0; });
        if (translation_weights[translation_stage1] != 0 &&
            stage1_napot_per_mille != 0 &&
            !non_hypervisor_napot_access) {
            throw std::invalid_argument(
                "host stage-1 NAPOT traffic requires a scalar, vector, "
                "segment, or prefetch operation weight");
        }
        if (translation_weights[translation_nested] != 0 &&
            (nested_vs_napot_per_mille != 0 ||
             nested_g_napot_per_mille != 0) &&
            !non_hypervisor_napot_access &&
            operation_weights[hypervisor] == 0) {
            throw std::invalid_argument(
                "nested NAPOT traffic requires a scalar, vector, segment, "
                "prefetch, or hypervisor operation weight");
        }
        const std::uint64_t non_hypervisor_weight = std::accumulate(
            operation_weights.begin(), operation_weights.end(), 0ULL) -
            operation_weights[hypervisor];
        if (non_hypervisor_weight == 0 &&
            (translation_weights[translation_bare] != 0 ||
             translation_weights[translation_stage1] != 0)) {
            throw std::invalid_argument(
                "hypervisor-only traffic cannot satisfy Bare or stage-1 "
                "translation coverage");
        }
        if (translation_weights[translation_bare] != 0 &&
            (operation_weights[noncacheable] != 0 ||
             operation_weights[mmio] != 0) &&
            std::accumulate(
                operation_weights.begin(),
                operation_weights.begin() + noncacheable,
                0ULL) == 0) {
            throw std::invalid_argument(
                "NC/MMIO-only traffic cannot satisfy Bare translation coverage "
                "at the MemBlock UT boundary");
        }
    }

    unsigned choose_operation(std::uint64_t random) const
    {
        const std::uint64_t total = std::accumulate(
            operation_weights.begin(), operation_weights.end(), 0ULL);
        std::uint64_t selection = random % total;
        for (unsigned operation = 0; operation < operation_count; ++operation) {
            if (selection < operation_weights[operation]) {
                return operation;
            }
            selection -= operation_weights[operation];
        }
        return scalar_load;
    }

    unsigned choose_locality(std::uint64_t random) const
    {
        const std::uint64_t total = std::accumulate(
            locality_weights.begin(), locality_weights.end(), 0ULL);
        std::uint64_t selection = random % total;
        for (unsigned locality = 0; locality < locality_weights.size(); ++locality) {
            if (selection < locality_weights[locality]) {
                return locality;
            }
            selection -= locality_weights[locality];
        }
        return 0;
    }

    unsigned choose_atomic_family(std::uint64_t random) const
    {
        return choose_weighted(atomic_family_weights, random);
    }

    unsigned choose_atomic_width(std::uint64_t random) const
    {
        return choose_weighted(atomic_width_weights, random);
    }

    unsigned choose_hypervisor_family(std::uint64_t random) const
    {
        return choose_weighted(hypervisor_family_weights, random);
    }

    unsigned choose_hypervisor_pbmt_pair(std::uint64_t random) const
    {
        return choose_weighted(hypervisor_pbmt_pair_weights, random);
    }

    unsigned choose_hypervisor_pmp_relation(std::uint64_t random) const
    {
        return choose_weighted(hypervisor_pmp_relation_weights, random);
    }

    unsigned choose_cmo_operation(std::uint64_t random) const
    {
        return choose_weighted(cmo_operation_weights, random);
    }

    unsigned choose_atomic_probe_depth(std::uint64_t random) const
    {
        return choose_weighted(atomic_probe_depth_weights, random);
    }

    unsigned choose_cmo_probe_depth(std::uint64_t random) const
    {
        return choose_weighted(cmo_probe_depth_weights, random);
    }

    unsigned choose_ptw_error_site(std::uint64_t random) const
    {
        return choose_weighted(ptw_error_site_weights, random);
    }

    unsigned choose_ptw_error_level(std::uint64_t random) const
    {
        return choose_weighted(ptw_error_level_weights, random);
    }

    unsigned choose_load_merge_depth(std::uint64_t random) const
    {
        return choose_weighted(load_merge_depth_weights, random);
    }

    unsigned choose_load_merge_pattern(std::uint64_t random) const
    {
        return choose_weighted(load_merge_pattern_weights, random);
    }

    unsigned choose_set_pressure_depth(std::uint64_t random) const
    {
        return choose_weighted(set_pressure_depth_weights, random);
    }

    unsigned choose_set_pressure_width(std::uint64_t random) const
    {
        return choose_weighted(set_pressure_width_weights, random);
    }

    unsigned choose_set_pressure_set(std::uint64_t random) const
    {
        return choose_weighted(set_pressure_set_weights, random);
    }

    std::array<unsigned, set_pressure_window_class_count>
    set_pressure_window_weights() const
    {
        const unsigned non_quad =
            1000U - set_pressure_quad_window_per_mille;
        const unsigned non_triple =
            1000U - set_pressure_triple_window_per_mille;
        return {{
            non_quad * non_triple *
                (1000U - set_pressure_dual_window_per_mille),
            non_quad * non_triple * set_pressure_dual_window_per_mille,
            non_quad * set_pressure_triple_window_per_mille * 1000U,
            set_pressure_quad_window_per_mille * 1000000U,
        }};
    }

    unsigned choose_set_pressure_window(std::uint64_t random) const
    {
        return choose_weighted(set_pressure_window_weights(), random);
    }

    bool set_pressure_window_enabled(unsigned window_class) const
    {
        return set_pressure_window_weights().at(window_class) != 0;
    }

    unsigned enabled_set_pressure_window_classes() const
    {
        const auto weights = set_pressure_window_weights();
        return static_cast<unsigned>(std::count_if(
            weights.begin(), weights.end(),
            [](unsigned weight) { return weight != 0; }));
    }

    unsigned choose_probe_depth(std::uint64_t random) const
    {
        if (random % 1000U >= probe_overlap_per_mille) {
            return 0;
        }
        random /= 1000U;
        if (random % 1000U >= probe_triple_overlap_per_mille) {
            return 1;
        }
        return 2U + choose_weighted(probe_deep_depth_weights, random / 1000U);
    }

    bool probe_depth_enabled(unsigned depth_class) const
    {
        if (depth_class == 0) {
            return probe_overlap_per_mille != 1000;
        }
        if (depth_class == 1) {
            return probe_overlap_per_mille != 0 &&
                probe_triple_overlap_per_mille != 1000;
        }
        return probe_overlap_per_mille != 0 &&
            probe_triple_overlap_per_mille != 0 &&
            probe_deep_depth_weights.at(depth_class - 2U) != 0;
    }

    bool probe_cross_enabled(
        unsigned depth_class, bool to_b, bool need_data) const
    {
        const bool cap_enabled = to_b
            ? probe_to_b_per_mille != 0
            : probe_to_b_per_mille != 1000;
        const bool data_enabled = need_data
            ? probe_need_data_per_mille != 0
            : probe_need_data_per_mille != 1000;
        return probes_per_mille != 0 && probe_depth_enabled(depth_class) &&
            cap_enabled && data_enabled;
    }

    unsigned maximum_enabled_probe_depth() const
    {
        for (unsigned depth = memblock::kDcacheProbeEntries; depth != 0;
             --depth) {
            if (probe_depth_enabled(depth - 1U)) {
                return depth;
            }
        }
        return 0;
    }

    unsigned choose_translation_regime(std::uint64_t random) const
    {
        return choose_weighted(translation_weights, random);
    }

    unsigned choose_stage1_mode(std::uint64_t random) const
    {
        return choose_weighted(stage1_mode_weights, random);
    }

    unsigned choose_vs_mode(std::uint64_t random) const
    {
        return choose_weighted(vs_mode_weights, random);
    }

    unsigned choose_g_mode(std::uint64_t random) const
    {
        return choose_weighted(g_mode_weights, random);
    }

    bool choose_stage1_napot(std::uint64_t random) const
    {
        return random % 1000 < stage1_napot_per_mille;
    }

    bool choose_nested_vs_napot(std::uint64_t random) const
    {
        return random % 1000 < nested_vs_napot_per_mille;
    }

    bool choose_nested_g_napot(std::uint64_t random) const
    {
        return random % 1000 < nested_g_napot_per_mille;
    }

    unsigned choose_fence_scope(std::uint64_t random) const
    {
        return choose_weighted(fence_scope_weights, random);
    }

    unsigned choose_fence_kind(
        unsigned translation_regime, std::uint64_t random) const
    {
        std::array<unsigned, fence_kind_count> compatible{};
        if (translation_regime == translation_stage1) {
            compatible[fence_sfence] = fence_kind_weights[fence_sfence];
        } else if (translation_regime == translation_nested) {
            compatible[fence_hfence_vvma] =
                fence_kind_weights[fence_hfence_vvma];
            compatible[fence_hfence_gvma] =
                fence_kind_weights[fence_hfence_gvma];
        }
        return choose_weighted(compatible, random);
    }

    bool has_compatible_fence_kind(unsigned translation_regime) const
    {
        if (translation_regime == translation_stage1) {
            return fence_kind_weights[fence_sfence] != 0;
        }
        if (translation_regime == translation_nested) {
            return fence_kind_weights[fence_hfence_vvma] != 0 ||
                fence_kind_weights[fence_hfence_gvma] != 0;
        }
        return false;
    }

    unsigned choose_concurrent_special_operation(std::uint64_t random) const
    {
        const std::array<unsigned, 2> weights{{
            operation_weights[noncacheable], operation_weights[mmio]}};
        return noncacheable + choose_weighted(weights, random);
    }

    bool uses_concurrent_special_operations() const
    {
        return operation_weights[noncacheable] != 0 ||
            operation_weights[mmio] != 0;
    }

    unsigned minimum_serial_actions() const
    {
        const auto uncache_outcome_actions = [&](unsigned store_per_mille) {
            const bool loads_enabled = store_per_mille != 1000;
            const bool stores_enabled = store_per_mille != 0;
            if (uncache_error_per_mille == 0) {
                return direction_classes(store_per_mille);
            }
            unsigned outcomes = 0;
            if (loads_enabled) {
                outcomes += uncache_error_per_mille == 1000 ? 0U : 1U;
                outcomes +=
                    uncache_load_error_denied_per_mille == 1000 ? 0U : 1U;
                outcomes +=
                    uncache_load_error_denied_per_mille == 0 ? 0U : 1U;
            }
            if (stores_enabled) {
                outcomes += uncache_error_per_mille == 1000 ? 0U : 1U;
                ++outcomes;
            }
            return std::max(direction_classes(store_per_mille), outcomes);
        };
        unsigned actions = 0;
        unsigned uncache_actions = 0;
        for (unsigned operation = 0; operation < operation_count; ++operation) {
            if (operation_weights[operation] == 0) {
                continue;
            }
            if (operation == atomic) {
                const unsigned families = static_cast<unsigned>(std::count_if(
                    atomic_family_weights.begin(), atomic_family_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned widths = static_cast<unsigned>(std::count_if(
                    atomic_width_weights.begin(), atomic_width_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned success_depths = atomic_error_per_mille == 1000
                    ? 0U
                    : static_cast<unsigned>(std::count_if(
                          atomic_probe_depth_weights.begin(),
                          atomic_probe_depth_weights.end(),
                          [](unsigned weight) { return weight != 0; }));
                const unsigned error_kinds =
                    (atomic_error_per_mille != 0 &&
                         atomic_error_denied_per_mille != 1000
                     ? 1U : 0U) +
                    (atomic_error_per_mille != 0 &&
                         atomic_error_denied_per_mille != 0
                     ? 1U : 0U);
                actions += families * widths * (success_depths + error_kinds);
            } else if (operation == scalar_load) {
                const unsigned error_kinds =
                    dcache_load_error_per_mille == 0 ? 0U :
                    (dcache_load_error_denied_per_mille == 0 ||
                     dcache_load_error_denied_per_mille == 1000 ? 1U : 2U);
                actions += error_kinds +
                    (dcache_load_error_per_mille == 1000 ? 0U : 1U);
            } else if (operation == hypervisor) {
                const unsigned families = static_cast<unsigned>(std::count_if(
                    hypervisor_family_weights.begin(),
                    hypervisor_family_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned pbmt_pairs = static_cast<unsigned>(std::count_if(
                    hypervisor_pbmt_pair_weights.begin(),
                    hypervisor_pbmt_pair_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned alignment_classes =
                    direction_classes(misaligned_per_mille);
                const unsigned address_classes =
                    direction_classes(hypervisor_pma_device_per_mille);
                const unsigned pmp_relations =
                    static_cast<unsigned>(std::count_if(
                        hypervisor_pmp_relation_weights.begin(),
                        hypervisor_pmp_relation_weights.end(),
                        [](unsigned weight) { return weight != 0; }));
                // PMP edge actions are fixed to DDR, PMA/PMA, and a
                // relation-specific alignment. Reserve independent no-PMP
                // actions for every other enabled PBMT/address/alignment
                // class rather than assuming all dimensions can overlap.
                const unsigned cross_actions = pmp_relations + pbmt_pairs +
                    alignment_classes + address_classes;
                actions += families *
                    direction_classes(hypervisor_spvp_user_per_mille) *
                    cross_actions;
            } else if (operation == noncacheable) {
                uncache_actions +=
                    uncache_outcome_actions(nc_stores_per_mille);
            } else if (operation == mmio) {
                uncache_actions +=
                    uncache_outcome_actions(mmio_stores_per_mille);
            } else if (operation == cmo) {
                const unsigned operations = static_cast<unsigned>(std::count_if(
                    cmo_operation_weights.begin(), cmo_operation_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned probe_depths = static_cast<unsigned>(std::count_if(
                    cmo_probe_depth_weights.begin(),
                    cmo_probe_depth_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned error_kinds =
                    cmo_error_per_mille == 0 ? 0U :
                    (cmo_error_denied_per_mille == 0 ||
                     cmo_error_denied_per_mille == 1000 ? 1U : 2U);
                const unsigned success_actions = cmo_error_per_mille == 1000
                    ? 0U
                    : operations * direction_classes(cmo_dirty_per_mille) *
                        probe_depths;
                const unsigned error_actions =
                    operations * error_kinds;
                actions += std::max(
                    success_actions + error_actions,
                    direction_classes(cmo_younger_overlap_per_mille));
            } else if (operation == ptw_error) {
                const unsigned sites = static_cast<unsigned>(std::count_if(
                    ptw_error_site_weights.begin(),
                    ptw_error_site_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned levels = static_cast<unsigned>(std::count_if(
                    ptw_error_level_weights.begin(),
                    ptw_error_level_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned corrupt_outcomes =
                    ptw_error_denied_per_mille == 1000
                    ? 0U
                    : direction_classes(
                          ptw_error_corrupt_first_per_mille);
                const unsigned outcomes =
                    (ptw_error_denied_per_mille == 0 ? 0U : 1U) +
                    corrupt_outcomes;
                actions += sites * direction_classes(
                    ptw_error_stores_per_mille) * levels * outcomes;
            } else if (operation == load_merge) {
                const unsigned depths = static_cast<unsigned>(std::count_if(
                    load_merge_depth_weights.begin(),
                    load_merge_depth_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned patterns = static_cast<unsigned>(std::count_if(
                    load_merge_pattern_weights.begin(),
                    load_merge_pattern_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                actions += depths * patterns * 2;
            } else if (operation == set_pressure) {
                const unsigned depths = static_cast<unsigned>(std::count_if(
                    set_pressure_depth_weights.begin(),
                    set_pressure_depth_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned widths = static_cast<unsigned>(std::count_if(
                    set_pressure_width_weights.begin(),
                    set_pressure_width_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                const unsigned regimes = static_cast<unsigned>(std::count_if(
                    translation_weights.begin(), translation_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                actions += direction_classes(set_pressure_dirty_per_mille) *
                    direction_classes(set_pressure_refill_overlap_per_mille) *
                    direction_classes(
                        set_pressure_release_backpressure_per_mille) *
                    enabled_set_pressure_window_classes() *
                    depths * widths * regimes;
            } else if (operation == vector_load || operation == vector_store) {
                ++actions;
                if (operation == vector_load ||
                    operation_weights[vector_load] == 0) {
                    const auto enabled = [](const auto &weights) {
                        return static_cast<unsigned>(std::count_if(
                            weights.begin(), weights.end(),
                            [](unsigned weight) { return weight != 0; }));
                    };
                    actions += enabled(vector_addressing_weights);
                    actions += enabled(vector_eew_weights);
                    actions += enabled(vector_sew_weights);
                    actions += enabled(vector_lmul_weights);
                    actions += enabled(vector_emul_weights);
                    actions += direction_classes(vector_masked_per_mille);
                    actions += direction_classes(vector_vma_per_mille);
                    actions += direction_classes(vector_vta_per_mille);
                    actions += direction_classes(vector_partial_vl_per_mille);
                    actions += direction_classes(vector_nonzero_vstart_per_mille);
                }
            } else if (operation == vector_segment) {
                const auto enabled = [](const auto &weights) {
                    return static_cast<unsigned>(std::count_if(
                        weights.begin(), weights.end(),
                        [](unsigned weight) { return weight != 0; }));
                };
                actions += direction_classes(
                    vector_segment_stores_per_mille);
                actions += enabled(vector_segment_addressing_weights);
                actions += enabled(vector_segment_eew_weights);
                actions += enabled(vector_segment_sew_weights);
                actions += enabled(vector_segment_lmul_weights);
                actions += enabled(vector_segment_emul_weights);
                actions += enabled(vector_segment_nf_weights);
            } else {
                ++actions;
            }
        }
        const unsigned uncache_latency_actions =
            response_latency.uncache == memblock::ResponseLatencyProfile::spec &&
                    uses_uncache()
                ? 4U
                : 0U;
        actions += std::max(uncache_actions, uncache_latency_actions);
        unsigned translation_actions =
            translation_weights[translation_bare] != 0;
        if (translation_weights[translation_stage1] != 0) {
            const unsigned modes = static_cast<unsigned>(std::count_if(
                stage1_mode_weights.begin(), stage1_mode_weights.end(),
                [](unsigned weight) { return weight != 0; }));
            translation_actions += std::max(
                modes, direction_classes(stage1_napot_per_mille));
        }
        if (translation_weights[translation_nested] != 0) {
            const unsigned vs_modes = static_cast<unsigned>(std::count_if(
                vs_mode_weights.begin(), vs_mode_weights.end(),
                [](unsigned weight) { return weight != 0; }));
            const unsigned g_modes = static_cast<unsigned>(std::count_if(
                g_mode_weights.begin(), g_mode_weights.end(),
                [](unsigned weight) { return weight != 0; }));
            const unsigned leaf_topologies =
                direction_classes(nested_vs_napot_per_mille) *
                direction_classes(nested_g_napot_per_mille);
            translation_actions += std::max(
                vs_modes * g_modes, leaf_topologies);
        }
        unsigned fence_actions = 0;
        if (tlb_flushes_per_mille != 0) {
            const unsigned scopes = static_cast<unsigned>(std::count_if(
                fence_scope_weights.begin(), fence_scope_weights.end(),
                [](unsigned weight) { return weight != 0; }));
            if (translation_weights[translation_stage1] != 0 &&
                fence_kind_weights[fence_sfence] != 0) {
                fence_actions += scopes;
            }
            if (translation_weights[translation_nested] != 0) {
                fence_actions += scopes * static_cast<unsigned>(
                    (fence_kind_weights[fence_hfence_vvma] != 0) +
                    (fence_kind_weights[fence_hfence_gvma] != 0));
            }
        }
        return std::max({actions, translation_actions, fence_actions}) +
            (stride_stream_per_mille == 0 ? 0U : 8U);
    }

    bool uses_dcache() const
    {
        return std::any_of(
            operation_weights.begin(), operation_weights.begin() + noncacheable,
            [](unsigned weight) { return weight != 0; }) ||
            operation_weights[hypervisor] != 0 ||
            operation_weights[cmo] != 0 ||
            operation_weights[load_merge] != 0 ||
            operation_weights[set_pressure] != 0;
    }

    bool uses_uncache() const
    {
        return operation_weights[noncacheable] != 0 ||
            operation_weights[mmio] != 0;
    }

    bool uses_locality() const
    {
        return std::any_of(
                   operation_weights.begin(),
                   operation_weights.begin() + atomic,
                   [](unsigned weight) { return weight != 0; }) ||
            operation_weights[hypervisor] != 0 ||
            operation_weights[cmo] != 0;
    }

    bool uses_translation() const
    {
        return translation_weights[translation_stage1] != 0 ||
            translation_weights[translation_nested] != 0;
    }

    bool samples_translation() const
    {
        return std::any_of(
            operation_weights.begin(), operation_weights.begin() + ptw_error,
            [](unsigned weight) { return weight != 0; }) ||
            operation_weights[load_merge] != 0 ||
            operation_weights[set_pressure] != 0;
    }

    unsigned vector_policy_minimum_vlmax() const
    {
        const bool masked = vector_masked_per_mille == 1000;
        const bool partial = vector_partial_vl_per_mille == 1000;
        const bool nonzero_vstart = vector_nonzero_vstart_per_mille == 1000;
        if (masked && partial && nonzero_vstart) {
            return 3;
        }
        if ((masked && partial) || (partial && nonzero_vstart) ||
            (masked && nonzero_vstart)) {
            return 2;
        }
        return 1;
    }

    std::string summary() const
    {
        std::ostringstream stream;
        stream << "constraint_schema=37 constraints=" << name
               << " target_ops=";
        for (std::size_t index = 0; index < operation_weights.size(); ++index) {
            stream << (index == 0 ? "" : ",") << operation_weights[index];
        }
        stream << " target_locality=" << locality_weights[0] << ','
               << locality_weights[1] << ',' << locality_weights[2]
               << " target_atomic_family=" << atomic_family_weights[0] << ','
               << atomic_family_weights[1] << ',' << atomic_family_weights[2]
               << " target_atomic_width=" << atomic_width_weights[0] << ','
               << atomic_width_weights[1]
               << " target_atomic_error=" << atomic_error_per_mille
               << " target_atomic_error_denied="
               << atomic_error_denied_per_mille
               << " target_atomic_probe_depth=";
        append_weights(stream, atomic_probe_depth_weights);
        stream << " target_hypervisor_family="
               << hypervisor_family_weights[0] << ','
               << hypervisor_family_weights[1] << ','
               << hypervisor_family_weights[2]
               << " target_hypervisor_spvp_user="
               << hypervisor_spvp_user_per_mille
               << " target_hypervisor_pbmt_pair=";
        append_weights(stream, hypervisor_pbmt_pair_weights);
        stream << " target_hypervisor_pma_device="
               << hypervisor_pma_device_per_mille
               << " target_hypervisor_pmp_relation=";
        append_weights(stream, hypervisor_pmp_relation_weights);
        stream
               << " target_cmo_operation=" << cmo_operation_weights[0] << ','
               << cmo_operation_weights[1] << ',' << cmo_operation_weights[2]
               << " target_cmo_dirty=" << cmo_dirty_per_mille
               << " target_cmo_younger_overlap="
               << cmo_younger_overlap_per_mille
               << " target_cmo_error=" << cmo_error_per_mille
               << " target_cmo_error_denied="
               << cmo_error_denied_per_mille
               << " target_cmo_probe_depth=";
        append_weights(stream, cmo_probe_depth_weights);
        stream << " target_dcache_load_error="
               << dcache_load_error_per_mille
               << " target_dcache_load_error_denied="
               << dcache_load_error_denied_per_mille
               << " target_ptw_error_site=";
        for (unsigned site = 0; site < ptw_error_site_weights.size(); ++site) {
            stream << (site == 0 ? "" : ",")
                   << ptw_error_site_weights[site];
        }
        stream << " target_ptw_error_level="
               << ptw_error_level_weights[0] << ','
               << ptw_error_level_weights[1] << ','
               << ptw_error_level_weights[2]
               << " target_ptw_error_store="
               << ptw_error_stores_per_mille
               << " target_ptw_error_denied="
               << ptw_error_denied_per_mille
               << " target_ptw_error_corrupt_first="
               << ptw_error_corrupt_first_per_mille
               << " target_load_merge_depth="
               << load_merge_depth_weights[0] << ','
               << load_merge_depth_weights[1]
               << " target_load_merge_pattern="
               << load_merge_pattern_weights[0] << ','
               << load_merge_pattern_weights[1] << ','
               << load_merge_pattern_weights[2]
               << " target_set_pressure_depth="
               << set_pressure_depth_weights[0] << ','
               << set_pressure_depth_weights[1]
               << " target_set_pressure_width="
               << set_pressure_width_weights[0] << ','
               << set_pressure_width_weights[1] << ','
               << set_pressure_width_weights[2] << ','
               << set_pressure_width_weights[3]
               << " target_set_pressure_set="
               << set_pressure_set_weights[0] << ','
               << set_pressure_set_weights[1] << ','
               << set_pressure_set_weights[2] << ','
               << set_pressure_set_weights[3]
               << " target_set_pressure_dirty="
               << set_pressure_dirty_per_mille
               << " target_set_pressure_refill_overlap="
               << set_pressure_refill_overlap_per_mille
               << " target_set_pressure_release_backpressure="
               << set_pressure_release_backpressure_per_mille
               << " target_set_pressure_dual_window="
               << set_pressure_dual_window_per_mille
               << " target_set_pressure_triple_window="
               << set_pressure_triple_window_per_mille
               << " target_set_pressure_quad_window="
               << set_pressure_quad_window_per_mille
               << " target_translation=" << translation_weights[0] << ','
               << translation_weights[1] << ',' << translation_weights[2]
               << " target_stage1_mode=" << stage1_mode_weights[0] << ','
               << stage1_mode_weights[1]
               << " target_vs_mode=" << vs_mode_weights[0] << ','
               << vs_mode_weights[1]
               << " target_g_mode=" << g_mode_weights[0] << ','
               << g_mode_weights[1]
               << " target_stage1_napot=" << stage1_napot_per_mille
               << " target_nested_vs_napot=" << nested_vs_napot_per_mille
               << " target_nested_g_napot=" << nested_g_napot_per_mille
               << " target_fence_kind=" << fence_kind_weights[0] << ','
               << fence_kind_weights[1] << ',' << fence_kind_weights[2]
               << " target_fence_scope=" << fence_scope_weights[0] << ','
               << fence_scope_weights[1]
               << " target_concurrent=" << concurrent_actions_per_mille
               << " target_special_concurrent="
               << special_concurrent_per_mille
               << " target_translation_switch="
               << translation_switches_per_mille
               << " target_tlb_flush=" << tlb_flushes_per_mille
               << " target_misaligned=" << misaligned_per_mille
               << " target_vector_corner=" << vector_corner_per_mille
               << " target_vector_masked=" << vector_masked_per_mille
               << " target_vector_vma=" << vector_vma_per_mille
               << " target_vector_vta=" << vector_vta_per_mille
               << " target_vector_partial_vl=" << vector_partial_vl_per_mille
               << " target_vector_nonzero_vstart="
               << vector_nonzero_vstart_per_mille
               << " target_vector_addressing=";
        append_weights(stream, vector_addressing_weights);
        stream << " target_vector_eew=";
        append_weights(stream, vector_eew_weights);
        stream << " target_vector_sew=";
        append_weights(stream, vector_sew_weights);
        stream << " target_vector_lmul=";
        append_weights(stream, vector_lmul_weights);
        stream << " target_vector_emul=";
        append_weights(stream, vector_emul_weights);
        stream
               << " target_vector_segment_store="
               << vector_segment_stores_per_mille
               << " target_vector_segment_addressing=";
        append_weights(stream, vector_segment_addressing_weights);
        stream << " target_vector_segment_eew=";
        append_weights(stream, vector_segment_eew_weights);
        stream << " target_vector_segment_sew=";
        append_weights(stream, vector_segment_sew_weights);
        stream << " target_vector_segment_lmul=";
        append_weights(stream, vector_segment_lmul_weights);
        stream << " target_vector_segment_emul=";
        append_weights(stream, vector_segment_emul_weights);
        stream << " target_vector_segment_nf=";
        append_weights(stream, vector_segment_nf_weights);
        stream
               << " target_probe=" << probes_per_mille
               << " target_probe_to_b=" << probe_to_b_per_mille
               << " target_probe_need_data="
               << probe_need_data_per_mille
               << " target_probe_overlap=" << probe_overlap_per_mille
               << " target_probe_triple_overlap="
               << probe_triple_overlap_per_mille
               << " target_probe_deep_depth=";
        append_weights(stream, probe_deep_depth_weights);
        stream
               << " target_nc_store=" << nc_stores_per_mille
               << " target_mmio_store=" << mmio_stores_per_mille
               << " target_uncache_error=" << uncache_error_per_mille
               << " target_uncache_load_error_denied="
               << uncache_load_error_denied_per_mille
               << " target_stride_stream=" << stride_stream_per_mille
               << " target_latency=" << latency_name(response_latency.dcache)
               << ',' << latency_name(response_latency.ptw) << ','
               << latency_name(response_latency.uncache);
        return stream.str();
    }

private:
    template <std::size_t N>
    static void append_weights(
        std::ostringstream &stream, const std::array<unsigned, N> &weights)
    {
        for (unsigned index = 0; index < weights.size(); ++index) {
            stream << (index == 0 ? "" : ",") << weights[index];
        }
    }

    template <std::size_t N>
    static unsigned choose_weighted(
        const std::array<unsigned, N> &weights, std::uint64_t random)
    {
        const std::uint64_t total =
            std::accumulate(weights.begin(), weights.end(), 0ULL);
        std::uint64_t selection = random % total;
        for (unsigned index = 0; index < weights.size(); ++index) {
            if (selection < weights[index]) {
                return index;
            }
            selection -= weights[index];
        }
        return 0;
    }

    static unsigned direction_classes(unsigned stores_per_mille)
    {
        return stores_per_mille == 0 || stores_per_mille == 1000 ? 1 : 2;
    }

    static const char *latency_name(memblock::ResponseLatencyProfile profile)
    {
        return profile == memblock::ResponseLatencyProfile::compact
            ? "compact" : "spec";
    }
};

RandomConstraints resolve_random_constraints(const Options &options)
{
    RandomConstraints constraints =
        RandomConstraints::preset(options.constraint_profile);
    for (const auto &assignment : options.constraint_overrides) {
        constraints.apply(assignment);
    }
    constraints.validate();
    return constraints;
}

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
        } else if (argument == "--constraints" && index + 1 < argc) {
            options.constraint_profile = argv[++index];
        } else if (argument == "--constraint" && index + 1 < argc) {
            options.constraint_overrides.emplace_back(argv[++index]);
        }
    }
    return options;
}

struct LoadCoverage {
    std::array<std::uint64_t, 7> operations{};
    std::array<std::uint64_t, memblock::kScalarLoadLanes> lanes{};
    std::uint64_t cache_hits = 0;
    std::uint64_t cache_misses = 0;

    void sample(
        const memblock::LoadTransaction &transaction,
        std::uint64_t requests_before,
        std::uint64_t requests_after)
    {
        ++operations.at(static_cast<unsigned>(transaction.op));
        ++lanes.at(transaction.lane);
        if (requests_after == requests_before) {
            ++cache_hits;
        } else {
            ++cache_misses;
        }
    }

    bool complete() const
    {
        for (const auto count : operations) {
            if (count == 0) {
                return false;
            }
        }
        for (const auto count : lanes) {
            if (count == 0) {
                return false;
            }
        }
        return cache_hits != 0 && cache_misses != 0;
    }

    std::string summary() const
    {
        std::string result = "ops=";
        for (std::size_t index = 0; index < operations.size(); ++index) {
            result += (index == 0 ? "" : ",") + std::to_string(operations[index]);
        }
        result += " lanes=";
        for (std::size_t index = 0; index < lanes.size(); ++index) {
            result += (index == 0 ? "" : ",") + std::to_string(lanes[index]);
        }
        result += " hits=" + std::to_string(cache_hits);
        result += " misses=" + std::to_string(cache_misses);
        return result;
    }
};

struct VectorCoverage {
    std::array<std::uint64_t, 4> eews{};
    std::array<std::uint64_t, memblock::kVectorMemoryLanes> lanes{};
    std::uint64_t masked = 0;
    std::uint64_t unmasked = 0;
    std::uint64_t zero_vstart = 0;
    std::uint64_t nonzero_vstart = 0;
    std::uint64_t full_vl = 0;
    std::uint64_t partial_vl = 0;
    std::uint64_t aligned = 0;
    std::uint64_t split = 0;
    std::uint64_t active = 0;
    std::uint64_t inactive = 0;
    std::uint64_t cache_hits = 0;
    std::uint64_t cache_misses = 0;

    void sample(
        const memblock::VectorMemoryTransaction &transaction,
        std::uint64_t requests_before,
        std::uint64_t requests_after)
    {
        ++eews.at(transaction.eew);
        ++lanes.at(transaction.lane);
        ++(transaction.vm ? unmasked : masked);
        ++(transaction.vstart == 0 ? zero_vstart : nonzero_vstart);
        const unsigned element_count = 16U >> transaction.eew;
        ++(transaction.vl == element_count ? full_vl : partial_vl);
        ++((transaction.address & 15U) == 0 ? aligned : split);
        ++(memblock::active_vector_elements(transaction) == 0 ? inactive : active);
        ++(requests_after == requests_before ? cache_hits : cache_misses);
    }

    bool complete(bool require_cache_mix = true) const
    {
        return std::all_of(eews.begin(), eews.end(), [](auto count) { return count != 0; }) &&
               std::all_of(lanes.begin(), lanes.end(), [](auto count) { return count != 0; }) &&
               masked != 0 && unmasked != 0 && zero_vstart != 0 &&
               nonzero_vstart != 0 && full_vl != 0 && partial_vl != 0 &&
               aligned != 0 && split != 0 && active != 0 && inactive != 0 &&
               (!require_cache_mix || (cache_hits != 0 && cache_misses != 0));
    }

    std::string summary() const
    {
        return "eews=" + std::to_string(eews[0]) + ',' +
               std::to_string(eews[1]) + ',' + std::to_string(eews[2]) + ',' +
               std::to_string(eews[3]) + " lanes=" + std::to_string(lanes[0]) +
               ',' + std::to_string(lanes[1]) + " masked=" +
               std::to_string(masked) + " unmasked=" + std::to_string(unmasked) +
               " vstart0=" + std::to_string(zero_vstart) + " vstartnz=" +
               std::to_string(nonzero_vstart) + " full=" + std::to_string(full_vl) +
               " partial=" + std::to_string(partial_vl) + " aligned=" +
               std::to_string(aligned) + " split=" + std::to_string(split) +
               " active=" + std::to_string(active) + " inactive=" +
               std::to_string(inactive) + " hits=" + std::to_string(cache_hits) +
               " misses=" + std::to_string(cache_misses);
    }
};

struct ConstraintCoverage {
    std::array<std::uint64_t, RandomConstraints::operation_count> operations{};
    std::array<std::uint64_t, 3> locality{};
    std::array<std::uint64_t, RandomConstraints::atomic_family_count>
        atomic_families{};
    std::array<std::uint64_t, 2> atomic_widths{};
    std::array<std::uint64_t, 2> atomic_errors{};
    std::array<std::uint64_t, 2> atomic_error_kinds{};
    // [AMO/LRSC/CAS][W/D][clean/corrupt/denied].
    std::array<std::array<std::array<std::uint64_t, 3>, 2>,
               RandomConstraints::atomic_family_count>
        atomic_outcomes{};
    // error responses/denied beats/corrupt beats/GrantAcks/refills.
    std::array<std::uint64_t, 5> atomic_error_manager{};
    std::array<std::uint64_t, RandomConstraints::atomic_probe_depth_count>
        atomic_probe_depths{};
    // [AMO/LRSC/AMOCAS][W/D][zero through eight concurrent Probes].
    std::array<
        std::array<
            std::array<
                std::uint64_t, RandomConstraints::atomic_probe_depth_count>,
            2>,
        RandomConstraints::atomic_family_count>
        atomic_probe_crosses{};
    std::array<std::uint64_t, RandomConstraints::hypervisor_family_count>
        hypervisor_families{};
    std::array<std::uint64_t, 2> hypervisor_spvp{};
    // [HLV/HLVX/HSV][SPVP=S/U].
    std::array<std::array<std::uint64_t, 2>,
               RandomConstraints::hypervisor_family_count>
        hypervisor_crosses{};
    std::array<std::uint64_t, 2> hypervisor_alignments{};
    // [HLV/HLVX/HSV][SPVP=S/U][aligned/misaligned].
    std::array<std::array<std::array<std::uint64_t, 2>, 2>,
               RandomConstraints::hypervisor_family_count>
        hypervisor_alignment_crosses{};
    std::array<std::uint64_t, RandomConstraints::hypervisor_pbmt_pair_count>
        hypervisor_pbmt_pairs{};
    // [HLV/HLVX/HSV][SPVP=S/U][VS/G PBMT pair].
    std::array<
        std::array<
            std::array<
                std::uint64_t, RandomConstraints::hypervisor_pbmt_pair_count>,
            2>,
        RandomConstraints::hypervisor_family_count>
        hypervisor_pbmt_crosses{};
    std::array<std::uint64_t, 2> hypervisor_pma_devices{};
    // [HLV/HLVX/HSV][SPVP=S/U][DDR/fixed-PMA device].
    std::array<std::array<std::array<std::uint64_t, 2>, 2>,
               RandomConstraints::hypervisor_family_count>
        hypervisor_pma_device_crosses{};
    std::array<
        std::uint64_t, RandomConstraints::hypervisor_pmp_relation_count>
        hypervisor_pmp_relations{};
    // [HLV/HLVX/HSV][SPVP=S/U][no PMP/first/last/below/above/crosses].
    std::array<
        std::array<
            std::array<
                std::uint64_t,
                RandomConstraints::hypervisor_pmp_relation_count>,
            2>,
        RandomConstraints::hypervisor_family_count>
        hypervisor_pmp_relation_crosses{};
    std::array<std::uint64_t, RandomConstraints::cmo_operation_count>
        cmo_operations{};
    std::array<std::uint64_t, 2> cmo_line_states{};
    std::array<std::uint64_t, 2> cmo_younger_overlaps{};
    std::array<std::uint64_t, 2> cmo_errors{};
    std::array<std::uint64_t, 2> cmo_error_kinds{};
    std::array<std::array<std::uint64_t, 2>,
               RandomConstraints::cmo_operation_count>
        cmo_operation_errors{};
    std::array<std::uint64_t, RandomConstraints::cmo_probe_depth_count>
        cmo_probe_depths{};
    // [CLEAN/FLUSH/INVAL][clean/dirty][one through eight Probes].
    std::array<
        std::array<
            std::array<
                std::uint64_t, RandomConstraints::cmo_probe_depth_count>,
            2>,
        RandomConstraints::cmo_operation_count>
        cmo_probe_crosses{};
    std::array<std::uint64_t, 2> dcache_load_errors{};
    std::array<std::uint64_t, 2> dcache_load_error_kinds{};
    // clean/corrupt/denied for weighted scalar-load actions.
    std::array<std::uint64_t, 3> dcache_load_outcomes{};
    // error responses/denied beats/corrupt beats/GrantAcks/refills.
    std::array<std::uint64_t, 5> dcache_load_error_manager{};
    // [depth2/depth3][same address/same beat/cross beat][critical beat].
    std::array<
        std::array<std::array<std::uint64_t, 2>,
                   RandomConstraints::load_merge_pattern_count>,
        2> load_merge_shapes{};
    std::array<std::uint64_t, RandomConstraints::translation_regime_count>
        load_merge_translations{};
    // Target-line requests/global refills/global GrantAcks/scalar writebacks.
    std::array<std::uint64_t, 4> load_merge_manager{};
    std::uint64_t load_merge_loads = 0;
    // [clean/dirty][no overlap/refill overlap][C ready/release backpressure]
    // [single/dual/triple/quad window][depth9/depth10][B/H/W/D]
    // [Bare/stage-1/nested].
    using SetPressureRegimeBins = std::array<
        std::uint64_t, RandomConstraints::translation_regime_count>;
    using SetPressureWidthBins = std::array<SetPressureRegimeBins, 4>;
    using SetPressureDepthBins = std::array<SetPressureWidthBins, 2>;
    using SetPressureWindowBins = std::array<
        SetPressureDepthBins,
        RandomConstraints::set_pressure_window_class_count>;
    using SetPressureBackpressureBins =
        std::array<SetPressureWindowBins, 2>;
    using SetPressureOverlapBins =
        std::array<SetPressureBackpressureBins, 2>;
    std::array<SetPressureOverlapBins, 2> set_pressure_crosses{};
    std::array<std::uint64_t, 2> set_pressure_line_states{};
    std::array<std::uint64_t, 2> set_pressure_refill_overlaps{};
    std::array<std::uint64_t, 2> set_pressure_release_backpressures{};
    std::array<
        std::uint64_t, RandomConstraints::set_pressure_window_class_count>
        set_pressure_window_counts{};
    std::array<std::uint64_t, 4> set_pressure_sets{};
    // Dirty stores only: STA-first/SDA-first.
    std::array<std::uint64_t, 2> set_pressure_issue_orders{};
    // Dirty actions/stores/target requests/target ReleaseData/global
    // ReleaseData/globally verified ReleaseData/store writebacks/SQ dequeues/
    // preserved target stores.
    std::array<std::uint64_t, 9> set_pressure_manager{};
    // Clean actions/initial loads/initial target requests/revisit loads/revisit
    // target requests/target releases/target ReleaseData/global releases/global
    // ReleaseData/globally verified ReleaseData/load writebacks/LQ dequeues.
    std::array<std::uint64_t, 12> set_pressure_clean_manager{};
    // Overlap actions/delayed target requests/target releases while the
    // delayed refill is pending/delayed load writebacks/LQ dequeues.
    std::array<std::uint64_t, 5> set_pressure_overlap_manager{};
    // Backpressured actions/stalled target releases/target C stall cycles/
    // stable-payload checks/completed ready windows.
    std::array<std::uint64_t, 5> set_pressure_backpressure_manager{};
    // [site][load/store][root/intermediate/leaf][denied/corrupt-first/
    // corrupt-last].
    std::array<std::array<std::array<std::array<std::uint64_t, 3>,
                                     RandomConstraints::ptw_error_level_count>,
                          2>,
               RandomConstraints::ptw_error_site_count>
        ptw_error_outcomes{};
    // Stage-1/G-only sites use slots 0..1; nested sites use VS x G slots 0..3.
    std::array<std::array<std::uint64_t, 4>,
               RandomConstraints::ptw_error_site_count>
        ptw_error_modes{};
    std::array<std::array<std::uint64_t, 4>,
               RandomConstraints::ptw_error_site_count>
        ptw_error_target_levels{};
    // Error response requests/denied D beats/corrupt D beats.
    std::array<std::uint64_t, 3> ptw_error_manager{};
    std::array<std::uint64_t, 2> vector_directions{};
    std::array<std::uint64_t, 4> vector_addressing{};
    std::array<std::uint64_t, 4> vector_eews{};
    std::array<std::uint64_t, 4> vector_sews{};
    std::array<std::uint64_t, 7> vector_lmuls{};
    std::array<std::uint64_t, 7> vector_emuls{};
    std::array<std::uint64_t, 2> vector_masked{};
    std::array<std::uint64_t, 2> vector_vma{};
    std::array<std::uint64_t, 2> vector_vta{};
    std::array<std::uint64_t, 2> vector_partial_vl{};
    std::array<std::uint64_t, 2> vector_nonzero_vstart{};
    std::uint64_t vector_mask_agnostic = 0;
    std::uint64_t vector_tail_agnostic = 0;
    std::uint64_t vector_shape_operations = 0;
    std::uint64_t vector_uops = 0;
    std::uint64_t vector_multi_uop = 0;
    std::array<std::uint64_t, 2> vector_segment_directions{};
    std::array<std::uint64_t, 4> vector_segment_addressing{};
    std::array<std::uint64_t, 4> vector_segment_eews{};
    std::array<std::uint64_t, 4> vector_segment_sews{};
    std::array<std::uint64_t, 7> vector_segment_lmuls{};
    std::array<std::uint64_t, 7> vector_segment_emuls{};
    std::array<std::uint64_t, 7> vector_segment_nfs{};
    std::array<std::uint64_t, 2> nc_directions{};
    std::array<std::uint64_t, 2> mmio_directions{};
    std::array<std::uint64_t, 2> uncache_errors{};
    std::array<std::uint64_t, 2> uncache_error_kinds{};
    // [NC/MMIO][load/store][clean/corrupt/denied]
    std::array<std::array<std::array<std::uint64_t, 3>, 2>, 2>
        uncache_outcomes{};
    // Atomic operations are pipeline-serializing at the MemBlock boundary;
    // only NC and MMIO traffic can be added to a legal mixed issue window.
    std::array<std::uint64_t, 2> special_concurrent{};
    std::array<std::uint64_t, RandomConstraints::translation_regime_count>
        translation_regimes{};
    std::array<std::uint64_t, 2> stage1_modes{};
    std::array<std::uint64_t, 2> vs_modes{};
    std::array<std::uint64_t, 2> g_modes{};
    std::array<std::uint64_t, 4> nested_mode_pairs{};
    std::array<std::uint64_t, 2> stage1_leaf_types{};
    std::array<std::uint64_t, 4> nested_leaf_topologies{};
    std::array<std::array<std::uint64_t, 2>, RandomConstraints::fence_kind_count>
        fences{};
    std::uint64_t translation_switches = 0;
    std::uint64_t translation_walk_windows = 0;
    std::uint64_t translation_reuse_windows = 0;
    std::uint64_t tlb_flushes = 0;
    std::uint64_t dcache_hits = 0;
    std::uint64_t dcache_misses = 0;
    std::uint64_t probe_sequences = 0;
    std::array<std::uint64_t, 2> probe_caps{};
    std::array<std::uint64_t, 2> probe_need_data{};
    std::array<std::uint64_t, 2> probe_overlaps{};
    std::array<std::uint64_t, memblock::kDcacheProbeEntries> probe_depths{};
    std::array<std::uint64_t, memblock::kDcacheProbeEntries * 4>
        probe_crosses{};
    std::uint64_t actions = 0;

    static constexpr unsigned probe_cross_index(
        unsigned depth_class, bool to_b, bool need_data)
    {
        return depth_class * 4U + static_cast<unsigned>(to_b) * 2U +
            static_cast<unsigned>(need_data);
    }

    std::optional<unsigned> first_missing_atomic_probe_cross(
        const RandomConstraints &constraints) const
    {
        if (constraints.atomic_error_per_mille == 1000) {
            return std::nullopt;
        }
        for (unsigned family = 0;
             family < RandomConstraints::atomic_family_count; ++family) {
            if (constraints.atomic_family_weights[family] == 0) {
                continue;
            }
            for (unsigned width = 0; width < 2; ++width) {
                if (constraints.atomic_width_weights[width] == 0) {
                    continue;
                }
                for (unsigned depth = 0;
                     depth < RandomConstraints::atomic_probe_depth_count;
                     ++depth) {
                    if (constraints.atomic_probe_depth_weights[depth] != 0 &&
                        atomic_probe_crosses[family][width][depth] == 0) {
                        return family * 2U *
                                RandomConstraints::atomic_probe_depth_count +
                            width * RandomConstraints::atomic_probe_depth_count +
                            depth;
                    }
                }
            }
        }
        return std::nullopt;
    }

    void sample_atomic_probe(unsigned family, unsigned width, unsigned depth)
    {
        ++atomic_probe_depths.at(depth);
        ++atomic_probe_crosses.at(family).at(width).at(depth);
    }

    std::optional<unsigned> first_missing_cmo_probe_cross(
        const RandomConstraints &constraints) const
    {
        if (constraints.cmo_error_per_mille == 1000) {
            return std::nullopt;
        }
        for (unsigned operation = 0;
             operation < RandomConstraints::cmo_operation_count; ++operation) {
            if (constraints.cmo_operation_weights[operation] == 0) {
                continue;
            }
            for (unsigned state = 0; state < 2; ++state) {
                const bool state_enabled = state == 0
                    ? constraints.cmo_dirty_per_mille != 1000
                    : constraints.cmo_dirty_per_mille != 0;
                if (!state_enabled) {
                    continue;
                }
                for (unsigned depth = 0;
                     depth < RandomConstraints::cmo_probe_depth_count; ++depth) {
                    if (constraints.cmo_probe_depth_weights[depth] != 0 &&
                        cmo_probe_crosses[operation][state][depth] == 0) {
                        return operation * 2U *
                                RandomConstraints::cmo_probe_depth_count +
                            state * RandomConstraints::cmo_probe_depth_count +
                            depth;
                    }
                }
            }
        }
        return std::nullopt;
    }

    void sample_cmo_probe(unsigned operation, bool dirty, unsigned depth_class)
    {
        ++cmo_probe_depths.at(depth_class);
        ++cmo_probe_crosses.at(operation)
              .at(dirty ? 1U : 0U)
              .at(depth_class);
    }

    std::optional<unsigned> first_missing_probe_cross(
        const RandomConstraints &constraints) const
    {
        for (unsigned index = 0; index < probe_crosses.size(); ++index) {
            const unsigned depth_class = index / 4U;
            const bool to_b = (index / 2U) % 2U != 0;
            const bool need_data = index % 2U != 0;
            if (constraints.probe_cross_enabled(
                    depth_class, to_b, need_data) &&
                probe_crosses[index] == 0) {
                return index;
            }
        }
        return std::nullopt;
    }

    void sample_probe(unsigned depth_class, bool to_b, bool need_data)
    {
        ++probe_sequences;
        ++probe_caps[to_b ? 1U : 0U];
        ++probe_need_data[need_data ? 1U : 0U];
        ++probe_overlaps[depth_class != 0 ? 1U : 0U];
        ++probe_depths.at(depth_class);
        ++probe_crosses.at(
            probe_cross_index(depth_class, to_b, need_data));
    }

    void sample_operation(unsigned operation)
    {
        ++operations.at(operation);
        ++actions;
    }

    void sample_uncache(
        unsigned memory_type, bool store, std::optional<bool> error_denied)
    {
        const unsigned outcome = !error_denied
            ? 0U : *error_denied ? 2U : 1U;
        ++uncache_outcomes.at(memory_type).at(store ? 1U : 0U).at(outcome);
        ++uncache_errors[error_denied ? 1U : 0U];
        if (error_denied) {
            ++uncache_error_kinds[*error_denied ? 1U : 0U];
        }
    }

    void sample_atomic(
        unsigned family, unsigned width, std::optional<bool> error_denied,
        const std::array<std::uint64_t, 5> &manager_delta = {})
    {
        const unsigned outcome = !error_denied
            ? 0U : *error_denied ? 2U : 1U;
        ++atomic_families.at(family);
        ++atomic_widths.at(width);
        ++atomic_outcomes.at(family).at(width).at(outcome);
        ++atomic_errors[error_denied ? 1U : 0U];
        if (error_denied) {
            ++atomic_error_kinds[*error_denied ? 1U : 0U];
        }
        for (unsigned index = 0; index < manager_delta.size(); ++index) {
            atomic_error_manager[index] += manager_delta[index];
        }
    }

    void sample_dcache_load(
        std::optional<bool> error_denied,
        const std::array<std::uint64_t, 5> &manager_delta = {})
    {
        const unsigned outcome = !error_denied
            ? 0U : *error_denied ? 2U : 1U;
        ++dcache_load_outcomes.at(outcome);
        ++dcache_load_errors[error_denied ? 1U : 0U];
        if (error_denied) {
            ++dcache_load_error_kinds[*error_denied ? 1U : 0U];
        }
        for (unsigned index = 0; index < manager_delta.size(); ++index) {
            dcache_load_error_manager[index] += manager_delta[index];
        }
    }

    void sample_load_merge(
        unsigned depth_index, unsigned pattern, unsigned critical_beat,
        unsigned translation_regime, unsigned loads,
        const std::array<std::uint64_t, 4> &manager_delta)
    {
        ++load_merge_shapes.at(depth_index).at(pattern).at(critical_beat);
        ++load_merge_translations.at(translation_regime);
        load_merge_loads += loads;
        for (unsigned index = 0; index < manager_delta.size(); ++index) {
            load_merge_manager[index] += manager_delta[index];
        }
    }

    void sample_set_pressure(
        bool dirty, bool refill_overlap, bool release_backpressure,
        unsigned window_class, unsigned depth_index, unsigned width,
        unsigned translation_regime, unsigned set_quartile,
        const std::array<std::uint64_t, 2> &issue_orders = {},
        const std::array<std::uint64_t, 9> &manager_delta = {},
        const std::array<std::uint64_t, 12> &clean_manager_delta = {},
        const std::array<std::uint64_t, 5> &overlap_manager_delta = {},
        const std::array<std::uint64_t, 5> &backpressure_manager_delta = {})
    {
        ++set_pressure_crosses.at(dirty ? 1U : 0U)
              .at(refill_overlap ? 1U : 0U)
              .at(release_backpressure ? 1U : 0U)
              .at(window_class)
              .at(depth_index)
              .at(width)
              .at(translation_regime);
        ++set_pressure_line_states.at(dirty ? 1U : 0U);
        ++set_pressure_refill_overlaps.at(refill_overlap ? 1U : 0U);
        ++set_pressure_release_backpressures.at(
            release_backpressure ? 1U : 0U);
        ++set_pressure_window_counts.at(window_class);
        ++set_pressure_sets.at(set_quartile);
        for (unsigned index = 0; index < issue_orders.size(); ++index) {
            set_pressure_issue_orders[index] += issue_orders[index];
        }
        for (unsigned index = 0; index < manager_delta.size(); ++index) {
            set_pressure_manager[index] += manager_delta[index];
        }
        for (unsigned index = 0; index < clean_manager_delta.size(); ++index) {
            set_pressure_clean_manager[index] += clean_manager_delta[index];
        }
        for (unsigned index = 0; index < overlap_manager_delta.size(); ++index) {
            set_pressure_overlap_manager[index] += overlap_manager_delta[index];
        }
        for (unsigned index = 0; index < backpressure_manager_delta.size();
             ++index) {
            set_pressure_backpressure_manager[index] +=
                backpressure_manager_delta[index];
        }
    }

    void sample_ptw_error(
        unsigned site, bool store, unsigned level_class, unsigned outcome,
        unsigned mode_index, unsigned target_level,
        const std::array<std::uint64_t, 3> &manager_delta)
    {
        ++ptw_error_outcomes.at(site).at(store ? 1U : 0U)
             .at(level_class).at(outcome);
        ++ptw_error_modes.at(site).at(mode_index);
        ++ptw_error_target_levels.at(site).at(target_level);
        for (unsigned index = 0; index < manager_delta.size(); ++index) {
            ptw_error_manager[index] += manager_delta[index];
        }
    }

    void sample_dcache(
        std::uint64_t requests_before, std::uint64_t requests_after)
    {
        ++(requests_after == requests_before ? dcache_hits : dcache_misses);
    }

    void sample_translation(
        const TranslationContext &context,
        const TranslationLeafTopology &leaf,
        std::uint64_t ptw_requests_before,
        std::uint64_t ptw_requests_after,
        unsigned action_count = 1,
        unsigned leaf_action_count = 1)
    {
        translation_regimes.at(context.regime) += action_count;
        if (context.regime == RandomConstraints::translation_stage1) {
            stage1_modes.at(context.stage1_mode) += action_count;
            stage1_leaf_types.at(leaf.stage1_napot ? 1U : 0U) +=
                leaf_action_count;
        } else if (context.regime == RandomConstraints::translation_nested) {
            vs_modes.at(context.vs_mode) += action_count;
            g_modes.at(context.g_mode) += action_count;
            nested_mode_pairs.at(context.vs_mode * 2 + context.g_mode) +=
                action_count;
            nested_leaf_topologies.at(leaf.nested_index()) += leaf_action_count;
        }
        if (context.regime != RandomConstraints::translation_bare) {
            ++(ptw_requests_after > ptw_requests_before
                    ? translation_walk_windows
                    : translation_reuse_windows);
        }
    }

    bool translation_complete(const RandomConstraints &constraints) const
    {
        if (!constraints.samples_translation()) {
            return true;
        }
        for (unsigned regime = 0;
             regime < RandomConstraints::translation_regime_count; ++regime) {
            if (constraints.translation_weights[regime] != 0 &&
                translation_regimes[regime] == 0) {
                return false;
            }
        }
        if (constraints.translation_weights[
                RandomConstraints::translation_stage1] != 0) {
            for (unsigned mode = 0; mode < stage1_modes.size(); ++mode) {
                if (constraints.stage1_mode_weights[mode] != 0 &&
                    stage1_modes[mode] == 0) {
                    return false;
                }
            }
            if (!binary_complete(
                    constraints.stage1_napot_per_mille,
                    stage1_leaf_types)) {
                return false;
            }
        }
        if (constraints.translation_weights[
                RandomConstraints::translation_nested] != 0) {
            for (unsigned vs = 0; vs < vs_modes.size(); ++vs) {
                for (unsigned g = 0; g < g_modes.size(); ++g) {
                    if (constraints.vs_mode_weights[vs] != 0 &&
                        constraints.g_mode_weights[g] != 0 &&
                        nested_mode_pairs[vs * 2 + g] == 0) {
                        return false;
                    }
                }
            }
            for (unsigned vs_napot = 0; vs_napot < 2; ++vs_napot) {
                for (unsigned g_napot = 0; g_napot < 2; ++g_napot) {
                    const bool vs_enabled = vs_napot
                        ? constraints.nested_vs_napot_per_mille != 0
                        : constraints.nested_vs_napot_per_mille != 1000;
                    const bool g_enabled = g_napot
                        ? constraints.nested_g_napot_per_mille != 0
                        : constraints.nested_g_napot_per_mille != 1000;
                    const bool observed =
                        nested_leaf_topologies[vs_napot * 2 + g_napot] != 0;
                    if (observed != (vs_enabled && g_enabled)) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    bool fences_complete(const RandomConstraints &constraints) const
    {
        if (!constraints.samples_translation() ||
            constraints.tlb_flushes_per_mille == 0) {
            return true;
        }
        for (unsigned kind = 0; kind < RandomConstraints::fence_kind_count;
             ++kind) {
            const bool compatible = kind == RandomConstraints::fence_sfence
                ? constraints.translation_weights[
                      RandomConstraints::translation_stage1] != 0
                : constraints.translation_weights[
                      RandomConstraints::translation_nested] != 0;
            if (!compatible || constraints.fence_kind_weights[kind] == 0) {
                continue;
            }
            for (unsigned scope = 0; scope < fences[kind].size(); ++scope) {
                if (constraints.fence_scope_weights[scope] != 0 &&
                    fences[kind][scope] == 0) {
                    return false;
                }
            }
        }
        return true;
    }

    bool operation_complete(
        const RandomConstraints &constraints, unsigned operation) const
    {
        if (constraints.operation_weights[operation] == 0) {
            return true;
        }
        if (operation == RandomConstraints::atomic) {
            std::array<
                std::uint64_t, RandomConstraints::atomic_probe_depth_count>
                crossed_depths{};
            std::uint64_t successful_atomics = 0;
            for (unsigned family = 0; family < atomic_outcomes.size();
                 ++family) {
                for (unsigned width = 0;
                     width < atomic_outcomes[family].size(); ++width) {
                    for (unsigned outcome = 0;
                         outcome < atomic_outcomes[family][width].size();
                         ++outcome) {
                        const bool enabled =
                            constraints.atomic_family_weights[family] != 0 &&
                            constraints.atomic_width_weights[width] != 0 &&
                            (outcome == 0
                                ? constraints.atomic_error_per_mille != 1000
                                : outcome == 1
                                ? constraints.atomic_error_per_mille != 0 &&
                                    constraints.atomic_error_denied_per_mille !=
                                        1000
                                : constraints.atomic_error_per_mille != 0 &&
                                    constraints.atomic_error_denied_per_mille !=
                                        0);
                        if ((atomic_outcomes[family][width][outcome] != 0) !=
                            enabled) {
                            return false;
                        }
                    }
                    std::uint64_t crossed_successes = 0;
                    for (unsigned depth = 0;
                         depth < RandomConstraints::atomic_probe_depth_count;
                         ++depth) {
                        const bool enabled =
                            constraints.atomic_error_per_mille != 1000 &&
                            constraints.atomic_family_weights[family] != 0 &&
                            constraints.atomic_width_weights[width] != 0 &&
                            constraints.atomic_probe_depth_weights[depth] != 0;
                        const std::uint64_t count =
                            atomic_probe_crosses[family][width][depth];
                        if ((count != 0) != enabled) {
                            return false;
                        }
                        crossed_successes += count;
                        crossed_depths[depth] += count;
                    }
                    if (crossed_successes != atomic_outcomes[family][width][0]) {
                        return false;
                    }
                    successful_atomics += crossed_successes;
                }
            }
            return operations[operation] != 0 &&
                crossed_depths == atomic_probe_depths &&
                successful_atomics == atomic_errors[0];
        }
        if (operation == RandomConstraints::scalar_load) {
            const std::array<bool, 3> enabled{{
                constraints.dcache_load_error_per_mille != 1000,
                constraints.dcache_load_error_per_mille != 0 &&
                    constraints.dcache_load_error_denied_per_mille != 1000,
                constraints.dcache_load_error_per_mille != 0 &&
                    constraints.dcache_load_error_denied_per_mille != 0,
            }};
            for (unsigned outcome = 0; outcome < enabled.size(); ++outcome) {
                if ((dcache_load_outcomes[outcome] != 0) != enabled[outcome]) {
                    return false;
                }
            }
            return operations[operation] != 0;
        }
        if (operation == RandomConstraints::load_merge) {
            for (unsigned depth = 0; depth < load_merge_shapes.size(); ++depth) {
                for (unsigned pattern = 0;
                     pattern < load_merge_shapes[depth].size(); ++pattern) {
                    for (unsigned beat = 0;
                         beat < load_merge_shapes[depth][pattern].size();
                         ++beat) {
                        const bool enabled =
                            constraints.load_merge_depth_weights[depth] != 0 &&
                            constraints.load_merge_pattern_weights[pattern] != 0;
                        if ((load_merge_shapes[depth][pattern][beat] != 0) !=
                            enabled) {
                            return false;
                        }
                    }
                }
            }
            for (unsigned regime = 0;
                 regime < load_merge_translations.size(); ++regime) {
                if ((load_merge_translations[regime] != 0) !=
                    (constraints.translation_weights[regime] != 0)) {
                    return false;
                }
            }
            return operations[operation] != 0;
        }
        if (operation == RandomConstraints::set_pressure) {
            for (unsigned state = 0;
                 state < set_pressure_crosses.size(); ++state) {
                const bool state_enabled = state == 0
                    ? constraints.set_pressure_dirty_per_mille != 1000
                    : constraints.set_pressure_dirty_per_mille != 0;
                for (unsigned overlap = 0;
                     overlap < set_pressure_crosses[state].size(); ++overlap) {
                    const bool overlap_enabled = overlap == 0
                        ? constraints.set_pressure_refill_overlap_per_mille !=
                              1000
                        : constraints.set_pressure_refill_overlap_per_mille != 0;
                    for (unsigned backpressure = 0;
                         backpressure <
                         set_pressure_crosses[state][overlap].size();
                         ++backpressure) {
                        const bool backpressure_enabled = backpressure == 0
                            ? constraints
                                      .set_pressure_release_backpressure_per_mille !=
                                  1000
                            : constraints
                                      .set_pressure_release_backpressure_per_mille !=
                                  0;
                        for (unsigned window_class = 0;
                             window_class < set_pressure_crosses[state][overlap]
                                                [backpressure]
                                                    .size();
                             ++window_class) {
                            const bool window_enabled =
                                constraints.set_pressure_window_enabled(
                                    window_class);
                            for (unsigned depth = 0;
                                 depth < set_pressure_crosses[state][overlap]
                                             [backpressure][window_class]
                                                 .size();
                                 ++depth) {
                                for (unsigned width = 0;
                                     width < set_pressure_crosses[state]
                                                 [overlap][backpressure]
                                                 [window_class]
                                                 [depth]
                                                     .size();
                                     ++width) {
                                    for (unsigned regime = 0;
                                         regime < set_pressure_crosses[state]
                                                      [overlap][backpressure]
                                                      [window_class][depth]
                                                      [width]
                                                          .size();
                                         ++regime) {
                                        const bool enabled = state_enabled &&
                                            overlap_enabled &&
                                            backpressure_enabled &&
                                            window_enabled &&
                                            constraints
                                                    .set_pressure_depth_weights
                                                        [depth] !=
                                                0 &&
                                            constraints
                                                    .set_pressure_width_weights
                                                        [width] !=
                                                0 &&
                                            constraints.translation_weights
                                                    [regime] !=
                                                0;
                                        if ((set_pressure_crosses[state]
                                                 [overlap][backpressure]
                                                 [window_class]
                                                 [depth][width][regime] != 0) !=
                                            enabled) {
                                            return false;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            for (unsigned set = 0; set < set_pressure_sets.size(); ++set) {
                if ((set_pressure_sets[set] != 0) !=
                    (constraints.set_pressure_set_weights[set] != 0)) {
                    return false;
                }
            }
            for (unsigned overlap = 0;
                 overlap < set_pressure_refill_overlaps.size(); ++overlap) {
                const bool enabled = overlap == 0
                    ? constraints.set_pressure_refill_overlap_per_mille != 1000
                    : constraints.set_pressure_refill_overlap_per_mille != 0;
                if ((set_pressure_refill_overlaps[overlap] != 0) != enabled) {
                    return false;
                }
            }
            for (unsigned backpressure = 0;
                 backpressure < set_pressure_release_backpressures.size();
                 ++backpressure) {
                const bool enabled = backpressure == 0
                    ? constraints
                              .set_pressure_release_backpressure_per_mille != 1000
                    : constraints
                              .set_pressure_release_backpressure_per_mille != 0;
                if ((set_pressure_release_backpressures[backpressure] != 0) !=
                    enabled) {
                    return false;
                }
            }
            for (unsigned window_class = 0;
                 window_class < set_pressure_window_counts.size();
                 ++window_class) {
                if ((set_pressure_window_counts[window_class] != 0) !=
                    constraints.set_pressure_window_enabled(window_class)) {
                    return false;
                }
            }
            return operations[operation] != 0;
        }
        if (operation == RandomConstraints::hypervisor) {
            std::array<std::uint64_t, 2> cross_spvp{};
            std::array<std::uint64_t, 2> cross_alignment{};
            std::array<
                std::uint64_t, RandomConstraints::hypervisor_pbmt_pair_count>
                cross_pbmt{};
            std::array<std::uint64_t, 2> cross_pma_device{};
            std::array<
                std::uint64_t,
                RandomConstraints::hypervisor_pmp_relation_count>
                cross_pmp_relation{};
            std::uint64_t cross_total = 0;
            for (unsigned family = 0; family < hypervisor_crosses.size();
                 ++family) {
                std::uint64_t cross_family = 0;
                for (unsigned spvp = 0;
                     spvp < hypervisor_crosses[family].size(); ++spvp) {
                    const bool spvp_enabled = spvp == 0
                        ? constraints.hypervisor_spvp_user_per_mille != 1000
                        : constraints.hypervisor_spvp_user_per_mille != 0;
                    const bool enabled =
                        constraints.hypervisor_family_weights[family] != 0 &&
                        spvp_enabled;
                    const std::uint64_t count =
                        hypervisor_crosses[family][spvp];
                    std::uint64_t alignment_total = 0;
                    for (unsigned alignment = 0;
                         alignment < hypervisor_alignment_crosses[family][spvp]
                                         .size();
                         ++alignment) {
                        const bool alignment_enabled = alignment == 0
                            ? constraints.misaligned_per_mille != 1000
                            : constraints.misaligned_per_mille != 0;
                        const bool alignment_cross_enabled =
                            enabled && alignment_enabled;
                        const std::uint64_t alignment_count =
                            hypervisor_alignment_crosses[
                                family][spvp][alignment];
                        if ((alignment_count != 0) !=
                            alignment_cross_enabled) {
                            return false;
                        }
                        alignment_total += alignment_count;
                        cross_alignment[alignment] += alignment_count;
                    }
                    if (alignment_total != count) {
                        return false;
                    }
                    std::uint64_t pbmt_total = 0;
                    for (unsigned pbmt = 0;
                         pbmt < hypervisor_pbmt_crosses[family][spvp].size();
                         ++pbmt) {
                        const bool pbmt_enabled = enabled &&
                            constraints.hypervisor_pbmt_pair_weights[pbmt] != 0;
                        const std::uint64_t pbmt_count =
                            hypervisor_pbmt_crosses[family][spvp][pbmt];
                        if ((pbmt_count != 0) != pbmt_enabled) {
                            return false;
                        }
                        pbmt_total += pbmt_count;
                        cross_pbmt[pbmt] += pbmt_count;
                    }
                    if (pbmt_total != count) {
                        return false;
                    }
                    std::uint64_t pma_device_total = 0;
                    for (unsigned pma_device = 0;
                         pma_device <
                             hypervisor_pma_device_crosses[family][spvp].size();
                         ++pma_device) {
                        const bool pma_device_enabled = pma_device == 0
                            ? constraints.hypervisor_pma_device_per_mille != 1000
                            : constraints.hypervisor_pma_device_per_mille != 0;
                        const bool pma_device_cross_enabled =
                            enabled && pma_device_enabled;
                        const std::uint64_t pma_device_count =
                            hypervisor_pma_device_crosses[
                                family][spvp][pma_device];
                        if ((pma_device_count != 0) !=
                            pma_device_cross_enabled) {
                            return false;
                        }
                        pma_device_total += pma_device_count;
                        cross_pma_device[pma_device] += pma_device_count;
                    }
                    if (pma_device_total != count) {
                        return false;
                    }
                    std::uint64_t pmp_relation_total = 0;
                    for (unsigned relation = 0;
                         relation < hypervisor_pmp_relation_crosses[family][spvp]
                                        .size();
                         ++relation) {
                        const bool relation_enabled = enabled &&
                            constraints.hypervisor_pmp_relation_weights[
                                relation] != 0;
                        const std::uint64_t relation_count =
                            hypervisor_pmp_relation_crosses[
                                family][spvp][relation];
                        if ((relation_count != 0) != relation_enabled) {
                            return false;
                        }
                        pmp_relation_total += relation_count;
                        cross_pmp_relation[relation] += relation_count;
                    }
                    if (pmp_relation_total != count) {
                        return false;
                    }
                    cross_family += count;
                    cross_spvp[spvp] += count;
                    cross_total += count;
                }
                if (cross_family != hypervisor_families[family]) {
                    return false;
                }
            }
            return operations[operation] != 0 &&
                cross_spvp == hypervisor_spvp &&
                cross_alignment == hypervisor_alignments &&
                cross_pbmt == hypervisor_pbmt_pairs &&
                cross_pma_device == hypervisor_pma_devices &&
                cross_pmp_relation == hypervisor_pmp_relations &&
                cross_total == operations[operation];
        }
        if (operation == RandomConstraints::cmo) {
            const bool operations_complete = std::equal(
                constraints.cmo_operation_weights.begin(),
                constraints.cmo_operation_weights.end(),
                cmo_operations.begin(), [](unsigned weight, std::uint64_t count) {
                    return (weight == 0) == (count == 0);
                });
            bool error_crosses_complete = true;
            bool probe_crosses_complete = true;
            std::array<
                std::uint64_t, RandomConstraints::cmo_probe_depth_count>
                crossed_depths{};
            std::uint64_t successful_cmo_count = 0;
            for (unsigned cmo_operation = 0;
                 cmo_operation < RandomConstraints::cmo_operation_count;
                 ++cmo_operation) {
                if (constraints.cmo_operation_weights[cmo_operation] == 0) {
                    error_crosses_complete &=
                        cmo_operation_errors[cmo_operation] ==
                            std::array<std::uint64_t, 2>{};
                } else if (constraints.cmo_error_per_mille != 0) {
                    error_crosses_complete &=
                        (constraints.cmo_error_denied_per_mille == 1000 ||
                         cmo_operation_errors[cmo_operation][0] != 0) &&
                        (constraints.cmo_error_denied_per_mille == 0 ||
                         cmo_operation_errors[cmo_operation][1] != 0);
                }
                std::uint64_t operation_successes = 0;
                for (unsigned state = 0; state < 2; ++state) {
                    const bool state_enabled = state == 0
                        ? constraints.cmo_dirty_per_mille != 1000
                        : constraints.cmo_dirty_per_mille != 0;
                    for (unsigned depth = 0;
                         depth < RandomConstraints::cmo_probe_depth_count;
                         ++depth) {
                        const bool enabled =
                            constraints.cmo_error_per_mille != 1000 &&
                            constraints.cmo_operation_weights[cmo_operation] != 0 &&
                            state_enabled &&
                            constraints.cmo_probe_depth_weights[depth] != 0;
                        const std::uint64_t count =
                            cmo_probe_crosses[cmo_operation][state][depth];
                        probe_crosses_complete &= (count != 0) == enabled;
                        operation_successes += count;
                        crossed_depths[depth] += count;
                    }
                }
                const std::uint64_t operation_errors =
                    cmo_operation_errors[cmo_operation][0] +
                    cmo_operation_errors[cmo_operation][1];
                probe_crosses_complete &=
                    operation_successes + operation_errors ==
                    cmo_operations[cmo_operation];
                successful_cmo_count += operation_successes;
            }
            return operations[operation] != 0 && operations_complete &&
                binary_complete(
                    constraints.cmo_dirty_per_mille, cmo_line_states) &&
                binary_complete(
                    constraints.cmo_younger_overlap_per_mille,
                    cmo_younger_overlaps) &&
                binary_complete(
                    constraints.cmo_error_per_mille, cmo_errors) &&
                (constraints.cmo_error_per_mille == 0 ||
                 direction_complete(
                     constraints.cmo_error_denied_per_mille,
                     cmo_error_kinds)) &&
                error_crosses_complete && probe_crosses_complete &&
                crossed_depths == cmo_probe_depths &&
                successful_cmo_count == cmo_errors[0];
        }
        if (operation == RandomConstraints::ptw_error) {
            std::uint64_t observed = 0;
            std::uint64_t denied_actions = 0;
            std::uint64_t corrupt_actions = 0;
            for (unsigned site = 0; site < ptw_error_outcomes.size(); ++site) {
                const bool site_enabled =
                    constraints.ptw_error_site_weights[site] != 0;
                for (unsigned direction = 0; direction < 2; ++direction) {
                    const bool direction_enabled = direction == 0
                        ? constraints.ptw_error_stores_per_mille != 1000
                        : constraints.ptw_error_stores_per_mille != 0;
                    for (unsigned level = 0;
                         level < RandomConstraints::ptw_error_level_count;
                         ++level) {
                        const bool level_enabled =
                            constraints.ptw_error_level_weights[level] != 0;
                        for (unsigned outcome = 0; outcome < 3; ++outcome) {
                            const bool outcome_enabled = outcome == 0
                                ? constraints.ptw_error_denied_per_mille != 0
                                : outcome == 1
                                ? constraints.ptw_error_denied_per_mille != 1000 &&
                                    constraints.
                                        ptw_error_corrupt_first_per_mille != 0
                                : constraints.ptw_error_denied_per_mille != 1000 &&
                                    constraints.
                                        ptw_error_corrupt_first_per_mille != 1000;
                            const std::uint64_t count =
                                ptw_error_outcomes[site][direction][level][outcome];
                            if ((count != 0) !=
                                (site_enabled && direction_enabled &&
                                 level_enabled && outcome_enabled)) {
                                return false;
                            }
                            observed += count;
                            if (outcome == 0) {
                                denied_actions += count;
                            } else {
                                corrupt_actions += count;
                            }
                        }
                    }
                }

                for (unsigned mode = 0; mode < 4; ++mode) {
                    bool mode_enabled = false;
                    if (site == RandomConstraints::ptw_error_stage1) {
                        mode_enabled = mode < 2 &&
                            constraints.stage1_mode_weights[mode] != 0;
                    } else if (site == RandomConstraints::ptw_error_gstage) {
                        mode_enabled = mode < 2 &&
                            constraints.g_mode_weights[mode] != 0;
                    } else {
                        mode_enabled =
                            constraints.vs_mode_weights[mode / 2] != 0 &&
                            constraints.g_mode_weights[mode % 2] != 0;
                    }
                    if ((ptw_error_modes[site][mode] != 0) !=
                        (site_enabled && mode_enabled)) {
                        return false;
                    }
                }

                for (unsigned target_level = 0; target_level < 4;
                     ++target_level) {
                    bool target_enabled = false;
                    for (unsigned mode = 0; mode < 2 && !target_enabled; ++mode) {
                        const bool mode_enabled =
                            site == RandomConstraints::ptw_error_stage1
                            ? constraints.stage1_mode_weights[mode] != 0
                            : site == RandomConstraints::ptw_error_nested_vs
                            ? constraints.vs_mode_weights[mode] != 0
                            : constraints.g_mode_weights[mode] != 0;
                        const unsigned levels = mode == 0 ? 3U : 4U;
                        target_enabled = site_enabled && mode_enabled &&
                            ((target_level == 0 &&
                              constraints.ptw_error_level_weights[
                                  RandomConstraints::ptw_error_leaf] != 0) ||
                             (target_level + 1 == levels &&
                              constraints.ptw_error_level_weights[
                                  RandomConstraints::ptw_error_root] != 0) ||
                             (target_level > 0 && target_level + 1 < levels &&
                              constraints.ptw_error_level_weights[
                                  RandomConstraints::ptw_error_intermediate] !=
                                  0));
                    }
                    if ((ptw_error_target_levels[site][target_level] != 0) !=
                        target_enabled) {
                        return false;
                    }
                }
            }
            return observed == operations[operation] &&
                ptw_error_manager[0] >= observed &&
                ptw_error_manager[1] >= denied_actions * 2 &&
                ptw_error_manager[1] / 2 <= ptw_error_manager[0] &&
                ptw_error_manager[0] - ptw_error_manager[1] / 2 >=
                    corrupt_actions &&
                (ptw_error_manager[1] & 1U) == 0 &&
                ptw_error_manager[2] ==
                    ptw_error_manager[0] + ptw_error_manager[1] / 2;
        }
        if (operation == RandomConstraints::vector_load ||
            operation == RandomConstraints::vector_store) {
            const auto target_complete = [](const auto &weights,
                                            const auto &counts) {
                return std::equal(
                    weights.begin(), weights.end(), counts.begin(),
                    [](unsigned weight, std::uint64_t count) {
                        return (weight == 0) == (count == 0);
                    });
            };
            const unsigned direction =
                operation == RandomConstraints::vector_store ? 1U : 0U;
            return vector_directions[direction] != 0 &&
                target_complete(
                    constraints.vector_addressing_weights, vector_addressing) &&
                target_complete(constraints.vector_eew_weights, vector_eews) &&
                target_complete(constraints.vector_sew_weights, vector_sews) &&
                target_complete(constraints.vector_lmul_weights, vector_lmuls) &&
                target_complete(constraints.vector_emul_weights, vector_emuls) &&
                binary_complete(
                    constraints.vector_masked_per_mille, vector_masked) &&
                binary_complete(constraints.vector_vma_per_mille, vector_vma) &&
                binary_complete(constraints.vector_vta_per_mille, vector_vta) &&
                binary_complete(
                    constraints.vector_partial_vl_per_mille,
                    vector_partial_vl) &&
                binary_complete(
                    constraints.vector_nonzero_vstart_per_mille,
                    vector_nonzero_vstart) &&
                (constraints.operation_weights[RandomConstraints::vector_load] == 0 ||
                 constraints.vector_vma_per_mille == 0 ||
                 constraints.vector_masked_per_mille == 0 ||
                 vector_mask_agnostic != 0) &&
                (constraints.operation_weights[RandomConstraints::vector_load] == 0 ||
                 constraints.vector_vta_per_mille == 0 ||
                 constraints.vector_partial_vl_per_mille == 0 ||
                 vector_tail_agnostic != 0);
        }
        if (operation == RandomConstraints::vector_segment) {
            const auto enabled_complete = [](const auto &weights,
                                             const auto &counts) {
                return std::equal(
                    weights.begin(), weights.end(), counts.begin(),
                    [](unsigned weight, std::uint64_t count) {
                        return weight == 0 || count != 0;
                    });
            };
            return operations[operation] != 0 &&
                direction_complete(
                    constraints.vector_segment_stores_per_mille,
                    vector_segment_directions) &&
                enabled_complete(
                    constraints.vector_segment_addressing_weights,
                    vector_segment_addressing) &&
                enabled_complete(
                    constraints.vector_segment_eew_weights,
                    vector_segment_eews) &&
                enabled_complete(
                    constraints.vector_segment_sew_weights,
                    vector_segment_sews) &&
                enabled_complete(
                    constraints.vector_segment_lmul_weights,
                    vector_segment_lmuls) &&
                enabled_complete(
                    constraints.vector_segment_emul_weights,
                    vector_segment_emuls) &&
                enabled_complete(
                    constraints.vector_segment_nf_weights,
                    vector_segment_nfs);
        }
        if (operation == RandomConstraints::noncacheable) {
            return uncache_operation_complete(
                constraints, operation, 0, constraints.nc_stores_per_mille,
                nc_directions);
        }
        if (operation == RandomConstraints::mmio) {
            return uncache_operation_complete(
                constraints, operation, 1, constraints.mmio_stores_per_mille,
                mmio_directions);
        }
        return operations[operation] != 0;
    }

    bool complete(
        const RandomConstraints &constraints, bool backpressure,
        const memblock::ResponseLatencyStats &dcache_latency,
        const memblock::ResponseLatencyStats &ptw_latency,
        const memblock::ResponseLatencyStats &uncache_latency) const
    {
        for (std::size_t index = 0; index < operations.size(); ++index) {
            if (!operation_complete(constraints, index)) {
                return false;
            }
        }
        if (constraints.uses_locality()) {
            for (std::size_t index = 0; index < locality.size(); ++index) {
                if (constraints.locality_weights[index] != 0 &&
                    locality[index] == 0) {
                    return false;
                }
            }
        }
        if (!translation_complete(constraints) ||
            !fences_complete(constraints)) {
            return false;
        }
        std::array<std::uint64_t, 3> atomic_outcome_totals{};
        for (const auto &family : atomic_outcomes) {
            for (const auto &width : family) {
                for (unsigned outcome = 0; outcome < width.size(); ++outcome) {
                    atomic_outcome_totals[outcome] += width[outcome];
                }
            }
        }
        const std::uint64_t atomic_actions =
            atomic_outcome_totals[0] + atomic_outcome_totals[1] +
            atomic_outcome_totals[2];
        const std::uint64_t atomic_error_actions =
            atomic_outcome_totals[1] + atomic_outcome_totals[2];
        if (atomic_actions != operations[RandomConstraints::atomic] ||
            atomic_errors != std::array<std::uint64_t, 2>{
                atomic_outcome_totals[0], atomic_error_actions} ||
            atomic_error_kinds != std::array<std::uint64_t, 2>{
                atomic_outcome_totals[1], atomic_outcome_totals[2]} ||
            atomic_error_manager != std::array<std::uint64_t, 5>{
                atomic_error_actions, atomic_outcome_totals[2] * 2,
                atomic_error_actions * 2, atomic_error_actions,
                atomic_error_actions}) {
            return false;
        }
        const std::uint64_t uncache_actions =
            operations[RandomConstraints::noncacheable] +
            operations[RandomConstraints::mmio];
        if (uncache_errors[0] + uncache_errors[1] != uncache_actions ||
            uncache_error_kinds[0] + uncache_error_kinds[1] !=
                uncache_errors[1] ||
            (uncache_actions != 0 && !binary_complete(
                constraints.uncache_error_per_mille, uncache_errors))) {
            return false;
        }
        const std::uint64_t dcache_load_actions =
            dcache_load_outcomes[0] + dcache_load_outcomes[1] +
            dcache_load_outcomes[2];
        const std::uint64_t dcache_load_error_actions =
            dcache_load_outcomes[1] + dcache_load_outcomes[2];
        const bool dcache_load_enabled =
            constraints.operation_weights[RandomConstraints::scalar_load] != 0;
        if ((!dcache_load_enabled &&
             (dcache_load_actions != 0 ||
              dcache_load_error_manager !=
                  std::array<std::uint64_t, 5>{})) ||
            (dcache_load_enabled &&
             (dcache_load_errors !=
                  std::array<std::uint64_t, 2>{
                      dcache_load_outcomes[0], dcache_load_error_actions} ||
              dcache_load_error_kinds !=
                  std::array<std::uint64_t, 2>{
                      dcache_load_outcomes[1], dcache_load_outcomes[2]} ||
              dcache_load_actions != operations[RandomConstraints::scalar_load] ||
              dcache_load_error_manager !=
                  std::array<std::uint64_t, 5>{
                      dcache_load_error_actions,
                      dcache_load_outcomes[2] * 2,
                      dcache_load_error_actions * 2,
                      dcache_load_error_actions,
                      dcache_load_error_actions}))) {
            return false;
        }
        const std::uint64_t load_merge_actions =
            operations[RandomConstraints::load_merge];
        if (load_merge_loads != load_merge_manager[3] ||
            load_merge_actions != load_merge_manager[0] ||
            load_merge_manager[1] < load_merge_actions ||
            load_merge_manager[2] != load_merge_manager[1] ||
            std::accumulate(
                load_merge_translations.begin(),
                load_merge_translations.end(), std::uint64_t{0}) !=
                load_merge_actions) {
            return false;
        }
        const std::uint64_t set_pressure_actions =
            operations[RandomConstraints::set_pressure];
        std::uint64_t set_pressure_cross_total = 0;
        std::uint64_t set_pressure_stores = 0;
        std::uint64_t set_pressure_min_releases = 0;
        std::uint64_t set_pressure_clean_loads = 0;
        std::uint64_t set_pressure_min_revisits = 0;
        std::uint64_t set_pressure_overlap_actions = 0;
        std::uint64_t set_pressure_overlap_windows = 0;
        std::uint64_t set_pressure_clean_overlap_windows = 0;
        std::uint64_t set_pressure_overlap_min_releases = 0;
        std::uint64_t set_pressure_backpressure_actions = 0;
        for (unsigned state = 0;
             state < set_pressure_crosses.size(); ++state) {
            for (unsigned overlap = 0;
                 overlap < set_pressure_crosses[state].size(); ++overlap) {
                for (unsigned backpressure = 0;
                     backpressure < set_pressure_crosses[state][overlap].size();
                     ++backpressure) {
                    for (unsigned window_class = 0;
                         window_class < set_pressure_crosses[state][overlap]
                                            [backpressure]
                                                .size();
                         ++window_class) {
                        const std::uint64_t windows = window_class + 1U;
                        for (unsigned depth = 0;
                             depth < set_pressure_crosses[state][overlap]
                                         [backpressure][window_class]
                                             .size();
                             ++depth) {
                            for (const auto &width : set_pressure_crosses[state]
                                                                   [overlap]
                                                                   [backpressure]
                                                                   [window_class]
                                                                   [depth]) {
                                for (const auto count : width) {
                                    set_pressure_cross_total += count;
                                    if (state == 0) {
                                        set_pressure_clean_loads += count *
                                            windows * (depth + 9U);
                                        set_pressure_min_revisits += count *
                                            windows * (depth + 1U);
                                    } else {
                                        set_pressure_stores += count * windows *
                                            (depth + 9U);
                                        set_pressure_min_releases += count *
                                            windows * (depth + 1U);
                                    }
                                    if (overlap != 0) {
                                        set_pressure_overlap_actions += count;
                                        set_pressure_overlap_windows +=
                                            count * windows;
                                        if (state == 0) {
                                            set_pressure_clean_overlap_windows +=
                                                count * windows;
                                        }
                                        set_pressure_overlap_min_releases +=
                                            count * windows * (depth + 1U);
                                    }
                                    if (backpressure != 0) {
                                        set_pressure_backpressure_actions +=
                                            count;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if (set_pressure_cross_total != set_pressure_actions ||
            set_pressure_line_states[0] + set_pressure_line_states[1] !=
                set_pressure_actions ||
            set_pressure_refill_overlaps[0] +
                    set_pressure_refill_overlaps[1] !=
                set_pressure_actions ||
            set_pressure_refill_overlaps[1] !=
                set_pressure_overlap_actions ||
            set_pressure_release_backpressures[0] +
                    set_pressure_release_backpressures[1] !=
                set_pressure_actions ||
            set_pressure_release_backpressures[1] !=
                set_pressure_backpressure_actions ||
            std::accumulate(
                set_pressure_window_counts.begin(),
                set_pressure_window_counts.end(), std::uint64_t{0}) !=
                set_pressure_actions ||
            std::accumulate(
                set_pressure_sets.begin(), set_pressure_sets.end(),
                std::uint64_t{0}) != set_pressure_actions ||
            set_pressure_issue_orders[0] + set_pressure_issue_orders[1] !=
                set_pressure_stores ||
            (set_pressure_line_states[1] != 0 &&
             (set_pressure_issue_orders[0] == 0 ||
              set_pressure_issue_orders[1] == 0)) ||
            (set_pressure_line_states[1] == 0 &&
             set_pressure_issue_orders != std::array<std::uint64_t, 2>{}) ||
            set_pressure_manager[0] != set_pressure_line_states[1] ||
            set_pressure_manager[1] != set_pressure_stores ||
            set_pressure_manager[2] != set_pressure_stores ||
            set_pressure_manager[3] < set_pressure_min_releases ||
            set_pressure_manager[4] < set_pressure_manager[3] ||
            set_pressure_manager[5] != set_pressure_manager[4] ||
            set_pressure_manager[6] != set_pressure_stores ||
            set_pressure_manager[7] != set_pressure_stores ||
            set_pressure_manager[8] != set_pressure_manager[3] ||
            set_pressure_clean_manager[0] != set_pressure_line_states[0] ||
            set_pressure_clean_manager[1] != set_pressure_clean_loads ||
            set_pressure_clean_manager[2] != set_pressure_clean_loads ||
            set_pressure_clean_manager[3] != set_pressure_clean_loads ||
            set_pressure_clean_manager[4] < set_pressure_min_revisits ||
            set_pressure_clean_manager[5] < set_pressure_min_revisits ||
            set_pressure_clean_manager[6] != 0 ||
            set_pressure_clean_manager[7] < set_pressure_clean_manager[5] ||
            set_pressure_clean_manager[7] < set_pressure_clean_manager[8] ||
            set_pressure_clean_manager[9] !=
                set_pressure_clean_manager[8] ||
            set_pressure_clean_manager[10] !=
                set_pressure_clean_loads * 2U +
                    set_pressure_clean_overlap_windows ||
            set_pressure_clean_manager[11] !=
                set_pressure_clean_loads * 2U +
                    set_pressure_clean_overlap_windows ||
            set_pressure_overlap_manager[0] != set_pressure_overlap_windows ||
            set_pressure_overlap_manager[1] != set_pressure_overlap_windows ||
            set_pressure_overlap_manager[2] <
                set_pressure_overlap_min_releases ||
            set_pressure_overlap_manager[3] != set_pressure_overlap_windows ||
            set_pressure_overlap_manager[4] != set_pressure_overlap_windows ||
            set_pressure_backpressure_manager[0] !=
                set_pressure_backpressure_actions ||
            set_pressure_backpressure_manager[1] !=
                set_pressure_backpressure_actions ||
            set_pressure_backpressure_manager[2] !=
                set_pressure_backpressure_actions * 16U ||
            set_pressure_backpressure_manager[3] !=
                set_pressure_backpressure_actions * 16U ||
            set_pressure_backpressure_manager[4] != set_pressure_actions) {
            return false;
        }
        if (constraints.samples_translation() &&
            constraints.uses_translation() &&
            (translation_walk_windows == 0 ||
             translation_reuse_windows == 0)) {
            return false;
        }
        if (constraints.samples_translation() &&
            constraints.tlb_flushes_per_mille != 0 && tlb_flushes == 0) {
            return false;
        }
        if (constraints.special_concurrent_per_mille != 0) {
            for (unsigned index = 0; index < special_concurrent.size(); ++index) {
                if (constraints.operation_weights[
                        RandomConstraints::noncacheable + index] != 0 &&
                    special_concurrent[index] == 0) {
                    return false;
                }
            }
        }
        if (constraints.probes_per_mille != 0 &&
            (probe_sequences == 0 ||
             !direction_complete(
                 constraints.probe_to_b_per_mille, probe_caps) ||
             !direction_complete(
                 constraints.probe_need_data_per_mille, probe_need_data) ||
             !binary_complete(
                 constraints.probe_overlap_per_mille, probe_overlaps))) {
            return false;
        }
        if (constraints.probes_per_mille != 0) {
            for (unsigned depth = 0; depth < probe_depths.size(); ++depth) {
                if ((probe_depths[depth] != 0) !=
                    constraints.probe_depth_enabled(depth)) {
                    return false;
                }
            }
            if (std::accumulate(
                    probe_depths.begin(), probe_depths.end(),
                    std::uint64_t{0}) != probe_sequences) {
                return false;
            }
            std::array<std::uint64_t, memblock::kDcacheProbeEntries>
                crossed_depths{};
            std::array<std::uint64_t, 2> crossed_caps{};
            std::array<std::uint64_t, 2> crossed_data{};
            for (unsigned depth = 0; depth < probe_depths.size(); ++depth) {
                for (unsigned cap = 0; cap < crossed_caps.size(); ++cap) {
                    for (unsigned data = 0; data < crossed_data.size(); ++data) {
                        const std::uint64_t count = probe_crosses[
                            probe_cross_index(depth, cap != 0, data != 0)];
                        const bool enabled = constraints.probe_cross_enabled(
                            depth, cap != 0, data != 0);
                        if ((count != 0) != enabled) {
                            return false;
                        }
                        crossed_depths[depth] += count;
                        crossed_caps[cap] += count;
                        crossed_data[data] += count;
                    }
                }
            }
            if (crossed_depths != probe_depths ||
                crossed_caps != probe_caps || crossed_data != probe_need_data) {
                return false;
            }
        }
        if (backpressure) {
            const auto complete_latency = [](
                memblock::ResponseLatencyProfile profile,
                const memblock::ResponseLatencyStats &latency) {
                return profile != memblock::ResponseLatencyProfile::spec ||
                    (latency.samples >= latency.buckets.size() &&
                     std::all_of(
                         latency.buckets.begin(), latency.buckets.end(),
                         [](auto samples) { return samples != 0; }));
            };
            if (!complete_latency(
                    constraints.response_latency.dcache, dcache_latency) ||
                !complete_latency(
                    constraints.response_latency.ptw, ptw_latency) ||
                !complete_latency(
                    constraints.response_latency.uncache, uncache_latency)) {
                return false;
            }
        }
        return actions != 0;
    }

private:
    bool uncache_operation_complete(
        const RandomConstraints &constraints, unsigned operation,
        unsigned memory_type, unsigned store_per_mille,
        const std::array<std::uint64_t, 2> &directions) const
    {
        if (operations[operation] == 0 ||
            !direction_complete(store_per_mille, directions)) {
            return false;
        }
        std::uint64_t observed = 0;
        for (unsigned direction = 0; direction < 2; ++direction) {
            const bool direction_enabled = direction == 0
                ? store_per_mille != 1000 : store_per_mille != 0;
            for (unsigned outcome = 0; outcome < 3; ++outcome) {
                const bool outcome_enabled = direction_enabled &&
                    (outcome == 0
                        ? constraints.uncache_error_per_mille != 1000
                        : outcome == 1
                        ? constraints.uncache_error_per_mille != 0 &&
                            direction == 0 &&
                            constraints.uncache_load_error_denied_per_mille !=
                                1000
                        : constraints.uncache_error_per_mille != 0 &&
                            (direction == 1 ||
                             constraints.uncache_load_error_denied_per_mille !=
                                 0));
                const std::uint64_t count =
                    uncache_outcomes[memory_type][direction][outcome];
                if ((count != 0) != outcome_enabled) {
                    return false;
                }
                observed += count;
            }
        }
        return observed == operations[operation];
    }

public:

    std::string summary(
        const RandomConstraints &constraints,
        const memblock::ResponseLatencyStats &dcache_latency,
        const memblock::ResponseLatencyStats &ptw_latency,
        const memblock::ResponseLatencyStats &uncache_latency) const
    {
        std::ostringstream stream;
        stream << constraints.summary() << " actual_ops=";
        for (std::size_t index = 0; index < operations.size(); ++index) {
            stream << (index == 0 ? "" : ",") << operations[index];
        }
        stream << " actual_locality=" << locality[0] << ',' << locality[1]
               << ',' << locality[2]
               << " actual_atomic_family=" << atomic_families[0] << ','
               << atomic_families[1] << ',' << atomic_families[2]
               << " actual_atomic_width=" << atomic_widths[0] << ','
               << atomic_widths[1]
               << " actual_atomic_error=" << atomic_errors[0] << ','
               << atomic_errors[1]
               << " actual_atomic_error_kind=" << atomic_error_kinds[0] << ','
               << atomic_error_kinds[1]
               << " actual_atomic_outcome=";
        for (unsigned family = 0; family < atomic_outcomes.size(); ++family) {
            for (unsigned width = 0; width < atomic_outcomes[family].size();
                 ++width) {
                for (unsigned outcome = 0;
                     outcome < atomic_outcomes[family][width].size(); ++outcome) {
                    stream << (family == 0 && width == 0 && outcome == 0
                                   ? "" : ",")
                           << atomic_outcomes[family][width][outcome];
                }
            }
        }
        stream << " actual_atomic_error_manager="
               << atomic_error_manager[0] << ',' << atomic_error_manager[1]
               << ',' << atomic_error_manager[2] << ','
               << atomic_error_manager[3] << ',' << atomic_error_manager[4]
               << " actual_atomic_probe_depth=";
        for (unsigned depth = 0; depth < atomic_probe_depths.size(); ++depth) {
            stream << (depth == 0 ? "" : ",") << atomic_probe_depths[depth];
        }
        stream << " actual_atomic_probe_cross=";
        bool first_atomic_probe_cross = true;
        for (const auto &family : atomic_probe_crosses) {
            for (const auto &width : family) {
                for (const auto count : width) {
                    stream << (first_atomic_probe_cross ? "" : ",") << count;
                    first_atomic_probe_cross = false;
                }
            }
        }
        stream << " actual_hypervisor_family=" << hypervisor_families[0]
               << ',' << hypervisor_families[1] << ','
               << hypervisor_families[2]
               << " actual_hypervisor_spvp=" << hypervisor_spvp[0] << ','
               << hypervisor_spvp[1]
               << " actual_hypervisor_cross=";
        for (unsigned family = 0; family < hypervisor_crosses.size();
             ++family) {
            for (unsigned spvp = 0;
                 spvp < hypervisor_crosses[family].size(); ++spvp) {
                stream << (family == 0 && spvp == 0 ? "" : ",")
                       << hypervisor_crosses[family][spvp];
            }
        }
        stream << " actual_hypervisor_alignment="
               << hypervisor_alignments[0] << ',' << hypervisor_alignments[1]
               << " actual_hypervisor_alignment_cross=";
        bool first_hypervisor_alignment = true;
        for (const auto &family : hypervisor_alignment_crosses) {
            for (const auto &spvp : family) {
                for (const auto count : spvp) {
                    stream << (first_hypervisor_alignment ? "" : ",")
                           << count;
                    first_hypervisor_alignment = false;
                }
            }
        }
        stream << " actual_hypervisor_pbmt_pair=";
        for (unsigned pbmt = 0; pbmt < hypervisor_pbmt_pairs.size(); ++pbmt) {
            stream << (pbmt == 0 ? "" : ",") << hypervisor_pbmt_pairs[pbmt];
        }
        stream << " actual_hypervisor_pbmt_cross=";
        bool first_hypervisor_pbmt = true;
        for (const auto &family : hypervisor_pbmt_crosses) {
            for (const auto &spvp : family) {
                for (const auto count : spvp) {
                    stream << (first_hypervisor_pbmt ? "" : ",") << count;
                    first_hypervisor_pbmt = false;
                }
            }
        }
        stream << " actual_hypervisor_pma_device="
               << hypervisor_pma_devices[0] << ','
               << hypervisor_pma_devices[1]
               << " actual_hypervisor_pma_device_cross=";
        bool first_hypervisor_pma_device = true;
        for (const auto &family : hypervisor_pma_device_crosses) {
            for (const auto &spvp : family) {
                for (const auto count : spvp) {
                    stream << (first_hypervisor_pma_device ? "" : ",")
                           << count;
                    first_hypervisor_pma_device = false;
                }
            }
        }
        stream << " actual_hypervisor_pmp_relation=";
        for (unsigned relation = 0;
             relation < hypervisor_pmp_relations.size(); ++relation) {
            stream << (relation == 0 ? "" : ",")
                   << hypervisor_pmp_relations[relation];
        }
        stream << " actual_hypervisor_pmp_relation_cross=";
        bool first_hypervisor_pmp_relation = true;
        for (const auto &family : hypervisor_pmp_relation_crosses) {
            for (const auto &spvp : family) {
                for (const auto count : spvp) {
                    stream << (first_hypervisor_pmp_relation ? "" : ",")
                           << count;
                    first_hypervisor_pmp_relation = false;
                }
            }
        }
        stream << " actual_cmo_operation=" << cmo_operations[0] << ','
               << cmo_operations[1] << ',' << cmo_operations[2]
               << " actual_cmo_line_state=" << cmo_line_states[0] << ','
               << cmo_line_states[1]
               << " actual_cmo_younger_overlap="
               << cmo_younger_overlaps[0] << ',' << cmo_younger_overlaps[1]
               << " actual_cmo_error=" << cmo_errors[0] << ','
               << cmo_errors[1]
               << " actual_cmo_error_kind=" << cmo_error_kinds[0] << ','
               << cmo_error_kinds[1]
               << " actual_cmo_operation_error="
               << cmo_operation_errors[0][0] << ','
               << cmo_operation_errors[0][1] << ','
               << cmo_operation_errors[1][0] << ','
               << cmo_operation_errors[1][1] << ','
               << cmo_operation_errors[2][0] << ','
               << cmo_operation_errors[2][1]
               << " actual_cmo_probe_depth=";
        for (unsigned depth = 0; depth < cmo_probe_depths.size(); ++depth) {
            stream << (depth == 0 ? "" : ",") << cmo_probe_depths[depth];
        }
        stream << " actual_cmo_probe_cross=";
        bool first_cmo_probe_cross = true;
        for (const auto &operation : cmo_probe_crosses) {
            for (const auto &state : operation) {
                for (const auto count : state) {
                    stream << (first_cmo_probe_cross ? "" : ",") << count;
                    first_cmo_probe_cross = false;
                }
            }
        }
        stream << " actual_dcache_load_error="
               << dcache_load_errors[0] << ',' << dcache_load_errors[1]
               << " actual_dcache_load_error_kind="
               << dcache_load_error_kinds[0] << ','
               << dcache_load_error_kinds[1]
               << " actual_dcache_load_outcome="
               << dcache_load_outcomes[0] << ','
               << dcache_load_outcomes[1] << ','
               << dcache_load_outcomes[2]
               << " actual_dcache_load_error_manager="
               << dcache_load_error_manager[0] << ','
               << dcache_load_error_manager[1] << ','
               << dcache_load_error_manager[2] << ','
               << dcache_load_error_manager[3] << ','
               << dcache_load_error_manager[4]
               << " actual_load_merge_shape=";
        bool first_load_merge_shape = true;
        for (const auto &depth : load_merge_shapes) {
            for (const auto &pattern : depth) {
                for (const auto count : pattern) {
                    stream << (first_load_merge_shape ? "" : ",") << count;
                    first_load_merge_shape = false;
                }
            }
        }
        stream << " actual_load_merge_translation="
               << load_merge_translations[0] << ','
               << load_merge_translations[1] << ','
               << load_merge_translations[2]
               << " actual_load_merge_manager="
               << load_merge_manager[0] << ',' << load_merge_manager[1]
               << ',' << load_merge_manager[2] << ','
               << load_merge_manager[3]
               << " actual_load_merge_loads=" << load_merge_loads
               << " actual_set_pressure_line_state="
               << set_pressure_line_states[0] << ','
               << set_pressure_line_states[1]
               << " actual_set_pressure_refill_overlap="
               << set_pressure_refill_overlaps[0] << ','
               << set_pressure_refill_overlaps[1]
               << " actual_set_pressure_release_backpressure="
               << set_pressure_release_backpressures[0] << ','
               << set_pressure_release_backpressures[1]
               << " actual_set_pressure_dual_window="
               << set_pressure_window_counts[0] +
                       set_pressure_window_counts[2] +
                       set_pressure_window_counts[3]
               << ',' << set_pressure_window_counts[1]
               << " actual_set_pressure_window_count="
               << set_pressure_window_counts[0] << ','
               << set_pressure_window_counts[1] << ','
               << set_pressure_window_counts[2] << ','
               << set_pressure_window_counts[3]
               << " actual_set_pressure_cross=";
        bool first_set_pressure_cross = true;
        for (const auto &state : set_pressure_crosses) {
            for (const auto &overlap : state) {
                for (const auto &backpressure : overlap) {
                    for (const auto &window_class : backpressure) {
                        for (const auto &depth : window_class) {
                            for (const auto &width : depth) {
                                for (const auto count : width) {
                                    stream <<
                                        (first_set_pressure_cross ? "" : ",")
                                           << count;
                                    first_set_pressure_cross = false;
                                }
                            }
                        }
                    }
                }
            }
        }
        stream << " actual_set_pressure_set="
               << set_pressure_sets[0] << ',' << set_pressure_sets[1] << ','
               << set_pressure_sets[2] << ',' << set_pressure_sets[3]
               << " actual_set_pressure_issue_order="
               << set_pressure_issue_orders[0] << ','
               << set_pressure_issue_orders[1]
               << " actual_set_pressure_manager=";
        for (unsigned index = 0; index < set_pressure_manager.size(); ++index) {
            stream << (index == 0 ? "" : ",") << set_pressure_manager[index];
        }
        stream << " actual_set_pressure_clean_manager=";
        for (unsigned index = 0;
             index < set_pressure_clean_manager.size(); ++index) {
            stream << (index == 0 ? "" : ",")
                   << set_pressure_clean_manager[index];
        }
        stream << " actual_set_pressure_overlap_manager=";
        for (unsigned index = 0;
             index < set_pressure_overlap_manager.size(); ++index) {
            stream << (index == 0 ? "" : ",")
                   << set_pressure_overlap_manager[index];
        }
        stream << " actual_set_pressure_backpressure_manager=";
        for (unsigned index = 0;
             index < set_pressure_backpressure_manager.size(); ++index) {
            stream << (index == 0 ? "" : ",")
                   << set_pressure_backpressure_manager[index];
        }
        stream << " actual_ptw_error_outcome=";
        bool first_ptw_outcome = true;
        for (const auto &site : ptw_error_outcomes) {
            for (const auto &direction : site) {
                for (const auto &level : direction) {
                    for (const auto count : level) {
                        stream << (first_ptw_outcome ? "" : ",") << count;
                        first_ptw_outcome = false;
                    }
                }
            }
        }
        stream << " actual_ptw_error_mode=";
        for (unsigned site = 0; site < ptw_error_modes.size(); ++site) {
            for (unsigned mode = 0; mode < ptw_error_modes[site].size(); ++mode) {
                stream << (site == 0 && mode == 0 ? "" : ",")
                       << ptw_error_modes[site][mode];
            }
        }
        stream << " actual_ptw_error_target_level=";
        for (unsigned site = 0; site < ptw_error_target_levels.size(); ++site) {
            for (unsigned level = 0;
                 level < ptw_error_target_levels[site].size(); ++level) {
                stream << (site == 0 && level == 0 ? "" : ",")
                       << ptw_error_target_levels[site][level];
            }
        }
        stream << " actual_ptw_error_manager="
               << ptw_error_manager[0] << ',' << ptw_error_manager[1] << ','
               << ptw_error_manager[2]
               << " actual_vector_direction=" << vector_directions[0] << ','
               << vector_directions[1]
               << " actual_vector_addressing=" << vector_addressing[0] << ','
               << vector_addressing[1] << ',' << vector_addressing[2] << ','
               << vector_addressing[3]
               << " actual_vector_eew=" << vector_eews[0] << ','
               << vector_eews[1] << ',' << vector_eews[2] << ','
               << vector_eews[3]
               << " actual_vector_sew=" << vector_sews[0] << ','
               << vector_sews[1] << ',' << vector_sews[2] << ','
               << vector_sews[3]
               << " actual_vector_lmul=" << vector_lmuls[0] << ','
               << vector_lmuls[1] << ',' << vector_lmuls[2] << ','
               << vector_lmuls[3] << ',' << vector_lmuls[4] << ','
               << vector_lmuls[5] << ',' << vector_lmuls[6]
               << " actual_vector_emul=" << vector_emuls[0] << ','
               << vector_emuls[1] << ',' << vector_emuls[2] << ','
               << vector_emuls[3] << ',' << vector_emuls[4] << ','
               << vector_emuls[5] << ',' << vector_emuls[6]
               << " actual_vector_masked=" << vector_masked[0] << ','
               << vector_masked[1]
               << " actual_vector_vma=" << vector_vma[0] << ','
               << vector_vma[1]
               << " actual_vector_vta=" << vector_vta[0] << ','
               << vector_vta[1]
               << " actual_vector_partial_vl=" << vector_partial_vl[0] << ','
               << vector_partial_vl[1]
               << " actual_vector_nonzero_vstart="
               << vector_nonzero_vstart[0] << ',' << vector_nonzero_vstart[1]
               << " actual_vector_agnostic=" << vector_mask_agnostic << ','
               << vector_tail_agnostic
               << " actual_vector_shape_ops=" << vector_shape_operations
               << " actual_vector_uops=" << vector_uops
               << " actual_vector_multi_uop=" << vector_multi_uop
               << " actual_vector_segment_direction="
               << vector_segment_directions[0] << ','
               << vector_segment_directions[1]
               << " actual_vector_segment_addressing="
               << vector_segment_addressing[0] << ','
               << vector_segment_addressing[1] << ','
               << vector_segment_addressing[2] << ','
               << vector_segment_addressing[3]
               << " actual_vector_segment_eew=" << vector_segment_eews[0]
               << ',' << vector_segment_eews[1] << ','
               << vector_segment_eews[2] << ',' << vector_segment_eews[3]
               << " actual_vector_segment_sew=" << vector_segment_sews[0]
               << ',' << vector_segment_sews[1] << ','
               << vector_segment_sews[2] << ',' << vector_segment_sews[3]
               << " actual_vector_segment_lmul="
               << vector_segment_lmuls[0] << ',' << vector_segment_lmuls[1]
               << ',' << vector_segment_lmuls[2] << ','
               << vector_segment_lmuls[3] << ',' << vector_segment_lmuls[4]
               << ',' << vector_segment_lmuls[5] << ','
               << vector_segment_lmuls[6]
               << " actual_vector_segment_emul="
               << vector_segment_emuls[0] << ',' << vector_segment_emuls[1]
               << ',' << vector_segment_emuls[2] << ','
               << vector_segment_emuls[3] << ',' << vector_segment_emuls[4]
               << ',' << vector_segment_emuls[5] << ','
               << vector_segment_emuls[6]
               << " actual_vector_segment_nf=" << vector_segment_nfs[0]
               << ',' << vector_segment_nfs[1] << ',' << vector_segment_nfs[2]
               << ',' << vector_segment_nfs[3] << ',' << vector_segment_nfs[4]
               << ',' << vector_segment_nfs[5] << ',' << vector_segment_nfs[6]
               << " actual_nc_direction=" << nc_directions[0] << ','
               << nc_directions[1]
               << " actual_mmio_direction=" << mmio_directions[0] << ','
               << mmio_directions[1]
               << " actual_uncache_error=" << uncache_errors[0] << ','
               << uncache_errors[1]
               << " actual_uncache_error_kind="
               << uncache_error_kinds[0] << ',' << uncache_error_kinds[1]
               << " actual_uncache_outcome="
               << uncache_outcomes[0][0][0] << ','
               << uncache_outcomes[0][0][1] << ','
               << uncache_outcomes[0][0][2] << ','
               << uncache_outcomes[0][1][0] << ','
               << uncache_outcomes[0][1][1] << ','
               << uncache_outcomes[0][1][2] << ','
               << uncache_outcomes[1][0][0] << ','
               << uncache_outcomes[1][0][1] << ','
               << uncache_outcomes[1][0][2] << ','
               << uncache_outcomes[1][1][0] << ','
               << uncache_outcomes[1][1][1] << ','
               << uncache_outcomes[1][1][2]
               << " actual_special_concurrent=" << special_concurrent[0] << ','
               << special_concurrent[1]
               << " actual_translation=" << translation_regimes[0] << ','
               << translation_regimes[1] << ',' << translation_regimes[2]
               << " actual_stage1_mode=" << stage1_modes[0] << ','
               << stage1_modes[1]
               << " actual_vs_mode=" << vs_modes[0] << ',' << vs_modes[1]
               << " actual_g_mode=" << g_modes[0] << ',' << g_modes[1]
               << " actual_nested_pairs=" << nested_mode_pairs[0] << ','
               << nested_mode_pairs[1] << ',' << nested_mode_pairs[2] << ','
               << nested_mode_pairs[3]
               << " actual_stage1_leaf=" << stage1_leaf_types[0] << ','
               << stage1_leaf_types[1]
               << " actual_nested_leaf_topology="
               << nested_leaf_topologies[0] << ','
               << nested_leaf_topologies[1] << ','
               << nested_leaf_topologies[2] << ','
               << nested_leaf_topologies[3]
               << " actual_fences=" << fences[0][0] << ',' << fences[0][1]
               << ',' << fences[1][0] << ',' << fences[1][1] << ','
               << fences[2][0] << ',' << fences[2][1]
               << " actual_translation_switch=" << translation_switches
               << " actual_translation_walk_reuse="
               << translation_walk_windows << ','
               << translation_reuse_windows
               << " actual_tlb_flush=" << tlb_flushes
               << " actual_dcache=" << dcache_hits << ',' << dcache_misses
               << " actual_probe_sequences=" << probe_sequences
               << " actual_probe_caps=" << probe_caps[0] << ','
               << probe_caps[1]
               << " actual_probe_need_data=" << probe_need_data[0] << ','
               << probe_need_data[1]
               << " actual_probe_overlap=" << probe_overlaps[0] << ','
               << probe_overlaps[1]
               << " actual_probe_depth=" << probe_depths[0] << ','
               << probe_depths[1] << ',' << probe_depths[2] << ','
               << probe_depths[3] << ',' << probe_depths[4] << ','
               << probe_depths[5] << ',' << probe_depths[6] << ','
               << probe_depths[7]
               << " actual_probe_cross=";
        for (unsigned index = 0; index < probe_crosses.size(); ++index) {
            stream << (index == 0 ? "" : ",") << probe_crosses[index];
        }
        stream
               << latency_summary("dcache_latency", dcache_latency)
               << latency_summary("ptw_latency", ptw_latency)
               << latency_summary("uncache_latency", uncache_latency);
        return stream.str();
    }

private:
    static bool binary_complete(
        unsigned true_per_mille,
        const std::array<std::uint64_t, 2> &counts)
    {
        if (true_per_mille == 0) {
            return counts[0] != 0 && counts[1] == 0;
        }
        if (true_per_mille == 1000) {
            return counts[0] == 0 && counts[1] != 0;
        }
        return counts[0] != 0 && counts[1] != 0;
    }

    static bool direction_complete(
        unsigned stores_per_mille,
        const std::array<std::uint64_t, 2> &directions)
    {
        if (stores_per_mille == 0) {
            return directions[0] != 0;
        }
        if (stores_per_mille == 1000) {
            return directions[1] != 0;
        }
        return directions[0] != 0 && directions[1] != 0;
    }

    static std::string latency_summary(
        const char *name, const memblock::ResponseLatencyStats &latency)
    {
        std::ostringstream stream;
        stream << ' ' << name << "_samples=" << latency.samples
               << ' ' << name << "_buckets=" << latency.buckets[0] << ','
               << latency.buckets[1] << ',' << latency.buckets[2] << ','
               << latency.buckets[3]
               << ' ' << name << "_total=" << latency.total_cycles
               << ' ' << name << "_max=" << latency.max_cycles;
        return stream.str();
    }
};

struct MixedCoverage {
    static constexpr unsigned kConcurrentClasses = 5;
    std::array<std::uint64_t, memblock::generated::kLsqEnqueueLanes>
        dispatch_widths{};
    std::array<std::uint64_t, memblock::generated::kLsqEnqueueLanes>
        dispatch_lanes{};
    std::array<std::uint64_t, 7> load_ops{};
    std::array<std::uint64_t, 4> store_ops{};
    std::array<std::uint64_t, memblock::kScalarLoadLanes> load_lanes{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> address_lanes{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> data_lanes{};
    std::array<std::uint64_t, 4> vector_load_eews{};
    std::array<std::uint64_t, 4> vector_store_eews{};
    std::array<std::uint64_t, 4> vector_load_address_modes{};
    std::array<std::uint64_t, 4> vector_store_address_modes{};
    std::array<std::uint64_t, 3> vector_load_stride_signs{};
    std::array<std::uint64_t, 2> vector_store_stride_signs{};
    std::array<std::uint64_t, 3> prefetch_ops{};
    std::array<std::uint64_t, memblock::kVectorMemoryLanes> vector_lanes{};
    std::uint64_t scalar_loads = 0;
    std::uint64_t scalar_stores = 0;
    std::uint64_t vector_loads = 0;
    std::uint64_t vector_stores = 0;
    std::uint64_t address_first = 0;
    std::uint64_t data_first = 0;
    std::uint64_t masked = 0;
    std::uint64_t unmasked = 0;
    std::uint64_t zero_vstart = 0;
    std::uint64_t nonzero_vstart = 0;
    std::uint64_t full_vl = 0;
    std::uint64_t partial_vl = 0;
    std::uint64_t aligned = 0;
    std::uint64_t split = 0;
    std::uint64_t scalar_misaligned = 0;
    std::uint64_t scalar_store_misaligned = 0;
    std::uint64_t vector_store_misaligned = 0;
    std::uint64_t vector_replays = 0;
    std::uint64_t two_stage = 0;
    std::uint64_t exceptions = 0;
    std::uint64_t heterogeneous_waves = 0;
    std::uint64_t simultaneous_scalar_vector = 0;
    std::uint64_t scalar_forwarding = 0;
    std::uint64_t vector_forwarding = 0;
    std::uint64_t scalar_to_vector = 0;
    std::uint64_t vector_to_scalar = 0;
    std::uint64_t cacheable = 0;
    std::uint64_t noncacheable = 0;
    std::uint64_t dcache_hits = 0;
    std::uint64_t dcache_misses = 0;
    std::uint64_t tlb_reuse = 0;
    std::uint64_t redirect_recovery = 0;
    std::uint64_t dirty_pressure = 0;
    std::uint64_t max_outstanding = 0;
    std::array<std::uint64_t, kConcurrentClasses> concurrent_ops{};
    std::uint64_t concurrent_windows = 0;
    std::uint64_t concurrent_actions = 0;
    std::uint64_t unresolved_overlap_samples = 0;
    std::uint64_t max_unresolved = 0;
    std::uint64_t max_unresolved_classes = 0;
    std::uint64_t dcache_request_stalls = 0;
    std::uint64_t dcache_response_delays = 0;
    std::uint64_t ptw_request_stalls = 0;
    std::uint64_t ptw_response_delays = 0;
    std::uint64_t uncache_request_stalls = 0;
    std::uint64_t uncache_response_delays = 0;
    std::uint64_t rvc = 0;
    std::uint64_t non_rvc = 0;
    std::uint64_t ftq_nonzero = 0;
    std::uint64_t store_set_hit = 0;
    std::uint64_t store_set_miss = 0;
    std::uint64_t load_wait = 0;
    std::uint64_t strict_load_wait = 0;

    void sample(const memblock::LoadTransaction &transaction)
    {
        ++scalar_loads;
        ++load_ops.at(static_cast<unsigned>(transaction.op) & 7U);
        ++load_lanes.at(transaction.lane);
        ++(transaction.predecode_rvc ? rvc : non_rvc);
        ftq_nonzero += transaction.ftq_ptr != 0 || transaction.ftq_offset != 0;
        ++(transaction.store_set_hit ? store_set_hit : store_set_miss);
        load_wait += transaction.load_wait_bit;
        strict_load_wait += transaction.load_wait_strict;
        const unsigned size = 1U << (static_cast<unsigned>(transaction.op) & 3U);
        scalar_misaligned += (transaction.address & (size - 1)) != 0;
    }

    void sample(const memblock::StoreTransaction &transaction, bool data_was_first)
    {
        ++scalar_stores;
        ++store_ops.at(static_cast<unsigned>(transaction.op) & 3U);
        ++address_lanes.at(transaction.address_lane);
        ++data_lanes.at(transaction.data_lane);
        ++(data_was_first ? data_first : address_first);
        const unsigned size = memblock::scalar_store_bytes(transaction.op);
        scalar_store_misaligned += (transaction.address & (size - 1)) != 0;
    }

    void sample(const memblock::VectorMemoryTransaction &transaction)
    {
        ++(transaction.store ? vector_stores : vector_loads);
        ++(transaction.store ? vector_store_eews : vector_load_eews)
              .at(transaction.eew);
        ++vector_lanes.at(transaction.lane);
        ++(transaction.store
               ? vector_store_address_modes
               : vector_load_address_modes)
              .at(static_cast<unsigned>(transaction.addressing));
        if (transaction.addressing == memblock::VectorAddressingMode::strided) {
            if (transaction.store) {
                ++vector_store_stride_signs.at(transaction.stride < 0 ? 0 : 1);
            } else {
                ++vector_load_stride_signs.at(
                    transaction.stride < 0 ? 0 : (transaction.stride == 0 ? 1 : 2));
            }
        }
        ++(transaction.vm ? unmasked : masked);
        ++(transaction.vstart == 0 ? zero_vstart : nonzero_vstart);
        const unsigned elements = 16U >> transaction.eew;
        ++(transaction.vl == elements ? full_vl : partial_vl);
        ++((transaction.address & 15U) == 0 ? aligned : split);
        if (transaction.store) {
            const unsigned bytes = 1U << transaction.eew;
            const std::uint16_t active = memblock::active_vector_elements(transaction);
            for (unsigned element = 0; element < elements; ++element) {
                if (((active >> element) & 1U) != 0 &&
                    (memblock::vector_element_address(transaction, element) &
                     (bytes - 1)) != 0) {
                    ++vector_store_misaligned;
                    break;
                }
            }
        }
    }

    void sample(const memblock::PrefetchTransaction &transaction)
    {
        const unsigned encoding = static_cast<unsigned>(transaction.op);
        ++prefetch_ops.at(encoding - 8);
    }

    bool complete(bool require_concurrent = true) const
    {
        const auto all_nonzero = [](const auto &values) {
            return std::all_of(
                values.begin(), values.end(), [](auto value) { return value != 0; });
        };
        return all_nonzero(dispatch_widths) && all_nonzero(dispatch_lanes) &&
               all_nonzero(load_ops) && all_nonzero(store_ops) &&
               all_nonzero(load_lanes) && all_nonzero(address_lanes) &&
               all_nonzero(data_lanes) && all_nonzero(vector_load_eews) &&
               all_nonzero(vector_store_eews) && all_nonzero(prefetch_ops) &&
               all_nonzero(vector_lanes) &&
               all_nonzero(vector_load_address_modes) &&
               all_nonzero(vector_store_address_modes) &&
               all_nonzero(vector_load_stride_signs) &&
               all_nonzero(vector_store_stride_signs) &&
               scalar_loads != 0 && scalar_stores != 0 && vector_loads != 0 &&
               vector_stores != 0 && address_first != 0 && data_first != 0 &&
               masked != 0 && unmasked != 0 && zero_vstart != 0 &&
               nonzero_vstart != 0 && full_vl != 0 && partial_vl != 0 &&
               aligned != 0 && split != 0 && scalar_misaligned != 0 &&
               scalar_store_misaligned != 0 &&
               vector_store_misaligned != 0 && vector_replays != 0 &&
               two_stage != 0 && exceptions != 0 &&
               heterogeneous_waves >= 2 &&
               simultaneous_scalar_vector != 0 && scalar_forwarding != 0 &&
               vector_forwarding != 0 && scalar_to_vector != 0 &&
               vector_to_scalar != 0 && cacheable != 0 && noncacheable != 0 &&
               dcache_hits != 0 && dcache_misses != 0 && tlb_reuse != 0 &&
               redirect_recovery != 0 && dirty_pressure != 0 &&
               max_outstanding >= 5 &&
               (!require_concurrent ||
                (concurrent_windows >= 4 && all_nonzero(concurrent_ops) &&
                 concurrent_actions != 0 && unresolved_overlap_samples != 0 &&
                 max_unresolved >= 2 && max_unresolved_classes >= 2)) &&
               rvc != 0 && non_rvc != 0 &&
               ftq_nonzero != 0 && store_set_hit != 0 && store_set_miss != 0 &&
               load_wait != 0;
    }

    bool backpressure_complete(bool required) const
    {
        return !required ||
               (dcache_request_stalls != 0 && dcache_response_delays != 0 &&
                ptw_request_stalls != 0 && ptw_response_delays != 0 &&
                uncache_request_stalls != 0 &&
                uncache_response_delays != 0);
    }

    std::string summary() const
    {
        return "dispatch_widths=" + std::to_string(dispatch_widths[0]) + ',' +
               std::to_string(dispatch_widths[1]) + ',' +
               std::to_string(dispatch_widths[2]) + ',' +
               std::to_string(dispatch_widths[3]) + ',' +
               std::to_string(dispatch_widths[4]) + ',' +
               std::to_string(dispatch_widths[5]) + " dispatch_lanes=" +
               std::to_string(dispatch_lanes[0]) + ',' +
               std::to_string(dispatch_lanes[1]) + ',' +
               std::to_string(dispatch_lanes[2]) + ',' +
               std::to_string(dispatch_lanes[3]) + ',' +
               std::to_string(dispatch_lanes[4]) + ',' +
               std::to_string(dispatch_lanes[5]) + " load_ops=" +
               std::to_string(load_ops[0]) + ',' +
               std::to_string(load_ops[1]) + ',' + std::to_string(load_ops[2]) +
               ',' + std::to_string(load_ops[3]) + ',' +
               std::to_string(load_ops[4]) + ',' + std::to_string(load_ops[5]) +
               ',' + std::to_string(load_ops[6]) + " store_ops=" +
               std::to_string(store_ops[0]) + ',' + std::to_string(store_ops[1]) +
               ',' + std::to_string(store_ops[2]) + ',' +
               std::to_string(store_ops[3]) + " scalar=" +
               std::to_string(scalar_loads) + ',' +
               std::to_string(scalar_stores) + " vector=" +
               std::to_string(vector_loads) + ',' +
               std::to_string(vector_stores) + " eew_load=" +
               std::to_string(vector_load_eews[0]) + ',' +
               std::to_string(vector_load_eews[1]) + ',' +
               std::to_string(vector_load_eews[2]) + ',' +
               std::to_string(vector_load_eews[3]) + " eew_store=" +
               std::to_string(vector_store_eews[0]) + ',' +
               std::to_string(vector_store_eews[1]) + ',' +
               std::to_string(vector_store_eews[2]) + ',' +
               std::to_string(vector_store_eews[3]) + " vec_load_modes=" +
               std::to_string(vector_load_address_modes[0]) + ',' +
               std::to_string(vector_load_address_modes[1]) + ',' +
               std::to_string(vector_load_address_modes[2]) + ',' +
               std::to_string(vector_load_address_modes[3]) +
               " vec_store_modes=" +
               std::to_string(vector_store_address_modes[0]) + ',' +
               std::to_string(vector_store_address_modes[1]) + ',' +
               std::to_string(vector_store_address_modes[2]) + ',' +
               std::to_string(vector_store_address_modes[3]) +
               " vec_load_stride=" +
               std::to_string(vector_load_stride_signs[0]) + ',' +
               std::to_string(vector_load_stride_signs[1]) + ',' +
               std::to_string(vector_load_stride_signs[2]) +
               " vec_store_stride=" +
               std::to_string(vector_store_stride_signs[0]) + ',' +
               std::to_string(vector_store_stride_signs[1]) + " metadata=" +
               std::to_string(rvc) + ',' + std::to_string(non_rvc) + ',' +
               std::to_string(ftq_nonzero) + ',' + std::to_string(store_set_hit) +
               ',' + std::to_string(store_set_miss) + ',' +
               std::to_string(load_wait) + ',' + std::to_string(strict_load_wait) +
               " prefetch=" +
               std::to_string(prefetch_ops[0]) + ',' +
               std::to_string(prefetch_ops[1]) + ',' +
               std::to_string(prefetch_ops[2]) + " masked=" +
               std::to_string(masked) + " unmasked=" +
               std::to_string(unmasked) + " vstart=" +
               std::to_string(zero_vstart) + ',' +
               std::to_string(nonzero_vstart) + " vl=" +
               std::to_string(full_vl) + ',' + std::to_string(partial_vl) +
               " align=" + std::to_string(aligned) + ',' +
               std::to_string(split) + " scalar_misaligned=" +
               std::to_string(scalar_misaligned) + " store_misaligned=" +
               std::to_string(scalar_store_misaligned) + ',' +
               std::to_string(vector_store_misaligned) + " store_order=" +
               std::to_string(address_first) + ',' +
               std::to_string(data_first) + " vector_replays=" +
               std::to_string(vector_replays) + " virtualization=" +
               std::to_string(two_stage) + " exceptions=" +
               std::to_string(exceptions) + " waves=" +
               std::to_string(heterogeneous_waves) + " coissue=" +
               std::to_string(simultaneous_scalar_vector) + " forwarding=" +
               std::to_string(scalar_forwarding) + ',' +
               std::to_string(vector_forwarding) + ',' +
               std::to_string(scalar_to_vector) + ',' +
               std::to_string(vector_to_scalar) + " memory_types=" +
               std::to_string(cacheable) + ',' + std::to_string(noncacheable) +
               " dcache=" + std::to_string(dcache_hits) + ',' +
               std::to_string(dcache_misses) + " tlb_reuse=" +
               std::to_string(tlb_reuse) + " redirects=" +
               std::to_string(redirect_recovery) + " dirty=" +
               std::to_string(dirty_pressure) + " max_outstanding=" +
               std::to_string(max_outstanding) + " concurrent_ops=" +
               std::to_string(concurrent_ops[0]) + ',' +
               std::to_string(concurrent_ops[1]) + ',' +
               std::to_string(concurrent_ops[2]) + ',' +
               std::to_string(concurrent_ops[3]) + ',' +
               std::to_string(concurrent_ops[4]) + " concurrent=" +
               std::to_string(concurrent_windows) + ',' +
               std::to_string(concurrent_actions) + ',' +
               std::to_string(unresolved_overlap_samples) + ',' +
               std::to_string(max_unresolved) + ',' +
               std::to_string(max_unresolved_classes) + " backpressure=" +
               std::to_string(dcache_request_stalls) + ',' +
               std::to_string(dcache_response_delays) + ',' +
               std::to_string(ptw_request_stalls) + ',' +
               std::to_string(ptw_response_delays) + ',' +
               std::to_string(uncache_request_stalls) + ',' +
               std::to_string(uncache_response_delays);
    }
};

struct StressCoverage {
    std::array<std::uint64_t, 7> load_ops{};
    std::array<std::uint64_t, 4> store_ops{};
    std::array<std::uint64_t, 3> load_lanes{};
    std::array<std::uint64_t, 2> address_lanes{};
    std::array<std::uint64_t, 2> data_lanes{};
    std::array<std::uint64_t, 2> store_order{};
    std::array<std::uint64_t, 4> vector_load_eews{};
    std::array<std::uint64_t, 4> vector_store_eews{};
    std::array<std::uint64_t, 4> vector_load_modes{};
    std::array<std::uint64_t, 4> vector_store_modes{};
    std::array<std::uint64_t, 2> vector_lanes{};
    std::array<std::uint64_t, 3> prefetch_ops{};
    std::uint64_t masked = 0;
    std::uint64_t unmasked = 0;
    std::uint64_t zero_vstart = 0;
    std::uint64_t nonzero_vstart = 0;
    std::uint64_t full_vl = 0;
    std::uint64_t partial_vl = 0;
    std::uint64_t aligned = 0;
    std::uint64_t split = 0;
    std::uint64_t scalar_misaligned = 0;
    std::uint64_t scalar_forwarding = 0;
    std::uint64_t vector_forwarding = 0;
    std::uint64_t waves = 0;
    std::uint64_t actions = 0;
    std::uint64_t max_outstanding = 0;
    std::uint64_t dcache_hits = 0;
    std::uint64_t dcache_misses = 0;
    std::uint64_t memory_regions = 0;
    std::array<std::uint64_t, 4> combinations{};
    std::uint64_t dcache_request_stalls = 0;
    std::uint64_t dcache_response_delays = 0;
    std::uint64_t ptw_request_stalls = 0;
    std::uint64_t ptw_response_delays = 0;
    std::uint64_t uncache_request_stalls = 0;
    std::uint64_t uncache_response_delays = 0;
    std::uint64_t rvc = 0;
    std::uint64_t non_rvc = 0;
    std::uint64_t ftq_nonzero = 0;
    std::uint64_t store_set_hit = 0;
    std::uint64_t store_set_miss = 0;
    std::uint64_t load_wait = 0;
    std::uint64_t strict_load_wait = 0;

    void sample(
        const memblock::LoadTransaction &transaction,
        std::uint64_t requests_before,
        std::uint64_t requests_after)
    {
        ++load_ops.at(static_cast<unsigned>(transaction.op));
        ++load_lanes.at(transaction.lane);
        ++(transaction.predecode_rvc ? rvc : non_rvc);
        ftq_nonzero += transaction.ftq_ptr != 0 || transaction.ftq_offset != 0;
        ++(transaction.store_set_hit ? store_set_hit : store_set_miss);
        load_wait += transaction.load_wait_bit;
        strict_load_wait += transaction.load_wait_strict;
        const unsigned size = 1U << (static_cast<unsigned>(transaction.op) & 3U);
        scalar_misaligned += (transaction.address & (size - 1)) != 0;
        ++(requests_after == requests_before ? dcache_hits : dcache_misses);
    }

    void sample(const memblock::StoreTransaction &transaction, bool data_first)
    {
        ++store_ops.at(static_cast<unsigned>(transaction.op));
        ++address_lanes.at(transaction.address_lane);
        ++data_lanes.at(transaction.data_lane);
        ++store_order.at(data_first ? 1 : 0);
        const unsigned size = 1U << static_cast<unsigned>(transaction.op);
        scalar_misaligned += (transaction.address & (size - 1)) != 0;
    }

    void sample(
        const memblock::VectorMemoryTransaction &transaction,
        std::uint64_t requests_before,
        std::uint64_t requests_after)
    {
        auto &eews = transaction.store ? vector_store_eews : vector_load_eews;
        auto &modes = transaction.store ? vector_store_modes : vector_load_modes;
        ++eews.at(transaction.eew);
        ++modes.at(static_cast<unsigned>(transaction.addressing));
        ++vector_lanes.at(transaction.lane);
        ++(transaction.vm ? unmasked : masked);
        ++(transaction.vstart == 0 ? zero_vstart : nonzero_vstart);
        const unsigned elements = 16U >> transaction.eew;
        ++(transaction.vl == elements ? full_vl : partial_vl);
        ++((transaction.address & 15U) == 0 ? aligned : split);
        ++(requests_after == requests_before ? dcache_hits : dcache_misses);
    }

    void sample(const memblock::PrefetchTransaction &transaction)
    {
        ++prefetch_ops.at(static_cast<unsigned>(transaction.op) - 8);
    }

    bool complete() const
    {
        const auto all_nonzero = [](const auto &values) {
            return std::all_of(
                values.begin(), values.end(), [](auto value) { return value != 0; });
        };
        const bool load_modes_without_ordered = vector_load_modes[0] != 0 &&
            vector_load_modes[1] != 0 && vector_load_modes[2] != 0;
        const bool store_modes_without_ordered = vector_store_modes[0] != 0 &&
            vector_store_modes[1] != 0 && vector_store_modes[2] != 0;
        return all_nonzero(load_ops) && all_nonzero(store_ops) &&
               all_nonzero(load_lanes) && all_nonzero(address_lanes) &&
               all_nonzero(data_lanes) && all_nonzero(store_order) &&
               all_nonzero(vector_load_eews) &&
               all_nonzero(vector_store_eews) && load_modes_without_ordered &&
               store_modes_without_ordered &&
               all_nonzero(vector_lanes) &&
               all_nonzero(prefetch_ops) && masked != 0 && unmasked != 0 &&
               zero_vstart != 0 && nonzero_vstart != 0 && full_vl != 0 &&
               partial_vl != 0 && aligned != 0 && split != 0 &&
               scalar_misaligned != 0 && scalar_forwarding != 0 &&
               vector_forwarding != 0 && waves >= 4 && max_outstanding >= 10 &&
               dcache_hits != 0 && dcache_misses != 0 && memory_regions >= 2 &&
               all_nonzero(combinations) && rvc != 0 && non_rvc != 0 &&
               ftq_nonzero != 0 && store_set_hit != 0 && store_set_miss != 0 &&
               load_wait != 0;
    }

    bool backpressure_complete(bool required) const
    {
        return !required ||
               (dcache_request_stalls != 0 && dcache_response_delays != 0);
    }

    std::string summary() const
    {
        auto csv = [](const auto &values) {
            std::string result;
            for (std::size_t index = 0; index < values.size(); ++index) {
                result += (index == 0 ? "" : ",") + std::to_string(values[index]);
            }
            return result;
        };
        return "stress_load_ops=" + csv(load_ops) +
               " stress_rng_streams=4" +
               " stress_store_ops=" + csv(store_ops) +
               " stress_load_lanes=" + csv(load_lanes) +
               " stress_address_lanes=" + csv(address_lanes) +
               " stress_data_lanes=" + csv(data_lanes) +
               " stress_store_order=" + csv(store_order) +
               " stress_eew_load=" + csv(vector_load_eews) +
               " stress_eew_store=" + csv(vector_store_eews) +
               " stress_vec_load_modes=" + csv(vector_load_modes) +
               " stress_vec_store_modes=" + csv(vector_store_modes) +
               " stress_vec_lanes=" + csv(vector_lanes) +
               " stress_prefetch=" + csv(prefetch_ops) +
               " stress_masked=" + std::to_string(masked) +
               " stress_unmasked=" + std::to_string(unmasked) +
               " stress_vstart=" + std::to_string(zero_vstart) + "," +
               std::to_string(nonzero_vstart) +
               " stress_vl=" + std::to_string(full_vl) + "," +
               std::to_string(partial_vl) +
               " stress_alignment=" + std::to_string(aligned) + "," +
               std::to_string(split) +
               " stress_metadata=" + std::to_string(rvc) + "," +
               std::to_string(non_rvc) + "," + std::to_string(ftq_nonzero) +
               "," + std::to_string(store_set_hit) + "," +
               std::to_string(store_set_miss) + "," +
               std::to_string(load_wait) + "," +
               std::to_string(strict_load_wait) +
               " stress_misaligned=" + std::to_string(scalar_misaligned) +
               " stress_forwarding=" + std::to_string(scalar_forwarding) + "," +
               std::to_string(vector_forwarding) +
               " stress_waves=" + std::to_string(waves) +
               " stress_actions=" + std::to_string(actions) +
               " stress_max_outstanding=" + std::to_string(max_outstanding) +
               " stress_dcache=" + std::to_string(dcache_hits) + "," +
               std::to_string(dcache_misses) +
               " stress_regions=" + std::to_string(memory_regions) +
               " stress_combinations=" + csv(combinations) +
               " stress_backpressure=" + std::to_string(dcache_request_stalls) + "," +
               std::to_string(dcache_response_delays) + "," +
               std::to_string(ptw_request_stalls) + "," +
               std::to_string(ptw_response_delays) + "," +
               std::to_string(uncache_request_stalls) + "," +
               std::to_string(uncache_response_delays);
    }
};

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
