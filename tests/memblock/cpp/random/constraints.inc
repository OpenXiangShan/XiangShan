// Constrained-random policy: legal knobs, presets, overrides, and validation.
// Included inside memblock_main.cpp's anonymous namespace.

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
        miss_burst,
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
        set_pressure_five_window,
        set_pressure_six_window,
        set_pressure_seven_window,
        set_pressure_eight_window,
        set_pressure_window_class_count,
    };
    static_assert(
        set_pressure_window_class_count < memblock::kDcacheMissEntries);
    static constexpr unsigned miss_burst_depth_count =
        memblock::kDcacheMissEntries - 1U;
    static constexpr unsigned miss_burst_issue_width_count =
        memblock::kScalarLoadLanes;

    enum MissBurstComposition : unsigned {
        miss_burst_scalar_only,
        miss_burst_scalar_vector_load,
        miss_burst_composition_count,
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
    std::array<unsigned, set_pressure_window_class_count>
        set_pressure_window_weights{};
    std::array<unsigned, miss_burst_depth_count> miss_burst_depth_weights{};
    std::array<unsigned, miss_burst_issue_width_count>
        miss_burst_issue_width_weights{};
    std::array<unsigned, miss_burst_composition_count>
        miss_burst_composition_weights{};
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
    // VFOF is a low-rate ordinary vector-load subclass.  The fault-position
    // split is architectural stimulus, not a pipeline/replay expectation.
    unsigned vector_fof_per_mille = 0;
    unsigned vector_fof_first_fault_per_mille = 0;
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
    // Probability of using an explicit same-bank scalar-load wave. Bank and
    // replay behavior are stimulus/coverage classifications only; correctness
    // is decided by the ordinary architectural load/queue scoreboards.
    unsigned bank_conflict_per_mille = 0;
    memblock::ResponseLatencyProfiles response_latency{};

    static RandomConstraints preset(std::string_view name)
    {
        if (name == "coverage") {
            return RandomConstraints{
                .name = "coverage",
                .operation_weights = {
                    200, 150, 150, 150, 75, 100, 100, 75, 75, 75, 75, 75,
                    100, 75, 100},
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
                .set_pressure_window_weights = {1, 1, 1, 1, 1, 1, 1, 1},
                .miss_burst_depth_weights = {
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                .miss_burst_issue_width_weights = {1, 1, 1},
                .miss_burst_composition_weights = {1, 1},
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
                .vector_fof_per_mille = 100,
                .vector_fof_first_fault_per_mille = 500,
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
                .bank_conflict_per_mille = 500,
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
                    648, 270, 90, 45, 20, 35, 5, 5, 5, 1, 1, 0, 10, 2,
                    15},
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
                .set_pressure_window_weights = {1000, 10, 2, 1, 1, 1, 1, 1},
                .miss_burst_depth_weights = {
                    1000, 200, 100, 50, 20, 10, 5, 2, 1, 1, 1, 1, 1, 1,
                    1},
                .miss_burst_issue_width_weights = {100, 20, 1},
                .miss_burst_composition_weights = {8, 1},
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
                .vector_fof_per_mille = 10,
                .vector_fof_first_fault_per_mille = 100,
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
                .bank_conflict_per_mille = 250,
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
                    125, 125, 125, 125},
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
                .set_pressure_window_weights = {1, 1, 1, 2, 4, 8, 16, 32},
                .miss_burst_depth_weights = {
                    1, 1, 1, 1, 1, 1, 1, 2, 2, 4, 4, 8, 8, 16, 32},
                .miss_burst_issue_width_weights = {1, 2, 4},
                .miss_burst_composition_weights = {1, 4},
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
                .vector_fof_per_mille = 250,
                .vector_fof_first_fault_per_mille = 500,
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
                .bank_conflict_per_mille = 750,
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
                {"miss-burst", miss_burst},
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
        constexpr std::array<std::string_view,
                             set_pressure_window_class_count>
            set_pressure_window_keys{{
                "set-pressure-window1", "set-pressure-window2",
                "set-pressure-window3", "set-pressure-window4",
                "set-pressure-window5", "set-pressure-window6",
                "set-pressure-window7", "set-pressure-window8",
            }};
        if (assign_weight(
                set_pressure_depth_keys, set_pressure_depth_weights) ||
            assign_weight(
                set_pressure_width_keys, set_pressure_width_weights) ||
            assign_weight(set_pressure_set_keys, set_pressure_set_weights) ||
            assign_weight(
                set_pressure_window_keys, set_pressure_window_weights)) {
            return;
        }
        constexpr std::array<std::string_view, miss_burst_depth_count>
            miss_burst_depth_keys{{
                "miss-burst-depth2", "miss-burst-depth3",
                "miss-burst-depth4", "miss-burst-depth5",
                "miss-burst-depth6", "miss-burst-depth7",
                "miss-burst-depth8", "miss-burst-depth9",
                "miss-burst-depth10", "miss-burst-depth11",
                "miss-burst-depth12", "miss-burst-depth13",
                "miss-burst-depth14", "miss-burst-depth15",
                "miss-burst-depth16",
            }};
        constexpr std::array<std::string_view, miss_burst_issue_width_count>
            miss_burst_issue_width_keys{{
                "miss-burst-issue-width1", "miss-burst-issue-width2",
                "miss-burst-issue-width3",
            }};
        constexpr std::array<std::string_view, miss_burst_composition_count>
            miss_burst_composition_keys{{
                "miss-burst-scalar-only",
                "miss-burst-scalar-vector-load",
            }};
        if (assign_weight(
                miss_burst_depth_keys, miss_burst_depth_weights) ||
            assign_weight(
                miss_burst_issue_width_keys,
                miss_burst_issue_width_weights) ||
            assign_weight(
                miss_burst_composition_keys,
                miss_burst_composition_weights)) {
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
        } else if (key == "vector-fof") {
            vector_fof_per_mille = parsed;
        } else if (key == "vector-fof-first-fault") {
            vector_fof_first_fault_per_mille = parsed;
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
        } else if (key == "bank-conflict") {
            bank_conflict_per_mille = parsed;
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
        if (operation_weights[set_pressure] != 0 &&
            std::accumulate(
                set_pressure_window_weights.begin(),
                set_pressure_window_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "set-pressure window constraint weights cannot all be zero");
        }
        if (operation_weights[miss_burst] != 0 &&
            std::accumulate(
                miss_burst_depth_weights.begin(),
                miss_burst_depth_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "miss-burst depth constraint weights cannot all be zero");
        }
        if (operation_weights[miss_burst] != 0 &&
            std::accumulate(
                miss_burst_issue_width_weights.begin(),
                miss_burst_issue_width_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "miss-burst issue-width constraint weights cannot all be zero");
        }
        if (operation_weights[miss_burst] != 0 &&
            std::accumulate(
                miss_burst_composition_weights.begin(),
                miss_burst_composition_weights.end(), 0ULL) == 0) {
            throw std::invalid_argument(
                "miss-burst composition constraint weights cannot all be zero");
        }
        if (operation_weights[miss_burst] != 0) {
            bool compatible = false;
            for (unsigned depth = 0;
                 depth < miss_burst_depth_weights.size(); ++depth) {
                for (unsigned width = 0;
                     width < miss_burst_issue_width_weights.size(); ++width) {
                    for (unsigned composition = 0;
                         composition < miss_burst_composition_weights.size();
                         ++composition) {
                        compatible = compatible || miss_burst_cross_enabled(
                            depth, width, composition);
                    }
                }
            }
            if (!compatible) {
                throw std::invalid_argument(
                    "miss-burst depth, issue-width, and composition "
                    "constraints have no legal combination");
            }
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
        if (vector_fof_per_mille != 0 &&
            operation_weights[vector_load] == 0) {
            throw std::invalid_argument(
                "vector-fof requires a nonzero vector-load weight");
        }
        if (vector_fof_per_mille == 1000) {
            throw std::invalid_argument(
                "vector-fof must leave a nonzero ordinary vector-load share");
        }
        if (vector_fof_per_mille != 0 &&
            translation_weights[translation_stage1] == 0) {
            throw std::invalid_argument(
                "vector-fof requires stage-1 translation traffic");
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
            std::array<bool, 2> reachable_concurrent_direction{};
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
                            const int uop_log2 = addressing >= 2
                                ? std::max(lmul_log2, emul_log2)
                                : emul_log2;
                            const unsigned uops = 1U <<
                                static_cast<unsigned>(std::max(uop_log2, 0));
                            unsigned flows_per_uop =
                                memblock::kVectorUnitStrideMaxFlows;
                            if (addressing == 1) {
                                flows_per_uop = (emul_log2 < 0
                                    ? 16U >> static_cast<unsigned>(-emul_log2)
                                    : 16U) >> eew;
                            } else if (addressing >= 2) {
                                const int flow_log2 =
                                    std::max(emul_log2, lmul_log2);
                                flows_per_uop = (flow_log2 < 0
                                    ? 16U >> static_cast<unsigned>(-flow_log2)
                                    : 16U) >> (emul_log2 > lmul_log2
                                        ? eew : vsew);
                            }
                            reachable_concurrent_direction[0] =
                                reachable_concurrent_direction[0] ||
                                uops * flows_per_uop <=
                                    memblock::kVirtualLoadQueueEntries -
                                        memblock::kLqEnqueueHeadroom - 3U;
                            reachable_concurrent_direction[1] =
                                reachable_concurrent_direction[1] ||
                                uops * flows_per_uop <=
                                    memblock::kStoreQueueEntries -
                                        memblock::kSqEnqueueHeadroom - 1U;
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
            if (concurrent_actions_per_mille != 0 &&
                (!reachable_concurrent_direction[0] ||
                 !reachable_concurrent_direction[1])) {
                throw std::invalid_argument(
                    "enabled vector shapes do not fit the concurrent LSQ budget");
            }
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
            vector_corner_per_mille > 1000 ||
            vector_fof_per_mille > 1000 ||
            vector_fof_first_fault_per_mille > 1000 ||
            probes_per_mille > 1000 ||
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
            probe_to_b_per_mille > 1000 ||
            probe_need_data_per_mille > 1000 ||
            probe_overlap_per_mille > 1000 ||
            probe_triple_overlap_per_mille > 1000 ||
            nc_stores_per_mille > 1000 ||
            mmio_stores_per_mille > 1000 ||
            uncache_error_per_mille > 1000 ||
            uncache_load_error_denied_per_mille > 1000 ||
            stride_stream_per_mille > 1000 ||
            bank_conflict_per_mille > 1000) {
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
        if (concurrent_actions_per_mille != 0) {
            for (const unsigned operation : {
                     scalar_load, scalar_store, vector_load, vector_store,
                     prefetch}) {
                if (operation_weights[operation] == 0) {
                    throw std::invalid_argument(
                        "concurrent windows require nonzero scalar-load, "
                        "scalar-store, vector-load, vector-store, and prefetch "
                        "weights");
                }
            }
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
        if (bank_conflict_per_mille != 0 &&
            operation_weights[scalar_load] == 0) {
            throw std::invalid_argument(
                "bank-conflict requires a nonzero scalar-load weight");
        }
        if (bank_conflict_per_mille != 0 &&
            dcache_load_error_per_mille == 1000) {
            throw std::invalid_argument(
                "bank-conflict requires at least one clean scalar-load class");
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

    unsigned choose_set_pressure_window(std::uint64_t random) const
    {
        return choose_weighted(set_pressure_window_weights, random);
    }

    unsigned choose_miss_burst_depth(std::uint64_t random) const
    {
        return choose_weighted(miss_burst_depth_weights, random);
    }

    unsigned choose_miss_burst_composition(std::uint64_t random) const
    {
        return choose_weighted(miss_burst_composition_weights, random);
    }

    static unsigned miss_burst_scalar_load_count(
        unsigned depth_index, unsigned composition)
    {
        const unsigned depth = depth_index + 2U;
        return composition == miss_burst_scalar_vector_load
            ? depth - 1U : depth;
    }

    unsigned choose_miss_burst_issue_width(
        unsigned depth_index, unsigned composition,
        std::uint64_t random) const
    {
        auto compatible = miss_burst_issue_width_weights;
        const unsigned scalar_loads =
            miss_burst_scalar_load_count(depth_index, composition);
        for (unsigned width = 0; width < compatible.size(); ++width) {
            if (width + 1U > scalar_loads) {
                compatible[width] = 0;
            }
        }
        return choose_weighted(compatible, random);
    }

    bool miss_burst_cross_enabled(
        unsigned depth_index, unsigned width_index,
        unsigned composition) const
    {
        return miss_burst_depth_weights.at(depth_index) != 0 &&
            miss_burst_issue_width_weights.at(width_index) != 0 &&
            miss_burst_composition_weights.at(composition) != 0 &&
            width_index + 1U <=
                miss_burst_scalar_load_count(depth_index, composition);
    }

    bool set_pressure_window_enabled(unsigned window_class) const
    {
        return set_pressure_window_weights.at(window_class) != 0;
    }

    unsigned enabled_set_pressure_window_classes() const
    {
        return static_cast<unsigned>(std::count_if(
            set_pressure_window_weights.begin(),
            set_pressure_window_weights.end(),
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

    unsigned choose_vector_eew(std::uint64_t random) const
    {
        return choose_weighted(vector_eew_weights, random);
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
        const auto enabled_vector_shape_classes = [&]() {
            unsigned count = 0;
            for (unsigned eew = 0; eew < vector_eew_weights.size(); ++eew) {
                if (vector_eew_weights[eew] == 0) {
                    continue;
                }
                for (unsigned sew = 0; sew < vector_sew_weights.size(); ++sew) {
                    if (vector_sew_weights[sew] == 0) {
                        continue;
                    }
                    for (unsigned lmul = 0; lmul < vector_lmul_weights.size();
                         ++lmul) {
                        if (vector_lmul_weights[lmul] == 0) {
                            continue;
                        }
                        const int lmul_log2 = static_cast<int>(lmul) - 3;
                        const int emul_log2 = static_cast<int>(eew) -
                            static_cast<int>(sew) + lmul_log2;
                        if (lmul_log2 < static_cast<int>(sew) - 3 ||
                            emul_log2 < -3 || emul_log2 > 3 ||
                            vector_emul_weights[emul_log2 + 3] == 0) {
                            continue;
                        }
                        const unsigned vector_bytes = lmul_log2 < 0
                            ? 16U >> static_cast<unsigned>(-lmul_log2)
                            : 16U << static_cast<unsigned>(lmul_log2);
                        if ((vector_bytes >> sew) < vector_policy_minimum_vlmax()) {
                            continue;
                        }
                        ++count;
                    }
                }
            }
            return count * static_cast<unsigned>(
                std::count_if(
                    vector_addressing_weights.begin(),
                    vector_addressing_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
        };
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
                if (bank_conflict_per_mille != 0) {
                    const unsigned regimes = static_cast<unsigned>(
                        std::count_if(
                            translation_weights.begin(),
                            translation_weights.end(),
                            [](unsigned weight) { return weight != 0; }));
                    actions += 2U * 8U * regimes;
                }
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
            } else if (operation == miss_burst) {
                const unsigned regimes = static_cast<unsigned>(std::count_if(
                    translation_weights.begin(), translation_weights.end(),
                    [](unsigned weight) { return weight != 0; }));
                for (unsigned depth = 0;
                     depth < miss_burst_depth_weights.size(); ++depth) {
                    for (unsigned width = 0;
                         width < miss_burst_issue_width_weights.size();
                         ++width) {
                        for (unsigned composition = 0;
                             composition < miss_burst_composition_weights.size();
                             ++composition) {
                            if (miss_burst_cross_enabled(
                                    depth, width, composition)) {
                                actions += regimes;
                            }
                        }
                    }
                }
            } else if (operation == vector_load || operation == vector_store) {
                actions += enabled_vector_shape_classes();
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
            operation_weights[set_pressure] != 0 ||
            operation_weights[miss_burst] != 0;
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
            operation_weights[set_pressure] != 0 ||
            operation_weights[miss_burst] != 0;
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
        stream << "constraint_schema=44 constraints=" << name
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
               << " target_set_pressure_window=";
        append_weights(stream, set_pressure_window_weights);
        stream << " target_miss_burst_depth=";
        append_weights(stream, miss_burst_depth_weights);
        stream << " target_miss_burst_issue_width=";
        append_weights(stream, miss_burst_issue_width_weights);
        stream << " target_miss_burst_composition=";
        append_weights(stream, miss_burst_composition_weights);
        stream << " target_translation=" << translation_weights[0] << ','
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
               << " target_vector_fof=" << vector_fof_per_mille
               << " target_vector_fof_first_fault="
               << vector_fof_first_fault_per_mille
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
               << " target_bank_conflict=" << bank_conflict_per_mille
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
