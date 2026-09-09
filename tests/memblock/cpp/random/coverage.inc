// Scenario-facing functional coverage models and closure gates.
// Included inside memblock_main.cpp's anonymous namespace.

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
    // [two/three same-cycle resident loads][address bank 0..7]
    // [Bare/stage-1/nested]. The bank index is derived solely from the legal
    // request address. It biases stimulus and does not predict replay policy.
    std::array<
        std::array<
            std::array<
                std::uint64_t, RandomConstraints::translation_regime_count>,
            8>,
        2>
        bank_conflict_crosses{};
    std::array<std::uint64_t, 2> bank_conflict_depths{};
    std::array<std::uint64_t, 8> bank_conflict_banks{};
    std::array<std::uint64_t, RandomConstraints::translation_regime_count>
        bank_conflict_translations{};
    // Actions/accepted loads/terminal scalar writebacks/LQ dequeues. These are
    // implementation-independent completion accounts, not bank/replay events.
    std::array<std::uint64_t, 4> bank_conflict_terminal{};
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
    // [depth2..depth16][same-cycle issue width1..3][Bare/stage-1/nested].
    std::array<
        std::array<
            std::array<
                std::uint64_t, RandomConstraints::translation_regime_count>,
            RandomConstraints::miss_burst_issue_width_count>,
        RandomConstraints::miss_burst_depth_count>
        miss_burst_crosses{};
    std::array<std::uint64_t, RandomConstraints::miss_burst_depth_count>
        miss_burst_depths{};
    std::array<
        std::uint64_t, RandomConstraints::miss_burst_issue_width_count>
        miss_burst_issue_widths{};
    std::array<std::uint64_t, RandomConstraints::translation_regime_count>
        miss_burst_translations{};
    // Actions/loads/target requests/refills/GrantAcks/scalar writebacks/
    // LQ dequeues.
    std::array<std::uint64_t, 7> miss_burst_manager{};
    std::uint64_t miss_burst_max_outstanding = 0;
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
    // [load/store][unit/strided/indexed-unordered/indexed-ordered]
    // [EEW][SEW][LMUL]. EMUL is uniquely derived by the RVV relation;
    // only legal relations are populated;
    // this is a stimulus-coverage cross, while data/exception correctness is
    // checked by the independent vector oracle.
    static constexpr unsigned vector_shape_cross_count = 2U * 4U * 4U * 4U * 7U;
    std::array<std::uint64_t, vector_shape_cross_count> vector_shape_crosses{};
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

    static constexpr unsigned vector_shape_cross_index(
        unsigned direction, unsigned addressing, unsigned eew,
        unsigned sew, int lmul_log2)
    {
        return ((((direction * 4U + addressing) * 4U + eew) * 4U + sew) *
                    7U) + static_cast<unsigned>(lmul_log2 + 3);
    }

    void sample_vector_shape(
        unsigned direction, unsigned addressing, unsigned eew,
        unsigned sew, int lmul_log2)
    {
        ++vector_shape_crosses.at(vector_shape_cross_index(
            direction, addressing, eew, sew, lmul_log2));
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

    void sample_bank_conflict(
        unsigned depth_index, unsigned bank, unsigned translation_regime,
        const std::array<std::uint64_t, 4> &terminal_delta)
    {
        ++bank_conflict_crosses.at(depth_index).at(bank).at(
            translation_regime);
        ++bank_conflict_depths.at(depth_index);
        ++bank_conflict_banks.at(bank);
        ++bank_conflict_translations.at(translation_regime);
        for (unsigned index = 0; index < terminal_delta.size(); ++index) {
            bank_conflict_terminal[index] += terminal_delta[index];
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

    void sample_miss_burst(
        unsigned depth_index, unsigned issue_width_index,
        unsigned translation_regime, std::uint64_t outstanding,
        const std::array<std::uint64_t, 7> &manager_delta)
    {
        ++miss_burst_crosses.at(depth_index)
              .at(issue_width_index)
              .at(translation_regime);
        ++miss_burst_depths.at(depth_index);
        ++miss_burst_issue_widths.at(issue_width_index);
        ++miss_burst_translations.at(translation_regime);
        miss_burst_max_outstanding = std::max(
            miss_burst_max_outstanding, outstanding);
        for (unsigned index = 0; index < manager_delta.size(); ++index) {
            miss_burst_manager[index] += manager_delta[index];
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
            if (operation == RandomConstraints::scalar_load) {
                return operations[operation] == 0 &&
                    dcache_load_outcomes ==
                        std::array<std::uint64_t, 3>{} &&
                    bank_conflict_crosses ==
                        decltype(bank_conflict_crosses){} &&
                    bank_conflict_depths ==
                        std::array<std::uint64_t, 2>{} &&
                    bank_conflict_banks ==
                        std::array<std::uint64_t, 8>{} &&
                    bank_conflict_translations ==
                        std::array<
                            std::uint64_t,
                            RandomConstraints::translation_regime_count>{} &&
                    bank_conflict_terminal ==
                        std::array<std::uint64_t, 4>{};
            }
            if (operation == RandomConstraints::vector_load ||
                operation == RandomConstraints::vector_store) {
                const unsigned direction =
                    operation == RandomConstraints::vector_store ? 1U : 0U;
                constexpr unsigned direction_stride =
                    vector_shape_cross_count / 2U;
                const auto begin = vector_shape_crosses.begin() +
                    direction * direction_stride;
                return vector_directions[direction] == 0 &&
                    std::all_of(
                        begin, begin + direction_stride,
                        [](std::uint64_t count) { return count == 0; });
            }
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
            const bool bank_enabled =
                constraints.bank_conflict_per_mille != 0;
            std::array<std::uint64_t, 2> crossed_depths{};
            std::array<std::uint64_t, 8> crossed_banks{};
            std::array<
                std::uint64_t, RandomConstraints::translation_regime_count>
                crossed_translations{};
            std::uint64_t bank_actions = 0;
            std::uint64_t bank_loads = 0;
            for (unsigned depth = 0;
                 depth < bank_conflict_crosses.size(); ++depth) {
                for (unsigned bank = 0;
                     bank < bank_conflict_crosses[depth].size(); ++bank) {
                    for (unsigned regime = 0;
                         regime < bank_conflict_crosses[depth][bank].size();
                         ++regime) {
                        const bool cross_enabled = bank_enabled &&
                            constraints.translation_weights[regime] != 0;
                        const std::uint64_t count =
                            bank_conflict_crosses[depth][bank][regime];
                        if ((count != 0) != cross_enabled) {
                            return false;
                        }
                        bank_actions += count;
                        bank_loads += count * (depth + 2U);
                        crossed_depths[depth] += count;
                        crossed_banks[bank] += count;
                        crossed_translations[regime] += count;
                    }
                }
            }
            if (crossed_depths != bank_conflict_depths ||
                crossed_banks != bank_conflict_banks ||
                crossed_translations != bank_conflict_translations) {
                return false;
            }
            const std::array<std::uint64_t, 4> expected_terminal{{
                bank_actions, bank_loads, bank_loads, bank_loads}};
            if (bank_conflict_terminal != expected_terminal) {
                return false;
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
        if (operation == RandomConstraints::miss_burst) {
            for (unsigned depth = 0; depth < miss_burst_crosses.size();
                 ++depth) {
                for (unsigned width = 0;
                     width < miss_burst_crosses[depth].size(); ++width) {
                    for (unsigned regime = 0;
                         regime < miss_burst_crosses[depth][width].size();
                         ++regime) {
                        const bool enabled =
                            constraints.miss_burst_cross_enabled(depth, width) &&
                            constraints.translation_weights[regime] != 0;
                        if ((miss_burst_crosses[depth][width][regime] != 0) !=
                            enabled) {
                            return false;
                        }
                    }
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
            const auto shape_enabled = [&](unsigned addressing, unsigned eew,
                                            unsigned sew, unsigned lmul) {
                const int lmul_log2 = static_cast<int>(lmul) - 3;
                const int emul_log2 = static_cast<int>(eew) -
                    static_cast<int>(sew) + lmul_log2;
                if (lmul_log2 < static_cast<int>(sew) - 3 ||
                    emul_log2 < -3 || emul_log2 > 3) {
                    return false;
                }
                const unsigned vector_bytes = lmul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-lmul_log2)
                    : 16U << static_cast<unsigned>(lmul_log2);
                return constraints.vector_addressing_weights[addressing] != 0 &&
                    constraints.vector_eew_weights[eew] != 0 &&
                    constraints.vector_sew_weights[sew] != 0 &&
                    constraints.vector_lmul_weights[lmul] != 0 &&
                    constraints.vector_emul_weights[emul_log2 + 3] != 0 &&
                    (vector_bytes >> sew) >=
                        constraints.vector_policy_minimum_vlmax();
            };
            bool cross_complete = true;
            std::uint64_t cross_total = 0;
            for (unsigned addressing = 0; addressing < 4; ++addressing) {
                for (unsigned eew = 0; eew < 4; ++eew) {
                    for (unsigned sew = 0; sew < 4; ++sew) {
                        for (unsigned lmul = 0; lmul < 7; ++lmul) {
                            const bool enabled = shape_enabled(
                                addressing, eew, sew, lmul);
                            const auto count = vector_shape_crosses.at(
                                vector_shape_cross_index(
                                    direction, addressing, eew, sew,
                                    static_cast<int>(lmul) - 3));
                            if ((count != 0) != enabled) {
                                cross_complete = false;
                            }
                            cross_total += count;
                        }
                    }
                }
            }
            return vector_directions[direction] != 0 && cross_complete &&
                cross_total == vector_directions[direction] &&
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
        const std::uint64_t miss_burst_actions =
            operations[RandomConstraints::miss_burst];
        std::uint64_t miss_burst_cross_total = 0;
        std::uint64_t miss_burst_loads = 0;
        std::array<std::uint64_t, RandomConstraints::miss_burst_depth_count>
            crossed_miss_burst_depths{};
        std::array<
            std::uint64_t, RandomConstraints::miss_burst_issue_width_count>
            crossed_miss_burst_widths{};
        std::array<std::uint64_t, RandomConstraints::translation_regime_count>
            crossed_miss_burst_translations{};
        for (unsigned depth = 0; depth < miss_burst_crosses.size(); ++depth) {
            for (unsigned width = 0;
                 width < miss_burst_crosses[depth].size(); ++width) {
                for (unsigned regime = 0;
                     regime < miss_burst_crosses[depth][width].size();
                     ++regime) {
                    const std::uint64_t count =
                        miss_burst_crosses[depth][width][regime];
                    miss_burst_cross_total += count;
                    miss_burst_loads += count * (depth + 2U);
                    crossed_miss_burst_depths[depth] += count;
                    crossed_miss_burst_widths[width] += count;
                    crossed_miss_burst_translations[regime] += count;
                }
            }
        }
        if (miss_burst_cross_total != miss_burst_actions ||
            crossed_miss_burst_depths != miss_burst_depths ||
            crossed_miss_burst_widths != miss_burst_issue_widths ||
            crossed_miss_burst_translations != miss_burst_translations ||
            miss_burst_manager[0] != miss_burst_actions ||
            miss_burst_manager[1] != miss_burst_loads ||
            miss_burst_manager[2] != miss_burst_loads ||
            miss_burst_manager[3] < miss_burst_loads ||
            miss_burst_manager[4] != miss_burst_manager[3] ||
            miss_burst_manager[5] != miss_burst_loads ||
            miss_burst_manager[6] != miss_burst_loads ||
            (miss_burst_actions != 0 &&
             miss_burst_max_outstanding < 2U)) {
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
               << " actual_bank_conflict_depth="
               << bank_conflict_depths[0] << ',' << bank_conflict_depths[1]
               << " actual_bank_conflict_bank=";
        for (unsigned bank = 0; bank < bank_conflict_banks.size(); ++bank) {
            stream << (bank == 0 ? "" : ",") << bank_conflict_banks[bank];
        }
        stream << " actual_bank_conflict_translation="
               << bank_conflict_translations[0] << ','
               << bank_conflict_translations[1] << ','
               << bank_conflict_translations[2]
               << " actual_bank_conflict_cross=";
        bool first_bank_conflict_cross = true;
        for (const auto &depth : bank_conflict_crosses) {
            for (const auto &bank : depth) {
                for (const auto count : bank) {
                    stream << (first_bank_conflict_cross ? "" : ",")
                           << count;
                    first_bank_conflict_cross = false;
                }
            }
        }
        stream << " actual_bank_conflict_terminal=";
        for (unsigned index = 0; index < bank_conflict_terminal.size();
             ++index) {
            stream << (index == 0 ? "" : ",")
                   << bank_conflict_terminal[index];
        }
        stream
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
               << std::accumulate(
                      set_pressure_window_counts.begin(),
                      set_pressure_window_counts.end(), std::uint64_t{0}) -
                      set_pressure_window_counts[1]
               << ',' << set_pressure_window_counts[1]
               << " actual_set_pressure_window_count="
               << set_pressure_window_counts[0] << ','
               << set_pressure_window_counts[1] << ','
               << set_pressure_window_counts[2] << ','
               << set_pressure_window_counts[3] << ','
               << set_pressure_window_counts[4] << ','
               << set_pressure_window_counts[5] << ','
               << set_pressure_window_counts[6] << ','
               << set_pressure_window_counts[7]
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
        stream << " actual_miss_burst_depth=";
        for (unsigned depth = 0; depth < miss_burst_depths.size(); ++depth) {
            stream << (depth == 0 ? "" : ",") << miss_burst_depths[depth];
        }
        stream << " actual_miss_burst_issue_width=";
        for (unsigned width = 0; width < miss_burst_issue_widths.size();
             ++width) {
            stream << (width == 0 ? "" : ",")
                   << miss_burst_issue_widths[width];
        }
        stream << " actual_miss_burst_translation=";
        for (unsigned regime = 0; regime < miss_burst_translations.size();
             ++regime) {
            stream << (regime == 0 ? "" : ",")
                   << miss_burst_translations[regime];
        }
        stream << " actual_miss_burst_cross=";
        bool first_miss_burst_cross = true;
        for (const auto &depth : miss_burst_crosses) {
            for (const auto &width : depth) {
                for (const auto count : width) {
                    stream << (first_miss_burst_cross ? "" : ",") << count;
                    first_miss_burst_cross = false;
                }
            }
        }
        stream << " actual_miss_burst_manager=";
        for (unsigned index = 0; index < miss_burst_manager.size(); ++index) {
            stream << (index == 0 ? "" : ",") << miss_burst_manager[index];
        }
        stream << " actual_miss_burst_max_outstanding="
               << miss_burst_max_outstanding;
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
               << " actual_vector_cross=";
        for (unsigned index = 0; index < vector_shape_crosses.size(); ++index) {
            stream << (index == 0 ? "" : ",") << vector_shape_crosses[index];
        }
        stream
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
               vector_store_misaligned != 0 &&
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
