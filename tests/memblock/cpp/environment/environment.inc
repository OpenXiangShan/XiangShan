// This file is included inside memblock_env.hpp's memblock namespace.

struct L2PrefetchControl {
    bool master_enable = false;
    bool receive_enable = false;
    bool pbop_enable = false;
    bool vbop_enable = false;
    bool tp_enable = false;
    std::uint16_t delay_latency = 0;

    bool operator==(const L2PrefetchControl &) const = default;
};

struct ExternalInterruptState {
    bool msip = false;
    bool mtip = false;
    bool meip = false;
    bool seip = false;
    bool debug = false;
    bool nmi_31 = false;
    bool nmi_43 = false;

    bool operator==(const ExternalInterruptState &) const = default;
};

constexpr unsigned kTraceGroups = 3;

struct TraceGroupState {
    bool valid = false;
    std::uint64_t iaddr = 0;
    std::uint8_t ftq_offset = 0;
    std::uint8_t itype = 0;
    std::uint8_t iretire = 0;
    bool ilastsize = false;
};

struct TraceBridgeStimulus {
    bool encoder_enable = false;
    bool encoder_stall = false;
    std::uint8_t privilege = 0;
    std::uint64_t mstatus = 0;
    std::uint64_t trap_cause = 0;
    std::uint64_t trap_tval = 0;
    std::array<TraceGroupState, kTraceGroups> groups{};
};

struct TopBridgeStimulus {
    bool msi_ack = false;
    bool frontend_reset = false;
    bool beu_valid = false;
    std::uint64_t beu_address = 0;
    bool msi_info_valid = false;
    std::uint16_t msi_info = 0;
    bool clint_time_valid = false;
    std::uint64_t clint_time = 0;
    bool interrupt_msip = false;
    bool interrupt_mtip = false;
    bool interrupt_meip = false;
    bool interrupt_seip = false;
    bool interrupt_debug = false;
    bool interrupt_nmi_31 = false;
    bool interrupt_nmi_43 = false;
    bool interrupt_beu_local = false;
    std::array<
        std::uint8_t, generated::kHcPerfEventHighestInputLane + 1>
        hc_perf_events{};
    L2PrefetchControl l2_prefetch{};
};

class Environment {
    struct VectorReplayRequest {
        unsigned lane;
        bool lq_flag;
        std::uint8_t lq_value;
        bool sq_flag;
        std::uint8_t sq_value;
        bool is_part_replay;
        std::uint16_t replay_mask;
        std::uint8_t replay_mb_index;
    };

    static generated::VectorMemoryIssue make_vector_memory_issue(
        const VectorMemoryTransaction &transaction)
    {
        generated::VectorMemoryIssue issue;
        issue.ftq_ptr = transaction.ftq_ptr;
        issue.ftq_offset = transaction.ftq_offset;
        issue.fu_type = vector_fu_type(transaction);
        issue.fu_op_type = vector_fu_op_type(transaction);
        issue.vec_wen = transaction.vec_wen.value_or(
            !transaction.store && !transaction.vl_wen);
        issue.vl_wen = transaction.vl_wen;
        issue.vma = transaction.vma;
        issue.vta = transaction.vta;
        issue.vsew = vector_vsew(transaction);
        issue.vlmul = transaction.vlmul;
        issue.vm = transaction.vm;
        issue.vstart = transaction.vstart;
        issue.vuop_idx = transaction.vuop_idx;
        issue.last_uop = transaction.last_uop;
        issue.nf = transaction.nf;
        issue.veew = transaction.eew;
        issue.is_vleff = transaction.is_vleff;
        issue.pdest = transaction.pdest;
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.lq_flag = transaction.lq_flag;
        issue.lq_value = transaction.lq;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.flow_num = transaction.flow_num;
        issue.is_part_replay = transaction.is_part_replay;
        issue.replay_mask = transaction.replay_mask;
        issue.replay_mb_index = transaction.replay_mb_index;
        for (unsigned byte = 0; byte < 8; ++byte) {
            issue.src[0][byte] = static_cast<unsigned char>(
                transaction.address >> (8 * byte));
        }
        issue.src[2] = transaction.data;
        if (transaction.addressing == VectorAddressingMode::strided) {
            const auto stride = static_cast<std::uint64_t>(transaction.stride);
            for (unsigned byte = 0; byte < 8; ++byte) {
                issue.src[1][byte] = static_cast<unsigned char>(
                    stride >> (8 * byte));
            }
        } else if (
            transaction.addressing == VectorAddressingMode::indexed_unordered ||
            transaction.addressing == VectorAddressingMode::indexed_ordered) {
            issue.src[1] = transaction.index;
        }
        issue.src[3][0] = static_cast<unsigned char>(transaction.mask_bits);
        issue.src[3][1] = static_cast<unsigned char>(transaction.mask_bits >> 8);
        issue.src[4][0] = transaction.vl;
        return issue;
    }

public:
    struct IFetchPtwResponse {
        std::uint8_t s2xlate = 0;
        std::uint64_t s1_tag = 0;
        std::uint16_t s1_asid = 0;
        std::uint16_t s1_vmid = 0;
        bool s1_n = false;
        std::uint8_t s1_pbmt = 0;
        bool s1_d = false;
        bool s1_a = false;
        bool s1_g = false;
        bool s1_u = false;
        bool s1_x = false;
        bool s1_w = false;
        bool s1_r = false;
        std::uint8_t s1_level = 0;
        bool s1_v = false;
        std::uint64_t s1_ppn = 0;
        std::uint8_t s1_addr_low = 0;
        std::array<std::uint8_t, 8> s1_ppn_low{};
        std::uint8_t s1_valididx = 0;
        std::uint8_t s1_pteidx = 0;
        bool s1_pf = false;
        bool s1_af = false;
        std::uint64_t s2_tag = 0;
        std::uint16_t s2_vmid = 0;
        bool s2_n = false;
        std::uint8_t s2_pbmt = 0;
        std::uint64_t s2_ppn = 0;
        bool s2_d = false;
        bool s2_a = false;
        bool s2_g = false;
        bool s2_u = false;
        bool s2_x = false;
        bool s2_w = false;
        bool s2_r = false;
        std::uint8_t s2_level = 0;
        bool s2_gpf = false;
        bool s2_gaf = false;

        bool operator==(const IFetchPtwResponse &other) const
        {
            return s2xlate == other.s2xlate && s1_tag == other.s1_tag &&
                s1_asid == other.s1_asid && s1_vmid == other.s1_vmid &&
                s1_n == other.s1_n && s1_pbmt == other.s1_pbmt &&
                s1_d == other.s1_d && s1_a == other.s1_a &&
                s1_g == other.s1_g && s1_u == other.s1_u &&
                s1_x == other.s1_x && s1_w == other.s1_w &&
                s1_r == other.s1_r && s1_level == other.s1_level &&
                s1_v == other.s1_v && s1_ppn == other.s1_ppn &&
                s1_addr_low == other.s1_addr_low &&
                s1_ppn_low == other.s1_ppn_low &&
                s1_valididx == other.s1_valididx &&
                s1_pteidx == other.s1_pteidx && s1_pf == other.s1_pf &&
                s1_af == other.s1_af && s2_tag == other.s2_tag &&
                s2_vmid == other.s2_vmid && s2_n == other.s2_n &&
                s2_pbmt == other.s2_pbmt && s2_ppn == other.s2_ppn &&
                s2_d == other.s2_d && s2_a == other.s2_a &&
                s2_g == other.s2_g && s2_u == other.s2_u &&
                s2_x == other.s2_x && s2_w == other.s2_w &&
                s2_r == other.s2_r && s2_level == other.s2_level &&
                s2_gpf == other.s2_gpf && s2_gaf == other.s2_gaf;
        }
    };

    struct L2TlbResponse {
        std::uint64_t paddr = 0;
        std::uint8_t pbmt = 0;
        bool miss = false;
        bool guest_page_fault = false;
        bool page_fault = false;
        bool access_fault = false;
        bool pmp_load_denied = false;
        bool pmp_mmio = false;
    };

    struct FrontendBridgeStats {
        std::uint64_t requests = 0;
        std::uint64_t responses = 0;
        std::uint64_t request_stalls = 0;
        std::uint64_t response_stalls = 0;
        std::uint64_t source_credit_stalls = 0;
        std::uint64_t field_checks = 0;
    };

    struct ScalarLoadWakeupSample {
        bool valid = false;
        bool rf_wen = false;
        bool fp_wen = false;
        std::uint8_t pdest = 0;
        std::uint64_t cycle = 0;
    };

    struct ScalarLoadFeedbackStats {
        std::array<std::uint64_t, kScalarLoadLanes> wakeups{};
        std::array<std::uint64_t, kScalarLoadLanes> ld2_cancels{};
        std::array<ScalarLoadWakeupSample, kScalarLoadLanes> last_wakeup{};
        std::array<std::uint64_t, kScalarLoadLanes> last_cancel_cycle{};
    };

    struct LoadMmioStats {
        std::array<std::uint64_t, kScalarLoadLanes> pulses{};
        std::array<std::uint8_t, kScalarLoadLanes> last_rob{};
        std::array<std::uint64_t, kScalarLoadLanes> last_cycle{};
    };

    struct StoreSlowFeedbackSample {
        unsigned lane = 0;
        bool hit = false;
        bool sq_flag = false;
        std::uint8_t sq_value = 0;
        std::uint64_t cycle = 0;
    };

    struct VectorStoreSlowFeedbackSample {
        unsigned lane = 0;
        bool hit = false;
        bool lq_flag = false;
        std::uint8_t lq_value = 0;
        bool sq_flag = false;
        std::uint8_t sq_value = 0;
        bool is_part_replay = false;
        std::uint16_t replay_mask = 0;
        std::uint8_t replay_mb_index = 0;
        std::uint64_t cycle = 0;
    };

    struct IqSlowFeedbackStats {
        std::array<std::uint64_t, kScalarStoreLanes> sta_valid{};
        std::array<std::uint64_t, kScalarStoreLanes> sta_hits{};
        std::array<std::uint64_t, kScalarStoreLanes> sta_misses{};
        std::array<std::uint64_t, kVectorMemoryLanes> vstu_valid{};
        std::array<std::uint64_t, kVectorMemoryLanes> vstu_hits{};
        std::array<std::uint64_t, kVectorMemoryLanes> vstu_misses{};
        std::vector<StoreSlowFeedbackSample> sta_samples;
        std::vector<VectorStoreSlowFeedbackSample> vstu_samples;
    };

    struct MemoryViolationStats {
        std::uint64_t count = 0;
        generated::MemoryViolation last{};
        std::uint64_t last_cycle = 0;
    };

    struct IfetchPrefetchStats {
        std::array<std::uint64_t, kScalarLoadLanes> requests{};
        std::array<std::uint64_t, kScalarLoadLanes> last_vaddr{};
        std::array<std::uint64_t, kScalarLoadLanes> last_cycle{};
    };

    struct HardwarePrefetchStats {
        std::uint64_t l2_requests = 0;
        std::uint64_t l3_requests = 0;
        std::array<std::uint64_t, 32> l2_source_counts{};
        std::array<std::vector<std::uint64_t>, 32> l2_addresses_by_source{};
        std::array<std::uint64_t, 32> last_l2_addr_by_source{};
        std::array<std::uint64_t, 32> last_l2_cycle_by_source{};
        std::uint64_t last_l2_addr = 0;
        std::uint8_t last_l2_source = 0;
        std::uint64_t last_l2_cycle = 0;
        std::uint64_t last_l3_addr = 0;
        std::uint64_t last_l3_cycle = 0;
    };

    struct BusErrorStats {
        std::uint64_t dcache_reports = 0;
        std::uint64_t uncache_reports = 0;
        std::uint64_t last_dcache_address = 0;
        std::uint64_t last_uncache_address = 0;
    };

    struct TopDownStats {
        std::uint64_t replay_allocate_cycles = 0;
        std::uint64_t sq_full_cycles = 0;
        std::uint64_t sb_full_cycles = 0;
        std::uint64_t l1_miss_cycles = 0;
        std::uint64_t l2_miss_cycles = 0;
        std::uint64_t l3_miss_cycles = 0;
        std::uint64_t delay_checks = 0;
    };

    Environment(int argc, char **argv)
        : dut_(argc, argv), memory_(&bus_memory_),
          memory_agent_(bus_memory_, memory_),
          ptw_agent_(bus_memory_), uncache_agent_(bus_memory_)
    {
        dut_.InitClock(dut_.clock);
        generated::drive_idle_inputs(dut_);
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{3});
        dut_.io_ooo_to_mem_tlbCsr_priv_imode.ImmSet(std::uint64_t{3});
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
        initialize_internal_debug();
#endif
    }

    ~Environment() { dut_.Finish(); }

    SparseMemory &memory() { return memory_; }
    std::uint64_t bus_expected_load(std::uint64_t address, LoadOp op) const
    {
        return bus_memory_.expected_load(address, op);
    }
    void expect_release_line(
        std::uint64_t base, const std::vector<unsigned char> &bytes)
    {
        memory_agent_.expect_release_line(base, bytes);
    }
    void clear_release_line_expectations()
    {
        memory_agent_.clear_release_line_expectations();
    }
    bool request_dcache_probe(
        std::uint64_t address, std::uint8_t cap, bool need_data,
        std::uint8_t expected_report,
        const std::vector<unsigned char> &expected_data = {})
    {
        return memory_agent_.request_probe(
            address, cap, need_data, expected_report, expected_data);
    }
    bool hold_dcache_c_until_probes(std::uint64_t target_requests)
    {
        memory_agent_.hold_c_until_probe_requests(target_requests);
        return check_components();
    }
    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        configure_backpressure(
            seed, enabled,
            ResponseLatencyProfiles{
                latency_profile, latency_profile, latency_profile});
    }

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        const ResponseLatencyProfiles &latency_profiles)
    {
        memory_agent_.configure_backpressure(
            seed, enabled, latency_profiles.dcache);
        ptw_agent_.configure_backpressure(
            seed ^ 0x9e3779b97f4a7c15ULL, enabled, latency_profiles.ptw);
        uncache_agent_.configure_backpressure(
            seed ^ 0x3c6ef372fe94f82aULL, enabled, latency_profiles.uncache);
    }

    void inject_next_dcache_response_error(
        bool denied, bool corrupt,
        DcacheCorruptBeat corrupt_beat = DcacheCorruptBeat::all)
    {
        memory_agent_.inject_next_response_error(
            denied, corrupt, corrupt_beat);
    }

    void inject_dcache_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        DcacheCorruptBeat corrupt_beat = DcacheCorruptBeat::all)
    {
        memory_agent_.inject_response_error_at(
            address, denied, corrupt, corrupt_beat);
    }

    void inject_dcache_cmo_response_error_at(
        std::uint64_t address, bool denied, bool corrupt)
    {
        memory_agent_.inject_cmo_response_error_at(
            address, denied, corrupt);
    }

    void inject_ptw_response_error_after(
        unsigned clean_requests, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all)
    {
        ptw_agent_.inject_response_error_after(
            clean_requests, denied, corrupt, corrupt_beat);
    }

    void inject_ptw_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all)
    {
        ptw_agent_.inject_response_error_at(
            address, denied, corrupt, corrupt_beat);
    }

    void inject_persistent_ptw_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all)
    {
        ptw_agent_.inject_persistent_response_error_at(
            address, denied, corrupt, corrupt_beat);
    }

    void clear_ptw_response_error()
    {
        ptw_agent_.clear_response_error();
    }

    void force_next_dcache_response_delay(unsigned cycles)
    {
        memory_agent_.force_next_response_delay(cycles);
    }

    void force_next_cmo_response_delay(unsigned cycles)
    {
        memory_agent_.force_next_cmo_response_delay(cycles);
    }

    void force_next_dcache_interbeat_delay(unsigned cycles)
    {
        memory_agent_.force_next_interbeat_delay(cycles);
    }

    void hold_dcache_response_at(std::uint64_t address)
    {
        memory_agent_.hold_response_at(address);
    }

    bool dcache_response_held() const
    {
        return memory_agent_.response_held();
    }

    bool dcache_response_held_at(std::uint64_t address) const
    {
        return memory_agent_.response_held_at(address);
    }

    std::size_t dcache_held_response_count() const
    {
        return memory_agent_.held_response_count();
    }

    bool release_held_dcache_response()
    {
        memory_agent_.release_held_response();
        return check_components();
    }

    bool release_held_dcache_response_at(std::uint64_t address)
    {
        memory_agent_.release_held_response_at(address);
        return check_components();
    }

    bool schedule_held_dcache_response_release_at(
        std::uint64_t address, unsigned after_ticks = 1)
    {
        memory_agent_.schedule_held_response_release_at(
            address, after_ticks);
        return check_components();
    }

    bool begin_dcache_release_ready_window(
        const std::vector<std::uint64_t> &target_lines,
        unsigned stall_valid_cycles)
    {
        memory_agent_.begin_release_ready_window(
            target_lines, stall_valid_cycles);
        return check_components();
    }

    bool end_dcache_release_ready_window()
    {
        memory_agent_.end_release_ready_window();
        return check_components();
    }

    void inject_next_uncache_response_error(bool denied, bool corrupt)
    {
        uncache_agent_.inject_next_response_error(denied, corrupt);
    }

    void force_next_ptw_response_delay(unsigned cycles)
    {
        ptw_agent_.force_next_response_delay(cycles);
    }

    void force_next_uncache_response_delay(unsigned cycles)
    {
        uncache_agent_.force_next_response_delay(cycles);
    }

    void configure_uncache_device(
        std::uint64_t base, std::uint64_t size, bool read_clear)
    {
        uncache_agent_.configure_device_window(base, size, read_clear);
    }

    const std::vector<UncacheMemoryAgent::DeviceAccess> &
    uncache_device_accesses() const
    {
        return uncache_agent_.device_accesses();
    }

    void configure_cache_error_enable(bool enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_cache_error_enable.ImmSet(enable);
    }

    void configure_ldld_violation_check(bool enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable.ImmSet(enable);
    }

    bool configure_stride_prefetch(bool enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable.ImmSet(enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable.ImmSet(enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold.ImmSet(
            std::uint64_t{12});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride.ImmSet(
            std::uint64_t{30});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride.ImmSet(enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable.ImmSet(
            enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency.ImmSet(
            std::uint64_t{0});
        return run_cycles(4);
    }

    bool configure_sms_pht_prefetch(bool output_enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht.ImmSet(
            output_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold.ImmSet(
            std::uint64_t{12});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride.ImmSet(
            std::uint64_t{30});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency.ImmSet(
            std::uint64_t{0});
        return run_cycles(4);
    }

    bool expect_l2_prefetch_control(bool master_enabled, bool receive_enabled)
    {
        dut_.RefreshComb();
        const bool matches =
            dut_.io_outer_l2PfCtrl_l2_pf_master_en.B() == master_enabled &&
            dut_.io_outer_l2PfCtrl_l2_pf_recv_en.B() == receive_enabled &&
            dut_.io_outer_l2PfCtrl_l2_pbop_en.B() &&
            dut_.io_outer_l2PfCtrl_l2_vbop_en.B() &&
            dut_.io_outer_l2PfCtrl_l2_tp_en.B() &&
            dut_.io_outer_l2PfCtrl_l2_pf_delay_latency.U() == 0;
        if (!matches) {
            std::ostringstream message;
            message << "L2 prefetch control output mismatch master="
                    << dut_.io_outer_l2PfCtrl_l2_pf_master_en.B()
                    << " recv="
                    << dut_.io_outer_l2PfCtrl_l2_pf_recv_en.B()
                    << " pbop=" << dut_.io_outer_l2PfCtrl_l2_pbop_en.B()
                    << " vbop=" << dut_.io_outer_l2PfCtrl_l2_vbop_en.B()
                    << " tp=" << dut_.io_outer_l2PfCtrl_l2_tp_en.B()
                    << " delay="
                    << dut_.io_outer_l2PfCtrl_l2_pf_delay_latency.U();
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    // The L2-to-L1 DTLB request has no ready pin at the MemBlock boundary:
    // MemBlock ties the response consumer ready high internally.  Hold the
    // request valid for one cycle, then wait for the returned response while
    // the ordinary PTW agent services any page walk generated by the request.
    bool issue_l2_tlb_request(
        std::uint64_t vaddr, std::uint8_t cmd, bool kill, bool is_prefetch,
        bool no_translate, L2TlbResponse &response, unsigned timeout = 4096)
    {
        dut_.io_l2_tlb_req_req_bits_vaddr.ImmSet(vaddr);
        dut_.io_l2_tlb_req_req_bits_cmd.ImmSet(cmd);
        dut_.io_l2_tlb_req_req_bits_kill.ImmSet(kill);
        dut_.io_l2_tlb_req_req_bits_isPrefetch.ImmSet(is_prefetch);
        dut_.io_l2_tlb_req_req_bits_no_translate.ImmSet(no_translate);
        dut_.io_l2_tlb_req_req_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_l2_tlb_req_req_valid.ImmSet(std::uint64_t{0});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (dut_.io_l2_tlb_req_resp_valid.B()) {
                response.paddr = dut_.io_l2_tlb_req_resp_bits_paddr_0.U();
                response.pbmt = static_cast<std::uint8_t>(
                    dut_.io_l2_tlb_req_resp_bits_pbmt_0.U());
                response.miss = dut_.io_l2_tlb_req_resp_bits_miss.B();
                response.guest_page_fault =
                    dut_.io_l2_tlb_req_resp_bits_excp_0_gpf_ld.B();
                response.page_fault =
                    dut_.io_l2_tlb_req_resp_bits_excp_0_pf_ld.B();
                response.access_fault =
                    dut_.io_l2_tlb_req_resp_bits_excp_0_af_ld.B();
                // The retained L2 PMP response comes from a leaveHitMux
                // checker, whose match and config are registered one cycle
                // after the TLB response payload becomes valid.
                tick(false);
                response.pmp_load_denied = dut_.io_l2_pmp_resp_ld.B();
                response.pmp_mmio = dut_.io_l2_pmp_resp_mmio.B();
                return check_components();
            }
            tick(false);
        }
        error_ = "timed out waiting for L2-to-L1 DTLB response";
        return false;
    }

    bool issue_killed_l2_tlb_request(
        std::uint64_t vaddr, std::uint8_t cmd, bool is_prefetch,
        bool no_translate, unsigned timeout = 128)
    {
        dut_.io_l2_tlb_req_req_bits_vaddr.ImmSet(vaddr);
        dut_.io_l2_tlb_req_req_bits_cmd.ImmSet(cmd);
        dut_.io_l2_tlb_req_req_bits_kill.ImmSet(std::uint64_t{1});
        dut_.io_l2_tlb_req_req_bits_isPrefetch.ImmSet(is_prefetch);
        dut_.io_l2_tlb_req_req_bits_no_translate.ImmSet(no_translate);
        dut_.io_l2_tlb_req_req_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_l2_tlb_req_req_valid.ImmSet(std::uint64_t{0});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (dut_.io_l2_tlb_req_resp_valid.B()) {
                error_ = "killed L2-to-L1 DTLB request produced a response";
                return false;
            }
            tick(false);
        }
        return check_components();
    }

    bool start_ifetch_ptw_request(
        std::uint64_t vpn, PtwTranslationMode mode, unsigned timeout = 16384)
    {
        if ((vpn >> 38) != 0) {
            error_ = "invalid IFetch PTW request";
            return false;
        }
        const auto s2xlate = static_cast<std::uint8_t>(mode);

        dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{0});
        dut_.io_fetch_to_mem_itlb_req_0_bits_vpn.ImmSet(vpn);
        dut_.io_fetch_to_mem_itlb_req_0_bits_s2xlate.ImmSet(s2xlate);
        dut_.io_fetch_to_mem_itlb_req_0_valid.ImmSet(std::uint64_t{1});
        bool accepted = false;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            dut_.RefreshComb();
            const bool fire = dut_.io_fetch_to_mem_itlb_req_0_ready.B();
            tick(false);
            if (fire) {
                accepted = true;
                break;
            }
            if (!check_components()) {
                break;
            }
        }
        dut_.io_fetch_to_mem_itlb_req_0_valid.ImmSet(std::uint64_t{0});
        if (!accepted) {
            if (error_.empty()) {
                error_ = "timed out accepting IFetch PTW request";
            }
            dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
            return false;
        }
        ++ifetch_ptw_pending_;
        return check_components();
    }

    bool complete_ifetch_ptw_request(
        IFetchPtwResponse &response, unsigned response_stall_cycles = 4,
        unsigned timeout = 16384)
    {
        if (ifetch_ptw_pending_ == 0) {
            error_ = "no IFetch PTW request pending";
            return false;
        }
        const auto capture = [&]() {
            IFetchPtwResponse result;
            result.s2xlate = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s2xlate.U());
            result.s1_tag = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag.U();
            result.s1_asid = static_cast<std::uint16_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid.U());
            result.s1_vmid = static_cast<std::uint16_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid.U());
            result.s1_n = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_n.B();
            result.s1_pbmt = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt.U());
            result.s1_d = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d.B();
            result.s1_a = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a.B();
            result.s1_g = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g.B();
            result.s1_u = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u.B();
            result.s1_x = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x.B();
            result.s1_w = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w.B();
            result.s1_r = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r.B();
            result.s1_level = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_level.U());
            result.s1_v = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_v.B();
            result.s1_ppn = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn.U();
            result.s1_addr_low = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_addr_low.U());
#define CAPTURE_IFETCH_SECTOR(index)                                           \
            result.s1_ppn_low[index] = static_cast<std::uint8_t>(             \
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_##index.U());  \
            result.s1_valididx |= static_cast<std::uint8_t>(                  \
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_##index.B())  \
                << index;                                                     \
            result.s1_pteidx |= static_cast<std::uint8_t>(                    \
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_##index.B())    \
                << index
            CAPTURE_IFETCH_SECTOR(0);
            CAPTURE_IFETCH_SECTOR(1);
            CAPTURE_IFETCH_SECTOR(2);
            CAPTURE_IFETCH_SECTOR(3);
            CAPTURE_IFETCH_SECTOR(4);
            CAPTURE_IFETCH_SECTOR(5);
            CAPTURE_IFETCH_SECTOR(6);
            CAPTURE_IFETCH_SECTOR(7);
#undef CAPTURE_IFETCH_SECTOR
            result.s1_pf = dut_.io_fetch_to_mem_itlb_resp_bits_s1_pf.B();
            result.s1_af = dut_.io_fetch_to_mem_itlb_resp_bits_s1_af.B();
            result.s2_tag = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag.U();
            result.s2_vmid = static_cast<std::uint16_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid.U());
            result.s2_n = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_n.B();
            result.s2_pbmt = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt.U());
            result.s2_ppn = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn.U();
            result.s2_d = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d.B();
            result.s2_a = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a.B();
            result.s2_g = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g.B();
            result.s2_u = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u.B();
            result.s2_x = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x.B();
            result.s2_w = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w.B();
            result.s2_r = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r.B();
            result.s2_level = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_level.U());
            result.s2_gpf = dut_.io_fetch_to_mem_itlb_resp_bits_s2_gpf.B();
            result.s2_gaf = dut_.io_fetch_to_mem_itlb_resp_bits_s2_gaf.B();
            return result;
        };

        bool observed = false;
        IFetchPtwResponse held;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (dut_.io_fetch_to_mem_itlb_resp_valid.B()) {
                held = capture();
                observed = true;
                break;
            }
            tick(false);
        }
        if (!observed) {
            error_ = "timed out waiting for IFetch PTW response";
            dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
            ifetch_ptw_pending_ = 0;
            return false;
        }
        for (unsigned cycle = 0; cycle < response_stall_cycles; ++cycle) {
            if (!dut_.io_fetch_to_mem_itlb_resp_valid.B() ||
                !(capture() == held)) {
                error_ = "IFetch PTW response changed while stalled";
                dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
                ifetch_ptw_pending_ = 0;
                return false;
            }
            tick(false);
        }
        response = held;
        dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
        tick(false);
        --ifetch_ptw_pending_;
        if (ifetch_ptw_pending_ != 0) {
            dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{0});
            dut_.RefreshComb();
        } else if (dut_.io_fetch_to_mem_itlb_resp_valid.B()) {
            error_ = "IFetch PTW response did not retire after handshake";
            return false;
        }
        return check_components();
    }

    bool issue_ifetch_ptw_request(
        std::uint64_t vpn, PtwTranslationMode mode,
        IFetchPtwResponse &response, unsigned response_stall_cycles = 4,
        unsigned timeout = 16384)
    {
        if (ifetch_ptw_pending_ != 0) {
            error_ = "synchronous IFetch PTW request requires an empty queue";
            return false;
        }
        return start_ifetch_ptw_request(vpn, mode, timeout) &&
            complete_ifetch_ptw_request(
                response, response_stall_cycles, timeout);
    }

    bool confirm_ifetch_ptw_flushed(unsigned quiet_cycles = 1024)
    {
        if (ifetch_ptw_pending_ == 0) {
            error_ = "no pending IFetch PTW request to flush";
            return false;
        }
        dut_.io_fetch_to_mem_itlb_req_0_valid.ImmSet(std::uint64_t{0});
        dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
        ifetch_ptw_pending_ = 0;
        for (unsigned cycle = 0; cycle < quiet_cycles; ++cycle) {
            dut_.RefreshComb();
            if (dut_.io_fetch_to_mem_itlb_resp_valid.B()) {
                error_ = "flushed IFetch PTW request produced a stale response";
                return false;
            }
            tick(false);
            if (!check_components()) {
                return false;
            }
        }
        dut_.RefreshComb();
        if (dut_.io_fetch_to_mem_itlb_resp_valid.B()) {
            error_ = "flushed IFetch PTW request produced a stale response";
            return false;
        }
        return check_components();
    }

    bool pulse_l2_hint(std::uint8_t source_id, bool is_keyword)
    {
        if (source_id >= 16) {
            error_ = "L2 hint source ID exceeds the top-level field width";
            return false;
        }
        dut_.io_l2_hint_bits_sourceId.ImmSet(source_id);
        dut_.io_l2_hint_bits_isKeyword.ImmSet(is_keyword);
        dut_.io_l2_hint_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_l2_hint_valid.ImmSet(std::uint64_t{0});
        // MemBlock registers the hint before distributing it to DCache/LSQ.
        // Let that one-cycle pulse drain and reject any unexpected terminal
        // traffic caused by a hint with no matching outstanding MSHR.
        return run_cycles(4) && check_components();
    }

    bool configure_memory_trigger(const MemoryTriggerConfig &config)
    {
        if (config.index >= 4 || config.action >= 16 ||
            (config.match_type != kTriggerMatchEqual &&
             config.match_type != kTriggerMatchGreaterOrEqual &&
             config.match_type != kTriggerMatchLessThan) ||
            config.enable_mask >= 16) {
            error_ = "invalid memory trigger configuration";
            return false;
        }
        // The CSR block presents a two-cycle delayed tdata update. Keep all
        // trigger fields explicit so this helper is independent of the idle
        // policy and can be reused by constrained-random trigger tests.
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr.ImmSet(
            config.index);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType.ImmSet(
            config.match_type);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select.ImmSet(
            config.select);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action.ImmSet(
            config.action);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain.ImmSet(
            config.chain);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store.ImmSet(
            config.store);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load.ImmSet(
            config.load);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2.ImmSet(
            config.address);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0.ImmSet(
            (config.enable_mask & 0x1U) != 0);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1.ImmSet(
            (config.enable_mask & 0x2U) != 0);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2.ImmSet(
            (config.enable_mask & 0x4U) != 0);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3.ImmSet(
            (config.enable_mask & 0x8U) != 0);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp.ImmSet(
            config.trigger_can_raise_breakpoint);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode.ImmSet(
            config.debug_mode);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid.ImmSet(
            std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid.ImmSet(
            std::uint64_t{0});
        return run_cycles(3) && check_components();
    }
    std::uint64_t cycle() const { return dut_.xclock.clk; }
    std::uint64_t tilelink_requests() const { return memory_agent_.request_count(); }
    std::uint64_t dcache_last_request_address() const
    {
        return memory_agent_.last_request_address();
    }
    std::uint64_t dcache_requests_covering_since(
        std::uint64_t address, std::uint64_t first_request) const
    {
        return memory_agent_.request_covering_count_since(
            address, first_request);
    }
    void begin_dcache_release_observation()
    {
        memory_agent_.begin_release_observation();
    }
    void end_dcache_release_observation()
    {
        memory_agent_.end_release_observation();
    }
    std::uint64_t observed_dcache_releases_covering(
        std::uint64_t address) const
    {
        return memory_agent_.observed_release_covering_count(address);
    }
    std::uint64_t observed_dcache_release_data_covering(
        std::uint64_t address) const
    {
        return memory_agent_.observed_release_covering_count(address, true);
    }
    std::uint64_t observed_dcache_stalled_releases_covering(
        std::uint64_t address, bool data_only = false) const
    {
        return memory_agent_.observed_stalled_release_covering_count(
            address, data_only);
    }
    std::uint64_t observed_dcache_release_stall_cycles_covering(
        std::uint64_t address, bool data_only = false) const
    {
        return memory_agent_.observed_release_stall_cycles_covering(
            address, data_only);
    }
    std::uint64_t dcache_gets() const { return memory_agent_.get_count(); }
    std::uint64_t dcache_refills() const { return memory_agent_.refill_count(); }
    std::uint64_t dcache_keyword_refills() const
    {
        return memory_agent_.keyword_refill_count();
    }
    std::uint64_t dcache_nonkeyword_refills() const
    {
        return memory_agent_.nonkeyword_refill_count();
    }
    std::uint64_t dcache_acquire_perms() const
    {
        return memory_agent_.acquire_perm_count();
    }
    std::uint64_t dcache_cmo_requests(StoreOp operation) const
    {
        return memory_agent_.cmo_count(cmo_operation_index(operation));
    }
    std::uint64_t dcache_last_cmo_address() const
    {
        return memory_agent_.last_cmo_address();
    }
    std::uint8_t dcache_last_cmo_source() const
    {
        return memory_agent_.last_cmo_source();
    }
    std::uint64_t tilelink_releases() const { return memory_agent_.release_count(); }
    std::uint64_t tilelink_release_data() const
    {
        return memory_agent_.release_data_count();
    }
    std::uint64_t tilelink_release_data_verified() const
    {
        return memory_agent_.release_data_verified_count();
    }
    std::uint64_t tilelink_expected_release_data_verified() const
    {
        return memory_agent_.expected_release_data_verified_count();
    }
    std::uint64_t dcache_probes() const
    {
        return memory_agent_.probe_request_count();
    }
    std::uint64_t dcache_probe_responses() const
    {
        return memory_agent_.probe_response_count();
    }
    std::uint64_t dcache_probe_data() const
    {
        return memory_agent_.probe_data_count();
    }
    std::uint64_t dcache_probe_sources() const
    {
        return memory_agent_.probe_source_count();
    }
    std::uint64_t dcache_probe_source_reuses() const
    {
        return memory_agent_.probe_source_reuse_count();
    }
    std::uint64_t dcache_probe_source_wraps() const
    {
        return memory_agent_.probe_source_wrap_count();
    }
    std::uint64_t dcache_max_probe_outstanding() const
    {
        return memory_agent_.max_probe_outstanding();
    }
    std::uint64_t dcache_probe_stalls() const
    {
        return memory_agent_.probe_stall_cycles();
    }
    std::uint64_t dcache_grant_acks() const
    {
        return memory_agent_.grant_ack_count();
    }
    std::uint64_t dcache_grant_data_beats() const
    {
        return memory_agent_.grant_data_beat_count();
    }
    std::uint64_t dcache_denied_d_beats() const
    {
        return memory_agent_.denied_d_beat_count();
    }
    std::uint64_t dcache_corrupt_d_beats() const
    {
        return memory_agent_.corrupt_d_beat_count();
    }
    std::uint64_t dcache_error_response_requests() const
    {
        return memory_agent_.error_response_request_count();
    }
    std::uint64_t dcache_error_response_refills() const
    {
        return memory_agent_.error_response_refill_count();
    }
    std::uint64_t dcache_error_response_grant_acks() const
    {
        return memory_agent_.error_response_grant_ack_count();
    }
    std::uint64_t dcache_last_error_response_address() const
    {
        return memory_agent_.last_error_response_address();
    }
    bool dcache_last_error_response_keyword() const
    {
        return memory_agent_.last_error_response_keyword();
    }
    std::uint64_t dcache_max_outstanding_requests() const
    {
        return memory_agent_.max_outstanding_requests();
    }
    std::uint64_t dcache_outstanding_requests() const
    {
        return memory_agent_.outstanding_requests();
    }
    std::uint64_t dcache_grant_ack_stalls() const
    {
        return memory_agent_.grant_ack_stall_cycles();
    }
    std::uint64_t dcache_c_payload_stability_checks() const
    {
        return memory_agent_.c_payload_stability_checks();
    }
    std::uint64_t dcache_release_target_payload_stability_checks() const
    {
        return memory_agent_.release_target_payload_stability_checks();
    }
    bool dcache_grants_drained() const
    {
        return memory_agent_.grant_acks_idle();
    }
    bool dcache_responses_idle() const
    {
        return memory_agent_.responses_idle();
    }
    std::uint64_t ptw_requests() const { return ptw_agent_.request_count(); }
    std::uint64_t ptw_error_response_requests() const
    {
        return ptw_agent_.error_response_requests();
    }
    std::uint64_t ptw_denied_d_beats() const
    {
        return ptw_agent_.denied_d_beats();
    }
    std::uint64_t ptw_corrupt_d_beats() const
    {
        return ptw_agent_.corrupt_d_beats();
    }
    std::uint64_t ptw_last_error_response_address() const
    {
        return ptw_agent_.last_error_response_address();
    }
    std::uint8_t ptw_last_error_response_source() const
    {
        return ptw_agent_.last_error_response_source();
    }
    std::uint64_t ptw_requests_covering_since(
        std::uint64_t address, std::uint64_t first_request) const
    {
        return ptw_agent_.request_covering_count_since(address, first_request);
    }
    std::uint64_t pending_ifetch_ptw_requests() const
    {
        return ifetch_ptw_pending_;
    }
    std::uint64_t dcache_request_stalls() const
    {
        return memory_agent_.request_stall_cycles();
    }
    std::uint64_t dcache_response_delays() const
    {
        return memory_agent_.response_delay_cycles();
    }
    const ResponseLatencyStats &dcache_response_latency_stats() const
    {
        return memory_agent_.response_latency_stats();
    }
    std::uint64_t ptw_request_stalls() const
    {
        return ptw_agent_.request_stall_cycles();
    }
    std::uint64_t ptw_response_delays() const
    {
        return ptw_agent_.response_delay_cycles();
    }
    std::uint64_t ptw_max_outstanding_requests() const
    {
        return ptw_agent_.max_outstanding_requests();
    }
    std::uint64_t ptw_outstanding_requests() const
    {
        return ptw_agent_.outstanding_requests();
    }
    const ResponseLatencyStats &ptw_response_latency_stats() const
    {
        return ptw_agent_.response_latency_stats();
    }
    std::uint64_t exception_vaddr()
    {
        return dut_.io_mem_to_ooo_lsqio_vaddr.U();
    }
    void select_store_exception_address(bool store)
    {
        dut_.io_ooo_to_mem_isStoreException.ImmSet(store);
    }
    std::uint64_t exception_gpaddr()
    {
        return dut_.io_mem_to_ooo_lsqio_gpaddr.U();
    }
    bool exception_is_for_vs_nonleaf_pte()
    {
        return dut_.io_mem_to_ooo_lsqio_isForVSnonLeafPTE.B();
    }
    std::uint64_t uncache_requests() const
    {
        return uncache_agent_.request_count();
    }
    std::uint64_t uncache_request_stalls() const
    {
        return uncache_agent_.request_stall_cycles();
    }
    std::uint64_t uncache_response_delays() const
    {
        return uncache_agent_.response_delay_cycles();
    }
    std::uint64_t uncache_max_outstanding_requests() const
    {
        return uncache_agent_.max_outstanding_requests();
    }
    std::uint64_t uncache_outstanding_requests() const
    {
        return uncache_agent_.outstanding_requests();
    }
    std::uint64_t uncache_error_response_requests() const
    {
        return uncache_agent_.error_response_requests();
    }
    std::uint64_t uncache_denied_d_beats() const
    {
        return uncache_agent_.denied_d_beat_count();
    }
    std::uint64_t uncache_corrupt_d_beats() const
    {
        return uncache_agent_.corrupt_d_beat_count();
    }
    const ResponseLatencyStats &uncache_response_latency_stats() const
    {
        return uncache_agent_.response_latency_stats();
    }
    bool store_mmio_valid()
    {
        return dut_.io_mem_to_ooo_lsqio_storeMmio.B();
    }
    std::uint8_t store_mmio_rob()
    {
        return static_cast<std::uint8_t>(
            dut_.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value.U());
    }
    std::uint64_t store_tlb_feedbacks() const { return store_tlb_feedbacks_; }
    std::uint64_t store_tlb_misses() const { return store_tlb_misses_; }
    std::uint64_t lq_allocated() const { return lq_allocated_; }
    std::uint64_t lq_enqueued_observed() const
    {
        return lq_enqueued_observed_;
    }
    std::uint64_t lq_dequeued() const { return lq_dequeued_; }
    std::uint64_t lq_canceled() const { return lq_canceled_; }
    std::uint64_t lq_redirect_canceled_observed() const
    {
        return lq_redirect_canceled_observed_;
    }
    std::uint64_t lq_canceled_unobserved() const
    {
        return lq_canceled_unobserved_;
    }
    std::uint64_t sq_allocated() const { return sq_allocated_; }
    std::uint64_t sq_enqueued_observed() const
    {
        return sq_enqueued_observed_;
    }
    std::uint64_t sq_dequeued() const { return sq_dequeued_; }
    std::uint64_t sq_canceled() const { return sq_canceled_; }
    std::uint64_t sq_redirect_canceled_observed() const
    {
        return sq_redirect_canceled_observed_;
    }
    std::uint64_t sq_canceled_unobserved() const
    {
        return sq_canceled_unobserved_;
    }
    std::uint64_t redirect_cancellation_events_observed() const
    {
        return redirect_cancellation_events_observed_;
    }
    const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
    lsq_enqueue_widths_observed() const
    {
        return lsq_enqueue_widths_observed_;
    }
    const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
    lsq_enqueue_lanes_observed() const
    {
        return lsq_enqueue_lanes_observed_;
    }
    std::uint64_t writebacks() const { return scoreboard_.observed(); }
    std::uint64_t prefetch_writebacks() const
    {
        return scoreboard_.prefetch_observed();
    }
    std::uint64_t store_writebacks() const { return store_scoreboard_.observed(); }
    std::uint64_t vector_load_writebacks() const
    {
        return vector_scoreboard_.load_observed();
    }
    std::uint64_t vector_store_writebacks() const
    {
        return vector_scoreboard_.store_observed();
    }
    std::uint64_t vector_fof_fix_writebacks() const
    {
        return vector_scoreboard_.fof_fix_observed();
    }
    std::size_t pending_scalar_loads() const { return scoreboard_.pending_load(); }
    bool scalar_load_pending(const LoadTransaction &transaction) const
    {
        return scoreboard_.pending(transaction);
    }
    std::size_t pending_prefetches() const { return scoreboard_.pending_prefetch(); }
    std::size_t pending_scalar_stores() const { return store_scoreboard_.pending(); }
    std::size_t pending_vector_loads() const
    {
        return vector_scoreboard_.pending_loads();
    }
    std::size_t pending_vector_stores() const
    {
        return vector_scoreboard_.pending_stores();
    }
    std::uint64_t vector_replay_feedbacks() const
    {
        return vector_replay_feedbacks_;
    }
    std::uint64_t pin_space_digest() const { return pin_space_digest_; }
    const FrontendBridgeStats &frontend_bridge_stats() const
    {
        return frontend_bridge_stats_;
    }
    const ScalarLoadFeedbackStats &scalar_load_feedback_stats() const
    {
        return scalar_load_feedback_stats_;
    }
    const LoadMmioStats &load_mmio_stats() const
    {
        return load_mmio_stats_;
    }
    const BusErrorStats &bus_error_stats() const { return bus_error_stats_; }
    const TopDownStats &top_down_stats() const { return top_down_stats_; }
    const IqSlowFeedbackStats &iq_slow_feedback_stats() const
    {
        return iq_slow_feedback_stats_;
    }
    const MemoryViolationStats &memory_violation_stats() const
    {
        return memory_violation_stats_;
    }
    const IfetchPrefetchStats &ifetch_prefetch_stats() const
    {
        return ifetch_prefetch_stats_;
    }
    const HardwarePrefetchStats &hardware_prefetch_stats() const
    {
        return hardware_prefetch_stats_;
    }
    bool sbuffer_empty()
    {
        dut_.RefreshComb();
        return generated::sample_sbuffer_empty(dut_);
    }

    void drive_top_down_misses(bool l2_miss, bool l3_miss)
    {
        dut_.io_topDownInfo_fromL2Top_l2Miss.ImmSet(l2_miss);
        dut_.io_topDownInfo_fromL2Top_l3Miss.ImmSet(l3_miss);
    }

    bool run_until_sbuffer_empty(unsigned timeout = 4096)
    {
        for (unsigned cycle = 0; cycle < timeout && !sbuffer_empty(); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!sbuffer_empty()) {
            error_ = "timed out waiting for SBuffer to empty";
            return false;
        }
        return check_components();
    }

    bool exercise_frontend_bridges(
        unsigned transaction_count, std::uint64_t seed)
    {
        if (transaction_count < 32) {
            error_ = "frontend bridge coverage requires at least 32 transactions";
            return false;
        }
        frontend_bridge_stats_ = {};

        auto mix64 = [](std::uint64_t value) -> std::uint64_t {
            value += 0x9e3779b97f4a7c15ULL;
            value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
            value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
            return value ^ (value >> 31);
        };
        auto pattern_u64 = [&](std::uint64_t domain,
                               std::uint64_t index) -> std::uint64_t {
            return mix64(seed ^ domain ^
                         (index * 0xd6e8feb86659fd93ULL));
        };
        auto pattern_bytes = [&](std::uint64_t domain, std::uint64_t index,
                                 std::size_t count) {
            std::vector<unsigned char> result(count);
            for (std::size_t byte = 0; byte < count; ++byte) {
                const std::uint64_t word = pattern_u64(
                    domain + byte / sizeof(std::uint64_t), index);
                result[byte] = static_cast<unsigned char>(
                    word >> (8 * (byte % sizeof(std::uint64_t))));
            }
            return result;
        };
        auto fail_value = [&](const char *path, std::uint64_t index,
                              const char *field, std::uint64_t expected,
                              std::uint64_t actual) {
            std::ostringstream message;
            message << "frontend bridge mismatch path=" << path
                    << " index=" << index << " field=" << field
                    << " expected=0x" << std::hex << expected
                    << " actual=0x" << actual;
            error_ = message.str();
            return false;
        };
        auto check_value = [&](const char *path, std::uint64_t index,
                               const char *field, std::uint64_t expected,
                               std::uint64_t actual) {
            if (expected != actual) {
                return fail_value(path, index, field, expected, actual);
            }
            ++frontend_bridge_stats_.field_checks;
            return true;
        };
        auto check_bytes = [&](const char *path, std::uint64_t index,
                               const char *field,
                               const std::vector<unsigned char> &expected,
                               const std::vector<unsigned char> &actual) {
            if (expected != actual) {
                std::ostringstream message;
                message << "frontend bridge mismatch path=" << path
                        << " index=" << index << " field=" << field;
                error_ = message.str();
                return false;
            }
            ++frontend_bridge_stats_.field_checks;
            return true;
        };
        auto check_index = [&](const char *path, std::uint64_t index,
                               std::uint64_t limit) {
            if (index < limit) {
                return true;
            }
            std::ostringstream message;
            message << "unexpected extra frontend bridge transfer path=" << path
                    << " index=" << index << " limit=" << limit;
            error_ = message.str();
            return false;
        };
        auto icache_address = [](std::uint64_t index) -> std::uint64_t {
            return 0x84000000ULL + index * 64;
        };
        auto instr_address = [](std::uint64_t index) -> std::uint64_t {
            return 0x88000000ULL + index * 8;
        };
        auto ctrl_opcode = [](std::uint64_t index) {
            constexpr std::array<std::uint8_t, 3> opcodes{0, 1, 4};
            return opcodes[index % opcodes.size()];
        };
        auto ctrl_size = [](std::uint64_t index) {
            return static_cast<unsigned>(index % 4);
        };
        auto ctrl_address = [&](std::uint64_t index) -> std::uint64_t {
            const unsigned bytes = 1U << ctrl_size(index);
            const unsigned lane = static_cast<unsigned>(
                ((index * 3U) & 7U) / bytes * bytes);
            return 0x100000ULL + index * 16 + lane;
        };
        auto ctrl_mask = [&](std::uint64_t index) {
            const unsigned bytes = 1U << ctrl_size(index);
            const unsigned lane = static_cast<unsigned>(ctrl_address(index) & 7U);
            std::uint8_t mask = static_cast<std::uint8_t>(
                ((1U << bytes) - 1U) << lane);
            if (ctrl_opcode(index) == 1 && bytes > 1) {
                const std::uint8_t partial = static_cast<std::uint8_t>(
                    pattern_u64(0x6374726c6d61736bULL, index));
                mask &= partial;
                if (mask == 0) {
                    mask = static_cast<std::uint8_t>(1U << lane);
                }
            }
            return mask;
        };

        std::uint64_t readiness = seed ^ 0x6a09e667f3bcc909ULL;
        if (readiness == 0) {
            readiness = 1;
        }
        auto next_ready = [&]() {
            readiness ^= readiness << 13;
            readiness ^= readiness >> 7;
            readiness ^= readiness << 17;
            return (readiness & 3U) != 0;
        };

        std::uint64_t icache_input = 0;
        std::uint64_t icache_requests = 0;
        std::uint64_t icache_response_input = 0;
        std::uint64_t icache_responses = 0;
        std::uint64_t instr_input = 0;
        std::uint64_t instr_requests = 0;
        std::uint64_t instr_response_input = 0;
        std::uint64_t instr_responses = 0;
        std::uint64_t ctrl_input = 0;
        std::uint64_t ctrl_requests = 0;
        std::uint64_t ctrl_response_input = 0;
        std::uint64_t ctrl_responses = 0;
        const std::uint64_t icache_response_count =
            std::uint64_t{transaction_count} * 2;
        const std::uint64_t timeout =
            std::uint64_t{transaction_count} * 64 + 4096;

        for (std::uint64_t local_cycle = 0; local_cycle < timeout; ++local_cycle) {
            const bool initial_request_stall = local_cycle < 16;
            const bool icache_a_ready =
                !initial_request_stall && next_ready();
            const bool instr_a_ready =
                !initial_request_stall && next_ready();
            const bool ctrl_a_ready =
                !initial_request_stall && next_ready();
            const bool ctrl_d_ready = local_cycle >= 48 && next_ready();

            const std::uint64_t icache_completed_requests =
                icache_responses / 2;
            frontend_bridge_stats_.source_credit_stalls +=
                (icache_input < transaction_count &&
                 icache_input - icache_completed_requests >= 16) +
                (instr_input < transaction_count &&
                 instr_input != instr_responses) +
                (ctrl_input < transaction_count &&
                 ctrl_input - ctrl_responses >= 32);
            const bool icache_input_valid = icache_input < transaction_count &&
                icache_input - icache_completed_requests < 16;
            dut_.auto_inner_frontendBridge_icache_in_a_valid.ImmSet(
                icache_input_valid);
            if (icache_input_valid) {
                dut_.auto_inner_frontendBridge_icache_in_a_bits_source.ImmSet(
                    icache_input & 0xfU);
                dut_.auto_inner_frontendBridge_icache_in_a_bits_address.ImmSet(
                    icache_address(icache_input));
            }
            dut_.auto_inner_frontendBridge_icache_out_a_ready.ImmSet(
                icache_a_ready);

            const bool instr_input_valid = instr_input < transaction_count &&
                instr_input == instr_responses;
            dut_.auto_inner_frontendBridge_instr_uncache_in_a_valid.ImmSet(
                instr_input_valid);
            if (instr_input_valid) {
                dut_.auto_inner_frontendBridge_instr_uncache_in_a_bits_address.ImmSet(
                    instr_address(instr_input));
            }
            dut_.auto_inner_frontendBridge_instr_uncache_out_a_ready.ImmSet(
                instr_a_ready);

            const bool ctrl_input_valid = ctrl_input < transaction_count &&
                ctrl_input - ctrl_responses < 32;
            dut_.auto_inner_frontendBridge_icachectrl_in_a_valid.ImmSet(
                ctrl_input_valid);
            if (ctrl_input_valid) {
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_opcode.ImmSet(
                    ctrl_opcode(ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_param.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_size.ImmSet(
                    ctrl_size(ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_source.ImmSet(
                    ctrl_input & 0x1fU);
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_address.ImmSet(
                    ctrl_address(ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_mask.ImmSet(
                    ctrl_mask(ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_data.ImmSet(
                    pattern_u64(0x6374726c64617461ULL, ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_corrupt.ImmSet(
                    std::uint64_t{0});
            }
            dut_.auto_inner_frontendBridge_icachectrl_out_a_ready.ImmSet(
                ctrl_a_ready);
            dut_.auto_inner_frontendBridge_icachectrl_in_d_ready.ImmSet(
                ctrl_d_ready);

            const bool icache_d_valid =
                icache_response_input < icache_requests * 2;
            dut_.auto_inner_frontendBridge_icache_out_d_valid.ImmSet(
                icache_d_valid);
            if (icache_d_valid) {
                const std::uint64_t request = icache_response_input / 2;
                dut_.auto_inner_frontendBridge_icache_out_d_bits_opcode.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_icache_out_d_bits_param.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icache_out_d_bits_size.ImmSet(
                    std::uint64_t{6});
                dut_.auto_inner_frontendBridge_icache_out_d_bits_source.ImmSet(
                    request & 0xfU);
                dut_.auto_inner_frontendBridge_icache_out_d_bits_sink.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icache_out_d_bits_denied.ImmSet(
                    std::uint64_t{0});
                auto data = pattern_bytes(
                    0x6963616368656461ULL, icache_response_input, 32);
                dut_.auto_inner_frontendBridge_icache_out_d_bits_data.ImmSetBytes(
                    data);
                dut_.auto_inner_frontendBridge_icache_out_d_bits_corrupt.ImmSet(
                    (icache_response_input % 11U) == 0);
            }

            const bool instr_d_valid = instr_response_input < instr_requests;
            dut_.auto_inner_frontendBridge_instr_uncache_out_d_valid.ImmSet(
                instr_d_valid);
            if (instr_d_valid) {
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_opcode.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_param.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_size.ImmSet(
                    std::uint64_t{3});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_source.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_sink.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_denied.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_data.ImmSet(
                    pattern_u64(0x696e737472646174ULL, instr_response_input));
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_corrupt.ImmSet(
                    (instr_response_input % 13U) == 0);
            }

            const bool ctrl_d_valid = ctrl_response_input < ctrl_requests;
            dut_.auto_inner_frontendBridge_icachectrl_out_d_valid.ImmSet(
                ctrl_d_valid);
            if (ctrl_d_valid) {
                dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_opcode.ImmSet(
                    ctrl_opcode(ctrl_response_input) == 4 ? 1 : 0);
                dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_size.ImmSet(
                    ctrl_size(ctrl_response_input));
                dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_source.ImmSet(
                    ctrl_response_input & 0x1fU);
                dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_data.ImmSet(
                    pattern_u64(0x6374726c72657370ULL, ctrl_response_input));
            }

            dut_.RefreshComb();

            if (dut_.auto_inner_frontendBridge_icache_out_a_valid.B()) {
                if (!check_index("icache-a", icache_requests, transaction_count) ||
                    !check_value("icache-a", icache_requests, "opcode", 4,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_opcode.U()) ||
                    !check_value("icache-a", icache_requests, "param", 0,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_param.U()) ||
                    !check_value("icache-a", icache_requests, "size", 6,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_size.U()) ||
                    !check_value("icache-a", icache_requests, "source",
                                 icache_requests & 0xfU,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_source.U()) ||
                    !check_value("icache-a", icache_requests, "address",
                                 icache_address(icache_requests),
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_address.U()) ||
                    !check_value("icache-a", icache_requests, "alias", 0,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_user_alias.U()) ||
                    !check_value("icache-a", icache_requests, "reqSource", 1,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_user_reqSource.U()) ||
                    !check_value("icache-a", icache_requests, "needHint", 0,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_user_needHint.U()) ||
                    !check_value("icache-a", icache_requests, "mask", 0xffffffffU,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_mask.U()) ||
                    !check_bytes("icache-a", icache_requests, "data",
                                 std::vector<unsigned char>(32, 0),
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_data.GetBytes()) ||
                    !check_value("icache-a", icache_requests, "corrupt", 0,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_corrupt.U())) {
                    return false;
                }
            }
            if (dut_.auto_inner_frontendBridge_instr_uncache_out_a_valid.B()) {
                if (!check_index("instr-a", instr_requests, transaction_count) ||
                    !check_value("instr-a", instr_requests, "param", 0,
                                 dut_.auto_inner_frontendBridge_instr_uncache_out_a_bits_param.U()) ||
                    !check_value("instr-a", instr_requests, "address",
                                 instr_address(instr_requests),
                                 dut_.auto_inner_frontendBridge_instr_uncache_out_a_bits_address.U()) ||
                    !check_value("instr-a", instr_requests, "corrupt", 0,
                                 dut_.auto_inner_frontendBridge_instr_uncache_out_a_bits_corrupt.U())) {
                    return false;
                }
            }
            if (dut_.auto_inner_frontendBridge_icachectrl_out_a_valid.B()) {
                if (!check_index("ctrl-a", ctrl_requests, transaction_count) ||
                    !check_value("ctrl-a", ctrl_requests, "opcode",
                                 ctrl_opcode(ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_opcode.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "size",
                                 ctrl_size(ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_size.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "source",
                                 ctrl_requests & 0x1fU,
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_source.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "address",
                                 ctrl_address(ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_address.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "mask",
                                 ctrl_mask(ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_mask.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "data",
                                 pattern_u64(0x6374726c64617461ULL, ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_data.U())) {
                    return false;
                }
            }

            if (dut_.auto_inner_frontendBridge_icache_in_d_valid.B()) {
                if (!check_index(
                        "icache-d", icache_responses, icache_response_count) ||
                    !check_value("icache-d", icache_responses, "opcode", 1,
                                 dut_.auto_inner_frontendBridge_icache_in_d_bits_opcode.U()) ||
                    !check_value("icache-d", icache_responses, "source",
                                 (icache_responses / 2) & 0xfU,
                                 dut_.auto_inner_frontendBridge_icache_in_d_bits_source.U()) ||
                    !check_bytes("icache-d", icache_responses, "data",
                                 pattern_bytes(
                                     0x6963616368656461ULL, icache_responses, 32),
                                 dut_.auto_inner_frontendBridge_icache_in_d_bits_data.GetBytes()) ||
                    !check_value("icache-d", icache_responses, "corrupt",
                                 (icache_responses % 11U) == 0,
                                 dut_.auto_inner_frontendBridge_icache_in_d_bits_corrupt.U())) {
                    return false;
                }
            }
            if (dut_.auto_inner_frontendBridge_instr_uncache_in_d_valid.B()) {
                if (!check_index("instr-d", instr_responses, transaction_count) ||
                    !check_value("instr-d", instr_responses, "source", 0,
                                 dut_.auto_inner_frontendBridge_instr_uncache_in_d_bits_source.U()) ||
                    !check_value("instr-d", instr_responses, "data",
                                 pattern_u64(0x696e737472646174ULL, instr_responses),
                                 dut_.auto_inner_frontendBridge_instr_uncache_in_d_bits_data.U()) ||
                    !check_value("instr-d", instr_responses, "corrupt",
                                 (instr_responses % 13U) == 0,
                                 dut_.auto_inner_frontendBridge_instr_uncache_in_d_bits_corrupt.U())) {
                    return false;
                }
            }
            if (dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B()) {
                if (!check_index("ctrl-d", ctrl_responses, transaction_count) ||
                    !check_value("ctrl-d", ctrl_responses, "opcode",
                                 ctrl_opcode(ctrl_responses) == 4 ? 1 : 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_opcode.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "param", 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_param.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "size",
                                 ctrl_size(ctrl_responses),
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_size.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "source",
                                 ctrl_responses & 0x1fU,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_source.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "sink", 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_sink.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "denied", 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_denied.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "data",
                                 pattern_u64(0x6374726c72657370ULL, ctrl_responses),
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_data.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "corrupt", 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_corrupt.U())) {
                    return false;
                }
            }

            const bool icache_input_fire = icache_input_valid &&
                dut_.auto_inner_frontendBridge_icache_in_a_ready.B();
            const bool instr_input_fire = instr_input_valid &&
                dut_.auto_inner_frontendBridge_instr_uncache_in_a_ready.B();
            const bool ctrl_input_fire = ctrl_input_valid &&
                dut_.auto_inner_frontendBridge_icachectrl_in_a_ready.B();
            const bool icache_request_fire =
                dut_.auto_inner_frontendBridge_icache_out_a_valid.B() &&
                icache_a_ready;
            const bool instr_request_fire =
                dut_.auto_inner_frontendBridge_instr_uncache_out_a_valid.B() &&
                instr_a_ready;
            const bool ctrl_request_fire =
                dut_.auto_inner_frontendBridge_icachectrl_out_a_valid.B() &&
                ctrl_a_ready;
            const bool icache_response_input_fire = icache_d_valid &&
                dut_.auto_inner_frontendBridge_icache_out_d_ready.B();
            const bool instr_response_input_fire = instr_d_valid &&
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_ready.B();
            const bool ctrl_response_input_fire = ctrl_d_valid &&
                dut_.auto_inner_frontendBridge_icachectrl_out_d_ready.B();
            const bool icache_response_fire =
                dut_.auto_inner_frontendBridge_icache_in_d_valid.B();
            const bool instr_response_fire =
                dut_.auto_inner_frontendBridge_instr_uncache_in_d_valid.B();
            const bool ctrl_response_fire =
                dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B() &&
                ctrl_d_ready;

            frontend_bridge_stats_.request_stalls +=
                (icache_input_valid &&
                 !dut_.auto_inner_frontendBridge_icache_in_a_ready.B()) +
                (instr_input_valid &&
                 !dut_.auto_inner_frontendBridge_instr_uncache_in_a_ready.B()) +
                (ctrl_input_valid &&
                 !dut_.auto_inner_frontendBridge_icachectrl_in_a_ready.B()) +
                (dut_.auto_inner_frontendBridge_icache_out_a_valid.B() &&
                 !icache_a_ready) +
                (dut_.auto_inner_frontendBridge_instr_uncache_out_a_valid.B() &&
                 !instr_a_ready) +
                (dut_.auto_inner_frontendBridge_icachectrl_out_a_valid.B() &&
                 !ctrl_a_ready);
            frontend_bridge_stats_.response_stalls +=
                (icache_d_valid &&
                 !dut_.auto_inner_frontendBridge_icache_out_d_ready.B()) +
                (instr_d_valid &&
                 !dut_.auto_inner_frontendBridge_instr_uncache_out_d_ready.B()) +
                (ctrl_d_valid &&
                 !dut_.auto_inner_frontendBridge_icachectrl_out_d_ready.B()) +
                (dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B() &&
                 !ctrl_d_ready);

            tick(false);
            icache_input += icache_input_fire;
            instr_input += instr_input_fire;
            ctrl_input += ctrl_input_fire;
            icache_requests += icache_request_fire;
            instr_requests += instr_request_fire;
            ctrl_requests += ctrl_request_fire;
            icache_response_input += icache_response_input_fire;
            instr_response_input += instr_response_input_fire;
            ctrl_response_input += ctrl_response_input_fire;
            icache_responses += icache_response_fire;
            instr_responses += instr_response_fire;
            ctrl_responses += ctrl_response_fire;

            if (icache_input == transaction_count &&
                icache_requests == transaction_count &&
                icache_response_input == icache_response_count &&
                icache_responses == icache_response_count &&
                instr_input == transaction_count &&
                instr_requests == transaction_count &&
                instr_response_input == transaction_count &&
                instr_responses == transaction_count &&
                ctrl_input == transaction_count &&
                ctrl_requests == transaction_count &&
                ctrl_response_input == transaction_count &&
                ctrl_responses == transaction_count) {
                frontend_bridge_stats_.requests =
                    icache_requests + instr_requests + ctrl_requests;
                frontend_bridge_stats_.responses =
                    icache_responses + instr_responses + ctrl_responses;
                if (frontend_bridge_stats_.request_stalls == 0 ||
                    frontend_bridge_stats_.response_stalls == 0 ||
                    frontend_bridge_stats_.source_credit_stalls == 0) {
                    error_ = "frontend bridge run missed a required stall class";
                    return false;
                }
                dut_.auto_inner_frontendBridge_icache_in_a_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_in_a_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icachectrl_in_a_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icache_out_d_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icachectrl_out_d_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icache_out_a_ready.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_instr_uncache_out_a_ready.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_icachectrl_out_a_ready.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_icachectrl_in_d_ready.ImmSet(
                    std::uint64_t{1});
                return check_idle(8);
            }
        }

        std::ostringstream message;
        message << "timed out draining frontend bridges"
                << " icache=" << icache_input << '/' << icache_requests
                << '/' << icache_response_input << '/' << icache_responses
                << " instr=" << instr_input << '/' << instr_requests
                << '/' << instr_response_input << '/' << instr_responses
                << " ctrl=" << ctrl_input << '/' << ctrl_requests
                << '/' << ctrl_response_input << '/' << ctrl_responses;
        error_ = message.str();
        return false;
    }

    bool check_frontend_bridge_reset_recovery()
    {
        constexpr std::uint64_t old_icache_address = 0x84001000ULL;
        constexpr std::uint64_t old_instr_address = 0x88001000ULL;
        constexpr std::uint64_t old_ctrl_address = 0x00123464ULL;
        constexpr std::uint64_t old_ctrl_data = 0x1122334455667788ULL;
        constexpr std::uint64_t new_icache_address = 0x84002000ULL;
        constexpr std::uint64_t new_instr_address = 0x88002000ULL;
        constexpr std::uint64_t new_ctrl_address = 0x00124560ULL;
        constexpr std::uint64_t new_ctrl_data = 0x8877665544332211ULL;
        constexpr unsigned stalled_cycles = 4;

        if (!reset()) {
            return false;
        }
        frontend_reset_canceled_requests_ = 0;
        frontend_reset_canceled_responses_ = 0;
        frontend_reset_survivor_requests_ = 0;
        frontend_reset_stall_checks_ = 0;

        const auto drive_request_valid = [&](const std::array<bool, 3> &valid) {
            dut_.auto_inner_frontendBridge_icache_in_a_valid.ImmSet(valid[0]);
            dut_.auto_inner_frontendBridge_instr_uncache_in_a_valid.ImmSet(
                valid[1]);
            dut_.auto_inner_frontendBridge_icachectrl_in_a_valid.ImmSet(
                valid[2]);
        };
        const auto drive_requests = [&](std::uint64_t icache_address,
                                        std::uint64_t instr_address,
                                        std::uint64_t ctrl_address,
                                        std::uint64_t ctrl_data) {
            dut_.auto_inner_frontendBridge_icache_in_a_bits_source.ImmSet(
                std::uint64_t{3});
            dut_.auto_inner_frontendBridge_icache_in_a_bits_address.ImmSet(
                icache_address);
            dut_.auto_inner_frontendBridge_instr_uncache_in_a_bits_address.ImmSet(
                instr_address);
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_opcode.ImmSet(
                std::uint64_t{1});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_param.ImmSet(
                std::uint64_t{0});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_size.ImmSet(
                std::uint64_t{2});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_source.ImmSet(
                std::uint64_t{27});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_address.ImmSet(
                ctrl_address);
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_mask.ImmSet(
                std::uint64_t{0xf0});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_data.ImmSet(
                ctrl_data);
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_corrupt.ImmSet(
                std::uint64_t{0});
        };
        const auto request_input_fires = [&]() {
            return std::array<bool, 3>{
                dut_.auto_inner_frontendBridge_icache_in_a_ready.B(),
                dut_.auto_inner_frontendBridge_instr_uncache_in_a_ready.B(),
                dut_.auto_inner_frontendBridge_icachectrl_in_a_ready.B(),
            };
        };
        const auto request_output_valids = [&]() {
            return std::array<bool, 3>{
                dut_.auto_inner_frontendBridge_icache_out_a_valid.B(),
                dut_.auto_inner_frontendBridge_instr_uncache_out_a_valid.B(),
                dut_.auto_inner_frontendBridge_icachectrl_out_a_valid.B(),
            };
        };
        const auto check_request_payload = [&](std::uint64_t icache_address,
                                               std::uint64_t instr_address,
                                               std::uint64_t ctrl_address,
                                               std::uint64_t ctrl_data,
                                               const char *phase,
                                               bool require_all_valid) {
            const auto valid = request_output_valids();
            const bool icache_mismatch = valid[0] &&
                (dut_.auto_inner_frontendBridge_icache_out_a_bits_opcode.U() != 4 ||
                 dut_.auto_inner_frontendBridge_icache_out_a_bits_size.U() != 6 ||
                 dut_.auto_inner_frontendBridge_icache_out_a_bits_source.U() != 3 ||
                 dut_.auto_inner_frontendBridge_icache_out_a_bits_address.U() !=
                     icache_address);
            const bool instr_mismatch = valid[1] &&
                dut_.auto_inner_frontendBridge_instr_uncache_out_a_bits_address.U() !=
                    instr_address;
            const bool ctrl_mismatch = valid[2] &&
                (dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_opcode.U() != 1 ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_size.U() != 2 ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_source.U() != 27 ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_address.U() !=
                     ctrl_address ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_mask.U() !=
                     0xf0 ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_data.U() !=
                     ctrl_data);
            if ((require_all_valid &&
                 (!valid[0] || !valid[1] || !valid[2])) ||
                icache_mismatch || instr_mismatch || ctrl_mismatch) {
                std::ostringstream message;
                message << "frontend request buffer mismatch phase=" << phase
                        << " valid=" << valid[0] << valid[1] << valid[2];
                error_ = message.str();
                return false;
            }
            return true;
        };
        const auto accept_all_requests = [&](std::array<bool, 3> &accepted,
                                             const char *phase) {
            for (unsigned elapsed = 0; elapsed < 16; ++elapsed) {
                const std::array<bool, 3> valid{
                    !accepted[0], !accepted[1], !accepted[2]};
                drive_request_valid(valid);
                dut_.RefreshComb();
                const auto ready = request_input_fires();
                const std::array<bool, 3> fire{
                    valid[0] && ready[0], valid[1] && ready[1],
                    valid[2] && ready[2]};
                tick(false);
                for (unsigned path = 0; path < accepted.size(); ++path) {
                    accepted[path] = accepted[path] || fire[path];
                }
                if (accepted[0] && accepted[1] && accepted[2]) {
                    drive_request_valid({false, false, false});
                    return true;
                }
            }
            std::ostringstream message;
            message << "timed out accepting frontend requests phase=" << phase
                    << " accepted=" << accepted[0] << accepted[1]
                    << accepted[2];
            error_ = message.str();
            return false;
        };
        const auto require_no_buffered_traffic = [&](const char *phase,
                                                     unsigned cycles) {
            for (unsigned elapsed = 0; elapsed < cycles; ++elapsed) {
                dut_.RefreshComb();
                const auto request_valid = request_output_valids();
                if (request_valid[0] || request_valid[1] || request_valid[2] ||
                    dut_.auto_inner_frontendBridge_icache_in_d_valid.B() ||
                    dut_.auto_inner_frontendBridge_instr_uncache_in_d_valid.B() ||
                    dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B()) {
                    std::ostringstream message;
                    message << "stale frontend bridge traffic phase=" << phase
                            << " elapsed=" << elapsed;
                    error_ = message.str();
                    return false;
                }
                tick(false);
            }
            return true;
        };

        dut_.auto_inner_frontendBridge_icache_out_a_ready.ImmSet(
            std::uint64_t{0});
        dut_.auto_inner_frontendBridge_instr_uncache_out_a_ready.ImmSet(
            std::uint64_t{0});
        dut_.auto_inner_frontendBridge_icachectrl_out_a_ready.ImmSet(
            std::uint64_t{0});
        drive_requests(old_icache_address, old_instr_address,
                       old_ctrl_address, old_ctrl_data);
        std::array<bool, 3> accepted{};
        if (!accept_all_requests(accepted, "pre-reset-request")) {
            return false;
        }
        bool all_requests_buffered = false;
        for (unsigned elapsed = 0; elapsed < 8; ++elapsed) {
            dut_.RefreshComb();
            const auto valid = request_output_valids();
            if (valid[0] && valid[1] && valid[2]) {
                all_requests_buffered = true;
                break;
            }
            tick(false);
        }
        if (!all_requests_buffered) {
            error_ = "timed out advancing accepted frontend requests to stalled outputs";
            return false;
        }
        for (unsigned elapsed = 0; elapsed < stalled_cycles; ++elapsed) {
            dut_.RefreshComb();
            if (!check_request_payload(
                    old_icache_address, old_instr_address, old_ctrl_address,
                    old_ctrl_data, "pre-reset-stall", true)) {
                return false;
            }
            ++frontend_reset_stall_checks_;
            tick(false);
        }
        if (!reset() ||
            !require_no_buffered_traffic("request-reset", stalled_cycles)) {
            return false;
        }
        frontend_reset_canceled_requests_ = 3;

        dut_.auto_inner_frontendBridge_icachectrl_in_d_ready.ImmSet(
            std::uint64_t{0});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_opcode.ImmSet(
            std::uint64_t{1});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_size.ImmSet(
            std::uint64_t{2});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_source.ImmSet(
            std::uint64_t{27});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_data.ImmSet(
            std::uint64_t{0xa5a55a5adeadbeefULL});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_valid.ImmSet(
            std::uint64_t{1});
        bool response_accepted = false;
        for (unsigned elapsed = 0; elapsed < 16 && !response_accepted;
             ++elapsed) {
            dut_.RefreshComb();
            response_accepted =
                dut_.auto_inner_frontendBridge_icachectrl_out_d_ready.B();
            tick(false);
        }
        dut_.auto_inner_frontendBridge_icachectrl_out_d_valid.ImmSet(
            std::uint64_t{0});
        if (!response_accepted) {
            error_ = "timed out buffering ICache-control response";
            return false;
        }
        bool response_buffered = false;
        for (unsigned elapsed = 0; elapsed < 8; ++elapsed) {
            dut_.RefreshComb();
            if (dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B()) {
                response_buffered = true;
                break;
            }
            tick(false);
        }
        if (!response_buffered) {
            error_ = "timed out advancing ICache-control response to stalled output";
            return false;
        }
        for (unsigned elapsed = 0; elapsed < stalled_cycles; ++elapsed) {
            dut_.RefreshComb();
            if (!dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B() ||
                dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_opcode.U() != 1 ||
                dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_size.U() != 2 ||
                dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_source.U() != 27 ||
                dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_data.U() !=
                    0xa5a55a5adeadbeefULL) {
                error_ = "ICache-control response changed while stalled before reset";
                return false;
            }
            ++frontend_reset_stall_checks_;
            tick(false);
        }
        if (!reset() ||
            !require_no_buffered_traffic("response-reset", stalled_cycles)) {
            return false;
        }
        frontend_reset_canceled_responses_ = 1;

        dut_.auto_inner_frontendBridge_icache_out_a_ready.ImmSet(
            std::uint64_t{1});
        dut_.auto_inner_frontendBridge_instr_uncache_out_a_ready.ImmSet(
            std::uint64_t{1});
        dut_.auto_inner_frontendBridge_icachectrl_out_a_ready.ImmSet(
            std::uint64_t{1});
        dut_.auto_inner_frontendBridge_icachectrl_in_d_ready.ImmSet(
            std::uint64_t{1});
        drive_requests(new_icache_address, new_instr_address,
                       new_ctrl_address, new_ctrl_data);
        std::array<bool, 3> survivor_accepted{};
        std::array<bool, 3> survivor_emerged{};
        for (unsigned elapsed = 0; elapsed < 32; ++elapsed) {
            const std::array<bool, 3> input_valid{
                !survivor_accepted[0], !survivor_accepted[1],
                !survivor_accepted[2]};
            drive_request_valid(input_valid);
            dut_.RefreshComb();
            const auto input_ready = request_input_fires();
            const auto output_valid = request_output_valids();
            if ((output_valid[0] || output_valid[1] || output_valid[2]) &&
                !check_request_payload(
                    new_icache_address, new_instr_address, new_ctrl_address,
                    new_ctrl_data, "post-reset-survivor", false)) {
                return false;
            }
            const std::array<bool, 3> input_fire{
                input_valid[0] && input_ready[0],
                input_valid[1] && input_ready[1],
                input_valid[2] && input_ready[2]};
            tick(false);
            for (unsigned path = 0; path < survivor_accepted.size(); ++path) {
                survivor_accepted[path] =
                    survivor_accepted[path] || input_fire[path];
                if (output_valid[path]) {
                    if (survivor_emerged[path]) {
                        error_ = "duplicate post-reset frontend request";
                        return false;
                    }
                    survivor_emerged[path] = true;
                    ++frontend_reset_survivor_requests_;
                }
            }
            if (survivor_emerged[0] && survivor_emerged[1] &&
                survivor_emerged[2]) {
                drive_request_valid({false, false, false});
                break;
            }
        }
        if (frontend_reset_survivor_requests_ != 3) {
            error_ = "timed out draining post-reset frontend requests";
            return false;
        }
        return reset() && check_idle(2);
    }

    std::uint64_t frontend_reset_canceled_requests() const
    {
        return frontend_reset_canceled_requests_;
    }

    std::uint64_t frontend_reset_canceled_responses() const
    {
        return frontend_reset_canceled_responses_;
    }

    std::uint64_t frontend_reset_survivor_requests() const
    {
        return frontend_reset_survivor_requests_;
    }

    std::uint64_t frontend_reset_stall_checks() const
    {
        return frontend_reset_stall_checks_;
    }

    bool check_pin_space()
    {
        pin_space_digest_ = 1469598103934665603ULL;
        for (unsigned pattern = 0;
             pattern < generated::kPinSpacePatternCount; ++pattern) {
            generated::drive_pin_space_pattern(dut_, pattern);
            dut_.reset.ImmSet(std::uint64_t{1});
            dut_.RefreshComb();
            if (!generated::verify_pin_space_pattern(dut_, pattern)) {
                error_ = "input-space pattern readback mismatch";
                return false;
            }
            pin_space_digest_ ^= generated::sample_all_outputs(dut_);
            pin_space_digest_ *= 1099511628211ULL;
        }
        generated::drive_idle_inputs(dut_);
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{3});
        dut_.io_ooo_to_mem_tlbCsr_priv_imode.ImmSet(std::uint64_t{3});
        dut_.RefreshComb();
        return true;
    }

    bool reset()
    {
        // Re-assert external reset for every invocation.  Relying on the
        // constructor's initial value made repeated-reset scenarios silently
        // run without resetting the DUT.
        // XSTileWrap applies the same child reset to the core and tile-local
        // managers. Model that reset domain by discarding every in-flight
        // protocol transaction before the DUT can reuse source identifiers.
        memory_agent_.reset_link_state();
        ptw_agent_.reset_link_state();
        uncache_agent_.reset_link_state();
        scalar_load_feedback_stats_ = {};
        load_mmio_stats_ = {};
        iq_slow_feedback_stats_ = {};
        memory_violation_stats_ = {};
        ifetch_prefetch_stats_ = {};
        hardware_prefetch_stats_ = {};
        bus_error_stats_ = {};
        top_down_stats_ = {};
        expected_top_down_l2_miss_ = false;
        expected_top_down_l3_miss_ = false;
        ifetch_ptw_pending_ = 0;
        dut_.io_fetch_to_mem_itlb_req_0_valid.ImmSet(std::uint64_t{0});
        dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
        dut_.reset.ImmSet(std::uint64_t{1});
        for (unsigned cycle = 0; cycle < 8; ++cycle) {
            tick(false);
        }
        dut_.reset.ImmSet(std::uint64_t{0});
        for (unsigned cycle = 0; cycle < 16; ++cycle) {
            tick(false);
            if (!dut_.io_reset_backend.B()) {
                return true;
            }
        }
        error_ = "internal reset did not deassert within 16 cycles";
        return false;
    }

    bool check_reset_backend_contract()
    {
        constexpr unsigned sync_stages = 3;
        constexpr unsigned functional_release_cycles = 2 * sync_stages;
        constexpr std::array<unsigned, 3> functional_pulse_widths{1, 2, 5};

        if (!reset()) {
            return false;
        }
        reset_functional_pulses_ = 0;
        reset_dft_pulses_ = 0;
        reset_scan_transitions_ = 0;
        reset_async_assertions_ = 0;

        const auto require_reset = [&](bool expected, const char *phase,
                                       unsigned elapsed) {
            dut_.RefreshComb();
            const bool observed = dut_.io_reset_backend.B();
            if (observed == expected) {
                return true;
            }
            std::ostringstream message;
            message << "backend reset mismatch phase=" << phase
                    << " elapsed=" << elapsed
                    << " expected=" << expected
                    << " observed=" << observed;
            error_ = message.str();
            return false;
        };
        const auto require_synchronous_release = [&](unsigned latency,
                                                     const char *phase) {
            if (!require_reset(true, phase, 0)) {
                return false;
            }
            for (unsigned elapsed = 1; elapsed <= latency; ++elapsed) {
                tick(false);
                if (!require_reset(elapsed < latency, phase, elapsed)) {
                    return false;
                }
            }
            return true;
        };

        // Functional reset crosses the top-level and right MemBlock reset
        // generators. Both asynchronously assert and synchronously release.
        for (const unsigned pulse_width : functional_pulse_widths) {
            dut_.reset.ImmSet(std::uint64_t{1});
            if (!require_reset(true, "functional-assert", 0)) {
                return false;
            }
            ++reset_async_assertions_;
            for (unsigned elapsed = 0; elapsed < pulse_width; ++elapsed) {
                tick(false);
                if (!require_reset(true, "functional-hold", elapsed + 1)) {
                    return false;
                }
            }
            dut_.reset.ImmSet(std::uint64_t{0});
            if (!require_synchronous_release(
                    functional_release_cycles, "functional-release")) {
                return false;
            }
            ++reset_functional_pulses_;
        }

        // DFT functional mode selects lgc_rst_n independently in each
        // ResetGen. External functional reset must therefore be isolated, and
        // release reaches this output after one three-stage synchronizer.
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        dut_.io_dft_reset_mode.ImmSet(std::uint64_t{1});
        dut_.io_dft_reset_scan_mode.ImmSet(std::uint64_t{0});
        dut_.reset.ImmSet(std::uint64_t{1});
        if (!require_reset(false, "dft-functional-isolation", 0)) {
            return false;
        }
        tick(false);
        if (!require_reset(false, "dft-functional-isolation", 1)) {
            return false;
        }
        dut_.reset.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{0});
        if (!require_reset(true, "dft-functional-assert", 0)) {
            return false;
        }
        ++reset_async_assertions_;
        for (unsigned elapsed = 0; elapsed < 2; ++elapsed) {
            tick(false);
            if (!require_reset(true, "dft-functional-hold", elapsed + 1)) {
                return false;
            }
        }
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        if (!require_synchronous_release(
                sync_stages, "dft-functional-release")) {
            return false;
        }
        ++reset_dft_pulses_;

        // Scan mode bypasses the synchronizer output and directly projects
        // the active-low logic reset pin.
        dut_.io_dft_reset_mode.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_scan_mode.ImmSet(std::uint64_t{1});
        if (!require_reset(false, "scan-idle", 0)) {
            return false;
        }
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{0});
        if (!require_reset(true, "scan-assert", 0)) {
            return false;
        }
        ++reset_async_assertions_;
        ++reset_scan_transitions_;
        tick(false);
        if (!require_reset(true, "scan-hold", 1)) {
            return false;
        }
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        if (!require_reset(false, "scan-release", 0)) {
            return false;
        }
        ++reset_scan_transitions_;

        dut_.io_dft_reset_scan_mode.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_mode.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        return reset() && check_idle(2);
    }

    std::uint64_t reset_functional_pulses() const
    {
        return reset_functional_pulses_;
    }

    std::uint64_t reset_dft_pulses() const { return reset_dft_pulses_; }

    std::uint64_t reset_scan_transitions() const
    {
        return reset_scan_transitions_;
    }

    std::uint64_t reset_async_assertions() const
    {
        return reset_async_assertions_;
    }

    bool configure_sv39(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x90000000ULL,
        bool writable = true,
        bool noncacheable = false)
    {
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
        constexpr std::uint64_t gigabyte_mask = (std::uint64_t{1} << 30) - 1;
        if ((physical_address & gigabyte_mask) !=
            (virtual_address & gigabyte_mask)) {
            error_ = "Sv39 NC helper requires equal 1-GiB page offsets";
            return false;
        }
        const std::uint64_t physical_base = physical_address & ~gigabyte_mask;
        const std::uint64_t vpn2 = (virtual_address >> 30) & 0x1ff;
        const std::uint64_t permissions = pte_read | (writable ? pte_write : 0);
        const std::uint64_t pte = ((physical_base >> 12) << 10) |
                                  (noncacheable ? pte_pbmt_nc : 0) |
                                  pte_dirty | pte_accessed |
                                  permissions | pte_valid;
        memory_.write_u64(root_page_table + vpn2 * 8, pte);

        // Permit S-mode page-table walks and data accesses over the full PA space.
        if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
            !write_distributed_csr(0x3a0, 0x1f)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(std::uint64_t{8});
        dut_.io_ooo_to_mem_tlbCsr_satp_asid.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_satp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_mPBMTE.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{0});
        // MemBlock pipelines the TLB CSR and fence indication before the DTLBs.
        // Do not issue the cold miss while that delayed flush is still active.
        return run_cycles(16) && check_components();
    }

    bool configure_sv39_nc(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x90000000ULL,
        bool writable = true)
    {
        return configure_sv39(
            virtual_address, physical_address, root_page_table, writable, true);
    }

    bool map_sv39_4k(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            virtual_address, physical_address, root_page_table,
            ReferencePageMode::sv39, false, 0, readable, writable,
            executable, user, noncacheable, true, std::nullopt, io, global);
    }

    bool map_sv48_4k(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            virtual_address, physical_address, root_page_table,
            ReferencePageMode::sv48, false, 0, readable, writable,
            executable, user, noncacheable, true, std::nullopt, io, global);
    }

    bool activate_sv39(
        std::uint64_t root_page_table = 0x91000000ULL,
        std::uint16_t asid = 0)
    {
        return activate_stage_one(
            ReferencePageMode::sv39, root_page_table, asid);
    }

    bool activate_sv48(
        std::uint64_t root_page_table = 0x91000000ULL,
        std::uint16_t asid = 0)
    {
        return activate_stage_one(
            ReferencePageMode::sv48, root_page_table, asid);
    }

    bool activate_bare(std::uint16_t asid = 0)
    {
        return activate_stage_one(ReferencePageMode::bare, 0, asid);
    }

    bool map_sv39_leaf(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        unsigned leaf_level,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            virtual_address, physical_address, root_page_table,
            ReferencePageMode::sv39, false, leaf_level, readable, writable,
            executable, user, noncacheable, accessed, dirty, io, global);
    }

    bool map_sv39_2m(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv39_leaf(
            virtual_address, physical_address, 1, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv39_1g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv39_leaf(
            virtual_address, physical_address, 2, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv48_leaf(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        unsigned leaf_level,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            virtual_address, physical_address, root_page_table,
            ReferencePageMode::sv48, false, leaf_level, readable, writable,
            executable, user, noncacheable, accessed, dirty, io, global);
    }

    bool map_sv48_2m(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv48_leaf(
            virtual_address, physical_address, 1, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv48_1g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv48_leaf(
            virtual_address, physical_address, 2, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv48_512g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv48_leaf(
            virtual_address, physical_address, 3, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv48x4_4k(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool global = false)
    {
        return map_reference_leaf(
            guest_physical_address, host_physical_address, root_page_table,
            ReferencePageMode::sv48, true, 0, readable, writable,
            executable, true, false, true, std::nullopt, false, global);
    }

    bool map_sv48x4_leaf(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        unsigned leaf_level,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool user = true,
        bool noncacheable = false,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            guest_physical_address, host_physical_address, root_page_table,
            ReferencePageMode::sv48, true, leaf_level, readable, writable,
            executable, user, noncacheable, accessed, dirty, io, global);
    }

    bool map_sv48x4_2m(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv48x4_leaf(
            guest_physical_address, host_physical_address, 1,
            root_page_table, readable, writable, executable);
    }

    bool map_sv48x4_1g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv48x4_leaf(
            guest_physical_address, host_physical_address, 2,
            root_page_table, readable, writable, executable);
    }

    bool map_sv48x4_512g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv48x4_leaf(
            guest_physical_address, host_physical_address, 3,
            root_page_table, readable, writable, executable);
    }

    bool map_sv39_napot64k(
        std::uint64_t virtual_base,
        std::uint64_t physical_base,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false)
    {
        return map_reference_napot64k(
            virtual_base, physical_base, root_page_table,
            ReferencePageMode::sv39, false, readable, writable, executable,
            user, noncacheable, accessed, dirty, io);
    }

    bool map_sv48_napot64k(
        std::uint64_t virtual_base,
        std::uint64_t physical_base,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false)
    {
        return map_reference_napot64k(
            virtual_base, physical_base, root_page_table,
            ReferencePageMode::sv48, false, readable, writable, executable,
            user, noncacheable, accessed, dirty, io);
    }

    bool map_sv39x4_napot64k(
        std::uint64_t guest_physical_base,
        std::uint64_t host_physical_base,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool user = true,
        bool noncacheable = false,
        bool io = false)
    {
        return map_reference_napot64k(
            guest_physical_base, host_physical_base, root_page_table,
            ReferencePageMode::sv39, true, readable, writable, executable,
            user, noncacheable, accessed, dirty, io);
    }

    bool map_sv48x4_napot64k(
        std::uint64_t guest_physical_base,
        std::uint64_t host_physical_base,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool user = true,
        bool noncacheable = false,
        bool io = false)
    {
        return map_reference_napot64k(
            guest_physical_base, host_physical_base, root_page_table,
            ReferencePageMode::sv48, true, readable, writable, executable,
            user, noncacheable, accessed, dirty, io);
    }

private:
    bool map_reference_leaf(
        std::uint64_t input_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table,
        ReferencePageMode mode,
        bool x4,
        unsigned leaf_level,
        bool readable,
        bool writable,
        bool executable,
        bool user,
        bool noncacheable,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false,
        bool global = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_global = std::uint64_t{1} << 5;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
        constexpr std::uint64_t pte_pbmt_io = std::uint64_t{1} << 62;
        if (leaf_level >= reference_page_levels(mode)) {
            error_ = "page mapping leaf level exceeds selected mode";
            return false;
        }
        const unsigned offset_bits = 12 + 9 * leaf_level;
        const std::uint64_t offset_mask =
            (std::uint64_t{1} << offset_bits) - 1;
        const std::uint64_t root_mask = x4 ? 0x3fffULL : page_mask;
        if ((root_page_table & root_mask) != 0 ||
            (input_address & offset_mask) != (physical_address & offset_mask)) {
            error_ = x4
                ? "x4 mapping requires a 16-KiB root and equal leaf offsets"
                : "mapping requires an aligned root and equal leaf offsets";
            return false;
        }
        if (x4 ? !reference_gpa_in_range(input_address, mode)
               : !reference_canonical_virtual_address(input_address, mode)) {
            error_ = x4 ? "x4 mapping exceeds the guest address space"
                        : "mapping requires a canonical virtual address";
            return false;
        }
        if (writable && !readable) {
            error_ = "page mapping does not permit W=1,R=0 leaf mappings";
            return false;
        }
        if (noncacheable && io) {
            error_ = "page mapping cannot select NC and IO simultaneously";
            return false;
        }

        auto &next_tables = x4 ? next_gstage_page_table_ : next_page_table_;
        const std::uint64_t initial = root_page_table + (x4 ? 0x4000 : 0x1000);
        auto allocate_table = [&]() {
            auto [it, inserted] = next_tables.emplace(root_page_table, initial);
            const std::uint64_t result = it->second;
            it->second += 0x1000;
            return result;
        };

        const unsigned top_level = reference_page_levels(mode) - 1;
        std::uint64_t table = root_page_table;
        for (int level = static_cast<int>(top_level);
             level > static_cast<int>(leaf_level); --level) {
            const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
            const std::uint64_t index_mask =
                x4 && static_cast<unsigned>(level) == top_level ? 0x7ff : 0x1ff;
            const std::uint64_t index = (input_address >> shift) & index_mask;
            const std::uint64_t pte_address = table + index * 8;
            const std::uint64_t pte = memory_.read_u64(pte_address);
            std::uint64_t child = 0;
            if ((pte & 1U) != 0 && !reference_pte_is_leaf(pte)) {
                child = reference_pte_ppn(pte) << 12;
            } else {
                child = allocate_table();
                memory_.write_u64(
                    pte_address, ((child >> 12) << 10) | pte_valid);
            }
            table = child;
        }

        const unsigned leaf_shift = 12 + 9 * leaf_level;
        const std::uint64_t leaf_mask =
            x4 && leaf_level == top_level ? 0x7ff : 0x1ff;
        const std::uint64_t leaf_index =
            (input_address >> leaf_shift) & leaf_mask;
        const std::uint64_t flags = pte_valid |
            (readable ? pte_read : 0) |
            (writable ? pte_write : 0) |
            (executable ? pte_execute : 0) |
            (user ? pte_user : 0) |
            (global ? pte_global : 0) |
            (accessed ? pte_accessed : 0) |
            (dirty.value_or(writable) ? pte_dirty : 0) |
            (noncacheable ? pte_pbmt_nc : 0) |
            (io ? pte_pbmt_io : 0);
        memory_.write_u64(
            table + leaf_index * 8,
            (((physical_address & ~page_mask) >> 12) << 10) | flags);
        return true;
    }

    bool map_reference_napot64k(
        std::uint64_t input_base,
        std::uint64_t physical_base,
        std::uint64_t root_page_table,
        ReferencePageMode mode,
        bool x4,
        bool readable,
        bool writable,
        bool executable,
        bool user,
        bool noncacheable,
        bool accessed,
        std::optional<bool> dirty,
        bool io)
    {
        constexpr std::uint64_t napot_size = 0x10000;
        constexpr std::uint64_t napot_mask = napot_size - 1;
        constexpr std::uint64_t page_size = 0x1000;
        constexpr std::uint64_t pte_ppn_mask =
            ((std::uint64_t{1} << 44) - 1) << 10;
        constexpr std::uint64_t pte_napot = std::uint64_t{1} << 63;
        if ((input_base & napot_mask) != 0 ||
            (physical_base & napot_mask) != 0) {
            error_ = "Svnapot 64-KiB mapping requires aligned virtual and physical bases";
            return false;
        }

        for (unsigned page = 0; page < 16; ++page) {
            const std::uint64_t offset = page * page_size;
            if (!map_reference_leaf(
                    input_base + offset, physical_base + offset,
                    root_page_table, mode, x4, 0, readable, writable,
                    executable, user, noncacheable, accessed, dirty, io)) {
                return false;
            }
        }

        const std::uint64_t napot_ppn = (physical_base >> 12) | 8U;
        for (unsigned page = 0; page < 16; ++page) {
            const std::uint64_t address = input_base + page * page_size;
            const auto pte_address = reference_pte_address_at_level(
                memory_, root_page_table, address, mode, 0, x4);
            if (!pte_address) {
                error_ = "failed to locate Svnapot leaf PTE";
                return false;
            }
            const std::uint64_t ordinary_pte = memory_.read_u64(*pte_address);
            memory_.write_u64(
                *pte_address,
                (ordinary_pte & ~pte_ppn_mask) | (napot_ppn << 10) |
                    pte_napot);
        }
        return true;
    }

    bool activate_stage_one(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid)
    {
        if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
            !write_distributed_csr(0x3a0, 0x1f)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(
            static_cast<std::uint64_t>(mode));
        dut_.io_ooo_to_mem_tlbCsr_satp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_satp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_mPBMTE.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

public:

    bool map_sv39x4_4k(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool global = false)
    {
        return map_reference_leaf(
            guest_physical_address, host_physical_address, root_page_table,
            ReferencePageMode::sv39, true, 0, readable, writable,
            executable, true, false, true, std::nullopt, false, global);
    }

    bool map_sv39x4_leaf(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        unsigned leaf_level,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool user = true,
        bool noncacheable = false,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            guest_physical_address, host_physical_address, root_page_table,
            ReferencePageMode::sv39, true, leaf_level, readable, writable,
            executable, user, noncacheable, accessed, dirty, io, global);
    }

    bool map_sv39x4_2m(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv39x4_leaf(
            guest_physical_address, host_physical_address, 1,
            root_page_table, readable, writable, executable);
    }

    bool map_sv39x4_1g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv39x4_leaf(
            guest_physical_address, host_physical_address, 2,
            root_page_table, readable, writable, executable);
    }

    bool activate_two_stage(
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid = 0,
        std::uint16_t vmid = 0)
    {
        return activate_two_stage_modes(
            ReferencePageMode::sv39,
            ReferencePageMode::sv39,
            vs_root_page_table,
            g_root_page_table,
            asid,
            vmid);
    }

    bool activate_two_stage_modes(
        ReferencePageMode vs_mode,
        ReferencePageMode g_mode,
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid = 0,
        std::uint16_t vmid = 0)
    {
        if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
            !write_distributed_csr(0x3a0, 0x1f)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_vsatp_mode.ImmSet(
            static_cast<std::uint64_t>(vs_mode));
        dut_.io_ooo_to_mem_tlbCsr_vsatp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_ppn.ImmSet(vs_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_mode.ImmSet(
            static_cast<std::uint64_t>(g_mode));
        dut_.io_ooo_to_mem_tlbCsr_hgatp_vmid.ImmSet(vmid);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_ppn.ImmSet(g_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool update_stage_one_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid)
    {
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(
            static_cast<std::uint64_t>(mode));
        dut_.io_ooo_to_mem_tlbCsr_satp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_satp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool update_vs_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid)
    {
        dut_.io_ooo_to_mem_tlbCsr_vsatp_mode.ImmSet(
            static_cast<std::uint64_t>(mode));
        dut_.io_ooo_to_mem_tlbCsr_vsatp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool update_two_stage_context(
        ReferencePageMode vs_mode,
        ReferencePageMode g_mode,
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid,
        std::uint16_t vmid)
    {
        dut_.io_ooo_to_mem_tlbCsr_vsatp_mode.ImmSet(
            static_cast<std::uint64_t>(vs_mode));
        dut_.io_ooo_to_mem_tlbCsr_vsatp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_ppn.ImmSet(vs_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_mode.ImmSet(
            static_cast<std::uint64_t>(g_mode));
        dut_.io_ooo_to_mem_tlbCsr_hgatp_vmid.ImmSet(vmid);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_ppn.ImmSet(g_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool update_g_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t vmid)
    {
        dut_.io_ooo_to_mem_tlbCsr_hgatp_mode.ImmSet(
            static_cast<std::uint64_t>(mode));
        dut_.io_ooo_to_mem_tlbCsr_hgatp_vmid.ImmSet(vmid);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool set_translation_permissions(
        ReferencePrivilegeMode data_privilege,
        bool mxr = false,
        bool sum = false,
        bool vmxr = false,
        bool vsum = false)
    {
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(
            static_cast<std::uint64_t>(data_privilege));
        dut_.io_ooo_to_mem_tlbCsr_priv_mxr.ImmSet(mxr);
        dut_.io_ooo_to_mem_tlbCsr_priv_sum.ImmSet(sum);
        dut_.io_ooo_to_mem_tlbCsr_priv_vmxr.ImmSet(vmxr);
        dut_.io_ooo_to_mem_tlbCsr_priv_vsum.ImmSet(vsum);
        // TlbCsrBundle is registered and duplicated before it reaches every
        // DTLB port. Let all copies settle before issuing the first request.
        return run_cycles(16) && check_components();
    }

    bool set_instruction_privilege(ReferencePrivilegeMode privilege)
    {
        dut_.io_ooo_to_mem_tlbCsr_priv_imode.ImmSet(
            static_cast<std::uint64_t>(privilege));
        return run_cycles(16) && check_components();
    }

    bool set_debug_mode(bool enabled)
    {
        dut_.io_ooo_to_mem_tlbCsr_priv_debug.ImmSet(enabled);
        // TlbCsrBundle is registered and duplicated before its PMA/PMP use.
        return run_cycles(16) && check_components();
    }

    bool set_hypervisor_access_permissions(
        ReferencePrivilegeMode spvp,
        bool mxr = false,
        bool vmxr = false,
        bool vsum = false,
        ReferencePrivilegeMode current_privilege =
            ReferencePrivilegeMode::supervisor)
    {
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(
            static_cast<std::uint64_t>(current_privilege));
        dut_.io_ooo_to_mem_tlbCsr_priv_virt.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_priv_spvp.ImmSet(
            static_cast<std::uint64_t>(spvp));
        dut_.io_ooo_to_mem_tlbCsr_priv_mxr.ImmSet(mxr);
        dut_.io_ooo_to_mem_tlbCsr_priv_vmxr.ImmSet(vmxr);
        dut_.io_ooo_to_mem_tlbCsr_priv_vsum.ImmSet(vsum);
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool configure_pmp(
        const std::vector<std::uint64_t> &encoded_addresses,
        const std::vector<std::uint8_t> &config_bytes)
    {
        constexpr unsigned pmp_entries = 32;
        constexpr unsigned entries_per_config_csr = 8;
        constexpr std::uint16_t pmpcfg0 = 0x3a0;
        constexpr std::uint16_t pmpaddr0 = 0x3b0;
        if (encoded_addresses.size() != config_bytes.size() ||
            encoded_addresses.size() > pmp_entries) {
            error_ = "PMP address/config vectors must have equal size at most 32";
            return false;
        }
        // Address registers must be established before enabling TOR/NAPOT.
        // This ordering also lets tests observe that a locked entry rejects a
        // later address rewrite.
        for (unsigned index = 0; index < encoded_addresses.size(); ++index) {
            if (!write_distributed_csr(
                    static_cast<std::uint16_t>(pmpaddr0 + index),
                    encoded_addresses[index])) {
                return false;
            }
        }
        for (unsigned bank = 0; bank < pmp_entries / entries_per_config_csr;
             ++bank) {
            std::uint64_t packed = 0;
            for (unsigned byte = 0; byte < entries_per_config_csr; ++byte) {
                const unsigned index = bank * entries_per_config_csr + byte;
                if (index < config_bytes.size()) {
                    packed |= std::uint64_t{config_bytes[index]} << (byte * 8);
                }
            }
            if (!write_distributed_csr(
                    static_cast<std::uint16_t>(pmpcfg0 + bank * 2), packed)) {
                return false;
            }
        }
        return run_cycles(16) && check_components();
    }

    bool set_page_based_memory_types(
        bool machine_enabled,
        bool hypervisor_enabled)
    {
        dut_.io_ooo_to_mem_tlbCsr_mPBMTE.ImmSet(machine_enabled);
        dut_.io_ooo_to_mem_tlbCsr_hPBMTE.ImmSet(hypervisor_enabled);
        return run_cycles(16) && check_components();
    }

    bool set_uncache_write_outstanding(bool enabled)
    {
        dut_.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable.ImmSet(
            enabled);
        return run_cycles(16) && check_components();
    }

    bool pulse_sbuffer_flush()
    {
        dut_.io_ooo_to_mem_flushSb.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_flushSb.ImmSet(std::uint64_t{0});
        tick();
        return check_components();
    }

    void drive_l2_flush(bool enable, bool done)
    {
        dut_.io_ooo_to_mem_csrCtrl_flush_l2_enable.ImmSet(enable);
        dut_.io_l2_flush_done.ImmSet(done);
    }

    bool outer_l2_flush_enabled()
    {
        return dut_.io_outer_l2_flush_en.B();
    }

    bool backend_l2_flush_done()
    {
        return dut_.io_mem_to_ooo_topToBackendBypass_l2FlushDone.B();
    }

    std::uint64_t l2_flush_checks() const
    {
        return l2_flush_checks_;
    }

    void drive_top_controls(
        std::uint8_t hart_id, std::uint64_t reset_vector,
        bool power_down, bool cpu_halted, bool cpu_critical_error)
    {
        dut_.io_hartId.ImmSet(hart_id & 0x3fU);
        dut_.io_outer_reset_vector.ImmSet(
            reset_vector & ((std::uint64_t{1} << 48) - 1U));
        dut_.io_ooo_to_mem_csrCtrl_power_down_enable.ImmSet(power_down);
        dut_.io_ooo_to_mem_backendToTopBypass_cpuHalted.ImmSet(cpu_halted);
        dut_.io_ooo_to_mem_backendToTopBypass_cpuCriticalError.ImmSet(
            cpu_critical_error);
    }

    std::uint8_t backend_hart_id()
    {
        return static_cast<std::uint8_t>(
            dut_.io_mem_to_ooo_topToBackendBypass_hartId.U());
    }

    std::uint64_t inner_reset_vector()
    {
        return dut_.io_inner_reset_vector.U();
    }

    bool outer_power_down_enabled()
    {
        return dut_.io_outer_power_down_en.B();
    }

    bool outer_cpu_halted()
    {
        return dut_.io_outer_cpu_halt.B();
    }

    bool outer_cpu_critical_error()
    {
        return dut_.io_outer_cpu_critical_error.B();
    }

    std::uint64_t top_control_checks() const
    {
        return top_control_checks_;
    }

    void drive_top_bridge_stimulus(const TopBridgeStimulus &stimulus)
    {
        dut_.io_ooo_to_mem_backendToTopBypass_msiAck.ImmSet(stimulus.msi_ack);
        dut_.io_resetInFrontendBypass_fromFrontend.ImmSet(
            stimulus.frontend_reset);
        dut_.io_inner_beu_errors_icache_ecc_error_valid.ImmSet(
            stimulus.beu_valid);
        dut_.io_inner_beu_errors_icache_ecc_error_bits.ImmSet(
            stimulus.beu_address & ((std::uint64_t{1} << 48) - 1U));
        dut_.io_fromTopToBackend_msiInfo_valid.ImmSet(
            stimulus.msi_info_valid);
        dut_.io_fromTopToBackend_msiInfo_bits.ImmSet(
            stimulus.msi_info & 0xfffU);
        dut_.io_fromTopToBackend_clintTime_valid.ImmSet(
            stimulus.clint_time_valid);
        dut_.io_fromTopToBackend_clintTime_bits.ImmSet(stimulus.clint_time);
        dut_.auto_inner_clint_int_sink_in_0.ImmSet(stimulus.interrupt_msip);
        dut_.auto_inner_clint_int_sink_in_1.ImmSet(stimulus.interrupt_mtip);
        dut_.auto_inner_plic_int_sink_in_0_0.ImmSet(stimulus.interrupt_meip);
        dut_.auto_inner_plic_int_sink_in_1_0.ImmSet(stimulus.interrupt_seip);
        dut_.auto_inner_debug_int_sink_in_0.ImmSet(stimulus.interrupt_debug);
        dut_.auto_inner_nmi_int_sink_in_0.ImmSet(stimulus.interrupt_nmi_31);
        dut_.auto_inner_nmi_int_sink_in_1.ImmSet(stimulus.interrupt_nmi_43);
        dut_.auto_inner_beu_local_int_sink_in_0.ImmSet(
            stimulus.interrupt_beu_local);
        for (unsigned lane = generated::kHcPerfEventFirstInputLane;
             lane <= generated::kHcPerfEventHighestInputLane; ++lane) {
            generated::drive_hc_perf_event(
                dut_, lane, stimulus.hc_perf_events[lane]);
        }
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable.ImmSet(
            stimulus.l2_prefetch.master_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable.ImmSet(
            stimulus.l2_prefetch.receive_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable.ImmSet(
            stimulus.l2_prefetch.pbop_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable.ImmSet(
            stimulus.l2_prefetch.vbop_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable.ImmSet(
            stimulus.l2_prefetch.tp_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency.ImmSet(
            stimulus.l2_prefetch.delay_latency & 0x3ffU);
    }

    std::uint64_t top_bridge_checks() const
    {
        return top_bridge_checks_;
    }

    void drive_trace_bridge_stimulus(const TraceBridgeStimulus &stimulus)
    {
        dut_.io_traceCoreInterfaceBypass_toL2Top_fromEncoder_enable.ImmSet(
            stimulus.encoder_enable);
        dut_.io_traceCoreInterfaceBypass_toL2Top_fromEncoder_stall.ImmSet(
            stimulus.encoder_stall);
        dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_priv.ImmSet(
            stimulus.privilege & 0x7U);
        dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_mstatus.ImmSet(
            stimulus.mstatus);
        dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_cause.ImmSet(
            stimulus.trap_cause);
        dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_tval.ImmSet(
            stimulus.trap_tval & ((std::uint64_t{1} << 50) - 1U));

        const auto drive_group = [](auto &valid, auto &iaddr, auto &ftq_offset,
                                    auto &itype, auto &iretire, auto &ilastsize,
                                    const TraceGroupState &group) {
            valid.ImmSet(group.valid);
            iaddr.ImmSet(group.iaddr & ((std::uint64_t{1} << 50) - 1U));
            ftq_offset.ImmSet(group.ftq_offset & 0xfU);
            itype.ImmSet(group.itype & 0xfU);
            iretire.ImmSet(group.iretire & 0x7fU);
            ilastsize.ImmSet(group.ilastsize);
        };
        drive_group(
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_valid,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iaddr,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ftqOffset,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_itype,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iretire,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ilastsize,
            stimulus.groups[0]);
        drive_group(
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_valid,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iaddr,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ftqOffset,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_itype,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iretire,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ilastsize,
            stimulus.groups[1]);
        drive_group(
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_valid,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iaddr,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ftqOffset,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_itype,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iretire,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ilastsize,
            stimulus.groups[2]);
    }

    std::uint64_t trace_bridge_checks() const
    {
        return trace_bridge_checks_;
    }

    bool check_dft_bridge_space()
    {
        dft_bridge_patterns_ = 0;
        dft_bridge_digest_ = 1469598103934665603ULL;
        dut_.reset.ImmSet(std::uint64_t{1});
        for (unsigned pattern = 0; pattern < 1024; ++pattern) {
            const bool ram_hold = (pattern & (1U << 0)) != 0;
            const bool ram_bypass = (pattern & (1U << 1)) != 0;
            const bool ram_bp_clken = (pattern & (1U << 2)) != 0;
            const bool ram_aux_clk = (pattern & (1U << 3)) != 0;
            const bool ram_aux_ckbp = (pattern & (1U << 4)) != 0;
            const bool ram_mcp_hold = (pattern & (1U << 5)) != 0;
            const bool cgen = (pattern & (1U << 6)) != 0;
            const bool lgc_rst_n = (pattern & (1U << 7)) != 0;
            const bool reset_mode = (pattern & (1U << 8)) != 0;
            const bool scan_mode = (pattern & (1U << 9)) != 0;
            dut_.io_dft_ram_hold.ImmSet(ram_hold);
            dut_.io_dft_ram_bypass.ImmSet(ram_bypass);
            dut_.io_dft_ram_bp_clken.ImmSet(ram_bp_clken);
            dut_.io_dft_ram_aux_clk.ImmSet(ram_aux_clk);
            dut_.io_dft_ram_aux_ckbp.ImmSet(ram_aux_ckbp);
            dut_.io_dft_ram_mcp_hold.ImmSet(ram_mcp_hold);
            dut_.io_dft_cgen.ImmSet(cgen);
            dut_.io_dft_reset_lgc_rst_n.ImmSet(lgc_rst_n);
            dut_.io_dft_reset_mode.ImmSet(reset_mode);
            dut_.io_dft_reset_scan_mode.ImmSet(scan_mode);
            dut_.RefreshComb();
            const bool matches =
                dut_.io_dft_frnt_ram_hold.B() == ram_hold &&
                dut_.io_dft_frnt_ram_bypass.B() == ram_bypass &&
                dut_.io_dft_frnt_ram_bp_clken.B() == ram_bp_clken &&
                dut_.io_dft_frnt_ram_aux_clk.B() == ram_aux_clk &&
                dut_.io_dft_frnt_ram_aux_ckbp.B() == ram_aux_ckbp &&
                dut_.io_dft_frnt_ram_mcp_hold.B() == ram_mcp_hold &&
                dut_.io_dft_frnt_cgen.B() == cgen &&
                dut_.io_dft_bcknd_cgen.B() == cgen &&
                dut_.io_dft_reset_frnt_lgc_rst_n.B() == lgc_rst_n &&
                dut_.io_dft_reset_frnt_mode.B() == reset_mode &&
                dut_.io_dft_reset_frnt_scan_mode.B() == scan_mode &&
                dut_.io_dft_reset_bcknd_lgc_rst_n.B() == lgc_rst_n &&
                dut_.io_dft_reset_bcknd_mode.B() == reset_mode &&
                dut_.io_dft_reset_bcknd_scan_mode.B() == scan_mode;
            if (!matches) {
                std::ostringstream message;
                message << "DFT bridge mismatch pattern=0x" << std::hex
                        << pattern;
                error_ = message.str();
                return false;
            }
            ++dft_bridge_patterns_;
            dft_bridge_digest_ ^= pattern;
            dft_bridge_digest_ *= 1099511628211ULL;
        }
        dut_.io_dft_ram_hold.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_bypass.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_bp_clken.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_aux_clk.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_aux_ckbp.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_mcp_hold.ImmSet(std::uint64_t{0});
        dut_.io_dft_cgen.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        dut_.io_dft_reset_mode.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_scan_mode.ImmSet(std::uint64_t{0});
        dut_.RefreshComb();
        return true;
    }

    std::uint64_t dft_bridge_patterns() const
    {
        return dft_bridge_patterns_;
    }

    std::uint64_t dft_bridge_digest() const
    {
        return dft_bridge_digest_;
    }

    bool set_sbuffer_timeout(std::uint32_t cycles)
    {
        constexpr std::uint32_t timeout_width = 22;
        if (cycles >= (std::uint32_t{1} << timeout_width)) {
            error_ = "SBuffer timeout exceeds the 22-bit CSR field";
            return false;
        }
        dut_.io_ooo_to_mem_csrCtrl_sbuffer_timeout.ImmSet(cycles);
        return run_cycles(4) && check_components();
    }

    bool set_mbmc(
        bool bitmap_enabled, bool confidential_mode,
        std::uint64_t bitmap_base)
    {
        if ((bitmap_base & 63U) != 0) {
            error_ = "MBMC bitmap base must be 64-byte aligned";
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BME.ImmSet(bitmap_enabled);
        dut_.io_ooo_to_mem_tlbCsr_mbmc_CMODE.ImmSet(confidential_mode);
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BMA.ImmSet(bitmap_base >> 6);
        return run_cycles(16) && check_components();
    }

    bool pulse_mbmc_bitmap_clear()
    {
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR.ImmSet(std::uint64_t{1});
        if (!run_cycles(8)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR.ImmSet(std::uint64_t{0});
        return run_cycles(8) && check_components();
    }

    bool set_pointer_masking(const PointerMaskingConfig &config)
    {
        dut_.io_ooo_to_mem_tlbCsr_pmm_mseccfg.ImmSet(
            static_cast<std::uint64_t>(config.machine));
        dut_.io_ooo_to_mem_tlbCsr_pmm_menvcfg.ImmSet(
            static_cast<std::uint64_t>(config.supervisor));
        dut_.io_ooo_to_mem_tlbCsr_pmm_henvcfg.ImmSet(
            static_cast<std::uint64_t>(config.virtual_supervisor));
        dut_.io_ooo_to_mem_tlbCsr_pmm_hstatus.ImmSet(
            static_cast<std::uint64_t>(config.hypervisor_user));
        dut_.io_ooo_to_mem_tlbCsr_pmm_senvcfg.ImmSet(
            static_cast<std::uint64_t>(config.user));
        return run_cycles(16) && check_components();
    }

    bool issue_sfence(
        std::uint64_t address = 0,
        std::uint16_t id = 0,
        bool all_virtual_addresses = true,
        bool all_contexts = true,
        bool hypervisor_virtual = false,
        bool hypervisor_guest = false,
        bool flush_pipe = true)
    {
        dut_.io_ooo_to_mem_sfence_bits_rs1.ImmSet(all_virtual_addresses);
        dut_.io_ooo_to_mem_sfence_bits_rs2.ImmSet(all_contexts);
        dut_.io_ooo_to_mem_sfence_bits_addr.ImmSet(address);
        dut_.io_ooo_to_mem_sfence_bits_id.ImmSet(id);
        dut_.io_ooo_to_mem_sfence_bits_flushPipe.ImmSet(flush_pipe);
        dut_.io_ooo_to_mem_sfence_bits_hv.ImmSet(hypervisor_virtual);
        dut_.io_ooo_to_mem_sfence_bits_hg.ImmSet(hypervisor_guest);
        dut_.io_ooo_to_mem_sfence_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_sfence_valid.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool issue_sfence_with_redirect(
        std::uint8_t redirect_rob_value,
        bool redirect_rob_flag,
        bool redirect_flush_itself,
        std::uint64_t address = 0,
        std::uint16_t id = 0,
        bool all_virtual_addresses = true,
        bool all_contexts = true,
        bool hypervisor_virtual = false,
        bool hypervisor_guest = false,
        bool flush_pipe = true)
    {
        dut_.io_ooo_to_mem_sfence_bits_rs1.ImmSet(all_virtual_addresses);
        dut_.io_ooo_to_mem_sfence_bits_rs2.ImmSet(all_contexts);
        dut_.io_ooo_to_mem_sfence_bits_addr.ImmSet(address);
        dut_.io_ooo_to_mem_sfence_bits_id.ImmSet(id);
        dut_.io_ooo_to_mem_sfence_bits_flushPipe.ImmSet(flush_pipe);
        dut_.io_ooo_to_mem_sfence_bits_hv.ImmSet(hypervisor_virtual);
        dut_.io_ooo_to_mem_sfence_bits_hg.ImmSet(hypervisor_guest);
        dut_.io_ooo_to_mem_sfence_valid.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_sfence_valid.ImmSet(std::uint64_t{0});

        // MemBlock registers sfence twice and redirect once. Pulse redirect
        // one top-level cycle later so both reach their consumers together.
        dut_.io_redirect_bits_robIdx_flag.ImmSet(redirect_rob_flag);
        dut_.io_redirect_bits_robIdx_value.ImmSet(redirect_rob_value);
        dut_.io_redirect_bits_level.ImmSet(redirect_flush_itself);
        dut_.io_redirect_bits_isVlsException.ImmSet(std::uint64_t{0});
        dut_.io_redirect_valid.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_redirect_valid.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool enable_misaligned_accesses(bool load = true, bool store = true)
    {
        dut_.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable.ImmSet(load);
        dut_.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable.ImmSet(store);
        return run_cycles(8) && check_components();
    }

    bool set_rob_head(std::uint8_t value, bool flag = false)
    {
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        tick();
        return check_components();
    }

    bool pulse_pending_load(std::uint8_t value, bool flag = false)
    {
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{0});
        return check_components();
    }

    bool wait_for_mmio_request(
        std::uint8_t value, bool flag = false, unsigned timeout = 2048)
    {
        const std::uint64_t request_before = uncache_agent_.request_count();
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{1});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            tick();
            if (!check_components()) {
                dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{0});
                return false;
            }
            if (uncache_agent_.request_count() > request_before) {
                dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{0});
                return true;
            }
        }
        dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{0});
        error_ = "timed out waiting for MMIO Uncache request";
        return false;
    }

    bool pulse_pending_store(std::uint8_t value, bool flag = false)
    {
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
        return check_components();
    }

    bool pulse_store_commit_keep_pending(
        std::uint8_t value, bool flag = false)
    {
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_lsqio_scommit.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_lsqio_scommit.ImmSet(std::uint64_t{0});
        return check_components();
    }

    bool wait_for_mmio_store_request(
        std::uint8_t value, bool flag = false, unsigned timeout = 2048)
    {
        const std::uint64_t request_before = uncache_agent_.request_count();
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        // ROB keeps pendingst high while this store remains at its head. Keep
        // the real level contract until StoreQueue emits the Uncache request.
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            tick();
            if (!check_components()) {
                dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
                return false;
            }
            if (uncache_agent_.request_count() > request_before) {
                dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
                return true;
            }
        }
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
        error_ = "timed out waiting for MMIO store Uncache request";
        return false;
    }

    bool set_wfi(bool enabled)
    {
        dut_.io_wfi_wfiReq.ImmSet(enabled);
        tick();
        return check_components();
    }

    bool wfi_safe()
    {
        dut_.RefreshComb();
        return dut_.io_wfi_wfiSafe.B();
    }

    bool require_wfi_unsafe(unsigned cycles)
    {
        for (unsigned elapsed = 0; elapsed < cycles; ++elapsed) {
            dut_.RefreshComb();
            if (dut_.io_wfi_wfiSafe.B()) {
                std::ostringstream message;
                message << "wfiSafe asserted while a manager response was pending"
                        << " elapsed=" << elapsed;
                error_ = message.str();
                return false;
            }
            tick();
            if (!check_components()) {
                return false;
            }
        }
        return true;
    }

    bool run_until_wfi_safe(unsigned timeout = 4096)
    {
        for (unsigned elapsed = 0; elapsed < timeout; ++elapsed) {
            dut_.RefreshComb();
            if (dut_.io_wfi_wfiSafe.B()) {
                return check_components();
            }
            tick();
            if (!check_components()) {
                return false;
            }
        }
        error_ = "timed out waiting for wfiSafe after manager drain";
        return false;
    }

    bool enqueue_load(const LoadTransaction &transaction)
    {
        if (!wait_for_enqueue_capacity(1, 0)) {
            return false;
        }
        generated::LsqEnqueue enqueue;
        enqueue.need_alloc = 1;
        enqueue.exception_mask = transaction.input_exception_mask;
        enqueue.trigger = transaction.input_trigger;
        enqueue.flush_pipe = transaction.input_flush_pipe;
        enqueue.fu_type = kFuTypeLoad;
        enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        enqueue.rob_flag = transaction.rob_flag;
        enqueue.rob_value = transaction.rob;
        enqueue.lq_flag = transaction.lq_flag;
        enqueue.lq_value = transaction.lq;
        enqueue.sq_flag = transaction.sq_flag;
        enqueue.sq_value = transaction.sq;
        generated::drive_lsq_enqueue(dut_, 0, enqueue);
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        ++lq_allocated_;
        return check_components();
    }

    bool enqueue_load_batch(
        const std::vector<LoadTransaction> &transactions,
        const std::vector<unsigned> &dispatch_lanes)
    {
        if (transactions.empty() ||
            transactions.size() != dispatch_lanes.size() ||
            transactions.size() > generated::kLsqEnqueueLanes ||
            !std::is_sorted(dispatch_lanes.begin(), dispatch_lanes.end())) {
            error_ = "LSQ load batch requires one sorted unique dispatch lane per load";
            return false;
        }
        std::array<bool, generated::kLsqEnqueueLanes> lane_used{};
        if (!wait_for_enqueue_capacity(transactions.size(), 0)) {
            return false;
        }
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const unsigned lane = dispatch_lanes[index];
            if (lane >= generated::kLsqEnqueueLanes || lane_used[lane]) {
                error_ = "LSQ load batch has an invalid or duplicate dispatch lane";
                generated::clear_lsq_enqueue_valids(dut_);
                return false;
            }
            lane_used[lane] = true;
            const auto &transaction = transactions[index];
            generated::LsqEnqueue enqueue;
            enqueue.need_alloc = 1;
            enqueue.exception_mask = transaction.input_exception_mask;
            enqueue.trigger = transaction.input_trigger;
            enqueue.flush_pipe = transaction.input_flush_pipe;
            enqueue.fu_type = kFuTypeLoad;
            enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
            enqueue.rob_flag = transaction.rob_flag;
            enqueue.rob_value = transaction.rob;
            enqueue.lq_flag = transaction.lq_flag;
            enqueue.lq_value = transaction.lq;
            enqueue.sq_flag = transaction.sq_flag;
            enqueue.sq_value = transaction.sq;
            generated::drive_lsq_enqueue(dut_, lane, enqueue);
        }
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        lq_allocated_ += transactions.size();
        return check_components();
    }

    bool issue_load(const LoadTransaction &transaction, unsigned timeout = 32)
    {
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_ = "scalar load immediate exceeds signed 12-bit range";
            return false;
        }
        generated::ScalarLoadIssue issue;
        issue.pc = transaction.pc.value_or(0x1000 + transaction.rob * 4);
        issue.predecode_rvc = transaction.predecode_rvc;
        issue.ftq_ptr = transaction.ftq_ptr;
        issue.ftq_offset = transaction.ftq_offset;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rf_wen = transaction.rf_wen;
        issue.fp_wen = transaction.fp_wen;
        issue.pdest = transaction.pdest;
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.lq_flag = transaction.lq_flag;
        issue.lq_value = transaction.lq;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.store_set_hit = transaction.store_set_hit;
        issue.wait_for_rob_flag = transaction.wait_for_rob_flag;
        issue.wait_for_rob_value = transaction.wait_for_rob_value;
        issue.load_wait_bit = transaction.load_wait_bit;
        issue.load_wait_strict = transaction.load_wait_strict;
        issue.imm = static_cast<std::uint32_t>(transaction.immediate);
        issue.src = scalar_issue_base_address(
            transaction.address, transaction.immediate);

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_load_issue(dut_, transaction.lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::scalar_load_issue_ready(dut_, transaction.lane);
            tick();
            if (ready) {
                generated::clear_scalar_load_issue_valid(dut_, transaction.lane);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_load_issue_valid(dut_, transaction.lane);
        error_ = "scalar load issue timed out waiting for ready";
        return false;
    }

    bool issue_load_batch(
        const std::vector<LoadTransaction> &transactions, unsigned timeout = 64,
        bool require_same_cycle = false)
    {
        if (transactions.empty() || transactions.size() > kScalarLoadLanes) {
            error_ = "scalar load batch must contain one to three transactions";
            return false;
        }
        std::array<bool, kScalarLoadLanes> lane_used{};
        std::vector<generated::ScalarLoadIssue> issues(transactions.size());
        std::vector<bool> pending(transactions.size(), true);
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const auto &transaction = transactions[index];
            if (transaction.lane >= kScalarLoadLanes || lane_used[transaction.lane]) {
                error_ = "scalar load batch lanes must be unique";
                return false;
            }
            if (!scalar_immediate_is_valid(transaction.immediate)) {
                error_ = "scalar load immediate exceeds signed 12-bit range";
                return false;
            }
            lane_used[transaction.lane] = true;
            auto &issue = issues[index];
            issue.pc = transaction.pc.value_or(
                0x1000 + transaction.rob * 4);
            issue.predecode_rvc = transaction.predecode_rvc;
            issue.ftq_ptr = transaction.ftq_ptr;
            issue.ftq_offset = transaction.ftq_offset;
            issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
            issue.rf_wen = transaction.rf_wen;
            issue.fp_wen = transaction.fp_wen;
            issue.pdest = transaction.pdest;
            issue.rob_flag = transaction.rob_flag;
            issue.rob_value = transaction.rob;
            issue.lq_flag = transaction.lq_flag;
            issue.lq_value = transaction.lq;
            issue.sq_flag = transaction.sq_flag;
            issue.sq_value = transaction.sq;
            issue.store_set_hit = transaction.store_set_hit;
            issue.wait_for_rob_flag = transaction.wait_for_rob_flag;
            issue.wait_for_rob_value = transaction.wait_for_rob_value;
            issue.load_wait_bit = transaction.load_wait_bit;
            issue.load_wait_strict = transaction.load_wait_strict;
            issue.imm = static_cast<std::uint32_t>(transaction.immediate);
            issue.src = scalar_issue_base_address(
                transaction.address, transaction.immediate);
        }

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (require_same_cycle) {
                for (std::size_t index = 0; index < transactions.size(); ++index) {
                    generated::drive_scalar_load_issue(
                        dut_, transactions[index].lane, issues[index]);
                }
                dut_.RefreshComb();
                const bool all_ready = std::all_of(
                    transactions.begin(), transactions.end(),
                    [&](const auto &transaction) {
                        return generated::scalar_load_issue_ready(
                            dut_, transaction.lane);
                    });
                if (all_ready) {
                    tick();
                    generated::clear_scalar_load_issue_valids(dut_);
                    return check_components();
                }
                generated::clear_scalar_load_issue_valids(dut_);
                tick();
                if (!check_components()) {
                    return false;
                }
                continue;
            }
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                if (pending[index]) {
                    generated::drive_scalar_load_issue(
                        dut_, transactions[index].lane, issues[index]);
                }
            }
            dut_.RefreshComb();
            std::vector<bool> accepted(transactions.size(), false);
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                accepted[index] = pending[index] && generated::scalar_load_issue_ready(
                    dut_, transactions[index].lane);
            }
            tick();
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                if (accepted[index]) {
                    pending[index] = false;
                    generated::clear_scalar_load_issue_valid(
                        dut_, transactions[index].lane);
                }
            }
            if (!check_components()) {
                return false;
            }
            bool any_pending = false;
            for (const bool value : pending) {
                any_pending = any_pending || value;
            }
            if (!any_pending) {
                return true;
            }
        }
        generated::clear_scalar_load_issue_valids(dut_);
        error_ = require_same_cycle
            ? "scalar load issue batch timed out waiting for same-cycle ready"
            : "scalar load issue batch timed out waiting for ready";
        return false;
    }

    void expect_load(const LoadTransaction &transaction)
    {
        scoreboard_.expect(
            transaction,
            memory_.expected_load(
                transaction.oracle_address.value_or(transaction.address),
                transaction.op));
    }

    void expect_load_data(const LoadTransaction &transaction, std::uint64_t data)
    {
        scoreboard_.expect(transaction, data);
    }

    void expect_prefetch(const PrefetchTransaction &transaction)
    {
        scoreboard_.expect_prefetch(transaction);
    }

    bool enqueue_prefetch(const PrefetchTransaction &transaction)
    {
        if (!wait_for_enqueue_capacity(1, 0)) {
            return false;
        }
        generated::LsqEnqueue enqueue;
        enqueue.need_alloc = 1;
        enqueue.exception_mask = transaction.input_exception_mask;
        enqueue.trigger = transaction.input_trigger;
        enqueue.flush_pipe = transaction.input_flush_pipe;
        enqueue.fu_type = kFuTypeLoad;
        enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        enqueue.rob_flag = transaction.rob_flag;
        enqueue.rob_value = transaction.rob;
        enqueue.lq_flag = transaction.lq_flag;
        enqueue.lq_value = transaction.lq;
        enqueue.sq_flag = transaction.sq_flag;
        enqueue.sq_value = transaction.sq;
        generated::drive_lsq_enqueue(dut_, 0, enqueue);
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        ++lq_allocated_;
        return check_components();
    }

    bool enqueue_prefetch_batch(
        const std::vector<PrefetchTransaction> &transactions,
        const std::vector<unsigned> &dispatch_lanes)
    {
        if (transactions.empty() ||
            transactions.size() != dispatch_lanes.size() ||
            transactions.size() > generated::kLsqEnqueueLanes ||
            !std::is_sorted(dispatch_lanes.begin(), dispatch_lanes.end())) {
            error_ = "LSQ prefetch batch requires one sorted unique dispatch lane per request";
            return false;
        }
        std::array<bool, generated::kLsqEnqueueLanes> lane_used{};
        if (!wait_for_enqueue_capacity(transactions.size(), 0)) {
            return false;
        }
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const unsigned lane = dispatch_lanes[index];
            if (lane >= generated::kLsqEnqueueLanes || lane_used[lane]) {
                error_ = "LSQ prefetch batch has an invalid or duplicate dispatch lane";
                generated::clear_lsq_enqueue_valids(dut_);
                return false;
            }
            lane_used[lane] = true;
            const auto &transaction = transactions[index];
            generated::LsqEnqueue enqueue;
            enqueue.need_alloc = 1;
            enqueue.exception_mask = transaction.input_exception_mask;
            enqueue.trigger = transaction.input_trigger;
            enqueue.flush_pipe = transaction.input_flush_pipe;
            enqueue.fu_type = kFuTypeLoad;
            enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
            enqueue.rob_flag = transaction.rob_flag;
            enqueue.rob_value = transaction.rob;
            enqueue.lq_flag = transaction.lq_flag;
            enqueue.lq_value = transaction.lq;
            enqueue.sq_flag = transaction.sq_flag;
            enqueue.sq_value = transaction.sq;
            generated::drive_lsq_enqueue(dut_, lane, enqueue);
        }
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        lq_allocated_ += transactions.size();
        return check_components();
    }

    bool issue_prefetch(
        const PrefetchTransaction &transaction, unsigned timeout = 32)
    {
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_ = "software-prefetch immediate exceeds signed 12-bit range";
            return false;
        }
        generated::ScalarLoadIssue issue;
        issue.pc = 0x1800 + transaction.rob * 4;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rf_wen = false;
        issue.fp_wen = false;
        issue.pdest = 0;
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.lq_flag = transaction.lq_flag;
        issue.lq_value = transaction.lq;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.imm = static_cast<std::uint32_t>(transaction.immediate);
        issue.src = scalar_issue_base_address(
            transaction.address, transaction.immediate);
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_load_issue(dut_, transaction.lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::scalar_load_issue_ready(
                dut_, transaction.lane);
            tick();
            if (ready) {
                generated::clear_scalar_load_issue_valid(
                    dut_, transaction.lane);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_load_issue_valid(
            dut_, transaction.lane);
        error_ = "software-prefetch issue timed out waiting for ready";
        return false;
    }

    bool issue_prefetch_batch_same_cycle(
        const std::vector<PrefetchTransaction> &transactions,
        unsigned timeout = 64)
    {
        if (transactions.empty() || transactions.size() > kScalarLoadLanes) {
            error_ = "software-prefetch batch must contain one to three requests";
            return false;
        }
        std::array<bool, kScalarLoadLanes> lane_used{};
        std::vector<generated::ScalarLoadIssue> issues(transactions.size());
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const auto &transaction = transactions[index];
            if (transaction.lane >= kScalarLoadLanes ||
                lane_used[transaction.lane]) {
                error_ = "software-prefetch batch lanes must be unique";
                return false;
            }
            if (!scalar_immediate_is_valid(transaction.immediate)) {
                error_ = "software-prefetch immediate exceeds signed 12-bit range";
                return false;
            }
            lane_used[transaction.lane] = true;
            auto &issue = issues[index];
            issue.pc = 0x1800 + transaction.rob * 4;
            issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
            issue.rf_wen = false;
            issue.fp_wen = false;
            issue.pdest = 0;
            issue.rob_flag = transaction.rob_flag;
            issue.rob_value = transaction.rob;
            issue.lq_flag = transaction.lq_flag;
            issue.lq_value = transaction.lq;
            issue.sq_flag = transaction.sq_flag;
            issue.sq_value = transaction.sq;
            issue.imm = static_cast<std::uint32_t>(transaction.immediate);
            issue.src = scalar_issue_base_address(
                transaction.address, transaction.immediate);
        }
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                generated::drive_scalar_load_issue(
                    dut_, transactions[index].lane, issues[index]);
            }
            dut_.RefreshComb();
            const bool all_ready = std::all_of(
                transactions.begin(), transactions.end(),
                [&](const auto &transaction) {
                    return generated::scalar_load_issue_ready(
                        dut_, transaction.lane);
                });
            if (all_ready) {
                tick();
                generated::clear_scalar_load_issue_valids(dut_);
                return check_components();
            }
            generated::clear_scalar_load_issue_valids(dut_);
            tick();
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_load_issue_valids(dut_);
        error_ = "software-prefetch batch timed out waiting for same-cycle ready";
        return false;
    }

    void expect_store(const StoreTransaction &transaction)
    {
        store_scoreboard_.expect(transaction);
    }

    bool enqueue_store(const StoreTransaction &transaction, std::uint8_t lq_value)
    {
        if (!wait_for_enqueue_capacity(0, 1)) {
            return false;
        }
        return enqueue_store_pressure(transaction, lq_value);
    }

    bool enqueue_store_pressure(
        const StoreTransaction &transaction, std::uint8_t lq_value)
    {
        const std::uint64_t retired = sq_dequeued_ + sq_canceled_;
        if (retired > sq_allocated_ ||
            sq_allocated_ - retired >= kStoreQueueEntries) {
            error_ = "store pressure enqueue exceeds StoreQueue capacity";
            return false;
        }
        generated::LsqEnqueue enqueue;
        enqueue.need_alloc = 2;
        enqueue.exception_mask = transaction.input_exception_mask;
        enqueue.trigger = transaction.input_trigger;
        enqueue.flush_pipe = transaction.input_flush_pipe;
        enqueue.fu_type = kFuTypeStore;
        enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        enqueue.rob_flag = transaction.rob_flag;
        enqueue.rob_value = transaction.rob;
        enqueue.lq_value = lq_value;
        enqueue.sq_flag = transaction.sq_flag;
        enqueue.sq_value = transaction.sq;
        generated::drive_lsq_enqueue(dut_, 0, enqueue);
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        ++sq_allocated_;
        scalar_store_sq_targets_[scalar_store_key(transaction)] =
            sq_allocated_ - sq_canceled_;
        return check_components();
    }

    void expect_vector(const VectorMemoryTransaction &transaction)
    {
        const auto expected = transaction.store
            ? transaction.data
            : memory_.expected_vector_load(transaction);
        vector_scoreboard_.expect(transaction, expected);
    }

    void expect_vector_data(
        const VectorMemoryTransaction &transaction,
        const std::array<unsigned char, 16> &data)
    {
        vector_scoreboard_.expect(transaction, data);
    }

    bool enqueue_vector(const VectorMemoryTransaction &transaction)
    {
        if (transaction.segment) {
            return check_components();
        }
        const unsigned elements = transaction.flow_num;
        if (!wait_for_enqueue_capacity(
                transaction.store ? 0 : elements,
                transaction.store ? elements : 0)) {
            return false;
        }
        generated::LsqEnqueue enqueue;
        enqueue.need_alloc = transaction.store ? 2 : 1;
        enqueue.exception_mask = transaction.input_exception_mask;
        enqueue.trigger = transaction.input_trigger;
        enqueue.flush_pipe = transaction.input_flush_pipe;
        enqueue.fu_type = vector_fu_type(transaction);
        enqueue.fu_op_type = vector_fu_op_type(transaction);
        enqueue.uop_idx = transaction.vuop_idx;
        enqueue.last_uop = transaction.last_uop;
        enqueue.rob_flag = transaction.rob_flag;
        enqueue.rob_value = transaction.rob;
        enqueue.lq_flag = transaction.lq_flag;
        enqueue.lq_value = transaction.lq;
        enqueue.sq_flag = transaction.sq_flag;
        enqueue.sq_value = transaction.sq;
        enqueue.num_ls_elem = transaction.flow_num;
        generated::drive_lsq_enqueue(dut_, 0, enqueue);
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        if (!transaction.store) {
            lq_allocated_ += enqueue.num_ls_elem;
        } else {
            sq_allocated_ += enqueue.num_ls_elem;
            vector_store_sq_targets_[vector_store_key(transaction)] =
                sq_allocated_ - sq_canceled_;
        }
        return check_components();
    }

    bool issue_vector(
        const VectorMemoryTransaction &transaction, unsigned timeout = 64)
    {
        const generated::VectorMemoryIssue issue =
            make_vector_memory_issue(transaction);

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_vector_memory_issue(dut_, transaction.lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::vector_memory_issue_ready(
                dut_, transaction.lane);
            tick();
            if (ready) {
                generated::clear_vector_memory_issue_valids(dut_);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_vector_memory_issue_valids(dut_);
        std::ostringstream message;
        message << "vector memory issue timed out waiting for ready"
                << " store=" << transaction.store
                << " addressing=" << static_cast<unsigned>(transaction.addressing)
                << " eew=" << static_cast<unsigned>(transaction.eew)
                << " vsew=" << static_cast<unsigned>(
                       vector_vsew(transaction))
                << " vlmul=" << static_cast<unsigned>(transaction.vlmul)
                << " vl=" << static_cast<unsigned>(transaction.vl)
                << " uop=" << static_cast<unsigned>(transaction.vuop_idx)
                << " last=" << transaction.last_uop
                << " flow_num=" << static_cast<unsigned>(transaction.flow_num)
                << " lane=" << transaction.lane
                << " rob=" << static_cast<unsigned>(transaction.rob)
                << '/' << transaction.rob_flag
                << " lq=" << static_cast<unsigned>(transaction.lq)
                << '/' << transaction.lq_flag
                << " sq=" << static_cast<unsigned>(transaction.sq)
                << '/' << transaction.sq_flag;
        error_ = message.str();
        return false;
    }

    bool issue_vector_batch_same_cycle(
        const std::vector<VectorMemoryTransaction> &transactions,
        unsigned timeout = 64)
    {
        if (transactions.empty() || transactions.size() > kVectorMemoryLanes) {
            error_ = "vector issue batch must contain one or two transactions";
            return false;
        }
        std::array<bool, kVectorMemoryLanes> lane_used{};
        std::vector<generated::VectorMemoryIssue> issues;
        issues.reserve(transactions.size());
        for (const auto &transaction : transactions) {
            if (transaction.lane >= kVectorMemoryLanes ||
                lane_used[transaction.lane]) {
                error_ = "vector issue batch lanes must be unique";
                return false;
            }
            lane_used[transaction.lane] = true;
            issues.push_back(make_vector_memory_issue(transaction));
        }

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                generated::drive_vector_memory_issue(
                    dut_, transactions[index].lane, issues[index]);
            }
            dut_.RefreshComb();
            const bool all_ready = std::all_of(
                transactions.begin(), transactions.end(), [&](const auto &transaction) {
                    return generated::vector_memory_issue_ready(
                        dut_, transaction.lane);
                });
            if (all_ready) {
                tick();
                generated::clear_vector_memory_issue_valids(dut_);
                return check_components();
            }
            generated::clear_vector_memory_issue_valids(dut_);
            tick();
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_vector_memory_issue_valids(dut_);
        error_ = "vector issue batch timed out waiting for same-cycle ready";
        return false;
    }

    bool issue_load_vector_pair(
        const LoadTransaction &load,
        const VectorMemoryTransaction &vector,
        unsigned timeout = 64)
    {
        if (!scalar_immediate_is_valid(load.immediate)) {
            error_ = "scalar load immediate exceeds signed 12-bit range";
            return false;
        }
        generated::ScalarLoadIssue scalar_issue;
        scalar_issue.pc = load.pc.value_or(0x1000 + load.rob * 4);
        scalar_issue.fu_op_type = static_cast<std::uint16_t>(load.op);
        scalar_issue.rf_wen = load.rf_wen;
        scalar_issue.fp_wen = load.fp_wen;
        scalar_issue.pdest = load.pdest;
        scalar_issue.rob_flag = load.rob_flag;
        scalar_issue.rob_value = load.rob;
        scalar_issue.lq_flag = load.lq_flag;
        scalar_issue.lq_value = load.lq;
        scalar_issue.sq_flag = load.sq_flag;
        scalar_issue.sq_value = load.sq;
        scalar_issue.imm = static_cast<std::uint32_t>(load.immediate);
        scalar_issue.src = scalar_issue_base_address(
            load.address, load.immediate);

        const generated::VectorMemoryIssue vector_issue =
            make_vector_memory_issue(vector);

        bool scalar_pending = true;
        bool vector_pending = true;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (scalar_pending) {
                generated::drive_scalar_load_issue(dut_, load.lane, scalar_issue);
            }
            if (vector_pending) {
                generated::drive_vector_memory_issue(
                    dut_, vector.lane, vector_issue);
            }
            dut_.RefreshComb();
            const bool scalar_accepted = scalar_pending &&
                generated::scalar_load_issue_ready(dut_, load.lane);
            const bool vector_accepted = vector_pending &&
                generated::vector_memory_issue_ready(dut_, vector.lane);
            tick();
            if (scalar_accepted) {
                scalar_pending = false;
                generated::clear_scalar_load_issue_valid(dut_, load.lane);
            }
            if (vector_accepted) {
                vector_pending = false;
                generated::clear_vector_memory_issue_valids(dut_);
            }
            if (!check_components()) {
                return false;
            }
            if (!scalar_pending && !vector_pending) {
                return true;
            }
        }
        generated::clear_scalar_load_issue_valids(dut_);
        generated::clear_vector_memory_issue_valids(dut_);
        error_ = "mixed scalar/vector load issue timed out waiting for ready";
        return false;
    }

    bool issue_store_address(const StoreTransaction &transaction, unsigned timeout = 32)
    {
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_ = "scalar store immediate exceeds signed 12-bit range";
            return false;
        }
        generated::ScalarStoreIssue issue;
        issue.fu_type = kFuTypeStore;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.imm = static_cast<std::uint32_t>(transaction.immediate);
        issue.src = scalar_issue_base_address(
            transaction.address, transaction.immediate);
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_store_address(
                dut_, transaction.address_lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::scalar_store_address_ready(
                dut_, transaction.address_lane);
            if (ready && !store_scoreboard_.mark_address_issued(
                    transaction, this->cycle())) {
                generated::clear_scalar_store_issue_valids(dut_);
                return false;
            }
            tick();
            if (ready) {
                generated::clear_scalar_store_issue_valids(dut_);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_store_issue_valids(dut_);
        error_ = "scalar store-address issue timed out waiting for ready";
        return false;
    }

    bool issue_store_address_batch(
        const std::vector<StoreTransaction> &transactions,
        unsigned timeout = 64)
    {
        if (transactions.empty() || transactions.size() > kScalarStoreLanes) {
            error_ = "scalar store-address batch must contain one or two transactions";
            return false;
        }
        std::array<bool, kScalarStoreLanes> lane_used{};
        std::vector<generated::ScalarStoreIssue> issues(transactions.size());
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const auto &transaction = transactions[index];
            if (transaction.address_lane >= kScalarStoreLanes ||
                lane_used[transaction.address_lane]) {
                error_ = "scalar store-address batch lanes must be unique";
                return false;
            }
            if (!scalar_immediate_is_valid(transaction.immediate)) {
                error_ = "scalar store immediate exceeds signed 12-bit range";
                return false;
            }
            lane_used[transaction.address_lane] = true;
            auto &issue = issues[index];
            issue.fu_type = kFuTypeStore;
            issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
            issue.rob_flag = transaction.rob_flag;
            issue.rob_value = transaction.rob;
            issue.sq_flag = transaction.sq_flag;
            issue.sq_value = transaction.sq;
            issue.imm = static_cast<std::uint32_t>(transaction.immediate);
            issue.src = scalar_issue_base_address(
                transaction.address, transaction.immediate);
        }

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                generated::drive_scalar_store_address(
                    dut_, transactions[index].address_lane, issues[index]);
            }
            dut_.RefreshComb();
            const bool all_ready = std::all_of(
                transactions.begin(), transactions.end(), [&](const auto &transaction) {
                    return generated::scalar_store_address_ready(
                        dut_, transaction.address_lane);
                });
            if (all_ready) {
                for (const auto &transaction : transactions) {
                    if (!store_scoreboard_.mark_address_issued(
                            transaction, this->cycle())) {
                        generated::clear_scalar_store_issue_valids(dut_);
                        return false;
                    }
                }
                tick();
                generated::clear_scalar_store_issue_valids(dut_);
                return check_components();
            }
            generated::clear_scalar_store_issue_valids(dut_);
            tick();
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_store_issue_valids(dut_);
        error_ = "scalar store-address batch timed out waiting for same-cycle ready";
        return false;
    }

    // Atomics enter the store-address port but are handled by AtomicsUnit,
    // rather than StoreQueue.  They therefore do not have an LSQ allocation
    // or StoreScoreboard entry; the scalar-load scoreboard observes their
    // old-value writeback on the atomic WB port.
    bool issue_atomic(const AtomicTransaction &transaction, unsigned timeout = 256)
    {
        generated::ScalarStoreIssue address;
        address.fu_type = kFuTypeAtomic;
        address.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        address.rf_wen = true;
        address.pdest = transaction.pdest;
        address.rob_flag = transaction.rob_flag;
        address.rob_value = transaction.rob;
        address.sq_flag = transaction.sq_flag;
        address.sq_value = transaction.sq;
        address.src = transaction.address;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_store_address(
                dut_, transaction.address_lane, address);
            dut_.RefreshComb();
            const bool ready = generated::scalar_store_address_ready(
                dut_, transaction.address_lane);
            tick();
            if (ready) {
                generated::clear_scalar_store_issue_valids(dut_);
                break;
            }
            if (!check_components()) {
                generated::clear_scalar_store_issue_valids(dut_);
                return false;
            }
            if (cycle + 1 == timeout) {
                generated::clear_scalar_store_issue_valids(dut_);
                error_ = "atomic address issue timed out waiting for ready";
                return false;
            }
        }

        const bool is_compare_swap = transaction.op == AtomicOp::amocas_w ||
            transaction.op == AtomicOp::amocas_d;
        const unsigned data_count = is_compare_swap ? 2 : 1;
        for (unsigned data_index = 0; data_index < data_count; ++data_index) {
            generated::ScalarStoreIssue data = address;
            data.src = is_compare_swap && data_index == 0
                ? transaction.compare
                : transaction.data;
            data.rf_wen = false;
            // AtomicsUnit uses the upper fuOpType bits to identify the
            // compare/swap std uop (index 0 and 1) while the low six bits
            // retain the AMOCAS.W/D operation encoding.
            data.fu_op_type = static_cast<std::uint16_t>(transaction.op) |
                static_cast<std::uint16_t>(data_index << 6);
            bool accepted = false;
            for (unsigned cycle = 0; cycle < timeout; ++cycle) {
                generated::drive_scalar_store_data(
                    dut_, transaction.data_lane, data);
                dut_.RefreshComb();
                const bool ready = generated::scalar_store_data_ready(
                    dut_, transaction.data_lane);
                tick();
                if (ready) {
                    accepted = true;
                    generated::clear_scalar_store_issue_valids(dut_);
                    break;
                }
                if (!check_components()) {
                    generated::clear_scalar_store_issue_valids(dut_);
                    return false;
                }
            }
            if (!accepted) {
                generated::clear_scalar_store_issue_valids(dut_);
                error_ = "atomic data issue timed out waiting for ready";
                return false;
            }
        }
        return check_components();
    }

    bool warm_store_translation(
        const StoreTransaction &transaction, unsigned timeout = 512)
    {
        const std::uint64_t initial_requests = ptw_agent_.request_count();
        if (!issue_store_address(transaction)) {
            return false;
        }
        for (unsigned cycle = 0;
             cycle < timeout && ptw_agent_.request_count() == initial_requests;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (ptw_agent_.request_count() == initial_requests) {
            std::ostringstream message;
            message << "timed out waiting for store DTLB page-table walk"
                    << " feedbacks=" << store_tlb_feedbacks_
                    << " misses=" << store_tlb_misses_;
            error_ = message.str();
            return false;
        }
        if (!run_cycles(64)) {
            return false;
        }
        return issue_store_address(transaction);
    }

    bool issue_store_address_until_tlb_hit(
        const StoreTransaction &transaction, unsigned timeout = 4096)
    {
        const std::uint64_t deadline = cycle() + timeout;
        while (cycle() < deadline) {
            const std::uint64_t feedbacks_before = store_tlb_feedbacks_;
            const std::uint64_t misses_before = store_tlb_misses_;
            const unsigned issue_timeout = static_cast<unsigned>(std::min(
                std::uint64_t{256}, deadline - cycle()));
            if (issue_timeout == 0 ||
                !issue_store_address(transaction, issue_timeout)) {
                return false;
            }
            while (cycle() < deadline &&
                   store_tlb_feedbacks_ == feedbacks_before) {
                tick();
                if (!check_components()) {
                    return false;
                }
            }
            if (store_tlb_feedbacks_ == feedbacks_before) {
                break;
            }
            if (store_tlb_misses_ == misses_before) {
                return check_components();
            }
            if (!run_cycles(static_cast<unsigned>(std::min(
                    std::uint64_t{8}, deadline - cycle())))) {
                return false;
            }
        }
        error_ = "timed out replaying store address until DTLB hit";
        return false;
    }

    bool run_until_store_complete_with_replay(
        const StoreTransaction &transaction, unsigned timeout = 4096,
        bool hold_pending_store = false)
    {
        // ROB keeps pendingst asserted while the same store remains at its
        // head. A cold replay may not enter StoreMisalignBuffer until well
        // after the first address issue, so retain that level across replays.
        if (hold_pending_store) {
            dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(transaction.rob);
            dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(transaction.rob_flag);
            dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        }
        const auto clear_pending_store = [&]() {
            if (hold_pending_store) {
                dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
            }
        };
        constexpr unsigned replay_interval = 32;
        for (unsigned elapsed = 0;
             elapsed < timeout && !store_scoreboard_.done();) {
            const unsigned cycles = std::min(replay_interval, timeout - elapsed);
            if (!run_cycles(cycles)) {
                clear_pending_store();
                return false;
            }
            elapsed += cycles;
            if (store_scoreboard_.done()) {
                clear_pending_store();
                return check_components();
            }
            if (!issue_store_address(transaction, replay_interval)) {
                clear_pending_store();
                return false;
            }
            ++elapsed;
        }
        clear_pending_store();
        if (!store_scoreboard_.done()) {
            error_ = "timed out waiting for replayed scalar store writeback";
            return false;
        }
        return check_components();
    }

    bool run_until_store_tlb_misses(
        std::uint64_t target, unsigned timeout = 512)
    {
        for (unsigned cycle = 0;
             cycle < timeout && store_tlb_misses_ < target; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (store_tlb_misses_ < target) {
            error_ = "timed out waiting for store TLB-miss feedback";
            return false;
        }
        return check_components();
    }

    bool run_until_ptw_requests(std::uint64_t target, unsigned timeout = 512)
    {
        for (unsigned cycle = 0;
             cycle < timeout && ptw_agent_.request_count() < target; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (ptw_agent_.request_count() < target) {
            error_ = "timed out waiting for page-table walk request";
            return false;
        }
        return check_components();
    }

    bool run_until_dcache_requests(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned elapsed = 0;
             elapsed < timeout && memory_agent_.request_count() < target;
             ++elapsed) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.request_count() < target) {
            error_ = "timed out waiting for target DCache request count";
            return false;
        }
        return check_components();
    }

    bool run_until_dcache_idle(unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
                 (!memory_agent_.responses_idle() ||
                  !memory_agent_.grant_acks_idle());
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!memory_agent_.responses_idle() ||
            !memory_agent_.grant_acks_idle()) {
            error_ = "timed out draining DCache responses and GrantAcks";
            return false;
        }
        return check_components();
    }

    bool run_until_ptw_idle(unsigned timeout = 4096)
    {
        for (unsigned cycle = 0; cycle < timeout && !ptw_agent_.idle();
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!ptw_agent_.idle()) {
            std::ostringstream message;
            message << "timed out waiting for PTW manager to become idle"
                    << " outstanding=" << ptw_agent_.outstanding_requests();
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool wait_for_cmo_store_request(
        const StoreTransaction &transaction, std::uint64_t target,
        unsigned timeout = 4096)
    {
        if (!is_cmo(transaction.op)) {
            error_ = "CMO request wait requires CLEAN, FLUSH, or INVAL";
            return false;
        }
        const unsigned operation = cmo_operation_index(transaction.op);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(transaction.rob);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(transaction.rob_flag);
        // StoreQueue samples pendingst as a level while the CMO remains at
        // the ROB head and may still be draining SBuffer.
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        for (unsigned elapsed = 0;
             elapsed < timeout && memory_agent_.cmo_count(operation) < target;
             ++elapsed) {
            tick();
            if (!check_components()) {
                dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
                return false;
            }
        }
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
        if (memory_agent_.cmo_count(operation) < target) {
            error_ = "timed out waiting for target DCache CMO request count";
            return false;
        }
        return check_components();
    }

    bool run_until_ptw_request_covering(
        std::uint64_t address,
        std::uint64_t first_request,
        unsigned timeout = 4096,
        unsigned min_response_delay = 0)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
             !ptw_agent_.request_covers_address_since(address, first_request);
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!ptw_agent_.request_covers_address_since(address, first_request)) {
            std::ostringstream message;
            message << "timed out waiting for PTW request covering address 0x"
                    << std::hex << address;
            error_ = message.str();
            return false;
        }
        if (!ptw_agent_.request_covering_address_has_min_delay_since(
                address, first_request, min_response_delay)) {
            std::ostringstream message;
            message << "PTW request covering address 0x" << std::hex
                    << address << std::dec
                    << " did not receive the required minimum response delay "
                    << min_response_delay;
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool run_until_uncache_requests(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout && uncache_agent_.request_count() < target;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (uncache_agent_.request_count() < target) {
            error_ = "timed out waiting for target Uncache request count";
            return false;
        }
        return check_components();
    }

    bool run_until_uncache_drained(
        std::uint64_t request_target, unsigned timeout = 32768)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
             (uncache_agent_.request_count() < request_target ||
              uncache_agent_.outstanding_requests() != 0);
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (uncache_agent_.request_count() < request_target ||
            uncache_agent_.outstanding_requests() != 0) {
            std::ostringstream message;
            message << "timed out waiting for Uncache traffic to drain"
                    << " requests=" << uncache_agent_.request_count()
                    << '/' << request_target
                    << " outstanding="
                    << uncache_agent_.outstanding_requests();
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool issue_store_data(const StoreTransaction &transaction, unsigned timeout = 32)
    {
        generated::ScalarStoreIssue issue;
        issue.fu_type = kFuTypeStore;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rob_value = transaction.rob;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.src = transaction.data;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_store_data(dut_, transaction.data_lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::scalar_store_data_ready(
                dut_, transaction.data_lane);
            if (ready && !store_scoreboard_.mark_data_issued(
                    transaction, transaction.data_lane, this->cycle())) {
                generated::clear_scalar_store_issue_valids(dut_);
                return false;
            }
            tick();
            if (ready) {
                generated::clear_scalar_store_issue_valids(dut_);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_store_issue_valids(dut_);
        error_ = "scalar store-data issue timed out waiting for ready";
        return false;
    }

    bool run_until_store_complete(unsigned timeout)
    {
        for (unsigned cycle = 0; cycle < timeout && !store_scoreboard_.done(); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!store_scoreboard_.done()) {
            error_ = "timed out waiting for scalar store writebacks";
            return false;
        }
        return check_components();
    }

    bool run_until_complete(unsigned timeout)
    {
        for (unsigned cycle = 0; cycle < timeout && !scoreboard_.done(); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!scoreboard_.done()) {
            error_ = "timed out waiting for scalar load writeback";
            return false;
        }
        return check_components();
    }

    bool run_until_load_complete(
        const LoadTransaction &transaction, unsigned timeout)
    {
        for (unsigned cycle = 0;
             cycle < timeout && scoreboard_.pending(transaction); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (scoreboard_.pending(transaction)) {
            std::ostringstream message;
            message << "timed out waiting for scalar load transaction"
                    << " rob=" << static_cast<unsigned>(transaction.rob)
                    << '/' << transaction.rob_flag
                    << " pdest=" << static_cast<unsigned>(transaction.pdest)
                    << " address=0x" << std::hex << transaction.address;
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool run_until_load_writebacks(std::uint64_t target, unsigned timeout)
    {
        for (unsigned cycle = 0;
             cycle < timeout && scoreboard_.observed() < target; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (scoreboard_.observed() < target) {
            std::ostringstream message;
            message << "timed out waiting for scalar load writebacks observed="
                    << scoreboard_.observed() << '/' << target;
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool run_until_vector_complete(unsigned timeout)
    {
        for (unsigned cycle = 0;
             cycle < timeout && !vector_scoreboard_.done(); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!vector_scoreboard_.done()) {
            error_ = "timed out waiting for vector memory writeback" +
                vector_scoreboard_.pending_summary();
            return false;
        }
        return check_components();
    }

    bool run_until_vector_complete_with_replays(
        const VectorMemoryTransaction &transaction,
        unsigned timeout,
        bool pulse_store_commit_after_replay = false,
        bool hold_pending_store = false)
    {
        // A store at the ROB head keeps pendingst asserted while a replaying
        // misaligned operation is still incomplete. Preserve that level so a
        // delayed replay sees the same architectural head throughout.
        if (hold_pending_store) {
            dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(transaction.rob);
            dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(transaction.rob_flag);
            dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        }
        const auto clear_pending_store = [&]() {
            if (hold_pending_store) {
                dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
            }
        };
        const std::uint64_t deadline = cycle() + timeout;
        while (!vector_scoreboard_.done() && cycle() < deadline) {
            const auto replay_it = std::find_if(
                vector_replay_requests_.begin(), vector_replay_requests_.end(),
                [&](const VectorReplayRequest &request) {
                    const unsigned entries = transaction.store
                        ? kStoreQueueEntries : kVirtualLoadQueueEntries;
                    const unsigned value = transaction.store
                        ? request.sq_value : request.lq_value;
                    const bool flag = transaction.store
                        ? request.sq_flag : request.lq_flag;
                    const unsigned base = transaction.store
                        ? transaction.sq : transaction.lq;
                    const bool base_flag = transaction.store
                        ? transaction.sq_flag : transaction.lq_flag;
                    const unsigned packed = value + (flag ? entries : 0);
                    const unsigned packed_base = base + (base_flag ? entries : 0);
                    const unsigned distance =
                        (packed + 2 * entries - packed_base) % (2 * entries);
                    return distance < transaction.flow_num;
            });
            if (replay_it == vector_replay_requests_.end()) {
                tick();
                if (!check_components()) {
                    clear_pending_store();
                    return false;
                }
                continue;
            }
            const VectorReplayRequest request = *replay_it;
            vector_replay_requests_.erase(replay_it);
            auto replay = transaction;
            replay.lane = request.lane;
            replay.is_part_replay = request.is_part_replay;
            replay.replay_mask = request.replay_mask;
            replay.replay_mb_index = request.replay_mb_index;
            if (!issue_vector(replay, 256)) {
                clear_pending_store();
                return false;
            }
            if (pulse_store_commit_after_replay && replay.store &&
                (!run_cycles(32) ||
                 !(hold_pending_store
                       ? pulse_store_commit_keep_pending(
                             replay.rob, replay.rob_flag)
                       : pulse_pending_store(replay.rob, replay.rob_flag)))) {
                clear_pending_store();
                return false;
            }
        }
        clear_pending_store();
        if (!vector_scoreboard_.done()) {
            std::ostringstream message;
            message << "timed out waiting for vector memory writeback after replay"
                    << " store=" << transaction.store
                    << " address=0x" << std::hex << transaction.address << std::dec
                    << " addressing="
                    << static_cast<unsigned>(transaction.addressing)
                    << " eew=" << static_cast<unsigned>(transaction.eew)
                    << " vl=" << static_cast<unsigned>(transaction.vl)
                    << " vstart=" << static_cast<unsigned>(transaction.vstart)
                    << " vm=" << transaction.vm
                    << " mask=0x" << std::hex << transaction.mask_bits << std::dec
                    << " active=0x" << std::hex
                    << active_vector_elements(transaction) << std::dec
                    << " index=0x" << std::hex;
            for (auto it = transaction.index.rbegin();
                 it != transaction.index.rend(); ++it) {
                message << std::setw(2) << std::setfill('0')
                        << static_cast<unsigned>(*it);
            }
            message << std::dec
                    << " stride=" << transaction.stride
                    << " flow_num="
                    << static_cast<unsigned>(transaction.flow_num)
                    << " lane=" << transaction.lane
                    << " rob=" << static_cast<unsigned>(transaction.rob)
                    << " lq=" << static_cast<unsigned>(transaction.lq)
                    << '/' << transaction.lq_flag
                    << " sq=" << static_cast<unsigned>(transaction.sq)
                    << '/' << transaction.sq_flag
                    << " ptw_requests=" << ptw_agent_.request_count()
                    << " dcache_requests=" << memory_agent_.request_count()
                    << " replay_feedbacks=" << vector_replay_feedbacks_
                    << " pending_replays=" << vector_replay_requests_.size()
                    << vector_scoreboard_.pending_summary();
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool run_until_vector_complete_with_replays(
        const std::vector<VectorMemoryTransaction> &transactions,
        unsigned timeout,
        bool pulse_store_commit_after_replay = false)
    {
        const auto matches = [](const VectorMemoryTransaction &transaction,
                                const VectorReplayRequest &request) {
            const unsigned entries = transaction.store
                ? kStoreQueueEntries : kVirtualLoadQueueEntries;
            const unsigned value = transaction.store
                ? request.sq_value : request.lq_value;
            const bool flag = transaction.store
                ? request.sq_flag : request.lq_flag;
            const unsigned base = transaction.store
                ? transaction.sq : transaction.lq;
            const bool base_flag = transaction.store
                ? transaction.sq_flag : transaction.lq_flag;
            const unsigned packed = value + (flag ? entries : 0);
            const unsigned packed_base = base + (base_flag ? entries : 0);
            const unsigned distance =
                (packed + 2 * entries - packed_base) % (2 * entries);
            return distance < transaction.flow_num;
        };

        const std::uint64_t deadline = cycle() + timeout;
        while (!vector_scoreboard_.done() && cycle() < deadline) {
            if (vector_replay_requests_.empty()) {
                tick();
                if (!check_components()) {
                    return false;
                }
                continue;
            }
            const VectorReplayRequest request = vector_replay_requests_.front();
            vector_replay_requests_.pop_front();
            const auto transaction = std::find_if(
                transactions.begin(), transactions.end(),
                [&](const VectorMemoryTransaction &candidate) {
                    return matches(candidate, request);
                });
            if (transaction == transactions.end()) {
                error_ = "vector replay did not match a mixed-window transaction";
                return false;
            }
            auto replay = *transaction;
            replay.lane = request.lane;
            replay.is_part_replay = request.is_part_replay;
            replay.replay_mask = request.replay_mask;
            replay.replay_mb_index = request.replay_mb_index;
            if (!issue_vector(replay, 256)) {
                return false;
            }
            if (pulse_store_commit_after_replay && replay.store &&
                (!run_cycles(32) ||
                 !pulse_pending_store(replay.rob, replay.rob_flag))) {
                return false;
            }
        }
        if (!vector_scoreboard_.done()) {
            std::ostringstream message;
            message << "timed out waiting for mixed vector replays"
                    << " transactions=" << transactions.size()
                    << " pending_replays=" << vector_replay_requests_.size()
                    << " ptw_requests=" << ptw_agent_.request_count()
                    << vector_scoreboard_.pending_summary();
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool run_until_all_complete(unsigned timeout)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
             (!scoreboard_.done() || !store_scoreboard_.done() ||
              !vector_scoreboard_.done());
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!scoreboard_.done() || !store_scoreboard_.done() ||
            !vector_scoreboard_.done()) {
            std::ostringstream message;
            message << "timed out waiting for mixed memory writebacks"
                    << " scalar_load_pending=" << scoreboard_.pending()
                    << " scalar_store_pending=" << store_scoreboard_.pending()
                    << " vector_pending=" << vector_scoreboard_.pending()
                    << scoreboard_.pending_summary()
                    << vector_scoreboard_.pending_summary();
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool run_until_lq_retired(unsigned timeout = 32)
    {
        for (unsigned cycle = 0;
             cycle < timeout && lq_dequeued_ + lq_canceled_ < lq_allocated_;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (lq_dequeued_ + lq_canceled_ < lq_allocated_) {
            std::ostringstream message;
            message << "timed out waiting for LQ retirement allocated="
                    << lq_allocated_ << " dequeued=" << lq_dequeued_
                    << " canceled=" << lq_canceled_;
            error_ = message.str();
            return false;
        }
        return true;
    }

    bool account_lq_cancellation(unsigned count)
    {
        if (lq_dequeued_ + lq_canceled_ + count > lq_allocated_) {
            error_ = "LQ cancellation accounting exceeds allocated entries";
            return false;
        }
        lq_canceled_ += count;
        lq_canceled_unobserved_ += count;
        return true;
    }

    bool account_sq_cancellation(unsigned count)
    {
        if (sq_dequeued_ + sq_canceled_ + count > sq_allocated_) {
            error_ = "SQ cancellation accounting exceeds allocated entries";
            return false;
        }
        sq_canceled_ += count;
        sq_canceled_unobserved_ += count;
        return true;
    }

    bool run_until_queues_retired(unsigned timeout = 512)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
             (lq_dequeued_ + lq_canceled_ < lq_allocated_ ||
              sq_dequeued_ + sq_canceled_ < sq_allocated_);
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (lq_dequeued_ + lq_canceled_ < lq_allocated_ ||
            sq_dequeued_ + sq_canceled_ < sq_allocated_) {
            std::ostringstream message;
            message << "timed out waiting for mixed LSQ retirement"
                    << " lq=" << lq_dequeued_ << '+' << lq_canceled_
                    << '/' << lq_allocated_
                    << " sq=" << sq_dequeued_ << '+' << sq_canceled_
                    << '/' << sq_allocated_;
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool commit_store(const StoreTransaction &transaction, unsigned timeout = 512)
    {
        const auto target_it = scalar_store_sq_targets_.find(
            scalar_store_key(transaction));
        if (target_it == scalar_store_sq_targets_.end()) {
            error_ = "scalar store has no recorded SQ allocation target";
            return false;
        }
        const std::uint64_t target = target_it->second;
        // A misaligned store at the ROB head can leave the SQ while another
        // outstanding class is still draining.  Do not turn that completed
        // store into a wait for the following SQ entry.
        // CMO may leave the externally monitored SQ before architectural
        // commit, but StoreQueue's shared CMO/MMIO state machine still needs
        // the backend scommit pulse to leave s_wait and accept another CMO.
        if ((is_cmo(transaction.op) || sq_dequeued_ < target) &&
            !commit_stores_through(transaction, 1)) {
            return false;
        }
        if (!run_until_sq_dequeued(target, timeout)) {
            std::ostringstream message;
            message << error_ << " rob=" << static_cast<unsigned>(transaction.rob)
                    << " rob_flag=" << transaction.rob_flag
                    << " address=0x" << std::hex << transaction.address
                    << " op=" << std::dec
                    << static_cast<unsigned>(transaction.op)
                    << " sq=" << static_cast<unsigned>(transaction.sq)
                    << " sq_target=" << target
                    << " sq_counts=" << sq_dequeued_ << '/' << sq_allocated_;
            error_ = message.str();
            return false;
        }
        scalar_store_sq_targets_.erase(target_it);
        if (is_cmo(transaction.op)) {
            return true;
        }
        const std::uint64_t raw_address = transaction.oracle_address.value_or(
            transaction.address);
        const std::uint64_t address = is_cbo_zero(transaction.op)
            ? raw_address & ~std::uint64_t{63}
            : raw_address;
        // CBO.ZERO is encoded as 0x7 but architecturally covers one cache
        // line, not a 128-byte scalar transfer.
        const unsigned bytes = scalar_store_bytes(transaction.op);
        for (unsigned byte = 0; byte < bytes; ++byte) {
            memory_.write_reference_byte(
                address + byte,
                is_cbo_zero(transaction.op)
                    ? 0
                    : static_cast<std::uint8_t>(transaction.data >> (8 * byte)));
        }
        return true;
    }

    bool commit_vector_store(
        const VectorMemoryTransaction &transaction, unsigned timeout = 512)
    {
        if (!transaction.store) {
            error_ = "cannot commit a vector load as a store";
            return false;
        }
        const auto target_it = vector_store_sq_targets_.find(
            vector_store_key(transaction));
        if (target_it == vector_store_sq_targets_.end()) {
            error_ = "vector store has no recorded SQ allocation target";
            return false;
        }
        const std::uint64_t target = target_it->second;
        StoreTransaction commit_point{
            .rob = transaction.rob,
            .rob_flag = transaction.rob_flag,
        };
        if (sq_dequeued_ < target &&
            !commit_stores_through(commit_point, 1)) {
            return false;
        }
        if (!run_until_sq_dequeued(target, timeout)) {
            std::ostringstream message;
            message << error_ << " vector_rob="
                    << static_cast<unsigned>(transaction.rob)
                    << " vector_rob_flag=" << transaction.rob_flag
                    << " vector_sq=" << static_cast<unsigned>(transaction.sq)
                    << " flow_num=" << static_cast<unsigned>(transaction.flow_num)
                    << " sq_target=" << target
                    << " sq_counts=" << sq_dequeued_ << '/' << sq_allocated_;
            error_ = message.str();
            return false;
        }
        vector_store_sq_targets_.erase(target_it);
        const std::uint64_t address = transaction.oracle_address.value_or(
            transaction.address);
        const unsigned element_bytes = 1U << vector_data_eew(transaction);
        const unsigned elements = 16U >> vector_data_eew(transaction);
        const std::uint16_t active = active_vector_elements(transaction);
        for (unsigned element = 0; element < elements; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            const std::uint64_t element_address =
                vector_element_address(transaction, element);
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                memory_.write_reference_byte(
                    element_address + byte,
                    transaction.data[element * element_bytes + byte]);
            }
        }
        return true;
    }

    // Stress bursts advance pendingPtr to retire a whole ROB window at once.
    // Keep the architectural reference image synchronized with those DUT
    // commits without issuing a second commit pulse for every store.
    void record_committed_store(const StoreTransaction &transaction)
    {
        if (is_cmo(transaction.op)) {
            return;
        }
        const std::uint64_t raw_address = transaction.oracle_address.value_or(
            transaction.address);
        const std::uint64_t address = is_cbo_zero(transaction.op)
            ? raw_address & ~std::uint64_t{63}
            : raw_address;
        const unsigned bytes = scalar_store_bytes(transaction.op);
        for (unsigned byte = 0; byte < bytes; ++byte) {
            memory_.write_reference_byte(
                address + byte,
                is_cbo_zero(transaction.op)
                    ? 0
                    : static_cast<std::uint8_t>(transaction.data >> (8 * byte)));
        }
    }

    void record_atomic_result(std::uint64_t address, std::uint64_t value)
    {
        for (unsigned byte = 0; byte < 8; ++byte) {
            memory_.write_reference_byte(
                address + byte,
                static_cast<std::uint8_t>(value >> (8 * byte)));
        }
    }

    void record_committed_vector_store(
        const VectorMemoryTransaction &transaction)
    {
        const unsigned element_bytes = 1U << vector_data_eew(transaction);
        const unsigned elements = 16U >> vector_data_eew(transaction);
        const std::uint16_t active = active_vector_elements(transaction);
        for (unsigned element = 0; element < elements; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            const std::uint64_t address = vector_element_address(transaction, element);
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                memory_.write_reference_byte(
                    address + byte,
                    transaction.data[element * element_bytes + byte]);
            }
        }
    }

    bool commit_stores_through(
        const StoreTransaction &transaction, unsigned count)
    {
        if (count == 0 || count > 8) {
            error_ = "store commit count must be between one and eight";
            return false;
        }
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(transaction.rob_flag);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(transaction.rob);
        dut_.io_ooo_to_mem_lsqio_scommit.ImmSet(count);
        tick();
        dut_.io_ooo_to_mem_lsqio_scommit.ImmSet(std::uint64_t{0});
        return check_components();
    }

    bool run_until_sq_dequeued(std::uint64_t target, unsigned timeout = 512)
    {
        for (unsigned cycle = 0; cycle < timeout && sq_dequeued_ < target; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (sq_dequeued_ < target) {
            error_ = "timed out waiting for committed store to leave SQ";
            return false;
        }
        return check_components();
    }

    bool run_until_release_data(unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout && memory_agent_.release_data_count() == 0; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.release_data_count() == 0) {
            error_ = "timed out waiting for DCache ReleaseData";
            return false;
        }
        return check_components();
    }

    bool run_until_release_data_count(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout && memory_agent_.release_data_count() < target;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.release_data_count() < target) {
            error_ = "timed out waiting for target DCache ReleaseData count";
            return false;
        }
        return check_components();
    }

    bool run_until_expected_release_data_count(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
                 memory_agent_.expected_release_data_verified_count() < target;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.expected_release_data_verified_count() < target) {
            error_ =
                "timed out waiting for expected DCache ReleaseData count";
            return false;
        }
        return check_components();
    }

    bool run_until_probe_responses(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout && memory_agent_.probe_response_count() < target;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.probe_response_count() < target ||
            !memory_agent_.probes_idle()) {
            error_ = "timed out waiting for DCache ProbeAck response";
            return false;
        }
        return check_components();
    }

    bool check_idle(unsigned cycles)
    {
        for (unsigned cycle = 0; cycle < cycles; ++cycle) {
            tick(false);
            if (!generated::expect_quiescent_outputs(dut_)) {
                error_ = "non-quiescent output during idle smoke interval";
                return false;
            }
        }
        return check_components();
    }

    bool run_cycles(unsigned cycles)
    {
        for (unsigned cycle = 0; cycle < cycles; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        return true;
    }

    bool redirect_after(std::uint8_t rob_value, bool rob_flag, bool flush_itself)
    {
        dut_.io_redirect_bits_robIdx_flag.ImmSet(rob_flag);
        dut_.io_redirect_bits_robIdx_value.ImmSet(rob_value);
        dut_.io_redirect_bits_level.ImmSet(flush_itself);
        dut_.io_redirect_bits_isVlsException.ImmSet(std::uint64_t{0});
        dut_.io_redirect_valid.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_redirect_valid.ImmSet(std::uint64_t{0});
        // MemBlock registers redirect once; the queues publish their retained
        // redirectCancelCount values after two more clock edges. Sample once
        // per known redirect instead of treating the retained value as a pulse.
        tick();
        tick();
        dut_.RefreshComb();
        const std::uint64_t lq_canceled =
            dut_.io_mem_to_ooo_lqCancelCnt.U();
        const std::uint64_t sq_canceled =
            dut_.io_mem_to_ooo_sqCancelCnt.U();
        if (lq_dequeued_ + lq_canceled_ + lq_canceled > lq_allocated_ ||
            sq_dequeued_ + sq_canceled_ + sq_canceled > sq_allocated_) {
            std::ostringstream message;
            message << "observed redirect cancellation exceeds queue allocation"
                    << " lq=" << lq_dequeued_ << '+' << lq_canceled_ << '+'
                    << lq_canceled << '/' << lq_allocated_
                    << " sq=" << sq_dequeued_ << '+' << sq_canceled_ << '+'
                    << sq_canceled << '/' << sq_allocated_;
            error_ = message.str();
            return false;
        }
        ++redirect_cancellation_events_observed_;
        lq_redirect_canceled_observed_ += lq_canceled;
        sq_redirect_canceled_observed_ += sq_canceled;
        lq_canceled_ += lq_canceled;
        sq_canceled_ += sq_canceled;
        return check_components();
    }

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_; }

private:
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
    static std::uint64_t debug_line_from_env()
    {
        const char *value = std::getenv("MEMBLOCK_MEM_DIRECT_DEBUG_LINE");
        return value == nullptr ? 0 : std::strtoull(value, nullptr, 0);
    }

    static bool debug_flag_from_env(const char *name)
    {
        const char *value = std::getenv(name);
        return value != nullptr && value[0] != '\0' && value[0] != '0';
    }

    static std::string debug_bytes(XData *signal)
    {
        if (signal == nullptr) {
            return "<missing>";
        }
        const auto bytes = signal->GetBytes();
        std::ostringstream value;
        value << "0x" << std::hex;
        for (auto it = bytes.rbegin(); it != bytes.rend(); ++it) {
            value << std::setw(2) << std::setfill('0')
                  << static_cast<unsigned>(*it);
        }
        return value.str();
    }

    static std::string debug_byte_array(
        const std::array<XData *, 16> &signals)
    {
        if (std::any_of(signals.begin(), signals.end(),
                        [](const XData *signal) { return signal == nullptr; })) {
            return "<missing>";
        }
        std::ostringstream value;
        value << "0x" << std::hex;
        for (auto it = signals.rbegin(); it != signals.rend(); ++it) {
            value << std::setw(2) << std::setfill('0')
                  << static_cast<unsigned>((*it)->U() & 0xffU);
        }
        return value.str();
    }

    static std::uint64_t debug_u(XData *signal)
    {
        return signal == nullptr ? 0 : signal->U();
    }

    static XData *debug_signal(UTMemBlock &dut, const std::string &name)
    {
        return dut.GetInternalSignal(name);
    }

    void initialize_internal_debug()
    {
        debug_line_ = debug_line_from_env();
        debug_trace_all_a_ = debug_flag_from_env(
            "MEMBLOCK_MEM_DIRECT_TRACE_ALL_A");
        const char *path = std::getenv("MEMBLOCK_MEM_DIRECT_TRACE_FILE");
        if (path != nullptr && path[0] != '\0') {
            const auto mode = debug_flag_from_env(
                "MEMBLOCK_MEM_DIRECT_TRACE_APPEND")
                ? std::ios::app : std::ios::trunc;
            debug_trace_.open(path, std::ios::out | mode);
        }
        if (!debug_trace_.is_open()) {
            return;
        }
        for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
            const auto load_prefix = std::string(
                "MemBlock_top.MemBlock.inner_LoadUnit_") +
                std::to_string(lane) + '.';
            debug_s2_valid_[lane] = debug_signal(dut_, load_prefix + "s2_valid");
            debug_s2_paddr_[lane] = debug_signal(
                dut_, load_prefix + "s2_in_r_paddr");
            debug_s2_vaddr_[lane] = debug_signal(
                dut_, load_prefix + "s2_in_r_vaddr");
            debug_s2_rob_value_[lane] = debug_signal(
                dut_, load_prefix + "s2_in_r_uop_robIdx_value");
            debug_s2_rob_flag_[lane] = debug_signal(
                dut_, load_prefix + "s2_in_r_uop_robIdx_flag");
            debug_s2_from_d_[lane] = debug_signal(
                dut_, load_prefix + "s2_fwd_frm_d_chan");
            debug_s2_miss_[lane] = debug_signal(
                dut_, load_prefix + "s2_dcache_miss");
            debug_s2_full_fwd_[lane] = debug_signal(
                dut_, load_prefix + "s2_full_fwd");
            for (unsigned byte = 0; byte < 16; ++byte) {
                debug_s2_d_data_[lane][byte] = debug_signal(
                    dut_, load_prefix + "s2_fwd_data_frm_d_chan_" +
                        std::to_string(byte));
            }
            debug_s3_valid_[lane] = debug_signal(
                dut_, load_prefix + "s3_out_valid");
            debug_s3_paddr_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_paddr");
            debug_s3_vaddr_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_vaddr");
            debug_s3_rob_value_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_uop_robIdx_value");
            debug_s3_rob_flag_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_uop_robIdx_flag");
            debug_s3_handled_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_handledByMSHR");
            debug_s3_mshrid_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_rep_info_mshr_id");
            debug_s3_data_select_[lane] = debug_signal(
                dut_, load_prefix + "s3_data_select");
            debug_s3_input_data_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_data");
            debug_s3_merged_data_[lane] = debug_signal(
                dut_, load_prefix + "s3_merged_data_frm_pipe");
            debug_s3_picked_data_[lane] = debug_signal(
                dut_, load_prefix + "s3_picked_data_frm_pipe_0");

            const auto mshr_prefix = std::string(
                "MemBlock_top.MemBlock.inner_dcache.dcache.missQueue.");
            const auto lane_suffix = lane == 0
                ? std::string{}
                : std::string("_") + std::to_string(lane);
            debug_mshr_forward_valid_[lane] = debug_signal(
                dut_, mshr_prefix + "io_forward_" + std::to_string(lane) +
                    "_forward_result_valid_REG");
            debug_mshr_forward_[lane] = debug_signal(
                dut_, mshr_prefix + "forward_mshr" + lane_suffix);
            for (unsigned byte = 0; byte < 16; ++byte) {
                const auto data_suffix = lane == 0
                    ? std::to_string(byte)
                    : std::to_string(lane) + '_' + std::to_string(byte);
                debug_mshr_forward_data_[lane][byte] = debug_signal(
                    dut_, mshr_prefix + "forwardData_" + data_suffix);
            }
        }
        debug_sms_req_valid_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_valid");
        debug_sms_buffer_state_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.bufferState");
        debug_sms_is_cross_page_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.isCrossPage");
        debug_sms_req_isvec_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_isvec");
        debug_sms_req_rob_value_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_uop_robIdx_value");
        debug_sms_req_sq_value_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_uop_sqIdx_value");
        debug_sms_req_sq_flag_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_uop_sqIdx_flag");
        debug_sms_cross_page_with_hit_ = debug_signal(
            dut_, "MemBlock_top.MemBlock._inner_storeMisalignBuffer_io_sqControl_toStoreQueue_crossPageWithHit");
        debug_sms_vec_wb_[0] = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.io_vecWriteBack_0_valid_0");
        debug_sms_vec_wb_[1] = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.io_vecWriteBack_1_valid_0");
        debug_sq_do_deq_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_lsq.storeQueue._io_maControl_toStoreMisalignBuffer_doDeq_T");
        debug_trace_ << "picker_mem_direct_debug line=0x" << std::hex
                     << debug_line_ << std::dec
                     << " trace_all_a=" << debug_trace_all_a_
                     << " sms_signals="
                     << (debug_sms_buffer_state_ != nullptr)
                     << (debug_sms_cross_page_with_hit_ != nullptr)
                     << (debug_sq_do_deq_ != nullptr) << '\n';
        debug_trace_.flush();
    }

    void sample_internal_debug()
    {
        if (!debug_trace_.is_open()) {
            return;
        }
        const auto emit = [&](const std::string &line) {
            debug_trace_ << "cycle=" << debug_cycle_ << ' ' << line << '\n';
            debug_trace_.flush();
        };
        const bool a_valid = dut_.auto_inner_dcache_client_out_a_valid.B();
        const bool a_fire = a_valid &&
            dut_.auto_inner_dcache_client_out_a_ready.B();
        if (a_fire) {
            const auto source = static_cast<unsigned>(
                dut_.auto_inner_dcache_client_out_a_bits_source.U());
            const auto address =
                dut_.auto_inner_dcache_client_out_a_bits_address.U();
            const auto line = address & ~std::uint64_t{63};
            const bool target = line == debug_line_;
            if (source < debug_target_sources_.size()) {
                debug_target_sources_[source] = target;
                const auto transfer_bytes = std::uint64_t{1} <<
                    dut_.auto_inner_dcache_client_out_a_bits_size.U();
                debug_target_beats_remaining_[source] = target
                    ? static_cast<unsigned>(std::max<std::uint64_t>(
                          1, transfer_bytes / 32))
                    : 0;
            }
            if (debug_trace_all_a_ || target) {
                std::ostringstream text;
                text << "A fire=1 opcode=0x" << std::hex
                     << dut_.auto_inner_dcache_client_out_a_bits_opcode.U()
                     << " source=" << source << " address=0x" << address
                     << " line=0x" << line << std::dec;
                emit(text.str());
            }
        }
        const bool d_valid = dut_.auto_inner_dcache_client_out_d_valid.B();
        const bool d_fire = d_valid &&
            dut_.auto_inner_dcache_client_out_d_ready.B();
        if (d_fire) {
            const auto source = static_cast<unsigned>(
                dut_.auto_inner_dcache_client_out_d_bits_source.U());
            const bool target = source < debug_target_sources_.size() &&
                debug_target_sources_[source];
            if (target) {
                std::ostringstream text;
                text << "D fire=1 opcode=0x" << std::hex
                     << dut_.auto_inner_dcache_client_out_d_bits_opcode.U()
                     << " source=" << source << " data="
                     << debug_bytes(&dut_.auto_inner_dcache_client_out_d_bits_data)
                     << " denied=" << std::dec
                     << dut_.auto_inner_dcache_client_out_d_bits_denied.B()
                     << " corrupt="
                     << dut_.auto_inner_dcache_client_out_d_bits_corrupt.B();
                emit(text.str());
                debug_internal_window_ = 4;
            }
            if (source < debug_target_sources_.size() &&
                debug_target_beats_remaining_[source] != 0 &&
                --debug_target_beats_remaining_[source] == 0) {
                debug_target_sources_[source] = false;
            }
        }
        if (debug_internal_window_ != 0) {
            --debug_internal_window_;
            for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
                if (debug_u(debug_mshr_forward_valid_[lane]) != 0) {
                    std::ostringstream text;
                    text << "mshr_forward lane=" << lane << " selected="
                         << debug_u(debug_mshr_forward_[lane]) << " data="
                         << debug_byte_array(debug_mshr_forward_data_[lane]);
                    emit(text.str());
                }
            }
        }
        for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
            const auto s2_paddr = debug_u(debug_s2_paddr_[lane]);
            if (debug_u(debug_s2_valid_[lane]) != 0 &&
                (s2_paddr & ~std::uint64_t{63}) == debug_line_) {
                std::ostringstream text;
                text << "load_s2 lane=" << lane << " rob="
                     << debug_u(debug_s2_rob_value_[lane]) << ':'
                     << debug_u(debug_s2_rob_flag_[lane]) << " vaddr=0x"
                     << std::hex << debug_u(debug_s2_vaddr_[lane])
                     << " paddr=0x" << s2_paddr << std::dec
                     << " from_d=" << debug_u(debug_s2_from_d_[lane])
                     << " miss=" << debug_u(debug_s2_miss_[lane])
                     << " full_fwd=" << debug_u(debug_s2_full_fwd_[lane])
                     << " d_data="
                     << debug_byte_array(debug_s2_d_data_[lane]);
                emit(text.str());
            }
            const auto s3_paddr = debug_u(debug_s3_paddr_[lane]);
            if (debug_u(debug_s3_valid_[lane]) != 0 &&
                (s3_paddr & ~std::uint64_t{63}) == debug_line_) {
                std::ostringstream text;
                text << "load_s3 lane=" << lane << " rob="
                     << debug_u(debug_s3_rob_value_[lane]) << ':'
                     << debug_u(debug_s3_rob_flag_[lane]) << " vaddr=0x"
                     << std::hex << debug_u(debug_s3_vaddr_[lane])
                     << " paddr=0x" << s3_paddr << " input="
                     << debug_bytes(debug_s3_input_data_[lane])
                     << " merged="
                     << debug_bytes(debug_s3_merged_data_[lane])
                     << " picked=0x" << debug_u(debug_s3_picked_data_[lane])
                     << std::dec << " select="
                     << debug_u(debug_s3_data_select_[lane])
                     << " handled=" << debug_u(debug_s3_handled_[lane])
                     << " mshr=" << debug_u(debug_s3_mshrid_[lane]);
                emit(text.str());
            }
        }
        const auto sms_state = debug_u(debug_sms_buffer_state_);
        const bool sms_active = debug_u(debug_sms_req_valid_) != 0;
        const bool sms_observable = sms_active ||
            debug_u(debug_sms_cross_page_with_hit_) != 0 ||
            debug_u(debug_sq_do_deq_) != 0 ||
            debug_u(debug_sms_vec_wb_[0]) != 0 ||
            debug_u(debug_sms_vec_wb_[1]) != 0 ||
            dut_.io_ooo_to_mem_lsqio_scommit.U() != 0;
        if (sms_observable) {
            std::ostringstream text;
            text << "sms state=" << sms_state
                 << " req=" << debug_u(debug_sms_req_valid_)
                 << " vec=" << debug_u(debug_sms_req_isvec_)
                 << " cross=" << debug_u(debug_sms_is_cross_page_)
                 << " rob=" << debug_u(debug_sms_req_rob_value_)
                 << " sq=" << debug_u(debug_sms_req_sq_value_) << ':'
                 << debug_u(debug_sms_req_sq_flag_)
                 << " with_hit=" << debug_u(debug_sms_cross_page_with_hit_)
                 << " do_deq=" << debug_u(debug_sq_do_deq_)
                 << " vec_wb=" << debug_u(debug_sms_vec_wb_[0]) << ','
                 << debug_u(debug_sms_vec_wb_[1])
                 << " top_vec_wb="
                 << dut_.io_mem_to_ooo_writebackVldu_0_valid.U() << ','
                 << dut_.io_mem_to_ooo_writebackVldu_1_valid.U()
                 << " slow_fb="
                 << dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid.U()
                 << ','
                 << dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid.U()
                 << " pendingst=" << dut_.io_ooo_to_mem_lsqio_pendingst.U()
                 << " scommit=" << dut_.io_ooo_to_mem_lsqio_scommit.U();
            emit(text.str());
        }
        ++debug_cycle_;
    }

    void trace_scalar_writeback_debug(
        unsigned lane, const generated::ScalarLoadWriteback &writeback)
    {
        if (!debug_trace_.is_open() || !writeback.valid) {
            return;
        }
        const auto *expected = scoreboard_.expected_for(writeback);
        if (expected == nullptr ||
            (expected->address & ~std::uint64_t{63}) != debug_line_) {
            return;
        }
        debug_trace_ << "cycle=" << debug_cycle_ << " writeback lane=" << lane
                     << " rob=" << static_cast<unsigned>(writeback.rob_value)
                     << ':' << writeback.rob_flag << " address=0x" << std::hex
                     << expected->address << " actual=0x" << writeback.data
                     << " expected=0x" << expected->data << std::dec << '\n';
        debug_trace_.flush();
    }

#endif
    bool wait_for_enqueue_capacity(
        unsigned lq_needed, unsigned sq_needed, unsigned timeout = 256)
    {
        if (lq_needed > kVirtualLoadQueueEntries - kLqEnqueueHeadroom ||
            sq_needed > kStoreQueueEntries - kSqEnqueueHeadroom) {
            error_ = "LSQ enqueue request exceeds reserved queue capacity";
            return false;
        }
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            const std::uint64_t lq_retired = lq_dequeued_ + lq_canceled_;
            const std::uint64_t sq_retired = sq_dequeued_ + sq_canceled_;
            if (lq_retired > lq_allocated_ || sq_retired > sq_allocated_) {
                error_ = "LSQ software accounting moved past allocation";
                return false;
            }
            const std::uint64_t lq_outstanding = lq_allocated_ - lq_retired;
            const std::uint64_t sq_outstanding = sq_allocated_ - sq_retired;
            const bool capacity_ok =
                lq_outstanding + lq_needed <=
                    kVirtualLoadQueueEntries - kLqEnqueueHeadroom &&
                sq_outstanding + sq_needed <=
                    kStoreQueueEntries - kSqEnqueueHeadroom;
            if (capacity_ok) {
                // canAccept and the queue counters are registered in
                // LsqEnqCtrl. A short quiet interval avoids sampling the
                // pre-retirement value after a commit/redirect transition.
                for (unsigned settle = 0;
                     settle < kEnqueueSettleCycles; ++settle) {
                    tick();
                    if (!check_components()) {
                        return false;
                    }
                }
                return true;
            }
            tick();
            if (!check_components()) {
                return false;
            }
        }
        std::ostringstream message;
        message << "timed out waiting for LSQ enqueue capacity"
                << " lq_needed=" << lq_needed
                << " sq_needed=" << sq_needed
                << " lq=" << lq_allocated_ - (lq_dequeued_ + lq_canceled_)
                << '/' << kVirtualLoadQueueEntries
                << " sq=" << sq_allocated_ - (sq_dequeued_ + sq_canceled_)
                << '/' << kStoreQueueEntries;
        error_ = message.str();
        return false;
    }

    bool write_distributed_csr(std::uint16_t address, std::uint64_t data)
    {
        dut_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr.ImmSet(address);
        dut_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data.ImmSet(data);
        dut_.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid.ImmSet(std::uint64_t{0});
        return check_components();
    }

    void tick(bool monitor = true)
    {
        memory_agent_.drive(dut_);
        ptw_agent_.drive(dut_);
        uncache_agent_.drive(dut_);
        dut_.RefreshComb();
        memory_agent_.capture_before_tick(dut_);
        ptw_agent_.capture_before_tick(dut_);
        uncache_agent_.capture_before_tick(dut_);
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
        sample_internal_debug();
#endif

        const bool top_down_l2_input =
            dut_.io_topDownInfo_fromL2Top_l2Miss.B();
        const bool top_down_l3_input =
            dut_.io_topDownInfo_fromL2Top_l3Miss.B();
        const bool l2_flush_enable_input =
            dut_.io_ooo_to_mem_csrCtrl_flush_l2_enable.B();
        const bool l2_flush_done_input = dut_.io_l2_flush_done.B();
        const std::uint8_t hart_id_input =
            static_cast<std::uint8_t>(dut_.io_hartId.U());
        const std::uint64_t reset_vector_input =
            dut_.io_outer_reset_vector.U();
        const bool power_down_input =
            dut_.io_ooo_to_mem_csrCtrl_power_down_enable.B();
        const bool cpu_halted_input =
            dut_.io_ooo_to_mem_backendToTopBypass_cpuHalted.B();
        const bool cpu_critical_error_input =
            dut_.io_ooo_to_mem_backendToTopBypass_cpuCriticalError.B();
        const bool msi_ack_input =
            dut_.io_ooo_to_mem_backendToTopBypass_msiAck.B();
        const bool frontend_reset_input =
            dut_.io_resetInFrontendBypass_fromFrontend.B();
        const bool beu_valid_input =
            dut_.io_inner_beu_errors_icache_ecc_error_valid.B();
        const std::uint64_t beu_address_input =
            dut_.io_inner_beu_errors_icache_ecc_error_bits.U();
        const bool msi_info_valid_input =
            dut_.io_fromTopToBackend_msiInfo_valid.B();
        const std::uint16_t msi_info_input = static_cast<std::uint16_t>(
            dut_.io_fromTopToBackend_msiInfo_bits.U());
        const bool clint_time_valid_input =
            dut_.io_fromTopToBackend_clintTime_valid.B();
        const std::uint64_t clint_time_input =
            dut_.io_fromTopToBackend_clintTime_bits.U();
        const ExternalInterruptState external_interrupt_input{
            .msip = dut_.auto_inner_clint_int_sink_in_0.B(),
            .mtip = dut_.auto_inner_clint_int_sink_in_1.B(),
            .meip = dut_.auto_inner_plic_int_sink_in_0_0.B(),
            .seip = dut_.auto_inner_plic_int_sink_in_1_0.B(),
            .debug = dut_.auto_inner_debug_int_sink_in_0.B(),
            .nmi_31 = dut_.auto_inner_nmi_int_sink_in_0.B() ||
                      dut_.auto_inner_beu_local_int_sink_in_0.B(),
            .nmi_43 = dut_.auto_inner_nmi_int_sink_in_1.B(),
        };
        std::array<
            std::uint8_t, generated::kHcPerfEventHighestInputLane + 1>
            hc_perf_event_inputs{};
        for (unsigned lane = generated::kHcPerfEventFirstInputLane;
             lane <= generated::kHcPerfEventHighestInputLane; ++lane) {
            hc_perf_event_inputs[lane] =
                generated::sample_hc_perf_event_input(dut_, lane);
        }
        const L2PrefetchControl l2_prefetch_input{
            .master_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable.B(),
            .receive_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable.B(),
            .pbop_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable.B(),
            .vbop_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable.B(),
            .tp_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable.B(),
            .delay_latency = static_cast<std::uint16_t>(
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency.U()),
        };
        const bool trace_encoder_enable_input =
            dut_.io_traceCoreInterfaceBypass_toL2Top_fromEncoder_enable.B();
        const bool trace_encoder_stall_input =
            dut_.io_traceCoreInterfaceBypass_toL2Top_fromEncoder_stall.B();
        const std::uint8_t trace_privilege_input = static_cast<std::uint8_t>(
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_priv.U());
        const std::uint64_t trace_mstatus_input =
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_mstatus.U();
        const std::uint64_t trace_trap_cause_input =
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_cause.U();
        const std::uint64_t trace_trap_tval_input =
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_tval.U();
        const auto sample_trace_group = [](auto &valid, auto &iaddr,
                                           auto &ftq_offset, auto &itype,
                                           auto &iretire, auto &ilastsize) {
            return TraceGroupState{
                .valid = valid.B(),
                .iaddr = iaddr.U(),
                .ftq_offset = static_cast<std::uint8_t>(ftq_offset.U()),
                .itype = static_cast<std::uint8_t>(itype.U()),
                .iretire = static_cast<std::uint8_t>(iretire.U()),
                .ilastsize = ilastsize.B(),
            };
        };
        const std::array<TraceGroupState, kTraceGroups> trace_group_inputs{{
            sample_trace_group(
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_valid,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iaddr,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ftqOffset,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_itype,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iretire,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ilastsize),
            sample_trace_group(
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_valid,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iaddr,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ftqOffset,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_itype,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iretire,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ilastsize),
            sample_trace_group(
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_valid,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iaddr,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ftqOffset,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_itype,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iretire,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ilastsize),
        }};

        // Writeback valid is a combinational projection of the execution-unit
        // output fire.  Observe the pins before the clock edge; after Step()
        // they may already describe the following transaction.  LSQ dequeue
        // pulses are registered separately and are counted below instead.
        if (monitor) {
            unsigned lsq_enqueue_width = 0;
            for (unsigned lane = 0; lane < generated::kLsqEnqueueLanes;
                 ++lane) {
                const auto enqueue =
                    generated::sample_lsq_enqueue(dut_, lane);
                if (!enqueue.valid) {
                    continue;
                }
                ++lsq_enqueue_width;
                ++lsq_enqueue_lanes_observed_[lane];
                if ((enqueue.need_alloc != 1 && enqueue.need_alloc != 2) ||
                    enqueue.num_ls_elem == 0) {
                    std::ostringstream message;
                    message << "invalid accepted LSQ enqueue on lane " << lane
                            << " need_alloc="
                            << static_cast<unsigned>(enqueue.need_alloc)
                            << " num_ls_elem="
                            << static_cast<unsigned>(enqueue.num_ls_elem);
                    error_ = message.str();
                    continue;
                }
                if (enqueue.need_alloc == 1) {
                    lq_enqueued_observed_ += enqueue.num_ls_elem;
                } else {
                    sq_enqueued_observed_ += enqueue.num_ls_elem;
                }
            }
            if (lsq_enqueue_width != 0) {
                ++lsq_enqueue_widths_observed_.at(lsq_enqueue_width - 1);
            }
            ++l2_flush_checks_;
            if (dut_.io_outer_l2_flush_en.B() != l2_flush_enable_input) {
                error_ = "L2 flush enable did not pass through combinationally";
            }
            if (dut_.io_mem_to_ooo_topToBackendBypass_l2FlushDone.B() !=
                expected_l2_flush_done_) {
                error_ = "L2 flush completion violated one-cycle delay";
            }
            ++top_control_checks_;
            if (dut_.io_mem_to_ooo_topToBackendBypass_hartId.U() !=
                hart_id_input) {
                error_ = "backend hart ID did not pass through combinationally";
            }
            if (dut_.io_outer_power_down_en.B() != power_down_input) {
                error_ = "power-down enable did not pass through combinationally";
            }
            if (dut_.io_inner_reset_vector.U() != expected_reset_vector_) {
                error_ = "reset vector violated one-cycle delay";
            }
            if (dut_.io_outer_cpu_halt.B() != expected_cpu_halted_) {
                error_ = "CPU halt violated one-cycle delay";
            }
            if (dut_.io_outer_cpu_critical_error.B() !=
                expected_cpu_critical_error_) {
                error_ = "CPU critical error violated one-cycle delay";
            }
            ++top_bridge_checks_;
            if (dut_.io_outer_msi_ack.B() != msi_ack_input) {
                error_ = "MSI acknowledgement did not pass through combinationally";
            }
            if (dut_.io_resetInFrontendBypass_toL2Top.B() !=
                frontend_reset_input) {
                error_ = "frontend reset bypass did not pass through combinationally";
            }
            if (dut_.io_outer_beu_errors_icache_ecc_error_valid.B() !=
                    expected_beu_valid_ ||
                dut_.io_outer_beu_errors_icache_ecc_error_bits.U() !=
                    expected_beu_address_) {
                error_ = "I-cache BEU metadata violated one-cycle delay";
            }
            if (dut_.io_mem_to_ooo_topToBackendBypass_msiInfo_valid.B() !=
                    expected_msi_info_valid_ ||
                (expected_msi_info_valid_ &&
                 dut_.io_mem_to_ooo_topToBackendBypass_msiInfo_bits.U() !=
                     expected_msi_info_)) {
                error_ = "MSI information violated valid-gated one-cycle delay";
            }
            if (dut_.io_mem_to_ooo_topToBackendBypass_clintTime_valid.B() !=
                    expected_clint_time_valid_ ||
                (expected_clint_time_valid_ &&
                 dut_.io_mem_to_ooo_topToBackendBypass_clintTime_bits.U() !=
                     expected_clint_time_)) {
                error_ = "CLINT time violated valid-gated one-cycle delay";
            }
            const ExternalInterruptState external_interrupt_output{
                .msip = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip.B(),
                .mtip = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip.B(),
                .meip = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip.B(),
                .seip = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip.B(),
                .debug = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug.B(),
                .nmi_31 = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31.B(),
                .nmi_43 = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43.B(),
            };
            if (external_interrupt_output != expected_external_interrupt_) {
                error_ = "external interrupts violated one-cycle mapping";
            }
            if (generated::sample_hc_perf_event_output(
                    dut_, generated::kHcPerfEventFirstOutputLane) != 0) {
                error_ = "constant hardware-counter event lane was nonzero";
            }
            for (unsigned lane = generated::kHcPerfEventFirstSharedLane;
                 lane <= generated::kHcPerfEventLastSharedLane; ++lane) {
                if (generated::sample_hc_perf_event_output(dut_, lane) !=
                    expected_hc_perf_events_[lane]) {
                    error_ = "hardware-counter perf event violated one-cycle delay";
                }
            }
            const L2PrefetchControl l2_prefetch_output{
                .master_enable =
                    dut_.io_outer_l2PfCtrl_l2_pf_master_en.B(),
                .receive_enable =
                    dut_.io_outer_l2PfCtrl_l2_pf_recv_en.B(),
                .pbop_enable = dut_.io_outer_l2PfCtrl_l2_pbop_en.B(),
                .vbop_enable = dut_.io_outer_l2PfCtrl_l2_vbop_en.B(),
                .tp_enable = dut_.io_outer_l2PfCtrl_l2_tp_en.B(),
                .delay_latency = static_cast<std::uint16_t>(
                    dut_.io_outer_l2PfCtrl_l2_pf_delay_latency.U()),
            };
            if (l2_prefetch_output != expected_l2_prefetch_output_) {
                error_ = "L2 prefetch control violated two-cycle delay";
            }
            ++trace_bridge_checks_;
            if (dut_.io_traceCoreInterfaceBypass_fromBackend_fromEncoder_enable.B() !=
                    expected_trace_encoder_enable_ ||
                dut_.io_traceCoreInterfaceBypass_fromBackend_fromEncoder_stall.B() !=
                    expected_trace_encoder_stall_) {
                error_ = "trace encoder feedback violated one-cycle delay";
            }
            if (dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_mstatus.U() !=
                expected_trace_mstatus_) {
                error_ = "trace mstatus violated one-cycle delay";
            }
            if (trace_privilege_initialized_ &&
                dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_priv.U() !=
                    expected_trace_privilege_) {
                error_ = "trace privilege violated group-0 valid hold contract";
            }
            if (trace_trap_initialized_ &&
                (dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_trap_cause.U() !=
                     expected_trace_trap_cause_ ||
                 dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_trap_tval.U() !=
                     expected_trace_trap_tval_)) {
                error_ = "trace trap metadata violated trap-only hold contract";
            }
            const auto sample_trace_output = [](auto &valid, auto &iaddr,
                                                auto &itype, auto &iretire,
                                                auto &ilastsize) {
                return TraceGroupState{
                    .valid = valid.B(),
                    .iaddr = iaddr.U(),
                    .itype = static_cast<std::uint8_t>(itype.U()),
                    .iretire = static_cast<std::uint8_t>(iretire.U()),
                    .ilastsize = ilastsize.B(),
                };
            };
            const std::array<TraceGroupState, kTraceGroups> trace_group_outputs{{
                sample_trace_output(
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_valid,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iaddr,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_itype,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iretire,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_ilastsize),
                sample_trace_output(
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_valid,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iaddr,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_itype,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iretire,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_ilastsize),
                sample_trace_output(
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_valid,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iaddr,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_itype,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iretire,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_ilastsize),
            }};
            for (unsigned group = 0; group < kTraceGroups; ++group) {
                const auto &actual = trace_group_outputs[group];
                const auto &expected = expected_trace_groups_[group];
                if (actual.valid != expected.valid ||
                    actual.itype != expected.itype ||
                    actual.iretire != expected.iretire) {
                    error_ = "trace group unconditional fields violated one-cycle delay";
                }
                if (trace_group_payload_initialized_[group] &&
                    (actual.iaddr != expected.iaddr ||
                     actual.ilastsize != expected.ilastsize)) {
                    error_ = "trace group payload violated valid hold contract";
                }
            }
            const bool top_down_l2_output =
                dut_.io_topDownInfo_toBackend_l2TopMiss_l2Miss.B();
            const bool top_down_l3_output =
                dut_.io_topDownInfo_toBackend_l2TopMiss_l3Miss.B();
            ++top_down_stats_.delay_checks;
            if (top_down_l2_output != expected_top_down_l2_miss_ ||
                top_down_l3_output != expected_top_down_l3_miss_) {
                std::ostringstream message;
                message << "top-down L2/L3 miss output violated one-cycle delay"
                        << " expected=" << expected_top_down_l2_miss_ << ','
                        << expected_top_down_l3_miss_ << " observed="
                        << top_down_l2_output << ',' << top_down_l3_output;
                error_ = message.str();
            }
            top_down_stats_.replay_allocate_cycles +=
                dut_.io_topDownInfo_toBackend_replayAllocate.B();
            top_down_stats_.sq_full_cycles +=
                dut_.io_topDownInfo_toBackend_sqFull.B();
            top_down_stats_.sb_full_cycles +=
                dut_.io_topDownInfo_toBackend_sbFull.B();
            top_down_stats_.l1_miss_cycles +=
                dut_.io_topDownInfo_toBackend_l1Miss.B();
            top_down_stats_.l2_miss_cycles += top_down_l2_output;
            top_down_stats_.l3_miss_cycles += top_down_l3_output;
            if (dut_.io_dcacheError_ecc_error_valid.B()) {
                ++bus_error_stats_.dcache_reports;
                bus_error_stats_.last_dcache_address =
                    dut_.io_dcacheError_ecc_error_bits.U();
            }
            if (dut_.io_uncacheError_ecc_error_valid.B()) {
                ++bus_error_stats_.uncache_reports;
                bus_error_stats_.last_uncache_address =
                    dut_.io_uncacheError_ecc_error_bits.U();
            }
            const auto memory_violation =
                generated::sample_memory_violation(dut_);
            if (memory_violation.valid) {
                ++memory_violation_stats_.count;
                memory_violation_stats_.last = memory_violation;
                memory_violation_stats_.last_cycle = cycle();
            }
            for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
                bool load_mmio = false;
                std::uint8_t load_mmio_rob = 0;
                switch (lane) {
                case 0:
                    load_mmio = dut_.io_mem_to_ooo_lsqio_loadMmio_0.B();
                    load_mmio_rob = static_cast<std::uint8_t>(
                        dut_.io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value.U());
                    break;
                case 1:
                    load_mmio = dut_.io_mem_to_ooo_lsqio_loadMmio_1.B();
                    load_mmio_rob = static_cast<std::uint8_t>(
                        dut_.io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value.U());
                    break;
                case 2:
                    load_mmio = dut_.io_mem_to_ooo_lsqio_loadMmio_2.B();
                    load_mmio_rob = static_cast<std::uint8_t>(
                        dut_.io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value.U());
                    break;
                default:
                    break;
                }
                if (load_mmio) {
                    ++load_mmio_stats_.pulses[lane];
                    load_mmio_stats_.last_rob[lane] = load_mmio_rob;
                    load_mmio_stats_.last_cycle[lane] = cycle();
                }
                const auto ifetch_prefetch =
                    generated::sample_ifetch_prefetch(dut_, lane);
                if (ifetch_prefetch.valid) {
                    ++ifetch_prefetch_stats_.requests[lane];
                    ifetch_prefetch_stats_.last_vaddr[lane] =
                        ifetch_prefetch.vaddr;
                    ifetch_prefetch_stats_.last_cycle[lane] = cycle();
                }
                const auto wakeup =
                    generated::sample_scalar_load_wakeup(dut_, lane);
                if (wakeup.valid) {
                    ++scalar_load_feedback_stats_.wakeups[lane];
                    scalar_load_feedback_stats_.last_wakeup[lane] = {
                        .valid = true,
                        .rf_wen = wakeup.rf_wen,
                        .fp_wen = wakeup.fp_wen,
                        .pdest = wakeup.pdest,
                        .cycle = cycle(),
                    };
                }
                if (generated::sample_scalar_load_cancel(dut_, lane)) {
                    ++scalar_load_feedback_stats_.ld2_cancels[lane];
                    scalar_load_feedback_stats_.last_cancel_cycle[lane] = cycle();
                }
                const auto writeback =
                    generated::sample_scalar_load_writeback(dut_, lane);
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
                trace_scalar_writeback_debug(lane, writeback);
#endif
                scoreboard_.observe(lane, writeback);
            }
            const auto hardware_prefetch =
                generated::sample_hardware_prefetch_outputs(dut_);
            if (hardware_prefetch.l2_valid) {
                ++hardware_prefetch_stats_.l2_requests;
                ++hardware_prefetch_stats_.l2_source_counts[
                    hardware_prefetch.l2_source];
                hardware_prefetch_stats_.l2_addresses_by_source[
                    hardware_prefetch.l2_source].push_back(
                        hardware_prefetch.l2_addr);
                hardware_prefetch_stats_.last_l2_addr_by_source[
                    hardware_prefetch.l2_source] = hardware_prefetch.l2_addr;
                hardware_prefetch_stats_.last_l2_cycle_by_source[
                    hardware_prefetch.l2_source] = cycle();
                hardware_prefetch_stats_.last_l2_addr =
                    hardware_prefetch.l2_addr;
                hardware_prefetch_stats_.last_l2_source =
                    hardware_prefetch.l2_source;
                hardware_prefetch_stats_.last_l2_cycle = cycle();
            }
            if (hardware_prefetch.l3_valid) {
                ++hardware_prefetch_stats_.l3_requests;
                hardware_prefetch_stats_.last_l3_addr =
                    hardware_prefetch.l3_addr;
                hardware_prefetch_stats_.last_l3_cycle = cycle();
            }
            for (unsigned lane = 0; lane < kScalarStoreLanes; ++lane) {
                const auto address_writeback =
                    generated::sample_scalar_store_address_writeback(dut_, lane);
                const auto data_writeback =
                    generated::sample_scalar_store_data_writeback(dut_, lane);
                store_scoreboard_.observe_address(
                    lane, address_writeback, cycle());
                store_scoreboard_.observe_data(lane, data_writeback, cycle());
            }
            for (unsigned lane = 0; lane < kVectorMemoryLanes; ++lane) {
                vector_scoreboard_.observe(
                    lane, generated::sample_vector_memory_writeback(dut_, lane));
            }
        }

        dut_.Step();
        expected_top_down_l2_miss_ = top_down_l2_input;
        expected_top_down_l3_miss_ = top_down_l3_input;
        expected_l2_flush_done_ = l2_flush_done_input;
        expected_reset_vector_ = reset_vector_input;
        expected_cpu_halted_ = cpu_halted_input;
        expected_cpu_critical_error_ = cpu_critical_error_input;
        expected_beu_valid_ = beu_valid_input;
        expected_beu_address_ = beu_address_input;
        expected_msi_info_valid_ = msi_info_valid_input;
        if (msi_info_valid_input) {
            expected_msi_info_ = msi_info_input;
        }
        expected_clint_time_valid_ = clint_time_valid_input;
        if (clint_time_valid_input) {
            expected_clint_time_ = clint_time_input;
        }
        expected_external_interrupt_ = external_interrupt_input;
        expected_hc_perf_events_ = hc_perf_event_inputs;
        expected_l2_prefetch_output_ = l2_prefetch_delay_stage_;
        l2_prefetch_delay_stage_ = l2_prefetch_input;
        expected_trace_encoder_enable_ = trace_encoder_enable_input;
        expected_trace_encoder_stall_ = trace_encoder_stall_input;
        expected_trace_mstatus_ = trace_mstatus_input;
        for (unsigned group = 0; group < kTraceGroups; ++group) {
            const auto &input = trace_group_inputs[group];
            auto &expected = expected_trace_groups_[group];
            expected.valid = input.valid;
            expected.itype = input.itype;
            expected.iretire = input.iretire;
            if (input.valid) {
                expected.iaddr =
                    (input.iaddr + (std::uint64_t{input.ftq_offset} << 1)) &
                    ((std::uint64_t{1} << 50) - 1U);
                expected.ilastsize = input.ilastsize;
                trace_group_payload_initialized_[group] = true;
            }
        }
        if (trace_group_inputs[0].valid) {
            expected_trace_privilege_ = trace_privilege_input;
            trace_privilege_initialized_ = true;
            if (trace_group_inputs[0].itype == 1 ||
                trace_group_inputs[0].itype == 2) {
                expected_trace_trap_cause_ = trace_trap_cause_input;
                expected_trace_trap_tval_ = trace_trap_tval_input;
                trace_trap_initialized_ = true;
            }
        }
        memory_agent_.update_after_tick();
        ptw_agent_.update_after_tick();
        uncache_agent_.update_after_tick();
        lq_dequeued_ += dut_.io_mem_to_ooo_lqDeq.U();
        sq_dequeued_ += dut_.io_mem_to_ooo_sqDeq.U();
        if (dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid.B()) {
            const bool hit =
                dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit.B();
            ++store_tlb_feedbacks_;
            store_tlb_misses_ += hit ? 0 : 1;
            ++iq_slow_feedback_stats_.sta_valid[0];
            ++(hit ? iq_slow_feedback_stats_.sta_hits[0]
                   : iq_slow_feedback_stats_.sta_misses[0]);
            iq_slow_feedback_stats_.sta_samples.push_back(
                StoreSlowFeedbackSample{
                    .lane = 0,
                    .hit = hit,
                    .sq_flag = dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag.B(),
                    .sq_value = static_cast<std::uint8_t>(
                        dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value.U()),
                    .cycle = cycle(),
                });
        }
        if (dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid.B()) {
            const bool hit =
                dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit.B();
            ++store_tlb_feedbacks_;
            store_tlb_misses_ += hit ? 0 : 1;
            ++iq_slow_feedback_stats_.sta_valid[1];
            ++(hit ? iq_slow_feedback_stats_.sta_hits[1]
                   : iq_slow_feedback_stats_.sta_misses[1]);
            iq_slow_feedback_stats_.sta_samples.push_back(
                StoreSlowFeedbackSample{
                    .lane = 1,
                    .hit = hit,
                    .sq_flag = dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag.B(),
                    .sq_value = static_cast<std::uint8_t>(
                        dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value.U()),
                    .cycle = cycle(),
                });
        }
        if (dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid.B()) {
            const VectorStoreSlowFeedbackSample sample{
                .lane = 0,
                .hit = dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit.B(),
                .lq_flag = dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag.B(),
                .lq_value = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value.U()),
                .sq_flag = dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag.B(),
                .sq_value = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value.U()),
                .is_part_replay = dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay.B(),
                .replay_mask = static_cast<std::uint16_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask.U()),
                .replay_mb_index = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx.U()),
                .cycle = cycle(),
            };
            ++iq_slow_feedback_stats_.vstu_valid[0];
            ++(sample.hit ? iq_slow_feedback_stats_.vstu_hits[0]
                          : iq_slow_feedback_stats_.vstu_misses[0]);
            iq_slow_feedback_stats_.vstu_samples.push_back(sample);
            if (!sample.hit) {
                vector_replay_requests_.push_back(VectorReplayRequest{
                    .lane = sample.lane,
                    .lq_flag = sample.lq_flag,
                    .lq_value = sample.lq_value,
                    .sq_flag = sample.sq_flag,
                    .sq_value = sample.sq_value,
                    .is_part_replay = sample.is_part_replay,
                    .replay_mask = sample.replay_mask,
                    .replay_mb_index = sample.replay_mb_index,
                });
                ++vector_replay_feedbacks_;
            }
        }
        if (dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid.B()) {
            const VectorStoreSlowFeedbackSample sample{
                .lane = 1,
                .hit = dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit.B(),
                .lq_flag = dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag.B(),
                .lq_value = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value.U()),
                .sq_flag = dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag.B(),
                .sq_value = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value.U()),
                .is_part_replay = dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay.B(),
                .replay_mask = static_cast<std::uint16_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask.U()),
                .replay_mb_index = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx.U()),
                .cycle = cycle(),
            };
            ++iq_slow_feedback_stats_.vstu_valid[1];
            ++(sample.hit ? iq_slow_feedback_stats_.vstu_hits[1]
                          : iq_slow_feedback_stats_.vstu_misses[1]);
            iq_slow_feedback_stats_.vstu_samples.push_back(sample);
            if (!sample.hit) {
                vector_replay_requests_.push_back(VectorReplayRequest{
                    .lane = sample.lane,
                    .lq_flag = sample.lq_flag,
                    .lq_value = sample.lq_value,
                    .sq_flag = sample.sq_flag,
                    .sq_value = sample.sq_value,
                    .is_part_replay = sample.is_part_replay,
                    .replay_mask = sample.replay_mask,
                    .replay_mb_index = sample.replay_mb_index,
                });
                ++vector_replay_feedbacks_;
            }
        }
    }

    bool check_components()
    {
        if (!memory_agent_.ok()) {
            error_ = memory_agent_.error();
        } else if (!ptw_agent_.ok()) {
            error_ = ptw_agent_.error();
        } else if (!uncache_agent_.ok()) {
            error_ = uncache_agent_.error();
        } else if (!scoreboard_.ok()) {
            error_ = scoreboard_.error();
        } else if (!store_scoreboard_.ok()) {
            error_ = store_scoreboard_.error();
        } else if (!vector_scoreboard_.ok()) {
            error_ = vector_scoreboard_.error();
        } else if (lq_enqueued_observed_ != lq_allocated_ ||
                   sq_enqueued_observed_ != sq_allocated_) {
            std::ostringstream message;
            message << "LSQ enqueue monitor disagrees with driver accounting"
                    << " lq_observed=" << lq_enqueued_observed_
                    << " lq_expected=" << lq_allocated_
                    << " sq_observed=" << sq_enqueued_observed_
                    << " sq_expected=" << sq_allocated_;
            error_ = message.str();
        }
        return error_.empty();
    }

    static std::uint64_t scalar_store_key(const StoreTransaction &transaction)
    {
        return transaction.rob |
            (std::uint64_t{transaction.rob_flag} << 8) |
            (std::uint64_t{transaction.sq} << 9) |
            (std::uint64_t{transaction.sq_flag} << 17);
    }

    static std::uint64_t vector_store_key(
        const VectorMemoryTransaction &transaction)
    {
        return transaction.rob |
            (std::uint64_t{transaction.rob_flag} << 8) |
            (std::uint64_t{transaction.sq} << 9) |
            (std::uint64_t{transaction.sq_flag} << 17);
    }

    UTMemBlock dut_;
    SparseMemory bus_memory_;
    SparseMemory memory_;
    TileLinkMemoryAgent memory_agent_;
    PtwMemoryAgent ptw_agent_;
    UncacheMemoryAgent uncache_agent_;
    LoadScoreboard scoreboard_;
    StoreScoreboard store_scoreboard_;
    VectorMemoryScoreboard vector_scoreboard_;
    std::uint64_t pin_space_digest_ = 0;
    FrontendBridgeStats frontend_bridge_stats_;
    std::uint64_t frontend_reset_canceled_requests_ = 0;
    std::uint64_t frontend_reset_canceled_responses_ = 0;
    std::uint64_t frontend_reset_survivor_requests_ = 0;
    std::uint64_t frontend_reset_stall_checks_ = 0;
    ScalarLoadFeedbackStats scalar_load_feedback_stats_;
    LoadMmioStats load_mmio_stats_;
    IqSlowFeedbackStats iq_slow_feedback_stats_;
    MemoryViolationStats memory_violation_stats_;
    IfetchPrefetchStats ifetch_prefetch_stats_;
    HardwarePrefetchStats hardware_prefetch_stats_;
    BusErrorStats bus_error_stats_;
    TopDownStats top_down_stats_;
    bool expected_top_down_l2_miss_ = false;
    bool expected_top_down_l3_miss_ = false;
    bool expected_l2_flush_done_ = false;
    std::uint64_t l2_flush_checks_ = 0;
    std::uint64_t expected_reset_vector_ = 0;
    bool expected_cpu_halted_ = false;
    bool expected_cpu_critical_error_ = false;
    std::uint64_t top_control_checks_ = 0;
    bool expected_beu_valid_ = false;
    std::uint64_t expected_beu_address_ = 0;
    bool expected_msi_info_valid_ = false;
    std::uint16_t expected_msi_info_ = 0;
    bool expected_clint_time_valid_ = false;
    std::uint64_t expected_clint_time_ = 0;
    ExternalInterruptState expected_external_interrupt_{};
    std::array<
        std::uint8_t, generated::kHcPerfEventHighestInputLane + 1>
        expected_hc_perf_events_{};
    L2PrefetchControl l2_prefetch_delay_stage_{};
    L2PrefetchControl expected_l2_prefetch_output_{};
    std::uint64_t top_bridge_checks_ = 0;
    bool expected_trace_encoder_enable_ = false;
    bool expected_trace_encoder_stall_ = false;
    std::uint8_t expected_trace_privilege_ = 0;
    std::uint64_t expected_trace_mstatus_ = 0;
    std::uint64_t expected_trace_trap_cause_ = 0;
    std::uint64_t expected_trace_trap_tval_ = 0;
    std::array<TraceGroupState, kTraceGroups> expected_trace_groups_{};
    std::array<bool, kTraceGroups> trace_group_payload_initialized_{};
    bool trace_privilege_initialized_ = false;
    bool trace_trap_initialized_ = false;
    std::uint64_t trace_bridge_checks_ = 0;
    std::uint64_t dft_bridge_patterns_ = 0;
    std::uint64_t dft_bridge_digest_ = 0;
    std::uint64_t reset_functional_pulses_ = 0;
    std::uint64_t reset_dft_pulses_ = 0;
    std::uint64_t reset_scan_transitions_ = 0;
    std::uint64_t reset_async_assertions_ = 0;
    std::uint64_t ifetch_ptw_pending_ = 0;
    std::uint64_t lq_allocated_ = 0;
    std::uint64_t lq_enqueued_observed_ = 0;
    std::uint64_t lq_dequeued_ = 0;
    std::uint64_t lq_canceled_ = 0;
    std::uint64_t lq_redirect_canceled_observed_ = 0;
    std::uint64_t lq_canceled_unobserved_ = 0;
    std::uint64_t sq_allocated_ = 0;
    std::uint64_t sq_enqueued_observed_ = 0;
    std::uint64_t sq_dequeued_ = 0;
    std::uint64_t sq_canceled_ = 0;
    std::uint64_t sq_redirect_canceled_observed_ = 0;
    std::uint64_t sq_canceled_unobserved_ = 0;
    std::uint64_t redirect_cancellation_events_observed_ = 0;
    std::array<std::uint64_t, generated::kLsqEnqueueLanes>
        lsq_enqueue_widths_observed_{};
    std::array<std::uint64_t, generated::kLsqEnqueueLanes>
        lsq_enqueue_lanes_observed_{};
    std::unordered_map<std::uint64_t, std::uint64_t>
        scalar_store_sq_targets_;
    std::unordered_map<std::uint64_t, std::uint64_t>
        vector_store_sq_targets_;
    std::uint64_t store_tlb_feedbacks_ = 0;
    std::uint64_t store_tlb_misses_ = 0;
    std::deque<VectorReplayRequest> vector_replay_requests_;
    std::uint64_t vector_replay_feedbacks_ = 0;
    std::unordered_map<std::uint64_t, std::uint64_t> next_page_table_;
    std::unordered_map<std::uint64_t, std::uint64_t> next_gstage_page_table_;
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
    std::ofstream debug_trace_;
    std::uint64_t debug_line_ = 0;
    std::uint64_t debug_cycle_ = 0;
    std::uint32_t debug_internal_window_ = 0;
    bool debug_trace_all_a_ = false;
    std::array<bool, 64> debug_target_sources_{};
    std::array<unsigned, 64> debug_target_beats_remaining_{};
    std::array<XData *, kScalarLoadLanes> debug_mshr_forward_valid_{};
    std::array<XData *, kScalarLoadLanes> debug_mshr_forward_{};
    std::array<std::array<XData *, 16>, kScalarLoadLanes>
        debug_mshr_forward_data_{};
    std::array<XData *, kScalarLoadLanes> debug_s2_valid_{};
    std::array<XData *, kScalarLoadLanes> debug_s2_paddr_{};
    std::array<XData *, kScalarLoadLanes> debug_s2_vaddr_{};
    std::array<XData *, kScalarLoadLanes> debug_s2_rob_value_{};
    std::array<XData *, kScalarLoadLanes> debug_s2_rob_flag_{};
    std::array<XData *, kScalarLoadLanes> debug_s2_from_d_{};
    std::array<XData *, kScalarLoadLanes> debug_s2_miss_{};
    std::array<XData *, kScalarLoadLanes> debug_s2_full_fwd_{};
    std::array<std::array<XData *, 16>, kScalarLoadLanes> debug_s2_d_data_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_valid_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_paddr_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_vaddr_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_rob_value_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_rob_flag_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_handled_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_mshrid_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_data_select_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_input_data_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_merged_data_{};
    std::array<XData *, kScalarLoadLanes> debug_s3_picked_data_{};
    XData *debug_sms_req_valid_ = nullptr;
    XData *debug_sms_buffer_state_ = nullptr;
    XData *debug_sms_is_cross_page_ = nullptr;
    XData *debug_sms_req_isvec_ = nullptr;
    XData *debug_sms_req_rob_value_ = nullptr;
    XData *debug_sms_req_sq_value_ = nullptr;
    XData *debug_sms_req_sq_flag_ = nullptr;
    XData *debug_sms_cross_page_with_hit_ = nullptr;
    std::array<XData *, 2> debug_sms_vec_wb_{};
    XData *debug_sq_do_deq_ = nullptr;
#endif
    std::string error_;
};
