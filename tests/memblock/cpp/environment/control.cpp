#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::pulse_sbuffer_flush()  -> bool
{
    dut_.backend.input.flush_sb.set(std::uint64_t{1});
    tick();
    dut_.backend.input.flush_sb.set(std::uint64_t{0});
    tick();
    return check_components();
}

auto Environment::Impl::drive_l2_flush(bool enable,
    bool done)  -> void
{
    dut_.control.csr.flush_l2_enable.set(enable);
    dut_.control.l2_flush_done.set(done);
}

auto Environment::Impl::outer_l2_flush_enabled()  -> bool
{
    return dut_.top.output.l2_flush_en.boolean();
}

auto Environment::Impl::backend_l2_flush_done()  -> bool
{
    return dut_.top.backend.l2_flush_done.boolean();
}

auto Environment::Impl::l2_flush_checks() const -> std::uint64_t
{
    return bridge_.l2_flush_checks();
}

auto Environment::Impl::drive_top_controls(std::uint8_t hart_id,
    std::uint64_t reset_vector,
    bool power_down,
    bool cpu_halted,
    bool cpu_critical_error)  -> void
{
    dut_.top.hart_id.set(hart_id & 0x3fU);
    dut_.top.output.reset_vector.set(
        reset_vector & ((std::uint64_t{1} << 48) - 1U));
    dut_.control.csr.power_down_enable.set(power_down);
    dut_.top.backend_input.cpu_halted.set(cpu_halted);
    dut_.top.backend_input.cpu_critical_error.set(
        cpu_critical_error);
}

auto Environment::Impl::backend_hart_id()  -> std::uint8_t
{
    return static_cast<std::uint8_t>(
        dut_.top.backend.hart_id.value());
}

auto Environment::Impl::inner_reset_vector()  -> std::uint64_t
{
    return dut_.top.inner_reset_vector.value();
}

auto Environment::Impl::outer_power_down_enabled()  -> bool
{
    return dut_.top.output.power_down_en.boolean();
}

auto Environment::Impl::outer_cpu_halted()  -> bool
{
    return dut_.top.output.cpu_halt.boolean();
}

auto Environment::Impl::outer_cpu_critical_error()  -> bool
{
    return dut_.top.output.cpu_critical_error.boolean();
}

auto Environment::Impl::top_control_checks() const -> std::uint64_t
{
    return bridge_.top_control_checks();
}

auto Environment::Impl::drive_top_bridge_stimulus(const TopBridgeStimulus &stimulus)  -> void
{
    dut_.top.backend_input.msi_ack.set(stimulus.msi_ack);
    dut_.reset.frontend.from_frontend.set(
        stimulus.frontend_reset);
    dut_.errors.frontend_input.icache_ecc_error_valid.set(
        stimulus.beu_valid);
    dut_.errors.frontend_input.icache_ecc_error_bits.set(
        stimulus.beu_address & ((std::uint64_t{1} << 48) - 1U));
    dut_.top.input.msi_info_valid.set(
        stimulus.msi_info_valid);
    dut_.top.input.msi_info_bits.set(
        stimulus.msi_info & 0xfffU);
    dut_.top.input.clint_time_valid.set(
        stimulus.clint_time_valid);
    dut_.top.input.clint_time_bits.set(stimulus.clint_time);
    dut_.interrupts.clint.lane_0.set(stimulus.interrupt_msip);
    dut_.interrupts.clint.lane_1.set(stimulus.interrupt_mtip);
    dut_.interrupts.plic.lane_0_0.set(stimulus.interrupt_meip);
    dut_.interrupts.plic.lane_1_0.set(stimulus.interrupt_seip);
    dut_.interrupts.debug.lane_0.set(stimulus.interrupt_debug);
    dut_.interrupts.nmi.lane_0.set(stimulus.interrupt_nmi_31);
    dut_.interrupts.nmi.lane_1.set(stimulus.interrupt_nmi_43);
    dut_.interrupts.bus_error.lane_0.set(
        stimulus.interrupt_beu_local);
    for (unsigned lane = generated::kHcPerfEventFirstInputLane;
         lane <= generated::kHcPerfEventHighestInputLane; ++lane) {
        dut_.drive_hc_perf_event(lane, stimulus.hc_perf_events[lane]);
    }
    dut_.control.prefetch.l2_pf_enable.set(
        stimulus.l2_prefetch.master_enable);
    dut_.control.prefetch.l2_pf_recv_enable.set(
        stimulus.l2_prefetch.receive_enable);
    dut_.control.prefetch.l2_pf_pbop_enable.set(
        stimulus.l2_prefetch.pbop_enable);
    dut_.control.prefetch.l2_pf_vbop_enable.set(
        stimulus.l2_prefetch.vbop_enable);
    dut_.control.prefetch.l2_pf_tp_enable.set(
        stimulus.l2_prefetch.tp_enable);
    dut_.control.prefetch.l2_pf_delay_latency.set(
        stimulus.l2_prefetch.delay_latency & 0x3ffU);
}

auto Environment::Impl::top_bridge_checks() const -> std::uint64_t
{
    return bridge_.top_bridge_checks();
}

auto Environment::Impl::drive_trace_bridge_stimulus(const TraceBridgeStimulus &stimulus)  -> void
{
    dut_.trace.top.from_encoder_enable.set(
        stimulus.encoder_enable);
    dut_.trace.top.from_encoder_stall.set(
        stimulus.encoder_stall);
    dut_.trace.backend.to_encoder_priv.set(
        stimulus.privilege & 0x7U);
    dut_.trace.backend.to_encoder_mstatus.set(
        stimulus.mstatus);
    dut_.trace.backend.to_encoder_trap_cause.set(
        stimulus.trap_cause);
    dut_.trace.backend.to_encoder_trap_tval.set(
        stimulus.trap_tval & ((std::uint64_t{1} << 50) - 1U));

    const auto drive_group = [](auto &valid, auto &iaddr, auto &ftq_offset,
                                auto &itype, auto &iretire, auto &ilastsize,
                                const TraceGroupState &group) {
        valid.set(group.valid);
        iaddr.set(group.iaddr & ((std::uint64_t{1} << 50) - 1U));
        ftq_offset.set(group.ftq_offset & 0xfU);
        itype.set(group.itype & 0xfU);
        iretire.set(group.iretire & 0x7fU);
        ilastsize.set(group.ilastsize);
    };
    drive_group(
        dut_.trace.backend.to_encoder_groups_0_valid,
        dut_.trace.backend.to_encoder_groups_0_iaddr,
        dut_.trace.backend.to_encoder_groups_0_ftq_offset,
        dut_.trace.backend.to_encoder_groups_0_itype,
        dut_.trace.backend.to_encoder_groups_0_iretire,
        dut_.trace.backend.to_encoder_groups_0_ilastsize,
        stimulus.groups[0]);
    drive_group(
        dut_.trace.backend.to_encoder_groups_1_valid,
        dut_.trace.backend.to_encoder_groups_1_iaddr,
        dut_.trace.backend.to_encoder_groups_1_ftq_offset,
        dut_.trace.backend.to_encoder_groups_1_itype,
        dut_.trace.backend.to_encoder_groups_1_iretire,
        dut_.trace.backend.to_encoder_groups_1_ilastsize,
        stimulus.groups[1]);
    drive_group(
        dut_.trace.backend.to_encoder_groups_2_valid,
        dut_.trace.backend.to_encoder_groups_2_iaddr,
        dut_.trace.backend.to_encoder_groups_2_ftq_offset,
        dut_.trace.backend.to_encoder_groups_2_itype,
        dut_.trace.backend.to_encoder_groups_2_iretire,
        dut_.trace.backend.to_encoder_groups_2_ilastsize,
        stimulus.groups[2]);
}

auto Environment::Impl::trace_bridge_checks() const -> std::uint64_t
{
    return bridge_.trace_bridge_checks();
}

auto Environment::Impl::check_dft_bridge_space()  -> bool
{
    dft_bridge_patterns_ = 0;
    dft_bridge_digest_ = 1469598103934665603ULL;
    dut_.reset.external.set(std::uint64_t{1});
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
        dut_.dft.ram_hold.set(ram_hold);
        dut_.dft.ram_bypass.set(ram_bypass);
        dut_.dft.ram_bp_clken.set(ram_bp_clken);
        dut_.dft.ram_aux_clk.set(ram_aux_clk);
        dut_.dft.ram_aux_ckbp.set(ram_aux_ckbp);
        dut_.dft.ram_mcp_hold.set(ram_mcp_hold);
        dut_.dft.cgen.set(cgen);
        dut_.dft.reset_lgc_rst_n.set(lgc_rst_n);
        dut_.dft.reset_mode.set(reset_mode);
        dut_.dft.reset_scan_mode.set(scan_mode);
        dut_.refresh_comb();
        const bool matches =
            dut_.dft.frnt_ram_hold.boolean() == ram_hold &&
            dut_.dft.frnt_ram_bypass.boolean() == ram_bypass &&
            dut_.dft.frnt_ram_bp_clken.boolean() == ram_bp_clken &&
            dut_.dft.frnt_ram_aux_clk.boolean() == ram_aux_clk &&
            dut_.dft.frnt_ram_aux_ckbp.boolean() == ram_aux_ckbp &&
            dut_.dft.frnt_ram_mcp_hold.boolean() == ram_mcp_hold &&
            dut_.dft.frnt_cgen.boolean() == cgen &&
            dut_.dft.bcknd_cgen.boolean() == cgen &&
            dut_.dft.reset_frnt_lgc_rst_n.boolean() == lgc_rst_n &&
            dut_.dft.reset_frnt_mode.boolean() == reset_mode &&
            dut_.dft.reset_frnt_scan_mode.boolean() == scan_mode &&
            dut_.dft.reset_bcknd_lgc_rst_n.boolean() == lgc_rst_n &&
            dut_.dft.reset_bcknd_mode.boolean() == reset_mode &&
            dut_.dft.reset_bcknd_scan_mode.boolean() == scan_mode;
        if (!matches) {
            std::ostringstream message;
            message << "DFT bridge mismatch pattern=0x" << std::hex
                    << pattern;
            error_.record(message.str());
            return false;
        }
        ++dft_bridge_patterns_;
        dft_bridge_digest_ ^= pattern;
        dft_bridge_digest_ *= 1099511628211ULL;
    }
    dut_.dft.ram_hold.set(std::uint64_t{0});
    dut_.dft.ram_bypass.set(std::uint64_t{0});
    dut_.dft.ram_bp_clken.set(std::uint64_t{0});
    dut_.dft.ram_aux_clk.set(std::uint64_t{0});
    dut_.dft.ram_aux_ckbp.set(std::uint64_t{0});
    dut_.dft.ram_mcp_hold.set(std::uint64_t{0});
    dut_.dft.cgen.set(std::uint64_t{0});
    dut_.dft.reset_lgc_rst_n.set(std::uint64_t{1});
    dut_.dft.reset_mode.set(std::uint64_t{0});
    dut_.dft.reset_scan_mode.set(std::uint64_t{0});
    dut_.refresh_comb();
    return true;
}

auto Environment::Impl::dft_bridge_patterns() const -> std::uint64_t
{
    return dft_bridge_patterns_;
}

auto Environment::Impl::dft_bridge_digest() const -> std::uint64_t
{
    return dft_bridge_digest_;
}

auto Environment::Impl::set_sbuffer_timeout(std::uint32_t cycles)  -> bool
{
    constexpr std::uint32_t timeout_width = 22;
    if (cycles >= (std::uint32_t{1} << timeout_width)) {
        error_.record("SBuffer timeout exceeds the 22-bit CSR field");
        return false;
    }
    dut_.control.csr.sbuffer_timeout.set(cycles);
    return run_cycles(4) && check_components();
}

auto Environment::Impl::set_mbmc(bool bitmap_enabled,
    bool confidential_mode,
    std::uint64_t bitmap_base)  -> bool
{
    if ((bitmap_base & 63U) != 0) {
        error_.record("MBMC bitmap base must be 64-byte aligned");
        return false;
    }
    dut_.translation.csr.mbmc_bme.set(bitmap_enabled);
    dut_.translation.csr.mbmc_cmode.set(confidential_mode);
    dut_.translation.csr.mbmc_bclear.set(std::uint64_t{0});
    dut_.translation.csr.mbmc_bma.set(bitmap_base >> 6);
    return run_cycles(16) && check_components();
}

auto Environment::Impl::pulse_mbmc_bitmap_clear()  -> bool
{
    dut_.translation.csr.mbmc_bclear.set(std::uint64_t{1});
    if (!run_cycles(8)) {
        return false;
    }
    dut_.translation.csr.mbmc_bclear.set(std::uint64_t{0});
    return run_cycles(8) && check_components();
}

auto Environment::Impl::set_pointer_masking(const PointerMaskingConfig &config)  -> bool
{
    dut_.translation.csr.pmm_mseccfg.set(
        static_cast<std::uint64_t>(config.machine));
    dut_.translation.csr.pmm_menvcfg.set(
        static_cast<std::uint64_t>(config.supervisor));
    dut_.translation.csr.pmm_henvcfg.set(
        static_cast<std::uint64_t>(config.virtual_supervisor));
    dut_.translation.csr.pmm_hstatus.set(
        static_cast<std::uint64_t>(config.hypervisor_user));
    dut_.translation.csr.pmm_senvcfg.set(
        static_cast<std::uint64_t>(config.user));
    return run_cycles(16) && check_components();
}

auto Environment::Impl::issue_sfence(std::uint64_t address,
    std::uint16_t id,
    bool all_virtual_addresses,
    bool all_contexts,
    bool hypervisor_virtual,
    bool hypervisor_guest,
    bool flush_pipe)  -> bool
{
    dut_.translation.fence.rs1.set(all_virtual_addresses);
    dut_.translation.fence.rs2.set(all_contexts);
    dut_.translation.fence.addr.set(address);
    dut_.translation.fence.id.set(id);
    dut_.translation.fence.flush_pipe.set(flush_pipe);
    dut_.translation.fence.hv.set(hypervisor_virtual);
    dut_.translation.fence.hg.set(hypervisor_guest);
    dut_.translation.fence.valid.set(std::uint64_t{1});
    tick(false);
    dut_.translation.fence.valid.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

auto Environment::Impl::issue_sfence_with_redirect(std::uint8_t redirect_rob_value,
    bool redirect_rob_flag,
    bool redirect_flush_itself,
    std::uint64_t address,
    std::uint16_t id,
    bool all_virtual_addresses,
    bool all_contexts,
    bool hypervisor_virtual,
    bool hypervisor_guest,
    bool flush_pipe)  -> bool
{
    dut_.translation.fence.rs1.set(all_virtual_addresses);
    dut_.translation.fence.rs2.set(all_contexts);
    dut_.translation.fence.addr.set(address);
    dut_.translation.fence.id.set(id);
    dut_.translation.fence.flush_pipe.set(flush_pipe);
    dut_.translation.fence.hv.set(hypervisor_virtual);
    dut_.translation.fence.hg.set(hypervisor_guest);
    dut_.translation.fence.valid.set(std::uint64_t{1});
    tick();
    dut_.translation.fence.valid.set(std::uint64_t{0});

    // MemBlock registers sfence twice and redirect once. Pulse redirect
    // one top-level cycle later so both reach their consumers together.
    dut_.queues.redirect.rob_idx_flag.set(redirect_rob_flag);
    dut_.queues.redirect.rob_idx_value.set(redirect_rob_value);
    dut_.queues.redirect.level.set(redirect_flush_itself);
    dut_.queues.redirect.is_vls_exception.set(std::uint64_t{0});
    dut_.queues.redirect.valid.set(std::uint64_t{1});
    tick();
    dut_.queues.redirect.valid.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

auto Environment::Impl::enable_misaligned_accesses(bool load,
    bool store)  -> bool
{
    dut_.control.csr.hd_misalign_ld_enable.set(load);
    dut_.control.csr.hd_misalign_st_enable.set(store);
    return run_cycles(8) && check_components();
}

auto Environment::Impl::set_rob_head(std::uint8_t value,
    bool flag)  -> bool
{
    dut_.queues.backend.pending_ptr_value.set(value);
    dut_.queues.backend.pending_ptr_flag.set(flag);
    tick();
    return check_components();
}

auto Environment::Impl::pulse_pending_load(std::uint8_t value,
    bool flag)  -> bool
{
    dut_.queues.backend.pending_ptr_value.set(value);
    dut_.queues.backend.pending_ptr_flag.set(flag);
    dut_.queues.backend.pending_mmi_old.set(std::uint64_t{1});
    tick();
    dut_.queues.backend.pending_mmi_old.set(std::uint64_t{0});
    return check_components();
}

auto Environment::Impl::wait_for_mmio_request(std::uint8_t value,
    bool flag,
    unsigned timeout)  -> bool
{
    const std::uint64_t request_before = uncache_agent_.request_count();
    dut_.queues.backend.pending_ptr_value.set(value);
    dut_.queues.backend.pending_ptr_flag.set(flag);
    dut_.queues.backend.pending_mmi_old.set(std::uint64_t{1});
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        tick();
        if (!check_components()) {
            dut_.queues.backend.pending_mmi_old.set(std::uint64_t{0});
            return false;
        }
        if (uncache_agent_.request_count() > request_before) {
            dut_.queues.backend.pending_mmi_old.set(std::uint64_t{0});
            return true;
        }
    }
    dut_.queues.backend.pending_mmi_old.set(std::uint64_t{0});
    error_.record("timed out waiting for MMIO Uncache request");
    return false;
}

auto Environment::Impl::pulse_pending_store(std::uint8_t value,
    bool flag)  -> bool
{
    dut_.queues.backend.pending_ptr_value.set(value);
    dut_.queues.backend.pending_ptr_flag.set(flag);
    dut_.queues.backend.pendingst.set(std::uint64_t{1});
    tick();
    dut_.queues.backend.pendingst.set(std::uint64_t{0});
    return check_components();
}

auto Environment::Impl::pulse_store_commit_keep_pending(std::uint8_t value,
    bool flag)  -> bool
{
    dut_.queues.backend.pending_ptr_value.set(value);
    dut_.queues.backend.pending_ptr_flag.set(flag);
    dut_.queues.backend.pendingst.set(std::uint64_t{1});
    dut_.queues.backend.scommit.set(std::uint64_t{1});
    tick();
    dut_.queues.backend.scommit.set(std::uint64_t{0});
    return check_components();
}

auto Environment::Impl::wait_for_mmio_store_request(std::uint8_t value,
    bool flag,
    unsigned timeout)  -> bool
{
    const std::uint64_t request_before = uncache_agent_.request_count();
    dut_.queues.backend.pending_ptr_value.set(value);
    dut_.queues.backend.pending_ptr_flag.set(flag);
    // ROB keeps pendingst high while this store remains at its head. Keep
    // the real level contract until StoreQueue emits the Uncache request.
    dut_.queues.backend.pendingst.set(std::uint64_t{1});
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        tick();
        if (!check_components()) {
            dut_.queues.backend.pendingst.set(std::uint64_t{0});
            return false;
        }
        if (uncache_agent_.request_count() > request_before) {
            dut_.queues.backend.pendingst.set(std::uint64_t{0});
            return true;
        }
    }
    dut_.queues.backend.pendingst.set(std::uint64_t{0});
    error_.record("timed out waiting for MMIO store Uncache request");
    return false;
}

auto Environment::Impl::set_wfi(bool enabled)  -> bool
{
    dut_.control.wfi.wfi_req.set(enabled);
    tick();
    return check_components();
}

auto Environment::Impl::wfi_safe()  -> bool
{
    dut_.refresh_comb();
    return dut_.control.wfi.wfi_safe.boolean();
}

auto Environment::Impl::require_wfi_unsafe(unsigned cycles)  -> bool
{
    for (unsigned elapsed = 0; elapsed < cycles; ++elapsed) {
        dut_.refresh_comb();
        if (dut_.control.wfi.wfi_safe.boolean()) {
            std::ostringstream message;
            message << "wfiSafe asserted while a manager response was pending"
                    << " elapsed=" << elapsed;
            error_.record(message.str());
            return false;
        }
        tick();
        if (!check_components()) {
            return false;
        }
    }
    return true;
}

auto Environment::Impl::run_until_wfi_safe(unsigned timeout)  -> bool
{
    for (unsigned elapsed = 0; elapsed < timeout; ++elapsed) {
        dut_.refresh_comb();
        if (dut_.control.wfi.wfi_safe.boolean()) {
            return check_components();
        }
        tick();
        if (!check_components()) {
            return false;
        }
    }
    error_.record("timed out waiting for wfiSafe after manager drain");
    return false;
}

auto Environment::Impl::enqueue_load_batch(const std::vector<LoadTransaction> &transactions,
    const std::vector<unsigned> &dispatch_lanes)  -> bool
{
    if (transactions.empty() ||
        transactions.size() != dispatch_lanes.size() ||
        transactions.size() > generated::kLsqEnqueueLanes ||
        !std::is_sorted(dispatch_lanes.begin(), dispatch_lanes.end())) {
        error_.record("LSQ load batch requires one sorted unique dispatch lane per load");
        return false;
    }
    std::array<bool, generated::kLsqEnqueueLanes> lane_used{};
    if (!wait_for_enqueue_capacity(transactions.size(), 0)) {
        return false;
    }
    for (std::size_t index = 0; index < transactions.size(); ++index) {
        const unsigned lane = dispatch_lanes[index];
        if (lane >= generated::kLsqEnqueueLanes || lane_used[lane]) {
            error_.record("LSQ load batch has an invalid or duplicate dispatch lane");
            dut_.clear_lsq_enqueue_valids();
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
        dut_.drive_lsq_enqueue(lane, enqueue);
    }
    tick();
    dut_.clear_lsq_enqueue_valids();
    queues_.allocate_load(transactions.size());
    return check_components();
}

auto Environment::Impl::issue_load(const LoadTransaction &transaction,
    unsigned timeout)  -> bool
{
    if (!scalar_immediate_is_valid(transaction.immediate)) {
        error_.record("scalar load immediate exceeds signed 12-bit range");
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
        dut_.drive_scalar_load_issue(transaction.lane, issue);
        dut_.refresh_comb();
        const bool ready = dut_.scalar_load_issue_ready(transaction.lane);
        tick();
        if (ready) {
            dut_.clear_scalar_load_issue_valid(transaction.lane);
            return check_components();
        }
        if (!check_components()) {
            return false;
        }
    }
    dut_.clear_scalar_load_issue_valid(transaction.lane);
    error_.record("scalar load issue timed out waiting for ready");
    return false;
}

auto Environment::Impl::issue_load_batch(const std::vector<LoadTransaction> &transactions,
    unsigned timeout,
    bool require_same_cycle)  -> bool
{
    if (transactions.empty() || transactions.size() > kScalarLoadLanes) {
        error_.record("scalar load batch must contain one to three transactions");
        return false;
    }
    std::array<bool, kScalarLoadLanes> lane_used{};
    std::vector<generated::ScalarLoadIssue> issues(transactions.size());
    std::vector<bool> pending(transactions.size(), true);
    for (std::size_t index = 0; index < transactions.size(); ++index) {
        const auto &transaction = transactions[index];
        if (transaction.lane >= kScalarLoadLanes || lane_used[transaction.lane]) {
            error_.record("scalar load batch lanes must be unique");
            return false;
        }
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_.record("scalar load immediate exceeds signed 12-bit range");
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
                dut_.drive_scalar_load_issue(transactions[index].lane, issues[index]);
            }
            dut_.refresh_comb();
            const bool all_ready = std::all_of(
                transactions.begin(), transactions.end(),
                [&](const auto &transaction) {
                    return dut_.scalar_load_issue_ready(transaction.lane);
                });
            if (all_ready) {
                tick();
                dut_.clear_scalar_load_issue_valids();
                return check_components();
            }
            dut_.clear_scalar_load_issue_valids();
            tick();
            if (!check_components()) {
                return false;
            }
            continue;
        }
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            if (pending[index]) {
                dut_.drive_scalar_load_issue(transactions[index].lane, issues[index]);
            }
        }
        dut_.refresh_comb();
        std::vector<bool> accepted(transactions.size(), false);
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            accepted[index] = pending[index] && dut_.scalar_load_issue_ready(transactions[index].lane);
        }
        tick();
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            if (accepted[index]) {
                pending[index] = false;
                dut_.clear_scalar_load_issue_valid(transactions[index].lane);
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
    dut_.clear_scalar_load_issue_valids();
    error_.record(require_same_cycle
        ? "scalar load issue batch timed out waiting for same-cycle ready"
        : "scalar load issue batch timed out waiting for ready");
    return false;
}

auto Environment::Impl::expect_load(const LoadTransaction &transaction)  -> void
{
    scoreboard_.expect(
        transaction,
        memory_.expected_load(
            transaction.oracle_address.value_or(transaction.address),
            transaction.op));
}

auto Environment::Impl::expect_load_data(const LoadTransaction &transaction,
    std::uint64_t data)  -> void
{
    scoreboard_.expect(transaction, data);
}

auto Environment::Impl::expect_prefetch(const PrefetchTransaction &transaction)  -> void
{
    scoreboard_.expect_prefetch(transaction);
}

auto Environment::Impl::enqueue_prefetch(const PrefetchTransaction &transaction)  -> bool
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
    dut_.drive_lsq_enqueue(0, enqueue);
    tick();
    dut_.clear_lsq_enqueue_valids();
    queues_.allocate_load(1);
    return check_components();
}

} // namespace memblock
