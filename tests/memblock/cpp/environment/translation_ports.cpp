#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::issue_l2_tlb_request(std::uint64_t vaddr,
    std::uint8_t cmd,
    bool kill,
    bool is_prefetch,
    bool no_translate,
    L2TlbResponse &response,
    unsigned timeout)  -> bool
{
    dut_.translation.l2_request.vaddr.set(vaddr);
    dut_.translation.l2_request.cmd.set(cmd);
    dut_.translation.l2_request.kill.set(kill);
    dut_.translation.l2_request.is_prefetch.set(is_prefetch);
    dut_.translation.l2_request.no_translate.set(no_translate);
    dut_.translation.l2_request.valid.set(std::uint64_t{1});
    tick(false);
    dut_.translation.l2_request.valid.set(std::uint64_t{0});
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        if (dut_.translation.l2_response.valid.boolean()) {
            response.paddr = dut_.translation.l2_response.paddr_0.value();
            response.pbmt = static_cast<std::uint8_t>(
                dut_.translation.l2_response.pbmt_0.value());
            response.miss = dut_.translation.l2_response.miss.boolean();
            response.guest_page_fault =
                dut_.translation.l2_response.excp_0_gpf_ld.boolean();
            response.page_fault =
                dut_.translation.l2_response.excp_0_pf_ld.boolean();
            response.access_fault =
                dut_.translation.l2_response.excp_0_af_ld.boolean();
            // The retained L2 PMP response comes from a leaveHitMux
            // checker, whose match and config are registered one cycle
            // after the TLB response payload becomes valid.
            tick(false);
            response.pmp_load_denied = dut_.translation.l2_protection.ld.boolean();
            response.pmp_mmio = dut_.translation.l2_protection.mmio.boolean();
            return check_components();
        }
        tick(false);
    }
    error_.record("timed out waiting for L2-to-L1 DTLB response");
    return false;
}

auto Environment::Impl::issue_killed_l2_tlb_request(std::uint64_t vaddr,
    std::uint8_t cmd,
    bool is_prefetch,
    bool no_translate,
    unsigned timeout)  -> bool
{
    dut_.translation.l2_request.vaddr.set(vaddr);
    dut_.translation.l2_request.cmd.set(cmd);
    dut_.translation.l2_request.kill.set(std::uint64_t{1});
    dut_.translation.l2_request.is_prefetch.set(is_prefetch);
    dut_.translation.l2_request.no_translate.set(no_translate);
    dut_.translation.l2_request.valid.set(std::uint64_t{1});
    tick(false);
    dut_.translation.l2_request.valid.set(std::uint64_t{0});
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        if (dut_.translation.l2_response.valid.boolean()) {
            error_.record("killed L2-to-L1 DTLB request produced a response");
            return false;
        }
        tick(false);
    }
    return check_components();
}

auto Environment::Impl::start_ifetch_ptw_request(std::uint64_t vpn,
    PtwTranslationMode mode,
    unsigned timeout)  -> bool
{
    if ((vpn >> 38) != 0) {
        error_.record("invalid IFetch PTW request");
        return false;
    }
    const auto s2xlate = static_cast<std::uint8_t>(mode);

    dut_.translation.ifetch_response.ready.set(std::uint64_t{0});
    dut_.translation.ifetch_request.vpn.set(vpn);
    dut_.translation.ifetch_request.s2xlate.set(s2xlate);
    dut_.translation.ifetch_request.valid.set(std::uint64_t{1});
    bool accepted = false;
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        dut_.refresh_comb();
        const bool fire = dut_.translation.ifetch_request.ready.boolean();
        tick(false);
        if (fire) {
            accepted = true;
            break;
        }
        if (!check_components()) {
            break;
        }
    }
    dut_.translation.ifetch_request.valid.set(std::uint64_t{0});
    if (!accepted) {
        if (error_.empty()) {
            error_.record("timed out accepting IFetch PTW request");
        }
        dut_.translation.ifetch_response.ready.set(std::uint64_t{1});
        return false;
    }
    ++ifetch_ptw_pending_;
    return check_components();
}

auto Environment::Impl::complete_ifetch_ptw_request(IFetchPtwResponse &response,
    unsigned response_stall_cycles,
    unsigned timeout)  -> bool
{
    if (ifetch_ptw_pending_ == 0) {
        error_.record("no IFetch PTW request pending");
        return false;
    }
    const auto capture = [&]() {
        IFetchPtwResponse result;
        result.s2xlate = static_cast<std::uint8_t>(
            dut_.translation.ifetch_response.s2xlate.value());
        result.s1_tag = dut_.translation.ifetch_response.s1_entry_tag.value();
        result.s1_asid = static_cast<std::uint16_t>(
            dut_.translation.ifetch_response.s1_entry_asid.value());
        result.s1_vmid = static_cast<std::uint16_t>(
            dut_.translation.ifetch_response.s1_entry_vmid.value());
        result.s1_n = dut_.translation.ifetch_response.s1_entry_n.boolean();
        result.s1_pbmt = static_cast<std::uint8_t>(
            dut_.translation.ifetch_response.s1_entry_pbmt.value());
        result.s1_d = dut_.translation.ifetch_response.s1_entry_perm_d.boolean();
        result.s1_a = dut_.translation.ifetch_response.s1_entry_perm_a.boolean();
        result.s1_g = dut_.translation.ifetch_response.s1_entry_perm_g.boolean();
        result.s1_u = dut_.translation.ifetch_response.s1_entry_perm_u.boolean();
        result.s1_x = dut_.translation.ifetch_response.s1_entry_perm_x.boolean();
        result.s1_w = dut_.translation.ifetch_response.s1_entry_perm_w.boolean();
        result.s1_r = dut_.translation.ifetch_response.s1_entry_perm_r.boolean();
        result.s1_level = static_cast<std::uint8_t>(
            dut_.translation.ifetch_response.s1_entry_level.value());
        result.s1_v = dut_.translation.ifetch_response.s1_entry_v.boolean();
        result.s1_ppn = dut_.translation.ifetch_response.s1_entry_ppn.value();
        result.s1_addr_low = static_cast<std::uint8_t>(
            dut_.translation.ifetch_response.s1_addr_low.value());
#define CAPTURE_IFETCH_SECTOR(index)                                           \
        result.s1_ppn_low[index] = static_cast<std::uint8_t>(             \
            dut_.translation.ifetch_response.s1_ppn_low_##index.value());  \
        result.s1_valididx |= static_cast<std::uint8_t>(                  \
            dut_.translation.ifetch_response.s1_valididx_##index.boolean())  \
            << index;                                                     \
        result.s1_pteidx |= static_cast<std::uint8_t>(                    \
            dut_.translation.ifetch_response.s1_pteidx_##index.boolean())    \
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
        result.s1_pf = dut_.translation.ifetch_response.s1_pf.boolean();
        result.s1_af = dut_.translation.ifetch_response.s1_af.boolean();
        result.s2_tag = dut_.translation.ifetch_response.s2_entry_tag.value();
        result.s2_vmid = static_cast<std::uint16_t>(
            dut_.translation.ifetch_response.s2_entry_vmid.value());
        result.s2_n = dut_.translation.ifetch_response.s2_entry_n.boolean();
        result.s2_pbmt = static_cast<std::uint8_t>(
            dut_.translation.ifetch_response.s2_entry_pbmt.value());
        result.s2_ppn = dut_.translation.ifetch_response.s2_entry_ppn.value();
        result.s2_d = dut_.translation.ifetch_response.s2_entry_perm_d.boolean();
        result.s2_a = dut_.translation.ifetch_response.s2_entry_perm_a.boolean();
        result.s2_g = dut_.translation.ifetch_response.s2_entry_perm_g.boolean();
        result.s2_u = dut_.translation.ifetch_response.s2_entry_perm_u.boolean();
        result.s2_x = dut_.translation.ifetch_response.s2_entry_perm_x.boolean();
        result.s2_w = dut_.translation.ifetch_response.s2_entry_perm_w.boolean();
        result.s2_r = dut_.translation.ifetch_response.s2_entry_perm_r.boolean();
        result.s2_level = static_cast<std::uint8_t>(
            dut_.translation.ifetch_response.s2_entry_level.value());
        result.s2_gpf = dut_.translation.ifetch_response.s2_gpf.boolean();
        result.s2_gaf = dut_.translation.ifetch_response.s2_gaf.boolean();
        return result;
    };

    bool observed = false;
    IFetchPtwResponse held;
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        if (dut_.translation.ifetch_response.valid.boolean()) {
            held = capture();
            observed = true;
            break;
        }
        tick(false);
    }
    if (!observed) {
        error_.record("timed out waiting for IFetch PTW response");
        dut_.translation.ifetch_response.ready.set(std::uint64_t{1});
        ifetch_ptw_pending_ = 0;
        return false;
    }
    for (unsigned cycle = 0; cycle < response_stall_cycles; ++cycle) {
        if (!dut_.translation.ifetch_response.valid.boolean() ||
            !(capture() == held)) {
            error_.record("IFetch PTW response changed while stalled");
            dut_.translation.ifetch_response.ready.set(std::uint64_t{1});
            ifetch_ptw_pending_ = 0;
            return false;
        }
        tick(false);
    }
    response = held;
    dut_.translation.ifetch_response.ready.set(std::uint64_t{1});
    tick(false);
    --ifetch_ptw_pending_;
    if (ifetch_ptw_pending_ != 0) {
        dut_.translation.ifetch_response.ready.set(std::uint64_t{0});
        dut_.refresh_comb();
    } else if (dut_.translation.ifetch_response.valid.boolean()) {
        error_.record("IFetch PTW response did not retire after handshake");
        return false;
    }
    return check_components();
}

auto Environment::Impl::issue_ifetch_ptw_request(std::uint64_t vpn,
    PtwTranslationMode mode,
    IFetchPtwResponse &response,
    unsigned response_stall_cycles,
    unsigned timeout)  -> bool
{
    if (ifetch_ptw_pending_ != 0) {
        error_.record("synchronous IFetch PTW request requires an empty queue");
        return false;
    }
    return start_ifetch_ptw_request(vpn, mode, timeout) &&
        complete_ifetch_ptw_request(
            response, response_stall_cycles, timeout);
}

auto Environment::Impl::confirm_ifetch_ptw_flushed(unsigned quiet_cycles)  -> bool
{
    if (ifetch_ptw_pending_ == 0) {
        error_.record("no pending IFetch PTW request to flush");
        return false;
    }
    dut_.translation.ifetch_request.valid.set(std::uint64_t{0});
    dut_.translation.ifetch_response.ready.set(std::uint64_t{1});
    ifetch_ptw_pending_ = 0;
    for (unsigned cycle = 0; cycle < quiet_cycles; ++cycle) {
        dut_.refresh_comb();
        if (dut_.translation.ifetch_response.valid.boolean()) {
            error_.record("flushed IFetch PTW request produced a stale response");
            return false;
        }
        tick(false);
        if (!check_components()) {
            return false;
        }
    }
    dut_.refresh_comb();
    if (dut_.translation.ifetch_response.valid.boolean()) {
        error_.record("flushed IFetch PTW request produced a stale response");
        return false;
    }
    return check_components();
}

auto Environment::Impl::pulse_l2_hint(std::uint8_t source_id,
    bool is_keyword)  -> bool
{
    if (source_id >= 16) {
        error_.record("L2 hint source ID exceeds the top-level field width");
        return false;
    }
    dut_.control.l2_hint.source_id.set(source_id);
    dut_.control.l2_hint.is_keyword.set(is_keyword);
    dut_.control.l2_hint.valid.set(std::uint64_t{1});
    tick(false);
    dut_.control.l2_hint.valid.set(std::uint64_t{0});
    // MemBlock registers the hint before distributing it to DCache/LSQ.
    // Let that one-cycle pulse drain and reject any unexpected terminal
    // traffic caused by a hint with no matching outstanding MSHR.
    return run_cycles(4) && check_components();
}

auto Environment::Impl::configure_memory_trigger(const MemoryTriggerConfig &config)  -> bool
{
    if (config.index >= 4 || config.action >= 16 ||
        (config.match_type != kTriggerMatchEqual &&
         config.match_type != kTriggerMatchGreaterOrEqual &&
         config.match_type != kTriggerMatchLessThan) ||
        config.enable_mask >= 16) {
        error_.record("invalid memory trigger configuration");
        return false;
    }
    // The CSR block presents a two-cycle delayed tdata update. Keep all
    // trigger fields explicit so this helper is independent of the idle
    // policy and can be reused by constrained-random trigger tests.
    dut_.control.trigger.t_update_addr.set(
        config.index);
    dut_.control.trigger.t_update_tdata_match_type.set(
        config.match_type);
    dut_.control.trigger.t_update_tdata_select.set(
        config.select);
    dut_.control.trigger.t_update_tdata_action.set(
        config.action);
    dut_.control.trigger.t_update_tdata_chain.set(
        config.chain);
    dut_.control.trigger.t_update_tdata_store.set(
        config.store);
    dut_.control.trigger.t_update_tdata_load.set(
        config.load);
    dut_.control.trigger.t_update_tdata_tdata2.set(
        config.address);
    dut_.control.trigger.t_enable_vec_0.set(
        (config.enable_mask & 0x1U) != 0);
    dut_.control.trigger.t_enable_vec_1.set(
        (config.enable_mask & 0x2U) != 0);
    dut_.control.trigger.t_enable_vec_2.set(
        (config.enable_mask & 0x4U) != 0);
    dut_.control.trigger.t_enable_vec_3.set(
        (config.enable_mask & 0x8U) != 0);
    dut_.control.trigger.trigger_can_raise_bp_exp.set(
        config.trigger_can_raise_breakpoint);
    dut_.control.trigger.debug_mode.set(
        config.debug_mode);
    dut_.control.trigger.t_update_valid.set(
        std::uint64_t{1});
    tick(false);
    dut_.control.trigger.t_update_valid.set(
        std::uint64_t{0});
    return run_cycles(3) && check_components();
}

} // namespace memblock
