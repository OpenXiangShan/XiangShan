#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::make_vector_memory_issue(const VectorMemoryTransaction &transaction)  -> generated::VectorMemoryIssue
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
    std::array<unsigned char, 16> mask_bytes{};
    if (transaction.vm) {
        mask_bytes.fill(0xff);
    } else {
        for (unsigned bit = 0; bit < 128; ++bit) {
            if (vector_mask_enabled(transaction, bit)) {
                mask_bytes[bit / 8] |=
                    static_cast<unsigned char>(1U << (bit % 8));
            }
        }
    }
    issue.vmask = mask_bytes;
    issue.src[3] = mask_bytes;
    issue.src[4][0] = transaction.vl;
    return issue;
}

auto Environment::Impl::memory()  -> SparseMemory &
{ return memory_; }

auto Environment::Impl::bus_expected_load(std::uint64_t address,
    LoadOp op) const -> std::uint64_t
{
    return bus_memory_.expected_load(address, op);
}

auto Environment::Impl::expect_release_line(std::uint64_t base,
    const std::vector<unsigned char> &bytes)  -> void
{
    memory_agent_.expect_release_line(base, bytes);
}

auto Environment::Impl::clear_release_line_expectations()  -> void
{
    memory_agent_.clear_release_line_expectations();
}

auto Environment::Impl::request_dcache_probe(std::uint64_t address,
    std::uint8_t cap,
    bool need_data,
    std::uint8_t expected_report,
    const std::vector<unsigned char> &expected_data)  -> bool
{
    return memory_agent_.request_probe(
        address, cap, need_data, expected_report, expected_data);
}

auto Environment::Impl::hold_dcache_c_until_probes(std::uint64_t target_requests)  -> bool
{
    memory_agent_.hold_c_until_probe_requests(target_requests);
    return check_components();
}

auto Environment::Impl::configure_backpressure(std::uint64_t seed,
    bool enabled,
    ResponseLatencyProfile latency_profile)  -> void
{
    configure_backpressure(
        seed, enabled,
        ResponseLatencyProfiles{
            latency_profile, latency_profile, latency_profile});
}

auto Environment::Impl::configure_backpressure(std::uint64_t seed,
    bool enabled,
    const ResponseLatencyProfiles &latency_profiles)  -> void
{
    memory_agent_.configure_backpressure(
        seed, enabled, latency_profiles.dcache);
    ptw_agent_.configure_backpressure(
        seed ^ 0x9e3779b97f4a7c15ULL, enabled, latency_profiles.ptw);
    uncache_agent_.configure_backpressure(
        seed ^ 0x3c6ef372fe94f82aULL, enabled, latency_profiles.uncache);
}

auto Environment::Impl::configure_dcache_response_order(std::uint64_t seed, unsigned per_mille) -> void
{ memory_agent_.configure_response_order(seed, per_mille); }

auto Environment::Impl::dcache_response_order_stats() const -> const std::array<std::uint64_t, 10> &
{ return memory_agent_.response_order_stats(); }

auto Environment::Impl::configure_ptw_response_order(std::uint64_t seed, unsigned per_mille) -> void
{ ptw_agent_.configure_response_order(seed, per_mille); }

auto Environment::Impl::ptw_response_order_stats() const -> const std::array<std::uint64_t, 8> &
{ return ptw_agent_.response_order_stats(); }

auto Environment::Impl::inject_next_dcache_response_error(bool denied,
    bool corrupt,
    DcacheCorruptBeat corrupt_beat)  -> void
{
    memory_agent_.inject_next_response_error(
        denied, corrupt, corrupt_beat);
}

auto Environment::Impl::inject_dcache_response_error_at(std::uint64_t address,
    bool denied,
    bool corrupt,
    DcacheCorruptBeat corrupt_beat)  -> void
{
    memory_agent_.inject_response_error_at(
        address, denied, corrupt, corrupt_beat);
}

auto Environment::Impl::inject_dcache_cmo_response_error_at(std::uint64_t address,
    bool denied,
    bool corrupt)  -> void
{
    memory_agent_.inject_cmo_response_error_at(
        address, denied, corrupt);
}

auto Environment::Impl::inject_ptw_response_error_after(unsigned clean_requests,
    bool denied,
    bool corrupt,
    PtwCorruptBeat corrupt_beat)  -> void
{
    ptw_agent_.inject_response_error_after(
        clean_requests, denied, corrupt, corrupt_beat);
}

auto Environment::Impl::inject_ptw_response_error_at(std::uint64_t address,
    bool denied,
    bool corrupt,
    PtwCorruptBeat corrupt_beat)  -> void
{
    ptw_agent_.inject_response_error_at(
        address, denied, corrupt, corrupt_beat);
}

auto Environment::Impl::inject_persistent_ptw_response_error_at(std::uint64_t address,
    bool denied,
    bool corrupt,
    PtwCorruptBeat corrupt_beat)  -> void
{
    ptw_agent_.inject_persistent_response_error_at(
        address, denied, corrupt, corrupt_beat);
}

auto Environment::Impl::clear_ptw_response_error()  -> void
{
    ptw_agent_.clear_response_error();
}

auto Environment::Impl::force_next_dcache_response_delay(unsigned cycles)  -> void
{
    memory_agent_.force_next_response_delay(cycles);
}

auto Environment::Impl::force_next_cmo_response_delay(unsigned cycles)  -> void
{
    memory_agent_.force_next_cmo_response_delay(cycles);
}

auto Environment::Impl::force_next_dcache_interbeat_delay(unsigned cycles)  -> void
{
    memory_agent_.force_next_interbeat_delay(cycles);
}

auto Environment::Impl::hold_dcache_response_at(std::uint64_t address)  -> void
{
    memory_agent_.hold_response_at(address);
}

auto Environment::Impl::dcache_line_grants_idle(std::uint64_t address) const -> bool
{
    return memory_agent_.line_grants_idle(address);
}

auto Environment::Impl::dcache_response_held() const -> bool
{
    return memory_agent_.response_held();
}

auto Environment::Impl::dcache_response_held_at(std::uint64_t address) const -> bool
{
    return memory_agent_.response_held_at(address);
}

auto Environment::Impl::dcache_held_response_count() const -> std::size_t
{
    return memory_agent_.held_response_count();
}

auto Environment::Impl::release_held_dcache_response()  -> bool
{
    memory_agent_.release_held_response();
    return check_components();
}

auto Environment::Impl::release_held_dcache_response_at(std::uint64_t address)  -> bool
{
    memory_agent_.release_held_response_at(address);
    return check_components();
}

auto Environment::Impl::schedule_held_dcache_response_release_at(std::uint64_t address,
    unsigned after_ticks)  -> bool
{
    memory_agent_.schedule_held_response_release_at(
        address, after_ticks);
    return check_components();
}

auto Environment::Impl::begin_dcache_release_ready_window(const std::vector<std::uint64_t> &target_lines,
    unsigned stall_valid_cycles)  -> bool
{
    memory_agent_.begin_release_ready_window(
        target_lines, stall_valid_cycles);
    return check_components();
}

auto Environment::Impl::end_dcache_release_ready_window()  -> bool
{
    memory_agent_.end_release_ready_window();
    return check_components();
}

auto Environment::Impl::inject_next_uncache_response_error(bool denied,
    bool corrupt)  -> void
{
    uncache_agent_.inject_next_response_error(denied, corrupt);
}

auto Environment::Impl::force_next_ptw_response_delay(unsigned cycles)  -> void
{
    ptw_agent_.force_next_response_delay(cycles);
}

auto Environment::Impl::force_next_uncache_response_delay(unsigned cycles)  -> void
{
    uncache_agent_.force_next_response_delay(cycles);
}

auto Environment::Impl::configure_uncache_device(std::uint64_t base,
    std::uint64_t size,
    bool read_clear)  -> void
{
    uncache_agent_.configure_device_window(base, size, read_clear);
}

auto Environment::Impl::uncache_device_accesses() const -> const std::vector<UncacheDeviceAccess> &
{
    return uncache_agent_.device_accesses();
}

auto Environment::Impl::configure_cache_error_enable(bool enable)  -> void
{
    dut_.control.csr.cache_error_enable.set(enable);
}

auto Environment::Impl::configure_ldld_violation_check(bool enable)  -> void
{
    dut_.control.csr.ldld_vio_check_enable.set(enable);
}

auto Environment::Impl::configure_stride_prefetch(bool enable)  -> bool
{
    dut_.control.prefetch.l2_pf_enable.set(enable);
    dut_.control.prefetch.l1_d_pf_enable.set(enable);
    dut_.control.prefetch.l1_d_pf_train_on_hit.set(
        std::uint64_t{0});
    dut_.control.prefetch.l1_d_pf_enable_agt.set(
        std::uint64_t{0});
    dut_.control.prefetch.l1_d_pf_enable_pht.set(
        std::uint64_t{0});
    dut_.control.prefetch.l1_d_pf_active_threshold.set(
        std::uint64_t{12});
    dut_.control.prefetch.l1_d_pf_active_stride.set(
        std::uint64_t{30});
    dut_.control.prefetch.l1_d_pf_enable_stride.set(enable);
    dut_.control.prefetch.l2_pf_store_only.set(
        std::uint64_t{0});
    dut_.control.prefetch.l2_pf_recv_enable.set(
        enable);
    dut_.control.prefetch.l2_pf_pbop_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l2_pf_vbop_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l2_pf_tp_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l2_pf_delay_latency.set(
        std::uint64_t{0});
    return run_cycles(4);
}

auto Environment::Impl::configure_sms_pht_prefetch(bool output_enable)  -> bool
{
    dut_.control.prefetch.l2_pf_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l1_d_pf_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l1_d_pf_train_on_hit.set(
        std::uint64_t{0});
    dut_.control.prefetch.l1_d_pf_enable_agt.set(
        std::uint64_t{0});
    dut_.control.prefetch.l1_d_pf_enable_pht.set(
        output_enable);
    dut_.control.prefetch.l1_d_pf_active_threshold.set(
        std::uint64_t{12});
    dut_.control.prefetch.l1_d_pf_active_stride.set(
        std::uint64_t{30});
    dut_.control.prefetch.l1_d_pf_enable_stride.set(
        std::uint64_t{0});
    dut_.control.prefetch.l2_pf_store_only.set(
        std::uint64_t{0});
    dut_.control.prefetch.l2_pf_recv_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l2_pf_pbop_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l2_pf_vbop_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l2_pf_tp_enable.set(
        std::uint64_t{1});
    dut_.control.prefetch.l2_pf_delay_latency.set(
        std::uint64_t{0});
    return run_cycles(4);
}

auto Environment::Impl::expect_l2_prefetch_control(bool master_enabled,
    bool receive_enabled)  -> bool
{
    dut_.refresh_comb();
    const bool matches =
        dut_.control.l2_prefetch_output.l2_pf_master_en.boolean() == master_enabled &&
        dut_.control.l2_prefetch_output.l2_pf_recv_en.boolean() == receive_enabled &&
        dut_.control.l2_prefetch_output.l2_pbop_en.boolean() &&
        dut_.control.l2_prefetch_output.l2_vbop_en.boolean() &&
        dut_.control.l2_prefetch_output.l2_tp_en.boolean() &&
        dut_.control.l2_prefetch_output.l2_pf_delay_latency.value() == 0;
    if (!matches) {
        std::ostringstream message;
        message << "L2 prefetch control output mismatch master="
                << dut_.control.l2_prefetch_output.l2_pf_master_en.boolean()
                << " recv="
                << dut_.control.l2_prefetch_output.l2_pf_recv_en.boolean()
                << " pbop=" << dut_.control.l2_prefetch_output.l2_pbop_en.boolean()
                << " vbop=" << dut_.control.l2_prefetch_output.l2_vbop_en.boolean()
                << " tp=" << dut_.control.l2_prefetch_output.l2_tp_en.boolean()
                << " delay="
                << dut_.control.l2_prefetch_output.l2_pf_delay_latency.value();
        error_.record(message.str());
        return false;
    }
    return check_components();
}

} // namespace memblock
