#include "implementation.hpp"

namespace memblock {

namespace {

void trace_reference_store(
    const SparseMemory &memory, const char *kind, std::uint64_t address,
    unsigned bytes, std::uint64_t data, unsigned rob, bool rob_flag,
    unsigned sq, bool sq_flag, const char *extra)
{
    if (!debug_trace::enabled_for(address, bytes)) {
        return;
    }
    std::uint8_t window[16]{};
    for (unsigned index = 0; index < sizeof(window); ++index) {
        window[index] = memory.read_byte(address + index);
    }
    const std::uint64_t line_base = address & ~std::uint64_t{63};
    std::uint8_t line[64]{};
    for (unsigned index = 0; index < sizeof(line); ++index) {
        line[index] = memory.read_byte(line_base + index);
    }
    std::ostringstream text;
    text << kind << " addr=0x" << std::hex << address
         << " bytes=" << std::dec << bytes
         << " data=0x" << std::hex << data
         << " rob=" << std::dec << rob << ':' << rob_flag
         << " sq=" << sq << ':' << sq_flag
         << " window=" << debug_trace::hex_bytes(window, sizeof(window))
         << " line=0x" << std::hex << line_base << ':'
         << debug_trace::hex_bytes(line, sizeof(line));
    if (extra != nullptr && extra[0] != '\0') {
        text << ' ' << extra;
    }
    debug_trace::emit(text.str());
}

} // namespace

auto Environment::Impl::run_until_store_complete(unsigned timeout)  -> bool
{
    for (unsigned cycle = 0; cycle < timeout && !store_scoreboard_.done(); ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (!store_scoreboard_.done()) {
        error_.record("timed out waiting for scalar store writebacks");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_complete(unsigned timeout)  -> bool
{
    for (unsigned cycle = 0; cycle < timeout && !scoreboard_.done(); ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (!scoreboard_.done()) {
        error_.record("timed out waiting for scalar load writeback");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_load_complete(const LoadTransaction &transaction,
    unsigned timeout)  -> bool
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
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_load_writebacks(std::uint64_t target,
    unsigned timeout)  -> bool
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
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_vector_complete(unsigned timeout)  -> bool
{
    for (unsigned cycle = 0;
         cycle < timeout && !vector_scoreboard_.done(); ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (!vector_scoreboard_.done()) {
        error_.record("timed out waiting for vector memory writeback" +
            vector_scoreboard_.pending_summary());
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_vector_complete_with_replays(const VectorMemoryTransaction &transaction,
    unsigned timeout,
    bool pulse_store_commit_after_replay,
    bool hold_pending_store)  -> bool
{
    // A store at the ROB head keeps pendingst asserted while a replaying
    // misaligned operation is still incomplete. Preserve that level so a
    // delayed replay sees the same architectural head throughout.
    if (hold_pending_store) {
        dut_.queues.backend.pending_ptr_value.set(transaction.rob);
        dut_.queues.backend.pending_ptr_flag.set(transaction.rob_flag);
        dut_.queues.backend.pendingst.set(std::uint64_t{1});
    }
    const auto clear_pending_store = [&]() {
        if (hold_pending_store) {
            dut_.queues.backend.pendingst.set(std::uint64_t{0});
        }
    };
    const std::uint64_t deadline = cycle() + timeout;
    while (!vector_scoreboard_.done() && cycle() < deadline) {
        const auto pending = feedback_.take_replay_for(transaction);
        if (!pending) {
            tick();
            if (!check_components()) {
                clear_pending_store();
                return false;
            }
            continue;
        }
        const VectorReplayRequest request = *pending;
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
                << " replay_feedbacks=" << feedback_.vector_replay_feedbacks()
                << " pending_replays=" << feedback_.pending_replays()
                << vector_scoreboard_.pending_summary();
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_vector_complete_with_replays(const std::vector<VectorMemoryTransaction> &transactions,
    unsigned timeout,
    bool pulse_store_commit_after_replay)  -> bool
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
        if (feedback_.pending_replays() == 0) {
            tick();
            if (!check_components()) {
                return false;
            }
            continue;
        }
        const VectorReplayRequest request = *feedback_.take_next_replay();
        const auto transaction = std::find_if(
            transactions.begin(), transactions.end(),
            [&](const VectorMemoryTransaction &candidate) {
                return matches(candidate, request);
            });
        if (transaction == transactions.end()) {
            error_.record("vector replay did not match a mixed-window transaction");
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
                << " pending_replays=" << feedback_.pending_replays()
                << " ptw_requests=" << ptw_agent_.request_count()
                << vector_scoreboard_.pending_summary();
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_all_complete(unsigned timeout)  -> bool
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
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_lq_retired(unsigned timeout)  -> bool
{
    for (unsigned cycle = 0;
         cycle < timeout && queues_.lq_dequeued() + queues_.lq_canceled() < queues_.lq_allocated();
         ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (queues_.lq_dequeued() + queues_.lq_canceled() < queues_.lq_allocated()) {
        std::ostringstream message;
        message << "timed out waiting for LQ retirement allocated="
                << queues_.lq_allocated() << " dequeued=" << queues_.lq_dequeued()
                << " canceled=" << queues_.lq_canceled();
        error_.record(message.str());
        return false;
    }
    return true;
}

auto Environment::Impl::account_lq_cancellation(unsigned count)  -> bool
{
    return queues_.account_lq_cancellation(count);
}

auto Environment::Impl::account_sq_cancellation(unsigned count)  -> bool
{
    return queues_.account_sq_cancellation(count);
}

auto Environment::Impl::run_until_queues_retired(unsigned timeout)  -> bool
{
    for (unsigned cycle = 0;
         cycle < timeout &&
         (queues_.lq_dequeued() + queues_.lq_canceled() < queues_.lq_allocated() ||
          queues_.sq_dequeued() + queues_.sq_canceled() < queues_.sq_allocated());
         ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (queues_.lq_dequeued() + queues_.lq_canceled() < queues_.lq_allocated() ||
        queues_.sq_dequeued() + queues_.sq_canceled() < queues_.sq_allocated()) {
        std::ostringstream message;
        message << "timed out waiting for mixed LSQ retirement"
                << " lq=" << queues_.lq_dequeued() << '+' << queues_.lq_canceled()
                << '/' << queues_.lq_allocated()
                << " sq=" << queues_.sq_dequeued() << '+' << queues_.sq_canceled()
                << '/' << queues_.sq_allocated();
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::commit_store(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
{
    const auto target_it = queues_.scalar_store_target(
        scalar_store_key(transaction));
    if (!target_it) {
        error_.record("scalar store has no recorded SQ allocation target");
        return false;
    }
    const std::uint64_t target = *target_it;
    // A misaligned store at the ROB head can leave the SQ while another
    // outstanding class is still draining.  Do not turn that completed
    // store into a wait for the following SQ entry.
    // CMO may leave the externally monitored SQ before architectural
    // commit, but StoreQueue's shared CMO/MMIO state machine still needs
    // the backend scommit pulse to leave s_wait and accept another CMO.
    if ((is_cmo(transaction.op) || queues_.sq_dequeued() < target) &&
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
                << " sq_counts=" << queues_.sq_dequeued() << '/' << queues_.sq_allocated();
        error_.record(message.str());
        return false;
    }
    queues_.forget_scalar_store(scalar_store_key(transaction));
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
    trace_reference_store(
        memory_, "store-commit", address, bytes, transaction.data,
        transaction.rob, transaction.rob_flag, transaction.sq,
        transaction.sq_flag, "");
    return true;
}

auto Environment::Impl::commit_vector_store(const VectorMemoryTransaction &transaction,
    unsigned timeout)  -> bool
{
    if (!transaction.store) {
        error_.record("cannot commit a vector load as a store");
        return false;
    }
    const auto target_it = queues_.vector_store_target(
        vector_store_key(transaction));
    if (!target_it) {
        error_.record("vector store has no recorded SQ allocation target");
        return false;
    }
    const std::uint64_t target = *target_it;
    StoreTransaction commit_point{
        .rob = transaction.rob,
        .rob_flag = transaction.rob_flag,
    };
    if (queues_.sq_dequeued() < target &&
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
                << " sq_counts=" << queues_.sq_dequeued() << '/' << queues_.sq_allocated();
        error_.record(message.str());
        return false;
    }
    queues_.forget_vector_store(vector_store_key(transaction));
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
        std::ostringstream extra;
        extra << "vector=true eew=" << vector_data_eew(transaction)
              << " element=" << element
              << " active=" << std::hex << active;
        std::uint64_t element_data = 0;
        for (unsigned byte = 0; byte < element_bytes; ++byte) {
            element_data |= std::uint64_t{
                transaction.data[element * element_bytes + byte]}
                << (8 * byte);
        }
        trace_reference_store(
            memory_, "vstore-commit", element_address, element_bytes,
            element_data, transaction.rob, transaction.rob_flag,
            transaction.sq, transaction.sq_flag, extra.str().c_str());
    }
    return true;
}

auto Environment::Impl::record_committed_store(const StoreTransaction &transaction)  -> void
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
    trace_reference_store(
        memory_, "store-record", address, bytes, transaction.data,
        transaction.rob, transaction.rob_flag, transaction.sq,
        transaction.sq_flag, "");
}

auto Environment::Impl::record_atomic_result(std::uint64_t address,
    std::uint64_t value)  -> void
{
    for (unsigned byte = 0; byte < 8; ++byte) {
        memory_.write_reference_byte(
            address + byte,
            static_cast<std::uint8_t>(value >> (8 * byte)));
    }
    trace_reference_store(
        memory_, "atomic-result", address, 8, value, 0, false, 0, false,
        "");
}

auto Environment::Impl::record_committed_vector_store(const VectorMemoryTransaction &transaction)  -> void
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
        std::uint64_t element_data = 0;
        for (unsigned byte = 0; byte < element_bytes; ++byte) {
            element_data |= std::uint64_t{
                transaction.data[element * element_bytes + byte]}
                << (8 * byte);
        }
        trace_reference_store(
            memory_, "vstore-record", address, element_bytes, element_data,
            transaction.rob, transaction.rob_flag, transaction.sq,
            transaction.sq_flag, "");
    }
}

auto Environment::Impl::commit_stores_through(const StoreTransaction &transaction,
    unsigned count)  -> bool
{
    if (count == 0 || count > 8) {
        error_.record("store commit count must be between one and eight");
        return false;
    }
    dut_.queues.backend.pending_ptr_flag.set(transaction.rob_flag);
    dut_.queues.backend.pending_ptr_value.set(transaction.rob);
    dut_.queues.backend.scommit.set(count);
    tick();
    dut_.queues.backend.scommit.set(std::uint64_t{0});
    return check_components();
}

auto Environment::Impl::run_until_sq_dequeued(std::uint64_t target,
    unsigned timeout)  -> bool
{
    for (unsigned cycle = 0; cycle < timeout && queues_.sq_dequeued() < target; ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (queues_.sq_dequeued() < target) {
        error_.record("timed out waiting for committed store to leave SQ");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_release_data(unsigned timeout)  -> bool
{
    for (unsigned cycle = 0;
         cycle < timeout && memory_agent_.release_data_count() == 0; ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (memory_agent_.release_data_count() == 0) {
        error_.record("timed out waiting for DCache ReleaseData");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_release_data_count(std::uint64_t target,
    unsigned timeout)  -> bool
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
        error_.record("timed out waiting for target DCache ReleaseData count");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_expected_release_data_count(std::uint64_t target,
    unsigned timeout)  -> bool
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
        error_.record("timed out waiting for expected DCache ReleaseData count");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_probe_responses(std::uint64_t target,
    unsigned timeout)  -> bool
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
        error_.record("timed out waiting for DCache ProbeAck response");
        return false;
    }
    return check_components();
}

auto Environment::Impl::check_idle(unsigned cycles)  -> bool
{
    for (unsigned cycle = 0; cycle < cycles; ++cycle) {
        tick(false);
        if (!dut_.expect_quiescent_outputs()) {
            error_.record("non-quiescent output during idle smoke interval");
            return false;
        }
    }
    return check_components();
}

auto Environment::Impl::run_cycles(unsigned cycles)  -> bool
{
    for (unsigned cycle = 0; cycle < cycles; ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    return true;
}

auto Environment::Impl::redirect_after(std::uint8_t rob_value,
    bool rob_flag,
    bool flush_itself,
    bool is_vls_exception)  -> bool
{
    if (is_vls_exception && !flush_itself) {
        error_.record("VLS exception redirect must enter as a flush redirect");
        return false;
    }
    dut_.queues.redirect.rob_idx_flag.set(rob_flag);
    dut_.queues.redirect.rob_idx_value.set(rob_value);
    dut_.queues.redirect.level.set(flush_itself);
    dut_.queues.redirect.is_vls_exception.set(is_vls_exception);
    dut_.queues.redirect.valid.set(std::uint64_t{1});
    tick();
    dut_.queues.redirect.valid.set(std::uint64_t{0});
    dut_.queues.redirect.is_vls_exception.set(std::uint64_t{0});
    // MemBlock registers redirect once; the queues publish their retained
    // redirectCancelCount values after two more clock edges. Sample once
    // per known redirect instead of treating the retained value as a pulse.
    tick();
    tick();
    dut_.refresh_comb();
    const std::uint64_t lq_canceled =
        dut_.queues.retirement.lq_cancel_cnt.value();
    const std::uint64_t sq_canceled =
        dut_.queues.retirement.sq_cancel_cnt.value();
    if (!queues_.observe_redirect_cancellation(lq_canceled, sq_canceled)) return false;
    return check_components();
}

auto Environment::Impl::ok() const -> bool
{ return error_.empty(); }

auto Environment::Impl::error() const -> const std::string &
{ return error_.message(); }

} // namespace memblock
