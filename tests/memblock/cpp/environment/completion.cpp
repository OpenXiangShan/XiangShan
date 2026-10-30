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

auto Environment::Impl::service_vector_replay(const VectorMemoryTransaction &transaction,
    bool &replayed, unsigned timeout) -> bool
{
    replayed = false;
    if (retained_issue_driver_) {
        error_.record("legacy service_vector_replay conflicts with retained producer pin ownership");
        return false;
    }
    const auto request = feedback_.take_replay_for(transaction);
    if (!request) return check_components();
    if (!vector_scoreboard_.pending(transaction)) {
        error_.record("vector replay matched an already completed stream uop");
        return false;
    }
    auto replay = transaction;
    replay.lane = request->lane;
    replay.is_part_replay = request->is_part_replay;
    replay.replay_mask = request->replay_mask;
    replay.replay_mb_index = request->replay_mb_index;
    if (!issue_vector(replay, timeout)) return false;
    replayed = true;
    return true;
}

auto Environment::Impl::run_until_vector_complete_with_replays(const VectorMemoryTransaction &transaction,
    unsigned timeout,
    bool pulse_pending_store_after_replay,
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
    while (vector_scoreboard_.pending(transaction) && cycle() < deadline) {
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
        if (pulse_pending_store_after_replay && replay.store &&
            (!run_cycles(32) ||
             !(hold_pending_store
                   ? run_cycles(1)
                   : pulse_pending_store(replay.rob, replay.rob_flag)))) {
            clear_pending_store();
            return false;
        }
    }
    clear_pending_store();
    if (vector_scoreboard_.pending(transaction)) {
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
    bool pulse_pending_store_after_replay) -> bool
{
    const auto pending = [&]() {
        return std::any_of(transactions.begin(), transactions.end(),
            [&](const auto &transaction) { return vector_scoreboard_.pending(transaction); });
    };
    const std::uint64_t deadline = cycle() + timeout;
    while (pending() && cycle() < deadline) {
        std::optional<VectorReplayRequest> request;
        const VectorMemoryTransaction *selected = nullptr;
        for (const auto &transaction : transactions) {
            request = feedback_.take_replay_for(transaction);
            if (request) {
                selected = &transaction;
                break;
            }
        }
        if (!request) {
            tick();
            if (!check_components()) return false;
            continue;
        }
        if (!vector_scoreboard_.pending(*selected)) {
            error_.record("vector replay matched an already completed uop");
            return false;
        }
        auto replay = *selected;
        replay.lane = request->lane;
        replay.is_part_replay = request->is_part_replay;
        replay.replay_mask = request->replay_mask;
        replay.replay_mb_index = request->replay_mb_index;
        if (!issue_vector(replay, 256)) return false;
        if (pulse_pending_store_after_replay && replay.store &&
            (!run_cycles(32) || !pulse_pending_store(replay.rob, replay.rob_flag))) return false;
    }
    if (pending()) {
        std::ostringstream message;
        message << "timed out waiting for selected vector replays"
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
        queues_.sq_dequeued() + queues_.sq_canceled() < queues_.sq_allocated() ||
        queues_.store_retirement_stats().pending != 0) {
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

auto Environment::Impl::set_final_observation_tracking(bool enabled)  -> void
{
    final_observation_tracking_ = enabled;
}

auto Environment::Impl::final_observation_words() const
    -> std::vector<std::uint64_t>
{
    return {final_observation_words_.begin(), final_observation_words_.end()};
}

auto Environment::Impl::final_observation_nc_words() const
    -> std::vector<std::uint64_t>
{
    return {
        final_observation_nc_words_.begin(),
        final_observation_nc_words_.end()};
}

auto Environment::Impl::final_observation_bytes() const -> std::uint64_t
{
    return final_observation_bytes_;
}

auto Environment::Impl::final_observation_nc_bytes() const -> std::uint64_t
{
    return final_observation_nc_bytes_;
}

auto Environment::Impl::verify_final_nc_effects()  -> bool
{
    for (const std::uint64_t word : final_observation_nc_words_) {
        const std::uint64_t expected = memory_.read_u64(word);
        const std::uint64_t actual = bus_memory_.read_u64(word);
        if (expected == actual) {
            continue;
        }
        std::ostringstream message;
        message << "non-cacheable final effect mismatch address=0x"
                << std::hex << word << " expected=0x" << expected
                << " actual=0x" << actual << std::dec;
        error_.record(message.str());
        return false;
    }
    return true;
}

auto Environment::Impl::final_observation_alias_skips() const -> std::uint64_t
{
    return final_observation_alias_skips_;
}

auto Environment::Impl::final_observation_observed_words() const
    -> std::uint64_t
{
    return final_observation_observed_words_;
}

auto Environment::Impl::set_final_observation_observed_words(
    std::uint64_t words)  -> void
{
    final_observation_observed_words_ = words;
}

auto Environment::Impl::add_final_observation_observed_words(
    std::uint64_t words)  -> void
{
    final_observation_observed_words_ += words;
}

auto Environment::Impl::discard_final_observation_range(
    std::uint64_t address, unsigned bytes)  -> void
{
    if (bytes == 0) {
        return;
    }
    const std::uint64_t first = address & ~std::uint64_t{7};
    const std::uint64_t last = (address + bytes - 1U) & ~std::uint64_t{7};
    for (std::uint64_t word = first; word <= last; word += 8U) {
        final_observation_words_.erase(word);
        final_observation_nc_words_.erase(word);
    }
}

auto Environment::Impl::track_final_observation_bytes(
    std::uint64_t address, unsigned bytes, bool cacheable,
    bool identity_mapped)  -> void
{
    if (!final_observation_tracking_ || bytes == 0) {
        return;
    }
    if (!cacheable) {
        const std::uint64_t first = address & ~std::uint64_t{7};
        const std::uint64_t last = (address + bytes - 1U) &
            ~std::uint64_t{7};
        for (std::uint64_t word = first; word <= last; word += 8U) {
            final_observation_nc_words_.insert(word);
        }
        final_observation_nc_bytes_ += bytes;
        return;
    }
    if (!identity_mapped) {
        ++final_observation_alias_skips_;
        return;
    }
    const std::uint64_t first = address & ~std::uint64_t{7};
    const std::uint64_t last = (address + bytes - 1U) & ~std::uint64_t{7};
    for (std::uint64_t word = first; word <= last; word += 8U) {
        final_observation_words_.insert(word);
    }
    final_observation_bytes_ += bytes;
}

auto Environment::Impl::track_store_release_inputs(const StoreTransaction &transaction) -> bool
{
    const auto key = scalar_store_key(transaction);
    const auto target = queues_.scalar_store_target(key);
    if (!target || !transaction.cacheable || static_cast<unsigned>(transaction.op) > 3U ||
        transaction.expected_exception_mask || transaction.input_exception_mask ||
        transaction.input_flush_pipe || transaction.input_trigger != kTriggerNone ||
        transaction.expected_output_flush_pipe.value_or(false) ||
        transaction.expected_debug_is_mmio.value_or(false) ||
        transaction.expected_debug_is_ncio.value_or(false) || scalar_release_inputs_.contains(key)) {
        error_.record("release input tracking requires a newly allocated normal cacheable scalar store");
        return false;
    }
    ReleaseStoreInputs inputs{transaction.rob, transaction.rob_flag};
    const unsigned width = 1U << static_cast<unsigned>(transaction.op);
    // Aligned scalar writes are indivisible. Split misalignment conservatively
    // into byte groups; this value oracle makes no early-effect timing claim.
    const bool aligned = (transaction.address & (width - 1U)) == 0;
    for (unsigned offset = 0; offset < width; offset += aligned ? width : 1U) {
        ReleaseInputGroup group{*target * 1024U + offset,
            reference_byte_address(transaction, transaction.address + offset), transaction.address + offset, {}};
        for (unsigned b = 0; b < (aligned ? width : 1U); ++b)
            group.bytes.push_back(transaction.data >> (8U * (offset + b)));
        inputs.groups.push_back(std::move(group));
    }
    scalar_release_inputs_.emplace(key, std::move(inputs));
    return true;
}

auto Environment::Impl::track_vector_release_inputs(const VectorMemoryTransaction &transaction) -> bool
{
    const auto key = vector_store_key(transaction);
    const auto target = queues_.vector_store_target(key);
    if (!target || !transaction.store || transaction.segment || !transaction.cacheable ||
        transaction.expected_exception_mask || transaction.allowed_additional_exception_mask ||
        transaction.input_exception_mask || transaction.input_flush_pipe ||
        transaction.input_trigger != kTriggerNone || transaction.exception_may_defer ||
        !transaction.check_data || transaction.expected_debug_is_mmio.value_or(false) ||
        transaction.expected_debug_is_ncio.value_or(false) || vector_release_inputs_.contains(key)) {
        error_.record("release input tracking requires a newly allocated normal cacheable vector store");
        return false;
    }
    ReleaseStoreInputs inputs{transaction.rob, transaction.rob_flag};
    const unsigned width = 1U << vector_data_eew(transaction);
    const auto active = active_vector_elements(transaction);
    for (unsigned element = 0; element < 16U / width; ++element) {
        if (!((active >> element) & 1U)) continue;
        const auto address = vector_element_virtual_address(transaction, element);
        const bool aligned = (address & (width - 1U)) == 0;
        for (unsigned offset = 0; offset < width; offset += aligned ? width : 1U) {
            ReleaseInputGroup group{*target * 1024U + element * 32U + offset,
                vector_byte_address(transaction, element, offset), address + offset, {}};
            for (unsigned b = 0; b < (aligned ? width : 1U); ++b)
                group.bytes.push_back(transaction.data[element * width + offset + b]);
            inputs.groups.push_back(std::move(group));
        }
    }
    vector_release_inputs_.emplace(key, std::move(inputs));
    return true;
}

auto Environment::Impl::accept_release_store_input(std::uint64_t key, bool vector, bool address) -> bool
{
    auto &tracked = vector ? vector_release_inputs_ : scalar_release_inputs_;
    const auto found = tracked.find(key);
    if (found == tracked.end()) return true; // Legacy and exceptional callers.
    auto &inputs = found->second;
    if (vector) inputs.address = inputs.data = true;
    else (address ? inputs.address : inputs.data) = true;
    if (inputs.address && inputs.data && !inputs.registered) {
        for (const auto &group : inputs.groups)
            if (!memory_agent_.register_release_store(group.order, group.physical,
                group.virtual_address, group.bytes)) {
                error_.merge(memory_agent_.failures());
                return false;
            }
        inputs.registered = true;
    }
    return true;
}

auto Environment::Impl::authorize_release_store_inputs() -> bool
{
    for (auto *tracked : {&scalar_release_inputs_, &vector_release_inputs_})
        for (auto &[key, inputs] : *tracked) {
            if (!inputs.registered || inputs.authorized ||
                dut_.queues.backend.pending_ptr_value.value() != inputs.rob ||
                dut_.queues.backend.pending_ptr_flag.value() != inputs.flag) continue;
            // Normal cacheable SQ effects use the public oldest identity.
            // pendingst controls special-store/ROB modes and is not a further
            // normal scalar/vector SQ eligibility condition.
            for (const auto &group : inputs.groups)
                if (!memory_agent_.authorize_release_store(group.order)) {
                    error_.merge(memory_agent_.failures());
                    return false;
                }
            inputs.authorized = true;
        }
    return true;
}

auto Environment::Impl::retire_store(const StoreTransaction &transaction, bool restore_after_walk) -> bool
{
    const auto target = queues_.scalar_store_target(scalar_store_key(transaction));
    if (static_cast<unsigned>(transaction.op) > static_cast<unsigned>(StoreOp::sd) ||
        !transaction.cacheable || transaction.expected_exception_mask ||
        transaction.input_exception_mask || transaction.input_flush_pipe ||
        transaction.input_trigger != kTriggerNone ||
        transaction.expected_output_flush_pipe.value_or(false) ||
        transaction.expected_debug_is_mmio.value_or(false) ||
        transaction.expected_debug_is_ncio.value_or(false) ||
        store_scoreboard_.pending(transaction) ||
        dut_.queues.backend.pending_ptr_value.value() != transaction.rob ||
        dut_.queues.backend.pending_ptr_flag.value() != transaction.rob_flag ||
        (!restore_after_walk && !dut_.queues.backend.pendingst.boolean()) ||
        !target || !queues_.valid_store_retirement_target(*target)) {
        error_.record("scalar architectural retirement requires a completed normal cacheable store at the held head");
        return false;
    }
    if (restore_after_walk) {
        if (dut_.queues.backend.pendingst.boolean() || dut_.queues.backend.scommit.boolean() ||
            dut_.queues.backend.pending_mmi_old.boolean() || !ordinary_walk_resume_cycle_ ||
            cycle() < *ordinary_walk_resume_cycle_) {
            error_.record("store walk restoration requires recovered low pending controls");
            return false;
        }
        // First unblocked registered store-head tuple also carries scommit
        // when H is already complete. Do not expose an extra high/zero edge.
        dut_.queues.backend.pendingst.set(std::uint64_t{1});
    }
    // ROB emits exactly one scalar scommit even if this ordinary SQ entry
    // already drained while it was the head. Dequeue is not the commit event.
    if (!commit_stores_through(transaction, 1) ||
        !queues_.retire_scalar_store(scalar_store_key(transaction))) return false;
    record_committed_store(transaction);
    scalar_release_inputs_.erase(scalar_store_key(transaction));
    ordinary_walk_resume_cycle_.reset();
    return check_components();
}

auto Environment::Impl::retire_vector_stores(
    const std::vector<VectorMemoryTransaction> &transactions) -> bool
{
    if (transactions.empty()) {
        error_.record("vector architectural retirement requires a complete instruction");
        return false;
    }
    const auto &first = transactions.front();
    std::set<std::uint64_t> keys;
    unsigned last_uops = 0;
    std::uint64_t previous_target = 0;
    for (const auto &transaction : transactions) {
        const auto key = vector_store_key(transaction);
        const auto target = queues_.vector_store_target(key);
        if (!transaction.store || transaction.segment || !transaction.cacheable ||
            transaction.expected_exception_mask || transaction.allowed_additional_exception_mask ||
            transaction.input_exception_mask || transaction.input_flush_pipe ||
            transaction.input_trigger != kTriggerNone || !transaction.check_data ||
            transaction.expected_debug_is_mmio.value_or(false) ||
            transaction.expected_debug_is_ncio.value_or(false) ||
            transaction.exception_may_defer || vector_scoreboard_.pending(transaction) ||
            transaction.rob != first.rob || transaction.rob_flag != first.rob_flag ||
            !keys.insert(key).second || !target || *target <= previous_target ||
            !queues_.valid_store_retirement_target(*target)) {
            error_.record("vector architectural retirement requires completed normal same-instruction uops in SQ order");
            return false;
        }
        previous_target = *target;
        last_uops += transaction.last_uop;
    }
    const bool restart_flush = std::any_of(transactions.begin(), transactions.end(),
        [](const auto &transaction) { return transaction.vstart != 0; });
    if (restart_flush && (!scalar_redirect_boundary_ ||
        scalar_redirect_boundary_->rob != first.rob || scalar_redirect_boundary_->flag != first.rob_flag ||
        scalar_redirect_boundary_->flush_itself || scalar_redirect_boundary_->remaining != 0 ||
        previous_target > scalar_redirect_boundary_->sq_allocation_limit || cycle() < scalar_redirect_boundary_->cycle + 6U)) {
        error_.record("nonzero-VSTART vector retirement requires a recovered same-instruction flush-after");
        return false;
    }
    if (last_uops != 1 || !transactions.back().last_uop ||
        !queues_.complete_vector_store_group(keys) ||
        dut_.queues.backend.pending_ptr_value.value() != first.rob ||
        dut_.queues.backend.pending_ptr_flag.value() != first.rob_flag ||
        !dut_.queues.backend.pendingst.boolean()) {
        error_.record("vector architectural retirement lacks the complete last-uop head");
        return false;
    }
    // Preserve the old head for its retirement edge. Vector ROB retirement
    // emits no scalar scommit; the SQ uses checked merge-buffer completion.
    tick();
    if (!check_components()) return false;
    for (const auto &transaction : transactions) {
        if (!queues_.retire_vector_store(vector_store_key(transaction))) return false;
        record_committed_vector_store(transaction);
        vector_release_inputs_.erase(vector_store_key(transaction));
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
    const bool deferred_cbo_zero = is_cbo_zero(transaction.op);
    // A cacheable CBO.ZERO reaches cboZeroStout only after its SBuffer line
    // drains.  ROB therefore cannot retire it before the SQ completion.
    if (deferred_cbo_zero &&
        (!run_until_sq_dequeued(target, timeout) ||
         !commit_stores_through(transaction, 1))) {
        return false;
    }
    // A misaligned store at the ROB head can leave the SQ while another
    // outstanding class is still draining.  Do not turn that completed
    // store into a wait for the following SQ entry.  Other CMO/MMIO state
    // machines still need the backend scommit pulse before their final wait.
    if (!deferred_cbo_zero &&
        (is_cmo(transaction.op) || queues_.sq_dequeued() < target) &&
        !commit_stores_through(transaction, 1)) {
        return false;
    }
    if (!deferred_cbo_zero && !run_until_sq_dequeued(target, timeout)) {
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
        // Only CBO.INVAL may discard dirty data.  Successful CLEAN/FLUSH
        // must retain observable final bytes, so keep those lines tracked.
        if (transaction.op == StoreOp::cbo_inval) {
            const std::uint64_t cmo_line =
                reference_byte_address(transaction, transaction.address) &
                ~std::uint64_t{63};
            discard_final_observation_range(cmo_line, 64U);
        }
        return true;
    }
    const std::uint64_t raw_address =
        reference_byte_address(transaction, transaction.address);
    const std::uint64_t address = is_cbo_zero(transaction.op)
        ? raw_address & ~std::uint64_t{63}
        : raw_address;
    // CBO.ZERO is encoded as 0x7 but architecturally covers one cache
    // line, not a 128-byte scalar transfer.
    const unsigned bytes = scalar_store_bytes(transaction.op);
    for (unsigned byte = 0; byte < bytes; ++byte) {
        const auto physical = reference_byte_address(transaction,
            (is_cbo_zero(transaction.op)
                ? transaction.address & ~std::uint64_t{63}
                : transaction.address) + byte);
        memory_.write_reference_byte(physical,
            is_cbo_zero(transaction.op)
                ? 0
                : static_cast<std::uint8_t>(transaction.data >> (8 * byte)));
        if (transaction.reference_pages) {
            if (bytes <= 8U && (transaction.address & (bytes - 1U)) == 0) {
                if (byte == 0) {
                    std::vector<unsigned char> update(bytes);
                    for (unsigned b = 0; b < bytes; ++b) update[b] = transaction.data >> (8U * b);
                    memory_agent_.note_reference_store(physical, transaction.address, update);
                }
            } else {
                memory_agent_.note_reference_write(physical, transaction.address + byte,
                    memory_.read_byte(physical));
            }
            track_final_observation_bytes(physical, 1U, transaction.cacheable, true);
        }
    }
    if (!transaction.reference_pages) {
        track_final_observation_bytes(address, bytes, transaction.cacheable,
            !transaction.oracle_address ||
                *transaction.oracle_address == transaction.address);
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
    // ROB's scalar scommit excludes vector stores. A vector SQ flow becomes
    // eligible from the architectural head and its own merge-buffer completion.
    // The caller must have retired every older instruction before this point.
    if (queues_.sq_dequeued() < target &&
        !set_rob_head(transaction.rob, transaction.rob_flag)) {
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
                vector_byte_address(transaction, element, byte),
                transaction.data[element * element_bytes + byte]);
            if (transaction.reference_pages) {
                const auto virtual_address = vector_element_virtual_address(transaction, element);
                if ((virtual_address & (element_bytes - 1U)) == 0) {
                    if (byte == 0) memory_agent_.note_reference_store(
                        vector_byte_address(transaction, element, 0), virtual_address,
                        std::vector<unsigned char>(transaction.data.begin() + element * element_bytes,
                            transaction.data.begin() + (element + 1U) * element_bytes));
                } else {
                    memory_agent_.note_reference_write(
                        vector_byte_address(transaction, element, byte), virtual_address + byte,
                        transaction.data[element * element_bytes + byte]);
                }
            }
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
        if (transaction.reference_pages) {
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                track_final_observation_bytes(
                    vector_byte_address(transaction, element, byte), 1U,
                    transaction.cacheable, true);
            }
        } else {
            track_final_observation_bytes(
                vector_element_address(transaction, element), element_bytes,
                transaction.cacheable, !transaction.oracle_address ||
                    *transaction.oracle_address == transaction.address);
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
        if (transaction.op == StoreOp::cbo_inval) {
            const std::uint64_t cmo_line =
                reference_byte_address(transaction, transaction.address) &
                ~std::uint64_t{63};
            discard_final_observation_range(cmo_line, 64U);
        }
        return;
    }
    const std::uint64_t raw_address =
        reference_byte_address(transaction, transaction.address);
    const std::uint64_t address = is_cbo_zero(transaction.op)
        ? raw_address & ~std::uint64_t{63}
        : raw_address;
    const unsigned bytes = scalar_store_bytes(transaction.op);
    for (unsigned byte = 0; byte < bytes; ++byte) {
        const auto physical = reference_byte_address(transaction,
            (is_cbo_zero(transaction.op)
                ? transaction.address & ~std::uint64_t{63}
                : transaction.address) + byte);
        memory_.write_reference_byte(physical,
            is_cbo_zero(transaction.op)
                ? 0
                : static_cast<std::uint8_t>(transaction.data >> (8 * byte)));
        if (transaction.reference_pages) {
            if (!scalar_release_inputs_.contains(scalar_store_key(transaction)) && bytes <= 8U && (transaction.address & (bytes - 1U)) == 0) {
                if (byte == 0) {
                    std::vector<unsigned char> update(bytes);
                    for (unsigned b = 0; b < bytes; ++b) update[b] = transaction.data >> (8U * b);
                    memory_agent_.note_reference_store(physical, transaction.address, update);
                }
            } else if (!scalar_release_inputs_.contains(scalar_store_key(transaction))) {
                memory_agent_.note_reference_write(physical, transaction.address + byte,
                    memory_.read_byte(physical));
            }
            track_final_observation_bytes(physical, 1U, transaction.cacheable, true);
        }
    }
    if (!transaction.reference_pages) {
        track_final_observation_bytes(address, bytes, transaction.cacheable,
            !transaction.oracle_address ||
                *transaction.oracle_address == transaction.address);
    }
    trace_reference_store(
        memory_, "store-record", address, bytes, transaction.data,
        transaction.rob, transaction.rob_flag, transaction.sq,
        transaction.sq_flag, "");
}

auto Environment::Impl::record_atomic_result(std::uint64_t address,
    std::uint64_t value, bool cacheable, bool identity_mapped)  -> void
{
    track_final_observation_bytes(address, 8U, cacheable, identity_mapped);
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
                vector_byte_address(transaction, element, byte),
                transaction.data[element * element_bytes + byte]);
            if (transaction.reference_pages && !vector_release_inputs_.contains(vector_store_key(transaction))) {
                const auto virtual_address = vector_element_virtual_address(transaction, element);
                if ((virtual_address & (element_bytes - 1U)) == 0) {
                    if (byte == 0) memory_agent_.note_reference_store(
                        vector_byte_address(transaction, element, 0), virtual_address,
                        std::vector<unsigned char>(transaction.data.begin() + element * element_bytes,
                            transaction.data.begin() + (element + 1U) * element_bytes));
                } else {
                    memory_agent_.note_reference_write(
                        vector_byte_address(transaction, element, byte), virtual_address + byte,
                        transaction.data[element * element_bytes + byte]);
                }
            }
        }
        std::uint64_t element_data = 0;
        for (unsigned byte = 0; byte < element_bytes; ++byte) {
            element_data |= std::uint64_t{
                transaction.data[element * element_bytes + byte]}
                << (8 * byte);
        }
        if (transaction.reference_pages) {
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                track_final_observation_bytes(
                    vector_byte_address(transaction, element, byte), 1U,
                    transaction.cacheable, true);
            }
        } else {
            track_final_observation_bytes(
                vector_element_address(transaction, element), element_bytes,
                transaction.cacheable, !transaction.oracle_address ||
                    *transaction.oracle_address == transaction.address);
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

auto Environment::Impl::return_load_exception_after_nonmemory_wb(const LoadTransaction &fault,
    std::uint8_t anchor, bool anchor_flag, std::uint64_t &captured_vaddr,
    LoadExceptionStoreObservation *stores) -> bool
{
    const unsigned anchor_identity = unsigned(anchor) + unsigned(anchor_flag) * kRobEntries;
    const unsigned fault_identity = unsigned(fault.rob) + unsigned(fault.rob_flag) * kRobEntries;
    const auto checked_edge = scoreboard_.checked_completion_cycle(fault);
    if (anchor >= kRobEntries || fault.rob >= kRobEntries ||
        (anchor_identity + 1U) % (2U * kRobEntries) != fault_identity ||
        fault.expected_exception_mask != kExceptionLoadPageFault || !checked_edge || cycle() < *checked_edge + 5U ||
        dut_.queues.backend.pending_ptr_value.value() != anchor ||
        dut_.queues.backend.pending_ptr_flag.boolean() != anchor_flag ||
        dut_.queues.backend.pendingst.boolean() || dut_.queues.backend.pending_mmi_old.boolean() ||
        dut_.queues.redirect.valid.boolean()) {
        error_.record("load exception requires checked F and its declared incomplete nonmemory predecessor A");
        return false;
    }
    if (!scoreboard_.consume_checked_completion(fault)) {
        error_.record("load exception terminal authorization was already consumed");
        return false;
    }
    const auto violation_count = feedback_.memory_violation_stats().count;
    // Omitted EXU's actual A WB is C. Ctrl WB C+1, bank commit-ready C+2,
    // internal F head C+3; public F head and ROB redirect both C+4. Existing
    // retained offers remain monitored; no new dispatch occurs in this helper.
    if (!run_cycles(4)) return false;
    if (feedback_.memory_violation_stats().count != violation_count) {
        error_.record("competing redirect during nonmemory exception handoff");
        return false;
    }
    dut_.queues.backend.pending_ptr_value.set(fault.rob);
    dut_.queues.backend.pending_ptr_flag.set(fault.rob_flag);
    return redirect_after(fault.rob, fault.rob_flag, true, false, &captured_vaddr, stores);
}

auto Environment::Impl::redirect_after(std::uint8_t rob_value,
    bool rob_flag,
    bool flush_itself,
    bool is_vls_exception, std::uint64_t *captured_vaddr,
    LoadExceptionStoreObservation *stores, bool ordinary_walk)  -> bool
{
    scalar_redirect_boundary_.reset();
    ordinary_walk_resume_cycle_.reset();
    if (is_vls_exception && !flush_itself) {
        error_.record("VLS exception redirect must enter as a flush redirect");
        return false;
    }
    dut_.queues.redirect.rob_idx_flag.set(rob_flag);
    dut_.queues.redirect.rob_idx_value.set(rob_value);
    dut_.queues.redirect.level.set(flush_itself);
    dut_.queues.redirect.is_vls_exception.set(is_vls_exception);
    dut_.queues.redirect.valid.set(std::uint64_t{1});
    // Observe the actual monitored pre-edge R after retained producers and
    // managers drive, before the DUT advances or dequeue accounting changes.
    tick(true, [&] {
        if (!stores) return;
        const auto &retirement = queues_.store_retirement_stats();
        *stores = {cycle(), retirement.scalar + retirement.vector_uops,
            retirement.pending, !dut_.sample_sbuffer_empty()};
    });
    // Rob.pendingst is registered from isCommit, which is false on the
    // redirect edge and during walk. Preserve the previous level at R, then
    // suppress at R+1 without moving the surviving architectural head.
    // Ordinary caller owns walk completion and explicit head restoration.
    if (ordinary_walk) {
        dut_.queues.backend.pendingst.set(std::uint64_t{0});
        dut_.queues.backend.pending_mmi_old.set(std::uint64_t{0});
        dut_.queues.backend.scommit.set(std::uint64_t{0});
    }
    dut_.queues.redirect.valid.set(std::uint64_t{0});
    dut_.queues.redirect.is_vls_exception.set(std::uint64_t{0});
    // MemBlock registers redirect once; the queues publish their retained
    // redirectCancelCount values after two more clock edges. Sample once
    // per known redirect instead of treating the retained value as a pulse.
    tick();
    // CSR consumes DelayN(rob.exception,2) pre-edge R+2. The payload after
    // return R+3 need not retain the exception consumed on this edge.
    if (captured_vaddr) {
        dut_.refresh_comb();
        *captured_vaddr = exception_vaddr();
    }
    tick();
    dut_.refresh_comb();
    const std::uint64_t lq_canceled =
        dut_.queues.retirement.lq_cancel_cnt.value();
    const std::uint64_t sq_canceled =
        dut_.queues.retirement.sq_cancel_cnt.value();
    if (!queues_.observe_redirect_cancellation(lq_canceled, sq_canceled)) return false;
    if (!check_components()) return false;
    // VLS exception recovery retains the faulting vector instruction's SQ
    // members; StoreQueue cancels strict-younger ROB identities in this mode.
    if (!queues_.cancel_store_targets_for_redirect(rob_value, rob_flag,
            flush_itself && !is_vls_exception)) return false;
    scalar_redirect_boundary_ = ScalarRedirectBoundary{rob_value, rob_flag,
        flush_itself, cycle(), lq_canceled, queues_.sq_allocated()};
    if (ordinary_walk) ordinary_walk_resume_cycle_ = cycle() + kStreamOrdinaryRecoveryBusy + 1U;
    return true;
}

auto Environment::Impl::ok() const -> bool
{ return error_.empty(); }

auto Environment::Impl::error() const -> const std::string &
{ return error_.message(); }

} // namespace memblock
