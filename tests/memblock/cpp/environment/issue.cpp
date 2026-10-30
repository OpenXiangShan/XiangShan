#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::enqueue_prefetch_batch(const std::vector<PrefetchTransaction> &transactions,
    const std::vector<unsigned> &dispatch_lanes)  -> bool
{
    if (transactions.empty() ||
        transactions.size() != dispatch_lanes.size() ||
        transactions.size() > generated::kLsqEnqueueLanes ||
        !std::is_sorted(dispatch_lanes.begin(), dispatch_lanes.end())) {
        error_.record("LSQ prefetch batch requires one sorted unique dispatch lane per request");
        return false;
    }
    std::array<bool, generated::kLsqEnqueueLanes> lane_used{};
    if (!wait_for_enqueue_capacity(transactions.size(), 0)) {
        return false;
    }
    for (std::size_t index = 0; index < transactions.size(); ++index) {
        const unsigned lane = dispatch_lanes[index];
        if (lane >= generated::kLsqEnqueueLanes || lane_used[lane]) {
            error_.record("LSQ prefetch batch has an invalid or duplicate dispatch lane");
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

auto Environment::Impl::issue_prefetch(const PrefetchTransaction &transaction,
    unsigned timeout)  -> bool
{
    if (!scalar_immediate_is_valid(transaction.immediate)) {
        error_.record("software-prefetch immediate exceeds signed 12-bit range");
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
    error_.record("software-prefetch issue timed out waiting for ready");
    return false;
}

auto Environment::Impl::issue_prefetch_batch_same_cycle(const std::vector<PrefetchTransaction> &transactions,
    unsigned timeout)  -> bool
{
    if (transactions.empty() || transactions.size() > kScalarLoadLanes) {
        error_.record("software-prefetch batch must contain one to three requests");
        return false;
    }
    std::array<bool, kScalarLoadLanes> lane_used{};
    std::vector<generated::ScalarLoadIssue> issues(transactions.size());
    for (std::size_t index = 0; index < transactions.size(); ++index) {
        const auto &transaction = transactions[index];
        if (transaction.lane >= kScalarLoadLanes ||
            lane_used[transaction.lane]) {
            error_.record("software-prefetch batch lanes must be unique");
            return false;
        }
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_.record("software-prefetch immediate exceeds signed 12-bit range");
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
    }
    dut_.clear_scalar_load_issue_valids();
    error_.record("software-prefetch batch timed out waiting for same-cycle ready");
    return false;
}

auto Environment::Impl::expect_store(const StoreTransaction &transaction)  -> void
{
    store_scoreboard_.expect(transaction);
}

auto Environment::Impl::enqueue_store(const StoreTransaction &transaction) -> bool
{
    const auto lq = queues_.lq_allocated() - queues_.lq_canceled();
    const auto sq = queues_.sq_allocated() - queues_.sq_canceled();
    if (transaction.lq != lq_pointer_value(lq) ||
        transaction.lq_flag != lq_pointer_flag(lq) ||
        transaction.sq != sq_pointer_value(sq) ||
        transaction.sq_flag != sq_pointer_flag(sq)) {
        error_.record("scalar store dispatch snapshot differs from allocation cursors");
        return false;
    }
    return enqueue_store(transaction, transaction.lq, transaction.lq_flag);
}

auto Environment::Impl::enqueue_store(const StoreTransaction &transaction,
    std::uint8_t lq_value, bool lq_flag) -> bool
{
    if (!wait_for_enqueue_capacity(0, 1)) {
        return false;
    }
    return enqueue_store_pressure(transaction, lq_value, lq_flag);
}

auto Environment::Impl::enqueue_store_pressure(const StoreTransaction &transaction,
    std::uint8_t lq_value, bool lq_flag) -> bool
{
    const std::uint64_t retired = queues_.sq_dequeued() + queues_.sq_canceled();
    if (retired > queues_.sq_allocated() ||
        queues_.sq_allocated() - retired >= kStoreQueueEntries) {
        error_.record("store pressure enqueue exceeds StoreQueue capacity");
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
    enqueue.lq_flag = lq_flag;
    enqueue.lq_value = lq_value;
    enqueue.sq_flag = transaction.sq_flag;
    enqueue.sq_value = transaction.sq;
    dut_.drive_lsq_enqueue(0, enqueue);
    tick();
    dut_.clear_lsq_enqueue_valids();
    queues_.allocate_scalar_store(scalar_store_key(transaction));
    return check_components();
}

auto Environment::Impl::expect_vector(const VectorMemoryTransaction &transaction)  -> void
{
    const auto expected = transaction.store
        ? transaction.data
        : memory_.expected_vector_load(transaction);
    vector_scoreboard_.expect(transaction, expected);
}

auto Environment::Impl::expect_vector_data(const VectorMemoryTransaction &transaction,
    const std::array<unsigned char, 16> &data)  -> void
{
    vector_scoreboard_.expect(transaction, data);
}

auto Environment::Impl::enqueue_vector(const VectorMemoryTransaction &transaction)  -> bool
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
    dut_.drive_lsq_enqueue(0, enqueue);
    tick();
    dut_.clear_lsq_enqueue_valids();
    if (!transaction.store) {
        queues_.allocate_load(enqueue.num_ls_elem);
    } else {
        queues_.allocate_vector_store(vector_store_key(transaction), enqueue.num_ls_elem);
    }
    return check_components();
}

namespace {

void trace_vector_issue(const VectorMemoryTransaction &transaction)
{
    if (!transaction.store) {
        return;
    }
    const unsigned element_bytes = 1U << vector_data_eew(transaction);
    const unsigned elements = 16U >> vector_data_eew(transaction);
    const std::uint16_t active = active_vector_elements(transaction);
    for (unsigned element = 0; element < elements; ++element) {
        if (((active >> element) & 1U) == 0) {
            continue;
        }
        const std::uint64_t address =
            vector_element_address(transaction, element);
        if (!debug_trace::enabled_for(address, element_bytes)) {
            continue;
        }
        std::uint64_t element_data = 0;
        for (unsigned byte = 0; byte < element_bytes; ++byte) {
            element_data |= std::uint64_t{
                transaction.data[element * element_bytes + byte]}
                << (8 * byte);
        }
        std::ostringstream text;
        text << "vstore-issue addr=0x" << std::hex << address
             << " bytes=" << std::dec << element_bytes
             << " data=0x" << std::hex << element_data
             << " rob=" << std::dec
             << static_cast<unsigned>(transaction.rob)
             << ':' << transaction.rob_flag
             << " sq=" << static_cast<unsigned>(transaction.sq)
             << ':' << transaction.sq_flag
             << " element=" << element
             << " uop=" << static_cast<unsigned>(transaction.vuop_idx)
             << '/' << static_cast<unsigned>(transaction.flow_num)
             << " vl=" << static_cast<unsigned>(transaction.vl)
             << " vstart=" << static_cast<unsigned>(transaction.vstart)
             << " mask=0x" << std::hex << active
             << " eew=" << std::dec
             << static_cast<unsigned>(transaction.eew)
             << " vsew=" << static_cast<unsigned>(
                    vector_vsew(transaction))
             << " vlmul=" << static_cast<unsigned>(transaction.vlmul)
             << " addressing=" << static_cast<unsigned>(
                    transaction.addressing);
        debug_trace::emit(text.str());
    }
}

} // namespace

auto Environment::Impl::issue_vector(const VectorMemoryTransaction &transaction,
    unsigned timeout)  -> bool
{
    trace_vector_issue(transaction);
    const generated::VectorMemoryIssue issue =
        make_vector_memory_issue(transaction);

    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        dut_.drive_vector_memory_issue(transaction.lane, issue);
        dut_.refresh_comb();
        const bool ready = dut_.vector_memory_issue_ready(transaction.lane);
        if (ready && transaction.store &&
            !accept_release_store_input(vector_store_key(transaction), true, true)) return false;
        tick();
        if (ready) {
            dut_.clear_vector_memory_issue_valids();
            return check_components();
        }
        if (!check_components()) {
            return false;
        }
    }
    dut_.clear_vector_memory_issue_valids();
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
    error_.record(message.str());
    return false;
}

auto Environment::Impl::issue_vector_batch_same_cycle(const std::vector<VectorMemoryTransaction> &transactions,
    unsigned timeout)  -> bool
{
    if (transactions.empty() || transactions.size() > kVectorMemoryLanes) {
        error_.record("vector issue batch must contain one or two transactions");
        return false;
    }
    std::array<bool, kVectorMemoryLanes> lane_used{};
    std::vector<generated::VectorMemoryIssue> issues;
    issues.reserve(transactions.size());
    for (const auto &transaction : transactions) {
        if (transaction.lane >= kVectorMemoryLanes ||
            lane_used[transaction.lane]) {
            error_.record("vector issue batch lanes must be unique");
            return false;
        }
        lane_used[transaction.lane] = true;
        issues.push_back(make_vector_memory_issue(transaction));
    }

    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            dut_.drive_vector_memory_issue(transactions[index].lane, issues[index]);
        }
        dut_.refresh_comb();
        const bool all_ready = std::all_of(
            transactions.begin(), transactions.end(), [&](const auto &transaction) {
                return dut_.vector_memory_issue_ready(transaction.lane);
            });
        if (all_ready) {
            tick();
            dut_.clear_vector_memory_issue_valids();
            return check_components();
        }
        dut_.clear_vector_memory_issue_valids();
        tick();
        if (!check_components()) {
            return false;
        }
    }
    dut_.clear_vector_memory_issue_valids();
    error_.record("vector issue batch timed out waiting for same-cycle ready");
    return false;
}

auto Environment::Impl::issue_load_vector_pair(const LoadTransaction &load,
    const VectorMemoryTransaction &vector,
    unsigned timeout)  -> bool
{
    last_load_vector_pair_same_cycle_ = false;
    if (!scalar_immediate_is_valid(load.immediate)) {
        error_.record("scalar load immediate exceeds signed 12-bit range");
        return false;
    }
    generated::ScalarLoadIssue scalar_issue;
    scalar_issue.pc = load.pc.value_or(0x1000 + load.rob * 4);
    scalar_issue.predecode_rvc = load.predecode_rvc;
    scalar_issue.ftq_ptr = load.ftq_ptr;
    scalar_issue.ftq_offset = load.ftq_offset;
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
    scalar_issue.store_set_hit = load.store_set_hit;
    scalar_issue.wait_for_rob_flag = load.wait_for_rob_flag;
    scalar_issue.wait_for_rob_value = load.wait_for_rob_value;
    scalar_issue.load_wait_bit = load.load_wait_bit;
    scalar_issue.load_wait_strict = load.load_wait_strict;
    scalar_issue.imm = static_cast<std::uint32_t>(load.immediate);
    scalar_issue.src = scalar_issue_base_address(
        load.address, load.immediate);

    const generated::VectorMemoryIssue vector_issue =
        make_vector_memory_issue(vector);

    bool scalar_pending = true;
    bool vector_pending = true;
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        if (scalar_pending) {
            dut_.drive_scalar_load_issue(load.lane, scalar_issue);
        }
        if (vector_pending) {
            dut_.drive_vector_memory_issue(vector.lane, vector_issue);
        }
        dut_.refresh_comb();
        const bool scalar_accepted = scalar_pending &&
            dut_.scalar_load_issue_ready(load.lane);
        const bool vector_accepted = vector_pending &&
            dut_.vector_memory_issue_ready(vector.lane);
        if (scalar_accepted && vector_accepted) {
            last_load_vector_pair_same_cycle_ = true;
        }
        tick();
        if (scalar_accepted) {
            scalar_pending = false;
            dut_.clear_scalar_load_issue_valid(load.lane);
        }
        if (vector_accepted) {
            vector_pending = false;
            dut_.clear_vector_memory_issue_valids();
        }
        if (!check_components()) {
            return false;
        }
        if (!scalar_pending && !vector_pending) {
            return true;
        }
    }
    dut_.clear_scalar_load_issue_valids();
    dut_.clear_vector_memory_issue_valids();
    error_.record("mixed scalar/vector load issue timed out waiting for ready");
    return false;
}

namespace {

void trace_store_issue(
    const char *kind, const StoreTransaction &transaction)
{
    const std::uint64_t address =
        reference_byte_address(transaction, transaction.address);
    const unsigned bytes = scalar_store_bytes(transaction.op);
    if (!debug_trace::enabled_for(transaction.address, bytes) &&
        !debug_trace::enabled_for(address, bytes)) {
        return;
    }
    std::ostringstream text;
    text << kind << " vaddr=0x" << std::hex << transaction.address
         << " paddr=0x" << address
         << " op=" << std::dec << static_cast<unsigned>(transaction.op)
         << " bytes=" << bytes
         << " data=0x" << std::hex << transaction.data
         << " rob=" << std::dec << static_cast<unsigned>(transaction.rob)
         << ':' << transaction.rob_flag
         << " sq=" << static_cast<unsigned>(transaction.sq)
         << ':' << transaction.sq_flag
         << " address_lane=" << transaction.address_lane
         << " data_lane=" << transaction.data_lane;
    debug_trace::emit(text.str());
}

} // namespace

auto Environment::Impl::issue_store_address(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
{
    trace_store_issue("store-address-issue", transaction);
    if (!scalar_immediate_is_valid(transaction.immediate)) {
        error_.record("scalar store immediate exceeds signed 12-bit range");
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
        dut_.drive_scalar_store_address(transaction.address_lane, issue);
        dut_.refresh_comb();
        const bool ready = dut_.scalar_store_address_ready(transaction.address_lane);
        if (ready && !store_scoreboard_.mark_address_issued(
                transaction, this->cycle())) {
            dut_.clear_scalar_store_issue_valids();
            return false;
        }
        if (ready && !accept_release_store_input(scalar_store_key(transaction), false, true)) return false;
        tick();
        if (ready) {
            dut_.clear_scalar_store_issue_valids();
            return check_components();
        }
        if (!check_components()) {
            return false;
        }
    }
    dut_.clear_scalar_store_issue_valids();
    error_.record("scalar store-address issue timed out waiting for ready");
    return false;
}

auto Environment::Impl::issue_store_address_batch(const std::vector<StoreTransaction> &transactions,
    unsigned timeout)  -> bool
{
    if (transactions.empty() || transactions.size() > kScalarStoreLanes) {
        error_.record("scalar store-address batch must contain one or two transactions");
        return false;
    }
    std::array<bool, kScalarStoreLanes> lane_used{};
    std::vector<generated::ScalarStoreIssue> issues(transactions.size());
    for (std::size_t index = 0; index < transactions.size(); ++index) {
        const auto &transaction = transactions[index];
        if (transaction.address_lane >= kScalarStoreLanes ||
            lane_used[transaction.address_lane]) {
            error_.record("scalar store-address batch lanes must be unique");
            return false;
        }
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_.record("scalar store immediate exceeds signed 12-bit range");
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
            dut_.drive_scalar_store_address(transactions[index].address_lane, issues[index]);
        }
        dut_.refresh_comb();
        const bool all_ready = std::all_of(
            transactions.begin(), transactions.end(), [&](const auto &transaction) {
                return dut_.scalar_store_address_ready(transaction.address_lane);
            });
        if (all_ready) {
            for (const auto &transaction : transactions) {
                if (!store_scoreboard_.mark_address_issued(
                        transaction, this->cycle())) {
                    dut_.clear_scalar_store_issue_valids();
                    return false;
                }
            }
            tick();
            dut_.clear_scalar_store_issue_valids();
            return check_components();
        }
        dut_.clear_scalar_store_issue_valids();
        tick();
        if (!check_components()) {
            return false;
        }
    }
    dut_.clear_scalar_store_issue_valids();
    error_.record("scalar store-address batch timed out waiting for same-cycle ready");
    return false;
}

auto Environment::Impl::issue_atomic(const AtomicTransaction &transaction,
    unsigned timeout)  -> bool
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
        dut_.drive_scalar_store_address(transaction.address_lane, address);
        dut_.refresh_comb();
        const bool ready = dut_.scalar_store_address_ready(transaction.address_lane);
        tick();
        if (ready) {
            dut_.clear_scalar_store_issue_valids();
            break;
        }
        if (!check_components()) {
            dut_.clear_scalar_store_issue_valids();
            return false;
        }
        if (cycle + 1 == timeout) {
            dut_.clear_scalar_store_issue_valids();
            error_.record("atomic address issue timed out waiting for ready");
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
            dut_.drive_scalar_store_data(transaction.data_lane, data);
            dut_.refresh_comb();
            const bool ready = dut_.scalar_store_data_ready(transaction.data_lane);
            tick();
            if (ready) {
                accepted = true;
                dut_.clear_scalar_store_issue_valids();
                break;
            }
            if (!check_components()) {
                dut_.clear_scalar_store_issue_valids();
                return false;
            }
        }
        if (!accepted) {
            dut_.clear_scalar_store_issue_valids();
            error_.record("atomic data issue timed out waiting for ready");
            return false;
        }
    }
    return check_components();
}

auto Environment::Impl::warm_store_translation(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
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
                << " feedbacks=" << feedback_.store_tlb_feedbacks()
                << " misses=" << feedback_.store_tlb_misses();
        error_.record(message.str());
        return false;
    }
    if (!run_cycles(64)) {
        return false;
    }
    return issue_store_address(transaction);
}

auto Environment::Impl::issue_store_address_until_tlb_hit(const StoreTransaction &transaction,
    unsigned timeout) -> bool
{
    const std::uint64_t deadline = cycle() + timeout;
    while (cycle() < deadline) {
        const auto first_sample = feedback_.iq_slow_feedback_stats().sta_samples.size();
        const unsigned issue_timeout = static_cast<unsigned>(std::min(
            std::uint64_t{256}, deadline - cycle()));
        if (issue_timeout == 0 || !issue_store_address(transaction, issue_timeout)) {
            return false;
        }
        std::optional<bool> hit;
        while (cycle() < deadline) {
            const auto &samples = feedback_.iq_slow_feedback_stats().sta_samples;
            const auto match = std::find_if(samples.begin() + first_sample, samples.end(),
                [&](const auto &sample) {
                    return sample.lane == transaction.address_lane &&
                        sample.sq_flag == transaction.sq_flag &&
                        sample.sq_value == transaction.sq;
                });
            if (match != samples.end()) {
                hit = match->hit;
                break;
            }
            tick();
            if (!check_components()) return false;
        }
        if (!hit) break;
        if (*hit) return check_components();
        if (!run_cycles(static_cast<unsigned>(std::min(
                std::uint64_t{8}, deadline - cycle())))) return false;
    }
    error_.record("timed out replaying store address until identified DTLB hit");
    return false;
}

auto Environment::Impl::run_until_store_complete_with_replay(const StoreTransaction &transaction,
    unsigned timeout,
    bool hold_pending_store) -> bool
{
    // ROB keeps pendingst asserted while this same store remains its head.
    // The caller owns architectural age eligibility; this wait owns only the
    // declared transaction and must retain unrelated younger obligations.
    if (hold_pending_store) {
        dut_.queues.backend.pending_ptr_value.set(transaction.rob);
        dut_.queues.backend.pending_ptr_flag.set(transaction.rob_flag);
        dut_.queues.backend.pendingst.set(std::uint64_t{1});
    }
    const auto clear_pending_store = [&]() {
        if (hold_pending_store) dut_.queues.backend.pendingst.set(std::uint64_t{0});
    };
    const std::uint64_t deadline = cycle() + timeout;
    while (store_scoreboard_.pending(transaction) && cycle() < deadline) {
        if (!run_cycles(static_cast<unsigned>(std::min(
                std::uint64_t{32}, deadline - cycle())))) {
            clear_pending_store();
            return false;
        }
        // A successful address result may precede STD. Reissuing that STA
        // while waiting only for data would fabricate duplicate completions.
        if (store_scoreboard_.address_pending(transaction) && cycle() < deadline &&
            !issue_store_address(transaction, static_cast<unsigned>(std::min(
                std::uint64_t{32}, deadline - cycle())))) {
            clear_pending_store();
            return false;
        }
    }
    clear_pending_store();
    if (store_scoreboard_.pending(transaction)) {
        error_.record("timed out waiting for identified replayed scalar store writeback");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_store_tlb_misses(std::uint64_t target,
    unsigned timeout)  -> bool
{
    for (unsigned cycle = 0;
         cycle < timeout && feedback_.store_tlb_misses() < target; ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (feedback_.store_tlb_misses() < target) {
        error_.record("timed out waiting for store TLB-miss feedback");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_ptw_requests(std::uint64_t target,
    unsigned timeout)  -> bool
{
    for (unsigned cycle = 0;
         cycle < timeout && ptw_agent_.request_count() < target; ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (ptw_agent_.request_count() < target) {
        error_.record("timed out waiting for page-table walk request");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_dcache_requests(std::uint64_t target,
    unsigned timeout)  -> bool
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
        error_.record("timed out waiting for target DCache request count");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_dcache_idle(unsigned timeout)  -> bool
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
        error_.record("timed out draining DCache responses and GrantAcks");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_dcache_release_idle(unsigned timeout)  -> bool
{
    for (unsigned cycle = 0;
         cycle < timeout && !memory_agent_.release_channel_idle();
         ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (!memory_agent_.release_channel_idle()) {
        error_.record("timed out draining the DCache release channel");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_ptw_idle(unsigned timeout)  -> bool
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
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::wait_for_cmo_store_request(const StoreTransaction &transaction,
    std::uint64_t target,
    unsigned timeout)  -> bool
{
    if (!is_cmo(transaction.op)) {
        error_.record("CMO request wait requires CLEAN, FLUSH, or INVAL");
        return false;
    }
    const unsigned operation = cmo_operation_index(transaction.op);
    dut_.queues.backend.pending_ptr_value.set(transaction.rob);
    dut_.queues.backend.pending_ptr_flag.set(transaction.rob_flag);
    // StoreQueue samples pendingst as a level while the CMO remains at
    // the ROB head and may still be draining SBuffer.
    dut_.queues.backend.pendingst.set(std::uint64_t{1});
    for (unsigned elapsed = 0;
         elapsed < timeout && memory_agent_.cmo_count(operation) < target;
         ++elapsed) {
        tick();
        if (!check_components()) {
            dut_.queues.backend.pendingst.set(std::uint64_t{0});
            return false;
        }
    }
    dut_.queues.backend.pendingst.set(std::uint64_t{0});
    if (memory_agent_.cmo_count(operation) < target) {
        error_.record("timed out waiting for target DCache CMO request count");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_ptw_request_covering(std::uint64_t address,
    std::uint64_t first_request,
    unsigned timeout,
    unsigned min_response_delay)  -> bool
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
        error_.record(message.str());
        return false;
    }
    if (!ptw_agent_.request_covering_address_has_min_delay_since(
            address, first_request, min_response_delay)) {
        std::ostringstream message;
        message << "PTW request covering address 0x" << std::hex
                << address << std::dec
                << " did not receive the required minimum response delay "
                << min_response_delay;
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_uncache_requests(std::uint64_t target,
    unsigned timeout)  -> bool
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
        error_.record("timed out waiting for target Uncache request count");
        return false;
    }
    return check_components();
}

auto Environment::Impl::run_until_uncache_drained(std::uint64_t request_target,
    unsigned timeout)  -> bool
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
        error_.record(message.str());
        return false;
    }
    return check_components();
}

auto Environment::Impl::issue_store_data(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
{
    trace_store_issue("store-data-issue", transaction);
    generated::ScalarStoreIssue issue;
    issue.fu_type = kFuTypeStore;
    issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
    issue.rob_value = transaction.rob;
    issue.sq_flag = transaction.sq_flag;
    issue.sq_value = transaction.sq;
    issue.src = transaction.data;
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        dut_.drive_scalar_store_data(transaction.data_lane, issue);
        dut_.refresh_comb();
        const bool ready = dut_.scalar_store_data_ready(transaction.data_lane);
        if (ready && !store_scoreboard_.mark_data_issued(
                transaction, transaction.data_lane, this->cycle())) {
            dut_.clear_scalar_store_issue_valids();
            return false;
        }
        if (ready && !accept_release_store_input(scalar_store_key(transaction), false, false)) return false;
        tick();
        if (ready) {
            dut_.clear_scalar_store_issue_valids();
            return check_components();
        }
        if (!check_components()) {
            return false;
        }
    }
    dut_.clear_scalar_store_issue_valids();
    error_.record("scalar store-data issue timed out waiting for ready");
    return false;
}

auto Environment::Impl::enqueue_load(const LoadTransaction &transaction)  -> bool
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
