#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::wait_for_enqueue_capacity(unsigned lq_needed,
    unsigned sq_needed,
    unsigned timeout)  -> bool
{
    if (lq_needed > kVirtualLoadQueueEntries - kLqEnqueueHeadroom ||
        sq_needed > kStoreQueueEntries - kSqEnqueueHeadroom) {
        error_.record("LSQ enqueue request exceeds reserved queue capacity");
        return false;
    }
    for (unsigned cycle = 0; cycle < timeout; ++cycle) {
        const auto capacity_ok = queues_.has_capacity(lq_needed, sq_needed);
        if (!capacity_ok) return false;
        if (*capacity_ok) return true;
        tick();
        if (!check_components()) {
            return false;
        }
    }
    std::ostringstream message;
    message << "timed out waiting for LSQ enqueue capacity"
            << " lq_needed=" << lq_needed
            << " sq_needed=" << sq_needed
            << " lq=" << queues_.lq_allocated() - (queues_.lq_dequeued() + queues_.lq_canceled())
            << '/' << kVirtualLoadQueueEntries
            << " sq=" << queues_.sq_allocated() - (queues_.sq_dequeued() + queues_.sq_canceled())
            << '/' << kStoreQueueEntries;
    error_.record(message.str());
    return false;
}

auto Environment::Impl::write_distributed_csr(std::uint16_t address,
    std::uint64_t data)  -> bool
{
    dut_.control.csr.distribute_csr_w_addr.set(address);
    dut_.control.csr.distribute_csr_w_data.set(data);
    dut_.control.csr.distribute_csr_w_valid.set(std::uint64_t{1});
    tick(false);
    dut_.control.csr.distribute_csr_w_valid.set(std::uint64_t{0});
    return check_components();
}

auto Environment::Impl::tick(bool monitor)  -> void
{
    set_failure_cycle();
    if (!authorize_release_store_inputs()) return;
    if (retained_issue_driver_) retained_issue_driver_->drive(dut_);
    memory_agent_.drive(dut_);
    ptw_agent_.drive(dut_);
    uncache_agent_.drive(dut_);
    dut_.refresh_comb();
    if (retained_issue_driver_) retained_issue_driver_->capture_before_edge(dut_, cycle(), {
        dut_.queues.redirect.valid.boolean(),
        static_cast<std::uint8_t>(dut_.queues.redirect.rob_idx_value.value()),
        dut_.queues.redirect.rob_idx_flag.boolean(),
        dut_.queues.redirect.level.boolean()});
    memory_agent_.capture_before_tick(dut_);
    ptw_agent_.capture_before_tick(dut_);
    uncache_agent_.capture_before_tick(dut_);
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
    sample_internal_debug();
#endif

    bridge_.capture_inputs();

    // Writeback valid is a combinational projection of the execution-unit
    // output fire.  Observe the pins before the clock edge; after Step()
    // they may already describe the following transaction.  LSQ dequeue
    // pulses are registered separately and are counted below instead.
    if (monitor) {
        queues_.sample_before_edge(dut_);
        bridge_.check_before_edge();
        feedback_.sample_before_edge(dut_, cycle(), scoreboard_, store_scoreboard_,
            vector_scoreboard_
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
            , [this](unsigned lane, const generated::ScalarLoadWriteback &writeback) {
                trace_scalar_writeback_debug(lane, writeback);
            }
#endif
        );
    }

    const bool redirect = dut_.queues.redirect.valid.boolean();
    dut_.step();
    if (retained_issue_driver_) retained_issue_driver_->advance_after_edge(dut_);
    set_failure_cycle();
    bridge_.advance_after_edge();
    memory_agent_.update_after_tick();
    ptw_agent_.update_after_tick();
    uncache_agent_.update_after_tick();
    queues_.sample_after_edge(dut_, redirect);
    feedback_.sample_after_edge(dut_, cycle());
}

auto Environment::Impl::check_components()  -> bool
{
    error_.merge(memory_agent_.failures());
    error_.merge(ptw_agent_.failures());
    error_.merge(uncache_agent_.failures());
    error_.merge(scoreboard_.failures());
    error_.merge(store_scoreboard_.failures());
    error_.merge(vector_scoreboard_.failures());
    queues_.check_driver_accounting();
    return error_.empty();
}

auto Environment::Impl::scalar_store_key(const StoreTransaction &transaction)  -> std::uint64_t
{
    return transaction.rob |
        (std::uint64_t{transaction.rob_flag} << 8) |
        (std::uint64_t{transaction.sq} << 9) |
        (std::uint64_t{transaction.sq_flag} << 17);
}

auto Environment::Impl::vector_store_key(const VectorMemoryTransaction &transaction)  -> std::uint64_t
{
    return transaction.rob |
        (std::uint64_t{transaction.rob_flag} << 8) |
        (std::uint64_t{transaction.sq} << 9) |
        (std::uint64_t{transaction.sq_flag} << 17);
}

} // namespace memblock
