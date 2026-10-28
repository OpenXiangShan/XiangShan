#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::execute_noncacheable(ActionWindow &window)
{
    auto &nc_store = window.nc_store;
    auto &uncache_error_denied = window.uncache_error_denied;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;

    ordinary_leaf_addressed = true;
    const std::uint64_t offset = (random() % 128) * 8;
    if (uncache_error_denied &&
        !environment.run_until_queues_retired(8192)) {
        return false;
    }
    if (!nc_store) {
        phase = "random-nc-load";
        auto transaction = make_load(
            nc_base + offset, memblock::LoadOp::ld, random() % 3);
        if (uncache_error_denied) {
            transaction.expected_exception_mask =
                *uncache_error_denied
                ? memblock::kExceptionLoadAccessFault
                : memblock::kExceptionHardwareError;
        }
        const std::uint64_t uncache_before =
            environment.uncache_requests();
        const std::uint64_t error_before =
            environment.uncache_error_response_requests();
        const std::uint64_t denied_before =
            environment.uncache_denied_d_beats();
        const std::uint64_t corrupt_before =
            environment.uncache_corrupt_d_beats();
        const std::uint64_t lq_dequeued_before =
            environment.lq_dequeued();
        const std::uint64_t sq_dequeued_before =
            environment.sq_dequeued();
        const std::uint64_t lq_cancels_before =
            environment.lq_redirect_canceled_observed();
        const std::uint64_t sq_cancels_before =
            environment.sq_redirect_canceled_observed();
        if (uncache_error_denied) {
            environment.inject_next_uncache_response_error(
                *uncache_error_denied,
                !*uncache_error_denied);
        }
        if (!issue_load(transaction) ||
            environment.uncache_requests() != uncache_before + 1 ||
            (uncache_error_denied &&
                (!recover_random_precise_error(
                     transaction.rob, transaction.rob_flag, false,
                     lq_dequeued_before, sq_dequeued_before,
                     lq_cancels_before, sq_cancels_before) ||
                    environment.uncache_error_response_requests() !=
                        error_before + 1 ||
                    environment.uncache_denied_d_beats() !=
                        denied_before +
                            (*uncache_error_denied ? 1U : 0U) ||
                    environment.uncache_corrupt_d_beats() !=
                        corrupt_before + 1))) {
            return false;
        }
    } else {
        phase = "random-nc-store";
        auto transaction = make_store(
            nc_base + offset, random(), memblock::StoreOp::sd,
            random() % 2);
        transaction.cacheable = false;
        transaction.expected_debug_is_mmio = false;
        transaction.expected_debug_is_ncio = false;
        const std::uint64_t uncache_before =
            environment.uncache_requests();
        const std::uint64_t error_before =
            environment.uncache_error_response_requests();
        const std::uint64_t denied_before =
            environment.uncache_denied_d_beats();
        const std::uint64_t corrupt_before =
            environment.uncache_corrupt_d_beats();
        const std::uint64_t bus_before =
            environment.bus_expected_load(
                transaction.address, memblock::LoadOp::ld);
        const auto bus_errors_before = environment.bus_error_stats();
        const std::uint64_t lq_dequeued_before =
            environment.lq_dequeued();
        const std::uint64_t sq_dequeued_before =
            environment.sq_dequeued();
        const std::uint64_t lq_cancels_before =
            environment.lq_redirect_canceled_observed();
        const std::uint64_t sq_cancels_before =
            environment.sq_redirect_canceled_observed();
        if (uncache_error_denied) {
            environment.inject_next_uncache_response_error(true, false);
        }
        bool store_completed = false;
        if (uncache_error_denied) {
            environment.expect_store(transaction);
            // NC stores are already committed before the Uncache
            // request. A denied AccessAck is reported through
            // uncacheError and must not create a second ROB
            // writeback; the manager-side memory remains intact.
            const bool data_first = false;
            store_completed =
                environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) &&
                environment.enqueue_store(transaction) &&
                (data_first
                        ? environment.issue_store_data(
                              transaction, 4096) &&
                            environment.
                            issue_store_address_until_tlb_hit(
                                transaction,
                                constrained_completion_timeout)
                        : environment.issue_store_address_until_tlb_hit(
                              transaction,
                              constrained_completion_timeout) &&
                            environment.issue_store_data(
                                transaction, 4096)) &&
                environment.run_cycles(64) &&
                (environment.uncache_requests() != uncache_before ||
                    environment.wait_for_mmio_store_request(
                        transaction.rob, transaction.rob_flag, 8192)) &&
                environment.run_until_uncache_requests(
                    uncache_before + 1, 8192) &&
                environment.run_until_store_complete(
                    constrained_completion_timeout);
            if (store_completed) {
                coverage.sample(transaction, data_first);
                ++actions;
            }
        } else {
            store_completed =
                issue_store(transaction, (random() & 1U) != 0) &&
                environment.commit_store(transaction, 8192) &&
                environment.run_until_queues_retired(8192);
        }
        if (!store_completed ||
            (uncache_error_denied &&
                (!retire_random_nc_store_error(
                     lq_dequeued_before, sq_dequeued_before,
                     lq_cancels_before, sq_cancels_before) ||
                    environment.uncache_error_response_requests() !=
                        error_before + 1 ||
                    environment.uncache_denied_d_beats() !=
                        denied_before + 1 ||
                    environment.uncache_corrupt_d_beats() !=
                        corrupt_before ||
                    environment.bus_expected_load(
                        transaction.address,
                        memblock::LoadOp::ld) != bus_before ||
                    environment.bus_error_stats().uncache_reports !=
                        bus_errors_before.uncache_reports + 1 ||
                    environment.bus_error_stats().dcache_reports !=
                        bus_errors_before.dcache_reports ||
                    environment.bus_error_stats().last_uncache_address !=
                        (transaction.address & ~std::uint64_t{63}))) ||
            environment.uncache_requests() != uncache_before + 1) {
            return false;
        }
    }
    ++constraint_coverage.nc_directions[nc_store ? 1 : 0];
    constraint_coverage.sample_uncache(
        0, nc_store, uncache_error_denied);
    ++coverage.noncacheable;

    return true;
}

bool MixedScenario::execute_mmio(ActionWindow &window)
{
    auto &mmio_store = window.mmio_store;
    auto &uncache_error_denied = window.uncache_error_denied;
    auto &sample_dcache = window.sample_dcache;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;

    ordinary_leaf_addressed = true;
    const std::uint64_t offset = (random() % 128) * 8;
    if (uncache_error_denied &&
        !environment.run_until_queues_retired(8192)) {
        return false;
    }
    if (!mmio_store) {
        phase = "random-mmio-load";
        auto transaction = make_load(
            mmio_access_address() + offset,
            memblock::LoadOp::ld, random() % 3);
        transaction.oracle_address = mmio_physical + offset;
        transaction.expected_debug_is_mmio = true;
        transaction.expected_debug_is_ncio = false;
        transaction.expected_debug_is_perf_cnt = false;
        if (uncache_error_denied) {
            transaction.expected_exception_mask =
                *uncache_error_denied
                ? memblock::kExceptionLoadAccessFault
                : memblock::kExceptionHardwareError;
        }
        const std::uint64_t uncache_before =
            environment.uncache_requests();
        const std::uint64_t error_before =
            environment.uncache_error_response_requests();
        const std::uint64_t denied_before =
            environment.uncache_denied_d_beats();
        const std::uint64_t corrupt_before =
            environment.uncache_corrupt_d_beats();
        const std::uint64_t lq_dequeued_before =
            environment.lq_dequeued();
        const std::uint64_t sq_dequeued_before =
            environment.sq_dequeued();
        const std::uint64_t lq_cancels_before =
            environment.lq_redirect_canceled_observed();
        const std::uint64_t sq_cancels_before =
            environment.sq_redirect_canceled_observed();
        if (uncache_error_denied) {
            environment.inject_next_uncache_response_error(
                *uncache_error_denied,
                !*uncache_error_denied);
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(
                transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 4096) ||
            !environment.wait_for_mmio_request(
                transaction.rob, transaction.rob_flag, 8192) ||
            !environment.run_until_uncache_requests(
                uncache_before + 1, 8192) ||
            !environment.run_until_complete(
                constrained_completion_timeout) ||
            (uncache_error_denied
                    ? (!recover_random_precise_error(
                           transaction.rob, transaction.rob_flag, false,
                           lq_dequeued_before, sq_dequeued_before,
                           lq_cancels_before, sq_cancels_before) ||
                          environment.uncache_error_response_requests() !=
                              error_before + 1 ||
                          environment.uncache_denied_d_beats() !=
                              denied_before +
                                  (*uncache_error_denied ? 1U : 0U) ||
                          environment.uncache_corrupt_d_beats() !=
                              corrupt_before + 1)
                    : !environment.run_until_lq_retired(8192))) {
            return false;
        }
        coverage.sample(transaction);
    } else {
        phase = "random-mmio-store";
        auto transaction = make_store(
            mmio_access_address() + offset, random(),
            memblock::StoreOp::sd,
            random() % 2);
        transaction.expected_address_writeback_lane = 0;
        transaction.oracle_address = mmio_physical + offset;
        transaction.cacheable = false;
        transaction.expected_debug_is_mmio = false;
        transaction.expected_debug_is_ncio = false;
        if (uncache_error_denied) {
            transaction.expected_exception_mask =
                memblock::kExceptionStoreAccessFault;
            transaction.allow_preliminary_success_writeback = true;
        }
        const std::uint64_t uncache_before =
            environment.uncache_requests();
        const std::uint64_t error_before =
            environment.uncache_error_response_requests();
        const std::uint64_t denied_before =
            environment.uncache_denied_d_beats();
        const std::uint64_t corrupt_before =
            environment.uncache_corrupt_d_beats();
        const std::uint64_t bus_before =
            environment.bus_expected_load(
                *transaction.oracle_address,
                memblock::LoadOp::ld);
        const auto bus_errors_before = environment.bus_error_stats();
        const std::uint64_t lq_dequeued_before =
            environment.lq_dequeued();
        const std::uint64_t sq_dequeued_before =
            environment.sq_dequeued();
        const std::uint64_t lq_cancels_before =
            environment.lq_redirect_canceled_observed();
        const std::uint64_t sq_cancels_before =
            environment.sq_redirect_canceled_observed();
        if (uncache_error_denied) {
            environment.inject_next_uncache_response_error(true, false);
        }
        environment.expect_store(transaction);
        const bool prepared_mmio_store = environment.set_rob_head(
            transaction.rob, transaction.rob_flag);
        if (!prepared_mmio_store ||
            !environment.enqueue_store(transaction) ||
            !environment.issue_store_address_until_tlb_hit(
                transaction, constrained_completion_timeout) ||
            !environment.issue_store_data(transaction, 4096) ||
            !environment.run_cycles(64) ||
            (environment.uncache_requests() == uncache_before &&
                !environment.wait_for_mmio_store_request(
                    transaction.rob, transaction.rob_flag, 8192)) ||
            !environment.run_until_uncache_requests(
                uncache_before + 1, 8192) ||
            !environment.run_until_store_complete(
                constrained_completion_timeout) ||
            (uncache_error_denied
                    ? (!recover_random_precise_error(
                           transaction.rob, transaction.rob_flag, true,
                           lq_dequeued_before, sq_dequeued_before,
                           lq_cancels_before, sq_cancels_before) ||
                          environment.uncache_error_response_requests() !=
                              error_before + 1 ||
                          environment.uncache_denied_d_beats() !=
                              denied_before + 1 ||
                          environment.uncache_corrupt_d_beats() !=
                              corrupt_before ||
                          environment.bus_expected_load(
                              *transaction.oracle_address,
                              memblock::LoadOp::ld) != bus_before ||
                          environment.bus_error_stats().uncache_reports !=
                              bus_errors_before.uncache_reports + 1 ||
                          environment.bus_error_stats().dcache_reports !=
                              bus_errors_before.dcache_reports ||
                          environment.bus_error_stats().
                              last_uncache_address !=
                              (*transaction.oracle_address &
                                  ~std::uint64_t{63}))
                    : (!environment.commit_stores_through(
                           transaction, 1) ||
                          !environment.run_cycles(16)))) {
            std::ostringstream detail;
            detail << "mmio-store"
                   << ":tlb_feedbacks="
                   << environment.store_tlb_feedbacks()
                   << ":tlb_misses=" << environment.store_tlb_misses()
                   << ":uncache=" << environment.uncache_requests()
                   << ":store_mmio_valid="
                   << environment.store_mmio_valid();
            phase = detail.str();
            return false;
        }
        if (!uncache_error_denied) {
            environment.record_committed_store(transaction);
        }
        coverage.sample(transaction, false);
    }
    ++actions;
    ++constraint_coverage.mmio_directions[mmio_store ? 1 : 0];
    constraint_coverage.sample_uncache(
        1, mmio_store, uncache_error_denied);
    ++coverage.noncacheable;
    sample_dcache = false;

    return true;
}

} // namespace memblock::scenarios::mixed
