// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_mmio_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50038000ULL;
    constexpr std::uint64_t physical_base = 0x90038000ULL;
    constexpr std::uint64_t root = 0x9700c000ULL;
    environment.memory().fill_incrementing(physical_base, 64, 0x2d);
    environment.configure_backpressure(0x1f83d9abfb41bd6bULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false,
            false, true) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    unsigned normal_count = 0;
    unsigned fault_count = 0;
    unsigned load_mmio_before_response = 0;
    auto run_case = [&](std::uint8_t rob, std::uint8_t lq,
                        std::uint8_t pdest, std::uint64_t offset,
                        std::uint32_t exception, bool denied, bool corrupt) {
        const memblock::LoadTransaction transaction{
            .address = virtual_base + offset,
            .oracle_address = physical_base + offset,
            .op = memblock::LoadOp::ld,
            .rob = rob,
            .lq = lq,
            .pdest = pdest,
            .lane = static_cast<unsigned>(rob % memblock::kScalarLoadLanes),
            .expected_exception_mask = exception,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        const auto load_mmio_before = environment.load_mmio_stats();
        environment.expect_load(transaction);
        environment.force_next_uncache_response_delay(64U << rob);
        if (denied || corrupt) {
            environment.inject_next_uncache_response_error(denied, corrupt);
        }
        const std::uint64_t dcache_before = environment.tilelink_requests();
        if (!environment.set_rob_head(transaction.rob) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.wait_for_mmio_request(
                transaction.rob, transaction.rob_flag, 4096)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=request rob="
                      << static_cast<unsigned>(rob) << " reason="
                      << environment.error() << '\n';
            return false;
        }
        const auto load_mmio_at_request = environment.load_mmio_stats();
        // LoadQueueUncache compacts valid requests in ROB order before
        // publishing this Vec, so a single valid request always occupies
        // output slot zero regardless of its originating load-unit lane.
        constexpr unsigned metadata_slot = 0;
        bool metadata_matches =
            load_mmio_at_request.last_rob[metadata_slot] == transaction.rob;
        for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
            metadata_matches = metadata_matches &&
                load_mmio_at_request.pulses[lane] ==
                    load_mmio_before.pulses[lane] +
                        (lane == metadata_slot ? 1U : 0U);
        }
        if (!metadata_matches) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=load-mmio-metadata rob="
                      << static_cast<unsigned>(rob) << " lane="
                      << transaction.lane << " slot=" << metadata_slot
                      << " pulses="
                      << load_mmio_at_request.pulses[0] << ','
                      << load_mmio_at_request.pulses[1] << ','
                      << load_mmio_at_request.pulses[2] << " last_rob="
                      << static_cast<unsigned>(
                             load_mmio_at_request.last_rob[metadata_slot])
                      << '\n';
            return false;
        }
        ++load_mmio_before_response;
        if (!environment.run_until_complete(8192)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=writeback rob="
                      << static_cast<unsigned>(rob) << " reason="
                      << environment.error() << '\n';
            return false;
        }
        if (environment.tilelink_requests() != dcache_before) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=dcache-bypass rob="
                      << static_cast<unsigned>(rob) << " reason=MMIO issued a DCache request\n";
            return false;
        }
        bool completed = false;
        if (exception == 0) {
            ++normal_count;
            completed = environment.run_until_lq_retired(2048);
        } else {
            ++fault_count;
            completed = environment.run_cycles(8) &&
                environment.redirect_after(
                    transaction.rob, transaction.rob_flag, true) &&
                environment.run_cycles(96);
            if (completed &&
                environment.lq_dequeued() + environment.lq_canceled() <
                    environment.lq_allocated()) {
                completed = environment.account_lq_cancellation(1);
            }
        }
        const auto &load_mmio_after = environment.load_mmio_stats();
        if (!completed ||
            load_mmio_after.pulses != load_mmio_at_request.pulses ||
            load_mmio_after.last_rob != load_mmio_at_request.last_rob ||
            load_mmio_after.last_cycle != load_mmio_at_request.last_cycle) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=load-mmio-duplicate-check rob="
                      << static_cast<unsigned>(rob) << " pulses="
                      << load_mmio_after.pulses[0] << ','
                      << load_mmio_after.pulses[1] << ','
                      << load_mmio_after.pulses[2] << " reason="
                      << environment.error() << '\n';
            return false;
        }
        return true;
    };

    if (!run_case(0, 0, 150, 8, 0, false, false) ||
        !run_case(1, 1, 151, 16, memblock::kExceptionLoadAccessFault,
                  true, false) ||
        !run_case(2, 2, 152, 24, memblock::kExceptionHardwareError,
                  false, true)) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=final reason="
                  << environment.error() << '\n';
        return 1;
    }

    unsigned load_mmio_output_slots = 0;
    {
        // In bare mode, three same-cycle MMIO classifications avoid TLB
        // replay skew and exercise every ROB-facing compacted output slot.
        // pendingMMIOld stays low, so no external request can race the check.
        memblock::Environment parallel(argc, argv);
        constexpr std::uint64_t parallel_base = 0x35000200ULL;
        if (!parallel.reset() || !parallel.set_rob_head(40)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << parallel.cycle()
                      << " phase=load-mmio-parallel-configuration reason="
                      << parallel.error() << '\n';
            return 1;
        }
        const std::vector<memblock::LoadTransaction> transactions{
            memblock::LoadTransaction{
                .address = parallel_base + 0x08,
                .oracle_address = parallel_base + 0x08,
                .op = memblock::LoadOp::ld,
                .rob = 24,
                .lq = 0,
                .pdest = 155,
                .lane = 2,
            },
            memblock::LoadTransaction{
                .address = parallel_base + 0x10,
                .oracle_address = parallel_base + 0x10,
                .op = memblock::LoadOp::ld,
                .rob = 25,
                .lq = 1,
                .pdest = 156,
                .lane = 0,
            },
            memblock::LoadTransaction{
                .address = parallel_base + 0x18,
                .oracle_address = parallel_base + 0x18,
                .op = memblock::LoadOp::ld,
                .rob = 26,
                .lq = 2,
                .pdest = 157,
                .lane = 1,
            },
        };
        const auto metadata_before = parallel.load_mmio_stats();
        const std::uint64_t uncache_before = parallel.uncache_requests();
        if (!parallel.enqueue_load_batch(transactions, {0, 1, 2}) ||
            !parallel.issue_load_batch(transactions, 2048, true)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << parallel.cycle()
                      << " phase=load-mmio-parallel-issue reason="
                      << parallel.error() << '\n';
            return 1;
        }
        bool all_slots_observed = false;
        for (unsigned cycle = 0; cycle < 128 && !all_slots_observed; ++cycle) {
            if (!parallel.run_cycles(1)) {
                break;
            }
            const auto &metadata = parallel.load_mmio_stats();
            all_slots_observed = true;
            const std::uint64_t classification_cycle = metadata.last_cycle[0];
            for (unsigned slot = 0; slot < memblock::kScalarLoadLanes; ++slot) {
                all_slots_observed = all_slots_observed &&
                    metadata.pulses[slot] == metadata_before.pulses[slot] + 1 &&
                    metadata.last_rob[slot] == transactions[slot].rob &&
                    metadata.last_cycle[slot] == classification_cycle;
            }
        }
        const auto metadata_after = parallel.load_mmio_stats();
        if (!all_slots_observed ||
            parallel.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << parallel.cycle()
                      << " phase=load-mmio-parallel-check pulses="
                      << metadata_after.pulses[0] << ','
                      << metadata_after.pulses[1] << ','
                      << metadata_after.pulses[2] << " robs="
                      << static_cast<unsigned>(metadata_after.last_rob[0]) << ','
                      << static_cast<unsigned>(metadata_after.last_rob[1]) << ','
                      << static_cast<unsigned>(metadata_after.last_rob[2])
                      << " uncache_delta="
                      << parallel.uncache_requests() - uncache_before
                      << " reason=" << parallel.error() << '\n';
            return 1;
        }
        if (!parallel.redirect_after(23, false, false) ||
            !parallel.run_cycles(32) ||
            parallel.uncache_requests() != uncache_before ||
            parallel.load_mmio_stats().pulses != metadata_after.pulses ||
            parallel.lq_dequeued() + parallel.lq_canceled() !=
                parallel.lq_allocated()) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << parallel.cycle()
                      << " phase=load-mmio-parallel-cleanup retired="
                      << parallel.lq_dequeued() << '+'
                      << parallel.lq_canceled() << '/'
                      << parallel.lq_allocated() << " reason="
                      << parallel.error() << '\n';
            return 1;
        }
        load_mmio_output_slots = memblock::kScalarLoadLanes;
    }

    std::uint64_t non_mmio_control_cycles = 0;
    {
        memblock::Environment non_mmio(argc, argv);
        constexpr std::uint64_t control_virtual = 0x54000000ULL;
        constexpr std::uint64_t control_physical = 0xa4000000ULL;
        constexpr std::uint64_t control_root = 0x97200000ULL;
        non_mmio.memory().fill_incrementing(control_physical, 0x2000, 0x4b);
        if (!non_mmio.reset() ||
            !non_mmio.map_sv39_4k(
                control_virtual, control_physical, control_root) ||
            !non_mmio.map_sv39_4k(
                control_virtual + 0x1000, control_physical + 0x1000,
                control_root, true, true, false, false, true, false) ||
            !non_mmio.set_page_based_memory_types(true, false) ||
            !non_mmio.activate_sv39(control_root, 45)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << non_mmio.cycle()
                      << " phase=load-mmio-negative-configuration reason="
                      << non_mmio.error() << '\n';
            return 1;
        }
        const std::vector<memblock::LoadTransaction> controls{
            memblock::LoadTransaction{
                .address = control_virtual + 0x18,
                .oracle_address = control_physical + 0x18,
                .op = memblock::LoadOp::ld,
                .rob = 16,
                .lq = 0,
                .pdest = 153,
                .lane = 0,
            },
            memblock::LoadTransaction{
                .address = control_virtual + 0x1018,
                .oracle_address = control_physical + 0x1018,
                .op = memblock::LoadOp::ld,
                .rob = 17,
                .lq = 1,
                .pdest = 154,
                .lane = 1,
            },
        };
        for (const auto &control : controls) {
            non_mmio.expect_load(control);
        }
        const auto metadata_before = non_mmio.load_mmio_stats();
        const std::uint64_t dcache_before = non_mmio.tilelink_requests();
        const std::uint64_t uncache_before = non_mmio.uncache_requests();
        if (!non_mmio.set_rob_head(
                controls.front().rob, controls.front().rob_flag) ||
            !non_mmio.enqueue_load_batch(controls, {0, 1}) ||
            !non_mmio.issue_load_batch(controls, 2048, true) ||
            !non_mmio.run_until_complete(16384) ||
            !non_mmio.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << non_mmio.cycle()
                      << " phase=load-mmio-negative-complete reason="
                      << non_mmio.error() << '\n';
            return 1;
        }
        const auto &metadata_after = non_mmio.load_mmio_stats();
        if (metadata_after.pulses != metadata_before.pulses ||
            non_mmio.tilelink_requests() != dcache_before + 1 ||
            non_mmio.uncache_requests() != uncache_before + 1) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << non_mmio.cycle()
                      << " phase=load-mmio-negative-check pulses="
                      << metadata_after.pulses[0] << ','
                      << metadata_after.pulses[1] << ','
                      << metadata_after.pulses[2] << " dcache_delta="
                      << non_mmio.tilelink_requests() - dcache_before
                      << " uncache_delta="
                      << non_mmio.uncache_requests() - uncache_before << '\n';
            return 1;
        }
        non_mmio_control_cycles = non_mmio.cycle();
    }

    // MMIO stores are committed through StoreQueue's uncache state machine;
    // unlike scalar MMIO loads they do not produce an address writeback until
    // the request/response sequence has completed.  Check both the absence
    // of DCache traffic and the final MMIO metadata on that delayed pulse.
    const memblock::StoreTransaction mmio_store{
        .address = virtual_base + 0x28,
        .oracle_address = physical_base + 0x28,
        .data = 0x0123456789abcdefULL,
        .op = memblock::StoreOp::sd,
        .rob = 3,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
        // The PBMT=IO page is backed by the configured DDR PMA region.  The
        // StoreQueue writeback therefore reports memBackTypeMM=1 and
        // debug.isMMIO=0; PMA-IO would be a separate device-map test.
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
    };
    const std::uint64_t dcache_before_store = environment.tilelink_requests();
    const std::uint64_t uncache_before_store = environment.uncache_requests();
    const std::uint64_t store_misses_before = environment.store_tlb_misses();
    environment.expect_store(mmio_store);
    if (!environment.set_rob_head(mmio_store.rob, mmio_store.rob_flag) ||
        !environment.enqueue_store(mmio_store, 0) ||
        !environment.issue_store_address(mmio_store, 2048) ||
        !environment.run_until_store_tlb_misses(store_misses_before + 1, 4096) ||
        !environment.run_cycles(256) ||
        !environment.issue_store_address(mmio_store, 2048) ||
        !environment.issue_store_data(mmio_store, 2048) ||
        !environment.run_cycles(64) ||
        !environment.wait_for_mmio_store_request(
            mmio_store.rob, mmio_store.rob_flag, 8192) ||
        !environment.run_until_store_complete(8192) ||
        !environment.commit_stores_through(mmio_store, 1) ||
        !environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=store-writeback reason="
                  << environment.error()
                  << " store_mmio_valid=" << environment.store_mmio_valid()
                  << " store_mmio_rob="
                  << static_cast<unsigned>(environment.store_mmio_rob())
                  << " store_tlb_feedbacks="
                  << environment.store_tlb_feedbacks()
                  << " store_tlb_misses=" << environment.store_tlb_misses()
                  << " uncache_requests=" << environment.uncache_requests()
                  << '\n';
        return 1;
    }
    environment.record_committed_store(mmio_store);
    if (environment.sq_dequeued() != environment.sq_allocated()) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=store-retirement reason="
                  << "MMIO store did not leave SQ"
                  << " sq_dequeued=" << environment.sq_dequeued()
                  << " sq_allocated=" << environment.sq_allocated() << '\n';
        return 1;
    }
    if (environment.tilelink_requests() != dcache_before_store ||
        environment.uncache_requests() != uncache_before_store + 1) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=store-bypass reason="
                  << "MMIO store used the wrong manager path"
                  << " dcache_requests=" << environment.tilelink_requests()
                  << " uncache_requests=" << environment.uncache_requests()
                  << '\n';
        return 1;
    }

    // Exercise the SoC's physical PMA device window independently of PBMT=IO.
    // The 0x30050000..0x3801ffff PMA interval has c=0 and is outside the
    // DebugModule-only access guard, so normal CPU accesses are classified as
    // MMIO from the physical PMA response.
    unsigned pma_load_count = 0;
    unsigned pma_store_count = 0;
    unsigned pma_denied_count = 0;
    std::uint64_t pma_denied_wakeups = 0;
    std::uint64_t pma_denied_cancels = 0;
    unsigned pma_debug_load_count = 0;
    unsigned pma_debug_store_count = 0;
    unsigned pma_edge_load_count = 0;
    std::uint64_t pma_edge_dcache_requests = 0;
    std::uint64_t pma_edge_uncache_requests = 0;
    std::uint64_t pma_cycles = 0;
    {
        memblock::Environment pma_environment(argc, argv);
        constexpr std::uint64_t pma_physical_base = 0x35000000ULL;
        constexpr std::uint64_t debug_physical_base = 0x38020000ULL;
        pma_environment.memory().fill_incrementing(
            pma_physical_base, 64, 0x63);
        pma_environment.memory().fill_incrementing(
            debug_physical_base, 64, 0x91);
        pma_environment.memory().fill_incrementing(
            0x7ffffff8ULL, 8, 0xb3);
        pma_environment.memory().fill_incrementing(
            0x80000000ULL, 8, 0xc7);
        pma_environment.configure_backpressure(
            0x5a17c3e9d2b84f61ULL, true);
        if (!pma_environment.reset() ||
            !pma_environment.activate_bare(43)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-configuration reason="
                      << pma_environment.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction pma_load{
            .address = pma_physical_base + 0x10,
            .oracle_address = pma_physical_base + 0x10,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 158,
            .lane = 0,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        pma_environment.expect_load(pma_load);
        const std::uint64_t pma_dcache_before =
            pma_environment.tilelink_requests();
        if (!pma_environment.set_rob_head(pma_load.rob) ||
            !pma_environment.enqueue_load(pma_load) ||
            !pma_environment.issue_load(pma_load, 2048) ||
            !pma_environment.wait_for_mmio_request(
                pma_load.rob, pma_load.rob_flag, 4096) ||
            !pma_environment.run_until_complete(8192) ||
            !pma_environment.run_until_lq_retired(2048) ||
            pma_environment.tilelink_requests() != pma_dcache_before) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-load reason="
                      << pma_environment.error() << '\n';
            return 1;
        }
        ++pma_load_count;

        const memblock::StoreTransaction pma_store{
            .address = pma_physical_base + 0x28,
            .oracle_address = pma_physical_base + 0x28,
            .data = 0x8877665544332211ULL,
            .op = memblock::StoreOp::sd,
            .rob = 1,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t pma_uncache_before =
            pma_environment.uncache_requests();
        pma_environment.expect_store(pma_store);
        if (!pma_environment.set_rob_head(pma_store.rob) ||
            !pma_environment.enqueue_store(pma_store, 0) ||
            !pma_environment.issue_store_address(pma_store, 2048) ||
            !pma_environment.issue_store_data(pma_store, 2048) ||
            !pma_environment.run_cycles(64) ||
            !pma_environment.wait_for_mmio_store_request(
                pma_store.rob, pma_store.rob_flag, 8192) ||
            !pma_environment.run_until_store_complete(8192) ||
            !pma_environment.commit_stores_through(pma_store, 1) ||
            !pma_environment.run_cycles(16) ||
            pma_environment.uncache_requests() != pma_uncache_before + 1) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-store reason="
                      << pma_environment.error() << '\n';
            return 1;
        }
        pma_environment.record_committed_store(pma_store);
        if (pma_environment.sq_dequeued() != pma_environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-store-retirement reason="
                      << "PMA MMIO store did not leave SQ\n";
            return 1;
        }
        ++pma_store_count;

        // The DebugModule PMA interval is c=0 but is additionally guarded by
        // the CSR debug-mode bit.  A normal CPU access must report an access
        // fault and must not leak a request onto either memory manager.
        const memblock::LoadTransaction pma_denied{
            .address = 0x38020010ULL,
            .oracle_address = 0x38020010ULL,
            .op = memblock::LoadOp::ld,
            .rob = 2,
            .lq = 1,
            .pdest = 159,
            .lane = 0,
            .expected_exception_mask = memblock::kExceptionLoadAccessFault,
        };
        const std::uint64_t denied_dcache_before =
            pma_environment.tilelink_requests();
        const std::uint64_t denied_uncache_before =
            pma_environment.uncache_requests();
        const auto denied_feedback_before =
            pma_environment.scalar_load_feedback_stats();
        pma_environment.expect_load(pma_denied);
        if (!pma_environment.set_rob_head(pma_denied.rob) ||
            !pma_environment.enqueue_load(pma_denied) ||
            !pma_environment.issue_load(pma_denied, 2048) ||
            !pma_environment.run_until_complete(8192) ||
            !pma_environment.run_until_lq_retired(2048) ||
            pma_environment.tilelink_requests() != denied_dcache_before ||
            pma_environment.uncache_requests() != denied_uncache_before) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-debug-denied reason="
                      << pma_environment.error() << '\n';
            return 1;
        }
        const auto denied_feedback_after =
            pma_environment.scalar_load_feedback_stats();
        for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
            pma_denied_wakeups +=
                denied_feedback_after.wakeups[lane] -
                denied_feedback_before.wakeups[lane];
            pma_denied_cancels +=
                denied_feedback_after.ld2_cancels[lane] -
                denied_feedback_before.ld2_cancels[lane];
        }
        if (pma_denied_cancels == 0 ||
            pma_denied_wakeups != pma_denied_cancels) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-debug-feedback wakeups="
                      << pma_denied_wakeups << " cancels="
                      << pma_denied_cancels << '\n';
            return 1;
        }
        ++pma_denied_count;

        // Entering Debug Mode makes the same PMA entry visible. It remains an
        // uncached device mapping, so both directions must use Uncache rather
        // than DCache and complete without an access fault.
        if (!pma_environment.set_debug_mode(true)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-debug-enable reason="
                      << pma_environment.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction pma_debug_load{
            .address = debug_physical_base + 0x10,
            .oracle_address = debug_physical_base + 0x10,
            .op = memblock::LoadOp::ld,
            .rob = 3,
            .lq = 2,
            .pdest = 160,
            .lane = 0,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        const std::uint64_t debug_load_dcache_before =
            pma_environment.tilelink_requests();
        const std::uint64_t debug_load_uncache_before =
            pma_environment.uncache_requests();
        pma_environment.expect_load(pma_debug_load);
        if (!pma_environment.set_rob_head(pma_debug_load.rob) ||
            !pma_environment.enqueue_load(pma_debug_load) ||
            !pma_environment.issue_load(pma_debug_load, 2048) ||
            !pma_environment.wait_for_mmio_request(
                pma_debug_load.rob, pma_debug_load.rob_flag, 4096) ||
            !pma_environment.run_until_complete(8192) ||
            !pma_environment.run_until_lq_retired(2048) ||
            pma_environment.tilelink_requests() != debug_load_dcache_before ||
            pma_environment.uncache_requests() !=
                debug_load_uncache_before + 1) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-debug-load reason="
                      << pma_environment.error()
                      << " dcache_requests="
                      << pma_environment.tilelink_requests()
                      << " uncache_requests="
                      << pma_environment.uncache_requests() << '\n';
            return 1;
        }
        ++pma_debug_load_count;

        const memblock::StoreTransaction pma_debug_store{
            .address = debug_physical_base + 0x28,
            .oracle_address = debug_physical_base + 0x28,
            .data = 0xfedcba9876543210ULL,
            .op = memblock::StoreOp::sd,
            .rob = 4,
            .sq = 1,
            .address_lane = 0,
            .data_lane = 1,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t debug_store_dcache_before =
            pma_environment.tilelink_requests();
        const std::uint64_t debug_store_uncache_before =
            pma_environment.uncache_requests();
        pma_environment.expect_store(pma_debug_store);
        if (!pma_environment.set_rob_head(pma_debug_store.rob) ||
            !pma_environment.enqueue_store(pma_debug_store, 0) ||
            !pma_environment.issue_store_address(pma_debug_store, 2048) ||
            !pma_environment.issue_store_data(pma_debug_store, 2048) ||
            !pma_environment.run_cycles(64) ||
            !pma_environment.wait_for_mmio_store_request(
                pma_debug_store.rob, pma_debug_store.rob_flag, 8192) ||
            !pma_environment.run_until_store_complete(8192) ||
            !pma_environment.commit_stores_through(pma_debug_store, 1) ||
            !pma_environment.run_cycles(16) ||
            pma_environment.tilelink_requests() != debug_store_dcache_before ||
            pma_environment.uncache_requests() !=
                debug_store_uncache_before + 1 ||
            pma_environment.bus_expected_load(
                pma_debug_store.address, memblock::LoadOp::ld) !=
                pma_debug_store.data) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-debug-store reason="
                      << pma_environment.error()
                      << " dcache_requests="
                      << pma_environment.tilelink_requests()
                      << " uncache_requests="
                      << pma_environment.uncache_requests() << '\n';
            return 1;
        }
        pma_environment.record_committed_store(pma_debug_store);
        if (pma_environment.sq_dequeued() != pma_environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-debug-store-retirement reason="
                      << "Debug PMA MMIO store did not leave SQ\n";
            return 1;
        }
        ++pma_debug_store_count;

        auto run_pma_edge_load = [&pma_environment, &pma_edge_load_count,
                                  &pma_edge_dcache_requests,
                                  &pma_edge_uncache_requests](
                                     const char *name,
                                     std::uint64_t address,
                                     std::uint8_t rob,
                                     std::uint8_t lq,
                                     std::uint8_t pdest,
                                     bool expected_mmio) {
            const memblock::LoadTransaction transaction{
                .address = address,
                .oracle_address = address,
                .op = memblock::LoadOp::ld,
                .rob = rob,
                .lq = lq,
                .pdest = pdest,
                .lane = static_cast<unsigned>(lq % memblock::kScalarLoadLanes),
                .expected_debug_is_mmio = expected_mmio,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            };
            const std::uint64_t dcache_before =
                pma_environment.tilelink_requests();
            const std::uint64_t uncache_before =
                pma_environment.uncache_requests();
            pma_environment.expect_load(transaction);
            if (!pma_environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !pma_environment.enqueue_load(transaction) ||
                !pma_environment.issue_load(transaction, 2048) ||
                (expected_mmio && !pma_environment.wait_for_mmio_request(
                    transaction.rob, transaction.rob_flag, 4096)) ||
                !pma_environment.run_until_complete(8192) ||
                !pma_environment.run_until_lq_retired(2048)) {
                std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                          << pma_environment.cycle() << " phase=" << name
                          << " reason=" << pma_environment.error() << '\n';
                return false;
            }
            const std::uint64_t dcache_delta =
                pma_environment.tilelink_requests() - dcache_before;
            const std::uint64_t uncache_delta =
                pma_environment.uncache_requests() - uncache_before;
            if (dcache_delta != (expected_mmio ? 0U : 1U) ||
                uncache_delta != (expected_mmio ? 1U : 0U)) {
                std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                          << pma_environment.cycle() << " phase=" << name
                          << " dcache_delta=" << dcache_delta
                          << " uncache_delta=" << uncache_delta << '\n';
                return false;
            }
            ++pma_edge_load_count;
            pma_edge_dcache_requests += dcache_delta;
            pma_edge_uncache_requests += uncache_delta;
            return true;
        };
        if (!run_pma_edge_load(
                "pma-edge-device", 0x7ffffff8ULL, 5, 3, 161, true) ||
            !run_pma_edge_load(
                "pma-edge-ddr", 0x80000000ULL, 6, 4, 162, false)) {
            return 1;
        }
        pma_cycles = pma_environment.cycle();
    }

    unsigned device_access_count = 0;
    std::uint64_t device_cycles = 0;
    unsigned queued_device_access_count = 0;
    std::uint64_t queued_device_cycles = 0;
    std::uint64_t queued_device_max_outstanding = 0;
    unsigned mixed_device_access_count = 0;
    std::uint64_t mixed_device_cycles = 0;
    std::uint64_t mixed_device_max_outstanding = 0;
    {
        memblock::Environment device(argc, argv);
        constexpr std::uint64_t device_base = 0x35000100ULL;
        constexpr std::uint64_t initial_value = 0x8877665544332211ULL;
        constexpr std::uint32_t denied_store_value = 0xdeadbeefU;
        constexpr std::uint32_t store_value = 0xa1b2c3d4U;
        constexpr std::uint64_t denied_store_bus_data =
            (std::uint64_t{denied_store_value} << 32) | denied_store_value;
        constexpr std::uint64_t store_bus_data =
            (std::uint64_t{store_value} << 32) | store_value;
        constexpr std::uint64_t stored_beat =
            std::uint64_t{store_value} << 32;
        device.memory().write_u64(device_base, initial_value);
        device.configure_uncache_device(device_base, 16, true);
        device.configure_backpressure(0x6c8e9cf570932bd5ULL, true);
        if (!device.reset() || !device.activate_bare(44)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << device.cycle()
                      << " phase=device-configuration reason="
                      << device.error() << '\n';
            return 1;
        }

        auto run_device_load = [&device](
                                   const char *name, std::uint8_t rob,
                                   std::uint8_t lq, std::uint8_t pdest,
                                   std::uint64_t expected,
                                   std::uint32_t expected_exception,
                                   bool denied, bool corrupt) {
            const memblock::LoadTransaction load{
                .address = device_base,
                .oracle_address = device_base,
                .op = memblock::LoadOp::ld,
                .rob = rob,
                .lq = lq,
                .pdest = pdest,
                .lane = static_cast<unsigned>(lq % memblock::kScalarLoadLanes),
                .expected_exception_mask = expected_exception,
                .expected_debug_is_mmio = true,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            };
            device.expect_load_data(load, expected);
            if (denied || corrupt) {
                device.inject_next_uncache_response_error(denied, corrupt);
            }
            if (!device.set_rob_head(load.rob, load.rob_flag) ||
                !device.enqueue_load(load) ||
                !device.issue_load(load, 2048) ||
                !device.wait_for_mmio_request(load.rob, load.rob_flag, 4096) ||
                !device.run_until_complete(8192)) {
                std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                          << device.cycle() << " phase=" << name
                          << " reason=" << device.error() << '\n';
                return false;
            }
            if (expected_exception == 0) {
                return device.run_until_lq_retired(2048);
            }
            if (!device.run_cycles(8) ||
                !device.redirect_after(load.rob, load.rob_flag, true) ||
                !device.run_cycles(96)) {
                return false;
            }
            return device.lq_dequeued() + device.lq_canceled() >=
                       device.lq_allocated() ||
                device.account_lq_cancellation(1);
        };

        if (!run_device_load(
                "device-denied-read", 0, 0, 163, 0,
                memblock::kExceptionLoadAccessFault, true, false) ||
            device.bus_expected_load(device_base, memblock::LoadOp::ld) !=
                initial_value ||
            !run_device_load(
                "device-corrupt-read", 1, 1, 164, 0,
                memblock::kExceptionHardwareError, false, true) ||
            device.bus_expected_load(device_base, memblock::LoadOp::ld) !=
                initial_value ||
            !run_device_load(
                "device-read-old", 2, 2, 165, initial_value, 0,
                false, false) ||
            device.bus_expected_load(device_base, memblock::LoadOp::ld) != 0 ||
            !run_device_load(
                "device-read-cleared", 3, 3, 166, 0, 0,
                false, false)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << device.cycle()
                      << " phase=device-read-error-policy bus_value=0x"
                      << std::hex
                      << device.bus_expected_load(
                             device_base, memblock::LoadOp::ld)
                      << std::dec << " reason=" << device.error() << '\n';
            return 1;
        }

        auto run_device_store = [&device](
                                    const char *name,
                                    const memblock::StoreTransaction &store,
                                    bool denied, bool corrupt) {
            device.expect_store(store);
            if (denied || corrupt) {
                device.inject_next_uncache_response_error(denied, corrupt);
            }
            if (!device.set_rob_head(store.rob, store.rob_flag) ||
                !device.enqueue_store(store, 0) ||
                !device.issue_store_address(store, 2048) ||
                !device.issue_store_data(store, 2048) ||
                !device.run_cycles(64) ||
                !device.wait_for_mmio_store_request(
                    store.rob, store.rob_flag, 8192) ||
                !device.run_until_store_complete(8192)) {
                std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                          << device.cycle() << " phase=" << name
                          << " reason=" << device.error() << '\n';
                return false;
            }
            if (store.expected_exception_mask == 0) {
                if (!device.commit_stores_through(store, 1) ||
                    !device.run_cycles(16)) {
                    return false;
                }
                device.record_committed_store(store);
            } else if (!device.run_cycles(8) ||
                       !device.redirect_after(
                           store.rob, store.rob_flag, true) ||
                       !device.run_cycles(96) ||
                       (device.sq_dequeued() + device.sq_canceled() <
                            device.sq_allocated() &&
                        !device.account_sq_cancellation(1))) {
                return false;
            }
            return device.sq_dequeued() + device.sq_canceled() ==
                device.sq_allocated();
        };

        const memblock::StoreTransaction denied_device_store{
            .address = device_base + 4,
            .oracle_address = device_base + 4,
            .data = denied_store_value,
            .op = memblock::StoreOp::sw,
            .rob = 4,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
            .expected_exception_mask = memblock::kExceptionStoreAccessFault,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
        };
        const memblock::StoreTransaction device_store{
            .address = device_base + 4,
            .oracle_address = device_base + 4,
            .data = store_value,
            .op = memblock::StoreOp::sw,
            .rob = 5,
            .sq = 1,
            .address_lane = 0,
            .data_lane = 1,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
        };
        if (!run_device_store(
                "device-denied-write", denied_device_store, true, false) ||
            device.bus_expected_load(device_base, memblock::LoadOp::ld) != 0 ||
            !run_device_store(
                "device-partial-write", device_store, false, false) ||
            device.bus_expected_load(device_base, memblock::LoadOp::ld) !=
                stored_beat ||
            !run_device_load(
                "device-read-written", 6, 4, 167, stored_beat, 0,
                false, false)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << device.cycle()
                      << " phase=device-write-error-policy bus_value=0x"
                      << std::hex
                      << device.bus_expected_load(
                             device_base, memblock::LoadOp::ld)
                      << " expected=0x" << stored_beat << std::dec
                      << " reason=" << device.error() << '\n';
            return 1;
        }

        const auto &accesses = device.uncache_device_accesses();
        if (device.tilelink_requests() != 0 || accesses.size() != 7 ||
            accesses[0].sequence != 0 || accesses[0].write ||
            accesses[0].address != device_base || accesses[0].size != 3 ||
            accesses[0].mask != 0xff ||
            accesses[0].read_data != initial_value ||
            accesses[1].sequence != 1 || accesses[1].write ||
            accesses[1].address != device_base || accesses[1].size != 3 ||
            accesses[1].mask != 0xff ||
            accesses[1].read_data != initial_value ||
            accesses[2].sequence != 2 || accesses[2].write ||
            accesses[2].address != device_base || accesses[2].size != 3 ||
            accesses[2].mask != 0xff ||
            accesses[2].read_data != initial_value ||
            accesses[3].sequence != 3 || accesses[3].write ||
            accesses[3].address != device_base || accesses[3].size != 3 ||
            accesses[3].mask != 0xff || accesses[3].read_data != 0 ||
            accesses[4].sequence != 4 || !accesses[4].write ||
            accesses[4].address != device_base + 4 ||
            accesses[4].size != 2 || accesses[4].mask != 0xf0 ||
            accesses[4].data != denied_store_bus_data ||
            accesses[5].sequence != 5 || !accesses[5].write ||
            accesses[5].address != device_base + 4 ||
            accesses[5].size != 2 || accesses[5].mask != 0xf0 ||
            accesses[5].data != store_bus_data ||
            accesses[6].sequence != 6 || accesses[6].write ||
            accesses[6].address != device_base || accesses[6].size != 3 ||
            accesses[6].mask != 0xff ||
            accesses[6].read_data != stored_beat ||
            !accesses[0].denied || !accesses[0].corrupt ||
            accesses[1].denied || !accesses[1].corrupt ||
            accesses[2].denied || accesses[2].corrupt ||
            accesses[3].denied || accesses[3].corrupt ||
            !accesses[4].denied || accesses[4].corrupt ||
            accesses[5].denied || accesses[5].corrupt ||
            accesses[6].denied || accesses[6].corrupt ||
            device.bus_expected_load(device_base, memblock::LoadOp::ld) != 0) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << device.cycle() << " phase=device-access-log"
                      << " accesses=" << accesses.size()
                      << " dcache_requests=" << device.tilelink_requests()
                      << " reason=device request order or payload mismatch";
            for (const auto &access : accesses) {
                std::cerr << " [sequence=" << access.sequence
                          << " write=" << access.write
                          << " address=0x" << std::hex << access.address
                          << " size=" << std::dec
                          << static_cast<unsigned>(access.size)
                          << " mask=0x" << std::hex
                          << static_cast<unsigned>(access.mask)
                          << " data=0x" << access.data
                          << " read_data=0x" << access.read_data
                          << std::dec << " denied=" << access.denied
                          << " corrupt=" << access.corrupt << ']';
            }
            std::cerr << '\n';
            return 1;
        }
        device_access_count = accesses.size();
        device_cycles = device.cycle();
    }

    {
        memblock::Environment queued(argc, argv);
        constexpr std::uint64_t device_base = 0x35000200ULL;
        constexpr std::uint64_t initial_value = 0xfedcba9876543210ULL;
        queued.memory().write_u64(device_base, initial_value);
        queued.configure_uncache_device(device_base, 8, true);
        if (!queued.reset() || !queued.activate_bare(45) ||
            !queued.set_uncache_write_outstanding(true)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << queued.cycle()
                      << " phase=device-queued-configuration reason="
                      << queued.error() << '\n';
            return 1;
        }

        const std::array<memblock::LoadTransaction, 3> loads{{
            {
                .address = device_base,
                .oracle_address = device_base,
                .op = memblock::LoadOp::ld,
                .rob = 0,
                .lq = 0,
                .pdest = 168,
                .lane = 0,
                .expected_debug_is_mmio = true,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            },
            {
                .address = device_base,
                .oracle_address = device_base,
                .op = memblock::LoadOp::ld,
                .rob = 1,
                .lq = 1,
                .pdest = 169,
                .lane = 1,
                .expected_debug_is_mmio = true,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            },
            {
                .address = device_base,
                .oracle_address = device_base,
                .op = memblock::LoadOp::ld,
                .rob = 2,
                .lq = 2,
                .pdest = 170,
                .lane = 2,
                .expected_debug_is_mmio = true,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            },
        }};
        for (std::size_t index = 0; index < loads.size(); ++index) {
            queued.expect_load_data(loads[index], index == 0 ? initial_value : 0);
            if (!queued.enqueue_load(loads[index]) ||
                !queued.issue_load(loads[index], 2048)) {
                std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                          << queued.cycle()
                          << " phase=device-queued-issue index=" << index
                          << " reason=" << queued.error() << '\n';
                return 1;
            }
        }
        if (queued.uncache_requests() != 0 ||
            queued.pending_scalar_loads() != loads.size()) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << queued.cycle()
                      << " phase=device-queued-before-head requests="
                      << queued.uncache_requests() << " pending="
                      << queued.pending_scalar_loads() << '\n';
            return 1;
        }

        queued.force_next_uncache_response_delay(512);
        if (!queued.wait_for_mmio_request(loads[0].rob, loads[0].rob_flag, 4096) ||
            !queued.run_cycles(256) || queued.uncache_requests() != 1 ||
            queued.uncache_outstanding_requests() != 1 ||
            queued.writebacks() != 0 ||
            queued.uncache_device_accesses().size() != 1) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << queued.cycle()
                      << " phase=device-queued-first-delay requests="
                      << queued.uncache_requests() << " outstanding="
                      << queued.uncache_outstanding_requests()
                      << " writebacks=" << queued.writebacks()
                      << " accesses="
                      << queued.uncache_device_accesses().size()
                      << " reason=" << queued.error() << '\n';
            return 1;
        }
        if (!queued.run_until_load_writebacks(1, 4096)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << queued.cycle()
                      << " phase=device-queued-first-writeback reason="
                      << queued.error() << '\n';
            return 1;
        }

        queued.force_next_uncache_response_delay(128);
        if (!queued.wait_for_mmio_request(loads[1].rob, loads[1].rob_flag, 4096) ||
            !queued.run_cycles(64) || queued.uncache_requests() != 2 ||
            queued.uncache_outstanding_requests() != 1 ||
            queued.writebacks() != 1 ||
            queued.uncache_device_accesses().size() != 2) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << queued.cycle()
                      << " phase=device-queued-second-delay requests="
                      << queued.uncache_requests() << " outstanding="
                      << queued.uncache_outstanding_requests()
                      << " writebacks=" << queued.writebacks()
                      << " accesses="
                      << queued.uncache_device_accesses().size()
                      << " reason=" << queued.error() << '\n';
            return 1;
        }
        if (!queued.run_until_load_writebacks(2, 4096) ||
            !queued.wait_for_mmio_request(loads[2].rob, loads[2].rob_flag, 4096) ||
            !queued.run_until_complete(4096) ||
            !queued.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << queued.cycle()
                      << " phase=device-queued-drain reason="
                      << queued.error() << '\n';
            return 1;
        }

        const auto &accesses = queued.uncache_device_accesses();
        if (accesses.size() != loads.size() ||
            queued.uncache_requests() != loads.size() ||
            queued.uncache_max_outstanding_requests() != 1 ||
            accesses[0].sequence != 0 || accesses[0].write ||
            accesses[0].read_data != initial_value ||
            accesses[1].sequence != 1 || accesses[1].write ||
            accesses[1].read_data != 0 ||
            accesses[2].sequence != 2 || accesses[2].write ||
            accesses[2].read_data != 0 ||
            accesses[0].denied || accesses[0].corrupt ||
            accesses[1].denied || accesses[1].corrupt ||
            accesses[2].denied || accesses[2].corrupt ||
            queued.bus_expected_load(device_base, memblock::LoadOp::ld) != 0) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << queued.cycle()
                      << " phase=device-queued-order requests="
                      << queued.uncache_requests() << " max_outstanding="
                      << queued.uncache_max_outstanding_requests()
                      << " accesses=" << accesses.size() << '\n';
            return 1;
        }
        queued_device_access_count = accesses.size();
        queued_device_cycles = queued.cycle();
        queued_device_max_outstanding =
            queued.uncache_max_outstanding_requests();
    }

    {
        memblock::Environment mixed(argc, argv);
        constexpr std::uint64_t device_base = 0x35000300ULL;
        constexpr std::uint64_t initial_value = 0x1021324354657687ULL;
        constexpr std::uint32_t store_value = 0xc1d2e3f4U;
        constexpr std::uint64_t store_bus_data =
            (std::uint64_t{store_value} << 32) | store_value;
        constexpr std::uint64_t stored_beat =
            std::uint64_t{store_value} << 32;
        mixed.memory().write_u64(device_base, initial_value);
        mixed.configure_uncache_device(device_base, 8, true);
        if (!mixed.reset() || !mixed.activate_bare(46) ||
            !mixed.set_uncache_write_outstanding(true)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << mixed.cycle()
                      << " phase=device-mixed-configuration reason="
                      << mixed.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction load_before{
            .address = device_base,
            .oracle_address = device_base,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 171,
            .lane = 0,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        const memblock::StoreTransaction store{
            .address = device_base + 4,
            .oracle_address = device_base + 4,
            .data = store_value,
            .op = memblock::StoreOp::sw,
            .rob = 1,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
        };
        const memblock::LoadTransaction load_after{
            .address = device_base,
            .oracle_address = device_base,
            .op = memblock::LoadOp::ld,
            .rob = 2,
            .lq = 1,
            .pdest = 172,
            .lane = 1,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        mixed.expect_load_data(load_before, initial_value);
        mixed.expect_store(store);
        mixed.expect_load_data(load_after, stored_beat);
        if (!mixed.enqueue_load(load_before) ||
            !mixed.issue_load(load_before, 2048) ||
            !mixed.enqueue_store(store, 0) ||
            !mixed.issue_store_address(store, 2048) ||
            !mixed.issue_store_data(store, 2048) ||
            !mixed.enqueue_load(load_after) ||
            !mixed.issue_load(load_after, 2048) ||
            mixed.uncache_requests() != 0 ||
            mixed.pending_scalar_loads() != 2 ||
            mixed.pending_scalar_stores() != 1) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << mixed.cycle()
                      << " phase=device-mixed-issue requests="
                      << mixed.uncache_requests() << " pending_loads="
                      << mixed.pending_scalar_loads() << " pending_stores="
                      << mixed.pending_scalar_stores()
                      << " reason=" << mixed.error() << '\n';
            return 1;
        }

        mixed.force_next_uncache_response_delay(256);
        if (!mixed.wait_for_mmio_request(
                load_before.rob, load_before.rob_flag, 4096) ||
            !mixed.run_cycles(128) || mixed.uncache_requests() != 1 ||
            mixed.uncache_outstanding_requests() != 1 ||
            mixed.writebacks() != 0 || mixed.store_writebacks() != 0 ||
            mixed.uncache_device_accesses().size() != 1 ||
            mixed.bus_expected_load(device_base, memblock::LoadOp::ld) != 0) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << mixed.cycle()
                      << " phase=device-mixed-load-delay requests="
                      << mixed.uncache_requests() << " outstanding="
                      << mixed.uncache_outstanding_requests()
                      << " load_writebacks=" << mixed.writebacks()
                      << " store_writebacks=" << mixed.store_writebacks()
                      << " accesses="
                      << mixed.uncache_device_accesses().size()
                      << " reason=" << mixed.error() << '\n';
            return 1;
        }
        if (!mixed.run_until_load_writebacks(1, 4096)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << mixed.cycle()
                      << " phase=device-mixed-load-writeback reason="
                      << mixed.error() << '\n';
            return 1;
        }

        mixed.force_next_uncache_response_delay(256);
        if (!mixed.wait_for_mmio_store_request(store.rob, store.rob_flag, 4096) ||
            !mixed.run_cycles(128) || mixed.uncache_requests() != 2 ||
            mixed.uncache_outstanding_requests() != 1 ||
            mixed.writebacks() != 1 || mixed.store_writebacks() != 0 ||
            mixed.uncache_device_accesses().size() != 2 ||
            mixed.bus_expected_load(device_base, memblock::LoadOp::ld) !=
                stored_beat) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << mixed.cycle()
                      << " phase=device-mixed-store-delay requests="
                      << mixed.uncache_requests() << " outstanding="
                      << mixed.uncache_outstanding_requests()
                      << " load_writebacks=" << mixed.writebacks()
                      << " store_writebacks=" << mixed.store_writebacks()
                      << " accesses="
                      << mixed.uncache_device_accesses().size()
                      << " reason=" << mixed.error() << '\n';
            return 1;
        }
        if (!mixed.run_until_store_complete(4096) ||
            !mixed.commit_stores_through(store, 1) ||
            !mixed.run_cycles(16)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << mixed.cycle()
                      << " phase=device-mixed-store-complete reason="
                      << mixed.error() << '\n';
            return 1;
        }
        mixed.record_committed_store(store);

        if (!mixed.wait_for_mmio_request(
                load_after.rob, load_after.rob_flag, 4096) ||
            !mixed.run_until_complete(4096) ||
            !mixed.run_until_queues_retired(2048)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << mixed.cycle()
                      << " phase=device-mixed-drain reason="
                      << mixed.error() << '\n';
            return 1;
        }

        const auto &accesses = mixed.uncache_device_accesses();
        if (accesses.size() != 3 || mixed.uncache_requests() != 3 ||
            mixed.uncache_max_outstanding_requests() != 1 ||
            accesses[0].sequence != 0 || accesses[0].write ||
            accesses[0].read_data != initial_value ||
            accesses[1].sequence != 1 || !accesses[1].write ||
            accesses[1].size != 2 || accesses[1].mask != 0xf0 ||
            accesses[1].data != store_bus_data ||
            accesses[2].sequence != 2 || accesses[2].write ||
            accesses[2].read_data != stored_beat ||
            accesses[0].denied || accesses[0].corrupt ||
            accesses[1].denied || accesses[1].corrupt ||
            accesses[2].denied || accesses[2].corrupt ||
            mixed.bus_expected_load(device_base, memblock::LoadOp::ld) != 0) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << mixed.cycle()
                      << " phase=device-mixed-order requests="
                      << mixed.uncache_requests() << " max_outstanding="
                      << mixed.uncache_max_outstanding_requests()
                      << " accesses=" << accesses.size() << '\n';
            return 1;
        }
        mixed_device_access_count = accesses.size();
        mixed_device_cycles = mixed.cycle();
        mixed_device_max_outstanding =
            mixed.uncache_max_outstanding_requests();
    }

    std::cout << "MEMBLOCK_MMIO_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " normal=" << normal_count
              << " denied=1 corrupt=1"
              << " load_mmio_pulses=3 load_mmio_input_lanes=3"
              << " load_mmio_output_slots=" << load_mmio_output_slots
              << " load_mmio_before_response=" << load_mmio_before_response
              << " non_mmio_controls=2 non_mmio_cycles="
              << non_mmio_control_cycles
              << " stores=1"
              << " pma_loads=" << pma_load_count
              << " pma_stores=" << pma_store_count
              << " pma_denied=" << pma_denied_count
              << " pma_denied_wakeups=" << pma_denied_wakeups
              << " pma_denied_cancels=" << pma_denied_cancels
              << " pma_debug_loads=" << pma_debug_load_count
              << " pma_debug_stores=" << pma_debug_store_count
              << " pma_edge_loads=" << pma_edge_load_count
              << " pma_edge_dcache=" << pma_edge_dcache_requests
              << " pma_edge_uncache=" << pma_edge_uncache_requests
              << " pma_cycles=" << pma_cycles
              << " device_accesses=" << device_access_count
              << " device_read_clear=1 device_partial_write=1"
              << " device_error_reads=2 device_error_writes=1"
              << " device_error_side_effects=0"
              << " device_cycles=" << device_cycles
              << " queued_device_loads=" << queued_device_access_count
              << " queued_device_max_outstanding="
              << queued_device_max_outstanding
              << " queued_device_cycles=" << queued_device_cycles
              << " mixed_device_ops=" << mixed_device_access_count
              << " mixed_device_max_outstanding="
              << mixed_device_max_outstanding
              << " mixed_device_cycles=" << mixed_device_cycles
              << " dcache_requests=" << environment.tilelink_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_cbo_zero_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x5003c000ULL;
    constexpr std::uint64_t physical_base = 0x9003c000ULL;
    constexpr std::uint64_t root = 0x97010000ULL;
    environment.memory().fill_incrementing(physical_base, 64, 0xa7);
    environment.configure_backpressure(0x4d595df4d0f33173ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false,
            false, false) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    // CBO.ZERO uses the 0x7 encoding and writes an entire 64-byte line through
    // the cacheable StoreQueue/SBuffer wline path.  Use a nonzero source
    // operand so the test also proves the StoreQueue's CBO.ZERO data override.
    const memblock::StoreTransaction cbo_zero{
        .address = virtual_base + 0x20,
        .oracle_address = physical_base,
        .data = 0xdeadbeefcafef00dULL,
        .op = memblock::StoreOp::cbo_zero,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
    };
    environment.expect_store(cbo_zero);
    const std::uint64_t dcache_before = environment.tilelink_requests();
    const std::uint64_t store_misses_before = environment.store_tlb_misses();
    const std::uint64_t sq_target = environment.sq_dequeued() + 1;
    if (!environment.set_rob_head(cbo_zero.rob, cbo_zero.rob_flag) ||
        !environment.enqueue_store(cbo_zero, 0) ||
        !environment.issue_store_address(cbo_zero, 2048) ||
        !environment.run_until_store_tlb_misses(store_misses_before + 1, 4096) ||
        !environment.run_cycles(256) ||
        !environment.issue_store_address(cbo_zero, 2048) ||
        !environment.issue_store_data(cbo_zero, 2048) ||
        !environment.commit_stores_through(cbo_zero, 1) ||
        !environment.run_until_store_complete(16384) ||
        !environment.run_until_sq_dequeued(sq_target, 16384)) {
        std::cerr << "MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=zero-store reason="
                  << environment.error() << " dcache_requests="
                  << environment.tilelink_requests() << " sq="
                  << environment.sq_dequeued() << '/' << environment.sq_allocated()
                  << " store_mmio_valid=" << environment.store_mmio_valid()
                  << '\n';
        return 1;
    }
    if (environment.tilelink_requests() <= dcache_before ||
        environment.uncache_requests() != 0 ||
        environment.dcache_request_stalls() == 0) {
        std::cerr << "MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=manager-path reason="
                  << "unexpected manager request count dcache="
                  << environment.tilelink_requests() << " uncache="
                  << environment.uncache_requests() << '\n';
        return 1;
    }

    // Read the line through the cache before updating the reference mirror.
    // This makes the result sensitive to the full-line zero emitted by RTL.
    const memblock::LoadTransaction readback{
        .address = virtual_base,
        .oracle_address = physical_base,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 0,
        .pdest = 191,
        .lane = 0,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
        .expected_debug_is_perf_cnt = false,
    };
    environment.expect_load_data(readback, 0);
    if (!environment.set_rob_head(readback.rob) ||
        !environment.enqueue_load(readback) ||
        !environment.issue_load(readback, 2048) ||
        !environment.run_until_complete(16384) ||
        !environment.run_until_lq_retired(2048) ||
        environment.uncache_requests() != 0) {
        std::cerr << "MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=zero-readback reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.record_committed_store(cbo_zero);
    std::cout << "MEMBLOCK_CBO_ZERO_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " cbo_zero_line=1"
              << " readback=1"
              << " dcache_requests=" << environment.tilelink_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " dcache_request_stalls=" << environment.dcache_request_stalls()
              << " dcache_response_delays=" << environment.dcache_response_delays()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_cmo_contracts(int argc, char **argv)
{
    struct CmoOperation {
        const char *name;
        memblock::StoreOp op;
    };
    struct CmoCase {
        const char *name;
        memblock::StoreOp op;
        std::uint8_t probe_cap;
        std::uint8_t expected_report;
        bool dirty;
        bool expect_probe_data;
        bool expect_resident;
    };
    const std::array<CmoOperation, 3> operations{{
        {"clean", memblock::StoreOp::cbo_clean},
        {"flush", memblock::StoreOp::cbo_flush},
        {"inval", memblock::StoreOp::cbo_inval},
    }};
    const std::array<CmoCase, 6> cases{{
        {"clean-dirty", memblock::StoreOp::cbo_clean, 1, 0,
         true, true, true},
        {"clean-clean", memblock::StoreOp::cbo_clean, 1, 4,
         false, false, true},
        {"flush-dirty", memblock::StoreOp::cbo_flush, 2, 1,
         true, true, false},
        {"flush-clean", memblock::StoreOp::cbo_flush, 2, 2,
         false, false, false},
        {"inval-dirty", memblock::StoreOp::cbo_inval, 2, 1,
         true, true, false},
        {"inval-clean", memblock::StoreOp::cbo_inval, 2, 2,
         false, false, false},
    }};

    unsigned dirty_probe_data = 0;
    unsigned automatic_sbuffer_drains = 0;
    unsigned retained_hits = 0;
    unsigned invalidation_refills = 0;
    std::uint64_t positive_cycles = 0;
    for (unsigned index = 0; index < cases.size(); ++index) {
        const CmoCase &test = cases[index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t line =
            memblock::kDefaultMemoryBase + 0x3c0000 + index * 0x1000;
        const std::uint64_t address = line + 24;
        environment.memory().fill_incrementing(
            line, 64, static_cast<std::uint8_t>(0x41 + index * 0x19));
        environment.configure_backpressure(
            0x243f6a8885a308d3ULL ^ (std::uint64_t{index} << 32), true,
            memblock::ResponseLatencyProfile::spec);
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-reset reason=" << environment.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction warm{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(180 + index * 3),
            .lane = index % memblock::kScalarLoadLanes,
        };
        const std::uint64_t warm_refills = environment.dcache_refills();
        environment.expect_load(warm);
        if (!environment.set_rob_head(warm.rob, warm.rob_flag) ||
            !environment.enqueue_load(warm) ||
            !environment.issue_load(warm, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(2048) ||
            environment.dcache_refills() != warm_refills + 1) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-warm reason=" << environment.error()
                      << " refills=" << environment.dcache_refills() - warm_refills
                      << '\n';
            return 1;
        }

        std::optional<memblock::StoreTransaction> dirty_store;
        if (test.dirty) {
            dirty_store = memblock::StoreTransaction{
                .address = address,
                .data = 0xdecafbad12345678ULL ^
                    (std::uint64_t{index} * 0x1111111111111111ULL),
                .op = memblock::StoreOp::sd,
                .rob = 1,
                .sq = 0,
                .address_lane = index % memblock::kScalarStoreLanes,
                .data_lane = (index + 1) % memblock::kScalarStoreLanes,
            };
            environment.expect_store(*dirty_store);
            if (!environment.set_rob_head(
                    dirty_store->rob, dirty_store->rob_flag) ||
                !environment.enqueue_store(*dirty_store, 0) ||
                !environment.issue_store_data(*dirty_store, 2048) ||
                !environment.issue_store_address(*dirty_store, 2048) ||
                !environment.run_until_store_complete(4096) ||
                !environment.commit_store(*dirty_store, 8192) ||
                environment.sbuffer_empty() ||
                !environment.dcache_responses_idle() ||
                !environment.dcache_grants_drained()) {
                std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << test.name
                          << "-dirty reason=" << environment.error() << '\n';
                return 1;
            }
        }

        const std::vector<unsigned char> expected_line =
            environment.memory().read_beat(line, 64);
        const memblock::StoreTransaction cmo{
            .address = line + 37,
            .oracle_address = line,
            .data = 0xa5a55a5af00dcafeULL,
            .op = test.op,
            .rob = static_cast<std::uint8_t>(test.dirty ? 2 : 1),
            .sq = static_cast<std::uint8_t>(test.dirty ? 1 : 0),
            .address_lane = (index + 1) % memblock::kScalarStoreLanes,
            .data_lane = index % memblock::kScalarStoreLanes,
            .expected_output_flush_pipe = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t cmo_before =
            environment.dcache_cmo_requests(cmo.op);
        const std::uint64_t probes_before = environment.dcache_probes();
        const std::uint64_t probe_responses_before =
            environment.dcache_probe_responses();
        const std::uint64_t probe_data_before =
            environment.dcache_probe_data();
        const std::uint64_t completed_stores_before =
            environment.store_writebacks();
        environment.expect_store(cmo);
        if (!environment.enqueue_store(cmo, 0) ||
            !environment.issue_store_address(cmo, 2048) ||
            !environment.issue_store_data(cmo, 2048)) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-issue reason=" << environment.error() << '\n';
            return 1;
        }

        // A real L2 completes a CMO only after the derived upper Probe has
        // returned. Hold CBOAck far enough away to exercise that ordering.
        environment.force_next_cmo_response_delay(1024);
        if (!environment.wait_for_cmo_store_request(cmo, cmo_before + 1, 8192) ||
            environment.dcache_last_cmo_address() != line ||
            environment.dcache_last_cmo_source() != memblock::kDcacheCmoSource ||
            environment.uncache_requests() != 0 ||
            !environment.request_dcache_probe(
                line, test.probe_cap, false, test.expected_report,
                test.expect_probe_data
                    ? expected_line
                    : std::vector<unsigned char>{}) ||
            !environment.run_until_probe_responses(
                probe_responses_before + 1, 4096) ||
            environment.dcache_probes() != probes_before + 1 ||
            environment.dcache_probe_data() !=
                probe_data_before + (test.expect_probe_data ? 1U : 0U) ||
            environment.store_writebacks() != completed_stores_before ||
            environment.pending_scalar_stores() != 1) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-probe-before-ack reason=" << environment.error()
                      << " cmo_address=0x" << std::hex
                      << environment.dcache_last_cmo_address()
                      << " expected=0x" << line << std::dec
                      << " source="
                      << static_cast<unsigned>(environment.dcache_last_cmo_source())
                      << " pending_stores="
                      << environment.pending_scalar_stores() << '\n';
            return 1;
        }
        automatic_sbuffer_drains += dirty_store.has_value();

        if (!environment.run_until_store_complete(16384) ||
            !environment.commit_store(cmo, 8192) ||
            !environment.run_cycles(16) ||
            !environment.dcache_responses_idle() ||
            environment.dcache_cmo_requests(cmo.op) != cmo_before + 1 ||
            !environment.sbuffer_empty() ||
            environment.sq_dequeued() != environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-completion reason=" << environment.error()
                      << " sq=" << environment.sq_dequeued() << '/'
                      << environment.sq_allocated() << '\n';
            return 1;
        }

        if (dirty_store &&
            environment.bus_expected_load(address, memblock::LoadOp::ld) !=
                dirty_store->data) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-dirty-writeback bus=0x" << std::hex
                      << environment.bus_expected_load(
                             address, memblock::LoadOp::ld)
                      << " expected=0x" << dirty_store->data << std::dec
                      << '\n';
            return 1;
        }

        const std::uint64_t refills_before_readback =
            environment.dcache_refills();
        const memblock::LoadTransaction readback{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(cmo.rob + 1),
            .lq = 1,
            .sq = cmo.sq,
            .pdest = static_cast<std::uint8_t>(181 + index * 3),
            .lane = (index + 1) % memblock::kScalarLoadLanes,
        };
        environment.expect_load(readback);
        if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-readback reason=" << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t readback_refills =
            environment.dcache_refills() - refills_before_readback;
        if (readback_refills != (test.expect_resident ? 0U : 1U)) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-line-state readback_refills=" << readback_refills
                      << " expected=" << (test.expect_resident ? 0 : 1)
                      << '\n';
            return 1;
        }
        dirty_probe_data += test.expect_probe_data;
        retained_hits += test.expect_resident;
        invalidation_refills += !test.expect_resident;
        positive_cycles += environment.cycle();
    }

    std::uint64_t concurrent_cycles = 0;
    unsigned flushed_younger_loads = 0;
    {
        memblock::Environment overlap(argc, argv);
        const std::uint64_t cmo_line =
            memblock::kDefaultMemoryBase + 0x430000;
        const std::uint64_t younger_line = cmo_line + 0x1000;
        overlap.memory().fill_incrementing(cmo_line, 64, 0x73);
        overlap.memory().fill_incrementing(younger_line, 64, 0xb1);
        overlap.configure_backpressure(
            0xa4093822299f31d0ULL, true,
            memblock::ResponseLatencyProfile::spec);
        if (!overlap.reset()) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << overlap.cycle()
                      << " phase=flushpipe-overlap-reset reason="
                      << overlap.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction warm{
            .address = cmo_line + 8,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 199,
            .lane = 0,
        };
        overlap.expect_load(warm);
        if (!overlap.set_rob_head(warm.rob, warm.rob_flag) ||
            !overlap.enqueue_load(warm) ||
            !overlap.issue_load(warm, 2048) ||
            !overlap.run_until_complete(16384) ||
            !overlap.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << overlap.cycle()
                      << " phase=flushpipe-overlap-warm reason="
                      << overlap.error() << '\n';
            return 1;
        }

        const memblock::StoreTransaction cmo{
            .address = cmo_line + 39,
            .oracle_address = cmo_line,
            .data = 0,
            .op = memblock::StoreOp::cbo_flush,
            .rob = 1,
            .sq = 0,
            .address_lane = 1,
            .data_lane = 0,
            .expected_output_flush_pipe = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        };
        overlap.expect_store(cmo);
        if (!overlap.enqueue_store(cmo, 0) ||
            !overlap.issue_store_address(cmo, 2048) ||
            !overlap.issue_store_data(cmo, 2048)) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << overlap.cycle()
                      << " phase=flushpipe-overlap-cmo-issue reason="
                      << overlap.error() << '\n';
            return 1;
        }
        overlap.force_next_cmo_response_delay(1024);
        const std::uint64_t probe_responses_before =
            overlap.dcache_probe_responses();
        if (!overlap.wait_for_cmo_store_request(cmo, 1, 8192) ||
            !overlap.request_dcache_probe(cmo_line, 2, false, 2, {}) ||
            !overlap.run_until_probe_responses(
                probe_responses_before + 1, 4096)) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << overlap.cycle()
                      << " phase=flushpipe-overlap-probe reason="
                      << overlap.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction younger{
            .address = younger_line + 24,
            .op = memblock::LoadOp::ld,
            .rob = 2,
            .lq = 1,
            .sq = 0,
            .pdest = 200,
            .lane = 2,
        };
        const std::uint64_t dcache_before = overlap.tilelink_requests();
        const std::uint64_t load_writebacks_before = overlap.writebacks();
        overlap.force_next_dcache_response_delay(2048);
        if (!overlap.enqueue_load(younger) ||
            !overlap.issue_load(younger, 2048) ||
            !overlap.run_until_dcache_requests(dcache_before + 1, 4096) ||
            overlap.store_writebacks() != 0 ||
            overlap.writebacks() != load_writebacks_before ||
            overlap.pending_scalar_stores() != 1) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << overlap.cycle()
                      << " phase=flushpipe-overlap-window reason="
                      << overlap.error() << " dcache="
                      << overlap.tilelink_requests() - dcache_before
                      << " store_wb=" << overlap.store_writebacks()
                      << " load_wb="
                      << overlap.writebacks() - load_writebacks_before << '\n';
            return 1;
        }

        if (!overlap.run_until_store_complete(16384) ||
            overlap.writebacks() != load_writebacks_before ||
            !overlap.redirect_after(cmo.rob, cmo.rob_flag, false) ||
            !overlap.commit_store(cmo, 8192) ||
            !overlap.run_cycles(4096) ||
            overlap.writebacks() != load_writebacks_before ||
            overlap.lq_redirect_canceled_observed() != 1 ||
            overlap.lq_dequeued() + overlap.lq_canceled() !=
                overlap.lq_allocated() ||
            overlap.sq_dequeued() != overlap.sq_allocated() ||
            !overlap.dcache_responses_idle() ||
            !overlap.dcache_grants_drained()) {
            std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                      << overlap.cycle()
                      << " phase=flushpipe-overlap-cancel reason="
                      << overlap.error() << " load_wb="
                      << overlap.writebacks() - load_writebacks_before
                      << " lq=" << overlap.lq_dequeued() << '+'
                      << overlap.lq_canceled() << '/'
                      << overlap.lq_allocated() << " sq="
                      << overlap.sq_dequeued() << '/'
                      << overlap.sq_allocated() << '\n';
            return 1;
        }
        flushed_younger_loads = 1;
        concurrent_cycles = overlap.cycle();
    }

    unsigned denied_cases = 0;
    unsigned corrupt_cases = 0;
    std::uint64_t error_cycles = 0;
    for (unsigned operation = 0; operation < operations.size(); ++operation) {
        for (unsigned error_kind = 0; error_kind < 2; ++error_kind) {
            const CmoOperation &test = operations[operation];
            const bool denied = error_kind == 0;
            const bool corrupt = !denied;
            memblock::Environment environment(argc, argv);
            const std::uint64_t line = memblock::kDefaultMemoryBase +
                0x400000 + operation * 0x2000 + error_kind * 0x1000;
            environment.memory().fill_incrementing(
                line, 64,
                static_cast<std::uint8_t>(0x91 + operation * 9 + error_kind));
            environment.configure_backpressure(
                0x13198a2e03707344ULL ^
                    (std::uint64_t{operation} << 16) ^ error_kind,
                true, memblock::ResponseLatencyProfile::spec);
            if (!environment.reset()) {
                std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << test.name
                          << '-' << (denied ? "denied" : "corrupt")
                          << "-reset reason=" << environment.error() << '\n';
                return 1;
            }

            const memblock::StoreTransaction cmo{
                .address = line + 55,
                .oracle_address = line,
                .data = 0x55aa55aa33cc33ccULL,
                .op = test.op,
                .rob = 0,
                .sq = 0,
                .address_lane = error_kind,
                .data_lane = 1 - error_kind,
                .expected_exception_mask = denied
                    ? memblock::kExceptionStoreAccessFault
                    : memblock::kExceptionHardwareError,
                .expected_output_flush_pipe = true,
                .expected_debug_is_mmio = false,
                .expected_debug_is_ncio = false,
            };
            const std::uint64_t bus_before = environment.bus_expected_load(
                line + 24, memblock::LoadOp::ld);
            const std::uint64_t denied_before =
                environment.dcache_denied_d_beats();
            const std::uint64_t corrupt_before =
                environment.dcache_corrupt_d_beats();
            const std::uint64_t errors_before =
                environment.dcache_error_response_requests();
            environment.expect_store(cmo);
            environment.inject_dcache_cmo_response_error_at(
                line, denied, corrupt);
            environment.force_next_cmo_response_delay(256);
            if (!environment.enqueue_store(cmo, 0) ||
                !environment.issue_store_address(cmo, 2048) ||
                !environment.issue_store_data(cmo, 2048) ||
                !environment.wait_for_cmo_store_request(cmo, 1, 8192) ||
                !environment.run_until_store_complete(16384) ||
                !environment.run_cycles(8) ||
                !environment.redirect_after(cmo.rob, cmo.rob_flag, true) ||
                !environment.run_cycles(96) ||
                (environment.sq_dequeued() + environment.sq_canceled() <
                     environment.sq_allocated() &&
                 !environment.account_sq_cancellation(1))) {
                std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << test.name
                          << '-' << (denied ? "denied" : "corrupt")
                          << "-execution reason=" << environment.error()
                          << '\n';
                return 1;
            }
            if (environment.dcache_cmo_requests(cmo.op) != 1 ||
                environment.dcache_last_cmo_address() != line ||
                environment.dcache_last_cmo_source() !=
                    memblock::kDcacheCmoSource ||
                environment.dcache_error_response_requests() !=
                    errors_before + 1 ||
                environment.dcache_denied_d_beats() !=
                    denied_before + (denied ? 1U : 0U) ||
                environment.dcache_corrupt_d_beats() !=
                    corrupt_before + (corrupt ? 1U : 0U) ||
                environment.dcache_probes() != 0 ||
                environment.uncache_requests() != 0 ||
                environment.bus_expected_load(
                    line + 24, memblock::LoadOp::ld) != bus_before ||
                environment.sq_dequeued() + environment.sq_canceled() !=
                    environment.sq_allocated() ||
                !environment.dcache_responses_idle()) {
                std::cerr << "MEMBLOCK_CMO_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << test.name
                          << '-' << (denied ? "denied" : "corrupt")
                          << "-oracle cmo="
                          << environment.dcache_cmo_requests(cmo.op)
                          << " denied_beats="
                          << environment.dcache_denied_d_beats() - denied_before
                          << " corrupt_beats="
                          << environment.dcache_corrupt_d_beats() - corrupt_before
                          << " sq=" << environment.sq_dequeued() << '+'
                          << environment.sq_canceled() << '/'
                          << environment.sq_allocated() << '\n';
                return 1;
            }
            denied_cases += denied;
            corrupt_cases += corrupt;
            error_cycles += environment.cycle();
        }
    }

    std::cout << "MEMBLOCK_CMO_CONTRACTS_PASS"
              << " operations=" << operations.size()
              << " line_state_cases=" << cases.size()
              << " dirty_probe_data=" << dirty_probe_data
              << " automatic_sbuffer_drains=" << automatic_sbuffer_drains
              << " retained_hits=" << retained_hits
              << " invalidation_refills=" << invalidation_refills
              << " flushed_younger_loads=" << flushed_younger_loads
              << " concurrent_cycles=" << concurrent_cycles
              << " denied_cases=" << denied_cases
              << " corrupt_cases=" << corrupt_cases
              << " positive_cycles=" << positive_cycles
              << " error_cycles=" << error_cycles
              << " cmo_ack_delay=1024"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_wfi_safety(int argc, char **argv)
{
    constexpr unsigned response_delay = 256;
    constexpr unsigned unsafe_window = 64;

    memblock::Environment dcache(argc, argv);
    constexpr std::uint64_t dcache_base =
        memblock::kDefaultMemoryBase + 0x3a0000;
    dcache.memory().fill_incrementing(dcache_base, 64, 0x47);
    dcache.configure_backpressure(0x6a09e667f3bcc909ULL, true);
    if (!dcache.reset() || !dcache.set_wfi(true) ||
        !dcache.run_until_wfi_safe(256) || !dcache.set_wfi(false) ||
        !dcache.run_cycles(4) || dcache.wfi_safe()) {
        std::cerr << "MEMBLOCK_WFI_SAFETY_FAIL cycle=" << dcache.cycle()
                  << " phase=idle-handshake reason=" << dcache.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction dcache_load{
        .address = dcache_base + 24,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 180,
        .lane = 0,
    };
    const std::uint64_t dcache_requests_before = dcache.tilelink_requests();
    dcache.expect_load(dcache_load);
    dcache.force_next_dcache_response_delay(response_delay);
    if (!dcache.enqueue_load(dcache_load) ||
        !dcache.issue_load(dcache_load, 2048) ||
        !dcache.run_until_dcache_requests(dcache_requests_before + 1, 4096) ||
        !dcache.set_wfi(true) ||
        !dcache.require_wfi_unsafe(unsafe_window) ||
        !dcache.run_until_wfi_safe(4096) ||
        !dcache.set_wfi(false) || !dcache.run_cycles(4) || dcache.wfi_safe() ||
        !dcache.run_until_complete(4096) ||
        !dcache.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_WFI_SAFETY_FAIL cycle=" << dcache.cycle()
                  << " phase=dcache-drain reason=" << dcache.error()
                  << " dcache_requests=" << dcache.tilelink_requests()
                  << " writebacks=" << dcache.writebacks() << '\n';
        return 1;
    }

    memblock::Environment ptw(argc, argv);
    constexpr std::uint64_t ptw_virtual = 0x5003a000ULL;
    constexpr std::uint64_t ptw_physical = 0x9003a000ULL;
    constexpr std::uint64_t ptw_root = 0x97010000ULL;
    ptw.memory().fill_incrementing(ptw_physical, 64, 0x6d);
    ptw.configure_backpressure(0xbb67ae8584caa73bULL, true);
    if (!ptw.reset() ||
        !ptw.map_sv39_4k(ptw_virtual, ptw_physical, ptw_root) ||
        !ptw.activate_sv39(ptw_root)) {
        std::cerr << "MEMBLOCK_WFI_SAFETY_FAIL cycle=" << ptw.cycle()
                  << " phase=ptw-configuration reason=" << ptw.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction ptw_load{
        .address = ptw_virtual + 8,
        .oracle_address = ptw_physical + 8,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 181,
        .lane = 1,
    };
    const std::uint64_t ptw_requests_before = ptw.ptw_requests();
    const std::uint64_t ptw_dcache_before = ptw.tilelink_requests();
    ptw.expect_load(ptw_load);
    ptw.force_next_ptw_response_delay(response_delay);
    if (!ptw.enqueue_load(ptw_load) || !ptw.issue_load(ptw_load, 2048) ||
        !ptw.run_until_ptw_requests(ptw_requests_before + 1, 4096) ||
        !ptw.set_wfi(true) || !ptw.require_wfi_unsafe(unsafe_window) ||
        !ptw.run_until_wfi_safe(4096) ||
        ptw.tilelink_requests() != ptw_dcache_before ||
        !ptw.set_wfi(false) || !ptw.run_cycles(4) || ptw.wfi_safe() ||
        !ptw.run_until_complete(8192) ||
        !ptw.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_WFI_SAFETY_FAIL cycle=" << ptw.cycle()
                  << " phase=ptw-drain reason=" << ptw.error()
                  << " ptw_requests=" << ptw.ptw_requests()
                  << " dcache_requests=" << ptw.tilelink_requests() << '\n';
        return 1;
    }

    memblock::Environment uncache(argc, argv);
    constexpr std::uint64_t mmio_virtual = 0x5003b000ULL;
    constexpr std::uint64_t mmio_physical = 0x9003b000ULL;
    constexpr std::uint64_t mmio_root = 0x97014000ULL;
    uncache.memory().fill_incrementing(mmio_physical, 64, 0x93);
    uncache.configure_backpressure(0x3c6ef372fe94f82bULL, true);
    if (!uncache.reset() ||
        !uncache.map_sv39_4k(
            mmio_virtual, mmio_physical, mmio_root,
            true, true, false, false, false, true) ||
        !uncache.activate_sv39(mmio_root)) {
        std::cerr << "MEMBLOCK_WFI_SAFETY_FAIL cycle=" << uncache.cycle()
                  << " phase=uncache-configuration reason="
                  << uncache.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction mmio_load{
        .address = mmio_virtual + 16,
        .oracle_address = mmio_physical + 16,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 182,
        .lane = 2,
        .expected_debug_is_mmio = true,
        .expected_debug_is_ncio = false,
        .expected_debug_is_perf_cnt = false,
    };
    const std::uint64_t uncache_requests_before = uncache.uncache_requests();
    uncache.expect_load(mmio_load);
    uncache.force_next_uncache_response_delay(response_delay);
    if (!uncache.set_rob_head(mmio_load.rob, mmio_load.rob_flag) ||
        !uncache.enqueue_load(mmio_load) ||
        !uncache.issue_load(mmio_load, 2048) ||
        !uncache.wait_for_mmio_request(mmio_load.rob, mmio_load.rob_flag, 4096) ||
        uncache.uncache_requests() != uncache_requests_before + 1 ||
        uncache.uncache_outstanding_requests() != 1 ||
        !uncache.set_wfi(true) ||
        !uncache.require_wfi_unsafe(unsafe_window) ||
        !uncache.run_until_wfi_safe(4096) ||
        !uncache.set_wfi(false) || !uncache.run_cycles(4) ||
        uncache.wfi_safe() || !uncache.run_until_complete(4096) ||
        !uncache.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_WFI_SAFETY_FAIL cycle=" << uncache.cycle()
                  << " phase=uncache-drain reason=" << uncache.error()
                  << " uncache_requests=" << uncache.uncache_requests()
                  << " uncache_outstanding="
                  << uncache.uncache_outstanding_requests() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_WFI_SAFETY_PASS"
              << " cycle=" << dcache.cycle() + ptw.cycle() + uncache.cycle()
              << " response_delay=" << response_delay
              << " unsafe_window=" << unsafe_window
              << " idle_safe=1 dcache_safe=1 ptw_safe=1 uncache_safe=1"
              << " dcache_requests=" << dcache.tilelink_requests()
              << " ptw_requests=" << ptw.ptw_requests()
              << " uncache_requests=" << uncache.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_reset_recovery(int argc, char **argv)
{
    constexpr unsigned response_delay = 256;
    auto account_canceled_load = [](memblock::Environment &environment) {
        const std::uint64_t retired =
            environment.lq_dequeued() + environment.lq_canceled();
        return retired >= environment.lq_allocated() ||
            environment.account_lq_cancellation(
                static_cast<unsigned>(environment.lq_allocated() - retired));
    };

    memblock::Environment dcache(argc, argv);
    constexpr std::uint64_t dcache_old =
        memblock::kDefaultMemoryBase + 0x3c0000;
    constexpr std::uint64_t dcache_new =
        memblock::kDefaultMemoryBase + 0x3c1000;
    dcache.memory().fill_incrementing(dcache_old, 64, 0x31);
    dcache.memory().fill_incrementing(dcache_new, 64, 0xa1);
    dcache.configure_backpressure(0x510e527fade682d1ULL, true);
    const memblock::LoadTransaction dcache_canceled{
        .address = dcache_old + 24,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 140,
        .lane = 0,
    };
    const auto dcache_requests_before = dcache.tilelink_requests();
    if (!dcache.reset() || !dcache.activate_bare(11)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=dcache-configuration"
                  << " reason=" << dcache.error() << '\n';
        return 1;
    }
    dcache.force_next_dcache_response_delay(response_delay);
    if (!dcache.enqueue_load(dcache_canceled) ||
        !dcache.issue_load(dcache_canceled, 2048) ||
        !dcache.run_until_dcache_requests(dcache_requests_before + 1, 4096) ||
        !dcache.reset() || !account_canceled_load(dcache) ||
        !dcache.activate_bare(12)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=dcache-reset"
                  << " cycle=" << dcache.cycle()
                  << " reason=" << dcache.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction dcache_survivor{
        .address = dcache_new + 8,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 0,
        .pdest = 141,
        .lane = 1,
    };
    dcache.expect_load(dcache_survivor);
    if (!dcache.set_rob_head(dcache_survivor.rob) ||
        !dcache.enqueue_load(dcache_survivor) ||
        !dcache.issue_load(dcache_survivor, 2048) ||
        !dcache.run_until_complete(8192) ||
        !dcache.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=dcache-survivor"
                  << " cycle=" << dcache.cycle()
                  << " reason=" << dcache.error() << '\n';
        return 1;
    }

    memblock::Environment ptw(argc, argv);
    constexpr std::uint64_t ptw_virtual = 0x50036000ULL;
    constexpr std::uint64_t ptw_old_physical = 0x90036000ULL;
    constexpr std::uint64_t ptw_new_physical = 0x90037000ULL;
    constexpr std::uint64_t ptw_old_root = 0x97008000ULL;
    constexpr std::uint64_t ptw_new_root = 0x9700c000ULL;
    ptw.memory().fill_incrementing(ptw_old_physical, 64, 0x42);
    ptw.memory().fill_incrementing(ptw_new_physical, 64, 0xb2);
    ptw.configure_backpressure(0xbb67ae8584caa73bULL, true);
    if (!ptw.reset() ||
        !ptw.map_sv39_4k(ptw_virtual, ptw_old_physical, ptw_old_root) ||
        !ptw.map_sv39_4k(ptw_virtual, ptw_new_physical, ptw_new_root) ||
        !ptw.activate_sv39(ptw_old_root, 21)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=ptw-configuration"
                  << " reason=" << ptw.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction ptw_canceled{
        .address = ptw_virtual + 24,
        .oracle_address = ptw_old_physical + 24,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 142,
        .lane = 1,
    };
    const auto ptw_requests_before = ptw.ptw_requests();
    ptw.force_next_ptw_response_delay(response_delay);
    if (!ptw.enqueue_load(ptw_canceled) ||
        !ptw.issue_load(ptw_canceled, 2048) ||
        !ptw.run_until_ptw_requests(ptw_requests_before + 1, 4096) ||
        !ptw.reset() || !account_canceled_load(ptw) ||
        !ptw.activate_sv39(ptw_new_root, 22)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=ptw-reset"
                  << " cycle=" << ptw.cycle()
                  << " reason=" << ptw.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction ptw_survivor{
        .address = ptw_virtual + 8,
        .oracle_address = ptw_new_physical + 8,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 0,
        .pdest = 143,
        .lane = 2,
    };
    ptw.expect_load(ptw_survivor);
    if (!ptw.set_rob_head(ptw_survivor.rob) ||
        !ptw.enqueue_load(ptw_survivor) ||
        !ptw.issue_load(ptw_survivor, 2048) ||
        !ptw.run_until_complete(8192) ||
        !ptw.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=ptw-survivor"
                  << " cycle=" << ptw.cycle()
                  << " reason=" << ptw.error() << '\n';
        return 1;
    }

    memblock::Environment uncache(argc, argv);
    constexpr std::uint64_t mmio_virtual = 0x50038000ULL;
    constexpr std::uint64_t mmio_old_physical = 0x90038000ULL;
    constexpr std::uint64_t mmio_new_physical = 0x90039000ULL;
    constexpr std::uint64_t mmio_old_root = 0x97010000ULL;
    constexpr std::uint64_t mmio_new_root = 0x97014000ULL;
    uncache.memory().fill_incrementing(mmio_old_physical, 64, 0x53);
    uncache.memory().fill_incrementing(mmio_new_physical, 64, 0xc3);
    uncache.configure_backpressure(0x3c6ef372fe94f82bULL, true);
    if (!uncache.reset() ||
        !uncache.map_sv39_4k(
            mmio_virtual, mmio_old_physical, mmio_old_root,
            true, true, false, false, false, true) ||
        !uncache.map_sv39_4k(
            mmio_virtual, mmio_new_physical, mmio_new_root,
            true, true, false, false, false, true) ||
        !uncache.activate_sv39(mmio_old_root, 31)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=uncache-configuration"
                  << " reason=" << uncache.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction uncache_canceled{
        .address = mmio_virtual + 24,
        .oracle_address = mmio_old_physical + 24,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 144,
        .lane = 2,
        .expected_debug_is_mmio = true,
        .expected_debug_is_ncio = false,
        .expected_debug_is_perf_cnt = false,
    };
    const auto uncache_requests_before = uncache.uncache_requests();
    uncache.force_next_uncache_response_delay(response_delay);
    if (!uncache.set_rob_head(
            uncache_canceled.rob, uncache_canceled.rob_flag) ||
        !uncache.enqueue_load(uncache_canceled) ||
        !uncache.issue_load(uncache_canceled, 2048) ||
        !uncache.wait_for_mmio_request(
            uncache_canceled.rob, uncache_canceled.rob_flag, 4096) ||
        uncache.uncache_requests() != uncache_requests_before + 1 ||
        uncache.uncache_outstanding_requests() != 1 ||
        !uncache.reset() || !account_canceled_load(uncache) ||
        !uncache.activate_sv39(mmio_new_root, 32)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=uncache-reset"
                  << " cycle=" << uncache.cycle()
                  << " reason=" << uncache.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction uncache_survivor{
        .address = mmio_virtual + 8,
        .oracle_address = mmio_new_physical + 8,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 0,
        .pdest = 145,
        .lane = 0,
        .expected_debug_is_mmio = true,
        .expected_debug_is_ncio = false,
        .expected_debug_is_perf_cnt = false,
    };
    uncache.expect_load(uncache_survivor);
    if (!uncache.set_rob_head(
            uncache_survivor.rob, uncache_survivor.rob_flag) ||
        !uncache.enqueue_load(uncache_survivor) ||
        !uncache.issue_load(uncache_survivor, 2048) ||
        !uncache.wait_for_mmio_request(
            uncache_survivor.rob, uncache_survivor.rob_flag, 4096) ||
        !uncache.run_until_complete(8192) ||
        !uncache.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL phase=uncache-survivor"
                  << " cycle=" << uncache.cycle()
                  << " reason=" << uncache.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_RESET_RECOVERY_PASS"
              << " cycle=" << dcache.cycle() + ptw.cycle() + uncache.cycle()
              << " outstanding_resets=3"
              << " canceled=3"
              << " survivor_writebacks=3"
              << " dcache=1 ptw=1 uncache=1"
              << " response_delay=" << response_delay
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_reset_tree_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    if (!environment.check_reset_backend_contract() ||
        environment.reset_functional_pulses() != 3 ||
        environment.reset_dft_pulses() != 1 ||
        environment.reset_scan_transitions() != 2 ||
        environment.reset_async_assertions() != 5) {
        std::cerr << "MEMBLOCK_RESET_TREE_CONTRACTS_FAIL cycle="
                  << environment.cycle()
                  << " functional_pulses="
                  << environment.reset_functional_pulses()
                  << " dft_pulses=" << environment.reset_dft_pulses()
                  << " scan_transitions="
                  << environment.reset_scan_transitions()
                  << " async_assertions="
                  << environment.reset_async_assertions()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_RESET_TREE_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " functional_pulses="
              << environment.reset_functional_pulses()
              << " functional_release_cycles=6"
              << " dft_pulses=" << environment.reset_dft_pulses()
              << " dft_release_cycles=3"
              << " scan_transitions="
              << environment.reset_scan_transitions()
              << " async_assertions="
              << environment.reset_async_assertions()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
