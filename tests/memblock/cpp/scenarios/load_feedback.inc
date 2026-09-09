// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_single_load(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase;
    environment.memory().fill_incrementing(base, 2 * 64, 0x80);
    constexpr std::uint64_t immediate_base = base + 0x1000;
    environment.memory().fill_incrementing(immediate_base, 0x2000, 0x4d);

    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    // MissQueue derives isKeyword from vaddr[5]. Exercise both legal refill
    // beat orders on distinct cold lines and let the scalar scoreboard prove
    // that the two 32-byte GrantData beats are restored to the right offsets.
    const std::array<memblock::LoadTransaction, 2> transactions{{
        {
            .address = base + 0x18,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 0,
            .sq = 0,
            .pdest = 5,
            .lane = 0,
        },
        {
            .address = base + 0x68,
            .op = memblock::LoadOp::ld,
            .rob = 2,
            .lq = 1,
            .sq = 0,
            .pdest = 6,
            .lane = 1,
        },
    }};
    for (const auto &transaction : transactions) {
        environment.expect_load(transaction);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction) ||
            !environment.run_until_complete(512)) {
            std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle="
                      << environment.cycle() << " address=0x" << std::hex
                      << transaction.address << std::dec << " reason="
                      << environment.error() << '\n';
            return 1;
        }
    }
    if (environment.dcache_refills() != 2 ||
        environment.dcache_nonkeyword_refills() != 1 ||
        environment.dcache_keyword_refills() != 1 ||
        environment.dcache_grant_acks() != 2 ||
        environment.writebacks() != 2) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle=" << environment.cycle()
                  << " phase=refill-order refills="
                  << environment.dcache_refills() << " nonkeyword="
                  << environment.dcache_nonkeyword_refills() << " keyword="
                  << environment.dcache_keyword_refills() << " grant_acks="
                  << environment.dcache_grant_acks() << " writebacks="
                  << environment.writebacks() << '\n';
        return 1;
    }

    constexpr std::array<std::int32_t, 5> immediates{
        0, 1, 2047, -1, -2048,
    };
    constexpr std::array<std::uint64_t, 5> immediate_addresses{
        immediate_base + 0x008,
        immediate_base + 0x040,
        immediate_base + 0x800,
        immediate_base + 0xfc0,
        immediate_base + 0x1000,
    };
    constexpr std::array<std::uint64_t, 5> immediate_values{
        0x0123456789abcdefULL,
        0xfedcba9876543210ULL,
        0x55aa33cc0ff09669ULL,
        0xa55a5aa5c33c3cc3ULL,
        0xdeadbeef10293847ULL,
    };
    for (unsigned index = 0; index < immediate_addresses.size(); ++index) {
        environment.memory().write_u64(
            immediate_addresses[index], immediate_values[index]);
    }
    for (unsigned index = 0; index < immediates.size(); ++index) {
        const memblock::LoadTransaction transaction{
            .address = immediate_addresses[index],
            .immediate = immediates[index],
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(16 + index),
            .lq = static_cast<std::uint8_t>(2 + index),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(16 + index),
            .lane = index % memblock::kScalarLoadLanes,
        };
        environment.expect_load(transaction);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction) ||
            !environment.run_until_complete(512)) {
            std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle="
                      << environment.cycle() << " phase=immediate index="
                      << index << " immediate=" << immediates[index]
                      << " effective_address=0x" << std::hex
                      << transaction.address << std::dec << " reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    memblock::Environment merge(argc, argv);
    constexpr std::uint64_t merge_line = base + 0x200;
    merge.memory().fill_incrementing(merge_line, 64, 0x39);
    if (!merge.reset()) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL phase=merge-reset reason="
                  << merge.error() << '\n';
        return 1;
    }
    const std::vector<memblock::LoadTransaction> merged_loads{
        {
            .address = merge_line + 0x08,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .sq = 0,
            .pdest = 7,
            .lane = 0,
        },
        {
            .address = merge_line + 0x18,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 1,
            .sq = 0,
            .pdest = 8,
            .lane = 1,
        },
    };
    for (const auto &transaction : merged_loads) {
        merge.expect_load(transaction);
    }
    merge.force_next_dcache_response_delay(128);
    if (!merge.enqueue_load_batch(merged_loads, {0, 1}) ||
        !merge.issue_load_batch(merged_loads, 128, true) ||
        !merge.run_until_complete(4096) ||
        merge.tilelink_requests() != 1 || merge.dcache_refills() != 1 ||
        merge.dcache_nonkeyword_refills() != 1 ||
        merge.dcache_keyword_refills() != 0 ||
        merge.dcache_grant_acks() != 1 || merge.writebacks() != 2 ||
        merge.dcache_response_delays() < 128) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle=" << merge.cycle()
                  << " phase=same-line-merge requests="
                  << merge.tilelink_requests() << " refills="
                  << merge.dcache_refills() << " nonkeyword="
                  << merge.dcache_nonkeyword_refills() << " keyword="
                  << merge.dcache_keyword_refills() << " grant_acks="
                  << merge.dcache_grant_acks() << " writebacks="
                  << merge.writebacks() << " response_delays="
                  << merge.dcache_response_delays() << " reason="
                  << merge.error() << '\n';
        return 1;
    }

    memblock::Environment partial(argc, argv);
    constexpr std::uint64_t partial_line = base + 0x400;
    partial.memory().fill_incrementing(partial_line, 64, 0xd1);
    if (!partial.reset()) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL phase=partial-reset reason="
                  << partial.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction partial_load{
        .address = partial_line + 0x28,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .sq = 0,
        .pdest = 9,
        .lane = 0,
    };
    partial.expect_load(partial_load);
    partial.force_next_dcache_interbeat_delay(128);
    if (!partial.enqueue_load(partial_load) ||
        !partial.issue_load(partial_load) ||
        !partial.run_until_complete(512) ||
        partial.dcache_keyword_refills() != 1 ||
        partial.dcache_grant_data_beats() != 1 ||
        partial.dcache_grant_acks() != 1 || partial.writebacks() != 1) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle=" << partial.cycle()
                  << " phase=partial-refill-early keyword="
                  << partial.dcache_keyword_refills() << " grant_beats="
                  << partial.dcache_grant_data_beats() << " grant_acks="
                  << partial.dcache_grant_acks() << " writebacks="
                  << partial.writebacks() << " reason=" << partial.error()
                  << '\n';
        return 1;
    }
    const std::uint64_t partial_writeback_cycle = partial.cycle();
    if (!partial.run_cycles(256) ||
        partial.dcache_grant_data_beats() != 2 ||
        partial.dcache_grant_acks() != 1 || partial.writebacks() != 1 ||
        !partial.dcache_grants_drained()) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle=" << partial.cycle()
                  << " phase=partial-refill-drain grant_beats="
                  << partial.dcache_grant_data_beats() << " grant_acks="
                  << partial.dcache_grant_acks() << " writebacks="
                  << partial.writebacks() << " reason=" << partial.error()
                  << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_SINGLE_LOAD_PASS"
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " writebacks=" << environment.writebacks()
              << " nonkeyword_refills="
              << environment.dcache_nonkeyword_refills()
              << " keyword_refills=" << environment.dcache_keyword_refills()
              << " grant_acks=" << environment.dcache_grant_acks()
              << " merged_loads=" << merged_loads.size()
              << " merge_refills=" << merge.dcache_refills()
              << " merge_cycles=" << merge.cycle()
              << " partial_writeback_cycle=" << partial_writeback_cycle
              << " partial_grant_beats="
              << partial.dcache_grant_data_beats()
              << " partial_cycles=" << partial.cycle()
              << " immediate_loads=" << immediates.size()
              << " immediate_min=-2048 immediate_max=2047"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_load_feedback(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x1a000;
    environment.memory().fill_incrementing(base, 3 * 64, 0x29);
    environment.configure_backpressure(
        0x243f6a8885a308d3ULL, true,
        memblock::ResponseLatencyProfile::spec);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::vector<memblock::LoadTransaction> cold;
    std::vector<unsigned> dispatch_lanes;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        cold.push_back(memblock::LoadTransaction{
            .address = base + lane * 64 + lane * 8,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(lane),
            .lq = static_cast<std::uint8_t>(lane),
            .pdest = static_cast<std::uint8_t>(17 + lane),
            .lane = lane,
            .rf_wen = lane != 1,
            .fp_wen = lane == 1,
        });
        dispatch_lanes.push_back(lane);
        environment.expect_load(cold.back());
    }

    if (!environment.enqueue_load_batch(cold, dispatch_lanes) ||
        !environment.issue_load_batch(cold, 256)) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << environment.cycle() << " phase=cold-issue reason="
                  << environment.error() << '\n';
        return 1;
    }
    auto check_wakeups = [&](const std::vector<memblock::LoadTransaction> &loads,
                             const char *phase) {
        const auto &stats = environment.scalar_load_feedback_stats();
        for (const auto &load : loads) {
            const auto &sample = stats.last_wakeup[load.lane];
            if (stats.wakeups[load.lane] == 0 || !sample.valid ||
                sample.pdest != load.pdest || sample.rf_wen != load.rf_wen ||
                sample.fp_wen != load.fp_wen) {
                std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                          << environment.cycle() << " phase=" << phase
                          << " lane=" << load.lane
                          << " expected_pdest="
                          << static_cast<unsigned>(load.pdest)
                          << " actual_pdest="
                          << static_cast<unsigned>(sample.pdest)
                          << " expected_rf_wen=" << load.rf_wen
                          << " actual_rf_wen=" << sample.rf_wen
                          << " expected_fp_wen=" << load.fp_wen
                          << " actual_fp_wen=" << sample.fp_wen
                          << " wakeups=" << stats.wakeups[load.lane] << '\n';
                return false;
            }
        }
        return true;
    };
    if (!check_wakeups(cold, "cold-wakeup") ||
        !environment.run_until_complete(8192) ||
        !environment.run_cycles(16)) {
        if (!environment.error().empty()) {
            std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                      << environment.cycle() << " phase=cold-complete reason="
                      << environment.error() << '\n';
        }
        return 1;
    }
    const auto cold_stats = environment.scalar_load_feedback_stats();
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        if (cold_stats.ld2_cancels[lane] == 0 ||
            cold_stats.wakeups[lane] != cold_stats.ld2_cancels[lane] + 1) {
            std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                      << environment.cycle() << " phase=cold-cancel lane="
                      << lane << " wakeups=" << cold_stats.wakeups[lane]
                      << " ld2_cancels=" << cold_stats.ld2_cancels[lane]
                      << " reason=unbalanced_early_wakeup\n";
            return 1;
        }
    }

    std::vector<memblock::LoadTransaction> warm = cold;
    for (unsigned lane = 0; lane < warm.size(); ++lane) {
        warm[lane].rob = static_cast<std::uint8_t>(16 + lane);
        warm[lane].lq = static_cast<std::uint8_t>(3 + lane);
        warm[lane].pdest = static_cast<std::uint8_t>(33 + lane);
    }
    environment.configure_backpressure(0, false);
    if (!environment.run_cycles(32)) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << environment.cycle() << " phase=warm-settle reason="
                  << environment.error() << '\n';
        return 1;
    }
    for (const auto &load : warm) {
        const std::uint64_t wakeups_before =
            environment.scalar_load_feedback_stats().wakeups[load.lane];
        const std::uint64_t cancels_before =
            environment.scalar_load_feedback_stats().ld2_cancels[load.lane];
        const std::uint64_t requests_before = environment.tilelink_requests();
        environment.expect_load(load);
        if (!environment.enqueue_load(load) ||
            !environment.issue_load(load, 256) ||
            !check_wakeups({load}, "warm-wakeup") ||
            !environment.run_until_complete(2048) ||
            !environment.run_cycles(8)) {
            if (!environment.error().empty()) {
                std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                          << environment.cycle()
                          << " phase=warm-complete lane=" << load.lane
                          << " reason=" << environment.error() << '\n';
            }
            return 1;
        }
        const std::uint64_t cancels_after =
            environment.scalar_load_feedback_stats().ld2_cancels[load.lane];
        const std::uint64_t wakeups_after =
            environment.scalar_load_feedback_stats().wakeups[load.lane];
        if (wakeups_after - wakeups_before !=
                cancels_after - cancels_before + 1 ||
            environment.tilelink_requests() != requests_before) {
            std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                      << environment.cycle() << " phase=warm-cancel lane="
                      << load.lane
                      << " wakeups=" << wakeups_after - wakeups_before
                      << " ld2_cancels=" << cancels_after - cancels_before
                      << " tilelink_before=" << requests_before
                      << " tilelink_after=" << environment.tilelink_requests()
                      << " reason=warm_feedback_or_residency_mismatch\n";
            return 1;
        }
    }

    std::vector<memblock::LoadTransaction> bank_conflicts;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        bank_conflicts.push_back(memblock::LoadTransaction{
            .address = base + lane * 64 + 24,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(32 + lane),
            .lq = static_cast<std::uint8_t>(6 + lane),
            .pdest = static_cast<std::uint8_t>(60 + lane),
            .lane = lane,
        });
        environment.expect_load(bank_conflicts.back());
    }
    const auto bank_stats_before = environment.scalar_load_feedback_stats();
    const std::uint64_t bank_writebacks_before = environment.writebacks();
    const std::uint64_t bank_dequeues_before = environment.lq_dequeued();
    if (!environment.enqueue_load_batch(bank_conflicts, {0, 1, 2}) ||
        !environment.issue_load_batch(bank_conflicts, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.run_until_lq_retired(4096)) {
        if (!environment.error().empty()) {
            std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                      << environment.cycle()
                      << " phase=bank-conflict-complete reason="
                      << environment.error() << '\n';
        }
        return 1;
    }
    const auto bank_stats_after = environment.scalar_load_feedback_stats();
    std::uint64_t bank_conflict_wakeups = 0;
    std::uint64_t bank_conflict_cancels = 0;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        bank_conflict_wakeups +=
            bank_stats_after.wakeups[lane] - bank_stats_before.wakeups[lane];
        bank_conflict_cancels += bank_stats_after.ld2_cancels[lane] -
            bank_stats_before.ld2_cancels[lane];
    }
    if (environment.writebacks() - bank_writebacks_before !=
            bank_conflicts.size() ||
        environment.lq_dequeued() - bank_dequeues_before !=
            bank_conflicts.size()) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << environment.cycle()
                  << " phase=bank-conflict-terminal wakeups="
                  << bank_conflict_wakeups << " cancels="
                  << bank_conflict_cancels << " writebacks="
                  << environment.writebacks() - bank_writebacks_before
                  << " dequeues="
                  << environment.lq_dequeued() - bank_dequeues_before << '\n';
        return 1;
    }

    memblock::Environment fault_environment(argc, argv);
    constexpr std::uint64_t fault_address = 0x60000000ULL;
    constexpr std::uint64_t fault_root = 0xe4000000ULL;
    const memblock::LoadTransaction fault_load{
        .address = fault_address,
        .op = memblock::LoadOp::ld,
        .rob = 72,
        .lq = 0,
        .pdest = 70,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
    };
    if (!fault_environment.reset() ||
        !fault_environment.activate_sv39(fault_root)) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << fault_environment.cycle()
                  << " phase=page-fault-configuration reason="
                  << fault_environment.error() << '\n';
        return 1;
    }
    const std::uint64_t fault_ptw_before = fault_environment.ptw_requests();
    const std::uint64_t fault_dcache_before =
        fault_environment.tilelink_requests();
    fault_environment.expect_load(fault_load);
    if (!fault_environment.enqueue_load(fault_load) ||
        !fault_environment.issue_load(fault_load, 256) ||
        !fault_environment.run_until_complete(4096) ||
        !fault_environment.run_until_lq_retired(1024) ||
        !fault_environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << fault_environment.cycle()
                  << " phase=page-fault-complete reason="
                  << fault_environment.error() << '\n';
        return 1;
    }
    const auto &fault_stats = fault_environment.scalar_load_feedback_stats();
    std::uint64_t fault_wakeups = 0;
    std::uint64_t fault_cancels = 0;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        fault_wakeups += fault_stats.wakeups[lane];
        fault_cancels += fault_stats.ld2_cancels[lane];
    }
    if (fault_cancels == 0 || fault_wakeups != fault_cancels ||
        fault_environment.ptw_requests() <= fault_ptw_before ||
        fault_environment.tilelink_requests() != fault_dcache_before) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << fault_environment.cycle()
                  << " phase=page-fault-classification wakeups="
                  << fault_wakeups << " cancels=" << fault_cancels
                  << " ptw_before=" << fault_ptw_before << " ptw_after="
                  << fault_environment.ptw_requests()
                  << " dcache_before=" << fault_dcache_before
                  << " dcache_after=" << fault_environment.tilelink_requests()
                  << '\n';
        return 1;
    }

    auto run_uncache_feedback = [&](bool mmio, std::uint64_t &wakeups,
                                    std::uint64_t &cancels,
                                    std::uint64_t &case_cycles) {
        memblock::Environment uncache_environment(argc, argv);
        const std::uint64_t virtual_address = mmio
            ? 0x61000018ULL : 0x62000018ULL;
        const std::uint64_t physical_address = mmio
            ? 0xa1000018ULL : 0xa2000018ULL;
        const std::uint64_t root = mmio ? 0xe5000000ULL : 0xe6000000ULL;
        uncache_environment.memory().fill_incrementing(
            physical_address & ~std::uint64_t{63}, 64, mmio ? 0xa5 : 0xb7);
        uncache_environment.configure_backpressure(
            mmio ? 0x9b05688c2b3e6c1fULL : 0x510e527fade682d1ULL,
            true, memblock::ResponseLatencyProfile::spec);
        if (!uncache_environment.reset() ||
            !uncache_environment.map_sv39_4k(
                virtual_address, physical_address, root, true, true, false,
                false, !mmio, mmio) ||
            !uncache_environment.activate_sv39(root)) {
            std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                      << uncache_environment.cycle() << " phase="
                      << (mmio ? "mmio" : "nc")
                      << "-configuration reason="
                      << uncache_environment.error() << '\n';
            return false;
        }
        const memblock::LoadTransaction transaction{
            .address = virtual_address,
            .oracle_address = physical_address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(mmio ? 80 : 81),
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(mmio ? 71 : 72),
            .lane = static_cast<unsigned>(mmio ? 1 : 2),
            .expected_debug_is_mmio = mmio,
            .expected_debug_is_ncio = mmio
                ? std::optional<bool>{false} : std::nullopt,
            .expected_debug_is_perf_cnt = false,
        };
        const std::uint64_t ptw_before = uncache_environment.ptw_requests();
        const std::uint64_t dcache_before =
            uncache_environment.tilelink_requests();
        const std::uint64_t uncache_before =
            uncache_environment.uncache_requests();
        uncache_environment.expect_load(transaction);
        if (!uncache_environment.set_rob_head(transaction.rob) ||
            !uncache_environment.enqueue_load(transaction) ||
            !uncache_environment.issue_load(transaction, 2048) ||
            (mmio && !uncache_environment.wait_for_mmio_request(
                transaction.rob, transaction.rob_flag, 4096)) ||
            !uncache_environment.run_until_complete(8192) ||
            !uncache_environment.run_until_lq_retired(2048) ||
            !uncache_environment.run_cycles(16)) {
            std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                      << uncache_environment.cycle() << " phase="
                      << (mmio ? "mmio" : "nc") << "-complete reason="
                      << uncache_environment.error() << '\n';
            return false;
        }
        const auto &feedback =
            uncache_environment.scalar_load_feedback_stats();
        bool metadata_match = false;
        for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
            wakeups += feedback.wakeups[lane];
            cancels += feedback.ld2_cancels[lane];
            const auto &sample = feedback.last_wakeup[lane];
            metadata_match = metadata_match ||
                (sample.valid && sample.pdest == transaction.pdest &&
                 sample.rf_wen == transaction.rf_wen &&
                 sample.fp_wen == transaction.fp_wen);
        }
        case_cycles = uncache_environment.cycle();
        if (cancels == 0 || wakeups != cancels + 1 || !metadata_match ||
            uncache_environment.ptw_requests() <= ptw_before ||
            uncache_environment.tilelink_requests() != dcache_before ||
            uncache_environment.uncache_requests() != uncache_before + 1) {
            std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                      << uncache_environment.cycle() << " phase="
                      << (mmio ? "mmio" : "nc")
                      << "-classification wakeups=" << wakeups
                      << " cancels=" << cancels
                      << " metadata_match=" << metadata_match
                      << " ptw_before=" << ptw_before << " ptw_after="
                      << uncache_environment.ptw_requests()
                      << " dcache_before=" << dcache_before
                      << " dcache_after="
                      << uncache_environment.tilelink_requests()
                      << " uncache_before=" << uncache_before
                      << " uncache_after="
                      << uncache_environment.uncache_requests() << '\n';
            return false;
        }
        return true;
    };
    std::uint64_t mmio_wakeups = 0;
    std::uint64_t mmio_cancels = 0;
    std::uint64_t mmio_cycles = 0;
    std::uint64_t nc_wakeups = 0;
    std::uint64_t nc_cancels = 0;
    std::uint64_t nc_cycles = 0;
    if (!run_uncache_feedback(
            true, mmio_wakeups, mmio_cancels, mmio_cycles) ||
        !run_uncache_feedback(false, nc_wakeups, nc_cancels, nc_cycles)) {
        return 1;
    }

    memblock::Environment forwarding_environment(argc, argv);
    constexpr std::uint64_t forwarding_line =
        memblock::kDefaultMemoryBase + 0x1e000;
    constexpr std::uint64_t forwarding_address = forwarding_line + 24;
    forwarding_environment.memory().fill_incrementing(
        forwarding_line, 64, 0xc3);
    if (!forwarding_environment.reset()) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << forwarding_environment.cycle()
                  << " phase=forwarding-reset reason="
                  << forwarding_environment.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction forwarding_warm{
        .address = forwarding_address,
        .op = memblock::LoadOp::ld,
        .rob = 88,
        .lq = 0,
        .sq = 0,
        .pdest = 73,
        .lane = 0,
    };
    forwarding_environment.expect_load(forwarding_warm);
    if (!forwarding_environment.enqueue_load(forwarding_warm) ||
        !forwarding_environment.issue_load(forwarding_warm, 256) ||
        !forwarding_environment.run_until_complete(4096) ||
        !forwarding_environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << forwarding_environment.cycle()
                  << " phase=forwarding-warm reason="
                  << forwarding_environment.error() << '\n';
        return 1;
    }
    const auto &forwarding_before_stats =
        forwarding_environment.scalar_load_feedback_stats();
    std::uint64_t forwarding_wakeups_before = 0;
    std::uint64_t forwarding_cancels_before = 0;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        forwarding_wakeups_before += forwarding_before_stats.wakeups[lane];
        forwarding_cancels_before += forwarding_before_stats.ld2_cancels[lane];
    }
    const std::uint64_t forwarding_dcache_before =
        forwarding_environment.tilelink_requests();
    const memblock::StoreTransaction forwarding_store{
        .address = forwarding_address,
        .data = 0xd1e2f30415263748ULL,
        .op = memblock::StoreOp::sd,
        .rob = 89,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
    };
    const memblock::LoadTransaction forwarding_load{
        .address = forwarding_address,
        .op = memblock::LoadOp::ld,
        .rob = 90,
        .lq = 1,
        .sq = 1,
        .pdest = 74,
        .lane = 1,
    };
    forwarding_environment.expect_store(forwarding_store);
    forwarding_environment.expect_load_data(
        forwarding_load, forwarding_store.data);
    if (!forwarding_environment.enqueue_store(forwarding_store, 0) ||
        !forwarding_environment.enqueue_load(forwarding_load) ||
        !forwarding_environment.issue_store_address(forwarding_store, 256) ||
        !forwarding_environment.issue_load(forwarding_load, 256) ||
        !forwarding_environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << forwarding_environment.cycle()
                  << " phase=forwarding-data-wait reason="
                  << forwarding_environment.error() << '\n';
        return 1;
    }
    const auto &forwarding_wait_stats =
        forwarding_environment.scalar_load_feedback_stats();
    std::uint64_t forwarding_cancels_wait = 0;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        forwarding_cancels_wait += forwarding_wait_stats.ld2_cancels[lane];
    }
    if (forwarding_cancels_wait <= forwarding_cancels_before) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << forwarding_environment.cycle()
                  << " phase=forwarding-pre-data-classification cancels_before="
                  << forwarding_cancels_before << " cancels_after="
                  << forwarding_cancels_wait << '\n';
        return 1;
    }
    if (!forwarding_environment.issue_store_data(forwarding_store, 256) ||
        !forwarding_environment.run_until_store_complete(512) ||
        !forwarding_environment.run_until_complete(4096) ||
        !forwarding_environment.run_until_lq_retired(1024) ||
        !forwarding_environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << forwarding_environment.cycle()
                  << " phase=forwarding-complete reason="
                  << forwarding_environment.error() << '\n';
        return 1;
    }
    const auto &forwarding_stats =
        forwarding_environment.scalar_load_feedback_stats();
    std::uint64_t forwarding_wakeups = 0;
    std::uint64_t forwarding_cancels = 0;
    bool forwarding_metadata_match = false;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        forwarding_wakeups += forwarding_stats.wakeups[lane];
        forwarding_cancels += forwarding_stats.ld2_cancels[lane];
        const auto &sample = forwarding_stats.last_wakeup[lane];
        forwarding_metadata_match = forwarding_metadata_match ||
            (sample.valid && sample.pdest == forwarding_load.pdest &&
             sample.rf_wen == forwarding_load.rf_wen &&
             sample.fp_wen == forwarding_load.fp_wen);
    }
    forwarding_wakeups -= forwarding_wakeups_before;
    forwarding_cancels -= forwarding_cancels_before;
    if (forwarding_cancels == 0 ||
        forwarding_wakeups != forwarding_cancels + 1 ||
        !forwarding_metadata_match ||
        forwarding_environment.tilelink_requests() != forwarding_dcache_before) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << forwarding_environment.cycle()
                  << " phase=forwarding-classification wakeups="
                  << forwarding_wakeups << " cancels=" << forwarding_cancels
                  << " metadata_match=" << forwarding_metadata_match
                  << " dcache_before=" << forwarding_dcache_before
                  << " dcache_after="
                  << forwarding_environment.tilelink_requests() << '\n';
        return 1;
    }

    memblock::Environment pmp_environment(argc, argv);
    constexpr std::uint64_t pmp_base =
        memblock::kDefaultMemoryBase + 0xa0000;
    constexpr std::uint64_t pmp_size = 0x1000;
    const std::uint64_t pmp_napot_address =
        (pmp_base | (pmp_size / 2 - 1)) >> 2;
    const memblock::LoadTransaction pmp_load{
        .address = pmp_base + 0x80,
        .op = memblock::LoadOp::ld,
        .rob = 96,
        .lq = 0,
        .pdest = 75,
        .lane = 2,
        .expected_exception_mask = memblock::kExceptionLoadAccessFault,
    };
    if (!pmp_environment.reset() || !pmp_environment.activate_bare() ||
        !pmp_environment.configure_pmp({pmp_napot_address}, {0x18})) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << pmp_environment.cycle()
                  << " phase=pmp-configuration reason="
                  << pmp_environment.error() << '\n';
        return 1;
    }
    const std::uint64_t pmp_ptw_before = pmp_environment.ptw_requests();
    const std::uint64_t pmp_dcache_before =
        pmp_environment.tilelink_requests();
    const std::uint64_t pmp_uncache_before = pmp_environment.uncache_requests();
    pmp_environment.expect_load(pmp_load);
    if (!pmp_environment.set_rob_head(pmp_load.rob) ||
        !pmp_environment.enqueue_load(pmp_load) ||
        !pmp_environment.issue_load(pmp_load, 256) ||
        !pmp_environment.run_until_complete(4096) ||
        !pmp_environment.run_until_lq_retired(1024) ||
        !pmp_environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << pmp_environment.cycle()
                  << " phase=pmp-complete reason="
                  << pmp_environment.error() << '\n';
        return 1;
    }
    const auto &pmp_stats = pmp_environment.scalar_load_feedback_stats();
    std::uint64_t pmp_wakeups = 0;
    std::uint64_t pmp_cancels = 0;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        pmp_wakeups += pmp_stats.wakeups[lane];
        pmp_cancels += pmp_stats.ld2_cancels[lane];
    }
    if (pmp_cancels == 0 || pmp_wakeups != pmp_cancels ||
        pmp_environment.ptw_requests() != pmp_ptw_before ||
        pmp_environment.tilelink_requests() != pmp_dcache_before ||
        pmp_environment.uncache_requests() != pmp_uncache_before) {
        std::cerr << "MEMBLOCK_LOAD_FEEDBACK_FAIL cycle="
                  << pmp_environment.cycle()
                  << " phase=pmp-classification wakeups=" << pmp_wakeups
                  << " cancels=" << pmp_cancels
                  << " ptw_before=" << pmp_ptw_before << " ptw_after="
                  << pmp_environment.ptw_requests()
                  << " dcache_before=" << pmp_dcache_before
                  << " dcache_after=" << pmp_environment.tilelink_requests()
                  << " uncache_before=" << pmp_uncache_before
                  << " uncache_after=" << pmp_environment.uncache_requests()
                  << '\n';
        return 1;
    }
    const auto &warm_stats = environment.scalar_load_feedback_stats();
    std::uint64_t wakeups = 0;
    std::uint64_t cancels = 0;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        wakeups += warm_stats.wakeups[lane];
        cancels += warm_stats.ld2_cancels[lane];
    }

    std::cout << "MEMBLOCK_LOAD_FEEDBACK_PASS"
              << " cycle=" << environment.cycle()
              << " lanes=" << memblock::kScalarLoadLanes
              << " cold=3 warm=3 bank_conflict=3"
              << " bank_conflict_cancels=" << bank_conflict_cancels
              << " page_fault_cancels=" << fault_cancels
              << " page_fault_cycles=" << fault_environment.cycle()
              << " page_fault_ptw="
              << fault_environment.ptw_requests() - fault_ptw_before
              << " mmio_cancels=" << mmio_cancels
              << " nc_cancels=" << nc_cancels
              << " mmio_cycles=" << mmio_cycles
              << " nc_cycles=" << nc_cycles
              << " forwarding_cancels=" << forwarding_cancels
              << " forwarding_cycles=" << forwarding_environment.cycle()
              << " pmp_cancels=" << pmp_cancels
              << " pmp_cycles=" << pmp_environment.cycle()
              << " wakeups=" << wakeups
              << " ld2_cancels=" << cancels
              << " writebacks=" << environment.writebacks()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_topdown_contracts(int argc, char **argv)
{
    memblock::Environment passthrough(argc, argv);
    if (!passthrough.reset()) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=reset reason="
                  << passthrough.error() << '\n';
        return 1;
    }
    constexpr std::array<std::pair<bool, bool>, 8> miss_pattern{{
        {false, false}, {true, false}, {false, true}, {true, true},
        {true, false}, {false, false}, {false, true}, {true, true},
    }};
    std::uint64_t expected_l2_cycles = 0;
    std::uint64_t expected_l3_cycles = 0;
    for (const auto &[l2_miss, l3_miss] : miss_pattern) {
        passthrough.drive_top_down_misses(l2_miss, l3_miss);
        expected_l2_cycles += l2_miss;
        expected_l3_cycles += l3_miss;
        if (!passthrough.run_cycles(1)) {
            std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=l2-l3-delay"
                      << " reason=" << passthrough.error() << '\n';
            return 1;
        }
    }
    passthrough.drive_top_down_misses(false, false);
    if (!passthrough.run_cycles(2)) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=l2-l3-drain"
                  << " reason=" << passthrough.error() << '\n';
        return 1;
    }
    const auto passthrough_stats = passthrough.top_down_stats();
    if (passthrough_stats.l2_miss_cycles != expected_l2_cycles ||
        passthrough_stats.l3_miss_cycles != expected_l3_cycles ||
        passthrough_stats.delay_checks != miss_pattern.size() + 2) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=l2-l3-counts"
                  << " expected=" << expected_l2_cycles << ','
                  << expected_l3_cycles << " observed="
                  << passthrough_stats.l2_miss_cycles << ','
                  << passthrough_stats.l3_miss_cycles << " checks="
                  << passthrough_stats.delay_checks << '\n';
        return 1;
    }

    memblock::Environment miss(argc, argv);
    constexpr std::uint64_t miss_line =
        memblock::kDefaultMemoryBase + 0x1e000;
    miss.memory().fill_incrementing(miss_line, 64, 0x91);
    if (!miss.reset()) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=miss-reset reason="
                  << miss.error() << '\n';
        return 1;
    }
    miss.force_next_dcache_response_delay(128);
    const memblock::LoadTransaction load{
        .address = miss_line + 24,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 0,
        .sq = 0,
        .pdest = 91,
        .lane = 0,
    };
    miss.expect_load(load);
    if (!miss.enqueue_load(load) || !miss.issue_load(load, 4096) ||
        !miss.run_until_complete(8192) || !miss.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=cold-miss reason="
                  << miss.error() << '\n';
        return 1;
    }
    const auto miss_stats = miss.top_down_stats();
    if (miss_stats.l1_miss_cycles == 0 ||
        miss_stats.replay_allocate_cycles == 0) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=miss-events"
                  << " l1_miss=" << miss_stats.l1_miss_cycles
                  << " replay=" << miss_stats.replay_allocate_cycles << '\n';
        return 1;
    }

    memblock::Environment sq(argc, argv);
    if (!sq.reset()) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=sq-reset reason="
                  << sq.error() << '\n';
        return 1;
    }
    for (unsigned index = 0; index < memblock::kStoreQueueEntries; ++index) {
        const memblock::StoreTransaction store{
            .address = memblock::kDefaultMemoryBase + 0x20000 + index * 8,
            .data = 0x1000000000000000ULL + index,
            .op = memblock::StoreOp::sd,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .sq = memblock::sq_pointer_value(index),
            .sq_flag = memblock::sq_pointer_flag(index),
            .address_lane = index % memblock::kScalarStoreLanes,
            .data_lane = (index + 1) % memblock::kScalarStoreLanes,
        };
        if (!sq.enqueue_store_pressure(store, 0)) {
            std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=sq-fill"
                      << " index=" << index << " reason=" << sq.error()
                      << '\n';
            return 1;
        }
    }
    if (!sq.run_cycles(4) || sq.top_down_stats().sq_full_cycles == 0 ||
        sq.sq_allocated() != memblock::kStoreQueueEntries) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=sq-full"
                  << " allocated=" << sq.sq_allocated() << " full_cycles="
                  << sq.top_down_stats().sq_full_cycles << " reason="
                  << sq.error() << '\n';
        return 1;
    }

    memblock::Environment sb(argc, argv);
    constexpr std::uint64_t sb_base =
        memblock::kDefaultMemoryBase + 0x24000;
    sb.memory().fill_incrementing(sb_base, 16 * 64, 0x39);
    sb.configure_backpressure(0x510e527fade682d1ULL, false);
    if (!sb.reset() || !sb.set_sbuffer_timeout((1U << 22) - 1)) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=sb-reset reason="
                  << sb.error() << '\n';
        return 1;
    }
    unsigned buffered_stores = 0;
    for (unsigned index = 0; index < 16; ++index) {
        const memblock::StoreTransaction store{
            .address = sb_base + index * 64,
            .data = 0xa500000000000000ULL + index,
            .op = memblock::StoreOp::sd,
            .rob = static_cast<std::uint8_t>(index),
            .sq = static_cast<std::uint8_t>(index),
            .address_lane = index % memblock::kScalarStoreLanes,
            .data_lane = (index + 1) % memblock::kScalarStoreLanes,
        };
        sb.expect_store(store);
        sb.force_next_dcache_response_delay(4096);
        if (!sb.set_rob_head(store.rob, store.rob_flag) ||
            !sb.enqueue_store(store, 0) ||
            !sb.issue_store_address(store, 4096) ||
            !sb.issue_store_data(store, 4096) ||
            !sb.run_until_store_complete(8192) ||
            !sb.commit_store(store, 8192)) {
            std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=sb-fill"
                      << " index=" << index << " reason=" << sb.error()
                      << '\n';
            return 1;
        }
        ++buffered_stores;
        if (!sb.run_cycles(2)) {
            std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=sb-sample"
                      << " index=" << index << " reason=" << sb.error()
                      << '\n';
            return 1;
        }
        if (sb.top_down_stats().sb_full_cycles != 0) {
            break;
        }
    }
    if (sb.top_down_stats().sb_full_cycles == 0) {
        std::cerr << "MEMBLOCK_TOPDOWN_CONTRACTS_FAIL phase=sb-full"
                  << " buffered=" << buffered_stores << " dcache_requests="
                  << sb.tilelink_requests() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_TOPDOWN_CONTRACTS_PASS"
              << " delay_checks=" << passthrough_stats.delay_checks
              << " l2_miss_cycles=" << passthrough_stats.l2_miss_cycles
              << " l3_miss_cycles=" << passthrough_stats.l3_miss_cycles
              << " l1_miss_cycles=" << miss_stats.l1_miss_cycles
              << " replay_cycles=" << miss_stats.replay_allocate_cycles
              << " sq_full_cycles=" << sq.top_down_stats().sq_full_cycles
              << " sb_full_cycles=" << sb.top_down_stats().sb_full_cycles
              << " buffered_stores=" << buffered_stores
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_memory_violation(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x1b000;
    constexpr std::uint64_t conflict_address = base + 24;
    environment.memory().fill_incrementing(base, 64, 0x4d);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::StoreTransaction nonoverlap_store{
        .address = base + 8,
        .data = 0x123456789abcdef0ULL,
        .op = memblock::StoreOp::sd,
        .rob = memblock::rob_pointer_value(156),
        .rob_flag = memblock::rob_pointer_flag(156),
        .sq = 0,
        .address_lane = 1,
        .data_lane = 0,
    };
    const memblock::LoadTransaction nonoverlap_load{
        .address = base + 40,
        .op = memblock::LoadOp::ld,
        .rob = memblock::rob_pointer_value(157),
        .rob_flag = memblock::rob_pointer_flag(157),
        .lq = 0,
        .sq = 1,
        .pdest = 40,
        .lane = 1,
        .ftq_ptr = 36,
        .ftq_offset = 4,
    };
    environment.expect_store(nonoverlap_store);
    environment.expect_load(nonoverlap_load);
    if (!environment.enqueue_store(nonoverlap_store, 0) ||
        !environment.enqueue_load(nonoverlap_load) ||
        !environment.issue_store_data(nonoverlap_store, 256) ||
        !environment.issue_load(nonoverlap_load, 256) ||
        !environment.run_until_complete(4096)) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=nonoverlap-load reason=" << environment.error()
                  << '\n';
        return 1;
    }
    const auto nonoverlap_violations =
        environment.memory_violation_stats().count;
    if (!environment.issue_store_address(nonoverlap_store, 256) ||
        !environment.run_until_store_complete(512) ||
        !environment.run_cycles(64) ||
        environment.memory_violation_stats().count != nonoverlap_violations ||
        !environment.commit_store(nonoverlap_store, 4096) ||
        !environment.run_until_sbuffer_empty(4096)) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=nonoverlap-check count="
                  << environment.memory_violation_stats().count
                  << " expected_count=" << nonoverlap_violations
                  << " reason=" << (environment.error().empty()
                          ? "nonoverlap_generated_redirect"
                          : environment.error()) << '\n';
        return 1;
    }

    const memblock::StoreTransaction older_store{
        .address = conflict_address,
        .data = 0xdecafbad12345678ULL,
        .op = memblock::StoreOp::sd,
        .rob = memblock::rob_pointer_value(158),
        .rob_flag = memblock::rob_pointer_flag(158),
        .sq = 1,
        .address_lane = 0,
        .data_lane = 1,
    };
    std::vector<memblock::LoadTransaction> younger_loads;
    for (unsigned index = 0; index < memblock::kScalarLoadLanes; ++index) {
        const std::uint64_t rob_offset = 159 + index;
        younger_loads.push_back(memblock::LoadTransaction{
            .address = conflict_address,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(rob_offset),
            .rob_flag = memblock::rob_pointer_flag(rob_offset),
            .lq = static_cast<std::uint8_t>(1 + index),
            .sq = 2,
            .pdest = static_cast<std::uint8_t>(41 + index),
            .lane = index,
            .predecode_rvc = index == 0,
            .ftq_ptr = 37 + index,
            .ftq_offset = static_cast<std::uint8_t>(6 + index),
        });
    }
    const auto &oldest_load = younger_loads.front();

    environment.expect_store(older_store);
    for (const auto &load : younger_loads) {
        environment.expect_load(load);
    }
    if (!environment.enqueue_store(older_store, 0) ||
        !environment.enqueue_load_batch(younger_loads, {0, 1, 2}) ||
        !environment.issue_store_data(older_store, 256) ||
        !environment.issue_load_batch(younger_loads, 256) ||
        !environment.run_until_complete(4096)) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=speculative-load reason=" << environment.error()
                  << '\n';
        return 1;
    }

    const auto violations_before = environment.memory_violation_stats().count;
    if (!environment.issue_store_address(older_store, 256) ||
        !environment.run_until_store_complete(512)) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=store-address reason=" << environment.error()
                  << '\n';
        return 1;
    }
    for (unsigned cycle = 0;
         cycle < 512 &&
         environment.memory_violation_stats().count == violations_before;
         ++cycle) {
        if (!environment.run_cycles(1)) {
            std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                      << environment.cycle()
                      << " phase=redirect-wait reason=" << environment.error()
                      << '\n';
            return 1;
        }
    }
    if (!environment.run_cycles(32)) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=redirect-settle reason=" << environment.error()
                  << '\n';
        return 1;
    }

    const auto &stats = environment.memory_violation_stats();
    const auto violation = stats.last;
    if (stats.count != violations_before + 1 || !violation.valid ||
        violation.is_rvc != oldest_load.predecode_rvc ||
        violation.rob_flag != oldest_load.rob_flag ||
        violation.rob_value != oldest_load.rob || violation.ftq_flag ||
        violation.ftq_value != oldest_load.ftq_ptr ||
        violation.ftq_offset != oldest_load.ftq_offset || !violation.level) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=redirect-check count=" << stats.count
                  << " expected_count=" << violations_before + 1
                  << " is_rvc=" << violation.is_rvc
                  << " rob=" << violation.rob_flag << ':'
                  << static_cast<unsigned>(violation.rob_value)
                  << " ftq=" << violation.ftq_flag << ':'
                  << static_cast<unsigned>(violation.ftq_value)
                  << " ftq_offset="
                  << static_cast<unsigned>(violation.ftq_offset)
                  << " level=" << violation.level << '\n';
        return 1;
    }

    const auto wait_for_single_violation = [&environment](
        const char *phase, bool expected_rvc, bool expected_rob_flag,
        std::uint8_t expected_rob, std::uint64_t expected_ftq,
        std::uint8_t expected_ftq_offset) {
        for (unsigned cycle = 0;
             cycle < 512 && environment.memory_violation_stats().count == 0;
             ++cycle) {
            if (!environment.run_cycles(1)) {
                std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                          << environment.cycle() << " phase=" << phase
                          << "-wait reason=" << environment.error() << '\n';
                return false;
            }
        }
        if (!environment.run_cycles(32)) {
            std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << "-settle reason=" << environment.error() << '\n';
            return false;
        }
        const auto &result = environment.memory_violation_stats();
        const auto &redirect = result.last;
        if (result.count != 1 || !redirect.valid ||
            redirect.is_rvc != expected_rvc ||
            redirect.rob_flag != expected_rob_flag ||
            redirect.rob_value != expected_rob || redirect.ftq_flag ||
            redirect.ftq_value != expected_ftq ||
            redirect.ftq_offset != expected_ftq_offset || !redirect.level) {
            std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << "-check count=" << result.count
                      << " is_rvc=" << redirect.is_rvc
                      << " rob=" << redirect.rob_flag << ':'
                      << static_cast<unsigned>(redirect.rob_value)
                      << " ftq=" << redirect.ftq_flag << ':'
                      << static_cast<unsigned>(redirect.ftq_value)
                      << " ftq_offset="
                      << static_cast<unsigned>(redirect.ftq_offset)
                      << " level=" << redirect.level << '\n';
            return false;
        }
        return true;
    };

    constexpr std::uint64_t vector_line = base + 0x1000;
    constexpr std::uint64_t vector_conflict = vector_line + 24;
    environment.memory().fill_incrementing(vector_line, 64, 0x75);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle() << " phase=vector-reset reason="
                  << environment.error() << '\n';
        return 1;
    }
    const memblock::StoreTransaction vector_older_store{
        .address = vector_conflict,
        .data = 0xfedcba9876543210ULL,
        .op = memblock::StoreOp::sd,
        .rob = 48,
        .sq = 0,
        .address_lane = 1,
        .data_lane = 0,
    };
    const memblock::VectorMemoryTransaction vector_younger_load{
        .address = vector_conflict,
        .eew = 3,
        .vl = 1,
        .rob = 49,
        .lq = 0,
        .sq = 1,
        .pdest = 91,
        .lane = 0,
        .flow_num = 1,
        .ftq_ptr = 52,
        .ftq_offset = 9,
    };
    environment.expect_store(vector_older_store);
    environment.expect_vector(vector_younger_load);
    if (!environment.enqueue_store(vector_older_store, 0) ||
        !environment.enqueue_vector(vector_younger_load) ||
        !environment.issue_store_data(vector_older_store, 256) ||
        !environment.issue_vector(vector_younger_load, 256) ||
        !environment.run_until_vector_complete(4096) ||
        !environment.issue_store_address(vector_older_store, 256) ||
        !environment.run_until_store_complete(512) ||
        !wait_for_single_violation(
            "vector-redirect", false, vector_younger_load.rob_flag,
            vector_younger_load.rob, vector_younger_load.ftq_ptr,
            vector_younger_load.ftq_offset)) {
        if (!environment.error().empty()) {
            std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                      << environment.cycle() << " phase=vector-trigger reason="
                      << environment.error() << '\n';
        }
        return 1;
    }

    constexpr std::uint64_t concurrent_line_a = base + 0x2000;
    constexpr std::uint64_t concurrent_line_b = base + 0x3000;
    constexpr std::uint64_t concurrent_address_a = concurrent_line_a + 8;
    constexpr std::uint64_t concurrent_address_b = concurrent_line_b + 40;
    environment.memory().fill_incrementing(concurrent_line_a, 64, 0x83);
    environment.memory().fill_incrementing(concurrent_line_b, 64, 0x91);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                  << environment.cycle() << " phase=concurrent-reset reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::vector<memblock::StoreTransaction> concurrent_stores{
        {
            .address = concurrent_address_a,
            .data = 0x0102030405060708ULL,
            .op = memblock::StoreOp::sd,
            .rob = 62,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 0,
        },
        {
            .address = concurrent_address_b,
            .data = 0x1112131415161718ULL,
            .op = memblock::StoreOp::sd,
            .rob = 63,
            .sq = 1,
            .address_lane = 1,
            .data_lane = 1,
        },
    };
    const std::vector<memblock::LoadTransaction> concurrent_loads{
        {
            .address = concurrent_address_a,
            .op = memblock::LoadOp::ld,
            .rob = 65,
            .lq = 0,
            .sq = 2,
            .pdest = 92,
            .lane = 1,
            .predecode_rvc = true,
            .ftq_ptr = 53,
            .ftq_offset = 10,
        },
        {
            .address = concurrent_address_b,
            .op = memblock::LoadOp::ld,
            .rob = 64,
            .lq = 1,
            .sq = 2,
            .pdest = 93,
            .lane = 2,
            .ftq_ptr = 54,
            .ftq_offset = 11,
        },
    };
    for (const auto &store : concurrent_stores) {
        environment.expect_store(store);
    }
    for (const auto &load : concurrent_loads) {
        environment.expect_load(load);
    }
    const auto &concurrent_oldest = concurrent_loads.back();
    if (!environment.enqueue_store(concurrent_stores[0], 0) ||
        !environment.enqueue_store(concurrent_stores[1], 1) ||
        !environment.enqueue_load_batch(concurrent_loads, {0, 1}) ||
        !environment.issue_store_data(concurrent_stores[0], 256) ||
        !environment.issue_store_data(concurrent_stores[1], 256) ||
        !environment.issue_load_batch(concurrent_loads, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.issue_store_address_batch(concurrent_stores, 256) ||
        !environment.run_until_store_complete(512) ||
        !wait_for_single_violation(
            "concurrent-redirect", concurrent_oldest.predecode_rvc,
            concurrent_oldest.rob_flag, concurrent_oldest.rob,
            concurrent_oldest.ftq_ptr, concurrent_oldest.ftq_offset)) {
        if (!environment.error().empty()) {
            std::cerr << "MEMBLOCK_MEMORY_VIOLATION_FAIL cycle="
                      << environment.cycle()
                      << " phase=concurrent-trigger reason="
                      << environment.error() << '\n';
        }
        return 1;
    }

    std::cout << "MEMBLOCK_MEMORY_VIOLATION_PASS"
              << " cycle=" << environment.cycle()
              << " violations=3"
              << " non_overlap=1"
              << " candidates=" << younger_loads.size()
              << " rob_wrap=1"
              << " vector_candidate=1"
              << " concurrent_sources=2"
              << " rob=" << violation.rob_flag << ':'
              << static_cast<unsigned>(violation.rob_value)
              << " ftq=" << violation.ftq_flag << ':'
              << static_cast<unsigned>(violation.ftq_value)
              << " ftq_offset="
              << static_cast<unsigned>(violation.ftq_offset)
              << " is_rvc=" << violation.is_rvc
              << " level=" << violation.level
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_rar_violation(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t line = memblock::kDefaultMemoryBase + 0x1c000;
    constexpr std::uint64_t address = line + 24;
    environment.memory().fill_incrementing(line, 64, 0x63);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.configure_ldld_violation_check(true);

    const memblock::LoadTransaction warm_load{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 28,
        .lq = 0,
        .sq = 0,
        .pdest = 50,
        .lane = 2,
    };
    environment.expect_load(warm_load);
    if (!environment.enqueue_load(warm_load) ||
        !environment.issue_load(warm_load, 256) ||
        !environment.run_until_complete(4096)) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=warm-line reason=" << environment.error()
                  << '\n';
        return 1;
    }

    const memblock::StoreTransaction dirty_store{
        .address = address,
        .data = 0xa5c35a3cc3a55ac3ULL,
        .op = memblock::StoreOp::sd,
        .rob = 29,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
    };
    environment.expect_store(dirty_store);
    if (!environment.enqueue_store(dirty_store, 1) ||
        !environment.issue_store_data(dirty_store, 256) ||
        !environment.issue_store_address(dirty_store, 256) ||
        !environment.run_until_store_complete(512) ||
        !environment.commit_store(dirty_store, 4096) ||
        !environment.run_until_sbuffer_empty(4096) ||
        !environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=dirty-line reason=" << environment.error()
                  << '\n';
        return 1;
    }

    const memblock::LoadTransaction older_load{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 30,
        .lq = 1,
        .sq = 1,
        .pdest = 51,
        .lane = 0,
        .predecode_rvc = true,
        .ftq_ptr = 43,
        .ftq_offset = 5,
    };
    const memblock::LoadTransaction younger_load{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 31,
        .lq = 2,
        .sq = 1,
        .pdest = 52,
        .lane = 1,
        .ftq_ptr = 44,
        .ftq_offset = 6,
    };

    environment.expect_load(younger_load);
    if (!environment.enqueue_load(older_load) ||
        !environment.enqueue_load(younger_load) ||
        !environment.issue_load(younger_load, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.request_dcache_probe(
            line, 2, false, 1, environment.memory().read_beat(line, 64)) ||
        !environment.run_until_probe_responses(1) ||
        !environment.run_cycles(8)) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=younger-release reason=" << environment.error()
                  << '\n';
        return 1;
    }

    const auto violations_before = environment.memory_violation_stats().count;
    environment.expect_load(older_load);
    if (!environment.issue_load(older_load, 256)) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=older-issue reason=" << environment.error()
                  << '\n';
        return 1;
    }
    for (unsigned cycle = 0;
         cycle < 1024 &&
         environment.memory_violation_stats().count == violations_before;
         ++cycle) {
        if (!environment.run_cycles(1)) {
            std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                      << environment.cycle()
                      << " phase=redirect-wait reason=" << environment.error()
                      << '\n';
            return 1;
        }
    }

    const auto &stats = environment.memory_violation_stats();
    const auto &violation = stats.last;
    if (stats.count != violations_before + 1 || !violation.valid ||
        violation.is_rvc != older_load.predecode_rvc ||
        violation.rob_flag != older_load.rob_flag ||
        violation.rob_value != older_load.rob || violation.ftq_flag ||
        violation.ftq_value != older_load.ftq_ptr ||
        violation.ftq_offset != older_load.ftq_offset || violation.level) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << environment.cycle()
                  << " phase=redirect-check count=" << stats.count
                  << " expected_count=" << violations_before + 1
                  << " is_rvc=" << violation.is_rvc
                  << " rob=" << violation.rob_flag << ':'
                  << static_cast<unsigned>(violation.rob_value)
                  << " ftq=" << violation.ftq_flag << ':'
                  << static_cast<unsigned>(violation.ftq_value)
                  << " ftq_offset="
                  << static_cast<unsigned>(violation.ftq_offset)
                  << " level=" << violation.level << '\n';
        return 1;
    }

    memblock::Environment concurrent(argc, argv);
    constexpr std::uint64_t concurrent_line =
        memblock::kDefaultMemoryBase + 0x1d000;
    concurrent.memory().fill_incrementing(concurrent_line, 64, 0x79);
    if (!concurrent.reset()) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << concurrent.cycle()
                  << " phase=concurrent-reset reason=" << concurrent.error()
                  << '\n';
        return 1;
    }
    concurrent.configure_ldld_violation_check(true);

    const memblock::LoadTransaction concurrent_warm{
        .address = concurrent_line + 40,
        .op = memblock::LoadOp::ld,
        .rob = memblock::rob_pointer_value(150),
        .rob_flag = memblock::rob_pointer_flag(150),
        .lq = 0,
        .sq = 0,
        .pdest = 60,
        .lane = 0,
    };
    concurrent.expect_load(concurrent_warm);
    if (!concurrent.enqueue_load(concurrent_warm) ||
        !concurrent.issue_load(concurrent_warm, 256) ||
        !concurrent.run_until_complete(4096)) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << concurrent.cycle()
                  << " phase=concurrent-warm reason=" << concurrent.error()
                  << '\n';
        return 1;
    }

    const memblock::StoreTransaction concurrent_dirty{
        .address = concurrent_line + 48,
        .data = 0x6a5b4c3d2e1f8071ULL,
        .op = memblock::StoreOp::sd,
        .rob = memblock::rob_pointer_value(151),
        .rob_flag = memblock::rob_pointer_flag(151),
        .sq = 0,
        .address_lane = 1,
        .data_lane = 0,
    };
    concurrent.expect_store(concurrent_dirty);
    if (!concurrent.enqueue_store(concurrent_dirty, 1) ||
        !concurrent.issue_store_data(concurrent_dirty, 256) ||
        !concurrent.issue_store_address(concurrent_dirty, 256) ||
        !concurrent.run_until_store_complete(512) ||
        !concurrent.commit_store(concurrent_dirty, 4096) ||
        !concurrent.run_until_sbuffer_empty(4096) ||
        !concurrent.run_cycles(16)) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << concurrent.cycle()
                  << " phase=concurrent-dirty reason=" << concurrent.error()
                  << '\n';
        return 1;
    }

    std::vector<memblock::LoadTransaction> concurrent_older;
    std::vector<memblock::LoadTransaction> concurrent_younger;
    for (unsigned index = 0; index < memblock::kScalarLoadLanes; ++index) {
        concurrent_older.push_back(memblock::LoadTransaction{
            .address = concurrent_line + 8 + 8 * index,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(158 + index),
            .rob_flag = memblock::rob_pointer_flag(158 + index),
            .lq = static_cast<std::uint8_t>(1 + index),
            .sq = 1,
            .pdest = static_cast<std::uint8_t>(61 + index),
            .lane = index,
            .predecode_rvc = index == 0,
            .ftq_ptr = 55 + index,
            .ftq_offset = static_cast<std::uint8_t>(12 + index),
        });
        concurrent_younger.push_back(memblock::LoadTransaction{
            .address = concurrent_line + 8 + 8 * index,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(161 + index),
            .rob_flag = memblock::rob_pointer_flag(161 + index),
            .lq = static_cast<std::uint8_t>(4 + index),
            .sq = 1,
            .pdest = static_cast<std::uint8_t>(64 + index),
            .lane = index,
            .ftq_ptr = 58 + index,
            .ftq_offset = static_cast<std::uint8_t>(15 + index),
        });
    }
    for (const auto &load : concurrent_younger) {
        concurrent.expect_load(load);
    }
    if (!concurrent.enqueue_load_batch(concurrent_older, {0, 1, 2}) ||
        !concurrent.enqueue_load_batch(concurrent_younger, {0, 1, 2}) ||
        !concurrent.issue_load_batch(concurrent_younger, 256, true) ||
        !concurrent.run_until_complete(4096) ||
        !concurrent.request_dcache_probe(
            concurrent_line, 2, false, 1,
            concurrent.memory().read_beat(concurrent_line, 64)) ||
        !concurrent.run_until_probe_responses(1) ||
        !concurrent.run_cycles(8)) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << concurrent.cycle()
                  << " phase=concurrent-younger-release reason="
                  << concurrent.error() << '\n';
        return 1;
    }

    for (const auto &load : concurrent_older) {
        concurrent.expect_load(load);
    }
    const auto concurrent_before = concurrent.memory_violation_stats().count;
    if (!concurrent.issue_load_batch(concurrent_older, 256, true)) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << concurrent.cycle()
                  << " phase=concurrent-older-issue reason="
                  << concurrent.error() << '\n';
        return 1;
    }
    for (unsigned cycle = 0;
         cycle < 1024 &&
         concurrent.memory_violation_stats().count == concurrent_before;
         ++cycle) {
        if (!concurrent.run_cycles(1)) {
            std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                      << concurrent.cycle()
                      << " phase=concurrent-redirect-wait reason="
                      << concurrent.error() << '\n';
            return 1;
        }
    }
    const auto &concurrent_stats = concurrent.memory_violation_stats();
    const auto &concurrent_violation = concurrent_stats.last;
    const auto &concurrent_oldest = concurrent_older.front();
    if (concurrent_stats.count != concurrent_before + 1 ||
        !concurrent_violation.valid ||
        concurrent_violation.is_rvc != concurrent_oldest.predecode_rvc ||
        concurrent_violation.rob_flag != concurrent_oldest.rob_flag ||
        concurrent_violation.rob_value != concurrent_oldest.rob ||
        concurrent_violation.ftq_flag ||
        concurrent_violation.ftq_value != concurrent_oldest.ftq_ptr ||
        concurrent_violation.ftq_offset != concurrent_oldest.ftq_offset ||
        concurrent_violation.level) {
        std::cerr << "MEMBLOCK_RAR_VIOLATION_FAIL cycle="
                  << concurrent.cycle()
                  << " phase=concurrent-redirect-check count="
                  << concurrent_stats.count << " expected_count="
                  << concurrent_before + 1 << " is_rvc="
                  << concurrent_violation.is_rvc << " rob="
                  << concurrent_violation.rob_flag << ':'
                  << static_cast<unsigned>(concurrent_violation.rob_value)
                  << " ftq=" << concurrent_violation.ftq_flag << ':'
                  << static_cast<unsigned>(concurrent_violation.ftq_value)
                  << " ftq_offset="
                  << static_cast<unsigned>(concurrent_violation.ftq_offset)
                  << " level=" << concurrent_violation.level << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_RAR_VIOLATION_PASS"
              << " cycle=" << environment.cycle()
              << " violations=" << stats.count
              << " probes=" << environment.dcache_probes()
              << " concurrent_sources=" << concurrent_older.size()
              << " rob_wrap=1"
              << " concurrent_cycle=" << concurrent.cycle()
              << " concurrent_rob=" << concurrent_violation.rob_flag << ':'
              << static_cast<unsigned>(concurrent_violation.rob_value)
              << " rob=" << violation.rob_flag << ':'
              << static_cast<unsigned>(violation.rob_value)
              << " ftq=" << violation.ftq_flag << ':'
              << static_cast<unsigned>(violation.ftq_value)
              << " ftq_offset="
              << static_cast<unsigned>(violation.ftq_offset)
              << " is_rvc=" << violation.is_rvc
              << " level=" << violation.level
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_ifetch_prefetch(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x1d000;
    environment.memory().fill_incrementing(base, 0x1000, 0x75);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    auto total_ifetch_requests = [&]() {
        std::uint64_t total = 0;
        for (const auto count :
             environment.ifetch_prefetch_stats().requests) {
            total += count;
        }
        return total;
    };

    constexpr std::array<std::int32_t, 3> instruction_immediates{
        0, -1, -2048,
    };
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        const memblock::PrefetchTransaction transaction{
            .address = base + lane * 64 + 16,
            .immediate = instruction_immediates[lane],
            .op = memblock::PrefetchOp::instruction,
            .rob = static_cast<std::uint8_t>(lane),
            .lq = static_cast<std::uint8_t>(lane),
            .sq = 0,
            .lane = lane,
        };
        const auto requests_before =
            environment.ifetch_prefetch_stats().requests;
        const std::uint64_t tilelink_before = environment.tilelink_requests();
        environment.expect_prefetch(transaction);
        if (!environment.enqueue_prefetch(transaction) ||
            !environment.issue_prefetch(transaction, 256) ||
            !environment.run_until_complete(512) ||
            !environment.run_cycles(2)) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << environment.cycle() << " phase=instruction lane="
                      << lane << " reason=" << environment.error() << '\n';
            return 1;
        }
        const auto &stats = environment.ifetch_prefetch_stats();
        bool lanes_match = true;
        for (unsigned observed = 0;
             observed < memblock::kScalarLoadLanes; ++observed) {
            const std::uint64_t expected =
                requests_before[observed] + (observed == lane ? 1 : 0);
            lanes_match = lanes_match && stats.requests[observed] == expected;
        }
        if (!lanes_match || stats.last_vaddr[lane] != transaction.address ||
            environment.tilelink_requests() != tilelink_before) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << environment.cycle() << " phase=instruction-check lane="
                      << lane << " requests=" << stats.requests[0] << ','
                      << stats.requests[1] << ',' << stats.requests[2]
                      << " expected_vaddr=0x" << std::hex
                      << transaction.address << " actual_vaddr=0x"
                      << stats.last_vaddr[lane] << std::dec
                      << " tilelink_before=" << tilelink_before
                      << " tilelink_after=" << environment.tilelink_requests()
                      << '\n';
            return 1;
        }
    }

    constexpr std::array<memblock::PrefetchOp, 2> data_ops{
        memblock::PrefetchOp::read,
        memblock::PrefetchOp::write,
    };
    constexpr std::array<std::int32_t, 2> data_immediates{1, 2047};
    const std::uint64_t ifetch_before_data = total_ifetch_requests();
    for (unsigned index = 0; index < data_ops.size(); ++index) {
        const memblock::PrefetchTransaction transaction{
            .address = base + 0x400 + index * 64,
            .immediate = data_immediates[index],
            .op = data_ops[index],
            .rob = static_cast<std::uint8_t>(3 + index),
            .lq = static_cast<std::uint8_t>(3 + index),
            .sq = 0,
            .lane = index,
        };
        const std::uint64_t dcache_before = environment.tilelink_requests();
        environment.expect_prefetch(transaction);
        if (!environment.enqueue_prefetch(transaction) ||
            !environment.issue_prefetch(transaction, 256) ||
            !environment.run_until_complete(2048) ||
            !environment.run_cycles(2)) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << environment.cycle() << " phase=data-prefetch index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t expected_line =
            transaction.address & ~std::uint64_t{63};
        if (environment.tilelink_requests() != dcache_before + 1 ||
            environment.dcache_last_request_address() != expected_line) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << environment.cycle()
                      << " phase=data-prefetch-address index=" << index
                      << " expected_address=0x" << std::hex << expected_line
                      << " actual_address=0x"
                      << environment.dcache_last_request_address() << std::dec
                      << " requests_before=" << dcache_before
                      << " requests_after=" << environment.tilelink_requests()
                      << '\n';
            return 1;
        }
    }
    if (total_ifetch_requests() != ifetch_before_data) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << environment.cycle()
                  << " phase=data-prefetch-check expected_ifetch="
                  << ifetch_before_data
                  << " actual_ifetch=" << total_ifetch_requests() << '\n';
        return 1;
    }

    memblock::Environment concurrent(argc, argv);
    constexpr std::uint64_t unmapped_base = 0x63000000ULL;
    constexpr std::uint64_t empty_root = 0xe7000000ULL;
    if (!concurrent.reset() || !concurrent.activate_sv39(empty_root)) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << concurrent.cycle()
                  << " phase=concurrent-configuration reason="
                  << concurrent.error() << '\n';
        return 1;
    }
    std::vector<memblock::PrefetchTransaction> concurrent_prefetches;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        concurrent_prefetches.push_back(memblock::PrefetchTransaction{
            .address = unmapped_base + lane * 0x1000 + 16,
            .op = memblock::PrefetchOp::instruction,
            .rob = static_cast<std::uint8_t>(16 + lane),
            .lq = static_cast<std::uint8_t>(lane),
            .sq = 0,
            .lane = lane,
        });
        concurrent.expect_prefetch(concurrent_prefetches.back());
    }
    const std::uint64_t concurrent_ptw_before = concurrent.ptw_requests();
    const std::uint64_t concurrent_dcache_before =
        concurrent.tilelink_requests();
    if (!concurrent.enqueue_prefetch_batch(
            concurrent_prefetches, {0, 1, 2}) ||
        !concurrent.issue_prefetch_batch_same_cycle(
            concurrent_prefetches, 256) ||
        !concurrent.run_until_complete(1024) ||
        !concurrent.run_until_lq_retired(1024) ||
        !concurrent.run_cycles(2)) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << concurrent.cycle() << " phase=concurrent-complete reason="
                  << concurrent.error() << '\n';
        return 1;
    }
    const auto &concurrent_stats = concurrent.ifetch_prefetch_stats();
    bool concurrent_match = true;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        concurrent_match = concurrent_match &&
            concurrent_stats.requests[lane] == 1 &&
            concurrent_stats.last_vaddr[lane] ==
                concurrent_prefetches[lane].address;
    }
    if (!concurrent_match ||
        concurrent.ptw_requests() != concurrent_ptw_before ||
        concurrent.tilelink_requests() != concurrent_dcache_before) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << concurrent.cycle()
                  << " phase=concurrent-translation-bypass requests="
                  << concurrent_stats.requests[0] << ','
                  << concurrent_stats.requests[1] << ','
                  << concurrent_stats.requests[2] << " ptw_before="
                  << concurrent_ptw_before << " ptw_after="
                  << concurrent.ptw_requests() << " dcache_before="
                  << concurrent_dcache_before << " dcache_after="
                  << concurrent.tilelink_requests() << '\n';
        return 1;
    }

    std::uint64_t mixed_cycles = 0;
    std::uint64_t mixed_writebacks = 0;
    std::uint64_t unmapped_data_ptw_requests = 0;
    std::uint64_t mapped_data_ptw_requests = 0;
    std::uint64_t mapped_data_dcache_requests = 0;
    std::uint64_t mapped_warmup_ptw_requests = 0;
    auto run_mixed_prefetches = [&](bool mapped) {
        memblock::Environment mixed(argc, argv);
        constexpr std::uint64_t mixed_virtual = 0x64000000ULL;
        constexpr std::uint64_t read_page =
            mixed_virtual + 0x40000000ULL;
        constexpr std::uint64_t write_page =
            mixed_virtual + 0x80000000ULL;
        constexpr std::uint64_t mapped_physical = 0xb4000000ULL;
        const std::uint64_t mixed_root = mapped
            ? 0xe7100000ULL
            : 0xe7200000ULL;
        const std::uint64_t instruction_address = mixed_virtual + 0x10;
        const std::uint64_t read_address = read_page + 0x100;
        const std::uint64_t write_address = write_page + 0x120;
        mixed.memory().fill_incrementing(mapped_physical, 0x2000, 0x96);
        if (!mixed.reset() ||
            (mapped &&
             (!mixed.map_sv39_4k(
                  read_page, mapped_physical, mixed_root) ||
              !mixed.map_sv39_4k(
                  write_page,
                  mapped_physical + 0x1000,
                  mixed_root))) ||
            !mixed.activate_sv39(mixed_root, mapped ? 28 : 27)) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << mixed.cycle() << " phase=mixed-"
                      << (mapped ? "mapped" : "unmapped")
                      << "-configuration reason=" << mixed.error() << '\n';
            return false;
        }

        if (mapped) {
            const std::array<memblock::LoadTransaction, 2> warmups{{
                memblock::LoadTransaction{
                    .address = read_page + 0x800,
                    .oracle_address = mapped_physical + 0x800,
                    .op = memblock::LoadOp::ld,
                    .rob = 28,
                    .lq = 0,
                    .pdest = 54,
                    .lane = 1,
                },
                memblock::LoadTransaction{
                    .address = write_page + 0x800,
                    .oracle_address = mapped_physical + 0x1800,
                    .op = memblock::LoadOp::ld,
                    .rob = 29,
                    .lq = 1,
                    .pdest = 55,
                    .lane = 2,
                },
            }};
            for (const auto &warmup : warmups) {
                mixed.expect_load(warmup);
                if (!mixed.set_rob_head(warmup.rob, warmup.rob_flag) ||
                    !mixed.enqueue_load(warmup) ||
                    !mixed.issue_load(warmup, 512) ||
                    !mixed.run_until_complete(8192) ||
                    !mixed.run_until_lq_retired(2048)) {
                    std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                              << mixed.cycle()
                              << " phase=mixed-mapped-tlb-warmup reason="
                              << mixed.error() << '\n';
                    return false;
                }
            }
            mapped_warmup_ptw_requests = mixed.ptw_requests();
        }

        const std::vector<memblock::PrefetchTransaction> transactions{
            memblock::PrefetchTransaction{
                .address = instruction_address,
                .op = memblock::PrefetchOp::instruction,
                .rob = 32,
                .lq = static_cast<std::uint8_t>(mapped ? 2 : 0),
                .lane = 0,
            },
            memblock::PrefetchTransaction{
                .address = read_address,
                .oracle_address = mapped
                    ? std::optional<std::uint64_t>{mapped_physical + 0x100}
                    : std::nullopt,
                .op = memblock::PrefetchOp::read,
                .rob = 33,
                .lq = static_cast<std::uint8_t>(mapped ? 3 : 1),
                .lane = 1,
            },
            memblock::PrefetchTransaction{
                .address = write_address,
                .oracle_address = mapped
                    ? std::optional<std::uint64_t>{
                          mapped_physical + 0x1120}
                    : std::nullopt,
                .op = memblock::PrefetchOp::write,
                .rob = 34,
                .lq = static_cast<std::uint8_t>(mapped ? 4 : 2),
                .lane = 2,
            },
        };
        const auto ifetch_before = mixed.ifetch_prefetch_stats().requests;
        const std::uint64_t ptw_before = mixed.ptw_requests();
        const std::uint64_t dcache_before = mixed.tilelink_requests();
        const std::uint64_t uncache_before = mixed.uncache_requests();
        for (const auto &transaction : transactions) {
            mixed.expect_prefetch(transaction);
        }
        if (!mixed.set_rob_head(
                transactions.front().rob, transactions.front().rob_flag) ||
            !mixed.enqueue_prefetch_batch(transactions, {0, 1, 2}) ||
            !mixed.issue_prefetch_batch_same_cycle(transactions, 512) ||
            !mixed.run_until_complete(8192) ||
            !mixed.run_until_lq_retired(2048) ||
            !mixed.run_cycles(2)) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << mixed.cycle() << " phase=mixed-"
                      << (mapped ? "mapped" : "unmapped")
                      << "-complete reason=" << mixed.error() << '\n';
            return false;
        }

        const auto &mixed_stats = mixed.ifetch_prefetch_stats();
        bool ifetch_match =
            mixed_stats.last_vaddr[0] == instruction_address;
        for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
            ifetch_match = ifetch_match &&
                mixed_stats.requests[lane] ==
                    ifetch_before[lane] + (lane == 0 ? 1 : 0);
        }
        const std::uint64_t ptw_delta = mixed.ptw_requests() - ptw_before;
        const std::uint64_t dcache_delta =
            mixed.tilelink_requests() - dcache_before;
        const std::uint64_t uncache_delta =
            mixed.uncache_requests() - uncache_before;
        const bool manager_match = mapped
            ? ptw_delta == 0 && dcache_delta >= 1 && uncache_delta == 0
            : ptw_delta == 0 && dcache_delta == 0 && uncache_delta == 0;
        if (!ifetch_match || !manager_match ||
            mixed.prefetch_writebacks() != transactions.size()) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << mixed.cycle() << " phase=mixed-"
                      << (mapped ? "mapped" : "unmapped")
                      << "-check ifetch=" << mixed_stats.requests[0] << ','
                      << mixed_stats.requests[1] << ','
                      << mixed_stats.requests[2]
                      << " ptw_delta=" << ptw_delta
                      << " dcache_delta=" << dcache_delta
                      << " uncache_delta=" << uncache_delta
                      << " writebacks=" << mixed.prefetch_writebacks()
                      << '\n';
            return false;
        }
        if (mapped) {
            const std::array<memblock::PrefetchTransaction, 2>
                individual_data_prefetches{{
                    memblock::PrefetchTransaction{
                        .address = read_page + 0x200,
                        .oracle_address = mapped_physical + 0x200,
                        .op = memblock::PrefetchOp::read,
                        .rob = 35,
                        .lq = 5,
                        .lane = 1,
                    },
                    memblock::PrefetchTransaction{
                        .address = write_page + 0x300,
                        .oracle_address = mapped_physical + 0x1300,
                        .op = memblock::PrefetchOp::write,
                        .rob = 36,
                        .lq = 6,
                        .lane = 2,
                    },
                }};
            for (const auto &prefetch : individual_data_prefetches) {
                const std::uint64_t request_before =
                    mixed.tilelink_requests();
                mixed.expect_prefetch(prefetch);
                if (!mixed.set_rob_head(prefetch.rob, prefetch.rob_flag) ||
                    !mixed.enqueue_prefetch(prefetch) ||
                    !mixed.issue_prefetch(prefetch, 512) ||
                    !mixed.run_until_complete(8192) ||
                    !mixed.run_until_lq_retired(2048) ||
                    mixed.tilelink_requests() != request_before + 1 ||
                    mixed.ptw_requests() != ptw_before ||
                    mixed.uncache_requests() != uncache_before) {
                    std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                              << mixed.cycle()
                              << " phase=mixed-mapped-individual-data"
                              << " op=" << static_cast<unsigned>(prefetch.op)
                              << " dcache_before=" << request_before
                              << " dcache_after="
                              << mixed.tilelink_requests()
                              << " reason=" << mixed.error() << '\n';
                    return false;
                }
            }
        }
        mixed_cycles += mixed.cycle();
        mixed_writebacks += mixed.prefetch_writebacks();
        if (mapped) {
            mapped_data_ptw_requests = mixed.ptw_requests() - ptw_before;
            mapped_data_dcache_requests =
                mixed.tilelink_requests() - dcache_before;
        } else {
            unmapped_data_ptw_requests = ptw_delta;
        }
        return true;
    };
    if (!run_mixed_prefetches(false) || !run_mixed_prefetches(true)) {
        return 1;
    }

    memblock::Environment memory_type(argc, argv);
    constexpr std::uint64_t memory_type_virtual = 0x74000000ULL;
    constexpr std::uint64_t memory_type_physical = 0xb5000000ULL;
    constexpr std::uint64_t memory_type_root = 0xe7300000ULL;
    memory_type.memory().fill_incrementing(
        memory_type_physical, 0x4000, 0xa7);
    if (!memory_type.reset()) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << memory_type.cycle()
                  << " phase=memory-type-reset reason="
                  << memory_type.error() << '\n';
        return 1;
    }
    for (unsigned index = 0; index < 4; ++index) {
        const bool noncacheable = index < 2;
        if (!memory_type.map_sv39_4k(
                memory_type_virtual + index * 0x1000,
                memory_type_physical + index * 0x1000,
                memory_type_root, true, true, false, false,
                noncacheable, !noncacheable)) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << memory_type.cycle()
                      << " phase=memory-type-map index=" << index
                      << " reason=" << memory_type.error() << '\n';
            return 1;
        }
    }
    if (!memory_type.activate_sv39(memory_type_root, 29)) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << memory_type.cycle()
                  << " phase=memory-type-activate reason="
                  << memory_type.error() << '\n';
        return 1;
    }

    for (unsigned index = 0; index < 4; ++index) {
        const memblock::LoadTransaction warmup{
            .address = memory_type_virtual + index * 0x1000 + 0x80,
            .oracle_address = memory_type_physical + index * 0x1000 + 0x80,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(64 + index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(80 + index),
            .lane = index % memblock::kScalarLoadLanes,
        };
        memory_type.expect_load(warmup);
        if (!memory_type.set_rob_head(warmup.rob, warmup.rob_flag) ||
            !memory_type.enqueue_load(warmup) ||
            !memory_type.issue_load(warmup, 512) ||
            (index >= 2 && !memory_type.wait_for_mmio_request(
                               warmup.rob, warmup.rob_flag, 4096)) ||
            !memory_type.run_until_complete(8192) ||
            !memory_type.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << memory_type.cycle()
                      << " phase=memory-type-warmup index=" << index
                      << " reason=" << memory_type.error() << '\n';
            return 1;
        }
    }
    const std::uint64_t memory_type_ptw_before = memory_type.ptw_requests();
    const std::uint64_t memory_type_dcache_before =
        memory_type.tilelink_requests();
    const std::uint64_t memory_type_uncache_before =
        memory_type.uncache_requests();
    const auto memory_type_ifetch_before =
        memory_type.ifetch_prefetch_stats().requests;
    if (memory_type_dcache_before != 0 || memory_type_uncache_before != 4) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << memory_type.cycle()
                  << " phase=memory-type-warmup-check dcache="
                  << memory_type_dcache_before << " uncache="
                  << memory_type_uncache_before << '\n';
        return 1;
    }

    const std::array<memblock::PrefetchTransaction, 4> memory_type_prefetches{{
        {
            .address = memory_type_virtual + 0x100,
            .oracle_address = memory_type_physical + 0x100,
            .op = memblock::PrefetchOp::read,
            .rob = 68,
            .lq = 4,
            .lane = 0,
        },
        {
            .address = memory_type_virtual + 0x1000 + 0x140,
            .oracle_address = memory_type_physical + 0x1000 + 0x140,
            .op = memblock::PrefetchOp::write,
            .rob = 69,
            .lq = 5,
            .lane = 1,
        },
        {
            .address = memory_type_virtual + 0x2000 + 0x180,
            .oracle_address = memory_type_physical + 0x2000 + 0x180,
            .op = memblock::PrefetchOp::read,
            .rob = 70,
            .lq = 6,
            .lane = 1,
        },
        {
            .address = memory_type_virtual + 0x3000 + 0x1c0,
            .oracle_address = memory_type_physical + 0x3000 + 0x1c0,
            .op = memblock::PrefetchOp::write,
            .rob = 71,
            .lq = 7,
            .lane = 2,
        },
    }};
    for (unsigned index = 0; index < memory_type_prefetches.size(); ++index) {
        const auto &prefetch = memory_type_prefetches[index];
        const std::uint64_t dcache_before = memory_type.tilelink_requests();
        const std::uint64_t uncache_before = memory_type.uncache_requests();
        memory_type.expect_prefetch(prefetch);
        if (!memory_type.set_rob_head(prefetch.rob, prefetch.rob_flag) ||
            !memory_type.enqueue_prefetch(prefetch) ||
            !memory_type.issue_prefetch(prefetch, 512) ||
            !memory_type.run_until_complete(8192) ||
            !memory_type.run_until_lq_retired(2048) ||
            memory_type.ptw_requests() != memory_type_ptw_before ||
            memory_type.uncache_requests() != uncache_before ||
            memory_type.tilelink_requests() !=
                dcache_before + (index < 2 ? 1 : 0)) {
            std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                      << memory_type.cycle()
                      << " phase=memory-type-prefetch index=" << index
                      << " dcache_before=" << dcache_before
                      << " dcache_after=" << memory_type.tilelink_requests()
                      << " uncache_before=" << uncache_before
                      << " uncache_after=" << memory_type.uncache_requests()
                      << " reason=" << memory_type.error() << '\n';
            return 1;
        }
    }
    if (memory_type.prefetch_writebacks() != memory_type_prefetches.size() ||
        memory_type.tilelink_requests() - memory_type_dcache_before != 2 ||
        memory_type.uncache_requests() != memory_type_uncache_before ||
        memory_type.ifetch_prefetch_stats().requests !=
            memory_type_ifetch_before) {
        std::cerr << "MEMBLOCK_IFETCH_PREFETCH_FAIL cycle="
                  << memory_type.cycle()
                  << " phase=memory-type-check prefetch_writebacks="
                  << memory_type.prefetch_writebacks()
                  << " dcache_delta="
                  << memory_type.tilelink_requests() -
                         memory_type_dcache_before
                  << " uncache_delta="
                  << memory_type.uncache_requests() -
                         memory_type_uncache_before << '\n';
        return 1;
    }

    const auto &stats = environment.ifetch_prefetch_stats();
    std::cout << "MEMBLOCK_IFETCH_PREFETCH_PASS"
              << " cycle="
              << environment.cycle() + concurrent.cycle() + mixed_cycles +
                    memory_type.cycle()
              << " lane_requests=" << stats.requests[0] << ','
              << stats.requests[1] << ',' << stats.requests[2]
              << " instruction=3 data=2"
              << " concurrent_lanes=3 translation_bypass=3"
              << " mixed_unmapped=3 mixed_mapped=3 mapped_individual=2"
              << " memory_type_prefetches=4"
              << " prefetch_writebacks="
              << environment.prefetch_writebacks() +
                    concurrent.prefetch_writebacks() + mixed_writebacks +
                    memory_type.prefetch_writebacks()
              << " unmapped_data_ptw=" << unmapped_data_ptw_requests
              << " mapped_warmup_ptw=" << mapped_warmup_ptw_requests
              << " mapped_data_ptw=" << mapped_data_ptw_requests
              << " mapped_data_dcache=" << mapped_data_dcache_requests
              << " nc_prefetch_dcache=2 nc_prefetch_uncache=0"
              << " io_prefetch_dcache=0 io_prefetch_uncache=0"
              << " immediate_cases=5 immediate_min=-2048 immediate_max=2047"
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_hardware_prefetch(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x200000;
    constexpr std::uint64_t pc = 0x4000;
    constexpr std::uint64_t stride = 128;
    constexpr std::uint8_t stride_source = 12;
    constexpr unsigned training_loads = 8;
    environment.memory().fill_incrementing(base, 0x10000, 0x39);
    if (!environment.reset() ||
        !environment.configure_stride_prefetch(true) ||
        !environment.expect_l2_prefetch_control(true, true)) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << environment.cycle() << " phase=setup reason="
                  << environment.error() << '\n';
        return 1;
    }

    for (unsigned index = 0; index < training_loads; ++index) {
        const memblock::LoadTransaction transaction{
            .address = base + index * stride,
            .pc = pc,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(index + 1),
            .lane = index % memblock::kScalarLoadLanes,
        };
        environment.expect_load(transaction);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 256) ||
            !environment.run_until_complete(2048) ||
            !environment.run_cycles(64)) {
            std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                      << environment.cycle() << " phase=train index=" << index
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
    }

    if (!environment.run_until_lq_retired(4096) ||
        !environment.configure_stride_prefetch(false) ||
        !environment.expect_l2_prefetch_control(false, false)) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << environment.cycle() << " phase=disable reason="
                  << environment.error() << '\n';
        return 1;
    }
    const auto stride_before_disable = environment.hardware_prefetch_stats()
                                           .l2_source_counts[stride_source];
    for (unsigned index = training_loads; index < training_loads + 3; ++index) {
        const memblock::LoadTransaction transaction{
            .address = base + index * stride,
            .pc = pc,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(index + 1),
            .lane = index % memblock::kScalarLoadLanes,
        };
        environment.expect_load(transaction);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 256) ||
            !environment.run_until_complete(2048) ||
            !environment.run_cycles(32)) {
            std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                      << environment.cycle() << " phase=disabled-load index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    const auto &stats = environment.hardware_prefetch_stats();
    if (stats.l2_source_counts[stride_source] != stride_before_disable) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << environment.cycle() << " phase=disabled-check"
                  << " expected_stride=" << stride_before_disable
                  << " actual_stride="
                  << stats.l2_source_counts[stride_source]
                  << " l3_total=" << stats.l3_requests << '\n';
        return 1;
    }

    memblock::Environment stream_environment(argc, argv);
    constexpr std::uint64_t stream_base =
        memblock::kDefaultMemoryBase + 0x300000;
    constexpr std::uint8_t stream_source = 11;
    constexpr unsigned stream_training_loads = 12;
    stream_environment.memory().fill_incrementing(
        stream_base, 0x20000, 0x5d);
    if (!stream_environment.reset() ||
        !stream_environment.configure_stride_prefetch(true) ||
        !stream_environment.expect_l2_prefetch_control(true, true)) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << stream_environment.cycle()
                  << " phase=stream-setup reason="
                  << stream_environment.error() << '\n';
        return 1;
    }
    for (unsigned index = 0; index < stream_training_loads; ++index) {
        const memblock::LoadTransaction transaction{
            .address = stream_base + index * 64,
            .pc = 0x8000 + index * 0x40,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(index + 1),
            .lane = index % memblock::kScalarLoadLanes,
        };
        stream_environment.expect_load(transaction);
        if (!stream_environment.enqueue_load(transaction) ||
            !stream_environment.issue_load(transaction, 256) ||
            !stream_environment.run_until_complete(2048) ||
            !stream_environment.run_until_lq_retired(1024) ||
            !stream_environment.run_cycles(32)) {
            std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                      << stream_environment.cycle()
                      << " phase=stream-train index=" << index
                      << " reason=" << stream_environment.error() << '\n';
            return 1;
        }
    }
    if (!stream_environment.run_cycles(128)) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << stream_environment.cycle()
                  << " phase=stream-drain reason="
                  << stream_environment.error() << '\n';
        return 1;
    }
    const auto &stream_stats =
        stream_environment.hardware_prefetch_stats();
    const std::uint64_t isolated_stream_requests =
        stream_stats.l2_source_counts[stream_source];
    const std::uint64_t stream_before_priority =
        stream_stats.l2_source_counts[stream_source];
    constexpr unsigned priority_training_loads = 6;
    for (unsigned index = 0; index < priority_training_loads; ++index) {
        const unsigned transaction_index = stream_training_loads + index;
        const memblock::LoadTransaction transaction{
            .address = stream_base + 1024 + index * 64,
            .pc = 0x10000,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(transaction_index),
            .lq = static_cast<std::uint8_t>(transaction_index),
            .pdest = static_cast<std::uint8_t>(transaction_index + 1),
            .lane = transaction_index % memblock::kScalarLoadLanes,
        };
        stream_environment.expect_load(transaction);
        if (!stream_environment.enqueue_load(transaction) ||
            !stream_environment.issue_load(transaction, 256) ||
            !stream_environment.run_until_complete(2048) ||
            !stream_environment.run_until_lq_retired(1024) ||
            !stream_environment.run_cycles(32)) {
            std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                      << stream_environment.cycle()
                      << " phase=stream-priority index=" << index
                      << " reason=" << stream_environment.error() << '\n';
            return 1;
        }
    }
    if (!stream_environment.run_cycles(128)) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << stream_environment.cycle()
                  << " phase=stream-priority-drain reason="
                  << stream_environment.error() << '\n';
        return 1;
    }
    const std::uint64_t stream_priority_requests =
        stream_stats.l2_source_counts[stream_source] - stream_before_priority;

    memblock::Environment sms_environment(argc, argv);
    constexpr std::uint64_t sms_base =
        memblock::kDefaultMemoryBase + 0x500000;
    constexpr std::uint64_t sms_trigger_base = sms_base + 0x1000;
    constexpr std::uint64_t sms_pc = 0x18000;
    constexpr std::uint8_t sms_source = 10;
    constexpr std::array<unsigned, 12> sms_training_offsets{
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
    sms_environment.memory().fill_incrementing(sms_base, 0x8000, 0x6d);
    if (!sms_environment.reset() ||
        !sms_environment.configure_sms_pht_prefetch(false) ||
        !sms_environment.expect_l2_prefetch_control(true, true)) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << sms_environment.cycle()
                  << " phase=sms-setup reason="
                  << sms_environment.error() << '\n';
        return 1;
    }
    for (unsigned index = 0; index < sms_training_offsets.size(); ++index) {
        const memblock::LoadTransaction transaction{
            .address = sms_base + sms_training_offsets[index] * 64,
            .pc = sms_pc,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(index + 1),
            .lane = index % memblock::kScalarLoadLanes,
        };
        sms_environment.expect_load(transaction);
        if (!sms_environment.enqueue_load(transaction) ||
            !sms_environment.issue_load(transaction, 256) ||
            !sms_environment.run_until_complete(2048) ||
            !sms_environment.run_until_lq_retired(1024) ||
            !sms_environment.run_cycles(16)) {
            std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                      << sms_environment.cycle()
                      << " phase=sms-train index=" << index
                      << " reason=" << sms_environment.error() << '\n';
            return 1;
        }
    }
    if (sms_environment.hardware_prefetch_stats()
            .l2_source_counts[sms_source] != 0 ||
        !sms_environment.configure_sms_pht_prefetch(true)) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << sms_environment.cycle()
                  << " phase=sms-enable source10_before="
                  << sms_environment.hardware_prefetch_stats()
                         .l2_source_counts[sms_source]
                  << " reason=" << sms_environment.error() << '\n';
        return 1;
    }
    const auto sms_before = sms_environment.hardware_prefetch_stats()
                                .l2_source_counts[sms_source];
    const memblock::LoadTransaction sms_trigger{
        .address = sms_trigger_base,
        .pc = sms_pc,
        .op = memblock::LoadOp::ld,
        .rob = static_cast<std::uint8_t>(sms_training_offsets.size()),
        .lq = static_cast<std::uint8_t>(sms_training_offsets.size()),
        .pdest = static_cast<std::uint8_t>(sms_training_offsets.size() + 1),
        .lane = sms_training_offsets.size() % memblock::kScalarLoadLanes,
    };
    sms_environment.expect_load(sms_trigger);
    if (!sms_environment.enqueue_load(sms_trigger) ||
        !sms_environment.issue_load(sms_trigger, 256) ||
        !sms_environment.run_until_complete(2048) ||
        !sms_environment.run_until_lq_retired(1024) ||
        !sms_environment.run_cycles(256)) {
        std::cerr << "MEMBLOCK_HARDWARE_PREFETCH_FAIL cycle="
                  << sms_environment.cycle()
                  << " phase=sms-trigger reason="
                  << sms_environment.error() << '\n';
        return 1;
    }
    const auto &sms_stats = sms_environment.hardware_prefetch_stats();
    const auto &sms_addresses =
        sms_stats.l2_addresses_by_source[sms_source];
    const auto sms_after = sms_stats.l2_source_counts[sms_source];
    std::uint16_t sms_offset_bitmap = 0;
    for (std::size_t index = sms_before;
         index < sms_addresses.size(); ++index) {
        const std::uint64_t address = sms_addresses[index];
        const bool offset_is_representable =
            address >= sms_trigger_base &&
            (address - sms_trigger_base) % 64 == 0 &&
            (address - sms_trigger_base) / 64 < 16;
        const unsigned offset = offset_is_representable
            ? static_cast<unsigned>((address - sms_trigger_base) / 64)
            : 0;
        const std::uint16_t offset_bit = offset_is_representable
            ? static_cast<std::uint16_t>(std::uint16_t{1} << offset)
            : 0;
        sms_offset_bitmap |= offset_bit;
    }
    const std::uint64_t sms_pht_requests = sms_after - sms_before;

    std::cout << "MEMBLOCK_HARDWARE_PREFETCH_PASS"
              << " cycle="
              << environment.cycle() + stream_environment.cycle() +
                    sms_environment.cycle()
              << " stride_cycles=" << environment.cycle()
              << " stream_cycles=" << stream_environment.cycle()
              << " training_loads=" << training_loads
              << " disabled_loads=3"
              << " l2_total=" << stats.l2_requests
              << " l2_stride=" << stats.l2_source_counts[stride_source]
              << " l2_stream=" << stats.l2_source_counts[11]
              << " l2_sms=" << stats.l2_source_counts[10]
              << " l3_total=" << stats.l3_requests
              << " stream_loads=" << stream_training_loads
              << " stream_l2=" << isolated_stream_requests
              << " stream_last=0x" << std::hex
              << stream_stats.last_l2_addr_by_source[stream_source] << std::dec
              << " stream_priority_loads=" << priority_training_loads
              << " stream_priority_l2=" << stream_priority_requests
              << " sms_training_loads=" << sms_training_offsets.size()
              << " sms_pht_l2=" << sms_pht_requests
              << " sms_last=0x" << std::hex
              << sms_stats.last_l2_addr_by_source[sms_source]
              << " sms_bitmap=0x" << sms_offset_bitmap << std::dec
              << " implementation_characterization=1 disable_controls=1"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_fp_loads(int argc, char **argv)
{
    auto expected_fp_data = [](
                                memblock::SparseMemory &memory,
                                std::uint64_t address,
                                memblock::LoadOp op) {
        const std::uint64_t raw = memory.expected_load(address, op);
        return op == memblock::LoadOp::lh
            ? std::uint64_t{0xffffffffffff0000ULL} | (raw & 0xffffULL)
            : op == memblock::LoadOp::lw
                ? std::uint64_t{0xffffffff00000000ULL} |
                    (raw & 0xffffffffULL)
                : raw;
    };
    auto run_fp_transaction = [](
                                  memblock::Environment &target,
                                  const memblock::LoadTransaction &transaction,
                                  std::uint64_t expected_data,
                                  bool forbid_data_request) {
        const std::uint64_t dcache_before = target.tilelink_requests();
        const std::uint64_t uncache_before = target.uncache_requests();
        target.expect_load_data(transaction, expected_data);
        if (!target.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !target.enqueue_load(transaction) ||
            !target.issue_load(transaction, 4096) ||
            !target.run_until_complete(32768) ||
            !target.run_until_lq_retired(8192)) {
            return false;
        }
        return !forbid_data_request ||
            (target.tilelink_requests() == dcache_before &&
             target.uncache_requests() == uncache_before);
    };

    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x18000;
    environment.memory().fill_incrementing(base, 64, 0x5b);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << environment.cycle()
                  << " phase=reset reason=" << environment.error() << '\n';
        return 1;
    }

    // The MemBlock boundary carries destination-class enables separately from
    // the width opcode.  Exercise both narrow and wide FP writebacks and
    // explicitly require that the integer register file remains untouched.
    const std::array<memblock::LoadTransaction, 3> transactions{{
        {
            .address = base + 6,
            .op = memblock::LoadOp::lh,
            .rob = 11,
            .lq = 0,
            .pdest = 15,
            .lane = 2,
            .rf_wen = false,
            .fp_wen = true,
        },
        {
            .address = base + 12,
            .op = memblock::LoadOp::lw,
            .rob = 12,
            .lq = 1,
            .pdest = 17,
            .lane = 0,
            .rf_wen = false,
            .fp_wen = true,
        },
        {
            .address = base + 24,
            .op = memblock::LoadOp::ld,
            .rob = 13,
            .lq = 2,
            .pdest = 19,
            .lane = 1,
            .rf_wen = false,
            .fp_wen = true,
        },
    }};
    for (const auto &transaction : transactions) {
        const auto expected = expected_fp_data(
            environment.memory(), transaction.address, transaction.op);
        environment.expect_load_data(transaction, expected);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 512) ||
            !environment.run_until_complete(2048)) {
            std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << environment.cycle()
                      << " phase=completion reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (environment.writebacks() != transactions.size()) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << environment.cycle()
                  << " phase=writeback-count expected=" << transactions.size()
                  << " actual=" << environment.writebacks() << '\n';
        return 1;
    }

    memblock::Environment mmio(argc, argv);
    constexpr std::uint64_t mmio_virtual = 0x5003a000ULL;
    constexpr std::uint64_t mmio_physical = 0x9003a000ULL;
    constexpr std::uint64_t mmio_root = 0x97010000ULL;
    mmio.memory().fill_incrementing(mmio_physical, 64, 0x8d);
    mmio.configure_backpressure(0x243f6a8885a308d3ULL, true);
    if (!mmio.reset() ||
        !mmio.map_sv39_4k(
            mmio_virtual, mmio_physical, mmio_root, true, true, false,
            false, false, true) ||
        !mmio.activate_sv39(mmio_root, 37)) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << mmio.cycle()
                  << " phase=mmio-configuration reason=" << mmio.error()
                  << '\n';
        return 1;
    }

    unsigned mmio_faults = 0;
    auto run_mmio_fp = [&](std::uint8_t rob, std::uint8_t lq,
                           memblock::LoadOp op, std::uint64_t offset,
                           std::uint32_t exception, bool denied, bool corrupt) {
        const memblock::LoadTransaction transaction{
            .address = mmio_virtual + offset,
            .oracle_address = mmio_physical + offset,
            .op = op,
            .rob = rob,
            .lq = lq,
            .pdest = static_cast<std::uint8_t>(21 + rob),
            .lane = static_cast<unsigned>(rob % memblock::kScalarLoadLanes),
            .expected_exception_mask = exception,
            .rf_wen = false,
            .fp_wen = true,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        const std::uint64_t expected = expected_fp_data(
            mmio.memory(), mmio_physical + offset, op);
        mmio.expect_load_data(transaction, expected);
        if (denied || corrupt) {
            mmio.inject_next_uncache_response_error(denied, corrupt);
        }
        const std::uint64_t dcache_before = mmio.tilelink_requests();
        const std::uint64_t uncache_before = mmio.uncache_requests();
        if (!mmio.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !mmio.enqueue_load(transaction) ||
            !mmio.issue_load(transaction, 2048) ||
            !mmio.wait_for_mmio_request(
                transaction.rob, transaction.rob_flag, 4096) ||
            !mmio.run_until_complete(8192) ||
            mmio.tilelink_requests() != dcache_before ||
            mmio.uncache_requests() != uncache_before + 1) {
            return false;
        }
        if (exception == 0) {
            return mmio.run_until_lq_retired(2048);
        }
        ++mmio_faults;
        if (!mmio.run_cycles(8) ||
            !mmio.redirect_after(
                transaction.rob, transaction.rob_flag, true) ||
            !mmio.run_cycles(96)) {
            return false;
        }
        if (mmio.lq_dequeued() + mmio.lq_canceled() < mmio.lq_allocated()) {
            return mmio.account_lq_cancellation(1);
        }
        return true;
    };

    if (!run_mmio_fp(0, 0, memblock::LoadOp::lh, 2, 0, false, false) ||
        !run_mmio_fp(1, 1, memblock::LoadOp::lw, 4, 0, false, false) ||
        !run_mmio_fp(2, 2, memblock::LoadOp::ld, 16, 0, false, false) ||
        !run_mmio_fp(
            3, 3, memblock::LoadOp::lw, 28,
            memblock::kExceptionLoadAccessFault, true, false) ||
        !run_mmio_fp(
            4, 4, memblock::LoadOp::ld, 40,
            memblock::kExceptionHardwareError, false, true)) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << mmio.cycle()
                  << " phase=mmio-completion reason=" << mmio.error()
                  << " dcache_requests=" << mmio.tilelink_requests()
                  << " uncache_requests=" << mmio.uncache_requests() << '\n';
        return 1;
    }

    memblock::Environment page_fault(argc, argv);
    constexpr std::uint64_t fault_address = 0x5003c000ULL;
    constexpr std::uint64_t fault_root = 0x97014000ULL;
    const memblock::LoadTransaction faulting_flw{
        .address = fault_address,
        .op = memblock::LoadOp::lw,
        .rob = 5,
        .lq = 0,
        .pdest = 25,
        .lane = 1,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
        .rf_wen = false,
        .fp_wen = true,
    };
    if (!page_fault.reset() || !page_fault.activate_sv39(fault_root, 41)) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << page_fault.cycle()
                  << " phase=page-fault-configuration reason="
                  << page_fault.error() << '\n';
        return 1;
    }
    page_fault.expect_load_data(faulting_flw, 0);
    if (!page_fault.set_rob_head(
            faulting_flw.rob, faulting_flw.rob_flag) ||
        !page_fault.enqueue_load(faulting_flw) ||
        !page_fault.issue_load(faulting_flw, 2048) ||
        !page_fault.run_until_complete(8192) ||
        !page_fault.run_until_lq_retired(2048) ||
        page_fault.tilelink_requests() != 0 ||
        page_fault.uncache_requests() != 0) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << page_fault.cycle()
                  << " phase=page-fault-completion reason="
                  << page_fault.error()
                  << " dcache_requests=" << page_fault.tilelink_requests()
                  << " uncache_requests=" << page_fault.uncache_requests()
                  << '\n';
        return 1;
    }

    memblock::Environment pmp_fault(argc, argv);
    constexpr std::uint64_t pmp_base = 0x80600000ULL;
    constexpr std::uint64_t pmp_size = 0x1000ULL;
    constexpr std::uint8_t pmp_napot_deny = 0x18;
    const std::uint64_t pmp_napot_address =
        (pmp_base | (pmp_size / 2 - 1)) >> 2;
    const memblock::LoadTransaction pmp_fld{
        .address = pmp_base + 0x188,
        .op = memblock::LoadOp::ld,
        .rob = 6,
        .lq = 0,
        .pdest = 26,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadAccessFault,
        .rf_wen = false,
        .fp_wen = true,
    };
    if (!pmp_fault.reset() || !pmp_fault.activate_bare(43) ||
        !pmp_fault.configure_pmp(
            {pmp_napot_address}, {pmp_napot_deny}) ||
        !run_fp_transaction(pmp_fault, pmp_fld, 0, true)) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << pmp_fault.cycle()
                  << " phase=pmp-fault reason="
                  << (pmp_fault.error().empty()
                          ? "PMP-denied FP load reached a data manager"
                          : pmp_fault.error())
                  << " dcache_requests=" << pmp_fault.tilelink_requests()
                  << " uncache_requests=" << pmp_fault.uncache_requests()
                  << '\n';
        return 1;
    }

    memblock::Environment permission_fault(argc, argv);
    constexpr std::uint64_t permission_virtual = 0x5003e000ULL;
    constexpr std::uint64_t permission_physical = 0xc003e000ULL;
    constexpr std::uint64_t permission_root = 0x97018000ULL;
    const memblock::LoadTransaction permission_flw{
        .address = permission_virtual + 0x188,
        .oracle_address = permission_physical + 0x188,
        .op = memblock::LoadOp::lw,
        .rob = 7,
        .lq = 0,
        .pdest = 27,
        .lane = 1,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
        .rf_wen = false,
        .fp_wen = true,
    };
    if (!permission_fault.reset() ||
        !permission_fault.map_sv39_4k(
            permission_virtual, permission_physical, permission_root,
            false, false, true) ||
        !permission_fault.activate_sv39(permission_root, 45) ||
        !run_fp_transaction(permission_fault, permission_flw, 0, true)) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle="
                  << permission_fault.cycle()
                  << " phase=permission-fault reason="
                  << (permission_fault.error().empty()
                          ? "permission-faulting FP load reached a data manager"
                          : permission_fault.error())
                  << " dcache_requests="
                  << permission_fault.tilelink_requests()
                  << " uncache_requests="
                  << permission_fault.uncache_requests() << '\n';
        return 1;
    }

    memblock::Environment guest_fault(argc, argv);
    constexpr std::uint64_t guest_virtual = 0x50040000ULL;
    constexpr std::uint64_t guest_physical = 0x90040000ULL;
    constexpr std::uint64_t host_physical = 0xc0040000ULL;
    constexpr std::uint64_t vs_root = 0x97100000ULL;
    constexpr std::uint64_t g_root = 0x97200000ULL;
    constexpr std::uint64_t guest_offset = 0x188ULL;
    const memblock::LoadTransaction guest_fld{
        .address = guest_virtual + guest_offset,
        .oracle_address = host_physical + guest_offset,
        .op = memblock::LoadOp::ld,
        .rob = 8,
        .lq = 0,
        .pdest = 28,
        .lane = 2,
        .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
        .rf_wen = false,
        .fp_wen = true,
    };
    if (!guest_fault.reset() ||
        !guest_fault.map_sv39_4k(
            guest_virtual, guest_physical, vs_root) ||
        !guest_fault.map_sv39x4_4k(vs_root, vs_root, g_root) ||
        !guest_fault.map_sv39x4_4k(
            vs_root + 0x1000, vs_root + 0x1000, g_root) ||
        !guest_fault.map_sv39x4_4k(
            vs_root + 0x2000, vs_root + 0x2000, g_root) ||
        !guest_fault.map_sv39x4_4k(
            guest_physical, host_physical, g_root,
            false, false, true) ||
        !guest_fault.activate_two_stage(vs_root, g_root, 47, 49) ||
        !run_fp_transaction(guest_fault, guest_fld, 0, true) ||
        guest_fault.exception_vaddr() != guest_fld.address ||
        guest_fault.exception_gpaddr() != guest_physical + guest_offset ||
        guest_fault.exception_is_for_vs_nonleaf_pte()) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << guest_fault.cycle()
                  << " phase=guest-fault reason="
                  << (guest_fault.error().empty()
                          ? "guest-fault metadata or data-manager bypass mismatch"
                          : guest_fault.error())
                  << " expected_vaddr=0x" << std::hex << guest_fld.address
                  << " actual_vaddr=0x" << guest_fault.exception_vaddr()
                  << " expected_gpaddr=0x" << guest_physical + guest_offset
                  << " actual_gpaddr=0x" << guest_fault.exception_gpaddr()
                  << " actual_vs_nonleaf=" << std::dec
                  << guest_fault.exception_is_for_vs_nonleaf_pte()
                  << " dcache_requests=" << guest_fault.tilelink_requests()
                  << " uncache_requests=" << guest_fault.uncache_requests()
                  << '\n';
        return 1;
    }

    memblock::Environment split(argc, argv);
    constexpr std::uint64_t split_base =
        memblock::kDefaultMemoryBase + 0x1c000;
    split.memory().fill_incrementing(split_base, 0x3000, 0xb3);
    if (!split.reset() || !split.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << split.cycle()
                  << " phase=misaligned-configuration reason="
                  << split.error() << '\n';
        return 1;
    }
    const std::array<memblock::LoadTransaction, 3> split_transactions{{
        {
            .address = split_base + 63,
            .op = memblock::LoadOp::lh,
            .rob = 9,
            .lq = 0,
            .pdest = 29,
            .lane = 0,
            .rf_wen = false,
            .fp_wen = true,
        },
        {
            .address = split_base + 0xffe,
            .op = memblock::LoadOp::lw,
            .rob = 10,
            .lq = 1,
            .pdest = 30,
            .lane = 1,
            .rf_wen = false,
            .fp_wen = true,
        },
        {
            .address = split_base + 0x1ffd,
            .op = memblock::LoadOp::ld,
            .rob = 11,
            .lq = 2,
            .pdest = 31,
            .lane = 2,
            .rf_wen = false,
            .fp_wen = true,
        },
    }};
    for (const auto &transaction : split_transactions) {
        const std::uint64_t expected = expected_fp_data(
            split.memory(), transaction.address, transaction.op);
        if (!run_fp_transaction(split, transaction, expected, false)) {
            std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << split.cycle()
                      << " phase=misaligned-cacheable reason="
                      << split.error() << '\n';
            return 1;
        }
    }

    memblock::Environment nc_misaligned(argc, argv);
    constexpr std::uint64_t nc_virtual = 0x50044000ULL;
    constexpr std::uint64_t nc_physical = 0xc0044000ULL;
    constexpr std::uint64_t nc_root = 0x97300000ULL;
    const memblock::LoadTransaction nc_fld{
        .address = nc_virtual + 1,
        .oracle_address = nc_physical + 1,
        .op = memblock::LoadOp::ld,
        .rob = 12,
        .lq = 0,
        .pdest = 32,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadAddressMisaligned,
        .rf_wen = false,
        .fp_wen = true,
    };
    if (!nc_misaligned.reset() ||
        !nc_misaligned.enable_misaligned_accesses() ||
        !nc_misaligned.map_sv39_4k(
            nc_virtual, nc_physical, nc_root,
            true, true, false, false, true) ||
        !nc_misaligned.activate_sv39(nc_root, 51) ||
        !run_fp_transaction(nc_misaligned, nc_fld, 0, true)) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle="
                  << nc_misaligned.cycle()
                  << " phase=misaligned-nc reason="
                  << (nc_misaligned.error().empty()
                          ? "misaligned NC FP load reached a data manager"
                          : nc_misaligned.error())
                  << " dcache_requests="
                  << nc_misaligned.tilelink_requests()
                  << " uncache_requests="
                  << nc_misaligned.uncache_requests() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_FP_LOADS_PASS"
              << " cycle="
              << environment.cycle() + mmio.cycle() + page_fault.cycle() +
                     pmp_fault.cycle() + permission_fault.cycle() +
                     guest_fault.cycle() + split.cycle() +
                     nc_misaligned.cycle()
              << " writebacks="
              << environment.writebacks() + mmio.writebacks() +
                     page_fault.writebacks() + pmp_fault.writebacks() +
                     permission_fault.writebacks() + guest_fault.writebacks() +
                     split.writebacks() + nc_misaligned.writebacks()
              << " fp_destinations=" << transactions.size()
              << " mmio_fp=5 mmio_flh=1 mmio_flw=2 mmio_fld=2"
              << " mmio_faults=" << mmio_faults
              << " page_faults=1"
              << " pmp_faults=1 permission_faults=1 guest_faults=1"
              << " misaligned_cacheable=3 misaligned_nc_faults=1"
              << " mmio_dcache_requests=" << mmio.tilelink_requests()
              << " mmio_uncache_requests=" << mmio.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_trigger_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x2a000;
    environment.memory().fill_incrementing(base, 0x5000, 0x62);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    unsigned next_identity = 0;
    std::uint64_t breakpoint_loads = 0;
    std::uint64_t suppressed_loads = 0;
    std::uint64_t debug_action_loads = 0;
    const auto run_load = [&](std::uint64_t address,
                              std::uint8_t expected_trigger,
                              std::uint32_t expected_exception,
                              bool expect_dcache_request,
                              const char *phase) {
        const bool breakpoint =
            expected_trigger == memblock::kTriggerBreakpoint;
        const bool debug_action =
            expected_trigger == memblock::kTriggerDebugMode;
        const bool terminal_trigger = breakpoint || debug_action;
        const auto identity = next_identity++;
        const memblock::LoadTransaction transaction{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(33 + identity),
            .lq = static_cast<std::uint8_t>(identity),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(23 + identity),
            .lane = identity % memblock::kScalarLoadLanes,
            .expected_exception_mask = expected_exception,
            .check_data = !debug_action,
            .expected_trigger = expected_trigger,
            .predecode_rvc = (identity & 1U) != 0,
            .ftq_ptr = 7 + identity,
            .ftq_offset = static_cast<std::uint8_t>(identity & 0xfU),
        };
        const std::uint64_t requests_before = environment.tilelink_requests();
        environment.expect_load(transaction);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 256) ||
            !environment.run_until_complete(2048)) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        const std::uint64_t requests_after = environment.tilelink_requests();
        if ((!expect_dcache_request && requests_after != requests_before) ||
            (expect_dcache_request && requests_after <= requests_before)) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " expected_trigger="
                      << static_cast<unsigned>(expected_trigger)
                      << " dcache_requests=" << requests_before << "->"
                      << requests_after << '\n';
            return false;
        }
        if (terminal_trigger) {
            breakpoint_loads += breakpoint;
            debug_action_loads += debug_action;
            if (!environment.redirect_after(
                    transaction.rob, transaction.rob_flag, true) ||
                !environment.run_cycles(16)) {
                std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << phase
                          << " reason=" << environment.error() << '\n';
                return false;
            }
            const std::uint64_t retired = environment.lq_dequeued() +
                environment.lq_canceled();
            if (retired > environment.lq_allocated() ||
                environment.lq_allocated() - retired > 1 ||
                (retired < environment.lq_allocated() &&
                 !environment.account_lq_cancellation(1))) {
                std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << phase
                          << " reason=load_queue_cancellation"
                          << " allocated=" << environment.lq_allocated()
                          << " retired=" << retired << '\n';
                return false;
            }
        } else {
            ++suppressed_loads;
            if (!environment.run_until_lq_retired(2048)) {
                std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << phase
                          << " reason=" << environment.error() << '\n';
                return false;
            }
        }
        return true;
    };

    const auto configure_and_run = [&](const memblock::MemoryTriggerConfig &config,
                                       std::uint64_t address, bool breakpoint,
                                       const char *phase) {
        if (!environment.configure_memory_trigger(config)) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        return run_load(
            address,
            breakpoint ? memblock::kTriggerBreakpoint : memblock::kTriggerNone,
            breakpoint ? memblock::kExceptionBreakpoint : std::uint32_t{0},
            !breakpoint, phase);
    };

    if (!configure_and_run(
            {.index = 0, .address = base + 0x028},
            base + 0x028, true, "equal-hit") ||
        !configure_and_run(
            {.index = 0, .address = base + 0x428,
             .trigger_can_raise_breakpoint = false},
            base + 0x428, false, "breakpoint-gate") ||
        !configure_and_run(
            {.index = 0, .address = base + 0x828, .debug_mode = true},
            base + 0x828, false, "debug-mode-suppression") ||
        !configure_and_run(
            {.index = 0, .address = base + 0xc28, .enable_mask = 0},
            base + 0xc28, false, "enable-suppression") ||
        !configure_and_run(
            {.index = 0, .address = base + 0x1028, .select = true},
            base + 0x1028, false, "select-suppression") ||
        !configure_and_run(
            {.index = 0, .address = base + 0x1428,
             .load = false, .store = true},
            base + 0x1428, false, "load-select-suppression") ||
        !configure_and_run(
            {.index = 0, .address = base + 0x1800,
             .match_type = memblock::kTriggerMatchGreaterOrEqual},
            base + 0x1828, true, "greater-equal-hit") ||
        !configure_and_run(
            {.index = 0, .address = base + 0x1c80,
             .match_type = memblock::kTriggerMatchGreaterOrEqual},
            base + 0x1c28, false, "greater-equal-miss") ||
        !configure_and_run(
            {.index = 0, .address = base + 0x2080,
             .match_type = memblock::kTriggerMatchLessThan},
            base + 0x2028, true, "less-than-hit") ||
        !configure_and_run(
            {.index = 0, .address = base + 0x2400,
             .match_type = memblock::kTriggerMatchLessThan},
            base + 0x2428, false, "less-than-miss")) {
        return 1;
    }

    const auto run_chain_case = [&](std::uint64_t first_address,
                                    std::uint64_t second_address,
                                    bool breakpoint, const char *phase) {
        if (!environment.configure_memory_trigger({
                .index = 0,
                .address = first_address,
                .enable_mask = 0,
                .chain = true,
            }) ||
            !environment.configure_memory_trigger({
                .index = 1,
                .address = second_address,
                .enable_mask = 0x3,
            })) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        return run_load(
            second_address,
            breakpoint ? memblock::kTriggerBreakpoint : memblock::kTriggerNone,
            breakpoint ? memblock::kExceptionBreakpoint : std::uint32_t{0},
            !breakpoint, phase);
    };
    if (!run_chain_case(
            base + 0x2828, base + 0x2828, true, "chain-hit") ||
        !run_chain_case(
            base + 0x2c28, base + 0x3028, false, "chain-miss") ||
        !configure_and_run(
            {.index = 2, .address = base + 0x3828, .enable_mask = 0x4},
            base + 0x3828, true, "slot-two-hit") ||
        !configure_and_run(
            {.index = 3, .address = base + 0x3c28, .enable_mask = 0x8},
            base + 0x3c28, true, "slot-three-hit")) {
        return 1;
    }

    constexpr std::uint64_t debug_action_address = base + 0x4028;
    if (!environment.configure_memory_trigger({
            .index = 0,
            .address = debug_action_address,
            .action = memblock::kTriggerDebugMode,
        }) ||
        !run_load(
            debug_action_address, memblock::kTriggerDebugMode, 0, false,
            "debug-action")) {
        return 1;
    }

    std::uint64_t scalar_store_breakpoints = 0;
    const auto run_store = [&](std::uint64_t address, std::uint8_t sq,
                               const char *phase) {
        if (!environment.configure_memory_trigger({
                .index = 0,
                .address = address,
                .load = false,
                .store = true,
            })) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        const memblock::StoreTransaction store{
            .address = address,
            .data = 0xa5a55a5af0f00f0fULL ^ address,
            .op = memblock::StoreOp::sd,
            .rob = static_cast<std::uint8_t>(33 + next_identity++),
            .sq = sq,
            .address_lane = sq % memblock::kScalarStoreLanes,
            .data_lane = (sq + 1) % memblock::kScalarStoreLanes,
            .expected_exception_mask = memblock::kExceptionBreakpoint,
            .expected_trigger = memblock::kTriggerBreakpoint,
        };
        const std::uint64_t requests_before = environment.tilelink_requests();
        const std::uint64_t value_before = environment.memory().read_u64(address);
        environment.expect_store(store);
        if (!environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 256) ||
            !environment.issue_store_data(store, 256) ||
            !environment.run_until_store_complete(2048) ||
            environment.tilelink_requests() != requests_before ||
            !environment.redirect_after(store.rob, store.rob_flag, true) ||
            !environment.run_cycles(16)) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " dcache_requests=" << requests_before << "->"
                      << environment.tilelink_requests()
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        const std::uint64_t retired = environment.sq_dequeued() +
            environment.sq_canceled();
        if (retired > environment.sq_allocated() ||
            environment.sq_allocated() - retired > 1 ||
            (retired < environment.sq_allocated() &&
             !environment.account_sq_cancellation(1)) ||
            environment.memory().read_u64(address) != value_before) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " allocated=" << environment.sq_allocated()
                      << " retired=" << retired
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        ++scalar_store_breakpoints;
        return true;
    };
    if (!run_store(base + 0x3428, 0, "aligned-store-hit") ||
        !run_store(base + 0x442b, 1, "misaligned-store-hit")) {
        return 1;
    }

    memblock::Environment vector_load_environment(argc, argv);
    constexpr std::uint64_t vector_load_base =
        memblock::kDefaultMemoryBase + 0x620000;
    constexpr std::uint64_t vector_load_address = vector_load_base + 0x20;
    vector_load_environment.memory().fill_incrementing(
        vector_load_base, 0x1000, 0x49);
    const memblock::VectorMemoryTransaction vector_load{
        .address = vector_load_address,
        .eew = 0,
        .vl = 16,
        .rob = 0,
        .lq = 0,
        .pdest = 91,
        .lane = 0,
        .flow_num = 2,
        .expected_exception_mask = memblock::kExceptionBreakpoint,
        .expected_trigger = memblock::kTriggerBreakpoint,
        .expected_writeback_vstart = 7,
    };
    if (!vector_load_environment.reset() ||
        !vector_load_environment.configure_memory_trigger({
            .index = 0,
            .address = vector_load_address + 7,
        })) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << vector_load_environment.cycle()
                  << " phase=vector-load-configuration reason="
                  << vector_load_environment.error() << '\n';
        return 1;
    }
    const std::uint64_t vector_load_requests_before =
        vector_load_environment.tilelink_requests();
    vector_load_environment.expect_vector(vector_load);
    if (!vector_load_environment.enqueue_vector(vector_load) ||
        !vector_load_environment.issue_vector(vector_load, 256) ||
        !vector_load_environment.run_until_vector_complete(2048) ||
        vector_load_environment.tilelink_requests() !=
            vector_load_requests_before ||
        !vector_load_environment.redirect_after(
            vector_load.rob, vector_load.rob_flag, true) ||
        !vector_load_environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << vector_load_environment.cycle()
                  << " phase=vector-load-hit reason="
                  << vector_load_environment.error() << '\n';
        return 1;
    }
    const std::uint64_t vector_lq_retired =
        vector_load_environment.lq_dequeued() +
        vector_load_environment.lq_canceled();
    const bool vector_lq_over_retired =
        vector_lq_retired > vector_load_environment.lq_allocated();
    const std::uint64_t vector_lq_remaining = vector_lq_over_retired
        ? 0
        : vector_load_environment.lq_allocated() - vector_lq_retired;
    if (vector_lq_over_retired ||
        vector_lq_remaining > vector_load.flow_num ||
        (vector_lq_remaining != 0 &&
         !vector_load_environment.account_lq_cancellation(
             static_cast<unsigned>(vector_lq_remaining)))) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << vector_load_environment.cycle()
                  << " phase=vector-load-cancellation"
                  << " allocated=" << vector_load_environment.lq_allocated()
                  << " retired=" << vector_lq_retired << '\n';
        return 1;
    }

    memblock::Environment vector_store_environment(argc, argv);
    constexpr std::uint64_t vector_store_base =
        memblock::kDefaultMemoryBase + 0x630000;
    constexpr std::uint64_t vector_store_address = vector_store_base + 0x20;
    vector_store_environment.memory().fill_incrementing(
        vector_store_base, 0x1000, 0x6b);
    memblock::VectorMemoryTransaction vector_store{
        .store = true,
        .address = vector_store_address,
        .eew = 0,
        .vl = 16,
        .rob = 0,
        .sq = 0,
        .lane = 1,
        .flow_num = 2,
        .expected_exception_mask = memblock::kExceptionBreakpoint,
        .expected_trigger = memblock::kTriggerBreakpoint,
        .expected_writeback_vstart = 9,
    };
    for (unsigned byte = 0; byte < vector_store.data.size(); ++byte) {
        vector_store.data[byte] = static_cast<unsigned char>(0xd1 + byte * 3);
    }
    const auto vector_store_before =
        vector_store_environment.memory().read_beat(vector_store_address, 16);
    if (!vector_store_environment.reset() ||
        !vector_store_environment.configure_memory_trigger({
            .index = 0,
            .address = vector_store_address + 9,
            .load = false,
            .store = true,
        })) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << vector_store_environment.cycle()
                  << " phase=vector-store-configuration reason="
                  << vector_store_environment.error() << '\n';
        return 1;
    }
    const std::uint64_t vector_store_requests_before =
        vector_store_environment.tilelink_requests();
    vector_store_environment.expect_vector(vector_store);
    if (!vector_store_environment.enqueue_vector(vector_store) ||
        !vector_store_environment.issue_vector(vector_store, 256) ||
        !vector_store_environment.run_until_vector_complete(2048) ||
        vector_store_environment.tilelink_requests() !=
            vector_store_requests_before ||
        !vector_store_environment.redirect_after(
            vector_store.rob, vector_store.rob_flag, true) ||
        !vector_store_environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << vector_store_environment.cycle()
                  << " phase=vector-store-hit reason="
                  << vector_store_environment.error() << '\n';
        return 1;
    }
    const std::uint64_t vector_sq_retired =
        vector_store_environment.sq_dequeued() +
        vector_store_environment.sq_canceled();
    const bool vector_sq_over_retired =
        vector_sq_retired > vector_store_environment.sq_allocated();
    const std::uint64_t vector_sq_remaining = vector_sq_over_retired
        ? 0
        : vector_store_environment.sq_allocated() - vector_sq_retired;
    if (vector_sq_over_retired ||
        vector_sq_remaining > vector_store.flow_num ||
        (vector_sq_remaining != 0 &&
         !vector_store_environment.account_sq_cancellation(
             static_cast<unsigned>(vector_sq_remaining))) ||
        vector_store_environment.memory().read_beat(
            vector_store_address, 16) != vector_store_before) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << vector_store_environment.cycle()
                  << " phase=vector-store-cancellation"
                  << " allocated=" << vector_store_environment.sq_allocated()
                  << " retired=" << vector_sq_retired << '\n';
        return 1;
    }

    std::uint64_t vector_cross_cycles = 0;
    std::uint64_t vector_breakpoints = 2;
    std::uint64_t vector_debug_actions = 0;
    std::uint64_t vector_segment_cases = 0;
    const auto run_vector_cross = [argc, argv, &vector_cross_cycles,
                                   &vector_breakpoints,
                                   &vector_debug_actions,
                                   &vector_segment_cases](
        memblock::VectorMemoryTransaction transaction,
        unsigned trigger_element, std::uint8_t action,
        std::uint64_t expected_request_delta, const char *phase) {
        memblock::Environment cross_environment(argc, argv);
        const std::uint64_t page_base = transaction.address & ~0xfffULL;
        cross_environment.memory().fill_incrementing(
            page_base, 0x1000,
            static_cast<std::uint8_t>(0x23 + vector_breakpoints * 7 +
                                      vector_debug_actions * 11));

        if (transaction.segment) {
            transaction.nf = 1;
            transaction.vuop_idx = 0;
            transaction.last_uop = false;
        }
        std::vector<memblock::VectorMemoryTransaction> transactions{
            transaction,
        };
        if (transaction.segment) {
            auto last_field = transaction;
            last_field.vuop_idx = 1;
            last_field.last_uop = true;
            if (!last_field.store) {
                ++last_field.pdest;
            }
            transactions.push_back(last_field);
        }
        for (unsigned field = 0; field < transactions.size(); ++field) {
            auto &field_transaction = transactions[field];
            for (unsigned byte = 0; byte < field_transaction.data.size(); ++byte) {
                field_transaction.data[byte] = static_cast<unsigned char>(
                    0x51 + byte * 5 + field * 0x20 + vector_breakpoints * 3);
            }
            field_transaction.expected_trigger = action;
            field_transaction.expected_exception_mask =
                action == memblock::kTriggerBreakpoint
                    ? memblock::kExceptionBreakpoint
                    : std::uint32_t{0};
            field_transaction.expected_writeback_vstart =
                static_cast<std::uint8_t>(trigger_element);
            field_transaction.check_data =
                action != memblock::kTriggerDebugMode;
        }

        const unsigned element_count = 16U >> transaction.eew;
        const std::uint16_t active =
            memblock::active_vector_elements(transaction);
        if (trigger_element >= element_count ||
            ((active >> trigger_element) & 1U) == 0) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL phase=" << phase
                      << " reason=trigger element is inactive"
                      << " element=" << trigger_element
                      << " active=0x" << std::hex << active << std::dec << '\n';
            return false;
        }
        const auto &trigger_transaction = transaction.segment
            ? transactions.back()
            : transactions.front();
        const std::uint64_t trigger_address =
            memblock::vector_element_address(
                trigger_transaction, trigger_element);
        std::vector<std::pair<std::uint64_t, unsigned char>> memory_before;
        if (transaction.store) {
            const unsigned element_bytes = 1U << transaction.eew;
            for (const auto &field_transaction : transactions) {
                for (unsigned element = 0; element < element_count; ++element) {
                    if (((active >> element) & 1U) == 0) {
                        continue;
                    }
                    const std::uint64_t element_address =
                        memblock::vector_element_address(
                            field_transaction, element);
                    for (unsigned byte = 0; byte < element_bytes; ++byte) {
                        memory_before.emplace_back(
                            element_address + byte,
                            cross_environment.memory().read_byte(
                                element_address + byte));
                    }
                }
            }
        }
        if (!cross_environment.reset() ||
            !cross_environment.configure_memory_trigger({
                .index = 0,
                .address = trigger_address,
                .action = action,
                .load = !transaction.store,
                .store = transaction.store,
            })) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << cross_environment.cycle() << " phase=" << phase
                      << " reason=" << cross_environment.error() << '\n';
            return false;
        }

        const std::uint64_t requests_before =
            cross_environment.tilelink_requests();
        for (const auto &field_transaction : transactions) {
            cross_environment.expect_vector(field_transaction);
            if (!cross_environment.enqueue_vector(field_transaction) ||
                !cross_environment.issue_vector(field_transaction, 512)) {
                std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                          << cross_environment.cycle() << " phase=" << phase
                          << " reason=" << cross_environment.error() << '\n';
                return false;
            }
        }
        if (!cross_environment.run_until_vector_complete(4096) ||
            cross_environment.tilelink_requests() !=
                requests_before + expected_request_delta ||
            !cross_environment.redirect_after(
                transaction.rob, transaction.rob_flag, true) ||
            !cross_environment.run_cycles(16)) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << cross_environment.cycle() << " phase=" << phase
                      << " dcache_requests=" << requests_before << "->"
                      << cross_environment.tilelink_requests()
                      << " reason=" << cross_environment.error() << '\n';
            return false;
        }

        const std::uint64_t allocated = transaction.store
            ? cross_environment.sq_allocated()
            : cross_environment.lq_allocated();
        const std::uint64_t retired = transaction.store
            ? cross_environment.sq_dequeued() + cross_environment.sq_canceled()
            : cross_environment.lq_dequeued() + cross_environment.lq_canceled();
        const bool over_retired = retired > allocated;
        const std::uint64_t remaining = over_retired ? 0 : allocated - retired;
        const bool cancellation_failed = remaining != 0 &&
            !(transaction.store
                  ? cross_environment.account_sq_cancellation(
                        static_cast<unsigned>(remaining))
                  : cross_environment.account_lq_cancellation(
                        static_cast<unsigned>(remaining)));
        bool memory_changed = false;
        for (const auto &[address, value] : memory_before) {
            memory_changed |=
                cross_environment.memory().read_byte(address) != value;
        }
        if (over_retired || remaining > transaction.flow_num ||
            cancellation_failed ||
            (transaction.segment && allocated != 0) || memory_changed) {
            std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                      << cross_environment.cycle() << " phase=" << phase
                      << " allocated=" << allocated << " retired=" << retired
                      << " remaining=" << remaining
                      << " memory_changed=" << memory_changed
                      << " reason=" << cross_environment.error() << '\n';
            return false;
        }

        vector_cross_cycles += cross_environment.cycle();
        vector_breakpoints += action == memblock::kTriggerBreakpoint;
        vector_debug_actions += action == memblock::kTriggerDebugMode;
        vector_segment_cases += transaction.segment;
        return true;
    };

    memblock::VectorMemoryTransaction wide_strided_load{
        .address = memblock::kDefaultMemoryBase + 0x640020,
        .stride = 24,
        .addressing = memblock::VectorAddressingMode::strided,
        .eew = 3,
        .vl = 2,
        .rob = 0,
        .lq = 0,
        .pdest = 92,
        .lane = 1,
        .flow_num = 2,
    };
    if (!run_vector_cross(
            wide_strided_load, 1, memblock::kTriggerBreakpoint,
            1,
            "vector-strided-eew64-breakpoint")) {
        return 1;
    }

    memblock::VectorMemoryTransaction indexed_store{
        .store = true,
        .address = memblock::kDefaultMemoryBase + 0x650020,
        .addressing = memblock::VectorAddressingMode::indexed_unordered,
        .eew = 2,
        .vl = 4,
        .rob = 0,
        .sq = 0,
        .lane = 0,
        .flow_num = 4,
    };
    constexpr std::array<std::uint32_t, 4> indexed_store_offsets{
        0x30, 0x08, 0x50, 0x18,
    };
    for (unsigned element = 0; element < indexed_store_offsets.size(); ++element) {
        for (unsigned byte = 0; byte < 4; ++byte) {
            indexed_store.index[element * 4 + byte] =
                static_cast<unsigned char>(
                    indexed_store_offsets[element] >> (8 * byte));
        }
    }
    if (!run_vector_cross(
            indexed_store, 0, memblock::kTriggerBreakpoint,
            0,
            "vector-indexed-eew32-store-breakpoint")) {
        return 1;
    }

    memblock::VectorMemoryTransaction segment_load{
        .segment = true,
        .address = memblock::kDefaultMemoryBase + 0x660020,
        .addressing = memblock::VectorAddressingMode::indexed_ordered,
        .eew = 1,
        .vl = 8,
        .rob = 0,
        .pdest = 93,
        .lane = 0,
        .flow_num = 8,
        .vuop_idx = 0,
        .nf = 1,
    };
    for (unsigned element = 0; element < 8; ++element) {
        const std::uint16_t offset = static_cast<std::uint16_t>(
            0x34 + element * 20);
        segment_load.index[element * 2] =
            static_cast<unsigned char>(offset);
        segment_load.index[element * 2 + 1] =
            static_cast<unsigned char>(offset >> 8);
    }
    if (!run_vector_cross(
            segment_load, 0, memblock::kTriggerBreakpoint,
            1,
            "vector-segment-indexed-load-breakpoint")) {
        return 1;
    }

    memblock::VectorMemoryTransaction segment_store{
        .store = true,
        .segment = true,
        .address = memblock::kDefaultMemoryBase + 0x670100,
        .stride = 12,
        .addressing = memblock::VectorAddressingMode::strided,
        .eew = 1,
        .vl = 8,
        .rob = 0,
        .lane = 0,
        .flow_num = 8,
        .vuop_idx = 0,
        .nf = 1,
    };
    if (!run_vector_cross(
            segment_store, 0, memblock::kTriggerBreakpoint,
            0,
            "vector-segment-strided-store-breakpoint")) {
        return 1;
    }

    memblock::VectorMemoryTransaction debug_vector_load{
        .address = memblock::kDefaultMemoryBase + 0x680020,
        .eew = 2,
        .vl = 4,
        .rob = 0,
        .lq = 0,
        .pdest = 94,
        .lane = 1,
        .flow_num = 4,
    };
    if (!run_vector_cross(
            debug_vector_load, 3, memblock::kTriggerDebugMode,
            1,
            "vector-unit-eew32-debug-action")) {
        return 1;
    }

    memblock::VectorMemoryTransaction debug_vector_store{
        .store = true,
        .address = memblock::kDefaultMemoryBase + 0x690020,
        .addressing = memblock::VectorAddressingMode::indexed_ordered,
        .eew = 3,
        .vl = 2,
        .rob = 0,
        .sq = 0,
        .lane = 0,
        .flow_num = 2,
    };
    constexpr std::array<std::uint64_t, 2> debug_store_offsets{0x40, 0x18};
    for (unsigned element = 0; element < debug_store_offsets.size(); ++element) {
        for (unsigned byte = 0; byte < 8; ++byte) {
            debug_vector_store.index[element * 8 + byte] =
                static_cast<unsigned char>(
                    debug_store_offsets[element] >> (8 * byte));
        }
    }
    if (!run_vector_cross(
            debug_vector_store, 0, memblock::kTriggerDebugMode,
            0,
            "vector-indexed-eew64-store-debug-action")) {
        return 1;
    }

    std::cout << "MEMBLOCK_TRIGGER_CONTRACTS_PASS"
              << " cycle=" << environment.cycle() +
                    vector_load_environment.cycle() +
                    vector_store_environment.cycle() + vector_cross_cycles
              << " cases=25 actions=2 match_types=3 enabled_slots=4"
              << " breakpoint_loads=" << breakpoint_loads
              << " suppressed_loads=" << suppressed_loads
              << " debug_action_loads=" << debug_action_loads
              << " scalar_store_breakpoints=" << scalar_store_breakpoints
              << " vector_cases=8 vector_loads=4 vector_stores=4"
              << " vector_breakpoints=" << vector_breakpoints
              << " vector_debug_actions=" << vector_debug_actions
              << " vector_segments=" << vector_segment_cases
              << " vector_widths=4 vector_addressing_modes=4 chain_cases=2"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_metadata_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x2b000;
    const std::uint64_t address = base + 0x10;
    environment.memory().fill_incrementing(base, 64, 0x37);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_METADATA_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction transaction{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 34,
        .lq = 1,
        .sq = 0,
        .pdest = 24,
        .lane = 1,
        // The top-level issueLda contract does not expose exceptionVec: LSQ
        // retains the enqueue copy for its own exception machinery, while
        // LoadUnit receives a separately shaped issue uop.  Keep this runtime
        // contract focused on the observable RVC/FTQ issue metadata.
        .expected_exception_mask = 0,
        .input_exception_mask = 0,
        .predecode_rvc = true,
        .ftq_ptr = 9,
        .ftq_offset = 3,
        .store_set_hit = true,
        .wait_for_rob_flag = false,
        .wait_for_rob_value = 0,
        .load_wait_bit = true,
        .load_wait_strict = true,
    };
    environment.expect_load(transaction);
    if (!environment.enqueue_load(transaction) ||
        !environment.issue_load(transaction) ||
        !environment.run_until_complete(512)) {
        std::cerr << "MEMBLOCK_METADATA_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=exception-vector reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_METADATA_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " issue_metadata=rvc-ftq-store-set-wait"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
