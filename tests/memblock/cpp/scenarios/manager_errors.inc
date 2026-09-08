// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_dcache_errors(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x2c000;
    environment.memory().fill_incrementing(base, 0x1000, 0x49);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle=" << environment.cycle()
                  << " phase=reset reason=" << environment.error() << '\n';
        return 1;
    }
    const auto feedback_totals = [&environment]() {
        const auto &feedback = environment.scalar_load_feedback_stats();
        std::pair<std::uint64_t, std::uint64_t> totals{};
        for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
            totals.first += feedback.wakeups[lane];
            totals.second += feedback.ld2_cancels[lane];
        }
        return totals;
    };

    const memblock::LoadTransaction denied{
        .address = base + 0x100,
        .op = memblock::LoadOp::ld,
        .rob = 35,
        .lq = 2,
        .pdest = 25,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadAccessFault,
    };
    environment.expect_load(denied);
    environment.inject_next_dcache_response_error(true, false);
    if (!environment.set_rob_head(denied.rob, denied.rob_flag) ||
        !environment.enqueue_load(denied) ||
        !environment.issue_load(denied) ||
        !environment.run_until_complete(1024) ||
        !environment.run_cycles(8) ||
        !environment.redirect_after(denied.rob, denied.rob_flag, true) ||
        !environment.run_cycles(96)) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle=" << environment.cycle()
                  << " phase=denied reason=" << environment.error() << '\n';
        return 1;
    }
    const auto denied_feedback = feedback_totals();

    const memblock::LoadTransaction corrupt{
        .address = base + 0x200,
        .op = memblock::LoadOp::ld,
        .rob = 36,
        .lq = 3,
        .pdest = 26,
        .lane = 1,
        .expected_exception_mask = memblock::kExceptionHardwareError,
    };
    environment.expect_load(corrupt);
    environment.inject_next_dcache_response_error(false, true);
    if (!environment.set_rob_head(corrupt.rob, corrupt.rob_flag) ||
        !environment.enqueue_load(corrupt) ||
        !environment.issue_load(corrupt) ||
        !environment.run_until_complete(1024) ||
        !environment.run_cycles(8) ||
        !environment.redirect_after(corrupt.rob, corrupt.rob_flag, true) ||
        !environment.run_cycles(96)) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle=" << environment.cycle()
                  << " phase=corrupt reason=" << environment.error() << '\n';
        return 1;
    }
    const auto combined_feedback = feedback_totals();
    const std::uint64_t corrupt_wakeups =
        combined_feedback.first - denied_feedback.first;
    const std::uint64_t corrupt_cancels =
        combined_feedback.second - denied_feedback.second;
    if (denied_feedback.second == 0 ||
        denied_feedback.first != denied_feedback.second ||
        corrupt_cancels == 0 || corrupt_wakeups != corrupt_cancels) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                  << environment.cycle()
                  << " phase=feedback-classification denied_wakeups="
                  << denied_feedback.first << " denied_cancels="
                  << denied_feedback.second << " corrupt_wakeups="
                  << corrupt_wakeups << " corrupt_cancels="
                  << corrupt_cancels << '\n';
        return 1;
    }

    struct MultibeatErrorCase {
        const char *name;
        bool denied;
        memblock::DcacheCorruptBeat corrupt_beat;
    };
    const std::array<MultibeatErrorCase, 3> multibeat_error_cases{{
        {"denied", true, memblock::DcacheCorruptBeat::all},
        {"corrupt-first", false, memblock::DcacheCorruptBeat::first},
        {"corrupt-last", false, memblock::DcacheCorruptBeat::last},
    }};
    unsigned multibeat_cases = 0;
    unsigned multibeat_denied_cases = 0;
    unsigned multibeat_corrupt_first_cases = 0;
    unsigned multibeat_corrupt_last_cases = 0;
    unsigned multibeat_keyword_cases = 0;
    unsigned multibeat_nonkeyword_cases = 0;
    unsigned multibeat_concurrent_mshr_cases = 0;
    unsigned multibeat_poisoned_hit_cases = 0;
    unsigned multibeat_healthy_hit_cases = 0;
    std::uint64_t multibeat_cycles = 0;
    for (unsigned keyword_index = 0; keyword_index < 2; ++keyword_index) {
        const bool keyword = keyword_index != 0;
        for (unsigned error_index = 0;
             error_index < multibeat_error_cases.size(); ++error_index) {
            const auto &test_case = multibeat_error_cases[error_index];
            memblock::Environment multibeat(argc, argv);
            const std::uint64_t case_index =
                keyword_index * multibeat_error_cases.size() + error_index;
            const std::uint64_t poison_line = base + 0x1000 + case_index * 0x100;
            const std::uint64_t healthy_line = poison_line + 0x40;
            const std::uint64_t primary_address =
                poison_line + (keyword ? 0x38 : 0x18);
            const std::uint64_t merged_address =
                poison_line + (keyword ? 0x18 : 0x38);
            const std::uint64_t healthy_address = healthy_line + 0x18;
            multibeat.memory().fill_incrementing(
                poison_line, 64,
                static_cast<std::uint8_t>(0x61 + case_index * 9));
            multibeat.memory().fill_incrementing(
                healthy_line, 64,
                static_cast<std::uint8_t>(0xa3 + case_index * 7));
            if (!multibeat.reset()) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-reset case=" << test_case.name
                          << " keyword=" << keyword << " reason="
                          << multibeat.error() << '\n';
                return 1;
            }

            const std::uint32_t poison_exception = test_case.denied
                ? memblock::kExceptionLoadAccessFault
                : memblock::kExceptionHardwareError;
            const bool primary_faults = test_case.denied ||
                test_case.corrupt_beat == memblock::DcacheCorruptBeat::first;
            const memblock::LoadTransaction primary{
                .address = primary_address,
                .op = memblock::LoadOp::ld,
                .rob = 0,
                .lq = 0,
                .pdest = 180,
                .lane = 0,
                .expected_exception_mask = primary_faults
                    ? poison_exception : 0,
                .expected_debug_is_mmio = false,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            };
            multibeat.expect_load(primary);
            multibeat.force_next_dcache_response_delay(32);
            multibeat.force_next_dcache_interbeat_delay(256);
            multibeat.inject_dcache_response_error_at(
                poison_line, test_case.denied, !test_case.denied,
                test_case.corrupt_beat);
            if (!multibeat.set_rob_head(primary.rob, primary.rob_flag) ||
                !multibeat.enqueue_load(primary) ||
                !multibeat.issue_load(primary, 512) ||
                !multibeat.run_until_complete(1024)) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-primary case=" << test_case.name
                          << " keyword=" << keyword << " reason="
                          << multibeat.error() << '\n';
                return 1;
            }
            const std::uint64_t expected_first_denied_beats =
                test_case.denied ? 1 : 0;
            const std::uint64_t expected_first_corrupt_beats =
                test_case.denied ||
                    test_case.corrupt_beat ==
                        memblock::DcacheCorruptBeat::first
                ? 1 : 0;
            if (multibeat.tilelink_requests() != 1 ||
                multibeat.dcache_refills() != 1 ||
                multibeat.dcache_grant_data_beats() != 1 ||
                multibeat.dcache_denied_d_beats() !=
                    expected_first_denied_beats ||
                multibeat.dcache_corrupt_d_beats() !=
                    expected_first_corrupt_beats ||
                multibeat.dcache_error_response_requests() != 1 ||
                (multibeat.dcache_last_error_response_address() &
                 ~std::uint64_t{63}) != poison_line ||
                multibeat.dcache_last_error_response_keyword() != keyword ||
                multibeat.dcache_responses_idle() ||
                multibeat.writebacks() != 1) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-first-beat case="
                          << test_case.name << " keyword=" << keyword
                          << " requests=" << multibeat.tilelink_requests()
                          << " refills=" << multibeat.dcache_refills()
                          << " grant_beats="
                          << multibeat.dcache_grant_data_beats()
                          << " denied_beats="
                          << multibeat.dcache_denied_d_beats()
                          << " corrupt_beats="
                          << multibeat.dcache_corrupt_d_beats()
                          << " error_requests="
                          << multibeat.dcache_error_response_requests()
                          << " error_address=0x" << std::hex
                          << multibeat.dcache_last_error_response_address()
                          << std::dec << " error_keyword="
                          << multibeat.dcache_last_error_response_keyword()
                          << " responses_idle="
                          << multibeat.dcache_responses_idle()
                          << " writebacks=" << multibeat.writebacks() << '\n';
                return 1;
            }

            const std::vector<memblock::LoadTransaction> overlap_loads{
                {
                    .address = merged_address,
                    .op = memblock::LoadOp::ld,
                    .rob = 1,
                    .lq = 1,
                    .pdest = 181,
                    .lane = 1,
                    .expected_exception_mask = poison_exception,
                    .expected_debug_is_mmio = false,
                    .expected_debug_is_ncio = false,
                    .expected_debug_is_perf_cnt = false,
                },
                {
                    .address = healthy_address,
                    .op = memblock::LoadOp::ld,
                    .rob = 2,
                    .lq = 2,
                    .pdest = 182,
                    .lane = 2,
                    .expected_debug_is_mmio = false,
                    .expected_debug_is_ncio = false,
                    .expected_debug_is_perf_cnt = false,
                },
            };
            multibeat.expect_load(overlap_loads[0]);
            multibeat.expect_load(overlap_loads[1]);
            if (!multibeat.enqueue_load_batch(overlap_loads, {0, 1}) ||
                !multibeat.issue_load(overlap_loads[1], 512)) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-healthy-issue case="
                          << test_case.name << " keyword=" << keyword
                          << " reason=" << multibeat.error() << '\n';
                return 1;
            }
            for (unsigned cycle = 0;
                 cycle < 128 && multibeat.tilelink_requests() < 2; ++cycle) {
                if (!multibeat.run_cycles(1) ||
                    multibeat.dcache_grant_data_beats() != 1) {
                    break;
                }
            }
            if (!multibeat.ok() || multibeat.tilelink_requests() != 2 ||
                multibeat.dcache_grant_data_beats() != 1 ||
                multibeat.dcache_max_outstanding_requests() < 2) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-overlap-window case="
                          << test_case.name << " keyword=" << keyword
                          << " requests=" << multibeat.tilelink_requests()
                          << " grant_beats="
                          << multibeat.dcache_grant_data_beats()
                          << " max_outstanding="
                          << multibeat.dcache_max_outstanding_requests()
                          << " reason=" << multibeat.error() << '\n';
                return 1;
            }
            if (!multibeat.issue_load(overlap_loads[0], 512) ||
                multibeat.dcache_grant_data_beats() != 1) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-merge-issue case="
                          << test_case.name << " keyword=" << keyword
                          << " grant_beats="
                          << multibeat.dcache_grant_data_beats()
                          << " reason=" << multibeat.error() << '\n';
                return 1;
            }
            if (!multibeat.run_until_complete(4096)) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-overlap-complete case="
                          << test_case.name << " keyword=" << keyword
                          << " reason=" << multibeat.error() << '\n';
                return 1;
            }
            for (unsigned cycle = 0;
                 cycle < 1024 &&
                     (!multibeat.dcache_responses_idle() ||
                      !multibeat.dcache_grants_drained()); ++cycle) {
                if (!multibeat.run_cycles(1)) {
                    break;
                }
            }
            const std::uint64_t expected_denied_beats =
                test_case.denied ? 2 : 0;
            const std::uint64_t expected_corrupt_beats =
                test_case.denied ? 2 : 1;
            if (!multibeat.ok() || !multibeat.dcache_responses_idle() ||
                !multibeat.dcache_grants_drained() ||
                multibeat.tilelink_requests() != 2 ||
                multibeat.dcache_refills() != 2 ||
                multibeat.dcache_grant_data_beats() != 4 ||
                multibeat.dcache_grant_acks() != 2 ||
                multibeat.dcache_denied_d_beats() != expected_denied_beats ||
                multibeat.dcache_corrupt_d_beats() !=
                    expected_corrupt_beats ||
                multibeat.dcache_response_delays() < 288 ||
                multibeat.writebacks() != 3) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-overlap-drain case="
                          << test_case.name << " keyword=" << keyword
                          << " responses_idle="
                          << multibeat.dcache_responses_idle()
                          << " grant_acks="
                          << multibeat.dcache_grant_acks()
                          << " grant_beats="
                          << multibeat.dcache_grant_data_beats()
                          << " denied_beats="
                          << multibeat.dcache_denied_d_beats()
                          << " corrupt_beats="
                          << multibeat.dcache_corrupt_d_beats()
                          << " response_delays="
                          << multibeat.dcache_response_delays()
                          << " writebacks=" << multibeat.writebacks()
                          << " reason=" << multibeat.error() << '\n';
                return 1;
            }

            if (!multibeat.redirect_after(
                    primary.rob, primary.rob_flag, true) ||
                !multibeat.run_cycles(64) ||
                !multibeat.run_until_lq_retired(1024)) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-overlap-retire case="
                          << test_case.name << " keyword=" << keyword
                          << " lq=" << multibeat.lq_allocated() << '/'
                          << multibeat.lq_dequeued() << '+'
                          << multibeat.lq_canceled() << " reason="
                          << multibeat.error() << '\n';
                return 1;
            }

            const memblock::LoadTransaction healthy_hit{
                .address = healthy_address,
                .op = memblock::LoadOp::ld,
                .rob = 8,
                .lq = 3,
                .pdest = 183,
                .lane = 0,
                .expected_debug_is_mmio = false,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            };
            const memblock::LoadTransaction poisoned_hit{
                .address = primary_address,
                .op = memblock::LoadOp::ld,
                .rob = 9,
                .lq = 4,
                .pdest = 184,
                .lane = 1,
                .expected_exception_mask = poison_exception,
                .expected_debug_is_mmio = false,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            };
            const std::uint64_t requests_before_hits =
                multibeat.tilelink_requests();
            multibeat.expect_load(healthy_hit);
            if (!multibeat.set_rob_head(
                    healthy_hit.rob, healthy_hit.rob_flag) ||
                !multibeat.enqueue_load(healthy_hit) ||
                !multibeat.issue_load(healthy_hit, 512) ||
                !multibeat.run_until_complete(2048) ||
                !multibeat.run_until_lq_retired(1024) ||
                multibeat.tilelink_requests() != requests_before_hits) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-healthy-hit case="
                          << test_case.name << " keyword=" << keyword
                          << " requests=" << multibeat.tilelink_requests()
                          << " expected_requests=" << requests_before_hits
                          << " reason=" << multibeat.error() << '\n';
                return 1;
            }
            multibeat.expect_load(poisoned_hit);
            if (!multibeat.set_rob_head(
                    poisoned_hit.rob, poisoned_hit.rob_flag) ||
                !multibeat.enqueue_load(poisoned_hit) ||
                !multibeat.issue_load(poisoned_hit, 512) ||
                !multibeat.run_until_complete(2048) ||
                multibeat.tilelink_requests() != requests_before_hits ||
                !multibeat.redirect_after(
                    poisoned_hit.rob, poisoned_hit.rob_flag, true) ||
                !multibeat.run_cycles(96) ||
                !multibeat.run_until_lq_retired(1024)) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-poisoned-hit case="
                          << test_case.name << " keyword=" << keyword
                          << " requests=" << multibeat.tilelink_requests()
                          << " expected_requests=" << requests_before_hits
                          << " lq=" << multibeat.lq_allocated() << '/'
                          << multibeat.lq_dequeued() << '+'
                          << multibeat.lq_canceled() << " reason="
                          << multibeat.error() << '\n';
                return 1;
            }

            std::uint64_t wakeups = 0;
            std::uint64_t cancels = 0;
            const auto &feedback =
                multibeat.scalar_load_feedback_stats();
            for (unsigned lane = 0;
                 lane < memblock::kScalarLoadLanes; ++lane) {
                wakeups += feedback.wakeups[lane];
                cancels += feedback.ld2_cancels[lane];
            }
            const std::uint64_t expected_clean_loads = primary_faults ? 2 : 3;
            // A current D-beat error is visible in stage 2 and cancels its
            // speculative wakeup.  Errors learned only from installed line
            // metadata arrive through LoadPipe's delayed stage-3 path: their
            // terminal writeback suppresses rfWen, but ld2Cancel cannot cover
            // that already-issued wakeup in the non-accurate-error build.
            constexpr std::uint64_t late_error_wakeups = 2;
            const std::uint64_t expected_uncanceled_wakeups =
                expected_clean_loads + late_error_wakeups;
            if (wakeups < cancels ||
                wakeups - cancels != expected_uncanceled_wakeups ||
                multibeat.writebacks() != 5 ||
                multibeat.lq_allocated() != 5 ||
                multibeat.lq_allocated() !=
                    multibeat.lq_dequeued() + multibeat.lq_canceled()) {
                std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                          << multibeat.cycle()
                          << " phase=multibeat-conservation case="
                          << test_case.name << " keyword=" << keyword
                          << " wakeups=" << wakeups
                          << " cancels=" << cancels
                          << " lane_wakeups=" << feedback.wakeups[0] << ','
                          << feedback.wakeups[1] << ','
                          << feedback.wakeups[2]
                          << " lane_cancels=" << feedback.ld2_cancels[0]
                          << ',' << feedback.ld2_cancels[1] << ','
                          << feedback.ld2_cancels[2]
                          << " expected_clean_loads="
                          << expected_clean_loads
                          << " expected_uncanceled_wakeups="
                          << expected_uncanceled_wakeups
                          << " writebacks=" << multibeat.writebacks()
                          << " lq=" << multibeat.lq_allocated() << '/'
                          << multibeat.lq_dequeued() << '+'
                          << multibeat.lq_canceled() << '\n';
                return 1;
            }

            ++multibeat_cases;
            multibeat_denied_cases += test_case.denied;
            multibeat_corrupt_first_cases +=
                test_case.corrupt_beat ==
                memblock::DcacheCorruptBeat::first;
            multibeat_corrupt_last_cases +=
                test_case.corrupt_beat ==
                memblock::DcacheCorruptBeat::last;
            multibeat_keyword_cases += keyword;
            multibeat_nonkeyword_cases += !keyword;
            ++multibeat_concurrent_mshr_cases;
            ++multibeat_poisoned_hit_cases;
            ++multibeat_healthy_hit_cases;
            multibeat_cycles += multibeat.cycle();
        }
    }
    if (multibeat_cases != 6 || multibeat_denied_cases != 2 ||
        multibeat_corrupt_first_cases != 2 ||
        multibeat_corrupt_last_cases != 2 ||
        multibeat_keyword_cases != 3 || multibeat_nonkeyword_cases != 3 ||
        multibeat_concurrent_mshr_cases != 6 ||
        multibeat_poisoned_hit_cases != 6 ||
        multibeat_healthy_hit_cases != 6) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL phase=multibeat-cardinality"
                  << " cases=" << multibeat_cases
                  << " denied=" << multibeat_denied_cases
                  << " corrupt_first=" << multibeat_corrupt_first_cases
                  << " corrupt_last=" << multibeat_corrupt_last_cases
                  << " keyword=" << multibeat_keyword_cases
                  << " nonkeyword=" << multibeat_nonkeyword_cases
                  << " concurrent_mshr="
                  << multibeat_concurrent_mshr_cases
                  << " poisoned_hits=" << multibeat_poisoned_hit_cases
                  << " healthy_hits=" << multibeat_healthy_hit_cases << '\n';
        return 1;
    }

    memblock::Environment ecc_environment(argc, argv);
    constexpr std::uint64_t ecc_base =
        memblock::kDefaultMemoryBase + 0x2d000;
    constexpr std::uint64_t dcache_ctrl_base = 0x38022000ULL;
    constexpr std::uint64_t dcache_ctrl_mask_bank0 =
        dcache_ctrl_base + 0x10;
    constexpr std::uint64_t tag_error_once = 0x11;
    constexpr std::uint64_t data_error_once = 0x19;
    constexpr unsigned dcache_banks = 8;
    constexpr std::array<std::uint64_t, dcache_banks> data_error_masks{{
        1ULL << 0, 1ULL << 1, 1ULL << 7, 1ULL << 15,
        1ULL << 31, 1ULL << 32, 1ULL << 47, 1ULL << 63,
    }};
    constexpr std::array<std::uint64_t, dcache_banks>
        data_double_error_masks{{
            (1ULL << 0) | (1ULL << 1),
            (1ULL << 1) | (1ULL << 2),
            (1ULL << 7) | (1ULL << 8),
            (1ULL << 15) | (1ULL << 16),
            (1ULL << 30) | (1ULL << 31),
            (1ULL << 32) | (1ULL << 33),
            (1ULL << 47) | (1ULL << 48),
            (1ULL << 62) | (1ULL << 63),
        }};
    constexpr std::uint64_t tag_address = ecc_base;
    constexpr std::uint64_t data_address = ecc_base + 0x40;
    ecc_environment.memory().fill_incrementing(ecc_base, 0x100, 0x9d);
    if (!ecc_environment.reset()) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                  << ecc_environment.cycle()
                  << " phase=ecc-reset reason=" << ecc_environment.error()
                  << '\n';
        return 1;
    }
    ecc_environment.configure_cache_error_enable(true);
    if (!ecc_environment.run_cycles(8)) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                  << ecc_environment.cycle()
                  << " phase=ecc-enable reason=" << ecc_environment.error()
                  << '\n';
        return 1;
    }

    auto run_clean_load = [&](std::uint64_t address, std::uint8_t rob,
                              std::uint8_t lq, std::uint8_t pdest,
                              unsigned lane, const char *phase,
                              std::uint64_t expected_data_xor = 0) {
        const memblock::LoadTransaction load{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = rob,
            .lq = lq,
            .pdest = pdest,
            .lane = lane,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        ecc_environment.expect_load_data(
            load,
            ecc_environment.bus_expected_load(address, memblock::LoadOp::ld) ^
                expected_data_xor);
        if (!ecc_environment.set_rob_head(load.rob, load.rob_flag) ||
            !ecc_environment.enqueue_load(load) ||
            !ecc_environment.issue_load(load, 512) ||
            !ecc_environment.run_until_complete(4096)) {
            std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                      << ecc_environment.cycle() << " phase=" << phase
                      << " reason=" << ecc_environment.error() << '\n';
            return false;
        }
        for (unsigned cycle = 0;
             cycle < 1024 && !ecc_environment.dcache_grants_drained();
             ++cycle) {
            if (!ecc_environment.run_cycles(1)) {
                return false;
            }
        }
        if (!ecc_environment.dcache_grants_drained() ||
            !ecc_environment.run_cycles(16) ||
            !ecc_environment.run_until_lq_retired(1024)) {
            std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                      << ecc_environment.cycle() << " phase=" << phase
                      << " reason=load did not drain refill/LQ"
                      << " grant_acks=" << ecc_environment.dcache_grant_acks()
                      << " data_beats="
                      << ecc_environment.dcache_grant_data_beats()
                      << " lq_allocated=" << ecc_environment.lq_allocated()
                      << " lq_dequeued=" << ecc_environment.lq_dequeued()
                      << " lq_canceled=" << ecc_environment.lq_canceled()
                      << '\n';
            return false;
        }
        return true;
    };

    auto run_clean_hit = [&](std::uint64_t address, std::uint8_t rob,
                             std::uint8_t lq, std::uint8_t pdest,
                             unsigned lane, const char *phase) {
        const std::uint64_t requests_before =
            ecc_environment.tilelink_requests();
        if (!run_clean_load(address, rob, lq, pdest, lane, phase)) {
            return false;
        }
        if (ecc_environment.tilelink_requests() != requests_before) {
            std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                      << ecc_environment.cycle() << " phase=" << phase
                      << " requests_before=" << requests_before
                      << " requests_after="
                      << ecc_environment.tilelink_requests()
                      << " reason=ECC target was not resident in DCache\n";
            return false;
        }
        return true;
    };

    if (!run_clean_load(tag_address, 40, 0, 40, 0, "tag-warm") ||
        !run_clean_load(data_address, 41, 1, 41, 1, "data-warm") ||
        !run_clean_hit(tag_address, 42, 2, 42, 0, "tag-warm-hit") ||
        !run_clean_hit(data_address, 43, 3, 43, 1, "data-warm-hit")) {
        return 1;
    }

    auto write_dcache_ctrl = [&](std::uint64_t address, std::uint64_t data,
                                 std::uint8_t rob, std::uint8_t sq,
                                 const char *phase) {
        const memblock::StoreTransaction store{
            .address = address,
            .oracle_address = address,
            .data = data,
            .op = memblock::StoreOp::sd,
            .rob = rob,
            .sq = sq,
            .address_lane = static_cast<unsigned>(sq & 1U),
            .data_lane = static_cast<unsigned>((sq + 1U) & 1U),
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t dcache_before =
            ecc_environment.tilelink_requests();
        const std::uint64_t uncache_before =
            ecc_environment.uncache_requests();
        ecc_environment.expect_store(store);
        if (!ecc_environment.set_rob_head(store.rob, store.rob_flag) ||
            !ecc_environment.enqueue_store(store, 0) ||
            !ecc_environment.issue_store_address(store, 512) ||
            !ecc_environment.issue_store_data(store, 512) ||
            !ecc_environment.run_until_store_complete_with_replay(
                store, 8192, true) ||
            !ecc_environment.commit_stores_through(store, 1) ||
            !ecc_environment.run_cycles(16) ||
            ecc_environment.tilelink_requests() != dcache_before ||
            ecc_environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                      << ecc_environment.cycle() << " phase=" << phase
                      << " dcache_before=" << dcache_before
                      << " dcache_after="
                      << ecc_environment.tilelink_requests()
                      << " uncache_before=" << uncache_before
                      << " uncache_after="
                      << ecc_environment.uncache_requests()
                      << " pending_stores="
                      << ecc_environment.pending_scalar_stores()
                      << " store_writebacks="
                      << ecc_environment.store_writebacks()
                      << " sq_allocated=" << ecc_environment.sq_allocated()
                      << " sq_dequeued=" << ecc_environment.sq_dequeued()
                      << " sq_canceled=" << ecc_environment.sq_canceled()
                      << " store_mmio_valid="
                      << ecc_environment.store_mmio_valid()
                      << " store_mmio_rob="
                      << static_cast<unsigned>(
                             ecc_environment.store_mmio_rob())
                      << " store_tlb_feedbacks="
                      << ecc_environment.store_tlb_feedbacks()
                      << " store_tlb_misses="
                      << ecc_environment.store_tlb_misses()
                      << " reason=" << ecc_environment.error() << '\n';
            return false;
        }
        ecc_environment.record_committed_store(store);
        return true;
    };

    const auto ecc_feedback_totals = [&ecc_environment]() {
        const auto &feedback = ecc_environment.scalar_load_feedback_stats();
        std::pair<std::uint64_t, std::uint64_t> totals{};
        for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
            totals.first += feedback.wakeups[lane];
            totals.second += feedback.ld2_cancels[lane];
        }
        return totals;
    };
    std::uint64_t physical_ecc_wakeups = 0;
    std::uint64_t physical_ecc_cancels = 0;
    std::uint64_t physical_ecc_reports = 0;
    unsigned tag_ecc_cases = 0;
    unsigned data_ecc_cases = 0;
    unsigned concurrent_ecc_cases = 0;
    auto run_tag_ecc_error = [&](std::uint64_t address, std::uint64_t control,
                                 std::uint8_t control_rob,
                                 std::uint8_t control_sq, std::uint8_t load_rob,
                                 std::uint8_t load_lq, std::uint8_t pdest,
                                 unsigned lane, const char *phase) {
        if (!write_dcache_ctrl(
                dcache_ctrl_base, control, control_rob, control_sq,
                phase)) {
            return false;
        }
        const auto feedback_before = ecc_feedback_totals();
        const auto errors_before = ecc_environment.bus_error_stats();
        const std::uint64_t requests_before =
            ecc_environment.tilelink_requests();
        const std::uint64_t writebacks_before = ecc_environment.writebacks();
        const memblock::LoadTransaction load{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = load_rob,
            .lq = load_lq,
            .pdest = pdest,
            .lane = lane,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        if (!ecc_environment.set_rob_head(load.rob, load.rob_flag) ||
            !ecc_environment.enqueue_load(load) ||
            !ecc_environment.issue_load(load, 512)) {
            std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                      << ecc_environment.cycle() << " phase=" << phase
                      << "-load reason=" << ecc_environment.error() << '\n';
            return false;
        }
        bool observed_cancel = false;
        for (unsigned cycle = 0; cycle < 128; ++cycle) {
            const auto feedback = ecc_feedback_totals();
            if (feedback.second > feedback_before.second) {
                observed_cancel = true;
                break;
            }
            if (!ecc_environment.run_cycles(1)) {
                break;
            }
        }
        if (observed_cancel &&
            (!ecc_environment.redirect_after(
                 load.rob, load.rob_flag, true) ||
             !ecc_environment.run_cycles(96))) {
            observed_cancel = false;
        }
        const auto feedback_after = ecc_feedback_totals();
        const auto errors_after = ecc_environment.bus_error_stats();
        const std::uint64_t wakeups =
            feedback_after.first - feedback_before.first;
        const std::uint64_t cancels =
            feedback_after.second - feedback_before.second;
        const bool lq_retired =
            ecc_environment.lq_dequeued() + ecc_environment.lq_canceled() >=
                ecc_environment.lq_allocated() ||
            ecc_environment.account_lq_cancellation(1);
        if (!observed_cancel || wakeups == 0 || cancels == 0 ||
            cancels > wakeups ||
            errors_after.dcache_reports != errors_before.dcache_reports + 1 ||
            errors_after.last_dcache_address != address ||
            ecc_environment.tilelink_requests() != requests_before ||
            ecc_environment.writebacks() != writebacks_before ||
            !lq_retired) {
            std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                      << ecc_environment.cycle() << " phase=" << phase
                      << "-classification wakeups=" << wakeups
                      << " cancels=" << cancels
                      << " error_reports_before="
                      << errors_before.dcache_reports
                      << " error_reports_after="
                      << errors_after.dcache_reports
                      << " error_address=0x" << std::hex
                      << errors_after.last_dcache_address
                      << " expected_address=0x" << address << std::dec
                      << " requests_before=" << requests_before
                      << " requests_after="
                      << ecc_environment.tilelink_requests()
                      << " writebacks_before=" << writebacks_before
                      << " writebacks_after="
                      << ecc_environment.writebacks()
                      << " reason=" << ecc_environment.error() << '\n';
            return false;
        }
        physical_ecc_wakeups += wakeups;
        physical_ecc_cancels += cancels;
        physical_ecc_reports +=
            errors_after.dcache_reports - errors_before.dcache_reports;
        return true;
    };

    auto run_data_ecc_error = [&](std::uint64_t address,
                                  std::uint64_t control,
                                  std::uint64_t expected_data_xor,
                                  std::uint8_t control_rob,
                                  std::uint8_t control_sq,
                                  std::uint8_t load_rob,
                                  std::uint8_t load_lq,
                                  std::uint8_t pdest,
                                  unsigned lane, const char *phase) {
        if (!write_dcache_ctrl(
                dcache_ctrl_base, control, control_rob, control_sq,
                phase)) {
            return false;
        }
        const auto feedback_before = ecc_feedback_totals();
        const auto errors_before = ecc_environment.bus_error_stats();
        const std::uint64_t requests_before =
            ecc_environment.tilelink_requests();
        const std::uint64_t writebacks_before = ecc_environment.writebacks();
        if (!run_clean_load(
                address, load_rob, load_lq, pdest, lane, phase,
                expected_data_xor)) {
            return false;
        }
        const auto feedback_after = ecc_feedback_totals();
        const auto errors_after = ecc_environment.bus_error_stats();
        const std::uint64_t wakeups =
            feedback_after.first - feedback_before.first;
        const std::uint64_t cancels =
            feedback_after.second - feedback_before.second;
        const std::uint64_t reports =
            errors_after.dcache_reports - errors_before.dcache_reports;
        if (wakeups == 0 || cancels != 0 || reports != 1 ||
            errors_after.last_dcache_address != address ||
            ecc_environment.tilelink_requests() != requests_before ||
            ecc_environment.writebacks() != writebacks_before + 1) {
            std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                      << ecc_environment.cycle() << " phase=" << phase
                      << "-classification wakeups=" << wakeups
                      << " cancels=" << cancels
                      << " error_reports=" << reports
                      << " error_address=0x" << std::hex
                      << errors_after.last_dcache_address
                      << " expected_address=0x" << address << std::dec
                      << " requests_before=" << requests_before
                      << " requests_after="
                      << ecc_environment.tilelink_requests()
                      << " writebacks_before=" << writebacks_before
                      << " writebacks_after=" << ecc_environment.writebacks()
                      << " reason=" << ecc_environment.error() << '\n';
            return false;
        }
        physical_ecc_wakeups += wakeups;
        physical_ecc_cancels += cancels;
        physical_ecc_reports += reports;
        return true;
    };

    if (!write_dcache_ctrl(
            dcache_ctrl_mask_bank0, 1, 44, 0,
            "tag-ecc-mask") ||
        !run_tag_ecc_error(
            tag_address, tag_error_once, 45, 1, 46, 4, 44, 0,
            "tag-ecc") ||
        !write_dcache_ctrl(dcache_ctrl_base, 0, 47, 2, "tag-disable") ||
        !run_clean_load(tag_address, 48, 4, 45, 1, "tag-survivor")) {
        return 1;
    }
    ++tag_ecc_cases;
    if (!run_clean_hit(
            tag_address, 49, 5, 46, 2, "tag-double-resident") ||
        !write_dcache_ctrl(
            dcache_ctrl_mask_bank0, 3, 50, 3, "tag-double-mask") ||
        !run_tag_ecc_error(
            tag_address, tag_error_once, 51, 4, 52, 6, 47, 1,
            "tag-double-ecc") ||
        !write_dcache_ctrl(
            dcache_ctrl_base, 0, 53, 5, "tag-double-disable") ||
        !run_clean_load(
            tag_address, 54, 6, 48, 0, "tag-double-survivor")) {
        return 1;
    }
    ++tag_ecc_cases;
    auto data_control_for_bank = [](unsigned bank) {
        return std::uint64_t{0x9} | (std::uint64_t{1} << (4 + bank));
    };
    if (data_control_for_bank(0) != data_error_once) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL phase=ecc-control-encoding\n";
        return 1;
    }
    std::uint8_t next_rob = 55;
    std::uint8_t next_lq = 7;
    std::uint8_t next_sq = 6;
    for (unsigned error_class = 0; error_class < 2; ++error_class) {
        for (unsigned bank = 0; bank < dcache_banks; ++bank) {
            const std::uint64_t address = data_address + bank * 8;
            const std::uint64_t error_mask = error_class == 0
                ? data_error_masks[bank]
                : data_double_error_masks[bank];
            const std::string prefix = std::string("data-ecc-") +
                (error_class == 0 ? "single-bank" : "double-bank") +
                std::to_string(bank);
            const std::string resident_phase = prefix + "-resident";
            const std::string mask_phase = prefix + "-mask";
            const std::string error_phase = prefix + "-error";
            const std::string disable_phase = prefix + "-disable";
            const std::string survivor_phase = prefix + "-survivor";
            const std::uint8_t resident_rob = next_rob++;
            const std::uint8_t resident_lq = next_lq++;
            const std::uint8_t mask_rob = next_rob++;
            const std::uint8_t mask_sq = next_sq++;
            const std::uint8_t control_rob = next_rob++;
            const std::uint8_t control_sq = next_sq++;
            const std::uint8_t error_rob = next_rob++;
            const std::uint8_t error_lq = next_lq++;
            const std::uint8_t disable_rob = next_rob++;
            const std::uint8_t disable_sq = next_sq++;
            const std::uint8_t survivor_rob = next_rob++;
            const std::uint8_t survivor_lq = next_lq++;
            if (!run_clean_hit(
                    address, resident_rob, resident_lq,
                    static_cast<std::uint8_t>(48 + bank * 3),
                    bank % memblock::kScalarLoadLanes,
                    resident_phase.c_str()) ||
                !write_dcache_ctrl(
                    dcache_ctrl_mask_bank0 + bank * 8,
                    error_mask, mask_rob, mask_sq,
                    mask_phase.c_str()) ||
                !run_data_ecc_error(
                    address, data_control_for_bank(bank), error_mask,
                    control_rob, control_sq, error_rob, error_lq,
                    static_cast<std::uint8_t>(50 + bank * 3),
                    bank % memblock::kScalarLoadLanes,
                    error_phase.c_str()) ||
                !write_dcache_ctrl(
                    dcache_ctrl_base, 0, disable_rob, disable_sq,
                    disable_phase.c_str()) ||
                !run_clean_load(
                    address, survivor_rob, survivor_lq,
                    static_cast<std::uint8_t>(51 + bank * 3),
                    (bank + 1) % memblock::kScalarLoadLanes,
                    survivor_phase.c_str())) {
                return 1;
            }
            ++data_ecc_cases;
        }
    }

    constexpr unsigned concurrent_target_bank = 2;
    constexpr unsigned concurrent_companion_bank = 5;
    constexpr std::uint64_t concurrent_error_mask = 1ULL << 23;
    const std::uint64_t concurrent_target_address =
        data_address + concurrent_target_bank * 8;
    const std::uint64_t concurrent_companion_address =
        data_address + concurrent_companion_bank * 8;
    if (!write_dcache_ctrl(
            dcache_ctrl_mask_bank0 + concurrent_target_bank * 8,
            concurrent_error_mask, 151, 54, "data-ecc-concurrent-mask") ||
        !write_dcache_ctrl(
            dcache_ctrl_base,
            data_control_for_bank(concurrent_target_bank), 152, 55,
            "data-ecc-concurrent-control")) {
        return 1;
    }
    const std::vector<memblock::LoadTransaction> concurrent_loads{
        {
            .address = concurrent_target_address,
            .op = memblock::LoadOp::ld,
            .rob = 153,
            .lq = 55,
            .pdest = 120,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        },
        {
            .address = concurrent_companion_address,
            .op = memblock::LoadOp::ld,
            .rob = 154,
            .lq = 56,
            .pdest = 121,
            .lane = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        },
    };
    ecc_environment.expect_load_data(
        concurrent_loads[0],
        ecc_environment.bus_expected_load(
            concurrent_target_address, memblock::LoadOp::ld) ^
            concurrent_error_mask);
    ecc_environment.expect_load_data(
        concurrent_loads[1],
        ecc_environment.bus_expected_load(
            concurrent_companion_address, memblock::LoadOp::ld));
    const auto concurrent_feedback_before =
        ecc_environment.scalar_load_feedback_stats();
    const auto concurrent_errors_before = ecc_environment.bus_error_stats();
    const std::uint64_t concurrent_requests_before =
        ecc_environment.tilelink_requests();
    const std::uint64_t concurrent_writebacks_before =
        ecc_environment.writebacks();
    if (!ecc_environment.set_rob_head(
            concurrent_loads[0].rob, concurrent_loads[0].rob_flag) ||
        !ecc_environment.enqueue_load_batch(concurrent_loads, {0, 1}) ||
        !ecc_environment.issue_load_batch(concurrent_loads, 512, true) ||
        !ecc_environment.run_until_complete(4096) ||
        !ecc_environment.run_cycles(16) ||
        !ecc_environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                  << ecc_environment.cycle()
                  << " phase=data-ecc-concurrent-execution reason="
                  << ecc_environment.error() << '\n';
        return 1;
    }
    const auto concurrent_feedback_after =
        ecc_environment.scalar_load_feedback_stats();
    const auto concurrent_errors_after = ecc_environment.bus_error_stats();
    std::uint64_t concurrent_wakeups = 0;
    std::uint64_t concurrent_cancels = 0;
    for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
        concurrent_wakeups += concurrent_feedback_after.wakeups[lane] -
            concurrent_feedback_before.wakeups[lane];
        concurrent_cancels += concurrent_feedback_after.ld2_cancels[lane] -
            concurrent_feedback_before.ld2_cancels[lane];
    }
    const bool both_load_lanes_woke =
        concurrent_feedback_after.wakeups[0] >
            concurrent_feedback_before.wakeups[0] &&
        concurrent_feedback_after.wakeups[1] >
            concurrent_feedback_before.wakeups[1];
    if (!both_load_lanes_woke || concurrent_cancels != 0 ||
        concurrent_errors_after.dcache_reports !=
            concurrent_errors_before.dcache_reports + 1 ||
        concurrent_errors_after.last_dcache_address !=
            concurrent_target_address ||
        ecc_environment.tilelink_requests() != concurrent_requests_before ||
        ecc_environment.writebacks() != concurrent_writebacks_before + 2) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                  << ecc_environment.cycle()
                  << " phase=data-ecc-concurrent-classification"
                  << " wakeups=" << concurrent_wakeups
                  << " cancels=" << concurrent_cancels
                  << " reports_before="
                  << concurrent_errors_before.dcache_reports
                  << " reports_after="
                  << concurrent_errors_after.dcache_reports
                  << " error_address=0x" << std::hex
                  << concurrent_errors_after.last_dcache_address
                  << " expected_address=0x" << concurrent_target_address
                  << std::dec
                  << " requests_before=" << concurrent_requests_before
                  << " requests_after="
                  << ecc_environment.tilelink_requests()
                  << " writebacks_before=" << concurrent_writebacks_before
                  << " writebacks_after=" << ecc_environment.writebacks()
                  << '\n';
        return 1;
    }
    physical_ecc_wakeups += concurrent_wakeups;
    physical_ecc_reports += 1;
    ++data_ecc_cases;
    ++concurrent_ecc_cases;
    if (!run_clean_load(
            concurrent_target_address, 155, 57, 122, 2,
            "data-ecc-concurrent-target-survivor") ||
        !run_clean_load(
            concurrent_companion_address, 156, 58, 123, 0,
            "data-ecc-concurrent-companion-survivor")) {
        return 1;
    }
    if (physical_ecc_reports != data_ecc_cases + tag_ecc_cases) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                  << ecc_environment.cycle()
                  << " phase=ecc-report-conservation"
                  << " reports=" << physical_ecc_reports
                  << " expected=" << data_ecc_cases + tag_ecc_cases << '\n';
        return 1;
    }
    if (ecc_environment.lq_allocated() !=
            ecc_environment.lq_dequeued() +
                ecc_environment.lq_canceled() ||
        ecc_environment.sq_allocated() !=
            ecc_environment.sq_dequeued() +
                ecc_environment.sq_canceled()) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle="
                  << ecc_environment.cycle()
                  << " phase=ecc-queue-conservation"
                  << " lq_allocated=" << ecc_environment.lq_allocated()
                  << " lq_dequeued=" << ecc_environment.lq_dequeued()
                  << " lq_canceled=" << ecc_environment.lq_canceled()
                  << " sq_allocated=" << ecc_environment.sq_allocated()
                  << " sq_dequeued=" << ecc_environment.sq_dequeued()
                  << " sq_canceled=" << ecc_environment.sq_canceled()
                  << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_DCACHE_ERRORS_PASS"
              << " cycle=" << environment.cycle()
              << " denied=1 corrupt=1"
              << " denied_wakeups=" << denied_feedback.first
              << " denied_cancels=" << denied_feedback.second
              << " corrupt_wakeups=" << corrupt_wakeups
              << " corrupt_cancels=" << corrupt_cancels
              << " multibeat_cases=" << multibeat_cases
              << " multibeat_denied=" << multibeat_denied_cases
              << " multibeat_corrupt_first="
              << multibeat_corrupt_first_cases
              << " multibeat_corrupt_last="
              << multibeat_corrupt_last_cases
              << " multibeat_keyword=" << multibeat_keyword_cases
              << " multibeat_nonkeyword=" << multibeat_nonkeyword_cases
              << " multibeat_concurrent_mshr="
              << multibeat_concurrent_mshr_cases
              << " multibeat_poisoned_hits="
              << multibeat_poisoned_hit_cases
              << " multibeat_healthy_hits="
              << multibeat_healthy_hit_cases
              << " multibeat_cycles=" << multibeat_cycles
              << " tag_ecc=" << tag_ecc_cases
              << " data_ecc=" << data_ecc_cases
              << " concurrent_ecc=" << concurrent_ecc_cases
              << " ecc_error_reports="
              << physical_ecc_reports
              << " ecc_wakeups=" << physical_ecc_wakeups
              << " ecc_cancels=" << physical_ecc_cancels
              << " ecc_lq_allocated=" << ecc_environment.lq_allocated()
              << " ecc_lq_canceled=" << ecc_environment.lq_canceled()
              << " ecc_sq_allocated=" << ecc_environment.sq_allocated()
              << " ecc_cycles=" << ecc_environment.cycle()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_atomic_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t address = memblock::kDefaultMemoryBase + 0x2e000;
    environment.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x2d);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::uint64_t model_value = environment.memory().expected_load(
        address, memblock::LoadOp::ld);
    const std::array<std::pair<memblock::AtomicOp, std::uint64_t>, 9> operations{{
        {memblock::AtomicOp::amoadd_d, 0x1020304050607080ULL},
        {memblock::AtomicOp::amoxor_d, 0x00ff00ff00ff00ffULL},
        {memblock::AtomicOp::amoand_d, 0xf0ffffffffffffffULL},
        {memblock::AtomicOp::amoor_d, 0x0000000000001200ULL},
        {memblock::AtomicOp::amoswap_d, 0x8899aabbccddeeffULL},
        {memblock::AtomicOp::amomin_d, 0x7fffffffffffffffULL},
        {memblock::AtomicOp::amomax_d, 0x8000000000000000ULL},
        {memblock::AtomicOp::amominu_d, 0x0000000000000011ULL},
        {memblock::AtomicOp::amomaxu_d, 0xfffffffffffffff0ULL},
    }};
    for (std::size_t index = 0; index < operations.size(); ++index) {
        const auto [op, operand] = operations[index];
        const std::uint8_t atomic_rob = static_cast<std::uint8_t>(index * 2);
        const memblock::AtomicTransaction atomic{
            .address = address,
            .op = op,
            .data = operand,
            .rob = atomic_rob,
            .pdest = static_cast<std::uint8_t>(62 + index),
            .address_lane = static_cast<unsigned>(index & 1U),
            .data_lane = static_cast<unsigned>(index & 1U),
        };
        const memblock::LoadTransaction old_value_wb{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
            .lane = 0,
            .rf_wen = true,
        };
        environment.expect_load_data(old_value_wb, model_value);
        if (!environment.issue_atomic(atomic, 1024) ||
            !environment.run_until_complete(8192)) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=amo-writeback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }

        switch (op) {
        case memblock::AtomicOp::amoadd_d:
            model_value += operand;
            break;
        case memblock::AtomicOp::amoxor_d:
            model_value ^= operand;
            break;
        case memblock::AtomicOp::amoand_d:
            model_value &= operand;
            break;
        case memblock::AtomicOp::amoor_d:
            model_value |= operand;
            break;
        case memblock::AtomicOp::amoswap_d:
            model_value = operand;
            break;
        case memblock::AtomicOp::amomin_d:
            model_value = static_cast<std::int64_t>(model_value) <
                    static_cast<std::int64_t>(operand)
                ? model_value
                : operand;
            break;
        case memblock::AtomicOp::amomax_d:
            model_value = static_cast<std::int64_t>(model_value) >
                    static_cast<std::int64_t>(operand)
                ? model_value
                : operand;
            break;
        case memblock::AtomicOp::amominu_d:
            model_value = std::min(model_value, operand);
            break;
        case memblock::AtomicOp::amomaxu_d:
            model_value = std::max(model_value, operand);
            break;
        default:
            break;
        }

        const memblock::LoadTransaction readback{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(atomic.rob + 1),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(80 + index),
            .lane = 1,
        };
        environment.expect_load_data(readback, model_value);
        if (!environment.set_rob_head(readback.rob) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 1024) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(1024)) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=amo-readback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }

    const std::uint64_t word_address = address + 8;

    // Atomics have no split/replay path: natural alignment is checked when
    // the DTLB response arrives.  Exercise every byte offset forbidden by
    // each operand width and require an exceptional writeback without any
    // DCache transaction or architectural register write.
    const std::uint64_t atomic_requests_before_misaligned =
        environment.tilelink_requests();
    auto check_misaligned_atomic = [&](std::uint64_t misaligned_address,
                                       memblock::AtomicOp op,
                                       std::uint64_t data,
                                       std::uint8_t rob,
                                       bool rob_flag,
                                       std::uint8_t pdest,
                                       unsigned lane) {
        const memblock::AtomicTransaction atomic{
            .address = misaligned_address,
            .op = op,
            .data = data,
            .rob = rob,
            .rob_flag = rob_flag,
            .pdest = pdest,
            .address_lane = lane,
            .data_lane = lane,
        };
        const bool is_lr = op == memblock::AtomicOp::lr_w ||
            op == memblock::AtomicOp::lr_d;
        const memblock::LoadTransaction writeback{
            .address = atomic.address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .rob_flag = atomic.rob_flag,
            .pdest = atomic.pdest,
            .lane = lane,
            .expected_exception_mask = is_lr
                ? memblock::kExceptionLoadAddressMisaligned
                : memblock::kExceptionStoreAddressMisaligned,
        };
        environment.expect_load_data(writeback, 0);
        return environment.issue_atomic(atomic, 1024) &&
            environment.run_until_complete(8192) &&
            environment.tilelink_requests() == atomic_requests_before_misaligned;
    };
    constexpr std::array<memblock::AtomicOp, 12> doubleword_atomic_ops{{
        memblock::AtomicOp::lr_d,
        memblock::AtomicOp::sc_d,
        memblock::AtomicOp::amoswap_d,
        memblock::AtomicOp::amoadd_d,
        memblock::AtomicOp::amoxor_d,
        memblock::AtomicOp::amoand_d,
        memblock::AtomicOp::amoor_d,
        memblock::AtomicOp::amomin_d,
        memblock::AtomicOp::amomax_d,
        memblock::AtomicOp::amominu_d,
        memblock::AtomicOp::amomaxu_d,
        memblock::AtomicOp::amocas_d,
    }};
    constexpr std::array<memblock::AtomicOp, 12> word_atomic_ops{{
        memblock::AtomicOp::lr_w,
        memblock::AtomicOp::sc_w,
        memblock::AtomicOp::amoswap_w,
        memblock::AtomicOp::amoadd_w,
        memblock::AtomicOp::amoxor_w,
        memblock::AtomicOp::amoand_w,
        memblock::AtomicOp::amoor_w,
        memblock::AtomicOp::amomin_w,
        memblock::AtomicOp::amomax_w,
        memblock::AtomicOp::amominu_w,
        memblock::AtomicOp::amomaxu_w,
        memblock::AtomicOp::amocas_w,
    }};
    std::uint64_t misaligned_rob_cursor = 66;
    unsigned misaligned_d_cases = 0;
    unsigned misaligned_w_cases = 0;
    unsigned misaligned_rob_wraps = 0;
    auto run_misaligned_matrix = [&](const auto &ops, std::uint64_t base,
                                     unsigned alignment,
                                     unsigned &case_count,
                                     const char *phase) {
        for (std::size_t op_index = 0; op_index < ops.size(); ++op_index) {
            for (unsigned offset = 1; offset < alignment; ++offset) {
                const std::uint64_t absolute_rob = misaligned_rob_cursor++;
                misaligned_rob_wraps += absolute_rob != 66 &&
                    absolute_rob % memblock::kRobEntries == 0;
                const unsigned lane = (op_index + offset) & 1U;
                if (!check_misaligned_atomic(
                        base + offset, ops[op_index],
                        0x1122334455667788ULL ^
                            (static_cast<std::uint64_t>(op_index) << 8) ^ offset,
                        static_cast<std::uint8_t>(absolute_rob %
                            memblock::kRobEntries),
                        ((absolute_rob / memblock::kRobEntries) & 1U) != 0,
                        static_cast<std::uint8_t>(32 + case_count), lane)) {
                    std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                              << environment.cycle() << " phase=" << phase
                              << " op=0x" << std::hex
                              << static_cast<unsigned>(ops[op_index])
                              << std::dec << " offset=" << offset
                              << " reason=" << environment.error()
                              << " dcache_requests="
                              << environment.tilelink_requests()
                              << " expected="
                              << atomic_requests_before_misaligned << '\n';
                    return false;
                }
                ++case_count;
            }
        }
        return true;
    };
    if (!run_misaligned_matrix(
            doubleword_atomic_ops, address, 8, misaligned_d_cases,
            "misaligned-d-matrix") ||
        !run_misaligned_matrix(
            word_atomic_ops, word_address, 4, misaligned_w_cases,
            "misaligned-w-matrix")) {
        return 1;
    }
    if (misaligned_d_cases != 84 || misaligned_w_cases != 36 ||
        misaligned_rob_wraps != 1) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle()
                  << " phase=misaligned-matrix-conservation"
                  << " d_cases=" << misaligned_d_cases
                  << " w_cases=" << misaligned_w_cases
                  << " rob_wraps=" << misaligned_rob_wraps << '\n';
        return 1;
    }

    // Exercise the complete W-width ALU family independently from the
    // D-width sequence.  Atomic W results are sign-extended to XLEN, while
    // only the addressed 32-bit word is updated in the cache line.
    std::uint64_t word_line_value = environment.memory().expected_load(
        word_address, memblock::LoadOp::ld);
    std::uint32_t word_value = static_cast<std::uint32_t>(
        word_line_value);
    const std::array<std::pair<memblock::AtomicOp, std::uint32_t>, 9>
        word_operations{{
            {memblock::AtomicOp::amoadd_w, 0x10203040U},
            {memblock::AtomicOp::amoxor_w, 0x00ff00ffU},
            {memblock::AtomicOp::amoand_w, 0xf0ffffffU},
            {memblock::AtomicOp::amoor_w, 0x00001200U},
            {memblock::AtomicOp::amoswap_w, 0x8899aabBU},
            {memblock::AtomicOp::amomin_w, 0x7fffffffU},
            {memblock::AtomicOp::amomax_w, 0x80000000U},
            {memblock::AtomicOp::amominu_w, 0x00000011U},
            {memblock::AtomicOp::amomaxu_w, 0xfffffff0U},
        }};
    for (std::size_t index = 0; index < word_operations.size(); ++index) {
        const auto [op, operand] = word_operations[index];
        const std::uint8_t atomic_rob = static_cast<std::uint8_t>(32 + index * 2);
        const memblock::AtomicTransaction atomic{
            .address = word_address,
            .op = op,
            .data = operand,
            .rob = atomic_rob,
            .pdest = static_cast<std::uint8_t>(102 + index),
            .address_lane = static_cast<unsigned>(index & 1U),
            .data_lane = static_cast<unsigned>(index & 1U),
        };
        const std::uint64_t old_value = memblock::sign_extend(word_value, 32);
        const memblock::LoadTransaction old_value_wb{
            .address = word_address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
            .lane = 0,
            .rf_wen = true,
        };
        environment.expect_load_data(old_value_wb, old_value);
        if (!environment.issue_atomic(atomic, 1024) ||
            !environment.run_until_complete(8192)) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=amo-w-writeback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }

        const std::int32_t signed_old = static_cast<std::int32_t>(word_value);
        const std::int32_t signed_operand = static_cast<std::int32_t>(operand);
        switch (op) {
        case memblock::AtomicOp::amoadd_w:
            word_value = static_cast<std::uint32_t>(word_value + operand);
            break;
        case memblock::AtomicOp::amoxor_w:
            word_value ^= operand;
            break;
        case memblock::AtomicOp::amoand_w:
            word_value &= operand;
            break;
        case memblock::AtomicOp::amoor_w:
            word_value |= operand;
            break;
        case memblock::AtomicOp::amoswap_w:
            word_value = operand;
            break;
        case memblock::AtomicOp::amomin_w:
            word_value = signed_old < signed_operand ? word_value : operand;
            break;
        case memblock::AtomicOp::amomax_w:
            word_value = signed_old > signed_operand ? word_value : operand;
            break;
        case memblock::AtomicOp::amominu_w:
            word_value = std::min(word_value, operand);
            break;
        case memblock::AtomicOp::amomaxu_w:
            word_value = std::max(word_value, operand);
            break;
        default:
            break;
        }
        const memblock::LoadTransaction readback{
            .address = word_address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(atomic.rob + 1),
            .lq = static_cast<std::uint8_t>(operations.size() + index),
            .pdest = static_cast<std::uint8_t>(120 + index),
            .lane = 1,
        };
        word_line_value = (word_line_value & ~std::uint64_t{0xffffffff}) |
            static_cast<std::uint64_t>(word_value);
        environment.expect_load_data(readback, word_line_value);
        if (!environment.set_rob_head(readback.rob) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 1024) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(1024)) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=amo-w-readback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }

    auto run_compare_swap = [&](std::uint64_t cas_address,
                                memblock::AtomicOp cas_op,
                                std::uint64_t &line_value,
                                std::uint64_t compare,
                                std::uint64_t swap,
                                std::uint8_t atomic_rob,
                                std::uint8_t pdest,
                                std::uint8_t lq,
                                bool word) {
        const std::uint64_t old_value = word
            ? memblock::sign_extend(static_cast<std::uint32_t>(line_value), 32)
            : line_value;
        const memblock::AtomicTransaction atomic{
            .address = cas_address,
            .op = cas_op,
            .data = swap,
            .compare = compare,
            .rob = atomic_rob,
            .pdest = pdest,
            .address_lane = 0,
            .data_lane = 0,
        };
        const memblock::LoadTransaction old_value_wb{
            .address = cas_address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
            .lane = 0,
            .rf_wen = true,
        };
        environment.expect_load_data(old_value_wb, old_value);
        if (!environment.issue_atomic(atomic, 1024) ||
            !environment.run_until_complete(8192)) {
            return false;
        }
        const bool match = word
            ? static_cast<std::uint32_t>(line_value) ==
                  static_cast<std::uint32_t>(compare)
            : line_value == compare;
        if (match) {
            line_value = word
                ? ((line_value & ~std::uint64_t{0xffffffff}) |
                   static_cast<std::uint32_t>(swap))
                : swap;
        }
        const memblock::LoadTransaction readback{
            .address = cas_address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(atomic.rob + 1),
            .lq = lq,
            .pdest = static_cast<std::uint8_t>(pdest + 1),
            .lane = 1,
        };
        environment.expect_load_data(readback, line_value);
        return environment.set_rob_head(readback.rob) &&
            environment.enqueue_load(readback) &&
            environment.issue_load(readback, 1024) &&
            environment.run_until_complete(8192) &&
            environment.run_until_lq_retired(1024);
    };

    const std::uint64_t word_compare = word_value;
    if (!run_compare_swap(
            word_address, memblock::AtomicOp::amocas_w, word_line_value,
            word_compare, 0x2468ace0U, 50, 140, 18, true) ||
        !run_compare_swap(
            word_address, memblock::AtomicOp::amocas_w, word_line_value,
            static_cast<std::uint32_t>(word_compare + 1), 0x13579bdfU,
            52, 142, 19, true)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=amocas-w reason="
                  << environment.error() << '\n';
        return 1;
    }
    word_value = static_cast<std::uint32_t>(word_line_value);
    if (!run_compare_swap(
            address, memblock::AtomicOp::amocas_d, model_value,
            model_value, 0x0123456789abcdefULL, 54, 144, 20, false) ||
        !run_compare_swap(
            address, memblock::AtomicOp::amocas_d, model_value,
            model_value ^ 1U, 0xfedcba9876543210ULL, 56, 146, 21, false)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=amocas-d reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::AtomicTransaction lr_word{
        .address = word_address,
        .op = memblock::AtomicOp::lr_w,
        .rob = 58,
        .pdest = 148,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction lr_word_wb{
        .address = word_address, .op = memblock::LoadOp::ld,
        .rob = lr_word.rob, .pdest = lr_word.pdest, .lane = 0,
        .rf_wen = true,
    };
    environment.expect_load_data(
        lr_word_wb, memblock::sign_extend(word_value, 32));
    if (!environment.issue_atomic(lr_word, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=lr-w reason="
                  << environment.error() << '\n';
        return 1;
    }
    constexpr std::uint32_t word_reservation_value = 0x13579bdfU;
    const memblock::AtomicTransaction sc_word{
        .address = word_address,
        .op = memblock::AtomicOp::sc_w,
        .data = word_reservation_value,
        .rob = 59,
        .pdest = 149,
        .address_lane = 1,
        .data_lane = 1,
    };
    const memblock::LoadTransaction sc_word_wb{
        .address = word_address, .op = memblock::LoadOp::ld,
        .rob = sc_word.rob, .pdest = sc_word.pdest, .lane = 0,
        .rf_wen = true,
    };
    environment.expect_load_data(sc_word_wb, 0);
    if (!environment.issue_atomic(sc_word, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-w-success reason="
                  << environment.error() << '\n';
        return 1;
    }
    word_value = word_reservation_value;
    word_line_value = (word_line_value & ~std::uint64_t{0xffffffff}) |
        word_value;
    const memblock::AtomicTransaction sc_word_failed{
        .address = word_address,
        .op = memblock::AtomicOp::sc_w,
        .data = 0x2468ace0U,
        .rob = 60,
        .pdest = 150,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction sc_word_failed_wb{
        .address = word_address, .op = memblock::LoadOp::ld,
        .rob = sc_word_failed.rob, .pdest = sc_word_failed.pdest,
        .lane = 0, .rf_wen = true,
    };
    environment.expect_load_data(sc_word_failed_wb, 1);
    if (!environment.issue_atomic(sc_word_failed, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-w-failure reason="
                  << environment.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction word_reservation_readback{
        .address = word_address, .op = memblock::LoadOp::ld, .rob = 61,
        .lq = 22, .pdest = 151, .lane = 1,
    };
    environment.expect_load_data(word_reservation_readback, word_line_value);
    if (!environment.set_rob_head(word_reservation_readback.rob) ||
        !environment.enqueue_load(word_reservation_readback) ||
        !environment.issue_load(word_reservation_readback, 1024) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-w-readback reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::AtomicTransaction lr{
        .address = address,
        .op = memblock::AtomicOp::lr_d,
        .rob = 62,
        .pdest = 92,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction lr_wb{
        .address = address, .op = memblock::LoadOp::ld, .rob = lr.rob,
        .pdest = lr.pdest, .lane = 0, .rf_wen = true,
    };
    environment.expect_load_data(lr_wb, model_value);
    if (!environment.issue_atomic(lr, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=lr reason="
                  << environment.error() << '\n';
        return 1;
    }

    constexpr std::uint64_t reservation_value = 0xfeedfacecafebeefULL;
    const memblock::AtomicTransaction sc{
        .address = address,
        .op = memblock::AtomicOp::sc_d,
        .data = reservation_value,
        .rob = 63,
        .pdest = 93,
        .address_lane = 1,
        .data_lane = 1,
    };
    const memblock::LoadTransaction sc_wb{
        .address = address, .op = memblock::LoadOp::ld, .rob = sc.rob,
        .pdest = sc.pdest, .lane = 0, .rf_wen = true,
    };
    environment.expect_load_data(sc_wb, 0);
    if (!environment.issue_atomic(sc, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-success reason="
                  << environment.error() << '\n';
        return 1;
    }
    model_value = reservation_value;

    const memblock::AtomicTransaction sc_failed{
        .address = address,
        .op = memblock::AtomicOp::sc_d,
        .data = 0x1111222233334444ULL,
        .rob = 64,
        .pdest = 94,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction sc_failed_wb{
        .address = address, .op = memblock::LoadOp::ld,
        .rob = sc_failed.rob, .pdest = sc_failed.pdest,
        .lane = 0, .rf_wen = true,
    };
    environment.expect_load_data(sc_failed_wb, 1);
    if (!environment.issue_atomic(sc_failed, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-failure reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction reservation_readback{
        .address = address, .op = memblock::LoadOp::ld, .rob = 65,
        .lq = static_cast<std::uint8_t>(operations.size() +
            word_operations.size() + 5),
        .pdest = 95, .lane = 1,
    };
    environment.expect_load_data(reservation_readback, model_value);
    if (!environment.set_rob_head(reservation_readback.rob) ||
        !environment.enqueue_load(reservation_readback) ||
        !environment.issue_load(reservation_readback, 1024) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-readback reason="
                  << environment.error() << '\n';
        return 1;
    }

    // The last naturally aligned doubleword below the DDR boundary belongs
    // to a readable/writable device PMA region whose atomic attribute is
    // clear. An AMO must fault before reaching either external data manager.
    memblock::Environment pma_atomic(argc, argv);
    constexpr std::uint64_t pma_atomic_address = 0x7ffffff8ULL;
    pma_atomic.memory().fill_incrementing(pma_atomic_address, 8, 0xe3);
    if (!pma_atomic.reset() || !pma_atomic.activate_bare(43)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << pma_atomic.cycle()
                  << " phase=pma-atomic-configuration reason="
                  << pma_atomic.error() << '\n';
        return 1;
    }
    const std::uint64_t pma_atomic_initial =
        pma_atomic.bus_expected_load(pma_atomic_address, memblock::LoadOp::ld);
    const memblock::AtomicTransaction pma_denied_atomic{
        .address = pma_atomic_address,
        .op = memblock::AtomicOp::amoadd_d,
        .data = 0x0102030405060708ULL,
        .rob = 0,
        .pdest = 96,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction pma_denied_writeback{
        .address = pma_atomic_address,
        .op = memblock::LoadOp::ld,
        .rob = pma_denied_atomic.rob,
        .pdest = pma_denied_atomic.pdest,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionStoreAccessFault,
        .rf_wen = true,
    };
    pma_atomic.expect_load_data(pma_denied_writeback, 0);
    if (!pma_atomic.set_rob_head(
            pma_denied_atomic.rob, pma_denied_atomic.rob_flag) ||
        !pma_atomic.issue_atomic(pma_denied_atomic, 1024) ||
        !pma_atomic.run_until_complete(8192) ||
        pma_atomic.tilelink_requests() != 0 ||
        pma_atomic.uncache_requests() != 0 ||
        pma_atomic.bus_expected_load(
            pma_atomic_address, memblock::LoadOp::ld) != pma_atomic_initial) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << pma_atomic.cycle() << " phase=pma-atomic-denied"
                  << " dcache_requests=" << pma_atomic.tilelink_requests()
                  << " uncache_requests=" << pma_atomic.uncache_requests()
                  << " reason=" << pma_atomic.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_ATOMIC_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " amo_d_variants=" << operations.size()
              << " amo_w_variants=" << word_operations.size()
              << " amocas_variants=4"
              << " lr_sc=1 misaligned_d_ops=" << doubleword_atomic_ops.size()
              << " misaligned_w_ops=" << word_atomic_ops.size()
              << " misaligned_d_cases=" << misaligned_d_cases
              << " misaligned_w_cases=" << misaligned_w_cases
              << " misaligned=" << misaligned_d_cases + misaligned_w_cases
              << " misaligned_rob_wraps=" << misaligned_rob_wraps
              << " pma_atomic_denied=1"
              << " pma_atomic_cycles=" << pma_atomic.cycle()
              << " final=0x" << std::hex << model_value << std::dec
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_uncache_errors(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50032000ULL;
    constexpr std::uint64_t physical_base = 0x90032000ULL;
    constexpr std::uint64_t root = 0x97000000ULL;
    environment.memory().fill_incrementing(physical_base & ~std::uint64_t{63}, 64, 0x5c);
    environment.configure_backpressure(0x4d595df4d0f33173ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false, true) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction denied{
        .address = virtual_base + 8,
        .oracle_address = physical_base + 8,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 104,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadAccessFault,
    };
    environment.expect_load(denied);
    environment.inject_next_uncache_response_error(true, false);
    const auto retire_denied = [&]() {
        return environment.lq_dequeued() + environment.lq_canceled() >=
                environment.lq_allocated() ||
            environment.account_lq_cancellation(1);
    };
    if (!environment.enqueue_load(denied) ||
        !environment.issue_load(denied, 1024) ||
        !environment.run_until_complete(8192) ||
        !environment.run_cycles(8) ||
        !environment.redirect_after(denied.rob, denied.rob_flag, true) ||
        !environment.run_cycles(96) ||
        !retire_denied()) {
        std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                  << environment.cycle() << " phase=denied reason="
                  << environment.error()
                  << " uncache_requests=" << environment.uncache_requests()
                  << " dcache_requests=" << environment.tilelink_requests() << '\n';
        return 1;
    }

    const memblock::LoadTransaction corrupt{
        .address = virtual_base + 16,
        .oracle_address = physical_base + 16,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 1,
        .pdest = 105,
        .lane = 1,
        .expected_exception_mask = memblock::kExceptionHardwareError,
    };
    environment.expect_load(corrupt);
    environment.inject_next_uncache_response_error(false, true);
    const auto retire_corrupt = [&]() {
        return environment.lq_dequeued() + environment.lq_canceled() >=
                environment.lq_allocated() ||
            environment.account_lq_cancellation(1);
    };
    if (!environment.set_rob_head(corrupt.rob) ||
        !environment.enqueue_load(corrupt) ||
        !environment.issue_load(corrupt, 1024) ||
        !environment.run_until_complete(8192) ||
        !environment.run_cycles(8) ||
        !environment.redirect_after(corrupt.rob, corrupt.rob_flag, true) ||
        !environment.run_cycles(96) ||
        !retire_corrupt()) {
        std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                  << environment.cycle() << " phase=corrupt reason="
                  << environment.error() << '\n';
        return 1;
    }

    unsigned store_error_reports = 0;
    auto run_store_error = [&](const char *name, std::uint64_t offset,
                               bool denied_response, bool corrupt_response,
                               std::uint32_t expected_exception) {
        memblock::Environment store_environment(argc, argv);
        const std::uint64_t store_virtual = virtual_base + 0x1000 + offset;
        const std::uint64_t store_physical = physical_base + 0x1000 + offset;
        const std::uint64_t store_root = root + 0x10000;
        store_environment.configure_backpressure(
            0x94d049bb133111ebULL ^ offset, true);
        store_environment.memory().fill_incrementing(
            store_physical & ~std::uint64_t{63}, 64,
            static_cast<std::uint8_t>(0x61U + offset));
        if (!store_environment.reset() ||
            !store_environment.map_sv39_4k(
                store_virtual, store_physical, store_root,
                true, true, false, false, false, true) ||
            !store_environment.activate_sv39(store_root)) {
            std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                      << store_environment.cycle() << " phase=" << name
                      << "-configuration reason="
                      << store_environment.error() << '\n';
            return false;
        }
        store_environment.configure_cache_error_enable(true);
        if (!store_environment.run_cycles(4)) {
            return false;
        }
        const memblock::StoreTransaction store{
            .address = store_virtual,
            .oracle_address = store_physical,
            .data = 0x8877665544332211ULL ^ offset,
            .op = memblock::StoreOp::sd,
            .rob = static_cast<std::uint8_t>(2 + store_error_reports),
            .sq = 0,
            .address_lane = store_error_reports & 1U,
            .data_lane = (store_error_reports + 1U) & 1U,
            .expected_exception_mask = expected_exception,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        };
        const auto errors_before = store_environment.bus_error_stats();
        const std::uint64_t bus_data_before =
            store_environment.bus_expected_load(
                store_physical, memblock::LoadOp::ld);
        const std::uint64_t dcache_before =
            store_environment.tilelink_requests();
        const std::uint64_t uncache_before =
            store_environment.uncache_requests();
        store_environment.expect_store(store);
        store_environment.inject_next_uncache_response_error(
            denied_response, corrupt_response);
        if (!store_environment.set_rob_head(store.rob, store.rob_flag) ||
            !store_environment.enqueue_store(store, 0) ||
            !store_environment.issue_store_address_until_tlb_hit(store, 16384) ||
            !store_environment.issue_store_data(store, 4096) ||
            !store_environment.run_cycles(64) ||
            (store_environment.uncache_requests() == uncache_before &&
             !store_environment.wait_for_mmio_store_request(
                 store.rob, store.rob_flag, 8192)) ||
            !store_environment.run_until_uncache_requests(
                uncache_before + 1, 8192) ||
            !store_environment.run_until_store_complete(8192) ||
            !store_environment.run_cycles(8) ||
            !store_environment.redirect_after(store.rob, store.rob_flag, true) ||
            !store_environment.run_cycles(96) ||
            (store_environment.sq_dequeued() +
                     store_environment.sq_canceled() <
                 store_environment.sq_allocated() &&
             !store_environment.account_sq_cancellation(1))) {
            std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                      << store_environment.cycle() << " phase=" << name
                      << "-store reason=" << store_environment.error()
                      << " uncache_requests="
                      << store_environment.uncache_requests() << '\n';
            return false;
        }
        const auto errors_after = store_environment.bus_error_stats();
        const std::uint64_t expected_error_address =
            store_physical & ~std::uint64_t{63};
        if (store_environment.tilelink_requests() != dcache_before ||
            store_environment.uncache_requests() != uncache_before + 1 ||
            errors_after.dcache_reports != errors_before.dcache_reports ||
            errors_after.uncache_reports != errors_before.uncache_reports + 1 ||
            errors_after.last_uncache_address != expected_error_address ||
            store_environment.bus_expected_load(
                store_physical, memblock::LoadOp::ld) != bus_data_before ||
            store_environment.sq_dequeued() +
                    store_environment.sq_canceled() !=
                store_environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                      << store_environment.cycle() << " phase=" << name
                      << "-bus-error expected_address=0x" << std::hex
                      << expected_error_address << " actual_address=0x"
                      << errors_after.last_uncache_address << std::dec
                      << " uncache_reports="
                      << errors_after.uncache_reports -
                             errors_before.uncache_reports
                      << " dcache_reports="
                      << errors_after.dcache_reports -
                             errors_before.dcache_reports << '\n';
            return false;
        }
        ++store_error_reports;
        return true;
    };

    if (!run_store_error(
            "denied", 8, true, false,
            memblock::kExceptionStoreAccessFault)) {
        return 1;
    }

    std::cout << "MEMBLOCK_UNCACHE_ERRORS_PASS"
              << " cycle=" << environment.cycle()
              << " load_denied=1 load_corrupt=1"
              << " store_denied=1 store_corrupt=0"
              << " uncache_error_reports=" << store_error_reports
              << " uncache_requests=" << environment.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_atomic_dchannel_errors(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x32000;
    struct AtomicErrorCase {
        memblock::AtomicOp op;
        const char *name;
        bool word;
        bool load_reserved;
    };
    const std::array<AtomicErrorCase, 22> cases{{
        {memblock::AtomicOp::lr_w, "lr_w", true, true},
        {memblock::AtomicOp::amoswap_w, "amoswap_w", true, false},
        {memblock::AtomicOp::amoadd_w, "amoadd_w", true, false},
        {memblock::AtomicOp::amoxor_w, "amoxor_w", true, false},
        {memblock::AtomicOp::amoand_w, "amoand_w", true, false},
        {memblock::AtomicOp::amoor_w, "amoor_w", true, false},
        {memblock::AtomicOp::amomin_w, "amomin_w", true, false},
        {memblock::AtomicOp::amomax_w, "amomax_w", true, false},
        {memblock::AtomicOp::amominu_w, "amominu_w", true, false},
        {memblock::AtomicOp::amomaxu_w, "amomaxu_w", true, false},
        {memblock::AtomicOp::amocas_w, "amocas_w", true, false},
        {memblock::AtomicOp::lr_d, "lr_d", false, true},
        {memblock::AtomicOp::amoswap_d, "amoswap_d", false, false},
        {memblock::AtomicOp::amoadd_d, "amoadd_d", false, false},
        {memblock::AtomicOp::amoxor_d, "amoxor_d", false, false},
        {memblock::AtomicOp::amoand_d, "amoand_d", false, false},
        {memblock::AtomicOp::amoor_d, "amoor_d", false, false},
        {memblock::AtomicOp::amomin_d, "amomin_d", false, false},
        {memblock::AtomicOp::amomax_d, "amomax_d", false, false},
        {memblock::AtomicOp::amominu_d, "amominu_d", false, false},
        {memblock::AtomicOp::amomaxu_d, "amomaxu_d", false, false},
        {memblock::AtomicOp::amocas_d, "amocas_d", false, false},
    }};
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.configure_backpressure(0x243f6a8885a308d3ULL, true);
    environment.configure_cache_error_enable(true);
    std::size_t denied_cases = 0;
    std::size_t corrupt_cases = 0;
    std::size_t readbacks = 0;
    std::size_t denied_line_hits = 0;
    std::size_t corrupt_line_hits = 0;
    std::size_t sc_denied_hit_checks = 0;
    std::size_t sc_corrupt_hit_checks = 0;
    std::size_t clean_recoveries = 0;

    for (unsigned error_kind = 0; error_kind < 2; ++error_kind) {
        const bool denied = error_kind == 0;
        const bool corrupt = !denied;
        for (std::size_t index = 0; index < cases.size(); ++index) {
            const AtomicErrorCase &test_case = cases[index];
            const std::size_t global_index = error_kind * cases.size() + index;
            const std::uint64_t line_address = base + global_index * 64;
            const std::uint64_t atomic_address = line_address +
                (test_case.word ? 4 : 0);
            const memblock::LoadOp read_op = test_case.word
                ? memblock::LoadOp::lwu
                : memblock::LoadOp::ld;
            environment.memory().fill_incrementing(
                line_address, 64,
                static_cast<std::uint8_t>(0x31U + global_index));
            const std::uint64_t original =
                environment.memory().expected_load(atomic_address, read_op);
            const std::uint64_t atomic_rob_offset =
                global_index * 2 + error_kind * 2;
            const memblock::AtomicTransaction atomic{
                .address = atomic_address,
                .op = test_case.op,
                .data = 0xfedcba9876543210ULL ^
                    (0x0101010101010101ULL * global_index),
                .compare = original,
                .rob = memblock::rob_pointer_value(atomic_rob_offset),
                .rob_flag = memblock::rob_pointer_flag(atomic_rob_offset),
                .pdest = static_cast<std::uint8_t>(32 + global_index),
                .address_lane = static_cast<unsigned>(global_index & 1U),
                .data_lane = static_cast<unsigned>((global_index >> 1) & 1U),
            };
            const std::uint32_t expected_exception = corrupt
                ? memblock::kExceptionHardwareError
                : (test_case.load_reserved
                    ? memblock::kExceptionLoadAccessFault
                    : memblock::kExceptionStoreAccessFault);
            const memblock::LoadTransaction atomic_wb{
                .address = atomic.address,
                .op = read_op,
                .rob = atomic.rob,
                .rob_flag = atomic.rob_flag,
                .pdest = atomic.pdest,
                .lane = static_cast<unsigned>(global_index % 3),
                .expected_exception_mask = expected_exception,
            };
            environment.expect_load_data(atomic_wb, 0);
            environment.inject_next_dcache_response_error(denied, corrupt);
            const std::uint64_t requests_before_atomic =
                environment.tilelink_requests();
            if (!environment.set_rob_head(atomic.rob, atomic.rob_flag) ||
                !environment.issue_atomic(atomic, 1024) ||
                !environment.run_until_complete(8192) ||
                environment.tilelink_requests() != requests_before_atomic + 1) {
                std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                          << environment.cycle() << " phase="
                          << (denied ? "denied" : "corrupt")
                          << "-atomic op=" << test_case.name
                          << " index=" << index
                          << " reason=" << environment.error()
                          << " dcache_requests="
                          << environment.tilelink_requests()
                          << " expected_requests=" << requests_before_atomic + 1
                          << '\n';
                return 1;
            }
            denied_cases += denied;
            corrupt_cases += corrupt;

            // Both denied and corrupt refills are installed as poisoned lines.
            // Their error metadata must survive the miss response and make a
            // following load fault on a cache hit.  Data on an exceptional
            // writeback is not architectural and is deliberately not checked.
            environment.configure_cache_error_enable(false);
            const std::uint64_t requests_before_readback =
                environment.tilelink_requests();
            const memblock::LoadTransaction readback{
                .address = atomic.address,
                .op = read_op,
                .rob = memblock::rob_pointer_value(atomic_rob_offset + 1),
                .rob_flag = memblock::rob_pointer_flag(atomic_rob_offset + 1),
                .lq = memblock::lq_pointer_value(global_index + error_kind),
                .lq_flag = memblock::lq_pointer_flag(global_index + error_kind),
                .pdest = static_cast<std::uint8_t>(96 + global_index),
                .lane = static_cast<unsigned>(global_index % 3),
                .expected_exception_mask = denied
                    ? memblock::kExceptionLoadAccessFault
                    : memblock::kExceptionHardwareError,
            };
            environment.expect_load_data(readback, original);
            const bool readback_completed =
                environment.set_rob_head(readback.rob) &&
                environment.enqueue_load(readback) &&
                environment.issue_load(readback, 1024) &&
                environment.run_until_complete(8192);
            const std::uint64_t expected_requests = requests_before_readback;
            const bool readback_retired = environment.run_cycles(8) &&
                environment.redirect_after(
                    readback.rob, readback.rob_flag, true) &&
                environment.run_cycles(96) &&
                (environment.lq_dequeued() + environment.lq_canceled() >=
                     environment.lq_allocated() ||
                 environment.account_lq_cancellation(1));
            if (!readback_completed ||
                environment.tilelink_requests() != expected_requests ||
                !readback_retired) {
                std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                          << environment.cycle() << " phase="
                          << (denied ? "denied" : "corrupt")
                          << "-readback op=" << test_case.name
                          << " index=" << index
                          << " reason=" << environment.error()
                          << " dcache_requests="
                          << environment.tilelink_requests()
                          << " expected_requests=" << expected_requests << '\n';
                return 1;
            }
            ++readbacks;
            denied_line_hits += denied;
            corrupt_line_hits += corrupt;
            environment.configure_cache_error_enable(true);

            if (denied && test_case.load_reserved) {
                // The delayed readback above leaves the denied line cached and
                // lets the LR reservation timer expire.  A following SC must
                // hit the poisoned line, report storeAccessFault, and issue no
                // new request.  This observes the error policy, not the
                // internal reservation bit.
                const std::uint8_t sc_rob = static_cast<std::uint8_t>(
                    readback.rob + 1);
                const memblock::AtomicTransaction sc{
                    .address = atomic.address,
                    .op = test_case.word
                        ? memblock::AtomicOp::sc_w
                        : memblock::AtomicOp::sc_d,
                    .data = 0x0123456789abcdefULL,
                    .rob = sc_rob,
                    .pdest = static_cast<std::uint8_t>(150 + sc_denied_hit_checks),
                    .address_lane = static_cast<unsigned>(sc_denied_hit_checks & 1U),
                    .data_lane = static_cast<unsigned>(sc_denied_hit_checks & 1U),
                };
                const memblock::LoadTransaction sc_wb{
                    .address = sc.address,
                    .op = read_op,
                    .rob = sc.rob,
                    .pdest = sc.pdest,
                    .lane = 0,
                    .expected_exception_mask =
                        memblock::kExceptionStoreAccessFault,
                };
                environment.expect_load_data(sc_wb, 0);
                const std::uint64_t requests_before_sc =
                    environment.tilelink_requests();
                if (!environment.set_rob_head(sc.rob) ||
                    !environment.issue_atomic(sc, 1024) ||
                    !environment.run_until_complete(8192) ||
                    environment.tilelink_requests() != requests_before_sc) {
                    std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                              << environment.cycle()
                              << " phase=denied-sc-hit op="
                              << test_case.name << " reason="
                              << environment.error() << " dcache_requests="
                              << environment.tilelink_requests()
                              << " expected_requests=" << requests_before_sc
                              << '\n';
                    return 1;
                }
                ++sc_denied_hit_checks;
            } else if (corrupt && test_case.load_reserved) {
                // SC cannot issue a D-channel request on a miss: it fails as
                // soon as either the reservation or cache hit is absent.  The
                // corrupt LR refill is retained, making a following SC hit on
                // cached corrupt metadata the reachable SC error case.  It
                // must report hardwareError and perform no write.
                const memblock::AtomicTransaction sc{
                    .address = atomic.address,
                    .op = test_case.word
                        ? memblock::AtomicOp::sc_w
                        : memblock::AtomicOp::sc_d,
                    .data = 0x89abcdef01234567ULL,
                    .rob = static_cast<std::uint8_t>(readback.rob + 1),
                    .pdest = static_cast<std::uint8_t>(170 + sc_corrupt_hit_checks),
                    .address_lane = static_cast<unsigned>(sc_corrupt_hit_checks & 1U),
                    .data_lane = static_cast<unsigned>(sc_corrupt_hit_checks & 1U),
                };
                const memblock::LoadTransaction sc_wb{
                    .address = sc.address,
                    .op = read_op,
                    .rob = sc.rob,
                    .pdest = sc.pdest,
                    .lane = 0,
                    .expected_exception_mask = memblock::kExceptionHardwareError,
                };
                environment.expect_load_data(sc_wb, 0);
                const std::uint64_t requests_before_sc =
                    environment.tilelink_requests();
                if (!environment.set_rob_head(sc.rob) ||
                    !environment.issue_atomic(sc, 1024) ||
                    !environment.run_until_complete(8192) ||
                    environment.tilelink_requests() != requests_before_sc) {
                    std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                              << environment.cycle()
                              << " phase=corrupt-sc-hit op="
                              << test_case.name << " reason="
                              << environment.error() << " dcache_requests="
                              << environment.tilelink_requests()
                              << " expected_requests=" << requests_before_sc
                              << '\n';
                    return 1;
                }
                ++sc_corrupt_hit_checks;
            }
        }

        // A clean AMO immediately after each complete error-kind matrix must
        // not inherit AtomicsUnit exception state.  Use a new line so the AMO
        // takes one cold request, then require an ordinary load to hit the
        // updated line without further traffic.
        const std::uint64_t recovery_line = base +
            (cases.size() * 2 + error_kind) * 64;
        const std::uint64_t recovery_address = recovery_line + 8;
        const std::uint64_t recovery_operand =
            0x5a5aa5a500000000ULL | error_kind;
        environment.memory().fill_incrementing(
            recovery_line, 64, static_cast<std::uint8_t>(0xc1U + error_kind));
        const std::uint64_t recovery_old = environment.memory().expected_load(
            recovery_address, memblock::LoadOp::ld);
        const std::uint64_t recovery_rob_offset =
            cases.size() * 2 * (error_kind + 1) + error_kind * 2;
        const memblock::AtomicTransaction recovery_atomic{
            .address = recovery_address,
            .op = memblock::AtomicOp::amoswap_d,
            .data = recovery_operand,
            .rob = memblock::rob_pointer_value(recovery_rob_offset),
            .rob_flag = memblock::rob_pointer_flag(recovery_rob_offset),
            .pdest = static_cast<std::uint8_t>(200 + error_kind * 2),
            .address_lane = error_kind & 1U,
            .data_lane = error_kind & 1U,
        };
        const memblock::LoadTransaction recovery_wb{
            .address = recovery_address,
            .op = memblock::LoadOp::ld,
            .rob = recovery_atomic.rob,
            .rob_flag = recovery_atomic.rob_flag,
            .pdest = recovery_atomic.pdest,
            .lane = error_kind % 3,
            .rf_wen = true,
        };
        environment.expect_load_data(recovery_wb, recovery_old);
        const std::uint64_t requests_before_recovery =
            environment.tilelink_requests();
        if (!environment.set_rob_head(
                recovery_atomic.rob, recovery_atomic.rob_flag) ||
            !environment.issue_atomic(recovery_atomic, 1024) ||
            !environment.run_until_complete(8192) ||
            environment.tilelink_requests() != requests_before_recovery + 1) {
            std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                      << environment.cycle() << " phase="
                      << (denied ? "denied" : "corrupt")
                      << "-clean-recovery-atomic reason=" << environment.error()
                      << " dcache_requests=" << environment.tilelink_requests()
                      << " expected_requests=" << requests_before_recovery + 1
                      << '\n';
            return 1;
        }

        const std::uint64_t recovery_lq_offset =
            cases.size() * (error_kind + 1) + error_kind;
        const memblock::LoadTransaction recovery_readback{
            .address = recovery_address,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(recovery_rob_offset + 1),
            .rob_flag = memblock::rob_pointer_flag(recovery_rob_offset + 1),
            .lq = memblock::lq_pointer_value(recovery_lq_offset),
            .lq_flag = memblock::lq_pointer_flag(recovery_lq_offset),
            .pdest = static_cast<std::uint8_t>(201 + error_kind * 2),
            .lane = (error_kind + 1) % 3,
        };
        environment.expect_load_data(recovery_readback, recovery_operand);
        const std::uint64_t requests_before_recovery_readback =
            environment.tilelink_requests();
        if (!environment.set_rob_head(
                recovery_readback.rob, recovery_readback.rob_flag) ||
            !environment.enqueue_load(recovery_readback) ||
            !environment.issue_load(recovery_readback, 1024) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(1024) ||
            environment.tilelink_requests() != requests_before_recovery_readback) {
            std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                      << environment.cycle() << " phase="
                      << (denied ? "denied" : "corrupt")
                      << "-clean-recovery-readback reason=" << environment.error()
                      << " dcache_requests=" << environment.tilelink_requests()
                      << " expected_requests="
                      << requests_before_recovery_readback << '\n';
            return 1;
        }
        ++clean_recoveries;
    }

    std::cout << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_PASS cycle="
              << environment.cycle()
              << " denied_cases=" << denied_cases
              << " corrupt_cases=" << corrupt_cases
              << " readbacks=" << readbacks
              << " denied_line_hits=" << denied_line_hits
              << " corrupt_line_hits=" << corrupt_line_hits
              << " sc_denied_hit_checks=" << sc_denied_hit_checks
              << " sc_corrupt_hit_checks=" << sc_corrupt_hit_checks
              << " clean_recoveries=" << clean_recoveries
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256="
              << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_uncache_widths(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50034000ULL;
    constexpr std::uint64_t physical_base = 0x90034000ULL;
    constexpr std::uint64_t root = 0x97004000ULL;
    environment.memory().fill_incrementing(physical_base, 64, 0x80);
    environment.configure_backpressure(0x6a09e667f3bcc909ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false, true) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_UNCACHE_WIDTHS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    // Exercise every scalar width and sign mode at every legal byte lane of
    // an 8-byte Uncache beat. The response agent returns the complete beat,
    // so this catches address-lane, size, mask, and sign-extension mistakes.
    const std::array<memblock::LoadOp, 2> byte_ops{
        memblock::LoadOp::lb, memblock::LoadOp::lbu};
    const std::array<memblock::LoadOp, 2> half_ops{
        memblock::LoadOp::lh, memblock::LoadOp::lhu};
    const std::array<memblock::LoadOp, 2> word_ops{
        memblock::LoadOp::lw, memblock::LoadOp::lwu};
    unsigned case_count = 0;
    std::uint8_t rob = 0;
    std::uint8_t lq = 0;
    auto run_case = [&](memblock::LoadOp op, unsigned offset) {
        const memblock::LoadTransaction transaction{
            .address = virtual_base + offset,
            .oracle_address = physical_base + offset,
            .op = op,
            .rob = rob,
            .lq = lq,
            .pdest = static_cast<std::uint8_t>(120 + case_count),
            .lane = case_count % memblock::kScalarLoadLanes,
        };
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_UNCACHE_WIDTHS_FAIL cycle="
                      << environment.cycle() << " case=" << case_count
                      << " op=" << static_cast<unsigned>(op)
                      << " offset=" << offset << " reason="
                      << environment.error() << '\n';
            return false;
        }
        ++case_count;
        ++rob;
        ++lq;
        return true;
    };

    for (const auto op : byte_ops) {
        for (unsigned offset = 0; offset < 8; ++offset) {
            if (!run_case(op, offset)) {
                return 1;
            }
        }
    }
    for (const auto op : half_ops) {
        for (unsigned offset = 0; offset < 8; offset += 2) {
            if (!run_case(op, offset)) {
                return 1;
            }
        }
    }
    for (const auto op : word_ops) {
        for (unsigned offset = 0; offset < 8; offset += 4) {
            if (!run_case(op, offset)) {
                return 1;
            }
        }
    }
    if (!run_case(memblock::LoadOp::ld, 0)) {
        return 1;
    }

    std::cout << "MEMBLOCK_UNCACHE_WIDTHS_PASS"
              << " cycle=" << environment.cycle()
              << " cases=" << case_count
              << " uncache_requests=" << environment.uncache_requests()
              << " request_stalls=" << environment.uncache_request_stalls()
              << " response_delays=" << environment.uncache_response_delays()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_uncache_outstanding(int argc, char **argv)
{
    constexpr unsigned request_count = 4;
    std::uint64_t disabled_load_max = 0;
    std::uint64_t enabled_load_max = 0;
    std::uint64_t disabled_store_max = 0;
    std::uint64_t enabled_store_max = 0;

    auto configure = [&](memblock::Environment &environment,
                         std::uint64_t virtual_base,
                         std::uint64_t physical_base,
                         std::uint64_t root,
                         bool enabled,
                         std::uint64_t seed) {
        environment.memory().fill_incrementing(physical_base, 0x1000, 0x53);
        const bool configured = environment.reset() &&
            environment.map_sv39_4k(
                virtual_base, physical_base, root, true, true, false,
                false, true) &&
            environment.activate_sv39(root) &&
            environment.set_uncache_write_outstanding(enabled);
        environment.configure_backpressure(
            seed, true,
            memblock::ResponseLatencyProfiles{
                memblock::ResponseLatencyProfile::compact,
                memblock::ResponseLatencyProfile::compact,
                memblock::ResponseLatencyProfile::spec});
        return configured;
    };

    auto run_loads = [&](bool enabled, std::uint64_t &observed_max) {
        memblock::Environment environment(argc, argv);
        const std::uint64_t virtual_base = enabled
            ? 0x50048000ULL : 0x50044000ULL;
        const std::uint64_t physical_base = enabled
            ? 0x90048000ULL : 0x90044000ULL;
        const std::uint64_t root = enabled
            ? 0x97048000ULL : 0x97044000ULL;
        if (!configure(
                environment, virtual_base, physical_base, root, enabled,
                enabled ? 0x510e527fade682d1ULL : 0x9b05688c2b3e6c1fULL)) {
            std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                      << environment.cycle() << " phase="
                      << (enabled ? "enabled" : "disabled")
                      << "-load-configuration reason=" << environment.error()
                      << '\n';
            return false;
        }
        environment.force_next_uncache_response_delay(4096);
        std::array<memblock::LoadTransaction, request_count> loads{};
        for (unsigned index = 0; index < loads.size(); ++index) {
            loads[index] = memblock::LoadTransaction{
                .address = virtual_base + index * 64,
                .oracle_address = physical_base + index * 64,
                .op = memblock::LoadOp::ld,
                .rob = static_cast<std::uint8_t>(index),
                .lq = static_cast<std::uint8_t>(index),
                .pdest = static_cast<std::uint8_t>(80 + index),
                .lane = index % memblock::kScalarLoadLanes,
                .expected_debug_is_mmio = false,
                .expected_debug_is_ncio = false,
            };
            environment.expect_load(loads[index]);
            if (!environment.enqueue_load(loads[index])) {
                std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                          << environment.cycle() << " phase="
                          << (enabled ? "enabled" : "disabled")
                          << "-load-enqueue index=" << index << " reason="
                          << environment.error() << '\n';
                return false;
            }
        }
        if (!environment.set_rob_head(loads.back().rob, loads.back().rob_flag)) {
            return false;
        }
        for (unsigned index = 0; index < loads.size(); ++index) {
            if (!environment.issue_load(loads[index], 4096)) {
                std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                          << environment.cycle() << " phase="
                          << (enabled ? "enabled" : "disabled")
                          << "-load-issue index=" << index << " reason="
                          << environment.error() << '\n';
                return false;
            }
        }
        if (!environment.run_until_complete(65536) ||
            !environment.run_until_uncache_drained(request_count, 65536) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                      << environment.cycle() << " phase="
                      << (enabled ? "enabled" : "disabled")
                      << "-load-drain reason=" << environment.error() << '\n';
            return false;
        }
        observed_max = environment.uncache_max_outstanding_requests();
        const bool legal_window = enabled
            ? observed_max > 1 && observed_max <= request_count
            : observed_max == 1;
        if (environment.uncache_requests() != request_count ||
            !legal_window) {
            std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                      << environment.cycle() << " phase="
                      << (enabled ? "enabled" : "disabled")
                      << "-load-window requests="
                      << environment.uncache_requests() << " max_outstanding="
                      << observed_max << " expected_range="
                      << (enabled ? "2..4" : "1") << '\n';
            return false;
        }
        return true;
    };

    auto run_stores = [&](bool enabled, std::uint64_t &observed_max) {
        memblock::Environment environment(argc, argv);
        const std::uint64_t virtual_base = enabled
            ? 0x50050000ULL : 0x5004c000ULL;
        const std::uint64_t physical_base = enabled
            ? 0x90050000ULL : 0x9004c000ULL;
        const std::uint64_t root = enabled
            ? 0x97050000ULL : 0x9704c000ULL;
        if (!configure(
                environment, virtual_base, physical_base, root, enabled,
                enabled ? 0x1f83d9abfb41bd6bULL : 0x5be0cd19137e2179ULL)) {
            std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                      << environment.cycle() << " phase="
                      << (enabled ? "enabled" : "disabled")
                      << "-store-configuration reason=" << environment.error()
                      << '\n';
            return false;
        }
        std::array<memblock::StoreTransaction, request_count> stores{};
        for (unsigned index = 0; index < stores.size(); ++index) {
            stores[index] = memblock::StoreTransaction{
                .address = virtual_base + index * 64,
                .oracle_address = physical_base + index * 64,
                .data = 0x1020304050607080ULL + index,
                .op = memblock::StoreOp::sd,
                .rob = static_cast<std::uint8_t>(index),
                .sq = static_cast<std::uint8_t>(index),
                .address_lane = index & 1U,
                .data_lane = (index >> 1) & 1U,
                .expected_debug_is_mmio = false,
                .expected_debug_is_ncio = false,
            };
            environment.expect_store(stores[index]);
            if (!environment.enqueue_store(stores[index], 0) ||
                !environment.issue_store_address_until_tlb_hit(
                    stores[index], 16384) ||
                !environment.issue_store_data(stores[index], 4096) ||
                !environment.run_until_store_complete(32768)) {
                std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                          << environment.cycle() << " phase="
                          << (enabled ? "enabled" : "disabled")
                          << "-store-issue index=" << index << " reason="
                          << environment.error() << '\n';
                return false;
            }
        }
        environment.force_next_uncache_response_delay(4096);
        if (!environment.set_rob_head(stores.back().rob, stores.back().rob_flag) ||
            !environment.commit_stores_through(
                stores.back(), static_cast<unsigned>(stores.size())) ||
            !environment.run_until_uncache_drained(request_count, 65536) ||
            !environment.run_until_sq_dequeued(request_count, 8192)) {
            std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                      << environment.cycle() << " phase="
                      << (enabled ? "enabled" : "disabled")
                      << "-store-drain reason=" << environment.error() << '\n';
            return false;
        }
        observed_max = environment.uncache_max_outstanding_requests();
        const bool legal_window = enabled
            ? observed_max > 1 && observed_max <= request_count
            : observed_max == 1;
        if (environment.uncache_requests() != request_count ||
            !legal_window) {
            std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                      << environment.cycle() << " phase="
                      << (enabled ? "enabled" : "disabled")
                      << "-store-window requests="
                      << environment.uncache_requests() << " max_outstanding="
                      << observed_max << " expected_range="
                      << (enabled ? "2..4" : "1") << '\n';
            return false;
        }
        for (const auto &store : stores) {
            environment.record_committed_store(store);
        }
        for (unsigned index = 0; index < stores.size(); ++index) {
            const memblock::LoadTransaction readback{
                .address = stores[index].address,
                .oracle_address = stores[index].oracle_address,
                .op = memblock::LoadOp::ld,
                .rob = static_cast<std::uint8_t>(request_count + index),
                .lq = static_cast<std::uint8_t>(index),
                .pdest = static_cast<std::uint8_t>(96 + index),
                .lane = index % memblock::kScalarLoadLanes,
            };
            environment.expect_load_data(readback, stores[index].data);
            if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
                !environment.enqueue_load(readback) ||
                !environment.issue_load(readback, 4096) ||
                !environment.run_until_complete(32768) ||
                !environment.run_until_lq_retired(8192)) {
                std::cerr << "MEMBLOCK_UNCACHE_OUTSTANDING_FAIL cycle="
                          << environment.cycle() << " phase="
                          << (enabled ? "enabled" : "disabled")
                          << "-store-readback index=" << index << " reason="
                          << environment.error() << '\n';
                return false;
            }
        }
        return true;
    };

    if (!run_loads(false, disabled_load_max) ||
        !run_loads(true, enabled_load_max) ||
        !run_stores(false, disabled_store_max) ||
        !run_stores(true, enabled_store_max)) {
        return 1;
    }
    std::cout << "MEMBLOCK_UNCACHE_OUTSTANDING_PASS"
              << " requests_per_case=" << request_count
              << " disabled_load_max=" << disabled_load_max
              << " enabled_load_max=" << enabled_load_max
              << " disabled_store_max=" << disabled_store_max
              << " enabled_store_max=" << enabled_store_max
              << " delayed_first_response=4096"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_sbuffer_flush(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t cacheable_virtual = 0x50054000ULL;
    constexpr std::uint64_t cacheable_physical = 0x90054000ULL;
    constexpr std::uint64_t nc_virtual = 0x50055000ULL;
    constexpr std::uint64_t nc_physical = 0x90055000ULL;
    constexpr std::uint64_t root = 0x97054000ULL;
    constexpr std::uint64_t nc_data = 0x3141592653589793ULL;
    constexpr std::uint64_t cacheable_data = 0x2718281828459045ULL;
    environment.memory().fill_incrementing(cacheable_physical, 0x1000, 0x67);
    environment.memory().fill_incrementing(nc_physical, 0x1000, 0x97);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            cacheable_virtual, cacheable_physical, root) ||
        !environment.map_sv39_4k(
            nc_virtual, nc_physical, root, true, true, false, false, true) ||
        !environment.activate_sv39(root) ||
        !environment.set_uncache_write_outstanding(true)) {
        std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.configure_backpressure(
        0x6a09e667f3bcc909ULL, true,
        memblock::ResponseLatencyProfiles{
            memblock::ResponseLatencyProfile::compact,
            memblock::ResponseLatencyProfile::compact,
            memblock::ResponseLatencyProfile::spec});

    const memblock::StoreTransaction nc_store{
        .address = nc_virtual + 0x80,
        .oracle_address = nc_physical + 0x80,
        .data = nc_data,
        .op = memblock::StoreOp::sd,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
    };
    environment.expect_store(nc_store);
    if (!environment.set_rob_head(nc_store.rob, nc_store.rob_flag) ||
        !environment.enqueue_store(nc_store, 0) ||
        !environment.issue_store_address_until_tlb_hit(nc_store, 16384) ||
        !environment.issue_store_data(nc_store, 4096) ||
        !environment.run_until_store_complete(32768)) {
        std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                  << environment.cycle() << " phase=nc-store-issue reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.force_next_uncache_response_delay(1024);
    if (!environment.commit_store(nc_store, 16384) ||
        !environment.run_until_uncache_requests(1, 4096) ||
        environment.uncache_requests() != 1 ||
        environment.uncache_outstanding_requests() != 1) {
        std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                  << environment.cycle() << " phase=nc-store-outstanding"
                  << " requests=" << environment.uncache_requests()
                  << " outstanding="
                  << environment.uncache_outstanding_requests()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    const memblock::StoreTransaction cacheable_store{
        .address = cacheable_virtual + 0x88,
        .oracle_address = cacheable_physical + 0x88,
        .data = cacheable_data,
        .op = memblock::StoreOp::sd,
        .rob = 1,
        .sq = 1,
        .address_lane = 1,
        .data_lane = 0,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
    };
    environment.expect_store(cacheable_store);
    if (!environment.set_rob_head(
            cacheable_store.rob, cacheable_store.rob_flag) ||
        !environment.enqueue_store(cacheable_store, 0) ||
        !environment.issue_store_address_until_tlb_hit(
            cacheable_store, 16384) ||
        !environment.issue_store_data(cacheable_store, 4096) ||
        !environment.run_until_store_complete(32768) ||
        !environment.commit_store(cacheable_store, 16384)) {
        std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                  << environment.cycle()
                  << " phase=cacheable-store-buffering reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t requests_before_flush =
        environment.tilelink_requests();
    if (environment.sbuffer_empty() ||
        environment.uncache_outstanding_requests() != 1) {
        std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                  << environment.cycle() << " phase=pre-flush-state"
                  << " sb_empty=" << environment.sbuffer_empty()
                  << " uncache_outstanding="
                  << environment.uncache_outstanding_requests() << '\n';
        return 1;
    }

    const std::uint64_t flush_cycle = environment.cycle();
    if (!environment.pulse_sbuffer_flush() ||
        !environment.run_cycles(256) ||
        environment.tilelink_requests() <= requests_before_flush ||
        environment.sbuffer_empty()) {
        std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                  << environment.cycle() << " phase=flush-start"
                  << " dcache_before=" << requests_before_flush
                  << " dcache_after=" << environment.tilelink_requests()
                  << " sb_empty=" << environment.sbuffer_empty()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    if (!environment.run_until_sbuffer_empty(8192) ||
        !environment.run_until_uncache_drained(1, 8192)) {
        std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                  << environment.cycle() << " phase=flush-drain reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t flush_busy_cycles = environment.cycle() - flush_cycle;
    if (flush_busy_cycles < 512 ||
        environment.uncache_outstanding_requests() != 0) {
        std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                  << environment.cycle() << " phase=flush-duration"
                  << " busy_cycles=" << flush_busy_cycles
                  << " uncache_outstanding="
                  << environment.uncache_outstanding_requests() << '\n';
        return 1;
    }

    const std::array<memblock::LoadTransaction, 2> readbacks{{
        {
            .address = cacheable_store.address,
            .oracle_address = cacheable_store.oracle_address,
            .op = memblock::LoadOp::ld,
            .rob = 2,
            .lq = 0,
            .pdest = 110,
            .lane = 0,
        },
        {
            .address = nc_store.address,
            .oracle_address = nc_store.oracle_address,
            .op = memblock::LoadOp::ld,
            .rob = 3,
            .lq = 1,
            .pdest = 111,
            .lane = 1,
        },
    }};
    const std::array<std::uint64_t, 2> expected{{
        cacheable_data, nc_data,
    }};
    for (unsigned index = 0; index < readbacks.size(); ++index) {
        environment.expect_load_data(readbacks[index], expected[index]);
        if (!environment.set_rob_head(
                readbacks[index].rob, readbacks[index].rob_flag) ||
            !environment.enqueue_load(readbacks[index]) ||
            !environment.issue_load(readbacks[index], 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_SBUFFER_FLUSH_FAIL cycle="
                      << environment.cycle() << " phase=readback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    std::cout << "MEMBLOCK_SBUFFER_FLUSH_PASS"
              << " flush_busy_cycles=" << flush_busy_cycles
              << " dcache_requests=" << environment.tilelink_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " combined_empty=1 cacheable_readback=1 nc_readback=1"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_sbuffer_timeout(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50056000ULL;
    constexpr std::uint64_t physical_base = 0x90056000ULL;
    constexpr std::uint64_t root = 0x97056000ULL;
    constexpr std::uint32_t high_timeout = (1U << 22) - 1;
    constexpr std::uint32_t low_timeout = 64;
    constexpr unsigned high_observation_cycles = 512;
    environment.memory().fill_incrementing(physical_base, 0x1000, 0x5a);
    environment.configure_backpressure(0xbb67ae8584caa73bULL, false);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root) ||
        !environment.activate_sv39(root) ||
        !environment.set_sbuffer_timeout(high_timeout)) {
        std::cerr << "MEMBLOCK_SBUFFER_TIMEOUT_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    auto issue_and_commit = [&](const memblock::StoreTransaction &store) {
        environment.expect_store(store);
        return environment.set_rob_head(store.rob, store.rob_flag) &&
            environment.enqueue_store(store, 0) &&
            environment.issue_store_address_until_tlb_hit(store, 16384) &&
            environment.issue_store_data(store, 4096) &&
            environment.run_until_store_complete(32768) &&
            environment.commit_store(store, 16384);
    };
    auto read_back = [&](const memblock::StoreTransaction &store,
                         std::uint8_t rob, std::uint8_t lq,
                         std::uint8_t pdest) {
        const memblock::LoadTransaction load{
            .address = store.address,
            .oracle_address = store.oracle_address,
            .op = memblock::LoadOp::ld,
            .rob = rob,
            .lq = lq,
            .pdest = pdest,
            .lane = static_cast<unsigned>(lq % memblock::kScalarLoadLanes),
        };
        environment.expect_load_data(load, store.data);
        return environment.set_rob_head(load.rob, load.rob_flag) &&
            environment.enqueue_load(load) &&
            environment.issue_load(load, 4096) &&
            environment.run_until_complete(32768) &&
            environment.run_until_lq_retired(8192);
    };

    const memblock::StoreTransaction high_store{
        .address = virtual_base + 0x80,
        .oracle_address = physical_base + 0x80,
        .data = 0x0123456789abcdefULL,
        .op = memblock::StoreOp::sd,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
    };
    if (!issue_and_commit(high_store) || environment.sbuffer_empty()) {
        std::cerr << "MEMBLOCK_SBUFFER_TIMEOUT_FAIL cycle="
                  << environment.cycle()
                  << " phase=high-timeout-buffering reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t high_requests_before = environment.tilelink_requests();
    if (!environment.run_cycles(high_observation_cycles) ||
        environment.sbuffer_empty() ||
        environment.tilelink_requests() != high_requests_before) {
        std::cerr << "MEMBLOCK_SBUFFER_TIMEOUT_FAIL cycle="
                  << environment.cycle() << " phase=high-timeout-hold"
                  << " dcache_before=" << high_requests_before
                  << " dcache_after=" << environment.tilelink_requests()
                  << " sb_empty=" << environment.sbuffer_empty()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    if (!environment.pulse_sbuffer_flush() ||
        !environment.run_until_sbuffer_empty(8192) ||
        !read_back(high_store, 1, 0, 112)) {
        std::cerr << "MEMBLOCK_SBUFFER_TIMEOUT_FAIL cycle="
                  << environment.cycle() << " phase=high-timeout-drain reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::StoreTransaction low_store{
        .address = virtual_base + 0xc8,
        .oracle_address = physical_base + 0xc8,
        .data = 0xfedcba9876543210ULL,
        .op = memblock::StoreOp::sd,
        .rob = 2,
        .sq = 1,
        .address_lane = 1,
        .data_lane = 0,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
    };
    if (!environment.set_sbuffer_timeout(low_timeout) ||
        !issue_and_commit(low_store) || environment.sbuffer_empty()) {
        std::cerr << "MEMBLOCK_SBUFFER_TIMEOUT_FAIL cycle="
                  << environment.cycle()
                  << " phase=low-timeout-buffering reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t low_start_cycle = environment.cycle();
    const std::uint64_t low_requests_before = environment.tilelink_requests();
    while (environment.tilelink_requests() == low_requests_before &&
           environment.cycle() - low_start_cycle < 256) {
        if (!environment.run_cycles(1)) {
            break;
        }
    }
    const std::uint64_t low_evict_cycles = environment.cycle() - low_start_cycle;
    if (environment.tilelink_requests() != low_requests_before + 1 ||
        low_evict_cycles == 0 || low_evict_cycles >= 256 ||
        !environment.run_until_sbuffer_empty(8192) ||
        !read_back(low_store, 3, 1, 113)) {
        std::cerr << "MEMBLOCK_SBUFFER_TIMEOUT_FAIL cycle="
                  << environment.cycle() << " phase=low-timeout-evict"
                  << " dcache_before=" << low_requests_before
                  << " dcache_after=" << environment.tilelink_requests()
                  << " evict_cycles=" << low_evict_cycles
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_SBUFFER_TIMEOUT_PASS"
              << " high_timeout=" << high_timeout
              << " high_hold_cycles=" << high_observation_cycles
              << " low_timeout=" << low_timeout
              << " low_evict_cycles=" << low_evict_cycles
              << " high_readback=1 low_readback=1"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_mbmc_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50000000ULL;
    constexpr std::uint64_t virtual_stride = 0x200000ULL;
    constexpr std::uint64_t physical_base = 0x90060000ULL;
    constexpr std::uint64_t root = 0x97060000ULL;
    constexpr std::uint64_t bitmap_base = 0x98000000ULL;
    constexpr std::uint64_t policy_word_address =
        memblock::reference_bitmap_word_address(bitmap_base, physical_base);
    constexpr std::uint64_t bme_gate_mask =
        memblock::reference_bitmap_deny_mask(physical_base);
    constexpr std::uint64_t cmode_gate_mask =
        memblock::reference_bitmap_deny_mask(physical_base + 0x1000);
    constexpr std::uint64_t cache_mask =
        memblock::reference_bitmap_deny_mask(physical_base + 0x2000);
    static_assert(policy_word_address == 0x98012008ULL);
    static_assert(bme_gate_mask == (std::uint64_t{1} << 32));
    static_assert(cmode_gate_mask == (std::uint64_t{1} << 33));
    static_assert(cache_mask == (std::uint64_t{1} << 34));

    environment.memory().fill_incrementing(physical_base, 0x3000, 0x35);
    environment.memory().write_u64(
        policy_word_address, bme_gate_mask | cmode_gate_mask);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }
    for (unsigned index = 0; index < 6; ++index) {
        const std::uint64_t physical = physical_base +
            (index == 0 ? 0 : index == 1 ? 0x1000 : 0x2000);
        if (!environment.map_sv39_4k(
                virtual_base + index * virtual_stride,
                physical, root)) {
            std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=mapping index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.activate_sv39(root, 91) ||
        !environment.set_mbmc(false, false, bitmap_base)) {
        std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    auto run_load = [&](unsigned index, std::uint32_t exception,
                        std::string_view phase) {
        const std::uint64_t physical = physical_base +
            (index == 0 ? 0 : index == 1 ? 0x1000 : 0x2000);
        const memblock::LoadTransaction load{
            .address = virtual_base + index * virtual_stride + 0x188,
            .oracle_address = physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(116 + index),
            .lane = index % memblock::kScalarLoadLanes,
            .expected_exception_mask = exception,
        };
        if (exception == 0) {
            environment.expect_load(load);
        } else {
            environment.expect_load_data(load, 0);
        }
        if (!environment.set_rob_head(load.rob, load.rob_flag) ||
            !environment.enqueue_load(load) ||
            !environment.issue_load(load, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=" << phase
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        return true;
    };

    if (!run_load(0, 0, "bme-disabled") ||
        !environment.set_mbmc(true, true, bitmap_base) ||
        !run_load(1, 0, "cmode-disabled")) {
        return 1;
    }

    if (!environment.set_mbmc(true, false, bitmap_base)) {
        std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=enable reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t allow_ptw_before = environment.ptw_requests();
    if (!run_load(2, 0, "bitmap-allow") ||
        environment.ptw_requests_covering_since(
            policy_word_address, allow_ptw_before) != 1) {
        std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=bitmap-allow-fetch"
                  << " bitmap_requests="
                  << environment.ptw_requests_covering_since(
                         policy_word_address, allow_ptw_before)
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    environment.memory().write_u64(
        policy_word_address,
        bme_gate_mask | cmode_gate_mask | cache_mask);
    const std::uint64_t cached_ptw_before = environment.ptw_requests();
    if (!run_load(3, 0, "bitmap-cache-retains-allow") ||
        environment.ptw_requests_covering_since(
            policy_word_address, cached_ptw_before) != 0) {
        std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=bitmap-cache-hit"
                  << " bitmap_requests="
                  << environment.ptw_requests_covering_since(
                         policy_word_address, cached_ptw_before)
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    if (!environment.pulse_mbmc_bitmap_clear()) {
        std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=bclear reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t denied_dcache_before = environment.tilelink_requests();
    const std::uint64_t denied_ptw_before = environment.ptw_requests();
    if (!run_load(4, memblock::kExceptionLoadAccessFault,
                  "bitmap-denied-load") ||
        environment.tilelink_requests() != denied_dcache_before ||
        environment.ptw_requests_covering_since(
            policy_word_address, denied_ptw_before) != 1) {
        std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=bitmap-denied-load-side-effect"
                  << " dcache_before=" << denied_dcache_before
                  << " dcache_after=" << environment.tilelink_requests()
                  << " bitmap_requests="
                  << environment.ptw_requests_covering_since(
                         policy_word_address, denied_ptw_before)
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    const memblock::StoreTransaction denied_store{
        .address = virtual_base + 5 * virtual_stride + 0x1c8,
        .oracle_address = physical_base + 0x2000 + 0x1c8,
        .data = 0xa55aa55a01234567ULL,
        .op = memblock::StoreOp::sd,
        .rob = 6,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
        .expected_exception_mask = memblock::kExceptionStoreAccessFault,
    };
    const std::uint64_t store_dcache_before = environment.tilelink_requests();
    environment.expect_store(denied_store);
    if (!environment.set_rob_head(
            denied_store.rob, denied_store.rob_flag) ||
        !environment.enqueue_store(denied_store, 0) ||
        !environment.issue_store_address(denied_store, 4096) ||
        !environment.issue_store_data(denied_store, 4096) ||
        !environment.run_until_store_complete_with_replay(
            denied_store, 32768) ||
        environment.tilelink_requests() != store_dcache_before ||
        (environment.sq_dequeued() + environment.sq_canceled() <
             environment.sq_allocated() &&
         !environment.account_sq_cancellation(1)) ||
        environment.sq_dequeued() + environment.sq_canceled() !=
            environment.sq_allocated()) {
        std::cerr << "MEMBLOCK_MBMC_CONTRACTS_FAIL phase=bitmap-denied-store"
                  << " dcache_before=" << store_dcache_before
                  << " dcache_after=" << environment.tilelink_requests()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_MBMC_CONTRACTS_PASS"
              << " bme_gate=1 cmode_gate=1 bitmap_allow=1"
              << " cache_hit=1 bclear_refetch=1"
              << " load_access_fault=1 store_access_fault=1"
              << " policy_word=0x" << std::hex << policy_word_address
              << std::dec
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
