// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_ptw_errors(int argc, char **argv)
{
    struct Case {
        const char *name;
        memblock::ReferencePageMode mode;
        bool store;
        bool denied;
        bool corrupt;
        unsigned clean_requests_before_error;
        memblock::PtwCorruptBeat corrupt_beat;
    };
    constexpr std::array<Case, 8> cases{{
        {"sv39-load-root-denied", memblock::ReferencePageMode::sv39,
         false, true, false, 0, memblock::PtwCorruptBeat::all},
        {"sv39-load-leaf-corrupt", memblock::ReferencePageMode::sv39,
         false, false, true, 2, memblock::PtwCorruptBeat::first},
        {"sv48-load-middle-denied", memblock::ReferencePageMode::sv48,
         false, true, false, 1, memblock::PtwCorruptBeat::all},
        {"sv48-load-leaf-corrupt", memblock::ReferencePageMode::sv48,
         false, false, true, 3, memblock::PtwCorruptBeat::last},
        {"sv39-store-root-corrupt", memblock::ReferencePageMode::sv39,
         true, false, true, 0, memblock::PtwCorruptBeat::first},
        {"sv39-store-leaf-denied", memblock::ReferencePageMode::sv39,
         true, true, false, 2, memblock::PtwCorruptBeat::all},
        {"sv48-store-middle-corrupt", memblock::ReferencePageMode::sv48,
         true, false, true, 2, memblock::PtwCorruptBeat::last},
        {"sv48-store-leaf-denied", memblock::ReferencePageMode::sv48,
         true, true, false, 3, memblock::PtwCorruptBeat::all},
    }};

    unsigned loads = 0;
    unsigned stores = 0;
    unsigned denied = 0;
    unsigned corrupt = 0;
    unsigned root = 0;
    unsigned intermediate = 0;
    unsigned leaf = 0;
    std::uint64_t total_cycles = 0;
    std::uint64_t total_ptw_requests = 0;
    unsigned stage2_cases = 0;
    unsigned nested_cases = 0;
    unsigned bitmap_cases = 0;
    unsigned first_beat_corrupt = 0;
    unsigned last_beat_corrupt = 0;
    unsigned clean_recoveries = 0;

    const auto run_clean_recovery = [&](
        memblock::Environment &environment, const char *phase,
        std::uint64_t address, std::uint64_t physical_address,
        bool fault_was_store, unsigned identity,
        std::uint64_t dcache_before, std::uint64_t uncache_before,
        bool hfence_vvma = false, bool hfence_gvma = false,
        std::uint64_t expected_failed_block_rereads = 1) {
        const std::uint64_t failed_block =
            environment.ptw_last_error_response_address();
        // The L1 DTLB may cache an access-fault translation.  Invalidate that
        // architecturally visible result before checking that the lower-level
        // page-table caches did not retain data from the bad block response.
        if (!environment.issue_sfence(
                0, 0, true, true, hfence_vvma, hfence_gvma)) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << phase
                      << "-recovery-fence reason=" << environment.error()
                      << '\n';
            return false;
        }
        const std::uint64_t ptw_before = environment.ptw_requests();
        const std::uint64_t lq_offset = fault_was_store ? 0 : 1;
        const memblock::LoadTransaction recovery{
            .address = address,
            .oracle_address = physical_address,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(1),
            .rob_flag = memblock::rob_pointer_flag(1),
            .lq = memblock::lq_pointer_value(lq_offset),
            .lq_flag = memblock::lq_pointer_flag(lq_offset),
            .pdest = static_cast<std::uint8_t>(224 + identity),
            .lane = identity % memblock::kScalarLoadLanes,
        };
        environment.expect_load(recovery);
        if (!environment.set_rob_head(recovery.rob, recovery.rob_flag) ||
            !environment.enqueue_load(recovery) ||
            !environment.issue_load(recovery, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192) ||
            environment.ptw_error_response_requests() != 1 ||
            environment.ptw_requests_covering_since(
                failed_block, ptw_before) != expected_failed_block_rereads ||
            environment.tilelink_requests() != dcache_before + 1 ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << phase
                      << "-clean-recovery reason=" << environment.error()
                      << " failed_block=0x" << std::hex << failed_block
                      << std::dec << " rereads="
                      << environment.ptw_requests_covering_since(
                             failed_block, ptw_before) << '/'
                      << expected_failed_block_rereads
                      << " errors="
                      << environment.ptw_error_response_requests()
                      << " dcache=" << environment.tilelink_requests() << '/'
                      << dcache_before + 1
                      << " uncache=" << environment.uncache_requests() << '/'
                      << uncache_before << '\n';
            return false;
        }
        ++clean_recoveries;
        return true;
    };

    for (std::size_t index = 0; index < cases.size(); ++index) {
        const Case &test = cases[index];
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_page = 0x78000000ULL;
        constexpr std::uint64_t physical_page = 0xce000000ULL;
        constexpr std::uint64_t root_page = 0xae000000ULL;
        environment.memory().fill_incrementing(physical_page, 0x1000, 0x4d);
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                      << "-reset reason=" << environment.error() << '\n';
            return 1;
        }
        const bool mapped = test.mode == memblock::ReferencePageMode::sv48
            ? environment.map_sv48_4k(
                  virtual_page, physical_page, root_page)
            : environment.map_sv39_4k(
                  virtual_page, physical_page, root_page);
        const bool activated = test.mode == memblock::ReferencePageMode::sv48
            ? environment.activate_sv48(root_page, 51 + index)
            : environment.activate_sv39(root_page, 51 + index);
        if (!mapped || !activated) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                      << "-configuration reason=" << environment.error()
                      << '\n';
            return 1;
        }
        environment.inject_ptw_response_error_after(
            test.clean_requests_before_error, test.denied, test.corrupt,
            test.corrupt_beat);
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        const std::uint64_t ptw_before = environment.ptw_requests();

        if (!test.store) {
            const memblock::LoadTransaction transaction{
                .address = virtual_page + 0x188,
                .op = memblock::LoadOp::ld,
                .rob = 0,
                .lq = 0,
                .pdest = static_cast<std::uint8_t>(180 + index),
                .lane = static_cast<unsigned>(
                    index % memblock::kScalarLoadLanes),
                .expected_exception_mask =
                    memblock::kExceptionLoadAccessFault,
            };
            environment.expect_load(transaction);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !environment.enqueue_load(transaction) ||
                !environment.issue_load(transaction, 2048) ||
                !environment.run_until_complete(16384) ||
                !environment.run_until_lq_retired(4096)) {
                std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                          << "-execution reason=" << environment.error()
                          << " ptw=" << environment.ptw_requests()
                          << " error_address=0x" << std::hex
                          << environment.ptw_last_error_response_address()
                          << std::dec << " error_source="
                          << static_cast<unsigned>(
                                 environment.ptw_last_error_response_source())
                          << '\n';
                return 1;
            }
            ++loads;
        } else {
            const memblock::StoreTransaction transaction{
                .address = virtual_page + 0x188,
                .data = 0x1020304050607080ULL + index,
                .op = memblock::StoreOp::sd,
                .rob = 0,
                .sq = 0,
                .address_lane = static_cast<unsigned>(
                    index % memblock::kScalarStoreLanes),
                .data_lane = static_cast<unsigned>(
                    (index + 1) % memblock::kScalarStoreLanes),
                .expected_exception_mask =
                    memblock::kExceptionStoreAccessFault,
            };
            environment.expect_store(transaction);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !environment.enqueue_store(transaction, 0) ||
                !environment.issue_store_address(transaction, 2048) ||
                !environment.issue_store_data(transaction, 2048) ||
                !environment.run_until_store_complete_with_replay(
                    transaction, 16384) ||
                (environment.sq_dequeued() + environment.sq_canceled() <
                     environment.sq_allocated() &&
                 !environment.account_sq_cancellation(1)) ||
                environment.sq_dequeued() + environment.sq_canceled() !=
                    environment.sq_allocated()) {
                std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                          << "-execution reason="
                          << (environment.error().empty()
                                  ? "faulting store left an unbalanced SQ"
                                  : environment.error())
                          << " ptw=" << environment.ptw_requests()
                          << " sq=" << environment.sq_dequeued() << '+'
                          << environment.sq_canceled() << '/'
                          << environment.sq_allocated()
                          << '\n';
                return 1;
            }
            ++stores;
        }

        const std::uint64_t expected_ptw_requests =
            ptw_before + test.clean_requests_before_error + 1;
        if (environment.ptw_error_response_requests() != 1 ||
            environment.ptw_requests() != expected_ptw_requests ||
            environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                      << "-side-effects"
                      << " injected="
                      << environment.ptw_error_response_requests()
                      << " ptw=" << environment.ptw_requests() << '/'
                      << expected_ptw_requests
                      << " dcache=" << environment.tilelink_requests() << '/'
                      << dcache_before
                      << " uncache=" << environment.uncache_requests() << '/'
                      << uncache_before << '\n';
            return 1;
        }
        if (!run_clean_recovery(
                environment, test.name, virtual_page + 0x188,
                physical_page + 0x188, test.store,
                static_cast<unsigned>(index), dcache_before,
                uncache_before, false, false)) {
            return 1;
        }
        denied += test.denied;
        corrupt += test.corrupt;
        first_beat_corrupt += test.corrupt &&
            test.corrupt_beat == memblock::PtwCorruptBeat::first;
        last_beat_corrupt += test.corrupt &&
            test.corrupt_beat == memblock::PtwCorruptBeat::last;
        const unsigned levels = memblock::reference_page_levels(test.mode);
        root += test.clean_requests_before_error == 0;
        leaf += test.clean_requests_before_error + 1 == levels;
        intermediate += test.clean_requests_before_error != 0 &&
            test.clean_requests_before_error + 1 != levels;
        total_cycles += environment.cycle();
        total_ptw_requests += environment.ptw_requests();
    }

    struct Stage2Case {
        const char *name;
        memblock::ReferencePageMode mode;
        bool store;
        bool denied;
        bool corrupt;
        unsigned clean_requests_before_error;
        memblock::PtwCorruptBeat corrupt_beat;
    };
    constexpr std::array<Stage2Case, 4> stage2_tests{{
        {"sv39x4-load-root-denied", memblock::ReferencePageMode::sv39,
         false, true, false, 0, memblock::PtwCorruptBeat::all},
        {"sv39x4-store-leaf-corrupt", memblock::ReferencePageMode::sv39,
         true, false, true, 2, memblock::PtwCorruptBeat::first},
        {"sv48x4-load-middle-corrupt", memblock::ReferencePageMode::sv48,
         false, false, true, 1, memblock::PtwCorruptBeat::last},
        {"sv48x4-store-leaf-denied", memblock::ReferencePageMode::sv48,
         true, true, false, 3, memblock::PtwCorruptBeat::all},
    }};
    for (std::size_t index = 0; index < stage2_tests.size(); ++index) {
        const auto &test = stage2_tests[index];
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_page = 0x120000000ULL;
        constexpr std::uint64_t physical_page = 0xcf000000ULL;
        constexpr std::uint64_t g_root = 0xaf000000ULL;
        environment.memory().fill_incrementing(physical_page, 0x1000, 0x6d);
        bool configured = environment.reset();
        if (configured) {
            configured = test.mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(
                      guest_page, physical_page, g_root)
                : environment.map_sv39x4_4k(
                      guest_page, physical_page, g_root);
        }
        if (configured) {
            configured = environment.activate_two_stage_modes(
                memblock::ReferencePageMode::bare, test.mode,
                0, g_root, 0, static_cast<std::uint16_t>(71 + index));
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                      << "-configuration reason=" << environment.error()
                      << '\n';
            return 1;
        }
        environment.inject_ptw_response_error_after(
            test.clean_requests_before_error, test.denied, test.corrupt,
            test.corrupt_beat);
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        const std::uint64_t address = guest_page + 0x188;
        if (!test.store) {
            const memblock::LoadTransaction transaction{
                .address = address,
                .op = memblock::LoadOp::ld,
                .rob = 0,
                .lq = 0,
                .pdest = static_cast<std::uint8_t>(196 + index),
                .lane = static_cast<unsigned>(
                    index % memblock::kScalarLoadLanes),
                .expected_exception_mask =
                    memblock::kExceptionLoadAccessFault,
            };
            environment.expect_load(transaction);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !environment.enqueue_load(transaction) ||
                !environment.issue_load(transaction, 2048) ||
                !environment.run_until_complete(16384) ||
                !environment.run_until_lq_retired(4096)) {
                std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                          << "-execution reason=" << environment.error()
                          << " ptw=" << environment.ptw_requests()
                          << " error_address=0x" << std::hex
                          << environment.ptw_last_error_response_address()
                          << std::dec << " error_source="
                          << static_cast<unsigned>(
                                 environment.ptw_last_error_response_source())
                          << '\n';
                return 1;
            }
            ++loads;
        } else {
            const memblock::StoreTransaction transaction{
                .address = address,
                .data = 0x2030405060708090ULL + index,
                .op = memblock::StoreOp::sd,
                .rob = 0,
                .sq = 0,
                .address_lane = static_cast<unsigned>(
                    index % memblock::kScalarStoreLanes),
                .data_lane = static_cast<unsigned>(
                    (index + 1) % memblock::kScalarStoreLanes),
                .expected_exception_mask =
                    memblock::kExceptionStoreAccessFault,
            };
            environment.expect_store(transaction);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !environment.enqueue_store(transaction, 0) ||
                !environment.issue_store_address(transaction, 2048) ||
                !environment.issue_store_data(transaction, 2048) ||
                !environment.run_until_store_complete_with_replay(
                    transaction, 16384) ||
                (environment.sq_dequeued() + environment.sq_canceled() <
                     environment.sq_allocated() &&
                 !environment.account_sq_cancellation(1)) ||
                environment.sq_dequeued() + environment.sq_canceled() !=
                    environment.sq_allocated()) {
                std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                          << "-execution reason=" << environment.error()
                          << " ptw=" << environment.ptw_requests()
                          << " error_address=0x" << std::hex
                          << environment.ptw_last_error_response_address()
                          << std::dec << " error_source="
                          << static_cast<unsigned>(
                                 environment.ptw_last_error_response_source())
                          << '\n';
                return 1;
            }
            ++stores;
        }
        const std::uint64_t expected_requests =
            test.clean_requests_before_error + 1;
        if (environment.ptw_error_response_requests() != 1 ||
            environment.ptw_requests() != expected_requests ||
            environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                      << "-side-effects injected="
                      << environment.ptw_error_response_requests()
                      << " ptw=" << environment.ptw_requests() << '/'
                      << expected_requests << '\n';
            return 1;
        }
        if (!run_clean_recovery(
                environment, test.name, address, physical_page + 0x188,
                test.store, static_cast<unsigned>(cases.size() + index),
                dcache_before, uncache_before, false, true)) {
            return 1;
        }
        denied += test.denied;
        corrupt += test.corrupt;
        first_beat_corrupt += test.corrupt &&
            test.corrupt_beat == memblock::PtwCorruptBeat::first;
        last_beat_corrupt += test.corrupt &&
            test.corrupt_beat == memblock::PtwCorruptBeat::last;
        root += test.clean_requests_before_error == 0;
        leaf += test.clean_requests_before_error + 1 ==
            memblock::reference_page_levels(test.mode);
        intermediate += test.clean_requests_before_error != 0 &&
            test.clean_requests_before_error + 1 !=
                memblock::reference_page_levels(test.mode);
        ++stage2_cases;
        total_cycles += environment.cycle();
        total_ptw_requests += environment.ptw_requests();
    }

    struct NestedCase {
        const char *name;
        memblock::ReferencePageMode vs_mode;
        memblock::ReferencePageMode g_mode;
        bool store;
        bool denied;
        bool corrupt;
        unsigned clean_requests_before_error;
        memblock::PtwCorruptBeat corrupt_beat;
    };
    constexpr std::array<NestedCase, 2> nested_tests{{
        {"sv39-sv39x4-g-root-denied", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv39, false, true, false, 0,
         memblock::PtwCorruptBeat::all},
        {"sv48-sv48x4-vs-root-corrupt", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv48, true, false, true, 4,
         memblock::PtwCorruptBeat::first},
    }};
    for (std::size_t index = 0; index < nested_tests.size(); ++index) {
        const auto &test = nested_tests[index];
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_physical = 0x120000000ULL;
        constexpr std::uint64_t host_physical = 0xc0000000ULL;
        constexpr std::uint64_t vs_root = 0x94000000ULL;
        constexpr std::uint64_t g_root = 0x95000000ULL;
        const std::uint64_t virtual_page =
            test.vs_mode == memblock::ReferencePageMode::sv48
                ? 0xffff800012340000ULL : 0x60000000ULL;
        environment.memory().fill_incrementing(host_physical, 0x1000, 0x7d);
        bool configured = environment.reset();
        if (configured) {
            configured = test.vs_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48_4k(
                      virtual_page, guest_physical, vs_root)
                : environment.map_sv39_4k(
                      virtual_page, guest_physical, vs_root);
        }
        const unsigned vs_table_pages =
            memblock::reference_page_levels(test.vs_mode);
        for (unsigned page = 0; configured && page < vs_table_pages; ++page) {
            const std::uint64_t table_page = vs_root + page * 0x1000ULL;
            configured = test.g_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(
                      table_page, table_page, g_root)
                : environment.map_sv39x4_4k(
                      table_page, table_page, g_root);
        }
        if (configured) {
            configured = test.g_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(
                      guest_physical, host_physical, g_root)
                : environment.map_sv39x4_4k(
                      guest_physical, host_physical, g_root);
        }
        if (configured) {
            configured = environment.activate_two_stage_modes(
                test.vs_mode, test.g_mode, vs_root, g_root,
                static_cast<std::uint16_t>(81 + index),
                static_cast<std::uint16_t>(83 + index));
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                      << "-configuration reason=" << environment.error()
                      << '\n';
            return 1;
        }
        environment.inject_ptw_response_error_after(
            test.clean_requests_before_error, test.denied, test.corrupt,
            test.corrupt_beat);
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        const std::uint64_t address = virtual_page + 0x188;
        if (!test.store) {
            const memblock::LoadTransaction transaction{
                .address = address,
                .op = memblock::LoadOp::ld,
                .rob = 0,
                .lq = 0,
                .pdest = static_cast<std::uint8_t>(204 + index),
                .lane = static_cast<unsigned>(index),
                .expected_exception_mask =
                    memblock::kExceptionLoadAccessFault,
            };
            environment.expect_load(transaction);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !environment.enqueue_load(transaction) ||
                !environment.issue_load(transaction, 2048) ||
                !environment.run_until_complete(32768) ||
                !environment.run_until_lq_retired(4096)) {
                std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                          << "-execution reason=" << environment.error()
                          << " ptw=" << environment.ptw_requests()
                          << " error_address=0x" << std::hex
                          << environment.ptw_last_error_response_address()
                          << std::dec << " error_source="
                          << static_cast<unsigned>(
                                 environment.ptw_last_error_response_source())
                          << '\n';
                return 1;
            }
            ++loads;
        } else {
            const memblock::StoreTransaction transaction{
                .address = address,
                .data = 0x30405060708090a0ULL + index,
                .op = memblock::StoreOp::sd,
                .rob = 0,
                .sq = 0,
                .address_lane = 0,
                .data_lane = 1,
                .expected_exception_mask =
                    memblock::kExceptionStoreAccessFault,
            };
            environment.expect_store(transaction);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !environment.enqueue_store(transaction, 0) ||
                !environment.issue_store_address(transaction, 2048) ||
                !environment.issue_store_data(transaction, 2048) ||
                !environment.run_until_store_complete_with_replay(
                    transaction, 32768) ||
                (environment.sq_dequeued() + environment.sq_canceled() <
                     environment.sq_allocated() &&
                 !environment.account_sq_cancellation(1)) ||
                environment.sq_dequeued() + environment.sq_canceled() !=
                    environment.sq_allocated()) {
                std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                          << "-execution reason=" << environment.error()
                          << " ptw=" << environment.ptw_requests()
                          << " error_address=0x" << std::hex
                          << environment.ptw_last_error_response_address()
                          << std::dec << " error_source="
                          << static_cast<unsigned>(
                                 environment.ptw_last_error_response_source())
                          << '\n';
                return 1;
            }
            ++stores;
        }
        const std::uint64_t expected_requests =
            test.clean_requests_before_error + 1;
        if (environment.ptw_error_response_requests() != 1 ||
            environment.ptw_requests() != expected_requests ||
            environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=" << test.name
                      << "-side-effects injected="
                      << environment.ptw_error_response_requests()
                      << " ptw=" << environment.ptw_requests() << '/'
                      << expected_requests << '\n';
            return 1;
        }
        if (!run_clean_recovery(
                environment, test.name, address, host_physical + 0x188,
                test.store,
                static_cast<unsigned>(
                    cases.size() + stage2_tests.size() + index),
                dcache_before, uncache_before, index != 0, index == 0,
                index == 0 ? 2 : 1)) {
            return 1;
        }
        denied += test.denied;
        corrupt += test.corrupt;
        first_beat_corrupt += test.corrupt &&
            test.corrupt_beat == memblock::PtwCorruptBeat::first;
        last_beat_corrupt += test.corrupt &&
            test.corrupt_beat == memblock::PtwCorruptBeat::last;
        ++nested_cases;
        total_cycles += environment.cycle();
        total_ptw_requests += environment.ptw_requests();
    }

    for (unsigned index = 0; index < 2; ++index) {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_page = 0x5b000000ULL;
        constexpr std::uint64_t physical_page = 0x91000000ULL;
        constexpr std::uint64_t root = 0x97100000ULL;
        constexpr std::uint64_t bitmap_base = 0x98100000ULL;
        constexpr std::uint64_t policy_word =
            memblock::reference_bitmap_word_address(
                bitmap_base, physical_page);
        environment.memory().fill_incrementing(physical_page, 0x1000, 0x8d);
        environment.memory().write_u64(policy_word, 0);
        if (!environment.reset() ||
            !environment.map_sv39_4k(
                virtual_page, physical_page, root) ||
            !environment.activate_sv39(root, 91 + index) ||
            !environment.set_mbmc(true, false, bitmap_base)) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=bitmap-"
                      << index << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const bool store = index != 0;
        const bool denied_response = index == 0;
        const bool corrupt_response = index != 0;
        const memblock::PtwCorruptBeat corrupt_beat = index == 0
            ? memblock::PtwCorruptBeat::all
            : memblock::PtwCorruptBeat::last;
        const std::uint64_t ptw_before = environment.ptw_requests();
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        environment.inject_ptw_response_error_after(
            3, denied_response, corrupt_response, corrupt_beat);
        if (!store) {
            const memblock::LoadTransaction transaction{
                .address = virtual_page + 0x188,
                .op = memblock::LoadOp::ld,
                .rob = 0,
                .lq = 0,
                .pdest = 212,
                .lane = 0,
                .expected_exception_mask =
                    memblock::kExceptionLoadAccessFault,
            };
            environment.expect_load(transaction);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !environment.enqueue_load(transaction) ||
                !environment.issue_load(transaction, 4096) ||
                !environment.run_until_complete(32768) ||
                !environment.run_until_lq_retired(8192)) {
                std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=bitmap-load"
                          << "-execution reason=" << environment.error()
                          << '\n';
                return 1;
            }
            ++loads;
        } else {
            const memblock::StoreTransaction transaction{
                .address = virtual_page + 0x188,
                .data = 0x405060708090a0b0ULL,
                .op = memblock::StoreOp::sd,
                .rob = 0,
                .sq = 0,
                .address_lane = 0,
                .data_lane = 1,
                .expected_exception_mask =
                    memblock::kExceptionStoreAccessFault,
            };
            environment.expect_store(transaction);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !environment.enqueue_store(transaction, 0) ||
                !environment.issue_store_address(transaction, 4096) ||
                !environment.issue_store_data(transaction, 4096) ||
                !environment.run_until_store_complete_with_replay(
                    transaction, 32768) ||
                (environment.sq_dequeued() + environment.sq_canceled() <
                     environment.sq_allocated() &&
                 !environment.account_sq_cancellation(1)) ||
                environment.sq_dequeued() + environment.sq_canceled() !=
                    environment.sq_allocated()) {
                std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=bitmap-store"
                          << "-execution reason=" << environment.error()
                          << '\n';
                return 1;
            }
            ++stores;
        }
        if (environment.ptw_error_response_requests() != 1 ||
            environment.ptw_requests() != ptw_before + 4 ||
            environment.ptw_requests_covering_since(
                policy_word, ptw_before) != 1 ||
            environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_PTW_ERRORS_FAIL phase=bitmap-" << index
                      << "-side-effects injected="
                      << environment.ptw_error_response_requests()
                      << " ptw=" << environment.ptw_requests() << '/'
                      << ptw_before + 4 << " bitmap="
                      << environment.ptw_requests_covering_since(
                             policy_word, ptw_before) << '\n';
            return 1;
        }
        if (!run_clean_recovery(
                environment, index == 0 ? "bitmap-load" : "bitmap-store",
                virtual_page + 0x188, physical_page + 0x188, store,
                static_cast<unsigned>(
                    cases.size() + stage2_tests.size() +
                    nested_tests.size() + index),
                dcache_before, uncache_before, false, false)) {
            return 1;
        }
        denied += denied_response;
        corrupt += corrupt_response;
        first_beat_corrupt += corrupt_response &&
            corrupt_beat == memblock::PtwCorruptBeat::first;
        last_beat_corrupt += corrupt_response &&
            corrupt_beat == memblock::PtwCorruptBeat::last;
        ++bitmap_cases;
        total_cycles += environment.cycle();
        total_ptw_requests += environment.ptw_requests() - ptw_before;
    }

    std::cout << "MEMBLOCK_PTW_ERRORS_PASS"
              << " cases="
              << cases.size() + stage2_cases + nested_cases + bitmap_cases
              << " stage1=" << cases.size()
              << " stage2=" << stage2_cases
              << " nested=" << nested_cases
              << " bitmap=" << bitmap_cases
              << " loads=" << loads
              << " stores=" << stores
              << " denied=" << denied
              << " corrupt=" << corrupt
              << " first_beat_corrupt=" << first_beat_corrupt
              << " last_beat_corrupt=" << last_beat_corrupt
              << " clean_recoveries=" << clean_recoveries
              << " levels=" << root << ',' << intermediate << ',' << leaf
              << " ptw_requests=" << total_ptw_requests
              << " cycles=" << total_cycles
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_faults(int argc, char **argv)
{
    unsigned canonical_boundary_cases = 0;
    unsigned ppn_access_fault_cases = 0;
    unsigned stage1_ppn_access_fault_cases = 0;
    unsigned vs_only_ppn_access_fault_cases = 0;
    unsigned gstage_ppn_access_fault_cases = 0;
    unsigned leaf_ppn_access_fault_cases = 0;
    unsigned nonleaf_ppn_access_fault_cases = 0;
    unsigned intermediate_ppn_access_fault_cases = 0;
    unsigned root_ppn_access_fault_cases = 0;
    unsigned lowest_high_ppn_cases = 0;
    unsigned highest_high_ppn_cases = 0;
    unsigned vs_gpa_width_cases = 0;
    unsigned sv39x4_gpa_width_cases = 0;
    unsigned sv48x4_gpa_width_cases = 0;
    unsigned vs_gpa_leaf_cases = 0;
    unsigned vs_gpa_intermediate_cases = 0;
    unsigned vs_gpa_root_cases = 0;
    std::uint64_t noncanonical_ptw_requests = 0;
    std::uint64_t ppn_access_fault_ptw_requests = 0;
    std::uint64_t vs_only_ppn_access_fault_ptw_requests = 0;
    std::uint64_t vs_gpa_width_ptw_requests = 0;
    struct CanonicalAddressCase {
        const char *name;
        memblock::ReferencePageMode mode;
        std::uint64_t address;
    };
    constexpr std::array<CanonicalAddressCase, 2> canonical_addresses{{
        {"sv39-high", memblock::ReferencePageMode::sv39,
         0xffffffc040000188ULL},
        {"sv48-high", memblock::ReferencePageMode::sv48,
         0xffff800040000188ULL},
    }};
    for (std::size_t index = 0; index < canonical_addresses.size(); ++index) {
        const auto &test = canonical_addresses[index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t physical = 0xca000000ULL + index * 0x200000ULL;
        const std::uint64_t root = 0x9b000000ULL + index * 0x100000ULL;
        environment.memory().fill_incrementing(physical, 0x1000, 0x21);
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=" << test.name
                      << "-reset reason=" << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t virtual_page = test.address & ~std::uint64_t{0xfff};
        const bool mapped = test.mode == memblock::ReferencePageMode::sv48
            ? environment.map_sv48_4k(virtual_page, physical, root)
            : environment.map_sv39_4k(virtual_page, physical, root);
        const auto mapped_reference = memblock::reference_page_walk(
            environment.memory(), root, test.address, test.mode);
        const memblock::LoadTransaction transaction{
            .address = test.address,
            .oracle_address = physical + (test.address & 0xfff),
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(140 + index),
            .lane = static_cast<unsigned>(index),
        };
        const bool activated = test.mode == memblock::ReferencePageMode::sv48
            ? environment.activate_sv48(root, 23 + index)
            : environment.activate_sv39(root, 23 + index);
        if (!mapped || !mapped_reference.translated || !activated) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=" << test.name
                      << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 1024) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=" << test.name
                      << "-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++canonical_boundary_cases;
    }

    constexpr std::array<CanonicalAddressCase, 4> noncanonical_addresses{{
        {"sv39-upper-zero-sign-one", memblock::ReferencePageMode::sv39,
         0x0000004000000188ULL},
        {"sv39-upper-one-sign-zero", memblock::ReferencePageMode::sv39,
         0xffffff8000000188ULL},
        {"sv48-upper-zero-sign-one", memblock::ReferencePageMode::sv48,
         0x0000800012340188ULL},
        {"sv48-upper-one-sign-zero", memblock::ReferencePageMode::sv48,
         0xffff000012340188ULL},
    }};
    for (std::size_t index = 0; index < noncanonical_addresses.size(); ++index) {
        const auto &test = noncanonical_addresses[index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t root = 0xa0000000ULL + index * 0x100000ULL;
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=" << test.name
                      << "-reset reason=" << environment.error() << '\n';
            return 1;
        }
        const auto reference = memblock::reference_page_walk(
            environment.memory(), root, test.address, test.mode);
        const bool activated = test.mode == memblock::ReferencePageMode::sv48
            ? environment.activate_sv48(root, 27 + index)
            : environment.activate_sv39(root, 27 + index);
        if (reference.translated || !activated) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=" << test.name
                      << "-configuration reason=" << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t ptw_before = environment.ptw_requests();
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        const memblock::LoadTransaction load{
            .address = test.address,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(142 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        environment.expect_load(load);
        if (!environment.set_rob_head(load.rob, load.rob_flag) ||
            !environment.enqueue_load(load) ||
            !environment.issue_load(load, 1024) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096) ||
            environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=" << test.name
                      << "-load reason="
                      << (environment.error().empty()
                              ? "noncanonical load reached a memory manager"
                              : environment.error()) << '\n';
            return 1;
        }
        ++canonical_boundary_cases;

        const memblock::StoreTransaction store{
            .address = test.address,
            .data = 0x8877665544332211ULL ^ index,
            .op = memblock::StoreOp::sd,
            .rob = 1,
            .sq = 0,
            .address_lane = static_cast<unsigned>(
                index % memblock::kScalarStoreLanes),
            .data_lane = static_cast<unsigned>(
                (index + 1) % memblock::kScalarStoreLanes),
            .expected_exception_mask = memblock::kExceptionStorePageFault,
        };
        environment.expect_store(store);
        if (!environment.set_rob_head(store.rob, store.rob_flag) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 2048) ||
            !environment.issue_store_data(store, 2048) ||
            !environment.run_until_store_complete_with_replay(store, 16384) ||
            environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before ||
            (environment.sq_dequeued() + environment.sq_canceled() <
                 environment.sq_allocated() &&
             !environment.account_sq_cancellation(1)) ||
            environment.sq_dequeued() + environment.sq_canceled() !=
                environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=" << test.name
                      << "-store reason="
                      << (environment.error().empty()
                              ? "noncanonical store reached memory or unbalanced SQ"
                              : environment.error())
                      << " ptw=" << ptw_before << '/' << environment.ptw_requests()
                      << " dcache=" << dcache_before << '/'
                      << environment.tilelink_requests()
                      << " uncache=" << uncache_before << '/'
                      << environment.uncache_requests()
                      << " sq=" << environment.sq_dequeued() << '+'
                      << environment.sq_canceled() << '/'
                      << environment.sq_allocated() << '\n';
            return 1;
        }
        noncanonical_ptw_requests += environment.ptw_requests() - ptw_before;
        ++canonical_boundary_cases;
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x68000000ULL;
        constexpr std::uint64_t high_guest_physical = 0x20000000000ULL;
        constexpr std::uint64_t vs_root = 0x9c000000ULL;
        constexpr std::uint64_t g_root = 0x9d000000ULL;
        // Sv39x4 has a 41-bit GPA.  The VS leaf is valid, but its GPA is one
        // bit above that range, so the fault must be a G-stage guest fault.
        if (!environment.reset() ||
            !environment.map_sv39_4k(
                guest_virtual, high_guest_physical, vs_root) ||
            !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
            !environment.map_sv39x4_4k(
                vs_root + 0x1000, vs_root + 0x1000, g_root) ||
            !environment.map_sv39x4_4k(
                vs_root + 0x2000, vs_root + 0x2000, g_root) ||
            !environment.activate_two_stage(
                vs_root, g_root, 25, 27)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, guest_virtual + 0x188);
        if (reference.translated || !reference.guest_page_fault ||
            reference.faulting_guest_physical_address != high_guest_physical + 0x188) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-reference\n";
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = guest_virtual + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 141,
            .lane = 1,
            .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
        };
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x20000000ULL;
        constexpr std::uint64_t misaligned_physical = 0xc4001000ULL;
        constexpr std::uint64_t root = 0x9f000000ULL;
        constexpr std::uint64_t l1 = root + 0x1000ULL;
        // A level-1 Sv39 leaf must describe a 2 MiB-aligned physical base.
        // Keep the low PPN bits set to make the DUT reject this malformed
        // superpage instead of silently masking them.
        constexpr std::uint64_t pte_valid = 1ULL << 0;
        constexpr std::uint64_t pte_read = 1ULL << 1;
        constexpr std::uint64_t pte_write = 1ULL << 2;
        constexpr std::uint64_t pte_accessed = 1ULL << 6;
        constexpr std::uint64_t pte_dirty = 1ULL << 7;
        constexpr std::uint64_t l2_index = 0;
        constexpr std::uint64_t l1_index = (virtual_address >> 21) & 0x1ffULL;
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=misaligned-superpage-reset reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.memory().write_u64(
            root + l2_index * 8,
            ((l1 >> 12) << 10) | pte_valid);
        environment.memory().write_u64(
            l1 + l1_index * 8,
            ((misaligned_physical >> 12) << 10) |
                pte_valid | pte_read | pte_write | pte_accessed | pte_dirty);
        const auto reference = memblock::reference_page_walk(
            environment.memory(), root, virtual_address + 0x188,
            memblock::ReferencePageMode::sv39);
        if (reference.translated || reference.faulting_pte_address == 0 ||
            reference.fault_level != 1) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=misaligned-superpage-reference\n";
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 142,
            .lane = 2,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        if (!environment.activate_sv39(root, 29)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=misaligned-superpage-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=misaligned-superpage-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x72000000ULL;
        constexpr std::uint64_t root = 0xa3000000ULL;
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=invalid-pte-reset reason="
                      << environment.error() << '\n';
            return 1;
        }
        const auto reference = memblock::reference_page_walk(
            environment.memory(), root, virtual_address + 0x188,
            memblock::ReferencePageMode::sv39);
        if (reference.translated || reference.faulting_pte_address == 0 ||
            reference.fault_level != 2) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=invalid-pte-reference\n";
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 143,
            .lane = 0,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        if (!environment.activate_sv39(root, 37)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=invalid-pte-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=invalid-pte-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x6a000000ULL;
        constexpr std::uint64_t high_guest_physical = 0x4000000000000ULL;
        constexpr std::uint64_t vs_root = 0xa4000000ULL;
        constexpr std::uint64_t g_root = 0xa5000000ULL;
        if (!environment.reset() ||
            !environment.map_sv48_4k(
                guest_virtual, high_guest_physical, vs_root) ||
            // A four-level VS walk creates root plus three child tables.
            !environment.map_sv48x4_4k(vs_root, vs_root, g_root) ||
            !environment.map_sv48x4_4k(vs_root + 0x1000, vs_root + 0x1000, g_root) ||
            !environment.map_sv48x4_4k(vs_root + 0x2000, vs_root + 0x2000, g_root) ||
            !environment.map_sv48x4_4k(vs_root + 0x3000, vs_root + 0x3000, g_root) ||
            !environment.activate_two_stage_modes(
                memblock::ReferencePageMode::sv48,
                memblock::ReferencePageMode::sv48,
                vs_root, g_root, 39, 41)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-sv48-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, guest_virtual + 0x188,
            memblock::ReferencePageMode::sv48,
            memblock::ReferencePageMode::sv48);
        if (reference.translated || !reference.guest_page_fault ||
            reference.faulting_guest_physical_address != high_guest_physical + 0x188) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-sv48-reference\n";
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = guest_virtual + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 144,
            .lane = 1,
            .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
        };
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-sv48-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    struct PpnAccessFaultCase {
        const char *name;
        memblock::ReferencePageMode mode;
        bool gstage;
        unsigned target_level;
        unsigned ppn_bit;
        bool highest_ppn_bit;
    };
    constexpr std::array<PpnAccessFaultCase, 28> ppn_access_fault_tests{{
        {"stage1-sv39-l0-ppn-bit36", memblock::ReferencePageMode::sv39,
         false, 0, 36, false},
        {"stage1-sv39-l0-ppn-bit43", memblock::ReferencePageMode::sv39,
         false, 0, 43, true},
        {"stage1-sv39-l1-ppn-bit36", memblock::ReferencePageMode::sv39,
         false, 1, 36, false},
        {"stage1-sv39-l1-ppn-bit43", memblock::ReferencePageMode::sv39,
         false, 1, 43, true},
        {"stage1-sv39-l2-ppn-bit36", memblock::ReferencePageMode::sv39,
         false, 2, 36, false},
        {"stage1-sv39-l2-ppn-bit43", memblock::ReferencePageMode::sv39,
         false, 2, 43, true},
        {"stage1-sv48-l0-ppn-bit36", memblock::ReferencePageMode::sv48,
         false, 0, 36, false},
        {"stage1-sv48-l0-ppn-bit43", memblock::ReferencePageMode::sv48,
         false, 0, 43, true},
        {"stage1-sv48-l1-ppn-bit36", memblock::ReferencePageMode::sv48,
         false, 1, 36, false},
        {"stage1-sv48-l1-ppn-bit43", memblock::ReferencePageMode::sv48,
         false, 1, 43, true},
        {"stage1-sv48-l2-ppn-bit36", memblock::ReferencePageMode::sv48,
         false, 2, 36, false},
        {"stage1-sv48-l2-ppn-bit43", memblock::ReferencePageMode::sv48,
         false, 2, 43, true},
        {"stage1-sv48-l3-ppn-bit36", memblock::ReferencePageMode::sv48,
         false, 3, 36, false},
        {"stage1-sv48-l3-ppn-bit43", memblock::ReferencePageMode::sv48,
         false, 3, 43, true},
        {"gstage-sv39-l0-ppn-bit36", memblock::ReferencePageMode::sv39,
         true, 0, 36, false},
        {"gstage-sv39-l0-ppn-bit43", memblock::ReferencePageMode::sv39,
         true, 0, 43, true},
        {"gstage-sv39-l1-ppn-bit36", memblock::ReferencePageMode::sv39,
         true, 1, 36, false},
        {"gstage-sv39-l1-ppn-bit43", memblock::ReferencePageMode::sv39,
         true, 1, 43, true},
        {"gstage-sv39-l2-ppn-bit36", memblock::ReferencePageMode::sv39,
         true, 2, 36, false},
        {"gstage-sv39-l2-ppn-bit43", memblock::ReferencePageMode::sv39,
         true, 2, 43, true},
        {"gstage-sv48-l0-ppn-bit36", memblock::ReferencePageMode::sv48,
         true, 0, 36, false},
        {"gstage-sv48-l0-ppn-bit43", memblock::ReferencePageMode::sv48,
         true, 0, 43, true},
        {"gstage-sv48-l1-ppn-bit36", memblock::ReferencePageMode::sv48,
         true, 1, 36, false},
        {"gstage-sv48-l1-ppn-bit43", memblock::ReferencePageMode::sv48,
         true, 1, 43, true},
        {"gstage-sv48-l2-ppn-bit36", memblock::ReferencePageMode::sv48,
         true, 2, 36, false},
        {"gstage-sv48-l2-ppn-bit43", memblock::ReferencePageMode::sv48,
         true, 2, 43, true},
        {"gstage-sv48-l3-ppn-bit36", memblock::ReferencePageMode::sv48,
         true, 3, 36, false},
        {"gstage-sv48-l3-ppn-bit43", memblock::ReferencePageMode::sv48,
         true, 3, 43, true},
    }};
    for (std::size_t index = 0; index < ppn_access_fault_tests.size(); ++index) {
        const auto &test = ppn_access_fault_tests[index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t input_address = test.gstage
            ? 0x7a000188ULL
            : test.mode == memblock::ReferencePageMode::sv48
                ? 0xffff900020000188ULL
                : 0x78000188ULL;
        const std::uint64_t root = 0xb0000000ULL + index * 0x100000ULL;
        constexpr std::uint64_t valid_physical_address = 0xce000000ULL;
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-reset reason="
                      << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t input_page =
            input_address & ~std::uint64_t{0xfff};
        bool mapped = false;
        if (test.gstage) {
            mapped = test.mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(
                      input_page, valid_physical_address, root)
                : environment.map_sv39x4_4k(
                      input_page, valid_physical_address, root);
        } else {
            mapped = test.mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48_4k(
                      input_page, valid_physical_address, root)
                : environment.map_sv39_4k(
                      input_page, valid_physical_address, root);
        }
        const unsigned target_level = test.target_level;
        const auto fault_pte = memblock::reference_pte_address_at_level(
            environment.memory(), root, input_address, test.mode,
            target_level, test.gstage);
        if (mapped && fault_pte.has_value()) {
            environment.memory().write_u64(
                *fault_pte,
                environment.memory().read_u64(*fault_pte) |
                    (std::uint64_t{1} << (test.ppn_bit + 10)));
        }
        const auto reference = memblock::reference_page_walk(
            environment.memory(), root, input_address, test.mode,
            test.gstage);
        const auto two_stage_reference = test.gstage
            ? memblock::reference_two_stage_walk(
                  environment.memory(), 0, root, input_address,
                  memblock::ReferencePageMode::bare, test.mode)
            : memblock::ReferenceTwoStageWalkResult{};
        if (!mapped || !fault_pte.has_value() || reference.translated ||
            !reference.access_fault ||
            reference.fault_level != target_level ||
            reference.faulting_pte_address != *fault_pte ||
            (test.gstage &&
             (two_stage_reference.translated ||
              !two_stage_reference.access_fault ||
              two_stage_reference.guest_page_fault ||
              two_stage_reference.stage1_page_fault ||
              two_stage_reference.faulting_guest_physical_address !=
                  input_address))) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name
                      << "-reference reason=PPN access fault not identified\n";
            return 1;
        }
        const bool activated = test.gstage
            ? environment.activate_two_stage_modes(
                  memblock::ReferencePageMode::bare, test.mode,
                  0, root, 53 + index, 71 + index)
            : test.mode == memblock::ReferencePageMode::sv48
                ? environment.activate_sv48(root, 53 + index)
                : environment.activate_sv39(root, 53 + index);
        if (!activated) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }

        const std::uint64_t ptw_before = environment.ptw_requests();
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        const memblock::LoadTransaction load{
            .address = input_address,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(210 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            .expected_exception_mask = memblock::kExceptionLoadAccessFault,
        };
        environment.expect_load(load);
        if (!environment.set_rob_head(load.rob, load.rob_flag) ||
            !environment.enqueue_load(load) ||
            !environment.issue_load(load, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096) ||
            environment.ptw_requests() !=
                ptw_before +
                    memblock::reference_page_levels(test.mode) -
                    target_level ||
            environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-load reason="
                      << (environment.error().empty()
                              ? "wrong PTW count or data-manager side effect"
                              : environment.error())
                      << " ptw=" << ptw_before << '/'
                      << environment.ptw_requests()
                      << " dcache=" << dcache_before << '/'
                      << environment.tilelink_requests()
                      << " uncache=" << uncache_before << '/'
                      << environment.uncache_requests() << '\n';
            return 1;
        }
        ++ppn_access_fault_cases;
        if (test.gstage) {
            ++gstage_ppn_access_fault_cases;
        } else {
            ++stage1_ppn_access_fault_cases;
        }
        if (target_level != 0) {
            ++nonleaf_ppn_access_fault_cases;
        } else {
            ++leaf_ppn_access_fault_cases;
        }
        if (target_level == memblock::reference_page_levels(test.mode) - 1) {
            ++root_ppn_access_fault_cases;
        } else if (target_level != 0) {
            ++intermediate_ppn_access_fault_cases;
        }

        const std::uint64_t store_ptw_before = environment.ptw_requests();
        const memblock::StoreTransaction store{
            .address = input_address,
            .data = 0x5566778899aabbccULL ^ index,
            .op = memblock::StoreOp::sd,
            .rob = 1,
            .sq = 0,
            .address_lane = static_cast<unsigned>(
                index % memblock::kScalarStoreLanes),
            .data_lane = static_cast<unsigned>(
                (index + 1) % memblock::kScalarStoreLanes),
            .expected_exception_mask = memblock::kExceptionStoreAccessFault,
        };
        environment.expect_store(store);
        if (!environment.set_rob_head(store.rob, store.rob_flag) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 2048) ||
            !environment.issue_store_data(store, 2048) ||
            !environment.run_until_store_complete_with_replay(store, 16384) ||
            // Load/store L1 TLBs are distinct, while the shared page-table
            // cache retains valid non-leaf entries.  The store must therefore
            // re-read exactly the AF leaf PTE.
            environment.ptw_requests() != store_ptw_before + 1 ||
            environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before ||
            (environment.sq_dequeued() + environment.sq_canceled() <
                 environment.sq_allocated() &&
             !environment.account_sq_cancellation(1)) ||
            environment.sq_dequeued() + environment.sq_canceled() !=
                environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-store reason="
                      << (environment.error().empty()
                              ? "wrong PTW count, AF changed, reached memory, or left SQ unbalanced"
                              : environment.error())
                      << " ptw=" << store_ptw_before << '/'
                      << environment.ptw_requests()
                      << " dcache=" << dcache_before << '/'
                      << environment.tilelink_requests()
                      << " uncache=" << uncache_before << '/'
                      << environment.uncache_requests()
                      << " sq=" << environment.sq_dequeued() << '+'
                      << environment.sq_canceled() << '/'
                      << environment.sq_allocated() << '\n';
            return 1;
        }
        ++ppn_access_fault_cases;
        if (test.gstage) {
            ++gstage_ppn_access_fault_cases;
        } else {
            ++stage1_ppn_access_fault_cases;
        }
        if (target_level != 0) {
            ++nonleaf_ppn_access_fault_cases;
        } else {
            ++leaf_ppn_access_fault_cases;
        }
        if (target_level == memblock::reference_page_levels(test.mode) - 1) {
            ++root_ppn_access_fault_cases;
        } else if (target_level != 0) {
            ++intermediate_ppn_access_fault_cases;
        }
        ppn_access_fault_ptw_requests +=
            environment.ptw_requests() - ptw_before;
        if (test.highest_ppn_bit) {
            highest_high_ppn_cases += 2;
        } else {
            lowest_high_ppn_cases += 2;
        }
    }

    constexpr std::array<memblock::ReferencePageMode, 2> paged_modes{{
        memblock::ReferencePageMode::sv39,
        memblock::ReferencePageMode::sv48,
    }};
    unsigned vs_only_ppn_case_index = 0;
    for (const auto mode : paged_modes) {
        const unsigned levels = memblock::reference_page_levels(mode);
        constexpr std::array<unsigned, 2> ppn_bits{{36U, 43U}};
        for (unsigned target_level = 0; target_level < levels;
             ++target_level) {
            for (const unsigned ppn_bit : ppn_bits) {
                memblock::Environment environment(argc, argv);
                const std::uint64_t input_address =
                    mode == memblock::ReferencePageMode::sv48
                        ? 0xffff900060000188ULL
                        : 0x7e000188ULL;
                constexpr std::uint64_t valid_physical_address =
                    0xcf000000ULL;
                constexpr std::uint64_t vs_root = 0xe0000000ULL;
                bool configured = environment.reset();
                if (configured) {
                    configured = mode == memblock::ReferencePageMode::sv48
                        ? environment.map_sv48_4k(
                              input_address & ~std::uint64_t{0xfff},
                              valid_physical_address, vs_root)
                        : environment.map_sv39_4k(
                              input_address & ~std::uint64_t{0xfff},
                              valid_physical_address, vs_root);
                }
                const auto fault_pte =
                    memblock::reference_pte_address_at_level(
                        environment.memory(), vs_root, input_address,
                        mode, target_level);
                std::uint64_t expected_fault_address = 0;
                if (configured && fault_pte.has_value()) {
                    const std::uint64_t overflow_pte =
                        environment.memory().read_u64(*fault_pte) |
                        (std::uint64_t{1} << (ppn_bit + 10));
                    environment.memory().write_u64(*fault_pte, overflow_pte);
                    expected_fault_address = memblock::reference_leaf_address(
                        overflow_pte, input_address, target_level);
                }
                const auto stage_reference = memblock::reference_page_walk(
                    environment.memory(), vs_root, input_address, mode);
                const auto nested_reference =
                    memblock::reference_two_stage_walk(
                        environment.memory(), vs_root, 0, input_address,
                        mode, memblock::ReferencePageMode::bare);
                if (!configured || !fault_pte.has_value() ||
                    stage_reference.translated ||
                    !stage_reference.access_fault ||
                    stage_reference.fault_level != target_level ||
                    stage_reference.faulting_pte_address != *fault_pte ||
                    nested_reference.translated ||
                    !nested_reference.access_fault ||
                    nested_reference.guest_page_fault ||
                    nested_reference.stage1_page_fault ||
                    nested_reference.is_for_vs_nonleaf_pte ||
                    nested_reference.faulting_guest_physical_address !=
                        expected_fault_address) {
                    std::cerr
                        << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=vs-only-ppn-reference"
                        << " case=" << vs_only_ppn_case_index
                        << " mode=" << static_cast<unsigned>(mode)
                        << " level=" << target_level
                        << " ppn_bit=" << ppn_bit << '\n';
                    return 1;
                }
                if (!environment.activate_two_stage_modes(
                        mode, memblock::ReferencePageMode::bare,
                        vs_root, 0, 181 + vs_only_ppn_case_index, 0)) {
                    std::cerr
                        << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=vs-only-ppn-configuration"
                        << " case=" << vs_only_ppn_case_index
                        << " reason=" << environment.error() << '\n';
                    return 1;
                }

                const std::uint64_t ptw_before = environment.ptw_requests();
                const std::uint64_t dcache_before =
                    environment.tilelink_requests();
                const std::uint64_t uncache_before =
                    environment.uncache_requests();
                const memblock::LoadTransaction load{
                    .address = input_address,
                    .op = memblock::LoadOp::ld,
                    .rob = 0,
                    .lq = 0,
                    .pdest = static_cast<std::uint8_t>(
                        100 + vs_only_ppn_case_index),
                    .lane = static_cast<unsigned>(
                        vs_only_ppn_case_index % memblock::kScalarLoadLanes),
                    .expected_exception_mask =
                        memblock::kExceptionLoadAccessFault,
                };
                environment.expect_load(load);
                if (!environment.set_rob_head(load.rob, load.rob_flag) ||
                    !environment.enqueue_load(load) ||
                    !environment.issue_load(load, 2048) ||
                    !environment.run_until_complete(16384) ||
                    !environment.run_until_lq_retired(4096) ||
                    environment.ptw_requests() !=
                        ptw_before + levels - target_level ||
                    environment.tilelink_requests() != dcache_before ||
                    environment.uncache_requests() != uncache_before ||
                    environment.exception_vaddr() != input_address ||
                    environment.exception_is_for_vs_nonleaf_pte()) {
                    std::cerr
                        << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=vs-only-ppn-load"
                        << " case=" << vs_only_ppn_case_index
                        << " reason="
                        << (environment.error().empty()
                                ? "wrong fault, PTW count, metadata, or data-manager side effect"
                                : environment.error())
                        << " ptw=" << ptw_before << '/'
                        << environment.ptw_requests()
                        << " vs_nonleaf="
                        << environment.exception_is_for_vs_nonleaf_pte()
                        << '\n';
                    return 1;
                }
                ++ppn_access_fault_cases;
                ++vs_only_ppn_access_fault_cases;

                const std::uint64_t store_ptw_before =
                    environment.ptw_requests();
                const memblock::StoreTransaction store{
                    .address = input_address,
                    .data = 0x8899aabbccddeeffULL ^ vs_only_ppn_case_index,
                    .op = memblock::StoreOp::sd,
                    .rob = 1,
                    .sq = 0,
                    .address_lane = static_cast<unsigned>(
                        vs_only_ppn_case_index % memblock::kScalarStoreLanes),
                    .data_lane = static_cast<unsigned>(
                        (vs_only_ppn_case_index + 1) %
                        memblock::kScalarStoreLanes),
                    .expected_exception_mask =
                        memblock::kExceptionStoreAccessFault,
                };
                environment.expect_store(store);
                if (!environment.set_rob_head(store.rob, store.rob_flag) ||
                    !environment.enqueue_store(store, 0) ||
                    !environment.issue_store_address(store, 2048) ||
                    !environment.issue_store_data(store, 2048) ||
                    !environment.run_until_store_complete_with_replay(
                        store, 16384) ||
                    environment.ptw_requests() != store_ptw_before + 1 ||
                    environment.tilelink_requests() != dcache_before ||
                    environment.uncache_requests() != uncache_before ||
                    (environment.sq_dequeued() + environment.sq_canceled() <
                         environment.sq_allocated() &&
                     !environment.account_sq_cancellation(1)) ||
                    environment.sq_dequeued() + environment.sq_canceled() !=
                        environment.sq_allocated()) {
                    std::cerr
                        << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=vs-only-ppn-store"
                        << " case=" << vs_only_ppn_case_index
                        << " reason="
                        << (environment.error().empty()
                                ? "wrong fault, PTW count, data-manager side effect, or SQ accounting"
                                : environment.error())
                        << " ptw=" << store_ptw_before << '/'
                        << environment.ptw_requests() << '\n';
                    return 1;
                }
                ++ppn_access_fault_cases;
                ++vs_only_ppn_access_fault_cases;
                if (target_level == 0) {
                    leaf_ppn_access_fault_cases += 2;
                } else {
                    nonleaf_ppn_access_fault_cases += 2;
                    if (target_level == levels - 1) {
                        root_ppn_access_fault_cases += 2;
                    } else {
                        intermediate_ppn_access_fault_cases += 2;
                    }
                }
                if (ppn_bit == 43) {
                    highest_high_ppn_cases += 2;
                } else {
                    lowest_high_ppn_cases += 2;
                }
                const std::uint64_t request_delta =
                    environment.ptw_requests() - ptw_before;
                ppn_access_fault_ptw_requests += request_delta;
                vs_only_ppn_access_fault_ptw_requests += request_delta;
                ++vs_only_ppn_case_index;
            }
        }
    }
    unsigned vs_gpa_case_index = 0;
    for (const auto vs_mode : paged_modes) {
        for (const auto g_mode : paged_modes) {
            const unsigned levels = memblock::reference_page_levels(vs_mode);
            const unsigned lowest_invalid_ppn_bit =
                g_mode == memblock::ReferencePageMode::sv48 ? 38U : 29U;
            const std::array<unsigned, 2> ppn_bits{{
                lowest_invalid_ppn_bit,
                43U,
            }};
            for (unsigned target_level = 0; target_level < levels;
                 ++target_level) {
                for (const unsigned ppn_bit : ppn_bits) {
                    memblock::Environment environment(argc, argv);
                    const std::uint64_t input_address =
                        vs_mode == memblock::ReferencePageMode::sv48
                            ? 0xffff900040000188ULL
                            : 0x7c000188ULL;
                    constexpr std::uint64_t valid_guest_physical =
                        0x7d000000ULL;
                    constexpr std::uint64_t vs_root = 0xd0000000ULL;
                    constexpr std::uint64_t g_root = 0xd4000000ULL;
                    bool configured = environment.reset();
                    if (configured) {
                        configured = vs_mode == memblock::ReferencePageMode::sv48
                            ? environment.map_sv48_4k(
                                  input_address & ~std::uint64_t{0xfff},
                                  valid_guest_physical, vs_root)
                            : environment.map_sv39_4k(
                                  input_address & ~std::uint64_t{0xfff},
                                  valid_guest_physical, vs_root);
                    }
                    const auto fault_pte =
                        memblock::reference_pte_address_at_level(
                            environment.memory(), vs_root, input_address,
                            vs_mode, target_level);
                    std::uint64_t expected_fault_gpa = 0;
                    if (configured && fault_pte.has_value()) {
                        const std::uint64_t overflow_pte =
                            environment.memory().read_u64(*fault_pte) |
                            (std::uint64_t{1} << (ppn_bit + 10));
                        environment.memory().write_u64(*fault_pte, overflow_pte);
                        expected_fault_gpa = memblock::reference_leaf_address(
                            overflow_pte, input_address, target_level);
                    }
                    const auto map_gstage = [&](std::uint64_t address) {
                        return g_mode == memblock::ReferencePageMode::sv48
                            ? environment.map_sv48x4_4k(
                                  address, address, g_root)
                            : environment.map_sv39x4_4k(
                                  address, address, g_root);
                    };
                    for (unsigned page = 0; configured && page < levels;
                         ++page) {
                        configured = map_gstage(
                            vs_root + page * 0x1000ULL);
                    }
                    const auto reference = memblock::reference_two_stage_walk(
                        environment.memory(), vs_root, g_root, input_address,
                        vs_mode, g_mode);
                    if (!configured || !fault_pte.has_value() ||
                        reference.translated || !reference.guest_page_fault ||
                        reference.stage1_page_fault || reference.access_fault ||
                        reference.is_for_vs_nonleaf_pte !=
                            (target_level != 0) ||
                        reference.faulting_guest_physical_address !=
                            expected_fault_gpa) {
                        std::cerr
                            << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=vs-gpa-width-reference"
                            << " case=" << vs_gpa_case_index
                            << " vs_mode=" << static_cast<unsigned>(vs_mode)
                            << " g_mode=" << static_cast<unsigned>(g_mode)
                            << " level=" << target_level
                            << " ppn_bit=" << ppn_bit << '\n';
                        return 1;
                    }
                    configured = environment.activate_two_stage_modes(
                        vs_mode, g_mode, vs_root, g_root,
                        91 + vs_gpa_case_index, 121 + vs_gpa_case_index);
                    if (!configured) {
                        std::cerr
                            << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=vs-gpa-width-configuration"
                            << " case=" << vs_gpa_case_index
                            << " reason=" << environment.error() << '\n';
                        return 1;
                    }

                    const std::uint64_t ptw_before = environment.ptw_requests();
                    // A fully nested walk first obtains the current VS PTE
                    // through G-stage and then re-accesses that block on the
                    // stage-1 result path that performs the GPA-width check.
                    constexpr std::uint64_t expected_fault_pte_reads = 2;
                    const std::uint64_t dcache_before =
                        environment.tilelink_requests();
                    const std::uint64_t uncache_before =
                        environment.uncache_requests();
                    const memblock::LoadTransaction load{
                        .address = input_address,
                        .op = memblock::LoadOp::ld,
                        .rob = 0,
                        .lq = 0,
                        .pdest = static_cast<std::uint8_t>(
                            32 + vs_gpa_case_index),
                        .lane = static_cast<unsigned>(
                            vs_gpa_case_index % memblock::kScalarLoadLanes),
                        .expected_exception_mask =
                            memblock::kExceptionLoadGuestPageFault,
                    };
                    environment.expect_load(load);
                    if (!environment.set_rob_head(load.rob, load.rob_flag) ||
                        !environment.enqueue_load(load) ||
                        !environment.issue_load(load, 4096) ||
                        !environment.run_until_complete(32768) ||
                        !environment.run_until_lq_retired(8192) ||
                        environment.ptw_requests() <= ptw_before ||
                        environment.ptw_requests_covering_since(
                            *fault_pte, ptw_before) !=
                            expected_fault_pte_reads ||
                        environment.tilelink_requests() != dcache_before ||
                        environment.uncache_requests() != uncache_before ||
                        environment.exception_vaddr() != input_address ||
                        environment.exception_is_for_vs_nonleaf_pte() !=
                            (target_level != 0)) {
                        std::cerr
                            << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=vs-gpa-width-load"
                            << " case=" << vs_gpa_case_index
                            << " reason="
                            << (environment.error().empty()
                                    ? "wrong fault, PTE traffic, metadata, or data-manager side effect"
                                    : environment.error())
                            << " pte_reads="
                            << environment.ptw_requests_covering_since(
                                   *fault_pte, ptw_before)
                            << " ptw=" << ptw_before << '/'
                            << environment.ptw_requests()
                            << " dcache=" << dcache_before << '/'
                            << environment.tilelink_requests()
                            << " uncache=" << uncache_before << '/'
                            << environment.uncache_requests()
                            << " vaddr=0x" << std::hex
                            << environment.exception_vaddr()
                            << "/0x" << input_address
                            << " vs_nonleaf=" << std::dec
                            << environment.exception_is_for_vs_nonleaf_pte()
                            << '\n';
                        return 1;
                    }
                    ++vs_gpa_width_cases;

                    const std::uint64_t store_ptw_before =
                        environment.ptw_requests();
                    const memblock::StoreTransaction store{
                        .address = input_address,
                        .data = 0x778899aabbccddeeULL ^ vs_gpa_case_index,
                        .op = memblock::StoreOp::sd,
                        .rob = 1,
                        .sq = 0,
                        .address_lane = static_cast<unsigned>(
                            vs_gpa_case_index % memblock::kScalarStoreLanes),
                        .data_lane = static_cast<unsigned>(
                            (vs_gpa_case_index + 1) %
                            memblock::kScalarStoreLanes),
                        .expected_exception_mask =
                            memblock::kExceptionStoreGuestPageFault,
                    };
                    environment.expect_store(store);
                    if (!environment.set_rob_head(store.rob, store.rob_flag) ||
                        !environment.enqueue_store(store, 0) ||
                        !environment.issue_store_address(store, 4096) ||
                        !environment.issue_store_data(store, 4096) ||
                        !environment.run_until_store_complete_with_replay(
                            store, 32768) ||
                        environment.ptw_requests() <= store_ptw_before ||
                        environment.ptw_requests_covering_since(
                            *fault_pte, store_ptw_before) !=
                            expected_fault_pte_reads ||
                        environment.tilelink_requests() != dcache_before ||
                        environment.uncache_requests() != uncache_before ||
                        (environment.sq_dequeued() + environment.sq_canceled() <
                             environment.sq_allocated() &&
                         !environment.account_sq_cancellation(1)) ||
                        environment.sq_dequeued() + environment.sq_canceled() !=
                            environment.sq_allocated()) {
                        std::cerr
                            << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=vs-gpa-width-store"
                            << " case=" << vs_gpa_case_index
                            << " reason="
                            << (environment.error().empty()
                                    ? "wrong fault, PTE traffic, data-manager side effect, or SQ accounting"
                                    : environment.error())
                            << " pte_reads="
                            << environment.ptw_requests_covering_since(
                                   *fault_pte, store_ptw_before)
                            << " ptw=" << store_ptw_before << '/'
                            << environment.ptw_requests() << '\n';
                        return 1;
                    }
                    ++vs_gpa_width_cases;
                    if (g_mode == memblock::ReferencePageMode::sv48) {
                        sv48x4_gpa_width_cases += 2;
                    } else {
                        sv39x4_gpa_width_cases += 2;
                    }
                    if (target_level == 0) {
                        vs_gpa_leaf_cases += 2;
                    } else if (target_level == levels - 1) {
                        vs_gpa_root_cases += 2;
                    } else {
                        vs_gpa_intermediate_cases += 2;
                    }
                    vs_gpa_width_ptw_requests +=
                        environment.ptw_requests() - ptw_before;
                    ++vs_gpa_case_index;
                }
            }
        }
    }

    struct PteEncodingCase {
        const char *name;
        memblock::ReferencePageMode mode;
        bool mutate_root;
        bool pbmte;
        std::uint64_t set_bits;
        std::uint64_t clear_bits;
    };
    constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
    constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
    constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
    constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
    constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
    constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
    constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
    constexpr std::uint64_t pte_reserved_54 = std::uint64_t{1} << 54;
    constexpr std::uint64_t pte_reserved_60 = std::uint64_t{1} << 60;
    constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
    constexpr std::uint64_t pte_pbmt_mask = std::uint64_t{3} << 61;
    constexpr std::uint64_t pte_napot = std::uint64_t{1} << 63;
    constexpr std::uint64_t pte_nonleaf_clear =
        pte_read | pte_write | pte_execute | pte_user | pte_accessed |
        pte_dirty | pte_reserved_54 | pte_reserved_60 | pte_pbmt_mask |
        pte_napot;
#define MEMBLOCK_PTE_ENCODING_CASES(mode_name) \
    {#mode_name "-leaf-v0", memblock::ReferencePageMode::mode_name, false, true, 0, pte_valid}, \
    {#mode_name "-leaf-w1-r0", memblock::ReferencePageMode::mode_name, false, true, pte_write, pte_read}, \
    {#mode_name "-leaf-reserved54", memblock::ReferencePageMode::mode_name, false, true, pte_reserved_54, 0}, \
    {#mode_name "-leaf-reserved60", memblock::ReferencePageMode::mode_name, false, true, pte_reserved_60, 0}, \
    {#mode_name "-leaf-pbmt3", memblock::ReferencePageMode::mode_name, false, true, pte_pbmt_mask, pte_pbmt_mask}, \
    {#mode_name "-leaf-pbmt-disabled", memblock::ReferencePageMode::mode_name, false, false, pte_pbmt_nc, pte_pbmt_mask}, \
    {#mode_name "-l0-nonleaf", memblock::ReferencePageMode::mode_name, false, true, pte_valid, pte_nonleaf_clear}, \
    {#mode_name "-nonleaf-u", memblock::ReferencePageMode::mode_name, true, true, pte_user, 0}, \
    {#mode_name "-nonleaf-a", memblock::ReferencePageMode::mode_name, true, true, pte_accessed, 0}, \
    {#mode_name "-nonleaf-d", memblock::ReferencePageMode::mode_name, true, true, pte_dirty, 0}, \
    {#mode_name "-nonleaf-pbmt", memblock::ReferencePageMode::mode_name, true, true, pte_pbmt_nc, 0}, \
    {#mode_name "-nonleaf-n", memblock::ReferencePageMode::mode_name, true, true, pte_napot, 0}, \
    {#mode_name "-leaf-napot-encoding", memblock::ReferencePageMode::mode_name, false, true, pte_napot, 0}
    const std::array<PteEncodingCase, 26> pte_encoding_cases{{
        MEMBLOCK_PTE_ENCODING_CASES(sv39),
        MEMBLOCK_PTE_ENCODING_CASES(sv48),
    }};
#undef MEMBLOCK_PTE_ENCODING_CASES

    for (std::size_t index = 0; index < pte_encoding_cases.size(); ++index) {
        const auto &test = pte_encoding_cases[index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t virtual_address =
            test.mode == memblock::ReferencePageMode::sv48
                ? 0xffff900000000000ULL
                : 0x74000000ULL;
        constexpr std::uint64_t physical_address = 0xcc000000ULL;
        constexpr std::uint64_t root = 0xaa000000ULL;
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-reset reason="
                      << environment.error() << '\n';
            return 1;
        }
        const bool mapped = test.mode == memblock::ReferencePageMode::sv48
            ? environment.map_sv48_4k(
                  virtual_address, physical_address, root)
            : environment.map_sv39_4k(
                  virtual_address, physical_address, root);
        const unsigned target_level = test.mutate_root
            ? memblock::reference_page_levels(test.mode) - 1
            : 0;
        const auto pte_address = memblock::reference_pte_address_at_level(
            environment.memory(), root, virtual_address + 0x188,
            test.mode, target_level);
        if (!mapped || !pte_address.has_value()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-setup reason="
                      << (environment.error().empty()
                              ? "reference PTE locator failed"
                              : environment.error())
                      << '\n';
            return 1;
        }
        const std::uint64_t original =
            environment.memory().read_u64(*pte_address);
        environment.memory().write_u64(
            *pte_address, (original & ~test.clear_bits) | test.set_bits);
        const auto reference = memblock::reference_page_walk(
            environment.memory(), root, virtual_address + 0x188,
            test.mode, false, test.pbmte);
        if (reference.translated ||
            reference.faulting_pte_address != *pte_address ||
            reference.fault_level != target_level) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-reference\n";
            return 1;
        }
        const bool activated = test.mode == memblock::ReferencePageMode::sv48
            ? environment.activate_sv48(root, 43)
            : environment.activate_sv39(root, 43);
        if (!activated ||
            !environment.set_page_based_memory_types(test.pbmte, true)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(145 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        if (environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name
                      << "-bypass reason=fault issued a data-manager request\n";
            return 1;
        }

        const std::uint64_t store_dcache_before =
            environment.tilelink_requests();
        const std::uint64_t store_uncache_before =
            environment.uncache_requests();
        const memblock::StoreTransaction store{
            .address = virtual_address + 0x188,
            .data = 0x1020304050607080ULL + index,
            .op = memblock::StoreOp::sd,
            .rob = 1,
            .sq = 0,
            .address_lane = static_cast<unsigned>(
                index % memblock::kScalarStoreLanes),
            .data_lane = static_cast<unsigned>(
                (index + 1) % memblock::kScalarStoreLanes),
            .expected_exception_mask = memblock::kExceptionStorePageFault,
        };
        environment.expect_store(store);
        if (!environment.set_rob_head(store.rob, store.rob_flag) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 2048) ||
            !environment.issue_store_data(store, 2048) ||
            !environment.run_until_store_complete_with_replay(store, 16384)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name << "-store-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        if (environment.tilelink_requests() != store_dcache_before ||
            environment.uncache_requests() != store_uncache_before ||
            (environment.sq_dequeued() + environment.sq_canceled() <
                 environment.sq_allocated() &&
             !environment.account_sq_cancellation(1)) ||
            environment.sq_dequeued() + environment.sq_canceled() !=
                environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase="
                      << test.name
                      << "-store-side-effect reason="
                      << (environment.error().empty()
                              ? "faulting store reached memory or unbalanced SQ"
                              : environment.error()) << '\n';
            return 1;
        }
    }

    for (std::size_t index = 0; index < pte_encoding_cases.size(); ++index) {
        const auto &test = pte_encoding_cases[index];
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x76000000ULL;
        // Keep the final data GPA in a different top-level x4 root entry from
        // the VS page-table GPAs. Root-PTE mutations then exercise the final
        // data translation instead of faulting an earlier implicit VS walk.
        constexpr std::uint64_t guest_physical = 0x8000000000ULL;
        constexpr std::uint64_t host_physical = 0xcd000000ULL;
        constexpr std::uint64_t vs_root = 0xac000000ULL;
        constexpr std::uint64_t g_root = 0xae000000ULL;
        if (!environment.reset() ||
            !environment.map_sv39_4k(
                guest_virtual, guest_physical, vs_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name << "-vs-setup reason="
                      << environment.error() << '\n';
            return 1;
        }
        const auto map_gstage = [&](std::uint64_t guest,
                                    std::uint64_t host) {
            return test.mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(guest, host, g_root)
                : environment.map_sv39x4_4k(guest, host, g_root);
        };
        if (!map_gstage(vs_root, vs_root) ||
            !map_gstage(vs_root + 0x1000, vs_root + 0x1000) ||
            !map_gstage(vs_root + 0x2000, vs_root + 0x2000) ||
            !map_gstage(guest_physical, host_physical)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name << "-g-setup reason="
                      << environment.error() << '\n';
            return 1;
        }
        const unsigned target_level = test.mutate_root
            ? memblock::reference_page_levels(test.mode) - 1
            : 0;
        const auto pte_address = memblock::reference_pte_address_at_level(
            environment.memory(), g_root, guest_physical + 0x188,
            test.mode, target_level, true);
        if (!pte_address.has_value()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name
                      << "-locator reason=reference PTE locator failed\n";
            return 1;
        }
        const std::uint64_t original =
            environment.memory().read_u64(*pte_address);
        environment.memory().write_u64(
            *pte_address, (original & ~test.clear_bits) | test.set_bits);
        const auto g_reference = memblock::reference_page_walk(
            environment.memory(), g_root, guest_physical + 0x188,
            test.mode, true, test.pbmte);
        const auto nested_reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, guest_virtual + 0x188,
            memblock::ReferencePageMode::sv39, test.mode, true, test.pbmte);
        if (g_reference.translated ||
            g_reference.faulting_pte_address != *pte_address ||
            g_reference.fault_level != target_level ||
            nested_reference.translated ||
            !nested_reference.guest_page_fault ||
            nested_reference.stage1_page_fault ||
            nested_reference.faulting_guest_physical_address !=
                guest_physical + 0x188 ||
            nested_reference.is_for_vs_nonleaf_pte) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name << "-reference\n";
            return 1;
        }
        if (!environment.activate_two_stage_modes(
                memblock::ReferencePageMode::sv39, test.mode,
                vs_root, g_root, 47, 49) ||
            !environment.set_page_based_memory_types(test.pbmte, true)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        const memblock::LoadTransaction transaction{
            .address = guest_virtual + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(180 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            .expected_exception_mask =
                memblock::kExceptionLoadGuestPageFault,
        };
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name << "-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        if (environment.exception_vaddr() != transaction.address ||
            environment.exception_gpaddr() != guest_physical + 0x188 ||
            environment.exception_is_for_vs_nonleaf_pte()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name << "-metadata expected_vaddr=0x"
                      << std::hex << transaction.address
                      << " actual_vaddr=0x" << environment.exception_vaddr()
                      << " expected_gpaddr=0x" << guest_physical + 0x188
                      << " actual_gpaddr=0x" << environment.exception_gpaddr()
                      << " actual_vs_nonleaf=" << std::dec
                      << environment.exception_is_for_vs_nonleaf_pte() << '\n';
            return 1;
        }
        if (environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name
                      << "-bypass reason=fault issued a data-manager request\n";
            return 1;
        }

        const std::uint64_t store_dcache_before =
            environment.tilelink_requests();
        const std::uint64_t store_uncache_before =
            environment.uncache_requests();
        const memblock::StoreTransaction store{
            .address = guest_virtual + 0x188,
            .data = 0x8899aabbccddeeffULL ^ index,
            .op = memblock::StoreOp::sd,
            .rob = 1,
            .sq = 0,
            .address_lane = static_cast<unsigned>(
                index % memblock::kScalarStoreLanes),
            .data_lane = static_cast<unsigned>(
                (index + 1) % memblock::kScalarStoreLanes),
            .expected_exception_mask =
                memblock::kExceptionStoreGuestPageFault,
        };
        environment.expect_store(store);
        if (!environment.set_rob_head(store.rob, store.rob_flag) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 2048) ||
            !environment.issue_store_data(store, 2048) ||
            !environment.run_until_store_complete_with_replay(store, 32768)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name << "-store-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        if (environment.tilelink_requests() != store_dcache_before ||
            environment.uncache_requests() != store_uncache_before ||
            (environment.sq_dequeued() + environment.sq_canceled() <
                 environment.sq_allocated() &&
             !environment.account_sq_cancellation(1)) ||
            environment.sq_dequeued() + environment.sq_canceled() !=
                environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=g-"
                      << test.name
                      << "-store-side-effect reason="
                      << (environment.error().empty()
                              ? "faulting store reached memory or unbalanced SQ"
                              : environment.error()) << '\n';
            return 1;
        }
    }

    std::cout << "MEMBLOCK_TRANSLATION_FAULTS_PASS cases="
              << canonical_boundary_cases + 4 + ppn_access_fault_cases +
                    vs_gpa_width_cases +
                    pte_encoding_cases.size() * 4
              << " canonical_boundary_cases=" << canonical_boundary_cases
              << " noncanonical_ptw_requests=" << noncanonical_ptw_requests
              << " ppn_access_fault_cases=" << ppn_access_fault_cases
              << " stage1_ppn_access_fault_cases="
              << stage1_ppn_access_fault_cases
              << " vs_only_ppn_access_fault_cases="
              << vs_only_ppn_access_fault_cases
              << " gstage_ppn_access_fault_cases="
              << gstage_ppn_access_fault_cases
              << " leaf_ppn_access_fault_cases="
              << leaf_ppn_access_fault_cases
              << " nonleaf_ppn_access_fault_cases="
              << nonleaf_ppn_access_fault_cases
              << " intermediate_ppn_access_fault_cases="
              << intermediate_ppn_access_fault_cases
              << " root_ppn_access_fault_cases="
              << root_ppn_access_fault_cases
              << " lowest_high_ppn_cases=" << lowest_high_ppn_cases
              << " highest_high_ppn_cases=" << highest_high_ppn_cases
              << " ppn_access_fault_ptw_requests="
              << ppn_access_fault_ptw_requests
              << " vs_only_ppn_access_fault_ptw_requests="
              << vs_only_ppn_access_fault_ptw_requests
              << " vs_gpa_width_cases=" << vs_gpa_width_cases
              << " sv39x4_gpa_width_cases=" << sv39x4_gpa_width_cases
              << " sv48x4_gpa_width_cases=" << sv48x4_gpa_width_cases
              << " vs_gpa_leaf_cases=" << vs_gpa_leaf_cases
              << " vs_gpa_intermediate_cases="
              << vs_gpa_intermediate_cases
              << " vs_gpa_root_cases=" << vs_gpa_root_cases
              << " vs_gpa_width_ptw_requests="
              << vs_gpa_width_ptw_requests
              << " stage1_pte_encoding_cases=" << pte_encoding_cases.size()
              << " gstage_pte_encoding_cases=" << pte_encoding_cases.size()
              << " stage1_store_pte_encoding_cases="
              << pte_encoding_cases.size()
              << " gstage_store_pte_encoding_cases="
              << pte_encoding_cases.size()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
