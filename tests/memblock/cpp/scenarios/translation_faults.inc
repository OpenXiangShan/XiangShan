// This file is included inside its generated scenario module private namespace.
// Keep scenario-local helpers here; shared scenario interfaces belong in shared.hpp.

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
              << '\n';
    return 0;
}

struct CanonicalRangeCoverage {
    unsigned faults = 0;
    unsigned controls = 0;
    unsigned readbacks = 0;
    unsigned recoveries = 0;
    unsigned scalar_loads = 0;
    unsigned scalar_stores = 0;
    unsigned vector_stores = 0;
};

bool check_canonical_access_ranges(
    int argc, char **argv, CanonicalRangeCoverage &coverage)
{
    // Both canonical sides are mapped.  If an invalid address is truncated
    // before translation, its alias must succeed rather than accidentally
    // reproducing the expected fault through a missing PTE.
    for (const auto mode : {memblock::ReferencePageMode::sv39,
                            memblock::ReferencePageMode::sv48}) {
        const unsigned address_bits = mode == memblock::ReferencePageMode::sv48
            ? 48U : 39U;
        const std::uint64_t low_limit = std::uint64_t{1} << (address_bits - 1);
        const std::uint64_t high_base = 0U - low_limit;
        for (unsigned kind = 0; kind < 5; ++kind) {
            for (bool invalid_first : {false, true}) {
                for (unsigned eew = 1; eew <= 3; ++eew) {
                    const unsigned width = 1U << eew;
                    for (unsigned distance = 1; distance < width; ++distance) {
                        memblock::Environment environment(argc, argv);
                        constexpr std::uint64_t physical = 0xcc000000ULL;
                        constexpr std::uint64_t root = 0x9e000000ULL;
                        const std::uint64_t address =
                            (invalid_first ? high_base : low_limit) - distance;
                        const std::uint64_t expected_fault = invalid_first
                            ? address : low_limit;
                        const auto fail = [&](const char *phase) {
                            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL"
                                      << " phase=canonical-range-" << phase
                                      << " mode=" << address_bits
                                      << " kind=" << kind << " width=" << width
                                      << " invalid_first=" << invalid_first
                                      << " distance=" << distance
                                      << " address=0x" << std::hex << address
                                      << " expected_vaddr=0x" << expected_fault
                                      << " actual_vaddr=0x"
                                      << environment.exception_vaddr() << std::dec
                                      << " reason=" << environment.error() << '\n';
                            return false;
                        };
                        environment.memory().fill_incrementing(
                            physical, 0x2000, 0x39 + kind);
                        if (!environment.reset() ||
                            !environment.enable_misaligned_accesses()) {
                            return fail("reset");
                        }
                        for (unsigned side = 0; side < 2; ++side) {
                            const std::uint64_t va = side == 0
                                ? low_limit - 0x1000 : high_base;
                            const std::uint64_t pa = physical + side * 0x1000;
                            const bool mapped = mode ==
                                    memblock::ReferencePageMode::sv48
                                ? environment.map_sv48_4k(va, pa, root)
                                : environment.map_sv39_4k(va, pa, root);
                            if (!mapped) {
                                return fail("map");
                            }
                        }
                        if (!(mode == memblock::ReferencePageMode::sv48
                                  ? environment.activate_sv48(root, 41)
                                  : environment.activate_sv39(root, 40))) {
                            return fail("activate");
                        }
                        std::optional<std::uint64_t> first_invalid;
                        unsigned valid_bytes = 0;
                        for (unsigned byte = 0; byte < width; ++byte) {
                            const auto walk = memblock::reference_page_walk(
                                environment.memory(), root, address + byte, mode);
                            if (walk.translated) {
                                ++valid_bytes;
                            } else if (!first_invalid) {
                                first_invalid = address + byte;
                            }
                        }
                        if (first_invalid != expected_fault ||
                            valid_bytes == 0 || valid_bytes == width) {
                            return fail("reference");
                        }
                        // Exact aligned loads at the two edges establish that
                        // both permissive mappings work before testing faults.
                        for (unsigned side = 0; side < 2; ++side) {
                            const memblock::LoadTransaction control{
                                .address = side == 0
                                    ? low_limit - width : high_base,
                                .oracle_address = side == 0
                                    ? physical + 0x1000 - width
                                    : physical + 0x1000,
                                .op = static_cast<memblock::LoadOp>(eew),
                                .rob = static_cast<std::uint8_t>(side),
                                .lq = static_cast<std::uint8_t>(side),
                                .pdest = static_cast<std::uint8_t>(30 + side),
                                .lane = side,
                            };
                            environment.expect_load(control);
                            if (!environment.set_rob_head(control.rob) ||
                                !environment.enqueue_load(control) ||
                                !environment.issue_load(control, 1024) ||
                                !environment.run_until_complete(16384) ||
                                !environment.run_until_lq_retired(4096)) {
                                return fail("control");
                            }
                            ++coverage.controls;
                        }
                        environment.select_store_exception_address(kind != 0);
                        if (!environment.set_rob_head(2)) {
                            return fail("head");
                        }
                        bool completed = false;
                        if (kind == 0) {
                            const memblock::LoadTransaction fault{
                                .address = address,
                                .op = static_cast<memblock::LoadOp>(eew),
                                .rob = 2,
                                .lq = 2,
                                .pdest = 32,
                                .lane = eew % memblock::kScalarLoadLanes,
                                .expected_exception_mask =
                                    memblock::kExceptionLoadPageFault,
                            };
                            environment.expect_load(fault);
                            completed = environment.enqueue_load(fault) &&
                                environment.issue_load(fault, 1024) &&
                                environment.run_until_complete(16384) &&
                                environment.run_until_lq_retired(4096);
                            ++coverage.scalar_loads;
                        } else if (kind == 1) {
                            const memblock::StoreTransaction fault{
                                .address = address,
                                .data = 0x13579bdf2468ace0ULL,
                                .op = static_cast<memblock::StoreOp>(eew),
                                .rob = 2,
                                .sq = 0,
                                .address_lane = eew % memblock::kScalarStoreLanes,
                                .data_lane = (eew + 1) % memblock::kScalarStoreLanes,
                                .expected_exception_mask =
                                    memblock::kExceptionStorePageFault,
                            };
                            environment.expect_store(fault);
                            completed = environment.enqueue_store(fault, 2) &&
                                environment.issue_store_address(fault, 2048) &&
                                environment.issue_store_data(fault, 2048) &&
                                environment.run_until_store_complete_with_replay(
                                    fault, 16384);
                            ++coverage.scalar_stores;
                        } else {
                            const memblock::VectorMemoryTransaction fault{
                                .store = true,
                                .address = address,
                                .stride = static_cast<std::int64_t>(width),
                                .addressing = kind == 2
                                    ? memblock::VectorAddressingMode::strided
                                    : kind == 3
                                        ? memblock::VectorAddressingMode::indexed_unordered
                                        : memblock::VectorAddressingMode::indexed_ordered,
                                .eew = static_cast<std::uint8_t>(eew),
                                .vl = 1,
                                .rob = 2,
                                .lq = 2,
                                .sq = 0,
                                .lane = eew % memblock::kVectorMemoryLanes,
                                .flow_num = static_cast<std::uint8_t>(16 / width),
                                .expected_exception_mask =
                                    memblock::kExceptionStorePageFault,
                                .expected_writeback_vstart = 0,
                            };
                            environment.expect_vector(fault);
                            completed = environment.enqueue_vector(fault) &&
                                environment.issue_vector(fault, 2048) &&
                                environment.run_until_vector_complete_with_replays(
                                    fault, 16384);
                            ++coverage.vector_stores;
                        }
                        // Legal speculative first-page traffic is allowed.
                        // The oracle is the identified terminal fault and its
                        // address, not an assumed absence of cache/PTW activity.
                        if (!completed || !environment.run_cycles(8) ||
                            environment.exception_vaddr() != expected_fault ||
                            environment.exception_is_for_vs_nonleaf_pte()) {
                            return fail("fault");
                        }
                        if (!environment.redirect_after(2, false, true, kind >= 2) ||
                            !environment.run_cycles(32)) {
                            return fail("redirect");
                        }
                        // Require externally observed retirement/cancellation
                        // for every flow; model-only teardown is not evidence
                        // that the faulting instruction released the queue.
                        if (environment.sq_dequeued() +
                                environment.sq_canceled() !=
                                environment.sq_allocated() ||
                            !environment.run_until_queues_retired(4096)) {
                            return fail("queue-conservation");
                        }

                        // Re-read both edge words against the unchanged
                        // architectural memory. This detects a faulting store
                        // that has silently modified either permissive alias,
                        // including data still resident in DCache or SBuffer.
                        for (unsigned side = 0; side < 2; ++side) {
                            const auto lq_offset = environment.lq_allocated() -
                                environment.lq_canceled();
                            const memblock::LoadTransaction readback{
                                .address = side == 0
                                    ? low_limit - 8 : high_base,
                                .oracle_address = side == 0
                                    ? physical + 0x1000 - 8
                                    : physical + 0x1000,
                                .op = memblock::LoadOp::ld,
                                .rob = static_cast<std::uint8_t>(3 + side),
                                .lq = memblock::lq_pointer_value(lq_offset),
                                .lq_flag = memblock::lq_pointer_flag(lq_offset),
                                .pdest = static_cast<std::uint8_t>(33 + side),
                                .lane = (eew + side) % memblock::kScalarLoadLanes,
                            };
                            environment.expect_load(readback);
                            if (!environment.set_rob_head(readback.rob) ||
                                !environment.enqueue_load(readback) ||
                                !environment.issue_load(readback, 1024) ||
                                !environment.run_until_complete(16384) ||
                                !environment.run_until_lq_retired(4096)) {
                                return fail("readback");
                            }
                            ++coverage.readbacks;
                        }
                        {
                            // A real committed store followed by a load in the
                            // same DUT instance proves SQ recovery after either
                            // a scalar fault or a vector fault.
                            const auto sq_offset = environment.sq_allocated() -
                                environment.sq_canceled();
                            const memblock::StoreTransaction survivor{
                                .address = high_base + 0x100,
                                .oracle_address = physical + 0x1100,
                                .data = 0xfedcba9876543210ULL ^ distance,
                                .op = memblock::StoreOp::sd,
                                .rob = 5,
                                .sq = memblock::sq_pointer_value(sq_offset),
                                .sq_flag = memblock::sq_pointer_flag(sq_offset),
                                .address_lane = eew % memblock::kScalarStoreLanes,
                                .data_lane = (eew + 1) % memblock::kScalarStoreLanes,
                            };
                            environment.expect_store(survivor);
                            if (!environment.set_rob_head(survivor.rob) ||
                                !environment.enqueue_store(
                                    survivor, memblock::lq_pointer_value(
                                        environment.lq_allocated() -
                                        environment.lq_canceled())) ||
                                !environment.issue_store_data(survivor, 2048) ||
                                !environment.issue_store_address(survivor, 2048) ||
                                !environment.run_until_store_complete_with_replay(
                                    survivor, 16384) ||
                                !environment.commit_store(survivor, 4096)) {
                                return fail("survivor-store");
                            }
                            const auto lq_offset = environment.lq_allocated() -
                                environment.lq_canceled();
                            const memblock::LoadTransaction survivor_load{
                                .address = survivor.address,
                                .oracle_address = survivor.oracle_address,
                                .op = memblock::LoadOp::ld,
                                .rob = 6,
                                .lq = memblock::lq_pointer_value(lq_offset),
                                .lq_flag = memblock::lq_pointer_flag(lq_offset),
                                .pdest = 35,
                                .lane = eew % memblock::kScalarLoadLanes,
                            };
                            environment.expect_load(survivor_load);
                            if (!environment.set_rob_head(survivor_load.rob) ||
                                !environment.enqueue_load(survivor_load) ||
                                !environment.issue_load(survivor_load, 1024) ||
                                !environment.run_until_complete(16384) ||
                                !environment.run_until_queues_retired(4096)) {
                                return fail("survivor-load");
                            }
                            ++coverage.recoveries;
                        }
                        ++coverage.faults;
                    }
                }
            }
        }
    }
    return coverage.faults == 220 && coverage.controls == 440 &&
        coverage.readbacks == 440 && coverage.recoveries == 220;
}

bool check_gstage_access_ranges(
    int argc, char **argv, CanonicalRangeCoverage &coverage)
{
    for (const auto mode : {memblock::ReferencePageMode::sv39,
                            memblock::ReferencePageMode::sv48}) {
        const unsigned address_bits = mode == memblock::ReferencePageMode::sv48
            ? 50U : 41U;
        const std::uint64_t limit = std::uint64_t{1} << address_bits;
        for (unsigned kind = 0; kind < 5; ++kind) {
            for (unsigned eew = 1; eew <= 3; ++eew) {
                const unsigned width = 1U << eew;
                for (unsigned distance = 1; distance < width; ++distance) {
                    memblock::Environment environment(argc, argv);
                    constexpr std::uint64_t physical = 0xcd000000ULL;
                    constexpr std::uint64_t root = 0x9f000000ULL;
                    const std::uint64_t address = limit - distance;
                    const auto fail = [&](const char *phase) {
                        std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL"
                                  << " phase=gstage-range-" << phase
                                  << " mode=" << address_bits
                                  << " kind=" << kind << " width=" << width
                                  << " distance=" << distance
                                  << " address=0x" << std::hex << address
                                  << " expected_vaddr=0x" << limit
                                  << " actual_vaddr=0x"
                                  << environment.exception_vaddr()
                                  << " expected_gpaddr=0x" << limit
                                  << " actual_gpaddr=0x"
                                  << environment.exception_gpaddr() << std::dec
                                  << " actual_vs_nonleaf="
                                  << environment.exception_is_for_vs_nonleaf_pte()
                                  << " reason=" << environment.error() << '\n';
                        return false;
                    };
                    environment.memory().fill_incrementing(
                        physical, 0x1000, 0x51 + kind);
                    environment.memory().fill_incrementing(
                        physical + 0x1000, 0x1000, 0xa7 + kind);
                    if (!environment.reset() ||
                        !environment.enable_misaligned_accesses() ||
                        !environment.set_pointer_masking(
                            memblock::PointerMaskingConfig{})) {
                        return fail("reset");
                    }
                    // The final valid GPA page and the truncation alias are
                    // both permissive. Losing a high address bit must not be
                    // hidden by an unrelated missing-PTE guest-page fault.
                    for (unsigned side = 0; side < 2; ++side) {
                        const std::uint64_t guest = side == 0
                            ? limit - 0x1000 : 0;
                        const std::uint64_t host = physical + side * 0x1000;
                        const bool mapped = mode ==
                                memblock::ReferencePageMode::sv48
                            ? environment.map_sv48x4_4k(guest, host, root)
                            : environment.map_sv39x4_4k(guest, host, root);
                        if (!mapped) {
                            return fail("map");
                        }
                    }
                    if (!environment.activate_two_stage_modes(
                            memblock::ReferencePageMode::bare, mode,
                            0, root, 42, 43)) {
                        return fail("activate");
                    }
                    std::optional<std::uint64_t> first_invalid;
                    unsigned valid_bytes = 0;
                    for (unsigned byte = 0; byte < width; ++byte) {
                        const auto walk = memblock::reference_two_stage_access(
                            environment.memory(), 0, root, address + byte,
                            kind == 0 ? memblock::ReferenceMemoryAccess::load
                                      : memblock::ReferenceMemoryAccess::store,
                            memblock::ReferencePageMode::bare, mode);
                        if (walk.translated) {
                            if (walk.physical_address !=
                                physical + 0x1000 - distance + byte) {
                                return fail("reference-prefix");
                            }
                            ++valid_bytes;
                        } else {
                            if (!walk.guest_page_fault ||
                                walk.stage1_page_fault || walk.access_fault ||
                                walk.is_for_vs_nonleaf_pte ||
                                walk.faulting_guest_physical_address !=
                                    address + byte) {
                                return fail("reference-fault");
                            }
                            if (!first_invalid) {
                                first_invalid = address + byte;
                            }
                        }
                    }
                    if (first_invalid != limit || valid_bytes != distance) {
                        return fail("reference-range");
                    }
                    const std::array<std::uint64_t, 2> edge_guest{{limit - 8, 0}};
                    const std::array<std::uint64_t, 2> edge_host{{
                        physical + 0xff8, physical + 0x1000}};
                    std::array<std::uint64_t, 2> edge_data{};
                    for (unsigned side = 0; side < 2; ++side) {
                        const auto walk = memblock::reference_two_stage_access(
                            environment.memory(), 0, root, edge_guest[side],
                            memblock::ReferenceMemoryAccess::load,
                            memblock::ReferencePageMode::bare, mode);
                        if (!walk.translated ||
                            walk.physical_address != edge_host[side]) {
                            return fail("control-reference");
                        }
                        edge_data[side] = environment.bus_expected_load(
                            edge_host[side], memblock::LoadOp::ld);
                        const memblock::LoadTransaction control{
                            .address = edge_guest[side],
                            .oracle_address = edge_host[side],
                            .op = memblock::LoadOp::ld,
                            .rob = static_cast<std::uint8_t>(side),
                            .lq = static_cast<std::uint8_t>(side),
                            .pdest = static_cast<std::uint8_t>(30 + side),
                            .lane = side,
                        };
                        environment.expect_load_data(control, edge_data[side]);
                        if (!environment.set_rob_head(control.rob) ||
                            !environment.enqueue_load(control) ||
                            !environment.issue_load(control, 1024) ||
                            !environment.run_until_complete(16384) ||
                            !environment.run_until_lq_retired(4096)) {
                            return fail("control");
                        }
                        ++coverage.controls;
                    }
                    const auto lq_before = environment.lq_dequeued();
                    const auto sq_before = environment.sq_dequeued();
                    const auto lq_cancels =
                        environment.lq_redirect_canceled_observed();
                    const auto sq_cancels =
                        environment.sq_redirect_canceled_observed();
                    const auto uncache_before = environment.uncache_requests();
                    environment.select_store_exception_address(kind != 0);
                    if (!environment.set_rob_head(2)) {
                        return fail("head");
                    }
                    bool completed = false;
                    const unsigned fault_flows = kind >= 2 ? 16U / width : 1U;
                    if (kind == 0) {
                        const memblock::LoadTransaction fault{
                            .address = address,
                            .op = static_cast<memblock::LoadOp>(eew),
                            .rob = 2,
                            .lq = 2,
                            .pdest = 32,
                            .lane = eew % memblock::kScalarLoadLanes,
                            .expected_exception_mask =
                                memblock::kExceptionLoadGuestPageFault,
                        };
                        environment.expect_load(fault);
                        completed = environment.enqueue_load(fault) &&
                            environment.issue_load(fault, 1024) &&
                            environment.run_until_complete(16384);
                        ++coverage.scalar_loads;
                    } else if (kind == 1) {
                        const memblock::StoreTransaction fault{
                            .address = address,
                            .data = 0x13579bdf2468ace0ULL ^ distance,
                            .op = static_cast<memblock::StoreOp>(eew),
                            .rob = 2,
                            .sq = 0,
                            .address_lane = eew % memblock::kScalarStoreLanes,
                            .data_lane = (eew + 1) % memblock::kScalarStoreLanes,
                            .expected_exception_mask =
                                memblock::kExceptionStoreGuestPageFault,
                        };
                        environment.expect_store(fault);
                        completed = environment.enqueue_store(fault, 2) &&
                            environment.issue_store_address(fault, 2048) &&
                            environment.issue_store_data(fault, 2048) &&
                            environment.run_until_store_complete_with_replay(
                                fault, 16384);
                        ++coverage.scalar_stores;
                    } else {
                        const memblock::VectorMemoryTransaction fault{
                            .store = true,
                            .address = address,
                            .stride = static_cast<std::int64_t>(width),
                            .addressing = kind == 2
                                ? memblock::VectorAddressingMode::strided
                                : kind == 3
                                    ? memblock::VectorAddressingMode::indexed_unordered
                                    : memblock::VectorAddressingMode::indexed_ordered,
                            .eew = static_cast<std::uint8_t>(eew),
                            .vsew = static_cast<std::uint8_t>(eew),
                            .vl = 1,
                            .rob = 2,
                            .lq = 2,
                            .sq = 0,
                            .lane = eew % memblock::kVectorMemoryLanes,
                            .flow_num = static_cast<std::uint8_t>(fault_flows),
                            .expected_exception_mask =
                                memblock::kExceptionStoreGuestPageFault,
                            .expected_writeback_vstart = 0,
                        };
                        if (memblock::active_vector_elements(fault) != 1U ||
                            memblock::vector_element_address(fault, 0) != address) {
                            return fail("vector-shape");
                        }
                        environment.expect_vector(fault);
                        completed = environment.enqueue_vector(fault) &&
                            environment.issue_vector(fault, 2048) &&
                            environment.run_until_vector_complete_with_replays(
                                fault, 16384);
                        ++coverage.vector_stores;
                    }
                    // Speculative accesses to the legal prefix are allowed.
                    // The identified completion and full VA/GPA decide the
                    // result independently of internal split/replay timing.
                    if (!completed || !environment.run_cycles(8) ||
                        environment.exception_vaddr() != limit ||
                        environment.exception_gpaddr() != limit ||
                        environment.exception_is_for_vs_nonleaf_pte() ||
                        environment.uncache_requests() != uncache_before) {
                        return fail("fault");
                    }
                    if (!environment.redirect_after(2, false, true, kind >= 2) ||
                        !environment.run_cycles(32) ||
                        !environment.run_until_queues_retired(4096)) {
                        return fail("redirect-retire");
                    }
                    const auto lq_dequeued = environment.lq_dequeued() - lq_before;
                    const auto sq_dequeued = environment.sq_dequeued() - sq_before;
                    const auto lq_canceled =
                        environment.lq_redirect_canceled_observed() - lq_cancels;
                    const auto sq_canceled =
                        environment.sq_redirect_canceled_observed() - sq_cancels;
                    if (lq_dequeued + lq_canceled != (kind == 0 ? 1U : 0U) ||
                        sq_dequeued + sq_canceled != (kind == 0 ? 0U : fault_flows)) {
                        return fail("queue-conservation");
                    }
                    for (unsigned side = 0; side < 2; ++side) {
                        const auto lq_offset = environment.lq_allocated() -
                            environment.lq_redirect_canceled_observed();
                        const memblock::LoadTransaction readback{
                            .address = edge_guest[side],
                            .oracle_address = edge_host[side],
                            .op = memblock::LoadOp::ld,
                            .rob = static_cast<std::uint8_t>(3 + side),
                            .lq = memblock::lq_pointer_value(lq_offset),
                            .lq_flag = memblock::lq_pointer_flag(lq_offset),
                            .pdest = static_cast<std::uint8_t>(33 + side),
                            .lane = (eew + side) % memblock::kScalarLoadLanes,
                        };
                        environment.expect_load_data(readback, edge_data[side]);
                        if (environment.bus_expected_load(
                                edge_host[side], memblock::LoadOp::ld) !=
                                edge_data[side] ||
                            !environment.set_rob_head(readback.rob) ||
                            !environment.enqueue_load(readback) ||
                            !environment.issue_load(readback, 1024) ||
                            !environment.run_until_complete(16384) ||
                            !environment.run_until_lq_retired(4096)) {
                            return fail("readback");
                        }
                        ++coverage.readbacks;
                    }
                    environment.select_store_exception_address(false);
                    const auto sq_offset = environment.sq_allocated() -
                        environment.sq_redirect_canceled_observed();
                    const memblock::StoreTransaction survivor{
                        .address = 0x100,
                        .oracle_address = physical + 0x1100,
                        .data = 0xfedcba9876543210ULL ^ distance,
                        .op = memblock::StoreOp::sd,
                        .rob = 5,
                        .sq = memblock::sq_pointer_value(sq_offset),
                        .sq_flag = memblock::sq_pointer_flag(sq_offset),
                        .address_lane = eew % memblock::kScalarStoreLanes,
                        .data_lane = (eew + 1) % memblock::kScalarStoreLanes,
                    };
                    const auto survivor_walk = memblock::reference_two_stage_access(
                        environment.memory(), 0, root, survivor.address,
                        memblock::ReferenceMemoryAccess::store,
                        memblock::ReferencePageMode::bare, mode);
                    environment.expect_store(survivor);
                    if (!survivor_walk.translated ||
                        survivor_walk.physical_address != *survivor.oracle_address ||
                        !environment.set_rob_head(survivor.rob) ||
                        !environment.enqueue_store(
                            survivor, memblock::lq_pointer_value(
                                environment.lq_allocated() -
                                environment.lq_redirect_canceled_observed())) ||
                        !environment.issue_store_data(survivor, 2048) ||
                        !environment.issue_store_address(survivor, 2048) ||
                        !environment.run_until_store_complete_with_replay(
                            survivor, 16384) ||
                        !environment.commit_store(survivor, 4096)) {
                        return fail("survivor-store");
                    }
                    const auto lq_offset = environment.lq_allocated() -
                        environment.lq_redirect_canceled_observed();
                    const memblock::LoadTransaction survivor_load{
                        .address = survivor.address,
                        .oracle_address = survivor.oracle_address,
                        .op = memblock::LoadOp::ld,
                        .rob = 6,
                        .lq = memblock::lq_pointer_value(lq_offset),
                        .lq_flag = memblock::lq_pointer_flag(lq_offset),
                        .pdest = 35,
                        .lane = eew % memblock::kScalarLoadLanes,
                    };
                    environment.expect_load_data(survivor_load, survivor.data);
                    if (!environment.set_rob_head(survivor_load.rob) ||
                        !environment.enqueue_load(survivor_load) ||
                        !environment.issue_load(survivor_load, 1024) ||
                        !environment.run_until_complete(16384) ||
                        !environment.run_until_queues_retired(4096)) {
                        return fail("survivor-load");
                    }
                    ++coverage.recoveries;
                    ++coverage.faults;
                }
            }
        }
    }
    return coverage.faults == 110 && coverage.controls == 220 &&
        coverage.readbacks == 220 && coverage.recoveries == 110 &&
        coverage.scalar_loads == 22 && coverage.scalar_stores == 22 &&
        coverage.vector_stores == 66;
}

int run_translation_faults(int argc, char **argv)
{
    CanonicalRangeCoverage range_coverage;
    if (!check_canonical_access_ranges(argc, argv, range_coverage)) {
        return 1;
    }

    CanonicalRangeCoverage gstage_range_coverage;
    if (!check_gstage_access_ranges(argc, argv, gstage_range_coverage)) {
        return 1;
    }

    unsigned canonical_boundary_cases = 0;
    unsigned vector_vstart_faults = 0;
    unsigned vector_later_element_faults = 0;
    unsigned vector_later_address_matches = 0;
    unsigned vector_all_masked_controls = 0;
    unsigned vector_store_fault_readbacks = 0;
    unsigned vector_masked_store_fault_readbacks = 0;
    unsigned vector_first_element_store_fault_readbacks = 0;
    unsigned vector_segment_store_fault_readbacks = 0;
    unsigned vector_segment_element_fault_readbacks = 0;
    unsigned vector_segment_first_field_fault_readbacks = 0;
    unsigned pmp_vector_store_fault_readbacks = 0;
    unsigned pmp_masked_vector_store_fault_readbacks = 0;
    unsigned pmp_cross_page_vector_store_fault_readbacks = 0;
    unsigned pmp_segment_store_fault_readbacks = 0;
    unsigned pmp_segment_first_field_fault_readbacks = 0;
    unsigned pmp_vector_width_fault_readbacks = 0;
    unsigned vector_masked_later_load_controls = 0;
    std::uint64_t vector_all_masked_ptw_requests = 0;
    std::uint64_t vector_all_masked_dcache_requests = 0;
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


    // Two-uop vector page fault: uop 0 reads the last 16 bytes of a mapped
    // page, uop 1 begins at the next (unmapped) page with vstart=16.  The
    // second exceptional writeback must report vstart=16.
    {
        memblock::Environment vstart_env(argc, argv);
        constexpr std::uint64_t vstart_virtual = 0x71000000ULL;
        constexpr std::uint64_t vstart_physical = 0xc5000000ULL;
        constexpr std::uint64_t vstart_root = 0x9a800000ULL;
        constexpr std::uint64_t vstart_base = vstart_virtual + 0xff0;
        vstart_env.memory().fill_incrementing(vstart_physical, 0x1000, 0x42);
        if (!vstart_env.reset() ||
            !vstart_env.map_sv39_4k(
                vstart_virtual, vstart_physical, vstart_root) ||
            !vstart_env.activate_sv39(vstart_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << vstart_env.cycle()
                      << " phase=vector-vstart-configuration reason="
                      << vstart_env.error() << '\n';
            return 1;
        }
        std::array<memblock::VectorMemoryTransaction, 2> uops{};
        uops[0] = memblock::VectorMemoryTransaction{
            .address = vstart_base,
            .oracle_address = vstart_physical + 0xff0,
            .eew = 1,
            .vl = 16,
            .rob = 90,
            .lq = 0,
            .pdest = 170,
            .lane = 0,
            .flow_num = 1,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = false,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        uops[1] = uops[0];
        uops[1].lq = 1;
        uops[1].pdest = 171;
        uops[1].vstart = 8;
        uops[1].vuop_idx = 1;
        uops[1].last_uop = true;
        uops[1].expected_exception_mask = memblock::kExceptionLoadPageFault;
        uops[1].expected_writeback_vstart = 8;
        vstart_env.expect_vector(uops[0]);
        vstart_env.expect_vector(uops[1]);
        if (!vstart_env.set_rob_head(uops[0].rob, uops[0].rob_flag) ||
            !vstart_env.enqueue_vector(uops[0]) ||
            !vstart_env.enqueue_vector(uops[1]) ||
            !vstart_env.issue_vector(uops[0], 1024) ||
            !vstart_env.issue_vector(uops[1], 1024) ||
            !vstart_env.run_until_vector_complete_with_replays(
                {uops[0], uops[1]}, 32768) ||
            vstart_env.exception_vaddr() != vstart_virtual + 0x1000) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << vstart_env.cycle()
                      << " phase=vector-vstart-fault reason="
                      << vstart_env.error() << " vaddr=0x" << std::hex
                      << vstart_env.exception_vaddr() << std::dec << '\n';
            return 1;
        }
        const std::uint64_t retired =
            vstart_env.lq_dequeued() + vstart_env.lq_canceled();
        if (retired > vstart_env.lq_allocated() ||
            (retired < vstart_env.lq_allocated() &&
             !vstart_env.account_lq_cancellation(
                 static_cast<unsigned>(
                     vstart_env.lq_allocated() - retired)))) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << vstart_env.cycle()
                      << " phase=vector-vstart-accounting reason="
                      << vstart_env.error() << '\n';
            return 1;
        }
        ++vector_vstart_faults;
    }

    // A single vector load whose first element is in a mapped page and whose
    // next element starts in an unmapped page must trap at the second
    // element.  The architectural element index, not the number of active
    // elements, is the expected vstart.  Run the same address with only the
    // faulting element predicated on: the fault is still on element 1, so a
    // masked case must report vstart=1 rather than 0.
    struct LaterElementFaultCase {
        const char *name;
        bool masked;
        std::uint64_t virtual_page;
        std::uint64_t physical_page;
        std::uint64_t root;
        std::uint8_t rob;
        std::uint8_t pdest;
        // The 16-byte vector access starts at 0xff8 and crosses the DCache
        // 128-bit split boundary, so it consumes two load-queue flows even
        // though only two (or, when masked, one) elements are active.
        std::uint8_t flow_num;
    };
    constexpr std::array<LaterElementFaultCase, 2> later_element_faults{{
        {"unmasked", false, 0x71200000ULL, 0xc5100000ULL,
         0x9a900000ULL, 95, 180, 2},
        {"masked", true, 0x71300000ULL, 0xc5200000ULL,
         0x9a980000ULL, 96, 181, 2},
    }};
    for (const auto &test : later_element_faults) {
        memblock::Environment later(argc, argv);
        const std::uint64_t address = test.virtual_page + 0xff8;
        const std::uint64_t physical_address = test.physical_page + 0xff8;
        later.memory().fill_incrementing(physical_address, 8, 0x5a);
        later.configure_backpressure(0x3c6ef372fe94f82bULL, true);
        if (!later.reset() ||
            !later.map_sv39_4k(
                test.virtual_page, test.physical_page, test.root) ||
            !later.activate_sv39(test.root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << later.cycle() << " phase=vector-later-"
                      << test.name << "-configuration reason="
                      << later.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction transaction{
            .address = address,
            .oracle_address = physical_address,
            .eew = 3,
            .vl = 2,
            .rob = test.rob,
            .lq = 0,
            .pdest = test.pdest,
            .lane = 0,
            .flow_num = test.flow_num,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
            .expected_writeback_vstart = 1,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        if (test.masked) {
            transaction.vm = false;
            transaction.mask_bits = 0x0002;
        }
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(0x30);
        }
        later.expect_vector(transaction);
        const std::uint64_t expected_fault_address =
            test.virtual_page + 0x1000;
        if (!later.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !later.enqueue_vector(transaction) ||
            !later.issue_vector(transaction, 1024) ||
            !later.run_until_vector_complete_with_replays(
                transaction, 32768) ||
            // The vector feedback reaches the LqExceptionBuffer one cycle
            // after the writeback.  Settle before sampling the separate
            // exception-address payload, which has no valid bit.
            !later.run_cycles(4) ||
            later.exception_vaddr() != expected_fault_address) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << later.cycle() << " phase=vector-later-"
                      << test.name << "-fault reason=" << later.error()
                      << " vaddr=0x" << std::hex << later.exception_vaddr()
                      << " expected_vaddr=0x" << expected_fault_address
                      << std::dec << '\n';
            return 1;
        }
        ++vector_later_address_matches;
        const std::uint64_t retired =
            later.lq_dequeued() + later.lq_canceled();
        if (retired > later.lq_allocated() ||
            (retired < later.lq_allocated() &&
             !later.account_lq_cancellation(
                 static_cast<unsigned>(later.lq_allocated() - retired)))) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << later.cycle() << " phase=vector-later-"
                      << test.name << "-accounting reason="
                      << later.error() << '\n';
            return 1;
        }
        ++vector_later_element_faults;
    }

    // A vector store whose first element is writable and whose second element
    // lands in a read-only page must fault at element 1 with vstart=1 and must
    // not write the read-only page.  The first element may have been written
    // before the fault, so only the read-only page is required to retain its
    // original bytes.
    {
        memblock::Environment store_fault(argc, argv);
        constexpr std::uint64_t store_virtual = 0x72000000ULL;
        constexpr std::uint64_t store_physical = 0xc6000000ULL;
        constexpr std::uint64_t store_root = 0x9ab00000ULL;
        constexpr std::uint64_t readonly_physical = store_physical + 0x1000;
        store_fault.memory().fill_incrementing(
            readonly_physical, 0x20, 0x3c);
        const std::uint64_t readonly_word =
            store_fault.memory().read_u64(readonly_physical + 0x8);
        store_fault.configure_backpressure(0x5be0cd19137e2179ULL, true);
        if (!store_fault.reset() ||
            !store_fault.map_sv39_4k(
                store_virtual, store_physical, store_root,
                true, true, false, false, false, false) ||
            !store_fault.map_sv39_4k(
                store_virtual + 0x1000, readonly_physical, store_root,
                true, false, false, false, false, false) ||
            !store_fault.activate_sv39(store_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << store_fault.cycle()
                      << " phase=vector-store-fault-configuration reason="
                      << store_fault.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = store_virtual + 0xff8,
            .eew = 3,
            .vl = 2,
            .rob = 98,
            .sq = 0,
            .pdest = 183,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStorePageFault,
            .expected_writeback_vstart = 1,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : store.data) {
            byte = static_cast<unsigned char>(0x44);
        }
        // The top-level exception-address mux selects the store queue only
        // when the backend marks the completing exception as a store fault.
        store_fault.select_store_exception_address(true);
        store_fault.expect_vector(store);
        if (!store_fault.set_rob_head(store.rob, store.rob_flag) ||
            !store_fault.enqueue_vector(store) ||
            !store_fault.issue_vector(store, 1024) ||
            !store_fault.run_until_vector_complete_with_replays(
                store, 32768) ||
            !store_fault.run_cycles(4) ||
            store_fault.exception_vaddr() != store_virtual + 0x1000) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << store_fault.cycle()
                      << " phase=vector-store-fault reason="
                      << store_fault.error() << " vaddr=0x" << std::hex
                      << store_fault.exception_vaddr() << std::dec
                      << '\n';
            return 1;
        }
        const std::uint64_t sq_retired =
            store_fault.sq_dequeued() + store_fault.sq_canceled();
        if (sq_retired > store_fault.sq_allocated() ||
            (sq_retired < store_fault.sq_allocated() &&
             !store_fault.account_sq_cancellation(
                 static_cast<unsigned>(
                     store_fault.sq_allocated() - sq_retired)))) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << store_fault.cycle()
                      << " phase=vector-store-fault-accounting reason="
                      << store_fault.error() << '\n';
            return 1;
        }
        if (!store_fault.redirect_after(
                store.rob, store.rob_flag, true, true) ||
            !store_fault.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << store_fault.cycle()
                      << " phase=vector-store-fault-redirect reason="
                      << store_fault.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = store_virtual + 0x1008,
            .oracle_address = readonly_physical + 0x8,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 184,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        store_fault.expect_load_data(readback, readonly_word);
        if (!store_fault.set_rob_head(readback.rob, readback.rob_flag) ||
            !store_fault.enqueue_load(readback) ||
            !store_fault.issue_load(readback, 4096) ||
            !store_fault.run_until_complete(16384) ||
            !store_fault.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << store_fault.cycle()
                      << " phase=vector-store-fault-readback reason="
                      << store_fault.error() << '\n';
            return 1;
        }
        store_fault.select_store_exception_address(false);
        ++vector_store_fault_readbacks;
    }

    // Masked vector store fault: only element 1 is active and it lands in a
    // read-only page.  The inactive element 0 must not write its writable
    // page, and the active faulting element must not write the read-only page.
    {
        memblock::Environment masked_store(argc, argv);
        constexpr std::uint64_t masked_virtual = 0x72100000ULL;
        constexpr std::uint64_t masked_physical = 0xc7000000ULL;
        constexpr std::uint64_t masked_root = 0x9ac00000ULL;
        constexpr std::uint64_t masked_readonly = masked_physical + 0x1000;
        masked_store.memory().fill_incrementing(
            masked_physical + 0xff8, 8, 0x28);
        masked_store.memory().fill_incrementing(
            masked_readonly, 0x20, 0x9d);
        const std::uint64_t inactive_word = masked_store.memory().read_u64(
            masked_physical + 0xff8);
        const std::uint64_t readonly_word = masked_store.memory().read_u64(
            masked_readonly);
        masked_store.configure_backpressure(0xcbbb9d5dc1059ed8ULL, true);
        if (!masked_store.reset() ||
            !masked_store.map_sv39_4k(
                masked_virtual, masked_physical, masked_root,
                true, true, false, false, false, false) ||
            !masked_store.map_sv39_4k(
                masked_virtual + 0x1000, masked_readonly, masked_root,
                true, false, false, false, false, false) ||
            !masked_store.activate_sv39(masked_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << masked_store.cycle()
                      << " phase=masked-store-fault-configuration reason="
                      << masked_store.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction masked_transaction{
            .store = true,
            .address = masked_virtual + 0xff8,
            .eew = 3,
            .vl = 2,
            .vm = false,
            .mask_bits = 0x0002,
            .rob = 99,
            .sq = 0,
            .pdest = 185,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStorePageFault,
            .expected_writeback_vstart = 1,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : masked_transaction.data) {
            byte = static_cast<unsigned char>(0x54);
        }
        masked_store.select_store_exception_address(true);
        masked_store.expect_vector(masked_transaction);
        if (!masked_store.set_rob_head(
                masked_transaction.rob, masked_transaction.rob_flag) ||
            !masked_store.enqueue_vector(masked_transaction) ||
            !masked_store.issue_vector(masked_transaction, 1024) ||
            !masked_store.run_until_vector_complete_with_replays(
                masked_transaction, 32768) ||
            !masked_store.run_cycles(4) ||
            masked_store.exception_vaddr() != masked_virtual + 0x1000) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << masked_store.cycle()
                      << " phase=masked-store-fault reason="
                      << masked_store.error() << " vaddr=0x" << std::hex
                      << masked_store.exception_vaddr() << std::dec
                      << '\n';
            return 1;
        }
        const std::uint64_t masked_retired =
            masked_store.sq_dequeued() + masked_store.sq_canceled();
        if (masked_retired > masked_store.sq_allocated() ||
            (masked_retired < masked_store.sq_allocated() &&
             !masked_store.account_sq_cancellation(
                 static_cast<unsigned>(
                     masked_store.sq_allocated() - masked_retired)))) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << masked_store.cycle()
                      << " phase=masked-store-fault-accounting reason="
                      << masked_store.error() << '\n';
            return 1;
        }
        if (!masked_store.redirect_after(
                masked_transaction.rob,
                masked_transaction.rob_flag, true, true) ||
            !masked_store.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << masked_store.cycle()
                      << " phase=masked-store-fault-redirect reason="
                      << masked_store.error() << '\n';
            return 1;
        }
        std::array<memblock::LoadTransaction, 2> readbacks{};
        readbacks[0] = memblock::LoadTransaction{
            .address = masked_virtual + 0xff8,
            .oracle_address = masked_physical + 0xff8,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 186,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        readbacks[1] = memblock::LoadTransaction{
            .address = masked_virtual + 0x1000,
            .oracle_address = masked_readonly,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 1,
            .pdest = 187,
            .lane = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        masked_store.expect_load_data(readbacks[0], inactive_word);
        masked_store.expect_load_data(readbacks[1], readonly_word);
        if (!masked_store.set_rob_head(
                readbacks[0].rob, readbacks[0].rob_flag) ||
            !masked_store.enqueue_load_batch(
                {readbacks[0], readbacks[1]}, {0, 1}) ||
            !masked_store.issue_load_batch(
                {readbacks[0], readbacks[1]}, 4096) ||
            !masked_store.run_until_all_complete(16384) ||
            !masked_store.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << masked_store.cycle()
                      << " phase=masked-store-fault-readback reason="
                      << masked_store.error() << '\n';
            return 1;
        }
        masked_store.select_store_exception_address(false);
        ++vector_masked_store_fault_readbacks;
    }

    // First-element vector store fault: element 0 itself is in a read-only
    // page, so vstart=0 and no later element may write.  Both the read-only
    // page and the following writable page must retain their original bytes.
    {
        memblock::Environment first_store(argc, argv);
        constexpr std::uint64_t first_virtual = 0x72200000ULL;
        constexpr std::uint64_t first_physical = 0xc8000000ULL;
        constexpr std::uint64_t first_root = 0x9ad00000ULL;
        constexpr std::uint64_t first_readonly = first_physical;
        constexpr std::uint64_t first_writable = first_physical + 0x1000;
        first_store.memory().fill_incrementing(
            first_readonly + 0xff8, 8, 0x2b);
        first_store.memory().fill_incrementing(
            first_writable, 0x20, 0x8a);
        const std::uint64_t readonly_word = first_store.memory().read_u64(
            first_readonly + 0xff8);
        const std::uint64_t writable_word = first_store.memory().read_u64(
            first_writable);
        first_store.configure_backpressure(0xc19bf174cf692694ULL, true);
        if (!first_store.reset() ||
            !first_store.map_sv39_4k(
                first_virtual, first_readonly, first_root,
                true, false, false, false, false, false) ||
            !first_store.map_sv39_4k(
                first_virtual + 0x1000, first_writable, first_root,
                true, true, false, false, false, false) ||
            !first_store.activate_sv39(first_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << first_store.cycle()
                      << " phase=first-element-store-configuration reason="
                      << first_store.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction first_transaction{
            .store = true,
            .address = first_virtual + 0xff8,
            .eew = 3,
            .vl = 2,
            .rob = 100,
            .sq = 0,
            .pdest = 188,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStorePageFault,
            .expected_writeback_vstart = 0,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : first_transaction.data) {
            byte = static_cast<unsigned char>(0x66);
        }
        first_store.select_store_exception_address(true);
        first_store.expect_vector(first_transaction);
        if (!first_store.set_rob_head(
                first_transaction.rob, first_transaction.rob_flag) ||
            !first_store.enqueue_vector(first_transaction) ||
            !first_store.issue_vector(first_transaction, 1024) ||
            !first_store.run_until_vector_complete_with_replays(
                first_transaction, 32768) ||
            !first_store.run_cycles(4) ||
            first_store.exception_vaddr() != first_virtual + 0xff8) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << first_store.cycle()
                      << " phase=first-element-store reason="
                      << first_store.error() << " vaddr=0x" << std::hex
                      << first_store.exception_vaddr() << std::dec
                      << '\n';
            return 1;
        }
        const std::uint64_t first_retired =
            first_store.sq_dequeued() + first_store.sq_canceled();
        if (first_retired > first_store.sq_allocated() ||
            (first_retired < first_store.sq_allocated() &&
             !first_store.account_sq_cancellation(
                 static_cast<unsigned>(
                     first_store.sq_allocated() - first_retired)))) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << first_store.cycle()
                      << " phase=first-element-store-accounting reason="
                      << first_store.error() << '\n';
            return 1;
        }
        if (!first_store.redirect_after(
                first_transaction.rob,
                first_transaction.rob_flag, true, true) ||
            !first_store.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << first_store.cycle()
                      << " phase=first-element-store-redirect reason="
                      << first_store.error() << '\n';
            return 1;
        }
        std::array<memblock::LoadTransaction, 2> readbacks{};
        readbacks[0] = memblock::LoadTransaction{
            .address = first_virtual + 0xff8,
            .oracle_address = first_readonly + 0xff8,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 189,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        readbacks[1] = memblock::LoadTransaction{
            .address = first_virtual + 0x1000,
            .oracle_address = first_writable,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 1,
            .pdest = 190,
            .lane = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        first_store.expect_load_data(readbacks[0], readonly_word);
        first_store.expect_load_data(readbacks[1], writable_word);
        if (!first_store.set_rob_head(
                readbacks[0].rob, readbacks[0].rob_flag) ||
            !first_store.enqueue_load_batch(
                {readbacks[0], readbacks[1]}, {0, 1}) ||
            !first_store.issue_load_batch(
                {readbacks[0], readbacks[1]}, 4096) ||
            !first_store.run_until_all_complete(16384) ||
            !first_store.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << first_store.cycle()
                      << " phase=first-element-store-readback reason="
                      << first_store.error() << '\n';
            return 1;
        }
        first_store.select_store_exception_address(false);
        ++vector_first_element_store_fault_readbacks;
    }

    // A partially masked vector load whose inactive element points into an
    // unmapped page must complete successfully.  Masked-off elements are not
    // accessed; only the active mapped element supplies data.
    {
        memblock::Environment masked_load(argc, argv);
        constexpr std::uint64_t masked_virtual = 0x72300000ULL;
        constexpr std::uint64_t masked_physical = 0xc9000000ULL;
        constexpr std::uint64_t masked_root = 0x9ae00000ULL;
        masked_load.memory().fill_incrementing(
            masked_physical + 0xff8, 8, 0x7b);
        masked_load.configure_backpressure(0xe49b69c19ef14ad2ULL, true);
        if (!masked_load.reset() ||
            !masked_load.map_sv39_4k(
                masked_virtual, masked_physical, masked_root,
                true, true, false, false, false, false) ||
            !masked_load.activate_sv39(masked_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << masked_load.cycle()
                      << " phase=masked-later-load-configuration reason="
                      << masked_load.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction transaction{
            .address = masked_virtual + 0xff8,
            .oracle_address = masked_physical + 0xff8,
            .eew = 3,
            .vl = 2,
            .vm = false,
            .mask_bits = 0x0001,
            .rob = 101,
            .lq = 0,
            .pdest = 191,
            .lane = 0,
            .flow_num = 2,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(0x77);
        }
        masked_load.expect_vector(transaction);
        if (!masked_load.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !masked_load.enqueue_vector(transaction) ||
            !masked_load.issue_vector(transaction, 1024) ||
            !masked_load.run_until_vector_complete_with_replays(
                transaction, 32768) ||
            !masked_load.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << masked_load.cycle()
                      << " phase=masked-later-load reason="
                      << masked_load.error() << '\n';
            return 1;
        }
        ++vector_masked_later_load_controls;
    }

    // Segment-store permission fault: field 0 is writable, field 1 crosses
    // into a read-only page.  The data and fix-VL uops must fault, and the
    // read-only page must retain its original bytes even if field 0 was
    // partially written before the fault.
    {
        memblock::Environment segment_store(argc, argv);
        constexpr std::uint64_t segment_virtual = 0x73000000ULL;
        constexpr std::uint64_t segment_physical = 0xc9000000ULL;
        constexpr std::uint64_t segment_root = 0x9af00000ULL;
        constexpr std::uint64_t field_one_physical = segment_physical + 0x1000;
        segment_store.memory().fill_incrementing(
            segment_physical + 0xff8, 8, 0x2d);
        segment_store.memory().fill_incrementing(
            field_one_physical, 0x20, 0x9e);
        const std::uint64_t field_one_word =
            segment_store.memory().read_u64(field_one_physical);
        segment_store.configure_backpressure(0xa54ff53a5f1d36f1ULL, true);
        if (!segment_store.reset() ||
            !segment_store.map_sv39_4k(
                segment_virtual, segment_physical, segment_root,
                true, true, false, false, false, false) ||
            !segment_store.map_sv39_4k(
                segment_virtual + 0x1000, field_one_physical, segment_root,
                true, false, false, false, false, false) ||
            !segment_store.activate_sv39(segment_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_store.cycle()
                      << " phase=segment-store-fault-configuration reason="
                      << segment_store.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction segment_data{
            .store = true,
            .segment = true,
            .address = segment_virtual + 0xff8,
            .eew = 3,
            .vl = 1,
            .rob = 102,
            .sq = 0,
            .pdest = 192,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStorePageFault,
            // vstart is the vector element index, not the field index.  Both
            // fields of element 0 are part of the same segment element.
            .expected_writeback_vstart = 0,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = false,
            .nf = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : segment_data.data) {
            byte = static_cast<unsigned char>(0x3a);
        }
        segment_store.expect_vector(segment_data);
        if (!segment_store.set_rob_head(
                segment_data.rob, segment_data.rob_flag) ||
            !segment_store.enqueue_vector(segment_data) ||
            !segment_store.issue_vector(segment_data, 1024) ||
            !segment_store.run_until_vector_complete_with_replays(
                segment_data, 32768) ||
            !segment_store.run_cycles(4) ||
            segment_store.exception_vaddr() != segment_virtual + 0x1000 ||
            segment_store.vector_store_writebacks() != 1) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_store.cycle()
                      << " phase=segment-store-fault-data reason="
                      << segment_store.error() << " vaddr=0x" << std::hex
                      << segment_store.exception_vaddr() << std::dec
                      << " writebacks="
                      << segment_store.vector_store_writebacks() << '\n';
            return 1;
        }
        auto segment_fix = segment_data;
        segment_fix.pdest = 193;
        segment_fix.expected_exception_mask =
            memblock::kExceptionStorePageFault;
        segment_fix.expected_vl = 1;
        segment_fix.expected_writeback_vstart = 0;
        segment_fix.vuop_idx = 1;
        segment_fix.last_uop = true;
        segment_fix.vl_wen = true;
        segment_store.expect_vector_data(segment_fix, {});
        if (!segment_store.issue_vector(segment_fix, 1024) ||
            !segment_store.run_until_vector_complete(16384) ||
            segment_store.vector_store_writebacks() != 2 ||
            segment_store.sq_dequeued() != segment_store.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_store.cycle()
                      << " phase=segment-store-fault-fix reason="
                      << segment_store.error() << " writebacks="
                      << segment_store.vector_store_writebacks()
                      << " sq=" << segment_store.sq_dequeued() << '/'
                      << segment_store.sq_allocated() << '\n';
            return 1;
        }
        if (!segment_store.redirect_after(
                segment_data.rob, segment_data.rob_flag, true, true) ||
            !segment_store.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_store.cycle()
                      << " phase=segment-store-fault-redirect reason="
                      << segment_store.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = segment_virtual + 0x1000,
            .oracle_address = field_one_physical,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 194,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        segment_store.expect_load_data(readback, field_one_word);
        if (!segment_store.set_rob_head(readback.rob, readback.rob_flag) ||
            !segment_store.enqueue_load(readback) ||
            !segment_store.issue_load(readback, 4096) ||
            !segment_store.run_until_complete(16384) ||
            !segment_store.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_store.cycle()
                      << " phase=segment-store-fault-readback reason="
                      << segment_store.error() << '\n';
            return 1;
        }
        ++vector_segment_store_fault_readbacks;
    }

    // Two-element segment store with the fault on element 1.  Element 0's
    // two fields are in a writable page; element 1's fields are in a read-only
    // page.  vstart must be the vector element index 1, and the read-only page
    // must stay unchanged while element 0 may have been written.
    {
        memblock::Environment segment_element(argc, argv);
        constexpr std::uint64_t element_virtual = 0x73200000ULL;
        constexpr std::uint64_t element_physical = 0xc9200000ULL;
        constexpr std::uint64_t element_root = 0x9b000000ULL;
        constexpr std::uint64_t element_readonly = element_physical + 0x1000;
        segment_element.memory().fill_incrementing(
            element_physical + 0xff0, 0x20, 0x47);
        segment_element.memory().fill_incrementing(
            element_readonly, 0x20, 0xa1);
        const std::uint64_t readonly_word =
            segment_element.memory().read_u64(element_readonly);
        segment_element.configure_backpressure(0x510e527fade682d1ULL, true);
        if (!segment_element.reset() ||
            !segment_element.map_sv39_4k(
                element_virtual, element_physical, element_root,
                true, true, false, false, false, false) ||
            !segment_element.map_sv39_4k(
                element_virtual + 0x1000, element_readonly, element_root,
                true, false, false, false, false, false) ||
            !segment_element.activate_sv39(element_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_element.cycle()
                      << " phase=segment-element-fault-configuration reason="
                      << segment_element.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction segment_data{
            .store = true,
            .segment = true,
            .address = element_virtual + 0xff0,
            .eew = 3,
            .vl = 2,
            .rob = 103,
            .sq = 0,
            .pdest = 195,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStorePageFault,
            .expected_writeback_vstart = 1,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = false,
            .nf = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : segment_data.data) {
            byte = static_cast<unsigned char>(0x4d);
        }
        segment_element.expect_vector(segment_data);
        if (!segment_element.set_rob_head(
                segment_data.rob, segment_data.rob_flag) ||
            !segment_element.enqueue_vector(segment_data) ||
            !segment_element.issue_vector(segment_data, 1024) ||
            !segment_element.run_until_vector_complete_with_replays(
                segment_data, 32768) ||
            !segment_element.run_cycles(4) ||
            segment_element.exception_vaddr() !=
                element_virtual + 0x1000 ||
            segment_element.vector_store_writebacks() != 1) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_element.cycle()
                      << " phase=segment-element-fault-data reason="
                      << segment_element.error() << " vaddr=0x" << std::hex
                      << segment_element.exception_vaddr() << std::dec
                      << " writebacks="
                      << segment_element.vector_store_writebacks() << '\n';
            return 1;
        }
        auto segment_fix = segment_data;
        segment_fix.pdest = 196;
        segment_fix.expected_exception_mask =
            memblock::kExceptionStorePageFault;
        segment_fix.expected_vl = 2;
        segment_fix.expected_writeback_vstart = 1;
        segment_fix.vuop_idx = 1;
        segment_fix.last_uop = true;
        segment_fix.vl_wen = true;
        segment_element.expect_vector_data(segment_fix, {});
        if (!segment_element.issue_vector(segment_fix, 1024) ||
            !segment_element.run_until_vector_complete(16384) ||
            segment_element.vector_store_writebacks() != 2 ||
            segment_element.sq_dequeued() !=
                segment_element.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_element.cycle()
                      << " phase=segment-element-fault-fix reason="
                      << segment_element.error() << " writebacks="
                      << segment_element.vector_store_writebacks()
                      << " sq=" << segment_element.sq_dequeued() << '/'
                      << segment_element.sq_allocated() << '\n';
            return 1;
        }
        if (!segment_element.redirect_after(
                segment_data.rob, segment_data.rob_flag, true, true) ||
            !segment_element.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_element.cycle()
                      << " phase=segment-element-fault-redirect reason="
                      << segment_element.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = element_virtual + 0x1000,
            .oracle_address = element_readonly,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 197,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        segment_element.expect_load_data(readback, readonly_word);
        if (!segment_element.set_rob_head(
                readback.rob, readback.rob_flag) ||
            !segment_element.enqueue_load(readback) ||
            !segment_element.issue_load(readback, 4096) ||
            !segment_element.run_until_complete(16384) ||
            !segment_element.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_element.cycle()
                      << " phase=segment-element-fault-readback reason="
                      << segment_element.error() << '\n';
            return 1;
        }
        ++vector_segment_element_fault_readbacks;
    }

    // First-field segment-store fault: field 0 itself is in a read-only page.
    // The writable field 1 must not be written when field 0 faults first.
    {
        memblock::Environment segment_first(argc, argv);
        constexpr std::uint64_t first_virtual = 0x73400000ULL;
        constexpr std::uint64_t first_physical = 0xc9400000ULL;
        constexpr std::uint64_t first_root = 0x9b100000ULL;
        constexpr std::uint64_t first_readonly = first_physical;
        constexpr std::uint64_t first_writable = first_physical + 0x1000;
        segment_first.memory().fill_incrementing(
            first_readonly + 0xff8, 8, 0x5a);
        segment_first.memory().fill_incrementing(
            first_writable, 0x20, 0xb2);
        const std::uint64_t readonly_word =
            segment_first.memory().read_u64(first_readonly + 0xff8);
        const std::uint64_t writable_word =
            segment_first.memory().read_u64(first_writable);
        segment_first.configure_backpressure(0x9b05688c2b3e6c1fULL, true);
        if (!segment_first.reset() ||
            !segment_first.map_sv39_4k(
                first_virtual, first_readonly, first_root,
                true, false, false, false, false, false) ||
            !segment_first.map_sv39_4k(
                first_virtual + 0x1000, first_writable, first_root,
                true, true, false, false, false, false) ||
            !segment_first.activate_sv39(first_root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_first.cycle()
                      << " phase=segment-first-field-configuration reason="
                      << segment_first.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction segment_data{
            .store = true,
            .segment = true,
            .address = first_virtual + 0xff8,
            .eew = 3,
            .vl = 1,
            .rob = 104,
            .sq = 0,
            .pdest = 198,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStorePageFault,
            .expected_writeback_vstart = 0,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = false,
            .nf = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : segment_data.data) {
            byte = static_cast<unsigned char>(0x5e);
        }
        segment_first.expect_vector(segment_data);
        if (!segment_first.set_rob_head(
                segment_data.rob, segment_data.rob_flag) ||
            !segment_first.enqueue_vector(segment_data) ||
            !segment_first.issue_vector(segment_data, 1024) ||
            !segment_first.run_until_vector_complete_with_replays(
                segment_data, 32768) ||
            !segment_first.run_cycles(4) ||
            segment_first.exception_vaddr() != first_virtual + 0xff8 ||
            segment_first.vector_store_writebacks() != 1) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_first.cycle()
                      << " phase=segment-first-field-data reason="
                      << segment_first.error() << " vaddr=0x" << std::hex
                      << segment_first.exception_vaddr() << std::dec
                      << " writebacks="
                      << segment_first.vector_store_writebacks() << '\n';
            return 1;
        }
        auto segment_fix = segment_data;
        segment_fix.pdest = 199;
        segment_fix.expected_exception_mask =
            memblock::kExceptionStorePageFault;
        segment_fix.expected_vl = 1;
        segment_fix.expected_writeback_vstart = 0;
        segment_fix.vuop_idx = 1;
        segment_fix.last_uop = true;
        segment_fix.vl_wen = true;
        segment_first.expect_vector_data(segment_fix, {});
        if (!segment_first.issue_vector(segment_fix, 1024) ||
            !segment_first.run_until_vector_complete(16384) ||
            segment_first.vector_store_writebacks() != 2 ||
            segment_first.sq_dequeued() !=
                segment_first.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_first.cycle()
                      << " phase=segment-first-field-fix reason="
                      << segment_first.error() << " writebacks="
                      << segment_first.vector_store_writebacks()
                      << " sq=" << segment_first.sq_dequeued() << '/'
                      << segment_first.sq_allocated() << '\n';
            return 1;
        }
        if (!segment_first.redirect_after(
                segment_data.rob, segment_data.rob_flag, true, true) ||
            !segment_first.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_first.cycle()
                      << " phase=segment-first-field-redirect reason="
                      << segment_first.error() << '\n';
            return 1;
        }
        std::array<memblock::LoadTransaction, 2> readbacks{};
        readbacks[0] = memblock::LoadTransaction{
            .address = first_virtual + 0xff8,
            .oracle_address = first_readonly + 0xff8,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 200,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        readbacks[1] = memblock::LoadTransaction{
            .address = first_virtual + 0x1000,
            .oracle_address = first_writable,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 1,
            .pdest = 201,
            .lane = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        segment_first.expect_load_data(readbacks[0], readonly_word);
        segment_first.expect_load_data(readbacks[1], writable_word);
        if (!segment_first.set_rob_head(
                readbacks[0].rob, readbacks[0].rob_flag) ||
            !segment_first.enqueue_load_batch(
                {readbacks[0], readbacks[1]}, {0, 1}) ||
            !segment_first.issue_load_batch(
                {readbacks[0], readbacks[1]}, 4096) ||
            !segment_first.run_until_all_complete(16384) ||
            !segment_first.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << segment_first.cycle()
                      << " phase=segment-first-field-readback reason="
                      << segment_first.error() << '\n';
            return 1;
        }
        ++vector_segment_first_field_fault_readbacks;
    }

    // PMP-denied vector store.  The page is readable but not writable, so the
    // readback is legal while the store must raise store access fault and
    // must not issue a cache/uncache request or modify memory.
    {
        memblock::Environment pmp_vector(argc, argv);
        constexpr std::uint64_t pmp_base =
            memblock::kDefaultMemoryBase + 0x360000ULL;
        constexpr std::uint8_t pmp_napot_read = 0x19;
        constexpr std::uint8_t pmp_napot_read_write_execute = 0x1f;
        pmp_vector.memory().fill_incrementing(pmp_base, 0x1000, 0x71);
        const std::uint64_t original = pmp_vector.memory().read_u64(
            pmp_base + 0x8);
        pmp_vector.configure_backpressure(0x5be0cd19137e2179ULL, true);
        const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
            return (base | (size / 2 - 1)) >> 2;
        };
        if (!pmp_vector.reset() || !pmp_vector.activate_bare() ||
            !pmp_vector.configure_pmp(
                {napot_address(pmp_base, 0x1000), ~std::uint64_t{0}},
                {pmp_napot_read, pmp_napot_read_write_execute})) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_vector.cycle()
                      << " phase=pmp-vector-store-configuration reason="
                      << pmp_vector.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = pmp_base + 0x8,
            .eew = 3,
            .vl = 1,
            .rob = 107,
            .sq = 0,
            .pdest = 204,
            .lane = 0,
            .flow_num = 1,
            .expected_exception_mask =
                memblock::kExceptionStoreAccessFault,
            .expected_writeback_vstart = 0,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : store.data) {
            byte = static_cast<unsigned char>(0x7f);
        }
        const std::uint64_t dcache_before = pmp_vector.tilelink_requests();
        const std::uint64_t uncache_before = pmp_vector.uncache_requests();
        pmp_vector.select_store_exception_address(true);
        pmp_vector.expect_vector(store);
        if (!pmp_vector.set_rob_head(store.rob, store.rob_flag) ||
            !pmp_vector.enqueue_vector(store) ||
            !pmp_vector.issue_vector(store, 1024) ||
            !pmp_vector.run_until_vector_complete_with_replays(
                store, 32768) ||
            !pmp_vector.run_cycles(4) ||
            pmp_vector.exception_vaddr() != pmp_base + 0x8 ||
            pmp_vector.tilelink_requests() != dcache_before ||
            pmp_vector.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_vector.cycle()
                      << " phase=pmp-vector-store reason="
                      << pmp_vector.error() << " vaddr=0x" << std::hex
                      << pmp_vector.exception_vaddr() << std::dec
                      << " dcache=" << pmp_vector.tilelink_requests()
                      << " uncache=" << pmp_vector.uncache_requests() << '\n';
            return 1;
        }
        const std::uint64_t retired =
            pmp_vector.sq_dequeued() + pmp_vector.sq_canceled();
        if (retired > pmp_vector.sq_allocated() ||
            (retired < pmp_vector.sq_allocated() &&
             !pmp_vector.account_sq_cancellation(
                 static_cast<unsigned>(
                     pmp_vector.sq_allocated() - retired))) ||
            !pmp_vector.redirect_after(
                store.rob, store.rob_flag, true, true) ||
            !pmp_vector.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_vector.cycle()
                      << " phase=pmp-vector-store-accounting reason="
                      << pmp_vector.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = pmp_base + 0x8,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 205,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        pmp_vector.expect_load_data(readback, original);
        if (!pmp_vector.set_rob_head(readback.rob, readback.rob_flag) ||
            !pmp_vector.enqueue_load(readback) ||
            !pmp_vector.issue_load(readback, 4096) ||
            !pmp_vector.run_until_complete(16384) ||
            !pmp_vector.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_vector.cycle()
                      << " phase=pmp-vector-store-readback reason="
                      << pmp_vector.error() << '\n';
            return 1;
        }
        pmp_vector.select_store_exception_address(false);
        ++pmp_vector_store_fault_readbacks;
    }

    // Masked PMP-denied vector store: only element 1 is active, and it is in
    // a PMP read-only page.  The store must fault at element 1 with vstart=1,
    // not at the masked-off element 0, and must not write either address.
    {
        memblock::Environment pmp_masked(argc, argv);
        constexpr std::uint64_t pmp_base =
            memblock::kDefaultMemoryBase + 0x370000ULL;
        constexpr std::uint8_t pmp_napot_read = 0x19;
        constexpr std::uint8_t pmp_napot_read_write_execute = 0x1f;
        pmp_masked.memory().fill_incrementing(pmp_base, 0x1000, 0x73);
        const std::uint64_t low_word = pmp_masked.memory().read_u64(
            pmp_base + 0x8);
        const std::uint64_t high_word = pmp_masked.memory().read_u64(
            pmp_base + 0x10);
        pmp_masked.configure_backpressure(0x1f83d9abfb41bd6bULL, true);
        const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
            return (base | (size / 2 - 1)) >> 2;
        };
        if (!pmp_masked.reset() || !pmp_masked.activate_bare() ||
            !pmp_masked.configure_pmp(
                {napot_address(pmp_base, 0x1000), ~std::uint64_t{0}},
                {pmp_napot_read, pmp_napot_read_write_execute})) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_masked.cycle()
                      << " phase=pmp-masked-store-configuration reason="
                      << pmp_masked.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = pmp_base + 0x8,
            .eew = 3,
            .vl = 2,
            .vm = false,
            .mask_bits = 0x0002,
            .rob = 108,
            .sq = 0,
            .pdest = 206,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStoreAccessFault,
            .expected_writeback_vstart = 1,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : store.data) {
            byte = static_cast<unsigned char>(0x81);
        }
        const std::uint64_t dcache_before = pmp_masked.tilelink_requests();
        const std::uint64_t uncache_before = pmp_masked.uncache_requests();
        pmp_masked.select_store_exception_address(true);
        pmp_masked.expect_vector(store);
        if (!pmp_masked.set_rob_head(store.rob, store.rob_flag) ||
            !pmp_masked.enqueue_vector(store) ||
            !pmp_masked.issue_vector(store, 1024) ||
            !pmp_masked.run_until_vector_complete_with_replays(
                store, 32768) ||
            !pmp_masked.run_cycles(4) ||
            pmp_masked.exception_vaddr() != pmp_base + 0x10 ||
            pmp_masked.tilelink_requests() != dcache_before ||
            pmp_masked.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_masked.cycle()
                      << " phase=pmp-masked-store reason="
                      << pmp_masked.error() << " vaddr=0x" << std::hex
                      << pmp_masked.exception_vaddr() << std::dec
                      << " dcache=" << pmp_masked.tilelink_requests()
                      << " uncache=" << pmp_masked.uncache_requests() << '\n';
            return 1;
        }
        const std::uint64_t retired =
            pmp_masked.sq_dequeued() + pmp_masked.sq_canceled();
        if (retired > pmp_masked.sq_allocated() ||
            (retired < pmp_masked.sq_allocated() &&
             !pmp_masked.account_sq_cancellation(
                 static_cast<unsigned>(
                     pmp_masked.sq_allocated() - retired))) ||
            !pmp_masked.redirect_after(
                store.rob, store.rob_flag, true, true) ||
            !pmp_masked.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_masked.cycle()
                      << " phase=pmp-masked-store-accounting reason="
                      << pmp_masked.error() << '\n';
            return 1;
        }
        std::array<memblock::LoadTransaction, 2> readbacks{};
        readbacks[0] = memblock::LoadTransaction{
            .address = pmp_base + 0x8,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 207,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        readbacks[1] = memblock::LoadTransaction{
            .address = pmp_base + 0x10,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 1,
            .pdest = 208,
            .lane = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        pmp_masked.expect_load_data(readbacks[0], low_word);
        pmp_masked.expect_load_data(readbacks[1], high_word);
        if (!pmp_masked.set_rob_head(
                readbacks[0].rob, readbacks[0].rob_flag) ||
            !pmp_masked.enqueue_load_batch(
                {readbacks[0], readbacks[1]}, {0, 1}) ||
            !pmp_masked.issue_load_batch(
                {readbacks[0], readbacks[1]}, 4096) ||
            !pmp_masked.run_until_all_complete(16384) ||
            !pmp_masked.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_masked.cycle()
                      << " phase=pmp-masked-store-readback reason="
                      << pmp_masked.error() << '\n';
            return 1;
        }
        pmp_masked.select_store_exception_address(false);
        ++pmp_masked_vector_store_fault_readbacks;
    }

    // PMP boundary crossing inside one vector store: element 0 is in a
    // PMP read/write page, element 1 crosses into a PMP read-only page.  The
    // store must fault at element 1 with vstart=1 and must not write the
    // read-only page, even though the base address is writable.
    {
        memblock::Environment pmp_cross(argc, argv);
        constexpr std::uint64_t pmp_base =
            memblock::kDefaultMemoryBase + 0x380000ULL;
        constexpr std::uint64_t pmp_high = pmp_base + 0x1000;
        constexpr std::uint8_t pmp_napot_read = 0x19;
        constexpr std::uint8_t pmp_napot_read_write = 0x1b;
        pmp_cross.memory().fill_incrementing(pmp_base + 0xff8, 8, 0x75);
        pmp_cross.memory().fill_incrementing(pmp_high, 0x20, 0xc4);
        const std::uint64_t high_word = pmp_cross.memory().read_u64(pmp_high);
        pmp_cross.configure_backpressure(0x3c6ef372fe94f82bULL, true);
        const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
            return (base | (size / 2 - 1)) >> 2;
        };
        if (!pmp_cross.reset() || !pmp_cross.activate_bare() ||
            !pmp_cross.configure_pmp(
                {napot_address(pmp_base, 0x1000),
                 napot_address(pmp_high, 0x1000)},
                {pmp_napot_read_write, pmp_napot_read})) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_cross.cycle()
                      << " phase=pmp-cross-store-configuration reason="
                      << pmp_cross.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = pmp_base + 0xff8,
            .eew = 3,
            .vl = 2,
            .rob = 109,
            .sq = 0,
            .pdest = 209,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStoreAccessFault,
            .expected_writeback_vstart = 1,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : store.data) {
            byte = static_cast<unsigned char>(0x8d);
        }
        const std::uint64_t dcache_before = pmp_cross.tilelink_requests();
        const std::uint64_t uncache_before = pmp_cross.uncache_requests();
        pmp_cross.select_store_exception_address(true);
        pmp_cross.expect_vector(store);
        if (!pmp_cross.set_rob_head(store.rob, store.rob_flag) ||
            !pmp_cross.enqueue_vector(store) ||
            !pmp_cross.issue_vector(store, 1024) ||
            !pmp_cross.run_until_vector_complete_with_replays(
                store, 32768) ||
            !pmp_cross.run_cycles(4) ||
            pmp_cross.exception_vaddr() != pmp_high ||
            pmp_cross.tilelink_requests() != dcache_before ||
            pmp_cross.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_cross.cycle()
                      << " phase=pmp-cross-store reason="
                      << pmp_cross.error() << " vaddr=0x" << std::hex
                      << pmp_cross.exception_vaddr() << std::dec
                      << " dcache=" << pmp_cross.tilelink_requests()
                      << " uncache=" << pmp_cross.uncache_requests() << '\n';
            return 1;
        }
        const std::uint64_t retired =
            pmp_cross.sq_dequeued() + pmp_cross.sq_canceled();
        if (retired > pmp_cross.sq_allocated() ||
            (retired < pmp_cross.sq_allocated() &&
             !pmp_cross.account_sq_cancellation(
                 static_cast<unsigned>(
                     pmp_cross.sq_allocated() - retired))) ||
            !pmp_cross.redirect_after(
                store.rob, store.rob_flag, true, true) ||
            !pmp_cross.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_cross.cycle()
                      << " phase=pmp-cross-store-accounting reason="
                      << pmp_cross.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = pmp_high,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 210,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        pmp_cross.expect_load_data(readback, high_word);
        if (!pmp_cross.set_rob_head(readback.rob, readback.rob_flag) ||
            !pmp_cross.enqueue_load(readback) ||
            !pmp_cross.issue_load(readback, 4096) ||
            !pmp_cross.run_until_complete(16384) ||
            !pmp_cross.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_cross.cycle()
                      << " phase=pmp-cross-store-readback reason="
                      << pmp_cross.error() << '\n';
            return 1;
        }
        pmp_cross.select_store_exception_address(false);
        ++pmp_cross_page_vector_store_fault_readbacks;
    }

    // PMP segment-store fault: field 0 is in a PMP read/write page and
    // field 1 in a PMP read-only page.  The segment data and fix-VL uops must
    // fault with vstart=0, no request may issue, and the read-only field page
    // must stay unchanged.
    {
        memblock::Environment pmp_segment(argc, argv);
        constexpr std::uint64_t segment_base =
            memblock::kDefaultMemoryBase + 0x390000ULL;
        constexpr std::uint64_t segment_high = segment_base + 0x1000;
        constexpr std::uint8_t pmp_napot_read = 0x19;
        constexpr std::uint8_t pmp_napot_read_write = 0x1b;
        pmp_segment.memory().fill_incrementing(
            segment_base + 0xff8, 8, 0x77);
        pmp_segment.memory().fill_incrementing(
            segment_high, 0x20, 0xd6);
        const std::uint64_t high_word =
            pmp_segment.memory().read_u64(segment_high);
        pmp_segment.configure_backpressure(0x9b05688c2b3e6c1fULL, true);
        const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
            return (base | (size / 2 - 1)) >> 2;
        };
        if (!pmp_segment.reset() || !pmp_segment.activate_bare() ||
            !pmp_segment.configure_pmp(
                {napot_address(segment_base, 0x1000),
                 napot_address(segment_high, 0x1000)},
                {pmp_napot_read_write, pmp_napot_read})) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_segment.cycle()
                      << " phase=pmp-segment-store-configuration reason="
                      << pmp_segment.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction segment_data{
            .store = true,
            .segment = true,
            .address = segment_base + 0xff8,
            .eew = 3,
            .vl = 1,
            .rob = 110,
            .sq = 0,
            .pdest = 211,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStoreAccessFault,
            .expected_writeback_vstart = 0,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = false,
            .nf = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : segment_data.data) {
            byte = static_cast<unsigned char>(0x8f);
        }
        const std::uint64_t dcache_before = pmp_segment.tilelink_requests();
        const std::uint64_t uncache_before = pmp_segment.uncache_requests();
        pmp_segment.select_store_exception_address(true);
        pmp_segment.expect_vector(segment_data);
        if (!pmp_segment.set_rob_head(
                segment_data.rob, segment_data.rob_flag) ||
            !pmp_segment.enqueue_vector(segment_data) ||
            !pmp_segment.issue_vector(segment_data, 1024) ||
            !pmp_segment.run_until_vector_complete_with_replays(
                segment_data, 32768) ||
            !pmp_segment.run_cycles(4) ||
            pmp_segment.exception_vaddr() != segment_high ||
            pmp_segment.tilelink_requests() != dcache_before ||
            pmp_segment.uncache_requests() != uncache_before ||
            pmp_segment.vector_store_writebacks() != 1) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_segment.cycle()
                      << " phase=pmp-segment-store-data reason="
                      << pmp_segment.error() << " vaddr=0x" << std::hex
                      << pmp_segment.exception_vaddr() << std::dec
                      << " dcache=" << pmp_segment.tilelink_requests()
                      << " uncache=" << pmp_segment.uncache_requests()
                      << " writebacks="
                      << pmp_segment.vector_store_writebacks() << '\n';
            return 1;
        }
        auto segment_fix = segment_data;
        segment_fix.pdest = 212;
        segment_fix.expected_exception_mask =
            memblock::kExceptionStoreAccessFault;
        segment_fix.expected_vl = 1;
        segment_fix.expected_writeback_vstart = 0;
        segment_fix.vuop_idx = 1;
        segment_fix.last_uop = true;
        segment_fix.vl_wen = true;
        pmp_segment.expect_vector_data(segment_fix, {});
        if (!pmp_segment.issue_vector(segment_fix, 1024) ||
            !pmp_segment.run_until_vector_complete(16384) ||
            pmp_segment.vector_store_writebacks() != 2 ||
            pmp_segment.sq_dequeued() != pmp_segment.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_segment.cycle()
                      << " phase=pmp-segment-store-fix reason="
                      << pmp_segment.error() << " writebacks="
                      << pmp_segment.vector_store_writebacks()
                      << " sq=" << pmp_segment.sq_dequeued() << '/'
                      << pmp_segment.sq_allocated() << '\n';
            return 1;
        }
        if (!pmp_segment.redirect_after(
                segment_data.rob, segment_data.rob_flag, true, true) ||
            !pmp_segment.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_segment.cycle()
                      << " phase=pmp-segment-store-redirect reason="
                      << pmp_segment.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = segment_high,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 213,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        pmp_segment.expect_load_data(readback, high_word);
        if (!pmp_segment.set_rob_head(readback.rob, readback.rob_flag) ||
            !pmp_segment.enqueue_load(readback) ||
            !pmp_segment.issue_load(readback, 4096) ||
            !pmp_segment.run_until_complete(16384) ||
            !pmp_segment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_segment.cycle()
                      << " phase=pmp-segment-store-readback reason="
                      << pmp_segment.error() << '\n';
            return 1;
        }
        pmp_segment.select_store_exception_address(false);
        ++pmp_segment_store_fault_readbacks;
    }

    // First-field PMP segment-store fault: field 0 is in a PMP read-only
    // page and field 1 in a PMP read/write page.  The store must fault at
    // field 0, and neither the read-only field page nor the following
    // writable field page may be written.
    {
        memblock::Environment pmp_first(argc, argv);
        constexpr std::uint64_t segment_base =
            memblock::kDefaultMemoryBase + 0x3a0000ULL;
        constexpr std::uint64_t segment_high = segment_base + 0x1000;
        constexpr std::uint8_t pmp_napot_read = 0x19;
        constexpr std::uint8_t pmp_napot_read_write = 0x1b;
        pmp_first.memory().fill_incrementing(segment_base + 0xff8, 8, 0x79);
        pmp_first.memory().fill_incrementing(segment_high, 0x20, 0xd8);
        const std::uint64_t low_word =
            pmp_first.memory().read_u64(segment_base + 0xff8);
        const std::uint64_t high_word =
            pmp_first.memory().read_u64(segment_high);
        pmp_first.configure_backpressure(0x510e527fade682d1ULL, true);
        const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
            return (base | (size / 2 - 1)) >> 2;
        };
        if (!pmp_first.reset() || !pmp_first.activate_bare() ||
            !pmp_first.configure_pmp(
                {napot_address(segment_base, 0x1000),
                 napot_address(segment_high, 0x1000)},
                {pmp_napot_read, pmp_napot_read_write})) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_first.cycle()
                      << " phase=pmp-segment-first-configuration reason="
                      << pmp_first.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction segment_data{
            .store = true,
            .segment = true,
            .address = segment_base + 0xff8,
            .eew = 3,
            .vl = 1,
            .rob = 111,
            .sq = 0,
            .pdest = 215,
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask =
                memblock::kExceptionStoreAccessFault,
            .expected_writeback_vstart = 0,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = false,
            .nf = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : segment_data.data) {
            byte = static_cast<unsigned char>(0x91);
        }
        const std::uint64_t dcache_before = pmp_first.tilelink_requests();
        const std::uint64_t uncache_before = pmp_first.uncache_requests();
        pmp_first.select_store_exception_address(true);
        pmp_first.expect_vector(segment_data);
        if (!pmp_first.set_rob_head(
                segment_data.rob, segment_data.rob_flag) ||
            !pmp_first.enqueue_vector(segment_data) ||
            !pmp_first.issue_vector(segment_data, 1024) ||
            !pmp_first.run_until_vector_complete_with_replays(
                segment_data, 32768) ||
            !pmp_first.run_cycles(4) ||
            pmp_first.exception_vaddr() != segment_base + 0xff8 ||
            pmp_first.tilelink_requests() != dcache_before ||
            pmp_first.uncache_requests() != uncache_before ||
            pmp_first.vector_store_writebacks() != 1) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_first.cycle()
                      << " phase=pmp-segment-first-data reason="
                      << pmp_first.error() << " vaddr=0x" << std::hex
                      << pmp_first.exception_vaddr() << std::dec
                      << " dcache=" << pmp_first.tilelink_requests()
                      << " uncache=" << pmp_first.uncache_requests()
                      << " writebacks="
                      << pmp_first.vector_store_writebacks() << '\n';
            return 1;
        }
        auto segment_fix = segment_data;
        segment_fix.pdest = 216;
        segment_fix.expected_exception_mask =
            memblock::kExceptionStoreAccessFault;
        segment_fix.expected_vl = 1;
        segment_fix.expected_writeback_vstart = 0;
        segment_fix.vuop_idx = 1;
        segment_fix.last_uop = true;
        segment_fix.vl_wen = true;
        pmp_first.expect_vector_data(segment_fix, {});
        if (!pmp_first.issue_vector(segment_fix, 1024) ||
            !pmp_first.run_until_vector_complete(16384) ||
            pmp_first.vector_store_writebacks() != 2 ||
            pmp_first.sq_dequeued() != pmp_first.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_first.cycle()
                      << " phase=pmp-segment-first-fix reason="
                      << pmp_first.error() << " writebacks="
                      << pmp_first.vector_store_writebacks()
                      << " sq=" << pmp_first.sq_dequeued() << '/'
                      << pmp_first.sq_allocated() << '\n';
            return 1;
        }
        if (!pmp_first.redirect_after(
                segment_data.rob, segment_data.rob_flag, true, true) ||
            !pmp_first.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_first.cycle()
                      << " phase=pmp-segment-first-redirect reason="
                      << pmp_first.error() << '\n';
            return 1;
        }
        std::array<memblock::LoadTransaction, 2> readbacks{};
        readbacks[0] = memblock::LoadTransaction{
            .address = segment_base + 0xff8,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 217,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        readbacks[1] = memblock::LoadTransaction{
            .address = segment_high,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 1,
            .pdest = 218,
            .lane = 1,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        pmp_first.expect_load_data(readbacks[0], low_word);
        pmp_first.expect_load_data(readbacks[1], high_word);
        if (!pmp_first.set_rob_head(
                readbacks[0].rob, readbacks[0].rob_flag) ||
            !pmp_first.enqueue_load_batch(
                {readbacks[0], readbacks[1]}, {0, 1}) ||
            !pmp_first.issue_load_batch(
                {readbacks[0], readbacks[1]}, 4096) ||
            !pmp_first.run_until_all_complete(16384) ||
            !pmp_first.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_first.cycle()
                      << " phase=pmp-segment-first-readback reason="
                      << pmp_first.error() << '\n';
            return 1;
        }
        pmp_first.select_store_exception_address(false);
        ++pmp_segment_first_field_fault_readbacks;
    }

    // PMP-denied vector stores at EEW 1 and 2.  These widths exercise the
    // 2-byte and 4-byte element decode paths while the PMP read-only page is
    // unchanged and no manager request is issued.
    for (const unsigned eew : {1U, 2U}) {
        memblock::Environment pmp_width(argc, argv);
        constexpr std::uint64_t width_base =
            memblock::kDefaultMemoryBase + 0x3b0000ULL;
        constexpr std::uint8_t pmp_napot_read = 0x19;
        constexpr std::uint8_t pmp_napot_read_write_execute = 0x1f;
        pmp_width.memory().fill_incrementing(width_base, 0x1000, 0x7b);
        const std::uint64_t original = pmp_width.memory().read_u64(
            width_base + 0x10);
        pmp_width.configure_backpressure(
            0x13198a2e03707344ULL ^ eew, true);
        const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
            return (base | (size / 2 - 1)) >> 2;
        };
        if (!pmp_width.reset() || !pmp_width.activate_bare() ||
            !pmp_width.configure_pmp(
                {napot_address(width_base, 0x1000), ~std::uint64_t{0}},
                {pmp_napot_read, pmp_napot_read_write_execute})) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_width.cycle()
                      << " phase=pmp-width-store-configuration eew=" << eew
                      << " reason=" << pmp_width.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = width_base + 0x10,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = 1,
            .rob = static_cast<std::uint8_t>(112 + eew),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(219 + eew),
            .lane = 0,
            .flow_num = 1,
            .expected_exception_mask =
                memblock::kExceptionStoreAccessFault,
            .expected_writeback_vstart = 0,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : store.data) {
            byte = static_cast<unsigned char>(0x83 + eew);
        }
        const std::uint64_t dcache_before = pmp_width.tilelink_requests();
        const std::uint64_t uncache_before = pmp_width.uncache_requests();
        pmp_width.select_store_exception_address(true);
        pmp_width.expect_vector(store);
        if (!pmp_width.set_rob_head(store.rob, store.rob_flag) ||
            !pmp_width.enqueue_vector(store) ||
            !pmp_width.issue_vector(store, 1024) ||
            !pmp_width.run_until_vector_complete_with_replays(
                store, 32768) ||
            !pmp_width.run_cycles(4) ||
            pmp_width.exception_vaddr() != width_base + 0x10 ||
            pmp_width.tilelink_requests() != dcache_before ||
            pmp_width.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_width.cycle()
                      << " phase=pmp-width-store eew=" << eew
                      << " reason=" << pmp_width.error() << " vaddr=0x"
                      << std::hex << pmp_width.exception_vaddr() << std::dec
                      << " dcache=" << pmp_width.tilelink_requests()
                      << " uncache=" << pmp_width.uncache_requests() << '\n';
            return 1;
        }
        const std::uint64_t retired =
            pmp_width.sq_dequeued() + pmp_width.sq_canceled();
        if (retired > pmp_width.sq_allocated() ||
            (retired < pmp_width.sq_allocated() &&
             !pmp_width.account_sq_cancellation(
                 static_cast<unsigned>(
                     pmp_width.sq_allocated() - retired))) ||
            !pmp_width.redirect_after(
                store.rob, store.rob_flag, true, true) ||
            !pmp_width.run_until_sbuffer_empty(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_width.cycle()
                      << " phase=pmp-width-store-accounting eew=" << eew
                      << " reason=" << pmp_width.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = width_base + 0x10,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 220,
            .lane = 0,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        pmp_width.expect_load_data(readback, original);
        if (!pmp_width.set_rob_head(readback.rob, readback.rob_flag) ||
            !pmp_width.enqueue_load(readback) ||
            !pmp_width.issue_load(readback, 4096) ||
            !pmp_width.run_until_complete(16384) ||
            !pmp_width.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << pmp_width.cycle()
                      << " phase=pmp-width-store-readback eew=" << eew
                      << " reason=" << pmp_width.error() << '\n';
            return 1;
        }
        pmp_width.select_store_exception_address(false);
        ++pmp_vector_width_fault_readbacks;
    }

    // A vector load with no active element must not fault even when its base
    // address is unmapped.  Masked-off elements are not accessed.  The
    // completion is checked by the normal vector scoreboard; this case also
    // records whether the implementation speculatively issued manager traffic.
    {
        memblock::Environment all_masked(argc, argv);
        constexpr std::uint64_t unmapped_page = 0x71400000ULL;
        constexpr std::uint64_t root = 0x9aa00000ULL;
        all_masked.configure_backpressure(0xa54ff53a5f1d36f1ULL, true);
        if (!all_masked.reset() ||
            !all_masked.activate_sv39(root)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << all_masked.cycle()
                      << " phase=vector-all-masked-configuration reason="
                      << all_masked.error() << '\n';
            return 1;
        }
        const std::uint64_t writebacks_before =
            all_masked.vector_load_writebacks();
        const std::uint64_t ptw_before = all_masked.ptw_requests();
        const std::uint64_t dcache_before = all_masked.tilelink_requests();
        memblock::VectorMemoryTransaction control{
            .address = unmapped_page,
            .eew = 3,
            .vl = 2,
            .vm = false,
            .mask_bits = 0,
            .rob = 97,
            .lq = 0,
            .pdest = 182,
            .lane = 0,
            .flow_num = 1,
            .vlmul = 1,
            .vuop_idx = 0,
            .last_uop = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        for (auto &byte : control.data) {
            byte = static_cast<unsigned char>(0x6c);
        }
        all_masked.expect_vector(control);
        if (!all_masked.set_rob_head(control.rob, control.rob_flag) ||
            !all_masked.enqueue_vector(control) ||
            !all_masked.issue_vector(control, 1024) ||
            !all_masked.run_until_vector_complete_with_replays(
                control, 32768) ||
            !all_masked.run_until_lq_retired(4096) ||
            all_masked.vector_load_writebacks() != writebacks_before + 1) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL cycle="
                      << all_masked.cycle()
                      << " phase=vector-all-masked reason="
                      << all_masked.error() << " writebacks="
                      << all_masked.vector_load_writebacks()
                      << " expected=" << writebacks_before + 1
                      << " ptw=" << all_masked.ptw_requests() - ptw_before
                      << " dcache="
                      << all_masked.tilelink_requests() - dcache_before
                      << '\n';
            return 1;
        }
        vector_all_masked_ptw_requests +=
            all_masked.ptw_requests() - ptw_before;
        vector_all_masked_dcache_requests +=
            all_masked.tilelink_requests() - dcache_before;
        ++vector_all_masked_controls;
    }

    std::cout << "MEMBLOCK_TRANSLATION_FAULTS_PASS cases="
              << canonical_boundary_cases + 4 + ppn_access_fault_cases +
                    vs_gpa_width_cases +
                    pte_encoding_cases.size() * 4 + range_coverage.faults +
                    range_coverage.controls + range_coverage.readbacks +
                    range_coverage.recoveries * 2 +
                    gstage_range_coverage.faults + gstage_range_coverage.controls +
                    gstage_range_coverage.readbacks + gstage_range_coverage.recoveries * 2
              << " canonical_range_faults=" << range_coverage.faults
              << " canonical_range_controls=" << range_coverage.controls
              << " canonical_range_readbacks=" << range_coverage.readbacks
              << " canonical_range_recoveries=" << range_coverage.recoveries
              << " canonical_range_scalar_loads=" << range_coverage.scalar_loads
              << " canonical_range_scalar_stores=" << range_coverage.scalar_stores
              << " canonical_range_vector_stores=" << range_coverage.vector_stores
              << " gstage_range_faults=" << gstage_range_coverage.faults
              << " gstage_range_controls=" << gstage_range_coverage.controls
              << " gstage_range_readbacks=" << gstage_range_coverage.readbacks
              << " gstage_range_recoveries=" << gstage_range_coverage.recoveries
              << " gstage_range_scalar_loads=" << gstage_range_coverage.scalar_loads
              << " gstage_range_scalar_stores=" << gstage_range_coverage.scalar_stores
              << " gstage_range_vector_stores=" << gstage_range_coverage.vector_stores
              << " canonical_boundary_cases=" << canonical_boundary_cases
              << " vector_vstart_faults=" << vector_vstart_faults
              << " vector_later_element_faults="
              << vector_later_element_faults
              << " vector_later_address_matches="
              << vector_later_address_matches
              << " vector_all_masked_controls="
              << vector_all_masked_controls
              << " vector_store_fault_readbacks="
              << vector_store_fault_readbacks
              << " vector_masked_store_fault_readbacks="
              << vector_masked_store_fault_readbacks
              << " vector_segment_store_fault_readbacks="
              << vector_segment_store_fault_readbacks
              << " vector_segment_element_fault_readbacks="
              << vector_segment_element_fault_readbacks
              << " vector_segment_first_field_fault_readbacks="
              << vector_segment_first_field_fault_readbacks
              << " pmp_vector_store_fault_readbacks="
              << pmp_vector_store_fault_readbacks
              << " pmp_masked_vector_store_fault_readbacks="
              << pmp_masked_vector_store_fault_readbacks
              << " pmp_cross_page_vector_store_fault_readbacks="
              << pmp_cross_page_vector_store_fault_readbacks
              << " pmp_segment_store_fault_readbacks="
              << pmp_segment_store_fault_readbacks
              << " pmp_segment_first_field_fault_readbacks="
              << pmp_segment_first_field_fault_readbacks
              << " pmp_vector_width_fault_readbacks="
              << pmp_vector_width_fault_readbacks
              << " vector_first_element_store_fault_readbacks="
              << vector_first_element_store_fault_readbacks
              << " vector_masked_later_load_controls="
              << vector_masked_later_load_controls
              << " vector_all_masked_ptw_requests="
              << vector_all_masked_ptw_requests
              << " vector_all_masked_dcache_requests="
              << vector_all_masked_dcache_requests
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
              << '\n';
    return 0;
}
