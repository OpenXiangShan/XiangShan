// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_vector_load(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x20000;
    environment.memory().fill_incrementing(base, 256, 0x41);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    for (unsigned eew = 0; eew < 4; ++eew) {
        memblock::VectorMemoryTransaction transaction{
            .address = base + eew * 64,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(16U >> eew),
            .rob = static_cast<std::uint8_t>(40 + eew),
            .lq = static_cast<std::uint8_t>(eew * 2),
            .pdest = static_cast<std::uint8_t>(80 + eew),
            .lane = eew % memblock::kVectorMemoryLanes,
        };
        if (transaction.lane == 0) {
            transaction.expected_trigger = memblock::kVectorWritebackTriggerNone;
            transaction.expected_debug_is_mmio = false;
            transaction.expected_debug_is_ncio = false;
            transaction.expected_debug_is_perf_cnt = false;
        }
        for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
            transaction.data[byte] = static_cast<unsigned char>(0xa0 + byte);
        }
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction) ||
            !environment.run_until_vector_complete(512)) {
            std::cerr << "MEMBLOCK_VECTOR_LOAD_FAIL"
                      << " width=" << (8U << eew)
                      << " lane=" << transaction.lane
                      << " cycle=" << environment.cycle()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_VECTOR_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_VECTOR_LOAD_PASS"
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " writebacks=" << environment.vector_load_writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_split_load(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t address = memblock::kDefaultMemoryBase + 0x371dc;
    constexpr std::uint64_t inactive_address =
        memblock::kDefaultMemoryBase + 0x3db81;
    environment.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x53);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_SPLIT_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::array<memblock::VectorMemoryTransaction, 3> transactions{{
        {
            .address = inactive_address,
            .eew = 0,
            .vl = 3,
            .vstart = 1,
            .vm = false,
            .mask_bits = 0,
            .rob = 38,
            .lq = 0,
            .pdest = 78,
            .lane = 1,
        },
        {
            .address = address - 0x14,
            .eew = 3,
            .vl = 0,
            .rob = 39,
            .lq = 2,
            .pdest = 79,
            .lane = 1,
        },
        {
        .address = address,
        .eew = 0,
        .vl = 12,
        .vstart = 1,
        .vm = true,
        .rob = 40,
        .lq = 4,
        .pdest = 80,
        .lane = 0,
        },
    }};
    for (unsigned index = 0; index < transactions.size(); ++index) {
        auto &transaction = transactions[index];
        for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
            transaction.data[byte] = static_cast<unsigned char>(
                0xa0 + index * 16 + byte);
        }
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction) ||
            !environment.run_until_vector_complete(1024)) {
            std::cerr << "MEMBLOCK_VECTOR_SPLIT_LOAD_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " requests=" << environment.tilelink_requests()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_VECTOR_SPLIT_LOAD_FAIL cycle="
                  << environment.cycle() << " reason=" << environment.error()
                  << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_VECTOR_SPLIT_LOAD_PASS"
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " writebacks=" << environment.vector_load_writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_fault_only_first(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_page = 0x52000000ULL;
    constexpr std::uint64_t physical_page =
        memblock::kDefaultMemoryBase + 0x5a000;
    constexpr std::uint64_t root = 0x97900000ULL;
    constexpr std::uint64_t address = virtual_page + 0xff8;
    constexpr std::uint64_t physical_address = physical_page + 0xff8;
    environment.memory().fill_incrementing(physical_address, 8, 0x6d);
    environment.configure_backpressure(0x159b33b1f07c4a82ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(address, physical_address, root) ||
        !environment.activate_sv39(root, 37)) {
        std::cerr << "MEMBLOCK_VECTOR_FOF_FAIL cycle=" << environment.cycle()
                  << " phase=configuration reason=" << environment.error()
                  << '\n';
        return 1;
    }

    const auto first = memblock::reference_sv39_walk(
        environment.memory(), root, address);
    const auto second = memblock::reference_sv39_walk(
        environment.memory(), root, address + 8);
    if (!first.translated || first.physical_address != physical_address ||
        second.translated) {
        std::cerr << "MEMBLOCK_VECTOR_FOF_FAIL cycle=" << environment.cycle()
                  << " phase=reference reason=expected one mapped and one unmapped element\n";
        return 1;
    }

    // The data uop carries the original VL=2. Element zero succeeds, while
    // element one reaches the next, deliberately unmapped page. VLEFF must
    // suppress that later page fault and complete the vector result with VL=1.
    memblock::VectorMemoryTransaction data_uop{
        .address = address,
        .oracle_address = physical_address,
        .eew = 3,
        .vl = 2,
        .rob = 72,
        .lq = 0,
        .pdest = 118,
        .lane = 0,
        .flow_num = 2,
        .expected_trigger = memblock::kVectorWritebackTriggerNone,
        .vuop_idx = 0,
        .last_uop = false,
        .fault_only_first = true,
        .is_vleff = true,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
        .expected_debug_is_perf_cnt = false,
    };
    for (unsigned byte = 0; byte < data_uop.data.size(); ++byte) {
        data_uop.data[byte] = static_cast<unsigned char>(0xb0 + byte);
    }
    auto expected_data = data_uop.data;
    for (unsigned byte = 0; byte < 8; ++byte) {
        expected_data[byte] = environment.memory().read_byte(
            physical_address + byte);
    }
    environment.expect_vector_data(data_uop, expected_data);
    if (!environment.set_rob_head(data_uop.rob, data_uop.rob_flag) ||
        !environment.enqueue_vector(data_uop) ||
        !environment.issue_vector(data_uop, 512) ||
        !environment.run_until_vector_complete_with_replays(data_uop, 16384) ||
        !environment.run_until_lq_retired(4096)) {
        std::cerr << "MEMBLOCK_VECTOR_FOF_FAIL cycle=" << environment.cycle()
                  << " phase=data-uop reason=" << environment.error()
                  << " ptw_requests=" << environment.ptw_requests()
                  << " vector_replays="
                  << environment.vector_replay_feedbacks() << '\n';
        return 1;
    }

    const std::uint64_t lq_allocated_before_fix = environment.lq_allocated();
    auto fix_vl_uop = data_uop;
    fix_vl_uop.pdest = 119;
    fix_vl_uop.expected_vl = 1;
    fix_vl_uop.vuop_idx = 1;
    fix_vl_uop.last_uop = true;
    fix_vl_uop.vl_wen = true;
    fix_vl_uop.expected_debug_is_mmio.reset();
    fix_vl_uop.expected_debug_is_ncio.reset();
    fix_vl_uop.expected_debug_is_perf_cnt.reset();
    environment.expect_vector_data(fix_vl_uop, {});
    if (!environment.issue_vector(fix_vl_uop, 512) ||
        !environment.run_until_vector_complete(2048) ||
        environment.lq_allocated() != lq_allocated_before_fix ||
        environment.lq_dequeued() != environment.lq_allocated() ||
        environment.vector_fof_fix_writebacks() != 1) {
        std::cerr << "MEMBLOCK_VECTOR_FOF_FAIL cycle=" << environment.cycle()
                  << " phase=fix-vl-uop reason=" << environment.error()
                  << " lq=" << environment.lq_dequeued() << '/'
                  << environment.lq_allocated()
                  << " fix_writebacks="
                  << environment.vector_fof_fix_writebacks() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_VECTOR_FOF_PASS"
              << " cycle=" << environment.cycle()
              << " original_vl=2 final_vl=1"
              << " data_writebacks="
              << environment.vector_load_writebacks()
              << " fix_vl_writebacks="
              << environment.vector_fof_fix_writebacks()
              << " ptw_requests=" << environment.ptw_requests()
              << " vector_replays="
              << environment.vector_replay_feedbacks()
              << " lq_allocated=" << environment.lq_allocated()
              << " lq_dequeued=" << environment.lq_dequeued()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_segment(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t load_base = memblock::kDefaultMemoryBase + 0x5c000;
    constexpr std::uint64_t store_base = memblock::kDefaultMemoryBase + 0x5c100;
    environment.memory().fill_incrementing(load_base, 32, 0x31);
    environment.memory().fill_incrementing(store_base, 32, 0xe1);
    environment.configure_backpressure(0x7c9e4d11a6b328f0ULL, true);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle=" << environment.cycle()
                  << " phase=reset reason=" << environment.error() << '\n';
        return 1;
    }

    const auto make_segment = [](
        bool store, std::uint64_t address, std::uint8_t rob,
        std::uint8_t vuop_idx, std::uint8_t pdest) {
        memblock::VectorMemoryTransaction transaction{
            .store = store,
            .segment = true,
            .address = address,
            .eew = 3,
            .vl = 2,
            .rob = rob,
            .pdest = pdest,
            .lane = 0,
            .flow_num = 2,
            .expected_trigger = memblock::kVectorWritebackTriggerNone,
            .vuop_idx = vuop_idx,
            .last_uop = vuop_idx == 1,
            .nf = 1,
        };
        for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
            transaction.data[byte] = static_cast<unsigned char>(
                (store ? 0x70 : 0xa0) + vuop_idx * 0x20 + byte);
        }
        return transaction;
    };

    std::array<memblock::VectorMemoryTransaction, 2> loads{{
        make_segment(false, load_base, 73, 0, 120),
        make_segment(false, load_base, 73, 1, 121),
    }};
    for (unsigned field = 0; field < loads.size(); ++field) {
        auto expected = loads[field].data;
        for (unsigned element = 0; element < 2; ++element) {
            for (unsigned byte = 0; byte < 8; ++byte) {
                expected[element * 8 + byte] = environment.memory().read_byte(
                    load_base + (element * 2 + field) * 8 + byte);
            }
        }
        environment.expect_vector_data(loads[field], expected);
        if (!environment.issue_vector(loads[field], 512)) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                      << environment.cycle() << " phase=load-issue field="
                      << field << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.run_until_vector_complete(32768) ||
        environment.vector_load_writebacks() != 2 ||
        environment.lq_allocated() != 0 || environment.sq_allocated() != 0) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle=" << environment.cycle()
                  << " phase=load-complete reason=" << environment.error()
                  << " writebacks=" << environment.vector_load_writebacks()
                  << " lq=" << environment.lq_allocated()
                  << " sq=" << environment.sq_allocated() << '\n';
        return 1;
    }

    std::array<memblock::VectorMemoryTransaction, 2> stores{{
        make_segment(true, store_base, 74, 0, 0),
        make_segment(true, store_base, 74, 1, 0),
    }};
    for (unsigned field = 0; field < stores.size(); ++field) {
        environment.expect_vector(stores[field]);
        if (!environment.issue_vector(stores[field], 512)) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                      << environment.cycle() << " phase=store-issue field="
                      << field << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.run_until_vector_complete(32768) ||
        environment.vector_store_writebacks() != 2 ||
        environment.lq_allocated() != 0 || environment.sq_allocated() != 0 ||
        !environment.pulse_sbuffer_flush() ||
        !environment.run_until_sbuffer_empty(32768)) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle=" << environment.cycle()
                  << " phase=store-complete reason=" << environment.error()
                  << " writebacks=" << environment.vector_store_writebacks()
                  << " lq=" << environment.lq_allocated()
                  << " sq=" << environment.sq_allocated() << '\n';
        return 1;
    }

    for (unsigned element = 0; element < 2; ++element) {
        memblock::VectorMemoryTransaction readback{
            .address = store_base + element * 16,
            .eew = 3,
            .vl = 2,
            .rob = static_cast<std::uint8_t>(75 + element),
            .lq = static_cast<std::uint8_t>(element * 2),
            .pdest = static_cast<std::uint8_t>(122 + element),
            .lane = element,
            .flow_num = 2,
            .expected_trigger = memblock::kVectorWritebackTriggerNone,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        std::array<unsigned char, 16> expected{};
        std::copy_n(
            stores[0].data.begin() + element * 8, 8, expected.begin());
        std::copy_n(
            stores[1].data.begin() + element * 8, 8, expected.begin() + 8);
        environment.expect_vector_data(readback, expected);
        if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
            !environment.enqueue_vector(readback) ||
            !environment.issue_vector(readback, 512) ||
            !environment.run_until_vector_complete_with_replays(readback, 16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                      << environment.cycle() << " phase=store-readback element="
                      << element << " reason=" << environment.error() << '\n';
            return 1;
        }
    }

    const std::array<memblock::VectorAddressingMode, 3> addressing_modes{{
        memblock::VectorAddressingMode::strided,
        memblock::VectorAddressingMode::indexed_unordered,
        memblock::VectorAddressingMode::indexed_ordered,
    }};
    unsigned addressing_load_writebacks = 0;
    unsigned addressing_store_writebacks = 0;
    std::uint64_t addressing_cycles = 0;
    std::uint64_t addressing_dcache_requests = 0;
    for (unsigned mode_index = 0; mode_index < addressing_modes.size();
         ++mode_index) {
        memblock::Environment mode_environment(argc, argv);
        const auto mode = addressing_modes[mode_index];
        const std::uint64_t mode_load_base =
            memblock::kDefaultMemoryBase + 0x60000 + mode_index * 0x1000;
        const std::uint64_t mode_store_base = mode_load_base + 0x400;
        mode_environment.memory().fill_incrementing(
            mode_load_base, 0x800, static_cast<std::uint8_t>(0x43 + mode_index));
        mode_environment.configure_backpressure(
            0x3c6ef372fe94f82bULL ^ mode_index, true);
        if (!mode_environment.reset()) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL phase=addressing-reset"
                      << " mode=" << mode_index << " reason="
                      << mode_environment.error() << '\n';
            return 1;
        }

        const auto make_addressed_field = [&](bool store, std::uint64_t base,
                                               std::uint8_t rob,
                                               unsigned field) {
            memblock::VectorMemoryTransaction transaction{
                .store = store,
                .segment = true,
                .address = base,
                .stride = 24,
                .addressing = mode,
                .eew = 2,
                .vl = 4,
                .rob = rob,
                .pdest = static_cast<std::uint8_t>(100 + field),
                .lane = 0,
                .flow_num = 4,
                .expected_trigger = memblock::kVectorWritebackTriggerNone,
                .vuop_idx = static_cast<std::uint8_t>(field),
                .last_uop = field == 1,
                .nf = 1,
            };
            for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
                transaction.data[byte] = static_cast<unsigned char>(
                    0x90 + mode_index * 0x10 + field * 0x20 + byte);
            }
            for (unsigned element = 0; element < 4; ++element) {
                const std::uint32_t offset = element * 32;
                for (unsigned byte = 0; byte < 4; ++byte) {
                    transaction.index[element * 4 + byte] =
                        static_cast<unsigned char>(offset >> (8 * byte));
                }
            }
            return transaction;
        };

        std::array<memblock::VectorMemoryTransaction, 2> mode_loads{{
            make_addressed_field(false, mode_load_base, 80, 0),
            make_addressed_field(false, mode_load_base, 80, 1),
        }};
        for (const auto &load : mode_loads) {
            mode_environment.expect_vector(load);
            if (!mode_environment.issue_vector(load, 1024)) {
                std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL"
                          << " phase=addressed-load-issue mode=" << mode_index
                          << " reason=" << mode_environment.error() << '\n';
                return 1;
            }
        }
        if (!mode_environment.run_until_vector_complete(32768) ||
            mode_environment.vector_load_writebacks() != 2) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL"
                      << " phase=addressed-load-complete mode=" << mode_index
                      << " reason=" << mode_environment.error() << '\n';
            return 1;
        }

        std::array<memblock::VectorMemoryTransaction, 2> mode_stores{{
            make_addressed_field(true, mode_store_base, 81, 0),
            make_addressed_field(true, mode_store_base, 81, 1),
        }};
        for (const auto &store : mode_stores) {
            mode_environment.expect_vector(store);
            if (!mode_environment.issue_vector(store, 1024)) {
                std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL"
                          << " phase=addressed-store-issue mode=" << mode_index
                          << " reason=" << mode_environment.error() << '\n';
                return 1;
            }
        }
        if (!mode_environment.run_until_vector_complete(32768) ||
            mode_environment.vector_store_writebacks() != 2) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL"
                      << " phase=addressed-store-complete mode=" << mode_index
                      << " reason=" << mode_environment.error() << '\n';
            return 1;
        }
        for (const auto &store : mode_stores) {
            mode_environment.record_committed_vector_store(store);
        }
        if (!mode_environment.pulse_sbuffer_flush() ||
            !mode_environment.run_until_sbuffer_empty(32768)) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL"
                      << " phase=addressed-store-drain mode=" << mode_index
                      << " reason=" << mode_environment.error() << '\n';
            return 1;
        }

        std::array<memblock::VectorMemoryTransaction, 2> mode_readbacks{{
            make_addressed_field(false, mode_store_base, 82, 0),
            make_addressed_field(false, mode_store_base, 82, 1),
        }};
        for (const auto &readback : mode_readbacks) {
            mode_environment.expect_vector(readback);
            if (!mode_environment.issue_vector(readback, 1024)) {
                std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL"
                          << " phase=addressed-readback-issue mode="
                          << mode_index << " reason="
                          << mode_environment.error() << '\n';
                return 1;
            }
        }
        if (!mode_environment.run_until_vector_complete(32768) ||
            mode_environment.vector_load_writebacks() != 4 ||
            mode_environment.lq_allocated() != 0 ||
            mode_environment.sq_allocated() != 0) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL"
                      << " phase=addressed-readback-complete mode="
                      << mode_index << " reason=" << mode_environment.error()
                      << " lq=" << mode_environment.lq_allocated()
                      << " sq=" << mode_environment.sq_allocated() << '\n';
            return 1;
        }
        addressing_load_writebacks += 4;
        addressing_store_writebacks += 2;
        addressing_cycles += mode_environment.cycle();
        addressing_dcache_requests += mode_environment.tilelink_requests();
    }

    memblock::Environment segment_lmul_matrix(argc, argv);
    constexpr std::uint64_t segment_lmul_base =
        memblock::kDefaultMemoryBase + 0x500000;
    segment_lmul_matrix.configure_backpressure(
        0x13198a2e03707344ULL, true);
    if (!segment_lmul_matrix.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                  << segment_lmul_matrix.cycle()
                  << " phase=segment-lmul-emul-reset reason="
                  << segment_lmul_matrix.error() << '\n';
        return 1;
    }

    constexpr std::array<memblock::VectorAddressingMode, 2>
        segment_lmul_modes{{
            memblock::VectorAddressingMode::unit_stride,
            memblock::VectorAddressingMode::strided,
        }};
    std::uint64_t segment_lmul_rob_cursor = 0;
    unsigned segment_lmul_rob_wraps = 0;
    unsigned segment_lmul_configurations = 0;
    unsigned segment_lmul_unit_stride = 0;
    unsigned segment_lmul_strided = 0;
    unsigned segment_lmul_positive_stride = 0;
    unsigned segment_lmul_negative_stride = 0;
    unsigned segment_lmul_load_uops = 0;
    unsigned segment_lmul_store_uops = 0;
    auto next_segment_lmul_rob = [&]() {
        const std::uint64_t absolute_rob = segment_lmul_rob_cursor++;
        segment_lmul_rob_wraps += absolute_rob != 0 &&
            absolute_rob % memblock::kRobEntries == 0;
        return absolute_rob;
    };
    auto make_segment_lmul_uops = [&] (
        bool store, std::uint64_t address, std::uint64_t absolute_rob,
        std::uint8_t eew, std::uint8_t vsew, int lmul_log2,
        int emul_log2, std::uint8_t nf,
        memblock::VectorAddressingMode addressing, std::int64_t stride,
        unsigned flow_num, std::uint8_t vl, std::uint8_t data_seed) {
        const unsigned fields = static_cast<unsigned>(nf) + 1U;
        const unsigned uops_per_field = 1U << static_cast<unsigned>(
            std::max(emul_log2, 0));
        const unsigned uop_count = fields * uops_per_field;
        const std::uint8_t vlmul = static_cast<std::uint8_t>(
            lmul_log2 < 0 ? lmul_log2 + 8 : lmul_log2);
        std::vector<memblock::VectorMemoryTransaction> uops;
        uops.reserve(uop_count);
        for (unsigned uop = 0; uop < uop_count; ++uop) {
            memblock::VectorMemoryTransaction transaction{
                .store = store,
                .segment = true,
                .address = address,
                .stride = stride,
                .addressing = addressing,
                .eew = eew,
                .vsew = vsew,
                .vl = vl,
                .rob = static_cast<std::uint8_t>(absolute_rob %
                    memblock::kRobEntries),
                .rob_flag = ((absolute_rob /
                    memblock::kRobEntries) & 1U) != 0,
                .pdest = static_cast<std::uint8_t>(64 + uop),
                .lane = 0,
                .flow_num = static_cast<std::uint8_t>(flow_num),
                .expected_trigger = memblock::kVectorWritebackTriggerNone,
                .vlmul = vlmul,
                .vuop_idx = static_cast<std::uint8_t>(uop),
                .last_uop = uop + 1 == uop_count,
                .nf = nf,
            };
            for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
                transaction.data[byte] = static_cast<unsigned char>(
                    data_seed + uop * 17 + byte * 7);
            }
            uops.push_back(transaction);
        }
        return uops;
    };
    auto initialize_segment_source = [&] (
        const std::vector<memblock::VectorMemoryTransaction> &uops,
        unsigned case_index) {
        for (const auto &uop : uops) {
            const unsigned element_bytes = 1U << uop.eew;
            const std::uint16_t active = memblock::active_vector_elements(uop);
            for (unsigned element = 0; element < 16U / element_bytes;
                 ++element) {
                if (((active >> element) & 1U) == 0) {
                    continue;
                }
                const std::uint64_t address =
                    memblock::vector_element_address(uop, element);
                for (unsigned byte = 0; byte < element_bytes; ++byte) {
                    segment_lmul_matrix.memory().write_byte(
                        address + byte,
                        static_cast<std::uint8_t>(
                            0x2d + case_index * 29 +
                            uop.vuop_idx * 13 + element * 5 + byte));
                }
            }
        }
    };
    auto run_segment_lmul_instruction = [&] (
        const std::vector<memblock::VectorMemoryTransaction> &uops,
        const char *phase, unsigned &uop_counter) {
        for (const auto &uop : uops) {
            segment_lmul_matrix.expect_vector(uop);
            if (!segment_lmul_matrix.issue_vector(uop, 1024)) {
                std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                          << segment_lmul_matrix.cycle()
                          << " phase=" << phase
                          << " reason=" << segment_lmul_matrix.error() << '\n';
                return false;
            }
        }
        if (!segment_lmul_matrix.run_until_vector_complete(131072)) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                      << segment_lmul_matrix.cycle()
                      << " phase=" << phase
                      << " reason=" << segment_lmul_matrix.error() << '\n';
            return false;
        }
        uop_counter += uops.size();
        return true;
    };

    for (const auto mode : segment_lmul_modes) {
        for (std::uint8_t eew = 0; eew < 4; ++eew) {
            for (std::uint8_t vsew = 0; vsew < 4; ++vsew) {
                for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                    const int emul_log2 = static_cast<int>(eew) -
                        static_cast<int>(vsew) + lmul_log2;
                    if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                        emul_log2 < -3 || emul_log2 > 3) {
                        continue;
                    }
                    const unsigned uops_per_field = 1U <<
                        static_cast<unsigned>(std::max(emul_log2, 0));
                    const unsigned bytes_per_uop = emul_log2 < 0
                        ? 16U >> static_cast<unsigned>(-emul_log2)
                        : 16U;
                    const unsigned flow_num = bytes_per_uop >> eew;
                    const unsigned vector_bytes = lmul_log2 < 0
                        ? 16U >> static_cast<unsigned>(-lmul_log2)
                        : 16U << static_cast<unsigned>(lmul_log2);
                    const unsigned vlmax = vector_bytes >> vsew;
                    for (unsigned fields = 2; fields <= 8; ++fields) {
                        if (uops_per_field * fields > 8) {
                            continue;
                        }
                        const unsigned case_index =
                            segment_lmul_configurations++;
                        segment_lmul_unit_stride += mode ==
                            memblock::VectorAddressingMode::unit_stride;
                        segment_lmul_strided += mode ==
                            memblock::VectorAddressingMode::strided;
                        const unsigned data_bytes = 1U << eew;
                        const std::int64_t stride_magnitude =
                            static_cast<std::int64_t>(fields * data_bytes * 2U);
                        const std::int64_t stride = mode ==
                                memblock::VectorAddressingMode::strided
                            ? ((case_index & 1U) == 0
                                ? stride_magnitude
                                : -stride_magnitude)
                            : 0;
                        segment_lmul_positive_stride += stride > 0;
                        segment_lmul_negative_stride += stride < 0;
                        const std::uint64_t case_base =
                            segment_lmul_base + case_index * 0x1000;
                        const std::uint64_t source = case_base + 0x400;
                        const std::uint64_t destination = case_base + 0xc00;
                        const auto nf = static_cast<std::uint8_t>(fields - 1);

                        auto loads = make_segment_lmul_uops(
                            false, source, next_segment_lmul_rob(), eew,
                            vsew, lmul_log2, emul_log2, nf, mode, stride,
                            flow_num, static_cast<std::uint8_t>(vlmax),
                            static_cast<std::uint8_t>(0x19 + case_index));
                        initialize_segment_source(loads, case_index);
                        if (!run_segment_lmul_instruction(
                                loads, "segment-lmul-emul-load",
                                segment_lmul_load_uops)) {
                            return 1;
                        }

                        auto stores = make_segment_lmul_uops(
                            true, destination, next_segment_lmul_rob(), eew,
                            vsew, lmul_log2, emul_log2, nf, mode, stride,
                            flow_num, static_cast<std::uint8_t>(vlmax),
                            static_cast<std::uint8_t>(0x59 + case_index));
                        if (!run_segment_lmul_instruction(
                                stores, "segment-lmul-emul-store",
                                segment_lmul_store_uops)) {
                            return 1;
                        }
                        for (const auto &store : stores) {
                            segment_lmul_matrix.record_committed_vector_store(
                                store);
                        }

                        auto readbacks = make_segment_lmul_uops(
                            false, destination, next_segment_lmul_rob(), eew,
                            vsew, lmul_log2, emul_log2, nf, mode, stride,
                            flow_num, static_cast<std::uint8_t>(vlmax),
                            static_cast<std::uint8_t>(0x99 + case_index));
                        if (!run_segment_lmul_instruction(
                                readbacks, "segment-lmul-emul-readback",
                                segment_lmul_load_uops)) {
                            return 1;
                        }
                    }
                }
            }
        }
    }
    if (!segment_lmul_matrix.pulse_sbuffer_flush() ||
        !segment_lmul_matrix.run_until_sbuffer_empty(32768) ||
        segment_lmul_configurations != 676 ||
        segment_lmul_unit_stride != 338 || segment_lmul_strided != 338 ||
        segment_lmul_positive_stride != 169 ||
        segment_lmul_negative_stride != 169 ||
        segment_lmul_load_uops != 7096 || segment_lmul_store_uops != 3548 ||
        segment_lmul_rob_wraps != 12 ||
        segment_lmul_matrix.lq_allocated() != 0 ||
        segment_lmul_matrix.sq_allocated() != 0) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                  << segment_lmul_matrix.cycle()
                  << " phase=segment-lmul-emul-conservation"
                  << " configurations=" << segment_lmul_configurations
                  << " unit_stride=" << segment_lmul_unit_stride
                  << " strided=" << segment_lmul_strided
                  << " positive=" << segment_lmul_positive_stride
                  << " negative=" << segment_lmul_negative_stride
                  << " load_uops=" << segment_lmul_load_uops
                  << " store_uops=" << segment_lmul_store_uops
                  << " rob_wraps=" << segment_lmul_rob_wraps
                  << " lq=" << segment_lmul_matrix.lq_allocated()
                  << " sq=" << segment_lmul_matrix.sq_allocated()
                  << " reason=" << segment_lmul_matrix.error() << '\n';
        return 1;
    }

    memblock::Environment indexed_segment_matrix(argc, argv);
    constexpr std::uint64_t indexed_segment_base =
        memblock::kDefaultMemoryBase + 0x800000;
    indexed_segment_matrix.configure_backpressure(
        0xa4093822299f31d0ULL, true);
    if (!indexed_segment_matrix.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                  << indexed_segment_matrix.cycle()
                  << " phase=indexed-segment-lmul-emul-reset reason="
                  << indexed_segment_matrix.error() << '\n';
        return 1;
    }

    constexpr std::array<memblock::VectorAddressingMode, 2>
        indexed_segment_modes{{
            memblock::VectorAddressingMode::indexed_unordered,
            memblock::VectorAddressingMode::indexed_ordered,
        }};
    std::uint64_t indexed_segment_rob_cursor = 0;
    unsigned indexed_segment_rob_wraps = 0;
    unsigned indexed_segment_configurations = 0;
    unsigned indexed_segment_unordered = 0;
    unsigned indexed_segment_ordered = 0;
    unsigned indexed_segment_extra_index_configs = 0;
    unsigned indexed_segment_load_uops = 0;
    unsigned indexed_segment_store_uops = 0;
    unsigned indexed_segment_load_index_only_uops = 0;
    unsigned indexed_segment_store_index_only_uops = 0;
    auto next_indexed_segment_rob = [&]() {
        const std::uint64_t absolute_rob = indexed_segment_rob_cursor++;
        indexed_segment_rob_wraps += absolute_rob != 0 &&
            absolute_rob % memblock::kRobEntries == 0;
        return absolute_rob;
    };
    auto make_indexed_segment_uops = [&] (
        bool store, std::uint64_t address, std::uint64_t absolute_rob,
        std::uint8_t eew, std::uint8_t vsew, int lmul_log2,
        int emul_log2, std::uint8_t nf,
        memblock::VectorAddressingMode addressing, unsigned flow_num,
        std::uint8_t vl, std::uint8_t data_seed) {
        const unsigned fields = static_cast<unsigned>(nf) + 1U;
        const unsigned lmul_uops = 1U << static_cast<unsigned>(
            std::max(lmul_log2, 0));
        const unsigned emul_uops = 1U << static_cast<unsigned>(
            std::max(emul_log2, 0));
        const unsigned data_uops = lmul_uops * fields;
        const unsigned uop_count = std::max(data_uops, emul_uops);
        const std::uint8_t vlmul = static_cast<std::uint8_t>(
            lmul_log2 < 0 ? lmul_log2 + 8 : lmul_log2);
        const unsigned index_bytes = 1U << eew;
        const unsigned data_bytes = 1U << vsew;
        std::array<unsigned char, 128> index_group{};
        for (unsigned element = 0; element < vl; ++element) {
            const std::uint64_t offset =
                static_cast<std::uint64_t>(element) * data_bytes *
                (fields + 1U);
            for (unsigned byte = 0; byte < index_bytes; ++byte) {
                index_group[element * index_bytes + byte] =
                    static_cast<unsigned char>(offset >> (8 * byte));
            }
        }

        std::vector<memblock::VectorMemoryTransaction> uops;
        uops.reserve(uop_count);
        for (unsigned uop = 0; uop < uop_count; ++uop) {
            memblock::VectorMemoryTransaction transaction{
                .store = store,
                .segment = true,
                .address = address,
                .addressing = addressing,
                .eew = eew,
                .vsew = vsew,
                .vl = vl,
                .rob = static_cast<std::uint8_t>(absolute_rob %
                    memblock::kRobEntries),
                .rob_flag = ((absolute_rob /
                    memblock::kRobEntries) & 1U) != 0,
                .pdest = static_cast<std::uint8_t>(64 + uop),
                .lane = 0,
                .flow_num = static_cast<std::uint8_t>(flow_num),
                .expected_trigger = memblock::kVectorWritebackTriggerNone,
                .vlmul = vlmul,
                .vuop_idx = static_cast<std::uint8_t>(uop),
                .last_uop = uop + 1 == uop_count,
                .nf = nf,
                .vec_wen = !store && uop < data_uops,
            };
            transaction.oracle_index_group = index_group;
            for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
                transaction.data[byte] = static_cast<unsigned char>(
                    data_seed + uop * 17 + byte * 7);
            }
            if (uop < emul_uops) {
                const unsigned index_group_bytes = emul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-emul_log2)
                    : 16U;
                const unsigned source_offset = emul_log2 < 0
                    ? 0
                    : uop * 16U;
                std::copy_n(
                    index_group.begin() + source_offset,
                    index_group_bytes, transaction.index.begin());
            }
            uops.push_back(transaction);
        }
        return std::pair{uops, data_uops};
    };
    auto initialize_indexed_segment_source = [&] (
        const std::vector<memblock::VectorMemoryTransaction> &uops,
        unsigned data_uops, unsigned case_index) {
        for (unsigned uop_index = 0; uop_index < data_uops; ++uop_index) {
            const auto &uop = uops[uop_index];
            const unsigned element_bytes = 1U << memblock::vector_vsew(uop);
            const std::uint16_t active = memblock::active_vector_elements(uop);
            for (unsigned element = 0; element < 16U / element_bytes;
                 ++element) {
                if (((active >> element) & 1U) == 0) {
                    continue;
                }
                const std::uint64_t address =
                    memblock::vector_element_address(uop, element);
                for (unsigned byte = 0; byte < element_bytes; ++byte) {
                    indexed_segment_matrix.memory().write_byte(
                        address + byte,
                        static_cast<std::uint8_t>(
                            0x3b + case_index * 31 +
                            uop.vuop_idx * 11 + element * 7 + byte));
                }
            }
        }
    };
    auto run_indexed_segment_instruction = [&] (
        const std::vector<memblock::VectorMemoryTransaction> &uops,
        unsigned data_uops, const char *phase, unsigned &uop_counter,
        unsigned &index_only_counter) {
        for (unsigned uop_index = 0; uop_index < uops.size(); ++uop_index) {
            const auto &uop = uops[uop_index];
            if (!uop.store && uop_index >= data_uops) {
                indexed_segment_matrix.expect_vector_data(uop, uop.data);
            } else {
                indexed_segment_matrix.expect_vector(uop);
            }
            if (!indexed_segment_matrix.issue_vector(uop, 1024)) {
                std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                          << indexed_segment_matrix.cycle()
                          << " phase=" << phase
                          << " reason=" << indexed_segment_matrix.error()
                          << '\n';
                return false;
            }
        }
        if (!indexed_segment_matrix.run_until_vector_complete(131072)) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                      << indexed_segment_matrix.cycle()
                      << " phase=" << phase
                      << " reason=" << indexed_segment_matrix.error() << '\n';
            return false;
        }
        uop_counter += uops.size();
        index_only_counter += uops.size() - data_uops;
        return true;
    };

    for (const auto mode : indexed_segment_modes) {
        for (std::uint8_t eew = 0; eew < 4; ++eew) {
            for (std::uint8_t vsew = 0; vsew < 4; ++vsew) {
                for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                    const int emul_log2 = static_cast<int>(eew) -
                        static_cast<int>(vsew) + lmul_log2;
                    if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                        emul_log2 < -3 || emul_log2 > 3) {
                        continue;
                    }
                    const unsigned lmul_uops = 1U <<
                        static_cast<unsigned>(std::max(lmul_log2, 0));
                    const unsigned emul_uops = 1U <<
                        static_cast<unsigned>(std::max(emul_log2, 0));
                    const unsigned bytes_per_uop = lmul_log2 < 0
                        ? 16U >> static_cast<unsigned>(-lmul_log2)
                        : 16U;
                    const unsigned flow_num = bytes_per_uop >> vsew;
                    const unsigned vector_bytes = lmul_log2 < 0
                        ? 16U >> static_cast<unsigned>(-lmul_log2)
                        : 16U << static_cast<unsigned>(lmul_log2);
                    const unsigned vlmax = vector_bytes >> vsew;
                    for (unsigned fields = 2; fields <= 8; ++fields) {
                        const unsigned data_uops = lmul_uops * fields;
                        if (data_uops > 8) {
                            continue;
                        }
                        const unsigned case_index =
                            indexed_segment_configurations++;
                        indexed_segment_unordered += mode ==
                            memblock::VectorAddressingMode::indexed_unordered;
                        indexed_segment_ordered += mode ==
                            memblock::VectorAddressingMode::indexed_ordered;
                        indexed_segment_extra_index_configs +=
                            emul_uops > data_uops;
                        const std::uint64_t case_base =
                            indexed_segment_base + case_index * 0x1000;
                        const std::uint64_t source = case_base + 0x400;
                        const std::uint64_t destination = case_base + 0xc00;
                        const auto nf = static_cast<std::uint8_t>(fields - 1);

                        auto [loads, load_data_uops] =
                            make_indexed_segment_uops(
                                false, source, next_indexed_segment_rob(),
                                eew, vsew, lmul_log2, emul_log2, nf, mode,
                                flow_num, static_cast<std::uint8_t>(vlmax),
                                static_cast<std::uint8_t>(0x23 + case_index));
                        initialize_indexed_segment_source(
                            loads, load_data_uops, case_index);
                        if (!run_indexed_segment_instruction(
                                loads, load_data_uops,
                                "indexed-segment-lmul-emul-load",
                                indexed_segment_load_uops,
                                indexed_segment_load_index_only_uops)) {
                            return 1;
                        }

                        auto [stores, store_data_uops] =
                            make_indexed_segment_uops(
                                true, destination, next_indexed_segment_rob(),
                                eew, vsew, lmul_log2, emul_log2, nf, mode,
                                flow_num, static_cast<std::uint8_t>(vlmax),
                                static_cast<std::uint8_t>(0x63 + case_index));
                        if (!run_indexed_segment_instruction(
                                stores, store_data_uops,
                                "indexed-segment-lmul-emul-store",
                                indexed_segment_store_uops,
                                indexed_segment_store_index_only_uops)) {
                            return 1;
                        }
                        for (unsigned uop = 0; uop < store_data_uops; ++uop) {
                            indexed_segment_matrix.
                                record_committed_vector_store(stores[uop]);
                        }

                        auto [readbacks, readback_data_uops] =
                            make_indexed_segment_uops(
                                false, destination,
                                next_indexed_segment_rob(), eew, vsew,
                                lmul_log2, emul_log2, nf, mode, flow_num,
                                static_cast<std::uint8_t>(vlmax),
                                static_cast<std::uint8_t>(0xa3 + case_index));
                        if (!run_indexed_segment_instruction(
                                readbacks, readback_data_uops,
                                "indexed-segment-lmul-emul-readback",
                                indexed_segment_load_uops,
                                indexed_segment_load_index_only_uops)) {
                            return 1;
                        }
                    }
                }
            }
        }
    }
    if (!indexed_segment_matrix.pulse_sbuffer_flush() ||
        !indexed_segment_matrix.run_until_sbuffer_empty(32768) ||
        indexed_segment_configurations != 676 ||
        indexed_segment_unordered != 338 || indexed_segment_ordered != 338 ||
        indexed_segment_extra_index_configs != 32 ||
        indexed_segment_load_uops != 7264 ||
        indexed_segment_store_uops != 3632 ||
        indexed_segment_load_index_only_uops != 168 ||
        indexed_segment_store_index_only_uops != 84 ||
        indexed_segment_rob_wraps != 12 ||
        indexed_segment_matrix.lq_allocated() != 0 ||
        indexed_segment_matrix.sq_allocated() != 0) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FAIL cycle="
                  << indexed_segment_matrix.cycle()
                  << " phase=indexed-segment-lmul-emul-conservation"
                  << " configurations=" << indexed_segment_configurations
                  << " unordered=" << indexed_segment_unordered
                  << " ordered=" << indexed_segment_ordered
                  << " extra_index_configs="
                  << indexed_segment_extra_index_configs
                  << " load_uops=" << indexed_segment_load_uops
                  << " store_uops=" << indexed_segment_store_uops
                  << " load_index_only="
                  << indexed_segment_load_index_only_uops
                  << " store_index_only="
                  << indexed_segment_store_index_only_uops
                  << " rob_wraps=" << indexed_segment_rob_wraps
                  << " lq=" << indexed_segment_matrix.lq_allocated()
                  << " sq=" << indexed_segment_matrix.sq_allocated()
                  << " reason=" << indexed_segment_matrix.error() << '\n';
        return 1;
    }

    const std::uint64_t aggregate_cycles =
        environment.cycle() + addressing_cycles + segment_lmul_matrix.cycle() +
        indexed_segment_matrix.cycle();
    const std::uint64_t aggregate_dcache_requests =
        environment.tilelink_requests() + addressing_dcache_requests +
        segment_lmul_matrix.tilelink_requests() +
        indexed_segment_matrix.tilelink_requests();

    std::cout << "MEMBLOCK_VECTOR_SEGMENT_PASS"
              << " cycle=" << aggregate_cycles
              << " fields=2 elements=2"
              << " segment_load_writebacks=2 segment_store_writebacks=2"
              << " addressed_modes=3 addressed_load_writebacks="
              << addressing_load_writebacks
              << " addressed_store_writebacks="
              << addressing_store_writebacks
              << " lmul_configurations=" << segment_lmul_configurations
              << " lmul_unit_stride=" << segment_lmul_unit_stride
              << " lmul_strided=" << segment_lmul_strided
              << " lmul_positive_stride=" << segment_lmul_positive_stride
              << " lmul_negative_stride=" << segment_lmul_negative_stride
              << " lmul_load_uops=" << segment_lmul_load_uops
              << " lmul_store_uops=" << segment_lmul_store_uops
              << " lmul_rob_wraps=" << segment_lmul_rob_wraps
              << " indexed_lmul_configurations="
                    << indexed_segment_configurations
              << " indexed_lmul_unordered=" << indexed_segment_unordered
              << " indexed_lmul_ordered=" << indexed_segment_ordered
              << " indexed_lmul_extra_index_configs="
                    << indexed_segment_extra_index_configs
              << " indexed_lmul_load_uops=" << indexed_segment_load_uops
              << " indexed_lmul_store_uops=" << indexed_segment_store_uops
              << " indexed_lmul_load_index_only="
                    << indexed_segment_load_index_only_uops
              << " indexed_lmul_store_index_only="
                    << indexed_segment_store_index_only_uops
              << " indexed_lmul_rob_wraps=" << indexed_segment_rob_wraps
              << " segment_lsq_allocations=0"
              << " readback_lq_allocated=" << environment.lq_allocated()
              << " readback_lq_dequeued=" << environment.lq_dequeued()
              << " dcache_requests=" << aggregate_dcache_requests
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_segment_fault_only_first(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_page = 0x54000000ULL;
    constexpr std::uint64_t physical_page =
        memblock::kDefaultMemoryBase + 0x5e000;
    constexpr std::uint64_t root = 0x97b00000ULL;
    constexpr std::uint64_t address = virtual_page + 0xff0;
    constexpr std::uint64_t physical_address = physical_page + 0xff0;
    environment.memory().fill_incrementing(physical_address, 16, 0x83);
    environment.configure_backpressure(0x6af8d37295c14e0bULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(address, physical_address, root) ||
        !environment.activate_sv39(root, 41)) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FOF_FAIL cycle="
                  << environment.cycle()
                  << " phase=configuration reason=" << environment.error()
                  << '\n';
        return 1;
    }

    const auto first_field = memblock::reference_sv39_walk(
        environment.memory(), root, address);
    const auto second_field = memblock::reference_sv39_walk(
        environment.memory(), root, address + 8);
    const auto next_segment = memblock::reference_sv39_walk(
        environment.memory(), root, address + 16);
    if (!first_field.translated || !second_field.translated ||
        first_field.physical_address != physical_address ||
        second_field.physical_address != physical_address + 8 ||
        next_segment.translated) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FOF_FAIL cycle="
                  << environment.cycle()
                  << " phase=reference reason=expected_one_mapped_segment\n";
        return 1;
    }

    std::array<memblock::VectorMemoryTransaction, 2> fields{};
    for (unsigned field = 0; field < fields.size(); ++field) {
        fields[field] = memblock::VectorMemoryTransaction{
            .segment = true,
            .address = address,
            .oracle_address = physical_address,
            .eew = 3,
            .vl = 2,
            .rob = 76,
            .pdest = static_cast<std::uint8_t>(124 + field),
            .lane = 0,
            .flow_num = 2,
            .expected_trigger = memblock::kVectorWritebackTriggerNone,
            .vuop_idx = static_cast<std::uint8_t>(field),
            .last_uop = false,
            .nf = 1,
            .fault_only_first = true,
            // DecodeUnit keeps this sideband low when NF is non-zero. The
            // segment unit recognizes FOF from fuOpType instead.
            .is_vleff = false,
        };
        for (unsigned byte = 0; byte < fields[field].data.size(); ++byte) {
            fields[field].data[byte] = static_cast<unsigned char>(
                0xc0 + field * 0x20 + byte);
        }
        auto expected = fields[field].data;
        for (unsigned byte = 0; byte < 8; ++byte) {
            expected[byte] = environment.memory().read_byte(
                physical_address + field * 8 + byte);
        }
        environment.expect_vector_data(fields[field], expected);
        if (!environment.issue_vector(fields[field], 512)) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FOF_FAIL cycle="
                      << environment.cycle() << " phase=data-issue field="
                      << field << " reason=" << environment.error() << '\n';
            return 1;
        }
    }

    auto fix_vl_uop = fields[0];
    fix_vl_uop.pdest = 126;
    fix_vl_uop.expected_vl = 1;
    fix_vl_uop.vuop_idx = 2;
    fix_vl_uop.last_uop = true;
    fix_vl_uop.vl_wen = true;
    environment.expect_vector_data(fix_vl_uop, {});
    if (!environment.issue_vector(fix_vl_uop, 512) ||
        !environment.run_until_vector_complete(32768) ||
        environment.vector_load_writebacks() != 2 ||
        environment.vector_fof_fix_writebacks() != 1 ||
        environment.lq_allocated() != 0 || environment.sq_allocated() != 0) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FOF_FAIL cycle="
                  << environment.cycle()
                  << " phase=completion reason=" << environment.error()
                  << " data_writebacks="
                  << environment.vector_load_writebacks()
                  << " fix_writebacks="
                  << environment.vector_fof_fix_writebacks()
                  << " lq=" << environment.lq_allocated()
                  << " sq=" << environment.sq_allocated() << '\n';
        return 1;
    }

    memblock::Environment first_fault(argc, argv);
    constexpr std::uint64_t first_fault_virtual = 0x55000180ULL;
    constexpr std::uint64_t first_fault_root = 0x97c00000ULL;
    first_fault.configure_backpressure(0xbb67ae8584caa73bULL, true);
    if (!first_fault.reset() ||
        !first_fault.activate_sv39(first_fault_root, 43) ||
        memblock::reference_sv39_walk(
            first_fault.memory(), first_fault_root,
            first_fault_virtual).translated) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FOF_FAIL cycle="
                  << first_fault.cycle()
                  << " phase=first-fault-configuration reason="
                  << first_fault.error() << '\n';
        return 1;
    }
    std::array<memblock::VectorMemoryTransaction, 2> faulting_fields{};
    for (unsigned field = 0; field < faulting_fields.size(); ++field) {
        faulting_fields[field] = memblock::VectorMemoryTransaction{
            .segment = true,
            .address = first_fault_virtual,
            .eew = 3,
            .vl = 2,
            .rob = 77,
            .pdest = static_cast<std::uint8_t>(120 + field),
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
            .expected_trigger = memblock::kVectorWritebackTriggerNone,
            .vuop_idx = static_cast<std::uint8_t>(field),
            .last_uop = false,
            .nf = 1,
            .fault_only_first = true,
            .is_vleff = false,
        };
        first_fault.expect_vector_data(
            faulting_fields[field], faulting_fields[field].data);
        if (!first_fault.issue_vector(faulting_fields[field], 512)) {
            std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FOF_FAIL cycle="
                      << first_fault.cycle()
                      << " phase=first-fault-data-issue field=" << field
                      << " reason=" << first_fault.error() << '\n';
            return 1;
        }
    }
    auto first_fault_fix = faulting_fields[0];
    first_fault_fix.pdest = 122;
    first_fault_fix.expected_exception_mask = 0;
    first_fault_fix.expected_vl = 2;
    first_fault_fix.vuop_idx = 2;
    first_fault_fix.last_uop = true;
    first_fault_fix.vl_wen = true;
    first_fault.expect_vector_data(first_fault_fix, {});
    if (!first_fault.issue_vector(first_fault_fix, 512) ||
        !first_fault.run_until_vector_complete(32768) ||
        first_fault.vector_load_writebacks() != 2 ||
        first_fault.vector_fof_fix_writebacks() != 1 ||
        first_fault.ptw_requests() == 0 ||
        first_fault.tilelink_requests() != 0 ||
        first_fault.exception_vaddr() != first_fault_virtual ||
        first_fault.lq_allocated() != 0 || first_fault.sq_allocated() != 0) {
        std::cerr << "MEMBLOCK_VECTOR_SEGMENT_FOF_FAIL cycle="
                  << first_fault.cycle()
                  << " phase=first-fault-completion reason="
                  << first_fault.error() << " data_writebacks="
                  << first_fault.vector_load_writebacks()
                  << " fix_writebacks="
                  << first_fault.vector_fof_fix_writebacks()
                  << " fault_vaddr=0x" << std::hex
                  << first_fault.exception_vaddr() << std::dec
                  << " ptw=" << first_fault.ptw_requests()
                  << " dcache=" << first_fault.tilelink_requests() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_VECTOR_SEGMENT_FOF_PASS"
              << " cycle=" << environment.cycle()
              << " fields=2 original_vl=2 final_vl=1"
              << " data_writebacks=" << environment.vector_load_writebacks()
              << " fix_vl_writebacks="
              << environment.vector_fof_fix_writebacks()
              << " segment_lsq_allocations=0"
              << " ptw_requests=" << environment.ptw_requests()
              << " first_element_faults=2"
              << " first_fault_fix_vl=2"
              << " first_fault_ptw=" << first_fault.ptw_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_store_forwarding(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x20400;
    environment.memory().fill_incrementing(base, 256, 0x19);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL cycle="
                  << environment.cycle() << " reason=" << environment.error() << '\n';
        return 1;
    }

    for (unsigned eew = 0; eew < 4; ++eew) {
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = base + eew * 64,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(16U >> eew),
            .rob = static_cast<std::uint8_t>(60 + eew * 2),
            .lq = static_cast<std::uint8_t>(eew * 2),
            .sq = static_cast<std::uint8_t>(eew * 2),
            .lane = eew % memblock::kVectorMemoryLanes,
        };
        for (unsigned byte = 0; byte < store.data.size(); ++byte) {
            store.data[byte] = static_cast<unsigned char>(
                0xd0 + eew * 7 + byte * 3);
        }
        environment.expect_vector(store);
        if (!environment.enqueue_vector(store) || !environment.issue_vector(store) ||
            !environment.run_until_vector_complete(256)) {
            std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL"
                      << " width=" << (8U << eew)
                      << " phase=store cycle=" << environment.cycle()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }

        memblock::VectorMemoryTransaction load{
            .address = store.address,
            .data = {},
            .eew = store.eew,
            .vl = store.vl,
            .rob = static_cast<std::uint8_t>(store.rob + 1),
            .lq = static_cast<std::uint8_t>(eew * 2),
            .sq = static_cast<std::uint8_t>(eew * 2 + 2),
            .pdest = static_cast<std::uint8_t>(100 + eew),
            .lane = (eew + 1) % memblock::kVectorMemoryLanes,
        };
        environment.expect_vector_data(load, store.data);
        if (!environment.enqueue_vector(load) || !environment.issue_vector(load) ||
            !environment.run_until_vector_complete(512)) {
            std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL"
                      << " width=" << (8U << eew)
                      << " phase=load cycle=" << environment.cycle()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        if (!environment.commit_vector_store(store)) {
            std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL"
                      << " width=" << (8U << eew)
                      << " phase=commit cycle=" << environment.cycle()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL cycle="
                  << environment.cycle() << " reason=" << environment.error()
                  << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_VECTOR_STORE_FORWARD_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << environment.vector_store_writebacks()
              << " loads=" << environment.vector_load_writebacks()
              << " sq_dequeued=" << environment.sq_dequeued()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_store_forwarding(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x2000;
    environment.memory().fill_incrementing(base, 128, 0x10);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_STORE_FORWARD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    constexpr std::array<memblock::StoreOp, 5> store_ops{
        memblock::StoreOp::sb,
        memblock::StoreOp::sh,
        memblock::StoreOp::sw,
        memblock::StoreOp::sd,
        memblock::StoreOp::sd,
    };
    constexpr std::array<memblock::LoadOp, 5> load_ops{
        memblock::LoadOp::lbu,
        memblock::LoadOp::lhu,
        memblock::LoadOp::lwu,
        memblock::LoadOp::ld,
        memblock::LoadOp::ld,
    };
    constexpr std::array<unsigned, 5> sizes{1, 2, 4, 8, 8};
    constexpr std::array<std::int32_t, 5> immediates{
        0, 1, 2047, -1, -2048,
    };
    constexpr std::uint64_t store_data = 0xfedcba9876543281ULL;

    for (unsigned index = 0; index < store_ops.size(); ++index) {
        const unsigned size = sizes[index];
        const std::uint64_t address = base + index * 16 + (8 - size);
        const memblock::StoreTransaction store{
            .address = address,
            .immediate = immediates[index],
            .data = store_data,
            .op = store_ops[index],
            .rob = static_cast<std::uint8_t>(index * 2),
            .sq = static_cast<std::uint8_t>(index),
            .address_lane = index % memblock::kScalarStoreLanes,
            .data_lane = (index + 1) % memblock::kScalarStoreLanes,
        };
        environment.expect_store(store);
        const bool issued = index % 2 == 0
            ? environment.enqueue_store(store, static_cast<std::uint8_t>(index)) &&
                  environment.issue_store_data(store) &&
                  environment.issue_store_address(store)
            : environment.enqueue_store(store, static_cast<std::uint8_t>(index)) &&
                  environment.issue_store_address(store) &&
                  environment.issue_store_data(store);
        if (!issued || !environment.run_until_store_complete(128)) {
            std::cerr << "MEMBLOCK_STORE_FORWARD_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=store reason=" << environment.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction load{
            .address = address,
            .op = load_ops[index],
            .rob = static_cast<std::uint8_t>(index * 2 + 1),
            .lq = static_cast<std::uint8_t>(index),
            .sq = static_cast<std::uint8_t>(index + 1),
            .pdest = static_cast<std::uint8_t>(20 + index),
            .lane = index % memblock::kScalarLoadLanes,
        };
        const std::uint64_t expected = size == 8
            ? store_data
            : store_data & ((std::uint64_t{1} << (size * 8)) - 1);
        environment.expect_load_data(load, expected);
        if (!environment.enqueue_load(load) || !environment.issue_load(load) ||
            !environment.run_until_complete(256)) {
            std::cerr << "MEMBLOCK_STORE_FORWARD_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=load reason=" << environment.error() << '\n';
            return 1;
        }
    }

    std::cout << "MEMBLOCK_STORE_FORWARD_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << environment.store_writebacks()
              << " loads=" << environment.writebacks()
              << " immediate_stores=" << immediates.size()
              << " immediate_min=-2048 immediate_max=2047"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
