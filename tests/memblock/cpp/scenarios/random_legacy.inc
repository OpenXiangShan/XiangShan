// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_random_loads(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed);
    constexpr std::uint64_t memory_size = 64 * 1024;
    for (std::uint64_t offset = 0; offset < memory_size; ++offset) {
        const std::uint8_t value = static_cast<std::uint8_t>(
            (offset * 73 + (offset >> 5) * 19 + options.seed) & 0xff);
        environment.memory().write_byte(memblock::kDefaultMemoryBase + offset, value);
    }
    environment.configure_backpressure(
        options.seed ^ 0xd1b54a32d192ed03ULL, options.backpressure);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RANDOM_FAIL seed=" << options.seed
                  << " transaction=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    LoadCoverage coverage;
    std::uint64_t recent_line = 0;
    for (unsigned index = 0; index < options.transactions; ++index) {
        const auto op = static_cast<memblock::LoadOp>(random() % 7);
        const unsigned size = 1U << (static_cast<unsigned>(op) & 3U);
        std::uint64_t line = random() % (memory_size / 64);
        if (index != 0 && (random() & 3U) != 0) {
            line = recent_line;
        }
        recent_line = line;
        const unsigned slots = 64 / size;
        const std::uint64_t address = memblock::kDefaultMemoryBase + line * 64 +
                                      (random() % slots) * size;
        const memblock::LoadTransaction transaction{
            .address = address,
            .op = op,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .lq = memblock::lq_pointer_value(index),
            .lq_flag = memblock::lq_pointer_flag(index),
            .sq = 0,
            .sq_flag = false,
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = static_cast<unsigned>(random() % memblock::kScalarLoadLanes),
        };
        const std::uint64_t requests_before = environment.tilelink_requests();
        environment.expect_load(transaction);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction) ||
            !environment.run_until_complete(1024)) {
            std::cerr << "MEMBLOCK_RANDOM_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " address=0x" << std::hex << address << std::dec
                      << " op=" << static_cast<unsigned>(op)
                      << " lane=" << transaction.lane
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        coverage.sample(
            transaction, requests_before, environment.tilelink_requests());
    }

    if (options.transactions >= 21 && !coverage.complete()) {
        std::cerr << "MEMBLOCK_RANDOM_FAIL seed=" << options.seed
                  << " transaction=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " reason=incomplete_coverage " << coverage.summary() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_PASS"
              << " seed=" << options.seed
              << " transactions=" << options.transactions
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " writebacks=" << environment.writebacks()
              << ' ' << coverage.summary()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_vector_loads(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed ^ 0x6a09e667f3bcc909ULL);
    constexpr std::uint64_t memory_size = 64 * 1024;
    for (std::uint64_t offset = 0; offset < memory_size; ++offset) {
        environment.memory().write_byte(
            memblock::kDefaultMemoryBase + 0x30000 + offset,
            static_cast<std::uint8_t>(
                (offset * 29 + (offset >> 4) * 113 + options.seed) & 0xff));
    }
    environment.configure_backpressure(
        options.seed ^ 0xbb67ae8584caa73bULL, options.backpressure);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FAIL seed=" << options.seed
                  << " transaction=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    VectorCoverage coverage;
    std::uint64_t recent_line = 0;
    for (unsigned index = 0; index < options.transactions; ++index) {
        const unsigned eew = random() % 4;
        const unsigned element_bytes = 1U << eew;
        const unsigned element_count = 16U / element_bytes;
        std::uint64_t line = random() % (memory_size / 64);
        if (index != 0 && (random() & 3U) != 0) {
            line = recent_line;
        }
        recent_line = line;
        const unsigned offset_slots = (64 - 16) / element_bytes + 1;
        const unsigned line_offset = (random() % offset_slots) * element_bytes;
        memblock::VectorMemoryTransaction transaction{
            .address = memblock::kDefaultMemoryBase + 0x30000 + line * 64 +
                       line_offset,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(random() % (element_count + 1)),
            .vm = (random() & 1U) != 0,
            .mask_bits = static_cast<std::uint16_t>(random()),
            .vma = (random() & 1U) != 0,
            .vta = (random() & 1U) != 0,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .lq = memblock::lq_pointer_value(index * 2),
            .lq_flag = memblock::lq_pointer_flag(index * 2),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = static_cast<unsigned>(random() % memblock::kVectorMemoryLanes),
        };
        transaction.vstart = transaction.vl == 0
            ? 0
            : static_cast<std::uint8_t>(random() % (transaction.vl + 1));
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(random());
        }
        const std::uint64_t requests_before = environment.tilelink_requests();
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction) ||
            !environment.run_until_vector_complete(1024)) {
            std::cerr << "MEMBLOCK_RANDOM_VECTOR_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " address=0x" << std::hex << transaction.address << std::dec
                      << " eew=" << eew << " vl="
                      << static_cast<unsigned>(transaction.vl)
                      << " vstart=" << static_cast<unsigned>(transaction.vstart)
                      << " vm=" << transaction.vm << " lane=" << transaction.lane
                      << " active=0x" << std::hex
                      << memblock::active_vector_elements(transaction) << std::dec
                      << " rob=" << static_cast<unsigned>(transaction.rob)
                      << ':' << transaction.rob_flag
                      << " lq=" << static_cast<unsigned>(transaction.lq)
                      << ':' << transaction.lq_flag
                      << " requests_before=" << requests_before
                      << " requests_after=" << environment.tilelink_requests()
                      << " releases=" << environment.tilelink_releases()
                      << " lq_allocated=" << environment.lq_allocated()
                      << " lq_dequeued=" << environment.lq_dequeued()
                      << " writebacks=" << environment.vector_load_writebacks()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        coverage.sample(
            transaction, requests_before, environment.tilelink_requests());
    }

    if (options.transactions >= 64 && !coverage.complete()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FAIL seed=" << options.seed
                  << " transaction=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " reason=incomplete_coverage " << coverage.summary() << '\n';
        return 1;
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FAIL seed=" << options.seed
                  << " transaction=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_VECTOR_PASS"
              << " seed=" << options.seed
              << " transactions=" << options.transactions
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " tilelink_releases=" << environment.tilelink_releases()
              << " lq_allocated=" << environment.lq_allocated()
              << " lq_dequeued=" << environment.lq_dequeued()
              << " writebacks=" << environment.vector_load_writebacks()
              << ' ' << coverage.summary()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_vector_forwarding(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed ^ 0x3c6ef372fe94f82bULL);
    constexpr unsigned max_transactions = 24;
    const unsigned transaction_count = std::min(options.transactions, max_transactions);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x50000;
    environment.memory().fill_incrementing(base, 4096, 0x27);
    environment.configure_backpressure(
        options.seed ^ 0xa54ff53a5f1d36f1ULL, options.backpressure);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                  << " transaction=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    VectorCoverage coverage;
    for (unsigned index = 0; index < transaction_count; ++index) {
        const unsigned eew = index < 4 ? index : random() % 4;
        const unsigned element_bytes = 1U << eew;
        const unsigned element_count = 16U / element_bytes;
        const unsigned line_offset = (index & 1U) == 0
            ? 0
            : element_bytes;
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = base + index * 64 + line_offset,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(
                index % 6 == 0 ? 0
                : index % 6 == 1 ? element_count
                                 : random() % (element_count + 1)),
            .vm = (index & 1U) != 0,
            .mask_bits = static_cast<std::uint16_t>(random()),
            .vma = (random() & 1U) != 0,
            .vta = (random() & 1U) != 0,
            .rob = memblock::rob_pointer_value(index * 2),
            .rob_flag = memblock::rob_pointer_flag(index * 2),
            .lq = memblock::lq_pointer_value(index * 2),
            .lq_flag = memblock::lq_pointer_flag(index * 2),
            .sq = memblock::sq_pointer_value(index * 2),
            .sq_flag = memblock::sq_pointer_flag(index * 2),
            .lane = index % memblock::kVectorMemoryLanes,
        };
        store.vstart = store.vl > 1 && index % 5 == 0
            ? 1
            : store.vl == 0 ? 0
                            : static_cast<std::uint8_t>(random() % (store.vl + 1));
        for (auto &byte : store.data) {
            byte = static_cast<unsigned char>(random());
        }
        environment.expect_vector(store);
        if (!environment.enqueue_vector(store) || !environment.issue_vector(store) ||
            !environment.run_until_vector_complete(512)) {
            std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " phase=store cycle=" << environment.cycle()
                      << " address=0x" << std::hex << store.address << std::dec
                      << " eew=" << eew << " reason=" << environment.error() << '\n';
            return 1;
        }

        memblock::VectorMemoryTransaction load{
            .address = store.address,
            .eew = store.eew,
            .vl = static_cast<std::uint8_t>(element_count),
            .rob = memblock::rob_pointer_value(index * 2 + 1),
            .rob_flag = memblock::rob_pointer_flag(index * 2 + 1),
            .lq = memblock::lq_pointer_value(index * 2),
            .lq_flag = memblock::lq_pointer_flag(index * 2),
            .sq = memblock::sq_pointer_value(index * 2 + 2),
            .sq_flag = memblock::sq_pointer_flag(index * 2 + 2),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = (index + 1) % memblock::kVectorMemoryLanes,
        };
        for (auto &byte : load.data) {
            byte = static_cast<unsigned char>(random());
        }
        auto expected = environment.memory().expected_vector_load(load);
        const std::uint16_t active = memblock::active_vector_elements(store);
        for (unsigned element = 0; element < element_count; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                const unsigned offset = element * element_bytes + byte;
                expected[offset] = store.data[offset];
            }
        }
        const std::uint64_t requests_before = environment.tilelink_requests();
        environment.expect_vector_data(load, expected);
        if (!environment.enqueue_vector(load) || !environment.issue_vector(load) ||
            !environment.run_until_vector_complete(1024)) {
            std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " phase=load cycle=" << environment.cycle()
                      << " address=0x" << std::hex << load.address << std::dec
                      << " eew=" << eew << " reason=" << environment.error() << '\n';
            return 1;
        }
        coverage.sample(store, requests_before, environment.tilelink_requests());
    }

    if (transaction_count >= 16 && !coverage.complete(false)) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                  << " transaction=" << transaction_count
                  << " cycle=" << environment.cycle()
                  << " reason=incomplete_coverage " << coverage.summary() << '\n';
        return 1;
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                  << " transaction=" << transaction_count
                  << " cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_VECTOR_FORWARD_PASS"
              << " seed=" << options.seed
              << " transactions=" << transaction_count
              << " cycle=" << environment.cycle()
              << " stores=" << environment.vector_store_writebacks()
              << " loads=" << environment.vector_load_writebacks()
              << ' ' << coverage.summary()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_forwarding(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed ^ 0x9e3779b97f4a7c15ULL);
    constexpr unsigned max_transactions = 48;
    const unsigned transaction_count = std::min(options.transactions, max_transactions);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x12000;
    environment.memory().fill_incrementing(base, 4096, 0x31);
    environment.configure_backpressure(
        options.seed ^ 0xd1b54a32d192ed03ULL, options.backpressure);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RANDOM_FORWARD_FAIL seed=" << options.seed
                  << " transaction=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::array<std::uint64_t, 4> operations{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> address_lanes{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> data_lanes{};
    std::uint64_t signed_loads = 0;
    std::uint64_t unsigned_loads = 0;
    for (unsigned index = 0; index < transaction_count; ++index) {
        const unsigned op_value = static_cast<unsigned>(random() % 4);
        const auto store_op = static_cast<memblock::StoreOp>(op_value);
        const unsigned size = 1U << op_value;
        const bool is_unsigned = size != 8 && (random() & 1U) != 0;
        const auto load_op = static_cast<memblock::LoadOp>(
            op_value + (is_unsigned ? 4U : 0U));
        const std::uint64_t address = base + index * 64 +
                                      ((random() % (64 / size)) * size);
        const std::uint64_t data = random();
        const memblock::StoreTransaction store{
            .address = address,
            .data = data,
            .op = store_op,
            .rob = memblock::rob_pointer_value(index * 2),
            .rob_flag = memblock::rob_pointer_flag(index * 2),
            .sq = memblock::sq_pointer_value(index),
            .sq_flag = memblock::sq_pointer_flag(index),
            .address_lane = static_cast<unsigned>(random() % memblock::kScalarStoreLanes),
            .data_lane = static_cast<unsigned>(random() % memblock::kScalarStoreLanes),
        };
        environment.expect_store(store);
        const bool data_first = (random() & 1U) != 0;
        const bool store_ok = environment.enqueue_store(
                                  store, static_cast<std::uint8_t>(index)) &&
                              (data_first
                                   ? environment.issue_store_data(store) &&
                                         environment.issue_store_address(store)
                                   : environment.issue_store_address(store) &&
                                         environment.issue_store_data(store)) &&
                              environment.run_until_store_complete(128);
        if (!store_ok) {
            std::cerr << "MEMBLOCK_RANDOM_FORWARD_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=store reason=" << environment.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction load{
            .address = address,
            .op = load_op,
            .rob = memblock::rob_pointer_value(index * 2 + 1),
            .rob_flag = memblock::rob_pointer_flag(index * 2 + 1),
            .lq = memblock::lq_pointer_value(index),
            .lq_flag = memblock::lq_pointer_flag(index),
            .sq = memblock::sq_pointer_value(index + 1),
            .sq_flag = memblock::sq_pointer_flag(index + 1),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = static_cast<unsigned>(random() % memblock::kScalarLoadLanes),
        };
        const unsigned bits = size * 8;
        const std::uint64_t mask = bits == 64
            ? ~std::uint64_t{0}
            : (std::uint64_t{1} << bits) - 1;
        const std::uint64_t raw = data & mask;
        const std::uint64_t expected = is_unsigned
            ? raw
            : memblock::sign_extend(raw, bits);
        environment.expect_load_data(load, expected);
        if (!environment.enqueue_load(load) || !environment.issue_load(load) ||
            !environment.run_until_complete(256)) {
            std::cerr << "MEMBLOCK_RANDOM_FORWARD_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=load reason=" << environment.error() << '\n';
            return 1;
        }
        ++operations[op_value];
        ++address_lanes[store.address_lane];
        ++data_lanes[store.data_lane];
        if (is_unsigned) {
            ++unsigned_loads;
        } else {
            ++signed_loads;
        }
    }

    const bool coverage_complete = transaction_count < 16 ||
        (std::all_of(operations.begin(), operations.end(), [](auto count) { return count != 0; }) &&
         std::all_of(address_lanes.begin(), address_lanes.end(), [](auto count) { return count != 0; }) &&
         std::all_of(data_lanes.begin(), data_lanes.end(), [](auto count) { return count != 0; }) &&
         signed_loads != 0 && unsigned_loads != 0);
    if (!coverage_complete) {
        std::cerr << "MEMBLOCK_RANDOM_FORWARD_FAIL seed=" << options.seed
                  << " transaction=" << transaction_count
                  << " cycle=" << environment.cycle()
                  << " reason=incomplete_coverage\n";
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_FORWARD_PASS"
              << " seed=" << options.seed
              << " transactions=" << transaction_count
              << " cycle=" << environment.cycle()
              << " stores=" << environment.store_writebacks()
              << " loads=" << environment.writebacks()
              << " store_ops=" << operations[0] << ',' << operations[1] << ','
              << operations[2] << ',' << operations[3]
              << " signed=" << signed_loads
              << " unsigned=" << unsigned_loads
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
