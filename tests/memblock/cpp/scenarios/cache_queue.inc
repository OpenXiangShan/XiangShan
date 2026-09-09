// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_dcache_release(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr unsigned dcache_sets = 128;
    constexpr unsigned dcache_ways = 8;
    constexpr unsigned line_bytes = 64;
    constexpr unsigned line_count = dcache_ways + 2;
    constexpr std::uint64_t same_set_stride = dcache_sets * line_bytes;
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x60000;
    std::array<memblock::StoreTransaction, line_count> stores{};

    for (unsigned index = 0; index < stores.size(); ++index) {
        const std::uint64_t line = base + index * same_set_stride;
        environment.memory().fill_incrementing(
            line, line_bytes, static_cast<std::uint8_t>(0x20 + index * 7));
        stores[index] = memblock::StoreTransaction{
            .address = line + 24,
            .data = 0xc0decafe00000000ULL | index,
            .op = memblock::StoreOp::sd,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .sq = memblock::sq_pointer_value(index),
            .sq_flag = memblock::sq_pointer_flag(index),
            .address_lane = index % memblock::kScalarStoreLanes,
            .data_lane = (index + 1) % memblock::kScalarStoreLanes,
        };
        auto expected_line = environment.memory().read_beat(line, line_bytes);
        for (unsigned byte = 0; byte < sizeof(std::uint64_t); ++byte) {
            expected_line[24 + byte] = static_cast<unsigned char>(
                stores[index].data >> (8 * byte));
        }
        environment.expect_release_line(line, expected_line);
    }

    environment.configure_backpressure(0x243f6a8885a308d3ULL, true);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    for (unsigned index = 0; index < stores.size(); ++index) {
        const auto &store = stores[index];
        environment.expect_store(store);
        const bool data_first = (index & 1U) == 0;
        const bool issued = environment.enqueue_store(store, 0) &&
            (data_first
                 ? environment.issue_store_data(store) &&
                       environment.issue_store_address(store)
                 : environment.issue_store_address(store) &&
                       environment.issue_store_data(store));
        if (!issued || !environment.run_until_store_complete(256) ||
            !environment.commit_store(store)) {
            std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=store reason=" << environment.error() << '\n';
            return 1;
        }
    }

    if (!environment.run_until_release_data() || !environment.run_cycles(32)) {
        std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL cycle=" << environment.cycle()
                  << " phase=release reason=" << environment.error() << '\n';
        return 1;
    }

    unsigned preserved_stores = 0;
    for (const auto &store : stores) {
        if (environment.bus_expected_load(store.address, memblock::LoadOp::ld) ==
            store.data) {
            ++preserved_stores;
        }
    }
    const std::uint64_t verified_releases =
        environment.tilelink_release_data_verified();
    if (environment.tilelink_release_data() < line_count - dcache_ways ||
        verified_releases != environment.tilelink_release_data() ||
        preserved_stores != verified_releases) {
        std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL cycle=" << environment.cycle()
                  << " phase=data reason=not every observed ReleaseData preserved bytes"
                  << " release_data=" << environment.tilelink_release_data()
                  << " verified=" << verified_releases
                  << " preserved=" << preserved_stores << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_DCACHE_RELEASE_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << stores.size()
              << " sq_dequeued=" << environment.sq_dequeued()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " releases=" << environment.tilelink_releases()
              << " release_data=" << environment.tilelink_release_data()
              << " preserved_stores=" << preserved_stores
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_dcache_coherence(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t line = memblock::kDefaultMemoryBase + 0xd0000;
    environment.memory().fill_incrementing(line, 64, 0x31);
    environment.configure_backpressure(0x6a09e667f3bcc909ULL, true);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    unsigned load_index = 0;
    auto load = [&](std::uint64_t address, std::uint64_t expected) {
        const memblock::LoadTransaction transaction{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(load_index * 2),
            .rob_flag = memblock::rob_pointer_flag(load_index * 2),
            .lq = memblock::lq_pointer_value(load_index),
            .lq_flag = memblock::lq_pointer_flag(load_index),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(8 + load_index),
            .lane = load_index % memblock::kScalarLoadLanes,
        };
        ++load_index;
        environment.expect_load_data(transaction, expected);
        return environment.enqueue_load(transaction) &&
               environment.issue_load(transaction) &&
               environment.run_until_complete(4096);
    };

    const std::uint64_t load_address = line + 24;
    const std::uint64_t initial_data =
        environment.memory().expected_load(load_address, memblock::LoadOp::ld);
    if (!load(load_address, initial_data) ||
        !environment.request_dcache_probe(line, 2, false, 2) ||
        !environment.run_until_probe_responses(1) ||
        !load(load_address, initial_data)) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << environment.cycle() << " phase=clean_invalidate reason="
                  << environment.error() << '\n';
        return 1;
    }

    const auto clean_line = environment.memory().read_beat(line, 64);
    if (!environment.request_dcache_probe(line, 2, true, 2, clean_line) ||
        !environment.run_until_probe_responses(2) ||
        !load(load_address, initial_data)) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << environment.cycle() << " phase=clean_data reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::StoreTransaction store{
        .address = load_address,
        .data = 0xdecafbad12345678ULL,
        .op = memblock::StoreOp::sd,
        .rob = 20,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
    };
    environment.expect_store(store);
    if (!environment.enqueue_store(store, 0) ||
        !environment.issue_store_data(store) ||
        !environment.issue_store_address(store) ||
        !environment.run_until_store_complete(4096) ||
        !environment.commit_store(store) || !environment.run_cycles(32)) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << environment.cycle() << " phase=dirty_store reason="
                  << environment.error() << '\n';
        return 1;
    }

    const auto dirty_line = environment.memory().read_beat(line, 64);
    if (!environment.request_dcache_probe(line, 2, false, 1, dirty_line) ||
        !environment.run_until_probe_responses(3) ||
        !load(load_address, store.data) || !environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << environment.cycle() << " phase=dirty_invalidate reason="
                  << environment.error() << '\n';
        return 1;
    }

    memblock::Environment overlap(argc, argv);
    constexpr std::uint64_t overlap_base =
        memblock::kDefaultMemoryBase + 0xe0000;
    constexpr std::uint64_t probe_line_a = overlap_base;
    constexpr std::uint64_t probe_line_b = overlap_base + 0x1000;
    constexpr std::uint64_t miss_line = overlap_base + 0x2000;
    overlap.memory().fill_incrementing(probe_line_a, 64, 0x51);
    overlap.memory().fill_incrementing(probe_line_b, 64, 0x67);
    overlap.memory().fill_incrementing(miss_line, 64, 0x7d);
    overlap.configure_backpressure(
        0xbb67ae8584caa73bULL, true,
        memblock::ResponseLatencyProfile::spec);
    if (!overlap.reset()) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << overlap.cycle() << " phase=overlap-reset reason="
                  << overlap.error() << '\n';
        return 1;
    }
    std::vector<memblock::LoadTransaction> overlap_loads;
    for (unsigned index = 0; index < 3; ++index) {
        const std::uint64_t load_line = index == 0
            ? probe_line_a : index == 1 ? probe_line_b : miss_line;
        overlap_loads.push_back(memblock::LoadTransaction{
            .address = load_line + 24,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(40 + index),
            .lq = static_cast<std::uint8_t>(index),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(30 + index),
            .lane = index,
        });
    }
    for (unsigned index = 0; index < 2; ++index) {
        const auto &warm = overlap_loads[index];
        overlap.expect_load(warm);
        if (!overlap.enqueue_load(warm) || !overlap.issue_load(warm, 256) ||
            !overlap.run_until_complete(4096)) {
            std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                      << overlap.cycle() << " phase=overlap-warm-" << index
                      << " reason=" << overlap.error() << '\n';
            return 1;
        }
    }

    const auto &cold = overlap_loads.back();
    const std::uint64_t writebacks_before = overlap.writebacks();
    const std::uint64_t refills_before = overlap.dcache_refills();
    overlap.expect_load(cold);
    if (!overlap.enqueue_load(cold) || !overlap.issue_load(cold, 256)) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << overlap.cycle() << " phase=overlap-cold-issue reason="
                  << overlap.error() << '\n';
        return 1;
    }
    for (unsigned cycle = 0;
         cycle < 1024 && overlap.dcache_refills() == refills_before;
         ++cycle) {
        if (!overlap.run_cycles(1)) {
            std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                      << overlap.cycle()
                      << " phase=overlap-refill-request reason="
                      << overlap.error() << '\n';
            return 1;
        }
    }
    if (overlap.dcache_refills() != refills_before + 1 ||
        overlap.writebacks() != writebacks_before ||
        !overlap.request_dcache_probe(probe_line_a, 2, false, 2) ||
        !overlap.request_dcache_probe(probe_line_b, 2, false, 2)) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << overlap.cycle()
                  << " phase=overlap-probe-request refills_before="
                  << refills_before << " refills_after="
                  << overlap.dcache_refills() << " writebacks_before="
                  << writebacks_before << " writebacks_after="
                  << overlap.writebacks() << " reason=" << overlap.error()
                  << '\n';
        return 1;
    }
    for (unsigned cycle = 0;
         cycle < 1024 && overlap.dcache_probes() < 2; ++cycle) {
        if (!overlap.run_cycles(1) ||
            overlap.writebacks() != writebacks_before) {
            std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                      << overlap.cycle()
                      << " phase=overlap-probe-accept probes="
                      << overlap.dcache_probes() << " writebacks_before="
                      << writebacks_before << " writebacks_after="
                      << overlap.writebacks() << " reason=" << overlap.error()
                      << '\n';
            return 1;
        }
    }
    if (overlap.dcache_probes() != 2 ||
        overlap.dcache_probe_sources() != 2 ||
        overlap.dcache_max_probe_outstanding() < 2 ||
        !overlap.run_until_probe_responses(2, 4096) ||
        !overlap.run_until_complete(4096) ||
        !overlap.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << overlap.cycle()
                  << " phase=overlap-complete probes="
                  << overlap.dcache_probes() << " probe_sources="
                  << overlap.dcache_probe_sources() << " max_probe_outstanding="
                  << overlap.dcache_max_probe_outstanding()
                  << " probe_responses=" << overlap.dcache_probe_responses()
                  << " writebacks=" << overlap.writebacks()
                  << " reason=" << overlap.error() << '\n';
        return 1;
    }

    const std::uint64_t expected_grants =
        environment.dcache_refills() + environment.dcache_acquire_perms();
    const bool passed = environment.dcache_probes() == 3 &&
        environment.dcache_probe_responses() == 3 &&
        environment.dcache_probe_data() == 2 &&
        environment.dcache_refills() >= 4 &&
        environment.dcache_grant_acks() == expected_grants &&
        environment.dcache_grant_ack_stalls() != 0;
    if (!passed) {
        std::cerr << "MEMBLOCK_DCACHE_COHERENCE_FAIL cycle="
                  << environment.cycle()
                  << " phase=coverage reason=coherence_count_gate_failed"
                  << " refills=" << environment.dcache_refills()
                  << " acquire_perms=" << environment.dcache_acquire_perms()
                  << " probes=" << environment.dcache_probes()
                  << " probe_responses=" << environment.dcache_probe_responses()
                  << " probe_data=" << environment.dcache_probe_data()
                  << " grant_acks=" << environment.dcache_grant_acks()
                  << " grant_ack_stalls="
                  << environment.dcache_grant_ack_stalls() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_DCACHE_COHERENCE_PASS"
              << " cycle=" << environment.cycle()
              << " scalar_loads=" << load_index
              << " scalar_stores=1"
              << " dcache_a=" << environment.tilelink_requests()
              << " dcache_gets=" << environment.dcache_gets()
              << " dcache_refills=" << environment.dcache_refills()
              << " dcache_acquire_perms="
              << environment.dcache_acquire_perms()
              << " probes=" << environment.dcache_probes()
              << " probe_responses=" << environment.dcache_probe_responses()
              << " probe_data=" << environment.dcache_probe_data()
              << " grant_acks=" << environment.dcache_grant_acks()
              << " grant_ack_stalls="
              << environment.dcache_grant_ack_stalls()
              << " overlap_cycles=" << overlap.cycle()
              << " overlap_refills=" << overlap.dcache_refills()
              << " overlap_probe_sources=" << overlap.dcache_probe_sources()
              << " overlap_probe_depth="
              << overlap.dcache_max_probe_outstanding()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_store_rdata_order(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t older_base =
        memblock::kDefaultMemoryBase + 0x74000;
    constexpr std::uint64_t younger_base =
        memblock::kDefaultMemoryBase + (std::uint64_t{1} << 30) + 0x7000;
    const memblock::StoreTransaction older{
        .address = older_base + 8,
        .data = 0x0123456789abcdefULL,
        .op = memblock::StoreOp::sd,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::StoreTransaction younger{
        .address = younger_base + 65,
        .data = 0xfedcba9876543210ULL,
        .op = memblock::StoreOp::sd,
        .rob = 1,
        .sq = 1,
        .address_lane = 1,
        .data_lane = 1,
        .expected_exception_mask = memblock::kExceptionStoreAddressMisaligned,
        // StoreUnit propagates TriggerAction.None on the exceptional address
        // writeback path; the standalone store-data adapter uses zero instead.
        .expected_trigger = memblock::kTriggerNone,
    };

    if (!environment.reset() ||
        !environment.configure_sv39_nc(older.address, older.address) ||
        !environment.configure_sv39_nc(younger.address, younger.address) ||
        !environment.enqueue_store(older, 0) ||
        !environment.enqueue_store(younger, 0)) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle() << " phase=allocate reason="
                  << environment.error() << '\n';
        return 1;
    }

    environment.expect_store(older);
    environment.expect_store(younger);
    if (!environment.warm_store_translation(younger) ||
        !environment.issue_store_data(younger) ||
        !environment.run_cycles(32) ||
        !environment.warm_store_translation(older) ||
        !environment.issue_store_data(older) ||
        !environment.run_until_store_complete(128) ||
        !environment.set_wfi(true) ||
        !environment.commit_stores_through(younger, 2) ||
        !environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle() << " phase=younger-first reason="
                  << environment.error() << '\n';
        return 1;
    }
    if (environment.sq_dequeued() != 0) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle()
                  << " phase=ordering reason=younger store dequeued before older\n";
        return 1;
    }

    if (!environment.set_wfi(false) ||
        !environment.run_until_sq_dequeued(2, 512) ||
        !environment.run_cycles(32)) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle() << " phase=older-complete reason="
                  << environment.error() << '\n';
        return 1;
    }

    // Uncache stores update the bus-side memory agent directly.  The
    // architectural reference image is intentionally updated only after an
    // observed commit, so this ordering oracle must inspect the bus image.
    const std::uint64_t observed_older =
        environment.bus_expected_load(older.address, memblock::LoadOp::ld);
    const std::uint64_t observed_younger =
        environment.bus_expected_load(younger.address, memblock::LoadOp::ld);
    if (observed_older != older.data || observed_younger != 0) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle()
                  << " phase=data older=0x" << std::hex << observed_older
                  << " younger=0x" << observed_younger << std::dec
                  << " reason=NC store used out-of-order SQ read data\n";
        return 1;
    }

    std::cout << "MEMBLOCK_STORE_RDATA_ORDER_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << environment.store_writebacks()
              << " sq_dequeued=" << environment.sq_dequeued()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " ptw_requests=" << environment.ptw_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_store_tlb_miss_preserve(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t address =
        memblock::kDefaultMemoryBase + 0x7c088;
    constexpr std::uint64_t miss_address = address + (std::uint64_t{1} << 30);
    const memblock::StoreTransaction store{
        .address = address,
        .data = 0x5aa5c33c96696996ULL,
        .op = memblock::StoreOp::sd,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
    };

    environment.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x51);
    if (!environment.reset() ||
        !environment.configure_sv39(store.address, store.address) ||
        !environment.configure_sv39(miss_address, miss_address) ||
        !environment.enqueue_store(store, 0)) {
        std::cerr << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_FAIL cycle="
                  << environment.cycle() << " phase=allocate reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.expect_store(store);
    if (!environment.warm_store_translation(store) ||
        !environment.run_cycles(8)) {
        std::cerr << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_FAIL cycle="
                  << environment.cycle() << " phase=initial-hit reason="
                  << environment.error() << '\n';
        return 1;
    }

    const std::uint64_t initial_misses = environment.store_tlb_misses();
    const std::uint64_t initial_ptw = environment.ptw_requests();
    auto miss_store = store;
    miss_store.address = miss_address;
    const std::uint64_t miss_target = initial_misses + 1;
    if (!environment.issue_store_address(miss_store) ||
        !environment.run_until_store_tlb_misses(miss_target) ||
        !environment.run_until_ptw_requests(initial_ptw + 1)) {
        std::cerr << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_FAIL cycle="
                  << environment.cycle() << " phase=tlb-miss"
                  << " feedbacks=" << environment.store_tlb_feedbacks()
                  << " misses=" << environment.store_tlb_misses()
                  << " ptw_requests=" << environment.ptw_requests()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    if (!environment.run_cycles(4)) {
        std::cerr << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_FAIL cycle="
                  << environment.cycle() << " phase=assertion"
                  << " feedbacks=" << environment.store_tlb_feedbacks()
                  << " misses=" << environment.store_tlb_misses()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << environment.store_writebacks()
              << " feedbacks=" << environment.store_tlb_feedbacks()
              << " misses=" << environment.store_tlb_misses()
              << " ptw_requests=" << environment.ptw_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_iq_slow_feedback(int argc, char **argv)
{
    constexpr std::uint64_t root = 0x9d000000ULL;
    constexpr std::array<std::uint64_t, 2> virtual_addresses{
        0x6d000018ULL, 0x6d002028ULL};
    constexpr std::array<std::uint64_t, 2> physical_addresses{
        0xd0000018ULL, 0xd1000028ULL};
    memblock::Environment scalar(argc, argv);
    for (const auto address : physical_addresses) {
        scalar.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x31);
    }
    if (!scalar.reset() ||
        !scalar.map_sv39_4k(virtual_addresses[0], physical_addresses[0], root) ||
        !scalar.map_sv39_4k(virtual_addresses[1], physical_addresses[1], root) ||
        !scalar.activate_sv39(root) || !scalar.set_rob_head(0)) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << scalar.cycle()
                  << " phase=sta-config reason=" << scalar.error() << '\n';
        return 1;
    }

    std::vector<memblock::StoreTransaction> misses;
    for (unsigned lane = 0; lane < memblock::kScalarStoreLanes; ++lane) {
        misses.push_back(memblock::StoreTransaction{
            .address = virtual_addresses[lane],
            .oracle_address = physical_addresses[lane],
            .data = 0x1020304050607080ULL + lane,
            .op = memblock::StoreOp::sd,
            .rob = static_cast<std::uint8_t>(lane),
            .sq = static_cast<std::uint8_t>(lane),
            .address_lane = lane,
            .data_lane = lane,
        });
        scalar.expect_store(misses.back());
        if (!scalar.enqueue_store(misses.back(), static_cast<std::uint8_t>(lane))) {
            std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << scalar.cycle()
                      << " phase=sta-miss-enqueue reason=" << scalar.error() << '\n';
            return 1;
        }
    }
    if (!scalar.issue_store_address_batch(misses, 256) ||
        !scalar.run_cycles(16)) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << scalar.cycle()
                  << " phase=sta-miss-issue reason=" << scalar.error() << '\n';
        return 1;
    }

    const auto find_sta = [](const auto &stats, unsigned lane, bool hit,
                             std::uint8_t sq_value) {
        return std::find_if(
            stats.sta_samples.begin(), stats.sta_samples.end(),
            [&](const auto &sample) {
                return sample.lane == lane && sample.hit == hit &&
                    !sample.sq_flag && sample.sq_value == sq_value;
            });
    };
    const auto &miss_stats = scalar.iq_slow_feedback_stats();
    const auto miss0 = find_sta(miss_stats, 0, false, 0);
    const auto miss1 = find_sta(miss_stats, 1, false, 1);
    if (miss0 == miss_stats.sta_samples.end() ||
        miss1 == miss_stats.sta_samples.end() || miss0->cycle != miss1->cycle) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << scalar.cycle()
                  << " phase=sta-same-cycle-miss samples="
                  << miss_stats.sta_samples.size() << " lane_valid="
                  << miss_stats.sta_valid[0] << ',' << miss_stats.sta_valid[1]
                  << " lane_miss=" << miss_stats.sta_misses[0] << ','
                  << miss_stats.sta_misses[1] << '\n';
        return 1;
    }

    for (const auto &transaction : misses) {
        if (!scalar.issue_store_address_until_tlb_hit(transaction, 4096)) {
            std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << scalar.cycle()
                      << " phase=sta-warm reason=" << scalar.error() << '\n';
            return 1;
        }
    }
    std::vector<memblock::StoreTransaction> hits = misses;
    for (unsigned lane = 0; lane < hits.size(); ++lane) {
        hits[lane].rob = static_cast<std::uint8_t>(lane + 2);
        hits[lane].sq = static_cast<std::uint8_t>(lane + 2);
        scalar.expect_store(hits[lane]);
        if (!scalar.enqueue_store(hits[lane], static_cast<std::uint8_t>(lane))) {
            std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << scalar.cycle()
                      << " phase=sta-hit-enqueue reason=" << scalar.error() << '\n';
            return 1;
        }
    }
    if (!scalar.issue_store_address_batch(hits, 256) || !scalar.run_cycles(8)) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << scalar.cycle()
                  << " phase=sta-hit-issue reason=" << scalar.error() << '\n';
        return 1;
    }
    const auto &hit_stats = scalar.iq_slow_feedback_stats();
    const auto hit0 = find_sta(hit_stats, 0, true, 2);
    const auto hit1 = find_sta(hit_stats, 1, true, 3);
    if (hit0 == hit_stats.sta_samples.end() ||
        hit1 == hit_stats.sta_samples.end() || hit0->cycle != hit1->cycle) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << scalar.cycle()
                  << " phase=sta-same-cycle-hit samples="
                  << hit_stats.sta_samples.size() << " lane_hit="
                  << hit_stats.sta_hits[0] << ',' << hit_stats.sta_hits[1] << '\n';
        return 1;
    }

    memblock::Environment vector(argc, argv);
    if (!vector.reset() || !vector.enable_misaligned_accesses() ||
        !vector.set_rob_head(0)) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << vector.cycle()
                  << " phase=vstu-config reason=" << vector.error() << '\n';
        return 1;
    }
    std::vector<memblock::VectorMemoryTransaction> vector_hits;
    for (unsigned lane = 0; lane < memblock::kVectorMemoryLanes; ++lane) {
        memblock::VectorMemoryTransaction transaction{
            .store = true,
            .address = memblock::kDefaultMemoryBase + 0x7e000 + lane * 0x100,
            .addressing = memblock::VectorAddressingMode::unit_stride,
            .eew = 0,
            .vl = 16,
            .vm = false,
            .mask_bits = 0,
            .rob = static_cast<std::uint8_t>(lane),
            .lq = static_cast<std::uint8_t>(lane + 4),
            .sq = static_cast<std::uint8_t>(lane),
            .lane = lane,
            .flow_num = 1,
        };
        vector_hits.push_back(transaction);
        vector.expect_vector(vector_hits.back());
        if (!vector.enqueue_vector(vector_hits.back())) {
            std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << vector.cycle()
                      << " phase=vstu-hit-enqueue reason=" << vector.error() << '\n';
            return 1;
        }
    }
    if (!vector.issue_vector_batch_same_cycle(vector_hits, 256) ||
        !vector.run_cycles(64)) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << vector.cycle()
                  << " phase=vstu-hit-issue reason=" << vector.error() << '\n';
        return 1;
    }
    const auto find_vstu_hit = [](const auto &stats, const auto &transaction) {
        return std::find_if(
            stats.vstu_samples.begin(), stats.vstu_samples.end(),
            [&](const auto &sample) {
                return sample.lane == transaction.lane && sample.hit &&
                    sample.lq_flag == transaction.lq_flag &&
                    sample.lq_value == transaction.lq &&
                    sample.sq_flag == transaction.sq_flag &&
                    sample.sq_value == transaction.sq &&
                    !sample.is_part_replay && sample.replay_mask == 0;
            });
    };
    const auto &vector_hit_stats = vector.iq_slow_feedback_stats();
    const auto vhit0 = find_vstu_hit(vector_hit_stats, vector_hits[0]);
    const auto vhit1 = find_vstu_hit(vector_hit_stats, vector_hits[1]);
    if (vhit0 == vector_hit_stats.vstu_samples.end() ||
        vhit1 == vector_hit_stats.vstu_samples.end() ||
        vhit0->cycle != vhit1->cycle) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << vector.cycle()
                  << " phase=vstu-same-cycle-hit samples="
                  << vector_hit_stats.vstu_samples.size() << " lane_hit="
                  << vector_hit_stats.vstu_hits[0] << ','
                  << vector_hit_stats.vstu_hits[1] << '\n';
        return 1;
    }

    memblock::Environment replay(argc, argv);
    constexpr std::uint64_t replay_base =
        memblock::kDefaultMemoryBase + 0x7f000;
    replay.memory().fill_incrementing(replay_base, 0x1000, 0x71);
    if (!replay.reset() || !replay.enable_misaligned_accesses() ||
        !replay.set_rob_head(0)) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << replay.cycle()
                  << " phase=vstu-replay-config reason=" << replay.error() << '\n';
        return 1;
    }
    memblock::VectorMemoryTransaction replay_store{
        .store = true,
        .address = replay_base + 5,
        .stride = 8,
        .addressing = memblock::VectorAddressingMode::strided,
        .eew = 3,
        .vl = 2,
        .rob = 0,
        .lq = 9,
        .sq = 0,
        .lane = 1,
        .flow_num = 2,
    };
    for (unsigned byte = 0; byte < replay_store.data.size(); ++byte) {
        replay_store.data[byte] = static_cast<unsigned char>(0xa1 + 3 * byte);
    }
    replay.expect_vector(replay_store);
    if (!replay.enqueue_vector(replay_store) ||
        !replay.issue_vector(replay_store, 512) || !replay.run_cycles(32) ||
        !replay.pulse_pending_store(replay_store.rob, replay_store.rob_flag) ||
        !replay.run_until_vector_complete_with_replays(replay_store, 8192) ||
        !replay.commit_vector_store(replay_store, 8192)) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << replay.cycle()
                  << " phase=vstu-replay-run reason=" << replay.error() << '\n';
        return 1;
    }
    const auto &replay_stats = replay.iq_slow_feedback_stats();
    const auto replay_sample = std::find_if(
        replay_stats.vstu_samples.begin(), replay_stats.vstu_samples.end(),
        [&](const auto &sample) {
            const unsigned packed = sample.sq_value +
                (sample.sq_flag ? memblock::kStoreQueueEntries : 0);
            const unsigned base = replay_store.sq +
                (replay_store.sq_flag ? memblock::kStoreQueueEntries : 0);
            const unsigned distance =
                (packed + 2 * memblock::kStoreQueueEntries - base) %
                (2 * memblock::kStoreQueueEntries);
            return sample.lane == replay_store.lane && !sample.hit &&
                sample.lq_flag == replay_store.lq_flag &&
                sample.lq_value == replay_store.lq &&
                distance < replay_store.flow_num && sample.is_part_replay &&
                sample.replay_mask != 0;
        });
    if (replay_sample == replay_stats.vstu_samples.end()) {
        std::cerr << "MEMBLOCK_IQ_SLOW_FEEDBACK_FAIL cycle=" << replay.cycle()
                  << " phase=vstu-replay-fields samples="
                  << replay_stats.vstu_samples.size() << " lane_miss="
                  << replay_stats.vstu_misses[0] << ','
                  << replay_stats.vstu_misses[1] << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_IQ_SLOW_FEEDBACK_PASS"
              << " sta_samples=" << hit_stats.sta_samples.size()
              << " sta_same_cycle_miss=2 sta_same_cycle_hit=2"
              << " vstu_samples=" << vector_hit_stats.vstu_samples.size()
              << " vstu_same_cycle_hit=2"
              << " vstu_replays=" << replay.vector_replay_feedbacks()
              << " replay_mask=0x" << std::hex << replay_sample->replay_mask
              << std::dec << " replay_mb="
              << static_cast<unsigned>(replay_sample->replay_mb_index)
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_redirect(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t address = memblock::kDefaultMemoryBase + 0x4380;
    environment.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x40);
    environment.configure_backpressure(0x123456789abcdef0ULL, true);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_REDIRECT_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction canceled{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 20,
        .lq = 0,
        .sq = 0,
        .pdest = 30,
        .lane = 2,
    };
    if (!environment.enqueue_load(canceled) || !environment.issue_load(canceled) ||
        !environment.redirect_after(19, false, false) ||
        !environment.run_cycles(96)) {
        std::cerr << "MEMBLOCK_REDIRECT_FAIL cycle=" << environment.cycle()
                  << " phase=cancel reason=" << environment.error() << '\n';
        return 1;
    }
    if (environment.redirect_cancellation_events_observed() != 1 ||
        environment.lq_redirect_canceled_observed() != 1 ||
        environment.sq_redirect_canceled_observed() != 0 ||
        environment.lq_canceled_unobserved() != 0 ||
        environment.sq_canceled_unobserved() != 0) {
        std::cerr << "MEMBLOCK_REDIRECT_FAIL cycle=" << environment.cycle()
                  << " phase=cancel-observation"
                  << " observed="
                  << environment.redirect_cancellation_events_observed() << ','
                  << environment.lq_redirect_canceled_observed() << ','
                  << environment.sq_redirect_canceled_observed()
                  << " unobserved="
                  << environment.lq_canceled_unobserved() << ','
                  << environment.sq_canceled_unobserved() << '\n';
        return 1;
    }

    const memblock::LoadTransaction survivor{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 20,
        .lq = 0,
        .sq = 0,
        .pdest = 31,
        .lane = 1,
    };
    environment.expect_load(survivor);
    if (!environment.enqueue_load(survivor) || !environment.issue_load(survivor) ||
        !environment.run_until_complete(256)) {
        std::cerr << "MEMBLOCK_REDIRECT_FAIL cycle=" << environment.cycle()
                  << " phase=recovery reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_REDIRECT_PASS"
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " surviving_writebacks=" << environment.writebacks()
              << " redirect_cancels_observed="
              << environment.redirect_cancellation_events_observed() << ','
              << environment.lq_redirect_canceled_observed() << ','
              << environment.sq_redirect_canceled_observed()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_queue_pressure(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr unsigned transaction_count = 120;
    constexpr unsigned wave_size = 60;
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x8000;
    environment.memory().fill_incrementing(base, 4096, 0x21);
    environment.configure_backpressure(0xa5a55a5af00dcafeULL, true);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_QUEUE_PRESSURE_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::vector<memblock::LoadTransaction> transactions;
    transactions.reserve(transaction_count);
    for (unsigned index = 0; index < transaction_count; ++index) {
        transactions.push_back(memblock::LoadTransaction{
            .address = base + ((index * 37) % 512) * 8,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .lq = memblock::lq_pointer_value(index),
            .lq_flag = memblock::lq_pointer_flag(index),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(1 + index),
            .lane = index % memblock::kScalarLoadLanes,
        });
    }

    for (unsigned wave = 0; wave < transaction_count; wave += wave_size) {
        const unsigned wave_end = std::min(wave + wave_size, transaction_count);
        for (unsigned index = wave; index < wave_end; ++index) {
            if (!environment.enqueue_load(transactions[index])) {
                std::cerr << "MEMBLOCK_QUEUE_PRESSURE_FAIL transaction=" << index
                          << " cycle=" << environment.cycle()
                          << " phase=enqueue reason=" << environment.error() << '\n';
                return 1;
            }
        }

        for (unsigned begin = wave; begin < wave_end; begin += 3) {
            std::vector<memblock::LoadTransaction> batch;
            for (unsigned offset = 0; offset < 3 && begin + offset < wave_end;
                 ++offset) {
                const auto &transaction = transactions[begin + offset];
                environment.expect_load(transaction);
                batch.push_back(transaction);
            }
            if (!environment.issue_load_batch(batch) ||
                !environment.run_until_complete(1024)) {
                std::cerr << "MEMBLOCK_QUEUE_PRESSURE_FAIL transaction=" << begin
                          << " cycle=" << environment.cycle()
                          << " phase=drain reason=" << environment.error() << '\n';
                return 1;
            }
        }
        if (!environment.run_until_lq_retired(256)) {
            std::cerr << "MEMBLOCK_QUEUE_PRESSURE_FAIL transaction=" << wave_end
                      << " cycle=" << environment.cycle()
                      << " phase=retire reason=" << environment.error() << '\n';
            return 1;
        }
    }
    std::cout << "MEMBLOCK_QUEUE_PRESSURE_PASS"
              << " cycle=" << environment.cycle()
              << " allocated=" << transaction_count
              << " writebacks=" << environment.writebacks()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_scalar_misaligned(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t bare_base = memblock::kDefaultMemoryBase + 0x50000;
    constexpr std::uint64_t virtual_base = 0x40000000ULL;
    constexpr std::uint64_t physical_page0 = 0xa1000000ULL;
    constexpr std::uint64_t physical_page1 = 0xa2000000ULL;
    constexpr std::uint64_t root = 0x91000000ULL;
    environment.memory().fill_incrementing(bare_base, 0x3000, 0x31);
    environment.memory().fill_incrementing(physical_page0, 0x1000, 0x57);
    environment.memory().fill_incrementing(physical_page1, 0x1000, 0xa3);
    environment.configure_backpressure(0x7f4a7c159e3779b9ULL, true);
    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    const std::array<memblock::LoadTransaction, 4> bare{{
        {
            .address = bare_base + 3,
            .op = memblock::LoadOp::lw,
            .rob = 0,
            .lq = 0,
            .pdest = 20,
            .lane = 0,
        },
        {
            .address = bare_base + 13,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 1,
            .pdest = 21,
            .lane = 1,
        },
        {
            .address = bare_base + 61,
            .op = memblock::LoadOp::ld,
            .rob = 2,
            .lq = 2,
            .pdest = 22,
            .lane = 2,
        },
        {
            .address = bare_base + 0xffd,
            .op = memblock::LoadOp::ld,
            .rob = 3,
            .lq = 3,
            .pdest = 23,
            .lane = 0,
        },
    }};
    for (std::size_t index = 0; index < bare.size(); ++index) {
        const auto &transaction = bare[index];
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 256) ||
            !environment.run_until_complete(4096) ||
            !environment.run_until_lq_retired(1024)) {
            std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=bare reason=" << environment.error() << '\n';
            return 1;
        }
    }

    if (!environment.map_sv39_4k(
            virtual_base, physical_page0, root) ||
        !environment.map_sv39_4k(
            virtual_base + 0x1000, physical_page1, root) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                  << environment.cycle() << " phase=sv39-config reason="
                  << environment.error() << '\n';
        return 1;
    }

    memblock::LoadTransaction cross_page{
        .address = virtual_base + 0xffd,
        .oracle_address = physical_page0 + 0xffd,
        .op = memblock::LoadOp::ld,
        .rob = 4,
        .lq = 4,
        .pdest = 24,
        .lane = 1,
    };
    std::uint64_t expected = 0;
    for (unsigned byte = 0; byte < 8; ++byte) {
        const std::uint64_t physical = byte < 3
            ? physical_page0 + 0xffd + byte
            : physical_page1 + byte - 3;
        expected |= std::uint64_t{environment.memory().read_byte(physical)}
                    << (8 * byte);
    }
    environment.expect_load_data(cross_page, expected);
    if (!environment.set_rob_head(cross_page.rob) ||
        !environment.enqueue_load(cross_page) ||
        !environment.issue_load(cross_page, 256) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                  << environment.cycle() << " phase=translated-cross-page reason="
                  << environment.error() << '\n';
        return 1;
    }

    memblock::Environment pressure(argc, argv);
    constexpr unsigned pressure_load_count = 60;
    constexpr std::uint64_t pressure_base =
        memblock::kDefaultMemoryBase + 0x80000;
    pressure.memory().fill_incrementing(pressure_base, 0x8000, 0x6d);
    pressure.configure_backpressure(0x36a5f19c7d4e82b1ULL, true);
    if (!pressure.reset() || !pressure.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                  << pressure.cycle()
                  << " phase=rar-pressure-reset reason="
                  << pressure.error() << '\n';
        return 1;
    }

    const std::array<memblock::LoadTransaction, 3> pending_split{{
        {
            .address = pressure_base + 0x1003,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(0),
            .rob_flag = memblock::rob_pointer_flag(0),
            .lq = memblock::lq_pointer_value(0),
            .lq_flag = memblock::lq_pointer_flag(0),
            .pdest = 40,
            .lane = 0,
        },
        {
            .address = pressure_base + 0x107d,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(1),
            .rob_flag = memblock::rob_pointer_flag(1),
            .lq = memblock::lq_pointer_value(1),
            .lq_flag = memblock::lq_pointer_flag(1),
            .pdest = 41,
            .lane = 1,
        },
        {
            .address = pressure_base + 0x1ffd,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(2),
            .rob_flag = memblock::rob_pointer_flag(2),
            .lq = memblock::lq_pointer_value(2),
            .lq_flag = memblock::lq_pointer_flag(2),
            .pdest = 42,
            .lane = 2,
        },
    }};
    for (const auto &transaction : pending_split) {
        if (!pressure.enqueue_load(transaction)) {
            std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                      << pressure.cycle()
                      << " phase=rar-pressure-split-enqueue reason="
                      << pressure.error() << '\n';
            return 1;
        }
    }

    std::vector<memblock::LoadTransaction> younger_loads;
    younger_loads.reserve(pressure_load_count);
    for (unsigned index = 0; index < pressure_load_count; ++index) {
        const unsigned pointer = pending_split.size() + index;
        younger_loads.push_back(memblock::LoadTransaction{
            .address = pressure_base + 0x4000 + index * 64 + 16,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(pointer),
            .rob_flag = memblock::rob_pointer_flag(pointer),
            .lq = memblock::lq_pointer_value(pointer),
            .lq_flag = memblock::lq_pointer_flag(pointer),
            .pdest = static_cast<std::uint8_t>(48 + index % 64),
            .lane = index % memblock::kScalarLoadLanes,
        });
        if (!pressure.enqueue_load(younger_loads.back())) {
            std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                      << pressure.cycle()
                      << " phase=rar-pressure-load-enqueue index=" << index
                      << " reason=" << pressure.error() << '\n';
            return 1;
        }
    }
    for (unsigned begin = 0; begin < pressure_load_count;
         begin += memblock::kScalarLoadLanes) {
        std::vector<memblock::LoadTransaction> batch;
        for (unsigned offset = 0; offset < memblock::kScalarLoadLanes;
             ++offset) {
            const auto &transaction = younger_loads[begin + offset];
            pressure.expect_load(transaction);
            batch.push_back(transaction);
        }
        if (!pressure.issue_load_batch(batch, 256, true) ||
            !pressure.run_until_complete(4096)) {
            std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                      << pressure.cycle()
                      << " phase=rar-pressure-fill index=" << begin
                      << " reason=" << pressure.error() << '\n';
            return 1;
        }
    }

    const auto pressure_violations_before =
        pressure.memory_violation_stats().count;
    for (std::size_t index = 0; index < pending_split.size(); ++index) {
        const auto &transaction = pending_split[index];
        pressure.expect_load(transaction);
        if (!pressure.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !pressure.issue_load(transaction, 256) ||
            !pressure.run_until_complete(8192)) {
            std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                      << pressure.cycle()
                      << " phase=rar-pressure-split index=" << index
                      << " reason=" << pressure.error() << '\n';
            return 1;
        }
    }
    if (!pressure.run_until_lq_retired(8192) ||
        pressure.memory_violation_stats().count != pressure_violations_before ||
        pressure.writebacks() != pressure_load_count + pending_split.size()) {
        std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                  << pressure.cycle()
                  << " phase=rar-pressure-check writebacks="
                  << pressure.writebacks() << " expected_writebacks="
                  << pressure_load_count + pending_split.size()
                  << " violations="
                  << pressure.memory_violation_stats().count
                  << " expected_violations=" << pressure_violations_before
                  << " reason=" << pressure.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_SCALAR_MISALIGNED_PASS"
              << " cycle=" << environment.cycle()
              << " writebacks=" << environment.writebacks()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " ptw_requests=" << environment.ptw_requests()
              << " rar_pressure_loads=" << pressure_load_count
              << " pending_splits=" << pending_split.size()
              << " pressure_writebacks=" << pressure.writebacks()
              << " pressure_violations="
              << pressure.memory_violation_stats().count
              << " pressure_cycle=" << pressure.cycle()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_misaligned_stores(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t bare_base = memblock::kDefaultMemoryBase + 0x58000;
    constexpr std::uint64_t virtual_base = 0x48000000ULL;
    constexpr std::uint64_t physical_page0 = 0xa4000000ULL;
    constexpr std::uint64_t physical_page1 = 0xa5000000ULL;
    constexpr std::uint64_t root = 0x93000000ULL;
    environment.memory().fill_incrementing(bare_base, 0x4000, 0x2d);
    environment.memory().fill_incrementing(physical_page0, 0x1000, 0x61);
    environment.memory().fill_incrementing(physical_page1, 0x1000, 0xb7);
    environment.configure_backpressure(0xbb67ae8584caa73bULL, true);
    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    const std::array<memblock::StoreTransaction, 3> scalar_stores{{
        {
            .address = bare_base + 13,
            .data = 0x0123456789abcdefULL,
            .op = memblock::StoreOp::sd,
            .rob = 0,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
            .expected_trigger = memblock::kStoreWritebackTriggerNone,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        },
        {
            .address = bare_base + 61,
            .data = 0xfedcba9876543210ULL,
            .op = memblock::StoreOp::sd,
            .rob = 2,
            .sq = 1,
            .address_lane = 1,
            .data_lane = 0,
            .expected_trigger = memblock::kStoreWritebackTriggerNone,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        },
        {
            .address = bare_base + 0xffd,
            .data = 0x55aa33cc0ff09669ULL,
            .op = memblock::StoreOp::sd,
            .rob = 4,
            .sq = 2,
            .address_lane = 0,
            .data_lane = 0,
            .expected_trigger = memblock::kStoreWritebackTriggerNone,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        },
    }};
    for (std::size_t index = 0; index < scalar_stores.size(); ++index) {
        const auto &store = scalar_stores[index];
        environment.expect_store(store);
        const bool data_first = (index & 1U) != 0;
        if (!environment.set_rob_head(store.rob) ||
            !environment.enqueue_store(store, static_cast<std::uint8_t>(index)) ||
            !(data_first
                  ? environment.issue_store_data(store, 256) &&
                        environment.issue_store_address(store, 256)
                  : environment.issue_store_address(store, 256) &&
                        environment.issue_store_data(store, 256)) ||
            !environment.pulse_pending_store(store.rob, store.rob_flag) ||
            !environment.run_until_store_complete(4096) ||
            !environment.commit_store(store, 8192)) {
            std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=scalar-store reason=" << environment.error()
                      << '\n';
            return 1;
        }

        const memblock::LoadTransaction readback{
            .address = store.address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(store.rob + 1),
            .lq = static_cast<std::uint8_t>(index),
            .sq = static_cast<std::uint8_t>(index + 1),
            .pdest = static_cast<std::uint8_t>(30 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
        };
        environment.expect_load_data(readback, store.data);
        if (!environment.set_rob_head(readback.rob) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 256) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=scalar-readback reason=" << environment.error()
                      << '\n';
            return 1;
        }
    }

    memblock::VectorMemoryTransaction vector_store{
        .store = true,
        .address = bare_base + 0x2005,
        .stride = 8,
        .addressing = memblock::VectorAddressingMode::strided,
        .eew = 3,
        .vl = 2,
        .rob = 6,
        .lq = 3,
        .sq = 3,
        .lane = 1,
        .flow_num = 2,
    };
    for (unsigned byte = 0; byte < vector_store.data.size(); ++byte) {
        vector_store.data[byte] = static_cast<unsigned char>(0x91 + byte * 7);
    }
    environment.expect_vector(vector_store);
    if (!environment.set_rob_head(vector_store.rob) ||
        !environment.enqueue_vector(vector_store) ||
        !environment.issue_vector(vector_store, 512) ||
        !environment.run_cycles(32) ||
        !environment.pulse_pending_store(
            vector_store.rob, vector_store.rob_flag) ||
        !environment.run_until_vector_complete_with_replays(vector_store, 8192) ||
        !environment.commit_vector_store(vector_store, 8192)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=vector-store reason=" << environment.error() << '\n';
        return 1;
    }
    auto vector_readback = vector_store;
    vector_readback.store = false;
    vector_readback.rob = 7;
    vector_readback.lq = 3;
    vector_readback.sq = 5;
    vector_readback.pdest = 40;
    vector_readback.lane = 0;
    vector_readback.expected_trigger = memblock::kVectorWritebackTriggerNone;
    vector_readback.expected_debug_is_mmio = false;
    vector_readback.expected_debug_is_ncio = false;
    vector_readback.expected_debug_is_perf_cnt = false;
    vector_readback.data.fill(0);
    environment.expect_vector_data(vector_readback, vector_store.data);
    if (!environment.set_rob_head(vector_readback.rob) ||
        !environment.enqueue_vector(vector_readback) ||
        !environment.issue_vector(vector_readback, 512) ||
        !environment.run_until_vector_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=vector-readback reason=" << environment.error()
                  << '\n';
        return 1;
    }

    if (!environment.map_sv39_4k(virtual_base, physical_page0, root) ||
        !environment.map_sv39_4k(
            virtual_base + 0x1000, physical_page1, root) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=sv39-config reason=" << environment.error() << '\n';
        return 1;
    }

    auto cross_page_store = vector_store;
    cross_page_store.address = virtual_base + 0xffd;
    cross_page_store.rob = 8;
    cross_page_store.lq = 5;
    cross_page_store.sq = 5;
    cross_page_store.lane = 0;
    cross_page_store.expected_trigger = memblock::kVectorWritebackTriggerNone;
    cross_page_store.expected_debug_is_mmio = false;
    cross_page_store.expected_debug_is_ncio = false;
    cross_page_store.expected_debug_is_perf_cnt = false;
    for (unsigned byte = 0; byte < cross_page_store.data.size(); ++byte) {
        cross_page_store.data[byte] = static_cast<unsigned char>(0xe3 - byte * 5);
    }
    environment.expect_vector(cross_page_store);
    if (!environment.set_rob_head(cross_page_store.rob) ||
        !environment.enqueue_vector(cross_page_store) ||
        !environment.issue_vector(cross_page_store, 512) ||
        !environment.run_cycles(256) ||
        !environment.pulse_pending_store(
            cross_page_store.rob, cross_page_store.rob_flag) ||
        !environment.run_until_vector_complete_with_replays(
            cross_page_store, 16384, true) ||
        !environment.commit_vector_store(cross_page_store, 16384)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=vector-cross-page-store reason="
                  << environment.error() << '\n';
        return 1;
    }
    auto cross_page_readback = cross_page_store;
    cross_page_readback.store = false;
    cross_page_readback.rob = 9;
    cross_page_readback.lq = 5;
    cross_page_readback.sq = 7;
    cross_page_readback.pdest = 41;
    cross_page_readback.lane = 1;
    cross_page_readback.data.fill(0);
    environment.expect_vector_data(cross_page_readback, cross_page_store.data);
    if (!environment.set_rob_head(cross_page_readback.rob) ||
        !environment.enqueue_vector(cross_page_readback) ||
        !environment.issue_vector(cross_page_readback, 512) ||
        !environment.run_until_vector_complete(16384) ||
        !environment.run_until_queues_retired(8192)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=vector-cross-page-readback reason="
                  << environment.error() << '\n';
        return 1;
    }

    // Reproduce the constrained-random corner where a scalar store first
    // misses in the DTLB and only enters StoreMisalignBuffer on a later replay.
    // pendingst must remain high while that store stays at the ROB head.
    memblock::Environment translated_scalar(argc, argv);
    constexpr std::uint64_t scalar_virtual = 0x49000000ULL;
    constexpr std::uint64_t scalar_physical0 = 0xa6000000ULL;
    constexpr std::uint64_t scalar_physical1 = 0xa7000000ULL;
    constexpr std::uint64_t scalar_root = 0x93010000ULL;
    translated_scalar.memory().fill_incrementing(
        scalar_physical0, 0x1000, 0x31);
    translated_scalar.memory().fill_incrementing(
        scalar_physical1, 0x1000, 0x97);
    translated_scalar.configure_backpressure(0x510e527fade682d1ULL, true);
    if (!translated_scalar.reset() ||
        !translated_scalar.enable_misaligned_accesses() ||
        !translated_scalar.map_sv39_4k(
            scalar_virtual, scalar_physical0, scalar_root) ||
        !translated_scalar.map_sv39_4k(
            scalar_virtual + 0x1000, scalar_physical1, scalar_root) ||
        !translated_scalar.activate_sv39(scalar_root)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << translated_scalar.cycle()
                  << " phase=translated-scalar-configuration reason="
                  << translated_scalar.error() << '\n';
        return 1;
    }
    const memblock::StoreTransaction translated_store{
        .address = scalar_virtual + 0xfff,
        .oracle_address = scalar_physical0 + 0xfff,
        .data = 0xa55a,
        .op = memblock::StoreOp::sh,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
    };
    translated_scalar.expect_store(translated_store);
    const std::uint64_t translated_misses_before =
        translated_scalar.store_tlb_misses();
    if (!translated_scalar.enqueue_store(translated_store, 0) ||
        !translated_scalar.issue_store_address(translated_store, 2048) ||
        !translated_scalar.issue_store_data(translated_store, 2048) ||
        !translated_scalar.run_until_store_tlb_misses(
            translated_misses_before + 1, 4096) ||
        !translated_scalar.set_rob_head(
            translated_store.rob, translated_store.rob_flag) ||
        !translated_scalar.run_until_store_complete_with_replay(
            translated_store, 8192, true) ||
        !translated_scalar.commit_store(translated_store, 8192)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << translated_scalar.cycle()
                  << " phase=translated-scalar-store reason="
                  << translated_scalar.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction translated_readback{
        .address = translated_store.address,
        .oracle_address = translated_store.oracle_address,
        .op = memblock::LoadOp::lhu,
        .rob = 1,
        .lq = 0,
        .sq = 1,
        .pdest = 42,
        .lane = 1,
    };
    translated_scalar.expect_load_data(
        translated_readback, translated_store.data & 0xffffU);
    if (!translated_scalar.set_rob_head(translated_readback.rob) ||
        !translated_scalar.enqueue_load(translated_readback) ||
        !translated_scalar.issue_load(translated_readback, 2048) ||
        !translated_scalar.run_until_complete(8192) ||
        !translated_scalar.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << translated_scalar.cycle()
                  << " phase=translated-scalar-readback reason="
                  << translated_scalar.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_MISALIGNED_STORES_PASS"
              << " cycle=" << environment.cycle() + translated_scalar.cycle()
              << " scalar_store_writebacks=" << environment.store_writebacks()
                  + translated_scalar.store_writebacks()
              << " scalar_load_writebacks=" << environment.writebacks()
                  + translated_scalar.writebacks()
              << " vector_store_writebacks="
              << environment.vector_store_writebacks()
              << " vector_load_writebacks="
              << environment.vector_load_writebacks()
              << " vector_replays=" << environment.vector_replay_feedbacks()
              << " ptw_requests="
              << environment.ptw_requests() + translated_scalar.ptw_requests()
              << " translated_scalar_cross_page=1"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_issue_order(int argc, char **argv)
{
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x5e000;
    constexpr unsigned uop_count = 2;
    constexpr unsigned flow_num = 16;

    for (unsigned order = 0; order < 2; ++order) {
        const bool reverse = order != 0;
        memblock::Environment environment(argc, argv);
        environment.memory().fill_incrementing(base, 0x1000, 0x39);
        environment.configure_backpressure(
            0x6a09e667f3bcc909ULL ^ order, false);
        if (!environment.reset() ||
            !environment.set_rob_head(20, false)) {
            std::cerr << "MEMBLOCK_VECTOR_ISSUE_ORDER_FAIL order="
                      << (reverse ? "reverse" : "forward")
                      << " cycle=" << environment.cycle()
                      << " phase=reset reason=" << environment.error() << '\n';
            return 1;
        }

        std::array<memblock::VectorMemoryTransaction, uop_count> uops{};
        for (unsigned uop = 0; uop < uop_count; ++uop) {
            auto &transaction = uops[uop];
            transaction.address = base + 0x100;
            transaction.addressing =
                memblock::VectorAddressingMode::indexed_unordered;
            transaction.eew = 0;
            transaction.vsew = 0;
            transaction.vl = 32;
            transaction.rob = 20;
            transaction.lq = static_cast<std::uint8_t>(uop * flow_num);
            transaction.pdest = static_cast<std::uint8_t>(80 + uop);
            transaction.lane = uop;
            transaction.flow_num = flow_num;
            transaction.vlmul = 1;
            transaction.vuop_idx = static_cast<std::uint8_t>(uop);
            transaction.last_uop = uop + 1 == uop_count;
            for (unsigned element = 0; element < flow_num; ++element) {
                transaction.index[element] = static_cast<unsigned char>(
                    2U * (uop * flow_num + element));
            }
            environment.expect_vector(transaction);
            if (!environment.enqueue_vector(transaction)) {
                std::cerr << "MEMBLOCK_VECTOR_ISSUE_ORDER_FAIL order="
                          << (reverse ? "reverse" : "forward")
                          << " uop=" << uop
                          << " cycle=" << environment.cycle()
                          << " phase=enqueue reason=" << environment.error()
                          << '\n';
                return 1;
            }
        }

        for (unsigned offset = 0; offset < uop_count; ++offset) {
            const unsigned uop = reverse ? uop_count - offset - 1U : offset;
            if (!environment.issue_vector(uops[uop], 1024)) {
                std::cerr << "MEMBLOCK_VECTOR_ISSUE_ORDER_FAIL order="
                          << (reverse ? "reverse" : "forward")
                          << " uop=" << uop
                          << " cycle=" << environment.cycle()
                          << " phase=issue reason=" << environment.error()
                          << '\n';
                return 1;
            }
        }
        const std::vector<memblock::VectorMemoryTransaction> transactions(
            uops.begin(), uops.end());
        if (!environment.run_until_vector_complete_with_replays(
                transactions, 16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_VECTOR_ISSUE_ORDER_FAIL order="
                      << (reverse ? "reverse" : "forward")
                      << " cycle=" << environment.cycle()
                      << " phase=complete reason=" << environment.error()
                      << '\n';
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        environment.memory().fill_incrementing(base, 0x1000, 0x5b);
        environment.configure_backpressure(
            0xbb67ae8584caa73bULL, false);
        if (!environment.reset() ||
            !environment.set_rob_head(24, false)) {
            std::cerr << "MEMBLOCK_VECTOR_ISSUE_ORDER_FAIL"
                      << " case=masked-indexed-ordered"
                      << " cycle=" << environment.cycle()
                      << " phase=reset reason=" << environment.error() << '\n';
            return 1;
        }

        constexpr unsigned ordered_uop_count = 8;
        constexpr unsigned ordered_flow_num = 2;
        std::array<memblock::VectorMemoryTransaction, ordered_uop_count> uops{};
        for (unsigned uop = 0; uop < ordered_uop_count; ++uop) {
            auto &transaction = uops[uop];
            transaction.address = base + 0x300;
            transaction.addressing =
                memblock::VectorAddressingMode::indexed_ordered;
            transaction.eew = 2;
            transaction.vsew = 3;
            transaction.vl = 16;
            transaction.vm = false;
            transaction.mask_bits = 0xc678;
            transaction.vma = true;
            transaction.rob = 24;
            transaction.lq = static_cast<std::uint8_t>(
                uop * ordered_flow_num);
            transaction.pdest = static_cast<std::uint8_t>(96 + uop);
            transaction.lane = uop % memblock::kVectorMemoryLanes;
            transaction.flow_num = ordered_flow_num;
            transaction.vlmul = 3;
            transaction.vuop_idx = static_cast<std::uint8_t>(uop);
            transaction.last_uop = uop + 1 == ordered_uop_count;
            const unsigned index_register_base = (uop / 2U) * 4U;
            for (unsigned element = 0; element < 4; ++element) {
                const std::uint32_t index =
                    16U * (index_register_base + element);
                for (unsigned byte = 0; byte < 4; ++byte) {
                    transaction.index[element * 4 + byte] =
                        static_cast<unsigned char>(index >> (8U * byte));
                }
            }
            environment.expect_vector(transaction);
            if (!environment.enqueue_vector(transaction)) {
                std::cerr << "MEMBLOCK_VECTOR_ISSUE_ORDER_FAIL"
                          << " case=masked-indexed-ordered"
                          << " uop=" << uop
                          << " cycle=" << environment.cycle()
                          << " phase=enqueue reason=" << environment.error()
                          << '\n';
                return 1;
            }
        }
        for (unsigned uop = 0; uop < ordered_uop_count; ++uop) {
            if (!environment.issue_vector(uops[uop], 1024)) {
                std::cerr << "MEMBLOCK_VECTOR_ISSUE_ORDER_FAIL"
                          << " case=masked-indexed-ordered"
                          << " uop=" << uop
                          << " cycle=" << environment.cycle()
                          << " phase=issue reason=" << environment.error()
                          << '\n';
                return 1;
            }
        }
        const std::vector<memblock::VectorMemoryTransaction> transactions(
            uops.begin(), uops.end());
        if (!environment.run_until_vector_complete_with_replays(
                transactions, 16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_VECTOR_ISSUE_ORDER_FAIL"
                      << " case=masked-indexed-ordered"
                      << " cycle=" << environment.cycle()
                      << " phase=complete reason=" << environment.error()
                      << '\n';
            return 1;
        }
    }

    std::cout << "MEMBLOCK_VECTOR_ISSUE_ORDER_PASS cases=3 uops=12"
              << " addressing=indexed-unordered,indexed-ordered"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_addressing(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x60000;
    environment.memory().fill_incrementing(base, 0x4000, 0x19);
    environment.configure_backpressure(0x243f6a8885a308d3ULL, true);
    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::array<memblock::VectorMemoryTransaction, 6> loads{};
    loads[0] = memblock::VectorMemoryTransaction{
        .address = base + 0x100,
        .stride = 6,
        .addressing = memblock::VectorAddressingMode::strided,
        .eew = 1,
        .vl = 8,
        .rob = 0,
        .lq = 0,
        .pdest = 70,
        .lane = 0,
        .flow_num = 8,
    };
    loads[1] = memblock::VectorMemoryTransaction{
        .address = base + 0x400,
        .addressing = memblock::VectorAddressingMode::indexed_unordered,
        .eew = 0,
        .vl = 16,
        .rob = 1,
        .lq = 8,
        .pdest = 71,
        .lane = 1,
        .flow_num = 16,
    };
    loads[2] = memblock::VectorMemoryTransaction{
        .address = base + 0x800,
        .addressing = memblock::VectorAddressingMode::indexed_ordered,
        .eew = 2,
        .vl = 4,
        .rob = 2,
        .lq = 24,
        .pdest = 72,
        .lane = 0,
        .flow_num = 4,
    };
    for (unsigned index = 3; index < loads.size(); ++index) {
        loads[index] = memblock::VectorMemoryTransaction{
            .address = base + (index == 3 ? 0x1800 : 0x1803),
            .addressing = memblock::VectorAddressingMode::indexed_unordered,
            .eew = 3,
            .vl = 2,
            .vstart = 1,
            .vm = false,
            .mask_bits = 0x2,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(28 + 2 * (index - 3)),
            .pdest = static_cast<std::uint8_t>(73 + index - 3),
            .lane = 1,
            .flow_num = 2,
        };
    }
    for (unsigned element = 0; element < 16; ++element) {
        loads[1].index[element] = static_cast<unsigned char>((element * 13) & 63);
    }
    const std::array<std::uint32_t, 4> word_indices{{52, 4, 36, 20}};
    for (unsigned element = 0; element < word_indices.size(); ++element) {
        for (unsigned byte = 0; byte < 4; ++byte) {
            loads[2].index[element * 4 + byte] = static_cast<unsigned char>(
                word_indices[element] >> (8 * byte));
        }
    }
    for (unsigned index = 3; index < loads.size(); ++index) {
        loads[index].index[0] = 0xc8;
        loads[index].index[8] = index == 4 ? 0xa0 : 0xa8;
    }
    for (std::size_t index = 0; index < loads.size(); ++index) {
        auto &transaction = loads[index];
        for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
            transaction.data[byte] = static_cast<unsigned char>(0xd0 + byte);
        }
        environment.expect_vector(transaction);
        if ((!transaction.store &&
             !environment.set_rob_head(
                 transaction.rob, transaction.rob_flag)) ||
            !environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction, 512) ||
            !environment.run_until_vector_complete(8192) ||
            !environment.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=load reason=" << environment.error() << '\n';
            return 1;
        }
    }

    std::array<memblock::VectorMemoryTransaction, 3> stores{};
    stores[0] = memblock::VectorMemoryTransaction{
        .store = true, .address = base + 0xc00, .stride = 5,
        .addressing = memblock::VectorAddressingMode::strided, .eew = 0,
        .vl = 16, .rob = 6, .lq = 34, .sq = 0, .lane = 1, .flow_num = 16,
    };
    stores[1] = memblock::VectorMemoryTransaction{
        .store = true, .address = base + 0x1000,
        .addressing = memblock::VectorAddressingMode::indexed_unordered, .eew = 1,
        .vl = 8, .rob = 8, .lq = 50, .sq = 16, .lane = 0, .flow_num = 8,
    };
    stores[2] = memblock::VectorMemoryTransaction{
        .store = true, .address = base + 0x1400,
        .addressing = memblock::VectorAddressingMode::indexed_ordered, .eew = 2,
        .vl = 4, .rob = 10, .lq = 58, .sq = 24, .lane = 1, .flow_num = 4,
    };
    for (std::size_t store_index = 0; store_index < stores.size(); ++store_index) {
        auto &store = stores[store_index];
        const unsigned element_bytes = 1U << store.eew;
        const unsigned elements = 16U >> store.eew;
        for (unsigned byte = 0; byte < store.data.size(); ++byte) {
            store.data[byte] = static_cast<unsigned char>(
                0x40 + store_index * 0x30 + byte * 3);
        }
        if (store.addressing != memblock::VectorAddressingMode::strided) {
            for (unsigned element = 0; element < elements; ++element) {
                const std::uint64_t offset =
                    ((element * 3 + store_index) % elements) * element_bytes;
                for (unsigned byte = 0; byte < element_bytes; ++byte) {
                    store.index[element * element_bytes + byte] =
                        static_cast<unsigned char>(offset >> (8 * byte));
                }
            }
        }
        environment.expect_vector(store);
        if (!environment.enqueue_vector(store) ||
            !environment.issue_vector(store, 512) ||
            !environment.run_until_vector_complete(8192) ||
            !environment.commit_vector_store(store, 8192)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL transaction="
                      << store_index << " cycle=" << environment.cycle()
                      << " phase=store reason=" << environment.error() << '\n';
            return 1;
        }
        auto verify = store;
        verify.store = false;
        verify.rob = static_cast<std::uint8_t>(store.rob + 1);
        verify.lq = store.lq;
        verify.sq = static_cast<std::uint8_t>(store.sq + store.flow_num);
        verify.pdest = static_cast<std::uint8_t>(73 + store_index);
        verify.lane = (store.lane + 1) % memblock::kVectorMemoryLanes;
        verify.data.fill(0);
        environment.expect_vector_data(verify, store.data);
        if (!environment.enqueue_vector(verify) ||
            !environment.issue_vector(verify, 512) ||
            !environment.run_until_vector_complete(8192) ||
            !environment.run_until_queues_retired(4096)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL transaction="
                      << store_index << " cycle=" << environment.cycle()
                      << " phase=store-readback reason=" << environment.error() << '\n';
            return 1;
        }
    }

    memblock::Environment multi_uop(argc, argv);
    constexpr std::uint64_t multi_uop_base =
        memblock::kDefaultMemoryBase + 0x70000;
    multi_uop.memory().fill_incrementing(multi_uop_base, 0x4000, 0x61);
    multi_uop.configure_backpressure(0x13198a2e03707344ULL, true);
    if (!multi_uop.reset() || !multi_uop.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << multi_uop.cycle()
                  << " phase=multi-uop-reset reason="
                  << multi_uop.error() << '\n';
        return 1;
    }

    std::array<std::array<memblock::VectorMemoryTransaction, 2>, 3>
        multi_uop_loads{};
    multi_uop_loads[0][0] = memblock::VectorMemoryTransaction{
        .address = multi_uop_base + 0x100,
        .addressing = memblock::VectorAddressingMode::unit_stride,
        .eew = 3,
        .vl = 4,
        .vm = false,
        .mask_bits = 0xb,
        .rob = 40,
        .lq = 0,
        .pdest = 80,
        .lane = 0,
        .flow_num = 2,
        .vlmul = 1,
        .vuop_idx = 0,
        .last_uop = false,
    };
    multi_uop_loads[1][0] = memblock::VectorMemoryTransaction{
        .address = multi_uop_base + 0x800,
        .stride = -12,
        .addressing = memblock::VectorAddressingMode::strided,
        .eew = 2,
        .vl = 7,
        .rob = 41,
        .lq = 4,
        .pdest = 82,
        .lane = 1,
        .flow_num = 4,
        .vlmul = 1,
        .vuop_idx = 0,
        .last_uop = false,
    };
    multi_uop_loads[2][0] = memblock::VectorMemoryTransaction{
        .address = multi_uop_base + 0x1000,
        .addressing = memblock::VectorAddressingMode::indexed_ordered,
        .eew = 1,
        .vl = 16,
        .vm = false,
        .mask_bits = 0xa55a,
        .rob = 42,
        .lq = 12,
        .pdest = 84,
        .lane = 0,
        .flow_num = 8,
        .vlmul = 1,
        .vuop_idx = 0,
        .last_uop = false,
    };
    constexpr std::array<std::uint16_t, 8> first_indices{
        28, 4, 40, 16, 52, 0, 36, 12,
    };
    constexpr std::array<std::uint16_t, 8> second_indices{
        156, 132, 168, 144, 180, 128, 164, 140,
    };
    for (unsigned element = 0; element < first_indices.size(); ++element) {
        for (unsigned byte = 0; byte < 2; ++byte) {
            multi_uop_loads[2][0].index[element * 2 + byte] =
                static_cast<unsigned char>(first_indices[element] >> (8 * byte));
        }
    }

    for (unsigned mode = 0; mode < multi_uop_loads.size(); ++mode) {
        auto &older = multi_uop_loads[mode][0];
        auto &younger = multi_uop_loads[mode][1];
        younger = older;
        younger.lq = static_cast<std::uint8_t>(older.lq + older.flow_num);
        younger.pdest = static_cast<std::uint8_t>(older.pdest + 1);
        younger.lane = mode == 2
            ? older.lane
            : (older.lane + 1) % memblock::kVectorMemoryLanes;
        younger.vuop_idx = 1;
        younger.last_uop = true;
        if (mode == 2) {
            for (unsigned element = 0; element < second_indices.size(); ++element) {
                for (unsigned byte = 0; byte < 2; ++byte) {
                    younger.index[element * 2 + byte] =
                        static_cast<unsigned char>(
                            second_indices[element] >> (8 * byte));
                }
            }
        }

        multi_uop.expect_vector(older);
        multi_uop.expect_vector(younger);
        const auto &first_issue = mode == 2 ? older : younger;
        const auto &second_issue = mode == 2 ? younger : older;
        if (!multi_uop.set_rob_head(older.rob, older.rob_flag) ||
            !multi_uop.enqueue_vector(older) ||
            !multi_uop.enqueue_vector(younger) ||
            !multi_uop.issue_vector(first_issue, 512) ||
            !multi_uop.issue_vector(second_issue, 512) ||
            !multi_uop.run_until_vector_complete_with_replays(
                {older, younger}, 16384) ||
            !multi_uop.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL mode=" << mode
                      << " cycle=" << multi_uop.cycle()
                      << " phase=multi-uop-load reason="
                      << multi_uop.error() << '\n';
            return 1;
        }
    }

    memblock::Environment lmul_matrix(argc, argv);
    constexpr std::uint64_t lmul_base =
        memblock::kDefaultMemoryBase + 0xe0000;
    lmul_matrix.memory().fill_incrementing(lmul_base, 0x20000, 0x5d);
    lmul_matrix.configure_backpressure(0xbe5466cf34e90c6cULL, true);
    if (!lmul_matrix.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << lmul_matrix.cycle()
                  << " phase=lmul-emul-reset reason="
                  << lmul_matrix.error() << '\n';
        return 1;
    }

    std::uint64_t lmul_lq_cursor = 0;
    std::uint64_t lmul_sq_cursor = 0;
    std::uint64_t lmul_rob_cursor = 0;
    unsigned lmul_rob_wraps = 0;
    unsigned lmul_configurations = 0;
    unsigned lmul_load_uops = 0;
    unsigned lmul_store_uops = 0;
    auto next_lmul_rob = [&]() {
        const std::uint64_t absolute_rob = lmul_rob_cursor++;
        lmul_rob_wraps += absolute_rob != 0 &&
            absolute_rob % memblock::kRobEntries == 0;
        return absolute_rob;
    };
    auto make_lmul_uops = [&] (
        bool store, std::uint64_t address, std::uint64_t absolute_rob,
        std::uint8_t eew, std::uint8_t vsew, int lmul_log2, int emul_log2,
        memblock::VectorAddressingMode addressing, std::int64_t stride,
        unsigned flow_num, std::uint8_t vl, std::uint64_t &queue_cursor,
        std::uint8_t data_seed) {
        const unsigned uop_count = 1U << static_cast<unsigned>(
            std::max(emul_log2, 0));
        const std::uint8_t vlmul = static_cast<std::uint8_t>(
            lmul_log2 < 0 ? lmul_log2 + 8 : lmul_log2);
        std::vector<memblock::VectorMemoryTransaction> uops;
        uops.reserve(uop_count);
        for (unsigned uop = 0; uop < uop_count; ++uop) {
            memblock::VectorMemoryTransaction transaction{
                .store = store,
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
                .lq = static_cast<std::uint8_t>(store ? 0 :
                    queue_cursor % memblock::kVirtualLoadQueueEntries),
                .lq_flag = !store && ((queue_cursor /
                    memblock::kVirtualLoadQueueEntries) & 1U) != 0,
                .sq = static_cast<std::uint8_t>(store ?
                    queue_cursor % memblock::kStoreQueueEntries : 0),
                .sq_flag = store && ((queue_cursor /
                    memblock::kStoreQueueEntries) & 1U) != 0,
                .pdest = static_cast<std::uint8_t>(64 + uop),
                .lane = uop % memblock::kVectorMemoryLanes,
                .flow_num = static_cast<std::uint8_t>(flow_num),
                .vlmul = vlmul,
                .vuop_idx = static_cast<std::uint8_t>(uop),
                .last_uop = uop + 1 == uop_count,
            };
            for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
                transaction.data[byte] = static_cast<unsigned char>(
                    data_seed + uop * 23 + byte * 11);
            }
            uops.push_back(transaction);
            queue_cursor += flow_num;
        }
        return uops;
    };
    auto run_matrix_load = [&] (
        memblock::Environment &matrix,
        const std::vector<memblock::VectorMemoryTransaction> &uops,
        const char *phase, unsigned &uop_counter, bool reverse_issue) {
        std::size_t begin = 0;
        while (begin < uops.size()) {
            std::size_t end = begin;
            unsigned window_flows = 0;
            while (end < uops.size() &&
                   window_flows + uops[end].flow_num <=
                       memblock::kVirtualLoadQueueEntries -
                           memblock::kLqEnqueueHeadroom) {
                window_flows += uops[end].flow_num;
                ++end;
            }
            const std::vector<memblock::VectorMemoryTransaction> window(
                uops.begin() + begin, uops.begin() + end);
            if (!matrix.set_rob_head(
                    window.front().rob, window.front().rob_flag)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << matrix.cycle() << " phase=" << phase
                          << " reason=" << matrix.error() << '\n';
                return false;
            }
            for (const auto &uop : window) {
                matrix.expect_vector(uop);
                if (!matrix.enqueue_vector(uop)) {
                    std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                              << matrix.cycle() << " phase=" << phase
                              << " reason=" << matrix.error() << '\n';
                    return false;
                }
            }
            for (std::size_t offset = 0; offset < window.size(); ++offset) {
                const std::size_t issue_index = reverse_issue
                    ? window.size() - offset - 1
                    : offset;
                if (!matrix.issue_vector(window[issue_index], 1024)) {
                    std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                              << matrix.cycle() << " phase=" << phase
                              << " reason=" << matrix.error() << '\n';
                    return false;
                }
            }
            if (!matrix.run_until_vector_complete_with_replays(
                    window, 32768) ||
                !matrix.run_until_lq_retired(8192)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << matrix.cycle() << " phase=" << phase
                          << " reason=" << matrix.error() << '\n';
                return false;
            }
            begin = end;
        }
        uop_counter += uops.size();
        return true;
    };
    auto run_matrix_store = [&] (
        memblock::Environment &matrix,
        const std::vector<memblock::VectorMemoryTransaction> &uops,
        const char *phase, unsigned &uop_counter, bool reverse_issue) {
        std::size_t begin = 0;
        while (begin < uops.size()) {
            std::size_t end = begin;
            unsigned window_flows = 0;
            while (end < uops.size() &&
                   window_flows + uops[end].flow_num <=
                       memblock::kStoreQueueEntries -
                           memblock::kSqEnqueueHeadroom) {
                window_flows += uops[end].flow_num;
                ++end;
            }
            const std::vector<memblock::VectorMemoryTransaction> window(
                uops.begin() + begin, uops.begin() + end);
            for (const auto &store : window) {
                matrix.expect_vector(store);
                if (!matrix.enqueue_vector(store)) {
                    std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                              << matrix.cycle() << " phase=" << phase
                              << " reason=" << matrix.error() << '\n';
                    return false;
                }
            }
            for (std::size_t offset = 0; offset < window.size(); ++offset) {
                const std::size_t issue_index = reverse_issue
                    ? window.size() - offset - 1
                    : offset;
                if (!matrix.issue_vector(window[issue_index], 1024)) {
                    std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                              << matrix.cycle() << " phase=" << phase
                              << " reason=" << matrix.error() << '\n';
                    return false;
                }
            }
            if (!matrix.run_until_vector_complete_with_replays(
                    window, 32768)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << matrix.cycle() << " phase=" << phase
                          << " reason=" << matrix.error() << '\n';
                return false;
            }
            for (const auto &store : window) {
                if (!matrix.commit_vector_store(store, 8192)) {
                    std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                              << matrix.cycle() << " phase=" << phase
                              << " reason=" << matrix.error() << '\n';
                    return false;
                }
            }
            if (!matrix.run_until_queues_retired(8192)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << matrix.cycle() << " phase=" << phase
                          << " reason=" << matrix.error() << '\n';
                return false;
            }
            begin = end;
        }
        if (!matrix.pulse_sbuffer_flush() ||
            !matrix.run_until_sbuffer_empty(32768)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                      << matrix.cycle() << " phase=" << phase
                      << " reason=" << matrix.error() << '\n';
            return false;
        }
        uop_counter += uops.size();
        return true;
    };

    for (std::uint8_t eew = 0; eew < 4; ++eew) {
        for (std::uint8_t vsew = 0; vsew < 4; ++vsew) {
            for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                const int emul_log2 = static_cast<int>(eew) -
                    static_cast<int>(vsew) + lmul_log2;
                // ELEN=64 requires LMUL >= SEW/ELEN. EMUL must remain in
                // the architectural fractional-1/8 through 8 range.
                if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                    emul_log2 < -3 || emul_log2 > 3) {
                    continue;
                }
                const unsigned vector_bytes = lmul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-lmul_log2)
                    : 16U << static_cast<unsigned>(lmul_log2);
                const unsigned vlmax = vector_bytes >> vsew;
                const unsigned case_index = lmul_configurations++;
                const std::uint64_t source =
                    lmul_base + case_index * 0x400;
                const std::uint64_t destination = source + 0x200;

                auto loads = make_lmul_uops(
                    false, source, next_lmul_rob(), eew, vsew,
                    lmul_log2, emul_log2,
                    memblock::VectorAddressingMode::unit_stride, 0,
                    memblock::kVectorUnitStrideMaxFlows,
                    static_cast<std::uint8_t>(vlmax), lmul_lq_cursor,
                    static_cast<std::uint8_t>(0x21 + case_index));
                if (!run_matrix_load(
                        lmul_matrix, loads, "lmul-emul-load",
                        lmul_load_uops, true)) {
                    return 1;
                }

                auto stores = make_lmul_uops(
                    true, destination, next_lmul_rob(), eew, vsew,
                    lmul_log2, emul_log2,
                    memblock::VectorAddressingMode::unit_stride, 0,
                    memblock::kVectorUnitStrideMaxFlows,
                    static_cast<std::uint8_t>(vlmax), lmul_sq_cursor,
                    static_cast<std::uint8_t>(0x81 + case_index));
                if (!run_matrix_store(
                        lmul_matrix, stores, "lmul-emul-store",
                        lmul_store_uops, true)) {
                    return 1;
                }

                auto readbacks = make_lmul_uops(
                    false, destination, next_lmul_rob(), eew, vsew,
                    lmul_log2, emul_log2,
                    memblock::VectorAddressingMode::unit_stride, 0,
                    memblock::kVectorUnitStrideMaxFlows,
                    static_cast<std::uint8_t>(vlmax), lmul_lq_cursor,
                    static_cast<std::uint8_t>(0xe1 + case_index));
                if (!run_matrix_load(
                        lmul_matrix, readbacks, "lmul-emul-readback",
                        lmul_load_uops, true)) {
                    return 1;
                }
            }
        }
    }
    if (lmul_configurations != 78 || lmul_load_uops != 404 ||
        lmul_store_uops != 202 || lmul_rob_wraps != 1 ||
        lmul_matrix.lq_allocated() != 808 ||
        lmul_matrix.sq_allocated() != 404 ||
        lmul_matrix.lq_allocated() !=
            lmul_matrix.lq_dequeued() + lmul_matrix.lq_canceled() ||
        lmul_matrix.sq_allocated() !=
            lmul_matrix.sq_dequeued() + lmul_matrix.sq_canceled()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << lmul_matrix.cycle()
                  << " phase=lmul-emul-conservation"
                  << " configurations=" << lmul_configurations
                  << " load_uops=" << lmul_load_uops
                  << " store_uops=" << lmul_store_uops
                  << " rob_wraps=" << lmul_rob_wraps
                  << " lq=" << lmul_matrix.lq_allocated() << '/'
                  << lmul_matrix.lq_dequeued() << '+'
                  << lmul_matrix.lq_canceled()
                  << " sq=" << lmul_matrix.sq_allocated() << '/'
                  << lmul_matrix.sq_dequeued() << '+'
                  << lmul_matrix.sq_canceled() << '\n';
        return 1;
    }

    memblock::Environment strided_matrix(argc, argv);
    constexpr std::uint64_t strided_matrix_base =
        memblock::kDefaultMemoryBase + 0x120000;
    strided_matrix.memory().fill_incrementing(
        strided_matrix_base, 0x80000, 0x39);
    strided_matrix.configure_backpressure(0x94d049bb133111ebULL, true);
    if (!strided_matrix.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << strided_matrix.cycle()
                  << " phase=strided-lmul-emul-reset reason="
                  << strided_matrix.error() << '\n';
        return 1;
    }

    std::uint64_t strided_lq_cursor = 0;
    std::uint64_t strided_sq_cursor = 0;
    std::uint64_t strided_rob_cursor = 0;
    unsigned strided_configurations = 0;
    unsigned strided_load_uops = 0;
    unsigned strided_store_uops = 0;
    unsigned strided_positive = 0;
    unsigned strided_negative = 0;
    auto next_strided_rob = [&]() { return strided_rob_cursor++; };
    for (std::uint8_t eew = 0; eew < 4; ++eew) {
        for (std::uint8_t vsew = 0; vsew < 4; ++vsew) {
            for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                const int emul_log2 = static_cast<int>(eew) -
                    static_cast<int>(vsew) + lmul_log2;
                if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                    emul_log2 < -3 || emul_log2 > 3) {
                    continue;
                }
                const unsigned vector_bytes = lmul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-lmul_log2)
                    : 16U << static_cast<unsigned>(lmul_log2);
                const unsigned vlmax = vector_bytes >> vsew;
                const unsigned bytes_per_uop = emul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-emul_log2)
                    : 16U;
                const unsigned flow_num = bytes_per_uop >> eew;
                const unsigned case_index = strided_configurations++;
                const std::uint64_t case_base =
                    strided_matrix_base + case_index * 0x1000;
                const std::int64_t stride_magnitude =
                    std::int64_t{2} << eew;
                const std::int64_t stride = (case_index & 1U) == 0
                    ? stride_magnitude
                    : -stride_magnitude;
                const std::uint64_t source = case_base + 0x400;
                const std::uint64_t destination = case_base + 0xc00;
                strided_positive += stride > 0;
                strided_negative += stride < 0;

                auto loads = make_lmul_uops(
                    false, source, next_strided_rob(), eew, vsew,
                    lmul_log2, emul_log2,
                    memblock::VectorAddressingMode::strided, stride, flow_num,
                    static_cast<std::uint8_t>(vlmax), strided_lq_cursor,
                    static_cast<std::uint8_t>(0x43 + case_index));
                if (!run_matrix_load(
                        strided_matrix, loads, "strided-lmul-emul-load",
                        strided_load_uops, true)) {
                    return 1;
                }

                auto stores = make_lmul_uops(
                    true, destination, next_strided_rob(), eew, vsew,
                    lmul_log2, emul_log2,
                    memblock::VectorAddressingMode::strided, stride, flow_num,
                    static_cast<std::uint8_t>(vlmax), strided_sq_cursor,
                    static_cast<std::uint8_t>(0xa3 + case_index));
                if (!run_matrix_store(
                        strided_matrix, stores, "strided-lmul-emul-store",
                        strided_store_uops, true)) {
                    return 1;
                }

                auto readbacks = make_lmul_uops(
                    false, destination, next_strided_rob(), eew, vsew,
                    lmul_log2, emul_log2,
                    memblock::VectorAddressingMode::strided, stride, flow_num,
                    static_cast<std::uint8_t>(vlmax), strided_lq_cursor,
                    static_cast<std::uint8_t>(0xc3 + case_index));
                if (!run_matrix_load(
                        strided_matrix, readbacks,
                        "strided-lmul-emul-readback", strided_load_uops,
                        true)) {
                    return 1;
                }
            }
        }
    }
    if (strided_configurations != 78 || strided_load_uops != 404 ||
        strided_store_uops != 202 || strided_positive != 39 ||
        strided_negative != 39 || strided_matrix.lq_allocated() != 2304 ||
        strided_matrix.sq_allocated() != 1152 ||
        strided_matrix.lq_allocated() !=
            strided_matrix.lq_dequeued() + strided_matrix.lq_canceled() ||
        strided_matrix.sq_allocated() !=
            strided_matrix.sq_dequeued() + strided_matrix.sq_canceled()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << strided_matrix.cycle()
                  << " phase=strided-lmul-emul-conservation"
                  << " configurations=" << strided_configurations
                  << " load_uops=" << strided_load_uops
                  << " store_uops=" << strided_store_uops
                  << " positive/negative=" << strided_positive << '/'
                  << strided_negative
                  << " lq=" << strided_matrix.lq_allocated() << '/'
                  << strided_matrix.lq_dequeued() << '+'
                  << strided_matrix.lq_canceled()
                  << " sq=" << strided_matrix.sq_allocated() << '/'
                  << strided_matrix.sq_dequeued() << '+'
                  << strided_matrix.sq_canceled() << '\n';
        return 1;
    }

    memblock::Environment indexed_matrix(argc, argv);
    constexpr std::uint64_t indexed_base =
        memblock::kDefaultMemoryBase + 0xb0000;
    indexed_matrix.memory().fill_incrementing(indexed_base, 0x20000, 0x93);
    indexed_matrix.configure_backpressure(0x452821e638d01377ULL, true);
    if (!indexed_matrix.reset() ||
        !indexed_matrix.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << indexed_matrix.cycle()
                  << " phase=indexed-matrix-reset reason="
                  << indexed_matrix.error() << '\n';
        return 1;
    }

    constexpr std::array<memblock::VectorAddressingMode, 2> indexed_modes{{
        memblock::VectorAddressingMode::indexed_unordered,
        memblock::VectorAddressingMode::indexed_ordered,
    }};
    std::uint64_t indexed_lq_cursor = 0;
    std::uint64_t indexed_sq_cursor = 0;
    std::uint8_t indexed_rob = 0;
    unsigned indexed_cases = 0;
    unsigned indexed_load_uops = 0;
    unsigned indexed_store_uops = 0;
    auto fill_indices = [](memblock::VectorMemoryTransaction &transaction,
                           bool alias_last) {
        const unsigned element_bytes = 1U << transaction.eew;
        const unsigned elements = 16U >> transaction.eew;
        const unsigned first_slot = 3U % elements;
        for (unsigned element = 0; element < elements; ++element) {
            const unsigned slot = alias_last && element + 1 == elements
                ? first_slot
                : (element * 5U + 3U) % elements;
            const std::uint64_t offset =
                static_cast<std::uint64_t>(slot) * element_bytes * 8U;
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                transaction.index[element * element_bytes + byte] =
                    static_cast<unsigned char>(offset >> (8 * byte));
            }
        }
    };
    for (const auto mode : indexed_modes) {
        for (std::uint8_t eew = 0; eew < 4; ++eew) {
            const unsigned elements = 16U >> eew;
            const std::uint64_t case_base =
                indexed_base + indexed_cases * 0x3000;
            memblock::VectorMemoryTransaction load{
                .address = case_base + 0xfc0,
                .addressing = mode,
                .eew = eew,
                .vl = static_cast<std::uint8_t>(elements),
                .rob = indexed_rob++,
                .lq = static_cast<std::uint8_t>(indexed_lq_cursor %
                    memblock::kVirtualLoadQueueEntries),
                .lq_flag = ((indexed_lq_cursor /
                    memblock::kVirtualLoadQueueEntries) & 1U) != 0,
                .pdest = static_cast<std::uint8_t>(96 + indexed_cases),
                .lane = indexed_cases % memblock::kVectorMemoryLanes,
                .flow_num = static_cast<std::uint8_t>(elements),
            };
            fill_indices(load, true);
            indexed_matrix.expect_vector(load);
            if (!indexed_matrix.set_rob_head(load.rob, load.rob_flag) ||
                !indexed_matrix.enqueue_vector(load) ||
                !indexed_matrix.issue_vector(load, 1024) ||
                !indexed_matrix.run_until_vector_complete_with_replays(
                    load, 32768) ||
                !indexed_matrix.run_until_lq_retired(8192)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << indexed_matrix.cycle()
                          << " phase=indexed-matrix-load mode="
                          << static_cast<unsigned>(mode)
                          << " eew=" << static_cast<unsigned>(eew)
                          << " reason=" << indexed_matrix.error() << '\n';
                return 1;
            }
            indexed_lq_cursor += elements;
            ++indexed_load_uops;

            memblock::VectorMemoryTransaction store{
                .store = true,
                .address = case_base + 0x1fc0,
                .addressing = mode,
                .eew = eew,
                .vl = static_cast<std::uint8_t>(elements),
                .rob = indexed_rob++,
                .sq = static_cast<std::uint8_t>(indexed_sq_cursor %
                    memblock::kStoreQueueEntries),
                .sq_flag = ((indexed_sq_cursor /
                    memblock::kStoreQueueEntries) & 1U) != 0,
                .lane = (indexed_cases + 1) % memblock::kVectorMemoryLanes,
                .flow_num = static_cast<std::uint8_t>(elements),
            };
            fill_indices(store, false);
            for (unsigned byte = 0; byte < store.data.size(); ++byte) {
                store.data[byte] = static_cast<unsigned char>(
                    0x31 + indexed_cases * 19 + byte * 7);
            }
            indexed_matrix.expect_vector(store);
            if (!indexed_matrix.enqueue_vector(store) ||
                !indexed_matrix.issue_vector(store, 1024) ||
                !indexed_matrix.run_until_vector_complete_with_replays(
                    store, 32768) ||
                !indexed_matrix.commit_vector_store(store, 8192) ||
                !indexed_matrix.run_until_queues_retired(8192) ||
                !indexed_matrix.pulse_sbuffer_flush() ||
                !indexed_matrix.run_until_sbuffer_empty(32768)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << indexed_matrix.cycle()
                          << " phase=indexed-matrix-store mode="
                          << static_cast<unsigned>(mode)
                          << " eew=" << static_cast<unsigned>(eew)
                          << " reason=" << indexed_matrix.error() << '\n';
                return 1;
            }
            indexed_sq_cursor += elements;
            ++indexed_store_uops;

            auto readback = store;
            readback.store = false;
            readback.rob = indexed_rob++;
            readback.lq = static_cast<std::uint8_t>(indexed_lq_cursor %
                memblock::kVirtualLoadQueueEntries);
            readback.lq_flag = ((indexed_lq_cursor /
                memblock::kVirtualLoadQueueEntries) & 1U) != 0;
            readback.sq = static_cast<std::uint8_t>(indexed_sq_cursor %
                memblock::kStoreQueueEntries);
            readback.sq_flag = ((indexed_sq_cursor /
                memblock::kStoreQueueEntries) & 1U) != 0;
            readback.pdest = static_cast<std::uint8_t>(112 + indexed_cases);
            readback.lane = indexed_cases % memblock::kVectorMemoryLanes;
            readback.data.fill(0);
            indexed_matrix.expect_vector_data(readback, store.data);
            if (!indexed_matrix.set_rob_head(
                    readback.rob, readback.rob_flag) ||
                !indexed_matrix.enqueue_vector(readback) ||
                !indexed_matrix.issue_vector(readback, 1024) ||
                !indexed_matrix.run_until_vector_complete_with_replays(
                    readback, 32768) ||
                !indexed_matrix.run_until_lq_retired(8192)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << indexed_matrix.cycle()
                          << " phase=indexed-matrix-readback mode="
                          << static_cast<unsigned>(mode)
                          << " eew=" << static_cast<unsigned>(eew)
                          << " reason=" << indexed_matrix.error() << '\n';
                return 1;
            }
            indexed_lq_cursor += elements;
            ++indexed_load_uops;
            ++indexed_cases;
        }
    }
    if (indexed_cases != 8 || indexed_load_uops != 16 ||
        indexed_store_uops != 8 || indexed_matrix.lq_allocated() != 120 ||
        indexed_matrix.sq_allocated() != 60 ||
        indexed_matrix.lq_allocated() !=
            indexed_matrix.lq_dequeued() + indexed_matrix.lq_canceled() ||
        indexed_matrix.sq_allocated() !=
            indexed_matrix.sq_dequeued() + indexed_matrix.sq_canceled()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << indexed_matrix.cycle()
                  << " phase=indexed-matrix-conservation"
                  << " cases=" << indexed_cases
                  << " load_uops=" << indexed_load_uops
                  << " store_uops=" << indexed_store_uops
                  << " lq=" << indexed_matrix.lq_allocated() << '/'
                  << indexed_matrix.lq_dequeued() << '+'
                  << indexed_matrix.lq_canceled()
                  << " sq=" << indexed_matrix.sq_allocated() << '/'
                  << indexed_matrix.sq_dequeued() << '+'
                  << indexed_matrix.sq_canceled() << '\n';
        return 1;
    }

    memblock::Environment indexed_lmul_matrix(argc, argv);
    constexpr std::uint64_t indexed_lmul_base =
        memblock::kDefaultMemoryBase + 0x1b0000;
    indexed_lmul_matrix.memory().fill_incrementing(
        indexed_lmul_base, 0xa0000, 0x6b);
    indexed_lmul_matrix.configure_backpressure(
        0x243f6a8885a308d3ULL, true);
    if (!indexed_lmul_matrix.reset() ||
        !indexed_lmul_matrix.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << indexed_lmul_matrix.cycle()
                  << " phase=indexed-lmul-emul-reset reason="
                  << indexed_lmul_matrix.error() << '\n';
        return 1;
    }

    std::uint64_t indexed_lmul_lq_cursor = 0;
    std::uint64_t indexed_lmul_sq_cursor = 0;
    std::uint64_t indexed_lmul_rob_cursor = 0;
    unsigned indexed_lmul_rob_wraps = 0;
    unsigned indexed_lmul_configurations = 0;
    unsigned indexed_lmul_unordered = 0;
    unsigned indexed_lmul_ordered = 0;
    unsigned indexed_lmul_special = 0;
    unsigned indexed_lmul_load_uops = 0;
    unsigned indexed_lmul_store_uops = 0;
    auto next_indexed_lmul_rob = [&]() {
        const std::uint64_t absolute_rob = indexed_lmul_rob_cursor++;
        indexed_lmul_rob_wraps += absolute_rob != 0 &&
            absolute_rob % memblock::kRobEntries == 0;
        return absolute_rob;
    };
    auto make_indexed_lmul_uops = [&] (
        bool store, std::uint64_t address, std::uint64_t absolute_rob,
        std::uint8_t eew, std::uint8_t vsew, int lmul_log2,
        int emul_log2, memblock::VectorAddressingMode addressing,
        unsigned flow_num, std::uint8_t vl, std::uint64_t &queue_cursor,
        std::uint8_t data_seed) {
        const unsigned uop_count = 1U << static_cast<unsigned>(
            std::max({lmul_log2, emul_log2, 0}));
        const std::uint8_t vlmul = static_cast<std::uint8_t>(
            lmul_log2 < 0 ? lmul_log2 + 8 : lmul_log2);
        const unsigned index_bytes = 1U << eew;
        const unsigned data_bytes = 1U << vsew;
        const unsigned index_group_bytes_per_uop = emul_log2 < 0
            ? 16U >> static_cast<unsigned>(-emul_log2)
            : 16U;
        const unsigned index_elements_per_vreg =
            index_group_bytes_per_uop >> eew;
        const bool special = emul_log2 > lmul_log2;
        std::vector<memblock::VectorMemoryTransaction> uops;
        uops.reserve(uop_count);
        for (unsigned uop = 0; uop < uop_count; ++uop) {
            const unsigned vd_index = special
                ? uop >> static_cast<unsigned>(emul_log2 - lmul_log2)
                : uop;
            memblock::VectorMemoryTransaction transaction{
                .store = store,
                .address = address,
                .addressing = addressing,
                .eew = eew,
                .vsew = vsew,
                .vl = vl,
                .rob = static_cast<std::uint8_t>(absolute_rob %
                    memblock::kRobEntries),
                .rob_flag = ((absolute_rob /
                    memblock::kRobEntries) & 1U) != 0,
                .lq = static_cast<std::uint8_t>(store ? 0 :
                    queue_cursor % memblock::kVirtualLoadQueueEntries),
                .lq_flag = !store && ((queue_cursor /
                    memblock::kVirtualLoadQueueEntries) & 1U) != 0,
                .sq = static_cast<std::uint8_t>(store ?
                    queue_cursor % memblock::kStoreQueueEntries : 0),
                .sq_flag = store && ((queue_cursor /
                    memblock::kStoreQueueEntries) & 1U) != 0,
                .pdest = static_cast<std::uint8_t>(64 + vd_index),
                .lane = uop % memblock::kVectorMemoryLanes,
                .flow_num = static_cast<std::uint8_t>(flow_num),
                .vlmul = vlmul,
                .vuop_idx = static_cast<std::uint8_t>(uop),
                .last_uop = uop + 1 == uop_count,
            };
            for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
                transaction.data[byte] = static_cast<unsigned char>(
                    data_seed + vd_index * 23 + byte * 11);
            }

            const unsigned global_first = uop * flow_num;
            const unsigned index_register_base =
                global_first / index_elements_per_vreg *
                index_elements_per_vreg;
            for (unsigned element = 0;
                 element < index_elements_per_vreg; ++element) {
                const std::uint64_t offset =
                    static_cast<std::uint64_t>(
                        index_register_base + element) * data_bytes * 2U;
                for (unsigned byte = 0; byte < index_bytes; ++byte) {
                    transaction.index[element * index_bytes + byte] =
                        static_cast<unsigned char>(offset >> (8 * byte));
                }
            }
            uops.push_back(transaction);
            queue_cursor += flow_num;
        }
        return uops;
    };

    for (const auto mode : indexed_modes) {
        for (std::uint8_t eew = 0; eew < 4; ++eew) {
            for (std::uint8_t vsew = 0; vsew < 4; ++vsew) {
                for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                    const int emul_log2 = static_cast<int>(eew) -
                        static_cast<int>(vsew) + lmul_log2;
                    if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                        emul_log2 < -3 || emul_log2 > 3) {
                        continue;
                    }
                    const unsigned vector_bytes = lmul_log2 < 0
                        ? 16U >> static_cast<unsigned>(-lmul_log2)
                        : 16U << static_cast<unsigned>(lmul_log2);
                    const unsigned vlmax = vector_bytes >> vsew;
                    const unsigned flow_bytes = emul_log2 > lmul_log2
                        ? (emul_log2 < 0
                            ? 16U >> static_cast<unsigned>(-emul_log2)
                            : 16U)
                        : (lmul_log2 < 0
                            ? 16U >> static_cast<unsigned>(-lmul_log2)
                            : 16U);
                    const unsigned flow_num = flow_bytes >>
                        (emul_log2 > lmul_log2 ? eew : vsew);
                    const unsigned case_index =
                        indexed_lmul_configurations++;
                    indexed_lmul_unordered +=
                        mode == memblock::VectorAddressingMode::indexed_unordered;
                    indexed_lmul_ordered +=
                        mode == memblock::VectorAddressingMode::indexed_ordered;
                    indexed_lmul_special += emul_log2 > lmul_log2;
                    const std::uint64_t case_base =
                        indexed_lmul_base + case_index * 0x1000;
                    const std::uint64_t source = case_base + 0x100;
                    const std::uint64_t destination = case_base + 0x800;
                    const bool reverse_issue = mode ==
                        memblock::VectorAddressingMode::indexed_unordered;

                    auto loads = make_indexed_lmul_uops(
                        false, source, next_indexed_lmul_rob(), eew, vsew,
                        lmul_log2, emul_log2, mode, flow_num,
                        static_cast<std::uint8_t>(vlmax),
                        indexed_lmul_lq_cursor,
                        static_cast<std::uint8_t>(0x17 + case_index));
                    if (!run_matrix_load(
                            indexed_lmul_matrix, loads,
                            "indexed-lmul-emul-load",
                            indexed_lmul_load_uops, reverse_issue)) {
                        return 1;
                    }

                    auto stores = make_indexed_lmul_uops(
                        true, destination, next_indexed_lmul_rob(), eew, vsew,
                        lmul_log2, emul_log2, mode, flow_num,
                        static_cast<std::uint8_t>(vlmax),
                        indexed_lmul_sq_cursor,
                        static_cast<std::uint8_t>(0x57 + case_index));
                    if (!run_matrix_store(
                            indexed_lmul_matrix, stores,
                            "indexed-lmul-emul-store",
                            indexed_lmul_store_uops, reverse_issue)) {
                        return 1;
                    }

                    auto readbacks = make_indexed_lmul_uops(
                        false, destination, next_indexed_lmul_rob(), eew,
                        vsew, lmul_log2, emul_log2, mode, flow_num,
                        static_cast<std::uint8_t>(vlmax),
                        indexed_lmul_lq_cursor,
                        static_cast<std::uint8_t>(0x97 + case_index));
                    if (!run_matrix_load(
                            indexed_lmul_matrix, readbacks,
                            "indexed-lmul-emul-readback",
                            indexed_lmul_load_uops, reverse_issue)) {
                        return 1;
                    }
                }
            }
        }
    }
    if (indexed_lmul_configurations != 156 ||
        indexed_lmul_unordered != 78 || indexed_lmul_ordered != 78 ||
        indexed_lmul_special != 56 || indexed_lmul_load_uops != 1016 ||
        indexed_lmul_store_uops != 508 || indexed_lmul_rob_wraps != 2 ||
        indexed_lmul_matrix.lq_allocated() != 4608 ||
        indexed_lmul_matrix.sq_allocated() != 2304 ||
        indexed_lmul_matrix.lq_allocated() !=
            indexed_lmul_matrix.lq_dequeued() +
                indexed_lmul_matrix.lq_canceled() ||
        indexed_lmul_matrix.sq_allocated() !=
            indexed_lmul_matrix.sq_dequeued() +
                indexed_lmul_matrix.sq_canceled()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << indexed_lmul_matrix.cycle()
                  << " phase=indexed-lmul-emul-conservation"
                  << " configurations=" << indexed_lmul_configurations
                  << " unordered=" << indexed_lmul_unordered
                  << " ordered=" << indexed_lmul_ordered
                  << " special=" << indexed_lmul_special
                  << " load_uops=" << indexed_lmul_load_uops
                  << " store_uops=" << indexed_lmul_store_uops
                  << " rob_wraps=" << indexed_lmul_rob_wraps
                  << " lq=" << indexed_lmul_matrix.lq_allocated() << '/'
                  << indexed_lmul_matrix.lq_dequeued() << '+'
                  << indexed_lmul_matrix.lq_canceled()
                  << " sq=" << indexed_lmul_matrix.sq_allocated() << '/'
                  << indexed_lmul_matrix.sq_dequeued() << '+'
                  << indexed_lmul_matrix.sq_canceled() << '\n';
        return 1;
    }

    memblock::Environment whole(argc, argv);
    constexpr std::uint64_t whole_base =
        memblock::kDefaultMemoryBase + 0x90000;
    whole.memory().fill_incrementing(whole_base, 0x10000, 0x27);
    whole.configure_backpressure(0xa4093822299f31d0ULL, true);
    if (!whole.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << whole.cycle() << " phase=whole-register-reset reason="
                  << whole.error() << '\n';
        return 1;
    }

    constexpr std::array<std::uint8_t, 4> whole_nfs{{0, 1, 3, 7}};
    std::uint64_t whole_lq_cursor = 0;
    std::uint64_t whole_sq_cursor = 0;
    std::uint8_t whole_rob = 80;
    unsigned whole_cases = 0;
    unsigned whole_load_uops = 0;
    unsigned whole_store_uops = 0;
    auto make_whole_uops = [&](bool store, std::uint64_t address,
                               std::uint8_t rob, std::uint8_t nf,
                               std::uint8_t eew, std::uint64_t queue_cursor,
                               std::uint8_t data_seed) {
        std::vector<memblock::VectorMemoryTransaction> uops;
        const unsigned registers = static_cast<unsigned>(nf) + 1U;
        uops.reserve(registers);
        for (unsigned uop = 0; uop < registers; ++uop) {
            memblock::VectorMemoryTransaction transaction{
                .store = store,
                .whole_register = true,
                .address = address,
                .eew = eew,
                // Whole-register operations ignore architectural VL and
                // derive EVL from NF and EEW. Keep this deliberately wrong.
                .vl = 1,
                .rob = rob,
                .lq = static_cast<std::uint8_t>(
                    queue_cursor % memblock::kVirtualLoadQueueEntries),
                .lq_flag = ((queue_cursor /
                    memblock::kVirtualLoadQueueEntries) & 1U) != 0,
                .sq = static_cast<std::uint8_t>(
                    queue_cursor % memblock::kStoreQueueEntries),
                .sq_flag = ((queue_cursor /
                    memblock::kStoreQueueEntries) & 1U) != 0,
                .pdest = static_cast<std::uint8_t>(128 + uop),
                .lane = uop % memblock::kVectorMemoryLanes,
                .flow_num = 2,
                .vuop_idx = static_cast<std::uint8_t>(uop),
                .last_uop = uop + 1 == registers,
                .nf = nf,
            };
            for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
                transaction.data[byte] = static_cast<unsigned char>(
                    data_seed + 17 * uop + 3 * byte);
            }
            uops.push_back(transaction);
            queue_cursor += transaction.flow_num;
        }
        return uops;
    };
    auto run_whole_load = [&](std::vector<memblock::VectorMemoryTransaction> uops,
                              const char *phase) {
        if (!whole.set_rob_head(uops.front().rob, uops.front().rob_flag)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                      << whole.cycle() << " phase=" << phase
                      << " reason=" << whole.error() << '\n';
            return false;
        }
        for (const auto &uop : uops) {
            whole.expect_vector(uop);
            if (!whole.enqueue_vector(uop)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << whole.cycle() << " phase=" << phase
                          << " reason=" << whole.error() << '\n';
                return false;
            }
        }
        for (auto uop = uops.rbegin(); uop != uops.rend(); ++uop) {
            if (!whole.issue_vector(*uop, 1024)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << whole.cycle() << " phase=" << phase
                          << " reason=" << whole.error() << '\n';
                return false;
            }
        }
        if (!whole.run_until_vector_complete_with_replays(uops, 32768) ||
            !whole.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                      << whole.cycle() << " phase=" << phase
                      << " reason=" << whole.error() << '\n';
            return false;
        }
        whole_load_uops += uops.size();
        whole_lq_cursor += 2 * uops.size();
        return true;
    };

    for (const std::uint8_t nf : whole_nfs) {
        for (std::uint8_t eew = 0; eew < 4; ++eew) {
            const unsigned case_index = whole_cases++;
            const std::uint64_t source =
                whole_base + case_index * 0x400;
            const std::uint64_t destination = source + 0x200;
            auto loads = make_whole_uops(
                false, source, whole_rob++, nf, eew, whole_lq_cursor,
                static_cast<std::uint8_t>(0x40 + case_index));
            if (!run_whole_load(loads, "whole-register-load")) {
                return 1;
            }

            auto stores = make_whole_uops(
                true, destination, whole_rob++, nf, eew, whole_sq_cursor,
                static_cast<std::uint8_t>(0x80 + case_index));
            for (const auto &store : stores) {
                whole.expect_vector(store);
                if (!whole.enqueue_vector(store)) {
                    std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                              << whole.cycle()
                              << " phase=whole-register-store-enqueue reason="
                              << whole.error() << '\n';
                    return 1;
                }
            }
            for (auto store = stores.rbegin(); store != stores.rend(); ++store) {
                if (!whole.issue_vector(*store, 1024)) {
                    std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                              << whole.cycle()
                              << " phase=whole-register-store-issue reason="
                              << whole.error() << '\n';
                    return 1;
                }
            }
            if (!whole.run_until_vector_complete_with_replays(stores, 32768)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << whole.cycle()
                          << " phase=whole-register-store-complete reason="
                          << whole.error() << '\n';
                return 1;
            }
            for (const auto &store : stores) {
                if (!whole.commit_vector_store(store, 8192)) {
                    std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                              << whole.cycle()
                              << " phase=whole-register-store-commit reason="
                              << whole.error() << '\n';
                    return 1;
                }
            }
            whole_store_uops += stores.size();
            whole_sq_cursor += 2 * stores.size();
            if (!whole.run_until_queues_retired(8192) ||
                !whole.pulse_sbuffer_flush() ||
                !whole.run_until_sbuffer_empty(32768)) {
                std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                          << whole.cycle()
                          << " phase=whole-register-store-drain reason="
                          << whole.error() << '\n';
                return 1;
            }

            auto readbacks = make_whole_uops(
                false, destination, whole_rob++, nf, eew, whole_lq_cursor,
                static_cast<std::uint8_t>(0xd0 + case_index));
            if (!run_whole_load(readbacks, "whole-register-readback")) {
                return 1;
            }
        }
    }
    if (whole_cases != 16 || whole_load_uops != 120 ||
        whole_store_uops != 60 ||
        whole.lq_allocated() != whole.lq_dequeued() + whole.lq_canceled() ||
        whole.sq_allocated() != whole.sq_dequeued() + whole.sq_canceled()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle=" << whole.cycle()
                  << " phase=whole-register-conservation"
                  << " cases=" << whole_cases
                  << " load_uops=" << whole_load_uops
                  << " store_uops=" << whole_store_uops
                  << " lq=" << whole.lq_allocated() << '/'
                  << whole.lq_dequeued() << '+' << whole.lq_canceled()
                  << " sq=" << whole.sq_allocated() << '/'
                  << whole.sq_dequeued() << '+' << whole.sq_canceled()
                  << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_VECTOR_ADDRESSING_PASS"
              << " cycle=" << environment.cycle() + multi_uop.cycle() +
                    lmul_matrix.cycle() + strided_matrix.cycle() +
                    indexed_matrix.cycle() + indexed_lmul_matrix.cycle() +
                    whole.cycle()
              << " load_writebacks=" << environment.vector_load_writebacks() +
                    multi_uop.vector_load_writebacks() +
                    lmul_matrix.vector_load_writebacks() +
                    strided_matrix.vector_load_writebacks() +
                    indexed_matrix.vector_load_writebacks() +
                    indexed_lmul_matrix.vector_load_writebacks() +
                    whole.vector_load_writebacks()
              << " store_writebacks=" << environment.vector_store_writebacks()
                    + lmul_matrix.vector_store_writebacks() +
                    strided_matrix.vector_store_writebacks() +
                    indexed_matrix.vector_store_writebacks() +
                    indexed_lmul_matrix.vector_store_writebacks() +
                    whole.vector_store_writebacks()
              << " store_modes=3"
              << " multi_uop_modes=3 multi_uop_writebacks=6"
              << " lmul_configurations=" << lmul_configurations
              << " lmul_load_uops=" << lmul_load_uops
              << " lmul_store_uops=" << lmul_store_uops
              << " lmul_rob_wraps=" << lmul_rob_wraps
              << " lmul_lq_allocated=" << lmul_matrix.lq_allocated()
              << " lmul_sq_allocated=" << lmul_matrix.sq_allocated()
              << " strided_configurations=" << strided_configurations
              << " strided_load_uops=" << strided_load_uops
              << " strided_store_uops=" << strided_store_uops
              << " strided_positive=" << strided_positive
              << " strided_negative=" << strided_negative
              << " strided_lq_allocated=" << strided_matrix.lq_allocated()
              << " strided_sq_allocated=" << strided_matrix.sq_allocated()
              << " indexed_cases=" << indexed_cases
              << " indexed_load_uops=" << indexed_load_uops
              << " indexed_store_uops=" << indexed_store_uops
              << " indexed_lq_allocated=" << indexed_matrix.lq_allocated()
              << " indexed_sq_allocated=" << indexed_matrix.sq_allocated()
              << " indexed_lmul_configurations="
                    << indexed_lmul_configurations
              << " indexed_lmul_unordered=" << indexed_lmul_unordered
              << " indexed_lmul_ordered=" << indexed_lmul_ordered
              << " indexed_lmul_special=" << indexed_lmul_special
              << " indexed_lmul_load_uops=" << indexed_lmul_load_uops
              << " indexed_lmul_store_uops=" << indexed_lmul_store_uops
              << " indexed_lmul_rob_wraps=" << indexed_lmul_rob_wraps
              << " indexed_lmul_lq_allocated="
                    << indexed_lmul_matrix.lq_allocated()
              << " indexed_lmul_sq_allocated="
                    << indexed_lmul_matrix.sq_allocated()
              << " whole_cases=" << whole_cases
              << " whole_load_uops=" << whole_load_uops
              << " whole_store_uops=" << whole_store_uops
              << " whole_lq_allocated=" << whole.lq_allocated()
              << " whole_sq_allocated=" << whole.sq_allocated()
              << " tilelink_requests=" << environment.tilelink_requests() +
                    multi_uop.tilelink_requests() +
                    lmul_matrix.tilelink_requests() +
                    strided_matrix.tilelink_requests() +
                    indexed_matrix.tilelink_requests() +
                    indexed_lmul_matrix.tilelink_requests() +
                    whole.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_exception_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50000000ULL;
    constexpr std::uint64_t physical_base = 0xa3000000ULL;
    constexpr std::uint64_t root = 0x92000000ULL;
    environment.memory().fill_incrementing(physical_base, 0x2000, 0x43);
    if (!environment.reset() || !environment.enable_misaligned_accesses() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root) ||
        !environment.map_sv39_4k(
            virtual_base + 0x1000,
            physical_base + 0x1000,
            root,
            true,
            true,
            false,
            false,
            true) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction page_fault{
        .address = virtual_base + 0x3000,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 40,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
    };
    environment.expect_load(page_fault);
    if (!environment.enqueue_load(page_fault) ||
        !environment.issue_load(page_fault, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=load-page-fault reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::PrefetchTransaction prefetch_fault{
        .address = virtual_base + 0x4000,
        .op = memblock::PrefetchOp::read,
        .rob = 1,
        .lq = 1,
        .lane = 1,
    };
    environment.expect_prefetch(prefetch_fault);
    if (!environment.enqueue_prefetch(prefetch_fault) ||
        !environment.issue_prefetch(prefetch_fault, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=prefetch-page-fault reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction nc_misaligned_load{
        .address = virtual_base + 0x1001,
        .oracle_address = physical_base + 0x1001,
        .op = memblock::LoadOp::lw,
        .rob = 2,
        .lq = 2,
        .pdest = 41,
        .lane = 2,
        .expected_exception_mask = memblock::kExceptionLoadAddressMisaligned,
    };
    environment.expect_load(nc_misaligned_load);
    if (!environment.set_rob_head(nc_misaligned_load.rob) ||
        !environment.enqueue_load(nc_misaligned_load) ||
        !environment.issue_load(nc_misaligned_load, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=pbmt-load-misaligned reason="
                  << environment.error() << '\n';
        return 1;
    }

    memblock::Environment load_priority(argc, argv);
    constexpr std::uint64_t load_priority_root = 0x92200000ULL;
    if (!load_priority.reset() ||
        !load_priority.activate_sv39(load_priority_root)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << load_priority.cycle()
                  << " phase=load-priority-configuration reason="
                  << load_priority.error() << '\n';
        return 1;
    }
    const std::vector<memblock::LoadTransaction> load_faults{
        memblock::LoadTransaction{
            .address = virtual_base + 0x5000,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(160),
            .rob_flag = memblock::rob_pointer_flag(160),
            .lq = memblock::lq_pointer_value(0),
            .lq_flag = memblock::lq_pointer_flag(0),
            .pdest = 42,
            .lane = 0,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        },
        memblock::LoadTransaction{
            .address = virtual_base + 0x6000,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(159),
            .rob_flag = memblock::rob_pointer_flag(159),
            .lq = memblock::lq_pointer_value(1),
            .lq_flag = memblock::lq_pointer_flag(1),
            .pdest = 43,
            .lane = 1,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        },
        memblock::LoadTransaction{
            .address = virtual_base + 0x7000,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(158),
            .rob_flag = memblock::rob_pointer_flag(158),
            .lq = memblock::lq_pointer_value(2),
            .lq_flag = memblock::lq_pointer_flag(2),
            .pdest = 44,
            .lane = 2,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        },
    };
    for (const auto &fault : load_faults) {
        load_priority.expect_load(fault);
    }
    const auto &oldest_load_fault = load_faults.back();
    if (!load_priority.set_rob_head(
            oldest_load_fault.rob, oldest_load_fault.rob_flag) ||
        !load_priority.enqueue_load_batch(load_faults, {0, 1, 2}) ||
        !load_priority.issue_load_batch(load_faults, 256, true) ||
        !load_priority.run_until_complete(8192) ||
        !load_priority.run_cycles(8) ||
        load_priority.exception_vaddr() != oldest_load_fault.address ||
        !load_priority.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << load_priority.cycle()
                  << " phase=load-priority expected_vaddr=0x" << std::hex
                  << oldest_load_fault.address << " actual_vaddr=0x"
                  << load_priority.exception_vaddr() << std::dec
                  << " reason=" << load_priority.error() << '\n';
        return 1;
    }

    memblock::Environment uop_priority(argc, argv);
    constexpr std::uint64_t uop_priority_root = 0x92300000ULL;
    constexpr std::uint64_t uop_priority_base = 0x52000000ULL;
    memblock::VectorMemoryTransaction older_uop{
        .address = uop_priority_base + 0x8,
        .eew = 3,
        .vl = 4,
        .rob = 32,
        .lq = 0,
        .pdest = 46,
        .lane = 1,
        .flow_num = 2,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
        .vlmul = 1,
        .vuop_idx = 0,
        .last_uop = false,
    };
    memblock::VectorMemoryTransaction younger_uop = older_uop;
    younger_uop.lq = 2;
    younger_uop.pdest = 47;
    younger_uop.lane = 0;
    younger_uop.vuop_idx = 1;
    younger_uop.last_uop = true;
    const std::uint64_t younger_uop_fault_address =
        memblock::vector_element_address(younger_uop, 0);
    const std::uint64_t older_uop_fault_address =
        memblock::vector_element_address(older_uop, 0);
    if (!uop_priority.reset() ||
        !uop_priority.activate_sv39(uop_priority_root, 23) ||
        !uop_priority.set_rob_head(older_uop.rob, older_uop.rob_flag) ||
        !uop_priority.enqueue_vector(older_uop) ||
        !uop_priority.enqueue_vector(younger_uop)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << uop_priority.cycle()
                  << " phase=same-rob-uop-configuration reason="
                  << uop_priority.error() << '\n';
        return 1;
    }
    uop_priority.expect_vector(younger_uop);
    if (!uop_priority.issue_vector(younger_uop, 512) ||
        !uop_priority.run_until_vector_complete_with_replays(
            younger_uop, 16384) ||
        !uop_priority.run_cycles(8) ||
        uop_priority.exception_vaddr() != younger_uop_fault_address) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << uop_priority.cycle()
                  << " phase=same-rob-younger-uop expected_vaddr=0x"
                  << std::hex << younger_uop_fault_address
                  << " actual_vaddr=0x"
                  << uop_priority.exception_vaddr() << std::dec
                  << " reason=" << uop_priority.error() << '\n';
        return 1;
    }
    uop_priority.expect_vector(older_uop);
    if (!uop_priority.issue_vector(older_uop, 512) ||
        !uop_priority.run_until_vector_complete_with_replays(
            older_uop, 16384) ||
        !uop_priority.run_cycles(8) ||
        uop_priority.exception_vaddr() != older_uop_fault_address ||
        !uop_priority.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << uop_priority.cycle()
                  << " phase=same-rob-older-uop expected_vaddr=0x"
                  << std::hex << older_uop_fault_address
                  << " actual_vaddr=0x"
                  << uop_priority.exception_vaddr() << std::dec
                  << " reason=" << uop_priority.error() << '\n';
        return 1;
    }

    memblock::Environment cross_cause(argc, argv);
    constexpr std::uint64_t cross_cause_virtual = 0x55000000ULL;
    constexpr std::uint64_t cross_cause_physical = 0xa5000000ULL;
    constexpr std::uint64_t cross_cause_root = 0x92700000ULL;
    cross_cause.memory().fill_incrementing(
        cross_cause_physical, 0x1000, 0x75);
    const std::vector<memblock::LoadTransaction> different_cause_faults{
        memblock::LoadTransaction{
            .address = cross_cause_virtual + 0x3000,
            .op = memblock::LoadOp::ld,
            .rob = 61,
            .lq = 0,
            .pdest = 48,
            .lane = 0,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        },
        memblock::LoadTransaction{
            .address = cross_cause_virtual + 0x1001,
            .oracle_address = cross_cause_physical + 1,
            .op = memblock::LoadOp::ld,
            .rob = 60,
            .lq = 1,
            .pdest = 49,
            .lane = 1,
            .expected_exception_mask =
                memblock::kExceptionLoadAddressMisaligned,
        },
    };
    for (const auto &fault : different_cause_faults) {
        cross_cause.expect_load(fault);
    }
    const auto &oldest_cross_cause = different_cause_faults.back();
    if (!cross_cause.reset() ||
        !cross_cause.enable_misaligned_accesses() ||
        !cross_cause.map_sv39_4k(
            cross_cause_virtual + 0x1000,
            cross_cause_physical,
            cross_cause_root,
            true,
            true,
            false,
            false,
            true) ||
        !cross_cause.activate_sv39(cross_cause_root, 24) ||
        !cross_cause.set_rob_head(
            oldest_cross_cause.rob, oldest_cross_cause.rob_flag) ||
        !cross_cause.enqueue_load_batch(different_cause_faults, {0, 1}) ||
        !cross_cause.issue_load_batch(different_cause_faults, 256, true) ||
        !cross_cause.run_until_complete(8192) ||
        !cross_cause.run_cycles(8) ||
        cross_cause.exception_vaddr() != oldest_cross_cause.address ||
        cross_cause.tilelink_requests() != 0 ||
        cross_cause.uncache_requests() != 0 ||
        !cross_cause.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << cross_cause.cycle()
                  << " phase=cross-cause-priority expected_vaddr=0x"
                  << std::hex << oldest_cross_cause.address
                  << " actual_vaddr=0x" << cross_cause.exception_vaddr()
                  << std::dec << " reason=" << cross_cause.error() << '\n';
        return 1;
    }

    memblock::Environment vector_over_scalar(argc, argv);
    constexpr std::uint64_t vector_over_scalar_root = 0x92500000ULL;
    constexpr std::uint64_t vector_over_scalar_base = 0x53000000ULL;
    const memblock::LoadTransaction younger_scalar{
        .address = vector_over_scalar_base + 0x1000,
        .op = memblock::LoadOp::ld,
        .rob = 91,
        .lq = 0,
        .pdest = 50,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
    };
    const memblock::VectorMemoryTransaction older_vector{
        .address = vector_over_scalar_base + 0x2000,
        .eew = 3,
        .vl = 2,
        .rob = 90,
        .lq = 1,
        .pdest = 51,
        .lane = 1,
        .flow_num = 2,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
    };
    if (!vector_over_scalar.reset() ||
        !vector_over_scalar.activate_sv39(vector_over_scalar_root, 25) ||
        !vector_over_scalar.set_rob_head(
            older_vector.rob, older_vector.rob_flag) ||
        !vector_over_scalar.enqueue_load(younger_scalar) ||
        !vector_over_scalar.enqueue_vector(older_vector)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << vector_over_scalar.cycle()
                  << " phase=vector-over-scalar-configuration reason="
                  << vector_over_scalar.error() << '\n';
        return 1;
    }
    vector_over_scalar.expect_load(younger_scalar);
    if (!vector_over_scalar.issue_load(younger_scalar, 512) ||
        !vector_over_scalar.run_until_complete(8192) ||
        !vector_over_scalar.run_cycles(8) ||
        vector_over_scalar.exception_vaddr() != younger_scalar.address) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << vector_over_scalar.cycle()
                  << " phase=younger-scalar-first expected_vaddr=0x"
                  << std::hex << younger_scalar.address
                  << " actual_vaddr=0x"
                  << vector_over_scalar.exception_vaddr() << std::dec
                  << " reason=" << vector_over_scalar.error() << '\n';
        return 1;
    }
    vector_over_scalar.expect_vector(older_vector);
    if (!vector_over_scalar.issue_vector(older_vector, 512) ||
        !vector_over_scalar.run_until_vector_complete_with_replays(
            older_vector, 16384) ||
        !vector_over_scalar.run_cycles(8) ||
        vector_over_scalar.exception_vaddr() != older_vector.address ||
        !vector_over_scalar.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << vector_over_scalar.cycle()
                  << " phase=older-vector-replacement expected_vaddr=0x"
                  << std::hex << older_vector.address
                  << " actual_vaddr=0x"
                  << vector_over_scalar.exception_vaddr() << std::dec
                  << " reason=" << vector_over_scalar.error() << '\n';
        return 1;
    }

    memblock::Environment scalar_over_vector(argc, argv);
    constexpr std::uint64_t scalar_over_vector_root = 0x92600000ULL;
    constexpr std::uint64_t scalar_over_vector_base = 0x54000000ULL;
    const memblock::VectorMemoryTransaction younger_vector{
        .address = scalar_over_vector_base + 0x1000,
        .eew = 3,
        .vl = 2,
        .rob = 101,
        .lq = 0,
        .pdest = 52,
        .lane = 1,
        .flow_num = 2,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
    };
    const memblock::LoadTransaction older_scalar{
        .address = scalar_over_vector_base + 0x2000,
        .op = memblock::LoadOp::ld,
        .rob = 100,
        .lq = 2,
        .pdest = 53,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
    };
    if (!scalar_over_vector.reset() ||
        !scalar_over_vector.activate_sv39(scalar_over_vector_root, 26) ||
        !scalar_over_vector.set_rob_head(
            older_scalar.rob, older_scalar.rob_flag) ||
        !scalar_over_vector.enqueue_vector(younger_vector) ||
        !scalar_over_vector.enqueue_load(older_scalar)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << scalar_over_vector.cycle()
                  << " phase=scalar-over-vector-configuration reason="
                  << scalar_over_vector.error() << '\n';
        return 1;
    }
    scalar_over_vector.expect_vector(younger_vector);
    if (!scalar_over_vector.issue_vector(younger_vector, 512) ||
        !scalar_over_vector.run_until_vector_complete_with_replays(
            younger_vector, 16384) ||
        !scalar_over_vector.run_cycles(8) ||
        scalar_over_vector.exception_vaddr() != younger_vector.address) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << scalar_over_vector.cycle()
                  << " phase=younger-vector-first expected_vaddr=0x"
                  << std::hex << younger_vector.address
                  << " actual_vaddr=0x"
                  << scalar_over_vector.exception_vaddr() << std::dec
                  << " reason=" << scalar_over_vector.error() << '\n';
        return 1;
    }
    scalar_over_vector.expect_load(older_scalar);
    if (!scalar_over_vector.issue_load(older_scalar, 512) ||
        !scalar_over_vector.run_until_complete(8192) ||
        !scalar_over_vector.run_cycles(8) ||
        scalar_over_vector.exception_vaddr() != older_scalar.address ||
        !scalar_over_vector.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << scalar_over_vector.cycle()
                  << " phase=older-scalar-replacement expected_vaddr=0x"
                  << std::hex << older_scalar.address
                  << " actual_vaddr=0x"
                  << scalar_over_vector.exception_vaddr() << std::dec
                  << " reason=" << scalar_over_vector.error() << '\n';
        return 1;
    }

    memblock::Environment store_priority(argc, argv);
    constexpr std::uint64_t store_priority_root = 0x92400000ULL;
    constexpr std::uint64_t store_priority_virtual = virtual_base + 0x8000;
    constexpr std::uint64_t store_priority_physical = physical_base + 0x8000;
    if (!store_priority.reset() ||
        !store_priority.enable_misaligned_accesses() ||
        !store_priority.map_sv39_4k(
            store_priority_virtual, store_priority_physical,
            store_priority_root, true, true, false, false, true) ||
        !store_priority.activate_sv39(store_priority_root)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << store_priority.cycle()
                  << " phase=store-priority-configuration reason="
                  << store_priority.error() << '\n';
        return 1;
    }
    store_priority.select_store_exception_address(true);
    const std::vector<memblock::StoreTransaction> store_warmups{
        memblock::StoreTransaction{
            .address = store_priority_virtual + 0x20,
            .oracle_address = store_priority_physical + 0x20,
            .data = 0xa5a55a5a11223344ULL,
            .op = memblock::StoreOp::sd,
            .rob = memblock::rob_pointer_value(156),
            .rob_flag = memblock::rob_pointer_flag(156),
            .sq = memblock::sq_pointer_value(0),
            .sq_flag = memblock::sq_pointer_flag(0),
            .address_lane = 0,
            .data_lane = 0,
        },
        memblock::StoreTransaction{
            .address = store_priority_virtual + 0x28,
            .oracle_address = store_priority_physical + 0x28,
            .data = 0x5a5aa5a588776655ULL,
            .op = memblock::StoreOp::sd,
            .rob = memblock::rob_pointer_value(157),
            .rob_flag = memblock::rob_pointer_flag(157),
            .sq = memblock::sq_pointer_value(1),
            .sq_flag = memblock::sq_pointer_flag(1),
            .address_lane = 1,
            .data_lane = 1,
        },
    };
    for (std::size_t index = 0; index < store_warmups.size(); ++index) {
        const auto &warmup = store_warmups[index];
        store_priority.expect_store(warmup);
        if (!store_priority.enqueue_store(warmup, 0) ||
            !(index == 0
                  ? store_priority.warm_store_translation(warmup)
                  : store_priority.issue_store_address(warmup, 256)) ||
            !store_priority.issue_store_data(warmup, 256)) {
            std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                      << store_priority.cycle()
                      << " phase=store-priority-warm reason="
                      << store_priority.error() << '\n';
            return 1;
        }
    }
    if (!store_priority.run_until_store_complete(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << store_priority.cycle()
                  << " phase=store-priority-warm-complete reason="
                  << store_priority.error() << '\n';
        return 1;
    }
    const std::vector<memblock::StoreTransaction> store_faults{
        memblock::StoreTransaction{
            .address = store_priority_virtual + 1,
            .oracle_address = store_priority_physical + 1,
            .data = 0x8877665544332211ULL,
            .op = memblock::StoreOp::sd,
            .rob = memblock::rob_pointer_value(160),
            .rob_flag = memblock::rob_pointer_flag(160),
            .sq = memblock::sq_pointer_value(2),
            .sq_flag = memblock::sq_pointer_flag(2),
            .address_lane = 0,
            .data_lane = 0,
            .expected_exception_mask =
                memblock::kExceptionStoreAddressMisaligned,
        },
        memblock::StoreTransaction{
            .address = store_priority_virtual + 0x11,
            .oracle_address = store_priority_physical + 0x11,
            .data = 0x1122334455667788ULL,
            .op = memblock::StoreOp::sd,
            .rob = memblock::rob_pointer_value(159),
            .rob_flag = memblock::rob_pointer_flag(159),
            .sq = memblock::sq_pointer_value(3),
            .sq_flag = memblock::sq_pointer_flag(3),
            .address_lane = 1,
            .data_lane = 1,
            .expected_exception_mask =
                memblock::kExceptionStoreAddressMisaligned,
        },
    };
    for (const auto &fault : store_faults) {
        store_priority.expect_store(fault);
    }
    const auto &oldest_store_fault = store_faults.back();
    if (!store_priority.set_rob_head(
            oldest_store_fault.rob, oldest_store_fault.rob_flag) ||
        !store_priority.enqueue_store(store_faults[0], 0) ||
        !store_priority.enqueue_store(store_faults[1], 0) ||
        !store_priority.issue_store_address_batch(store_faults, 256) ||
        !store_priority.run_until_store_complete(8192) ||
        !store_priority.run_cycles(8) ||
        store_priority.exception_vaddr() != oldest_store_fault.address) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << store_priority.cycle()
                  << " phase=store-priority expected_vaddr=0x" << std::hex
                  << oldest_store_fault.address << " actual_vaddr=0x"
                  << store_priority.exception_vaddr() << std::dec
                  << " reason=" << store_priority.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction selector_load{
        .address = virtual_base + 0xa000,
        .op = memblock::LoadOp::ld,
        .rob = memblock::rob_pointer_value(161),
        .rob_flag = memblock::rob_pointer_flag(161),
        .lq = memblock::lq_pointer_value(0),
        .lq_flag = memblock::lq_pointer_flag(0),
        .pdest = 45,
        .lane = 1,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
    };
    store_priority.expect_load(selector_load);
    if (!store_priority.enqueue_load(selector_load) ||
        !store_priority.issue_load(selector_load, 256) ||
        !store_priority.run_until_complete(8192) ||
        !store_priority.run_cycles(8) ||
        store_priority.exception_vaddr() != oldest_store_fault.address) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << store_priority.cycle()
                  << " phase=selector-store-retention expected_vaddr=0x"
                  << std::hex << oldest_store_fault.address
                  << " actual_vaddr=0x" << store_priority.exception_vaddr()
                  << std::dec << " reason=" << store_priority.error() << '\n';
        return 1;
    }
    store_priority.select_store_exception_address(false);
    if (!store_priority.run_cycles(3) ||
        store_priority.exception_vaddr() != selector_load.address) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << store_priority.cycle()
                  << " phase=selector-load expected_vaddr=0x" << std::hex
                  << selector_load.address << " actual_vaddr=0x"
                  << store_priority.exception_vaddr() << std::dec
                  << " reason=" << store_priority.error() << '\n';
        return 1;
    }
    store_priority.select_store_exception_address(true);
    if (!store_priority.run_cycles(3) ||
        store_priority.exception_vaddr() != oldest_store_fault.address ||
        !store_priority.run_until_lq_retired(1024) ||
        !store_priority.account_sq_cancellation(
            store_priority.sq_allocated() - store_priority.sq_dequeued() -
            store_priority.sq_canceled())) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << store_priority.cycle()
                  << " phase=selector-store-restore expected_vaddr=0x"
                  << std::hex << oldest_store_fault.address
                  << " actual_vaddr=0x" << store_priority.exception_vaddr()
                  << std::dec << " reason=" << store_priority.error() << '\n';
        return 1;
    }

    std::uint64_t store_vector_cycles = 0;
    std::uint64_t store_vector_store_writebacks = 0;
    std::uint64_t store_vector_load_writebacks = 0;
    std::uint64_t store_vector_ptw_requests = 0;
    auto run_store_vector_selector = [&](bool store_first) {
        memblock::Environment source_priority(argc, argv);
        const std::uint64_t source_base = store_first
            ? 0x56000000ULL : 0x57000000ULL;
        const std::uint64_t source_root = store_first
            ? 0x92800000ULL : 0x92900000ULL;
        const unsigned older_rob = store_first ? 120 : 130;
        const memblock::StoreTransaction store_fault{
            .address = source_base + 0x1000,
            .data = 0x5aa53cc36996877aULL,
            .op = memblock::StoreOp::sd,
            .rob = memblock::rob_pointer_value(
                older_rob + (store_first ? 1 : 0)),
            .rob_flag = memblock::rob_pointer_flag(
                older_rob + (store_first ? 1 : 0)),
            .sq = 0,
            .address_lane = store_first ? 0U : 1U,
            .data_lane = store_first ? 0U : 1U,
            .expected_exception_mask = memblock::kExceptionStorePageFault,
        };
        const memblock::VectorMemoryTransaction vector_fault{
            .address = source_base + 0x2000,
            .eew = 3,
            .vl = 2,
            .rob = memblock::rob_pointer_value(
                older_rob + (store_first ? 0 : 1)),
            .rob_flag = memblock::rob_pointer_flag(
                older_rob + (store_first ? 0 : 1)),
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(store_first ? 56 : 57),
            .lane = store_first ? 1U : 0U,
            .flow_num = 2,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        const std::uint8_t oldest_rob_value = memblock::rob_pointer_value(
            older_rob);
        const bool oldest_rob_flag = memblock::rob_pointer_flag(older_rob);
        if (!source_priority.reset() ||
            !source_priority.activate_sv39(
                source_root, store_first ? 30 : 31) ||
            !source_priority.set_rob_head(
                oldest_rob_value, oldest_rob_flag)) {
            std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                      << source_priority.cycle()
                      << " phase=store-vector-configuration order="
                      << (store_first ? "store-first" : "vector-first")
                      << " reason=" << source_priority.error() << '\n';
            return false;
        }

        auto issue_store_fault = [&]() {
            source_priority.expect_store(store_fault);
            return source_priority.enqueue_store(store_fault, 0) &&
                source_priority.issue_store_address(store_fault, 512) &&
                source_priority.issue_store_data(store_fault, 512) &&
                source_priority.run_until_store_complete_with_replay(
                    store_fault, 16384);
        };
        auto issue_vector_fault = [&]() {
            source_priority.expect_vector(vector_fault);
            return source_priority.enqueue_vector(vector_fault) &&
                source_priority.issue_vector(vector_fault, 512) &&
                source_priority.run_until_vector_complete_with_replays(
                    vector_fault, 16384) &&
                source_priority.run_until_lq_retired(2048);
        };

        source_priority.select_store_exception_address(store_first);
        const bool first_issued = store_first
            ? issue_store_fault() : issue_vector_fault();
        const std::uint64_t first_address = store_first
            ? store_fault.address : vector_fault.address;
        if (!first_issued || !source_priority.run_cycles(8) ||
            source_priority.exception_vaddr() != first_address) {
            std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                      << source_priority.cycle()
                      << " phase=store-vector-first order="
                      << (store_first ? "store-first" : "vector-first")
                      << " expected_vaddr=0x" << std::hex << first_address
                      << " actual_vaddr=0x"
                      << source_priority.exception_vaddr() << std::dec
                      << " reason=" << source_priority.error() << '\n';
            return false;
        }

        const bool second_issued = store_first
            ? issue_vector_fault() : issue_store_fault();
        if (!second_issued || !source_priority.run_cycles(8) ||
            source_priority.exception_vaddr() != first_address) {
            std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                      << source_priority.cycle()
                      << " phase=store-vector-retain-first order="
                      << (store_first ? "store-first" : "vector-first")
                      << " expected_vaddr=0x" << std::hex << first_address
                      << " actual_vaddr=0x"
                      << source_priority.exception_vaddr() << std::dec
                      << " reason=" << source_priority.error() << '\n';
            return false;
        }

        const std::uint64_t second_address = store_first
            ? vector_fault.address : store_fault.address;
        source_priority.select_store_exception_address(!store_first);
        if (!source_priority.run_cycles(3) ||
            source_priority.exception_vaddr() != second_address) {
            std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                      << source_priority.cycle()
                      << " phase=store-vector-select-second order="
                      << (store_first ? "store-first" : "vector-first")
                      << " expected_vaddr=0x" << std::hex << second_address
                      << " actual_vaddr=0x"
                      << source_priority.exception_vaddr() << std::dec
                      << " reason=" << source_priority.error() << '\n';
            return false;
        }
        source_priority.select_store_exception_address(store_first);
        if (!source_priority.run_cycles(3) ||
            source_priority.exception_vaddr() != first_address ||
            source_priority.tilelink_requests() != 0 ||
            source_priority.uncache_requests() != 0 ||
            !source_priority.account_sq_cancellation(
                source_priority.sq_allocated() -
                source_priority.sq_dequeued() -
                source_priority.sq_canceled())) {
            std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                      << source_priority.cycle()
                      << " phase=store-vector-restore-first order="
                      << (store_first ? "store-first" : "vector-first")
                      << " expected_vaddr=0x" << std::hex << first_address
                      << " actual_vaddr=0x"
                      << source_priority.exception_vaddr() << std::dec
                      << " reason=" << source_priority.error() << '\n';
            return false;
        }
        store_vector_cycles += source_priority.cycle();
        store_vector_store_writebacks +=
            source_priority.store_writebacks();
        store_vector_load_writebacks +=
            source_priority.vector_load_writebacks();
        store_vector_ptw_requests += source_priority.ptw_requests();
        return true;
    };
    if (!run_store_vector_selector(true) ||
        !run_store_vector_selector(false)) {
        return 1;
    }

    std::cout << "MEMBLOCK_EXCEPTION_CONTRACTS_PASS"
              << " cycle=" << environment.cycle() + load_priority.cycle() +
                    uop_priority.cycle() + cross_cause.cycle() +
                    vector_over_scalar.cycle() + scalar_over_vector.cycle() +
                    store_priority.cycle() + store_vector_cycles
              << " load_writebacks="
              << environment.writebacks() + load_priority.writebacks() +
                    cross_cause.writebacks() +
                    vector_over_scalar.writebacks() +
                    scalar_over_vector.writebacks() + store_priority.writebacks()
              << " store_writebacks=" << store_priority.store_writebacks() +
                    store_vector_store_writebacks
              << " vector_writebacks="
              << uop_priority.vector_load_writebacks() +
                    vector_over_scalar.vector_load_writebacks() +
                    scalar_over_vector.vector_load_writebacks() +
                    store_vector_load_writebacks
              << " prefetch_writebacks=" << environment.prefetch_writebacks()
              << " load_priority=3 same_rob_uop_priority=2"
              << " cross_cause_priority=2 scalar_vector_priority=2"
              << " store_priority=2 selector_cross=1"
              << " store_vector_priority=2"
              << " load_oldest=0x" << std::hex
              << oldest_load_fault.address
              << " store_oldest=0x" << oldest_store_fault.address << std::dec
              << " ptw_requests="
              << environment.ptw_requests() + load_priority.ptw_requests() +
                    vector_over_scalar.ptw_requests() +
                    scalar_over_vector.ptw_requests() +
                    store_priority.ptw_requests() +
                    store_vector_ptw_requests
              << " uncache_requests="
              << environment.uncache_requests() +
                    load_priority.uncache_requests() +
                    cross_cause.uncache_requests() +
                    vector_over_scalar.uncache_requests() +
                    scalar_over_vector.uncache_requests() +
                    store_priority.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
