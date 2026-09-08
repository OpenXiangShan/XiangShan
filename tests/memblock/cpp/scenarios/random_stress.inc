// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_random_stress(int argc, char **argv, const Options &options)
{
    constexpr unsigned minimum_actions = 96;
    if (options.transactions < minimum_actions) {
        std::cerr << "MEMBLOCK_RANDOM_STRESS_FAIL seed=" << options.seed
                  << " transactions=0 cycle=0 reason=random-stress_requires_at_least_"
                  << minimum_actions << "_actions\n";
        return 1;
    }

    enum class Kind { scalar_store, scalar_load, vector_store, vector_load,
                      prefetch, extra_load };
    struct Action {
        Kind kind;
        unsigned group;
    };
    struct Group {
        bool has_scalar_store = false;
        bool has_scalar_load = false;
        bool has_vector_store = false;
        bool has_vector_load = false;
        bool has_prefetch = false;
        bool has_extra_load = false;
        memblock::StoreTransaction scalar_store{};
        memblock::LoadTransaction scalar_load{};
        memblock::VectorMemoryTransaction vector_store{};
        memblock::VectorMemoryTransaction vector_load{};
        memblock::PrefetchTransaction prefetch{};
        memblock::LoadTransaction extra_load{};
    };

    memblock::Environment environment(argc, argv);
    StressRandom random(options.seed);
    StressCoverage coverage;
    // The generated DUT resets pendingPtr to ROB zero. Start at one so the
    // first store cannot be interpreted as already committed before the
    // stress driver's explicit commit point.
    std::uint64_t rob_offset = 1;
    std::uint64_t lq_offset = 0;
    std::uint64_t sq_offset = 0;
    unsigned completed_actions = 0;
    unsigned burst_index = 0;
    std::string phase = "reset";
    constexpr std::uint64_t region0 = memblock::kDefaultMemoryBase + 0x500000;
    constexpr std::uint64_t region1 = memblock::kDefaultMemoryBase + 0x900000;
    constexpr std::uint64_t region_span = 0x400000;

    environment.memory().fill_incrementing(region0, region_span, 0x13);
    environment.memory().fill_incrementing(region1, region_span, 0x97);
    environment.configure_backpressure(
        StressRandom::splitmix64(options.seed ^ 0x13198a2e03707344ULL),
        options.backpressure);

    auto make_load = [&](std::uint64_t address, memblock::LoadOp op,
                         unsigned lane) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset++;
        auto transaction = memblock::LoadTransaction{
            .address = address,
            .op = op,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq),
            .lq_flag = memblock::lq_pointer_flag(lq),
            .sq = memblock::sq_pointer_value(sq_offset),
            .sq_flag = memblock::sq_pointer_flag(sq_offset),
            .pdest = static_cast<std::uint8_t>(1 + random.next_payload() % 255),
            .lane = lane,
        };
        transaction.predecode_rvc = (rob % 4U) == 0 ||
            (random.next_shape() & 3U) == 0;
        transaction.ftq_ptr = random.next_shape() & 0x3fU;
        transaction.ftq_offset = static_cast<std::uint8_t>(random.next_shape() & 7U);
        transaction.store_set_hit = (rob % 5U) == 0 ||
            (random.next_shape() % 5U) == 0;
        transaction.load_wait_bit = rob != 0 &&
            ((rob % 7U) == 0 || (random.next_shape() % 7U) == 0);
        // Keep strict wait out of overlapping stress bursts; its independent
        // sideband drive is covered by metadata-contracts.
        transaction.load_wait_strict = false;
        transaction.wait_for_rob_flag = transaction.load_wait_bit &&
            memblock::rob_pointer_flag(rob - 1);
        transaction.wait_for_rob_value = transaction.load_wait_bit
            ? memblock::rob_pointer_value(rob - 1)
            : 0;
        return transaction;
    };
    auto make_store = [&](std::uint64_t address, std::uint64_t data,
                          memblock::StoreOp op, unsigned address_lane,
                          unsigned data_lane) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t sq = sq_offset++;
        auto transaction = memblock::StoreTransaction{
            .address = address,
            .data = data,
            .op = op,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .sq = memblock::sq_pointer_value(sq),
            .sq_flag = memblock::sq_pointer_flag(sq),
            .address_lane = address_lane,
            .data_lane = data_lane,
        };
        return transaction;
    };
    auto make_prefetch = [&](std::uint64_t address, memblock::PrefetchOp op,
                             unsigned lane) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset++;
        return memblock::PrefetchTransaction{
            .address = address,
            .op = op,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq),
            .lq_flag = memblock::lq_pointer_flag(lq),
            .sq = memblock::sq_pointer_value(sq_offset),
            .sq_flag = memblock::sq_pointer_flag(sq_offset),
            .lane = lane,
        };
    };
    auto make_vector = [&](bool store, std::uint64_t address, unsigned eew,
                           unsigned lane, memblock::VectorAddressingMode mode) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset;
        const std::uint64_t sq = sq_offset;
        const std::uint8_t flow_num = mode ==
                memblock::VectorAddressingMode::unit_stride
            ? 2
            : static_cast<std::uint8_t>(16U >> eew);
        if (store) {
            sq_offset += flow_num;
        } else {
            lq_offset += flow_num;
        }
        memblock::VectorMemoryTransaction transaction{
            .store = store,
            .address = address,
            .addressing = mode,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(16U >> eew),
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq),
            .lq_flag = memblock::lq_pointer_flag(lq),
            .sq = memblock::sq_pointer_value(sq),
            .sq_flag = memblock::sq_pointer_flag(sq),
            .pdest = static_cast<std::uint8_t>(1 + random.next_payload() % 255),
            .lane = lane,
            .flow_num = flow_num,
        };
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(random.next_payload());
        }
        transaction.ftq_ptr = random.next_shape() & 0x3fU;
        transaction.ftq_offset = static_cast<std::uint8_t>(random.next_shape() & 7U);
        // Keep LMUL coupled to the explicitly modeled flow count; the
        // standalone issue boundary does not carry vset/vl context.
        transaction.vlmul = 0;
        return transaction;
    };
    auto randomize_vector_addressing = [&](memblock::VectorMemoryTransaction &transaction) {
        const unsigned element_bytes = 1U << transaction.eew;
        const unsigned elements = 16U >> transaction.eew;
        if (transaction.addressing == memblock::VectorAddressingMode::strided) {
            // Repeated-address stores have no deterministic forwarding order.
            // Keep forwarding bursts non-overlapping while retaining zero-
            // stride coverage for independent loads in random-mixed.
            if (transaction.store) {
                const std::array<std::int64_t, 5> store_strides{{
                    -static_cast<std::int64_t>(2U * element_bytes),
                    -static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(2U * element_bytes),
                    static_cast<std::int64_t>(4U * element_bytes)}};
                transaction.stride = store_strides[
                    random.next_shape() % store_strides.size()];
            } else {
                const std::array<std::int64_t, 6> load_strides{{
                    -static_cast<std::int64_t>(2U * element_bytes),
                    -static_cast<std::int64_t>(element_bytes), 0,
                    static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(2U * element_bytes),
                    static_cast<std::int64_t>(4U * element_bytes)}};
                transaction.stride = load_strides[
                    random.next_shape() % load_strides.size()];
            }
            return;
        }
        if (transaction.addressing != memblock::VectorAddressingMode::indexed_unordered &&
            transaction.addressing != memblock::VectorAddressingMode::indexed_ordered) {
            return;
        }
        std::vector<unsigned> slots(256U / element_bytes);
        std::iota(slots.begin(), slots.end(), 0U);
        if (transaction.store) {
            std::shuffle(slots.begin(), slots.end(), random.payload);
        }
        for (unsigned element = 0; element < elements; ++element) {
            const std::uint64_t slot = transaction.store
                ? slots[element]
                : random.next_shape() % slots.size();
            const std::uint64_t offset = slot * element_bytes;
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                transaction.index[element * element_bytes + byte] =
                    static_cast<unsigned char>(offset >> (8 * byte));
            }
        }
    };
    auto scalar_forward_value = [&](const memblock::StoreTransaction &store,
                                    const memblock::LoadTransaction &load) {
        const unsigned load_bytes = 1U <<
            (static_cast<unsigned>(load.op) & 3U);
        const unsigned store_bytes = 1U << static_cast<unsigned>(store.op);
        std::uint64_t value = 0;
        const std::uint64_t old = load.oracle_address.value_or(load.address);
        for (unsigned byte = 0; byte < load_bytes; ++byte) {
            std::uint8_t result = environment.memory().read_byte(old + byte);
            for (unsigned store_byte = 0; store_byte < store_bytes; ++store_byte) {
                if (old + byte == store.address + store_byte) {
                    result = static_cast<std::uint8_t>(
                        store.data >> (8 * store_byte));
                }
            }
            value |= std::uint64_t{result} << (8 * byte);
        }
        const bool is_unsigned = (static_cast<unsigned>(load.op) & 4U) != 0;
        return is_unsigned ? value : memblock::sign_extend(value, load_bytes * 8);
    };
    auto vector_forward_value = [&](const memblock::VectorMemoryTransaction &store,
                                    const memblock::VectorMemoryTransaction &load) {
        auto expected = environment.memory().expected_vector_load(load);
        std::unordered_map<std::uint64_t, unsigned char> forwarded;
        const unsigned store_bytes = 1U << store.eew;
        const unsigned store_elements = 16U >> store.eew;
        const std::uint16_t store_active = memblock::active_vector_elements(store);
        for (unsigned element = 0; element < store_elements; ++element) {
            if (((store_active >> element) & 1U) == 0) {
                continue;
            }
            const auto address = memblock::vector_element_address(store, element);
            for (unsigned byte = 0; byte < store_bytes; ++byte) {
                forwarded[address + byte] = store.data[element * store_bytes + byte];
            }
        }
        const unsigned load_bytes = 1U << load.eew;
        const unsigned load_elements = 16U >> load.eew;
        const std::uint16_t load_active = memblock::active_vector_elements(load);
        for (unsigned element = 0; element < load_elements; ++element) {
            if (((load_active >> element) & 1U) == 0) {
                continue;
            }
            const auto address = memblock::vector_element_address(load, element);
            for (unsigned byte = 0; byte < load_bytes; ++byte) {
                const auto it = forwarded.find(address + byte);
                if (it != forwarded.end()) {
                    expected[element * load_bytes + byte] = it->second;
                }
            }
        }
        return expected;
    };
    auto observe_outstanding = [&]() {
        const std::size_t scalar = environment.pending_scalar_loads();
        const std::size_t prefetch = environment.pending_prefetches();
        const std::size_t store = environment.pending_scalar_stores();
        const std::size_t vector_load = environment.pending_vector_loads();
        const std::size_t vector_store = environment.pending_vector_stores();
        const std::size_t total = scalar + prefetch + store + vector_load + vector_store;
        coverage.max_outstanding = std::max<std::uint64_t>(coverage.max_outstanding, total);
    };

    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_RANDOM_STRESS_FAIL seed=" << options.seed
                  << " transactions=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    while (completed_actions < options.transactions) {
        phase = "burst-" + std::to_string(burst_index);
        const unsigned remaining = options.transactions - completed_actions;
        const unsigned burst_actions = remaining >= 12 && burst_index >= 4 &&
                burst_index % 5 != 0
            ? 12
            : std::min(6U, remaining);
        const unsigned group_count = burst_actions > 6 ? 2 : 1;
        std::vector<Group> groups(group_count);
        std::vector<Action> issue_actions;
        issue_actions.reserve(burst_actions);
        const bool second_region = (burst_index & 1U) != 0;
        const std::uint64_t region = second_region ? region1 : region0;
        coverage.memory_regions = second_region ? 2 : std::max<std::uint64_t>(1, coverage.memory_regions);

        for (unsigned group_index = 0; group_index < group_count; ++group_index) {
            const unsigned slots = std::min(6U, burst_actions - group_index * 6);
            auto &group = groups[group_index];
            const unsigned eew = burst_index < 4
                ? burst_index
                : (group_count == 2 ? 1U + random.next_shape() % 3U
                                     : random.next_shape() % 4U);
            auto vector_mode = static_cast<memblock::VectorAddressingMode>(
                burst_index < 3 ? burst_index : random.next_shape() % 3);
            // Ordered indexed operations must see all older LSQ elements
            // retired. Keep that mode in the single-group prefix; later
            // two-group bursts still cover indexed unordered traffic while
            // preserving genuine outstanding pressure.
            if (group_count == 2 &&
                vector_mode == memblock::VectorAddressingMode::indexed_ordered) {
                vector_mode = memblock::VectorAddressingMode::indexed_unordered;
            }
            const auto store_mode = vector_mode ==
                    memblock::VectorAddressingMode::indexed_ordered
                ? memblock::VectorAddressingMode::indexed_unordered
                : vector_mode;
            const std::uint64_t base = region + (burst_index % 256) * 0x1000 +
                group_index * 0x400;
            const unsigned store_op_value = burst_index < 4
                ? burst_index
                : random.next_shape() % 4;
            // Keep the store/forwarding pair naturally aligned.  The scalar
            // load in front of it still alternates aligned and split-line
            // addresses, so the stress campaign retains misaligned DCache
            // traffic without depending on StoreQueue's known unsupported
            // unaligned-forwarding replay path.
            const auto store_address = base + 0x80;
            // ROB order intentionally puts an independent load before the
            // store, then creates dependent scalar/vector loads after it.
            // This gives pendingPtr a legal head while retaining real
            // forwarding checks for the younger loads.
            group.has_scalar_load = slots >= 1;
            group.has_scalar_store = slots >= 2;
            group.has_extra_load = slots >= 3;
            group.has_vector_store = slots >= 4;
            group.has_vector_load = slots >= 5;
            group.has_prefetch = slots >= 6;
            if (group.has_scalar_load) {
                const bool is_unsigned = store_op_value != 3 &&
                    (random.next_shape() & 1U) != 0;
                group.scalar_load = make_load(
                    base + 0x40 + ((burst_index & 1U) ? 1U : 0U),
                    static_cast<memblock::LoadOp>(store_op_value +
                        (is_unsigned ? 4U : 0U)),
                    (burst_index + group_index) % memblock::kScalarLoadLanes);
            }
            if (group.has_scalar_store) {
                group.scalar_store = make_store(
                    store_address, random.next_payload(),
                    static_cast<memblock::StoreOp>(store_op_value),
                    group_index % memblock::kScalarStoreLanes,
                    (group_index + 1) % memblock::kScalarStoreLanes);
            }
            if (group.has_extra_load) {
                group.extra_load = make_load(
                    store_address, static_cast<memblock::LoadOp>(store_op_value),
                    (burst_index + group_index + 2) % memblock::kScalarLoadLanes);
            }
            if (group.has_vector_store) {
                group.vector_store = make_vector(
                    true, base + 0x200 +
                        ((burst_index & 1U) ? (1U << eew) : 0U),
                    eew, (burst_index + group_index) % memblock::kVectorMemoryLanes,
                    store_mode);
                group.vector_store.vm = (burst_index % 2) == 0;
                group.vector_store.mask_bits = static_cast<std::uint16_t>(
                    random.next_shape());
                const unsigned elements = 16U >> eew;
                group.vector_store.vl = burst_index % 2 == 0
                    ? static_cast<std::uint8_t>(elements)
                    : static_cast<std::uint8_t>(
                        1U + random.next_shape() % elements);
                group.vector_store.vstart = burst_index % 3 == 1 &&
                        group.vector_store.vl != 0
                    ? static_cast<std::uint8_t>(std::min<unsigned>(1, group.vector_store.vl))
                    : 0;
                randomize_vector_addressing(group.vector_store);
            }
            if (group.has_vector_load) {
                group.vector_load = make_vector(
                    false, group.vector_store.address, eew,
                    group_index % memblock::kVectorMemoryLanes,
                    vector_mode);
                group.vector_load.vl = group.vector_store.vl;
                group.vector_load.vstart = group.vector_store.vstart;
                group.vector_load.vm = group.vector_store.vm;
                group.vector_load.mask_bits = group.vector_store.mask_bits;
                group.vector_load.vma = group.vector_store.vma;
                group.vector_load.vta = group.vector_store.vta;
                group.vector_load.stride = group.vector_store.stride;
                group.vector_load.index = group.vector_store.index;
                group.vector_load.flow_num = group.vector_store.flow_num;
            }
            if (group.has_prefetch) {
                group.prefetch = make_prefetch(
                    base + 0x380, static_cast<memblock::PrefetchOp>(8 + burst_index % 3),
                    (burst_index + group_index) % memblock::kScalarLoadLanes);
            }
            if (group.has_scalar_load) {
                environment.expect_load(group.scalar_load);
                if (!environment.enqueue_load(group.scalar_load)) {
                    break;
                }
                issue_actions.push_back({Kind::scalar_load, group_index});
            }
            if (group.has_scalar_store) {
                environment.expect_store(group.scalar_store);
                if (!environment.enqueue_store(
                        group.scalar_store, memblock::lq_pointer_value(lq_offset))) {
                    break;
                }
                issue_actions.push_back({Kind::scalar_store, group_index});
            }
            if (group.has_extra_load) {
                environment.expect_load_data(
                    group.extra_load,
                    scalar_forward_value(group.scalar_store, group.extra_load));
                if (!environment.enqueue_load(group.extra_load)) {
                    break;
                }
                issue_actions.push_back({Kind::extra_load, group_index});
            }
            if (group.has_vector_store) {
                environment.expect_vector(group.vector_store);
                if (!environment.enqueue_vector(group.vector_store)) {
                    break;
                }
                issue_actions.push_back({Kind::vector_store, group_index});
            }
            if (group.has_vector_load) {
                environment.expect_vector_data(
                    group.vector_load,
                    vector_forward_value(group.vector_store, group.vector_load));
                if (!environment.enqueue_vector(group.vector_load)) {
                    break;
                }
                issue_actions.push_back({Kind::vector_load, group_index});
            }
            if (group.has_prefetch) {
                environment.expect_prefetch(group.prefetch);
                if (!environment.enqueue_prefetch(group.prefetch)) {
                    break;
                }
                issue_actions.push_back({Kind::prefetch, group_index});
            }
        }
        if (!environment.ok() || issue_actions.size() != burst_actions) {
            break;
        }
        const Group &first = groups.front();
        const auto issue_head = first.has_scalar_load
            ? memblock::rob_identity(first.scalar_load.rob, first.scalar_load.rob_flag)
            : first.has_scalar_store
                ? memblock::rob_identity(first.scalar_store.rob, first.scalar_store.rob_flag)
                : first.has_extra_load
                    ? memblock::rob_identity(first.extra_load.rob, first.extra_load.rob_flag)
                    : first.has_vector_store
                        ? memblock::rob_identity(first.vector_store.rob, first.vector_store.rob_flag)
                        : first.has_vector_load
                            ? memblock::rob_identity(first.vector_load.rob, first.vector_load.rob_flag)
                            : memblock::rob_identity(first.prefetch.rob, first.prefetch.rob_flag);
        if (!environment.set_rob_head(issue_head.value, issue_head.flag)) {
            break;
        }
        std::vector<bool> issued(issue_actions.size(), false);
        unsigned issued_count = 0;
        while (issued_count < issue_actions.size()) {
            std::vector<unsigned> candidates;
            for (unsigned index = 0; index < issue_actions.size(); ++index) {
                if (issued[index]) {
                    continue;
                }
                const auto action = issue_actions[index];
                bool dependency_ready = true;
                if (action.kind == Kind::extra_load) {
                    for (unsigned prior = 0; prior < issue_actions.size(); ++prior) {
                        if (issue_actions[prior].group == action.group &&
                            issue_actions[prior].kind == Kind::scalar_store) {
                            dependency_ready = issued[prior];
                        }
                    }
                }
                if (action.kind == Kind::vector_load) {
                    for (unsigned prior = 0; prior < issue_actions.size(); ++prior) {
                        if (issue_actions[prior].group == action.group &&
                            issue_actions[prior].kind == Kind::vector_store) {
                            dependency_ready = issued[prior];
                        }
                    }
                }
                if (dependency_ready) {
                    candidates.push_back(index);
                }
            }
            if (candidates.empty()) {
                phase = "issue-dependency-deadlock";
                break;
            }
            const unsigned selected = candidates[
                random.next_schedule() % candidates.size()];
            const auto action = issue_actions[selected];
            auto &group = groups[action.group];
            bool ok = false;
            std::uint64_t requests_before = environment.tilelink_requests();
            switch (action.kind) {
            case Kind::scalar_store: {
                const bool data_first = (random.next_schedule() & 1U) != 0;
                ok = data_first
                    ? environment.issue_store_data(group.scalar_store, 2048) &&
                          environment.issue_store_address(group.scalar_store, 2048)
                    : environment.issue_store_address(group.scalar_store, 2048) &&
                          environment.issue_store_data(group.scalar_store, 2048);
                coverage.sample(group.scalar_store, data_first);
                break;
            }
            case Kind::scalar_load:
                ok = environment.issue_load(group.scalar_load, 2048);
                coverage.sample(group.scalar_load, requests_before,
                                environment.tilelink_requests());
                break;
            case Kind::vector_store:
                ok = environment.issue_vector(group.vector_store, 2048);
                coverage.sample(group.vector_store, requests_before,
                                environment.tilelink_requests());
                break;
            case Kind::vector_load:
                ok = environment.issue_vector(group.vector_load, 2048);
                coverage.sample(group.vector_load, requests_before,
                                environment.tilelink_requests());
                ++coverage.vector_forwarding;
                break;
            case Kind::prefetch:
                ok = environment.issue_prefetch(group.prefetch, 2048);
                coverage.sample(group.prefetch);
                break;
            case Kind::extra_load:
                ok = environment.issue_load(group.extra_load, 2048);
                coverage.sample(group.extra_load, requests_before,
                                environment.tilelink_requests());
                ++coverage.scalar_forwarding;
                break;
            }
            if (!ok || !environment.run_cycles(static_cast<unsigned>(
                    random.next_schedule() % 4))) {
                break;
            }
            issued[selected] = true;
            ++issued_count;
            observe_outstanding();
        }
        if (issued_count != issue_actions.size()) {
            break;
        }
        const Group &last = groups.back();
        const memblock::RobIdentity last_rob = last.has_prefetch
            ? memblock::rob_identity(last.prefetch.rob, last.prefetch.rob_flag)
            : last.has_vector_load
                ? memblock::rob_identity(last.vector_load.rob, last.vector_load.rob_flag)
                : last.has_vector_store
                    ? memblock::rob_identity(last.vector_store.rob, last.vector_store.rob_flag)
                    : last.has_extra_load
                        ? memblock::rob_identity(last.extra_load.rob, last.extra_load.rob_flag)
                        : last.has_scalar_store
                            ? memblock::rob_identity(last.scalar_store.rob, last.scalar_store.rob_flag)
                            : memblock::rob_identity(last.scalar_load.rob, last.scalar_load.rob_flag);
        if (!environment.set_rob_head(last_rob.value, last_rob.flag) ||
            !environment.run_until_all_complete(8192) ||
            !environment.run_until_queues_retired(8192) ||
            !environment.run_cycles(4)) {
            break;
        }
        for (const auto &group : groups) {
            if (group.has_scalar_store) {
                environment.record_committed_store(group.scalar_store);
            }
            if (group.has_vector_store) {
                environment.record_committed_vector_store(group.vector_store);
            }
        }
        bool masked_vector = false;
        bool unmasked_vector = false;
        bool strided_vector = false;
        bool indexed_vector = false;
        bool scalar_misaligned = false;
        bool scalar_forwarding = false;
        for (const auto &group : groups) {
            if (group.has_vector_store) {
                masked_vector |= !group.vector_store.vm;
                unmasked_vector |= group.vector_store.vm;
                strided_vector |= group.vector_store.addressing ==
                    memblock::VectorAddressingMode::strided;
                indexed_vector |= group.vector_store.addressing ==
                    memblock::VectorAddressingMode::indexed_unordered;
            }
            if (group.has_vector_load) {
                strided_vector |= group.vector_load.addressing ==
                    memblock::VectorAddressingMode::strided;
                indexed_vector |= group.vector_load.addressing ==
                    memblock::VectorAddressingMode::indexed_unordered;
            }
            if (group.has_scalar_load) {
                const unsigned bytes = 1U <<
                    (static_cast<unsigned>(group.scalar_load.op) & 3U);
                scalar_misaligned |= (group.scalar_load.address & (bytes - 1)) != 0;
            }
            if (group.has_scalar_store) {
                const unsigned bytes = 1U <<
                    static_cast<unsigned>(group.scalar_store.op);
                scalar_misaligned |= (group.scalar_store.address & (bytes - 1)) != 0;
            }
            scalar_forwarding |= group.has_extra_load;
        }
        // These counters represent actual generated crosses, rather than
        // phase indices, so a passing gate proves feature overlap occurred.
        if (group_count == 2 && masked_vector) {
            ++coverage.combinations[0];
        }
        if (group_count == 2 && unmasked_vector) {
            ++coverage.combinations[1];
        }
        if (scalar_misaligned && strided_vector) {
            ++coverage.combinations[2];
        }
        if (indexed_vector && scalar_forwarding) {
            ++coverage.combinations[3];
        }
        ++coverage.waves;
        coverage.actions += burst_actions;
        completed_actions += burst_actions;
        ++burst_index;
    }

    coverage.dcache_request_stalls = environment.dcache_request_stalls();
    coverage.dcache_response_delays = environment.dcache_response_delays();
    coverage.ptw_request_stalls = environment.ptw_request_stalls();
    coverage.ptw_response_delays = environment.ptw_response_delays();
    coverage.uncache_request_stalls = environment.uncache_request_stalls();
    coverage.uncache_response_delays = environment.uncache_response_delays();
    const bool passed = completed_actions == options.transactions &&
        environment.ok() && environment.dcache_grants_drained() &&
        coverage.complete() &&
        coverage.backpressure_complete(options.backpressure) &&
        environment.lq_dequeued() + environment.lq_canceled() == environment.lq_allocated() &&
        environment.sq_dequeued() + environment.sq_canceled() == environment.sq_allocated();
    if (!passed) {
        std::cerr << "MEMBLOCK_RANDOM_STRESS_FAIL seed=" << options.seed
                  << " transactions=" << completed_actions
                  << " requested=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " phase=" << phase
                  << " lq=" << environment.lq_dequeued() << '+'
                  << environment.lq_canceled() << '/' << environment.lq_allocated()
                  << " sq=" << environment.sq_dequeued() << '+'
                  << environment.sq_canceled() << '/' << environment.sq_allocated()
                  << " reason=" << (environment.error().empty()
                                         ? "stress_coverage_or_accounting_gate_failed"
                                         : environment.error())
                  << ' ' << coverage.summary() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_STRESS_PASS"
              << " seed=" << options.seed
              << " transactions=" << completed_actions
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " dcache_gets=" << environment.dcache_gets()
              << " dcache_refills=" << environment.dcache_refills()
              << " dcache_acquire_perms="
              << environment.dcache_acquire_perms()
              << " probes=" << environment.dcache_probes()
              << " grant_acks=" << environment.dcache_grant_acks()
              << " ptw_requests=" << environment.ptw_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " lq=" << environment.lq_dequeued() << '+'
              << environment.lq_canceled() << '/' << environment.lq_allocated()
              << " sq=" << environment.sq_dequeued() << '+'
              << environment.sq_canceled() << '/' << environment.sq_allocated()
              << ' ' << coverage.summary()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_boundary_hunt(int argc, char **argv, const Options &options)
{
    constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    constexpr std::uint64_t guest_physical = 0xb0000000ULL;
    constexpr std::uint64_t vs_root = 0x94000000ULL;
    constexpr std::uint64_t g_root = 0x95000000ULL;
    const unsigned samples = std::max(1U, options.transactions);
    std::mt19937_64 random(options.seed ^ 0x6a09e667f3bcc909ULL);
    unsigned failures = 0;
    for (unsigned sample = 0; sample < samples; ++sample) {
        memblock::Environment environment(argc, argv);
        const std::uint64_t backpressure_seed = random();
        environment.configure_backpressure(backpressure_seed, options.backpressure);
        if (!environment.reset() ||
            !environment.map_sv39_4k(guest_virtual, guest_physical, vs_root) ||
            !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
            !environment.activate_two_stage(vs_root, g_root, 7, 11)) {
            std::cerr << "MEMBLOCK_RANDOM_BOUNDARY_HUNT_FAIL seed="
                      << options.seed << " sample=" << sample
                      << " reason=configuration\n";
            return 1;
        }
        // Keep the fault in the VS L1 PTE page, but randomize all lower VA
        // bits so split-byte and active-element offsets are independently
        // explored for every seed/sample.
        const std::uint64_t address = guest_virtual + random() % 0x1000;
        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, address);
        if (reference.translated || !reference.guest_page_fault ||
            !reference.is_for_vs_nonleaf_pte) {
            std::cerr << "MEMBLOCK_RANDOM_BOUNDARY_HUNT_FAIL seed="
                      << options.seed << " sample=" << sample
                      << " reason=reference-walk address=0x" << std::hex
                      << address << std::dec << '\n';
            return 1;
        }
        const unsigned eew = static_cast<unsigned>(random() % 4);
        const unsigned elements = 16U >> eew;
        memblock::VectorMemoryTransaction transaction{
            .store = false,
            .address = address,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(1U + random() % elements),
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(1U + random() % 255),
            .lane = static_cast<unsigned>(random() % memblock::kVectorMemoryLanes),
            .flow_num = 2,
            .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
        };
        transaction.vm = (random() & 1U) != 0;
        transaction.mask_bits = static_cast<std::uint16_t>(random());
        transaction.vma = (random() & 1U) != 0;
        transaction.vta = (random() & 1U) != 0;
        transaction.vstart = static_cast<std::uint8_t>(
            transaction.vl == 0 ? 0 : random() % transaction.vl);
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(random());
        }
        if (!transaction.vm) {
            const std::uint32_t vl_mask = transaction.vl >= 16
                ? 0xffffU
                : ((std::uint32_t{1} << transaction.vl) - 1U);
            transaction.mask_bits &= static_cast<std::uint16_t>(vl_mask);
            if (memblock::active_vector_elements(transaction) == 0) {
                const unsigned first_active = transaction.vstart +
                    static_cast<unsigned>(random() %
                        (transaction.vl - transaction.vstart));
                transaction.mask_bits |= static_cast<std::uint16_t>(
                    1U << first_active);
            }
        }
        // Vary the inter-arrival point relative to PTW/DCache backpressure;
        // this does not alter the architectural oracle.
        const unsigned pre_issue_delay = static_cast<unsigned>(random() % 16);
        if (!environment.run_cycles(pre_issue_delay)) {
            ++failures;
            continue;
        }
        environment.expect_vector(transaction);
        // The vector exception VA is the address of the first active element
        // in the current micro-op.  The GPA oracle, in contrast, identifies
        // the page-table byte and must remain unshifted for VS-non-leaf faults.
        unsigned first_active_element = transaction.vstart;
        while (first_active_element < (16U >> transaction.eew) &&
               ((memblock::active_vector_elements(transaction) >>
                 first_active_element) & 1U) == 0) {
            ++first_active_element;
        }
        const std::uint64_t expected_vaddr = address +
            static_cast<std::uint64_t>(first_active_element << transaction.eew);
        bool passed =
            environment.set_rob_head(transaction.rob, transaction.rob_flag) &&
            environment.enqueue_vector(transaction) &&
            environment.issue_vector(transaction, 512) &&
            environment.run_until_vector_complete_with_replays(transaction, 16384) &&
            environment.run_until_lq_retired(2048) &&
            environment.exception_is_for_vs_nonleaf_pte() &&
            environment.exception_vaddr() == expected_vaddr &&
            environment.exception_gpaddr() ==
                reference.faulting_guest_physical_address;
        if (!passed) {
            ++failures;
            std::cerr << "MEMBLOCK_RANDOM_BOUNDARY_HUNT_SAMPLE_FAIL seed="
                      << options.seed << " sample=" << sample
                      << " address=0x" << std::hex << address
                      << " expected_vaddr=0x" << expected_vaddr
                      << " actual_vaddr=0x" << environment.exception_vaddr()
                      << " expected_vs_nonleaf=1 actual_vs_nonleaf="
                      << std::dec << environment.exception_is_for_vs_nonleaf_pte()
                      << " expected_gpaddr=0x"
                      << reference.faulting_guest_physical_address
                      << " actual_gpaddr=0x" << environment.exception_gpaddr()
                      << " eew=" << std::dec << eew
                      << " vl=" << static_cast<unsigned>(transaction.vl)
                      << " vstart=" << static_cast<unsigned>(transaction.vstart)
                      << " vm=" << transaction.vm
                      << " mask=0x" << std::hex << transaction.mask_bits
                      << " active=0x" << memblock::active_vector_elements(transaction)
                      << std::dec << " vma=" << transaction.vma
                      << " vta=" << transaction.vta
                      << " lane=" << transaction.lane
                      << " flow_num=" << static_cast<unsigned>(transaction.flow_num)
                      << " pre_issue_delay=" << pre_issue_delay
                      << " backpressure_seed=0x" << std::hex << backpressure_seed
                      << std::dec << " reason=" << environment.error() << '\n';
        }
    }
    std::cout << "MEMBLOCK_RANDOM_BOUNDARY_HUNT_"
              << (failures == 0 ? "PASS" : "FAIL")
              << " seed=" << options.seed << " transactions=" << samples
              << " failures=" << failures
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return failures == 0 ? 0 : 1;
}
