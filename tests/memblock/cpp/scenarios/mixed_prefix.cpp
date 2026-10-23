#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::run_prefix()
{
    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        return false;
    }
    if (constraints.uncache_error_per_mille != 0 ||
        constraints.dcache_load_error_per_mille != 0 ||
        constraints.atomic_error_per_mille != 0) {
        environment.configure_cache_error_enable(true);
        if (!environment.run_cycles(4)) {
            return false;
        }
    }

    phase = "bare-heterogeneous-wave";
    std::vector<memblock::LoadTransaction> scalar_wave;
    std::vector<unsigned> scalar_lanes(memblock::kScalarLoadLanes);
    std::iota(scalar_lanes.begin(), scalar_lanes.end(), 0U);
    std::shuffle(scalar_lanes.begin(), scalar_lanes.end(), random);
    for (const unsigned lane : scalar_lanes) {
        const auto op = static_cast<memblock::LoadOp>(random() % 7);
        const unsigned size = 1U << (static_cast<unsigned>(op) & 3U);
        scalar_wave.push_back(make_load(
            random_bare_address(bare_base + 0x200, size, 0x1000 - size),
            op, lane));
        environment.expect_load(scalar_wave.back());
        if (!environment.enqueue_load(scalar_wave.back())) {
            return false;
        }
    }
    std::array<memblock::VectorMemoryTransaction, 2> vector_wave{
        make_vector(false, bare_base + 0x1000 + (random() % 64) * 16,
            random() % 4, random() % memblock::kVectorMemoryLanes),
        make_vector(false, bare_base + 0x3000 + (random() % 128) * 8,
            random() % 4, random() % memblock::kVectorMemoryLanes),
    };
    for (auto &transaction : vector_wave) {
        const unsigned elements = 16U >> transaction.eew;
        transaction.vl = static_cast<std::uint8_t>(
            1U + random() % elements);
        transaction.vm = (random() & 1U) != 0;
        transaction.mask_bits = static_cast<std::uint16_t>(random());
    }
    for (auto &transaction : vector_wave) {
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction)) {
            return false;
        }
    }
    if (!environment.issue_load_batch(scalar_wave)) {
        return false;
    }
    for (const auto &transaction : vector_wave) {
        if (!environment.issue_vector(transaction)) {
            return false;
        }
    }
    if (!environment.run_cycles(random_delay(0, 7)) ||
        !environment.run_until_all_complete(2048)) {
        return false;
    }
    for (const auto &transaction : scalar_wave) {
        coverage.sample(transaction);
    }
    for (const auto &transaction : vector_wave) {
        coverage.sample(transaction);
    }
    actions += scalar_wave.size() + vector_wave.size();
    ++coverage.heterogeneous_waves;
    coverage.max_outstanding = 5;
    ++coverage.cacheable;

    phase = "scalar-load-op-wave";
    std::vector<memblock::LoadTransaction> load_sweep;
    std::vector<unsigned> load_ops(7);
    std::iota(load_ops.begin(), load_ops.end(), 0U);
    std::shuffle(load_ops.begin(), load_ops.end(), random);
    for (const unsigned op : load_ops) {
        const unsigned size = 1U << (op & 3U);
        const std::uint64_t region = bare_base + 0x4000 +
            (random() % 28U) * 64;
        load_sweep.push_back(make_load(
            random_bare_address(region, size, 64 - size),
            static_cast<memblock::LoadOp>(op), 0));
    }
    for (unsigned begin = 0; begin < load_sweep.size(); begin += 3) {
        std::vector<memblock::LoadTransaction> batch;
        std::array<unsigned, 3> batch_lanes{{0, 1, 2}};
        std::shuffle(batch_lanes.begin(), batch_lanes.end(), random);
        for (unsigned index = begin;
             index < std::min<unsigned>(begin + 3, load_sweep.size()); ++index) {
            load_sweep[index].lane = batch_lanes[index - begin];
            environment.expect_load(load_sweep[index]);
            if (!environment.enqueue_load(load_sweep[index])) {
                return false;
            }
            batch.push_back(load_sweep[index]);
        }
        if (!environment.issue_load_batch(batch)) {
            return false;
        }
    }
    if (!environment.run_until_all_complete(2048)) {
        return false;
    }
    for (const auto &transaction : load_sweep) {
        coverage.sample(transaction);
    }
    actions += load_sweep.size();
    coverage.max_outstanding = std::max<std::uint64_t>(
        coverage.max_outstanding, load_sweep.size());

    phase = "multi-lane-lsq-dispatch";
    for (unsigned width = 1;
         width <= memblock::generated::kLsqEnqueueLanes; ++width) {
        std::vector<unsigned> dispatch_lanes(
            memblock::generated::kLsqEnqueueLanes);
        std::iota(dispatch_lanes.begin(), dispatch_lanes.end(), 0U);
        std::shuffle(dispatch_lanes.begin(), dispatch_lanes.end(), random);
        dispatch_lanes.resize(width);
        std::sort(dispatch_lanes.begin(), dispatch_lanes.end());

        std::vector<memblock::LoadTransaction> batch;
        batch.reserve(width);
        for (unsigned index = 0; index < width; ++index) {
            const auto op = static_cast<memblock::LoadOp>((width + index) % 7);
            const unsigned size = 1U <<
                (static_cast<unsigned>(op) & 3U);
            batch.push_back(make_load(
                random_bare_address(
                    bare_base + 0x5000 + width * 0x100, size, 0x80),
                op, index % memblock::kScalarLoadLanes));
            environment.expect_load(batch.back());
        }
        if (!environment.enqueue_load_batch(batch, dispatch_lanes)) {
            return false;
        }
        for (unsigned begin = 0; begin < batch.size();
             begin += memblock::kScalarLoadLanes) {
            const unsigned end = std::min<unsigned>(
                begin + memblock::kScalarLoadLanes, batch.size());
            const std::vector<memblock::LoadTransaction> issue_batch(
                batch.begin() + begin, batch.begin() + end);
            if (!environment.issue_load_batch(issue_batch)) {
                return false;
            }
        }
        if (!environment.run_until_all_complete(2048)) {
            return false;
        }
        for (const auto &transaction : batch) {
            coverage.sample(transaction);
        }
        actions += batch.size();
        coverage.cacheable += batch.size();
        coverage.max_outstanding = std::max<std::uint64_t>(
            coverage.max_outstanding, batch.size());
    }

    phase = "vector-load-shape-wave";
    std::vector<memblock::VectorMemoryTransaction> vector_sweep;
    std::vector<unsigned> load_eews{0, 1, 2, 3};
    std::shuffle(load_eews.begin(), load_eews.end(), random);
    for (unsigned shape = 0; shape < load_eews.size(); ++shape) {
        const unsigned eew = load_eews[shape];
        const unsigned element_bytes = 1U << eew;
        auto transaction = make_vector(
            false,
            random_bare_address(bare_base + 0x8000 + random() % 0x700,
                element_bytes, 128) +
                (random() % 4) * element_bytes,
            eew, random() % memblock::kVectorMemoryLanes);
        const unsigned elements = 16U >> eew;
        transaction.vm = shape == 0 ? false : (shape == 1 ? true : (random() & 1U) != 0);
        transaction.mask_bits = static_cast<std::uint16_t>(random());
        transaction.vl = static_cast<std::uint8_t>(
            shape == 1 || shape == 2
                ? elements
                : shape == 3 ? elements - 1U
                             : 1U + random() % elements);
        transaction.vstart = static_cast<std::uint8_t>(
            shape == 2 && transaction.vl > 1
                ? 1U + random() % transaction.vl
                : 0U);
        if (!transaction.vm && shape == 0) {
            transaction.mask_bits &= static_cast<std::uint16_t>(~1U);
            if (memblock::active_vector_elements(transaction) == 0) {
                transaction.mask_bits |= static_cast<std::uint16_t>(1U <<
                    (transaction.vl == 0 ? 0 : transaction.vl - 1));
            }
        }
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction)) {
            return false;
        }
        vector_sweep.push_back(transaction);
    }
    for (const auto &transaction : vector_sweep) {
        if (!environment.issue_vector(transaction)) {
            return false;
        }
    }
    if (!environment.run_until_all_complete(2048)) {
        return false;
    }
    for (const auto &transaction : vector_sweep) {
        coverage.sample(transaction);
    }
    actions += vector_sweep.size();
    coverage.max_outstanding = std::max<std::uint64_t>(
        coverage.max_outstanding, vector_sweep.size());

    phase = "advanced-addressing-wave";
    const auto misaligned = make_load(
        random_bare_address(bare_base + 0x9000, 1, 0x80) | 0x3d,
        memblock::LoadOp::ld, random() % memblock::kScalarLoadLanes);
    if (!environment.set_rob_head(misaligned.rob, misaligned.rob_flag) ||
        !issue_load(misaligned)) {
        return false;
    }
    ++coverage.cacheable;

    const unsigned indexed_eew = 0;
    auto indexed_store = make_vector(
        true,
        bare_base + 0x7000,
        indexed_eew,
        1,
        memblock::VectorAddressingMode::indexed_unordered);
    for (unsigned element = 0; element < indexed_store.index.size(); ++element) {
        indexed_store.index[element] = static_cast<unsigned char>((element * 11) & 63);
    }
    if (!issue_vector(indexed_store)) {
        return false;
    }
    auto indexed_load = make_vector(
        false,
        indexed_store.address,
        indexed_store.eew,
        0,
        indexed_store.addressing);
    indexed_load.index = indexed_store.index;
    if (!issue_vector(
            indexed_load,
            overlay_vector_store(indexed_store, indexed_load)) ||
        !environment.commit_vector_store(indexed_store, 4096)) {
        return false;
    }
    ++coverage.vector_forwarding;
    coverage.cacheable += 2;

    auto indexed_ordered_store = make_vector(
        true, bare_base + 0x7400, 2, 0,
        memblock::VectorAddressingMode::indexed_ordered);
    for (unsigned element = 0; element < indexed_ordered_store.index.size(); ++element) {
        indexed_ordered_store.index[element] = static_cast<unsigned char>((element * 7) & 63);
    }
    if (!issue_vector(indexed_ordered_store)) {
        return false;
    }
    auto indexed_ordered_load = make_vector(
        false, indexed_ordered_store.address, indexed_ordered_store.eew, 1,
        indexed_ordered_store.addressing);
    indexed_ordered_load.index = indexed_ordered_store.index;
    if (!issue_vector(
            indexed_ordered_load,
            overlay_vector_store(indexed_ordered_store, indexed_ordered_load)) ||
        !environment.commit_vector_store(indexed_ordered_store, 4096)) {
        return false;
    }
    ++coverage.vector_forwarding;
    coverage.cacheable += 2;

    phase = "vector-stride-signs";
    constexpr std::array<std::int64_t, 3> load_strides{{-4, 0, 4}};
    for (unsigned index = 0; index < load_strides.size(); ++index) {
        auto load = make_vector(
            false, bare_base + 0x7800 + index * 0x80 + 0x30, 2,
            index % memblock::kVectorMemoryLanes,
            memblock::VectorAddressingMode::strided);
        load.stride = load_strides[index];
        if (!issue_vector(load)) {
            return false;
        }
        ++coverage.cacheable;
    }
    constexpr std::array<std::int64_t, 2> store_strides{{-4, 4}};
    for (unsigned index = 0; index < store_strides.size(); ++index) {
        auto store = make_vector(
            true, bare_base + 0x7a00 + index * 0x80 + 0x30, 2,
            index % memblock::kVectorMemoryLanes,
            memblock::VectorAddressingMode::strided);
        store.stride = store_strides[index];
        if (!issue_vector(store) || !environment.commit_vector_store(store)) {
            return false;
        }
        ++coverage.cacheable;
    }

    phase = "scalar-forwarding";
    std::vector<unsigned> store_ops{0, 1, 2, 3};
    std::shuffle(store_ops.begin(), store_ops.end(), random);
    for (const unsigned op : store_ops) {
        const unsigned size = 1U << op;
        const bool is_unsigned = op != 3 && (random() & 1U) != 0;
        const std::uint64_t region = bare_base + 0x8000 +
            (random() % 28U) * 64;
        const auto store = make_store(
            random_bare_address(region, size, 64 - size), random(),
            static_cast<memblock::StoreOp>(op), op % 2, (op + 1) % 2);
        const bool data_first = (random() & 1U) != 0;
        if (!issue_store(store, data_first)) {
            return false;
        }
        const auto load = make_load(
            store.address,
            static_cast<memblock::LoadOp>(op + (is_unsigned ? 4U : 0U)),
            op % 3);
        if (!issue_load(load, scalar_forward_value(store.data, size, is_unsigned)) ||
            !environment.commit_store(store)) {
            return false;
        }
        ++coverage.scalar_forwarding;
    }

    phase = "vector-forwarding";
    std::vector<unsigned> store_eews{0, 1, 2, 3};
    std::shuffle(store_eews.begin(), store_eews.end(), random);
    for (const unsigned eew : store_eews) {
        const unsigned element_bytes = 1U << eew;
        const std::uint64_t vector_region = bare_base + 0xc000 +
            (random() % 28U) * 64;
        auto store = make_vector(
            true,
            vector_region + (random() % (16U / element_bytes)) * element_bytes,
            eew, random() % 2, memblock::VectorAddressingMode::unit_stride);
        const unsigned elements = 16U >> eew;
        store.vm = (random() & 1U) != 0;
        store.mask_bits = static_cast<std::uint16_t>(random());
        store.vl = static_cast<std::uint8_t>(1U + random() % elements);
        store.vstart = static_cast<std::uint8_t>(
            store.vl > 1 && (random() & 1U) ? random() % store.vl : 0U);
        if (!issue_vector(store)) {
            return false;
        }
        auto load = make_vector(false, store.address, eew, random() % 2,
            store.addressing);
        load.vl = store.vl;
        load.vstart = store.vstart;
        load.vm = store.vm;
        load.mask_bits = store.mask_bits;
        load.vma = store.vma;
        load.vta = store.vta;
        load.stride = store.stride;
        load.index = store.index;
        const auto expected = overlay_vector_store(store, load);
        if (!issue_vector(load, expected) ||
            !environment.commit_vector_store(store)) {
            return false;
        }
        ++coverage.vector_forwarding;
    }

    phase = "vector-misaligned-store";
    auto misaligned_vector_store = make_vector(
        true,
        bare_base + 0xe005,
        3,
        0,
        memblock::VectorAddressingMode::strided);
    misaligned_vector_store.stride = 8;
    misaligned_vector_store.vl = 2;
    const std::uint64_t replays_before =
        environment.vector_replay_feedbacks();
    environment.expect_vector(misaligned_vector_store);
    if (!environment.set_rob_head(
            misaligned_vector_store.rob,
            misaligned_vector_store.rob_flag) ||
        !environment.enqueue_vector(misaligned_vector_store) ||
        !environment.issue_vector(misaligned_vector_store, 256) ||
        !environment.run_cycles(32) ||
        !environment.pulse_pending_store(
            misaligned_vector_store.rob,
            misaligned_vector_store.rob_flag) ||
        !environment.run_until_vector_complete_with_replays(
            misaligned_vector_store, 4096) ||
        !environment.commit_vector_store(
            misaligned_vector_store, 4096)) {
        return false;
    }
    coverage.sample(misaligned_vector_store);
    ++actions;
    coverage.vector_replays +=
        environment.vector_replay_feedbacks() - replays_before;
    auto misaligned_vector_load = make_vector(
        false,
        misaligned_vector_store.address,
        misaligned_vector_store.eew,
        1,
        misaligned_vector_store.addressing);
    misaligned_vector_load.stride = misaligned_vector_store.stride;
    misaligned_vector_load.vl = misaligned_vector_store.vl;
    if (!environment.set_rob_head(
            misaligned_vector_load.rob,
            misaligned_vector_load.rob_flag) ||
        !issue_vector(
            misaligned_vector_load, misaligned_vector_store.data)) {
        return false;
    }
    coverage.cacheable += 2;

    phase = "scalar-misaligned-store";
    const auto misaligned_store = make_store(
        bare_base + 0xf045,
        0xa5c33c5af00f6996ULL,
        memblock::StoreOp::sd,
        1,
        0);
    environment.expect_store(misaligned_store);
    if (!environment.set_rob_head(
            misaligned_store.rob, misaligned_store.rob_flag) ||
        !environment.enqueue_store(
            misaligned_store, memblock::lq_pointer_value(lq_offset)) ||
        !environment.issue_store_address(misaligned_store, 256) ||
        !environment.issue_store_data(misaligned_store, 256) ||
        !environment.pulse_pending_store(
            misaligned_store.rob, misaligned_store.rob_flag) ||
        !environment.run_until_all_complete(4096) ||
        !environment.commit_store(misaligned_store, 4096)) {
        return false;
    }
    coverage.sample(misaligned_store, false);
    ++actions;
    const auto misaligned_store_readback = make_load(
        misaligned_store.address, memblock::LoadOp::ld, 2);
    if (!environment.set_rob_head(
            misaligned_store_readback.rob,
            misaligned_store_readback.rob_flag) ||
        !issue_load(
            misaligned_store_readback, misaligned_store.data)) {
        return false;
    }
    coverage.cacheable += 2;

    phase = "cross-forwarding-wave";
    const auto scalar_store = make_store(
        bare_base + 0x12008, 0xfedcba9876543210ULL,
        memblock::StoreOp::sd, 0, 1);
    auto vector_store = make_vector(true, bare_base + 0x12100, 0, 1);
    vector_store.vm = true;
    environment.expect_store(scalar_store);
    environment.expect_vector(vector_store);
    if (!environment.enqueue_store(
            scalar_store, memblock::lq_pointer_value(lq_offset)) ||
        !environment.enqueue_vector(vector_store) ||
        !environment.issue_store_data(scalar_store) ||
        !environment.issue_store_address(scalar_store) ||
        !environment.issue_vector(vector_store) ||
        !environment.run_until_all_complete(1024)) {
        return false;
    }
    coverage.sample(scalar_store, true);
    coverage.sample(vector_store);
    actions += 2;
    auto vector_load = make_vector(false, bare_base + 0x12000, 0, 0);
    auto vector_expected = environment.memory().expected_vector_load(vector_load);
    for (unsigned byte = 0; byte < 8; ++byte) {
        vector_expected[8 + byte] = static_cast<unsigned char>(
            scalar_store.data >> (8 * byte));
    }
    const auto scalar_load = make_load(
        vector_store.address + 8, memblock::LoadOp::ld, 2);
    std::uint64_t scalar_expected = 0;
    for (unsigned byte = 0; byte < 8; ++byte) {
        scalar_expected |= std::uint64_t{vector_store.data[8 + byte]}
            << (8 * byte);
    }
    environment.expect_vector_data(vector_load, vector_expected);
    environment.expect_load_data(scalar_load, scalar_expected);
    if (!environment.enqueue_vector(vector_load) ||
        !environment.enqueue_load(scalar_load) ||
        !environment.issue_load_vector_pair(scalar_load, vector_load) ||
        !environment.run_until_all_complete(2048) ||
        !environment.commit_store(scalar_store) ||
        !environment.commit_vector_store(vector_store)) {
        return false;
    }
    coverage.sample(vector_load);
    coverage.sample(scalar_load);
    actions += 2;
    ++coverage.heterogeneous_waves;
    ++coverage.simultaneous_scalar_vector;
    ++coverage.scalar_to_vector;
    ++coverage.vector_to_scalar;

    phase = "bare-queue-retirement";
    if (!environment.run_until_queues_retired(2048)) {
        return false;
    }

    phase = "two-stage-translation";
    if (!environment.map_sv39_4k(
            guest_virtual, guest_physical, vs_root) ||
        !environment.map_sv39_4k(
            guest_fault_virtual, guest_physical, vs_root) ||
        !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
        !environment.map_sv39x4_4k(
            vs_root + 0x1000, vs_root + 0x1000, g_root) ||
        !environment.map_sv39x4_4k(
            vs_root + 0x2000, vs_root + 0x2000, g_root) ||
        !environment.map_sv39x4_4k(
            guest_physical, host_physical, g_root) ||
        !environment.activate_two_stage(vs_root, g_root, 3, 5)) {
        return false;
    }
    const auto guest_reference = memblock::reference_two_stage_walk(
        environment.memory(), vs_root, g_root, guest_virtual + 0x188);
    if (!guest_reference.translated) {
        return false;
    }
    auto guest_cold = make_load(
        guest_virtual + 0x188, memblock::LoadOp::ld, 0);
    guest_cold.oracle_address = guest_reference.physical_address;
    if (!issue_load(guest_cold)) {
        return false;
    }
    const std::uint64_t ptw_after_guest_cold = environment.ptw_requests();
    auto guest_warm = make_load(
        guest_virtual + 0x188, memblock::LoadOp::lwu, 1);
    guest_warm.oracle_address = host_physical + 0x188;
    if (!issue_load(guest_warm) ||
        environment.ptw_requests() != ptw_after_guest_cold ||
        !environment.run_until_lq_retired(2048)) {
        return false;
    }
    coverage.two_stage += 2;
    coverage.cacheable += 2;
    ++coverage.tlb_reuse;

    phase = "vector-guest-page-fault";
    const auto guest_fault_reference = memblock::reference_two_stage_walk(
        environment.memory(), vs_root, g_root, guest_fault_virtual + 0x180);
    if (guest_fault_reference.translated ||
        !guest_fault_reference.guest_page_fault ||
        !guest_fault_reference.is_for_vs_nonleaf_pte) {
        return false;
    }
    auto guest_fault = make_vector(
        false, guest_fault_virtual + 0x180, 3, 0);
    guest_fault.expected_exception_mask =
        memblock::kExceptionLoadGuestPageFault;
    if (!environment.set_rob_head(
            guest_fault.rob, guest_fault.rob_flag) ||
        !issue_vector(guest_fault) ||
        !environment.run_cycles(8)) {
        phase += ":completion";
        return false;
    }
    if (!environment.exception_is_for_vs_nonleaf_pte() ||
        environment.exception_vaddr() != guest_fault.address ||
        environment.exception_gpaddr() !=
            guest_fault_reference.faulting_guest_physical_address) {
        if (environment.error().empty()) {
            phase += ":metadata";
        }
        return false;
    }
    if (!environment.redirect_after(
            guest_fault.rob, guest_fault.rob_flag, true, true)) {
        phase += ":trap-redirect";
        return false;
    }
    if (!environment.run_until_lq_retired(2048)) {
        phase += ":queue-retirement";
        return false;
    }
    ++coverage.two_stage;
    ++coverage.exceptions;

    // Boundary-hunt mode deliberately fans out the VS-non-leaf fault
    // across vector widths and 16-byte offsets.  The ordinary green
    // mixed regression keeps this disabled because the clean baseline
    // contains a known split-GPA defect; the hunt runner records that
    // architectural mismatch as evidence instead of hiding it.
    if (options.hunt_boundaries) {
        phase = "vector-guest-page-fault-boundary-hunt";
        // Constrain the generator to the VS L1 PTE page while randomizing
        // the split byte, EEW, mask, vstart, and vl.  No single historical
        // offset is required: every candidate is checked against the
        // software two-stage walk and a mismatch is a genuine oracle hit.
        const unsigned attempts = 8 + static_cast<unsigned>(random() % 9);
        for (unsigned index = 0; index < attempts; ++index) {
            const unsigned eew = static_cast<unsigned>(random() % 4);
            const unsigned element_count = 16U >> eew;
            // Pick any offset in the faulting page.  The independent
            // two-stage walk identifies the VS-non-leaf PTE GPA; the
            // vector width/mask then determines whether the internal
            // operation crosses a beat boundary.
            const std::uint64_t offset = random() % 0x1000U;
            const std::uint64_t address = guest_fault_virtual + offset;
            const auto reference = memblock::reference_two_stage_walk(
                environment.memory(), vs_root, g_root, address);
            if (reference.translated || !reference.guest_page_fault ||
                !reference.is_for_vs_nonleaf_pte) {
                return false;
            }
            auto candidate = make_vector(
                false, address, eew, random() % 2);
            candidate.vl = static_cast<std::uint8_t>(
                element_count == 1 ? 1U : 2U + random() % (element_count - 1));
            candidate.vstart = 0;
            // A masked vector with element zero disabled creates a
            // randomized non-zero firstUnmask offset; vm=true exercises
            // the zero-offset control.  Both remain legal completed-uop
            // shapes, unlike randomizing vstart on a faulting uop.
            candidate.vm = (random() & 3U) == 0;
            candidate.mask_bits = static_cast<std::uint16_t>(random());
            if (!candidate.vm) {
                const std::uint32_t vl_mask = candidate.vl >= 16
                    ? 0xffffU
                    : ((std::uint32_t{1} << candidate.vl) - 1U);
                candidate.mask_bits &= static_cast<std::uint16_t>(vl_mask);
                candidate.mask_bits &= static_cast<std::uint16_t>(~1U);
                if (memblock::active_vector_elements(candidate) == 0) {
                    const unsigned element = 1U +
                        static_cast<unsigned>(random() % (candidate.vl - 1));
                    candidate.mask_bits |= static_cast<std::uint16_t>(1U << element);
                }
            }
            // Vector memory faults report the VA of the first active
            // element selected by vstart/mask.  The VS-non-leaf GPA is
            // different: it remains the exact page-table byte reported
            // by the two-stage walk and is never element-offset.
            unsigned first_active_element = 0;
            while (first_active_element < element_count &&
                ((memblock::active_vector_elements(candidate) >>
                     first_active_element) & 1U) == 0) {
                ++first_active_element;
            }
            if (first_active_element == element_count) {
                return false;
            }
            const std::uint64_t expected_exception_vaddr = address +
                static_cast<std::uint64_t>(first_active_element << eew);
            candidate.expected_exception_mask =
                memblock::kExceptionLoadGuestPageFault;
            if (!environment.set_rob_head(candidate.rob, candidate.rob_flag) ||
                !issue_vector(candidate) ||
                !environment.run_cycles(8)) {
                return false;
            }
            if (!environment.exception_is_for_vs_nonleaf_pte() ||
                environment.exception_vaddr() != expected_exception_vaddr ||
                environment.exception_gpaddr() !=
                    reference.faulting_guest_physical_address) {
                if (environment.error().empty()) {
                    std::cerr << "MEMBLOCK_RANDOM_MIXED_FAIL seed=" << options.seed
                              << " transactions=" << actions
                              << " requested=" << options.transactions
                              << " cycle=" << environment.cycle()
                              << " phase=" << phase
                              << " boundary_address=0x" << std::hex << address
                              << " expected_vaddr=0x" << expected_exception_vaddr
                              << " actual_vaddr=0x" << environment.exception_vaddr()
                              << " expected_gpaddr=0x"
                              << reference.faulting_guest_physical_address
                              << " actual_gpaddr=0x" << environment.exception_gpaddr()
                              << std::dec << "\n";
                }
                return false;
            }
            if (!environment.redirect_after(
                    candidate.rob, candidate.rob_flag, true, true) ||
                !environment.run_until_lq_retired(2048)) {
                phase += ":trap-recovery";
                return false;
            }
            ++coverage.two_stage;
            ++coverage.exceptions;
        }
    }

    phase = "sv39-configuration";
    if (!environment.configure_sv39(cache0_base, cache0_base) ||
        !environment.configure_sv39(cache1_base, cache1_base) ||
        !environment.configure_sv39_nc(nc_base, nc_base) ||
        !environment.map_sv39_4k(
            mmio_virtual, mmio_physical, sv39_root,
            true, true, false, false, false, true) ||
        !environment.activate_sv39(sv39_root)) {
        return false;
    }

    phase = "software-prefetch";
    std::vector<unsigned> prefetch_types{0, 1, 2};
    std::shuffle(prefetch_types.begin(), prefetch_types.end(), random);
    for (const unsigned type : prefetch_types) {
        const auto prefetch = make_prefetch(
            cache0_base + 0x5000 + (random() % 8) * 0x100,
            static_cast<memblock::PrefetchOp>(8 + type), type % 3);
        environment.expect_prefetch(prefetch);
        if (!environment.enqueue_prefetch(prefetch) ||
            !environment.issue_prefetch(prefetch, 256) ||
            !environment.run_until_all_complete(2048)) {
            return false;
        }
        coverage.sample(prefetch);
        ++coverage.cacheable;
        ++actions;
    }

    phase = "sv39-coissue";
    const std::uint64_t requests_before = environment.tilelink_requests();
    const auto translated_scalar = make_load(
        cache0_base + 0x1000, memblock::LoadOp::lwu, 1);
    auto translated_vector = make_vector(
        false, cache1_base + 0x1000, 2, 0);
    environment.expect_load(translated_scalar);
    environment.expect_vector(translated_vector);
    if (!environment.enqueue_load(translated_scalar) ||
        !environment.enqueue_vector(translated_vector) ||
        !environment.issue_load_vector_pair(
            translated_scalar, translated_vector, 256) ||
        !environment.run_until_all_complete(4096)) {
        return false;
    }
    coverage.sample(translated_scalar);
    coverage.sample(translated_vector);
    actions += 2;
    coverage.cacheable += 2;
    ++coverage.heterogeneous_waves;
    ++coverage.simultaneous_scalar_vector;
    if (environment.tilelink_requests() > requests_before) {
        ++coverage.dcache_misses;
    }

    phase = "dcache-tlb-hit";
    const std::uint64_t hit_requests_before = environment.tilelink_requests();
    const auto hit = make_load(
        translated_scalar.address, memblock::LoadOp::ld, 0);
    if (!issue_load(hit)) {
        return false;
    }
    ++coverage.cacheable;
    ++coverage.tlb_reuse;
    if (environment.tilelink_requests() == hit_requests_before) {
        ++coverage.dcache_hits;
    }

    phase = "exception-contracts";
    auto page_fault = make_load(
        0x50003000ULL, memblock::LoadOp::ld, 0);
    page_fault.expected_exception_mask = memblock::kExceptionLoadPageFault;
    if (!issue_load(page_fault) ||
        !environment.redirect_after(
            page_fault.rob, page_fault.rob_flag, true)) {
        return false;
    }
    const auto prefetch_fault = make_prefetch(
        0x50004000ULL, memblock::PrefetchOp::read, 1);
    environment.expect_prefetch(prefetch_fault);
    if (!environment.enqueue_prefetch(prefetch_fault) ||
        !environment.issue_prefetch(prefetch_fault, 256) ||
        !environment.run_until_all_complete(4096)) {
        return false;
    }
    coverage.sample(prefetch_fault);
    ++actions;
    coverage.exceptions += 2;

    phase = "pbmt-nc-store-load";
    auto nc_store = make_store(
        nc_base + 0x180, 0x13579bdf2468ace0ULL,
        memblock::StoreOp::sd, 1, 0);
    nc_store.expected_debug_is_mmio = false;
    // PBMT=NC still targets main memory; NCIO is reserved for an
    // uncached access whose PMA class is IO.
    nc_store.expected_debug_is_ncio = false;
    environment.expect_store(nc_store);
    if (!environment.enqueue_store(
            nc_store, memblock::lq_pointer_value(lq_offset)) ||
        !environment.warm_store_translation(nc_store, 2048) ||
        !environment.issue_store_data(nc_store) ||
        !environment.run_until_all_complete(2048)) {
        return false;
    }
    coverage.sample(nc_store, false);
    ++actions;
    if (
        !environment.commit_store(nc_store, 2048)) {
        return false;
    }
    ++coverage.noncacheable;
    const auto nc_load = make_load(nc_store.address, memblock::LoadOp::ld, 2);
    if (!issue_load(nc_load, nc_store.data)) {
        return false;
    }
    ++coverage.noncacheable;

    phase = "dcache-dirty-pressure";
    constexpr unsigned dcache_sets = 128;
    constexpr unsigned dcache_ways = 8;
    constexpr unsigned line_bytes = 64;
    constexpr unsigned dirty_lines = 10;
    constexpr std::uint64_t same_set_stride = dcache_sets * line_bytes;
    constexpr std::uint64_t dirty_base = cache0_base + 0x30000;
    release_before = environment.tilelink_release_data();
    const std::uint64_t verified_release_before =
        environment.tilelink_release_data_verified();
    std::array<memblock::StoreTransaction, dirty_lines> dirty_stores{};
    for (unsigned index = 0; index < dirty_lines; ++index) {
        const std::uint64_t line = dirty_base + index * same_set_stride;
        dirty_stores[index] = make_store(
            line + 24, 0xc001d00d00000000ULL | index,
            memblock::StoreOp::sd, index % 2, (index + 1) % 2);
        std::vector<unsigned char> expected_line =
            environment.memory().read_beat(line, line_bytes);
        for (unsigned byte = 0; byte < sizeof(std::uint64_t); ++byte) {
            expected_line[24 + byte] = static_cast<unsigned char>(
                dirty_stores[index].data >> (8 * byte));
        }
        // Snapshot the architectural line before the DUT can evict it.
        // The TileLink agent compares ReleaseData against this immutable
        // image before updating its backing memory model.
        environment.expect_release_line(line, expected_line);
        if (index == 0) {
            environment.expect_store(dirty_stores[index]);
            if (!environment.enqueue_store(
                    dirty_stores[index],
                    memblock::lq_pointer_value(lq_offset)) ||
                !environment.issue_store_address(dirty_stores[index]) ||
                !environment.run_cycles(64) ||
                !environment.issue_store_address(dirty_stores[index]) ||
                !environment.issue_store_data(dirty_stores[index]) ||
                !environment.run_until_all_complete(2048)) {
                return false;
            }
            coverage.sample(dirty_stores[index], false);
            ++actions;
        } else if (!issue_store(
                       dirty_stores[index], (index & 1U) == 0)) {
            return false;
        }
        if (!environment.commit_store(dirty_stores[index], 2048)) {
            return false;
        }
        ++coverage.cacheable;
        if ((index & 1U) != 0) {
            const auto pressure_load = make_load(
                cache0_base + 0x18000 + index * 64,
                static_cast<memblock::LoadOp>(index % 7), index % 3);
            if (!issue_load(pressure_load)) {
                return false;
            }
            ++coverage.cacheable;
        }
    }
    if (!environment.run_until_release_data_count(release_before + 1, 8192) ||
        !environment.run_cycles(32)) {
        return false;
    }
    const std::uint64_t release_delta =
        environment.tilelink_release_data() - release_before;
    const std::uint64_t verified_release_delta =
        environment.tilelink_release_data_verified() - verified_release_before;
    if (release_delta < dirty_lines - dcache_ways ||
        verified_release_delta != release_delta) {
        return false;
    }
    coverage.dirty_pressure = verified_release_delta;

    // Exact snapshots belong only to the directed replacement check above.
    // The random tail may legally revisit the surviving dirty lines, so its
    // later releases must follow the live architectural reference image.
    environment.clear_release_line_expectations();


    return true;
}

} // namespace memblock::scenarios::mixed
