#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::run_concurrent_windows()
{
    // Each window contains all five producer classes before any
    // completion drain, so cache, TLB, forwarding, and queue timing can
    // interact in one simulation.
    while (actions + 7 <= concurrent_action_limit) {
        report_progress();
        std::optional<unsigned> special_kind;
        if (actions + 8 <= concurrent_action_limit &&
            constraints.special_concurrent_per_mille != 0 &&
            constraints.uses_concurrent_special_operations() &&
            random() % 1000 < constraints.special_concurrent_per_mille) {
            special_kind =
                constraints.choose_concurrent_special_operation(random());
        }
        TranslationContext translation = choose_translation_context();
        if (special_kind &&
            (*special_kind == RandomConstraints::noncacheable ||
                *special_kind == RandomConstraints::mmio) &&
            translation.regime == RandomConstraints::translation_bare) {
            translation = translated_context();
        }
        const TranslationLeafTopology leaf =
            choose_translation_leaf(translation);
        if (!enter_translation_context(translation) ||
            !issue_constrained_fence(
                special_kind.value_or(RandomConstraints::scalar_load))) {
            return false;
        }
        const std::uint64_t ptw_at_window_start =
            environment.ptw_requests();
        const std::uint64_t requests_at_window_start =
            environment.tilelink_requests();
        const std::uint64_t window_base =
            constrained_cacheable_address(64, translation, leaf) &
            ~std::uint64_t{63};
        std::optional<memblock::LoadTransaction> special_load;
        if (special_kind == RandomConstraints::noncacheable) {
            special_load = make_load(
                nc_base + (random() % 128) * 8,
                memblock::LoadOp::ld, random() % 3);
        } else if (special_kind == RandomConstraints::mmio) {
            const std::uint64_t offset = (random() % 128) * 8;
            special_load = make_load(
                mmio_access_address() + offset,
                memblock::LoadOp::ld, random() % 3);
            special_load->oracle_address = mmio_physical + offset;
            special_load->expected_debug_is_mmio = true;
            special_load->expected_debug_is_ncio = false;
            special_load->expected_debug_is_perf_cnt = false;
        }
        const bool vector_corner =
            random() % 1000 < constraints.vector_corner_per_mille;
        const auto &vector_load_shape = choose_vector_shape(
            0U, concurrent_vector_load_flow_limit, true);
        const auto &vector_store_shape = choose_vector_shape(
            1U, concurrent_vector_store_flow_limit, true);
        const auto scalar_shape = constraints.scalar_load_policy().choose(random);
        const auto scalar_op = static_cast<memblock::LoadOp>(scalar_shape.op);
        auto scalar = make_load(
            constrained_window_address(
                window_base,
                1U << (static_cast<unsigned>(scalar_op) & 3U)),
            scalar_op, random() % 3, scalar_shape.fp);
        auto vector_load_uops = make_random_vector_uops(
            false, window_base + 32, vector_load_shape, vector_corner);
        const auto scalar_store_op =
            static_cast<memblock::StoreOp>(random() % 4);
        const unsigned scalar_store_width =
            1U << (static_cast<unsigned>(scalar_store_op) & 3U);
        std::uint64_t scalar_store_address = constrained_window_address(
            window_base, scalar_store_width);
        if ((scalar_store_address & 0xfffU) + scalar_store_width > 0x1000U) {
            scalar_store_address =
                (scalar_store_address & ~std::uint64_t{0xfff}) |
                (0x1000U - scalar_store_width);
        }
        auto scalar_store = make_store(
            scalar_store_address, random(), scalar_store_op,
            random() % 2);
        // This window uses legal cacheable stores with hardware
        // misalignment enabled.  A crossing of the 16-byte beat is
        // completed by the shared store misalignment output.
        if ((scalar_store.address & 15U) + scalar_store_width > 16U) {
            scalar_store.expected_address_writeback_lane = 0;
        }
        auto vector_store_uops = make_random_vector_uops(
            true, window_base + 0x100, vector_store_shape, vector_corner);
        auto prefetch = make_prefetch(
            window_base + 0x180 + (random() % 8U) * 8U,
            static_cast<memblock::PrefetchOp>(8 + random() % 3), random() % 3);

        {
            std::ostringstream window;
            window << "seeded-mixed-tail"
                   << ":window=" << coverage.concurrent_windows
                   << ":scalar=0x" << std::hex << scalar.address
                   << "/" << static_cast<unsigned>(scalar.op)
                   << ":vload=0x" << vector_load_uops.front().address
                   << std::dec << "/" << vector_load_shape.addressing
                   << "/" << vector_load_shape.eew
                   << "/" << vector_load_shape.vsew
                   << "/" << vector_load_shape.lmul_log2
                   << "/" << vector_load_shape.emul_log2
                   << "/vl=" << static_cast<unsigned>(
                                    vector_load_uops.front().vl)
                   << "/vstart=" << static_cast<unsigned>(
                                        vector_load_uops.front().vstart)
                   << "/vm=" << vector_load_uops.front().vm
                   << "/vma=" << vector_load_uops.front().vma
                   << "/vta=" << vector_load_uops.front().vta
                   << "/mask=0x" << std::hex
                   << vector_load_uops.front().mask_bits << std::dec
                   << "/uops=" << vector_load_uops.size()
                   << ":store=0x" << std::hex << scalar_store.address
                   << std::dec << "/" << static_cast<unsigned>(scalar_store.op)
                   << ":vstore=0x" << std::hex
                   << vector_store_uops.front().address << std::dec
                   << "/" << vector_store_shape.addressing
                   << "/" << vector_store_shape.eew
                   << "/" << vector_store_shape.vsew
                   << "/" << vector_store_shape.lmul_log2
                   << "/" << vector_store_shape.emul_log2
                   << "/vl=" << static_cast<unsigned>(
                                    vector_store_uops.front().vl)
                   << "/vstart=" << static_cast<unsigned>(
                                        vector_store_uops.front().vstart)
                   << "/vm=" << vector_store_uops.front().vm
                   << "/vma=" << vector_store_uops.front().vma
                   << "/vta=" << vector_store_uops.front().vta
                   << "/mask=0x" << std::hex
                   << vector_store_uops.front().mask_bits << std::dec
                   << "/uops=" << vector_store_uops.size();
            phase = window.str();
        }

        if (special_load) {
            environment.expect_load(*special_load);
        }
        const auto scalar_expected = environment.memory().expected_load(scalar);
        environment.expect_load(scalar);
        environment.expect_store(scalar_store);
        environment.expect_prefetch(prefetch);
        for (const auto &uop : vector_load_uops) {
            environment.expect_vector(uop);
        }
        for (const auto &uop : vector_store_uops) {
            environment.expect_vector(uop);
        }
        bool window_enqueued =
            (!special_load || environment.enqueue_load(*special_load)) &&
            environment.enqueue_load(scalar);
        for (const auto &uop : vector_load_uops) {
            window_enqueued = window_enqueued &&
                environment.enqueue_vector(uop);
        }
        window_enqueued = window_enqueued && environment.enqueue_store(
                                                 scalar_store, memblock::lq_pointer_value(lq_offset));
        for (const auto &uop : vector_store_uops) {
            window_enqueued = window_enqueued &&
                environment.enqueue_vector(uop);
        }
        window_enqueued = window_enqueued &&
            environment.enqueue_prefetch(prefetch);
        if (!window_enqueued) {
            return false;
        }

        if (special_load) {
            const bool mmio = special_kind == RandomConstraints::mmio;
            const std::uint64_t uncache_before =
                environment.uncache_requests();
            if ((mmio && !environment.set_rob_head(
                             special_load->rob, special_load->rob_flag)) ||
                !environment.issue_load(*special_load, 2048) ||
                (mmio && !environment.wait_for_mmio_request(
                             special_load->rob, special_load->rob_flag,
                             8192)) ||
                (mmio && !environment.run_until_uncache_requests(
                             uncache_before + 1, 8192))) {
                return false;
            }
        }

        std::array<unsigned, 5> issue_order{{0, 1, 2, 3, 4}};
        std::shuffle(issue_order.begin(), issue_order.end(), random);
        // Ordered indexed vector loads wait for all older LQ entries.
        // Keep the dependency legal while retaining a randomized order
        // for the remaining independent classes.
        if (vector_load_shape.addressing == static_cast<unsigned>(
                                                memblock::VectorAddressingMode::indexed_ordered)) {
            auto scalar_position = std::find(
                issue_order.begin(), issue_order.end(), 0U);
            auto vector_position = std::find(
                issue_order.begin(), issue_order.end(), 1U);
            if (vector_position < scalar_position) {
                std::iter_swap(vector_position, scalar_position);
            }
        }
        if (!environment.run_cycles(
                special_kind ? 0 : random_delay(0, 3))) {
            return false;
        }
        bool scalar_store_data_first = false;
        for (const unsigned issue_class : issue_order) {
            switch (issue_class) {
            case 0:
                if (!environment.set_rob_head(scalar.rob, scalar.rob_flag) ||
                    !environment.issue_load(scalar, 2048)) {
                    return false;
                }
                break;
            case 1:
                if (vector_load_shape.addressing ==
                        static_cast<unsigned>(
                            memblock::VectorAddressingMode::indexed_ordered) &&
                    ((special_load &&
                         !environment.run_until_load_complete(
                             *special_load, constrained_completion_timeout)) ||
                        !environment.run_until_load_complete(
                            scalar, constrained_completion_timeout) ||
                        !environment.set_rob_head(
                            vector_load_uops.front().rob,
                            vector_load_uops.front().rob_flag))) {
                    return false;
                }
                for (const unsigned index : vector_uop_issue_order(vector_load_uops, random)) {
                    const auto &uop = vector_load_uops[index];
                    if (index != 0 && vector_load_depends_on_previous(
                            vector_load_uops[index - 1U], uop)) {
                        if (!environment.run_until_vector_complete_with_replays(
                                vector_load_uops[index - 1U],
                                constrained_completion_timeout)) {
                            return false;
                        }
                        ++coverage.vector_operand_chains[2];
                    }
                    if (!environment.issue_vector(uop, 2048)) {
                        return false;
                    }
                }
                break;
            case 2:
                scalar_store_data_first = (random() & 1U) != 0;
                if (scalar_store_data_first
                        ? (!environment.issue_store_data(scalar_store, 2048) ||
                              !environment.issue_store_address(scalar_store, 2048))
                        : (!environment.issue_store_address(scalar_store, 2048) ||
                              !environment.issue_store_data(scalar_store, 2048))) {
                    return false;
                }
                break;
            case 3:
                for (const unsigned index : vector_uop_issue_order(vector_store_uops, random)) {
                    if (!environment.issue_vector(vector_store_uops[index], 2048)) {
                        return false;
                    }
                }
                break;
            default:
                if (!environment.issue_prefetch(prefetch, 2048)) {
                    return false;
                }
                break;
            }
            if (!environment.run_cycles(random_delay(0, 5))) {
                return false;
            }
            const std::size_t pending_scalar =
                environment.pending_scalar_loads();
            const std::size_t pending_prefetch =
                environment.pending_prefetches();
            const std::size_t pending_store =
                environment.pending_scalar_stores();
            const std::size_t pending_vector_load =
                environment.pending_vector_loads();
            const std::size_t pending_vector_store =
                environment.pending_vector_stores();
            const std::size_t pending_total = pending_scalar + pending_prefetch +
                pending_store + pending_vector_load + pending_vector_store;
            unsigned pending_classes = 0;
            pending_classes += pending_scalar != 0;
            pending_classes += pending_prefetch != 0;
            pending_classes += pending_store != 0;
            pending_classes += pending_vector_load != 0;
            pending_classes += pending_vector_store != 0;
            coverage.max_unresolved = std::max<std::uint64_t>(
                coverage.max_unresolved, pending_total);
            coverage.max_unresolved_classes = std::max<std::uint64_t>(
                coverage.max_unresolved_classes, pending_classes);
            if (pending_total >= 2 && pending_classes >= 2) {
                ++coverage.unresolved_overlap_samples;
            }
        }
        coverage.sample(scalar);
        coverage.sample_fp_load(scalar, scalar_expected, 1);
        coverage.sample_scalar_format_choice(1, scalar.fp_wen);
        coverage.sample(scalar_store, scalar_store_data_first);
        for (const auto &uop : vector_load_uops) {
            coverage.sample(uop);
        }
        for (const auto &uop : vector_store_uops) {
            coverage.sample(uop);
        }
        coverage.sample(prefetch);
        const unsigned scalar_store_bytes =
            1U << static_cast<unsigned>(scalar_store.op);
        const bool scalar_store_crosses_page =
            (scalar_store.address & 0xfffU) + scalar_store_bytes > 0x1000U;
        std::vector<memblock::VectorMemoryTransaction> window_vectors;
        window_vectors.reserve(
            vector_load_uops.size() + vector_store_uops.size());
        window_vectors.insert(
            window_vectors.end(), vector_load_uops.begin(),
            vector_load_uops.end());
        window_vectors.insert(
            window_vectors.end(), vector_store_uops.begin(),
            vector_store_uops.end());
        // A cross-page split store would need the backend to make it the ROB
        // head before pulsing a pending-store commit.  That is only legal
        // after every older instruction has retired, which would serialize
        // this heterogeneous window.  Page-crossing scalar stores are covered
        // by the focused `scalar-misaligned`/`misaligned-stores` contracts;
        // keep the mixed window at line-crossing granularity so a younger
        // store can never be committed while an older load is pending.
        if ((scalar_store_crosses_page &&
                !environment.set_rob_head(
                    scalar_store.rob, scalar_store.rob_flag)) ||
            !environment.run_until_store_complete_with_replay(
                scalar_store, constrained_completion_timeout,
                scalar_store_crosses_page) ||
            !environment.run_until_vector_complete_with_replays(
                window_vectors, constrained_completion_timeout) ||
            !environment.run_until_all_complete(
                constrained_completion_timeout) ||
            !environment.run_cycles(8) ||
            !environment.commit_store(scalar_store, 4096)) {
            return false;
        }
        for (const auto &uop : vector_store_uops) {
            if (!environment.commit_vector_store(uop, 4096)) {
                return false;
            }
        }
        if (!environment.set_rob_head(prefetch.rob, prefetch.rob_flag) ||
            !environment.run_until_queues_retired(4096)) {
            return false;
        }
        if (special_load) {
            coverage.sample(*special_load);
        }
        const auto scalar_readback = make_load(
            scalar_store.address,
            static_cast<memblock::LoadOp>(scalar_store.op), random() % 3);
        if (!environment.set_rob_head(
                scalar_readback.rob, scalar_readback.rob_flag) ||
            !issue_load(
                scalar_readback,
                scalar_forward_value(
                    scalar_store.data, scalar_store_bytes, false)) ||
            !environment.run_until_queues_retired(4096)) {
            return false;
        }
        auto vector_readback_uops = vector_store_uops;
        const std::uint64_t vector_readback_rob = rob_offset++;
        const auto vector_readback_pdest = random_destination_block(
            random, memblock::kVectorPhysicalRegisters, vector_readback_uops.size());
        for (auto &uop : vector_readback_uops) {
            uop.store = false;
            uop.pdest = static_cast<std::uint8_t>(vector_readback_pdest + uop.vuop_idx);
            uop.rob = memblock::rob_pointer_value(vector_readback_rob);
            uop.rob_flag = memblock::rob_pointer_flag(vector_readback_rob);
            uop.lq = memblock::lq_pointer_value(lq_offset);
            uop.lq_flag = memblock::lq_pointer_flag(lq_offset);
            uop.sq = memblock::sq_pointer_value(sq_offset);
            uop.sq_flag = memblock::sq_pointer_flag(sq_offset);
            lq_offset += uop.flow_num;
        }
        coverage.vector_operand_chains[0] += chain_indexed_vector_sources(
            vector_readback_uops, environment.memory());
        if (!environment.set_rob_head(
                vector_readback_uops.front().rob,
                vector_readback_uops.front().rob_flag) ||
            !issue_random_vector_instruction(vector_readback_uops)) {
            return false;
        }
        sample_constrained_vector_instruction(
            false, vector_load_shape, vector_load_uops);
        sample_constrained_vector_instruction(
            true, vector_store_shape, vector_store_uops);
        sample_constrained_vector_instruction(
            false, vector_store_shape, vector_readback_uops);
        constraint_coverage.sample_concurrent_vector_shape(
            false, vector_load_shape.addressing,
            vector_load_uops.size());
        constraint_coverage.sample_concurrent_vector_shape(
            true, vector_store_shape.addressing,
            vector_store_uops.size());
        if (
            // Keep the commit boundary at the last uop in this window.
            // StoreQueue treats a uop whose ROB equals pendingPtr as
            // committed; pointing at the next ROB would auto-commit the
            // first store of the following window before its explicit
            // commit operation is issued.
            !environment.set_rob_head(
                memblock::rob_pointer_value(rob_offset - 1),
                memblock::rob_pointer_flag(rob_offset - 1)) ||
            !environment.run_until_queues_retired(4096) ||
            // LSQ free-count updates trail the externally visible dequeue
            // pulse by a few cycles. Let enqueue readiness settle before
            // starting the next five-class window.
            !environment.run_cycles(8)) {
            return false;
        }
        ++coverage.concurrent_windows;
        coverage.concurrent_actions += special_kind ? 8 : 7;
        for (auto &count : coverage.concurrent_ops) {
            ++count;
        }
        constraint_coverage.operations[RandomConstraints::scalar_load] += 2;
        constraint_coverage.sample_dcache_load(std::nullopt);
        constraint_coverage.sample_dcache_load(std::nullopt);
        ++constraint_coverage.operations[RandomConstraints::scalar_store];
        constraint_coverage.operations[RandomConstraints::vector_load] += 2;
        ++constraint_coverage.operations[RandomConstraints::vector_store];
        ++constraint_coverage.operations[RandomConstraints::prefetch];
        constraint_coverage.actions += 7;
        if (special_kind) {
            const unsigned special_index =
                *special_kind - RandomConstraints::noncacheable;
            ++constraint_coverage.special_concurrent[special_index];
            constraint_coverage.sample_operation(*special_kind);
            constraint_coverage.sample_uncache(
                special_index, false, std::nullopt);
            auto &directions = *special_kind ==
                    RandomConstraints::noncacheable
                ? constraint_coverage.nc_directions
                : constraint_coverage.mmio_directions;
            ++directions[0];
            ++coverage.noncacheable;
            ++actions;
        }
        constraint_coverage.sample_dcache(
            requests_at_window_start, environment.tilelink_requests());
        constraint_coverage.sample_translation(
            translation, leaf, ptw_at_window_start,
            environment.ptw_requests(), special_kind ? 8 : 7, 7);
        actions += 5;
        coverage.cacheable += 7;
    }
    return true;
}

} // namespace memblock::scenarios::mixed
