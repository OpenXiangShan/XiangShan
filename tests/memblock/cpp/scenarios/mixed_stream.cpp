#include "mixed/scenario.hpp"
#include "mixed/stream_oracle.hpp"

namespace memblock::scenarios::mixed {
namespace {
// Architectural age belongs to this deque, independent of LSQ dequeue order.
struct StreamWork {
    unsigned kind = 0;
    memblock::LoadTransaction load;
    memblock::StoreTransaction store;
    memblock::PrefetchTransaction prefetch;
    std::vector<memblock::VectorMemoryTransaction> vectors;
    RandomVectorShape shape{};
    std::vector<bool> issued;
    bool address_issued = false;
    bool data_issued = false;
    bool requires_head = false;
    bool data_first = false;
    std::uint64_t last_address_issue = 0;
    std::uint64_t expected_scalar = 0;
    std::uint8_t rob() const {
        if (kind == 0) return load.rob;
        if (kind == 2) return store.rob;
        if (kind == 4) return prefetch.rob;
        return vectors.front().rob;
    }
    bool flag() const {
        if (kind == 0) return load.rob_flag;
        if (kind == 2) return store.rob_flag;
        if (kind == 4) return prefetch.rob_flag;
        return vectors.front().rob_flag;
    }
    bool is_store() const { return kind == 2 || kind == 3; }
    bool accepted() const {
        return address_issued || data_issued ||
            std::find(issued.begin(), issued.end(), true) != issued.end();
    }
};
constexpr std::array<unsigned, 5> stream_operations{{
    RandomConstraints::scalar_load, RandomConstraints::vector_load,
    RandomConstraints::scalar_store, RandomConstraints::vector_store,
    RandomConstraints::prefetch}};
}

bool MixedScenario::run_concurrent_stream(unsigned instruction_limit)
{
    if (instruction_limit == 0) return true;
    phase = "mixed-stream-enter";
    const auto translation = choose_translation_context();
    const auto leaf = choose_translation_leaf(translation);
    if (!enter_translation_context(translation) ||
        !issue_constrained_fence(RandomConstraints::scalar_load) ||
        !environment.run_until_all_complete(constrained_completion_timeout) ||
        !environment.run_until_queues_retired(constrained_completion_timeout)) return false;
    const auto ptw_before = environment.ptw_requests();
    const auto requests_before = environment.tilelink_requests();
    const auto arena = constrained_cacheable_address(64, translation, leaf, false);
    StreamByteOracle oracle(environment.memory());
    std::deque<StreamWork> live;
    std::array<std::set<unsigned>, 3> destinations;
    const unsigned depth = 2U + random() % 7U;
    const unsigned length = std::min(instruction_limit, 8U + unsigned(random() % 25U));
    unsigned admitted = 0, retired = 0;
    auto last_progress = environment.cycle();
    const auto deadline = environment.cycle() + std::uint64_t{length} * constrained_completion_timeout * 4U;
    const auto start_action = actions;
    ++coverage.stream[0];
    coverage.stream[10] += actions > coverage.concurrent_actions + coverage.stream[2];

    const auto pending = [&](const StreamWork &work) {
        if (work.kind == 0) return environment.scalar_load_pending(work.load);
        if (work.kind == 2) return environment.scalar_store_pending(work.store);
        if (work.kind == 4) return environment.prefetch_pending(work.prefetch);
        return std::any_of(work.vectors.begin(), work.vectors.end(),
            [&](const auto &uop) { return environment.vector_memory_pending(uop); });
    };
    const auto all_issued = [](const StreamWork &work) {
        if (work.kind == 2) return work.address_issued && work.data_issued;
        return std::find(work.issued.begin(), work.issued.end(), false) == work.issued.end();
    };
    const auto allocate_destination = [&](unsigned domain) {
        const unsigned count = domain == 0 ? memblock::kIntegerPhysicalRegisters :
            domain == 1 ? memblock::kFpPhysicalRegisters : memblock::kVectorPhysicalRegisters;
        std::vector<unsigned> available;
        for (unsigned value = 1; value < count; ++value)
            if (!destinations[domain].count(value)) available.push_back(value);
        if (available.empty()) throw std::logic_error("stream destination pool exhausted");
        const auto value = available[random() % available.size()];
        destinations[domain].insert(value);
        return static_cast<std::uint8_t>(value);
    };
    const auto install_head = [&]() {
        return live.empty() || environment.set_rob_head_state(
            live.front().rob(), live.front().flag(), live.front().is_store());
    };

    while (retired < length) {
        phase = "mixed-stream:admitted=" + std::to_string(admitted) +
            ":retired=" + std::to_string(retired) + ":live=" + std::to_string(live.size());
        report_progress();
        if (environment.cycle() > deadline) { phase += ":epoch-deadline"; return false; }
        while (!live.empty() && all_issued(live.front()) && !pending(live.front())) {
            auto &work = live.front();
            for (const auto &uop : work.vectors) {
                bool replayed = false;
                if (!environment.service_vector_replay(uop, replayed, 256)) return false;
            }
            coverage.stream[5] += live.size() > 1;
            if (work.kind == 2 && !environment.commit_store(work.store, constrained_completion_timeout)) return false;
            if (work.kind == 3) for (const auto &uop : work.vectors)
                if (!environment.commit_vector_store(uop, constrained_completion_timeout)) return false;
            if (work.kind == 0) destinations[work.load.fp_wen ? 1U : 0U].erase(work.load.pdest);
            if (work.kind == 1) for (const auto &uop : work.vectors) destinations[2].erase(uop.pdest);
            const auto old_rob = work.rob(); const auto old_flag = work.flag();
            ++coverage.stream_ops[work.kind];
            ++coverage.stream[2]; ++retired; ++actions;
            ++constraint_coverage.actions;
            ++constraint_coverage.operations[stream_operations[work.kind]];
            if (work.kind == 0) {
                coverage.sample(work.load);
                coverage.sample_fp_load(work.load, work.expected_scalar, 1);
                coverage.sample_scalar_format_choice(1, work.load.fp_wen);
                constraint_coverage.sample_dcache_load(std::nullopt);
            } else if (work.kind == 2) coverage.sample(work.store, work.data_first);
            else if (work.kind == 4) coverage.sample(work.prefetch);
            else {
                for (const auto &uop : work.vectors) coverage.sample(uop);
                sample_constrained_vector_instruction(work.kind == 3, work.shape, work.vectors);
            }
            ++coverage.cacheable;
            live.pop_front();
            if (live.empty()) {
                if (!environment.set_rob_head_state(old_rob, old_flag, false)) return false;
            } else if (!install_head()) return false;
            last_progress = environment.cycle();
        }
        if (retired == length) break;

        bool replayed = false;
        for (auto &work : live) {
            for (unsigned i = 0; i < work.vectors.size(); ++i) {
                if (!work.issued[i]) continue;
                bool serviced = false;
                if (!environment.service_vector_replay(work.vectors[i], serviced, 256)) return false;
                if (serviced) { ++coverage.stream[9]; replayed = true; break; }
            }
            if (replayed) break;
        }
        if (replayed) { last_progress = environment.cycle(); continue; }

        const auto lq_live = environment.lq_allocated() - environment.lq_canceled() - environment.lq_dequeued();
        const auto sq_live = environment.sq_allocated() - environment.sq_canceled() - environment.sq_dequeued();
        const auto lq_free = memblock::kVirtualLoadQueueEntries - memblock::kLqEnqueueHeadroom - lq_live;
        const auto sq_free = memblock::kStoreQueueEntries - memblock::kSqEnqueueHeadroom - sq_live;
        const auto fits_vector = [&](std::uint64_t capacity) {
            return std::any_of(vector_shapes.begin(), vector_shapes.end(),
                [&](const auto &shape) { return vector_shape_total_flows(shape) <= capacity; });
        };
        std::array<unsigned, 5> weights{};
        for (unsigned kind = 0; kind < 5; ++kind) {
            const auto capacity = kind == 2 || kind == 3 ? sq_free : lq_free;
            const bool fits = kind == 1 || kind == 3 ? fits_vector(capacity) : capacity != 0;
            if (fits) weights[kind] = constraints.operation_weights[stream_operations[kind]];
        }
        // 0 = admission, 1 = scalar load/prefetch, 2 = STA, 3 = STD,
        // 4 = vector uop. Candidates are regenerated from current obligations.
        struct Event { unsigned kind, instruction, uop; };
        std::vector<Event> ready;
        if (admitted < length && live.size() < depth &&
            std::accumulate(weights.begin(), weights.end(), std::uint64_t{0}) != 0)
            ready.push_back({0, 0, 0});
        bool older_addresses_known = true;
        for (unsigned i = 0; i < live.size(); ++i) {
            auto &work = live[i];
            const bool head_eligible = !work.requires_head || i == 0;
            if ((work.kind == 0 || work.kind == 4) && !work.issued[0] && head_eligible &&
                (work.kind == 4 || older_addresses_known)) ready.push_back({1, i, 0});
            if (work.kind == 2) {
                // A beat-crossing STA can be nacked until it reaches SQ head.
                // Its blocking hit wait must not prevent older work retiring.
                if (head_eligible && (!work.address_issued ||
                    (environment.scalar_store_address_pending(work.store) &&
                     environment.cycle() - work.last_address_issue >= 32))) ready.push_back({2, i, 0});
                if (!work.data_issued) ready.push_back({3, i, 0});
            }
            if (work.kind == 1 || work.kind == 3) {
                const bool ordered_load = work.kind == 1 && work.vectors.front().addressing ==
                    memblock::VectorAddressingMode::indexed_ordered;
                if (head_eligible && (!ordered_load || i == 0) && (work.kind == 3 || older_addresses_known)) {
                    for (unsigned u = 0; u < work.vectors.size(); ++u) {
                        if (work.issued[u]) continue;
                        if (work.vectors[u].addressing != memblock::VectorAddressingMode::indexed_unordered &&
                            u != unsigned(std::count(work.issued.begin(), work.issued.end(), true))) continue;
                        if (u && vector_load_depends_on_previous(work.vectors[u-1], work.vectors[u]) &&
                            (!work.issued[u-1] || environment.vector_memory_pending(work.vectors[u-1]))) continue;
                        ready.push_back({4, i, u});
                    }
                }
            }
            if (work.kind == 2) older_addresses_known &= work.address_issued &&
                !environment.scalar_store_address_pending(work.store);
            if (work.kind == 3) older_addresses_known &= all_issued(work) && !pending(work);
        }
        if (ready.empty()) {
            if (!environment.run_cycles(1)) return false;
            if (environment.cycle() - last_progress > 2U * constrained_completion_timeout) {
                phase += ":no-accepted-or-retired-progress";
                return false;
            }
            continue;
        }
        const auto event = ready[random() % ready.size()];
        if (event.kind == 0) {
            const bool had_accepted_pending = std::any_of(live.begin(), live.end(),
                [&](const auto &work) { return work.accepted() && pending(work); });
            coverage.stream[3] += had_accepted_pending;
            coverage.stream[4] += retired != 0 && !live.empty();
            StreamWork work;
            auto selection = random() % std::accumulate(weights.begin(), weights.end(), std::uint64_t{0});
            for (work.kind = 0; work.kind < 4 && selection >= weights[work.kind]; ++work.kind)
                selection -= weights[work.kind];
            auto line = random() % 4U != 0 ? arena + (random() % 8U) * 64U :
                constrained_cacheable_address(64, translation, leaf, false);
            if (work.kind == 0) {
                const auto format = constraints.scalar_load_policy().choose(random);
                work.load = make_load(constrained_window_address(line, 1U << (format.op & 3U)),
                    static_cast<memblock::LoadOp>(format.op), random() % memblock::kScalarLoadLanes, format.fp);
                work.load.pdest = allocate_destination(format.fp ? 1U : 0U);
                work.expected_scalar = oracle.scalar_load(work.load);
                work.requires_head = (work.load.address & 15U) + (1U << (format.op & 3U)) > 16U;
                work.issued.assign(1, false);
                environment.expect_load_data(work.load, work.expected_scalar);
                if (!environment.enqueue_load(work.load)) return false;
            } else if (work.kind == 2) {
                const auto op = static_cast<memblock::StoreOp>(random() % 4U);
                const unsigned width = 1U << static_cast<unsigned>(op);
                auto address = constrained_window_address(line, width);
                if ((address & 0xfffU) + width > 0x1000U)
                    address = (address & ~std::uint64_t{0xfff}) | (0x1000U - width);
                work.store = make_store(address, random(), op, random() % memblock::kScalarStoreLanes);
                if ((address & 15U) + width > 16U) {
                    work.store.expected_address_writeback_lane = 0;
                    work.requires_head = true;
                }
                oracle.store(work.store);
                environment.expect_store(work.store);
                if (!environment.enqueue_store(work.store)) return false;
            } else if (work.kind == 4) {
                work.prefetch = make_prefetch(line, static_cast<memblock::PrefetchOp>(8U + random() % 3U),
                    random() % memblock::kScalarLoadLanes);
                work.issued.assign(1, false);
                environment.expect_prefetch(work.prefetch);
                if (!environment.enqueue_prefetch(work.prefetch)) return false;
            } else {
                const bool store = work.kind == 3;
                work.shape = choose_vector_shape(store ? 1U : 0U, store ? sq_free : lq_free, false);
                const auto eew = work.shape.addressing >= 2 ? work.shape.vsew : work.shape.eew;
                work.vectors = make_random_vector_uops(store, constrained_window_address(line, 1U << eew),
                    work.shape, random() % 1000U < constraints.vector_corner_per_mille);
                work.issued.assign(work.vectors.size(), false);
                if (!store) {
                    for (auto &uop : work.vectors) uop.pdest = allocate_destination(2);
                    oracle.prepare_vector_load(work.vectors);
                }
                for (const auto &uop : work.vectors) {
                    if (!validate_vector_translation(uop)) return false;
                    const unsigned width = 1U << memblock::vector_data_eew(uop);
                    const auto active = memblock::active_vector_elements(uop);
                    for (unsigned element = 0; element < 16U / width; ++element) {
                        if (!((active >> element) & 1U)) continue;
                        const auto address = memblock::vector_element_virtual_address(uop, element);
                        work.requires_head |= store ? (address & (width - 1U)) != 0 :
                            (address & 15U) + width > 16U;
                    }
                    if (store) { oracle.store(uop); environment.expect_vector(uop); }
                    else environment.expect_vector_data(uop, oracle.vector_load(uop));
                    if (!environment.enqueue_vector(uop)) return false;
                }
                coverage.stream_vector_uops[store ? 1U : 0U] += work.vectors.size();
            }
            const bool was_empty = live.empty();
            live.push_back(std::move(work)); ++admitted; ++coverage.stream[1];
            if (was_empty && !install_head()) return false;
        } else {
            auto &work = live[event.instruction];
            bool initial_event = true;
            if (event.kind == 1) {
                if (work.kind == 0 ? !environment.issue_load(work.load, constrained_completion_timeout) :
                    !environment.issue_prefetch(work.prefetch, constrained_completion_timeout)) return false;
                work.issued[0] = true;
                ++coverage.stream_issue[work.kind == 0 ? 0U : 5U];
            } else if (event.kind == 2) {
                const bool first = !work.address_issued;
                initial_event = first;
                coverage.stream_scalar_retries += !first;
                if (!environment.issue_store_address_until_tlb_hit(work.store, constrained_completion_timeout)) return false;
                work.address_issued = true; work.last_address_issue = environment.cycle();
                coverage.stream_issue[1] += first;
            } else if (event.kind == 3) {
                work.data_first = !work.address_issued;
                if (!environment.issue_store_data(work.store, constrained_completion_timeout)) return false;
                work.data_issued = true; ++coverage.stream_issue[2];
            } else {
                const auto &uop = work.vectors[event.uop];
                if (!environment.issue_vector(uop, constrained_completion_timeout)) return false;
                if (event.uop && vector_load_depends_on_previous(work.vectors[event.uop-1], uop))
                    ++coverage.vector_operand_chains[2];
                work.issued[event.uop] = true;
                ++coverage.stream_issue[work.kind == 1 ? 3U : 4U];
            }
            coverage.stream[8] += initial_event;
        }
        coverage.stream[6] = std::max<std::uint64_t>(coverage.stream[6], live.size());
        const auto accepted_pending = std::count_if(live.begin(), live.end(),
            [&](const auto &work) { return work.accepted() && pending(work); });
        coverage.stream[7] = std::max<std::uint64_t>(coverage.stream[7], accepted_pending);
        last_progress = environment.cycle();
        if (!environment.run_cycles(random() % 4U)) return false;
    }
    phase = "mixed-stream-drain";
    if (!environment.run_until_queues_retired(constrained_completion_timeout)) return false;
    coverage.stream[11] = actions;
    constraint_coverage.sample_dcache(requests_before, environment.tilelink_requests());
    constraint_coverage.sample_translation(translation, leaf, ptw_before,
        environment.ptw_requests(), actions - start_action, actions - start_action);
    return environment.run_cycles(8);
}
} // namespace memblock::scenarios::mixed
