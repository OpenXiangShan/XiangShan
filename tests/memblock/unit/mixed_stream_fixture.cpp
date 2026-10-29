// Synthetic timing and ports; transaction builders, vector producers, coverage,
// constraints, and both scheduling decision methods are production code.
#include "scenarios/mixed/state.hpp"
#include "scenarios/mixed/vector_indices.hpp"
#include "scenarios/mixed/vector_operands.hpp"
#include "scenarios/mixed/stream_oracle.hpp"
#include <functional>
#include <map>
#include <tuple>

using namespace memblock;
using namespace memblock::scenarios;
using namespace memblock::scenarios::mixed;

static void require(bool condition, const std::string &message) {
    if (!condition) throw std::runtime_error(message);
}

struct SyntheticEnvironment {
    struct Uop {
        bool issued = false, committed = false;
        std::uint64_t due = 0;
        unsigned destination = 0;
        VectorMemoryTransaction vector;
    };
    struct Instruction {
        unsigned kind = 0, id = 0, flows = 0;
        std::uint8_t rob = 0;
        bool flag = false, address = false, data = false, committed = false;
        bool last_uop_seen = false, needs_head = false;
        std::uint64_t address_due = 0, data_due = 0;
        std::vector<Uop> uops;
    };
    SparseMemory bytes;
    std::vector<Instruction> instructions;
    std::array<std::set<unsigned>, 3> destinations;
    std::set<std::tuple<unsigned, unsigned, unsigned>> expected, enqueued;
    std::vector<std::string> trace;
    std::vector<unsigned> producer_counts;
    unsigned *actions = nullptr;
    unsigned retired = 0, latency = 0, peak_live = 0, peak_accepted = 0;
    unsigned overlap_admissions = 0, continued_admissions = 0;
    unsigned loads_before_older_std = 0, head_checks = 0, dependency_checks = 0;
    unsigned issues = 0, commits = 0;
    std::optional<unsigned> installed_head;
    bool pending_store_level = false;
    std::uint64_t tick = 0, lq_used = 0, sq_used = 0, lq_done = 0, sq_done = 0;
    static constexpr unsigned initial_lq = 69, initial_sq = 53;

    explicit SyntheticEnvironment(unsigned timing) : latency(timing) {
        bytes.define_repeating_incrementing(0x80000000, 0x100000, 256, 0x39);
    }
    SparseMemory &memory() { return bytes; }
    std::uint64_t cycle() const { return tick; }
    std::uint64_t ptw_requests() const { return 0; }
    std::uint64_t tilelink_requests() const { return issues; }
    std::uint64_t lq_allocated() const { return lq_used; }
    std::uint64_t sq_allocated() const { return sq_used; }
    std::uint64_t lq_canceled() const { return 0; }
    std::uint64_t sq_canceled() const { return 0; }
    std::uint64_t lq_dequeued() const { return lq_done; }
    std::uint64_t sq_dequeued() const { return sq_done; }
    bool complete(const Instruction &work) const {
        if (work.kind == 2)
            return work.address && work.data && tick >= work.address_due && tick >= work.data_due;
        return work.last_uop_seen && !work.uops.empty() &&
            std::all_of(work.uops.begin(), work.uops.end(), [&](const auto &uop) {
                return uop.issued && tick >= uop.due;
            });
    }
    bool accepted(const Instruction &work) const {
        return work.address || work.data || std::any_of(work.uops.begin(), work.uops.end(),
            [](const auto &uop) { return uop.issued; });
    }
    void synchronize_retirement() {
        require(actions != nullptr, "missing architectural action counter");
        require(*actions >= retired && *actions <= instructions.size(), "retirement outside admitted prefix");
        while (retired < *actions) {
            const auto &work = instructions.at(retired);
            require(complete(work), "retired incomplete architectural head");
            if (work.kind == 2) require(work.committed, "scalar store retired without commit");
            if (work.kind == 3) for (const auto &uop : work.uops)
                require(uop.committed, "vector store retired without all commits");
            if (work.kind == 0 || work.kind == 1) {
                const unsigned domain = work.kind == 1 ? 2 : scalar_domains.at(work.id);
                for (const auto &uop : work.uops)
                    require(destinations[domain].erase(uop.destination) == 1, "destination released twice");
            }
            (work.kind == 2 || work.kind == 3 ? sq_done : lq_done) += work.flows;
            trace.push_back("R" + std::to_string(work.id));
            ++retired;
        }
    }
    std::map<unsigned, unsigned> scalar_domains;
    Instruction &find(std::uint8_t rob, bool flag) {
        synchronize_retirement();
        for (unsigned index = retired; index < instructions.size(); ++index)
            if (instructions[index].rob == rob && instructions[index].flag == flag)
                return instructions[index];
        throw std::runtime_error("event did not match a live architectural identity");
    }
    const Instruction &find(std::uint8_t rob, bool flag) const {
        for (unsigned index = retired; index < instructions.size(); ++index)
            if (instructions[index].rob == rob && instructions[index].flag == flag)
                return instructions[index];
        throw std::runtime_error("pending query did not match a live identity");
    }
    static unsigned identity(std::uint8_t rob, bool flag) { return rob + (flag ? kRobEntries : 0U); }
    template<class T> auto key(const T &t, unsigned kind, unsigned uop = 0) const {
        return std::make_tuple(identity(t.rob, t.rob_flag), kind, uop);
    }
    template<class T> void expect(const T &t, unsigned kind, unsigned uop = 0) {
        require(expected.insert(key(t, kind, uop)).second, "duplicate expectation");
    }
    void expect_load_data(const LoadTransaction &t, std::uint64_t) { expect(t, 0); }
    void expect_store(const StoreTransaction &t) { expect(t, 2); }
    void expect_prefetch(const PrefetchTransaction &t) { expect(t, 4); }
    void expect_vector(const VectorMemoryTransaction &t) { expect(t, t.store ? 3 : 1, t.vuop_idx); }
    void expect_vector_data(const VectorMemoryTransaction &t, std::array<unsigned char, 16>) { expect_vector(t); }

    template<class T> Instruction &admit(const T &t, unsigned kind, unsigned uop = 0) {
        synchronize_retirement();
        require(expected.count(key(t, kind, uop)) != 0, "dispatch lacked prior expectation");
        require(enqueued.insert(key(t, kind, uop)).second, "duplicate dispatch");
        if (uop == 0) {
            const unsigned next = instructions.size();
            // Builders start immediately before ROB/queue wrap boundaries.
            require(t.rob == rob_pointer_value(157 + next) && t.rob_flag == rob_pointer_flag(157 + next),
                "admission reordered or duplicated the architectural ROB prefix");
            bool had_pending = false;
            for (unsigned index = retired; index < instructions.size(); ++index)
                had_pending |= accepted(instructions[index]) && !complete(instructions[index]);
            overlap_admissions += had_pending;
            continued_admissions += retired != 0 && retired != instructions.size();
            instructions.push_back(Instruction{.kind = kind, .id = next, .rob = t.rob, .flag = t.rob_flag});
            trace.push_back("A" + std::to_string(next) + ":" + std::to_string(kind));
            producer_counts.push_back(0);
        }
        auto &work = instructions.back();
        require(work.rob == t.rob && work.flag == t.rob_flag && work.kind == kind,
            "vector siblings escaped their architectural admission");
        const auto lq = initial_lq + lq_used, sq = initial_sq + sq_used;
        require(t.lq == lq_pointer_value(lq) && t.lq_flag == lq_pointer_flag(lq) &&
            t.sq == sq_pointer_value(sq) && t.sq_flag == sq_pointer_flag(sq),
            "dispatch cursor snapshot disagreed with allocated prefix");
        peak_live = std::max(peak_live, unsigned(instructions.size() - retired));
        require(peak_live <= 8, "live-depth bound exceeded");
        return work;
    }
    void allocate(Instruction &work, unsigned count) {
        work.flows += count;
        (work.kind == 2 || work.kind == 3 ? sq_used : lq_used) += count;
        require(lq_used - lq_done <= kVirtualLoadQueueEntries - kLqEnqueueHeadroom,
            "LQ admission exceeded reserved capacity");
        require(sq_used - sq_done <= kStoreQueueEntries - kSqEnqueueHeadroom,
            "SQ admission exceeded reserved capacity");
    }
    void destination(Instruction &work, unsigned domain, unsigned value) {
        const unsigned registers = domain == 0 ? kIntegerPhysicalRegisters :
            domain == 1 ? kFpPhysicalRegisters : kVectorPhysicalRegisters;
        require(value > 0 && value < registers, "destination outside register-file domain");
        require(destinations[domain].insert(value).second, "physical destination reused while live");
        work.uops.back().destination = value;
    }
    bool enqueue_load(const LoadTransaction &t) {
        auto &work = admit(t, 0); work.last_uop_seen = true; work.uops.emplace_back();
        const unsigned width = 1U << (static_cast<unsigned>(t.op) & 3U);
        work.needs_head = (t.address & 15U) + width > 16;
        scalar_domains[work.id] = t.fp_wen ? 1 : 0;
        destination(work, t.fp_wen ? 1 : 0, t.pdest); allocate(work, 1); return true;
    }
    bool enqueue_store(const StoreTransaction &t) {
        auto &work = admit(t, 2);
        const unsigned width = 1U << static_cast<unsigned>(t.op);
        work.needs_head = (t.address & 15U) + width > 16U;
        allocate(work, 1); return true;
    }
    bool enqueue_prefetch(const PrefetchTransaction &t) {
        auto &work = admit(t, 4); work.last_uop_seen = true; work.uops.emplace_back(); allocate(work, 1); return true;
    }
    bool enqueue_vector(const VectorMemoryTransaction &t) {
        auto &work = admit(t, t.store ? 3 : 1, t.vuop_idx);
        require(t.vuop_idx == work.uops.size(), "non-prefix vector dispatch");
        work.uops.push_back(Uop{.vector = t}); work.last_uop_seen = t.last_uop;
        if (!t.store) destination(work, 2, t.pdest);
        const unsigned width = 1U << vector_data_eew(t);
        const auto active = active_vector_elements(t);
        for (unsigned element = 0; element < 16U / width; ++element) {
            if (((active >> element) & 1U) == 0) continue;
            const auto address = vector_element_virtual_address(t, element);
            work.needs_head |= t.store ? (address & (width - 1U)) != 0 : (address & 15U) + width > 16U;
        }
        allocate(work, t.flow_num); return true;
    }
    void check_load_age(const Instruction &work) {
        for (unsigned index = retired; index < work.id; ++index) {
            const auto &older = instructions[index];
            if (older.kind == 2) {
                require(older.address && tick >= older.address_due,
                    "younger load issued before an older scalar-store address was known");
                loads_before_older_std += !older.data;
            }
            if (older.kind == 3) require(complete(older),
                "younger load issued before an older vector-store address set was complete");
        }
    }
    void check_installed_head() const {
        require(installed_head && *installed_head == retired, "accepted producer without current architectural ROB head");
        const auto &head = instructions.at(retired);
        require(pending_store_level == (head.kind == 2 || head.kind == 3), "accepted producer with wrong pending-store level");
    }
    void check_head(const Instruction &work, bool ordered = false) {
        if (work.needs_head || ordered) {
            ++head_checks;
            require(work.id == retired, "head-constrained instruction issued behind an older instruction");
        }
    }
    void sample_accepted() {
        unsigned count = 0;
        for (unsigned index = retired; index < instructions.size(); ++index)
            count += accepted(instructions[index]) && !complete(instructions[index]);
        peak_accepted = std::max(peak_accepted, count);
    }
    std::uint64_t due(const Instruction &work, unsigned uop = 0) const {
        // Completion timing does not inspect the scheduler's candidate list.
        return tick + (latency ? 37U + ((work.id * 7U + uop * 11U) % 31U) : 3U + (work.id + uop) % 5U);
    }
    void issue(Instruction &work, unsigned index, const char *tag) {
        check_installed_head();
        auto &uop = work.uops.at(index);
        require(!uop.issued, "duplicate uop acceptance");
        uop.issued = true; uop.due = due(work, index); ++producer_counts[work.id]; ++issues;
        trace.push_back(std::string(tag) + std::to_string(work.id) + ":" + std::to_string(index));
        sample_accepted(); ++tick;
    }
    bool issue_load(const LoadTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag); check_load_age(work); check_head(work); issue(work, 0, "L"); return true;
    }
    bool issue_prefetch(const PrefetchTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag); issue(work, 0, "P"); return true;
    }
    bool issue_store_address_until_tlb_hit(const StoreTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag);
        check_installed_head(); check_head(work);
        if (!work.address) { work.address = true; work.address_due = latency ? due(work) : tick + 1U; ++producer_counts[work.id]; ++issues; }
        else require(tick < work.address_due, "reissued an already completed store address");
        trace.push_back("SA" + std::to_string(work.id)); sample_accepted(); ++tick; return true;
    }
    bool issue_store_data(const StoreTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag); check_installed_head(); require(!work.data, "duplicate scalar store data");
        work.data = true; work.data_due = due(work, 1); ++producer_counts[work.id]; ++issues;
        trace.push_back("SD" + std::to_string(work.id)); sample_accepted(); ++tick; return true;
    }
    bool issue_vector(const VectorMemoryTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag);
        const unsigned index = t.vuop_idx;
        if (!t.store) check_load_age(work);
        check_head(work, !t.store && t.addressing == VectorAddressingMode::indexed_ordered);
        if (t.addressing != VectorAddressingMode::indexed_unordered)
            for (unsigned prior = 0; prior < index; ++prior)
                require(work.uops[prior].issued, "ordered vector producer issued out of order");
        if (!t.store && index && vector_is_special_indexed(t) &&
            vector_indexed_vd_index(work.uops[index - 1].vector) == vector_indexed_vd_index(t)) {
            ++dependency_checks;
            const auto &prior = work.uops[index - 1];
            require(prior.issued && tick >= prior.due, "repeated-Vd consumer preceded producer completion");
        }
        issue(work, index, t.store ? "VS" : "VL"); return true;
    }
    bool scalar_load_pending(const LoadTransaction &t) const { return !complete(find(t.rob, t.rob_flag)); }
    bool scalar_store_pending(const StoreTransaction &t) const { return !complete(find(t.rob, t.rob_flag)); }
    bool prefetch_pending(const PrefetchTransaction &t) const { return !complete(find(t.rob, t.rob_flag)); }
    bool scalar_store_address_pending(const StoreTransaction &t) const {
        const auto &work = find(t.rob, t.rob_flag); return !work.address || tick < work.address_due;
    }
    bool vector_memory_pending(const VectorMemoryTransaction &t) const {
        const auto &uop = find(t.rob, t.rob_flag).uops.at(t.vuop_idx); return !uop.issued || tick < uop.due;
    }
    bool service_vector_replay(const VectorMemoryTransaction &, bool &serviced, unsigned) { serviced = false; return true; }
    bool commit_store(const StoreTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag); require(work.id == retired && complete(work), "commit outside completed head");
        require(!work.committed, "duplicate scalar commit"); work.committed = true; ++commits; return true;
    }
    bool commit_vector_store(const VectorMemoryTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag); require(work.id == retired && complete(work), "vector commit outside completed head");
        auto &uop = work.uops.at(t.vuop_idx); require(!uop.committed, "duplicate vector commit");
        uop.committed = true; ++commits; return true;
    }
    bool set_rob_head_state(std::uint8_t rob, bool flag, bool pending_store) {
        synchronize_retirement();
        if (retired == instructions.size()) {
            require(!pending_store, "pending-store level escaped empty stream");
            installed_head.reset(); pending_store_level = false; return true;
        }
        const auto &head = instructions.at(retired);
        require(head.rob == rob && head.flag == flag && pending_store == (head.kind == 2 || head.kind == 3),
            "installed ROB head or pending-store level disagreed with oldest live instruction");
        installed_head = retired; pending_store_level = pending_store; return true;
    }
    bool run_cycles(unsigned count) { synchronize_retirement(); tick += count; return true; }
    bool run_until_all_complete(unsigned) {
        synchronize_retirement();
        return std::all_of(instructions.begin() + retired, instructions.end(), [&](const auto &work) { return complete(work); });
    }
    bool run_until_queues_retired(unsigned) {
        synchronize_retirement(); return lq_used == lq_done && sq_used == sq_done;
    }
    void finish(unsigned count, const MixedCoverage &coverage, const ConstraintCoverage &constraints) {
        synchronize_retirement();
        require(retired == count && instructions.size() == count, "lost or excess architectural work");
        require(expected == enqueued, "expectation/dispatch conservation failed");
        require(lq_used == lq_done && sq_used == sq_done, "queue conservation failed");
        for (const auto &domain : destinations) require(domain.empty(), "physical destination leaked");
        require(coverage.stream[1] == count && coverage.stream[2] == count && constraints.actions == count,
            "production action/admission/retirement accounting disagreed");
        require(coverage.stream[3] == overlap_admissions && coverage.stream[4] == continued_admissions,
            "production overlap coverage disagreed with independent admission observations");
        require(coverage.stream[8] == issues, "production issue count disagreed with accepted producers");
        std::array<std::uint64_t, 5> operations{};
        for (const auto &work : instructions) {
            ++operations[work.kind];
            const auto count = work.kind == 2 ? 2 : work.uops.size();
            require(producer_counts[work.id] == count, "producer conservation failed");
        }
        require(coverage.stream_ops == operations, "retired operation-kind accounting disagreed");
        const std::array<unsigned, 5> mapping{{RandomConstraints::scalar_load, RandomConstraints::vector_load,
            RandomConstraints::scalar_store, RandomConstraints::vector_store, RandomConstraints::prefetch}};
        for (unsigned kind = 0; kind < mapping.size(); ++kind)
            require(constraints.operations[mapping[kind]] == operations[kind], "constraint operation count disagreed");
    }
};

namespace memblock::scenarios::mixed {
class MixedScenario : public IdentityState, public ProgressState, public VectorShapeState {
public:
    RandomConstraints constraints;
    SyntheticEnvironment environment;
    std::mt19937_64 random;
    MixedCoverage coverage;
    ConstraintCoverage constraint_coverage;
    static constexpr unsigned constrained_completion_timeout = 512;
    explicit MixedScenario(std::uint64_t seed, unsigned timing, unsigned family)
        : constraints(RandomConstraints::preset("coverage")), environment(timing), random(seed) {
        rob_offset = 157; lq_offset = SyntheticEnvironment::initial_lq; sq_offset = SyntheticEnvironment::initial_sq;
        environment.actions = &actions;
        constraints.operation_weights.fill(0);
        const std::array<unsigned, 5> operations{{RandomConstraints::scalar_load, RandomConstraints::vector_load,
            RandomConstraints::scalar_store, RandomConstraints::vector_store, RandomConstraints::prefetch}};
        for (unsigned index = 0; index < operations.size(); ++index)
            constraints.operation_weights[operations[index]] = family == 0 ? (index == 0 ? 7 : 3) : (family == index + 1 ? 1 : 0);
        constraints.misaligned_per_mille = 450;
        constraints.scalar_fp_per_mille = 350;
        // Decoder-legal representatives include ordinary, unordered, ordered,
        // multi-uop and repeated-Vd shapes. Shape generation is production code.
        vector_shapes = {{0, 0, 0, 0, 0, 1}, {0, 2, 2, 1, 1, 1},
            {1, 1, 1, 0, 0, 1}, {2, 2, 0, 0, 2, 1}, {2, 1, 1, 1, 1, 1}, {3, 2, 2, 1, 1, 1}};
    }
    bool run_concurrent_stream(unsigned);
    void report_progress(bool = false) { environment.synchronize_retirement(); }
    TranslationContext choose_translation_context() { return {}; }
    TranslationLeafTopology choose_translation_leaf(const TranslationContext &) { return {}; }
    bool enter_translation_context(const TranslationContext &) { return true; }
    bool issue_constrained_fence(unsigned) { return true; }
    std::uint64_t constrained_cacheable_address(unsigned, const TranslationContext &, const TranslationLeafTopology &,
        std::optional<bool> = {}, bool = true) { return 0x80010000 + (random() % 128U) * 64U; }
    unsigned long constrained_window_address(std::uint64_t, unsigned);
    std::shared_ptr<const ReferencePageMap> reference_pages_for(std::uint64_t) { return {}; }
    bool validate_vector_translation(const VectorMemoryTransaction &) { return true; }
    bool choose_binary_class(unsigned probability, const std::array<std::uint64_t, 2> &) { return random() % 1000U < probability; }
    LoadTransaction make_load(std::uint64_t, LoadOp, unsigned, bool = false);
    StoreTransaction make_store(std::uint64_t, std::uint64_t, StoreOp, unsigned);
    PrefetchTransaction make_prefetch(std::uint64_t, PrefetchOp, unsigned);
    unsigned vector_shape_uop_count(const RandomVectorShape &);
    unsigned vector_shape_flow_num(const RandomVectorShape &);
    unsigned vector_shape_total_flows(const RandomVectorShape &);
    unsigned vector_shape_deficit(const RandomVectorShape &, unsigned);
    const RandomVectorShape &choose_vector_shape(unsigned, std::optional<unsigned>, bool);
    std::vector<VectorMemoryTransaction> make_random_vector_uops(bool, std::uint64_t, const RandomVectorShape &, bool);
    void sample_constrained_vector_instruction(bool, const RandomVectorShape &, const std::vector<VectorMemoryTransaction> &);
};
// PRODUCTION_BUILDERS_AND_VECTOR_PRODUCERS
} // namespace memblock::scenarios::mixed

// PRODUCTION_STREAM

namespace memblock::scenarios::mixed {
class BudgetFixture {
public:
    RandomConstraints constraints;
    std::mt19937_64 random;
    MixedCoverage coverage;
    std::string phase;
    unsigned actions = 0, target_before_redirect = 0, concurrent_action_limit = 0, budget = 0;
    std::vector<std::string> trace;
    BudgetFixture(std::uint64_t seed, unsigned total, unsigned concurrent, unsigned rate)
        : constraints(RandomConstraints::preset("coverage")), random(seed), target_before_redirect(total),
          concurrent_action_limit(concurrent), budget(concurrent) { constraints.concurrent_stream_per_mille = rate; }
    bool initialize_run() { return true; }
    bool prepare_random_translation_contexts() { return true; }
    bool prepare_tail() { return true; }
    bool run_serial_action() { trace.push_back("S"); ++actions; return true; }
    bool run_concurrent_windows() {
        // Model the legacy helper's public granularity, including its optional
        // special load and unused residual; do not consume a whole allowance.
        while (actions + 7 <= concurrent_action_limit) {
            const unsigned count = actions + 8 <= concurrent_action_limit && random() % 2U ? 8U : 7U;
            actions += count; coverage.concurrent_actions += count;
            trace.push_back("W" + std::to_string(count));
            if (constraints.concurrent_stream_per_mille != 0) return true;
        }
        return true;
    }
    bool run_concurrent_stream(unsigned maximum) {
        require(maximum > 0, "empty stream budget");
        const unsigned count = std::min(maximum, 3U + unsigned(random() % 13U));
        actions += count; coverage.stream[2] += count; trace.push_back("C" + std::to_string(count)); return true;
    }
    bool finish() {
        require(actions == target_before_redirect, "execute did not conserve total action budget");
        const auto used = coverage.stream[2] + coverage.concurrent_actions;
        require(used <= budget, "execute exceeded concurrent budget");
        require(constraints.concurrent_stream_per_mille == 0 ? budget - used < 7U : used == budget,
            "execute lost usable concurrent budget");
        return true;
    }
    bool execute();
};
// PRODUCTION_EXECUTE
} // namespace memblock::scenarios::mixed

static std::string join(const std::vector<std::string> &values) {
    std::string text;
    for (const auto &value : values) { if (!text.empty()) text += ','; text += value; }
    return text;
}
int main(int argc, char **argv) {
    try {
        require(argc >= 2, "fixture mode required");
        const std::string mode = argv[1];
        if (mode == "constraints") {
            unsigned presets = 0, boundaries = 0, rejected = 0;
            require(RandomConstraints{}.concurrent_stream_per_mille == 750, "default stream rate changed");
            for (const auto *name : {"coverage", "spec", "corner", "cmo-stress"}) {
                auto policy = RandomConstraints::preset(name);
                policy.validate();
                require(policy.concurrent_stream_per_mille == 750, "preset lost default stream rate");
                require(policy.summary().find("target_concurrent_stream=750") != std::string::npos,
                    "default stream rate absent from serialized policy");
                ++presets;
                for (unsigned value : {0U, 1000U}) {
                    policy.apply("concurrent-stream=" + std::to_string(value));
                    policy.validate();
                    require(policy.concurrent_stream_per_mille == value, "stream-rate override was not applied");
                    require(policy.summary().find("target_concurrent_stream=" + std::to_string(value) + " ") != std::string::npos,
                        "stream-rate override absent from serialized policy");
                    ++boundaries;
                }
                policy.apply("concurrent-stream=1001");
                try { policy.validate(); }
                catch (const std::invalid_argument &) { ++rejected; }
            }
            require(rejected == presets, "stream rate above 1000 was accepted");
            std::cout << "{\"presets\":" << presets << ",\"boundary_overrides\":" << boundaries
                << ",\"out_of_range_rejections\":" << rejected << "}\n";
        } else if (mode == "stream") {
            require(argc == 6, "stream seed/timing/family/limit required");
            MixedScenario fixture(std::stoull(argv[2]), std::stoul(argv[3]), std::stoul(argv[4]));
            const unsigned limit = std::stoul(argv[5]);
            require(fixture.run_concurrent_stream(limit), "production stream failed: " + fixture.phase);
            auto &env = fixture.environment;
            env.finish(fixture.actions, fixture.coverage, fixture.constraint_coverage);
            require(fixture.actions <= limit && (limit == 0 || fixture.actions != 0), "stream violated caller instruction limit");
            std::cout << "{\"schedule\":\"" << join(env.trace) << "\",\"actions\":" << fixture.actions
                << ",\"peak_live\":" << env.peak_live << ",\"peak_accepted\":" << env.peak_accepted
                << ",\"overlap_admissions\":" << env.overlap_admissions << ",\"continued_admissions\":" << env.continued_admissions
                << ",\"loads_before_older_std\":" << env.loads_before_older_std << ",\"head_checks\":" << env.head_checks
                << ",\"dependency_checks\":" << env.dependency_checks << ",\"issues\":" << env.issues
                << ",\"commits\":" << env.commits << ",\"lq_allocated\":" << env.lq_used
                << ",\"sq_allocated\":" << env.sq_used << ",\"producer_counts\":[";
            for (unsigned i = 0; i < env.producer_counts.size(); ++i) std::cout << (i ? "," : "") << env.producer_counts[i];
            std::cout << "],\"kinds\":[";
            for (unsigned i = 0; i < env.instructions.size(); ++i) std::cout << (i ? "," : "") << env.instructions[i].kind;
            std::cout << "]}\n";
        } else if (mode == "budget") {
            require(argc == 6, "budget seed/total/concurrent/rate required");
            BudgetFixture fixture(std::stoull(argv[2]), std::stoul(argv[3]), std::stoul(argv[4]), std::stoul(argv[5]));
            require(fixture.execute(), "production execute failed");
            std::cout << "{\"schedule\":\"" << join(fixture.trace) << "\",\"actions\":" << fixture.actions
                << ",\"concurrent\":" << fixture.coverage.stream[2] + fixture.coverage.concurrent_actions << "}\n";
        } else throw std::runtime_error("unknown fixture mode");
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n'; return 1;
    }
}
