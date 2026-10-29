// Independent public-edge/generation obligations for the pure controller.
// No DUT, queue-count model, architectural data oracle or native build is used.
#include "environment/memory_replay_controller.hpp"
#include <random>

using namespace memblock;
using Controller = MemoryReplayController;

static void require(bool value, const std::string &message)
{
    if (!value) throw std::runtime_error(message);
}

static Controller::Instruction instruction(int ordinal, bool replayable = true)
{
    Controller::Instruction result;
    const unsigned packed = (ordinal % 320 + 320) % 320;
    result.rob = packed % 160; result.flag = packed / 160;
    result.ftq = (ordinal / 2 + 640) % 64;
    result.offset = (ordinal + 320) % 8;
    result.ftq_flag = (ordinal / 128 + 4) % 2;
    result.rvc = ordinal % 2;
    result.replayable_load = replayable;
    return result;
}

static generated::MemoryViolation request(const Controller::Instruction &item,
                                           bool self = true)
{
    generated::MemoryViolation result{};
    result.valid = true;
    result.rob_value = item.rob; result.rob_flag = item.flag;
    result.ftq_value = item.ftq; result.ftq_flag = item.ftq_flag;
    result.ftq_offset = item.offset; result.is_rvc = item.rvc;
    result.level = self;
    return result;
}

static bool same_request(const generated::MemoryViolation &a,
                         const generated::MemoryViolation &b)
{
    return a.valid == b.valid && a.rob_value == b.rob_value &&
        a.rob_flag == b.rob_flag && a.ftq_value == b.ftq_value &&
        a.ftq_flag == b.ftq_flag && a.ftq_offset == b.ftq_offset &&
        a.is_rvc == b.is_rvc && a.level == b.level;
}

struct Group {
    Controller::Instruction instruction;
    int ordinal;
    std::uint64_t generation;
    bool retired = false;
    std::optional<std::uint64_t> canceled;
    // Sibling completion is speculative. Even a completed sibling belongs to
    // its architectural generation until prefix retirement or cancellation.
    std::array<bool, 4> sibling_completed{true, false, true, false};
};

struct Event {
    std::uint64_t id, generation, sampled;
    generated::MemoryViolation request;
    std::vector<std::uint64_t> killed;
};

struct Model {
    Controller controller;
    std::vector<Group> groups;
    std::vector<Event> events;
    // Independent pipelines advance one slot per public edge. They encode two
    // return edges and four count-publication edges without inspecting source.
    std::array<std::optional<Event>, 2> return_pipe{};
    std::array<std::optional<Event>, 4> count_pipe{};
    unsigned filter_ticks = 0;
    std::optional<Event> filter;
    std::uint64_t filtered = 0;
    unsigned edges = 0;

    void add(int ordinal)
    {
        auto input = instruction(ordinal);
        const auto generation = controller.admit(input);
        require(generation.has_value(), "legal independent instruction admission failed");
        require(groups.empty() || *generation > groups.back().generation,
                "architectural generation identity was reused");
        groups.push_back({input, ordinal, *generation});
    }
    explicit Model(int base = 155, unsigned count = 8)
    {
        for (unsigned index = 0; index < count; ++index) add(base + index);
    }
    void check_groups() const
    {
        std::size_t live = 0;
        for (const auto &group : groups) {
            live += !group.retired;
            require(controller.will_cancel(group.generation) ==
                        (!group.retired && group.canceled.has_value()),
                    "generation cancellation differs from independent instruction suffix");
            // Every sibling has exactly the generation's fate. Its speculative
            // completion status grants no exemption from level1 cancellation.
            for (bool completed : group.sibling_completed) {
                (void)completed;
                require(controller.will_cancel(group.generation) ==
                            group.canceled.has_value(),
                        "completed or pending vector sibling escaped generation cancellation");
            }
        }
        require(controller.instructions().size() == live,
                "controller lost or duplicated a registered generation");
    }
    static void compare(const std::optional<Controller::Return> &actual,
                        const std::optional<Event> &expected,
                        const char *phase)
    {
        require(actual.has_value() == expected.has_value(),
                std::string(phase) + " action appeared on the wrong public edge");
        if (!expected) return;
        require(actual->id == expected->id && actual->generation == expected->generation &&
                    actual->request_edge == expected->sampled &&
                    actual->return_edge == expected->sampled + 2 &&
                    same_request(actual->request, expected->request),
                std::string(phase) + " action lost an immutable request identity");
        require(actual->newly_killed == expected->killed,
                "newly killed generation set violates instruction suffix");
    }
    bool step(std::uint64_t edge, std::optional<unsigned> victim = {}, bool self = true)
    {
        std::optional<Event> selected;
        bool legal = true;
        generated::MemoryViolation input{};
        if (victim) {
            const auto &group = groups.at(*victim);
            input = request(group.instruction, self);
            const auto anchor = std::find_if(groups.begin(), groups.end(), [&](const auto &candidate) {
                return filter && candidate.generation == filter->generation;
            });
            const bool filter_kills = filter && anchor != groups.end() &&
                (group.ordinal > anchor->ordinal ||
                 (filter->request.level && group.ordinal == anchor->ordinal));
            if (group.retired) legal = false;
            else if (filter_ticks && filter_kills) ++filtered;
            else if (group.canceled) legal = false;
            else {
                selected = Event{events.size() + 1, group.generation, edge, input, {}};
                for (auto &candidate : groups) {
                    if (!candidate.retired && !candidate.canceled &&
                        (candidate.ordinal > group.ordinal ||
                         (self && candidate.ordinal == group.ordinal))) {
                        candidate.canceled = selected->id;
                        selected->killed.push_back(candidate.generation);
                    }
                }
                events.push_back(*selected);
            }
        }
        auto action = controller.plan(edge, input);
        ++edges;
        if (!legal) {
            require(!controller.ok(), "controller accepted an invalid public edge");
            return false;
        }
        require(controller.ok(), "controller rejected a legal public edge: " +
                    controller.failures().message());
        compare(action.redirect, return_pipe.front(), "return");
        compare(action.sample_cancellation, count_pipe.front(), "cancellation-count");
        if (action.redirect)
            require(controller.returned(action.redirect->id, edge),
                    "exact return acknowledgment was rejected");
        if (action.sample_cancellation)
            require(controller.cancellation_observed(action.sample_cancellation->id, edge),
                    "exact count acknowledgment was rejected");
        require(controller.selected() == events.size() && controller.filtered() == filtered,
                "selected/filtered public-event accounting differs from independent history");
        check_groups();
        for (unsigned index = 1; index < return_pipe.size(); ++index)
            return_pipe[index - 1] = return_pipe[index];
        return_pipe.back() = selected;
        for (unsigned index = 1; index < count_pipe.size(); ++index)
            count_pipe[index - 1] = count_pipe[index];
        count_pipe.back() = selected;
        if (selected) { filter = selected; filter_ticks = 4; }
        else if (filter_ticks) --filter_ticks;
        return true;
    }
};

static void timeline()
{
    for (bool self : {false, true}) {
        Model model;
        for (unsigned edge = 10; edge <= 19; ++edge) {
            require(model.step(edge, edge == 10 ? std::optional<unsigned>{3} : std::nullopt, self),
                    "isolated timeline rejected");
            require(model.controller.recovery_ready(edge) == (edge == 19),
                    "isolated recovery minimum differs from stepped return plus six guard edges and next edge");
        }
        require(model.events.size() == 1 && model.events[0].killed.size() == (self ? 5 : 4),
                "flush level did not preserve/cancel the anchor generation");
        require(model.controller.finish_recovery(19, true), "certified isolated recovery failed");
        require(!model.controller.recovering(), "finished recovery retained a redirect history");
    }
    std::cout << "isolated_timelines=2\n";
}

static void cross_product()
{
    unsigned accepted = 0, rejected = 0, edge_count = 0;
    for (int base : {0, 155, 316})
        for (unsigned first = 1; first <= 5; ++first)
            for (unsigned second = 0; second < 8; ++second)
                for (bool first_self : {false, true})
                    for (bool second_self : {false, true})
                        for (unsigned gap = 1; gap <= 5; ++gap) {
                            Model model(base);
                            bool legal = true;
                            for (unsigned edge = 0; edge <= gap + 9; ++edge) {
                                std::optional<unsigned> victim;
                                if (edge == 0) victim = first;
                                if (edge == gap) victim = second;
                                if (!model.step(edge, victim, edge == 0 ? first_self : second_self)) {
                                    legal = false; break;
                                }
                                const unsigned last_request = model.events.back().sampled;
                                require(model.controller.recovery_ready(edge) == (edge >= last_request + 9),
                                        "adjacent return recovery used an earlier redirect");
                            }
                            accepted += legal; rejected += !legal;
                            edge_count += model.edges;
                        }
    require(accepted && rejected, "cross product did not cover accepted and rejected request histories");
    std::cout << "cross_product_cases=" << accepted + rejected << " accepted=" << accepted
              << " rejected=" << rejected << " public_edges=" << edge_count << '\n';
}

static void random_sequences()
{
    unsigned accepted = 0, rejected = 0, selected = 0, filtered = 0, edges = 0;
    for (unsigned seed = 0; seed < 512; ++seed) {
        std::mt19937_64 random(seed);
        Model model((random() % 320), 12);
        bool legal = true;
        for (unsigned edge = 0; edge < 32; ++edge) {
            std::optional<unsigned> victim;
            if (edge < 20 && (edge == 0 || random() % 3 == 0)) victim = random() % 12;
            if (!model.step(edge, victim, random() % 2)) { legal = false; break; }
        }
        accepted += legal; rejected += !legal;
        selected += model.events.size(); filtered += model.filtered; edges += model.edges;
    }
    require(accepted && rejected && selected > 512 && filtered > 100,
            "short randomized histories missed selection/filter/rejection activity");
    std::cout << "random_sequences=512 accepted=" << accepted << " rejected=" << rejected
              << " selected=" << selected << " filtered=" << filtered << " public_edges=" << edges << '\n';
}

static Controller single_pending(unsigned through, bool return_ack = true, bool count_ack = true)
{
    Controller controller;
    require(controller.admit(instruction(159)).has_value(), "negative setup admission");
    for (unsigned edge = 0; edge <= through; ++edge) {
        const auto action = controller.plan(edge, edge == 0 ? request(instruction(159))
                                                          : generated::MemoryViolation{});
        require(controller.ok(), "negative setup rejected an earlier edge");
        if (action.redirect && return_ack)
            require(controller.returned(action.redirect->id, edge), "negative setup return");
        if (action.sample_cancellation && count_ack)
            require(controller.cancellation_observed(action.sample_cancellation->id, edge),
                    "negative setup count");
    }
    return controller;
}

static void acknowledgments()
{
    unsigned cases = 0;
    {
        auto c = single_pending(2, false); c.plan(3);
        require(!c.ok(), "missing return acknowledgment was accepted"); ++cases;
    }
    {
        auto c = single_pending(4, true, false); c.plan(5);
        require(!c.ok(), "missing cancellation acknowledgment was accepted"); ++cases;
    }
    for (unsigned fault = 0; fault < 4; ++fault) {
        auto c = single_pending(fault == 2 ? 1 : 2, false);
        const bool ok = c.returned(fault == 0 ? 99 : 1, fault == 1 ? 1 : fault == 2 ? 1 : 2);
        if (fault == 3) {
            require(ok, "duplicate return setup");
            require(!c.returned(1, 2), "duplicate return acknowledgment was accepted");
        } else require(!ok, "wrong/early return acknowledgment was accepted");
        require(!c.ok(), "invalid return acknowledgment did not poison controller"); ++cases;
    }
    for (unsigned fault = 0; fault < 4; ++fault) {
        auto c = single_pending(fault == 2 ? 3 : 4, true, false);
        const bool ok = c.cancellation_observed(fault == 0 ? 99 : 1,
                                               fault == 1 ? 3 : fault == 2 ? 3 : 4);
        if (fault == 3) {
            require(ok, "duplicate cancellation setup");
            require(!c.cancellation_observed(1, 4), "duplicate cancellation acknowledgment was accepted");
        } else require(!ok, "wrong/early cancellation acknowledgment was accepted");
        require(!c.ok(), "invalid count acknowledgment did not poison controller"); ++cases;
    }
    for (unsigned bad_edge : {0U, 2U, 99U}) {
        auto c = single_pending(0); c.plan(bad_edge);
        require(!c.ok(), "skipped or repeated public edge was accepted"); ++cases;
    }
    std::cout << "rejected_acknowledgment_edge_cases=" << cases << '\n';
}

static void identity()
{
    unsigned rejected = 0;
    for (unsigned field = 0; field < 7; ++field) {
        Controller c;
        auto input = instruction(159, field != 6);
        require(c.admit(input).has_value(), "identity setup failed");
        auto bad = request(input);
        switch (field) {
        case 0: bad.rob_value = 158; break;
        case 1: bad.rob_flag = !bad.rob_flag; break;
        case 2: bad.ftq_value ^= 1; break;
        case 3: bad.ftq_flag = !bad.ftq_flag; break;
        case 4: bad.ftq_offset ^= 1; break;
        case 5: bad.is_rvc = !bad.is_rvc; break;
        default: break;
        }
        c.plan(0, bad);
        require(!c.ok() && c.selected() == 0,
                "FTQ/ROB mismatch or unsupported victim selected a return"); ++rejected;
    }
    for (unsigned field = 0; field < 3; ++field) {
        Controller c; auto bad = instruction(0);
        if (field == 0) bad.rob = 160;
        if (field == 1) bad.ftq = 64;
        if (field == 2) bad.offset = 8;
        require(!c.admit(bad) && !c.ok(), "out-of-range registered identity was admitted"); ++rejected;
    }
    for (int second : {8, 7, 168}) {
        Controller c;
        require(c.admit(instruction(8)).has_value(), "age setup failed");
        require(!c.admit(instruction(second)) && !c.ok(),
                "duplicate/nonincreasing/half-window live age was admitted"); ++rejected;
    }
    std::cout << "rejected_identity_cases=" << rejected << '\n';
}

static void recovery()
{
    unsigned rejected = 0;
    {
        auto c = single_pending(4);
        require(!c.recovery_ready(9), "future unstepped edge certified recovery readiness");
        require(!c.finish_recovery(9, true) && !c.ok(),
                "future unstepped edge finished recovery"); ++rejected;
    }
    {
        auto c = single_pending(9);
        require(c.recovery_ready(9), "certification setup minimum failed");
        require(!c.finish_recovery(9, false) && !c.ok(),
                "timer alone certified producer recovery"); ++rejected;
    }
    {
        auto c = single_pending(8);
        require(!c.finish_recovery(8, true) && !c.ok(),
                "recovery bypassed the next-edge admission policy"); ++rejected;
    }
    {
        Controller c; c.plan(0);
        require(!c.finish_recovery(0, true) && !c.ok(), "empty history finished recovery"); ++rejected;
    }
    {
        auto c = single_pending(0);
        require(!c.admit(instruction(160)) && !c.ok(),
                "new instruction was admitted before producer recovery"); ++rejected;
    }
    unsigned fresh = 0;
    for (bool self : {false, true}) {
        Model m;
        for (unsigned edge = 0; edge <= 9; ++edge)
            require(m.step(edge, edge == 0 ? std::optional<unsigned>{3} : std::nullopt, self),
                    "fresh generation recovery setup failed");
        const auto prior_generation = m.groups.back().generation;
        for (unsigned prefix = 0; prefix < 3; ++prefix) {
            require(m.controller.retire(m.groups[prefix].generation),
                    "uncanceled architectural prefix could not retire during recovery");
            m.groups[prefix].retired = true;
        }
        require(m.controller.finish_recovery(9, true), "certified producer recovery failed");
        const unsigned first_replay = self ? 3 : 4;
        const unsigned surviving = self ? 0 : 1;
        require(m.controller.instructions().size() == surviving,
                "recovery retained canceled generations or erased the level0 anchor");
        for (unsigned index = first_replay; index < 8; ++index) {
            require(!m.controller.will_cancel(m.groups[index].generation),
                    "recovery retained an old canceled generation");
            const auto generation = m.controller.admit(m.groups[index].instruction);
            require(generation && *generation > prior_generation &&
                        !m.controller.will_cancel(*generation),
                    "same numerical identity did not create a fresh generation");
            ++fresh;
        }
        const auto next = m.controller.plan(10, request(m.groups[first_replay].instruction));
        require(m.controller.ok() && !next.redirect && m.controller.selected() == 2,
                "fresh generation inherited the old redirect filter");
    }
    std::cout << "rejected_recovery_cases=" << rejected << " fresh_generations=" << fresh << '\n';
}

static void retirement()
{
    unsigned rejected = 0;
    for (unsigned fault = 0; fault < 3; ++fault) {
        Model m;
        m.step(0, 3, true);
        if (fault == 0) {
            for (unsigned prefix = 0; prefix < 3; ++prefix)
                require(m.controller.retire(m.groups[prefix].generation),
                        "canceled-head retirement setup failed");
        }
        const auto generation = fault == 0 ? m.groups[3].generation :
                                fault == 1 ? m.groups[1].generation : 9999;
        require(!m.controller.retire(generation) && !m.controller.ok(),
                "killed/nonprefix/unknown generation retired"); ++rejected;
    }
    Model legal;
    require(legal.controller.retire(legal.groups[0].generation), "oldest prefix retirement failed");
    legal.groups[0].retired = true;
    require(legal.step(0, 3, true), "retired prefix disturbed selected suffix");
    require(legal.controller.retire(legal.groups[1].generation),
            "older prefix could not retire during redirect propagation");
    legal.groups[1].retired = true;
    const auto action = legal.controller.plan(1, request(legal.groups[0].instruction));
    (void)action;
    require(!legal.controller.ok(), "retired victim was accepted as a fresh memory request");
    std::cout << "rejected_retirement_cases=" << rejected + 1 << " legal_prefix_retirements=2\n";
}

int main(int argc, char **argv)
{
    try {
        const std::string mode = argc > 1 ? argv[1] : "";
        if (mode == "timeline") timeline();
        else if (mode == "cross") cross_product();
        else if (mode == "random") random_sequences();
        else if (mode == "ack") acknowledgments();
        else if (mode == "identity") identity();
        else if (mode == "recovery") recovery();
        else if (mode == "retirement") retirement();
        else return 2;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n'; return 1;
    }
}
