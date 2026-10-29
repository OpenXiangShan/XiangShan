#!/usr/bin/env python3
"""Check shared unresolved execution epochs and the isolated readback contract."""
import json
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

CPP = r'''
#include "scenarios/mixed/unresolved_load_group.hpp"
#include <iostream>
#include <type_traits>

using namespace memblock::scenarios::mixed;
static void require(bool value, const char *message) {
    if (!value) throw std::runtime_error(message);
}
struct Load {
    unsigned rob; bool rob_flag; std::uint64_t ftq_ptr; unsigned ftq_offset;
    bool predecode_rvc; unsigned pdest;
};
struct Redirect {
    bool valid = true, level = true, ftq_flag = false;
    unsigned rob_value; bool rob_flag; std::uint64_t ftq_value;
    unsigned ftq_offset; bool is_rvc;
};
static std::vector<Load> loads() {
    return {{158, false, 10, 0, false, 1}, {159, false, 10, 2, true, 2},
            {0, true, 10, 3, false, 3}, {1, true, 10, 5, true, 4}};
}
static Redirect redirect_for(const Load &load) {
    return {true, true, false, load.rob, load.rob_flag, load.ftq_ptr,
            load.ftq_offset, load.predecode_rvc};
}
template<class Ledger>
concept HasReadback = requires(Ledger &ledger) { ledger.readback(); };
static_assert(!HasReadback<UnresolvedLoadEpochs>);
static_assert(HasReadback<UnresolvedLoadGroupObligation>);
static_assert(std::is_constructible_v<UnresolvedLoadEpochs, const std::vector<Load> &,
                                    unsigned, std::optional<unsigned>>);
static_assert(std::is_constructible_v<UnresolvedLoadGroupObligation, const std::vector<Load> &,
                                    unsigned, std::optional<unsigned>>);

template<class Ledger>
static void provisional(Ledger &ledger) {
    for (unsigned index : {2U, 0U, 3U, 1U})
        require(ledger.provisional(index), "legal initial execution rejected");
}
template<class Ledger>
static void round(Ledger &ledger, std::vector<Load> &current, unsigned victim) {
    require(ledger.redirect(redirect_for(current[victim]), victim), "legal redirect rejected");
    require(!ledger.done(), "redirected epoch completed early");
    require(ledger.returned(), "legal redirect return rejected");
    require(!ledger.done(), "returned epoch completed early");
    reset_unresolved_ftq_suffix(current, victim);
    for (unsigned index = current.size(); index > victim; --index)
        require(ledger.refetched(index - 1, current[index - 1]), "legal suffix rejected");
    require(!ledger.done() && ledger.finish_round(), "completed suffix rejected");
}
template<class Ledger>
static void terminal(Ledger &ledger) {
    require(!ledger.done() && ledger.resolved(), "legal resolution rejected");
    if constexpr (HasReadback<Ledger>) {
        require(!ledger.done(), "isolated group completed without readback");
        require(ledger.readback() && ledger.done(), "isolated readback did not complete");
    } else {
        require(ledger.done(), "execution ledger did not complete at resolution");
    }
}
template<class Ledger>
static void lifecycle() {
    auto current = loads(); Ledger ledger(current, 4);
    require(!ledger.done(), "new epoch already complete");
    provisional(ledger);
    for (unsigned victim : {2U, 0U, 3U, 1U}) round(ledger, current, victim);
    require(ledger.rounds() == 4, "lost recovery round");
    for (unsigned index = 0; index < current.size(); ++index)
        require(ledger.executions(index) == index + 2, "lost or duplicate execution");
    terminal(ledger);

    current = loads(); Ledger disjoint(current, 4); provisional(disjoint);
    terminal(disjoint);
    require(disjoint.rounds() == 0, "disjoint control gained a recovery round");

    current = loads(); Ledger cold(current, 4, 3);
    for (unsigned index : {1U, 0U, 2U})
        require(cold.provisional(index), "warm prefix rejected");
    require(cold.redirect(redirect_for(current[1]), 1) && cold.initial_canceled(3) &&
            cold.returned(), "legal initial cancellation rejected");
    reset_unresolved_ftq_suffix(current, 1);
    for (unsigned index : {3U, 1U, 2U})
        require(cold.refetched(index, current[index]), "cold suffix rejected");
    require(cold.finish_round(), "cold round incomplete");
    round(cold, current, 2);
    require(cold.executions(0) == 1 && cold.executions(1) == 2 &&
            cold.executions(2) == 3 && cold.executions(3) == 2,
            "cancellation counted as a completed execution");
    terminal(cold);
    std::cout << "{\"recovery_rounds\":4,\"executions\":[2,3,4,5],"
              << "\"cold_rounds\":2,\"readback_required\":"
              << (HasReadback<Ledger> ? "true" : "false") << "}\n";
}
template<class Ledger>
static void rejected(Ledger &ledger, bool accepted, unsigned count) {
    require(!accepted && !ledger.done(), "illegal transition accepted");
    const auto current = loads();
    const auto rounds = ledger.rounds();
    std::vector<unsigned> executions;
    for (unsigned index = 0; index < count; ++index)
        executions.push_back(ledger.executions(index));
    require(!ledger.provisional(0) && !ledger.redirect(redirect_for(current[0]), 0) &&
            !ledger.initial_canceled(3) && !ledger.returned() &&
            !ledger.refetched(0, current[0]) && !ledger.finish_round() &&
            !ledger.resolved(), "failed ledger recovered");
    if constexpr (HasReadback<Ledger>)
        require(!ledger.readback(), "readback revived failed ledger");
    require(!ledger.done() && ledger.rounds() == rounds, "failure changed terminal state or rounds");
    for (unsigned index = 0; index < count; ++index)
        require(ledger.executions(index) == executions[index], "failure changed execution count");
}
template<class Ledger>
static void invalid() {
    unsigned cases = 0;
    for (unsigned fault = 0; fault < 35; ++fault) {
        auto current = loads(); Ledger ledger(current, fault == 31 ? 1 : 4);
        bool accepted = false;
        if (fault == 0) accepted = ledger.resolved();
        else if (fault == 1) accepted = ledger.returned();
        else if (fault == 2) accepted = ledger.refetched(0, current[0]);
        else if (fault == 3) accepted = ledger.finish_round();
        else if (fault == 4) accepted = ledger.initial_canceled(3);
        else if (fault == 5) { provisional(ledger); accepted = ledger.provisional(0); }
        else if (fault == 6) accepted = ledger.provisional(current.size());
        else if (fault == 7) {
            require(ledger.provisional(0), "partial warm setup failed");
            accepted = ledger.redirect(redirect_for(current[0]), 0);
        } else if (fault <= 16) {
            provisional(ledger); auto event = redirect_for(current[1]);
            if (fault == 8) event.valid = false;
            if (fault == 9) event.level = false;
            if (fault == 10) event.ftq_flag = true;
            if (fault == 11) ++event.rob_value;
            if (fault == 12) event.rob_flag = !event.rob_flag;
            if (fault == 13) ++event.ftq_value;
            if (fault == 14) ++event.ftq_offset;
            if (fault == 15) event.is_rvc = !event.is_rvc;
            accepted = ledger.redirect(event, fault == 16 ? current.size() : 1);
        } else if (fault == 31) {
            provisional(ledger); round(ledger, current, 1);
            accepted = ledger.redirect(redirect_for(current[1]), 1);
        } else if (fault == 32) {
            // FTQ wrap remains outside the bounded witness contract.
            current[1].ftq_ptr = 63; Ledger wrap(current, 4); provisional(wrap);
            require(wrap.redirect(redirect_for(current[1]), 1), "wrap setup failed");
            rejected(wrap, wrap.returned(), current.size()); ++cases; continue;
        } else if (fault == 33) {
            current.push_back({2, true, 10, 7, false, 5});
            current[1].predecode_rvc = current[3].predecode_rvc = false;
            Ledger wide(current, 4);
            for (unsigned index = 0; index < current.size(); ++index)
                require(wide.provisional(index), "packet setup failed");
            require(wide.redirect(redirect_for(current[0]), 0), "packet redirect failed");
            rejected(wide, wide.returned(), current.size()); ++cases; continue;
        } else if (fault == 34) {
            provisional(ledger); terminal(ledger); accepted = ledger.resolved();
        } else {
            provisional(ledger);
            require(ledger.redirect(redirect_for(current[1]), 1) && ledger.returned(), "suffix setup failed");
            if (fault == 17) accepted = ledger.refetched(0, current[0]);
            else if (fault == 18) accepted = ledger.refetched(1, current[1]);
            else {
                reset_unresolved_ftq_suffix(current, 1); auto value = current[1];
                if (fault == 19) ++value.rob;
                if (fault == 20) value.rob_flag = !value.rob_flag;
                if (fault == 21) ++value.pdest;
                if (fault == 22) ++value.ftq_ptr;
                if (fault == 23) ++value.ftq_offset;
                if (fault == 24) value.predecode_rvc = !value.predecode_rvc;
                if (fault <= 24) accepted = ledger.refetched(1, value);
                else if (fault == 25) {
                    require(ledger.refetched(1, value), "duplicate setup failed");
                    accepted = ledger.refetched(1, value);
                } else if (fault == 26) accepted = ledger.finish_round();
                else if (fault == 27) accepted = ledger.returned();
                else if (fault == 28) accepted = ledger.redirect(redirect_for(current[2]), 2);
                else if (fault == 29) accepted = ledger.resolved();
                else if (fault == 30) {
                    for (unsigned index = 1; index < current.size(); ++index)
                        require(ledger.refetched(index, current[index]), "finish setup failed");
                    require(ledger.finish_round(), "first finish failed");
                    accepted = ledger.finish_round();
                }
            }
        }
        rejected(ledger, accepted, current.size()); ++cases;
    }
    for (unsigned fault = 0; fault < 6; ++fault) {
        auto current = loads();
        if (fault == 0) current.clear();
        if (fault == 4) current.resize(1);
        const std::optional<unsigned> cold = fault < 2 ? std::nullopt :
            std::optional<unsigned>(fault == 2 ? 0U : fault == 3 ? 4U : fault == 4 ? 0U : 64U);
        Ledger ledger(current, fault == 1 ? 0 : 4, cold);
        rejected(ledger, ledger.resolved(), current.size()); ++cases;
    }
    for (unsigned fault = 0; fault < 7; ++fault) {
        auto current = loads(); Ledger ledger(current, 4, 3);
        bool accepted = false;
        if (fault == 0) accepted = ledger.provisional(3);
        else {
            for (unsigned index = 0; index < (fault == 1 ? 2U : 3U); ++index)
                require(ledger.provisional(index), "cold setup failed");
            if (fault == 1 || fault == 2)
                accepted = ledger.redirect(redirect_for(current[fault == 1 ? 0 : 3]), fault == 1 ? 0 : 3);
            else {
                require(ledger.redirect(redirect_for(current[1]), 1), "cold redirect setup failed");
                if (fault == 3) accepted = ledger.returned();
                if (fault == 4) accepted = ledger.initial_canceled(2);
                if (fault >= 5) {
                    require(ledger.initial_canceled(3), "cold cancellation setup failed");
                    if (fault == 5) accepted = ledger.initial_canceled(3);
                    else {
                        require(ledger.returned(), "cold return setup failed");
                        reset_unresolved_ftq_suffix(current, 1);
                        for (unsigned index = 1; index < current.size(); ++index)
                            require(ledger.refetched(index, current[index]), "second cold round setup failed");
                        require(ledger.finish_round() && ledger.redirect(redirect_for(current[1]), 1), "second cold redirect failed");
                        accepted = ledger.initial_canceled(3);
                    }
                }
            }
        }
        rejected(ledger, accepted, current.size()); ++cases;
    }
    std::cout << "{\"invalid_transitions_rejected\":" << cases << ",\"sticky_failures\":true}\n";
}
static void readback_gates() {
    unsigned cases = 0;
    for (unsigned stage = 0; stage < 5; ++stage) {
        auto current = loads(); UnresolvedLoadGroupObligation ledger(current, 4);
        if (stage >= 1) provisional(ledger);
        if (stage >= 2) require(ledger.redirect(redirect_for(current[1]), 1), "readback redirect setup failed");
        if (stage >= 3) require(ledger.returned(), "readback return setup failed");
        if (stage >= 4) {
            reset_unresolved_ftq_suffix(current, 1);
            for (unsigned index = 1; index < current.size(); ++index)
                require(ledger.refetched(index, current[index]), "readback refetch setup failed");
            require(ledger.finish_round(), "readback finish setup failed");
        }
        rejected(ledger, ledger.readback(), current.size()); ++cases;
    }
    auto current = loads(); UnresolvedLoadGroupObligation ledger(current, 4);
    provisional(ledger); terminal(ledger);
    rejected(ledger, ledger.readback(), current.size()); ++cases;
    std::cout << "{\"premature_readbacks_rejected\":5,\"duplicate_readbacks_rejected\":1,"
              << "\"sticky_failures\":true,\"cases\":" << cases << "}\n";
}
int main(int argc, char **argv) {
    try {
        const std::string mode = argc > 1 ? argv[1] : "";
        if (mode == "core") lifecycle<UnresolvedLoadEpochs>();
        else if (mode == "wrapper") lifecycle<UnresolvedLoadGroupObligation>();
        else if (mode == "invalid-core") invalid<UnresolvedLoadEpochs>();
        else if (mode == "invalid-wrapper") invalid<UnresolvedLoadGroupObligation>();
        else if (mode == "readback") readback_gates();
        else return 2;
    } catch (const std::exception &error) { std::cerr << error.what() << '\n'; return 1; }
}
'''


class UnresolvedExecutionLedgerTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = tempfile.TemporaryDirectory(prefix="memblock-unresolved-execution-ledger-")
        cls.addClassCleanup(cls.directory.cleanup)
        source = Path(cls.directory.name) / "fixture.cpp"
        source.write_text(CPP)
        cls.binary = source.with_suffix("")
        built = subprocess.run(["c++", "-std=c++20", "-O0", "-I", str(ROOT / "cpp"),
                                str(source), "-o", str(cls.binary)],
                               capture_output=True, text=True, timeout=60)
        if built.returncode:
            raise AssertionError(built.stdout + built.stderr)

    def case(self, mode):
        ran = subprocess.run([str(self.binary), mode], capture_output=True, text=True, timeout=15)
        self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
        evidence = json.loads(ran.stdout)
        print("unresolved_execution_ledger_evidence=" + json.dumps(evidence, sort_keys=True))
        return evidence

    def test_core_resolves_without_readback_and_counts_canceled_epochs(self):
        self.assertFalse(self.case("core")["readback_required"])

    def test_isolated_wrapper_still_requires_readback(self):
        self.assertTrue(self.case("wrapper")["readback_required"])

    def test_core_rejects_illegal_transitions_with_sticky_failures(self):
        self.assertEqual(self.case("invalid-core")["invalid_transitions_rejected"], 48)

    def test_wrapper_preserves_shared_illegal_transition_rules(self):
        self.assertEqual(self.case("invalid-wrapper")["invalid_transitions_rejected"], 48)

    def test_wrapper_rejects_premature_and_duplicate_readback(self):
        self.assertEqual(self.case("readback")["cases"], 6)


if __name__ == "__main__":
    unittest.main()
