#!/usr/bin/env python3
"""Scenario compatibility, catalog rejection, and independent-TU contracts."""

from __future__ import annotations

import copy
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))
import scenario_catalog  # noqa: E402


class ScenarioCatalogTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        (self.root / "alpha.inc").write_text(
            "int run_alpha(int argc, char **argv, bool selective = false) "
            "{ return selective ? 9 : 3; }\n"
        )
        self.document = {
            "schema_version": 1,
            "scenarios": [{
                "name": "alpha", "kind": "directed-tests", "group": "alpha",
                "call": "run_alpha(argc, argv)",
                "make_targets": [{"name": "alpha", "args": ""}],
                "benchmark_order": 0,
            }],
        }

    def validate(self, document=None):
        return scenario_catalog.validate_catalog(
            self.document if document is None else document, self.root
        )

    def test_existing_cli_make_defaults_and_benchmark_inventory_are_preserved(self):
        entries = scenario_catalog.load_catalog()
        self.assertEqual(len(entries), 88)
        by_name = {entry["name"]: entry for entry in entries}
        self.assertEqual(by_name["vector-guest-fault"]["call"], "run_vector_guest_fault(argc, argv)")
        self.assertEqual(by_name["vector-guest-fault-split"]["call"], "run_vector_guest_fault(argc, argv, true)")
        self.assertEqual(by_name["translation-fence-sv48-sv39x4-selective"]["call"], "run_translation_fence(argc, argv, true, true, false)")
        self.assertEqual(by_name["translation-inflight-context-sv39-sv48x4"]["call"], "run_translation_inflight_context(argc, argv, false, true)")
        targets = {target["name"]: (entry["name"], target["args"]) for entry in entries for target in entry["make_targets"]}
        self.assertEqual(len(targets), 88)
        self.assertEqual(targets["smoke"], ("smoke", ""))
        self.assertEqual(targets["frontend-bridge"], ("frontend-bridge", "--seed $(or $(SEED),1) --actions $(or $(ACTIONS),4096)"))
        self.assertEqual(targets["constrained-random"], ("constrained-random", "--seed $(or $(SEED),1) --actions $(or $(ACTIONS),16384) $(CONSTRAINT_ARGS)"))
        benchmark = scenario_catalog.benchmark_scenarios(entries)
        self.assertEqual(
            {entry["name"] for entry in benchmark},
            set(by_name) - {
                "cmo-atomic-flush", "cmo-store-flush", "redirect-atomic-flush",
            },
        )
        self.assertEqual({entry["name"]: entry["benchmark_actions"] for entry in benchmark if "benchmark_actions" in entry}, {
            "frontend-bridge": 4096, "vector-guest-fault-boundary": 512,
        })
        self.assertEqual(
            (MEMBLOCK_ROOT / "cpp/scenarios/generated_targets.mk").read_text(),
            scenario_catalog.render_make(entries),
        )
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text()
        self.assertIn("SCENARIO_MAKE := cpp/scenarios/generated_targets.mk", makefile)
        self.assertIn("include $(SCENARIO_MAKE)", makefile)

    def test_rejects_duplicate_scenario_names(self):
        self.document["scenarios"].append(copy.deepcopy(self.document["scenarios"][0]))
        with self.assertRaisesRegex(scenario_catalog.CatalogError, "duplicate scenario"):
            self.validate()

    def test_rejects_duplicate_make_alias(self):
        entry = copy.deepcopy(self.document["scenarios"][0])
        entry.update(name="alpha-alias", benchmark_order=1)
        self.document["scenarios"].append(entry)
        with self.assertRaisesRegex(scenario_catalog.CatalogError, "duplicate make target"):
            self.validate()

    def test_rejects_missing_and_unregistered_modules(self):
        self.document["scenarios"][0]["group"] = "absent"
        with self.assertRaisesRegex(scenario_catalog.CatalogError, "missing scenario module"):
            self.validate()
        self.document["scenarios"][0]["group"] = "alpha"
        (self.root / "orphan.inc").write_text("int run_orphan() {}")
        with self.assertRaisesRegex(scenario_catalog.CatalogError, "unregistered scenario modules: orphan"):
            self.validate()

    def test_rejects_unknown_functions_and_arbitrary_call_expressions(self):
        for call, message in (
            ("run_missing(argc, argv)", "unknown scenario function"),
            ("run_alpha(argc, argv); abort()", "unsupported call expression"),
            ("run_alpha(argc, argv, 42)", "unsupported call expression"),
            ("run_alpha(argc, argv, options.seed)", "unsupported call expression"),
        ):
            with self.subTest(call=call):
                self.document["scenarios"][0]["call"] = call
                with self.assertRaisesRegex(scenario_catalog.CatalogError, message):
                    self.validate()

    def test_alias_arguments_and_benchmark_exclusion(self):
        entry = copy.deepcopy(self.document["scenarios"][0])
        entry.update(name="alpha-selective", call="run_alpha(argc, argv, true)", make_targets=[], benchmark_order=None)
        self.document["scenarios"].append(entry)
        entries = self.validate()
        self.assertEqual([e["name"] for e in scenario_catalog.benchmark_scenarios(entries)], ["alpha"])
        outputs = scenario_catalog.render_modules(entries)
        self.assertIn("return run_alpha(argc, argv, true);", outputs["module_alpha.cpp"])
        self.assertIn('{"alpha-selective", &scenario_alpha_selective}', outputs["registry.cpp"])

    @unittest.skipUnless(shutil.which("g++"), "host C++ compiler unavailable")
    def test_independent_modules_link_lookup_aliases_and_unknown_names(self):
        (self.root / "beta.inc").write_text(
            "int run_beta(int argc, char **argv, const Options &options) "
            "{ return static_cast<int>(options.transactions); }\n"
        )
        self.document["scenarios"].extend([
            {"name": "alpha-selective", "kind": "directed-tests", "group": "alpha", "call": "run_alpha(argc, argv, true)", "make_targets": [], "benchmark_order": 1},
            {"name": "beta", "kind": "directed-tests", "group": "beta", "call": "run_beta(argc, argv, options)", "make_targets": [], "benchmark_order": 2},
        ])
        entries = self.validate()
        (self.root / "shared.hpp").write_text(
            "#pragma once\n#include <array>\n#include <string_view>\n"
            "namespace memblock::scenarios { struct Options { unsigned transactions = 17; }; }\n"
        )
        shutil.copyfile(MEMBLOCK_ROOT / "cpp/scenarios/registry.hpp", self.root / "registry.hpp")
        sources = scenario_catalog.write_modules(entries, self.root)
        (self.root / "host.cpp").write_text('''#include "registry.hpp"
int main() {
    const memblock::scenarios::Options options;
    const auto entries = memblock::scenarios::scenario_catalog();
    if (entries.size() != 3) return 1;
    if (entries[0].run(0, nullptr, options) != 3) return 2;
    if (entries[1].run(0, nullptr, options) != 9) return 3;
    if (entries[2].run(0, nullptr, options) != 17) return 4;
    for (const auto &entry : entries) if (entry.name == "unknown") return 5;
    return 0;
}
''')
        binary = self.root / "host"
        compiled = subprocess.run(
            ["g++", "-std=c++20", "-I", str(self.root), *map(str, sources), str(self.root / "host.cpp"), "-o", str(binary)],
            capture_output=True, text=True,
        )
        self.assertEqual(compiled.returncode, 0, compiled.stdout + compiled.stderr)
        subprocess.run([str(binary)], check=True)


if __name__ == "__main__":
    unittest.main()
