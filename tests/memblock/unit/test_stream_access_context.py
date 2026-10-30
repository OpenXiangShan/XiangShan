#!/usr/bin/env python3
"""Compile actual ordinary user-root/profile methods against a recording driver.

PTE writes use the real PageTableBuilder; a separate literal walk checks declared
physical relations and U/R/W/A/D bits. Private compiled faults must fail at
their designated semantic check. Optional MEMBLOCK_STREAM_ACCESS_ARTIFACT_DIR
retains generated sources, dependency/method hashes, compiler commands and runs.
"""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
FIXTURE = Path(__file__).with_name("stream_access_context_fixture.cpp")
MAPPING = ROOT / "cpp/scenarios/mixed_translation_mapping.cpp"
SCHEDULE = ROOT / "cpp/scenarios/mixed_translation_schedule.cpp"
METHOD_FILES = {
    **{name: MAPPING for name in (
        "page_mode", "map_stage_page", "map_g_page", "map_stage_napot",
        "prepare_user_translation_pages", "validate_translation_byte",
        "activate_translation_context", "prepare_stream_permission_pages",
        "stream_permission_physical", "validate_stream_permission_load",
    )},
    "choose_stream_access": SCHEDULE,
    "leave_stream_access_context": SCHEDULE,
}


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def production_methods() -> dict[str, str]:
    methods = {}
    for name, path in METHOD_FILES.items():
        source = path.read_text()
        marker = f"MixedScenario::{name}("
        position = source.index(marker)
        begin = source.rfind("\n", 0, position) + 1
        end = source.index("\n}\n", position) + 3
        methods[name] = source[begin:end].replace("MixedScenario::", "Fixture::")
    return methods


def private_fault(methods: dict[str, str], fault: str) -> dict[str, str]:
    result = methods.copy()

    def replace(name: str, old: str, new: str, count: int = 1) -> None:
        if result[name].count(old) != count:
            raise AssertionError(f"{fault}: production mutation anchor changed: {name}")
        result[name] = result[name].replace(old, new)

    if fault == "wrong-root":
        replace("activate_translation_context",
                "(context.user_pages() ? user_stage1_roots : random_stage1_roots)",
                "random_stage1_roots")
        replace("activate_translation_context",
                "(context.user_pages() ? user_vs_roots : random_vs_roots)",
                "random_vs_roots")
    elif fault == "user-bit":
        replace("map_stage_page", "executable, user, noncacheable, io",
                "executable, false, noncacheable, io", 2)
    elif fault == "implicit-g":
        replace("prepare_user_translation_pages", "for (auto page = root;",
                "for (auto page = root + 0x1000;")
    elif fault == "fence-order":
        name = "activate_translation_context"
        source = result[name]
        begin = source.index("    if (!context.user_pages() && !context.mxr) return true;")
        suffix = source[begin:source.rfind("\n}")]
        install = suffix.replace("    if (!context.user_pages() && !context.mxr) return true;\n", "")
        install = install.replace("    return environment.set_translation_permissions(",
                                  "    const bool installed = environment.set_translation_permissions(")
        install += "\n    if (!installed) return false;\n"
        source = source[:begin] + "    return true;\n}\n"
        anchor = "    if (context.regime == RandomConstraints::translation_bare) {"
        if source.count(anchor) != 1:
            raise AssertionError("fence-order: production mutation anchor changed")
        result[name] = source.replace(anchor,
            "    if (context.user_pages()) {\n" + install + "    }\n" + anchor)
    elif fault == "permission-install":
        replace("activate_translation_context", "privilege.mxr, privilege.sum, privilege.vmxr, privilege.vsum",
                "false, false, false, false")
    elif fault == "handoff-omitted":
        replace("leave_stream_access_context", "return enter_translation_context(context);", "return true;")
    elif fault == "handoff-user-context":
        replace("leave_stream_access_context", "context.access = 0;", "context.access = 1;")
    elif fault == "handoff-mode":
        replace("leave_stream_access_context", "context.access = 0;", "context.access = 0; context.g_mode ^= 1;")
    elif fault == "handoff-failure":
        replace("leave_stream_access_context", "return enter_translation_context(context);",
                "enter_translation_context(context); return true;")
    else:
        raise AssertionError(f"unknown private fault: {fault}")
    return result


class StreamAccessContextTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        retained = os.environ.get("MEMBLOCK_STREAM_ACCESS_ARTIFACT_DIR")
        if retained:
            cls.folder = Path(retained).resolve()
            cls.folder.mkdir(parents=True, exist_ok=True)
        else:
            cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-stream-access-context-")
            cls.addClassCleanup(cls.workspace.cleanup)
            cls.folder = Path(cls.workspace.name)
        cls.methods = production_methods()
        cls.report = {
            "schema_version": 1,
            "production_methods": {
                name: {"source": str(METHOD_FILES[name]),
                       "extracted_sha256": hashlib.sha256(source.encode()).hexdigest()}
                for name, source in cls.methods.items()
            },
            "commands": [], "runs": [], "faults": {},
        }
        cls.addClassCleanup(cls.save_report)
        cls.builder_object = cls.folder / "page_table_builder.o"
        cls.compile(["-MMD", "-MF", str(cls.folder / "builder.d"), "-c",
                     str(ROOT / "cpp/environment/page_table_builder.cpp"),
                     "-o", str(cls.builder_object)])
        cls.baseline = cls.build("baseline", cls.methods)
        dependencies = set(METHOD_FILES.values()) | {FIXTURE}
        for filename in ("baseline.d", "builder.d"):
            text = (cls.folder / filename).read_text().replace("\\\n", " ")
            dependencies.update(Path(item).resolve() for item in text.split(":", 1)[1].split())
        cls.input_hashes = {str(path): digest(path) for path in sorted(dependencies)}
        cls.report["dependency_sha256"] = cls.input_hashes

    @classmethod
    def save_report(cls) -> None:
        (cls.folder / "report.json").write_text(json.dumps(cls.report, indent=2) + "\n")

    @classmethod
    def tearDownClass(cls) -> None:
        current = {path: digest(Path(path)) for path in cls.input_hashes}
        if current != cls.input_hashes:
            raise AssertionError("compiled production dependencies changed during host tests")

    @classmethod
    def compile(cls, arguments: list[str]) -> None:
        command = ["c++", "-std=c++20", "-O0", "-I", str(ROOT / "cpp"), *arguments]
        built = subprocess.run(command, capture_output=True, text=True, timeout=60)
        cls.report["commands"].append({"argv": command, "returncode": built.returncode,
                                       "stdout": built.stdout, "stderr": built.stderr})
        if built.returncode:
            raise AssertionError(built.stdout + built.stderr)

    @classmethod
    def build(cls, name: str, methods: dict[str, str]) -> Path:
        source = cls.folder / f"{name}.cpp"
        source.write_text(FIXTURE.read_text().replace("// PRODUCTION_METHODS",
                          "\n".join(methods.values())))
        binary = cls.folder / name
        cls.compile(["-MMD", "-MF", str(cls.folder / f"{name}.d"),
                     str(source), str(cls.builder_object), "-o", str(binary)])
        cls.report.setdefault("compiled_sources", {})[name] = digest(source)
        return binary

    def run_case(self, binary: Path, case: str) -> subprocess.CompletedProcess:
        ran = subprocess.run([str(binary), case], capture_output=True, text=True, timeout=30)
        self.report["runs"].append({"binary": binary.name, "case": case,
                                   "returncode": ran.returncode,
                                   "stdout": ran.stdout, "stderr": ran.stderr})
        return ran

    def check_case(self, case: str) -> str:
        ran = self.run_case(self.baseline, case)
        self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
        self.assertIn(f"{case} PASS", ran.stdout)
        return ran.stdout

    def test_user_roots_identity_napot_permutation_and_implicit_g_pages(self) -> None:
        for case in ("construction", "construction-seed2"):
            with self.subTest(case=case):
                output = self.check_case(case)
                self.assertIn("contexts=12 identity_checks=6336 permuted_checks=6336 "
                              "napot_checks=192 implicit_pages=2048 immutable_normal_roots=4", output)

    def test_identity_only_setup_and_disabled_user_profile(self) -> None:
        self.assertIn("permuted_checks=0", self.check_case("construction-no-permutation"))
        self.assertIn("one_hot_profiles=3 weighted_counts=20,30,50 replay_choices=1024 bare_controls=8",
                      self.check_case("profiles"))

    def test_execute_only_maps_and_mxr_controls(self) -> None:
        self.assertIn("permission_map_checks=2304 denied=1152 mxr_reads=1152",
                      self.check_case("permission-maps"))

    def test_access_aware_permission_counter_controls(self) -> None:
        self.assertIn("permission_controls=282", self.check_case("permissions"))

    def test_privileged_activation_roots_fence_order_and_final_bundles(self) -> None:
        self.assertIn("activation_contexts=21", self.check_case("activation"))

    def test_user_and_boundary_table_pools_preserve_independent_images(self) -> None:
        self.assertIn("pool_cases=8 disjoint_owner_pairs=528 boundary_controls=48 construction_orders=2",
                      self.check_case("pool-isolation"))

    def test_user_epoch_returns_to_privileged_legacy_context(self) -> None:
        self.assertIn("handoff_contexts=14 noop_contexts=8 rejected_entries=14", self.check_case("handoff"))

    def test_private_handoff_omission_context_mode_and_failure_faults_reject(self) -> None:
        for fault, marker in (("handoff-omitted", "handoff-entry-count"),
                              ("handoff-user-context", "handoff-legacy-context"),
                              ("handoff-mode", "handoff-legacy-context"),
                              ("handoff-failure", "handoff-entry-failure-suppressed")):
            with self.subTest(fault=fault):
                ran = self.run_case(self.build(fault, private_fault(self.methods, fault)), "handoff")
                self.assertEqual(ran.returncode, 1, ran.stdout + ran.stderr)
                self.assertIn(marker, ran.stderr)
                self.report["faults"][fault] = {"case": "handoff", "designated_rejection": marker}

    def test_private_root_and_child_storage_aliases_reject(self) -> None:
        anchor = "struct Fixture : AddressConstants {"
        self.assertEqual(FIXTURE.read_text().count(anchor), 1)
        for name, offset in (("root-alias", 0), ("child-alias", 0x4000)):
            with self.subTest(fault=name):
                source = self.folder / f"{name}.cpp"
                text = FIXTURE.read_text().replace(anchor, anchor +
                    "\n    static constexpr std::array<std::uint64_t, 2> user_stage1_roots{{" +
                    f"gstage_boundary_roots[0] + {offset}, gstage_boundary_roots[1] + {offset}" + "}};")
                source.write_text(text.replace("// PRODUCTION_METHODS", "\n".join(self.methods.values())))
                binary = self.folder / name
                self.compile([str(source), str(self.builder_object), "-o", str(binary)])
                ran = self.run_case(binary, "pool-isolation")
                self.assertEqual(ran.returncode, 1, ran.stdout + ran.stderr)
                self.assertIn("pool-isolation:", ran.stderr)
                self.report["faults"][name] = {"case": "pool-isolation",
                    "designated_rejection": ran.stderr.strip()}

    def test_private_faults_reach_designated_semantic_rejections(self) -> None:
        cases = {
            "wrong-root": ("activation", "activation-root"),
            "user-bit": ("construction", "user-setup-rejected:random-translation-byte-reference:store=0:context=1/0/0/0"),
            "implicit-g": ("construction", "user-setup-rejected:random-translation-byte-reference:store=0:context=2/0/0/0"),
            "fence-order": ("activation", "activation-fence-privilege"),
            "permission-install": ("activation", "activation-final-permissions"),
        }
        for fault, (case, expected) in cases.items():
            with self.subTest(fault=fault):
                binary = self.build(fault, private_fault(self.methods, fault))
                ran = self.run_case(binary, case)
                self.assertEqual(ran.returncode, 1, ran.stdout + ran.stderr)
                self.assertIn(expected, ran.stderr)
                self.report["faults"][fault] = {"case": case, "designated_rejection": expected}


if __name__ == "__main__":
    unittest.main()
