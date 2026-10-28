#!/usr/bin/env python3
"""Check schema52 segment high-VA gates and completion accounting without a DUT."""
from __future__ import annotations

import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))
import run_regression  # noqa: E402
import verify_regression  # noqa: E402

CPP = r'''
struct Options {
    std::string constraint_profile = "coverage";
    std::vector<std::string> constraint_overrides;
};
std::uint64_t parse_u64(std::string_view value, const char *) {
    return std::stoull(std::string(value));
}
#include "random/constraints.inc"
#include "translation_types.inc"
#include "constraint_coverage.inc"

static RandomConstraints segment_constraints() {
    auto c = RandomConstraints::preset("coverage");
    c.operation_weights[RandomConstraints::hypervisor] = 0;
    c.operation_weights[RandomConstraints::ptw_error] = 0;
    c.canonical_boundary_per_mille = 0;
    c.gstage_boundary_per_mille = 0;
    c.vector_segment_fof_per_mille = 0;
    c.vector_segment_addressing_weights = {1, 0, 0, 0};
    c.vector_segment_eew_weights = {0, 1, 0, 0};
    c.vector_segment_sew_weights = {0, 1, 0, 0};
    c.vector_segment_lmul_weights = {0, 0, 0, 1, 0, 0, 0};
    c.vector_segment_emul_weights = {0, 0, 0, 1, 0, 0, 0};
    c.vector_segment_nf_weights = {1, 0, 0, 0, 0, 0, 0};
    return c;
}
static void rejected(const RandomConstraints &c, const char *message) {
    bool caught = false;
    try { c.validate(); }
    catch (const std::invalid_argument &e) {
        caught = std::string(e.what()).find(message) != std::string::npos;
        if (!caught) std::cerr << "expected " << message << ", got "
                               << e.what() << '\n';
    }
    assert(caught);
}
static void presets() {
    for (const auto &[name, rate] :
         std::array<std::pair<const char *, unsigned>, 3>{{
             {"spec", 1}, {"coverage", 100}, {"corner", 250}}}) {
        auto c = RandomConstraints::preset(name);
        c.validate();
        assert(c.vector_segment_high_va_per_mille == rate);
        assert(c.enabled_vector_segment_high_va_crosses() == 24);
        assert(!c.vector_segment_high_va_cross_enabled(24));
        assert(c.summary().find("constraint_schema=55") != std::string::npos);
        assert(c.summary().find(" target_vector_segment_high_va=" +
                               std::to_string(rate)) != std::string::npos);
        const auto reserved = c.minimum_serial_actions();
        c.apply("vector-segment-high-va=0");
        c.validate();
        assert(c.enabled_vector_segment_high_va_crosses() == 0);
        assert(c.minimum_serial_actions() + 24 == reserved);
        for (unsigned endpoint : {1U, 1000U}) {
            c.apply("vector-segment-high-va=" + std::to_string(endpoint));
            c.validate();
            assert(c.vector_segment_high_va_per_mille == endpoint);
        }
        c.apply("vector-segment-high-va=1001");
        rejected(c, "per-mille");
    }
}
static void endpoints() {
    for (unsigned mode = 0; mode < 2; ++mode) {
        for (unsigned store = 0; store < 2; ++store) {
            for (unsigned misaligned = 0; misaligned < 2; ++misaligned) {
                auto c = segment_constraints();
                c.translation_weights[RandomConstraints::translation_nested] = 0;
                c.stage1_mode_weights = {0, 0};
                c.stage1_mode_weights[mode] = 1;
                c.vector_segment_stores_per_mille = store ? 1000 : 0;
                c.misaligned_per_mille = misaligned ? 1000 : 0;
                c.validate();
                const unsigned wanted = (mode * 2 + store) * 2 + misaligned;
                assert(c.enabled_vector_segment_high_va_crosses() == 1);
                for (unsigned cross = 0; cross < 24; ++cross) {
                    assert(c.vector_segment_high_va_cross_enabled(cross) ==
                           (cross == wanted));
                }
            }
        }
    }
    for (unsigned pair = 0; pair < 4; ++pair) {
        for (unsigned store = 0; store < 2; ++store) {
            for (unsigned misaligned = 0; misaligned < 2; ++misaligned) {
                auto c = segment_constraints();
                c.vector_fof_per_mille = 0;
                c.translation_weights[RandomConstraints::translation_stage1] = 0;
                c.vs_mode_weights = {0, 0};
                c.vs_mode_weights[pair / 2U] = 1;
                c.g_mode_weights = {0, 0};
                c.g_mode_weights[pair % 2U] = 1;
                c.vector_segment_stores_per_mille = store ? 1000 : 0;
                c.misaligned_per_mille = misaligned ? 1000 : 0;
                c.validate();
                const unsigned wanted = 8 + (pair * 2U + store) * 2U +
                    misaligned;
                assert(c.enabled_vector_segment_high_va_crosses() == 1);
                for (unsigned cross = 0; cross < 24; ++cross) {
                    assert(c.vector_segment_high_va_cross_enabled(cross) ==
                           (cross == wanted));
                }
            }
        }
    }
    for (unsigned gate = 0; gate < 4; ++gate) {
        auto c = segment_constraints();
        switch (gate) {
        case 0: c.operation_weights[RandomConstraints::vector_segment] = 0; break;
        case 1: c.translation_weights[RandomConstraints::translation_stage1] = 0;
                c.translation_weights[RandomConstraints::translation_nested] = 0;
                break;
        case 2: c.stage1_mode_weights = {0, 0}; c.vs_mode_weights = {0, 0}; break;
        case 3: c.vector_segment_eew_weights.fill(0); break;
        }
        assert(c.enabled_vector_segment_high_va_crosses() == 0);
        rejected(c, "vector-segment-high-va");
    }
    for (unsigned gate = 0; gate < 3; ++gate) {
        auto c = segment_constraints();
        c.translation_weights[RandomConstraints::translation_stage1] = 0;
        switch (gate) {
        case 0: c.translation_weights[RandomConstraints::translation_nested] = 0; break;
        case 1: c.vs_mode_weights = {0, 0}; break;
        case 2: c.g_mode_weights = {0, 0}; break;
        }
        assert(c.enabled_vector_segment_high_va_crosses() == 0);
        rejected(c, "vector-segment-high-va");
    }
    auto c = segment_constraints();
    c.vector_segment_stores_per_mille = 0;
    c.vector_segment_fof_per_mille = 1000;
    assert(c.enabled_vector_segment_high_va_crosses() == 0);
}
static void shapes() {
    // Enumerate singleton constraints independently. Indexed data uses SEW;
    // ordinary unit/strided data uses EEW, including byte-sized endpoints.
    for (unsigned a = 0; a < 4; ++a) {
        for (unsigned eew = 0; eew < 4; ++eew) {
            for (unsigned sew = 0; sew < 4; ++sew) {
                for (int lmul = -3; lmul <= 3; ++lmul) {
                    for (unsigned nf = 0; nf < 7; ++nf) {
                        auto c = segment_constraints();
                        c.vector_segment_addressing_weights.fill(0);
                        c.vector_segment_addressing_weights[a] = 1;
                        c.vector_segment_eew_weights.fill(0);
                        c.vector_segment_eew_weights[eew] = 1;
                        c.vector_segment_sew_weights.fill(0);
                        c.vector_segment_sew_weights[sew] = 1;
                        c.vector_segment_lmul_weights.fill(0);
                        c.vector_segment_lmul_weights[lmul + 3] = 1;
                        c.vector_segment_nf_weights.fill(0);
                        c.vector_segment_nf_weights[nf] = 1;
                        c.vector_segment_emul_weights.fill(1);
                        const int emul = int(eew) - int(sew) + lmul;
                        const bool legal = lmul >= int(sew) - 3 &&
                            emul >= -3 && emul <= 3 &&
                            (1U << unsigned(std::max(a >= 2 ? lmul : emul, 0))) *
                                (nf + 2) <= 8;
                        assert(c.vector_segment_high_va_shape_reachable(false) ==
                               legal);
                        assert(c.vector_segment_high_va_shape_reachable(true) ==
                               (legal && (a >= 2 ? sew : eew) > 0));
                        c.vector_segment_emul_weights.fill(0);
                        assert(!c.vector_segment_high_va_shape_reachable(false));
                    }
                }
            }
        }
    }
    auto byte = segment_constraints();
    byte.vector_segment_eew_weights = {1, 0, 0, 0};
    byte.vector_segment_sew_weights = {1, 0, 0, 0};
    assert(byte.enabled_vector_segment_high_va_crosses() == 12);
    byte.validate();
    byte.misaligned_per_mille = 1000;
    rejected(byte, "vector-segment-high-va");
}
static void coverage() {
    const auto c = segment_constraints();
    ConstraintCoverage good;
    good.operations[RandomConstraints::vector_segment] = 24;
    good.vector_segment_directions = {12, 12};
    good.stage1_modes = {4, 4};
    good.nested_mode_pairs.fill(4);
    good.vector_segment_addressing[0] = 24;
    good.vector_segment_eews[1] = 24;
    good.vector_segment_sews[1] = 24;
    good.vector_segment_lmuls[3] = 24;
    good.vector_segment_emuls[3] = 24;
    good.vector_segment_nfs[0] = 24;
    good.vector_segment_fof_presence = {12, 0};
    good.vector_segment_high_va_crosses.fill(1);
    good.vector_segment_high_va_data_uops = 48;
    good.vector_segment_high_va_elements = 48;
    const auto complete = [&](const auto &v, const auto &constraints) {
        return v.operation_complete(constraints, RandomConstraints::vector_segment);
    };
    assert(complete(good, c));
    auto mutant = good;
    mutant.vector_segment_directions = {7, 1};
    mutant.vector_segment_fof_presence[0] = 7;
    assert(!complete(mutant, c));
    mutant = good;
    mutant.vector_segment_directions = {1, 7};
    mutant.vector_segment_fof_presence[0] = 1;
    assert(!complete(mutant, c));
    mutant = good;
    mutant.stage1_modes = {3, 5};
    assert(!complete(mutant, c));
    mutant = good;
    mutant.stage1_modes = {5, 3};
    assert(!complete(mutant, c));
    mutant = good;
    mutant.nested_mode_pairs[1] = 3;
    assert(!complete(mutant, c));
    mutant = good;
    mutant.vector_segment_high_va_crosses[0] = 0;
    assert(!complete(mutant, c));
    mutant = good;
    mutant.vector_segment_high_va_crosses[0] = 2;
    assert(!complete(mutant, c));
    mutant = good;
    mutant.vector_segment_high_va_data_uops = 15;
    assert(!complete(mutant, c));
    mutant = good;
    mutant.vector_segment_high_va_elements = 15;
    assert(!complete(mutant, c));
    mutant = good;
    mutant.vector_segment_high_va_data_uops = 193;
    assert(!complete(mutant, c));
    mutant = good;
    mutant.vector_segment_high_va_elements = 3073;
    assert(!complete(mutant, c));
    auto disabled = c;
    disabled.vector_segment_high_va_per_mille = 0;
    assert(!complete(good, disabled));
    mutant = good;
    mutant.vector_segment_high_va_crosses.fill(0);
    mutant.vector_segment_high_va_data_uops = 0;
    mutant.vector_segment_high_va_elements = 0;
    assert(complete(mutant, disabled));
    mutant.vector_segment_high_va_elements = 1;
    assert(!complete(mutant, disabled));
    disabled.operation_weights[RandomConstraints::vector_segment] = 0;
    ConstraintCoverage empty;
    assert(complete(empty, disabled));
    empty.vector_segment_high_va_data_uops = 1;
    assert(!complete(empty, disabled));
    const auto summary = good.summary(c, {}, {}, {});
    assert(summary.find(
               " actual_vector_segment_high_va_cross="
               "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1")
           != std::string::npos);
    assert(summary.find(" actual_vector_segment_high_va=24 ") != std::string::npos);
    assert(summary.find(" actual_vector_segment_high_va_data_uops=48 ")
           != std::string::npos);
    assert(summary.find(" actual_vector_segment_high_va_elements=48 ")
           != std::string::npos);
}
int main(int argc, char **argv) {
    assert(argc == 2);
    const std::string test = argv[1];
    if (test == "presets") presets();
    else if (test == "endpoints") endpoints();
    else if (test == "shapes") shapes();
    else if (test == "coverage") coverage();
    else return 2;
}
'''


class CompiledHighVaTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-high-va-")
        cls.addClassCleanup(cls.workspace.cleanup)
        root = Path(cls.workspace.name)
        facade = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        includes = re.findall(r"^#include <[^>]+>$", facade, re.MULTILINE)
        generated = (MEMBLOCK_ROOT / "cpp/generated_interface_types.hpp").read_text()
        constants = []
        for name in ("kScalarLoadFeedbackLanes", "kIfetchPrefetchLanes"):
            match = re.search(rf"^inline constexpr unsigned {name} = .*?;",
                              generated, re.MULTILINE)
            if match is None:
                raise AssertionError(f"missing generated constant {name}")
            constants.append(match.group())
        coverage = (MEMBLOCK_ROOT / "cpp/random/coverage.inc").read_text()
        (root / "constraint_coverage.inc").write_text(
            "struct ConstraintCoverage {" + coverage.split(
                "struct ConstraintCoverage {", 1)[1].split(
                "struct MixedCoverage {", 1)[0]
        )
        main_source = (MEMBLOCK_ROOT / "cpp/scenarios/shared.hpp").read_text()
        (root / "translation_types.inc").write_text(
            "struct TranslationContext {" + main_source.split(
                "struct TranslationContext {", 1)[1].split(
                '#include "../random/coverage.inc"', 1)[0]
        )
        source = root / "high_va.cpp"
        source.write_text(
            "\n".join(includes)
            + "\n#include <cassert>\n#include <limits>\n#include <numeric>\n"
            + "namespace memblock::generated {\n"
            + "\n".join(constants)
            + '\n}\nnamespace memblock {\n#include "environment/model.inc"\n}\n'
            + CPP
        )
        cls.binary = root / "high_va"
        subprocess.run(
            ["c++", "-std=c++20", "-O0", "-fsanitize=undefined",
             "-fno-sanitize-recover=undefined", "-I", str(MEMBLOCK_ROOT / "cpp"),
             str(source), "-o", str(cls.binary)],
            check=True, capture_output=True, text=True, timeout=60,
        )

    def check_case(self, case: str) -> None:
        result = subprocess.run([str(self.binary), case], capture_output=True,
                                text=True, timeout=30)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_presets_parser_ranges_and_reservations(self) -> None:
        self.check_case("presets")

    def test_all_24_cross_endpoints_and_gates(self) -> None:
        self.check_case("endpoints")

    def test_3136_singleton_shapes_and_byte_endpoint(self) -> None:
        self.check_case("shapes")

    def test_completion_accounting_and_corrupted_counters(self) -> None:
        self.check_case("coverage")


def high_va_fields() -> dict[str, object]:
    return {
        "target_vector_segment_high_va": 100,
        "target_vector_segment_store": 500,
        "target_vector_segment_fof": 100,
        "target_misaligned": 500,
        "target_vector_segment_addressing": "1,1,1,1",
        "target_vector_segment_eew": "1,1,1,1",
        "target_vector_segment_sew": "1,1,1,1",
        "target_vector_segment_lmul": "1,1,1,1,1,1,1",
        "target_vector_segment_emul": "1,1,1,1,1,1,1",
        "target_vector_segment_nf": "1,1,1,1,1,1,1",
        "target_translation": "1,1,1",
        "target_stage1_mode": "1,1",
        "target_vs_mode": "1,1",
        "target_g_mode": "1,1",
        "actual_stage1_mode": "4,4",
        "actual_nested_pairs": "4,4,4,4",
        "actual_vector_segment_high_va_cross": ",".join(["1"] * 24),
        "actual_vector_segment_high_va": 24,
        "actual_vector_segment_high_va_data_uops": 48,
        "actual_vector_segment_high_va_elements": 48,
    }


class VerifyHighVaTest(unittest.TestCase):
    def check(self, fields: dict[str, object], *, operations=None,
              translation=None, modes=None, vs_modes=None, g_modes=None,
              directions=None) -> None:
        verify_regression._check_vector_segment_high_va_coverage(
            fields, operations if operations is not None else [1] * 15,
            translation if translation is not None else [1] * 3,
            modes if modes is not None else [1] * 2,
            vs_modes if vs_modes is not None else [1] * 2,
            g_modes if g_modes is not None else [1] * 2,
            directions if directions is not None else [12, 12], 52,
        )

    @staticmethod
    def set_crosses(fields: dict[str, object], crosses: list[int]) -> None:
        fields["actual_vector_segment_high_va_cross"] = ",".join(map(str, crosses))
        fields["actual_vector_segment_high_va"] = sum(crosses)
        fields["actual_vector_segment_high_va_data_uops"] = 2 * sum(crosses)
        fields["actual_vector_segment_high_va_elements"] = 2 * sum(crosses)

    def test_valid_all_crosses_and_rate_endpoints(self) -> None:
        for rate in (1, 100, 1000):
            fields = high_va_fields()
            fields["target_vector_segment_high_va"] = rate
            self.check(fields)

    def test_schema51_eight_bin_evidence_remains_verifiable(self) -> None:
        fields = high_va_fields()
        fields["actual_vector_segment_high_va_cross"] = ",".join(["1"] * 8)
        fields["actual_vector_segment_high_va"] = 8
        fields["actual_vector_segment_high_va_data_uops"] = 16
        fields["actual_vector_segment_high_va_elements"] = 16
        verify_regression._check_vector_segment_high_va_coverage(
            fields, [1] * 15, [1] * 3, [1] * 2, [1] * 2, [1] * 2,
            [4, 4], 51,
        )

    def test_mode_direction_and_alignment_endpoints(self) -> None:
        for mode in range(2):
            for store in range(2):
                for misaligned in range(2):
                    fields = high_va_fields()
                    fields["target_vector_segment_store"] = 1000 if store else 0
                    fields["target_misaligned"] = 1000 if misaligned else 0
                    crosses = [0] * 24
                    crosses[(mode * 2 + store) * 2 + misaligned] = 1
                    self.set_crosses(fields, crosses)
                    self.check(
                        fields,
                        translation=[0, 1, 0],
                        modes=[int(mode == i) for i in range(2)],
                    )
        for pair in range(4):
            for store in range(2):
                for misaligned in range(2):
                    fields = high_va_fields()
                    fields["target_vector_segment_store"] = 1000 if store else 0
                    fields["target_misaligned"] = 1000 if misaligned else 0
                    crosses = [0] * 24
                    crosses[8 + (pair * 2 + store) * 2 + misaligned] = 1
                    self.set_crosses(fields, crosses)
                    self.check(
                        fields,
                        modes=[0, 0],
                        vs_modes=[int(pair // 2 == i) for i in range(2)],
                        g_modes=[int(pair % 2 == i) for i in range(2)],
                    )

    def test_byte_data_and_indexed_data_width(self) -> None:
        for addressing, eew, sew, expected in (
            (0, 0, 1, False), (1, 1, 0, True),
            (2, 0, 1, True), (3, 1, 0, False),
        ):
            fields = high_va_fields()
            for dimension, value in (("addressing", addressing), ("eew", eew),
                                     ("sew", sew)):
                fields[f"target_vector_segment_{dimension}"] = ",".join(
                    str(int(i == value)) for i in range(4))
            crosses = [1 if expected or i % 2 == 0 else 0 for i in range(24)]
            self.set_crosses(fields, crosses)
            self.check(fields)
            if not expected:
                fields["target_misaligned"] = 1000
                self.set_crosses(fields, [0] * 24)
                with self.assertRaisesRegex(verify_regression.VerificationError,
                                            "no eligible"):
                    self.check(fields)

    def test_disabled_target_and_operation_require_zero_counters(self) -> None:
        fields = high_va_fields()
        fields["target_vector_segment_high_va"] = 0
        self.set_crosses(fields, [0] * 24)
        self.check(fields)
        operations = [1] * 15
        operations[4] = 0
        self.check(fields, operations=operations, directions=[0, 0])
        for key in ("actual_vector_segment_high_va_data_uops",
                    "actual_vector_segment_high_va_elements"):
            mutant = dict(fields)
            mutant[key] = 1
            with self.assertRaises(verify_regression.VerificationError):
                self.check(mutant)
        self.set_crosses(fields, [1] + [0] * 23)
        with self.assertRaises(verify_regression.VerificationError):
            self.check(fields)

    def test_unreachable_nonzero_target_is_rejected(self) -> None:
        for kwargs in ({"operations": [0] * 15},
                       {"translation": [0, 0, 0]},
                       {"modes": [0, 0], "vs_modes": [0, 0]}):
            fields = high_va_fields()
            self.set_crosses(fields, [0] * 24)
            with self.assertRaisesRegex(verify_regression.VerificationError,
                                        "no eligible"):
                self.check(fields, **kwargs)
        fields = high_va_fields()
        fields["target_vector_segment_eew"] = "0,1,0,0"
        fields["target_vector_segment_sew"] = "0,1,0,0"
        fields["target_vector_segment_lmul"] = "0,0,0,0,0,0,1"
        fields["target_vector_segment_nf"] = "0,0,0,0,0,0,1"
        self.set_crosses(fields, [0] * 24)
        with self.assertRaisesRegex(verify_regression.VerificationError,
                                    "no eligible"):
            self.check(fields)

    def test_cross_total_uop_and_element_tampering(self) -> None:
        for key, value in (
            ("actual_vector_segment_high_va_cross", ",".join(
                ["0"] + ["1"] * 23)),
            ("actual_vector_segment_high_va_cross", ",".join(
                ["2"] + ["1"] * 23)),
            ("actual_vector_segment_high_va_cross", ",".join(["1"] * 23)),
            ("actual_vector_segment_high_va_cross", ",".join(
                ["-1"] + ["1"] * 23)),
            ("actual_vector_segment_high_va", 23),
            ("actual_vector_segment_high_va_data_uops", 47),
            ("actual_vector_segment_high_va_data_uops", 193),
            ("actual_vector_segment_high_va_elements", 47),
            ("actual_vector_segment_high_va_elements", 3073),
        ):
            fields = high_va_fields()
            fields[key] = value
            with self.subTest(key=key, value=value), self.assertRaises(
                    verify_regression.VerificationError):
                self.check(fields)
        with self.assertRaises(verify_regression.VerificationError):
            self.check(high_va_fields(), directions=[11, 12])

    def test_direction_subsets_cannot_borrow_from_other_direction(self) -> None:
        for directions in ([23, 1], [1, 23]):
            with self.subTest(directions=directions), self.assertRaisesRegex(
                    verify_regression.VerificationError, "direction accounting"):
                self.check(high_va_fields(), directions=directions)
        self.check(high_va_fields(), directions=[23, 12])

    def test_mode_subsets_cannot_borrow_from_other_mode(self) -> None:
        for modes in ("3,5", "5,3"):
            fields = high_va_fields()
            fields["actual_stage1_mode"] = modes
            with self.subTest(modes=modes), self.assertRaisesRegex(
                    verify_regression.VerificationError, "mode accounting"):
                self.check(fields)
        fields = high_va_fields()
        fields["actual_stage1_mode"] = "7,4"
        self.check(fields)
        for pairs in ("3,5,4,4", "4,4,3,5"):
            fields = high_va_fields()
            fields["actual_nested_pairs"] = pairs
            with self.subTest(pairs=pairs), self.assertRaisesRegex(
                    verify_regression.VerificationError, "nested"):
                self.check(fields)

    def test_exact_integer_targets_and_scalar_counters(self) -> None:
        for key in high_va_fields():
            if key.startswith("target_") and key in (
                "target_vector_segment_high_va", "target_vector_segment_store",
                "target_vector_segment_fof", "target_misaligned",
            ):
                values = (None, True, "100", 100.0, -1, 1001)
            elif key in ("actual_vector_segment_high_va",
                         "actual_vector_segment_high_va_data_uops",
                         "actual_vector_segment_high_va_elements"):
                values = (None, True, "24", 24.0, -1)
            else:
                continue
            for value in values:
                fields = high_va_fields()
                fields[key] = value
                with self.subTest(key=key, value=value), self.assertRaises(
                        verify_regression.VerificationError):
                    self.check(fields)

    def test_parser_preserves_exact_large_integer_counters(self) -> None:
        expected = 2**60 + 31
        summary = (
            "MEMBLOCK_CONSTRAINED_RANDOM_PASS constraint_schema=52 "
            "target_vector_segment_high_va=100 "
            f"actual_vector_segment_high_va_cross={','.join(['1'] * 24)} "
            f"actual_vector_segment_high_va={expected} "
            f"actual_vector_segment_high_va_data_uops={2 * expected} "
            f"actual_vector_segment_high_va_elements={expected}"
        )
        parsed = run_regression.parse_summary(summary)
        self.assertEqual(parsed["target_vector_segment_high_va"], 100)
        self.assertEqual(parsed["actual_vector_segment_high_va"], expected)
        self.assertEqual(parsed["actual_vector_segment_high_va_data_uops"],
                         2 * expected)
        self.assertEqual(parsed["actual_vector_segment_high_va_elements"], expected)
        self.assertIs(type(parsed["actual_vector_segment_high_va"]), int)
        self.assertEqual(parsed["actual_vector_segment_high_va_cross"],
                         ",".join(["1"] * 24))


if __name__ == "__main__":
    unittest.main()
