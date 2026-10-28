#!/usr/bin/env python3

from __future__ import annotations

import io
import json
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import verify_regression  # noqa: E402


def mixed_result(seed: int) -> dict[str, object]:
    fields: dict[str, object] = {
        "seed": seed,
        "transactions": 64,
        "cycle": 100,
        "scalar_writebacks": 20,
        "prefetch_writebacks": 3,
        "store_writebacks": 12,
        "vector_load_writebacks": 8,
        "vector_store_writebacks": 4,
        "tilelink_requests": 30,
        "release_data": 2,
        "scalar_misaligned": 1,
        "ptw_requests": 3,
        "uncache_requests": 2,
        "lq": "31+1/32",
        "sq": "16+0/16",
        "lsq_monitor_schema": 2,
        "lsq_enqueued_observed": "32,16",
        "redirect_cancels_observed": "1,1,0",
        "unobserved_cancels": "0,0",
        "load_ops": "1,1,1,1,1,1,1",
        "store_ops": "1,1,1,1",
        "scalar": "12,8",
        "vector": "8,4",
        "eew_load": "1,1,1,1",
        "eew_store": "1,1,1,1",
        "vec_load_modes": "1,1,1,1",
        "vec_store_modes": "1,1,1,1",
        "vec_load_stride": "1,1,1",
        "vec_store_stride": "1,1",
        "prefetch": "1,1,1",
        "masked": 2,
        "unmasked": 4,
        "vstart": "3,3",
        "vl": "3,3",
        "align": "3,3",
        "store_order": "3,3",
        "waves": 2,
        "coissue": 1,
        "forwarding": "1,1,1,1",
        "memory_types": "5,2",
        "dcache": "1,1",
        "dispatch_widths": "1,1,1,1,1,1",
        "dispatch_lanes": "1,1,1,1,1,1",
        "tlb_reuse": 1,
        "redirects": 1,
        "dirty": 1,
        "max_outstanding": 3,
        "concurrent_ops": "1,1,1,1,1",
        "concurrent": "4,20,4,5,3",
        "backpressure": "1,1,1,1,1,1",
    }
    summary = "MEMBLOCK_CONSTRAINED_RANDOM_PASS " + " ".join(
        f"{name}={value}" for name, value in fields.items()
    )
    return {
        **fields,
        "status": "pass",
        "scenario": "constrained-random",
        "returncode": 0,
        "output": "",
        "elapsed_seconds": 0.1,
        "submitted_offset_seconds": float(seed - 7) / 10,
        "completed_offset_seconds": 4.5 + float(seed - 7) / 2,
        "command": [
            "/frozen/memblock_sim",
            "--test",
            "constrained-random",
            "--seed",
            str(seed),
            "--actions",
            "64",
        ],
        "summary": summary,
    }


def regression_document(results: list[dict[str, object]]) -> dict[str, object]:
    return {
        "schema_version": 2,
        "campaign_status": "complete",
        "run_id": "0123456789abcdef0123456789abcdef",
        "started_at": "2026-01-01T00:00:00+00:00",
        "finished_at": "2026-01-01T00:00:05+00:00",
        "elapsed_seconds": 5.0,
        "binary": "/frozen/memblock_sim",
        "configuration": {
            "backpressure": True,
            "duration_seconds": 4.0,
            "jobs": 8,
            "actions_per_seed": 64,
            "scenarios": ["constrained-random"],
            "start_seed": 7,
        },
        "runtime": {
            "metadata": "/frozen/runtime.json",
            "root": "/frozen",
            "artifacts": {
                "binary": "/frozen/memblock_sim",
                "model": "/frozen/libUTMemBlock.so",
                "rtl_metadata": "/frozen/rtl.json",
                "xspcomm": "/frozen/libxspcomm.so.0.0.1",
            },
            "external_dependencies": ["/lib/system.so"],
        },
        "summary": {
            "seeds_completed": len(results),
            "statuses": {"pass": len(results)},
            "actions_completed": 64 * len(results),
        },
        "results": results,
    }


class VerifyRegressionTest(unittest.TestCase):
    def test_probe_source_lifecycle_boundaries(self) -> None:
        for probes, expected in (
            (0, [0, 0, 0]),
            (1, [1, 0, 0]),
            (64, [64, 0, 0]),
            (65, [64, 1, 1]),
            (128, [64, 64, 1]),
            (129, [64, 65, 2]),
        ):
            self.assertEqual(
                verify_regression._expected_probe_source_lifecycle(probes, 64),
                expected,
            )

    def test_lsq_enqueue_monitor_must_match_queue_allocation(self) -> None:
        result = mixed_result(7)
        verify_regression._check_mixed_coverage(result)

        result["lsq_enqueued_observed"] = "31,16"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "observed LSQ enqueue counts disagree",
        ):
            verify_regression._check_mixed_coverage(result)

        result = mixed_result(7)
        result["redirect_cancels_observed"] = "1,0,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "cancellation classes do not conserve",
        ):
            verify_regression._check_mixed_coverage(result)

        result = mixed_result(7)
        result["redirect_cancels_observed"] = "1,0,0"
        result["unobserved_cancels"] = "1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "unobserved queue cancellation",
        ):
            verify_regression._check_mixed_coverage(result)

    def test_dcache_grants_require_one_grant_ack_each(self) -> None:
        result = mixed_result(7)
        result.update(
            {"dcache_refills": 3, "dcache_acquire_perms": 1, "grant_acks": 4}
        )
        result["summary"] += (
            " dcache_refills=3 dcache_acquire_perms=1 grant_acks=4"
        )
        verify_regression._check_result(
            result,
            0,
            {"constrained-random": 64},
            {"constrained-random": 64},
        )

        result["grant_acks"] = 3
        result["summary"] = str(result["summary"]).replace(
            "grant_acks=4", "grant_acks=3"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "GrantAcks are not conserved"
        ):
            verify_regression._check_result(
                result,
                0,
                {"constrained-random": 64},
                {"constrained-random": 64},
            )

    def test_require_coverage_uses_full_mixed_checker(self) -> None:
        result = mixed_result(7)
        verify_regression._check_result(
            result,
            0,
            {"constrained-random": 64},
            {"constrained-random": 64},
            require_backpressure=True,
            require_coverage=True,
        )

        sparse = mixed_result(7)
        sparse["masked"] = 0
        sparse["summary"] = str(sparse["summary"]).replace(
            "masked=2", "masked=0")
        # The default accounting path tolerates sparse observational coverage.
        verify_regression._check_result(
            sparse,
            0,
            {"constrained-random": 64},
            {"constrained-random": 64},
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "seed=7 scenario=constrained-random: masked coverage is absent",
        ):
            verify_regression._check_result(
                sparse,
                0,
                {"constrained-random": 64},
                {"constrained-random": 64},
                require_backpressure=True,
                require_coverage=True,
            )

    def test_unknown_constraint_schema_is_rejected(self) -> None:
        result = mixed_result(7)
        result["constraint_schema"] = 60
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "unsupported constraint_schema"
        ):
            verify_regression._check_mixed_coverage(result)

    def test_schema_48_checks_canonical_boundary_accounting(self) -> None:
        # Unequal counts make direction swaps and incorrect marginals visible.
        fields: dict[str, object] = {
            "target_canonical_boundary": 100,
            "actual_canonical_boundary_cross": ",".join(map(str, range(1, 25))),
            "actual_canonical_boundary": 300,
            "actual_canonical_boundary_controls": 936,
            "actual_canonical_boundary_recoveries": 300,
            "actual_canonical_boundary_store_checks": 168,
        }
        args = (fields, [1, 1], [137, 173], [0, 1, 0], [1, 1])
        for target in (1, 100, 1000):
            with self.subTest(target=target):
                fields["target_canonical_boundary"] = target
                self.assertEqual(
                    verify_regression._check_canonical_boundary_coverage_schema48(
                        *args
                    ),
                    (132, 168),
                )
        for name in (
            "actual_canonical_boundary",
            "actual_canonical_boundary_controls",
            "actual_canonical_boundary_recoveries",
            "actual_canonical_boundary_store_checks",
        ):
            with self.subTest(counter=name):
                original = fields[name]
                fields[name] = int(original) - 1
                with self.assertRaisesRegex(
                    verify_regression.VerificationError,
                    "fault/control/recovery accounting",
                ):
                    verify_regression._check_canonical_boundary_coverage_schema48(
                        *args
                    )
                fields[name] = original
        fields["actual_canonical_boundary_cross"] = ",".join(
            ["0"] + list(map(str, range(2, 25)))
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "does not match enabled classes"
        ):
            verify_regression._check_canonical_boundary_coverage_schema48(*args)
        fields["actual_canonical_boundary_cross"] = ",".join(map(str, range(1, 25)))
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "exceed scalar operation counts"
        ):
            verify_regression._check_canonical_boundary_coverage_schema48(
                fields, [1, 1], [131, 168], [0, 1, 0], [1, 1]
            )

    def test_schema_48_enforces_disabled_canonical_boundary_classes(self) -> None:
        fields: dict[str, object] = {
            "target_canonical_boundary": 100,
            # Only Sv39 scalar loads; both invalid-first values, all widths.
            "actual_canonical_boundary_cross": ",".join(
                (["1"] * 3 + ["0"] * 9) * 2
            ),
            "actual_canonical_boundary": 6,
            "actual_canonical_boundary_controls": 12,
            "actual_canonical_boundary_recoveries": 6,
            "actual_canonical_boundary_store_checks": 0,
        }
        args = (fields, [1, 0], [7, 0], [0, 1, 0], [1, 0])
        self.assertEqual(
            verify_regression._check_canonical_boundary_coverage_schema48(*args),
            (6, 0),
        )
        sv48_stores = {
            **fields,
            "actual_canonical_boundary_cross": ",".join(
                (["0"] * 9 + ["1"] * 3) * 2
            ),
            "actual_canonical_boundary_controls": 24,
            "actual_canonical_boundary_store_checks": 6,
        }
        self.assertEqual(
            verify_regression._check_canonical_boundary_coverage_schema48(
                sv48_stores, [0, 1], [0, 7], [0, 1, 0], [0, 1]
            ),
            (0, 6),
        )
        # Disabled observations must fail even when their counters conserve.
        for index, store_checks, controls in ((6, 0, 12), (3, 1, 14)):
            with self.subTest(disabled_index=index):
                cross = [1] * 3 + [0] * 9 + [1] * 3 + [0] * 9
                cross[index] = 1
                fields["actual_canonical_boundary_cross"] = ",".join(map(str, cross))
                fields["actual_canonical_boundary"] = 7
                fields["actual_canonical_boundary_recoveries"] = 7
                fields["actual_canonical_boundary_store_checks"] = store_checks
                fields["actual_canonical_boundary_controls"] = controls + 2
                with self.assertRaisesRegex(
                    verify_regression.VerificationError,
                    "does not match enabled classes",
                ):
                    verify_regression._check_canonical_boundary_coverage_schema48(
                        *args
                    )
        fields.update({
            "target_canonical_boundary": 0,
            "actual_canonical_boundary_cross": ",".join(["0"] * 24),
            "actual_canonical_boundary": 0,
            "actual_canonical_boundary_controls": 0,
            "actual_canonical_boundary_recoveries": 0,
            "actual_canonical_boundary_store_checks": 0,
        })
        self.assertEqual(
            verify_regression._check_canonical_boundary_coverage_schema48(*args),
            (0, 0),
        )
        for name in (
            "actual_canonical_boundary",
            "actual_canonical_boundary_controls",
            "actual_canonical_boundary_recoveries",
            "actual_canonical_boundary_store_checks",
        ):
            with self.subTest(disabled_counter=name):
                fields[name] = 1
                with self.assertRaisesRegex(
                    verify_regression.VerificationError,
                    "fault/control/recovery accounting",
                ):
                    verify_regression._check_canonical_boundary_coverage_schema48(
                        *args
                    )
                fields[name] = 0
        fields.update({
            "actual_canonical_boundary_cross": ",".join(["1"] + ["0"] * 23),
            "actual_canonical_boundary": 1,
            "actual_canonical_boundary_controls": 2,
            "actual_canonical_boundary_recoveries": 1,
        })
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "does not match enabled classes"
        ):
            verify_regression._check_canonical_boundary_coverage_schema48(*args)

    def test_schema_48_rejects_invalid_canonical_boundary_fields(self) -> None:
        fields: dict[str, object] = {
            "target_canonical_boundary": 0,
            "actual_canonical_boundary_cross": ",".join(["0"] * 24),
            "actual_canonical_boundary": 0,
            "actual_canonical_boundary_controls": 0,
            "actual_canonical_boundary_recoveries": 0,
            "actual_canonical_boundary_store_checks": 0,
        }
        args = (fields, [1, 1], [1, 1], [0, 1, 0], [1, 1])
        for invalid in (-1, 1001, True, 1.5, None, "1"):
            with self.subTest(target=invalid):
                fields["target_canonical_boundary"] = invalid
                with self.assertRaisesRegex(
                    verify_regression.VerificationError,
                    "target_canonical_boundary must be an integer",
                ):
                    verify_regression._check_canonical_boundary_coverage_schema48(
                        *args
                    )
        fields["target_canonical_boundary"] = 0
        for name in (
            "actual_canonical_boundary",
            "actual_canonical_boundary_controls",
            "actual_canonical_boundary_recoveries",
            "actual_canonical_boundary_store_checks",
        ):
            for invalid in (-1, True, 1.5, None, "1"):
                with self.subTest(counter=name, value=invalid):
                    fields[name] = invalid
                    with self.assertRaisesRegex(
                        verify_regression.VerificationError,
                        "not a nonnegative integer",
                    ):
                        verify_regression._check_canonical_boundary_coverage_schema48(
                            *args
                        )
            fields[name] = 0
        for value in (
            ",".join(["0"] * 23),
            ",".join(["-1"] + ["0"] * 23),
            ",".join(["0.5"] + ["0"] * 23),
        ):
            with self.subTest(cross=value):
                fields["actual_canonical_boundary_cross"] = value
                with self.assertRaises(verify_regression.VerificationError):
                    verify_regression._check_canonical_boundary_coverage_schema48(
                        *args
                    )
        fields["actual_canonical_boundary_cross"] = ",".join(["0"] * 24)
        fields["target_canonical_boundary"] = 1
        for operations, translation, modes in (
            ([0, 0], [0, 1, 0], [1, 1]),
            ([1, 1], [1, 0, 0], [1, 1]),
            ([1, 1], [0, 1, 0], [0, 0]),
        ):
            with self.subTest(operations=operations, translation=translation, modes=modes):
                with self.assertRaisesRegex(
                    verify_regression.VerificationError,
                    "lacks stage-1 scalar traffic",
                ):
                    verify_regression._check_canonical_boundary_coverage_schema48(
                        fields, operations, [1, 1], translation, modes
                    )

    @staticmethod
    def vector_canonical_fields(counts: list[int]) -> dict[str, object]:
        total = sum(counts)
        return {
            "target_canonical_boundary": 100,
            "target_vector_addressing": "0,1,1,1",
            "target_vector_eew": "0,1,1,1",
            "target_vector_sew": "0,1,1,1",
            "target_vector_lmul": "0,0,0,1,0,0,0",
            "target_vector_emul": "0,0,0,1,0,0,0",
            "target_vector_partial_vl": 500,
            "target_vector_nonzero_vstart": 500,
            "target_vector_masked": 500,
            "actual_vector_canonical_boundary_cross": ",".join(map(str, counts)),
            "actual_vector_canonical_boundary": total,
            "actual_vector_canonical_boundary_controls": 4 * total,
            "actual_vector_canonical_boundary_recoveries": total,
            "actual_vector_canonical_boundary_store_checks": total,
            "actual_vector_canonical_boundary_flows": sum(
                count * flows for count, flows in zip(counts, [8, 4, 2] * 12)
            ),
        }

    def test_schema_49_conserves_vector_canonical_faults_and_flows(self) -> None:
        counts = list(range(1, 37))
        fields = self.vector_canonical_fields(counts)
        args = (fields, [0, 0, 0, 1], [0, 0, 0, 667], [0, 1, 0], [1, 1])
        check = verify_regression._check_vector_canonical_boundary_coverage_schema49
        self.assertEqual(check(*args), 666)
        for name in (
            "actual_vector_canonical_boundary",
            "actual_vector_canonical_boundary_controls",
            "actual_vector_canonical_boundary_recoveries",
            "actual_vector_canonical_boundary_store_checks",
            "actual_vector_canonical_boundary_flows",
        ):
            with self.subTest(counter=name):
                malformed = dict(fields)
                malformed[name] = int(malformed[name]) - 1
                with self.assertRaisesRegex(
                    verify_regression.VerificationError, "accounting is not conserved"
                ):
                    check(malformed, *args[1:])
                for value in (None, False, -1, "666"):
                    malformed[name] = value
                    with self.assertRaisesRegex(
                        verify_regression.VerificationError, "nonnegative integer"
                    ):
                        check(malformed, *args[1:])
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "exceed vector-store operation counts"
        ):
            check(fields, args[1], [0, 0, 0, 665], *args[3:])
        # This is a dedicated subclass, separate from the 24 scalar bins.
        fields.update({
            "actual_canonical_boundary_cross": ",".join(["0"] * 24),
            "actual_canonical_boundary": 0,
            "actual_canonical_boundary_controls": 0,
            "actual_canonical_boundary_recoveries": 0,
            "actual_canonical_boundary_store_checks": 0,
        })
        self.assertEqual(
            verify_regression._check_canonical_boundary_coverage_schema48(
                *args, vector_faults=666
            ), (0, 0)
        )

    def test_schema_49_rejects_reused_vector_store_operation_counts(self) -> None:
        check = verify_regression._check_vector_store_subclass_accounting
        check([0, 0, 7, 8], [7, 3], 5)
        check([0, 0, 0, 0], [0, 0], 0)
        for actual_store_ops in (3, 5, 7, 9):
            with self.subTest(store_ops=actual_store_ops), self.assertRaisesRegex(
                verify_regression.VerificationError,
                "ordinary/canonical operation accounting is not conserved",
            ):
                check([0, 0, 7, actual_store_ops], [7, 3], 5)

    def test_schema_49_enforces_vector_canonical_enabled_subsets(self) -> None:
        # Sv48 indexed-ordered, 8-byte stores: bin 17 and bin 35.
        counts = [0] * 36
        counts[17], counts[35] = 3, 5
        fields = self.vector_canonical_fields(counts)
        fields.update({
            "target_vector_addressing": "0,0,0,1",
            "target_vector_eew": "0,0,0,1",
            "target_vector_sew": "0,0,0,1",
        })
        args = (fields, [0, 0, 0, 1], [0, 0, 0, 8], [0, 1, 0], [0, 1])
        check = verify_regression._check_vector_canonical_boundary_coverage_schema49
        self.assertEqual(check(*args), 8)
        for index in (0, 8, 14, 16, 34):
            with self.subTest(disabled=index):
                bad_counts = counts[:]
                bad_counts[index] = 1
                malformed = dict(fields)
                malformed["actual_vector_canonical_boundary_cross"] = ",".join(
                    map(str, bad_counts)
                )
                with self.assertRaisesRegex(
                    verify_regression.VerificationError, "does not match enabled classes"
                ):
                    check(malformed, *args[1:])
        for index in (17, 35):
            with self.subTest(missing=index):
                bad_counts = counts[:]
                bad_counts[index] = 0
                malformed = dict(fields)
                malformed["actual_vector_canonical_boundary_cross"] = ",".join(
                    map(str, bad_counts)
                )
                with self.assertRaisesRegex(
                    verify_regression.VerificationError, "does not match enabled classes"
                ):
                    check(malformed, *args[1:])

    def test_schema_49_disables_ineligible_vector_canonical_subclasses(self) -> None:
        check = verify_regression._check_vector_canonical_boundary_coverage_schema49
        disabling_overrides = (
            {"target_canonical_boundary": 0},
            {"target_vector_partial_vl": 0},
            {"target_vector_nonzero_vstart": 1000},
            {"target_vector_masked": 1000},
            {"target_vector_lmul": "1,0,0,0,0,0,0"},
            {"target_vector_emul": "1,0,0,0,0,0,0"},
            {"target_vector_addressing": "1,0,0,0"},
            {"target_vector_eew": "1,0,0,0"},
            {"target_vector_sew": "1,0,0,0"},
        )
        for override in disabling_overrides:
            with self.subTest(override=override):
                fields = self.vector_canonical_fields([0] * 36)
                fields.update(override)
                args = (fields, [1, 1, 1, 1], [10] * 4, [0, 1, 0], [1, 1])
                self.assertEqual(check(*args), 0)
                fields["actual_vector_canonical_boundary_cross"] = ",".join(
                    map(str, [1] + [0] * 35)
                )
                with self.assertRaisesRegex(
                    verify_regression.VerificationError, "does not match enabled classes"
                ):
                    check(*args)
        for operations, translation, modes in (
            ([1, 1, 1, 0], [0, 1, 0], [1, 1]),
            ([1, 1, 1, 1], [1, 0, 0], [1, 1]),
            ([1, 1, 1, 1], [0, 1, 0], [0, 0]),
        ):
            fields = self.vector_canonical_fields([0] * 36)
            self.assertEqual(check(fields, operations, [10] * 4, translation, modes), 0)

    def test_schema_49_rejects_invalid_vector_canonical_summary(self) -> None:
        check = verify_regression._check_vector_canonical_boundary_coverage_schema49
        fields = self.vector_canonical_fields([1] * 36)
        for name in ("target_canonical_boundary", "target_vector_partial_vl",
                     "target_vector_nonzero_vstart", "target_vector_masked"):
            for value in (None, False, -1, 1001, "100"):
                malformed = dict(fields)
                malformed[name] = value
                with self.subTest(field=name, value=value), self.assertRaises(
                    verify_regression.VerificationError
                ):
                    check(malformed, [1] * 4, [100] * 4, [0, 1, 0], [1, 1])
        for value in (None, "1,2", ",".join(["-1"] + ["1"] * 35),
                      ",".join(["1"] * 35 + ["oops"])):
            malformed = dict(fields)
            malformed["actual_vector_canonical_boundary_cross"] = value
            with self.subTest(counts=value), self.assertRaises(
                verify_regression.VerificationError
            ):
                check(malformed, [1] * 4, [100] * 4, [0, 1, 0], [1, 1])

    @staticmethod
    def gstage_boundary_fields(
        scalar_counts: list[int], vector_counts: list[int]
    ) -> dict[str, object]:
        scalar_total = sum(scalar_counts)
        vector_total = sum(vector_counts)
        return {
            "target_gstage_boundary": 100,
            "target_vector_addressing": "0,1,1,1",
            "target_vector_eew": "0,1,1,1",
            "target_vector_sew": "0,1,1,1",
            "target_vector_lmul": "0,0,0,1,0,0,0",
            "target_vector_emul": "0,0,0,1,0,0,0",
            "target_vector_partial_vl": 500,
            "target_vector_nonzero_vstart": 500,
            "target_vector_masked": 500,
            "actual_gstage_boundary_cross": ",".join(map(str, scalar_counts)),
            "actual_gstage_boundary": scalar_total,
            "actual_gstage_boundary_controls": 4 * scalar_total,
            "actual_gstage_boundary_recoveries": scalar_total,
            "actual_gstage_boundary_store_checks": sum(
                scalar_counts[3:6] + scalar_counts[9:12]
            ),
            "actual_vector_gstage_boundary_cross": ",".join(
                map(str, vector_counts)
            ),
            "actual_vector_gstage_boundary": vector_total,
            "actual_vector_gstage_boundary_controls": 4 * vector_total,
            "actual_vector_gstage_boundary_recoveries": vector_total,
            "actual_vector_gstage_boundary_store_checks": vector_total,
            "actual_vector_gstage_boundary_flows": sum(
                count * flows
                for count, flows in zip(vector_counts, [8, 4, 2] * 6)
            ),
        }

    def test_schema_50_checks_gstage_fault_control_and_flow_accounting(self) -> None:
        fields = self.gstage_boundary_fields(
            list(range(1, 13)), list(range(1, 19))
        )
        check = verify_regression._check_gstage_boundary_coverage_schema50
        args = ([1, 1, 0, 1], [31, 49, 0, 172], [1, 1])
        self.assertEqual(check(fields, *args), (30, 48, 171))
        for name in (
            "actual_gstage_boundary", "actual_gstage_boundary_controls",
            "actual_gstage_boundary_recoveries",
            "actual_gstage_boundary_store_checks",
            "actual_vector_gstage_boundary",
            "actual_vector_gstage_boundary_controls",
            "actual_vector_gstage_boundary_recoveries",
            "actual_vector_gstage_boundary_store_checks",
            "actual_vector_gstage_boundary_flows",
        ):
            with self.subTest(counter=name):
                malformed = dict(fields)
                malformed[name] = int(malformed[name]) - 1
                with self.assertRaisesRegex(
                    verify_regression.VerificationError, "accounting is not conserved"
                ):
                    check(malformed, *args)
                for value in (None, False, -1, "1", 1.5):
                    malformed[name] = value
                    with self.assertRaisesRegex(
                        verify_regression.VerificationError, "nonnegative integer"
                    ):
                        check(malformed, *args)
                del malformed[name]
                with self.assertRaises(verify_regression.VerificationError):
                    check(malformed, *args)
        for actual in ([29, 49, 0, 172], [31, 47, 0, 172], [31, 49, 0, 170]):
            with self.subTest(actual=actual), self.assertRaisesRegex(
                verify_regression.VerificationError, "exceed operation counts"
            ):
                check(fields, args[0], actual, args[2])

    def test_schema_50_enforces_restricted_gstage_bins(self) -> None:
        scalar = [0] * 12
        scalar[9:12] = [2, 3, 5]
        vector = [0] * 18
        vector[17] = 7
        fields = self.gstage_boundary_fields(scalar, vector)
        fields.update({
            "target_vector_addressing": "0,0,0,1",
            "target_vector_eew": "0,0,0,1",
            "target_vector_sew": "0,0,0,1",
        })
        args = ([0, 1, 0, 1], [0, 10, 0, 7], [0, 1])
        check = verify_regression._check_gstage_boundary_coverage_schema50
        self.assertEqual(check(fields, *args), (0, 10, 7))
        for field, counts, indexes in (
            ("actual_gstage_boundary_cross", scalar, (0, 3, 6, 8, 9, 10, 11)),
            ("actual_vector_gstage_boundary_cross", vector, (0, 8, 14, 16, 17)),
        ):
            for index in indexes:
                with self.subTest(field=field, index=index):
                    malformed = dict(fields)
                    changed = counts[:]
                    changed[index] = 0 if changed[index] else 1
                    malformed[field] = ",".join(map(str, changed))
                    with self.assertRaisesRegex(
                        verify_regression.VerificationError,
                        "does not match enabled classes",
                    ):
                        check(malformed, *args)

    def test_schema_50_gstage_does_not_require_nested_or_vs_traffic(self) -> None:
        fields = self.gstage_boundary_fields([1] * 12, [1] * 18)
        fields.update({
            "target_translation": "1,0,0",
            "target_stage1_mode": "0,0", "target_vs_mode": "0,0",
            "target_g_mode": "1,1",
        })
        self.assertEqual(
            verify_regression._check_gstage_boundary_coverage_schema50(
                fields, [1, 1, 0, 1], [6, 6, 0, 18], [1, 1]
            ), (6, 6, 18)
        )
        # Load-only faults need neither stores nor an eligible vector shape.
        fields = self.gstage_boundary_fields([1, 1, 1, 0, 0, 0] * 2, [0] * 18)
        fields["target_vector_partial_vl"] = 0
        self.assertEqual(
            verify_regression._check_gstage_boundary_coverage_schema50(
                fields, [1, 0, 0, 0], [6, 0, 0, 0], [1, 1]
            ), (6, 0, 0)
        )

    def test_schema_50_gstage_vector_policy_endpoints_and_scalar_absence(self) -> None:
        check = verify_regression._check_gstage_boundary_coverage_schema50
        for partial in (0, 500, 1000):
            for vstart in (0, 500, 1000):
                for masked in (0, 500, 1000):
                    enabled = partial != 0 and vstart != 1000 and masked != 1000
                    fields = self.gstage_boundary_fields(
                        [0] * 12, [int(enabled)] * 18
                    )
                    fields.update({
                        "target_vector_partial_vl": partial,
                        "target_vector_nonzero_vstart": vstart,
                        "target_vector_masked": masked,
                    })
                    args = ([0, 0, 0, 1], [0, 0, 0, 18], [1, 1])
                    with self.subTest(partial=partial, vstart=vstart, masked=masked):
                        if enabled:
                            self.assertEqual(check(fields, *args), (0, 0, 18))
                        else:
                            with self.assertRaisesRegex(
                                verify_regression.VerificationError,
                                "lacks scalar traffic or an eligible vector",
                            ):
                                check(fields, *args)

    def test_schema_50_gstage_disables_ineligible_vector_shapes(self) -> None:
        check = verify_regression._check_gstage_boundary_coverage_schema50
        for override in (
            {"target_vector_partial_vl": 0},
            {"target_vector_nonzero_vstart": 1000},
            {"target_vector_masked": 1000},
            {"target_vector_lmul": "1,0,0,0,0,0,0"},
            {"target_vector_emul": "1,0,0,0,0,0,0"},
            {"target_vector_addressing": "1,0,0,0"},
            {"target_vector_eew": "1,0,0,0"},
            {"target_vector_sew": "1,0,0,0"},
        ):
            fields = self.gstage_boundary_fields([1] * 12, [0] * 18)
            fields.update(override)
            args = ([1, 1, 0, 1], [6, 6, 0, 18], [1, 1])
            with self.subTest(override=override):
                self.assertEqual(check(fields, *args), (6, 6, 0))
                fields["actual_vector_gstage_boundary_cross"] = ",".join(
                    map(str, [1] + [0] * 17)
                )
                with self.assertRaisesRegex(
                    verify_regression.VerificationError,
                    "does not match enabled classes",
                ):
                    check(fields, *args)

    def test_schema_50_gstage_disabled_and_unreachable_targets(self) -> None:
        check = verify_regression._check_gstage_boundary_coverage_schema50
        fields = self.gstage_boundary_fields([0] * 12, [0] * 18)
        fields["target_gstage_boundary"] = 0
        args = ([1, 1, 0, 1], [0] * 4, [1, 1])
        self.assertEqual(check(fields, *args), (0, 0, 0))
        for field, bins in (("actual_gstage_boundary_cross", 12),
                            ("actual_vector_gstage_boundary_cross", 18)):
            malformed = dict(fields)
            malformed[field] = ",".join(["1"] + ["0"] * (bins - 1))
            with self.subTest(field=field), self.assertRaisesRegex(
                verify_regression.VerificationError, "does not match enabled classes"
            ):
                check(malformed, *args)
        fields["target_gstage_boundary"] = 100
        for operations, modes in (([0] * 4, [1, 1]),
                                  ([0, 0, 1, 0], [1, 1]),
                                  ([1, 1, 0, 1], [0, 0])):
            with self.subTest(operations=operations, modes=modes), self.assertRaisesRegex(
                verify_regression.VerificationError,
                "lacks scalar traffic or an eligible vector",
            ):
                check(fields, operations, [10] * 4, modes)
        for rate in (1, 1000):
            fields = self.gstage_boundary_fields([1] * 12, [1] * 18)
            fields["target_gstage_boundary"] = rate
            self.assertEqual(check(fields, args[0], [20] * 4, args[2]), (6, 6, 18))

    def test_schema_50_rejects_invalid_or_missing_gstage_fields(self) -> None:
        check = verify_regression._check_gstage_boundary_coverage_schema50
        fields = self.gstage_boundary_fields([1] * 12, [1] * 18)
        args = ([1, 1, 0, 1], [100] * 4, [1, 1])
        for field in ("target_gstage_boundary", "target_vector_partial_vl",
                      "target_vector_nonzero_vstart", "target_vector_masked"):
            for value in (None, False, -1, 1001, "100", 1.5):
                malformed = dict(fields)
                malformed[field] = value
                with self.subTest(field=field, value=value), self.assertRaises(
                    verify_regression.VerificationError
                ):
                    check(malformed, *args)
        for field, bins in (("actual_gstage_boundary_cross", 12),
                            ("actual_vector_gstage_boundary_cross", 18)):
            for value in (None, "1,2", ",".join(["1"] * (bins + 1)),
                          ",".join(["-1"] + ["1"] * (bins - 1)),
                          ",".join(["1"] * (bins - 1) + ["oops"])):
                malformed = dict(fields)
                malformed[field] = value
                with self.subTest(field=field, value=value), self.assertRaises(
                    verify_regression.VerificationError
                ):
                    check(malformed, *args)
        for field in fields:
            malformed = dict(fields)
            del malformed[field]
            with self.subTest(missing=field), self.assertRaises(
                verify_regression.VerificationError
            ):
                check(malformed, *args)

    def test_schema_50_scalar_and_vector_subclasses_are_disjoint(self) -> None:
        scalar = verify_regression._check_scalar_boundary_subclass_accounting
        # 7 ordinary DCache loads remain after both fault subclasses are removed.
        self.assertEqual(scalar([20, 17], (5, 8), (8, 9)), 7)
        self.assertEqual(scalar([13, 17], (5, 8), (8, 9)), 0)
        for actions in ([12, 17], [20, 16]):
            with self.subTest(actions=actions), self.assertRaisesRegex(
                verify_regression.VerificationError, "exceed operation counts"
            ):
                scalar(actions, (5, 8), (8, 9))
        vector = verify_regression._check_vector_store_subclass_accounting
        vector([0, 0, 0, 15], [0, 3], 5, 7)
        for actions in (3, 5, 7, 8, 10, 12, 14, 16):
            with self.subTest(actions=actions), self.assertRaisesRegex(
                verify_regression.VerificationError, "operation accounting is not conserved"
            ):
                vector([0, 0, 0, actions], [0, 3], 5, 7)

    def test_schema_44_checks_vector_fof_accounting(self) -> None:
        fields: dict[str, object] = {
            "target_vector_fof": 100,
            "target_vector_fof_first_fault": 500,
            "actual_vector_fof": "7,16",
            # later-element fault, first-element fault
            "actual_vector_fof_fault": "8,8",
            "actual_vector_fof_eew": "4,4,4,4",
            "actual_vector_fof_stage1_mode": "8,8",
            "actual_vector_fof_cross": ",".join(["1"] * 16),
            "actual_vector_fof_fix_vl": 16,
        }
        target_operations = [0, 0, 1, 0]
        actual_operations = [0, 0, 23, 0]
        target_translation = [1, 1, 0]
        target_stage1 = [1, 1]
        target_vector_eew = [1, 1, 1, 1]
        vector_directions = [7, 0]
        verify_regression._check_vector_fof_coverage(
            fields,
            target_operations,
            actual_operations,
            target_translation,
            target_stage1,
            target_vector_eew,
            vector_directions,
        )

        fields["actual_vector_fof_fix_vl"] = 15
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "fault-position/fix-VL accounting",
        ):
            verify_regression._check_vector_fof_coverage(
                fields,
                target_operations,
                actual_operations,
                target_translation,
                target_stage1,
                target_vector_eew,
                vector_directions,
            )

        fields.update(
            {
                "target_vector_fof": 0,
                "actual_vector_fof": "7,0",
                "actual_vector_fof_fault": "0,0",
                "actual_vector_fof_eew": "0,0,0,0",
                "actual_vector_fof_stage1_mode": "0,0",
                "actual_vector_fof_cross": ",".join(["0"] * 16),
                "actual_vector_fof_fix_vl": 0,
            }
        )
        actual_operations[2] = 7
        verify_regression._check_vector_fof_coverage(
            fields,
            target_operations,
            actual_operations,
            target_translation,
            target_stage1,
            target_vector_eew,
            vector_directions,
        )
        fields["actual_vector_fof_fault"] = "1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "disabled VFOF"
        ):
            verify_regression._check_vector_fof_coverage(
                fields,
                target_operations,
                actual_operations,
                target_translation,
                target_stage1,
                target_vector_eew,
                vector_directions,
            )

    def test_schema_46_checks_vector_segment_fof_accounting(self) -> None:
        def make_fields(
            first_fault: int, target_fof: int = 100
        ) -> dict[str, object]:
            crosses: list[int] = []
            fault_positions = [0, 0]
            modes = [0, 0]
            eews = [0, 0, 0, 0]
            nfs = [0] * 7
            data_uops = 0
            for fault in range(2):
                enabled_fault = (
                    first_fault != 0 if fault == 1 else first_fault != 1000
                )
                for mode in range(2):
                    for eew in range(4):
                        for nf in range(7):
                            count = int(enabled_fault)
                            crosses.append(count)
                            fault_positions[fault] += count
                            modes[mode] += count
                            eews[eew] += count
                            nfs[nf] += count
                            data_uops += count * (nf + 2)
            fof_actions = sum(crosses)
            return {
                "target_vector_segment_fof": target_fof,
                "target_vector_segment_fof_first_fault": first_fault,
                "target_vector_segment_addressing": "1,1,1,1",
                "target_vector_segment_eew": "1,1,1,1",
                "target_vector_segment_sew": "1,1,1,1",
                "target_vector_segment_lmul": "1,1,1,1,1,1,1",
                "target_vector_segment_emul": "1,1,1,1,1,1,1",
                "target_vector_segment_nf": "1,1,1,1,1,1,1",
                "actual_vector_segment_fof": f"1,{fof_actions}",
                "actual_vector_segment_fof_fault": ",".join(
                    str(value) for value in fault_positions
                ),
                "actual_vector_segment_fof_stage1_mode": ",".join(
                    str(value) for value in modes
                ),
                "actual_vector_segment_fof_eew": ",".join(
                    str(value) for value in eews
                ),
                "actual_vector_segment_fof_nf": ",".join(
                    str(value) for value in nfs
                ),
                "actual_vector_segment_fof_cross": ",".join(
                    str(value) for value in crosses
                ),
                "actual_vector_segment_fof_data_uops": data_uops,
                "actual_vector_segment_fof_fix_vl": fof_actions,
            }

        target_operations = [0, 0, 0, 0, 1]
        target_translation = [0, 1, 0]
        target_stage1 = [1, 1]
        segment_directions = [1, 1]
        fields = make_fields(500)
        actual_operations = [0, 0, 0, 0, 114]
        verify_regression._check_vector_segment_fof_coverage_schema46(
            fields,
            target_operations,
            actual_operations,
            target_translation,
            target_stage1,
            500,
            segment_directions,
        )

        fields["actual_vector_segment_fof_data_uops"] = 559
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "cross/data-uop/fix-VL accounting",
        ):
            verify_regression._check_vector_segment_fof_coverage_schema46(
                fields,
                target_operations,
                actual_operations,
                target_translation,
                target_stage1,
                500,
                segment_directions,
            )

        fields = make_fields(500)
        disabled_fields = make_fields(500, target_fof=0)
        disabled_fields.update(
            {
                "actual_vector_segment_fof": "0,0",
                "actual_vector_segment_fof_fault": "0,0",
                "actual_vector_segment_fof_stage1_mode": "0,0",
                "actual_vector_segment_fof_eew": "0,0,0,0",
                "actual_vector_segment_fof_nf": "0,0,0,0,0,0,0",
                "actual_vector_segment_fof_cross": ",".join(["0"] * 112),
                "actual_vector_segment_fof_data_uops": 0,
                "actual_vector_segment_fof_fix_vl": 0,
            }
        )
        verify_regression._check_vector_segment_fof_coverage_schema46(
            disabled_fields,
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0],
            [0, 0],
            500,
            [0, 0],
        )
        disabled_fields["actual_vector_segment_fof"] = "0,1"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "disabled segment FOF"
        ):
            verify_regression._check_vector_segment_fof_coverage_schema46(
                disabled_fields,
                [0, 0, 0, 0, 0],
                [0, 0, 0, 0, 1],
                [0, 0, 0],
                [0, 0],
                500,
                [0, 0],
            )

        fields = make_fields(500)
        fields["actual_vector_segment_fof_fix_vl"] = 111
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "cross/data-uop/fix-VL accounting",
        ):
            verify_regression._check_vector_segment_fof_coverage_schema46(
                fields,
                target_operations,
                actual_operations,
                target_translation,
                target_stage1,
                500,
                segment_directions,
            )

        fields = make_fields(500)
        fields["actual_vector_segment_fof_cross"] = ",".join(["1"] * 111 + ["0"])
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "does not match enabled classes",
        ):
            verify_regression._check_vector_segment_fof_coverage_schema46(
                fields,
                target_operations,
                actual_operations,
                target_translation,
                target_stage1,
                500,
                segment_directions,
            )

        fields = make_fields(500)
        fields["actual_vector_segment_fof_cross"] = ",".join(["1"] * 111)
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "expected 112"
        ):
            verify_regression._check_vector_segment_fof_coverage_schema46(
                fields,
                target_operations,
                actual_operations,
                target_translation,
                target_stage1,
                500,
                segment_directions,
            )

        for first_fault in (0, 1000):
            fields = make_fields(first_fault)
            actual_fof = 56
            verify_regression._check_vector_segment_fof_coverage_schema46(
                fields,
                target_operations,
                [0, 0, 0, 0, actual_fof + 2],
                target_translation,
                target_stage1,
                500,
                segment_directions,
            )

        fields = make_fields(500, target_fof=0)
        fields.update(
            {
                "actual_vector_segment_fof": "1,0",
                "actual_vector_segment_fof_fault": "0,0",
                "actual_vector_segment_fof_stage1_mode": "0,0",
                "actual_vector_segment_fof_eew": "0,0,0,0",
                "actual_vector_segment_fof_nf": "0,0,0,0,0,0,0",
                "actual_vector_segment_fof_cross": ",".join(["0"] * 112),
                "actual_vector_segment_fof_data_uops": 0,
                "actual_vector_segment_fof_fix_vl": 0,
            }
        )
        verify_regression._check_vector_segment_fof_coverage_schema46(
            fields,
            target_operations,
            [0, 0, 0, 0, 2],
            target_translation,
            target_stage1,
            500,
            segment_directions,
        )

        fields["actual_vector_segment_fof_fault"] = "1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "disabled segment FOF"
        ):
            verify_regression._check_vector_segment_fof_coverage_schema46(
                fields,
                target_operations,
                [0, 0, 0, 0, 2],
                target_translation,
                target_stage1,
                500,
                segment_directions,
            )

        fields = make_fields(500, target_fof=0)
        fields["actual_vector_segment_fof"] = "0,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "ordinary load/store and segment FOF accounting",
        ):
            verify_regression._check_vector_segment_fof_coverage_schema46(
                fields,
                target_operations,
                [0, 0, 0, 0, 2],
                target_translation,
                target_stage1,
                500,
                segment_directions,
            )

        fields = make_fields(500)
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "ordinary load/store and segment FOF accounting",
        ):
            verify_regression._check_vector_segment_fof_coverage_schema46(
                fields,
                [0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0],
                target_translation,
                [0, 0],
                500,
                [0, 0],
            )

    def test_schema_47_checks_all_legal_vector_segment_fof_shapes(self) -> None:
        crosses: list[int] = []
        fault_positions = [0, 0]
        modes = [0, 0]
        eews = [0] * 4
        sews = [0] * 4
        lmuls = [0] * 7
        emuls = [0] * 7
        nfs = [0] * 7
        data_uops = 0
        shape_count = 0
        shapes: list[tuple[int, int, int, int, int, int]] = []
        for eew in range(4):
            for sew in range(4):
                for lmul in range(-3, 4):
                    emul = eew - sew + lmul
                    if lmul < sew - 3 or emul < -3 or emul > 3:
                        continue
                    uops_per_field = 1 << max(emul, 0)
                    for nf in range(7):
                        shape_uops = uops_per_field * (nf + 2)
                        if shape_uops <= 8:
                            shapes.append(
                                (eew, sew, lmul, emul, nf, shape_uops)
                            )
        self.assertEqual(len(shapes), 338)
        for fault in range(2):
            for mode in range(2):
                for eew, sew, lmul, emul, nf, shape_uops in shapes:
                    crosses.append(1)
                    fault_positions[fault] += 1
                    modes[mode] += 1
                    eews[eew] += 1
                    sews[sew] += 1
                    lmuls[lmul + 3] += 1
                    emuls[emul + 3] += 1
                    nfs[nf] += 1
                    data_uops += shape_uops
                    shape_count += 1
        self.assertEqual(shape_count, 1352)
        fields: dict[str, object] = {
            "target_vector_segment_fof": 100,
            "target_vector_segment_fof_first_fault": 500,
            "target_vector_segment_addressing": "1,1,1,1",
            "target_vector_segment_eew": "1,1,1,1",
            "target_vector_segment_sew": "1,1,1,1",
            "target_vector_segment_lmul": "1,1,1,1,1,1,1",
            "target_vector_segment_emul": "1,1,1,1,1,1,1",
            "target_vector_segment_nf": "1,1,1,1,1,1,1",
            "actual_vector_segment_fof": f"1,{shape_count}",
            "actual_vector_segment_fof_fault": ",".join(
                str(value) for value in fault_positions
            ),
            "actual_vector_segment_fof_stage1_mode": ",".join(
                str(value) for value in modes
            ),
            "actual_vector_segment_fof_eew": ",".join(
                str(value) for value in eews
            ),
            "actual_vector_segment_fof_sew": ",".join(
                str(value) for value in sews
            ),
            "actual_vector_segment_fof_lmul": ",".join(
                str(value) for value in lmuls
            ),
            "actual_vector_segment_fof_emul": ",".join(
                str(value) for value in emuls
            ),
            "actual_vector_segment_fof_nf": ",".join(
                str(value) for value in nfs
            ),
            "actual_vector_segment_fof_cross": ",".join(
                str(value) for value in crosses
            ),
            "actual_vector_segment_fof_data_uops": data_uops,
            "actual_vector_segment_fof_fix_vl": shape_count,
        }
        args = (
            fields,
            [0, 0, 0, 0, 1],
            [0, 0, 0, 0, shape_count + 2],
            [0, 1, 0],
            [1, 1],
            500,
            [1, 1],
        )
        self.assertEqual(
            verify_regression._check_vector_segment_fof_coverage_schema47(
                *args
            ),
            shape_count,
        )

        fields["actual_vector_segment_fof_data_uops"] = data_uops - 1
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "shape-cross/data-uop/fix-VL accounting",
        ):
            verify_regression._check_vector_segment_fof_coverage_schema47(
                *args
            )
        fields["actual_vector_segment_fof_data_uops"] = data_uops
        fields["actual_vector_segment_fof_cross"] = ",".join(
            ["1"] * 1351 + ["0"]
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "does not match enabled classes",
        ):
            verify_regression._check_vector_segment_fof_coverage_schema47(
                *args
            )

    def test_schema_42_checks_concurrent_full_vector_shapes(self) -> None:
        result = {
            "target_concurrent": 1000,
            "actual_concurrent_vector_shape": ",".join(["1"] * 16),
            "actual_concurrent_vector_uops": "20,24",
            "concurrent": "8,56,4,8,5",
        }
        target_operations = [1, 1, 1, 1, 1]
        vector_targets = {
            "addressing": [1] * 4,
            "eew": [1] * 4,
            "sew": [1] * 4,
            "lmul": [1] * 7,
            "emul": [1] * 7,
        }
        policy_targets = {
            "masked": 500,
            "partial_vl": 500,
            "nonzero_vstart": 500,
        }
        verify_regression._check_concurrent_vector_shapes(
            result, target_operations, vector_targets, policy_targets
        )

        result["actual_concurrent_vector_shape"] = ",".join(
            ["0"] + ["1"] * 15
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_concurrent_vector_shape",
        ):
            verify_regression._check_concurrent_vector_shapes(
                result, target_operations, vector_targets, policy_targets
            )

        result["actual_concurrent_vector_shape"] = ",".join(["1"] * 16)
        result["actual_concurrent_vector_uops"] = "8,24"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "concurrent vector uop counts"
        ):
            verify_regression._check_concurrent_vector_shapes(
                result, target_operations, vector_targets, policy_targets
            )

        result.update(
            {
                "target_concurrent": 0,
                "actual_concurrent_vector_shape": ",".join(["0"] * 16),
                "actual_concurrent_vector_uops": "0,0",
                "concurrent": "0,0,0,0,0",
            }
        )
        verify_regression._check_concurrent_vector_shapes(
            result, target_operations, vector_targets, policy_targets
        )

    def test_schema_43_checks_mixed_miss_burst_accounting(self) -> None:
        # depth2/width1/scalar, depth2/width1/vector, and
        # depth2/width2/scalar are the only enabled Bare crosses.
        cross = [0] * 270
        cross[0] = 1
        cross[3] = 1
        cross[6] = 1
        fields: dict[str, object] = {
            "target_miss_burst_depth": "1," + ",".join(["0"] * 14),
            "target_miss_burst_issue_width": "1,1,1",
            "target_miss_burst_composition": "1,1",
            "actual_miss_burst_depth": "3," + ",".join(["0"] * 14),
            "actual_miss_burst_issue_width": "2,1,0",
            "actual_miss_burst_composition": "2,1",
            "actual_miss_burst_translation": "3,0,0",
            "actual_miss_burst_cross": ",".join(map(str, cross)),
            "actual_miss_burst_manager": "3,6,5,1,6,6,6,5,1,7",
            "actual_miss_burst_max_outstanding": 2,
        }
        target_operations = [0] * 14 + [1]
        actual_operations = [0] * 14 + [3]
        verify_regression._check_miss_burst_coverage(
            fields, 43, target_operations, actual_operations, [1, 0, 0]
        )

        broken_cross = cross.copy()
        broken_cross[3] = 0
        broken_cross[9] = 1
        fields["actual_miss_burst_cross"] = ",".join(map(str, broken_cross))
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_miss_burst_cross",
        ):
            verify_regression._check_miss_burst_coverage(
                fields, 43, target_operations, actual_operations, [1, 0, 0]
            )

        fields["actual_miss_burst_cross"] = ",".join(map(str, cross))
        manager = [3, 6, 5, 1, 6, 6, 6, 5, 0, 7]
        fields["actual_miss_burst_manager"] = ",".join(map(str, manager))
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "miss-burst manager accounting",
        ):
            verify_regression._check_miss_burst_coverage(
                fields, 43, target_operations, actual_operations, [1, 0, 0]
            )

        manager[8] = 1
        manager[9] -= 1
        fields["actual_miss_burst_manager"] = ",".join(map(str, manager))
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "miss-burst manager accounting",
        ):
            verify_regression._check_miss_burst_coverage(
                fields, 43, target_operations, actual_operations, [1, 0, 0]
            )

        fields.update(
            {
                "actual_miss_burst_depth": ",".join(["0"] * 15),
                "actual_miss_burst_issue_width": "0,0,0",
                "actual_miss_burst_composition": "0,0",
                "actual_miss_burst_translation": "0,0,0",
                "actual_miss_burst_cross": ",".join(["0"] * 270),
                "actual_miss_burst_manager": "0,0,0,0,0,0,0,0,0,0",
                "actual_miss_burst_max_outstanding": 0,
            }
        )
        disabled_operations = [0] * 15
        verify_regression._check_miss_burst_coverage(
            fields, 43, disabled_operations, disabled_operations, [1, 0, 0]
        )
        fields["actual_miss_burst_manager"] = "0,0,0,0,1,1,1,0,0,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "disabled miss-burst",
        ):
            verify_regression._check_miss_burst_coverage(
                fields, 43, disabled_operations, disabled_operations, [1, 0, 0]
            )

    def test_schema_45_checks_miss_burst_saturation_progress(self) -> None:
        cross = [0] * (16 * 3 * 2 * 3)
        for width in range(3):
            for composition in range(2):
                cross[((15 * 3 + width) * 2 + composition) * 3] = 1
        fields: dict[str, object] = {
            "target_miss_burst_depth": ",".join(["0"] * 15 + ["1"]),
            "target_miss_burst_issue_width": "1,1,1",
            "target_miss_burst_composition": "1,1",
            "actual_miss_burst_depth": ",".join(["0"] * 15 + ["6"]),
            "actual_miss_burst_issue_width": "2,2,2",
            "actual_miss_burst_composition": "3,3",
            "actual_miss_burst_translation": "6,0,0",
            "actual_miss_burst_cross": ",".join(map(str, cross)),
            "actual_miss_burst_manager": "6,102,99,3,102,102,102,99,3,105",
            "actual_miss_burst_saturation": "6,6,6",
            "actual_miss_burst_max_outstanding": 16,
        }
        target_operations = [0] * 14 + [1]
        actual_operations = [0] * 14 + [6]
        verify_regression._check_miss_burst_coverage(
            fields, 45, target_operations, actual_operations, [1, 0, 0]
        )

        fields["actual_miss_burst_max_outstanding"] = 15
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "miss-burst manager accounting",
        ):
            verify_regression._check_miss_burst_coverage(
                fields, 45, target_operations, actual_operations, [1, 0, 0]
            )

        fields["actual_miss_burst_max_outstanding"] = 16
        for broken_stage in range(3):
            saturation = [6, 6, 6]
            saturation[broken_stage] = 5
            fields["actual_miss_burst_saturation"] = ",".join(
                map(str, saturation)
            )
            with self.assertRaisesRegex(
                verify_regression.VerificationError,
                "miss-burst manager accounting",
            ):
                verify_regression._check_miss_burst_coverage(
                    fields,
                    45,
                    target_operations,
                    actual_operations,
                    [1, 0, 0],
                )

    def test_constraint_schemas_ten_and_eleven_check_vector_shapes_and_policy(
        self,
    ) -> None:
        result = mixed_result(10)
        result.update(
            {
                "constraint_schema": 10,
                "target_translation": "1,0,0",
                "actual_translation": "7,0,0",
                "target_stage1_mode": "0,0",
                "actual_stage1_mode": "0,0",
                "target_vs_mode": "0,0",
                "actual_vs_mode": "0,0",
                "target_g_mode": "0,0",
                "actual_g_mode": "0,0",
                "actual_nested_pairs": "0,0,0,0",
                "target_fence_kind": "0,0,0",
                "target_fence_scope": "0,0",
                "target_tlb_flush": 0,
                "actual_fences": "0,0,0,0,0,0",
                "actual_translation_switch": 0,
                "actual_translation_walk_reuse": "0,0",
                "target_probe": 0,
                "target_probe_to_b": 500,
                "target_probe_need_data": 500,
                "actual_probe_sequences": 0,
                "actual_probe_caps": "0,0",
                "actual_probe_need_data": "0,0",
                "probes": 0,
                "load_wakeups": "9,7,5",
                "load_cancels": "3,2,1",
                "ifetch_prefetches": 1,
                "target_stride_stream": 0,
                "l2_stride_prefetches": 0,
                "raw_load_wakeups": "9,7,5",
                "raw_load_cancels": "3,2,1",
                "target_ops": "0,0,3,2,0,0,0,0,0,0",
                "actual_ops": "0,0,3,2,0,0,0,0,0,0",
                "target_hypervisor_family": "0,0,0",
                "actual_hypervisor_family": "0,0,0",
                "target_vector_segment_store": 0,
                "actual_vector_segment_direction": "0,0",
                "target_vector_segment_addressing": "0,0,0,0",
                "actual_vector_segment_addressing": "0,0,0,0",
                "target_vector_segment_eew": "0,0,0,0",
                "actual_vector_segment_eew": "0,0,0,0",
                "target_vector_segment_sew": "0,0,0,0",
                "actual_vector_segment_sew": "0,0,0,0",
                "target_vector_segment_lmul": "0,0,0,0,0,0,0",
                "actual_vector_segment_lmul": "0,0,0,0,0,0,0",
                "target_vector_segment_emul": "0,0,0,0,0,0,0",
                "actual_vector_segment_emul": "0,0,0,0,0,0,0",
                "target_vector_segment_nf": "0,0,0,0,0,0,0",
                "actual_vector_segment_nf": "0,0,0,0,0,0,0",
                "target_vector_addressing": "1,0,1,0",
                "actual_vector_direction": "2,1",
                "actual_vector_addressing": "2,0,1,0",
                "target_vector_eew": "1,0,1,0",
                "actual_vector_eew": "1,0,2,0",
                "target_vector_sew": "0,1,0,1",
                "actual_vector_sew": "0,2,0,1",
                "target_vector_lmul": "0,0,1,1,1,0,0",
                "actual_vector_lmul": "0,0,1,1,1,0,0",
                "target_vector_emul": "0,1,0,1,0,1,0",
                "actual_vector_emul": "0,1,0,1,0,1,0",
                "actual_vector_shape_ops": 3,
                "actual_vector_uops": 5,
                "actual_vector_multi_uop": 1,
            }
        )
        verify_regression._check_mixed_coverage(result)

        result.update(
            {
                "constraint_schema": 11,
                "target_vector_masked": 500,
                "target_vector_vma": 500,
                "target_vector_vta": 500,
                "target_vector_partial_vl": 500,
                "target_vector_nonzero_vstart": 500,
                "actual_vector_masked": "2,1",
                "actual_vector_vma": "1,2",
                "actual_vector_vta": "2,1",
                "actual_vector_partial_vl": "1,2",
                "actual_vector_nonzero_vstart": "2,1",
                "actual_vector_agnostic": "1,1",
            }
        )
        verify_regression._check_mixed_coverage(result)

        result.update(
            {
                "constraint_schema": 12,
                "target_stage1_napot": 500,
                "target_nested_vs_napot": 500,
                "target_nested_g_napot": 500,
                "actual_stage1_leaf": "0,0",
                "actual_nested_leaf_topology": "0,0,0,0",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_stage1_leaf"] = "1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "disabled stage-1"
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "target_translation": "0,1,1",
                "actual_translation": "0,2,4",
                "target_stage1_mode": "1,0",
                "actual_stage1_mode": "2,0",
                "target_vs_mode": "1,0",
                "actual_vs_mode": "4,0",
                "target_g_mode": "1,0",
                "actual_g_mode": "4,0",
                "actual_nested_pairs": "4,0,0,0",
                "actual_translation_switch": 1,
                "actual_translation_walk_reuse": "2,4",
                "actual_stage1_leaf": "1,1",
                "actual_nested_leaf_topology": "1,1,1,1",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_nested_leaf_topology"] = "1,1,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_nested_leaf_topology",
        ):
            verify_regression._check_mixed_coverage(result)

        result.update(
            {
                "target_translation": "1,0,0",
                "actual_translation": "7,0,0",
                "target_stage1_mode": "0,0",
                "actual_stage1_mode": "0,0",
                "target_vs_mode": "0,0",
                "actual_vs_mode": "0,0",
                "target_g_mode": "0,0",
                "actual_g_mode": "0,0",
                "actual_nested_pairs": "0,0,0,0",
                "actual_translation_switch": 0,
                "actual_translation_walk_reuse": "0,0",
                "actual_stage1_leaf": "0,0",
                "actual_nested_leaf_topology": "0,0,0,0",
            }
        )
        result.update(
            {
                "constraint_schema": 13,
                "target_ops": "0,0,3,2,0,0,0,0,0,0,1",
                "actual_ops": "0,0,3,2,0,0,0,0,0,0,6",
                "target_cmo_operation": "1,1,1",
                "actual_cmo_operation": "1,2,3",
                "target_cmo_dirty": 500,
                "actual_cmo_line_state": "3,3",
                "target_cmo_younger_overlap": 10,
                "actual_cmo_younger_overlap": "5,1",
                "probes": 6,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["probes"] = 5
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "constrained/overlap/CMO accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["probes"] = 6
        result["actual_cmo_younger_overlap"] = "5,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_cmo_younger_overlap",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_cmo_younger_overlap"] = "5,1"
        result["target_cmo_operation"] = "1,0,1"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_cmo_operation",
        ):
            verify_regression._check_mixed_coverage(result)
        result["target_cmo_operation"] = "1,1,1"
        result["target_cmo_dirty"] = 0
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_cmo_line_state",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 14,
                "target_cmo_dirty": 500,
                "target_probe": 1,
                "target_probe_to_b": 500,
                "target_probe_need_data": 500,
                "target_probe_overlap": 500,
                "actual_probe_sequences": 2,
                "actual_probe_caps": "1,1",
                "actual_probe_need_data": "1,1",
                "actual_probe_overlap": "1,1",
                "probe_max_outstanding": 2,
                "probes": 10,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["probe_max_outstanding"] = 1
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "two outstanding sources",
        ):
            verify_regression._check_mixed_coverage(result)
        result["probe_max_outstanding"] = 2
        result["actual_probe_overlap"] = "2,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_probe_overlap",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 15,
                "actual_cmo_operation": "2,2,2",
                "target_cmo_error": 1000,
                "target_cmo_error_denied": 500,
                "actual_cmo_error": "0,6",
                "actual_cmo_error_kind": "3,3",
                "actual_cmo_operation_error": "1,1,1,1,1,1",
                "actual_probe_overlap": "1,1",
                "probes": 4,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_cmo_error_kind"] = "4,2"
        result["actual_cmo_operation_error"] = "1,1,1,1,2,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "operation/error cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_cmo_error_kind"] = "3,3"
        result["actual_cmo_operation_error"] = "1,1,1,1,1,1"
        result["target_cmo_error"] = 500
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_cmo_error",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 16,
                "target_ops": "0,0,3,2,0,0,0,1,1,0,1",
                "actual_ops": "0,0,3,2,0,0,0,5,5,0,6",
                "target_cmo_error": 1000,
                "target_nc_store": 500,
                "target_mmio_store": 500,
                "target_uncache_error": 500,
                "target_uncache_load_error_denied": 500,
                "actual_nc_direction": "3,2",
                "actual_mmio_direction": "3,2",
                "actual_uncache_error": "4,6",
                "actual_uncache_error_kind": "2,4",
                "actual_uncache_outcome":
                    "1,1,1,1,0,1,1,1,1,1,0,1",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_uncache_outcome"] = (
            "1,1,1,0,1,1,1,1,1,1,0,1"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_uncache_outcome",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 17,
                "target_ops": "1,0,3,2,0,0,0,1,1,0,1",
                "actual_ops": "10,0,3,2,0,0,0,5,5,0,6",
                "actual_uncache_outcome":
                    "1,1,1,1,0,1,1,1,1,1,0,1",
                "target_dcache_load_error": 500,
                "target_dcache_load_error_denied": 500,
                "actual_dcache_load_error": "4,6",
                "actual_dcache_load_error_kind": "2,4",
                "actual_dcache_load_outcome": "4,2,4",
                "actual_dcache_load_error_manager": "6,8,12,6,6",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_dcache_load_error_manager"] = "6,7,12,6,6"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "DCache load error manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_dcache_load_error_manager"] = "6,8,12,6,6"
        result["actual_ops"] = "11,0,3,2,0,0,0,5,5,0,6"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "DCache load error coverage is not conserved",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_ops"] = "10,0,3,2,0,0,0,5,5,0,6"
        result.update(
            {
                "constraint_schema": 18,
                "target_ops": "1,0,3,2,0,0,1,1,1,0,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,0,6",
                "target_atomic_family": "1,1,1",
                "target_atomic_width": "1,1",
                "target_atomic_error": 500,
                "target_atomic_error_denied": 500,
                "actual_atomic_family": "6,6,6",
                "actual_atomic_width": "9,9",
                "actual_atomic_error": "6,12",
                "actual_atomic_error_kind": "6,6",
                "actual_atomic_outcome":
                    "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1",
                "actual_atomic_error_manager": "12,12,24,12,12",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_atomic_error_manager"] = "12,11,24,12,12"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "atomic error manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_atomic_error_manager"] = "12,12,24,12,12"
        result["actual_atomic_outcome"] = (
            "2,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "actual_atomic_outcome"
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_atomic_outcome"] = (
            "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1"
        )
        result["actual_ops"] = "10,0,3,2,0,0,19,5,5,0,6"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "atomic error coverage is not conserved",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_ops"] = "10,0,3,2,0,0,18,5,5,0,6"
        result.update(
            {
                "constraint_schema": 19,
                "target_ops": "1,0,3,2,0,0,1,1,1,0,1,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,0,6,90",
                "target_stage1_mode": "1,1",
                "target_vs_mode": "1,1",
                "target_g_mode": "1,1",
                "target_ptw_error_site": "1,1,1,1,1",
                "target_ptw_error_level": "1,1,1",
                "target_ptw_error_store": 500,
                "target_ptw_error_denied": 500,
                "target_ptw_error_corrupt_first": 500,
                "actual_ptw_error_outcome": ",".join(["1"] * 90),
                "actual_ptw_error_mode": (
                    "9,9,0,0,9,9,0,0,4,4,5,5,4,4,5,5,4,4,5,5"
                ),
                "actual_ptw_error_target_level": (
                    "6,4,5,3,6,4,5,3,6,4,5,3,6,4,5,3,6,4,5,3"
                ),
                "actual_ptw_error_manager": "90,60,120",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_ptw_error_manager"] = "93,62,124"
        verify_regression._check_mixed_coverage(result)
        result["actual_ptw_error_manager"] = "93,62,123"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "PTW error manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_ptw_error_manager"] = "90,59,120"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "PTW error manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_ptw_error_manager"] = "90,60,120"
        result["actual_ptw_error_outcome"] = ",".join(
            ["0"] + ["1"] * 89
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_ptw_error_outcome",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_ptw_error_outcome"] = ",".join(["1"] * 90)
        result.update(
            {
                "constraint_schema": 20,
                "target_ops": "1,0,3,2,0,0,1,1,1,0,1,1,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,0,6,90,12",
                "target_load_merge_depth": "1,1",
                "target_load_merge_pattern": "1,1,1",
                "actual_load_merge_shape": ",".join(["1"] * 12),
                "actual_load_merge_translation": "12,0,0",
                "actual_load_merge_manager": "12,14,14,30",
                "actual_load_merge_loads": 30,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_load_merge_shape"] = ",".join(
            ["0"] + ["1"] * 11
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_load_merge_shape",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_load_merge_shape"] = ",".join(["1"] * 12)
        result["actual_load_merge_manager"] = "13,14,14,30"
        verify_regression._check_mixed_coverage(result)
        result["actual_load_merge_manager"] = "15,14,14,30"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "load-merge manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_load_merge_manager"] = "12,14,15,30"
        result["dcache_acquire_perms"] = 1
        verify_regression._check_mixed_coverage(result)
        result["dcache_acquire_perms"] = 0
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "load-merge manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_load_merge_manager"] = "12,14,14,30"
        result.update(
            {
                "constraint_schema": 21,
                "target_ops": "1,0,3,2,0,0,1,1,1,0,1,1,1,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,0,6,90,12,8",
                "target_set_pressure_depth": "1,1",
                "target_set_pressure_width": "1,1,1,1",
                "target_set_pressure_set": "1,1,1,1",
                "actual_set_pressure_cross": ",".join(["1,0,0"] * 8),
                "actual_set_pressure_set": "2,2,2,2",
                "actual_set_pressure_issue_order": "38,38",
                "actual_set_pressure_manager": (
                    "8,76,76,12,14,14,76,76,12"
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_cross"] = ",".join(
            ["0,0,0"] + ["1,0,0"] * 7
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_set_pressure_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_cross"] = ",".join(["1,0,0"] * 8)
        result["actual_set_pressure_manager"] = (
            "8,76,75,12,14,14,76,76,12"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_manager"] = (
            "8,76,76,12,14,14,76,76,12"
        )
        result.update(
            {
                "constraint_schema": 22,
                "target_ops": "1,0,3,2,0,0,1,1,1,1,1,1,1,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,6,6,90,12,8",
                "target_hypervisor_family": "1,1,1",
                "actual_hypervisor_family": "2,2,2",
                "target_hypervisor_spvp_user": 500,
                "actual_hypervisor_spvp": "3,3",
                "actual_hypervisor_cross": "1,1,1,1,1,1",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_cross"] = "2,0,1,1,1,1"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_cross"] = "1,1,1,1,1,1"
        result["actual_hypervisor_spvp"] = "4,2"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "hypervisor SPVP/cross coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "actual_ops": "10,0,3,2,0,0,18,5,5,3,6,90,12,8",
                "actual_hypervisor_family": "1,1,1",
                "target_hypervisor_spvp_user": 0,
                "actual_hypervisor_spvp": "3,0",
                "actual_hypervisor_cross": "1,0,1,0,1,0",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "target_hypervisor_spvp_user": 1000,
                "actual_hypervisor_spvp": "0,3",
                "actual_hypervisor_cross": "0,1,0,1,0,1",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["target_hypervisor_spvp_user"] = 1001
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "target_hypervisor_spvp_user",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 23,
                "target_ops": "1,0,3,2,0,0,1,1,1,1,1,1,1,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,12,6,90,12,8",
                "target_hypervisor_family": "1,1,1",
                "actual_hypervisor_family": "4,4,4",
                "target_hypervisor_spvp_user": 500,
                "actual_hypervisor_spvp": "6,6",
                "actual_hypervisor_cross": "2,2,2,2,2,2",
                "target_misaligned": 500,
                "actual_hypervisor_alignment": "6,6",
                "actual_hypervisor_alignment_cross": ",".join(["1"] * 12),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_alignment_cross"] = (
            "2,0," + ",".join(["1"] * 10)
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_alignment_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "target_misaligned": 0,
                "actual_hypervisor_alignment": "12,0",
                "actual_hypervisor_alignment_cross": ",".join(
                    ["2,0"] * 6
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "target_misaligned": 1000,
                "actual_hypervisor_alignment": "0,12",
                "actual_hypervisor_alignment_cross": ",".join(
                    ["0,2"] * 6
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["target_misaligned"] = 1001
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "target_misaligned",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 24,
                "target_ops": "1,0,3,2,0,0,1,1,1,1,1,1,1,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,30,6,90,12,8",
                "target_hypervisor_family": "1,1,1",
                "actual_hypervisor_family": "10,10,10",
                "target_hypervisor_spvp_user": 500,
                "actual_hypervisor_spvp": "15,15",
                "actual_hypervisor_cross": "5,5,5,5,5,5",
                "target_misaligned": 500,
                "actual_hypervisor_alignment": "24,6",
                "actual_hypervisor_alignment_cross": ",".join(
                    ["4,1"] * 6
                ),
                "target_hypervisor_pbmt_pair": "1,1,1,1,1",
                "actual_hypervisor_pbmt_pair": "6,6,6,6,6",
                "actual_hypervisor_pbmt_cross": ",".join(["1"] * 30),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_pbmt_cross"] = ",".join(
            ["0"] + ["1"] * 29
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_pbmt_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "actual_ops": "10,0,3,2,0,0,18,5,5,24,6,90,12,8",
                "actual_hypervisor_family": "8,8,8",
                "actual_hypervisor_spvp": "12,12",
                "actual_hypervisor_cross": "4,4,4,4,4,4",
                "actual_hypervisor_alignment": "18,6",
                "actual_hypervisor_alignment_cross": ",".join(
                    ["3,1"] * 6
                ),
                "target_hypervisor_pbmt_pair": "1,0,1,1,1",
                "actual_hypervisor_pbmt_pair": "6,0,6,6,6",
                "actual_hypervisor_pbmt_cross": ",".join(
                    ["1,0,1,1,1"] * 6
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_pbmt_cross"] = ",".join(["1"] * 30)
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_pbmt_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 25,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,8",
                "actual_hypervisor_family": "12,12,12",
                "actual_hypervisor_spvp": "18,18",
                "actual_hypervisor_cross": "6,6,6,6,6,6",
                "actual_hypervisor_alignment": "30,6",
                "actual_hypervisor_alignment_cross": ",".join(
                    ["5,1"] * 6
                ),
                "target_hypervisor_pbmt_pair": "1,1,1,1,1",
                "actual_hypervisor_pbmt_pair": "12,6,6,6,6",
                "actual_hypervisor_pbmt_cross": ",".join(
                    ["2,1,1,1,1"] * 6
                ),
                "target_hypervisor_pma_device": 500,
                "actual_hypervisor_pma_device": "30,6",
                "actual_hypervisor_pma_device_cross": ",".join(
                    ["5,1"] * 6
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_pma_device_cross"] = ",".join(
            ["5,0"] + ["5,1"] * 5
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_pma_device_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "target_hypervisor_pma_device": 0,
                "actual_hypervisor_pma_device": "36,0",
                "actual_hypervisor_pma_device_cross": ",".join(
                    ["6,0"] * 6
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_pma_device_cross"] = ",".join(
            ["5,1"] + ["6,0"] * 5
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_pma_device_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 26,
                "actual_ops": "10,0,3,2,0,0,18,5,5,66,6,90,12,8",
                "actual_hypervisor_family": "22,22,22",
                "actual_hypervisor_spvp": "33,33",
                "actual_hypervisor_cross": "11,11,11,11,11,11",
                "actual_hypervisor_alignment": "48,18",
                "actual_hypervisor_alignment_cross": ",".join(
                    ["8,3"] * 6
                ),
                "target_hypervisor_pbmt_pair": "1,1,1,1,1",
                "actual_hypervisor_pbmt_pair": "42,6,6,6,6",
                "actual_hypervisor_pbmt_cross": ",".join(
                    ["7,1,1,1,1"] * 6
                ),
                "target_hypervisor_pma_device": 500,
                "actual_hypervisor_pma_device": "60,6",
                "actual_hypervisor_pma_device_cross": ",".join(
                    ["10,1"] * 6
                ),
                "target_hypervisor_pmp_relation": "1,1,1,1,1,1,1",
                "actual_hypervisor_pmp_relation": "30,6,6,6,6,6,6",
                "actual_hypervisor_pmp_relation_cross": ",".join(
                    ["5,1,1,1,1,1,1"] * 6
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_pmp_relation_cross"] = ",".join(
            ["5,1,1,1,1,1,0"] + ["5,1,1,1,1,1,1"] * 5
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_pmp_relation_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_pmp_relation_cross"] = ",".join(
            ["6,0,0,0,0,0,0"] * 6
        )
        result.update(
            {
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,8",
                "actual_hypervisor_family": "12,12,12",
                "actual_hypervisor_spvp": "18,18",
                "actual_hypervisor_cross": "6,6,6,6,6,6",
                "actual_hypervisor_alignment": "30,6",
                "actual_hypervisor_alignment_cross": ",".join(
                    ["5,1"] * 6
                ),
                "actual_hypervisor_pbmt_pair": "12,6,6,6,6",
                "actual_hypervisor_pbmt_cross": ",".join(
                    ["2,1,1,1,1"] * 6
                ),
                "actual_hypervisor_pma_device": "30,6",
                "actual_hypervisor_pma_device_cross": ",".join(
                    ["5,1"] * 6
                ),
                "target_hypervisor_pmp_relation": "1,0,0,0,0,0,0",
                "actual_hypervisor_pmp_relation": "36,0,0,0,0,0,0",
                "actual_hypervisor_pmp_relation_cross": ",".join(
                    ["6,0,0,0,0,0,0"] * 6
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_pmp_relation_cross"] = ",".join(
            ["5,1,0,0,0,0,0"] + ["6,0,0,0,0,0,0"] * 5
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_pmp_relation_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_hypervisor_pmp_relation_cross"] = ",".join(
            ["6,0,0,0,0,0,0"] * 6
        )
        result.update(
            {
                "constraint_schema": 27,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,16",
                "target_set_pressure_dirty": 500,
                "actual_set_pressure_line_state": "8,8",
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 16
                ),
                "actual_set_pressure_set": "4,4,4,4",
                "actual_set_pressure_issue_order": "38,38",
                "actual_set_pressure_manager": (
                    "8,76,76,12,14,14,76,76,12"
                ),
                "actual_set_pressure_clean_manager": (
                    "8,76,76,76,12,12,0,14,2,2,152,152"
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_clean_manager"] = (
            "8,76,76,76,11,12,0,14,2,2,152,152"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "clean set-pressure manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_clean_manager"] = (
            "8,76,76,76,12,12,0,14,2,2,152,152"
        )
        result["actual_set_pressure_clean_manager"] = (
            "8,76,76,76,12,11,0,14,2,2,152,152"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "clean set-pressure manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_clean_manager"] = (
            "8,76,76,76,12,12,1,14,2,2,152,152"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "clean set-pressure manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_clean_manager"] = (
            "8,76,76,76,12,12,0,14,2,2,152,152"
        )
        result["actual_set_pressure_clean_manager"] = (
            "8,76,76,76,12,12,0,12,13,13,152,152"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "clean set-pressure manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_clean_manager"] = (
            "8,76,76,76,12,12,0,14,2,2,152,152"
        )
        result.update(
            {
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,8",
                "target_set_pressure_dirty": 0,
                "actual_set_pressure_line_state": "8,0",
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 8 + ["0,0,0"] * 8
                ),
                "actual_set_pressure_set": "2,2,2,2",
                "actual_set_pressure_issue_order": "0,0",
                "actual_set_pressure_manager": "0,0,0,0,0,0,0,0,0",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_cross"] = ",".join(
            ["1,0,0"] * 9 + ["0,0,0"] * 7
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_set_pressure_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 28,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,32",
                "target_set_pressure_dirty": 500,
                "target_set_pressure_refill_overlap": 500,
                "actual_set_pressure_line_state": "16,16",
                "actual_set_pressure_refill_overlap": "16,16",
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 32
                ),
                "actual_set_pressure_set": "8,8,8,8",
                "actual_set_pressure_issue_order": "76,76",
                "actual_set_pressure_manager": (
                    "16,152,152,24,28,28,152,152,24"
                ),
                "actual_set_pressure_clean_manager": (
                    "16,152,152,152,24,24,0,28,4,4,312,312"
                ),
                "actual_set_pressure_overlap_manager": "16,16,24,16,16",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_overlap_manager"] = "16,16,23,16,16"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure refill-overlap accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_overlap_manager"] = "16,16,24,16,16"
        result["actual_set_pressure_refill_overlap"] = "17,15"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure overlap/operation coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 29,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,64",
                "target_set_pressure_dirty": 500,
                "target_set_pressure_refill_overlap": 500,
                "target_set_pressure_release_backpressure": 500,
                "actual_set_pressure_line_state": "32,32",
                "actual_set_pressure_refill_overlap": "32,32",
                "actual_set_pressure_release_backpressure": "32,32",
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 64
                ),
                "actual_set_pressure_set": "16,16,16,16",
                "actual_set_pressure_issue_order": "152,152",
                "actual_set_pressure_manager": (
                    "32,304,304,48,56,56,304,304,48"
                ),
                "actual_set_pressure_clean_manager": (
                    "32,304,304,304,48,48,0,56,8,8,624,624"
                ),
                "actual_set_pressure_overlap_manager": "32,32,48,32,32",
                "actual_set_pressure_backpressure_manager": (
                    "32,32,512,512,64"
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_backpressure_manager"] = (
            "32,32,511,512,64"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure release-backpressure accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_backpressure_manager"] = (
            "32,32,512,512,64"
        )
        result["actual_set_pressure_release_backpressure"] = "33,31"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure backpressure/operation coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 30,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,128",
                "actual_set_pressure_line_state": "64,64",
                "actual_set_pressure_refill_overlap": "64,64",
                "actual_set_pressure_release_backpressure": "64,64",
                "target_set_pressure_dual_window": 500,
                "actual_set_pressure_dual_window": "64,64",
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 128
                ),
                "actual_set_pressure_set": "32,32,32,32",
                "actual_set_pressure_issue_order": "456,456",
                "actual_set_pressure_manager": (
                    "64,912,912,144,144,144,912,912,144"
                ),
                "actual_set_pressure_clean_manager": (
                    "64,912,912,912,144,144,0,144,0,0,1872,1872"
                ),
                "actual_set_pressure_overlap_manager": "96,96,144,96,96",
                "actual_set_pressure_backpressure_manager": (
                    "64,64,1024,1024,128"
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_dual_window"] = "65,63"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure window-count/operation coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 31,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,192",
                "actual_set_pressure_line_state": "96,96",
                "actual_set_pressure_refill_overlap": "96,96",
                "actual_set_pressure_release_backpressure": "96,96",
                "target_set_pressure_dual_window": 500,
                "target_set_pressure_triple_window": 333,
                "actual_set_pressure_dual_window": "128,64",
                "actual_set_pressure_window_count": "64,64,64",
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 192
                ),
                "actual_set_pressure_set": "48,48,48,48",
                "actual_set_pressure_issue_order": "912,912",
                "actual_set_pressure_manager": (
                    "96,1824,1824,288,288,288,1824,1824,288"
                ),
                "actual_set_pressure_clean_manager": (
                    "96,1824,1824,1824,288,288,0,288,0,0,3744,3744"
                ),
                "actual_set_pressure_overlap_manager": (
                    "192,192,576,192,192"
                ),
                "actual_set_pressure_backpressure_manager": (
                    "96,96,1536,1536,192"
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_window_count"] = "65,63,64"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure window-count/operation coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 32,
                "actual_set_pressure_window_count": "64,64,64",
                "target_probe_triple_overlap": 500,
                "actual_probe_sequences": 3,
                "actual_probe_caps": "1,2",
                "actual_probe_need_data": "1,2",
                "actual_probe_overlap": "1,2",
                "actual_probe_depth": "1,1,1",
                "probe_max_outstanding": 3,
                "probes": 8,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["probe_max_outstanding"] = 2
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "selected outstanding depth",
        ):
            verify_regression._check_mixed_coverage(result)
        result["probe_max_outstanding"] = 3
        result["actual_probe_depth"] = "1,2,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_probe_depth",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 33,
                "target_probe_deep_depth": "1,1,1,1,1,1",
                "actual_probe_sequences": 32,
                "actual_probe_caps": "16,16",
                "actual_probe_need_data": "16,16",
                "actual_probe_overlap": "4,28",
                "actual_probe_depth": "4,4,4,4,4,4,4,4",
                "actual_probe_cross": ",".join(["1"] * 32),
                "probe_max_outstanding": 8,
                "probes": 160,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 34,
                "probe_source_space": 64,
                "probe_source_lifecycle": "64,96,2",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["probe_source_lifecycle"] = "64,95,2"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "source lifecycle",
        ):
            verify_regression._check_mixed_coverage(result)
        result["probe_source_lifecycle"] = "64,96,1"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "source lifecycle",
        ):
            verify_regression._check_mixed_coverage(result)
        result["probe_source_lifecycle"] = "64,96,2"
        result["probe_source_space"] = 32
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "source space",
        ):
            verify_regression._check_mixed_coverage(result)
        result["probe_source_space"] = 64
        verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 35,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,256",
                "actual_atomic_family": "6,6,6",
                "actual_atomic_width": "9,9",
                "actual_atomic_error": "6,12",
                "actual_atomic_error_kind": "6,6",
                "actual_atomic_outcome": ",".join(["1,1,1"] * 6),
                "actual_atomic_error_manager": "12,12,24,12,12",
                "actual_set_pressure_line_state": "128,128",
                "actual_set_pressure_refill_overlap": "128,128",
                "actual_set_pressure_release_backpressure": "128,128",
                "target_set_pressure_quad_window": 250,
                "actual_set_pressure_dual_window": "192,64",
                "actual_set_pressure_window_count": "64,64,64,64",
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 256
                ),
                "actual_set_pressure_set": "64,64,64,64",
                "actual_set_pressure_issue_order": "1520,1520",
                "actual_set_pressure_manager": (
                    "128,3040,3040,480,480,480,3040,3040,480"
                ),
                "actual_set_pressure_clean_manager": (
                    "128,3040,3040,3040,480,480,0,480,0,0,6240,6240"
                ),
                "actual_set_pressure_overlap_manager": (
                    "320,320,480,320,320"
                ),
                "actual_set_pressure_backpressure_manager": (
                    "128,128,2048,2048,256"
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_window_count"] = "65,63,64,64"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure window-count/operation coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_window_count"] = "64,64,64,64"
        result["target_set_pressure_quad_window"] = 1001
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "target_set_pressure_quad_window",
        ):
            verify_regression._check_mixed_coverage(result)
        result["target_set_pressure_quad_window"] = 250
        verify_regression._check_mixed_coverage(result)
        result["probe_max_outstanding"] = 7
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "selected outstanding depth",
        ):
            verify_regression._check_mixed_coverage(result)
        result["probe_max_outstanding"] = 8
        result["target_probe_deep_depth"] = "0,0,0,0,0,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "target_probe_deep_depth cannot be all zero",
        ):
            verify_regression._check_mixed_coverage(result)
        result["target_probe_deep_depth"] = "1,1,1,1,1,1"
        result["actual_probe_cross"] = ",".join(["0"] + ["1"] * 31)
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_probe_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_probe_cross"] = ",".join(["2"] + ["1"] * 31)
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "does not match its marginals",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_probe_cross"] = ",".join(["1"] * 32)
        result.update(
            {
                "constraint_schema": 36,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,48,90,12,256",
                "actual_cmo_operation": "16,16,16",
                "actual_cmo_line_state": "24,24",
                "actual_cmo_younger_overlap": "24,24",
                "target_cmo_error": 0,
                "actual_cmo_error": "48,0",
                "actual_cmo_error_kind": "0,0",
                "actual_cmo_operation_error": "0,0,0,0,0,0",
                "target_cmo_probe_depth": "1,1,1,1,1,1,1,1",
                "actual_cmo_probe_depth": "6,6,6,6,6,6,6,6",
                "actual_cmo_probe_cross": ",".join(["1"] * 48),
                "probes": 376,
                "probe_source_lifecycle": "64,312,5",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_cmo_probe_cross"] = ",".join(["0"] + ["1"] * 47)
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_cmo_probe_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_cmo_probe_cross"] = ",".join(["1"] * 48)
        result["actual_cmo_probe_depth"] = "7,5,6,6,6,6,6,6"
        result["probes"] = 375
        result["probe_source_lifecycle"] = "64,311,5"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "does not match depth marginals",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_cmo_probe_depth"] = "6,6,6,6,6,6,6,6"
        result["probes"] = 376
        result["probe_source_lifecycle"] = "64,312,5"
        result["target_cmo_probe_depth"] = "0,0,0,0,0,0,0,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "target_cmo_probe_depth cannot be all zero",
        ):
            verify_regression._check_mixed_coverage(result)
        result["target_cmo_probe_depth"] = "1,1,1,1,1,1,1,1"
        result["probe_max_outstanding"] = 7
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "selected outstanding depth",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 37,
                "actual_ops": "10,0,3,2,0,0,66,5,5,36,48,90,12,256",
                "actual_atomic_family": "22,22,22",
                "actual_atomic_width": "33,33",
                "actual_atomic_error": "54,12",
                "actual_atomic_error_kind": "6,6",
                "actual_atomic_outcome": ",".join(["9,1,1"] * 6),
                "actual_atomic_error_manager": "12,12,24,12,12",
                "target_atomic_probe_depth": "1,1,1,1,1,1,1,1,1",
                "actual_atomic_probe_depth": "6,6,6,6,6,6,6,6,6",
                "actual_atomic_probe_cross": ",".join(["1"] * 54),
                "probes": 592,
                "probe_source_lifecycle": "64,528,9",
                "probe_max_outstanding": 8,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_atomic_probe_cross"] = ",".join(["0"] + ["1"] * 53)
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_atomic_probe_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_atomic_probe_cross"] = ",".join(["1"] * 54)
        result["actual_atomic_probe_depth"] = "7,5,6,6,6,6,6,6,6"
        result["probes"] = 591
        result["probe_source_lifecycle"] = "64,527,9"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "depth/cross coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_atomic_probe_depth"] = "6,6,6,6,6,6,6,6,6"
        result["probes"] = 592
        result["probe_source_lifecycle"] = "64,528,9"
        result["target_atomic_probe_depth"] = "0,0,0,0,0,0,0,0,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "target_atomic_probe_depth cannot be all zero",
        ):
            verify_regression._check_mixed_coverage(result)
        result["target_atomic_probe_depth"] = "1,1,1,1,1,1,1,1,1"
        result["probe_max_outstanding"] = 7
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "selected outstanding depth",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 38,
                "actual_ops": "10,0,3,2,0,0,66,5,5,36,48,90,12,512",
                "target_set_pressure_window": "1,1,1,1,1,1,1,1",
                "actual_set_pressure_line_state": "256,256",
                "actual_set_pressure_refill_overlap": "256,256",
                "actual_set_pressure_release_backpressure": "256,256",
                "actual_set_pressure_dual_window": "448,64",
                "actual_set_pressure_window_count": (
                    "64,64,64,64,64,64,64,64"
                ),
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 512
                ),
                "actual_set_pressure_set": "128,128,128,128",
                "actual_set_pressure_issue_order": "5472,5472",
                "actual_set_pressure_manager": (
                    "256,10944,10944,1728,1728,1728,10944,10944,1728"
                ),
                "actual_set_pressure_clean_manager": (
                    "256,10944,10944,10944,1728,1728,0,1728,0,0,"
                    "22464,22464"
                ),
                "actual_set_pressure_overlap_manager": (
                    "1152,1152,1728,1152,1152"
                ),
                "actual_set_pressure_backpressure_manager": (
                    "256,256,4096,4096,512"
                ),
                "probe_max_outstanding": 8,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_set_pressure_window_count"] = (
            "64,64,64,64,64,64,65,63"
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "set-pressure window-count/operation coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        miss_cross = []
        for depth in range(15):
            for width in range(3):
                for regime in range(3):
                    miss_cross.append(
                        1 if width + 1 <= depth + 2 and regime == 0 else 0
                    )
        result.update(
            {
                "constraint_schema": 39,
                "target_ops": "1,0,3,2,0,0,1,1,1,1,1,1,1,1,1",
                "actual_ops": "10,0,3,2,0,0,66,5,5,36,48,90,12,512,44",
                "target_miss_burst_depth": ",".join(["1"] * 15),
                "target_miss_burst_issue_width": "1,1,1",
                "actual_miss_burst_depth": ",".join(
                    ["2"] + ["3"] * 14
                ),
                "actual_miss_burst_issue_width": "15,15,14",
                "actual_miss_burst_translation": "44,0,0",
                "actual_miss_burst_cross": ",".join(
                    str(count) for count in miss_cross
                ),
                "actual_miss_burst_manager": "44,403,403,403,403,403,403",
                "actual_miss_burst_max_outstanding": 16,
                "actual_set_pressure_window_count": (
                    "64,64,64,64,64,64,64,64"
                ),
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_miss_burst_manager"] = "44,403,404,403,403,403,403"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "miss-burst manager accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_miss_burst_manager"] = (
            "44,403,403,403,403,403,403"
        )
        bank_cross = []
        for depth in range(2):
            for _bank in range(8):
                for regime in range(3):
                    bank_cross.append(1 if regime == 0 else 0)
        result.update(
            {
                "constraint_schema": 40,
                "actual_ops": "26,0,3,2,0,0,66,5,5,36,48,90,12,512,44",
                "actual_dcache_load_error": "20,6",
                "actual_dcache_load_outcome": "20,2,4",
                "target_bank_conflict": 1,
                "actual_bank_conflict_depth": "8,8",
                "actual_bank_conflict_bank": ",".join(["2"] * 8),
                "actual_bank_conflict_translation": "16,0,0",
                "actual_bank_conflict_cross": ",".join(
                    str(count) for count in bank_cross
                ),
                "actual_bank_conflict_terminal": "16,40,40,40",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_bank_conflict_terminal"] = "16,40,39,40"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "architectural terminal accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_bank_conflict_terminal"] = "16,40,40,40"
        result["actual_bank_conflict_bank"] = ",".join(
            ["1"] + ["2"] * 7
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "cross/marginal coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_bank_conflict_bank"] = ",".join(["2"] * 8)
        broken_bank_cross = list(bank_cross)
        broken_bank_cross[0] = 0
        result["actual_bank_conflict_cross"] = ",".join(
            str(count) for count in broken_bank_cross
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_bank_conflict_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_bank_conflict_cross"] = ",".join(
            str(count) for count in bank_cross
        )
        result.update(
            {
                "target_bank_conflict": 0,
                "actual_bank_conflict_depth": "0,0",
                "actual_bank_conflict_bank": ",".join(["0"] * 8),
                "actual_bank_conflict_translation": "0,0,0",
                "actual_bank_conflict_cross": ",".join(["0"] * 48),
                "actual_bank_conflict_terminal": "0,0,0,0",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_bank_conflict_depth"] = "1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "cross/marginal coverage",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_bank_conflict_depth"] = "0,0"
        result["actual_bank_conflict_cross"] = ",".join(
            ["1"] + ["0"] * 47
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_bank_conflict_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_bank_conflict_cross"] = ",".join(["0"] * 48)
        result["actual_bank_conflict_terminal"] = "0,0,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "architectural terminal accounting",
        ):
            verify_regression._check_mixed_coverage(result)
        result["constraint_schema"] = 39
        result["actual_ops"] = (
            "10,0,3,2,0,0,66,5,5,36,48,90,12,512,44"
        )
        result["actual_dcache_load_error"] = "4,6"
        result["actual_dcache_load_outcome"] = "4,2,4"
        result["actual_miss_burst_cross"] = ",".join(
            ["0"] + [str(count) for count in miss_cross[1:]]
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_miss_burst_cross",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "constraint_schema": 35,
                "target_ops": "1,0,3,2,0,0,1,1,1,1,1,1,1,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,256",
                "actual_cmo_operation": "2,2,2",
                "actual_atomic_family": "6,6,6",
                "actual_atomic_width": "9,9",
                "actual_atomic_error": "6,12",
                "actual_atomic_error_kind": "6,6",
                "actual_atomic_outcome": ",".join(["1,1,1"] * 6),
                "actual_atomic_error_manager": "12,12,24,12,12",
                "actual_cmo_line_state": "3,3",
                "actual_cmo_younger_overlap": "5,1",
                "actual_set_pressure_line_state": "128,128",
                "actual_set_pressure_refill_overlap": "128,128",
                "actual_set_pressure_release_backpressure": "128,128",
                "target_set_pressure_quad_window": 250,
                "actual_set_pressure_dual_window": "192,64",
                "actual_set_pressure_window_count": "64,64,64,64",
                "actual_set_pressure_cross": ",".join(
                    ["1,0,0"] * 256
                ),
                "actual_set_pressure_set": "64,64,64,64",
                "actual_set_pressure_issue_order": "1520,1520",
                "actual_set_pressure_manager": (
                    "128,3040,3040,480,480,480,3040,3040,480"
                ),
                "actual_set_pressure_clean_manager": (
                    "128,3040,3040,3040,480,480,0,480,0,0,6240,6240"
                ),
                "actual_set_pressure_overlap_manager": (
                    "320,320,480,320,320"
                ),
                "actual_set_pressure_backpressure_manager": (
                    "128,128,2048,2048,256"
                ),
                "target_cmo_error": 1000,
                "actual_cmo_error": "0,6",
                "actual_cmo_error_kind": "3,3",
                "actual_cmo_operation_error": "1,1,1,1,1,1",
                "probes": 160,
                "probe_max_outstanding": 8,
                "probe_source_lifecycle": "64,96,2",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["probes"] = 159
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "manager Probe count",
        ):
            verify_regression._check_mixed_coverage(result)
        result.update(
            {
                "actual_probe_sequences": 3,
                "actual_probe_caps": "1,2",
                "actual_probe_need_data": "1,2",
                "actual_probe_overlap": "1,2",
                "probe_max_outstanding": 3,
                "probes": 8,
            }
        )
        result.update(
            {
                "constraint_schema": 25,
                "actual_ops": "10,0,3,2,0,0,18,5,5,36,6,90,12,8",
                "actual_hypervisor_family": "12,12,12",
                "actual_hypervisor_spvp": "18,18",
                "actual_hypervisor_cross": "6,6,6,6,6,6",
                "actual_hypervisor_alignment": "30,6",
                "actual_hypervisor_alignment_cross": ",".join(
                    ["5,1"] * 6
                ),
                "actual_hypervisor_pbmt_pair": "12,6,6,6,6",
                "actual_hypervisor_pbmt_cross": ",".join(
                    ["2,1,1,1,1"] * 6
                ),
                "actual_hypervisor_pma_device": "30,6",
                "actual_hypervisor_pma_device_cross": ",".join(
                    ["5,1"] * 6
                ),
            }
        )
        result.update(
            {
                "constraint_schema": 14,
                "target_ops": "1,0,3,2,0,0,1,1,1,0,1",
                "actual_ops": "10,0,3,2,0,0,18,5,5,0,6",
                "target_probe": 0,
                "actual_probe_sequences": 0,
                "actual_probe_caps": "0,0",
                "actual_probe_need_data": "0,0",
                "actual_probe_overlap": "0,0",
                "probes": 6,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["target_ops"] = "0,0,3,2,0,0,0,0,0,0"
        result["actual_ops"] = "0,0,3,2,0,0,0,0,0,0"
        result["constraint_schema"] = 11
        result["probes"] = 0

        result["actual_vector_vta"] = "3,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "actual_vector_vta"
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_vector_vta"] = "2,1"
        result["actual_vector_agnostic"] = "0,1"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "mask-agnostic"
        ):
            verify_regression._check_mixed_coverage(result)
        result["actual_vector_agnostic"] = "1,1"

        result["actual_vector_lmul"] = "0,1,1,1,0,0,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "actual_vector_lmul"
        ):
            verify_regression._check_mixed_coverage(result)

        result["actual_vector_lmul"] = "0,0,1,1,1,0,0"
        result["actual_vector_shape_ops"] = 4
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "not conserved"
        ):
            verify_regression._check_mixed_coverage(result)

        result.update(
            {
                "target_ops": "0,0,0,0,0,0,0,0,0,0",
                "actual_ops": "0,0,0,0,0,0,0,0,0,0",
                "actual_vector_direction": "0,0",
                "actual_vector_addressing": "0,0,0,0",
                "actual_vector_eew": "0,0,0,0",
                "actual_vector_sew": "0,0,0,0",
                "actual_vector_lmul": "0,0,0,0,0,0,0",
                "actual_vector_emul": "0,0,0,0,0,0,0",
                "actual_vector_shape_ops": 0,
                "actual_vector_uops": 0,
                "actual_vector_multi_uop": 0,
                "actual_vector_masked": "0,0",
                "actual_vector_vma": "0,0",
                "actual_vector_vta": "0,0",
                "actual_vector_partial_vl": "0,0",
                "actual_vector_nonzero_vstart": "0,0",
                "actual_vector_agnostic": "0,0",
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["actual_vector_uops"] = 1
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "operations are disabled"
        ):
            verify_regression._check_mixed_coverage(result)

    def test_schema_41_checks_every_legal_ordinary_vector_shape(self) -> None:
        target_operations = [0, 0, 1, 1]
        vector_targets = {
            "addressing": [1] * 4,
            "eew": [1] * 4,
            "sew": [1] * 4,
            "lmul": [1] * 7,
            "emul": [1] * 7,
        }
        policy_targets = {
            "masked": 500,
            "partial_vl": 500,
            "nonzero_vstart": 500,
        }
        vector_cross: list[int] = []
        directions = [0, 0]
        actuals = {
            name: [0] * size
            for name, size in (
                ("addressing", 4),
                ("eew", 4),
                ("sew", 4),
                ("lmul", 7),
                ("emul", 7),
            )
        }
        for direction in range(2):
            for addressing in range(4):
                for eew in range(4):
                    for sew in range(4):
                        for lmul in range(7):
                            lmul_log2 = lmul - 3
                            emul_log2 = eew - sew + lmul_log2
                            legal = (
                                lmul_log2 >= sew - 3
                                and -3 <= emul_log2 <= 3
                            )
                            vector_cross.append(int(legal))
                            if legal:
                                directions[direction] += 1
                                for name, field in (
                                    ("addressing", addressing),
                                    ("eew", eew),
                                    ("sew", sew),
                                    ("lmul", lmul),
                                    ("emul", emul_log2 + 3),
                                ):
                                    actuals[name][field] += 1
        result = {"actual_vector_cross": ",".join(map(str, vector_cross))}
        verify_regression._check_vector_shape_cross(
            result,
            target_operations,
            vector_targets,
            actuals,
            policy_targets,
            directions,
            sum(vector_cross),
        )

        broken = vector_cross.copy()
        broken[broken.index(1)] = 0
        result["actual_vector_cross"] = ",".join(map(str, broken))
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "actual_vector_cross"
        ):
            verify_regression._check_vector_shape_cross(
                result,
                target_operations,
                vector_targets,
                actuals,
                policy_targets,
                directions,
                sum(vector_cross),
            )

    def test_constraint_schema_nine_checks_weighted_vector_segment_shapes(
        self,
    ) -> None:
        result = mixed_result(9)
        result.update(
            {
                "constraint_schema": 9,
                "target_translation": "1,0,0",
                "actual_translation": "7,0,0",
                "target_stage1_mode": "0,0",
                "actual_stage1_mode": "0,0",
                "target_vs_mode": "0,0",
                "actual_vs_mode": "0,0",
                "target_g_mode": "0,0",
                "actual_g_mode": "0,0",
                "actual_nested_pairs": "0,0,0,0",
                "target_fence_kind": "0,0,0",
                "target_fence_scope": "0,0",
                "target_tlb_flush": 0,
                "actual_fences": "0,0,0,0,0,0",
                "actual_translation_switch": 0,
                "actual_translation_walk_reuse": "0,0",
                "target_probe": 0,
                "target_probe_to_b": 500,
                "target_probe_need_data": 500,
                "actual_probe_sequences": 0,
                "actual_probe_caps": "0,0",
                "actual_probe_need_data": "0,0",
                "probes": 0,
                "load_wakeups": "9,7,5",
                "load_cancels": "3,2,1",
                "ifetch_prefetches": 1,
                "target_stride_stream": 0,
                "l2_stride_prefetches": 0,
                "raw_load_wakeups": "9,7,5",
                "raw_load_cancels": "3,2,1",
                "target_ops": "0,0,0,0,3,0,0,0,0,0",
                "actual_ops": "0,0,0,0,3,0,0,0,0,0",
                "target_hypervisor_family": "0,0,0",
                "actual_hypervisor_family": "0,0,0",
                "target_vector_segment_store": 500,
                "actual_vector_segment_direction": "2,1",
                "target_vector_segment_addressing": "1,0,1,0",
                "actual_vector_segment_addressing": "2,0,1,0",
                "target_vector_segment_eew": "1,0,1,0",
                "actual_vector_segment_eew": "1,0,2,0",
                "target_vector_segment_sew": "0,1,0,1",
                "actual_vector_segment_sew": "0,2,0,1",
                "target_vector_segment_lmul": "0,0,1,1,1,0,0",
                "actual_vector_segment_lmul": "0,0,1,1,1,0,0",
                "target_vector_segment_emul": "0,1,0,1,0,1,0",
                "actual_vector_segment_emul": "0,1,0,1,0,1,0",
                "target_vector_segment_nf": "1,1,0,0,0,0,0",
                "actual_vector_segment_nf": "2,1,0,0,0,0,0",
            }
        )
        verify_regression._check_mixed_coverage(result)

        result["actual_vector_segment_lmul"] = "0,0,1,2,0,0,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_vector_segment_lmul",
        ):
            verify_regression._check_mixed_coverage(result)

        result["actual_vector_segment_lmul"] = "0,0,1,1,1,0,0"
        result["actual_vector_segment_eew"] = "1,1,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_vector_segment_eew",
        ):
            verify_regression._check_mixed_coverage(result)

    def test_constraint_schema_eight_requires_vector_segment_crosses(self) -> None:
        result = mixed_result(8)
        result.update(
            {
                "constraint_schema": 8,
                "target_translation": "1,0,0",
                "actual_translation": "7,0,0",
                "target_stage1_mode": "0,0",
                "actual_stage1_mode": "0,0",
                "target_vs_mode": "0,0",
                "actual_vs_mode": "0,0",
                "target_g_mode": "0,0",
                "actual_g_mode": "0,0",
                "actual_nested_pairs": "0,0,0,0",
                "target_fence_kind": "0,0,0",
                "target_fence_scope": "0,0",
                "target_tlb_flush": 0,
                "actual_fences": "0,0,0,0,0,0",
                "actual_translation_switch": 0,
                "actual_translation_walk_reuse": "0,0",
                "target_probe": 0,
                "target_probe_to_b": 500,
                "target_probe_need_data": 500,
                "actual_probe_sequences": 0,
                "actual_probe_caps": "0,0",
                "actual_probe_need_data": "0,0",
                "probes": 0,
                "load_wakeups": "9,7,5",
                "load_cancels": "3,2,1",
                "ifetch_prefetches": 1,
                "target_stride_stream": 0,
                "l2_stride_prefetches": 0,
                "raw_load_wakeups": "9,7,5",
                "raw_load_cancels": "3,2,1",
                "target_ops": "0,0,0,0,1,0,0,0,0,0",
                "actual_ops": "0,0,0,0,7,0,0,0,0,0",
                "target_hypervisor_family": "0,0,0",
                "actual_hypervisor_family": "0,0,0",
                "target_vector_segment_store": 500,
                "actual_vector_segment_direction": "4,3",
                "actual_vector_segment_eew": "2,2,2,1",
                "actual_vector_segment_nf": "1,1,1,1,1,1,1",
            }
        )
        verify_regression._check_mixed_coverage(result)

        result["actual_vector_segment_eew"] = "3,2,2,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_vector_segment_eew",
        ):
            verify_regression._check_mixed_coverage(result)

        result["actual_vector_segment_eew"] = "2,2,2,1"
        result["actual_vector_segment_nf"] = "2,1,1,1,1,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_vector_segment_nf",
        ):
            verify_regression._check_mixed_coverage(result)

    def test_constraint_schema_seven_requires_hypervisor_families(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "constraint_schema": 7,
                "target_translation": "0,0,1",
                "actual_translation": "0,0,3",
                "target_stage1_mode": "0,0",
                "actual_stage1_mode": "0,0",
                "target_vs_mode": "1,0",
                "actual_vs_mode": "3,0",
                "target_g_mode": "1,0",
                "actual_g_mode": "3,0",
                "actual_nested_pairs": "3,0,0,0",
                "target_fence_kind": "0,0,0",
                "target_fence_scope": "0,0",
                "target_tlb_flush": 0,
                "actual_fences": "0,0,0,0,0,0",
                "actual_translation_switch": 0,
                "actual_translation_walk_reuse": "1,2",
                "target_probe": 0,
                "target_probe_to_b": 500,
                "target_probe_need_data": 500,
                "actual_probe_sequences": 0,
                "actual_probe_caps": "0,0",
                "actual_probe_need_data": "0,0",
                "probes": 0,
                "load_wakeups": "9,7,5",
                "load_cancels": "3,2,1",
                "ifetch_prefetches": 1,
                "target_stride_stream": 0,
                "l2_stride_prefetches": 0,
                "raw_load_wakeups": "9,7,5",
                "raw_load_cancels": "3,2,1",
                "target_ops": "0,0,0,0,0,0,0,0,1",
                "actual_ops": "0,0,0,0,0,0,0,0,3",
                "target_hypervisor_family": "1,1,1",
                "actual_hypervisor_family": "1,1,1",
            }
        )
        verify_regression._check_mixed_coverage(result)

        result["actual_hypervisor_family"] = "2,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError,
            "actual_hypervisor_family",
        ):
            verify_regression._check_mixed_coverage(result)

    def test_constraint_schema_four_and_five_require_new_output_crosses(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "constraint_schema": 4,
                "target_translation": "0,0,0",
                "actual_translation": "0,0,0",
                "target_stage1_mode": "0,0",
                "actual_stage1_mode": "0,0",
                "target_vs_mode": "0,0",
                "actual_vs_mode": "0,0",
                "target_g_mode": "0,0",
                "actual_g_mode": "0,0",
                "actual_nested_pairs": "0,0,0,0",
                "target_fence_kind": "0,0,0",
                "target_fence_scope": "0,0",
                "target_tlb_flush": 0,
                "actual_fences": "0,0,0,0,0,0",
                "actual_translation_switch": 0,
                "actual_translation_walk_reuse": "0,0",
                "target_probe": 0,
                "target_probe_to_b": 500,
                "target_probe_need_data": 500,
                "actual_probe_sequences": 0,
                "actual_probe_caps": "0,0",
                "actual_probe_need_data": "0,0",
                "probes": 0,
                "load_wakeups": "9,7,5",
                "load_cancels": "3,2,1",
            }
        )
        verify_regression._check_mixed_coverage(result)

        result["load_wakeups"] = "3,0,1"
        verify_regression._check_mixed_coverage(result)

        result["load_wakeups"] = "9,7,5"
        result["constraint_schema"] = 5
        result["ifetch_prefetches"] = 1
        verify_regression._check_mixed_coverage(result)

        result["ifetch_prefetches"] = 0
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "instruction-prefetch"
        ):
            verify_regression._check_mixed_coverage(result)

        result["ifetch_prefetches"] = 1
        result["constraint_schema"] = 6
        result["target_stride_stream"] = 100
        result["l2_stride_prefetches"] = 3
        result["raw_load_wakeups"] = "12,11,10"
        result["raw_load_cancels"] = "6,5,4"
        verify_regression._check_mixed_coverage(result)

        result["l2_stride_prefetches"] = 0
        verify_regression._check_mixed_coverage(result)

        result["l2_stride_prefetches"] = 3
        result["raw_load_cancels"] = "2,5,4"
        verify_regression._check_mixed_coverage(result)

    def test_constraint_schema_three_requires_probe_crosses(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "constraint_schema": 3,
                "target_translation": "0,1,0",
                "actual_translation": "0,8,0",
                "target_stage1_mode": "1,0",
                "actual_stage1_mode": "8,0",
                "target_vs_mode": "1,1",
                "actual_vs_mode": "0,0",
                "target_g_mode": "1,1",
                "actual_g_mode": "0,0",
                "actual_nested_pairs": "0,0,0,0",
                "target_fence_kind": "1,0,0",
                "target_fence_scope": "1,1",
                "target_tlb_flush": 20,
                "actual_fences": "1,1,0,0,0,0",
                "actual_translation_switch": 0,
                "actual_translation_walk_reuse": "4,4",
                "target_probe": 1,
                "target_probe_to_b": 500,
                "target_probe_need_data": 500,
                "actual_probe_sequences": 4,
                "actual_probe_caps": "2,2",
                "actual_probe_need_data": "2,2",
                "probes": 6,
            }
        )
        verify_regression._check_mixed_coverage(result)

        result["actual_probe_caps"] = "4,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "actual_probe_caps"
        ):
            verify_regression._check_mixed_coverage(result)

    def test_constraint_schema_requires_enabled_translation_crosses(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "constraint_schema": 2,
                "target_translation": "1,1,1",
                "actual_translation": "4,8,16",
                "target_stage1_mode": "1,1",
                "actual_stage1_mode": "4,4",
                "target_vs_mode": "1,1",
                "actual_vs_mode": "8,8",
                "target_g_mode": "1,1",
                "actual_g_mode": "8,8",
                "actual_nested_pairs": "4,4,4,4",
                "target_fence_kind": "1,1,1",
                "target_fence_scope": "1,1",
                "target_tlb_flush": 50,
                "actual_fences": "1,1,1,1,1,1",
                "actual_translation_switch": 6,
                "actual_translation_walk_reuse": "5,19",
            }
        )
        verify_regression._check_mixed_coverage(result)

        result["actual_nested_pairs"] = "4,4,0,4"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "actual_nested_pairs"
        ):
            verify_regression._check_mixed_coverage(result)

    def test_constraint_schema_requires_walk_and_reuse(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "constraint_schema": 2,
                "target_translation": "0,1,0",
                "actual_translation": "0,8,0",
                "target_stage1_mode": "1,0",
                "actual_stage1_mode": "8,0",
                "target_vs_mode": "1,1",
                "actual_vs_mode": "0,0",
                "target_g_mode": "1,1",
                "actual_g_mode": "0,0",
                "actual_nested_pairs": "0,0,0,0",
                "target_fence_kind": "1,0,0",
                "target_fence_scope": "1,1",
                "target_tlb_flush": 20,
                "actual_fences": "1,1,0,0,0,0",
                "actual_translation_switch": 0,
                "actual_translation_walk_reuse": "8,0",
            }
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "cold-walk or reuse"
        ):
            verify_regression._check_mixed_coverage(result)

    def test_historical_coverage_analysis_has_no_submission_minimum(self) -> None:
        self.assertEqual(
            verify_regression.ENHANCED_MIXED_COVERAGE_TRANSACTIONS, 128
        )
        self.assertFalse(
            hasattr(
                verify_regression.run_regression,
                "MINIMUM_MIXED_TRANSACTIONS",
            )
        )

    def verify(self, document: dict[str, object]) -> dict[str, object]:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "result.json"
            path.write_text(json.dumps(document, sort_keys=True), encoding="utf-8")
            return verify_regression.verify_regression(
                path,
                min_duration_seconds=4,
                min_results=2,
                expected_scenario="constrained-random",
                expected_actions=64,
                require_backpressure=True,
                require_frozen_runtime=True,
                expected_jobs=8,
                chunk_size=17,
            )

    def test_streams_and_verifies_complete_artifact(self) -> None:
        verified = self.verify(regression_document([mixed_result(7), mixed_result(8)]))
        self.assertEqual(verified["result_count"], 2)
        self.assertEqual(verified["actions"], 128)
        self.assertEqual(verified["first_seed"], 7)
        self.assertEqual(verified["last_seed"], 8)

    def test_constraint_command_must_match_recorded_configuration(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["configuration"]["constraint_profile"] = "spec"
        document["configuration"]["constraint_overrides"] = ["tlb-flush=40"]
        for result in document["results"]:
            result["command"].extend(
                ("--constraints", "spec", "--constraint", "tlb-flush=40")
            )
        self.verify(document)

        document["results"][0]["command"][-1] = "tlb-flush=400"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "command options"
        ):
            self.verify(document)

    def test_allow_finite_campaign_without_duration_deadline(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["configuration"]["duration_seconds"] = None
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "finite.json"
            path.write_text(json.dumps(document), encoding="utf-8")
            verified = verify_regression.verify_regression(
                path,
                min_duration_seconds=4,
                min_results=2,
                expected_scenario="constrained-random",
                expected_actions=64,
                require_backpressure=True,
                require_frozen_runtime=True,
                expected_jobs=8,
                allow_finite=True,
            )
            self.assertEqual(verified["result_count"], 2)


    def test_rejects_wrong_worker_count(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["configuration"]["jobs"] = 1
        with self.assertRaisesRegex(verify_regression.VerificationError, "worker count"):
            with tempfile.TemporaryDirectory() as temporary:
                path = Path(temporary) / "result.json"
                path.write_text(json.dumps(document), encoding="utf-8")
                verify_regression.verify_regression(
                    path,
                    min_duration_seconds=4,
                    min_results=2,
                    expected_scenario="constrained-random",
                    expected_actions=64,
                    expected_jobs=8,
                    require_backpressure=True,
                    require_frozen_runtime=True,
                )

    def test_rejects_running_campaign_marker(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["campaign_status"] = "running"
        with self.assertRaisesRegex(verify_regression.VerificationError, "not complete"):
            self.verify(document)

    def test_rejects_nonfinite_elapsed_time(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["elapsed_seconds"] = float("nan")
        with self.assertRaisesRegex(verify_regression.VerificationError, "not finite"):
            self.verify(document)

    def test_requires_a_result_after_duration_deadline(self) -> None:
        first = mixed_result(7)
        second = mixed_result(8)
        first["completed_offset_seconds"] = 3.0
        second["completed_offset_seconds"] = 3.9
        document = regression_document([first, second])
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "after the required duration"
        ):
            self.verify(document)

    def test_streaming_number_is_not_truncated_at_chunk_boundary(self) -> None:
        reader = verify_regression.StreamingJsonReader(io.StringIO("12345,"), chunk_size=2)
        self.assertEqual(reader.value(), 12345)
        reader.expect(",")
        reader.finish()

    def test_streaming_float_is_not_truncated_before_decimal_point(self) -> None:
        reader = verify_regression.StreamingJsonReader(io.StringIO("1234.5,"), chunk_size=5)
        self.assertEqual(reader.value(), 1234.5)
        reader.expect(",")
        reader.finish()

    def test_rejects_noncontinuous_seeds(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(9)])
        with self.assertRaisesRegex(verify_regression.VerificationError, "continuous"):
            self.verify(document)

    def test_accepts_sparse_per_seed_coverage(self) -> None:
        result = mixed_result(7)
        result["eew_load"] = "1,1,1,0"
        result["summary"] = str(result["summary"]).replace(
            "eew_load=1,1,1,1", "eew_load=1,1,1,0"
        )
        document = regression_document([result, mixed_result(8)])
        verified = self.verify(document)
        self.assertIsInstance(verified, dict)

    def test_rejects_missing_vector_store_address_mode(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
                "vec_store_modes": "1,1,1,0",
            }
        )
        with self.assertRaisesRegex(verify_regression.VerificationError, "vec_store_modes"):
            verify_regression._check_mixed_coverage(result)

    def test_frontend_bridge_coverage_checks_counts_stalls_and_fields(self) -> None:
        result: dict[str, object] = {
            "transactions": 32,
            "requests": 96,
            "responses": 128,
            "request_stalls": 1,
            "response_stalls": 1,
            "source_credit_stalls": 1,
            "field_checks": 32 * 39,
        }
        verify_regression._check_frontend_bridge_coverage(result)

        for name in ("request_stalls", "response_stalls", "source_credit_stalls"):
            invalid = dict(result)
            invalid[name] = 0
            with self.assertRaisesRegex(
                verify_regression.VerificationError, name
            ):
                verify_regression._check_frontend_bridge_coverage(invalid)

        invalid = dict(result)
        invalid["responses"] = 127
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "response count"
        ):
            verify_regression._check_frontend_bridge_coverage(invalid)

        invalid = dict(result)
        invalid["field_checks"] = 32 * 39 - 1
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "field checking"
        ):
            verify_regression._check_frontend_bridge_coverage(invalid)

    def test_rejects_missing_scalar_store_issue_order(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_order": "1,0",
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
            }
        )
        with self.assertRaisesRegex(verify_regression.VerificationError, "store_order"):
            verify_regression._check_mixed_coverage(result)

    def test_enhanced_mixed_treats_replay_as_diagnostic(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["vector_replays"] = 0
        verify_regression._check_mixed_coverage(result)
        result["vector_replays"] = -1
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "vector_replays diagnostic"
        ):
            verify_regression._check_mixed_coverage(result)

    def test_required_backpressure_must_be_observed_on_all_managers(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
            }
        )
        verify_regression._check_mixed_coverage(result, require_backpressure=True)
        result["backpressure"] = "1,1,1,1,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "uncovered class"
        ):
            verify_regression._check_mixed_coverage(
                result, require_backpressure=True
            )

    def test_mixed_requires_real_unresolved_overlap(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
                "concurrent": "4,20,0,1,1",
            }
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "unresolved overlap"
        ):
            verify_regression._check_mixed_coverage(result)


    def test_rejects_aggregate_count_mismatch(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["summary"]["actions_completed"] = 64
        with self.assertRaisesRegex(verify_regression.VerificationError, "actions_completed"):
            self.verify(document)
