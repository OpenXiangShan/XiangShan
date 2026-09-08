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


RTL_HASH = "a" * 64


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
        "rtl_sha256": RTL_HASH,
    }
    summary = "MEMBLOCK_RANDOM_MIXED_PASS " + " ".join(
        f"{name}={value}" for name, value in fields.items()
    )
    return {
        **fields,
        "status": "pass",
        "scenario": "random-mixed",
        "returncode": 0,
        "output": "",
        "elapsed_seconds": 0.1,
        "submitted_offset_seconds": float(seed - 7) / 10,
        "completed_offset_seconds": 4.5 + float(seed - 7) / 2,
        "command": [
            "/frozen/memblock_sim",
            "--test",
            "random-mixed",
            "--seed",
            str(seed),
            "--transactions",
            "64",
        ],
        "summary": summary,
    }


def regression_document(results: list[dict[str, object]]) -> dict[str, object]:
    runtime_hashes = {
        "binary": "b" * 64,
        "model": "c" * 64,
        "rtl_metadata": "2" * 64,
        "xspcomm": "d" * 64,
    }
    external_hashes = {"/lib/system.so": "e" * 64}
    controller_hashes = {"runner": "f" * 64, "rtl_metadata": "1" * 64}
    return {
        "schema_version": 2,
        "campaign_status": "complete",
        "run_id": "0123456789abcdef0123456789abcdef",
        "started_at": "2026-01-01T00:00:00+00:00",
        "finished_at": "2026-01-01T00:00:05+00:00",
        "elapsed_seconds": 5.0,
        "binary": "/frozen/memblock_sim",
        "binary_sha256": runtime_hashes["binary"],
        "complete_rtl_sha256": RTL_HASH,
        "configuration": {
            "backpressure": True,
            "duration_seconds": 4.0,
            "jobs": 8,
            "mixed_transactions_per_seed": 64,
            "scenarios": ["random-mixed"],
            "start_seed": 7,
        },
        "controller": {
            "hashes_before": controller_hashes,
            "hashes_after": controller_hashes,
            "unchanged": True,
            "error": None,
        },
        "runtime": {
            "metadata_sha256_before": "2" * 64,
            "metadata_sha256_after": "2" * 64,
            "artifact_hashes_before": runtime_hashes,
            "artifact_hashes_after": runtime_hashes,
            "external_dependency_hashes_before": external_hashes,
            "external_dependency_hashes_after": external_hashes,
            "unchanged": True,
            "error": None,
        },
        "summary": {
            "seeds_completed": len(results),
            "statuses": {"pass": len(results)},
            "transactions_completed": 64 * len(results),
            "rtl_sha256": [RTL_HASH],
            "rtl_hash_consistent": True,
            "runtime_unchanged": True,
            "controller_unchanged": True,
        },
        "results": results,
    }


class VerifyRegressionTest(unittest.TestCase):
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
            {"random-mixed": 64},
            {"random-mixed": 64},
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
                {"random-mixed": 64},
                {"random-mixed": 64},
            )

    def test_unknown_constraint_schema_is_rejected(self) -> None:
        result = mixed_result(7)
        result["constraint_schema"] = 23
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "unsupported constraint_schema"
        ):
            verify_regression._check_mixed_coverage(result)

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
        result["actual_load_merge_manager"] = "12,14,13,30"
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

        result["load_wakeups"] = "3,2,1"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "canceled and uncanceled"
        ):
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
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "stride stream"
        ):
            verify_regression._check_mixed_coverage(result)

        result["l2_stride_prefetches"] = 3
        result["raw_load_cancels"] = "2,5,4"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "cannot be smaller"
        ):
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

    def test_historical_coverage_gate_is_independent_of_submission_minimum(self) -> None:
        self.assertEqual(
            verify_regression.ENHANCED_MIXED_COVERAGE_TRANSACTIONS, 128
        )
        self.assertLess(
            verify_regression.ENHANCED_MIXED_COVERAGE_TRANSACTIONS,
            verify_regression.run_regression.MINIMUM_MIXED_TRANSACTIONS,
        )

    def verify(self, document: dict[str, object]) -> dict[str, object]:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "result.json"
            path.write_text(json.dumps(document, sort_keys=True), encoding="utf-8")
            return verify_regression.verify_regression(
                path,
                min_duration_seconds=4,
                min_results=2,
                expected_scenario="random-mixed",
                expected_transactions=64,
                expected_rtl_sha256=RTL_HASH,
                require_backpressure=True,
                require_frozen_runtime=True,
                expected_jobs=8,
                chunk_size=17,
            )

    def test_streams_and_verifies_complete_artifact(self) -> None:
        verified = self.verify(regression_document([mixed_result(7), mixed_result(8)]))
        self.assertEqual(verified["result_count"], 2)
        self.assertEqual(verified["transactions"], 128)
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
                expected_scenario="random-mixed",
                expected_transactions=64,
                expected_rtl_sha256=RTL_HASH,
                require_backpressure=True,
                require_frozen_runtime=True,
                expected_jobs=8,
                allow_finite=True,
            )
            self.assertEqual(verified["result_count"], 2)

    def test_controller_file_hash_is_checked_when_requested(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "source.cpp"
            source.write_text("source", encoding="utf-8")
            document = regression_document([mixed_result(7), mixed_result(8)])
            document["controller"]["paths"] = {"source": str(source)}
            document["controller"]["hashes_before"] = {
                **document["controller"]["hashes_before"],
                "source": verify_regression.run_regression.sha256(source),
            }
            document["controller"]["hashes_after"] = document["controller"]["hashes_before"]
            with tempfile.NamedTemporaryFile(suffix=".json") as artifact:
                artifact_path = Path(artifact.name)
                artifact_path.write_text(json.dumps(document), encoding="utf-8")
                verified = verify_regression.verify_regression(
                    artifact_path,
                    min_duration_seconds=4,
                    min_results=2,
                    expected_scenario="random-mixed",
                    expected_transactions=64,
                    expected_rtl_sha256=RTL_HASH,
                    require_backpressure=True,
                    require_frozen_runtime=True,
                    runner=None,
                    controller_files=(source,),
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
                    expected_scenario="random-mixed",
                    expected_transactions=64,
                    expected_rtl_sha256=RTL_HASH,
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

    def test_rejects_missing_per_seed_coverage(self) -> None:
        result = mixed_result(7)
        result["eew_load"] = "1,1,1,0"
        result["summary"] = str(result["summary"]).replace(
            "eew_load=1,1,1,1", "eew_load=1,1,1,0"
        )
        document = regression_document([result, mixed_result(8)])
        with self.assertRaisesRegex(verify_regression.VerificationError, "uncovered class"):
            self.verify(document)

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

    def test_stress_coverage_requires_combinations_and_outstanding_depth(self) -> None:
        result: dict[str, object] = {
            "stress_load_ops": "1,1,1,1,1,1,1",
            "stress_store_ops": "1,1,1,1",
            "stress_load_lanes": "1,1,1",
            "stress_address_lanes": "1,1",
            "stress_data_lanes": "1,1",
            "stress_store_order": "1,1",
            "stress_eew_load": "1,1,1,1",
            "stress_eew_store": "1,1,1,1",
            "stress_vec_load_modes": "1,1,1,0",
            "stress_vec_store_modes": "1,1,1,0",
            "stress_vec_lanes": "1,1",
            "stress_prefetch": "1,1,1",
            "stress_vstart": "1,1",
            "stress_vl": "1,1",
            "stress_alignment": "1,1",
            "stress_forwarding": "1,1",
            "stress_dcache": "1,1",
            "stress_combinations": "1,1,1,1",
            "stress_masked": 1,
            "stress_unmasked": 1,
            "stress_misaligned": 1,
            "stress_waves": 4,
            "stress_regions": 2,
            "stress_max_outstanding": 10,
            "stress_actions": 96,
            "transactions": 96,
            "stress_backpressure": "1,1,0,0,0,0",
        }
        verify_regression._check_stress_coverage(result, require_backpressure=True)
        result["stress_combinations"] = "1,1,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "stress_combinations"
        ):
            verify_regression._check_stress_coverage(result)

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

    def test_enhanced_mixed_requires_replay_virtualization_and_exceptions(self) -> None:
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
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "vector_replays"
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

    def test_rejects_runtime_hash_change(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["runtime"]["artifact_hashes_after"] = {"binary": "9" * 64}
        with self.assertRaisesRegex(verify_regression.VerificationError, "changed"):
            self.verify(document)

    def test_rejects_aggregate_count_mismatch(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["summary"]["transactions_completed"] = 64
        with self.assertRaisesRegex(verify_regression.VerificationError, "transactions_completed"):
            self.verify(document)

    def test_forwarding_command_request_and_bounded_result_are_distinct(self) -> None:
        result = {
            "seed": 7,
            "scenario": "random-forwarding",
            "transactions": 48,
            "rtl_sha256": RTL_HASH,
            "status": "pass",
            "returncode": 0,
            "output": "",
            "elapsed_seconds": 0.1,
            "submitted_offset_seconds": 0.0,
            "completed_offset_seconds": 5.0,
            "command": [
                "/frozen/memblock_sim",
                "--test",
                "random-forwarding",
                "--seed",
                "7",
                "--transactions",
                "64",
            ],
            "summary": (
                "MEMBLOCK_RANDOM_FORWARD_PASS seed=7 transactions=48 "
                f"rtl_sha256={RTL_HASH}"
            ),
        }
        checked = verify_regression._check_result(
            result,
            0,
            {"random-forwarding": 64},
            {"random-forwarding": 48},
        )
        self.assertEqual(checked[3], 48)

        result["transactions"] = 47
        result["summary"] = str(result["summary"]).replace(
            "transactions=48", "transactions=47"
        )
        with self.assertRaisesRegex(verify_regression.VerificationError, "expected 48"):
            verify_regression._check_result(
                result,
                0,
                {"random-forwarding": 64},
                {"random-forwarding": 48},
            )

    def test_forwarding_result_caps(self) -> None:
        self.assertEqual(
            verify_regression.completed_transaction_count("random-forwarding", 64),
            48,
        )
        self.assertEqual(
            verify_regression.completed_transaction_count(
                "random-vector-forwarding", 64
            ),
            24,
        )
        self.assertEqual(
            verify_regression.completed_transaction_count("random-loads", 1000),
            1000,
        )


if __name__ == "__main__":
    unittest.main()
