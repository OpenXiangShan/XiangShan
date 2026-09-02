#!/usr/bin/env python3

from __future__ import annotations

import re
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = MEMBLOCK_ROOT.parents[1]


class MemBlockEnvironmentContractTest(unittest.TestCase):
    def test_queue_capacity_constants_match_xiangshan_parameters(self) -> None:
        parameters = (
            REPO_ROOT / "src/main/scala/xiangshan/Parameters.scala"
        ).read_text()
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for scala_name, cpp_name in (
            ("VirtualLoadQueueSize", "kVirtualLoadQueueEntries"),
            ("StoreQueueSize", "kStoreQueueEntries"),
            ("RobSize", "kRobEntries"),
        ):
            scala_match = re.search(rf"{scala_name}: Int = (\d+)", parameters)
            cpp_match = re.search(rf"{cpp_name} = (\d+)", environment)
            self.assertIsNotNone(scala_match, scala_name)
            self.assertIsNotNone(cpp_match, cpp_name)
            self.assertEqual(int(cpp_match.group(1)), int(scala_match.group(1)))

        config = (
            MEMBLOCK_ROOT / "config/memblock.json"
        ).read_text()
        configured_store_entries = re.search(
            r'"queue\.store_entries"\s*:\s*(\d+)', config
        )
        scala_store_entries = re.search(r"StoreQueueSize: Int = (\d+)", parameters)
        self.assertIsNotNone(configured_store_entries)
        self.assertIsNotNone(scala_store_entries)
        self.assertEqual(
            int(configured_store_entries.group(1)),
            int(scala_store_entries.group(1)),
        )

    def test_vector_fu_type_constants_match_scala_one_hot_order(self) -> None:
        fu_type = (
            REPO_ROOT / "src/main/scala/xiangshan/backend/fu/FuType.scala"
        ).read_text()
        names = re.findall(r'val\s+(\w+)\s*=\s*addType\(name\s*=\s*"[^"]+"\)', fu_type)
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for scala_name, cpp_name in (
            ("vldu", "kFuTypeVectorLoad"),
            ("vstu", "kFuTypeVectorStore"),
        ):
            match = re.search(
                rf"{cpp_name}\s*=\s*std::uint64_t\{{1\}}\s*<<\s*(\d+)",
                environment,
            )
            self.assertIsNotNone(match, cpp_name)
            self.assertEqual(int(match.group(1)), names.index(scala_name))

    def test_software_prefetch_encodings_match_lsu_op_type(self) -> None:
        package = (REPO_ROOT / "src/main/scala/xiangshan/package.scala").read_text()
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for scala_name, cpp_name in (
            ("prefetch_i", "instruction"),
            ("prefetch_r", "read"),
            ("prefetch_w", "write"),
        ):
            scala_match = re.search(rf"def\s+{scala_name}\s*=\s*\"b([01]+)\"", package)
            cpp_match = re.search(rf"{cpp_name}\s*=\s*0x([0-9a-f]+)", environment)
            self.assertIsNotNone(scala_match, scala_name)
            self.assertIsNotNone(cpp_match, cpp_name)
            self.assertEqual(
                int(scala_match.group(1), 2), int(cpp_match.group(1), 16)
            )

    def test_mixed_environment_has_combined_drain_and_queue_accounting(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()

        for contract in (
            "run_until_all_complete",
            "run_until_queues_retired",
            "account_lq_cancellation",
            "commit_vector_store",
            "expect_prefetch",
        ):
            self.assertIn(contract, environment)
        for gate in (
            "simultaneous_scalar_vector",
            "scalar_to_vector",
            "vector_to_scalar",
            "dirty_pressure",
            "redirect_recovery",
            "prefetch_ops",
            "backpressure_complete",
        ):
            self.assertIn(gate, main)

        for contract in (
            "dcache_request_stalls",
            "dcache_response_delays",
            "ptw_request_stalls",
            "ptw_response_delays",
            "uncache_request_stalls",
            "uncache_response_delays",
        ):
            self.assertIn(contract, environment)

    def test_scoreboards_reject_duplicate_identity_and_store_halves(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()

        for diagnostic in (
            "duplicate outstanding scalar load ROB value",
            "duplicate outstanding scalar store ROB value",
            "duplicate outstanding vector memory ROB value",
            "duplicate store-address writeback",
            "duplicate store-data writeback",
        ):
            self.assertIn(diagnostic, environment)

    def test_store_writeback_is_gated_by_issue_handshake_epoch(self) -> None:
        environment = (MEMBLOCK_ROOT / "cpp/memblock_env.hpp").read_text()
        for contract in (
            "address_issued",
            "data_issued",
            "address_issue_cycle",
            "data_issue_cycle",
            "sample_cycle < it->second.address_issue_cycle",
            "sample_cycle < it->second.data_issue_cycle",
            "mark_address_issued",
            "mark_data_issued",
        ):
            self.assertIn(contract, environment)

    def test_mixed_commit_boundary_does_not_auto_commit_next_rob(self) -> None:
        main = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text()
        self.assertIn("rob_offset - 1", main)
        self.assertIn("Keep the commit boundary at the last uop", main)


if __name__ == "__main__":
    unittest.main()
