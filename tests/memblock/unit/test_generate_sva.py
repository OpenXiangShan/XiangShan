#!/usr/bin/env python3

from __future__ import annotations

import sys
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import generate_sva  # noqa: E402


def port(name, direction, protocol, interface, role, width=1, channel=None):
    return {
        "name": name,
        "direction": direction,
        "protocol": protocol,
        "interface": interface,
        "channel": channel,
        "role": role,
        "width": width,
    }


class GenerateSvaTest(unittest.TestCase):
    def test_emits_only_dut_producers_with_backpressure(self) -> None:
        manifest = {
            "rtl_sha256": "abc",
            "dimensions": {"queue": {"store_entries": 4}},
            "ports": [
                port("out_valid", "output", "decoupled", "out", "valid"),
                port("out_ready", "input", "decoupled", "out", "ready"),
                port("out_bits_data", "output", "decoupled", "out", "payload", 64),
                port("in_valid", "input", "decoupled", "in", "valid"),
                port("in_ready", "output", "decoupled", "in", "ready"),
                port("in_bits_data", "input", "decoupled", "in", "payload", 64),
            ],
        }
        producers = generate_sva.producer_interfaces(manifest)
        self.assertEqual(1, len(producers))
        self.assertEqual("out", producers[0]["name"])
        self.assertEqual(64, producers[0]["width"])
        rendered = generate_sva.render(manifest)
        self.assertIn("bind MemBlock", rendered)
        self.assertIn(".valid(out_valid)", rendered)
        self.assertNotIn(".valid(in_valid)", rendered)
        self.assertEqual(4, rendered.count("bind StoreQueue"))
        self.assertIn(".lane0_miss(io_storeAddrIn_0_bits_miss)", rendered)
        self.assertIn(".lane1_miss(io_storeAddrIn_1_bits_miss)", rendered)
        self.assertIn("check_next <= allocated && addr_valid", rendered)
        self.assertIn("stored_rob_value != previous_rob_value", rendered)
        self.assertIn(".allocated(allocated_3)", rendered)
        self.assertIn(".addr_valid(addrvalid_3)", rendered)
        self.assertIn(".stored_rob_flag(uop_3_robIdx_flag)", rendered)
        self.assertIn(".stored_uop_idx(uop_3_uopIdx)", rendered)

    def test_requires_checked_store_queue_dimension(self) -> None:
        with self.assertRaisesRegex(
            generate_sva.SvaGenerationError, "queue.store_entries"
        ):
            generate_sva.render({"rtl_sha256": "abc", "ports": []})

    def test_rejects_unknown_payload_width(self) -> None:
        manifest = {
            "ports": [
                port("out_valid", "output", "decoupled", "out", "valid"),
                port("out_ready", "input", "decoupled", "out", "ready"),
                port("out_bits_data", "output", "decoupled", "out", "payload", None),
            ]
        }
        with self.assertRaises(generate_sva.SvaGenerationError):
            generate_sva.producer_interfaces(manifest)


if __name__ == "__main__":
    unittest.main()
