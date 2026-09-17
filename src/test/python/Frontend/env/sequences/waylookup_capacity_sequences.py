"""Observe legal WayLookup traffic with phase-specific capacity diagnostics."""

from collections import deque
from dataclasses import asdict

from ..support.bpu_v3_contract import sample_prefetch_depth_cycle


class WayLookupCapacitySequence:
    def __init__(self, env, *, reader=None):
        self.env = env
        if reader is None:
            recorder = env.functional_coverage
            if recorder is None:
                raise ValueError("WayLookup sequence requires an explicit reader or DUT recorder")
            reader = lambda name: recorder._try_read_dut_signal(env.dut, name)
        self.reader = reader
        self.history = deque(maxlen=32)
        self.max_occupancy = 0

    def sample(self):
        snapshot = sample_prefetch_depth_cycle(self.reader)
        if not 0 <= snapshot.num_valid_entries <= 32:
            raise AssertionError("WayLookup occupancy outside 0..32")
        self.max_occupancy = max(self.max_occupancy, snapshot.num_valid_entries)
        self.history.append({"cycle": int(self.env.current_cycle), **asdict(snapshot)})
        return snapshot

    def wait(self, predicate, *, phase, max_cycles):
        for elapsed in range(int(max_cycles) + 1):
            snapshot = self.sample()
            if predicate(snapshot):
                return snapshot
            if elapsed < int(max_cycles):
                self.env.step(1)
        raise AssertionError({"phase": phase, "max_occupancy": self.max_occupancy,
                              "tail": list(self.history), "max_cycles": max_cycles})

    def wait_full(self, *, max_cycles=4096):
        return self.wait(lambda s: s.full, phase="fill_to_32", max_cycles=max_cycles)

    def wait_one_slot_blocks_write(self, *, max_cycles=4096):
        # V3 reserves TWO slots even when write1_valid is low.
        return self.wait(lambda s: s.one_slot_left and s.write0_valid and not s.shared_write_ready,
                         phase="31_blocks_single_or_dual", max_cycles=max_cycles)

    def run_boundary_driver(self, driver):
        """Execute against a module-interface driver, without touching queue state.

        Driver methods: reset(), write(count), read(count), occupancy(), ready().
        write/read each perform a real clocked handshake, or assert on failure.
        """
        driver.reset()
        for _ in range(15):
            driver.write(2)
        assert driver.occupancy() == 30
        driver.write(1)
        assert driver.occupancy() == 31 and not driver.ready()
        driver.read(1)
        assert driver.occupancy() == 30 and driver.ready()
        driver.write(2)
        assert driver.occupancy() == 32 and not driver.ready()
