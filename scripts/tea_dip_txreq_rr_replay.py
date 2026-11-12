"""Replay the six-port TXREQ TwoLevelRR/FastArbiter control on recorded masks.

The order maps arbiter positions to canonical bank0..3/MMIO/stash ports.
All model state starts at RTL reset and persists across logger clean/dump edges.
A different order on frozen arrivals is a selection experiment, not a prediction
of request latency or CPU performance: changed grants alter future traffic.
"""

BANK_ORDERS = {
    'original': (0, 1, 2, 3, 4, 5),
    'balanced01': (0, 1, 4, 2, 3, 5),
    'balanced02': (0, 2, 4, 1, 3, 5),
    'balanced03': (0, 3, 4, 1, 2, 5),
}


class TwoLevelRRReplay:
    """Match the existing six-input RTL; step returns the canonical ready mask."""

    def __init__(self, order):
        self.order = tuple(order)
        self.pending = [0, 0]
        self.rr_mask = [0, 0]
        self.select_low = False

    def step(self, valid_mask, out_ready):
        valid = [sum(bool(valid_mask & (1 << self.order[group * 3 + port])) << port
                     for port in range(3)) for group in range(2)]
        chosen = []
        for group in range(2):
            pending = self.pending[group] & self.rr_mask[group]
            next_one = pending & -pending
            chosen.append(next_one if next_one & valid[group] else valid[group] & -valid[group])
        low = self.select_low if all(valid) else bool(valid[0])
        group = 0 if low else 1
        one = chosen[group]
        if not one or not out_ready:
            return 0
        self.pending[group] = valid[group] & ~one
        self.rr_mask[group] = 7 & ~((one << 1) - 1)
        self.select_low = not bool(valid[1]) if low else bool(valid[0])
        return 1 << self.order[group * 3 + one.bit_length() - 1]
