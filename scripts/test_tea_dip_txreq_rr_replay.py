"""Directed selection sequences covering the actual RTL's stored pending mask."""
import unittest

from tea_dip_txreq_rr_replay import BANK_ORDERS, TwoLevelRRReplay


class TxreqRRReplayTests(unittest.TestCase):
    def test_original_saturated_bank_sequence(self):
        model = TwoLevelRRReplay(BANK_ORDERS['original'])
        self.assertEqual([model.step(15, True) for _ in range(12)],
                         [8, 1, 8, 2, 8, 4, 8, 1, 8, 2, 8, 4])

    def test_balanced_saturated_bank_sequence(self):
        model = TwoLevelRRReplay(BANK_ORDERS['balanced01'])
        self.assertEqual([model.step(15, True) for _ in range(12)],
                         [4, 1, 8, 2, 4, 1, 8, 2, 4, 1, 8, 2])

    def test_backpressure_and_idle_do_not_advance_state(self):
        model = TwoLevelRRReplay(BANK_ORDERS['original'])
        self.assertEqual([model.step(mask, ready) for mask, ready in
                          [(15, False), (0, True), (15, True), (15, False), (15, True)]],
                         [0, 0, 8, 0, 1])

    def test_pending_request_and_withdrawal(self):
        model = TwoLevelRRReplay(BANK_ORDERS['original'])
        self.assertEqual([model.step(mask, True) for mask in [3, 7, 1, 4, 3]],
                         [1, 2, 1, 4, 1])


if __name__ == '__main__':
    unittest.main()
