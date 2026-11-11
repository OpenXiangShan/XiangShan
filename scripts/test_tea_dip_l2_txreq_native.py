"""Boundary and backpressure regressions for the existing native-counter ABI."""
import unittest

from tea_dip_l2_txreq_native import summarize

PREFIX = 'SimTop.cpu.l_soc.core_with_l2.l2top.inner.l2cache.txreq_arb'


def counters(multiplicity, wait):
    values = {PREFIX + f'ArbConflict_{i}_{i + 1}': multiplicity.get(i, 0) for i in range(1, 7)}
    values.update({PREFIX + 'ArbConflict_sampled': sum(multiplicity.values()),
                   PREFIX + 'ArbConflict_sum': sum(i * n for i, n in multiplicity.items()),
                   PREFIX + 'ArbConflict_underflow': 0, PREFIX + 'ArbConflict_overflow': 0})
    values.update({PREFIX + 'ArbWaitTime_' + str(i): wait.get(i, 0) for i in range(13)})
    return values


class TxreqTests(unittest.TestCase):
    def test_zero_ready_wait_does_not_hide_downstream_backpressure(self):
        # One request waits through three blocked cycles, then fires on dump.
        # ArbPerf never increments its ready-qualified wait timer in this case.
        result = summarize(counters({1: 3}, {0: 1}), 10)
        self.assertEqual(result['accepted_ready_wait_sum_lower_bound'], 0)
        self.assertEqual(result['output_not_ready_cycle_bounds'], {'lower': 2, 'upper': 3})
        self.assertEqual(result['unaccepted_input_request_cycle_bounds'], {'lower': 2, 'upper': 3})

    def test_competing_requests_and_output_cycles_have_different_units(self):
        # Two requests on the first blocked cycle, then two successive accepts.
        result = summarize(counters({2: 2, 1: 1}, {0: 1, 1: 1}), 10)
        self.assertEqual(result['output_not_ready_cycle_bounds'], {'lower': 1, 'upper': 2})
        self.assertEqual(result['unaccepted_input_request_cycle_bounds'], {'lower': 3, 'upper': 4})
        self.assertEqual(result['excess_valid_input_request_cycles'], 2)

    def test_dump_handshake_after_an_idle_histogram_is_not_negative_waiting(self):
        result = summarize(counters({}, {0: 1}), 10)
        self.assertEqual(result['histogram_window_accepted_request_bounds'], {'lower': 0, 'upper': 0})
        self.assertEqual(result['output_not_ready_cycle_bounds'], {'lower': 0, 'upper': 0})

    def test_multiplicity_does_not_claim_which_banks_contend(self):
        result = summarize(counters({1: 1, 3: 2, 4: 1}, {0: 4}), 10)
        self.assertEqual(result['three_or_more_active_input_cycles'], 3)
        self.assertEqual(result['four_or_more_active_input_cycles'], 1)
        self.assertEqual(result['active_input_multiplicity'], {1: 1, 2: 0, 3: 2, 4: 1, 5: 0, 6: 0})

    def test_absent_or_inconsistent_counters_do_not_become_zero(self):
        self.assertEqual(summarize({}, 10)['status'], 'unavailable')
        values = counters({1: 2}, {0: 1})
        del values[PREFIX + 'ArbWaitTime_3']
        with self.assertRaises(KeyError):
            summarize(values, 10)
        values = counters({1: 2}, {0: 1})
        values[PREFIX + 'ArbConflict_sum'] = 3
        with self.assertRaises(AssertionError):
            summarize(values, 10)
        with self.assertRaises(AssertionError):
            summarize(counters({1: 1}, {0: 3}), 10)


if __name__ == '__main__':
    unittest.main()
