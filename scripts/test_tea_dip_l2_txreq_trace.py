"""Event/endpoint regressions; actual GSIM/SQLite producer ABI needs separate calibration."""
from pathlib import Path
import sqlite3
import tempfile
import unittest

from tea_dip_l2_txreq_trace import analyze, reconstruct


def row(cycle, valid=0, ready=0, out_ready=False, dump=False, clean=False):
    return dict(NATIVECYCLE=cycle, EVENTS_VALIDMASK=valid, EVENTS_READYMASK=ready,
                EVENTS_OUTREADY=out_ready, NATIVEDUMP=dump, LOGENABLE=True, CLEAN=clean)


class TxreqTraceTests(unittest.TestCase):
    def test_port_service_is_after_fifo_and_dump_handshake_is_separate(self):
        rows = [row(1, 3), row(2, 3, 1, True), row(3, 2, 2, True, dump=True)]
        result = reconstruct(rows)[3]
        self.assertEqual(result['active_input_multiplicity'][2], 2)
        self.assertEqual(result['ports']['bank0'], dict(active_cycles=2, accepted_requests=1,
                         ready_qualified_wait_cycles=0, output_not_ready_cycles=1))
        self.assertEqual(result['ports']['bank1'], dict(active_cycles=2, accepted_requests=0,
                         ready_qualified_wait_cycles=1, output_not_ready_cycles=1))
        self.assertEqual(result['native_next_accepted_wait_buckets'][0], 1)
        self.assertEqual(result['native_next_accepted_wait_buckets'][1], 1)

    def test_wait_timer_crosses_clean_while_native_totals_reset(self):
        result = reconstruct([row(1, 3, 1, True), row(2, 3, 1, True, dump=True, clean=True),
                              row(3, 2, 2, True, dump=True)])[3]
        self.assertEqual(sum(result['active_input_multiplicity'].values()), 0)
        self.assertEqual(result['ports']['bank1']['accepted_requests'], 0)
        self.assertEqual(result['native_next_accepted_wait_buckets'][2], 1)

    def test_idle_gap_clears_wait_and_mmio_stash_are_canonical_ports(self):
        result = reconstruct([row(1, 48, 16, True), row(5, 32, 32, True, dump=True)])[5]
        self.assertEqual(result['ports']['MMIO']['accepted_requests'], 1)
        self.assertEqual(result['ports']['stash']['ready_qualified_wait_cycles'], 1)
        self.assertEqual(result['native_next_accepted_wait_buckets'][0], 2)
        self.assertEqual(result['native_next_accepted_wait_buckets'][1], 0)

    def test_multiple_or_missing_handshakes_are_rejected(self):
        for event in [row(1, 3, 3, True), row(1, 3, 0, True), row(1, 1, 1, False)]:
            with self.assertRaises(AssertionError):
                reconstruct([event])

    def test_sqlite_native_join_detects_dropped_active_cycles(self):
        rows = [row(1, 3), row(2, 3, 1, True), row(3, 2, 2, True, dump=True)]
        prefix = 'txreq_arb'
        values = {prefix + 'ArbConflict_sampled': 2, prefix + 'ArbConflict_sum': 4,
                  prefix + 'ArbConflict_underflow': 0, prefix + 'ArbConflict_overflow': 0}
        values.update({f'{prefix}ArbConflict_{i}_{i + 1}': int(i == 2) * 2 for i in range(1, 7)})
        values.update({prefix + 'ArbWaitTime_' + str(i): int(i in [0, 1]) for i in range(13)})
        with tempfile.TemporaryDirectory() as directory:
            db, log = Path(directory) / 'trace.db', Path(directory) / 'native.txt'
            with sqlite3.connect(db) as connection:
                columns = list(rows[0])
                connection.execute('CREATE TABLE L2TxreqArbTrace (' +
                                   ','.join(name + ' INT' for name in columns) + ')')
                connection.executemany('INSERT INTO L2TxreqArbTrace VALUES (' +
                                       ','.join('?' for _ in columns) + ')', [list(r.values()) for r in rows])
            log.write_text(''.join(f'[PERF ][time=3] Top.l2cache: {k}, {v}\n' for k, v in values.items()))
            self.assertTrue(analyze(db, log)['passed'])
            with sqlite3.connect(db) as connection:
                connection.execute('DELETE FROM L2TxreqArbTrace WHERE NATIVECYCLE=1')
            with self.assertRaises(AssertionError):
                analyze(db, log)


if __name__ == '__main__':
    unittest.main()
