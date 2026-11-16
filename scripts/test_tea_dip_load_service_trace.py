"""Directed qualification and attribution tests for the S2 attempt consumer."""
import copy
import unittest

from tea_dip_load_service_trace import measure, METRICS


class LoadServiceTests(unittest.TestCase):
    def setUp(self):
        self.rows = [dict(core_cycle=i, lane=0, fire=fire, raw_miss=miss,
                          full_forward=full, store_full_forward=full, need_dcache=1-full,
                          entrance=entrance, instr_type=kind, pc=16, cause_mask=cause)
                     for i, fire, miss, full, entrance, kind, cause in [
                         (1, 1, 1, 0, 32, 1, 64),
                         (2, 0, 1, 0, 32, 1, 64),
                         (3, 1, 0, 1, 4, 1, 256 | 16),
                         (4, 1, 1, 0, 64, 4, 64)]]
        self.boundaries = [dict(core_cycle=0, clean=1, native_dump=0),
                           dict(core_cycle=4, clean=0, native_dump=1)]
        actual = [4, 3, 2, 2, 2, 1, 1]
        self.blocks = {7: {f'core.LoadUnit_{lane}.s2.{name}': [actual[i] if lane == 0 else 0]
                          for lane in range(3) for i, name in enumerate(METRICS)}}

    def test_qualified_attempts_preserve_overlapping_causes(self):
        row = measure(self.rows, self.boundaries, self.blocks)
        pc = row['phases'][0]['scalar_PC_attempts']['0x10']
        self.assertEqual(row['core_to_native_offset'], 3)
        self.assertEqual((pc['attempts'], pc['raw_miss'], pc['full_forward']), (2, 1, 1))
        self.assertEqual((pc['cause_C_DM'], pc['cause_C_BC'], pc['cause_C_FF']), (1, 1, 1))
        self.assertEqual(row['WB_identity'], 'not_captured')

    def test_one_pass_iterator_covers_warmup_and_ROI(self):
        rows = self.rows + [dict(row, core_cycle=row['core_cycle'] + 4) for row in self.rows]
        boundaries = [self.boundaries[0], dict(core_cycle=4, clean=1, native_dump=1),
                      dict(core_cycle=8, clean=0, native_dump=1)]
        result = measure(iter(rows), boundaries, {7: self.blocks[7], 11: self.blocks[7]})
        self.assertEqual(result['events'], 8)
        self.assertEqual([phase['events'] for phase in result['phases']], [4, 4])
        self.assertEqual(result['phases'][0]['scalar_PC_attempts'], result['phases'][1]['scalar_PC_attempts'])

    def test_wrong_dump_phase_rejected_by_native(self):
        boundaries = copy.deepcopy(self.boundaries)
        boundaries[-1]['core_cycle'] -= 1
        with self.assertRaises(AssertionError):
            measure(self.rows, boundaries, self.blocks)

    def test_ambiguous_native_value_is_not_silently_selected(self):
        blocks = copy.deepcopy(self.blocks)
        blocks[7]['core.LoadUnit_0.s2.fire'].append(3)
        with self.assertRaises(ValueError):
            measure(self.rows, self.boundaries, blocks)


if __name__ == '__main__':
    unittest.main()
