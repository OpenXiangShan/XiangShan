"""Check overlapping source partitions and rejection of a wrong same-S2 condition."""
import unittest

from tea_dip_load_cause_details import decompose


class CauseDetailsTest(unittest.TestCase):
    def test_overlapping_sources_remain_one_cause(self):
        row = dict(trouble_maker=1, tlb_access_result=4, special_request=1, need_dcache=1,
                   read_read_conflict=1, read_write_conflict=0, readline_conflict=1,
                   unaligned_replay=1, cause_mask=(1 << 3) | (1 << 8))
        self.assertEqual(decompose(row), (5, True, True, True))
        row.update(trouble_maker=0, cause_mask=1 << 3)
        self.assertEqual(decompose(row), (0, False, False, True))

    def test_wrong_bank_or_translation_source_is_rejected(self):
        row = dict(trouble_maker=1, tlb_access_result=2, special_request=0, need_dcache=1,
                   read_read_conflict=0, read_write_conflict=0, readline_conflict=0,
                   unaligned_replay=0, cause_mask=1 << 8)
        with self.assertRaisesRegex(AssertionError, 'C_BC'):
            decompose(row)
        row.update(cause_mask=1 << 3)
        with self.assertRaisesRegex(AssertionError, 'C_TM'):
            decompose(row)


if __name__ == '__main__':
    unittest.main()
