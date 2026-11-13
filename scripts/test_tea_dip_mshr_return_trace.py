"""Generation-reuse and forced-boundary regressions for the actual join helpers."""
import unittest

from tea_dip_mshr_return_trace import coalesce, generations, partition_wait


class MSHRReturnTests(unittest.TestCase):
    def owner(self, generation, begin, end, first, last):
        return dict(slot=0, generation=generation, begin=begin, end=end, line=0x1000,
                    allocation_observed=True, acquire_recorded=begin,
                    first_data=first, last_data=last, hint_recorded=None)

    def test_continuous_wait_does_not_rebind_same_line_reused_slot(self):
        owners={0:[self.owner(1,1,8,4,5),self.owner(2,8,20,10,11)]}
        counts,binding=partition_wait(2,15,dict(handled=1,mshr_id=0),15,owners,0x1000)
        self.assertEqual(counts,dict(before_recorded_acquire=0,acquired_before_first_data=2,
                                    between_first_and_last_data=1,at_or_after_last_data=3,unknown=7))
        self.assertEqual(binding['generation'],1)
        self.assertEqual(binding['unknown_reason'],'owner_or_tag_ended_during_continuous_wait')
        fresh,_=partition_wait(9,15,dict(handled=1,mshr_id=0),15,owners,0x1000)
        self.assertEqual(fresh['unknown'],0)
        self.assertEqual(fresh['at_or_after_last_data'],4)

    def test_stale_unhandled_tag_is_unknown(self):
        owners={0:[self.owner(1,1,20,4,5)]}
        counts,binding=partition_wait(2,15,dict(handled=0,mshr_id=0),15,owners,0x1000)
        self.assertEqual(counts['unknown'],13)
        self.assertEqual(binding['unknown_reason'],'no_qualified_tag')

    def test_forced_snapshot_does_not_split_wait_or_tag(self):
        row=dict(entry=3,handled=1,mshr_id=0)
        spans=[(2,8,dict(row,core_cycle=2)),(8,15,dict(row,core_cycle=8))]
        merged=coalesce(spans,'entry',['handled','mshr_id'])
        self.assertEqual([(a,b)for a,b,_ in merged],[(2,15)])

    def test_return_flag_without_real_grant_is_rejected(self):
        rows=[]
        for slot in range(16):
            rows.append(dict(core_cycle=0,kind=1,mshr=slot,valid=0,line=0,source=0,
                             primary_source=0,acquired=0,first=0,last=0,hint=0,has_data=0,
                             grant_has_data=0,grant_complete=0))
        rows.append(dict(rows[0],core_cycle=1,valid=1,line=0x1000,acquired=1))
        rows.append(dict(rows[-1],core_cycle=3,first=1,last=1,has_data=1))
        with self.assertRaisesRegex(AssertionError,'Return flag has no preceding grant'):
            generations(rows,5)


if __name__=='__main__':
    unittest.main()
