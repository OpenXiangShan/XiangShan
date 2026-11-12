"""Test actual request-attempt distinctions that a single WB latency hides."""
import unittest
from tea_dip_load_s1_association import associate


class AssociationTest(unittest.TestCase):
    def load(self):
        return dict(allocation_cycle=10,writeback_cycle=40,virtual_address=128,
                    physical_address=4096,sample_cycles=[30,35])

    def event(self,cycle,**values):
        return dict(core_cycle=cycle,lane=0,instr_type=1,virtual_address=128,killed=0,
                    tlb_hit=1,tlb_miss=0,no_query=0,paddr_effective=1,exception=0,
                    special=0,physical_address=4096,**values)

    def test_first_hit_survives_later_cache_retry(self):
        first=self.event(16);retry=self.event(28)
        miss=self.event(12);miss.update(tlb_hit=0,tlb_miss=1,paddr_effective=0)
        late_miss=self.event(25);late_miss.update(tlb_hit=0,tlb_miss=1,paddr_effective=0)
        row=associate(self.load(),[retry,first,miss,late_miss])
        self.assertEqual((row['attempts_to_first_address_cycles'],row['first_address_to_WB_cycles']), (4,24))
        self.assertEqual((row['later_address_retry_span_cycles'],row['last_address_to_WB_cycles']), (12,12))
        self.assertTrue(row['first_address_before_first_TEA_sample'])
        self.assertEqual(row['tlb_miss_attempts'],2)
        self.assertEqual(row['TLB_miss_attempts_after_first_address'],1)
        self.assertEqual(row['TLB_miss_cycles_after_first_address'],[25])

    def test_nokey_unknown_and_lifetime_edges_do_not_become_hits(self):
        before=self.event(9);after=self.event(40);wrong=self.event(18);wrong['virtual_address']=256
        killed=self.event(15);killed['killed']=1
        row=associate(self.load(),[before,after,wrong,killed])
        self.assertEqual(row['status'],'no_matching_nonkilled_scalar_attempt')
        self.assertEqual(row['killed_attempts'],1)

    def test_no_query_is_separate_from_translation(self):
        event=self.event(17);event.update(no_query=1,tlb_hit=0)
        row=associate(self.load(),[event])
        self.assertEqual(row['successful_translation_attempts'],0)
        self.assertEqual(row['first_address_kind'],'no_query')
        self.assertEqual(row['first_address_to_WB_cycles'],23)

    def test_untranslated_and_exception_attempts_stay_unknown(self):
        miss=self.event(15);miss.update(tlb_hit=0,tlb_miss=1,paddr_effective=0)
        exception=self.event(20);exception['exception']=1
        row=associate(self.load(),[miss,exception])
        self.assertEqual(row['status'],'no_successful_address_observed')
        self.assertEqual(row['nonkilled_attempts'],2)


if __name__=='__main__':
    unittest.main()
