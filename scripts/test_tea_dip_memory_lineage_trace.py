import copy
import unittest

from tea_dip_memory_lineage_trace import attach_lineage, generations


def owner(bank,slot,generation,begin,end,line=0x1000,**values):
    return dict(bank=bank,slot=slot,generation=generation,begin=begin,end=end,line=line,**values)


def fixture():
    ncb=[owner(0,3,1,4,15,src_id=2,txn_id=23,states=[dict(core_cycle=4,r_pending=1)])]
    llc=[owner(0,1,1,2,16,tgt_id=0,req_id=23,src_id=0,txn_id=(2<<9)|5)]
    l2=[owner(2,5,1,1,17)]
    memory=[dict(is_write=0,id=0x20003,accepted_core=7,address=0x1000)]
    return memory,ncb,llc,l2


class MemoryLineage(unittest.TestCase):
    def test_registered_lifetimes_preserve_reuse_and_capture_edges(self):
        def row(cycle,valid=0,txn=0,slot=0):
            return dict(core_cycle=cycle,kind=1,bank=0,slot=slot,valid=valid,
                        line=0x1000 if valid else 0,src_id=int(bool(valid)),txn_id=txn,
                        return_txn_id=0,ar_pending=int(bool(valid)),r_pending=int(bool(valid)))
        rows=[row(0,slot=slot) for slot in range(64)]
        rows += [row(2,1,23),row(8),row(9,1,24),dict(core_cycle=12,kind=0)]
        owners=generations(rows,'ncb')
        self.assertEqual([(o['begin'],o['end'],o['generation']) for o in owners],[(2,8,1),(9,13,2)])
        self.assertFalse(owners[0]['left_censored'])
        self.assertFalse(owners[0]['right_censored'])
        self.assertTrue(owners[1]['right_censored'])

    def test_read_after_NCB_lifetime_is_rejected(self):
        args=fixture();args[0][0]['accepted_core']=15
        with self.assertRaisesRegex(AssertionError,'no live NCB'):
            attach_lineage(*args)

    def test_actual_identity_chain(self):
        args=fixture()
        self.assertEqual(attach_lineage(*args),dict(complete_L2_LLC_NCB_memory_chain=1))
        self.assertEqual(args[0][0]['lineage']['L2']['slot'],5)

    def test_same_address_reused_slot_does_not_inherit_origin(self):
        memory,ncb,llc,l2=fixture()
        ncb.append(owner(0,3,2,18,30,src_id=2,txn_id=24,states=[dict(core_cycle=18,r_pending=1)]))
        memory.append(dict(is_write=0,id=0x20003,accepted_core=20,address=0x1000))
        counts=attach_lineage(memory,ncb,llc,l2)
        self.assertEqual(counts['complete_L2_LLC_NCB_memory_chain'],1)
        self.assertEqual(counts['no_live_LLC_reqid_owner'],1)
        self.assertIsNone(memory[1]['lineage']['LLC'])

    def test_wrong_NCB_CHI_source_is_unknown(self):
        args=fixture();args[1][0]['src_id']=3
        self.assertEqual(attach_lineage(*args),dict(non_LLC_NCB_source=1))

    def test_normal_semantic_corruptions_rejected(self):
        for layer in [1,2,3]:
            args=copy.deepcopy(fixture());args[layer][0]['line']^=64
            with self.assertRaisesRegex(AssertionError,'block mismatch'):
                attach_lineage(*args)

    def test_external_AXI_namespace_is_not_truncated_to_NCB_slot(self):
        args=fixture();args[0][0]['id']=3
        self.assertEqual(attach_lineage(*args),dict(other_AXI_master_namespace=1))


if __name__=='__main__':
    unittest.main()
