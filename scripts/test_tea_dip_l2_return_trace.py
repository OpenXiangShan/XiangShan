"""Directed identity/handshake failures for the actual L2 trace consumer."""
import copy
import unittest

from tea_dip_gsim_l2_return_probe import FIELDS
from tea_dip_l2_return_trace import owners, queues


def queue_fixture():
    def row(cycle,bank,kind,enq,deq,n_enq,n_deq,txn=0):
        return dict(core_cycle=cycle,bank=bank,kind=kind,enq_ptr=enq,deq_ptr=deq,full=0,
                    next_enq_ptr=n_enq,next_deq_ptr=n_deq,next_full=0,
                    count=(enq-deq)&15,next_count=(n_enq-n_deq)&15,txn_id=txn,opcode=1,line=0)
    rows=[row(1,bank,1,0,0,0,0)for bank in range(4)]
    rows += [row(2,0,k,0,0,1,0,3 if k==2 else 0)for k in [1,2]]
    rows += [row(4,0,k,1,0,1,1,3 if k==3 else 0)for k in [1,3]]
    return rows


class L2ReturnTest(unittest.TestCase):
    def test_FIFO_latency_and_corrupt_transaction(self):
        rows=queue_fixture()
        transactions,pending=queues(rows)
        self.assertEqual(transactions[0]['queue_cycles'],2)
        self.assertFalse(any(pending.values()))
        rows[-1]['txn_id']=4
        with self.assertRaisesRegex(AssertionError,'FIFO member mismatch'):
            queues(rows)

    def test_missing_dequeue_is_rejected(self):
        rows=queue_fixture()[:-1]
        with self.assertRaisesRegex(AssertionError,'pointer/event mismatch'):
            queues(rows)

    def test_same_line_reuse_never_shares_returns(self):
        def row(cycle,kind=1,bank=0,slot=0,valid=0):
            return dict(core_cycle=cycle,kind=kind,bank=bank,mshr=slot,
                        **{k:0 for k in FIELDS if k!='valid'},valid=valid,
                        rxdat_opcode=0,rxdat_data_id=0)
        rows=[row(1,bank=b,slot=s)for b in range(4)for s in range(16)]
        rows += [row(2,3),row(3,valid=1),row(5),row(6,3),row(7,valid=1),row(8,2,valid=1),dict(row(9,valid=1),first=1,has_data=1)]
        records,_,_=owners(rows,10)
        a,b=records[0,0]
        self.assertEqual((a['generation'],b['generation']),(1,2))
        self.assertIsNone(a['first_data'])
        self.assertEqual(b['first_data'],8)
        rows.insert(-1,copy.deepcopy(rows[-2]))
        with self.assertRaisesRegex(AssertionError,'Duplicate L2 RXDAT'):
            owners(rows,10)

    def test_valid_edge_requires_actual_allocation(self):
        def row(bank,slot):
            return dict(core_cycle=1,kind=1,bank=bank,mshr=slot,**{k:0 for k in FIELDS},rxdat_opcode=0,rxdat_data_id=0)
        rows=[row(b,s)for b in range(4)for s in range(16)]
        bad=dict(rows[0],core_cycle=3,valid=1)
        with self.assertRaisesRegex(AssertionError,'valid edge without allocation'):
            owners(rows+[bad],5)


if __name__=='__main__':
    unittest.main()
