"""Check full routing ids, merged reads and real memory-trace identity rejection."""
import unittest
from tea_dip_memory_return_trace import transactions


def row(cycle,kind,ident=0,address=0x80000000,command=0,channel=0):
    return dict(core_cycle=cycle,step=cycle+10,kind=kind,accepted=1,is_write=0,id=ident,
                address=address,axi_id=ident,controller=0,command=command,dram_cycle=cycle,
                channel=channel,rank=0,bankgroup=0,bank=0,row=0,column=0)


class MemoryTraceTest(unittest.TestCase):
    def test_full_routing_id_reuse_after_response(self):
        ident=0x20004
        rows=[row(1,1,ident),row(2,3),row(3,2,ident),row(4,1,ident),row(5,3),row(6,2,ident)]
        complete,pending,_,_=transactions(rows)
        self.assertEqual([r['generation']for r in complete],[1,2])
        self.assertEqual([r['axi_id']for r in complete],[ident,ident])
        self.assertFalse(pending)

    def test_early_reuse_and_wrong_tracker_are_rejected(self):
        ident=0x10004
        with self.assertRaisesRegex(AssertionError,'id reused before its response'):
            transactions([row(1,1,ident),row(2,1,ident)])
        with self.assertRaisesRegex(AssertionError,'read-tracker identity mismatch'):
            transactions([row(1,1,ident),row(2,3),row(3,2,ident,address=0x80000040)])

    def test_one_READ_may_serve_two_accepted_reads(self):
        rows=[row(1,1,0x10004),row(2,1,0x20005),row(3,3),row(4,2,0x10004),row(5,2,0x20005)]
        complete,_,_,_=transactions(rows)
        self.assertEqual([r['read_command']['core_cycle']for r in complete],[3,3])

    def test_refresh_channel_is_resolved_from_real_commands(self):
        rows=[row(1,3,command=7,channel=-1),row(2,1,0x10004),row(3,3,channel=1),row(4,2,0x10004)]
        _,_,commands,_=transactions(rows)
        self.assertEqual(commands[0]['resolved_channel'],1)
        with self.assertRaisesRegex(AssertionError,'READ without an accepted request'):
            transactions([row(1,3)])


if __name__=='__main__':
    unittest.main()
