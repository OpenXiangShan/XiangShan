"""Pure Python regression for ending-half FTQ recovery identity.
The literals below are extracted from the existing failed FST, not a replacement
program or golden instruction stream. No compiled DUT, NEMU or simulator.
"""
from dataclasses import replace
import importlib.util
import os
from pathlib import Path
import sys
from types import SimpleNamespace as NS
import unittest

ROOT=next(p for p in Path(__file__).resolve().parents if (p/'src/test/python/Frontend/env/core/frontend_env.py').is_file())
FRONT=ROOT/'src/test/python/Frontend'
sys.path.insert(0,str(FRONT))
# Production import after integration; review supplies the ignored proposal.
if os.environ.get('AUDIT_BACKEND_MODEL_SOURCE'):
    spec=importlib.util.spec_from_file_location('env.core._cross_ftq_recovery_review',os.environ['AUDIT_BACKEND_MODEL_SOURCE'])
    module=importlib.util.module_from_spec(spec);sys.modules[spec.name]=module;spec.loader.exec_module(module)
    BackendModel=module.BackendModel
else:
    from env.core.backend_model import BackendModel
from env.model.backend_state import ActiveWrongPathEpisode,QueueInstr
from env.model.backend_runtime import BackendObservationSnapshot,CfVecCycleSnapshot,CfVecSlotSnapshot
from env.support.pc_utils import fold_pc

class CrossFtqRecovery(unittest.TestCase):
    def setup_model(self, *, flag=1, value=39, target=0x80001ffe, driven=13969):
        m=BackendModel()
        m.current_cycle=13987
        m._active_wrong_path_episode_state=ActiveWrongPathEpisode(origin_index=0,target_pc=target,
            redirect_driven=True,expected_recovery_ftq=(flag,value),redirect_driven_cycle=driven)
        self.write_pc(m,value,target,13972,flag)
        nxt=(value+1)%m.ftq_size
        self.write_pc(m,nxt,target+2,13973,flag ^ int(nxt == 0))
        m.current_cycle=13987
        return m
    def write_pc(self,m,value,pc,cycle,flag=1):
        m.current_cycle=cycle
        m._update_ftq_start_pc_cache(BackendObservationSnapshot(from_ftq_wen=1,from_ftq_ftq_idx=value,from_ftq_start_pc_addr=pc>>1,from_ftq_flag=flag))
    def entry(self,**kwargs):
        e=QueueInstr(cycle=13987,slot=0,pc=0x80001ffe,instr=0x001a0a13,is_rvc=False,pred_taken=False,
            ftq_flag=1,ftq_value=40,ftq_offset=0,is_last_in_entry=False)
        return replace(e,**kwargs)
    def accepts(self,m,e=None):
        return m._queue_entry_matches_recovery_target(self.entry() if e is None else e,0x80001ffe)
    def test_recorded_target_matches_ending_half_FTQ(self):
        self.assertTrue(self.accepts(self.setup_model()))
    def test_original_same_FTQ_identity_stays_valid_without_split_metadata(self):
        m=self.setup_model();m._ftq_write_proof_by_value.clear()
        self.assertTrue(self.accepts(m,self.entry(ftq_value=39,ftq_offset=1)))
    def test_different_actual_PC_not_backfilled_from_target(self):
        self.assertFalse(self.accepts(self.setup_model(),self.entry(pc=0)))
    def test_arbitrary_next_next_FTQ_rejected(self):
        self.assertFalse(self.accepts(self.setup_model(),self.entry(ftq_value=41)))
    def test_wrong_FTQ_flag_rejected(self):
        self.assertFalse(self.accepts(self.setup_model(),self.entry(ftq_flag=0)))
    def test_RVC_cannot_use_ending_half_exception(self):
        self.assertFalse(self.accepts(self.setup_model(),self.entry(is_rvc=True)))
    def test_nonzero_end_offset_cannot_use_ending_half_exception(self):
        self.assertFalse(self.accepts(self.setup_model(),self.entry(ftq_offset=1)))
    def test_fetch_fault_cannot_use_ending_half_exception(self):
        self.assertFalse(self.accepts(self.setup_model(),self.entry(exception_bits=1<<1)))
    def test_missing_first_block_start_rejected(self):
        m=self.setup_model();del m._ftq_write_proof_by_value[39];self.assertFalse(self.accepts(m))
    def test_missing_ending_block_start_rejected(self):
        m=self.setup_model();del m._ftq_write_proof_by_value[40];self.assertFalse(self.accepts(m))
    def test_previous_PC_history_cannot_substitute_current_start(self):
        m=self.setup_model();m._ftq_previous_start_pc_by_value[40]=0x80002000;m._ftq_write_proof_by_value[40]=(1,0,13973)
        self.assertFalse(self.accepts(m))
    def test_noncontiguous_block_starts_rejected(self):
        m=self.setup_model();self.write_pc(m,40,0x80002040,13974);self.assertFalse(self.accepts(m))
    def test_missing_write_cycle_rejected(self):
        m=self.setup_model();del m._ftq_write_proof_by_value[40];self.assertFalse(self.accepts(m))
    def test_pre_redirect_first_block_cache_rejected(self):
        m=self.setup_model();m._ftq_write_proof_by_value[39]=(1,0x80001ffe,13969);self.assertFalse(self.accepts(m))
    def test_pre_redirect_ending_block_cache_rejected(self):
        m=self.setup_model();m._ftq_write_proof_by_value[40]=(1,0x80002000,13968);self.assertFalse(self.accepts(m))
    def test_future_cache_write_not_used_for_earlier_packet(self):
        m=self.setup_model();m._ftq_write_proof_by_value[40]=(1,0x80002000,13988);self.assertFalse(self.accepts(m))
    def test_packet_from_redirect_cycle_rejected(self):
        self.assertFalse(self.accepts(self.setup_model(),self.entry(cycle=13969)))
    def test_missing_expected_FTQ_rejected(self):
        m=self.setup_model();m._active_wrong_path_episode_state.expected_recovery_ftq=None;self.assertFalse(self.accepts(m))
    def test_missing_redirect_cycle_rejected_for_split(self):
        m=self.setup_model();m._active_wrong_path_episode_state.redirect_driven_cycle=None;self.assertFalse(self.accepts(m))
    def test_FTQ_wrap_retains_flag_identity(self):
        m=self.setup_model(flag=0,value=63)
        self.assertTrue(m._queue_entry_matches_recovery_target(self.entry(ftq_flag=1,ftq_value=0),0x80001ffe))
        self.assertFalse(m._queue_entry_matches_recovery_target(self.entry(ftq_flag=0,ftq_value=0),0x80001ffe))
    def test_ftq_zero_wrap_retains_flag_identity(self):
        m=self.setup_model(flag=1,value=63)
        self.assertTrue(m._queue_entry_matches_recovery_target(self.entry(ftq_flag=0,ftq_value=0),0x80001ffe))
    def test_wrong_generation_cannot_prove_matching_slot(self):
        m=self.setup_model(); m._ftq_write_proof_by_value[40]=(0,0x80002000,13973)
        self.assertFalse(self.accepts(m))
    def test_unidentified_write_invalidates_prior_proof(self):
        m=self.setup_model(); m.current_cycle=13980
        m._update_ftq_start_pc_cache(BackendObservationSnapshot(from_ftq_wen=1,from_ftq_ftq_idx=40,from_ftq_start_pc_addr=0x80002000>>1))
        self.assertFalse(self.accepts(m))
    def test_cache_timestamp_changes_on_real_write_only(self):
        m=self.setup_model();m.current_cycle=13980
        m._update_ftq_start_pc_cache(BackendObservationSnapshot(from_ftq_wen=0,from_ftq_ftq_idx=40,from_ftq_start_pc_addr=0))
        self.assertEqual(m._ftq_write_proof_by_value[40][2],13973)
        self.write_pc(m,40,0x80002000,13981)
        self.assertEqual(m._ftq_write_proof_by_value[40][2],13981)
    def test_hardware_reset_clears_cache_proof(self):
        m=self.setup_model();m.on_hardware_reset(13980)
        self.assertFalse(m._ftq_write_proof_by_value);self.assertFalse(m._ftq_start_pc_by_value)
    def snapshot(self,m,pc=0x80001ffe,ftq_value=40,offset=0,exception=0,instr=0x001a0a13):
        m.current_cycle=13987;m.cfvec_if=object()
        slots=[CfVecSlotSnapshot(slot=0,valid=True,foldpc=fold_pc(pc),instr=instr,is_rvc=False,
            ftq_flag=1,ftq_value=ftq_value,ftq_offset=offset,exception_bits=exception)]
        slots.extend(CfVecSlotSnapshot(slot=i) for i in range(1,8))
        snap=CfVecCycleSnapshot(cycle=13987,slots=tuple(slots));m.accept_cfvec_snapshot(snap)
        return snap
    def test_full_sample_uses_observed_PC_and_completes_recovery(self):
        m=self.setup_model();snap=self.snapshot(m);m._sample_cfvec(snap)
        self.assertFalse(m._recovery_phase_active())
        self.assertEqual(m._cfvec_queue[0].pc,0x80001ffe)
        self.assertEqual((m._cfvec_queue[0].ftq_flag,m._cfvec_queue[0].ftq_value),(1,40))
    def test_first_recovery_PC_assertion_still_fails_for_real_zero_packet(self):
        m=self.setup_model();self.write_pc(m,41,0,13980)
        snap=self.snapshot(m,pc=0,ftq_value=41,offset=1,exception=1<<1,instr=0)
        with self.assertRaisesRegex(AssertionError,'first cfvec is not target.*actual_pc=0x0'):
            m._sample_cfvec(snap)
    def test_unresolvable_foldpc_still_fails(self):
        m=self.setup_model()
        with self.assertRaisesRegex(AssertionError,'foldpc does not match'):
            m._resolve_cfvec_pc(slot=0,ftq_flag=1,ftq_value=40,ftq_offset=0,is_rvc=False,observed_foldpc=17)

if __name__=='__main__':unittest.main(verbosity=2)
