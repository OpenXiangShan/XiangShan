"""Pure input/evidence negatives; no compilation, emulator, DUT or new corpus."""
import importlib.util
from pathlib import Path
import unittest
from dataclasses import dataclass
from types import SimpleNamespace
import tempfile

spec=importlib.util.spec_from_file_location('recovery_contract_under_review',Path(__file__).with_name('page_tail_recovery_contract.py'))
c=importlib.util.module_from_spec(spec);spec.loader.exec_module(c)

class RecoveryContract(unittest.TestCase):
    def program(self):
        source=Path(__file__).resolve().parents[2]/'tests/asm_cases/jiabowen/fe_page_tail_ftq_recovery.S'
        image=c.expected_image();rows=[{'index':i,'pc':hex(pc),'instr':hex(ins),'size':size} for i,(pc,ins,size) in enumerate(c.expected_trace())]
        words={pc:(ins,size) for pc,ins,size in c.expected_trace()};words[c.TRAP]=(0x5006b,4)
        pm={'binary_text_verified':True,'binary_sha256':c.hashlib.sha256(image).hexdigest(),'source_sha256':c.sha(source),
            'labels':dict(recovery_call=c.CALL,after_call=c.RETURN,recovery_tail=c.TAIL,recovery_ret=c.RET,good_trap=c.TRAP),
            'instructions':[{'pc':pc,'size':size,'encoding':hex(ins)} for pc,(ins,size) in words.items()]}
        log=f'HIT GOOD TRAP at pc = {hex(c.TRAP)}\ntrap code:0\ntotal guest instructions = 21\nNEMU exit with good state: 2, halt ret: 0\n'
        return [image,rows,pm,source,log]
    def evidence(self):
        def entry(pc,instr,cycle,offset):return dict(pc=pc,instr=instr,cycle=cycle,slot=0,is_rvc=False,ftq_flag=1,ftq_value=40,ftq_offset=offset,exception_bits=0,path_state='correct',golden_index={c.TAIL:17,c.RET:18,c.RETURN:19}[pc],resolve_state='not_needed')
        tail=entry(c.TAIL,0x001a0a13,120,0);ret=entry(c.RET,0x8067,120,2);cont=entry(c.RETURN,0x001a0a93,160,1)
        cont.update(ftq_value=42)
        packets=[]
        for e in (tail,ret,cont):packets.append({'cycle':e['cycle'],'packets':[{**e,'foldpc':c.fold(e['pc']),'processed_by_model':True}]})
        cache={};history=[]
        for cycle in range(100,121):
            before={k:dict(v) for k,v in cache.items()}
            idx,pc=(39,c.TAIL) if cycle==103 else ((40,c.TAIL+2) if cycle==104 else (0,0))
            wen=int(cycle in (103,104))
            if wen:cache[str(idx)]=dict(start_pc=pc,write_cycle=cycle)
            history.append(dict(cycle=cycle,frontend_snapshot_cycle=cycle,write_generation_flag=None,
                raw=dict(from_ftq_wen=wen,from_ftq_ftq_idx=idx,from_ftq_start_pc_addr=pc>>1),
                cache_before=before,cache_after={k:dict(v) for k,v in cache.items()}))
        live1=dict(s1_valid=1,s1_fire=1,s1_flush=0,s1_instrCount=2,s1_prevEndHalfRviInfo_valid=1,
            s1_prevEndHalfRviInfo_bits_pc_addr=c.TAIL>>1,s1_prevEndHalfRviInfo_bits_data=0x0a13,
            s1_alignedInstrPcVec_0_addr=c.TAIL>>1,s1_alignedInstrVec_0_data=0x001a0a13,ftq_override_valid=0,ftq_s2Override=0,ftq_s3Override=0)
        live2=dict(s2_fire=1,s2_flush=0,io_toIBuffer_valid=1,io_toIBuffer_ready=1,io_toIBuffer_bits_valid=1,io_toIBuffer_bits_enqEnable=1,
            s2_alignedInstrPcVec_0_addr=c.TAIL>>1,s2_alignedInstrVec_0_data=0x001a0a13,
            s2_alignedInstrVec_0_blockSel=0,s2_alignedInstrVec_0_isCrossBlockInstr=0,s2_alignedInstrVec_0_endOffset=0,
            s2_fetchBlock_0_valid=1,s2_fetchBlock_0_ftqIdx_flag=1,s2_fetchBlock_0_ftqIdx_value=40,ftq_override_valid=0,ftq_s2Override=0,ftq_s3Override=0)
        native=dict(fromFtq_history=history,fromFtq_generation_flag=dict(observed=False),
            recovery_exits=[dict(cycle=120,target_pc=c.TAIL,before_active=True,after_active=False,cursor_before=17,cursor_after=18,queue_after=[dict(tail)])],
            live_ifu=[dict(cycle=110,values=live1),dict(cycle=111,values=live2)],
            resolve_plans=[dict(cycle=130,entry=dict(inst_pc=c.RET,target=c.RETURN,branch_type=3,ras_action=1,ftq_flag=1,ftq_value=40,ftq_offset=2,pc=c.TAIL+2),queue_entry=dict(ret))],
            resolve_bus=[dict(cycle=130,phase='post_drive_input_not_edge',ftq=[1,40],offset=2,target_addr_raw=c.RETURN>>1,pc_addr_raw=(c.TAIL+2)>>1,branch_type=3,ras_action=1,taken=1)])
        return dict(**native,overflow=False,redirects=[dict(cycle=101,phase='pre_drive_input',pc=c.CALL,target=c.TAIL,ftq=[1,38])],
            matches=[dict(entry=tail,expected_ftq=[1,39],redirect_cycle=100,target_pc=c.TAIL,accept_reason='fresh_start_pair_ending_half_next_ftq',
                fromFtq={'39':dict(start_pc=c.TAIL,write_cycle=103),'40':dict(start_pc=c.TAIL+2,write_cycle=104)})],
            snapshots=packets,commits=[dict(cycle=140,mode='queue',entries=[tail,ret],actual_bus=dict(valid=1,flag=1,value=40),model_ptr=[1,40]),
                dict(cycle=180,mode='queue',entries=[cont],actual_bus=dict(valid=1,flag=1,value=42),model_ptr=[1,42])])
    def rejects(self,data):
        with self.assertRaises((AssertionError,KeyError,TypeError)):c.validate_observations(data)
    def test_new_source_and_known_image_layout(self):
        values=self.program();self.assertEqual(len(values[0]),0x4000)
        result=c.validate_program(*values);self.assertEqual(result['reference_entries'],21)
        self.assertEqual(c.TAIL%4096,4094);self.assertEqual(c.RET,c.TAIL+4)
        self.assertEqual(values[0][0xffe:0x1002],b'\x13\x0a\x1a\x00')
    def test_indirect_call_real_target_and_link_register_independently_decoded(self):
        image=c.expected_image()
        auipc=int.from_bytes(image[4:8],'little');addi=int.from_bytes(image[8:12],'little')
        self.assertEqual(auipc&127,0x17);self.assertEqual((auipc>>7)&31,6)
        immediate=addi>>20
        if immediate&0x800:immediate-=4096
        target=c.BASE+4+(auipc&0xfffff000)+immediate
        self.assertEqual(target,c.TAIL)
        call=int.from_bytes(image[0x40:0x44],'little')
        self.assertEqual(call&127,0x67);self.assertEqual((call>>7)&31,1)
        self.assertEqual((call>>15)&31,6);self.assertEqual(call>>20,0)
        self.assertEqual(c.CALL+4,c.RETURN)
    def test_full_image_byte_change_rejected(self):
        v=self.program();v[0]=b'\0'+v[0][1:]
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_wrong_trace_bytes_rejected(self):
        v=self.program();v[1][17]['instr']='0x13'
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_short_trace_rejected(self):
        v=self.program();v[1].pop()
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_wrong_trace_order_rejected(self):
        v=self.program();v[1][17],v[1][18]=v[1][18],v[1][17]
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_BAD_TRAP_rejected(self):
        v=self.program();v[4]+='HIT BAD TRAP'
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_missing_NEMU_good_exit_rejected(self):
        v=self.program();v[4]=v[4].replace('NEMU exit with good state: 2, halt ret: 0','')
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_wrong_trap_PC_rejected(self):
        v=self.program();v[4]=v[4].replace(hex(c.TRAP),'0x0')
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_unverified_ProgramMap_rejected(self):
        v=self.program();v[2]['binary_text_verified']=False
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_wrong_source_hash_rejected(self):
        v=self.program();v[2]['source_sha256']='0'*64
        with self.assertRaises(AssertionError):c.validate_program(*v)
    def test_positive_complete_recovery_RET_and_commit_evidence(self):
        result=c.validate_observations(self.evidence());self.assertFalse(result['canonical_HIT'])
        self.assertEqual(len(result['retired_cycles']),3)
    def test_no_actual_redirect_rejected(self):
        d=self.evidence();d['redirects']=[];self.rejects(d)
    def test_postdrive_only_redirect_not_called_edge_evidence(self):
        d=self.evidence();d['redirects'][0]['phase']='post_drive_input';self.rejects(d)
    def test_wrong_raw_redirect_FTQ_rejected(self):
        d=self.evidence();d['redirects'][0]['ftq']=[1,37];self.rejects(d)
    def test_no_actual_recovery_match_rejected(self):
        d=self.evidence();d['matches']=[];self.rejects(d)
    def test_wrong_target_instruction_PC_rejected(self):
        d=self.evidence();d['matches'][0]['entry']['pc']=0;self.rejects(d)
    def test_fault_on_tail_rejected(self):
        d=self.evidence();d['matches'][0]['entry']['exception_bits']=2;self.rejects(d)
    def test_wrong_ending_half_identity_rejected(self):
        d=self.evidence();d['matches'][0]['entry']['ftq_value']=41;self.rejects(d)
    def test_old_FTQ_start_write_rejected(self):
        d=self.evidence();d['matches'][0]['fromFtq']['40']['write_cycle']=99;self.rejects(d)
    def test_missing_startPc_proof_rejected(self):
        d=self.evidence();d['matches'][0]['fromFtq']['39']['start_pc']=None;self.rejects(d)
    def test_wrong_fold_RET_rejected(self):
        d=self.evidence();d['snapshots'][1]['packets'][0]['foldpc']^=1;self.rejects(d)
    def test_wrong_RET_bytes_rejected(self):
        d=self.evidence();d['snapshots'][1]['packets'][0]['instr']=0x13;self.rejects(d)
    def test_cursor_only_no_commits_rejected(self):
        d=self.evidence();d['commits']=[];self.rejects(d)
    def test_continuation_missing_commit_rejected(self):
        d=self.evidence();d['commits'].pop();self.rejects(d)
    def test_commit_bus_model_disagreement_rejected(self):
        d=self.evidence();d['commits'][0]['actual_bus']['value']=39;self.rejects(d)
    def test_commit_without_valid_rejected(self):
        d=self.evidence();d['commits'][0]['actual_bus']['valid']=0;self.rejects(d)
    def test_wrong_retired_entry_identity_rejected(self):
        d=self.evidence();d['commits'][0]['entries'][1]['ftq_value']=41;self.rejects(d)
    def test_missing_continuation_snapshot_rejected(self):
        d=self.evidence();d['snapshots'].pop();self.rejects(d)
    def test_unprocessed_snapshot_cannot_prove_RET(self):
        d=self.evidence();d['snapshots'][1]['packets'][0]['processed_by_model']=False;self.rejects(d)
    def test_wrong_committed_RET_bytes_rejected(self):
        d=self.evidence();d['commits'][0]['entries'][1]['instr']=0x13;self.rejects(d)
    def test_wrong_path_commit_rejected(self):
        d=self.evidence();d['commits'][0]['entries'][1]['path_state']='wrong';self.rejects(d)
    def test_wrong_reference_instance_commit_rejected(self):
        d=self.evidence();d['commits'][0]['entries'][1]['golden_index']=17;self.rejects(d)
    def test_gap_in_fromFtq_write_history_rejected(self):
        d=self.evidence();d['fromFtq_history'].pop(5);self.rejects(d)
    def test_wrong_same_cycle_timestamp_rejected(self):
        d=self.evidence();d['fromFtq_history'][4]['cache_after']['40']['write_cycle']=103;self.rejects(d)
    def test_stale_frontend_snapshot_rejected(self):
        d=self.evidence();d['fromFtq_history'][4]['frontend_snapshot_cycle']=103;self.rejects(d)
    def test_write_generation_flag_cannot_be_invented(self):
        d=self.evidence();d['fromFtq_history'][4]['write_generation_flag']=1;self.rejects(d)
    def test_slot_reuse_requires_separate_analysis(self):
        d=self.evidence();h=d['fromFtq_history'][5];h['raw']=dict(from_ftq_wen=1,from_ftq_ftq_idx=40,from_ftq_start_pc_addr=(c.TAIL+2)>>1)
        h['cache_after']['40']['write_cycle']=105;self.rejects(d)
    def test_saved_half_invalid_payload_cannot_prove_scenario(self):
        d=self.evidence();d['live_ifu'][0]['values']['s1_prevEndHalfRviInfo_valid']=0;self.rejects(d)
    def test_saved_half_wrong_low_data_rejected(self):
        d=self.evidence();d['live_ifu'][0]['values']['s1_prevEndHalfRviInfo_bits_data']=0;self.rejects(d)
    def test_missing_ready_is_not_zero_or_pass(self):
        d=self.evidence();d['live_ifu'][1]['values'].pop('io_toIBuffer_ready');self.rejects(d)
    def test_masked_off_aligned_payload_not_consumed(self):
        d=self.evidence();d['live_ifu'][1]['values']['io_toIBuffer_bits_enqEnable']=0;self.rejects(d)
    def test_wrong_live_completion_tuple_rejected(self):
        d=self.evidence();d['live_ifu'][1]['values']['s2_fetchBlock_0_ftqIdx_flag']=0;self.rejects(d)
    def test_missing_model_recovery_exit_rejected(self):
        d=self.evidence();d['recovery_exits']=[];self.rejects(d)
    def test_no_golden_progress_after_recovery_rejected(self):
        d=self.evidence();d['recovery_exits'][0]['cursor_after']=17;self.rejects(d)
    def test_missing_actual_RET_resolve_rejected(self):
        d=self.evidence();d['resolve_bus']=[];self.rejects(d)
    def test_wrong_actual_RET_target_rejected(self):
        d=self.evidence();d['resolve_bus'][0]['target_addr_raw']=0;self.rejects(d)
    def test_extra_redirect_during_pairing_rejected(self):
        d=self.evidence();d['redirects'].append(dict(cycle=112,pc=c.RET,target=c.RETURN,phase='pre_drive_input',ftq=[1,40]));self.rejects(d)
    def test_unobserved_override_history_rejected(self):
        d=self.evidence();d['live_ifu'][0]['values'].pop('ftq_override_valid');self.rejects(d)
    def test_overflow_rejected(self):
        d=self.evidence();d['overflow']=True;self.rejects(d)

@dataclass(frozen=True)
class Lane:
    slot:int=0
    valid:bool=True

class TapDelegation(unittest.TestCase):
    def make(self):
        class Model:
            def __init__(self):
                self.current_cycle=4
                self._cfvec_cycle_snapshot=SimpleNamespace(cycle=4,slots=(Lane(),))
                self._cfvec_snapshot_pc_cache={}
                self.calls=[];self.skip=False;self.fail=None
                self._planned_commit_apply=None;self.commit_count=0;self.golden_trace=None
                self._ftq_start_pc_by_value={};self._ftq_start_pc_write_cycle_by_value={}
            def _sample_cfvec(self,*args,**kwargs):
                self.calls.append((args,kwargs))
                if self.fail:raise self.fail
                if not self.skip:self._cfvec_snapshot_pc_cache[0]=c.TAIL
                return 'original-result'
            def consume_backend_observation(self,observation):return 'consume-result'
            def _apply_recovery_if_target_queued(self):return 'recovery-result'
            def _ready_resolves_for_cycle(self):return ()
            def _recovery_phase_active(self):return False
            def _current_recovery_target_pc(self):return None
            def observed_cfvec_snapshot_pc(self,*args):
                raise AssertionError('tap must never invoke the PC resolver')
            def _queue_entry_matches_recovery_target(self,entry,target_pc):
                self.calls.append((entry,target_pc));return False
            def commit_entry_driven(self,entry):
                self.calls.append(entry);return 'commit-result'
        model=Model();observers=[]
        env=SimpleNamespace(backend_model=model,register_pre_drive_cycle_observer=observers.append,register_cycle_observer=observers.append)
        tap=c.ObservationTap(env);tap.install()
        self.addCleanup(tap.restore)
        return model,tap,observers
    def test_sample_default_forwards_once_and_uses_original_cache(self):
        m,t,_=self.make();self.assertEqual(m._sample_cfvec(),'original-result')
        self.assertEqual(m.calls,[((),{})]);p=t.data['snapshots'][0]['packets'][0]
        self.assertEqual(p['pc'],c.TAIL);self.assertTrue(p['processed_by_model'])
    def test_sample_keyword_argument_and_return_preserved(self):
        m,t,_=self.make();snap=m._cfvec_cycle_snapshot
        self.assertEqual(m._sample_cfvec(snapshot=snap),'original-result')
        self.assertEqual(m.calls,[((),{'snapshot':snap})])
    def test_sample_positional_argument_preserved(self):
        m,t,_=self.make();snap=m._cfvec_cycle_snapshot
        m._sample_cfvec(snap);self.assertEqual(m.calls,[((snap,),{})])
    def test_skipped_lane_never_extra_resolved_or_marked_processed(self):
        m,t,_=self.make();m.skip=True;m._sample_cfvec()
        p=t.data['snapshots'][0]['packets'][0]
        self.assertIsNone(p['pc']);self.assertFalse(p['processed_by_model'])
    def test_missing_snapshot_during_skip_preserves_original_return(self):
        m,t,_=self.make();m.skip=True;m._cfvec_cycle_snapshot=None
        self.assertEqual(m._sample_cfvec(),'original-result');self.assertEqual(t.data['snapshots'],[])
    def test_original_assertion_object_and_single_call_preserved(self):
        m,t,_=self.make();failure=AssertionError('strict first recovery failure');m.fail=failure
        with self.assertRaises(AssertionError) as caught:m._sample_cfvec()
        self.assertIs(caught.exception,failure);self.assertEqual(len(m.calls),1)
    def test_rejected_match_and_commit_forward_without_fabricated_evidence(self):
        m,t,_=self.make();entry=object()
        self.assertIs(m._queue_entry_matches_recovery_target(entry,c.TAIL),False)
        self.assertEqual(m.commit_entry_driven(entry),'commit-result')
        self.assertEqual(m.calls,[(entry,c.TAIL),entry]);self.assertFalse(t.data['matches']);self.assertFalse(t.data['commits'])
    def test_restore_original_methods_and_disable_observer(self):
        m,t,observers=self.make();t.restore()
        self.assertNotIn('_sample_cfvec',m.__dict__)
        self.assertNotIn('_queue_entry_matches_recovery_target',m.__dict__)
        self.assertNotIn('commit_entry_driven',m.__dict__)
        observers[0](5,None) # inert after restoration; no env access
        self.assertFalse(t.active)

class ProbeReader(unittest.TestCase):
    def reader(self,source='Frontend_top.Frontend.inner_ifu.s1_fire',value=1):
        temp=tempfile.TemporaryDirectory();self.addCleanup(temp.cleanup)
        path=Path(temp.name)/'inventory.yaml'
        path.write_text('signals:\n  - name: "inner_ifu.s1_fire"\n    kind: direct\n    source: "'+source+'"\n')
        dut=SimpleNamespace(GetInternalSignal=lambda name:SimpleNamespace(value=value))
        return c.NativeProbeReader(dut,path)
    def test_true_native_alias_can_be_read(self):
        r=self.reader();self.assertEqual(r.sample()['s1_fire'],1)
        self.assertIn('io_toIBuffer_ready',r.missing)
    def test_toggle_coverage_alias_cannot_be_evidence(self):
        r=self.reader(source='Frontend_top.Frontend.inner_ifu.__Vtogcov__s1_fire')
        self.assertNotIn('s1_fire',r.sample());self.assertIn('s1_fire',r.missing)
    def test_failed_value_read_kept_unobserved(self):
        r=self.reader(value=None);self.assertIsNone(r.sample()['s1_fire']);self.assertIn('s1_fire',r.missing)

if __name__=='__main__':unittest.main(verbosity=2)
