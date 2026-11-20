"""Independent short repair regression contracts; no canonical/random admission.
Expected bytes describe a new hand-written program. Actual cfVec PC is always
resolved from observed FTQ context, never replaced by these expected addresses.
"""
from dataclasses import asdict
import hashlib
import json
from pathlib import Path
import re

BASE=0x80000000
CALL=BASE+0x40
RETURN=BASE+0x44
TAIL=BASE+0xffe
RET=BASE+0x1002
TRAP=BASE+0x4c
MAX_CYCLES=10000
CASE_ID='independent-page-tail-ftq-recovery-v1'
ASSEMBLY_REL='src/test/python/Frontend/tests/asm_cases/jiabowen/fe_page_tail_ftq_recovery.S'
NODE_REL='src/test/python/Frontend/tests/random_case/test_page_tail_recovery_dut.py::test_independent_page_tail_recovery_ret_commits'


def sha(path):return hashlib.sha256(Path(path).read_bytes()).hexdigest()
def fold(pc):
    value=int(pc)>>1;result=0
    while value:result^=value&1023;value>>=10
    return result&1023


def expected_image():
    """In-memory byte specification only; callers never write a program corpus."""
    payload=bytearray(b'\x01\x00'*0x2000)
    words={0:0x00000a13,4:0x00001317,8:0xffa30313,**{4*i:0x13 for i in range(3,16)},0x40:0x000300e7,
           0x44:0x001a0a93,0x48:0x00000513,0x4c:0x0005006b,
           0xffe:0x001a0a13,0x1002:0x00008067}
    for offset,word in words.items():payload[offset:offset+4]=word.to_bytes(4,'little')
    return bytes(payload)


def expected_trace():
    image=expected_image();pcs=[BASE+4*i for i in range(16)]+[CALL,TAIL,RET,RETURN,BASE+0x48]
    return [(pc,int.from_bytes(image[pc-BASE:pc-BASE+4],'little'),4) for pc in pcs]


def validate_program(binary,trace,program_map,source_path,nemu_log):
    assert binary==expected_image(),'binary differs from independent hand-written program'
    expected=expected_trace()
    assert len(trace)==len(expected)==21,'short regression needs exactly21 full reference entries'
    for index,(row,(pc,instr,size)) in enumerate(zip(trace,expected)):
        assert type(row['index']) is int and row['index']==index,'reference order/index mismatch'
        assert int(str(row['pc']),0)==pc and int(str(row['instr']),0)==instr and row['size']==size,'reference PC/instruction/size mismatch'
    text=re.sub(r'\x1b\[[0-9;]*m','',nemu_log)
    assert text.count('HIT GOOD TRAP')==1 and 'HIT BAD TRAP' not in text,'NEMU trap not clean'
    traps=re.findall(r'HIT GOOD TRAP[^\n]*?at pc\s*=\s*(0x[0-9a-fA-F]+)',text)
    assert len(traps)==1 and int(traps[0],0)==TRAP,'wrong NEMU terminal PC'
    totals=re.findall(r'total guest instructions\s*=\s*([0-9,]+)',text)
    assert len(totals)==1 and int(totals[0].replace(',',''))==21,'NEMU full trace count differs'
    assert re.search(r'trap code:\s*0\b',text) and re.search(r'NEMU exit with good state:\s*2,\s*halt ret:\s*0\b',text),'missing zero trap code/clean NEMU state'
    assert program_map['binary_text_verified'] is True
    assert program_map['binary_sha256']==hashlib.sha256(binary).hexdigest()
    assert program_map['source_sha256']==sha(source_path)
    labels=program_map['labels']
    for label,pc in [('recovery_call',CALL),('after_call',RETURN),('recovery_tail',TAIL),('recovery_ret',RET),('good_trap',TRAP)]:
        assert labels[label]==pc,'linked label differs: '+label
    by_pc={i['pc']:i for i in program_map['instructions']}
    for pc,instr,size in expected+[(TRAP,0x5006b,4)]:
        item=by_pc[pc]
        assert item['size']==size and int(item['encoding'],16)==instr,'ProgramMap differs from linked instruction bytes'
    return {'case_id':CASE_ID,'reference_entries':21,'tail':hex(TAIL),'RET':hex(RET),'return_to':hex(RETURN),
            'scope':'short standalone repair regression only; not random >=5000 admission'}


def entry_record(entry):
    return {k:getattr(entry,k) for k in ('cycle','slot','pc','instr','is_rvc','ftq_flag','ftq_value','ftq_offset','exception_bits','path_state','golden_index','resolve_state')}


class NativeProbeReader:
    """Read registered, native direct probes only; absent is None, never zero."""
    def __init__(self,dut,inventory):
        self.dut=dut;self.signals={};self.paths={};self.missing={}
        text=Path(inventory).read_text()
        records={}
        for block in re.split(r'(?m)^  - name: ',text)[1:]:
            lines=block.splitlines();name=lines[0].strip().strip('"')
            source=re.search(r'(?m)^    source: "?([^"\n]+)',block)
            kind=re.search(r'(?m)^    kind: (.+)',block)
            records[name]=(source.group(1).strip() if source else name,kind.group(1).strip('"') if kind else 'native')
        keys=['s1_valid','s1_fire','s1_flush','s1_instrCount','s1_prevEndHalfRviInfo_valid',
              's1_prevEndHalfRviInfo_bits_pc_addr','s1_prevEndHalfRviInfo_bits_data',
              's2_fire','s2_flush','s2_alignedInstrValid','s2_fixedInstrValid',
              'io_toIBuffer_valid','io_toIBuffer_ready','io_toIBuffer_bits_valid','io_toIBuffer_bits_enqEnable',
              'wbRedirect_valid','uncacheRedirect_valid']
        for stage in ('s1','s2'):
            for lane in range(36):
                keys += [f'{stage}_alignedInstrPcVec_{lane}_addr',f'{stage}_alignedInstrVec_{lane}_data']
        for block in (0,1):
            keys += [f's2_fetchBlock_{block}_valid',f's2_fetchBlock_{block}_ftqIdx_flag',f's2_fetchBlock_{block}_ftqIdx_value']
        for lane in range(36):keys += [f's2_alignedInstrVec_{lane}_blockSel',f's2_alignedInstrVec_{lane}_isCrossBlockInstr',f's2_alignedInstrVec_{lane}_endOffset']
        requested={key:('inner_ifu.'+key,'Frontend_top.Frontend.inner_ifu.'+key) for key in keys}
        for stage in (2,3):requested[f'ftq_s{stage}Override']=(f'inner_ftq.io_fromBpu_prediction_bits_s{stage}Override',)
        requested['ftq_override_valid']=('inner_ftq.io_fromBpu_prediction_valid',)
        for key,candidates in requested.items():
            for name in candidates:
                if name not in records:continue
                source,kind=records[name]
                if '__Vtogcov__' in source or '__Vtogcov__' in name or '__Vdly__' in source or kind not in ('native','direct'):continue
                try:signal=dut.GetInternalSignal(name)
                except Exception:continue
                if signal is not None:
                    self.signals[key]=signal;self.paths[key]={'name':name,'source':source};break
            if key not in self.signals:self.missing[key]='UNOBSERVED: no usable native probe'
    def sample(self):
        values={}
        for key,signal in self.signals.items():
            try:values[key]=int(signal.value)
            except Exception:self.missing[key]='UNOBSERVED: native read failed';values[key]=None
        return values


class ObservationTap:
    """Read-only delegates around three model methods; original args/results kept.
    Capture model's native monitor snapshot and actual queue commit, not a guessed
    PC or replacement checker. No method returns or model inputs are changed.
    """
    def __init__(self,env,signal_inventory=None):
        self.env=env;self.model=env.backend_model
        self.probes=None if signal_inventory is None else NativeProbeReader(env.dut,signal_inventory)
        self.data={'snapshots':[],'matches':[],'commits':[],'redirects':[],'fromFtq_history':[],'recovery_exits':[],'resolve_plans':[],'resolve_bus':[],'live_ifu':[],
            'fromFtq_generation_flag':{'observed':False,'reason':'native fromFtq write port has value only; no generation flag'},
            'probe_paths':{} if self.probes is None else self.probes.paths,'missing_probes':{} if self.probes is None else self.probes.missing,'overflow':False,'sampling_phase':'monitor_snapshot_before_backend_drive; no standalone lane-ready proof'}
        self.original={};self.instance_original={};self.active=False
    def append(self,kind,value):
        if len(self.data[kind])>=20000:self.data['overflow']=True
        else:self.data[kind].append(value)
    def install(self):
        model=self.model
        self.active=True
        for name in ('_sample_cfvec','_queue_entry_matches_recovery_target','commit_entry_driven','consume_backend_observation','_apply_recovery_if_target_queued','_ready_resolves_for_cycle'):
            self.instance_original[name]=model.__dict__.get(name)
            self.original[name]=getattr(model,name)
        def sample(*args,**kwargs):
            snap=kwargs.get('snapshot',args[0] if args else model._cfvec_cycle_snapshot)
            if snap is None:snap=model._cfvec_cycle_snapshot
            # Let the original decide whether this cycle is skipped or invalid.
            # Never resolve an additional PC: copy only its current-cycle cache.
            result=self.original['_sample_cfvec'](*args,**kwargs)
            if snap is not None and snap is model._cfvec_cycle_snapshot and snap.cycle==model.current_cycle:
                packet=[]
                for lane in snap.slots:
                    if not lane.valid:continue
                    pc=model._cfvec_snapshot_pc_cache.get(lane.slot)
                    packet.append({**asdict(lane),'pc':pc,'processed_by_model':pc is not None})
                if packet:self.append('snapshots',{'cycle':snap.cycle,'packets':packet})
            return result
        def match(entry,target_pc):
            accepted=self.original['_queue_entry_matches_recovery_target'](entry,target_pc)
            if accepted:
                expected=model._current_expected_recovery_ftq();episode=model._active_wrong_path_episode()
                assert expected is not None and episode is not None
                self.append('matches',{'entry':entry_record(entry),'expected_ftq':list(expected),
                    'redirect_cycle':episode['redirect_driven_cycle'],'target_pc':int(target_pc),
                    'accept_reason':'exact_expected_ftq' if list(expected)==[entry.ftq_flag,entry.ftq_value] else 'fresh_start_pair_ending_half_next_ftq',
                    'golden_cursor':None if model.golden_trace is None else int(model.golden_trace.cursor),
                    'fromFtq':{str(value):{'start_pc':model._ftq_start_pc_by_value.get(value),
                         'write_cycle':model._ftq_start_pc_write_cycle_by_value.get(value)} for value in (expected[1],entry.ftq_value)}})
            return accepted
        def commit(entry):
            plan=model._planned_commit_apply
            before=model.commit_count
            kept=[]
            if plan is not None and plan.get('mode')=='queue':
                kept=[entry_record(e) for e in list(model._cfvec_queue)[:int(plan['span_len'])]]
            result=self.original['commit_entry_driven'](entry)
            if model.commit_count>before:
                signal=self.env.backend_to_ftq_if
                self.append('commits',{'cycle':model.current_cycle,'entries':kept,'mode':None if plan is None else plan.get('mode'),
                    'actual_bus':{'valid':int(signal.commit_valid.value),'flag':int(signal.commit_bits_flag.value),'value':int(signal.commit_bits_value.value)},
                    'model_ptr':[model.commit_ptr_flag,model.commit_ptr_value]})
            return result
        def cache():
            return {str(v):{'start_pc':pc,'write_cycle':model._ftq_start_pc_write_cycle_by_value.get(v)} for v,pc in model._ftq_start_pc_by_value.items()}
        def consume(*args,**kwargs):
            observation=kwargs.get('observation',args[0] if args else None)
            before=cache();result=self.original['consume_backend_observation'](*args,**kwargs)
            if self.probes is not None:self.append('live_ifu',{'cycle':model.current_cycle,'phase':'monitor_callback_before_backend_drive_not_edge','values':self.probes.sample()})
            self.append('fromFtq_history',{'cycle':model.current_cycle,'phase':'native_backend_snapshot_cache_apply',
                'raw':asdict(observation),'cache_before':before,'cache_after':cache(),
                'frontend_snapshot_cycle':getattr(getattr(getattr(self.env,'monitor',None),'cycle_snapshot',None),'cycle',None),
                'write_generation_flag':None})
            return result
        def apply_recovery(*args,**kwargs):
            before=model._recovery_phase_active();target=model._current_recovery_target_pc()
            cursor=None if model.golden_trace is None else int(model.golden_trace.cursor)
            result=self.original['_apply_recovery_if_target_queued'](*args,**kwargs)
            if before and not model._recovery_phase_active():
                self.append('recovery_exits',{'cycle':model.current_cycle,'target_pc':target,'before_active':True,'after_active':False,
                    'cursor_before':cursor,'cursor_after':None if model.golden_trace is None else int(model.golden_trace.cursor),
                    'queue_after':[entry_record(e) for e in model._cfvec_queue if e.pc in (TAIL,RET,RETURN)]})
            return result
        def ready_resolves(*args,**kwargs):
            result=self.original['_ready_resolves_for_cycle'](*args,**kwargs)
            for e in result:
                self.append('resolve_plans',{'cycle':model.current_cycle,'entry':asdict(e),
                    'queue_entry':entry_record(model._cfvec_queue[e.queue_index]) if e.queue_index is not None and e.queue_index<len(model._cfvec_queue) else None})
            return result
        def capture_resolves(cycle,current_env):
            if not self.active:return
            signal=current_env.backend_to_ftq_if
            for ch in range(3):
                if int(signal.resolve_valid[ch].value):
                    self.append('resolve_bus',{'cycle':int(cycle),'phase':'post_drive_input_not_edge','channel':ch,
                        'ftq':[int(signal.resolve_bits_ftq_idx_flag[ch].value),int(signal.resolve_bits_ftq_idx_value[ch].value)],
                        'offset':int(signal.resolve_bits_ftq_offset[ch].value),'pc_addr_raw':int(signal.resolve_bits_pc_addr[ch].value),
                        'target_addr_raw':int(signal.resolve_bits_target_addr[ch].value),
                        'taken':int(signal.resolve_bits_taken[ch].value),'branch_type':int(signal.resolve_bits_attribute_branch_type[ch].value),
                        'ras_action':int(signal.resolve_bits_attribute_ras_action[ch].value)})
        def capture_redirect(cycle,current_env):
            if not self.active:return
            signal=current_env.backend_to_ftq_if
            if int(current_env.clock_reset.reset.value):return
            if int(signal.redirect_valid.value):
                self.append('redirects',{'cycle':int(cycle),'phase':'pre_drive_input',
                    'pc':int(signal.redirect_bits_pc.value),'target':int(signal.redirect_bits_target.value),
                    'ftq':[int(signal.redirect_bits_ftq_idx_flag.value),int(signal.redirect_bits_ftq_idx_value.value)],
                    'offset':int(signal.redirect_bits_ftq_offset.value),'level':int(signal.redirect_bits_level.value)})
        self.redirect_observer=capture_redirect
        self.env.register_pre_drive_cycle_observer(capture_redirect)
        self.env.register_cycle_observer(capture_resolves)
        model._sample_cfvec=sample
        model._queue_entry_matches_recovery_target=match
        model.commit_entry_driven=commit
        model.consume_backend_observation=consume
        model._apply_recovery_if_target_queued=apply_recovery
        model._ready_resolves_for_cycle=ready_resolves
    def restore(self):
        self.active=False
        for name in self.original:
            value=self.instance_original[name]
            if value is None:self.model.__dict__.pop(name,None)
            else:setattr(self.model,name,value)


def validate_observations(data):
    assert data['overflow'] is False,'observation buffer overflow'
    target_matches=[m for m in data['matches'] if m['entry']['pc']==TAIL and m['target_pc']==TAIL]
    assert target_matches,'natural redirect/recovery to the new tail target was not realized'
    redirects=[r for r in data['redirects'] if r['pc']==CALL and r['target']==TAIL and r['phase']=='pre_drive_input']
    assert redirects,'actual backend redirect CALL->tail was not observed'
    match=target_matches[0];entry=match['entry'];expected=match['expected_ftq']
    assert entry['instr']==0x001a0a13 and entry['is_rvc'] is False and entry['ftq_offset']==0 and entry['exception_bits']==0
    matching_redirects=[r for r in redirects if r['cycle']<=entry['cycle'] and ((r['ftq'][0]*64+r['ftq'][1]+1)%128)==(expected[0]*64+expected[1])]
    assert matching_redirects,'raw redirect identity is inconsistent with recovery expected FTQ'
    absolute=expected[0]*64+expected[1];following=(absolute+1)%128
    assert [entry['ftq_flag'],entry['ftq_value']]==[following//64,following%64],'wrong ending-half FTQ identity'
    for value,pc in [(expected[1],TAIL),(entry['ftq_value'],TAIL+2)]:
        source=match['fromFtq'][str(value)]
        assert source['start_pc']==pc and match['redirect_cycle']<source['write_cycle']<=entry['cycle'],'unproven fresh native FTQ starts'
    packets=[{'cycle':s['cycle'],**p} for s in data['snapshots'] for p in s['packets']]
    for pc,instr in [(TAIL,0x001a0a13),(RET,0x00008067),(RETURN,0x001a0a93)]:
        selected=[p for p in packets if p.get('processed_by_model') is True and p['pc']==pc and p['cycle']>=entry['cycle']]
        assert selected,'missing actual target/RET/continuation snapshot '+hex(pc)
        assert any(p['instr']==instr and p['foldpc']==fold(pc) and not p['is_rvc'] and p['exception_bits']==0 for p in selected),'bad bytes/fold/type/exception '+hex(pc)
    native=validate_native_chain(data,match)
    # Actual FTQ commit bus and popped queue span, not cursor-only completion.
    retired={}
    for commit in data['commits']:
        if commit['mode']!='queue':continue
        bus=commit['actual_bus']
        assert bus['valid']==1 and [bus['flag'],bus['value']]==commit['model_ptr'],'commit model/bus mismatch'
        for e in commit['entries']:
            if e['pc'] in (TAIL,RET,RETURN):
                assert [e['ftq_flag'],e['ftq_value']]==commit['model_ptr'],'retired entry is not in driven FTQ commit'
                assert e['exception_bits']==0 and commit['cycle']>=e['cycle']>=entry['cycle']
                expected={TAIL:(0x001a0a13,17),RET:(0x00008067,18),RETURN:(0x001a0a93,19)}[e['pc']]
                assert not e['is_rvc'] and e['instr']==expected[0] and e['path_state']=='correct' and e['golden_index']==expected[1],'committed instruction is not the exact correct reference instance'
                retired[e['pc']]=commit['cycle']
    assert set(retired)=={TAIL,RET,RETURN},'target/RET/continuation did not all commit'
    assert retired[RET]>=retired[TAIL] and retired[RETURN]>=retired[RET],'commit order differs'
    return {'case_id':CASE_ID,'target_recovery':match,'retired_cycles':{hex(k):v for k,v in retired.items()},
            'native_completion_chain':native,
            'proof':'native monitor snapshots, strict model identity and FTQ commit bus',
            'edge_lane_ready_proof':False,'canonical_HIT':False,'random_case_admission':False}


def validate_native_chain(data,match):
    """Section7 proof for this short low-address no-fault case; no generation claim."""
    d=match['redirect_cycle'];end=match['entry']['cycle'];expected=match['expected_ftq']
    history=[h for h in data['fromFtq_history'] if d<=h['cycle']<=end]
    assert [h['cycle'] for h in history]==list(range(d,end+1)),'incomplete redirect-to-target fromFtq history'
    for h in history:
        assert h['frontend_snapshot_cycle']==h['cycle'],'backend/cache observation is not same-cycle Frontend snapshot'
        raw=h['raw'];idx=str(raw['from_ftq_ftq_idx'])
        if raw['from_ftq_wen']:
            assert h['cache_after'][idx]=={'start_pc':(raw['from_ftq_start_pc_addr']<<1)&((1<<50)-1),'write_cycle':h['cycle']},'cache/timestamp differs from same-cycle native write'
        else:assert h['cache_before']==h['cache_after'],'cache changed without native wen'
        assert h['write_generation_flag'] is None,'fromFtq port does not expose generation flag'
    for value,pc in [(expected[1],TAIL),(match['entry']['ftq_value'],TAIL+2)]:
        writes=[h for h in history if h['raw']['from_ftq_wen'] and h['raw']['from_ftq_ftq_idx']==value and h['cycle']>d]
        assert len(writes)==1 and ((writes[0]['raw']['from_ftq_start_pc_addr']<<1)&((1<<50)-1))==pc,'physical slot reuse/override pairing requires separate analysis'
        assert history[-1]['cache_after'][str(value)]==match['fromFtq'][str(value)],'matcher cache differs from same-cycle history'
    assert match['accept_reason']=='fresh_start_pair_ending_half_next_ftq'
    exits=[e for e in data['recovery_exits'] if e['target_pc']==TAIL and e['cycle']>=end and e['before_active'] is True and e['after_active'] is False]
    assert any(e['cursor_before']<=17<e['cursor_after'] and any(q['pc']==TAIL and q['path_state']=='correct' and q['golden_index']==17 for q in e['queue_after']) for e in exits),'no proved recovery exit/golden target progress'
    live=[h for h in data['live_ifu'] if d<h['cycle']<=end]
    window_redirects=[r for r in data['redirects'] if d<r['cycle']<=end]
    assert len(window_redirects)==1 and window_redirects[0]['pc']==CALL and window_redirects[0]['target']==TAIL,'extra or missing redirect during this live recovery window'
    overrides=[]
    for h in live:
        v=h['values']
        assert all(v.get(k) is not None for k in ('ftq_override_valid','ftq_s2Override','ftq_s3Override')),'override history unobserved'
        if v['ftq_override_valid'] and (v['ftq_s2Override'] or v['ftq_s3Override']):overrides.append(h)
    s1=[];s2=[]
    for h in live:
        v=h['values']
        if v.get('s1_valid')==1 and v.get('s1_fire')==1 and v.get('s1_flush')==0 and v.get('s1_instrCount',0)>0 and v.get('s1_prevEndHalfRviInfo_valid')==1:
            if v.get('s1_prevEndHalfRviInfo_bits_pc_addr')==TAIL>>1 and v.get('s1_prevEndHalfRviInfo_bits_data')==0x0a13:
                for lane in range(36):
                    if v.get(f's1_alignedInstrPcVec_{lane}_addr')==TAIL>>1 and v.get(f's1_alignedInstrVec_{lane}_data')==0x001a0a13:s1.append(h)
        if v.get('s2_fire')==1 and v.get('s2_flush')==0 and v.get('io_toIBuffer_valid')==1 and v.get('io_toIBuffer_ready')==1:
            valid=v.get('io_toIBuffer_bits_valid');enq=v.get('io_toIBuffer_bits_enqEnable')
            if valid is None or enq is None:continue
            for lane in range(36):
                if not ((valid&enq)>>lane)&1:continue
                if v.get(f's2_alignedInstrPcVec_{lane}_addr')!=TAIL>>1 or v.get(f's2_alignedInstrVec_{lane}_data')!=0x001a0a13:continue
                select=v.get(f's2_alignedInstrVec_{lane}_blockSel');cross=v.get(f's2_alignedInstrVec_{lane}_isCrossBlockInstr')
                if select is None or cross is None:continue
                owner=int(bool(select or cross))
                ftq=[v.get(f's2_fetchBlock_{owner}_ftqIdx_flag'),v.get(f's2_fetchBlock_{owner}_ftqIdx_value')]
                if v.get(f's2_fetchBlock_{owner}_valid')==1 and ftq==[match['entry']['ftq_flag'],match['entry']['ftq_value']] and v.get(f's2_alignedInstrVec_{lane}_endOffset')==0:s2.append(h)
    assert any(a['cycle']<=b['cycle'] for a in s1 for b in s2),'missing valid live saved-half -> aligned s1/s2 -> IFU/IBuffer mask/owner chain'
    plans=[r for r in data['resolve_plans'] if r['entry']['inst_pc']==RET and r['entry']['target']==RETURN and r['entry']['branch_type']==3 and r['entry']['ras_action']==1]
    assert plans,'RET had no actual ready resolve to the returned PC'
    def actual_resolve(plan):
        e=plan['entry'];q=plan['queue_entry']
        if q is None or q['instr']!=0x8067 or q['path_state']!='correct' or q['golden_index']!=18 or q['exception_bits']!=0:return False
        return any(b['cycle']==plan['cycle'] and b['phase']=='post_drive_input_not_edge' and b['ftq']==[e['ftq_flag'],e['ftq_value']] and b['offset']==e['ftq_offset'] and b['target_addr_raw']==RETURN>>1 and b['pc_addr_raw']==e['pc']>>1 and b['branch_type']==3 and b['ras_action']==1 and b['taken']==1 for b in data['resolve_bus'])
    assert any(actual_resolve(r) for r in plans),'RET resolve plan was not actually driven with exact target/identity'
    assert data['fromFtq_generation_flag']['observed'] is False
    return {'history_cycles':[d,end],'full_write_history':history,'s1_saved_half_cycles':[h['cycle'] for h in s1],
        's2_masked_enqueue_cycles':[h['cycle'] for h in s2],'recovery_exits':exits,'RET_resolve_plans':plans,
        'override_samples':overrides,'all_window_redirects':window_redirects,
        'generation_flag_observed':False,'generation_proof':'UNOBSERVED: physical value/timestamp and live consumption chain only',
        'edge_phase_validation':'coord FST/calibration still required; callback != settled rising edge'}
