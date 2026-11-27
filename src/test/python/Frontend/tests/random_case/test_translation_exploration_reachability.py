"""Real public Python model/agent gates; no DUT, NEMU or generated corpus."""
from dataclasses import replace
from types import SimpleNamespace

import pytest

from env.agents.ptw_agent import PTWAgent
from env.core.backend_model import BackendModel
from env.model.page_table_model import PageTableModel
from env.monitors.frontend_monitor import PTWAgentInputSnapshot
from env.sequences import TranslationScenarioBuilder

from .translation_exploration_plan import TranslationExplorationPlan


class _Signal:
    def __init__(self): self.value=0


class _SignalArray:
    def __init__(self): self.signals=[_Signal() for _ in range(8)]
    def __iter__(self): return iter(self.signals)


def _interface():
    # Agent-only output pins, no DUT object and no hidden test-module dependency.
    table=PageTableModel()
    names=['req_0_ready','resp_valid']
    names += ['resp_bits_'+key for key in table.build_ptw_resp(0)
              if key not in {'s1_ppn_low','s1_valididx','s1_pteidx','get_gpa',
                             'memidx_is_ld','memidx_is_st','memidx_idx'}]
    interface=SimpleNamespace(**{name:_Signal() for name in names})
    for name in ['s1_ppn_low','s1_valididx','s1_pteidx']:
        setattr(interface,'resp_bits_'+name,_SignalArray())
    return interface


def _snapshot(cycle,*,request=0,stage=0,get_gpa=0,fence=0,ready=0,request_ready=1):
    return PTWAgentInputSnapshot(cycle=cycle,sfence_valid=fence,req_valid=request,req_ready=request_ready,
        req_vpn=0x80000,req_s2xlate=stage,req_get_gpa=get_gpa,resp_ready=ready,context={})


def _agent(keep=False):
    table=PageTableModel('sv39')
    table.map_page(0x80000,0x80400)
    table.map_stage2_page(0x80000,0x80800)
    table.map_stage2_page(0x80400,0x80c00)
    a=PTWAgent(table);a.interface=_interface()
    a.configure(latency=3,flush_pending_on_sfence=not keep,response_source='model')
    events=[];a.set_event_sink(events.append)
    return a,events


@pytest.mark.parametrize('mode',['stage1','only_stage1','only_stage2','all_stage'])
@pytest.mark.parametrize('pbmt',[0,1,2])
def test_explicit_model_modes_pbmt_and_context_remain_model_only(mode,pbmt):
    plan=TranslationExplorationPlan(12,34,mode=mode,asid=3,asid_bits=16,vmid=0 if mode=='stage1' else 2,
        vmid_bits=4,s1_pbmt=pbmt if mode=='only_stage1' or mode=='stage1' else 0,
        s2_pbmt=pbmt if mode in ('only_stage2','all_stage') else 0,
        ptw_ready_high=2,ptw_ready_low=3)
    scenario=plan.model_scenario(b'\x13\x00\x00\x00')
    validator=TranslationScenarioBuilder(SimpleNamespace(ptw_agent=PTWAgent(PageTableModel())))
    validator.validate(scenario)
    assert scenario.s2xlate=={'stage1':0,'only_stage1':1,'only_stage2':2,'all_stage':3}[mode]
    assert scenario.ptw_req_ready_strategy=='periodic'
    assert plan.admission()['long_golden_trace_admitted'] is False
    assert plan.admission()['runtime_reachability']=='UNCONFIRMED'
    assert scenario==plan.model_scenario(b'\x13\x00\x00\x00')


@pytest.mark.parametrize('fault',['s1_pf','s1_af','s2_gpf','s2_gaf'])
def test_fault_plans_never_enter_unmodified_long_golden_path(fault):
    plan=TranslationExplorationPlan(12,34,mode='all_stage',vmid=2,vmid_bits=4,fault=fault)
    scenario=plan.model_scenario(b'\x13\x00\x00\x00')
    TranslationScenarioBuilder(SimpleNamespace(ptw_agent=PTWAgent(PageTableModel()))).validate(scenario)
    assert getattr(scenario,fault)==1 and scenario.expected_path=='fault'
    assert plan.admission()['long_golden_trace_admitted'] is False


@pytest.mark.parametrize('change',[{'vmid_bits':None},{'vmid':16},{'mode':'stage1','vmid':1},
 {'s1_pbmt':1},{'ptw_ready_high':0},{'program_seed':True},{'fault':'s1_pf'},
 {'asid':1},{'asid':65536,'asid_bits':16}])
def test_plan_rejects_unreviewed_identity_or_no_progress(change):
    plan=TranslationExplorationPlan(12,34,mode='only_stage2',vmid=2,vmid_bits=4)
    with pytest.raises(ValueError):replace(plan,**change).validate()


def test_default_fence_drops_pending_so_late_case_is_not_reachable_in_default_agent():
    agent,events=_agent()
    agent.on_clock_edge(1,_snapshot(1,request=1))
    assert len(agent.pending)==1
    agent.on_clock_edge(2,_snapshot(2,fence=1))
    for cycle in range(3,8):agent.on_clock_edge(cycle,_snapshot(cycle,ready=1))
    assert agent.sfence_dropped_responses==1 and agent.resp_count==0
    assert not [e for e in events if e['type']=='handshake.ptw_resp']
    assert [e for e in events if e['type']=='control.ptw_sfence_flush'][0]['payload']['dropped']==1


def test_explicit_retention_delivers_old_identity_after_fence_at_agent_output_only():
    agent,events=_agent(keep=True)
    agent.set_request_context_provider(lambda raw:{'satp_asid':5,'translation_epoch':1})
    agent.on_clock_edge(1,_snapshot(1,request=1,stage=1,get_gpa=1))
    captured=agent.pending[0].snapshot
    agent.on_clock_edge(2,_snapshot(2,fence=1))
    agent.on_clock_edge(4,_snapshot(4,ready=0))
    assert agent.interface.resp_valid.value==1
    drive=agent.get_active_drive_expectation()
    assert (drive['sequence_id'],drive['s2xlate'],drive['get_gpa'])==(1,1,1)
    assert captured.extra=={'satp_asid':5,'translation_epoch':1}
    agent.on_clock_edge(5,_snapshot(5,ready=1))
    response=[e for e in events if e['type']=='handshake.ptw_resp']
    assert len(response)==1 and response[0]['cycle']==5
    assert response[0]['payload']['sequence_id']==1
    assert agent.sfence_dropped_responses==0
    # Pure agent output reachability; no TLB/filter/consumer correctness is claimed.


@pytest.mark.parametrize('stage,get_gpa',[(0,0),(1,1),(2,0),(3,1)])
def test_same_vpn_real_request_identity_is_preserved(stage,get_gpa):
    agent,events=_agent()
    agent.on_clock_edge(1,_snapshot(1,request=1,stage=stage,get_gpa=get_gpa))
    agent.on_clock_edge(4,_snapshot(4,ready=1))
    request=next(e for e in events if e['type']=='handshake.ptw_req')
    response=next(e for e in events if e['type']=='handshake.ptw_resp')
    for field in ('vpn','s2xlate','get_gpa','sequence_id'):
        assert request['payload'][field]==response['payload'][field]


@pytest.mark.parametrize('field',['s2xlate','get_gpa','memidx_is_ld','memidx_is_st','memidx_idx'])
def test_normal_response_override_never_changes_request_identity(field):
    agent,_=_agent()
    with pytest.raises(ValueError,match='cannot change request identity'):
        agent.set_response_overrides([{'vpn':0x80000,'s2xlate':1,'get_gpa':1,'patch':{field:2}}])


@pytest.mark.parametrize('pbmt,cacheable,expected',[(0,True,False),(0,False,True),(1,True,True),(2,True,True)])
def test_new_backend_onlystage2_active_context_uses_stage2_pa_pbmt(pbmt,cacheable,expected):
    table=PageTableModel('bare');va=0x80000000;pa=0x80400000
    table.map_stage2_page(va>>12,pa>>12,pbmt=pbmt)
    model=BackendModel();lookups=[]
    model.env=SimpleNamespace(page_table=table,
        translation_oracle=SimpleNamespace(get_active=lambda:{'expected_ptw_request':{'s2xlate':2}}),
        pma_cacheable_for_addr=lambda address:lookups.append(address) or cacheable)
    assert model._cfvec_uses_uncache_fetch(va) is expected
    if pbmt==0:assert lookups==[pa]
    else:assert lookups==[]


def test_onlystage2_model_context_is_not_inferred_from_bare_mode_alone():
    table=PageTableModel('bare');table.map_stage2_page(0x80000,0x80400,pbmt=1)
    model=BackendModel();model.env=SimpleNamespace(page_table=table,
        translation_oracle=SimpleNamespace(get_active=lambda:None),pma_cacheable_for_addr=lambda address:True)
    assert model._cfvec_uses_uncache_fetch(0x80000000) is False


@pytest.mark.parametrize('fault',['gpf','gaf'])
def test_onlystage2_fault_then_same_vpn_recovery_keeps_identity_in_model(fault):
    agent,_=_agent()
    agent.page_table.set_stage2_response_fault(0x80000,guest_page_fault=int(fault=='gpf'),guest_access_fault=int(fault=='gaf'))
    old=agent.page_table.build_ptw_resp(0x80000,s2xlate=2,get_gpa=1)
    assert old['s2xlate']==2 and old['get_gpa']==1
    assert old['s2_'+fault]==1
    agent.page_table.set_stage2_response_fault(0x80000,guest_page_fault=0,guest_access_fault=0)
    recovered=agent.page_table.build_ptw_resp(0x80000,s2xlate=2,get_gpa=1)
    assert recovered['s2_gpf']==recovered['s2_gaf']==0
    assert recovered['s2_entry_ppn']==0x80800
    assert recovered['s2xlate']==old['s2xlate'] and recovered['get_gpa']==old['get_gpa']
    assert old['s2_'+fault]==1  # Already-built response does not mutate in place.


def test_periodic_request_ready_blocks_then_accepts_a_real_request_snapshot():
    agent,events=_agent()
    agent.configure(latency=1,req_ready_strategy='periodic',req_ready_high_cycles=1,req_ready_low_cycles=2)
    agent.on_clock_edge(1,_snapshot(1,request=1,request_ready=0))
    agent.on_clock_edge(2,_snapshot(2,request=1,request_ready=0))
    assert agent.req_count==0 and agent.req_blocked_cycles==2
    agent.on_clock_edge(3,_snapshot(3,request=1,request_ready=0))
    agent.on_clock_edge(4,_snapshot(4,request=1,request_ready=1))
    agent.on_clock_edge(5,_snapshot(5,ready=1))
    assert agent.req_count==agent.resp_count==1
    assert [e['cycle'] for e in events if e['type']=='handshake.ptw_req']==[4]
    assert [e['cycle'] for e in events if e['type']=='handshake.ptw_resp']==[5]


def test_pending_snapshot_preserves_asid_vmid_epoch_during_context_reuse():
    agent,_=_agent(keep=True)
    context={'satp_asid':3,'hgatp_vmid':2,'translation_epoch':4}
    agent.set_request_context_provider(lambda raw:dict(context))
    agent.on_clock_edge(1,_snapshot(1,request=1,stage=3))
    old=agent.pending[0]
    context.update(satp_asid=5,hgatp_vmid=6,translation_epoch=5)
    agent.on_clock_edge(2,_snapshot(2,request=1,stage=1))
    new=agent.pending[1]
    assert old.vpn==new.vpn and old.s2xlate!=new.s2xlate
    assert old.snapshot.extra=={'satp_asid':3,'hgatp_vmid':2,'translation_epoch':4}
    assert new.snapshot.extra=={'satp_asid':5,'hgatp_vmid':6,'translation_epoch':5}
    assert old.snapshot.sequence_id!=new.snapshot.sequence_id
    # Model maps are VPN keyed; this proves snapshot retention, NOT concurrent ASID/VMID lookup semantics.


def test_model_identity_binds_full_plan_and_payload_not_just_seeds():
    plan=TranslationExplorationPlan(12,34,mode='only_stage2',vmid=2,vmid_bits=4)
    original=plan.model_scenario(b'\x13\x00\x00\x00')
    pbmt=replace(plan,s2_pbmt=1).model_scenario(b'\x13\x00\x00\x00')
    context=replace(plan,vmid=3).model_scenario(b'\x13\x00\x00\x00')
    payload=plan.model_scenario(b'\x13\x00\x00\x00'*2)
    assert len({s.scenario_id for s in (original,pbmt,context,payload)})==4
    assert plan.admission()['plan_sha256']!=replace(plan,s2_pbmt=1).admission()['plan_sha256']
