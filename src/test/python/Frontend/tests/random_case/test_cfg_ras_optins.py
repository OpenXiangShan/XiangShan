"""Offline opt-in construction gates. Temporary programs are not campaign corpus."""
from dataclasses import replace
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

import pytest

from .generate_random_asm import CaseSpec,generate_case,site8_pattern
from .program_map import build_program_map,analyze_trace
from .scenario_profiles import build_scenario_spec
from .testpoint_catalog import build_catalog,DEFAULT_SOURCE
from ._bounded_program_model import execute


@pytest.mark.parametrize('index',range(6))
def test_frozen_default_source_and_manifest_fields_unchanged(index):
    fixture=json.loads((Path(__file__).parent/'fixtures/default_generator_optin_compatibility.json').read_text())
    record=fixture['cases'][index]
    source,metadata=generate_case(CaseSpec(**record['config']));metadata.pop('generator_sha256')
    assert hashlib.sha256(source.encode()).hexdigest()==record['source_sha256']
    assert hashlib.sha256(json.dumps(metadata,sort_keys=True).encode()).hexdigest()==record['metadata_without_generator_sha256']
    assert 'branch_history_mode' not in metadata['spec'] and 'ras_return_mode' not in metadata['spec']


@pytest.mark.parametrize('family,kwargs',[('cfg_layout_mix',{'ras_return_mode':'shared_leaf'}),
 ('ras_indirect_history',{'branch_history_mode':'site8'}),('ibuffer_pressure_cross',{'branch_history_mode':'site8'}),
 ('cfg_layout_mix',{'branch_history_mode':'site8','iterations':7}),('cfg_layout_mix',{'branch_history_mode':'arbitrary'}),
 ('ras_indirect_history',{'ras_return_mode':'arbitrary'})])
def test_illegal_modes_or_insufficient_period_rejected(family,kwargs):
    with pytest.raises(ValueError):CaseSpec(family=family,seed=0xA55A,**kwargs).validate()


@pytest.mark.parametrize('family,kwargs',[('cfg_layout_mix',{'branch_history_mode':'site8'}),
 ('ras_indirect_history',{'ras_return_mode':'shared_leaf'}),
 ('translation_timing_cross',{'branch_history_mode':'site8','indirect_target_mode':'history4'})])
def test_optins_deterministic_fully_serialized_and_named(family,kwargs):
    spec=CaseSpec(family=family,seed=0xA55A,blocks=16,iterations=8,max_call_depth=4,**kwargs)
    source,metadata=generate_case(spec)
    assert (source,metadata)==generate_case(spec)
    assert generate_case(CaseSpec(**metadata['spec']))==(source,metadata)
    assert spec.name!=replace(spec,branch_history_mode='loop_mask',ras_return_mode='private').name
    scenario=build_scenario_spec(metadata,timing_seed=34,testpoint_catalog=build_catalog(DEFAULT_SOURCE))
    assert scenario.parameters['generator_parameters']==metadata['spec']
    expected='trace.shared_return_distinct_successors' if spec.ras_return_mode=='shared_leaf' else 'trace.site8_branch_sites_with_both_successors'
    assert expected in {r.event for r in scenario.required_events}


def test_site_patterns_are_per_site_and_layout_rng_independent():
    assert 0<site8_pattern(0xA55A,0,0)<255
    assert len({site8_pattern(0xA55A,0,i) for i in range(16)})>1
    # This proves deterministic different values in this fixed test, not statistical independence.
    assert site8_pattern(0xA55A,0,0)==site8_pattern(0xA55A,0,0)


TOOLS=('riscv64-unknown-elf-gcc','riscv64-unknown-elf-objcopy','riscv64-unknown-elf-objdump')


def _linked(tmp_path,spec):
    source,metadata=generate_case(spec)
    asm,elf,binary=(tmp_path/name for name in ('case.S','case.elf','case.bin'))
    asm.write_text(source)
    subprocess.run([TOOLS[0],'-march=rv64gc_zicsr_zifencei','-mabi=lp64','-static','-no-pie','-nostdlib','-nostartfiles',
        '-Wl,--build-id=none','-Wl,-Ttext=0x80000000','-Wl,-e,_start','-o',str(elf),str(asm)],check=True,capture_output=True)
    subprocess.run([TOOLS[1],'-O','binary','-j','.text',str(elf),str(binary)],check=True,capture_output=True)
    pm=build_program_map(asm,binary,metadata,link_address=0x80000000,binary_base=0x80000000)
    model=execute(pm)
    trace=tmp_path/'model.trace.jsonl';trace.write_text(''.join(json.dumps(row)+'\n' for row in model['trace']))
    return metadata,pm,model,analyze_trace(trace,pm)


@pytest.mark.skipif(any(shutil.which(t) is None for t in TOOLS),reason='requires RISC-V assembler/ELF tools')
@pytest.mark.parametrize('family,kwargs',[('cfg_layout_mix',{'branch_history_mode':'site8'}),
 ('cfg_layout_mix',{'branch_history_mode':'site8','indirect_target_mode':'history4'}),
 ('translation_timing_cross',{'branch_history_mode':'site8','directed_pattern':'page_tail_pair_sparse'}),
 ('ras_indirect_history',{'ras_return_mode':'shared_leaf'})])
def test_actual_linked_optins_terminate_and_exercise_actual_required_paths(tmp_path,family,kwargs):
    spec=CaseSpec(family=family,seed=0xA55A,blocks=16,iterations=8,max_call_depth=4,**kwargs)
    metadata,pm,model,analysis=_linked(tmp_path,spec)
    assert model['terminated'] and model['registers'][10]==0 and model['registers'][8]==0
    assert model['registers'][2]==0x80020000
    assert analysis['missing_program_map_pc_count']==0
    events={e['event']:e['count'] for e in analysis['events']}
    if spec.branch_history_mode=='site8':
        scheduled=[e for e in metadata['episodes'] if e['parameters'].get('branch_history_mode')=='site8']
        assert events['trace.site8_branch_sites_with_both_successors']==len(scheduled)>0
        actual={pc:[] for pc in [pm['labels'][e['parameters']['branch_history_label']] for e in scheduled]}
        rows=model['trace']
        for a,b in zip(rows,rows[1:]):
            pc=int(a['pc'],0)
            if pc in actual:actual[pc].append(int(b['pc'],0)==pc+a['size'])
        for e in scheduled:
            pattern=e['parameters']['branch_history_pattern'];pc=pm['labels'][e['parameters']['branch_history_label']]
            assert actual[pc]==[bool(pattern>>(iteration&7)&1) for iteration in range(8,0,-1)]
    else:
        pc=pm['labels']['ras_shared_return'];rows=model['trace']
        targets={int(b['pc'],0) for a,b in zip(rows,rows[1:]) if int(a['pc'],0)==pc}
        assert len(targets)==spec.blocks
        assert events['trace.shared_return_distinct_successors']==spec.blocks
        assert sum(int(r['pc'],0)==pc for r in rows)==spec.blocks*spec.iterations


def test_realization_gates_use_actual_successors_not_declared_targets(tmp_path):
    pm={'binary_sha256':'synthetic','instructions':[
      {'pc':0x1000,'size':4,'conditional_branch':True,'labels':['site'],'operands':'x1,x0,1010',
       'episode_parameters':{'branch_history_mode':'site8','branch_history_label':'site'}},
      {'pc':0x1004,'size':4},{'pc':0x1010,'size':4}]}
    trace=tmp_path/'trace'
    trace.write_text(''.join(json.dumps({'index':n,'pc':hex(pc),'taken':bool(n%2),'target_pc':'0x1010'})+'\n'
                            for n,pc in enumerate([0x1000,0x1004,0x1000,0x1004])))
    analysis=analyze_trace(trace,pm)
    assert next(e['count'] for e in analysis['events'] if e['event']=='trace.site8_branch_sites_with_both_successors')==0


def test_bounded_model_rejects_nonterminating_and_uninitialized_stack():
    pm={'labels':{'_start':0x1000},'instructions':[{'pc':0x1000,'size':4,'encoding':'0000006f','mnemonic':'jal','operands':'x0,1000'}]}
    with pytest.raises(ValueError,match='failed to terminate'):execute(pm,max_steps=5)
    pm['instructions'][0].update(encoding='00013083',mnemonic='ld',operands='x1,0(x2)')
    with pytest.raises(ValueError,match='uninitialized'):execute(pm,max_steps=5)


@pytest.mark.parametrize('family,flag,value',[
    ('cfg_layout_mix','--branch-history-mode','site8'),
    ('ras_indirect_history','--ras-return-mode','shared_leaf'),
])
def test_generator_and_campaign_cli_roundtrip_and_replay(family,flag,value,monkeypatch,tmp_path):
    import ast
    import shlex
    from . import generate_random_asm as generator
    from . import run_random_campaign as runner
    monkeypatch.setattr(sys,'argv',['generator','--family',family,'--seed','0xa55a',flag,value,'--out-dir',str(tmp_path/'unused')])
    parsed=generator._parse_args()
    field=flag[2:].replace('-','_')
    assert getattr(parsed,field)==value
    monkeypatch.setattr(sys,'argv',['campaign','--family',family,'--master-seed','0xa55a',flag,value,'--output-root',str(tmp_path/'unused')])
    assert getattr(runner._parse_args(),field)==value
    spec=CaseSpec(family=family,seed=0xA55A,**{field:value}).to_dict()
    # Execute only the production replay-string assignment; never generate or run a case.
    tree=ast.parse(Path(runner.__file__).read_text())
    assignment=next(n for n in ast.walk(tree) if isinstance(n,ast.Assign) and any(
        isinstance(t,ast.Subscript) and isinstance(t.value,ast.Name) and t.value.id=='record'
        and isinstance(t.slice,ast.Constant) and t.slice.value=='generator_replay_command' for t in n.targets))
    record={}
    exec(compile(ast.Module(body=[assignment],type_ignores=[]),'<production replay assignment>','exec'),{'record':record,'spec':spec,'Path':Path,'__file__':runner.__file__})
    words=shlex.split(record['generator_replay_command'])
    assert words[words.index(flag)+1]==value
    monkeypatch.setattr(sys,'argv',words[1:])
    assert getattr(generator._parse_args(),field)==value
    assert not (tmp_path/'unused').exists()


@pytest.mark.parametrize('flags',[
    ['--branch-history-mode','site8'], # default all families is incompatible
    ['--family','cfg_layout_mix','--ras-return-mode','shared_leaf'],
    ['--family','cfg_layout_mix','--branch-history-mode','site8','--iterations','7'],
])
def test_campaign_rejects_incompatible_modes_before_artifact_creation(tmp_path,monkeypatch,flags):
    from . import run_random_campaign as runner
    out=tmp_path/'must-not-exist'
    monkeypatch.setattr(sys,'argv',['campaign','--master-seed','0xa55a','--output-root',str(out),*flags])
    monkeypatch.setattr(runner,'write_cases',lambda *a,**kw:pytest.fail('must reject before generation'))
    with pytest.raises(SystemExit,match='invalid generator configuration'):runner.main()
    assert not out.exists()


@pytest.mark.parametrize('parameters,expected',[
    ({},('loop_mask','private')),
    ({'branch_history_mode':'site8'},('site8','private')),
    ({'ras_return_mode':'shared_leaf'},('loop_mask','shared_leaf')),
])
def test_resume_restores_frozen_modes_before_any_execution(tmp_path,monkeypatch,parameters,expected):
    from . import run_random_campaign as runner
    family='ras_indirect_history' if expected[1]=='shared_leaf' else 'cfg_layout_mix'
    scenario=tmp_path/'frozen-scenario.json';scenario.write_text('{}')
    manifest={'master_seed':0xA55A,'timing_master_seed':34,'mode':'generate','families':[family],
              'parameters':parameters,'cases':[{'name':'synthetic-test-only','scenario_spec_path':str(scenario)}]}
    (tmp_path/'campaign.json').write_text(json.dumps(manifest))
    monkeypatch.setattr(sys,'argv',['campaign','--resume','--output-root',str(tmp_path),
        '--branch-history-mode','site8','--ras-return-mode','shared_leaf'])
    args=runner._parse_args();monkeypatch.setattr(runner,'_parse_args',lambda:args)
    class StopBeforeExecution(Exception):pass
    def stop(_):raise StopBeforeExecution()
    monkeypatch.setattr(runner,'scenario_spec_from_dict',stop)
    monkeypatch.setattr(runner,'write_cases',lambda *a,**kw:pytest.fail('resume must not generate'))
    with pytest.raises(StopBeforeExecution):runner.main()
    assert (args.branch_history_mode,args.ras_return_mode)==expected
    assert json.loads((tmp_path/'campaign.json').read_text())==manifest


def test_optin_path_gates_reject_unlinked_branch_and_return_successors(tmp_path):
    instructions=[{'pc':0x1000,'size':4,'conditional_branch':True,'labels':['site'],
        'operands':'x1,x0,1010 <linked_target>',
        'episode_parameters':{'branch_history_mode':'site8','branch_history_label':'site'}},
        {'pc':0x1004,'size':4},{'pc':0x1010,'size':4},{'pc':0x1020,'size':4}]
    pm={'binary_sha256':'test-only','instructions':instructions}
    trace=tmp_path/'trace'
    trace.write_text(''.join(json.dumps({'pc':hex(pc)})+'\n' for pc in [0x1000,0x1004,0x1000,0x1020]))
    result=analyze_trace(trace,pm)
    assert next(e['count'] for e in result['events'] if e['event']=='trace.site8_branch_sites_with_both_successors')==0
    instructions[0]={'pc':0x1000,'size':4,'indirect_jump':True,'return':True,'labels':['ret'],
        'episode_kind':'shared_return_contexts','episode_parameters':{'return_label':'ret','return_context_labels':['legal']}}
    pm['labels']={'legal':0x1010}
    result=analyze_trace(trace,pm)
    assert next(e['count'] for e in result['events'] if e['event']=='trace.shared_return_distinct_successors')==0
    trace.write_text(json.dumps({'pc':'0x1000','target_pc':'0x1010'})+'\n')
    result=analyze_trace(trace,pm)
    assert next(e['count'] for e in result['events'] if e['event']=='trace.shared_return_distinct_successors')==0


@pytest.mark.skipif(any(shutil.which(t) is None for t in TOOLS),reason='requires RISC-V assembler/ELF tools')
@pytest.mark.parametrize('family,kwargs',[
    ('cfg_layout_mix',{'branch_history_mode':'site8'}),
    ('ras_indirect_history',{'ras_return_mode':'shared_leaf'}),
])
def test_same_seed_control_and_optin_program_space_comparison(tmp_path,family,kwargs,record_property):
    base=CaseSpec(family=family,seed=0xA55A,blocks=16,iterations=8,max_call_depth=4)
    results=[]
    for name,spec in [('control',base),('candidate',replace(base,**kwargs))]:
        out=tmp_path/name;out.mkdir()
        metadata,pm,model,analysis=_linked(out,spec)
        assert model['terminated'] and model['registers'][2]==0x80020000
        assert model['registers'][8]==0 and model['registers'][10]==0
        branches={}
        targets={}
        by_pc={i['pc']:i for i in pm['instructions']}
        for a,b in zip(model['trace'],model['trace'][1:]):
            pc,nxt=int(a['pc'],0),int(b['pc'],0);ins=by_pc[pc]
            if ins.get('conditional_branch'):branches.setdefault(pc,[]).append(nxt!=pc+ins['size'])
            if ins.get('return'):targets.setdefault(pc,set()).add(nxt)
        summary={'dynamic_instructions':len(model['trace']),'binary_bytes':(out/'case.bin').stat().st_size,
                 'conditional_sites':len(branches),'distinct_branch_sequences':len({tuple(s) for s in branches.values()}),
                 'max_return_successors':max(map(len,targets.values()),default=0),
                 'max_stack_bytes':0x80020000-model['minimum_sp'],
                 'source_sha256':hashlib.sha256((out/'case.S').read_bytes()).hexdigest(),
                 'binary_sha256':pm['binary_sha256'],'model_terminated':True}
        results.append(summary)
    if family=='cfg_layout_mix':assert results[1]['distinct_branch_sequences']>results[0]['distinct_branch_sequences']
    else:assert results[0]['max_return_successors']==1 and results[1]['max_return_successors']==base.blocks
    assert results[0]['max_stack_bytes']==results[1]['max_stack_bytes']
    record_property('bounded_program_comparison',json.dumps({'family':family,'program_seed':base.seed,'ordinal':base.ordinal,
        'blocks':base.blocks,'iterations':base.iterations,'max_call_depth':base.max_call_depth,
        'control':results[0],'candidate':results[1],'timing_seed':'not_run_no_cycle_model',
        'evidence_kind':'test_only_subset_execution_not_runtime_qualification'}))
