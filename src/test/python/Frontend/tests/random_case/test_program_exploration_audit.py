"""Pure synthetic prefixes; no generator, NEMU, DUT or corpus output."""
import copy
import hashlib
import json

import pytest

from . import program_exploration_audit as audit
from .generate_random_asm import FAMILIES


def _write(path, payload):
    path.write_text(json.dumps(payload))
    return audit._sha(path)


def _map():
    return {'instructions': [
        {'pc': 0x1000, 'size': 4, 'encoding': '00051063', 'mnemonic': 'beq', 'operands': 'x10,x0,1008'},
        {'pc': 0x1004, 'size': 4, 'encoding': '00000013', 'mnemonic': 'addi', 'operands': 'x0,x0,0'},
        {'pc': 0x1008, 'size': 4, 'encoding': '00030067', 'mnemonic': 'jalr', 'operands': 'x0,0(x6)'},
        {'pc': 0x100c, 'size': 4, 'encoding': '00000013', 'mnemonic': 'addi', 'operands': 'x0,x0,0'},
    ]}


def _trace(path, pm, pcs):
    by_pc = {i['pc']: i for i in pm['instructions']}
    rows = [{'index': n, 'pc': hex(pc), 'size': by_pc[pc]['size'],
             'instr': hex(int(by_pc[pc]['encoding'], 16)), 'taken': False,
             'target_pc': '0xdeadbeef'} for n, pc in enumerate(pcs)]
    path.write_text(''.join(json.dumps(row)+'\n' for row in rows))
    return rows


def test_repeating_trace_plateaus_observed_space_without_bug_or_time_claim(tmp_path):
    pm, trace = _map(), tmp_path/'trace'
    _trace(trace, pm, [0x1000, 0x1004, 0x1008, 0x100c]*12)
    r = audit.analyze_program_prefixes(trace, pm, instruction_budgets=[20, 32, 48], history_bits=2)
    first, middle, last = r['prefix_curve']
    assert first['unique_pcs'] == last['unique_pcs'] == 4
    assert first['branch_history_outcome_combinations'] == last['branch_history_outcome_combinations'] == 1
    assert middle['unique_cfi_edges'] == last['unique_cfi_edges'] == 2
    assert last['instructions_since_new_observation'] > first['instructions_since_new_observation']
    assert r['indirect_sites_with_multiple_successors'] == 0
    assert r['elapsed_time'] == r['dut_coverage'] == 'unmeasured'


def test_new_actual_successor_adds_history_edge_and_indirect_space(tmp_path):
    pm, trace = _map(), tmp_path/'trace'
    _trace(trace, pm, [0x1000, 0x1004, 0x1008, 0x100c]*3 +
                    [0x1000, 0x1008, 0x1004, 0x1000, 0x1008, 0x100c])
    r = audit.analyze_program_prefixes(trace, pm, instruction_budgets=[12, 18], history_bits=2)
    a, b = r['prefix_curve']
    assert b['unique_pcs'] == a['unique_pcs']
    assert b['unique_cfi_edges'] > a['unique_cfi_edges']
    assert b['branch_history_outcome_combinations'] > a['branch_history_outcome_combinations']
    assert r['indirect_sites_with_multiple_successors'] == 1
    assert r['branch_successor_relations']['nonfallthrough_successor'] == 2


def test_prefix_does_not_use_future_instruction_for_last_cfi(tmp_path):
    pm, trace = _map(), tmp_path/'trace'
    _trace(trace, pm, [0x1008, 0x100c])
    r = audit.analyze_program_prefixes(trace, pm, instruction_budgets=[1, 2])
    assert r['prefix_curve'][0]['unique_cfi_edges'] == 0
    assert r['prefix_curve'][1]['unique_cfi_edges'] == 1


@pytest.mark.parametrize('change', ['index_gap','missing_index','boolean_size','wrong_width','wrong_instr','unknown_pc','bad_json'])
def test_broken_trace_never_becomes_measured_growth(tmp_path, change):
    pm, trace = _map(), tmp_path/'trace'
    rows = _trace(trace, pm, [0x1000, 0x1004])
    if change == 'index_gap': rows[1]['index'] = 3
    elif change == 'missing_index': del rows[0]['index']
    elif change == 'boolean_size': rows[0]['size'] = True
    elif change == 'wrong_width': rows[0]['size'] = 2
    elif change == 'wrong_instr': rows[0]['instr'] = '0x13'
    elif change == 'unknown_pc': rows[0]['pc'] = '0xdeadbeef'
    trace.write_text('invalid' if change == 'bad_json' else ''.join(json.dumps(row)+'\n' for row in rows))
    with pytest.raises(ValueError): audit.analyze_program_prefixes(trace, pm)


@pytest.mark.parametrize('budgets', [[0], [True], [2,1], [1,1], [3]])
def test_prefix_budgets_do_not_extrapolate(tmp_path, budgets):
    pm, trace = _map(), tmp_path/'trace'
    _trace(trace, pm, [0x1000,0x1004])
    with pytest.raises(ValueError): audit.analyze_program_prefixes(trace, pm, instruction_budgets=budgets)


@pytest.mark.parametrize('change', ['empty','overlap','unaligned','bad_encoding','missing_disassembly'])
def test_invalid_map_fails_closed(tmp_path, change):
    pm, trace = _map(), tmp_path/'trace'
    _trace(trace, pm, [0x1000])
    if change == 'empty': pm['instructions'] = []
    elif change == 'overlap': pm['instructions'][1]['pc'] = 0x1002
    elif change == 'unaligned': pm['instructions'][0]['pc'] += 1
    elif change == 'bad_encoding': pm['instructions'][0]['encoding'] = 'unknown!'
    elif change == 'missing_disassembly': del pm['instructions'][0]['mnemonic']
    with pytest.raises(ValueError): audit.analyze_program_prefixes(trace, pm)


@pytest.fixture
def input_set(tmp_path):
    pm = _map()
    binary = b''.join(bytes.fromhex(i['encoding'])[::-1] for i in pm['instructions'])
    (tmp_path/'source').write_text('synthetic disassembly fixture, never a campaign')
    (tmp_path/'bin').write_bytes(binary)
    _trace(tmp_path/'trace', pm, [0x1000,0x1004,0x1008,0x100c]*3)
    pm.update(binary_text_verified=True,binary_base=0x1000,text_offset_in_binary=0,text_size_bytes=len(binary),
              text_sha256=hashlib.sha256(binary).hexdigest(),source_sha256=audit._sha(tmp_path/'source'),
              binary_sha256=audit._sha(tmp_path/'bin'))
    _write(tmp_path/'program_map', pm)
    scenario = {'scenario_id':'synthetic','family':'cfg_layout_mix','program_seed':2**64-1,'timing_seed':9}
    _write(tmp_path/'scenario_spec',scenario)
    index = {'schema_version':1,'case_name':'synthetic','family':'cfg_layout_mix',
             'program_seed':2**64-1,'timing_seed':9,
             'inputs':{k:{'path':k,'sha256':audit._sha(tmp_path/k)} for k in ('source','bin','trace','program_map','scenario_spec')}}
    _write(tmp_path/'index.json',index)
    return tmp_path,index


@pytest.mark.parametrize('family',FAMILIES)
def test_all_four_families_preserve_inputs_and_scope(input_set,family):
    root,index=input_set
    index['family']=family
    scenario=json.loads((root/'scenario_spec').read_text());scenario['family']=family
    index['inputs']['scenario_spec']['sha256']=_write(root/'scenario_spec',scenario)
    _write(root/'index.json',index)
    before={p:p.read_bytes() for p in root.iterdir()}
    r=audit.build_program_report(root/'index.json',audit._sha(root/'index.json'),instruction_budgets=[4,12])
    assert r['family']==family and r['program_seed']==2**64-1
    assert r['analysis']['total_committed_instructions']==12
    assert not r['canonical_hit_eligible'] and not r['case_qualification_upgraded']
    assert all(p.read_bytes()==b for p,b in before.items())


@pytest.mark.parametrize('change',['missing_pin','stale_input','wrong_family','wrong_seed','scenario','map_hash','binary_text','instruction_encoding'])
def test_index_and_binary_bindings_are_required(input_set,change):
    root,index=input_set
    if change=='stale_input':(root/'trace').write_text('{}')
    elif change=='wrong_family':index['family']='unconstrained'
    elif change=='wrong_seed':index['program_seed']=True
    elif change=='scenario':
        payload=json.loads((root/'scenario_spec').read_text());payload['timing_seed']+=1
        index['inputs']['scenario_spec']['sha256']=_write(root/'scenario_spec',payload)
    elif change in ('map_hash','binary_text','instruction_encoding'):
        payload=json.loads((root/'program_map').read_text())
        if change=='map_hash':payload['source_sha256']='0'*64
        elif change=='binary_text':payload['text_sha256']='0'*64
        else:payload['instructions'][0]['encoding']='00000013'
        index['inputs']['program_map']['sha256']=_write(root/'program_map',payload)
    _write(root/'index.json',index)
    with pytest.raises(ValueError):
        audit.build_program_report(root/'index.json','' if change=='missing_pin' else audit._sha(root/'index.json'))


def test_cli_creates_only_new_report_and_never_replays(input_set):
    root,_=input_set;output=root/'report.json'
    args=['--input-index',str(root/'index.json'),'--input-index-sha256',audit._sha(root/'index.json'),'--output',str(output)]
    assert audit.main(args)==0
    original=output.read_bytes()
    with pytest.raises(FileExistsError):audit.main(args)
    assert output.read_bytes()==original


def test_declared_target_and_taken_flags_do_not_create_observed_space(tmp_path):
    pm,trace=_map(),tmp_path/'trace'
    rows=_trace(trace,pm,[0x1000,0x1004]*4)
    a=audit.analyze_program_prefixes(trace,pm,history_bits=1)
    for row in rows:row.update(taken=True,target_pc='0x1008')
    trace.write_text(''.join(json.dumps(row)+'\n' for row in rows))
    b=audit.analyze_program_prefixes(trace,pm,history_bits=1)
    assert a==b


def test_same_final_pc_coverage_can_have_different_cfi_order_space(tmp_path):
    pm,trace=_map(),tmp_path/'trace'
    # Both paths execute both JALR successors, but their CFI ordering differs.
    _trace(trace,pm,[0x1000,0x1004,0x1008,0x100c,0x1008,0x1004])
    first=audit.analyze_program_prefixes(trace,pm)
    _trace(trace,pm,[0x1000,0x1004,0x1008,0x100c,0x1000,0x1004,0x1008,0x1004])
    second=audit.analyze_program_prefixes(trace,pm)
    assert first['prefix_curve'][-1]['unique_pcs']==second['prefix_curve'][-1]['unique_pcs']==4
    assert first['prefix_curve'][-1]['indirect_site_target_pairs']==second['prefix_curve'][-1]['indirect_site_target_pairs']==2
    assert set(first['cfi_layout_transition_keys'])!=set(second['cfi_layout_transition_keys'])


def test_empty_trace_and_excessive_history_are_rejected(tmp_path):
    trace=tmp_path/'trace';trace.write_text('')
    with pytest.raises(ValueError,match='empty'):
        audit.analyze_program_prefixes(trace,_map())
    with pytest.raises(ValueError,match='capped'):
        audit.analyze_program_prefixes(trace,_map(),history_bits=9)


def test_program_call_depth_is_not_hardware_ras_occupancy(tmp_path):
    pm={'instructions':[
      {'pc':0x1000,'size':4,'encoding':'010000ef','mnemonic':'jal','operands':'x1,1010'},
      {'pc':0x1004,'size':4,'encoding':'00000013','mnemonic':'addi','operands':'x0,x0,0'},
      {'pc':0x1010,'size':4,'encoding':'010000ef','mnemonic':'jal','operands':'x1,1020'},
      {'pc':0x1014,'size':4,'encoding':'00008067','mnemonic':'jalr','operands':'x0,0(x1)'},
      {'pc':0x1020,'size':4,'encoding':'00008067','mnemonic':'jalr','operands':'x0,0(x1)'}]}
    trace=tmp_path/'trace'
    _trace(trace,pm,[0x1000,0x1010,0x1020,0x1014,0x1004])
    r=audit.analyze_program_prefixes(trace,pm)
    assert r['prefix_curve'][-1]['observed_call_hint_stack_depths']==[1,2]
    assert r['call_hint_return_relations']=={'matched_program_return':2}
    assert r['hardware_ras_state']=='unmeasured'


def test_computed_x5_target_does_not_pop_a_real_matching_return_context(tmp_path):
    pm={'instructions':[
      {'pc':0x1000,'size':4,'encoding':'010000ef','mnemonic':'jal','operands':'x1,1010'},
      {'pc':0x1004,'size':4,'encoding':'00000013','mnemonic':'addi','operands':'x0,x0,0'},
      {'pc':0x1010,'size':4,'encoding':'00028067','mnemonic':'jalr','operands':'x0,0(x5)'},
      {'pc':0x1020,'size':4,'encoding':'00008067','mnemonic':'jalr','operands':'x0,0(x1)'}]}
    trace=tmp_path/'trace';_trace(trace,pm,[0x1000,0x1010,0x1020,0x1004])
    r=audit.analyze_program_prefixes(trace,pm)
    assert r['call_hint_return_relations']=={'unmatched_program_return':1,'matched_program_return':1}
    assert r['hardware_ras_state']=='unmeasured'
