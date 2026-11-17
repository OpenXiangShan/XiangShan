"""Four-family committed-program exploration; offline, never DUT coverage/HIT.

Read hash-pinned original inputs. Prefixes use only successors already consumed;
committed instruction count is not elapsed simulation or regression time.
"""
from __future__ import annotations

import argparse
from collections import Counter, deque
import hashlib
import json
from pathlib import Path

try:
    from .generate_random_asm import FAMILIES
    from .program_map import _classify_instruction
except ImportError:
    from generate_random_asm import FAMILIES
    from program_map import _classify_instruction


def _sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def _int(value, label, minimum=0):
    if type(value) is not int or value < minimum:
        raise ValueError('invalid integer: ' + label)
    return value


def _address(value):
    if type(value) is int and value >= 0:
        return value
    if isinstance(value, str):
        try:
            result = int(value, 0)
            if result >= 0:
                return result
        except ValueError:
            pass
    raise ValueError('invalid trace address')


def _instruction_index(program_map):
    by_pc, previous_end = {}, -1
    for item in program_map.get('instructions', []):
        pc, size = _int(item.get('pc'), 'pc'), _int(item.get('size'), 'size', 1)
        if size not in (2, 4) or pc % 2 or pc < previous_end:
            raise ValueError('unsupported/overlapping/unordered ProgramMap instruction')
        encoding = item.get('encoding')
        if not isinstance(encoding, str) or len(encoding) != size*2:
            raise ValueError('instruction encoding missing or wrong width')
        try:
            int(encoding, 16)
        except ValueError as exc:
            raise ValueError('invalid instruction encoding') from exc
        if not isinstance(item.get('mnemonic'), str) or not isinstance(item.get('operands'), str):
            raise ValueError('disassembly identity missing')
        by_pc[pc] = {**item, **_classify_instruction(item)}
        previous_end = pc + size
    if not by_pc:
        raise ValueError('ProgramMap instructions missing')
    return by_pc


def analyze_program_prefixes(trace_path, program_map, *, instruction_budgets=(), history_bits=4):
    """Diagnostic observed space; no DUT cycles, coverage percentage or entropy score."""
    _int(history_bits, 'history_bits', 1)
    if history_bits > 8:
        raise ValueError('history_bits is capped at 8')
    budgets = list(instruction_budgets)
    if (any(type(n) is not int or n <= 0 for n in budgets) or budgets != sorted(set(budgets))):
        raise ValueError('instruction budgets must be unique increasing positive integers')
    by_pc = _instruction_index(program_map)
    pcs, edges, layouts, layout_transitions, histories, indirect_pairs, page_edges = (set() for _ in range(7))
    outcomes = Counter()
    recent = deque(maxlen=history_bits)
    stack, call_depths, return_matches = [], set(), Counter()
    visits = Counter()
    curve, previous, total, last_novelty, previous_cfi_layout = [], None, 0, 0, None

    def snapshot():
        return {'committed_instructions': total, 'unique_pcs': len(pcs),
                'unique_cfi_edges': len(edges), 'unique_cfi_layouts': len(layouts),
                'unique_cfi_layout_transitions': len(layout_transitions),
                'branch_history_outcome_combinations': len(histories),
                'indirect_site_target_pairs': len(indirect_pairs),
                'unique_program_page_transitions': len(page_edges),
                'observed_call_hint_stack_depths': sorted(call_depths),
                'max_pc_visit_count': max(visits.values(), default=0),
                'last_new_observation_at_instruction': last_novelty,
                'instructions_since_new_observation': total-last_novelty}

    with Path(trace_path).open() as stream:
        for line in stream:
            if not line.strip():
                continue
            entry = json.loads(line)
            if type(entry.get('index')) is not int or entry['index'] != total:
                raise ValueError('trace index must be contiguous and start at zero')
            pc = _address(entry.get('pc'))
            if pc not in by_pc:
                raise ValueError('trace PC absent from ProgramMap: ' + hex(pc))
            instruction = by_pc[pc]
            if (type(entry.get('size')) is not int or entry['size'] != instruction['size']
                    or _address(entry.get('instr')) != int(instruction['encoding'], 16)):
                raise ValueError('trace instruction bytes/width disagree with ProgramMap')
            before = sum(map(len, (pcs, edges, layouts, layout_transitions, histories, indirect_pairs, page_edges, call_depths)))
            pcs.add(pc);visits[pc] += 1
            if previous is not None:
                prior_pc, prior = previous
                cfi = any(prior[k] for k in ('conditional_branch','direct_jump','indirect_jump','call','return'))
                if cfi:
                    edges.add((prior_pc, pc))
                    kind = ('return' if prior['return'] else 'call' if prior['call'] else
                            'branch' if prior['conditional_branch'] else 'indirect' if prior['indirect_jump'] else 'direct')
                    layout = (kind, prior['size'], prior_pc % 32, prior_pc % 64,
                              prior_pc//4096 != pc//4096)
                    layouts.add(layout)
                    if previous_cfi_layout is not None:
                        layout_transitions.add((previous_cfi_layout, layout))
                    previous_cfi_layout = layout
                if prior['conditional_branch']:
                    # Derive the actual successor relation, ignoring declared taken/target.
                    # Target==fallthrough is indistinguishable, so label relation, not taken.
                    relation = 'fallthrough_successor' if pc == prior_pc+prior['size'] else 'nonfallthrough_successor'
                    outcomes[relation] += 1
                    if len(recent) == history_bits:
                        histories.add((prior_pc, tuple(recent), relation))
                    recent.append(relation)
                if prior['indirect_jump']:
                    indirect_pairs.add((prior_pc, pc))
                if prior['return']:
                    return_matches['matched_program_return' if stack and stack[-1] == pc else 'unmatched_program_return'] += 1
                    if stack and stack[-1] == pc:
                        stack.pop()
                elif prior['call']:
                    stack.append(prior_pc+prior['size'])
                    call_depths.add(len(stack))
                if prior_pc//4096 != pc//4096:
                    page_edges.add((prior_pc//4096, pc//4096))
            total += 1
            if sum(map(len, (pcs, edges, layouts, layout_transitions, histories, indirect_pairs, page_edges, call_depths))) > before:
                last_novelty = total
            previous = (pc, instruction)
            if total in budgets:
                curve.append(snapshot())
    if total == 0:
        raise ValueError('trace is empty')
    if any(n > total for n in budgets):
        raise ValueError('budget exceeds retained trace; no extrapolation')
    if not curve or curve[-1]['committed_instructions'] != total:
        curve.append(snapshot())
    targets = {}
    for site, target in indirect_pairs:
        targets.setdefault(site, set()).add(target)
    return {'schema_version': 1, 'status': 'program_trace_only', 'total_committed_instructions': total,
            'history_bits': history_bits, 'prefix_curve': curve,
            'branch_successor_relations': dict(outcomes), 'call_hint_return_relations': dict(return_matches),
            'cfi_layout_transition_keys': sorted(json.dumps(key, separators=(',', ':')) for key in layout_transitions),
            'layout_key_fields': ['CFI kind', 'width bytes', 'source offset32', 'source offset64', 'target in different page'],
            'indirect_sites_with_multiple_successors': sum(len(v)>1 for v in targets.values()),
            'indirect_sites_with_multiple_target_pages': sum(len({p//4096 for p in v})>1 for v in targets.values()),
            'unresolved_last_cfi_successor': any(previous[1][k] for k in ('conditional_branch','direct_jump','indirect_jump')),
            'last_new_observation_at_instruction': last_novelty,
            'terminal_repeat_fraction': (total-last_novelty)/total,
            'dut_coverage': 'unmeasured', 'elapsed_time': 'unmeasured', 'hardware_ras_state': 'unmeasured',
            'call_hint_model_policy': 'Disassembly link-register hints plus actual successor; unmatched return-like jump does not pop; exception, coroutine and nonstandard control flow are not reconstructed',
            'policy': 'Program-only observations, successor relations not inferred taken flags; repeated training may remain useful; no claim of saturation or bug-finding superiority'}


def build_program_report(input_index, index_sha256, *, instruction_budgets=(), history_bits=4):
    """Read one explicitly pinned case; never discover inputs or create qualification."""
    input_index = Path(input_index)
    if not index_sha256 or _sha(input_index) != index_sha256:
        raise ValueError('input index must match explicit SHA256')
    index = json.loads(input_index.read_text())
    family = index.get('family')
    if type(index.get('schema_version')) is not int or index['schema_version'] != 1 or family not in FAMILIES:
        raise ValueError('unknown family/index schema')
    seeds = [index.get('program_seed'), index.get('timing_seed')]
    if any(type(s) is not int or not 0 <= s < 2**64 for s in seeds):
        raise ValueError('exact uint64 program/timing seeds required')
    refs = {str(input_index.resolve()): index_sha256}
    paths = {}
    for kind in ('source', 'bin', 'trace', 'program_map', 'scenario_spec'):
        ref = index['inputs'][kind]
        path = Path(ref['path'])
        if not path.is_absolute():
            path = input_index.parent/path
        if not ref.get('sha256') or _sha(path) != ref['sha256']:
            raise ValueError('input hash mismatch: ' + kind)
        refs[str(path.resolve())] = ref['sha256'];paths[kind] = path
    scenario = json.loads(paths['scenario_spec'].read_text())
    if (scenario.get('scenario_id') != index.get('case_name') or scenario.get('family') != family
            or scenario.get('program_seed') != seeds[0] or scenario.get('timing_seed') != seeds[1]):
        raise ValueError('scenario/index identity mismatch')
    pm = json.loads(paths['program_map'].read_text())
    if (pm.get('source_sha256') != refs[str(paths['source'].resolve())]
            or pm.get('binary_sha256') != refs[str(paths['bin'].resolve())]
            or pm.get('binary_text_verified') is not True):
        raise ValueError('ProgramMap input identity mismatch')
    data = paths['bin'].read_bytes()
    start = _int(pm.get('text_offset_in_binary'), 'text_offset_in_binary')
    size = _int(pm.get('text_size_bytes'), 'text_size_bytes', 1)
    base = _int(pm.get('binary_base'), 'binary_base')
    text = data[start:start+size]
    if len(text) != size or hashlib.sha256(text).hexdigest() != pm.get('text_sha256'):
        raise ValueError('actual binary text hash mismatch')
    for pc, instruction in _instruction_index(pm).items():
        off = pc-base
        raw = bytes.fromhex(instruction['encoding'])[::-1]
        if off < start or off+len(raw) > start+size or data[off:off+len(raw)] != raw:
            raise ValueError('linked instruction bytes disagree with actual binary')
    analysis = analyze_program_prefixes(paths['trace'], pm, instruction_budgets=instruction_budgets, history_bits=history_bits)
    if any(_sha(path) != pin for path, pin in refs.items()):
        raise ValueError('inputs changed during analysis')
    return {'schema_version': 1, 'family': family, 'case_name': index['case_name'],
            'program_seed': seeds[0], 'timing_seed': seeds[1],
            'input_sha256': refs, 'analysis': analysis,
            'case_qualification_upgraded': False, 'canonical_hit_eligible': False,
            'runtime_provenance_status': 'not_evaluated; historical inputs remain historical'}


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--input-index', type=Path, required=True)
    p.add_argument('--input-index-sha256', required=True)
    p.add_argument('--instruction-budget', type=int, action='append', default=[])
    p.add_argument('--history-bits', type=int, default=4)
    p.add_argument('--output', type=Path, required=True)
    a = p.parse_args(argv)
    report = build_program_report(a.input_index, a.input_index_sha256,
                                  instruction_budgets=a.instruction_budget, history_bits=a.history_bits)
    with a.output.open('x', encoding='utf-8') as f:
        json.dump(report, f, indent=2);f.write('\n')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
