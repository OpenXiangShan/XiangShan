"""Observe old GSIM FTQ commit metadata without requiring a BPU ChiselDB table."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_load_service_probe import build
from tea_dip_gsim_lsu_s1_probe import digest

PREFIX = 'cpu__DOT__l_soc__DOT__core_with_l2__DOT__core__DOT__frontend__DOT__inner__DOT__ftq__DOT__'
REASONS = ['BTB', 'TAGE', 'RAS', 'ITTAGE', 'SC']
COLUMNS = ['kind', 'ftq_index', 'fetch_pc_halfword', 'cfi_mask', 'conditional', 'direct', 'indirect',
           'call', 'ret', 'indirect_retcall', 'mispredict', 'actual_position', 'actual_taken',
           'actual_type', 'actual_ras', 's1_source', 's3_source', 's2_override', 's3_override',
           's1_position', 's1_taken', 's2_position', 's2_taken', 's3_position', 's3_taken', 'reason_mask']


def source(root, header, database, symbols, capture_redirect=False):
    fields, predicates = {}, {}

    def field(name, width, shape=''):
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(name + shape) +
                         r'; // width = ' + str(width) + ',', header), name
        fields[name] = dict(width=width, shape=shape)
        return 'model->' + name

    def array(name, width, suffix='[64]'):
        return field(PREFIX + name, width, suffix) + '[index]'

    commit = field(PREFIX + 'commit', 1)
    index = field(PREFIX + 'commitPtr__DOT__ptrs__DOT__value_0', 6)
    start = array('entryQueue__DOT__startPc__DOT__addr', 50)
    is_cfi = array('perfQueue__DOT__isCfi', 1, '[64][32]') + '[position]'
    branch_type = array('perfQueue__DOT__cfiAttr__DOT__branchType', 2, '[64][32]') + '[position]'
    ras = array('perfQueue__DOT__cfiAttr__DOT__rasAction', 2, '[64][32]') + '[position]'
    mispredict = array('perfQueue__DOT__mispredict', 1)
    actual = 'perfQueue__DOT__mispredictBranchInfo__DOT__'
    metadata = 'perfQueue__DOT__bpuPerf__DOT__'
    payload = [array(actual + name, width) for name, width in
               [('cfiPosition', 5), ('taken', 1), ('attribute__DOT__branchType', 2),
                ('attribute__DOT__rasAction', 2)]]
    payload += [array(metadata + 'bpSource__DOT__' + name, width) for name, width in
                [('s1Source', 3), ('s3Source', 3), ('s2Override', 1), ('s3Override', 1)]]
    for stage in ['s1', 's2', 's3']:
        payload += [array(metadata + stage + 'Prediction__DOT__' + name, width)
                    for name, width in [('cfiPosition', 5), ('taken', 1)]]
    model = root / 'build/gsim-compile/model'
    generated_hashes, expressions = {}, {}
    declared = {m[0]: (int(m[2]), m[1]) for m in re.findall(
        r'uint(?:8|16|32|64)_t ([A-Za-z0-9_$]+)((?:\[\d+\])*)?; // width = (\d+),', header)}

    def value(name):
        if name in declared:
            width, shape = declared[name]
            return field(name, width, shape)
        if name not in expressions:
            paths = subprocess.check_output(['rg', '-l', '-F', '--glob', '*.cpp', name + ' = ', str(model)], text=True).splitlines()
            rhs = []
            for path in paths:
                generated_hashes[path] = digest(path)
                rhs += re.findall(r'^\s*' + re.escape(name) + r' = (.*);$', Path(path).read_text(), re.M)
            expressions[name], = set(rhs)
        return '(' + re.sub(r'\b(?:cpu|difftest)__[A-Za-z0-9_$]+',
                            lambda match: value(match[0]), expressions[name]) + ')'

    for label in REASONS:
        name = 'logEndpoint__DOT___commit_branch_mispredicts_reason_' + label + 'Next_T'
        paths = subprocess.check_output(['rg', '-l', '-F', '--glob', '*.cpp', name + ' = ', str(model)], text=True).splitlines()
        assignments = []
        for path in paths:
            text = Path(path).read_text()
            generated_hashes[path] = digest(path)
            assignments += re.findall(r'^\s*' + re.escape(name) + r' = (.*);$', text, re.M)
        rhs, = set(assignments)
        predicate, = re.findall(r'\+ \(\(uint64_t\)([A-Za-z0-9_$]+)\)', rhs)
        predicates[label] = dict(assignment=rhs, signal=predicate, expression=value(predicate))
    reason_mask = ' | '.join(f'(uint64_t({predicates[label]["expression"]}) << {i})'
                             for i, label in enumerate(REASONS))
    clean = field('difftest__DOT__perfCtrl__DOT__clean', 1)
    dump = field('difftest__DOT__perfCtrl__DOT__dump', 1)
    step = '_ZN7SSimTop4stepEv'
    assert any(line.split()[-1:] == [step] for line in symbols.splitlines())
    writer, = [line.split()[-1] for line in symbols.splitlines() if 'InstTable0Writer' in line]
    signature, = re.findall(r'void InstTable0Writer\((.*?)\) \{', database, re.S)
    args = [item.strip().split()[-1].lstrip('*') for item in signature.split(',')]
    paths = [root / ('src/main/scala/xiangshan/frontend/' + name) for name in
             ['ftq/Ftq.scala', 'ftq/Bundles.scala', 'PrunedAddr.scala']]
    assert 'private val commitPerfMeta   = perfQueue(commitPtr(0).value)' in paths[0].read_text()
    assert 'val startPc:     GuardedPc' in paths[1].read_text()
    values = ['1', 'index', start, 'mask', 'conditional', 'direct', 'indirect', 'call', 'ret', 'retcall',
              mispredict, *payload, 'reason_mask']
    assert len(values) == len(COLUMNS)
    cpp = f'''#include "SimTop.h"
#include <cassert>
#include <cstdint>
#include <cstdio>
struct Context {{ bool seen; uint64_t core; unsigned clean, dump; }};
static Context context{{}};
static bool in_step = false;
static FILE *output = nullptr;
static void record(const uint64_t *row) {{
  if (!output) {{
    output = fopen("bpu-commit-probe.csv", "w"); assert(output);
    fprintf(output, "core_cycle,clean,native_dump,{','.join(COLUMNS)}\\n");
  }}
  fprintf(output, "%lu,%u,%u", context.core, context.clean, context.dump);
  for (unsigned i = 0; i < {len(COLUMNS)}; ++i) fprintf(output, ",%lu", row[i]);
  fputc('\\n', output);
}}
extern "C" void __real_{writer}({signature});
extern "C" void __wrap_{writer}({signature}) {{
  if (!reset) {{
    assert(in_step);
    if (context.seen) assert(context.core == stamp);
    context.seen = true; context.core = stamp;
  }}
  __real_{writer}({','.join(args)});
}}
extern "C" void __real_{step}(SSimTop *);
extern "C" void __wrap_{step}(SSimTop *model) {{
  assert(!in_step);
  context = {{false,0,unsigned({clean}),unsigned({dump})}};
  in_step = true; __real_{step}(model); in_step = false;
  assert(context.clean == {clean} && context.dump == {dump});
  if (!context.seen) return;
  if (context.clean || context.dump) {{ uint64_t boundary[{len(COLUMNS)}]{{}}; record(boundary); }}
  if (!({commit})) return;
  const unsigned index = {index};
  uint64_t mask = 0, conditional = 0, direct = 0, indirect = 0, call = 0, ret = 0, retcall = 0;
  for (unsigned position = 0; position < 32; ++position) {{
    if (!({is_cfi})) continue;
    mask |= uint64_t(1) << position;
    const unsigned type = {branch_type}, ras = {ras};
    conditional += type == 1; direct += type == 2; indirect += type == 3;
    call += ras == 2; ret += ras == 1; retcall += type == 3 && ras == 3;
  }}
  const uint64_t reason_mask = {reason_mask};
  assert(bool({mispredict}) == bool(reason_mask));
  assert(!reason_mask || !(reason_mask & (reason_mask - 1)));
  const uint64_t row[] = {{{','.join('uint64_t(' + value + ')' for value in values)}}};
  record(row);
}}
'''
    extra = {}
    if capture_redirect:
        from tea_dip_gsim_bpu_redirect_probe import add_capture
        cpp, extra['redirect_binding'] = add_capture(root, cpp, value)
    binding = dict(**extra, kind='FTQ_commit_metadata', columns=COLUMNS, entries=64, positions=32,
                   fields=fields, reason_predicates=predicates, generated_expressions=expressions, generated_source_sha256=generated_hashes,
                   RTL_source_sha256={str(p): digest(p) for p in paths},
                   generator_sha256=digest(Path(__file__)), new_RTL_state_bits=0,
                   scope='All actual FTQ commit entries and boundary rows, not all BPU predictions. '
                         'GuardedPc.addr is retained in halfwords, with guard bit; no reconstructed instruction PC. '
                         'Position/direction metadata and native reason predicates are observed together. '
                         'Actual targets, actual instruction PC and resolution/recovery latency are not captured. '
                         'Clock phase, native margins and execution neutrality require closed-guest calibration.')
    return cpp, [step, writer], binding


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--compiler', default='clang++')
    parser.add_argument('--capture-redirect', action='store_true', help='Also record same-event backend redirects, separately from committed entries')
    parser.add_argument('--dramsim3-library', type=Path, required=True)
    args = parser.parse_args()
    proof = build(args.root, args.output, args.compiler, args.dramsim3_library,
                  source_factory=lambda *values: source(*values, capture_redirect=args.capture_redirect))
    print(json.dumps({k: proof[k] for k in ['passed', 'binary', 'binary_sha256', 'original_files_unchanged']}))


if __name__ == '__main__':
    main()
