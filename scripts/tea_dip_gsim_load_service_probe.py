"""Relink the existing three-lane GSIM model to observe qualified load S2 attempts.

Uses the existing InstTable callback for core time and top perf controls for
boundaries. Captured member identities describe attempts, not a proven final-WB
join. Original RTL, objects and binary are unchanged.
"""
import argparse
import json
from pathlib import Path
import re
import shlex
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest

PREFIX = 'cpu__DOT__l_soc__DOT__core_with_l2__DOT__core__DOT__memBlock__DOT__inner__DOT__'
IDENTITY = [('uop__DOT__robIdx__DOT__flag', 1), ('uop__DOT__robIdx__DOT__value', 8),
            ('uop__DOT__robIdx__DOT__isFormer', 1), ('uop__DOT__perfDebugInfo__DOT__teaMemberRank', 3),
            ('uop__DOT__debug_seqNum__DOT__uopIdx', 8), ('uop__DOT__perfDebugInfo__DOT__issueTime', 64),
            ('uop__DOT__pc', 50), ('vaddr', 50), ('paddr', 48),
            ('entrance', 8), ('accessType__DOT__instrType', 3)]


def source(root, header, database, symbols):
    model = root / 'build/gsim-compile/model'
    declared = set(re.findall(r'uint(?:8|16|32|64)_t ([A-Za-z0-9_$]+)(?:;|\[)', header))
    assignments, code_hashes = {}, {}

    def assignment(name):
        if name not in assignments:
            paths = subprocess.check_output(['rg', '-l', '-F', '--glob', '*.cpp', name + ' = ', str(model)], text=True).splitlines()
            found = []
            for path in paths:
                text = Path(path).read_text()
                code_hashes[path] = digest(path)
                found += re.findall(r'^\s*' + re.escape(name) + r' = (.*);$', text, re.M)
            rhs, = set(found)
            assignments[name] = rhs
        return assignments[name]

    def value(name):
        if name in declared:
            return 'model->' + name
        rhs = assignment(name)
        return '(' + re.sub(r'\b(?:cpu|difftest)__[A-Za-z0-9_$]+', lambda m: value(m[0]), rhs) + ')'

    def field(name, width):
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(name) + r'; // width = ' + str(width) + ',', header), name
        return 'model->' + name

    unit_path = root / 'src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala'
    enum_path = root / 'src/main/scala/xiangshan/mem/lsqueue/LoadQueueReplay.scala'
    unit, enum = unit_path.read_text(), enum_path.read_text()
    assert 'cause(C_WF) := false.B' in unit and 'cause(C_MF) := false.B' in unit
    assert re.search(r'val C_WF\s*= 7\b', enum) and re.search(r'val C_MF\s*= 12\b', enum)
    package = root / 'src/main/scala/xiangshan/mem/pipeline/package.scala'
    assert re.search(r'val noQuery = addType.*?val hit = addType.*?val miss = addType', package.read_text(), re.S)
    snapshots, commits, cases, bindings = [], [], [], []
    detail_bindings = []
    for lane in range(3):
        p = PREFIX + f'LoadUnit_{lane}__DOT__'
        inp, out = p + 's1__DOT__pipeOutBits__DOT__', p + 's2__DOT__pipeOutBits__DOT__'
        valid = field(p + 's1__DOT__pipeOutValid', 1)
        assert assignment(p + 's2__DOT__io_perf_0_value_REG$NEXT') == p + 's1__DOT__pipeOutValid'
        fire_name = p + 's2__DOT___perfEvents_T_2'
        assignment(fire_name)
        fire = value(fire_name)
        raw_miss_name = PREFIX + f'dcache__DOT__dcache__DOT__ldu_{lane}__DOT__real_miss'
        raw_miss = field(raw_miss_name, 1)
        assert raw_miss_name in assignment(p + 's2__DOT___cause_6_T_1')
        values = [field(inp + name, width) for name, width in IDENTITY]
        match = []
        for index, (name, width) in enumerate(IDENTITY):
            current, nxt = field(out + name, width), field(out + name + '$NEXT', width)
            snapshots.append(f'  previous[{lane}][{index}] = {nxt};')
            commits.append(f'  assert(previous[{lane}][{index}] == {current});')
            match.append(f'    assert({nxt} == {values[index]});')
        cause_names = {0: '_cause_0_T_6', 1: '_cause_1_T', 2: '_cause_2_T_1',
                       3: '_cause_3_T_1', 4: '_cause_4_T', 5: '_cause_5_T_1',
                       6: '_cause_6_T_1', 8: '_cause_8_T_3', 9: '_cause_9_T',
                       10: '_cause_10_T', 11: '_cause_11_T'}
        for name in cause_names.values():
            assignment(p + 's2__DOT__' + name)
        causes = [f'(unsigned({value(p + "s2__DOT__" + name)}) << {i})'
                  for i, name in cause_names.items()]
        mask = ' | '.join(causes)
        current_mask = ' | '.join(f'(unsigned({field(out + "cause_" + str(i), 1)}) << {i})' for i in cause_names)
        full = field(p + 's2__DOT__fullForward', 1)
        store = field(p + 's2__DOT__storeFullForward', 1)
        need = field(p + 's2__DOT__needDCacheAccess', 1)
        perf_miss = field(p + 's2__DOT__io_perf_1_value_REG$NEXT', 1)
        bank = PREFIX + 'dcache__DOT__dcache__DOT__bankedDataArray__DOT__'
        suffix = '' if lane == 0 else '_' + str(lane)
        detail = [value(p + 's2__DOT__troubleMaker'), field(inp + 'tlbAccessResult', 3),
                  field(inp + 'specialModeRequest', 1),
                  '(' + field(inp + 'unalignHead', 1) + ' && ' + field(inp + 'shouldFastReplay', 1) + ')',
                  field(bank + 'real_rr_bank_conflict_reg' + suffix, 1),
                  field(bank + 'real_wr_bank_conflict_reg' + suffix, 1),
                  field(bank + 'real_rrl_bank_conflict_reg' + suffix, 1)]
        detail_bindings.append(dict(lane=lane, expressions=detail))

        scalar = [f'uint64_t({v})' for v in values]
        cases.append(f'''  if (prior_valid[{lane}]) assert(prior_causes[{lane}] == ({current_mask}));
  prior_valid[{lane}] = bool({valid});
  if ({valid}) {{
    assert(context.seen);
{chr(10).join(match)}
    const unsigned cause = {mask};
    prior_causes[{lane}] = cause;
    const unsigned fire = unsigned({fire});
    assert({perf_miss} == bool(fire && (cause & (1u << 6))));
    const unsigned trouble = unsigned({detail[0]});
    const unsigned tlb = unsigned({detail[1]});
    const unsigned special = unsigned({detail[2]});
    const unsigned unaligned = unsigned({detail[3]});
    const unsigned rr = unsigned({detail[4]}), rw = unsigned({detail[5]}), rrl = unsigned({detail[6]});
    assert(bool(cause & (1u << 3)) == bool((trouble && tlb == 4) || special));
    assert(bool(cause & (1u << 8)) == bool(trouble && (({need} && (rr || rw || rrl)) || unaligned)));
    write_event({lane}, fire, unsigned({raw_miss}), unsigned({full}), unsigned({store}), unsigned({need}), cause,
      trouble, tlb, special, unaligned, rr, rw, rrl, {','.join(scalar)});
  }}''')
        bindings.append(dict(lane=lane, valid_equals_handshake=True, fire_expression=assignments[fire_name], raw_miss=raw_miss_name))
    step = '_ZN7SSimTop4stepEv'
    assert any(line.split()[-1:] == [step] for line in symbols.splitlines())
    writer, = [line.split()[-1] for line in symbols.splitlines() if 'InstTable0Writer' in line]
    signature, = re.findall(r'void InstTable0Writer\((.*?)\) \{', database, re.S)
    args = [item.strip().split()[-1].lstrip('*') for item in signature.split(',')]
    clean = field('difftest__DOT__perfCtrl__DOT__clean', 1)
    dump = field('difftest__DOT__perfCtrl__DOT__dump', 1)
    cpp = f'''#include "SimTop.h"
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
struct Context {{ bool seen; uint64_t core; unsigned clean, dump; }};
static Context context{{}};
static bool in_step = false;
static bool prior_valid[3]{{}};
static unsigned prior_causes[3]{{}};
static FILE *output = nullptr;
static void write_event(unsigned lane, unsigned fire, unsigned miss, unsigned full, unsigned store,
  unsigned need, unsigned cause, unsigned trouble, unsigned tlb, unsigned special, unsigned unaligned,
  unsigned rr, unsigned rw, unsigned rrl, uint64_t flag, uint64_t index, uint64_t former, uint64_t rank,
  uint64_t uop, uint64_t issue, uint64_t pc, uint64_t va, uint64_t pa, uint64_t entrance, uint64_t type) {{
  if (!output) {{
    output = fopen("load-service-probe.csv", "w");
    if (!output) {{ perror("load-service-probe.csv"); abort(); }}
    fprintf(output, "core_cycle,clean,native_dump,lane,fire,raw_miss,full_forward,store_full_forward,need_dcache,cause_mask,trouble_maker,tlb_access_result,special_request,unaligned_replay,read_read_conflict,read_write_conflict,readline_conflict,rob_flag,rob_index,rob_former,rank,uop_index,issue_cycle,pc,virtual_address,physical_address,entrance,instr_type\\n");
  }}
  fprintf(output, "%lu,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%lu,%lu,%lu,%lu,%lu,%lu,%lu,%lu,%lu,%lu,%lu\\n",
    context.core,context.clean,context.dump,lane,fire,miss,full,store,need,cause,trouble,tlb,special,unaligned,rr,rw,rrl,flag,index,former,rank,uop,issue,pc,va,pa,entrance,type);
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
  uint64_t previous[3][{len(IDENTITY)}];
{chr(10).join(snapshots)}
  context = {{false,0,unsigned({clean}),unsigned({dump})}};
  in_step = true; __real_{step}(model); in_step = false;
{chr(10).join(commits)}
  assert(context.clean == {clean} && context.dump == {dump});
  if (context.seen && (context.clean || context.dump))
    write_event(3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
{chr(10).join(cases)}
}}
'''
    return cpp, [step, writer], dict(lanes=bindings, assignments=assignments,
        detail_bindings=detail_bindings, cause_details_asserted=True,
        generated_source_sha256=code_hashes, RTL_source_sha256={str(p): digest(p) for p in [unit_path, enum_path, package]},
        cause_next_commit_asserted=True, WB_identity='not_captured', scope='S2 attempts only. Next payload commitments and qualified miss event are asserted; final WB/lifetime association is not established.')


def build(root, output, compiler, library):
    root, output, library = Path(root).resolve(), Path(output).resolve(), Path(library).resolve()
    assert not output.exists()
    model = root / 'build/gsim-compile/model'
    original, header, database = root / 'build/gsim-compile/emu', model / 'SimTop.h', root / 'build/chisel_db.cpp'
    objects = sorted(model.glob('*.o')) + sorted((root / 'build/gsim-compile/other').glob('*.o'))
    originals = {str(p): digest(p) for p in [original, header, database, library, *objects]}
    symbols = subprocess.check_output(['nm', '--defined-only', str(original)], text=True)
    cpp, wrapped, binding = source(root, header.read_text(), database.read_text(), symbols)
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_text('\n'.join(shlex.quote(str(p)) for p in objects) + '\n')
    commands = [[compiler, '-std=c++17', '-O2', '-DGSIM', '-fbracket-depth=2048', '-I' + str(model),
                 '-c', str(output / 'probe.cpp'), '-o', str(output / 'probe.o')],
                [compiler, '@' + str(output / 'objects.rsp'), str(output / 'probe.o'),
                 *['-Wl,--wrap=' + name for name in wrapped], str(library), '-lsqlite3', '-lz', '-lzstd', '-ldl', '-o', str(output / 'emu')]]
    for i, command in enumerate(commands):
        result = subprocess.run(command, capture_output=True, text=True)
        (output / f'command-{i}.stdout').write_text(result.stdout)
        (output / f'command-{i}.stderr').write_text(result.stderr)
        assert result.returncode == 0, result.stderr[-5000:]
    assert all(digest(p) == value for p, value in originals.items())
    proof = dict(passed=True, root=str(root), binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'),
                 original_binary=str(original), original_binary_sha256=originals[str(original)],
                 original_files_unchanged=True, original_objects=len(objects), original_sha256=originals,
                 commands=commands, binding=binding, probe_cpp_sha256=digest(output / 'probe.cpp'),
                 generator_sha256=digest(Path(__file__)), scope='One host-only observer object relinked with immutable originals. No modeled state or RTL change. Closed-guest correctness, native marginals, old SQL and measurement semantics remain to be calibrated.')
    (output / 'build-proof.json').write_text(json.dumps(proof, indent=2) + '\n')
    return proof


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--compiler', default='clang++')
    parser.add_argument('--dramsim3-library', type=Path, required=True)
    args = parser.parse_args()
    proof = build(args.root, args.output, args.compiler, args.dramsim3_library)
    print(json.dumps({k: proof[k] for k in ['passed', 'binary', 'binary_sha256', 'original_files_unchanged', 'original_objects']}))


if __name__ == '__main__':
    main()
