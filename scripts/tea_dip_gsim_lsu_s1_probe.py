"""Relink a calibrated immutable GSIM build to observe same-uop load S1 events.

Input is the existing TXREQ probe build proof, which binds the original objects,
header, database writer and DRAM library. This observer reuses that exact link
recipe, without the queue hooks. It supports the observed three LoadUnits whose
S1 input ready is optimized to true, and proves that binding from generated C++.
No original binary, object, header, RTL or modeled state is changed.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shlex
import subprocess

PREFIX = 'cpu__DOT__l_soc__DOT__core_with_l2__DOT__core__DOT__memBlock__DOT__inner__DOT__LoadUnit_{}__DOT__'
KEYS = [('uop__DOT__robIdx__DOT__flag', 1), ('uop__DOT__robIdx__DOT__value', 8),
        ('uop__DOT__perfDebugInfo__DOT__teaMemberRank', 3), ('uop__DOT__debug_seqNum__DOT__uopIdx', 8),
        ('uop__DOT__perfDebugInfo__DOT__issueTime', 64), ('vaddr', 50)]


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def source(root, header, database, symbols):
    assert 'LoadUnit_3__DOT__s0__DOT__pipeOutValid;' not in header
    model = root / 'build/gsim-compile/model'
    bindings = []
    declarations = []
    cases = []
    snapshots = []
    commits = []
    cached = {}
    for lane in range(3):
        prefix = PREFIX.format(lane)
        inp, out = prefix + 's0__DOT__pipeOutBits__DOT__', prefix + 's1__DOT__pipeOutBits__DOT__'
        def field(name, width):
            full = prefix + name
            assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(full) + r'; // width = ' + str(width) + r',', header), full
            declarations.append(full)
            return 'model->' + full
        valid = field('s0__DOT__pipeOutValid', 1)
        raw_fire = field('s1__DOT__io_perf_0_value_REG$NEXT', 1)
        timer = field('s1__DOT__stageInfo_uop_perfDebugInfo_tlbRespTime_c', 64)
        bools = {name: field('s1__DOT__' + name, 1) for name in ['kill','tlbHit','tlbMiss','paddrEffective','tlbException','specialRequest']}
        values = {name: field('s0__DOT__pipeOutBits__DOT__' + name, width) for name, width in KEYS}
        extras = {name: field('s0__DOT__pipeOutBits__DOT__' + name, width) for name, width in [('noQuery',1),('entrance',8),('accessType__DOT__instrType',3),('accessType__DOT__pftType',2),('uop__DOT__robIdx__DOT__isFormer',1),('uop__DOT__pc',50)]}
        paddr = field('s1__DOT__pipeOutBits__DOT__paddr$NEXT', 48)
        tlb_end = field('s1__DOT__pipeOutBits__DOT__uop__DOT__perfDebugInfo__DOT__tlbRespTime$NEXT', 64)
        identity = []
        for index, (name, width) in enumerate(KEYS):
            current = field('s1__DOT__pipeOutBits__DOT__' + name, width)
            nxt = field('s1__DOT__pipeOutBits__DOT__' + name + '$NEXT', width)
            snapshots.append(f'  previous_next[{lane}][{index}] = {nxt};')
            commits.append(f'  assert(previous_next[{lane}][{index}] == {current});')
            identity.append(f'    assert({nxt} == {values[name]});')
        needle = prefix + 's1__DOT__io_perf_0_value_REG$NEXT = '
        found = subprocess.check_output(['rg','-l','-F','--glob','*.cpp',needle,str(model)],text=True).splitlines()
        assignments = []
        for path in found:
            if path not in cached:
                cached[path] = Path(path).read_text()
            assignments += re.findall(r'^\s*' + re.escape(needle) + r'(.*);$', cached[path], re.M)
        assert assignments and set(assignments) == {prefix + 's0__DOT__pipeOutValid'}, assignments
        bindings.append(dict(lane=lane,raw_fire_equals_input_valid=True,assignments=assignments,generated_source_sha256={path:hashlib.sha256(cached[path].encode()).hexdigest() for path in found}))
        args = [str(lane), bools['kill'], bools['tlbHit'], bools['tlbMiss'], extras['noQuery'], bools['paddrEffective'], bools['tlbException'], bools['specialRequest'], extras['entrance'], extras['accessType__DOT__instrType'], extras['accessType__DOT__pftType'], values[KEYS[0][0]], values[KEYS[1][0]], extras['uop__DOT__robIdx__DOT__isFormer'], values[KEYS[2][0]], values[KEYS[3][0]]]
        # Use homogeneous casts in the generated printf argument list.
        args = [f'unsigned({value})' for value in args]
        args += [f'uint64_t({values[KEYS[4][0]]})', f'uint64_t({values[KEYS[5][0]]})', f'uint64_t({paddr})', f'uint64_t({extras["uop__DOT__pc"]})']
        cases.append(f'''  assert({raw_fire} == {valid});
  if ({valid}) {{
    assert(context.native_seen && context.core_seen);
    assert({timer} == context.core_cycle);
    assert(!({bools['tlbHit']} && {bools['tlbMiss']}));
    assert({bools['paddrEffective']} == bool({bools['tlbHit']} || {extras['noQuery']}));
{chr(10).join(identity)}
    if ({bools['paddrEffective']}) assert({tlb_end} == context.core_cycle);
    write_event({','.join(args)});
  }}''')
    step = '_ZN7SSimTop4stepEv'
    assert any(line.split()[-1:] == [step] for line in symbols.splitlines())
    signatures = {}
    wrappers = []
    wrapped = [step]
    for name in ['L2TxreqArbTraceWriter', 'InstTable0Writer']:
        symbol, = [line.split()[-1] for line in symbols.splitlines() if name in line]
        signature, = re.findall(r'void ' + name + r'\((.*?)\) \{', database, re.S)
        arguments = [parameter.strip().split()[-1].lstrip('*') for parameter in signature.split(',')]
        if name == 'InstTable0Writer':
            capture = '''  if (!reset) {
    assert(in_step);
    if (context.core_seen) assert(context.core_cycle == stamp);
    context.core_seen = true; context.core_cycle = stamp;
  }'''
        else:
            capture = '''  if (!reset) {
    assert(in_step && !context.native_seen && stamp == data_nativeCycle);
    context.native_seen = true; context.native_cycle = data_nativeCycle;
    context.clean = data_clean; context.dump = data_nativeDump; context.enabled = data_logEnable;
  }'''
        wrappers.append(f'extern "C" void __real_{symbol}({signature});\nextern "C" void __wrap_{symbol}({signature}) {{\n{capture}\n  __real_{symbol}({",".join(arguments)});\n}}')
        signatures[name] = signature
        wrapped.append(symbol)
    cpp = '''#include "SimTop.h"
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
struct Context { bool native_seen, core_seen; uint64_t native_cycle, core_cycle; unsigned clean, dump, enabled; };
static Context context{};
static bool in_step = false;
static FILE *output = nullptr;
static void write_event(unsigned lane, unsigned killed, unsigned hit, unsigned miss, unsigned no_query,
  unsigned effective, unsigned exception, unsigned special, unsigned entrance, unsigned instr_type,
  unsigned prefetch_type, unsigned rob_flag, unsigned rob_index, unsigned rob_former, unsigned rank,
  unsigned uop_index, uint64_t issue, uint64_t vaddr, uint64_t paddr, uint64_t pc) {
  if (!output) {
    output = fopen("lsu-s1-probe.csv", "w");
    if (!output) { perror("lsu-s1-probe.csv"); abort(); }
    fprintf(output, "core_cycle,native_cycle,clean,native_dump,log_enable,lane,killed,tlb_hit,tlb_miss,no_query,paddr_effective,exception,special,entrance,instr_type,prefetch_type,rob_flag,rob_index,rob_former,rank,uop_index,issue_cycle,virtual_address,physical_address,pc\\n");
  }
  fprintf(output, "%lu,%lu,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%lu,%lu,%lu,%lu\\n",
    context.core_cycle,context.native_cycle,context.clean,context.dump,context.enabled,lane,killed,hit,miss,
    no_query,effective,exception,special,entrance,instr_type,prefetch_type,rob_flag,rob_index,rob_former,rank,uop_index,issue,vaddr,paddr,pc);
}
WRAPPERS
extern "C" void REAL_STEP(SSimTop *);
extern "C" void WRAP_STEP(SSimTop *model) {
  assert(!in_step);
  uint64_t previous_next[3][6];
SNAPSHOTS
  context = {};
  in_step = true;
  REAL_STEP(model);
  in_step = false;
COMMITS
  if (context.native_seen && (context.clean || context.dump))
    write_event(3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
CASES
}
'''
    for name, value in [('WRAPPERS','\n'.join(wrappers)),('REAL_STEP','__real_'+step),('WRAP_STEP','__wrap_'+step),('SNAPSHOTS','\n'.join(snapshots)),('COMMITS','\n'.join(commits)),('CASES','\n'.join(cases))]:
        cpp = cpp.replace(name, value)
    return cpp, wrapped, dict(ready_bindings=bindings,field_names=declarations,writer_signatures=signatures)


def build(queue_build_proof, output):
    proof = json.loads(Path(queue_build_proof).read_text())
    assert proof['passed'] and proof['original_files_unchanged']
    root, output = Path(proof['root']), Path(output).resolve()
    assert not output.exists()
    originals = proof['original_sha256']
    assert all(digest(path) == value for path,value in originals.items())
    model = root / 'build/gsim-compile/model'
    symbols = subprocess.check_output(['nm','--defined-only',proof['original_binary']],text=True)
    cpp, wrapped, binding = source(root,(model/'SimTop.h').read_text(),(root/'build/chisel_db.cpp').read_text(),symbols)
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    objects = [path for path in originals if path.endswith('.o')]
    assert len(objects) == proof['original_objects']
    (output/'objects.rsp').write_text('\n'.join(shlex.quote(path)for path in objects)+'\n')
    old_output = Path(proof['binary']).parent
    commands = []
    for old in proof['commands']:
        command = []
        for argument in old:
            if argument.startswith('-Wl,--wrap='):
                continue
            replacements = {str(old_output/'probe.cpp'):str(output/'probe.cpp'),str(old_output/'probe.o'):str(output/'probe.o'),str(old_output/'emu'):str(output/'emu'),'-I'+str(old_output):'-I'+str(output),'@'+str(old_output/'objects.rsp'):'@'+str(output/'objects.rsp')}
            command.append(replacements.get(argument,argument))
        if len(commands) == 1:
            command += ['-Wl,--wrap='+symbol for symbol in wrapped]
        commands.append(command)
    for index, command in enumerate(commands):
        result = subprocess.run(command,capture_output=True,text=True)
        (output/f'command-{index}.stdout').write_text(result.stdout)
        (output/f'command-{index}.stderr').write_text(result.stderr)
        assert result.returncode == 0,result.stderr[-6000:]
    assert all(digest(path)==value for path,value in originals.items())
    result = dict(passed=True,root=str(root),binary=str(output/'emu'),binary_sha256=digest(output/'emu'),original_binary=proof['original_binary'],original_binary_sha256=proof['original_binary_sha256'],original_files_unchanged=True,original_objects=len(objects),original_sha256=originals,commands=commands,wrapped_symbols=wrapped,binding=binding,probe_cpp_sha256=digest(output/'probe.cpp'),input_proof_sha256=digest(queue_build_proof),generator_sha256=digest(Path(__file__)),scope='Host-only S1 observer relink. Ready and field bindings are checked, not new RTL or modeled state. Counter marginals, clock/payload phase, execution neutrality and same-uop analysis still require real closed-guest calibration.')
    (output/'build-proof.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--queue-build-proof',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args = parser.parse_args()
    result = build(args.queue_build_proof,args.output)
    print(json.dumps({key:result[key]for key in ['passed','binary','binary_sha256','original_objects','original_files_unchanged']}))


if __name__ == '__main__':
    main()
