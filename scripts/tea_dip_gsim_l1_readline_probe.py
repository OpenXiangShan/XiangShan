"""Relink an immutable GSIM model to observe L1 readline progress eligibility.

Readline conflicts are port conditions, not exclusive CPU delays. No original
binary, object, RTL or modeled state changes. Actual generated bindings and
closed-guest/native/SQL calibration are required before interpreting results.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shlex
import subprocess
from tea_dip_gsim_lsu_s1_probe import digest

BASE = 'cpu__DOT__l_soc__DOT__core_with_l2__DOT__core__DOT__memBlock__DOT__inner__DOT__dcache__DOT__dcache__DOT__'


def source(root, header, database, symbols):
    main, array = BASE + 'mainPipe__DOT__', BASE + 'bankedDataArray__DOT__'
    declarations = []
    def field(full, width):
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(full) + r'; // width = ' + str(width) + ',', header), full
        declarations.append(full)
        return 'model->' + full
    names = dict(s1_valid='s1_valid', need_data='s1_need_data',
                 s2_valid='s2_valid', s2_can_go='s2_can_go', s1_fire='s1_fire',
                 line_valid='_io_data_readline_valid_T', source='s1_req__DOT__source',
                 miss='s1_req__DOT__miss', probe='s1_req__DOT__probe')
    fields = {key:field(main + name, 4 if key == 'source' else 1) for key,name in names.items()}
    fields['s2_ready'] = f"(!{fields['s2_valid']} || {fields['s2_can_go']})"
    fields['line_ready'] = field(array + '_io_readline_ready_T', 1)
    rrl = [field(array + 'real_rrl_bank_conflict_reg' + ('_' + str(i) if i else '') + '$NEXT', 1) for i in range(3)]
    rw = [field(array + 'wr_bank_conflict_' + str(i), 1) for i in range(3)]
    snapshots, commits = [], []
    for i, name in enumerate(['s1_valid','s1_req__DOT__source','s1_req__DOT__miss','s1_req__DOT__probe']):
        width = 4 if name.endswith('source') else 1
        current, nxt = field(main + name, width), field(main + name + '$NEXT', width)
        snapshots.append(f'  previous[{i}] = {nxt};')
        commits.append(f'  assert(previous[{i}] == {current});')
    # Bind actual generated combinational assignments, excluding change flags.
    wanted = [main + names[key] for key in ['s1_fire','line_valid']] + [main + '_s2_ready_T',main + '_s2_ready_T_1'] + [array + '_io_readline_ready_T'] + [value.removeprefix('model->') for value in rrl]
    args = ['rg','-l','--glob','*.cpp']
    for name in wanted: args += ['-e',r'^\s*' + re.escape(name) + ' = ']
    paths = subprocess.check_output(args + [str(root / 'build/gsim-compile/model')],text=True).splitlines()
    assignments = {name:[] for name in wanted}
    for path in paths:
        for line in Path(path).read_text().splitlines():
            for name in wanted:
                if re.match(r'\s*' + re.escape(name) + ' = ',line): assignments[name].append(line.strip())
    assert all(assignments.values())
    assert assignments[main + '_s2_ready_T'] == [main + '_s2_ready_T = (' + main + 's2_valid == 0x0);']
    assert assignments[main + '_s2_ready_T_1'] == [main + '_s2_ready_T_1 = (' + main + '_s2_ready_T | ' + main + 's2_can_go);']
    step = '_ZN7SSimTop4stepEv'
    assert any(line.split()[-1:] == [step] for line in symbols.splitlines())
    writer, = [line.split()[-1] for line in symbols.splitlines() if 'L2TxreqArbTraceWriter' in line]
    signature, = re.findall(r'void L2TxreqArbTraceWriter\((.*?)\) \{',database,re.S)
    arguments = [parameter.strip().split()[-1].lstrip('*') for parameter in signature.split(',')]
    values = [fields[key] for key in ['s1_valid','need_data','s2_ready','s1_fire','line_valid','line_ready','source','miss','probe']]
    rrl_mask = ' | '.join(f'(unsigned({value}) << {i})' for i,value in enumerate(rrl))
    rw_mask = ' | '.join(f'(unsigned({value}) << {i})' for i,value in enumerate(rw))
    cpp = f'''#include "SimTop.h"
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
struct Context {{ bool seen; uint64_t cycle; unsigned clean, dump, enabled; }};
static Context context{{}};
static bool in_step = false;
static FILE *output = nullptr;
extern "C" void __real_{writer}({signature});
extern "C" void __wrap_{writer}({signature}) {{
  if (!reset) {{
    assert(in_step && !context.seen && stamp == data_nativeCycle);
    context = {{true, data_nativeCycle, unsigned(data_clean), unsigned(data_nativeDump), unsigned(data_logEnable)}};
  }}
  __real_{writer}({','.join(arguments)});
}}
extern "C" void __real_{step}(SSimTop *);
extern "C" void __wrap_{step}(SSimTop *model) {{
  assert(!in_step);
  uint64_t previous[4];
{chr(10).join(snapshots)}
  context = {{}}; in_step = true;
  __real_{step}(model);
  in_step = false;
{chr(10).join(commits)}
  if (!context.seen) return;
  assert({fields['line_valid']} == bool({fields['s1_valid']} && {fields['need_data']}));
  assert({fields['s1_fire']} == bool({fields['s1_valid']} && {fields['s2_ready']} && ({fields['line_ready']} || !{fields['need_data']})));
  const unsigned rrl = {rrl_mask};
  const unsigned rw = {rw_mask};
  assert(!rrl || {fields['line_valid']});
  if (!({fields['line_valid']} || rrl || rw || context.clean || context.dump)) return;
  if (!output) {{
    output = fopen("l1-readline-probe.csv", "w");
    if (!output) {{ perror("l1-readline-probe.csv"); abort(); }}
    fprintf(output, "native_cycle,clean,native_dump,log_enable,s1_valid,need_data,s2_ready,s1_fire,line_valid,line_ready,source,miss,probe,rrl_mask,rw_mask\\n");
  }}
  fprintf(output, "%lu,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u\\n", context.cycle, context.clean, context.dump, context.enabled,
    {','.join('unsigned(' + value + ')' for value in values)}, rrl, rw);
}}
'''
    return cpp, [step, writer], dict(field_names=declarations, generated_assignments=assignments,
        generated_source_sha256={path:digest(path) for path in paths}, writer_signature=signature,
        scope='Actual committed S1 payload and computed combinational conditions; native-time anchor from the unchanged L2 writer. Runtime formulas and next-step commits are asserted.')


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
    (output/'objects.rsp').write_text('\n'.join(shlex.quote(path) for path in objects) + '\n')
    old_output = Path(proof['binary']).parent
    commands = []
    for old in proof['commands']:
        replace = {str(old_output/name):str(output/name) for name in ['probe.cpp','probe.o','emu']}
        replace.update({'-I'+str(old_output):'-I'+str(output),'@'+str(old_output/'objects.rsp'):'@'+str(output/'objects.rsp')})
        command = [replace.get(arg,arg) for arg in old if not arg.startswith('-Wl,--wrap=')]
        if len(commands) == 1: command += ['-Wl,--wrap='+symbol for symbol in wrapped]
        commands.append(command)
    for index,command in enumerate(commands):
        result = subprocess.run(command,capture_output=True,text=True)
        (output/f'command-{index}.stdout').write_text(result.stdout)
        (output/f'command-{index}.stderr').write_text(result.stderr)
        assert result.returncode == 0,result.stderr[-6000:]
    assert all(digest(path) == value for path,value in originals.items())
    result = dict(passed=True,root=str(root),binary=str(output/'emu'),binary_sha256=digest(output/'emu'),
        original_binary=proof['original_binary'],original_binary_sha256=proof['original_binary_sha256'],
        original_files_unchanged=True,original_objects=len(objects),original_sha256=originals,
        commands=commands,wrapped_symbols=wrapped,binding=binding,probe_cpp_sha256=digest(output/'probe.cpp'),
        input_proof_sha256=digest(queue_build_proof),generator_sha256=digest(Path(__file__)),
        scope='Host-only readline qualification observer. No original objects/header/binary or modeled state changed. Actual native marginals, old outputs and phase/semantic calibration remain required.')
    (output/'build-proof.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--queue-build-proof',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args = parser.parse_args()
    result = build(args.queue_build_proof,args.output)
    print(json.dumps({key:result[key] for key in ['passed','binary','binary_sha256','original_files_unchanged','original_objects']}))

if __name__ == '__main__': main()
