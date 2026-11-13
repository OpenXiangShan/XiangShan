"""Append registered L2 merged-demand provenance to an accepted memory host relink."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_l2_return_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

FIELDS = ['valid', 'line', 'merge_active', 'merge_source_id', 'merge_opcode']


def add_capture(root, header, cpp):
    source = root / 'XSCache/src/main/scala/coupledL2/MSHR.scala'
    text = source.read_text()
    assert 'val mergeA = RegInit(false.B)' in text
    assert 'val merge_task = RegEnable(io.aMergeTask.bits' in text
    assert 'mp_grant.aMergeTask.sourceId := merge_task.sourceId' in text
    snapshots, captures, bindings = [], [], []
    for bank in range(4):
        for slot in range(16):
            prefix = PREFIX + f'slices_{bank}__DOT__mshrCtl__DOT__mshrs_{slot}__DOT__'
            regs = [('req_valid', 1), ('req__DOT__tag', 30), ('req__DOT__set', 10),
                    ('mergeA', 1), ('mp_grant_task_merge_task__DOT__sourceId', 8),
                    ('mp_grant_task_merge_task__DOT__opcode', 4)]
            def fields(suffix):
                values = []
                for name, width in regs:
                    name = prefix + name + suffix
                    assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(name)
                                     + r'; // width = ' + str(width) + ',', header), name
                    values.append('model->' + name)
                return values
            current, nxt = fields(''), fields('$NEXT')
            def normalized(v):
                return [v[0], f'({v[0]} ? ((uint64_t({v[1]})<<18) | (uint64_t({v[2]})<<8) | {bank*64}u) : 0)',
                        f'({v[0]} && {v[3]})', f'({v[0]} && {v[3]} ? {v[4]} : 0)',
                        f'({v[0]} && {v[3]} ? {v[5]} : 0)']
            index = bank * 16 + slot
            snapshots.extend(f'  merge_next[{index}][{j}] = uint64_t({v});'
                             for j, v in enumerate(normalized(nxt)))
            captures.append(f'''  {{
    const uint64_t now[5] = {{{','.join('uint64_t('+v+')' for v in normalized(current))}}};
    if (context.seen) {{
      for (unsigned j=0;j<5;++j) assert(merge_next[{index}][j]==now[j]);
      if (!merge_seen || std::memcmp(now,merge_previous[{index}],sizeof(now)) || context.clean || context.dump)
        merge_write(1,{bank},{slot},now);
      std::memcpy(merge_previous[{index}],now,sizeof(now));
    }}
  }}''')
            bindings.append(dict(bank=bank, slot=slot, current=current, next=nxt))
    helper = f'''
static uint64_t merge_next[64][5]{{}}, merge_previous[64][5]{{}};
static bool merge_seen=false;
static FILE *merge_output=nullptr;
static void merge_write(unsigned kind,unsigned bank,unsigned slot,const uint64_t *values) {{
  if (!merge_output) {{
    merge_output=fopen("load-l2-merge.csv","w");
    if (!merge_output) {{ perror("load-l2-merge.csv"); abort(); }}
    fprintf(merge_output,"core_cycle,clean,native_dump,kind,bank,mshr,{','.join(FIELDS)}\\n");
  }}
  fprintf(merge_output,"%lu,%u,%u,%u,%u,%u",context.core,context.clean,context.dump,kind,bank,slot);
  for (unsigned j=0;j<5;++j) fprintf(merge_output,",%lu",values[j]);
  fprintf(merge_output,"\\n");
}}
static void merge_snapshot(SSimTop *model) {{
{chr(10).join(snapshots)}
}}
static void merge_observe(SSimTop *model) {{
  if (context.seen && (context.clean || context.dump)) {{ const uint64_t zero[5]{{}}; merge_write(0,4,16,zero); }}
{chr(10).join(captures)}
  if (context.seen) merge_seen=true;
}}
'''
    for old, new in [('static void l2_snapshot(SSimTop *model)', helper + '\nstatic void l2_snapshot(SSimTop *model)'),
                     ('  l2_snapshot(model);', '  merge_snapshot(model);\n  l2_snapshot(model);'),
                     ('  l2_observe(model);', '  l2_observe(model);\n  merge_observe(model);')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(banks=4, entries_per_bank=16, fields=FIELDS, bindings=bindings,
                     next_register_commit_asserted=True, source_sha256={str(source): digest(source)},
                     scope='Registered mergeA plus its RegEnable source/opcode, qualified by the active owner. State observation follows the accepting edge; it is not a separately observed combinational handshake or CPU retirement.')


def build(memory_build_proof, output):
    proof_path, output = Path(memory_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and previous['original_objects'] == 577
    assert previous['generator_sha256'] == digest(Path(__file__).with_name('tea_dip_gsim_memory_return_probe.py'))
    parent, root = proof_path.parent, Path(previous['root'])
    assert digest(parent/'probe.cpp') == previous['probe_cpp_sha256'] and not output.exists()
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    cpp, binding = add_capture(root, (root/'build/gsim-compile/model/SimTop.h').read_text(), (parent/'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output/f'build-{i}.stdout').open('wb') as stdout, (output/f'build-{i}.stderr').open('wb') as stderr:
            subprocess.run(command, stdout=stdout, stderr=stderr, check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    result = dict(previous, binary=str(output/'emu'), binary_sha256=digest(output/'emu'), commands=commands,
                  probe_cpp_sha256=digest(output/'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  base_memory_build_proof_sha256=digest(proof_path), L2_merge_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result, indent=2)+'\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--memory-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = build(args.memory_build_proof, args.output)
    print(json.dumps({key: result[key] for key in ['passed','binary','binary_sha256','original_objects']}))


if __name__ == '__main__':
    main()
