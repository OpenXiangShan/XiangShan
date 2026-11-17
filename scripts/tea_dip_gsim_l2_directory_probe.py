"""Capture existing L2 tag/meta write ports and directory read snapshots, without RTL edits."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_l2_return_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

FIELDS = ['set', 'way_mask', 'tag', 'state', 'prefetch', 'prefetch_source', 'accessed',
          'source_valid', 'req_source', 'channel', 'opcode', 'mshr_task', 'merge_a',
          'read_count', 'tag_count', 'meta_count', 'response_count',
          *['tag_' + str(i) for i in range(8)], *['state_' + str(i) for i in range(8)]]


def add_capture(root, header, cpp):
    cache = Path(root) / 'XSCache/src/main/scala/coupledL2'
    required = {
        'Directory.scala': ['val tagWen  = io.tagWReq.valid', 'val metaWen = io.metaWReq.valid',
                           'val reqValid_s2 = RegNext(io.read.fire, false.B)',
                           'val reqValid_s3 = RegNext(reqValid_s2, false.B)',
                           'val metaAll_s3 = RegEnable(metaRead, 0.U.asTypeOf(metaRead), reqValid_s2)',
                           'val tagRead_s3 = RegEnable(tagRead, reqValid_s2)'],
        'MainPipe.scala': ['io.metaWReq.bits.set := Mux(resetFinish, req_s3.set, resetIdx)',
                          'io.metaWReq.bits.wayOH := Mux(resetFinish, metaW_wayOH, Fill(cacheParams.ways, true.B))',
                          'io.tagWReq.valid := task_s3.valid && req_s3.tagWen && mshr_refill_s3 && !retry',
                          'io.tagWReq.bits.wtag := req_s3.tag'],
        'Slice.scala': ['directory.io.metaWReq := mainPipe.io.metaWReq',
                        'directory.io.tagWReq := mainPipe.io.tagWReq'],
    }
    for name, fragments in required.items():
        text = (cache / name).read_text()
        for fragment in fragments:
            assert fragment in text, (name, fragment)

    declarations = dict(re.findall(
        r'(?:uint(?:8|16|32|64)_t|unsigned _BitInt\(128\)) (\S+); // width = (\d+),', header))

    def field(name, width):
        assert declarations.get(name) == str(width), name
        return 'model->' + name

    captures, bindings = [], []
    for bank in range(4):
        prefix = PREFIX + f'slices_{bank}__DOT__'
        def f(name, width=1):
            return field(prefix + name, width)
        mp, directory = 'mainPipe__DOT__', 'directory__DOT__'
        task = mp + 'task_s3__DOT__'
        bits = task + 'bits__DOT__'
        reset, valid = f(mp + 'resetFinish'), f(task + 'valid')
        task_set, task_tag = f(bits + 'set', 10), f(bits + 'tag', 30)
        read = f(directory + 'reqValid_s2$NEXT')
        tag_write = f(mp + '_io_tagWReq_valid_T_3')
        meta_write = f(mp + '_io_metaWReq_valid_T_6')
        response = f(directory + 'reqValid_s3')
        # MetaEntry is the actual 16-bit bundle repeated eight times at this SRAM port.
        # Chisel's field declaration order places state at bits14:13 in this model.
        widths = [('dirty', 1), ('state', 2), ('clients', 1), ('alias', 2), ('prefetch', 1),
                  ('prefetchSrc', 4), ('accessed', 1), ('tagErr', 1), ('dataErr', 1), ('cdpPfDepth', 2)]
        for name, width in widths:
            f(directory + 'metaAll_s3__DOT__' + name + '_0', width)
        packed = 'uint64_t(' + f(directory + 'metaArray__DOT__array_1_0_0__DOT___wdata_T', 128) + ')'
        origin = [f'uint64_t({reset} && {valid})', f(bits + 'reqSource', 5),
                  'uint64_t(' + ' | '.join(f'({f(bits + "channel$" + str(i) + "_" + str(i))} << {i})'
                                           for i in range(3)) + ')',
                  f(bits + 'opcode', 4), f(bits + 'mshrTask'), f(bits + 'mergeA')]
        zero = ['0'] * len(FIELDS)
        def values(**entries):
            row = zero.copy()
            for key, value in entries.items():
                row[FIELDS.index(key)] = value
            return row
        def sourced(row):
            row[7:13] = origin
            return row
        read_values = values(set=f('reqArb__DOT___io_dirRead_s1_bits_set_T', 10))
        tag_values = sourced(values(set=task_set, way_mask=f(mp + '_io_tagWReq_bits_wayOH_T_1', 8), tag=task_tag))
        meta_set = f'({reset} ? {task_set} : {f(mp + "resetIdx", 10)})'
        meta_values = sourced(values(set=meta_set, way_mask=f(mp + '_io_metaWReq_bits_wayOH_T_1', 8),
                                    state=f'(({packed} >> 13) & 3u)', prefetch=f'(({packed} >> 9) & 1u)',
                                    prefetch_source=f'(({packed} >> 5) & 15u)', accessed=f'(({packed} >> 4) & 1u)'))
        response_values = values(set=f(directory + 'req_s3__DOT__set', 10))
        for way in range(8):
            response_values[FIELDS.index('tag_' + str(way))] = f(directory + '_tagAll_s3_T' + (f'_{2*way}' if way else ''), 30)
            response_values[FIELDS.index('state_' + str(way))] = f(directory + f'metaAll_s3__DOT__state_{way}', 2)
        block = [f'  directory_counts[{bank}][0] += bool({read});',
                 f'  directory_counts[{bank}][1] += bool({tag_write});',
                 f'  directory_counts[{bank}][2] += bool({meta_write});',
                 f'  directory_counts[{bank}][3] += bool({response});']
        for kind, enable, row in [(1, read, read_values), (2, tag_write, tag_values),
                                  (3, meta_write, meta_values), (4, response, response_values)]:
            block.append(f'  if ({enable}) {{ const uint64_t row[{len(FIELDS)}] = {{{",".join(row)}}}; directory_write({kind},{bank},row); }}')
        block.append(f'  if(context.clean || context.dump) directory_footer(0,{bank});')
        captures.extend(block)
        bindings.append(dict(bank=bank, enables=[read, tag_write, meta_write, response], read=read_values,
                             tag_write=tag_values, meta_write=meta_values, response=response_values,
                             packed_meta=packed, packed_meta_fields=widths))
    helper = f'''
static FILE *directory_output=nullptr;
static uint64_t directory_counts[4][4]{{}},directory_last_cycle=0;
static void directory_finish();
static void directory_write(unsigned kind,unsigned bank,const uint64_t *row) {{
  if(!directory_output) {{
    directory_output=fopen("load-l2-directory.csv","w");
    if(!directory_output) {{perror("load-l2-directory.csv");abort();}}
    fprintf(directory_output,"core_cycle,clean,native_dump,kind,bank,{','.join(FIELDS)}\\n");
    std::atexit(directory_finish);
  }}
  fprintf(directory_output,"%lu,%u,%u,%u,%u",directory_last_cycle,
    kind==5 ? 0 : context.clean,kind==5 ? 0 : context.dump,kind,bank);
  for(unsigned i=0;i<{len(FIELDS)};++i) fprintf(directory_output,",%lu",row[i]);
  fprintf(directory_output,"\\n");
}}
static void directory_footer(unsigned kind,unsigned bank) {{
  uint64_t row[{len(FIELDS)}]{{}};
  for(unsigned i=0;i<4;++i) row[13+i]=directory_counts[bank][i];
  directory_write(kind,bank,row);
}}
static void directory_finish() {{
  if(!directory_output) return;
  for(unsigned bank=0;bank<4;++bank) directory_footer(5,bank);
  fclose(directory_output);directory_output=nullptr;
}}
static void directory_observe(SSimTop *model) {{
  if(!context.seen) return;
  directory_last_cycle=context.core;
{chr(10).join(captures)}
}}
'''
    anchor = 'static void route_observe(SSimTop *model)'
    assert cpp.count(anchor) == 1 and cpp.count('  route_observe(model);') == 1
    cpp = cpp.replace(anchor, helper + '\n' + anchor)
    cpp = cpp.replace('  route_observe(model);', '  route_observe(model);\n  directory_observe(model);')
    return cpp, dict(fields=FIELDS, banks=4, sets=1024, ways=8, directory_latency=2,
                     kinds=dict(boundary=0, read=1, tag_write=2, meta_write=3, response=4, final=5),
                     bindings=bindings, new_RTL_state_bits=0,
                     source_sha256={str(cache / name): digest(cache / name) for name in required},
                     scope='Actual existing logical tag/meta write-valid ports, including reset and invalidations. '
                           'Cumulative host-observer margins are independent of emitted data rows. '
                           'All-way response snapshots enable lifecycle checks against earlier accepted reads. '
                           'Source is the same MainPipe task, with merged-A status retained; not an exclusive cause.')


def build(previous_proof, output):
    previous_proof, output = Path(previous_proof).resolve(), Path(output).resolve()
    previous = json.loads(previous_proof.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and 'L2_demand_route_binding' in previous
    assert not output.exists()
    parent, root = previous_proof.parent, Path(previous['root'])
    assert digest(parent / 'probe.cpp') == previous['probe_cpp_sha256']
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    cpp, binding = add_capture(root, (root / 'build/gsim-compile/model/SimTop.h').read_text(),
                               (parent / 'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_bytes((parent / 'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in cmd] for cmd in previous['commands']]
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as stdout, (output / f'build-{i}.stderr').open('wb') as stderr:
            subprocess.run(command, stdout=stdout, stderr=stderr, check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    result = dict(previous, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'), commands=commands,
                  probe_cpp_sha256=digest(output / 'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  base_memory_build_proof_sha256=digest(previous_proof), L2_directory_binding=binding)
    (output / 'build-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--memory-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    value = build(args.memory_build_proof, args.output)
    print(json.dumps({name: value[name] for name in ['passed', 'source', 'binary', 'binary_sha256']}))


if __name__ == '__main__':
    main()
