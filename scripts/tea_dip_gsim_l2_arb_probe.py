"""Add source-bound, read-only L2 SinkA eligibility masks to a route observer."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_l2_return_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

CAUSES = ['directory_not_ready', 'reset_not_finished', 'MSHR_task', 'DS_MCP2',
          'hint_staging_stall', 'MSHR_full', 'grant_buffer', 'SinkB_priority', 'SinkC_priority']
FIELDS = ['line', 'source_id', 'opcode', 'param', 'req_source', 'ready', 'chosen_valid', 'cause_mask']


def add_capture(root, header, cpp, route):
    source = root / 'XSCache/src/main/scala/coupledL2/RequestArb.scala'
    text = source.read_text()
    for fragment in ['val sink_ready_basic = io.dirRead_s1.ready && resetFinish && !mshr_task_s1.valid && s2_ready',
                     'io.sinkA.ready := sink_ready_basic && !block_A && !sinkValids(1) && !sinkValids(0)',
                     's2_ready  := !ds_mcp2_stall && !hint_stall',
                     'val hint_stall = (mshr_wouldHint_s1 || chnl_wouldHint_s1) && io.hint_s1QueueFull']:
        assert fragment in text, fragment
    captures, bindings = [], []
    for bank in range(4):
        prefix = PREFIX + f'slices_{bank}__DOT__'
        def field(name, width=1):
            full = prefix + name
            assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(full)
                             + r'; // width = ' + str(width) + ',', header), full
            return 'model->' + full
        arb = 'reqArb__DOT__'
        directory = field('directory__DOT___io_read_ready_T_4')
        reset = field(arb + 'resetFinish')
        mshr = field(arb + 'mshr_task_s1__DOT__valid')
        ds = field(arb + 'ds_mcp2_stall')
        valids = field(arb + 'sinkValids', 3)
        mshr_full = field('mshrCtl__DOT__a_mshrFull')
        grant = field('grantBuf__DOT___io_toReqArb_blockSinkReqEntrance_blockA_s1_T_1')
        block_a_not = field(arb + '_sinkValids_T_4')
        actual_s2_ready = field(arb + '_s2_ready_T_2')
        from_a = field(arb + 'io__DOT__mshrHintQInfo__DOT__bits__DOT__channel')
        opcode = field(arb + 'mshr_task_s1__DOT__bits__DOT__opcode', 4)
        merged = field(arb + 'mshr_task_s1__DOT__bits__DOT__mergeA')
        merge_opcode = field(arb + 'mshr_task_s1__DOT__bits__DOT__aMergeTask__DOT__opcode', 3)
        queue = 'mainPipe__DOT__customL1Hint__DOT__hint_s1Queue__DOT__'
        full = f'({field(queue+"ptr_match")} && {field(queue+"maybe_full")})'
        hint = (f'(({mshr} && {from_a} && ({opcode}==4 || {opcode}==5 || {opcode}==1 || {opcode}==8 || '
                f'({merged} && ({merge_opcode}==4 || {merge_opcode}==5)))) || '
                f'(!{mshr} && ({valids}&1))) && {full}')
        causes = [f'!{directory}', f'!{reset}', mshr, ds, hint, mshr_full, grant,
                  f'({valids}>>1)&1', f'{valids}&1']
        row = route['bindings'][bank]['output']
        assert bank == route['bindings'][bank]['bank'] and len(row) == 16
        chosen = field('reqBuf__DOT__chosenQ__DOT___io_deq_valid_T')
        # Generated block_A has no MainPipe term in these actual models. Check the
        # formula in the generated source and again against its retained complement.
        lhs = prefix + arb + 'block_A'
        pattern = r'^\s*' + re.escape(lhs) + r'\s*='
        result = subprocess.run(['rg', '--no-heading', '--no-filename', '--glob', '*.cpp',
                                 pattern, str(root/'build/gsim-compile/model')],
                                text=True, capture_output=True, check=True)
        expected = f'{lhs} = ({mshr_full[7:]} | {grant[7:]});'
        assert result.stdout.strip() == expected, result.stdout
        values = [*row[1:7], chosen, 'mask']
        captures.append(f'''  {{
    const bool c[9]={{{','.join('bool('+value+')' for value in causes)}}}; uint64_t mask=0;
    for(unsigned j=0;j<9;++j) mask |= uint64_t(c[j])<<j;
    assert(bool({block_a_not}) == !(c[5] || c[6]));
    assert(bool({actual_s2_ready}) == !(c[3] || c[4]));
    assert(bool({row[6]}) == (mask==0));
    if ({row[0]}) {{ const uint64_t values[8]={{{','.join(values)}}}; arb_write(1,{bank},values); }}
  }}''')
        bindings.append(dict(bank=bank, cause_expressions=causes, output=values,
                             generated_block_A_equation=expected))
    helper = f'''
static FILE *arb_output=nullptr;
static void arb_write(unsigned kind,unsigned bank,const uint64_t *values) {{
  if(!arb_output) {{
    arb_output=fopen("l2-request-arbitration.csv","w");
    if(!arb_output) {{perror("l2-request-arbitration.csv");abort();}}
    fprintf(arb_output,"core_cycle,clean,native_dump,kind,bank,{','.join(FIELDS)}\\n");
  }}
  fprintf(arb_output,"%lu,%u,%u,%u,%u",context.core,context.clean,context.dump,kind,bank);
  for(unsigned j=0;j<8;++j) fprintf(arb_output,",%lu",values[j]);
  fprintf(arb_output,"\\n");
}}
static void arb_observe(SSimTop *model) {{
  if(!context.seen) return;
  if(context.clean || context.dump) {{const uint64_t zero[8]{{}};arb_write(0,4,zero);}}
{chr(10).join(captures)}
}}
'''
    for old, new in [('static void l2_snapshot(SSimTop *model)', helper+'\nstatic void l2_snapshot(SSimTop *model)'),
                     ('  route_observe(model);', '  route_observe(model);\n  arb_observe(model);')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(fields=FIELDS, causes=CAUSES, bindings=bindings,
                     source_sha256={str(source): digest(source)},
                     ready_and_s2_formula_asserted=True,
                     scope='Actual SinkA valid cycles, including unaccepted presentations; masks overlap. '
                           'MainPipe block_A is generated constant zero in these bound models. '
                           'Bank-cycles and local request intervals are not CPU lost cycles.')


def build(route_build_proof, output):
    proof_path, output = Path(route_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and 'L2_demand_route_binding' in previous
    assert not output.exists()
    parent, root = proof_path.parent, Path(previous['root'])
    assert digest(parent/'probe.cpp') == previous['probe_cpp_sha256']
    for name, expected in previous['original_sha256'].items():
        assert digest(name) == expected, name
    cpp, binding = add_capture(root, (root/'build/gsim-compile/model/SimTop.h').read_text(),
                               (parent/'probe.cpp').read_text(), previous['L2_demand_route_binding'])
    output.mkdir()
    (output/'probe.cpp').write_text(cpp)
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output/f'build-{i}.stdout').open('wb') as out, (output/f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(name) == expected, name
    result = dict(previous, binary=str(output/'emu'), binary_sha256=digest(output/'emu'), commands=commands,
                  probe_cpp_sha256=digest(output/'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  parent_route_build_proof_sha256=digest(proof_path), L2_arbitration_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result, indent=2)+'\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--route-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    value = build(args.route_build_proof, args.output)
    print(json.dumps({name: value[name] for name in ['passed', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
