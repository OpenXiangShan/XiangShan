"""Add registered NCB and LLC response identities to an accepted memory/merge relink."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest

NCB = 'cpu__DOT__l_soc__DOT__chi_llcBridge_opt__DOT__uNCB200__DOT__uTransactionQueue__DOT__regQueue__DOT__'
LLC = 'cpu__DOT__l_soc__DOT__chi_openllc_opt__DOT__'
NCB_FIELDS = ['valid', 'line', 'src_id', 'txn_id', 'return_txn_id', 'ar_pending', 'r_pending']
LLC_FIELDS = ['valid', 'line', 'src_id', 'txn_id', 'req_id', 'tgt_id', 'is_miss']


def add_capture(root, header, cpp):
    declarations = dict(re.findall(r'uint(?:8|16|32|64)_t ([^;]+); // width = (\d+),', header))

    def field(name, width, array=''):
        assert declarations.get(name + array)==str(width), name
        return 'model->' + name

    captures, snapshots, bindings = [], [], []
    for kind in ['ncb', 'llc']:
        for bank in range(1 if kind == 'ncb' else 4):
            for slot in range(64 if kind == 'ncb' else 16):
                if kind == 'ncb':
                    regs = [('valid', 1), ('bits__DOT__operand__DOT__axi__DOT__Addr', 49),
                            ('bits__DOT__info__DOT__SrcID', 11), ('bits__DOT__info__DOT__TxnID', 12),
                            ('bits__DOT__info__DOT__ReturnTxnID', 12),
                            ('bits__DOT__op__DOT__axi__DOT__ReadAddress__DOT__valid', 1),
                            ('bits__DOT__op__DOT__axi__DOT__ReadData__DOT__valid', 1)]
                    current = [field(NCB + name + f'_{slot}', width) for name, width in regs]
                    nxt = [field(NCB + name + f'$NEXT_{slot}', width) for name, width in regs]
                else:
                    prefix = LLC + f'slices_{bank}__DOT__responseUnit__DOT__buffer__DOT__'
                    regs = [('valid', 1), ('task__DOT__tag', 27), ('task__DOT__set', 13),
                            ('task__DOT__bank', 2), ('task__DOT__srcID', 11), ('task__DOT__txnID', 12),
                            ('task__DOT__reqID', 12), ('task__DOT__tgtID', 11), ('is_miss', 1)]
                    current = [field(prefix + name, width, '[16]') + f'[{slot}]' for name, width in regs]
                    nxt = [field(prefix + name + '$NEXT', width, '[16]') + f'[{slot}]' for name, width in regs]

                def normalized(values):
                    if kind == 'ncb':
                        values = [values[0], f'(uint64_t({values[1]}) & ~uint64_t(63))', *values[2:]]
                    else:
                        values = [values[0], f'((uint64_t({values[1]})<<21) | (uint64_t({values[2]})<<8) | (uint64_t({values[3]})<<6))', *values[4:]]
                    return [values[0], *[f'({values[0]} ? uint64_t({v}) : 0)' for v in values[1:]]]

                index = bank * 16 + slot
                snapshots.extend(f'  {kind}_lineage_next[{index}][{j}] = uint64_t({v});'
                                 for j, v in enumerate(normalized(nxt)))
                captures.append(f'''  {{
    const uint64_t now[7] = {{{','.join('uint64_t('+v+')' for v in normalized(current))}}};
    if (context.seen) {{
      for (unsigned j=0;j<7;++j) assert({kind}_lineage_next[{index}][j]==now[j]);
      if (!lineage_seen || std::memcmp(now,{kind}_lineage_previous[{index}],sizeof(now)) || context.clean || context.dump)
        lineage_write({1 if kind == 'ncb' else 2},1,{bank},{slot},now);
      std::memcpy({kind}_lineage_previous[{index}],now,sizeof(now));
    }}
  }}''')
                bindings.append(dict(kind=kind, bank=bank, slot=slot, current=current, next=nxt))
    helper = f'''
static uint64_t ncb_lineage_next[64][7]{{}}, ncb_lineage_previous[64][7]{{}};
static uint64_t llc_lineage_next[64][7]{{}}, llc_lineage_previous[64][7]{{}};
static bool lineage_seen=false;
static FILE *lineage_outputs[2]{{}};
static void lineage_write(unsigned stream,unsigned kind,unsigned bank,unsigned slot,const uint64_t *values) {{
  FILE *&out=lineage_outputs[stream-1];
  if (!out) {{
    const char *name=stream==1 ? "load-ncb-state.csv" : "load-llc-response.csv";
    out=fopen(name,"w"); if (!out) {{ perror(name); abort(); }}
    fprintf(out,"core_cycle,clean,native_dump,kind,bank,slot,%s\\n",stream==1 ? "{','.join(NCB_FIELDS)}" : "{','.join(LLC_FIELDS)}");
  }}
  fprintf(out,"%lu,%u,%u,%u,%u,%u",context.core,context.clean,context.dump,kind,bank,slot);
  for (unsigned j=0;j<7;++j) fprintf(out,",%lu",values[j]);
  fprintf(out,"\\n");
}}
static void lineage_snapshot(SSimTop *model) {{
{chr(10).join(snapshots)}
}}
static void lineage_observe(SSimTop *model) {{
  if (context.seen && (context.clean || context.dump)) {{
    const uint64_t zero[7]{{}}; lineage_write(1,0,0,64,zero); lineage_write(2,0,4,16,zero);
  }}
{chr(10).join(captures)}
  if (context.seen) lineage_seen=true;
}}
'''
    for old, new in [('static void merge_snapshot(SSimTop *model)', helper + '\nstatic void merge_snapshot(SSimTop *model)'),
                     ('  merge_snapshot(model);', '  lineage_snapshot(model);\n  merge_snapshot(model);'),
                     ('  merge_observe(model);', '  merge_observe(model);\n  lineage_observe(model);')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(NCB_slots=64, LLC_banks=4, LLC_slots_per_bank=16,
                     NCB_fields=NCB_FIELDS, LLC_fields=LLC_FIELDS, bindings=bindings,
                     next_register_commit_asserted=True, new_RTL_state_bits=0,
                     source_sha256={str(root/name):digest(root/name) for name in [
                         'src/main/scala/top/Top.scala',
                         'XSCache/OpenNCB/src/main/scala/openncb/logical/NCBTransactionQueue.scala',
                         'XSCache/src/main/scala/coupledL2/CoupledL2.scala',
                         'XSCache/src/main/scala/openLLC/chi/RXREQ.scala',
                         'XSCache/src/main/scala/openLLC/chi/LinkLayer.scala',
                         'XSCache/src/main/scala/openLLC/MainPipe.scala',
                         'XSCache/src/main/scala/openLLC/ResponseUnit.scala',
                         'XSCache/src/main/scala/openLLC/Common.scala',
                         'XSCache/src/main/scala/openLLC/LLCParam.scala']},
                     scope='Registered valid-qualified identities after the committing edge, including warmup and ROI. State changes are not external AXI/CHI handshakes. Identity and latency joins require natural closure and independent calibration.')


def route_proof(root):
    n = 'cpu__DOT__l_soc__DOT__nocMisc__DOT__'
    q = '__DOT__nodeOut_ar_deq_q__DOT__'
    spill = 'cpu__DOT__l_soc__DOT__chi_llcBridge_opt__DOT__uNCB200__DOT__uAR__DOT__wireSpillAR_uSpillRegister__DOT___io_out_bits_T__DOT__id'
    mem = 'cpu__DOT__l_memXbar__DOT__axi4xbar__DOT__'
    expressions = [
        n+'axi4xbar__DOT__in__DOT__ar__DOT__bits__DOT__id_0 = '+spill+';',
        n+'axi4index__DOT__nodeOut__DOT__ar__DOT__bits__DOT__id = ((uint16_t)'+n+'axi4xbar__DOT___out_0_ar_bits_T_26);',
        mem+'_in_0_ar_bits_id_T = (((uint32_t)'+n+'axi4buf'+q+'io_deq_bits_MPORT__DOT__id) | 0x20000);',
        'cpu__DOT__memory__DOT__ram__DOT__arQueue__DOT__ram__DOT__id[cpu__DOT__memory__DOT__ram__DOT__arQueue__DOT__enq_ptr_value] = '+mem+'_out_0_ar_bits_T_26;']
    sn = LLC+'snLinkMonitor__DOT__'
    expressions.append(sn+'_T_1 = ((-(uint16_t)'+sn+'_T & '+LLC+'mmioMerger__DOT___io_out_tx_req_bits_T__DOT__srcID) | ((-(uint16_t)!'+sn+'_T) & 0x2));')
    for suffix, upstream in [('_2', n+'axi4index__DOT__nodeOut__DOT__ar__DOT__bits__DOT__id'),
                             ('_1', n+'axi4buf_2'+q+'io_deq_bits_MPORT__DOT__id'),
                             ('', n+'axi4buf_1'+q+'io_deq_bits_MPORT__DOT__id')]:
        base = n+'axi4buf'+suffix+q
        expressions.extend([base+'ram__DOT__id['+base+'enq_ptr_value] = '+upstream+';',
                            base+'io_deq_bits_MPORT__DOT__id = '+base+'ram__DOT__id['+base+'deq_ptr_value];'])
    for prefix, own in [(n+'axi4xbar__DOT__', n+'axi4xbar__DOT__in__DOT__ar__DOT__bits__DOT__id_0'),
                        (mem, mem+'_in_0_ar_bits_id_T')]:
        # Both arbiters preserve the selected input's ID; the other mux input is
        # a distinct master namespace. Record the exact generated equations.
        expressions.extend([prefix+'_out_0_ar_bits_T_26 = ('+prefix+'_out_0_ar_bits_T_24 | '+prefix+'_out_0_ar_bits_T_25);'])
        typename = 'uint8_t' if prefix.startswith(n) else 'uint32_t'
        expressions.append(prefix+'_out_0_ar_bits_T_24 = ((-('+typename+')'+prefix+'muxState_0 & '+own+') | ((-('+typename+')!'+prefix+'muxState_0) & 0x0));')
    command = ['rg', '-l', '-F']
    for text in expressions:
        command.extend(['-e', text])
    command.extend([str(root/'build/gsim-compile/model'), '--glob', '*.cpp'])
    paths = [Path(p) for p in subprocess.check_output(command,text=True).splitlines()]
    text = '\n'.join(p.read_text() for p in paths)
    for expression in expressions:
        assert expression in text, expression
    return dict(NCB_read_API_namespace=0x20000, slot_mask=63,
                generated_equations=expressions, source_sha256={str(p):digest(p) for p in paths})


def build(merge_build_proof, output):
    proof_path, output = Path(merge_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and previous['original_objects']==577
    assert previous['generator_sha256']==digest(Path(__file__).with_name('tea_dip_gsim_l2_merge_probe.py'))
    parent, root = proof_path.parent, Path(previous['root'])
    assert digest(parent/'probe.cpp')==previous['probe_cpp_sha256'] and not output.exists()
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name))==expected, name
    cpp, binding = add_capture(root,(root/'build/gsim-compile/model/SimTop.h').read_text(),(parent/'probe.cpp').read_text())
    binding['AXI_ID_route'] = route_proof(root)
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent),str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output/f'build-{i}.stdout').open('wb') as stdout, (output/f'build-{i}.stderr').open('wb') as stderr:
            subprocess.run(command,stdout=stdout,stderr=stderr,check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name))==expected, name
    result = dict(previous,binary=str(output/'emu'),binary_sha256=digest(output/'emu'),commands=commands,
                  probe_cpp_sha256=digest(output/'probe.cpp'),generator_sha256=digest(Path(__file__)),
                  base_merge_build_proof_sha256=digest(proof_path),memory_lineage_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--merge-build-proof',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    result=build(args.merge_build_proof,args.output)
    print(json.dumps({key:result[key]for key in ['passed','binary','binary_sha256','original_objects']}))


if __name__=='__main__':
    main()
