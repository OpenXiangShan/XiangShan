"""Extend an accepted L1 MSHR host relink with actual L2 TXREQ and RXDAT events."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest

PREFIX = 'cpu__DOT__l_soc__DOT__core_with_l2__DOT__l2top__DOT__inner__DOT__l2cache__DOT__'
FIELDS = ['valid', 'line', 'source_id', 'channel', 'opcode', 'req_source',
          'acquired', 'first', 'last', 'grant', 'refill', 'has_data']
REGS = [('req_valid', 1), ('req__DOT__tag', 30), ('req__DOT__set', 10),
        ('req__DOT__sourceId', 8), ('req__DOT__channel', 3), ('req__DOT__opcode', 4),
        ('req__DOT__reqSource', 5), ('state__DOT__s_acquire', 1),
        ('state__DOT__w_grantfirst', 1), ('state__DOT__w_grantlast', 1),
        ('state__DOT__w_grant', 1), ('state__DOT__s_refill', 1), ('gotGrantData', 1)]


def add_capture(root, header, cpp):
    cache = root / 'XSCache/src/main/scala/coupledL2'
    mshr = (cache / 'MSHR.scala').read_text()
    txreq = (cache / 'TXREQ.scala').read_text()
    rxdat = (cache / 'RXDAT.scala').read_text()
    assert 'when (io.tasks.txreq.fire)' in mshr and 'oa.txnID := io.id' in mshr
    assert 'io.out <> queue.io.deq' in txreq and 'flow = false' in txreq
    assert 'io.out.ready := true.B' in rxdat

    def field(name, width, array=''):
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(name + array)
                         + r'; // width = ' + str(width) + ',', header), name
        return 'model->' + name

    captures, snapshots, queues, queue_snapshots, bindings = [], [], [], [], []
    for bank in range(4):
        prefix = PREFIX + f'slices_{bank}__DOT__'
        for slot in range(16):
            base = prefix + f'mshrCtl__DOT__mshrs_{slot}__DOT__'
            current = [field(base + name, width) for name, width in REGS]
            nxt = [field(base + name + '$NEXT', width) for name, width in REGS]
            alloc = field(prefix + f'mshrCtl__DOT___mshrs_{slot}_io_alloc_valid_T_1', 1)
            grant = field(prefix + f'mshrCtl__DOT___mshrs_{slot}_io_resps_rxdat_valid_T_2', 1)
            data_base = PREFIX + 'rxdatPipe_pipeline__DOT__stages_0__DOT__ram__DOT__'
            opcode, data_id = field(data_base + 'opcode', 4), field(data_base + 'dataID', 2)
            index = bank * 16 + slot
            def values(regs):
                return [regs[0], f'((uint64_t({regs[1]}) << 18) | (uint64_t({regs[2]}) << 8) | {bank*64}u)', *regs[3:]]
            for j, value in enumerate(values(nxt)):
                snapshots.append(f'  l2_next[{index}][{j}] = ' +
                                 (f'({nxt[0]} ? uint64_t({value}) : 0);' if 1 <= j <= 5
                                  else f'uint64_t({value});'))
            assignments = '\n'.join(f'    now[{j}] = uint64_t({value});'
                                    for j, value in enumerate(values(current)))
            captures.append(f'''  {{
    const unsigned index = {index};
    uint64_t now[12]{{}};
{assignments}
    if (!now[0]) for (unsigned j = 1; j <= 5; ++j) now[j] = 0;
    if (context.seen) {{
      for (unsigned j = 0; j < 12; ++j) assert(l2_next[index][j] == now[j]);
      if (!l2_seen || std::memcmp(now,l2_previous[index],sizeof(now)) || context.clean || context.dump)
        l2_write(1,{bank},{slot},now,0,0);
      if ({alloc}) {{ assert(!now[0] && {nxt[0]}); l2_write(3,{bank},{slot},now,0,0); }}
      if ({grant}) {{
        assert(now[0]);
        l2_write(2,{bank},{slot},now,unsigned({opcode}),unsigned({data_id}));
      }}
      std::memcpy(l2_previous[index],now,sizeof(now));
    }}
  }}''')
            bindings.append(dict(bank=bank, slot=slot, current=current, next=nxt,
                                 allocation=alloc, RXDAT_valid=grant, RXDAT_opcode=opcode, RXDAT_data_id=data_id))
        q = prefix + 'txreq__DOT__queue__DOT__'
        current = [field(q + name, width) for name, width in
                   [('enq_ptr_value', 4), ('deq_ptr_value', 4), ('maybe_full', 1)]]
        nxt = [field(q + name + '$NEXT', width) for name, width in
               [('enq_ptr_value', 4), ('deq_ptr_value', 4), ('maybe_full', 1)]]
        arrays = [field(q + 'ram__DOT__' + name, width, '[16]')
                  for name, width in [('txnID', 12), ('opcode', 7), ('addr', 48)]]
        queue_snapshots.extend(f'  l2q_next[{bank}][{j}] = {value};' for j, value in enumerate(nxt))
        queues.append(f'''  {{
    unsigned before[3] = {{{','.join(current)}}}, after[3] = {{{','.join(nxt)}}};
    if (context.seen) {{
      for (unsigned j = 0; j < 3; ++j) assert(l2q_next[{bank}][j] == before[j]);
      const unsigned count = l2q_count(before), next_count = l2q_count(after);
      const unsigned enq = (after[0]-before[0]) & 15u, deq = (after[1]-before[1]) & 15u;
      assert(enq <= 1 && deq <= 1 && count + enq == next_count + deq);
      assert(!deq || count);
      if (!l2_seen || context.clean || context.dump || enq || deq)
        l2q_write(1,{bank},before,after,count,next_count,0,0,0);
      if (enq) l2q_write(2,{bank},before,after,count,next_count,{arrays[0]}[before[0]],{arrays[1]}[before[0]],{arrays[2]}[before[0]]);
      if (deq) l2q_write(3,{bank},before,after,count,next_count,{arrays[0]}[before[1]],{arrays[1]}[before[1]],{arrays[2]}[before[1]]);
    }}
  }}''')
    helper = f'''
static uint64_t l2_next[64][12]{{}}, l2_previous[64][12]{{}};
static unsigned l2q_next[4][3]{{}};
static bool l2_seen = false;
static FILE *l2_output = nullptr, *l2q_output = nullptr;
static void l2_write(unsigned kind,unsigned bank,unsigned slot,const uint64_t *values,unsigned opcode,unsigned data_id) {{
  if (!l2_output) {{
    l2_output=fopen("load-l2-mshr.csv","w");
    if (!l2_output) {{ perror("load-l2-mshr.csv"); abort(); }}
    fprintf(l2_output,"core_cycle,clean,native_dump,kind,bank,mshr,{','.join(FIELDS)},rxdat_opcode,rxdat_data_id\\n");
  }}
  fprintf(l2_output,"%lu,%u,%u,%u,%u,%u",context.core,context.clean,context.dump,kind,bank,slot);
  for (unsigned j=0;j<12;++j) fprintf(l2_output,",%lu",values[j]);
  fprintf(l2_output,",%u,%u\\n",opcode,data_id);
}}
static unsigned l2q_count(const unsigned *p) {{ return p[0]==p[1] ? (p[2] ? 16u : 0u) : ((p[0]-p[1]) & 15u); }}
static void l2q_write(unsigned kind,unsigned bank,const unsigned *p,const unsigned *n,unsigned count,unsigned next_count,unsigned txn,unsigned opcode,uint64_t address) {{
  if (!l2q_output) {{
    l2q_output=fopen("load-l2-txreq.csv","w");
    if (!l2q_output) {{ perror("load-l2-txreq.csv"); abort(); }}
    fprintf(l2q_output,"core_cycle,clean,native_dump,kind,bank,enq_ptr,deq_ptr,full,next_enq_ptr,next_deq_ptr,next_full,count,next_count,txn_id,opcode,line\\n");
  }}
  const uint64_t line=kind>=2 ? ((address >> 6) << 8) | (uint64_t(bank)<<6) : 0;
  fprintf(l2q_output,"%lu,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%lu\\n",context.core,context.clean,context.dump,kind,bank,p[0],p[1],p[2],n[0],n[1],n[2],count,next_count,txn,opcode,line);
}}
static void l2_snapshot(SSimTop *model) {{
{chr(10).join(snapshots)}
{chr(10).join(queue_snapshots)}
}}
static void l2_observe(SSimTop *model) {{
  if (context.seen && (context.clean || context.dump)) {{
    const uint64_t empty[12]{{}}; const unsigned zero[3]{{}};
    l2_write(0,4,16,empty,0,0); l2q_write(0,4,zero,zero,0,0,0,0,0);
  }}
{chr(10).join(captures)}
{chr(10).join(queues)}
  if (context.seen) l2_seen=true;
}}
'''
    for old, new in [('static void miss_snapshot(SSimTop *model)', helper + '\nstatic void miss_snapshot(SSimTop *model)'),
                     ('  miss_snapshot(model);', '  l2_snapshot(model);\n  miss_snapshot(model);'),
                     ('  miss_observe(model);', '  miss_observe(model);\n  l2_observe(model);')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(banks=4, entries_per_bank=16, TXREQ_entries=16, fields=FIELDS, bindings=bindings,
                     next_register_commit_asserted=True, TXREQ_pointer_conservation_asserted=True,
                     RTL_source_sha256={str(cache/name): digest(cache/name) for name in
                                        ['MSHR.scala','MSHRCtl.scala','TXREQ.scala','RXDAT.scala','CoupledL2.scala']},
                     scope='Actual MSHR-qualified RXDAT and queue pointer handshakes. s_acquire is enqueue-side, not queue exit. Cross-level source mapping and closed-output neutrality still need calibration.')


def build(mshr_build_proof, output):
    proof_path, output = Path(mshr_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and previous['original_objects'] == 577
    assert previous['generator_sha256'] == digest(Path(__file__).with_name('tea_dip_gsim_mshr_return_probe.py'))
    parent, root = proof_path.parent, Path(previous['root'])
    assert digest(parent/'probe.cpp') == previous['probe_cpp_sha256'] and not output.exists()
    for name, value in previous['original_sha256'].items():
        assert digest(Path(name)) == value, name
    cpp, binding = add_capture(root, (root/'build/gsim-compile/model/SimTop.h').read_text(), (parent/'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output/f'build-{i}.stdout').open('wb') as out, (output/f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, value in previous['original_sha256'].items():
        assert digest(Path(name)) == value, name
    result = dict(previous, binary=str(output/'emu'), binary_sha256=digest(output/'emu'), commands=commands,
                  probe_cpp_sha256=digest(output/'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  base_MSHR_build_proof_sha256=digest(proof_path), L2_return_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result, indent=2)+'\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mshr-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = build(args.mshr_build_proof, args.output)
    print(json.dumps({name: result[name] for name in ['passed','binary','binary_sha256','original_objects']}))


if __name__ == '__main__':
    main()
