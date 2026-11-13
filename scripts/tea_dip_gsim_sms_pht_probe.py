"""Observe actual PHT queue owners and service hazards in an existing targeted GSIM model."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest
from tea_dip_gsim_sms_internal_probe import SMS


def add_capture(root, header, cpp):
    declarations = dict(re.findall(r'uint(?:8|16|32|64)_t ([^;]+); // width = (\d+),', header))
    bindings = {}

    def field(name, width):
        assert declarations.get(name) == str(width), name
        bindings[name] = width
        return 'model->' + name

    prefix = SMS + 'pht__DOT__'
    source = root / 'src/main/scala/xiangshan/mem/prefetch/SMSPrefetcher.scala'
    queue_source = root / 'src/main/scala/utils/OverrideableQueue.scala'
    text = source.read_text()
    if re.search(r'val s0_evict\s*=\s*evict.valid\s*$', text, re.M):
        policy = 'update_priority'
        assert 'lookup.ready := evict.ready && !evict.valid' in text
    else:
        assert 'val s0_evict = evict.valid && (!lookup.valid || !s1_valid || !s1_evict)' in text
        assert 'evict.ready := s0_ready && s0_evict' in text and 'lookup.ready := s0_ready && !s0_evict' in text
        policy = 'existing_S1_type_alternation'
    assert 's1_wait := (s2_valid && s2_evict && s2_ram_waddr === s1_ram_raddr) || s3_ram_en' in text
    assert 's3_ram_en := s3_valid && s3_evict' in text
    qtext = queue_source.read_text()
    assert 'when(r && io.out.fire)' in qtext and 'when(io.in.valid && w)' in qtext
    assert qtext.index('v := false.B') < qtext.index('v := true.B')
    compact = re.sub(r'\s+', '', qtext)
    assert 'io.out.valid:=Mux1H(r_mask,valids)' in compact and 'io.out.bits:=Mux1H(r_mask,entries)' in compact
    assert 'valr_mask=(0untiln).map(i=>i.U===rd_ptr)' in compact
    assert compact.count('rd_ptr:=rd_ptr+1.U') == 1
    if 'valadvance_empty_head=!io.out.valid&&io.out.ready&&valids.reduce(_||_)' in compact:
        assert policy == 'update_priority'
        assert 'evict.ready := !s1_valid || !s1_wait' in text
        if 'valnext_valid_ptr=Mux1H(' in compact:
            assert 'valnext_valid_ptr=Mux1H((0untiln).map(i=>i.U===rd_ptr),(0untiln).map(i=>PriorityMux((1untiln).map(j=>valids((i+j)%n)->((i+j)%n).U(log2Up(n).W)))))' in compact
            assert 'when(io.out.fire){rd_ptr:=rd_ptr+1.U}.elsewhen(advance_empty_head){rd_ptr:=next_valid_ptr}' in compact
            read_policy = 'advance_nearest_ready'
        else:
            assert 'when(io.out.fire||advance_empty_head){rd_ptr:=rd_ptr+1.U}' in compact
            read_policy = 'advance_empty_ready'
    elif 'valadvance_empty_head=!io.out.valid&&valids.reduce(_||_)' in compact:
        assert 'when(io.out.fire||advance_empty_head){rd_ptr:=rd_ptr+1.U}' in compact
        read_policy = 'advance_empty'
    else:
        assert 'when(io.out.fire){rd_ptr:=rd_ptr+1.U}' in compact
        read_policy = 'hold_empty'
    generated = subprocess.check_output(['rg', '-l', '-F', *sum((['-e', 'sms_pht_' + k + 'Counter + '] for k in ['lookup_in', 'lookup_out', 'evict_in', 'evict_out']), []), str(root / 'build/gsim-compile/model'), '--glob', '*.cpp'], text=True).splitlines()
    equations = '\n'.join(Path(p).read_text() for p in generated)
    event = {}
    for name in ['lookup_in', 'lookup_out', 'evict_in', 'evict_out']:
        values = set(re.findall(r'sms_pht_' + name + r'Counter \+ \(\(uint64_t\)([\w$]+)\)', equations))
        value, = values
        event[name] = field(value, 1)
    current, nxt, masks, next_masks, reads, writes, next_reads, next_writes = [], [], [], [], [], [], [], []
    for q in ['lookup_queue', 'evict_queue']:
        base = prefix + q + '__DOT__'
        payload = []
        next_payload = []
        for slot in range(4):
            names = [('pht_index', 5), ('pht_tag', 13), ('region_offset', 4)]
            if q == 'lookup_queue':
                names.append(('region_paddr', 40))
            a = [field(base + f'entries_{slot}__DOT__' + n, w) for n, w in names]
            b = [field(base + f'entries_{slot}__DOT__' + n + '$NEXT', w) for n, w in names]
            if q == 'evict_queue':
                a.append('0'); b.append('0')
            payload.append(a); next_payload.append(b)
        current.append(payload); nxt.append(next_payload)
        masks.append(' | '.join(f'(unsigned({field(base + f"valids_{i}", 1)}) << {i})' for i in range(4)))
        next_masks.append(' | '.join(f'(unsigned({field(base + f"valids_{i}$NEXT", 1)}) << {i})' for i in range(4)))
        reads.append(field(base + 'rd_ptr', 2)); writes.append(field(base + 'wr_ptr', 2))
        next_reads.append(field(base + 'rd_ptr$NEXT', 2)); next_writes.append(field(base + 'wr_ptr$NEXT', 2))
    registers = [('s1_valid_r', 1), ('s1_evict', 1), ('s1_ram_raddr', 5), ('s2_valid_last_REG', 1), ('s2_evict', 1), ('s2_ram_waddr', 5), ('s3_valid_last_REG', 1), ('s3_evict', 1)]
    values = [field(prefix + n, w) for n, w in registers]
    tensor = lambda data: '{' + ','.join('{' + ','.join('{' + ','.join('uint64_t(' + x + ')' for x in row) + '}' for row in q) + '}' for q in data) + '}'
    array = lambda data: '{' + ','.join(data) + '}'
    helper = r'''
static bool pq_seen=false;
static uint64_t pq_committing[2][4][4]{};
static unsigned pq_committing_mask[2]{},pq_committing_read[2]{},pq_committing_write[2]{};
static FILE *pq_file=nullptr;
static void pht_snapshot(SSimTop *model) {
  const uint64_t data[2][4][4]=NEXT_DATA;
  const unsigned masks[2]=NEXT_MASKS,reads[2]=NEXT_READS,writes[2]=NEXT_WRITES;
  std::memcpy(pq_committing,data,sizeof(data));
  std::memcpy(pq_committing_mask,masks,sizeof(masks));
  std::memcpy(pq_committing_read,reads,sizeof(reads));
  std::memcpy(pq_committing_write,writes,sizeof(writes));
}
static void pht_observe(SSimTop *model) {
  if(!context.seen) return;
  if(!pq_file) {
    pq_file=fopen("sms-pht-queues.csv","w");assert(pq_file);
    fprintf(pq_file,"core_cycle,clean,native_dump,kind,queue,valid_mask,read_slot,write_slot,input,output,s1_valid,s1_update,s1_index,s2_valid,s2_update,s2_index,s3_valid,s3_update,raw_hazard,write_busy,head_index,head_tag,head_offset,head_region,input_index,input_tag,input_offset,input_region\n");
  }
  const uint64_t data[2][4][4]=CURRENT_DATA,next[2][4][4]=NEXT_DATA;
  const unsigned masks[2]=MASKS,reads[2]=READS,writes[2]=WRITES;
  const unsigned next_masks[2]=NEXT_MASKS,next_reads[2]=NEXT_READS,next_writes[2]=NEXT_WRITES;
  const unsigned inputs[2]=INPUTS,outputs[2]=OUTPUTS,pipe[8]=PIPE;
  const bool raw=pipe[3]&&pipe[4]&&pipe[5]==pipe[2],busy=pipe[6]&&pipe[7];
  const bool ready=!pipe[0]||!(raw||busy);
  const bool lookup=(masks[0]>>reads[0])&1,update=(masks[1]>>reads[1])&1;
  const bool select_update=SELECT_UPDATE;
  assert(outputs[0]==unsigned(lookup&&ready&&!select_update));
  assert(outputs[1]==unsigned(update&&ready&&select_update));
  for(unsigned q=0;q<2;++q) {
    const unsigned mask=masks[q],r=reads[q],w=writes[q],in=inputs[q],out=outputs[q];
    const bool advance_empty=ADVANCE_EMPTY;
    assert(!(out&&advance_empty));
    assert(mask==pq_committing_mask[q]&&r==pq_committing_read[q]&&w==pq_committing_write[q]);
    if(!pq_seen) assert(mask==0);
    for(unsigned slot=0;slot<4;++slot) if(mask>>slot&1) for(unsigned k=0;k<4;++k) assert(data[q][slot][k]==pq_committing[q][slot][k]);
    assert(!out||(mask>>r&1));
    const unsigned expected_mask=(mask&~(out?1u<<r:0))|(in?1u<<w:0);
    assert(next_masks[q]==expected_mask&&next_reads[q]==EXPECTED_READ_SLOT&&next_writes[q]==((w+in)&3));
    for(unsigned slot=0;slot<4;++slot) if(expected_mask>>slot&1) if(!(in&&slot==w)) for(unsigned k=0;k<4;++k) assert(next[q][slot][k]==data[q][slot][k]);
    const uint64_t zero[4]{};
    const uint64_t *head=(mask>>r&1)?data[q][r]:zero,*incoming=in?next[q][w]:zero;
    for(unsigned kind=0;kind<2;++kind) {
      if(kind==0&&!(context.clean||context.dump)) continue;
      if(kind==1&&pq_seen&&!mask&&!in&&!out&&!pipe[0]) continue;
      fprintf(pq_file,"%lu,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%lu,%lu,%lu,%lu,%lu,%lu,%lu,%lu\n",
        context.core,context.clean,context.dump,kind,q,mask,r,w,in,out,pipe[0],pipe[1],pipe[2],pipe[3],pipe[4],pipe[5],pipe[6],pipe[7],unsigned(raw),unsigned(busy),head[0],head[1],head[2],head[3],incoming[0],incoming[1],incoming[2],incoming[3]);
    }
  }
  pq_seen=true;
}
'''
    replacements = dict(NEXT_DATA=tensor(nxt), CURRENT_DATA=tensor(current), MASKS=array(masks), NEXT_MASKS=array(next_masks), READS=array(reads), WRITES=array(writes), NEXT_READS=array(next_reads), NEXT_WRITES=array(next_writes), INPUTS=array([event['lookup_in'], event['evict_in']]), OUTPUTS=array([event['lookup_out'], event['evict_out']]), PIPE=array(values), SELECT_UPDATE='update' if policy == 'update_priority' else 'update&&(!lookup||!pipe[0]||!pipe[1])')
    replacements['ADVANCE_EMPTY'] = 'bool(mask&&!(mask>>r&1))' if read_policy == 'advance_empty' else 'false'
    if read_policy in ['advance_empty_ready', 'advance_nearest_ready']:
        replacements['ADVANCE_EMPTY'] = 'bool(mask&&!(mask>>r&1)&&(q==0?ready&&!update:ready))'
    replacements['EXPECTED_READ_SLOT'] = '((r+out+unsigned(advance_empty))&3)'
    if read_policy == 'advance_nearest_ready':
        replacements['EXPECTED_READ_SLOT'] = '([&](){unsigned target=r;if(out)target=(r+1)&3;else if(advance_empty)for(unsigned step=1;step<4;++step)if(mask>>((r+step)&3)&1){target=(r+step)&3;break;}return target;}())'
    for name in sorted(replacements, key=len, reverse=True):
        helper = helper.replace(name, replacements[name])
    for old, new in [('static void sms_snapshot(SSimTop *model)', helper + '\nstatic void sms_snapshot(SSimTop *model)'), ('  sms_snapshot(model);', '  pht_snapshot(model);\n  sms_snapshot(model);'), ('  sms_observe(model);', '  sms_observe(model);\n  pht_observe(model);')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(queue_slots=4, policy=policy, queue_read_policy=read_policy, actual_counter_events=event, current_NEXT_and_queue_equations_asserted=True, service_selection_and_hazards_asserted=True, new_RTL_state_bits=0, bindings=bindings, source_sha256={str(p): digest(p) for p in [source, queue_source, *map(Path, generated)]}, scope='Physical four-slot owners, input generations, pre-write departures and overwrite fates. Wait masks are module cycles, not CPU loss; PHT index/tag are not full PC or unique load causality.')


def build(proof_path, output):
    proof_path, output = Path(proof_path).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    parent, root = proof_path.parent, Path(previous['root'])
    assert previous['passed'] and previous['original_files_unchanged'] and not output.exists()
    assert previous['SMS_internal_binding']['actual_filter_allocation_edges']
    assert digest(parent / 'probe.cpp') == previous['probe_cpp_sha256']
    for name, value in previous['original_sha256'].items():
        assert digest(Path(name)) == value, name
    cpp, binding = add_capture(root, (root / 'build/gsim-compile/model/SimTop.h').read_text(), (parent / 'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_bytes((parent / 'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as out, (output / f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, value in previous['original_sha256'].items():
        assert digest(Path(name)) == value, name
    proof = dict(previous, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'), commands=commands, probe_cpp_sha256=digest(output / 'probe.cpp'), generator_sha256=digest(Path(__file__)), base_targeted_build_proof_sha256=digest(proof_path), SMS_PHT_binding=binding, scope='Eight source-qualified S2/WB/SMS/PHT CSVs, with actual source-bound read-pointer policy and NEXT equations. Host-only observer; own-source execution neutrality, semantics and cost require closed calibration.')
    (output / 'build-proof.json').write_text(json.dumps(proof, indent=2) + '\n')
    return proof


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--sms-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    proof = build(args.sms_build_proof, args.output)
    print(json.dumps({k: proof[k] for k in ['passed', 'source', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
