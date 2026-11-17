"""Capture actual bank prefetch FIFO ownership and the following pipeline/port."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_l2_return_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

FIELDS = ['head', 'tail', 'maybe_full', 'enqueue', 'pipe_ready', 'flow', 'overwrite',
          'pipe_valid', 'prefetch_ready', 'reqbuf_ready', 'a_valid',
          *[stage + '_' + name for stage in ['input', 'head', 'pipe']
            for name in ['line', 'source_id', 'req_source', 'need_t']],
          'enqueue_count', 'dequeue_count', 'overwrite_count', 'port_fire_count']


def add_capture(root, header, cpp):
    cache = Path(root) / 'XSCache/src/main/scala/coupledL2'
    requirements = {
        'utils/OverwriteQueue.scala': ['io.enq.ready := true.B',
            'val overwriteHappens =  full &&  do_enq && !do_deq',
            'io.deq.bits := Mux(flowHappens, io.enq.bits, queue(headPtr))'],
        'prefetch/Prefetcher.scala': ['pipe(i).io.in <> pftQueue(i).io.deq',
            'io.req(i) <> pipe(i).io.out', 'hasFlow = true'],
        'SinkA.scala': ['io.prefetchReq.get.ready := io.task.ready && !io.a.valid']}
    for name, fragments in requirements.items():
        for fragment in fragments:
            assert fragment in (cache / name).read_text(), (name, fragment)
    declarations = dict(re.findall(r'uint(?:8|16|32|64)_t (\S+); // width = (\d+),', header))

    def field(name, width=1):
        assert declarations.get(PREFIX + name) == str(width), name
        return 'model->' + PREFIX + name

    blocks, bindings = [], []
    for bank in range(4):
        parent = 'prefetcher__DOT__'
        queue = parent + f'pftQueue_{bank}__DOT__'
        pipe = parent + f'pipe_{bank}__DOT__stages_0__DOT__'
        sink = f'slices_{bank}__DOT__'
        head, tail = [field(queue + n, 5) for n in ['headPtr', 'tailPtr']]
        full_flag = field(queue + 'maybe_full')
        enqueue = field(parent + f'_pftQueue_{bank}_io_enq_valid_T_1')
        pipe_ready = field(pipe + 'io__DOT__enq__DOT__ready')
        pipe_valid = field(pipe + '_io_deq_valid_T')
        port_ready = field(sink + 'sinkA__DOT___io_prefetchReq_ready_T_1')
        reqbuf_ready = field(sink + 'reqBuf__DOT___io_in_ready_T_10')
        lhs = PREFIX + sink + 'sinkA__DOT___io_prefetchReq_ready_T'
        result = subprocess.run(['rg', '--no-heading', '--no-filename', '--glob', '*.cpp',
            r'^\s*' + re.escape(lhs) + r'\s*=', str(Path(root) / 'build/gsim-compile/model')],
            text=True, capture_output=True, check=True)
        match = re.fullmatch(r'\s*' + re.escape(lhs) + r' = \((\S+) == 0x0\);\s*', result.stdout)
        assert match and declarations.get(match[1]) == '1', result.stdout
        a_valid = 'model->' + match[1]

        def payload(base, index=None, following=False):
            def get(name, width):
                if index is None:
                    return field(base + name + ('$NEXT' if following else ''), width)
                decl = base + name + ('$NEXT' if following else '') + '[32]'
                return field(decl, width)[:-4] + '[' + index + ']'
            return [f'((uint64_t({get("tag",32)})<<16) | (uint64_t({get("set",10)})<<6))',
                    get('source', 8), get('pfSource', 5), get('needT', 1)]
        incoming = payload(parent + f'_pftQueue_{bank}_io_enq_bits_T_16__DOT__')
        current_head = payload(queue + 'queue__DOT__', head)
        current_pipe = payload(pipe + 'ram__DOT__')
        next_pipe = payload(pipe + 'ram__DOT__', following=True)
        next_write = payload(queue + 'queue__DOT__', tail, following=True)
        next_state = [field(queue + n + '$NEXT', width) for n, width in
                      [('headPtr',5), ('tailPtr',5), ('maybe_full',1)]]
        next_state.append(field(pipe + 'maybe_full$NEXT'))
        values = [head, tail, full_flag, enqueue, pipe_ready,
                  field(queue + 'flowHappens'), field(queue + 'overwriteHappens'),
                  pipe_valid, port_ready, reqbuf_ready, a_valid,
                  *incoming, *current_head, *current_pipe,
                  *[f'q_counts[{bank}][{i}]' for i in range(4)]]
        assert len(values) == len(FIELDS)
        blocks.append(f'''  {{
    uint64_t row[{len(FIELDS)}]={{{','.join(values)}}};
    const bool empty=(row[0]==row[1] && !row[2]);
    const bool full=(row[0]==row[1] && row[2]);
    const bool deq=(!empty || row[3]) && row[4];
    const bool port_fire=row[7] && row[8];
    assert(row[5]==(empty && row[3] && row[4]));
    assert(row[6]==(full && row[3] && !deq));
    assert(row[7]=={field(pipe+'maybe_full')});
    assert(row[8]==(row[9] && !row[10]));
    assert(row[4]==(row[8] || !row[7]));
    if(q_seen) {{
      const uint64_t current[4]={{{head},{tail},{full_flag},{pipe_valid}}};
      for(unsigned j=0;j<4;++j) assert(current[j]==q_next[{bank}][j]);
    }}
    const uint64_t next[4]={{{','.join(next_state)}}};
    for(unsigned j=0;j<4;++j) q_next[{bank}][j]=next[j];
    if(row[3] && !row[5]) {{
      const uint64_t written[4]={{{','.join(next_write)}}};
      for(unsigned j=0;j<4;++j) assert(written[j]==row[11+j]);
    }}
    if(deq) {{
      const uint64_t pipe_next[4]={{{','.join(next_pipe)}}};
      for(unsigned j=0;j<4;++j) assert(pipe_next[j]==row[(row[5]?11:15)+j]);
    }}
    q_counts[{bank}][0]+=bool(row[3]);q_counts[{bank}][1]+=deq;
    q_counts[{bank}][2]+=bool(row[6]);q_counts[{bank}][3]+=port_fire;
    for(unsigned j=0;j<4;++j) row[23+j]=q_counts[{bank}][j];
    if(!q_seen || row[3] || !empty || row[7]) q_write(1,{bank},row);
    if(context.clean || context.dump) q_footer(0,{bank});
  }}''')
        bindings.append(dict(bank=bank, values=values, a_valid_equation=result.stdout.strip()))
    helper = f'''
static FILE *q_output=nullptr;
static uint64_t q_counts[4][4]{{}},q_next[4][4]{{}},q_last_cycle=0;
static bool q_seen=false;
static void q_finish();
static void q_write(unsigned kind,unsigned bank,const uint64_t *row) {{
  if(!q_output) {{
    q_output=fopen("prefetch-queue.csv","w");
    if(!q_output) {{perror("prefetch-queue.csv");abort();}}
    fprintf(q_output,"core_cycle,clean,native_dump,kind,bank,{','.join(FIELDS)}\\n");
    std::atexit(q_finish);
  }}
  fprintf(q_output,"%lu,%u,%u,%u,%u",q_last_cycle,
    kind==2?0:context.clean,kind==2?0:context.dump,kind,bank);
  for(unsigned j=0;j<{len(FIELDS)};++j) fprintf(q_output,",%lu",row[j]);
  fprintf(q_output,"\\n");
}}
static void q_footer(unsigned kind,unsigned bank) {{
  uint64_t row[{len(FIELDS)}]{{}};
  for(unsigned j=0;j<4;++j) row[23+j]=q_counts[bank][j];
  q_write(kind,bank,row);
}}
static void q_finish() {{
  if(!q_output) return;
  for(unsigned bank=0;bank<4;++bank) q_footer(2,bank);
  fclose(q_output);q_output=nullptr;
}}
static void prefetch_queue_observe(SSimTop *model) {{
  if(!context.seen) return;
  q_last_cycle=context.core;
{chr(10).join(blocks)}
  q_seen=true;
}}
'''
    assert cpp.count('static void pbop_observe(SSimTop *model)') == 1
    assert cpp.count('  pbop_observe(model);') == 1
    cpp = cpp.replace('static void pbop_observe(SSimTop *model)', helper + '\nstatic void pbop_observe(SSimTop *model)')
    cpp = cpp.replace('  pbop_observe(model);', '  pbop_observe(model);\n  prefetch_queue_observe(model);')
    return cpp, dict(fields=FIELDS, banks=4, entries_per_bank=32, bindings=bindings,
        source_sha256={name:digest(cache/name) for name in requirements}, new_RTL_state_bits=0,
        scope='Actual FIFO enqueue/head overwrite/dequeue, one-stage pipeline and prefetch port. '
              'Observer asserts pointer commitments and actual NEXT payloads; not CPU loss.')


def build(previous_proof, output):
    previous_proof, output = Path(previous_proof).resolve(), Path(output).resolve()
    previous = json.loads(previous_proof.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and 'PBOP_train_issue_binding' in previous
    assert not output.exists()
    parent, root = previous_proof.parent, Path(previous['root'])
    assert digest(parent/'probe.cpp') == previous['probe_cpp_sha256']
    for name, expected in previous['original_sha256'].items():
        assert digest(name) == expected, name
    cpp, binding = add_capture(root, (root/'build/gsim-compile/model/SimTop.h').read_text(), (parent/'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent),str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output/f'build-{i}.stdout').open('wb') as out, (output/f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(name) == expected, name
    result = dict(previous, binary=str(output/'emu'), binary_sha256=digest(output/'emu'), commands=commands,
        probe_cpp_sha256=digest(output/'probe.cpp'), generator_sha256=digest(Path(__file__)),
        base_PBOP_build_proof_sha256=digest(previous_proof), prefetch_queue_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--pbop-build-proof',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    proof=build(args.pbop_build_proof,args.output)
    print(json.dumps({name:proof[name] for name in ['passed','source','binary','binary_sha256']}))


if __name__=='__main__':
    main()
