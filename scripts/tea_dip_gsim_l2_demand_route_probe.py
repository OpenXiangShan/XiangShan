"""Relink a memory observer with L2 request-buffer ownership and actual demand returns."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_l2_return_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

FIELDS = ['valid', 'line', 'source_id', 'opcode', 'param', 'req_source', 'ready',
          'wait_mp', 'wait_ms', 'merge_mask', 'alloc_mask', 'flow', 'duplicate',
          'mshr_task', 'dir_hit', 'need_mshr']


def add_capture(root, header, cpp):
    cache = root / 'XSCache/src/main/scala/coupledL2'
    requirements = {
        'RequestBuffer.scala': ['class RequestBuffer(flow: Boolean = true, entries: Int = 4)', 'io.aMergeTask.valid := io.in.valid && mergeA',
                               'val alloc = !full && io.in.valid && !prefetchBlocked(in) && !doFlow && !dup && !mergeA',
                               'io.out.valid := chosenQValid && !cancel || io.in.valid && canFlow'],
        'Slice.scala': ['reqBuf.io.in <> sinkA.io.task', 'reqArb.io.sinkA <> reqBuf.io.out'],
        'MainPipe.scala': ['val req_s3          = task_s3.bits', 'val dirResult_s3    = io.dirResp_s3'],
    }
    for name, fragments in requirements.items():
        text = (cache / name).read_text()
        for fragment in fragments:
            assert fragment in text, (name, fragment)

    def field(name, width):
        assert re.search(r'uint(?:8|16|32|64)_t ' + re.escape(name)
                         + r'; // width = ' + str(width) + ',', header), name
        return 'model->' + name

    captures, snapshots, bindings = [], [], []
    for bank in range(4):
        prefix = PREFIX + f'slices_{bank}__DOT__'
        def f(name, width=1):
            return field(prefix + name, width)
        def line(tag, index):
            return f'((uint64_t({tag})<<18) | (uint64_t({index})<<8) | {bank*64}u)'
        def task(base):
            return [line(f(base+'tag',30), f(base+'set',10)),
                    *[f(base+name,width) for name,width in
                      [('sourceId',8),('opcode',4),('param',3),('reqSource',5)]]]
        q = 'reqBuf__DOT__'
        in_valid = f('sinkA__DOT___io_task_valid_T_2')
        in_ready = f(q+'_io_in_ready_T_10')
        incoming = task('sinkA__DOT___io_task_bits_T_1__DOT__')
        alloc, insert, flow, duplicate, merge = [f(q+n,w) for n,w in
            [('alloc',1),('insertOH',4),('doFlow',1),('dup',1),('mergeAMask',16)]]
        chosen = f(q+'chosenQ__DOT___io_deq_valid_T')
        cancel = f(q+'cancel')
        can_flow = f(q+'canFlow')
        out_ready = f('reqArb__DOT___io_sinkA_ready_T_7')
        out_valid = f'(({chosen} && !{cancel}) || ({in_valid} && {can_flow}))'
        selected = task(q+'chosenQ__DOT__ram__DOT__bits__DOT__task__DOT__')
        out_task = [f'({chosen} ? {a} : {b})' for a,b in zip(selected,incoming)]
        inp = [in_valid,*incoming,in_ready,'0','0',merge,f'uint64_t({alloc} ? {insert} : 0)',flow,duplicate,'0','0','0']
        outgoing = [out_valid,*out_task,out_ready,*(['0']*9)]
        assert len(inp)==len(outgoing)==len(FIELDS)
        blocks = [f'''  if ({in_valid}) {{
    const uint64_t values[16] = {{{','.join(inp)}}};
    assert(!values[6] || (bool(values[9]) + bool(values[10]) + bool(values[11]) + bool(values[12]) >= 1));
    route_write(1,{bank},4,values);
  }}''',f'''  if ({out_valid}) {{
    const uint64_t values[16] = {{{','.join(outgoing)}}}; route_write(3,{bank},4,values);
  }}''']
        entry_bindings=[]
        for slot in range(4):
            base=q+'buffer__DOT__'
            specs=[('valid',1),('task__DOT__tag',30),('task__DOT__set',10),
                   ('task__DOT__sourceId',8),('task__DOT__opcode',4),('task__DOT__param',3),
                   ('task__DOT__reqSource',5),('rdy',1),('waitMP',4),('waitMS',16)]
            now=[f(base+name+'_'+str(slot),width)for name,width in specs]
            nxt=[f(base+name+'$NEXT_'+str(slot),width)for name,width in specs]
            def values(regs):
                return [regs[0],line(regs[1],regs[2]),*regs[3:],*(['0']*7)]
            current,following=values(now),values(nxt)
            assert len(current)==len(following)==16
            index=bank*4+slot
            snapshots.append(f'''  {{ const uint64_t values[16]={{{','.join(following)}}};
    for (unsigned j=0;j<16;++j) route_next[{index}][j]=values[0] ? values[j] : 0;
  }}''')
            blocks.append(f'''  {{
    uint64_t values[16]={{{','.join(current)}}};
    if (!values[0]) std::memset(values,0,sizeof(values));
    if (route_seen) for(unsigned j=0;j<16;++j) assert(values[j]==route_next[{index}][j]);
    if (!route_seen || context.clean || context.dump || std::memcmp(values,route_previous[{index}],sizeof(values)))
      route_write(2,{bank},{slot},values);
    std::memcpy(route_previous[{index}],values,sizeof(values));
  }}''')
            entry_bindings.append(dict(slot=slot,current=now,next=nxt))
        mp='mainPipe__DOT__task_s3__DOT__'
        mp_valid=f(mp+'valid')
        mp_values=[mp_valid,*task(mp+'bits__DOT__'),'0','0','0','0','0','0','0',
                   f(mp+'bits__DOT__mshrTask'),f('mainPipe__DOT__nestable_dirResult_s3__DOT__hit'),
                   f('mainPipe__DOT__need_mshr_s3')]
        assert len(mp_values)==16
        # The observed S3 population is channel A only, including MSHR refill tasks.
        from_a=f(mp+'bits__DOT__channel$0_0')
        blocks.append(f'''  if ({mp_valid} && {from_a}) {{
    const uint64_t values[16]={{{','.join(mp_values)}}}; route_write(4,{bank},4,values);
  }}''')
        gb='grantBuf__DOT__'
        dvalid=f(gb+'_io_d_valid_T')
        dready=field(PREFIX+f'_slice_io_in_d_ready_T_{2*bank+1}',1)
        held=f(gb+'grantBufValid')
        def dfield(name,width):
            a=f(gb+'grantBuf__DOT__task__DOT__'+name,width)
            b=f(gb+'grantQueue__DOT__io_deq_bits_MPORT__DOT__task__DOT__'+name,width)
            return f'({held} ? {a} : {b})'
        dv=[dvalid,'0',dfield('sourceId',8),f(gb+'_io_d_bits_T_1__DOT__opcode',4),
            '0','0',dready,*(['0']*9)]
        assert len(dv)==16
        blocks.append(f'''  if ({dvalid}) {{
    const uint64_t values[16]={{{','.join(dv)}}}; route_write(5,{bank},4,values);
  }}''')
        captures.extend(blocks)
        bindings.append(dict(bank=bank,input=inp,output=outgoing,entries=entry_bindings,
                             mainpipe=mp_values,D=dv))
    helper=f'''
static uint64_t route_next[16][16]{{}},route_previous[16][16]{{}};
static bool route_seen=false;
static FILE *route_output=nullptr;
static void route_write(unsigned kind,unsigned bank,unsigned slot,const uint64_t *values) {{
  if(!route_output) {{
    route_output=fopen("load-l2-demand-route.csv","w");
    if(!route_output) {{ perror("load-l2-demand-route.csv");abort(); }}
    fprintf(route_output,"core_cycle,clean,native_dump,kind,bank,entry,{','.join(FIELDS)}\\n");
  }}
  fprintf(route_output,"%lu,%u,%u,%u,%u,%u",context.core,context.clean,context.dump,kind,bank,slot);
  for(unsigned j=0;j<16;++j) fprintf(route_output,",%lu",values[j]);
  fprintf(route_output,"\\n");
}}
static void route_snapshot(SSimTop *model) {{
{chr(10).join(snapshots)}
}}
static void route_observe(SSimTop *model) {{
  if(!context.seen) return;
  if(context.clean || context.dump) {{const uint64_t zero[16]{{}};route_write(0,4,4,zero);}}
{chr(10).join(captures)}
  route_seen=true;
}}
'''
    for old,new in [('static void l2_snapshot(SSimTop *model)',helper+'\nstatic void l2_snapshot(SSimTop *model)'),
                    ('  l2_snapshot(model);','  route_snapshot(model);\n  l2_snapshot(model);'),
                    ('  l2_observe(model);','  l2_observe(model);\n  route_observe(model);')]:
        assert cpp.count(old)==1,old
        cpp=cpp.replace(old,new)
    return cpp,dict(fields=FIELDS,kinds={'boundary':0,'input':1,'entry_state':2,'output':3,'mainpipe_A_S3':4,'D':5},
                    bindings=bindings,next_register_commit_asserted=True,
                    source_sha256={str(cache/n):digest(cache/n)for n in requirements},
                    scope='Current core-edge combination and registers; entry next commitments asserted. D has source, no line. Source generations and native/old-output calibration remain required.')


def build(memory_build_proof,output):
    proof_path,output=Path(memory_build_proof).resolve(),Path(output).resolve()
    previous=json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and 'memory_lineage_binding'in previous
    assert previous['original_objects']==sum(Path(n).suffix=='.o'for n in previous['original_sha256'])
    assert not output.exists()
    parent,root=proof_path.parent,Path(previous['root'])
    assert digest(parent/'probe.cpp')==previous['probe_cpp_sha256']
    for name,expected in previous['original_sha256'].items():assert digest(Path(name))==expected,name
    cpp,binding=add_capture(root,(root/'build/gsim-compile/model/SimTop.h').read_text(),(parent/'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands=[[a.replace(str(parent),str(output))for a in c]for c in previous['commands']]
    for i,command in enumerate(commands):
        with(output/f'build-{i}.stdout').open('wb')as out,(output/f'build-{i}.stderr').open('wb')as err:
            subprocess.run(command,stdout=out,stderr=err,check=True)
    for name,expected in previous['original_sha256'].items():assert digest(Path(name))==expected,name
    result=dict(previous,binary=str(output/'emu'),binary_sha256=digest(output/'emu'),commands=commands,
                probe_cpp_sha256=digest(output/'probe.cpp'),generator_sha256=digest(Path(__file__)),
                base_memory_build_proof_sha256=digest(proof_path),L2_demand_route_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--memory-build-proof',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    value=build(args.memory_build_proof,args.output)
    print(json.dumps({n:value[n]for n in ['passed','binary','binary_sha256','original_objects']}))


if __name__=='__main__':main()
