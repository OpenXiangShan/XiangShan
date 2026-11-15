"""Relink a closed route observer with the full L2 MSHR release predicate."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_l2_return_probe import PREFIX
from tea_dip_gsim_lsu_s1_probe import digest

SCHEDULE = ['s_refill', 's_probeack', 's_release', 's_rcompack', 's_wcompack',
            's_cbwrdata', 's_reissue', 's_dct', 's_cmoresp', 's_cmometaw']
WAIT = ['w_rprobeacklast', 'w_pprobeacklast', 'w_grantlast', 'w_grant',
        'w_releaseack', 'w_replResp']
FIELDS = ['valid', *SCHEDULE, *WAIT, 'will_free']


def add_capture(root, header, cpp):
    source = root/'XSCache/src/main/scala/coupledL2/MSHR.scala'
    text = source.read_text()
    for fragment in ['val will_free = no_schedule && no_wait',
                     'when (will_free && req_valid)', 'req_valid := false.B']:
        assert fragment in text, fragment
    for name, flags in [('no_schedule', SCHEDULE), ('no_wait', WAIT)]:
        expression = re.search(r'val '+name+r' = (.*?)(?=\n  val )', text, re.S)[1]
        assert re.findall(r'state\.(\w+)', expression) == flags, name
    captures, snapshots, bindings = [], [], []
    for bank in range(4):
        for slot in range(16):
            prefix = PREFIX+f'slices_{bank}__DOT__mshrCtl__DOT__mshrs_{slot}__DOT__'
            def field(name):
                full = prefix+name
                assert re.search(r'uint8_t '+re.escape(full)+r'; // width = 1,', header), full
                return 'model->'+full
            regs = ['req_valid', *['state__DOT__'+n for n in SCHEDULE+WAIT]]
            current = [field(n) for n in regs]+[field('will_free')]
            following = [field(n+'$NEXT') for n in regs]
            index = bank*16+slot
            captures.append(f'''  {{
    uint64_t values[18]={{{','.join(current)}}};
    if(release_seen) for(unsigned j=0;j<17;++j) assert(values[j]==release_next[{index}][j]);
    if(values[0]) {{
      bool done=true; for(unsigned j=1;j<17;++j) done=done && values[j];
      assert(done==bool(values[17]));
    }} else std::memset(values,0,sizeof(values));
    if(!release_seen || context.clean || context.dump || std::memcmp(values,release_previous[{index}],sizeof(values)))
      release_write(1,{bank},{slot},values);
    std::memcpy(release_previous[{index}],values,sizeof(values));
  }}''')
            snapshots.append(f'''  {{ const uint64_t values[17]={{{','.join(following)}}};
    for(unsigned j=0;j<17;++j) release_next[{index}][j]=values[j];
  }}''')
            bindings.append(dict(bank=bank,slot=slot,current=current,next=following))
    helper=f'''
static uint64_t release_next[64][17]{{}},release_previous[64][18]{{}};
static bool release_seen=false;
static FILE *release_output=nullptr;
static void release_write(unsigned kind,unsigned bank,unsigned slot,const uint64_t *values) {{
  if(!release_output) {{
    release_output=fopen("load-l2-release.csv","w");
    if(!release_output) {{perror("load-l2-release.csv");abort();}}
    fprintf(release_output,"core_cycle,clean,native_dump,kind,bank,mshr,{','.join(FIELDS)}\\n");
  }}
  fprintf(release_output,"%lu,%u,%u,%u,%u,%u",context.core,context.clean,context.dump,kind,bank,slot);
  for(unsigned j=0;j<18;++j) fprintf(release_output,",%lu",values[j]);
  fprintf(release_output,"\\n");
}}
static void release_snapshot(SSimTop *model) {{
{chr(10).join(snapshots)}
}}
static void release_observe(SSimTop *model) {{
  if(!context.seen) return;
  if(context.clean || context.dump) {{const uint64_t zero[18]{{}};release_write(0,4,16,zero);}}
{chr(10).join(captures)}
  release_seen=true;
}}
'''
    for old,new in [('static void route_snapshot(SSimTop *model)',helper+'\nstatic void route_snapshot(SSimTop *model)'),
                    ('  route_snapshot(model);','  release_snapshot(model);\n  route_snapshot(model);'),
                    ('  route_observe(model);','  route_observe(model);\n  release_observe(model);')]:
        assert cpp.count(old)==1, old
        cpp=cpp.replace(old,new)
    return cpp,dict(fields=FIELDS,bindings=bindings,source_sha256={str(source):digest(source)},
                    next_commit_and_release_conjunction_asserted=True,
                    scope='Actual full no_schedule/no_wait conjunction, valid generations joined to the original L2 owner stream. Residence is not CPU lost cycles.')


def build(route_build_proof, output):
    proof_path,output=Path(route_build_proof).resolve(),Path(output).resolve()
    previous=json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and 'L2_demand_route_binding'in previous
    assert previous['original_objects']==sum(Path(n).suffix=='.o'for n in previous['original_sha256'])
    parent,root=proof_path.parent,Path(previous['root'])
    assert not output.exists() and digest(parent/'probe.cpp')==previous['probe_cpp_sha256']
    for name,expected in previous['original_sha256'].items():assert digest(name)==expected,name
    cpp,binding=add_capture(root,(root/'build/gsim-compile/model/SimTop.h').read_text(),(parent/'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands=[[a.replace(str(parent),str(output))for a in c]for c in previous['commands']]
    for i,command in enumerate(commands):
        with(output/f'build-{i}.stdout').open('wb')as out,(output/f'build-{i}.stderr').open('wb')as err:
            subprocess.run(command,stdout=out,stderr=err,check=True)
    for name,expected in previous['original_sha256'].items():assert digest(name)==expected,name
    result=dict(previous,binary=str(output/'emu'),binary_sha256=digest(output/'emu'),commands=commands,
                probe_cpp_sha256=digest(output/'probe.cpp'),generator_sha256=digest(Path(__file__)),
                base_route_build_proof_sha256=digest(proof_path),L2_release_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--route-build-proof',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args();value=build(args.route_build_proof,args.output)
    print(json.dumps({n:value[n]for n in ['passed','binary','binary_sha256','original_objects']}))


if __name__=='__main__':main()
