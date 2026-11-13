"""Observe actual SMS fire and the existing two-stage L2 prefetch pipeline without new RTL."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest

PREFIX='cpu__DOT__l_soc__DOT__core_with_l2__DOT__core__DOT__memBlock__DOT__inner__DOT__prefetcher__DOT__'


def add_capture(root,header,cpp):
    declarations=dict(re.findall(r'uint(?:8|16|32|64)_t ([^;]+); // width = (\d+),',header))
    def field(name,width):
        assert declarations.get(name)==str(width),name
        return 'model->'+name
    wrapper=root/'src/main/scala/xiangshan/mem/prefetch/PrefetcherWrapper.scala'
    pipeline=root/'utility/src/main/scala/utility/Pipeline.scala'
    assert 'l2_pf_req.ready := true.B' in wrapper.read_text()
    assert 'new Queue[T](gen, 1, pipe = pipe, flow = false)' in pipeline.read_text()
    fire=[field(PREFIX+name,1)for name in ['l2_pf_arb__DOT__grant_2','smsOpt__DOT___io_l2_req_valid_T']]
    current,next_values=[],[]
    for stage in range(2):
        base=PREFIX+f'l2_pf_req_pf_to_l2cache_reg__DOT__stages_{stage}__DOT__'
        regs=[('maybe_full',1),('ram__DOT__addr',48),('ram__DOT__source',5)]
        current.append([field(base+name,width)for name,width in regs])
        next_values.append([field(base+name+'$NEXT',width)for name,width in regs])
    helper=r'''
#include <cstring>
static uint64_t sms_next[2][3]{},sms_prior_stage0[3]{},sms_prior_input[3]{};
static bool sms_seen=false,sms_input_seen=false;
static uint64_t sms_last_core=0;
static FILE *sms_output=nullptr;
static void sms_write(unsigned kind,uint64_t address) {
  if(!sms_output) {
    sms_output=fopen("sms-l2-requests.csv","w");
    if(!sms_output) { perror("sms-l2-requests.csv");abort(); }
    fprintf(sms_output,"core_cycle,clean,native_dump,kind,address\n");
  }
  fprintf(sms_output,"%lu,%u,%u,%u,%lu\n",context.core,context.clean,context.dump,kind,address);
}
static void sms_snapshot(SSimTop *model) {
SNAPSHOTS
}
static void sms_observe(SSimTop *model) {
  const uint64_t now[2][3]={SMS_CURRENT_VALUES};
  const uint64_t next[2][3]={SMS_NEXT_VALUES};
  const bool fire=FIRE;
  if(!context.seen) return;
  for(unsigned stage=0;stage<2;++stage) {
    assert(sms_next[stage][0]==now[stage][0]);
    if(now[stage][0]) for(unsigned j=1;j<3;++j) assert(sms_next[stage][j]==now[stage][j]);
  }
  if(sms_seen) {
    assert(context.core==sms_last_core+1);
    assert(now[1][0]==sms_prior_stage0[0]);
    if(now[1][0]) for(unsigned j=1;j<3;++j) assert(now[1][j]==sms_prior_stage0[j]);
    if(sms_input_seen) for(unsigned j=0;j<3;++j) assert(now[0][j]==sms_prior_input[j]);
  } else { assert(!now[0][0]&&!now[1][0]); }
  if(context.clean||context.dump) sms_write(0,0);
  if(fire) {
    assert(next[0][0]&&next[0][2]==10);
    sms_write(1,next[0][1]);
  }
  if(now[1][0]&&now[1][2]==10) sms_write(2,now[1][1]);
  std::memcpy(sms_prior_stage0,now[0],sizeof(sms_prior_stage0));
  std::memcpy(sms_prior_input,next[0],sizeof(sms_prior_input));
  sms_input_seen=fire;sms_seen=true;sms_last_core=context.core;
}
'''
    helper=helper.replace('SNAPSHOTS','\n'.join(f'  sms_next[{i}][{j}]={v};'for i,values in enumerate(next_values)for j,v in enumerate(values)))
    for label,values in [('SMS_CURRENT_VALUES',current),('SMS_NEXT_VALUES',next_values)]:
        helper=helper.replace(label,','.join('{'+','.join('uint64_t('+v+')'for v in stage)+'}'for stage in values))
    helper=helper.replace('FIRE',' && '.join(fire))
    for old,new in [('static Context context{};','static Context context{};\n'+helper),
                    ('  in_step = true; __real_','  sms_snapshot(model);\n  in_step = true; __real_'),
                    ('  assert(context.clean == ','  sms_observe(model);\n  assert(context.clean == ')]:
        assert cpp.count(old)==1,old
        cpp=cpp.replace(old,new)
    # The actual generated native equation defines SMS fire. Preserve this source proof.
    targets=[PREFIX+'_T_19 =', 'logEndpoint__DOT___SMS_fire_l2Next_T =']
    cmd=['rg','-l','-F']
    for target in targets:cmd+=['-e',target]
    cmd+=[str(root/'build/gsim-compile/model'),'--glob','*.cpp']
    paths=[Path(p)for p in subprocess.check_output(cmd,text=True).splitlines()]
    generated='\n'.join(p.read_text()for p in paths)
    assert PREFIX+'_T_19 = ('+fire[0].removeprefix('model->')+' & '+fire[1].removeprefix('model->')+');'in generated
    assert 'SMS_fire_l2Counter + ((uint64_t)'+PREFIX+'_T_19)'in generated
    return cpp,dict(stages=2,source_id=10,input_fire=fire,current=current,next=next_values,
                    actual_pipeline_commit_and_forward_asserted=True,source_sha256={str(p):digest(p)for p in [wrapper,pipeline,*paths]},
                    new_RTL_state_bits=0,scope='Native SMS_fire_l2 predicate and registered flow=false/pipe=true pipeline. Final output is always ready. Does not observe SMS generation before its output or prove L2 admission/cache residency.')


def build(lineage_build_proof,output):
    proof_path,output=Path(lineage_build_proof).resolve(),Path(output).resolve();previous=json.loads(proof_path.read_text())
    assert previous['passed']and previous['original_files_unchanged']and previous['original_objects']==577
    assert previous['memory_lineage_binding']['next_register_commit_asserted']and previous['memory_lineage_binding']['new_RTL_state_bits']==0
    parent,root=proof_path.parent,Path(previous['root']);assert digest(parent/'probe.cpp')==previous['probe_cpp_sha256']and not output.exists()
    for name,h in previous['original_sha256'].items():assert digest(Path(name))==h,name
    cpp,binding=add_capture(root,(root/'build/gsim-compile/model/SimTop.h').read_text(),(parent/'probe.cpp').read_text())
    output.mkdir(parents=True);(output/'probe.cpp').write_text(cpp);(output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands=[[arg.replace(str(parent),str(output))for arg in command]for command in previous['commands']]
    for i,command in enumerate(commands):
        with(output/f'build-{i}.stdout').open('wb')as stdout,(output/f'build-{i}.stderr').open('wb')as stderr:subprocess.run(command,stdout=stdout,stderr=stderr,check=True)
    for name,h in previous['original_sha256'].items():assert digest(Path(name))==h,name
    result=dict(previous,binary=str(output/'emu'),binary_sha256=digest(output/'emu'),commands=commands,probe_cpp_sha256=digest(output/'probe.cpp'),generator_sha256=digest(Path(__file__)),base_lineage_build_proof_sha256=digest(proof_path),SMS_request_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result,indent=2)+'\n');return result


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--lineage-build-proof',type=Path,required=True);p.add_argument('--output',type=Path,required=True);args=p.parse_args();result=build(args.lineage_build_proof,args.output);print(json.dumps({k:result[k]for k in ['passed','binary','binary_sha256','original_objects']}))


if __name__=='__main__':main()
