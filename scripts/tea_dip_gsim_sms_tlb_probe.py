"""Observe SMS filter TLB eligibility, arbitration and pipeline ownership without RTL state."""
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest
from tea_dip_gsim_sms_internal_probe import SMS

COLUMNS = ['base_mask', 'eligible_mask', 'chosen_mask', 'stage1_mask', 'stage2_mask', 'stage3_mask',
           's1_valid', 's1_hit', 'replace_mask', 'request_fire', 'request_va', 'response_fire',
           'response_miss', 'response_pa', 'registered_response_fire', 'registered_response_miss',
           'registered_response_pa', 'update_valid', 'drop', 'selected_tag', 'selected_region']


def add_capture(root, header, cpp):
    root = Path(root)
    prefix = SMS + 'pf_filter__DOT__'
    declared = dict(re.findall(r'uint(?:8|16|32|64)_t ([^;]+); // width = (\d+),', header))
    bindings = {}

    def full(name, width):
        assert declared.get(name) == str(width), name
        bindings[name] = width
        return 'model->' + name

    def field(name, width):
        return full(prefix + name, width)

    source = root / 'src/main/scala/xiangshan/mem/prefetch/SMSPrefetcher.scala'
    compact = re.sub(r'\s+', '', source.read_text())
    line, = re.findall(r'tlb_req_arb\.io\.in\(i\)\.valid:=([^\n]*?)(?=tlb_req_arb\.io\.in\(i\)\.bits)', compact)
    assert line in ['v&&not_tlbing_vec(i)&&!ent.paddr_valid&&!is_evicted',
                    'v&&not_tlbing_vec(i)&&!ent.paddr_valid&&(!is_evicted||s1_hit)'], line
    eviction, = re.findall(r'valis_evicted=(.*?)(?=tlb_req_arb\.io\.in\(i\)\.valid)', compact)
    assert eviction in ['s1_valid&&s1_replace_vec(i)', 's1_valid&&!s1_hit&&s1_replace_vec(i)'], eviction
    allow_hit = '||s1_hit' in line or eviction == 's1_valid&&!s1_hit&&s1_replace_vec(i)'
    for equation in ['tlb_req_arb.io.out.ready:=true.B',
                     'vals1_tlb_fire_vec_r=GatedValidRegNext(s0_tlb_fire_vec)',
                     'vals2_tlb_fire_vec_r=GatedValidRegNext(s1_tlb_fire_vec_r)',
                     'vals3_tlb_fire_vec_r=GatedValidRegNext(s2_tlb_fire_vec_r)',
                     'vals3_tlb_resp_fire=RegNext(io.tlb_req.resp.fire)',
                     'vals3_update_valid=s3_tlb_resp_fire&&!s3_tlb_resp.miss']:
        assert equation in compact, equation
    paths = [Path(p) for p in subprocess.check_output(
        ['rg', '-l', '-F', '-e', 'sms_pf_filter_tlb_reqCounter + ', '-e', 'sms_pf_filter_tlb_resp_missCounter + ',
         '-e', 'sms_pf_filter_tlb_resp_dropCounter + ', str(root/'build/gsim-compile/model'), '--glob', '*.cpp'], text=True).splitlines()]
    generated = '\n'.join(p.read_text() for p in paths)
    events = {}
    for counter in ['tlb_req', 'tlb_resp_miss', 'tlb_resp_drop']:
        name, = set(re.findall(r'sms_pf_filter_' + counter + r'Counter \+ \(\(uint64_t\)([\w$]+)\)', generated))
        events[counter] = full(name, 1)
    stages = [' | '.join(f'(uint64_t({field(f"s{s}_tlb_fire_vec_r_r_{i}", 1)}) << {i})' for i in range(16))
              for s in range(1,4)]
    entries = ','.join('{' + ','.join(field(name, width) for name,width in
                        [(f'valids_{i}',1), (f'entries_{i}__DOT__paddr_valid',1),
                         (f'entries_{i}__DOT__region_tag',11), (f'entries_{i}__DOT__region_addr',40)]) + '}' for i in range(16))
    values = dict(STAGES=','.join('('+s+')' for s in stages), ENTRIES=entries,
                  ELIGIBLE=field('tlb_req_arb__DOT__lowArb__DOT__valids',8)+' | (uint64_t('+field('tlb_req_arb__DOT__highArb__DOT__valids',8)+') << 8)',
                  SELECT_LOW=field('tlb_req_arb__DOT___finalSelLow_T_8',1),
                  CHOSEN_LOW=field('tlb_req_arb__DOT__lowArb__DOT___chosenOH_T',8),
                  CHOSEN_HIGH=field('tlb_req_arb__DOT__highArb__DOT___chosenOH_T',8),
                  S1_VALID=field('s1_valid_r_last_REG',1), S1_HIT=field('s1_hit_r',1),
                  REPLACE=field('s1_replace_vec_r',16), REQUEST=events['tlb_req'],
                  REQUEST_VA=field('s1_tlb_req_bits__DOT__vaddr',50),
                  RESPONSE=field('s3_tlb_resp_fire$NEXT',1), RESPONSE_MISS=field('s3_tlb_resp__DOT__miss$NEXT',1),
                  RESPONSE_PA=field('s3_tlb_resp__DOT__paddr$NEXT_0',48),
                  REGISTERED_RESPONSE=field('s3_tlb_resp_fire',1), REGISTERED_MISS=field('s3_tlb_resp__DOT__miss',1),
                  REGISTERED_PA=field('s3_tlb_resp__DOT__paddr_0',48), UPDATE=field('s3_update_valid',1),
                  MISS_EVENT=events['tlb_resp_miss'], DROP=events['tlb_resp_drop'], ALLOW_HIT=str(int(allow_hit)),
                  HEADER='core_cycle,clean,native_dump,kind,'+','.join(COLUMNS))
    helper = r'''
static FILE *st_file=nullptr;
static bool st_seen=false;
static uint64_t st_previous[4]{},st_previous_region=0,st_previous_miss=0,st_previous_pa=0;
static void sms_tlb_observe(SSimTop *model) {
  if(!context.seen) return;
  if(!st_file) {st_file=fopen("sms-tlb.csv","w");assert(st_file);fprintf(st_file,"HEADER\n");}
  const uint64_t stage[3]={STAGES},entries[16][4]={ENTRIES};
  const uint64_t active=S1_VALID,hit=S1_HIT,replace=REPLACE,evicted=active?replace:0;
  uint64_t base=0;
  for(unsigned i=0;i<16;++i) if(entries[i][0]&&!entries[i][1]&&!((stage[0]|stage[1]|stage[2])&(1u<<i))) base|=1u<<i;
  const uint64_t eligible=ELIGIBLE;
  assert(eligible==(base&(~evicted|uint64_t(ALLOW_HIT&&hit)*65535)&65535));
  const uint64_t chosen=eligible?(SELECT_LOW?uint64_t(CHOSEN_LOW):uint64_t(CHOSEN_HIGH)<<8):0;
  assert(!chosen||(!(chosen&(chosen-1))&&(chosen&eligible)));
  const uint64_t req=REQUEST,resp=RESPONSE,miss=RESPONSE_MISS,pa=RESPONSE_PA;
  const uint64_t registered=REGISTERED_RESPONSE,registered_miss=REGISTERED_MISS,registered_pa=REGISTERED_PA;
  if(st_seen) {
    assert(stage[0]==st_previous[0]&&stage[1]==st_previous[1]&&stage[2]==st_previous[2]);
    assert(registered==st_previous[3]);
    if(registered) assert(registered_miss==st_previous_miss&&registered_pa==st_previous_pa);
    if(req) assert(uint64_t(REQUEST_VA)==(st_previous_region<<10));
  }
  assert(req==bool(stage[0]&&!(stage[0]&replace&&active&&!hit)));
  assert(!resp||stage[1]);assert(bool(MISS_EVENT)==bool(resp&&miss));
  assert(bool(UPDATE)==bool(registered&&!registered_miss));assert(!DROP||UPDATE);
  const unsigned slot=chosen?__builtin_ctzll(chosen):0;
  const uint64_t r[21]={base,eligible,chosen,stage[0],stage[1],stage[2],active,hit,replace,req,
    req?uint64_t(REQUEST_VA):0,resp,resp?miss:0,resp?pa:0,registered,registered?registered_miss:0,
    registered?registered_pa:0,uint64_t(UPDATE),uint64_t(DROP),chosen?entries[slot][2]:0,chosen?entries[slot][3]:0};
  for(unsigned kind=0;kind<2;++kind) {
    if(kind==0&&!(context.clean||context.dump)) continue;
    if(kind==1&&!(base|stage[0]|stage[1]|stage[2]|resp|registered)) continue;
    fprintf(st_file,"%lu,%u,%u,%u",context.core,context.clean,context.dump,kind);
    for(uint64_t value:r) fprintf(st_file,",%lu",kind?value:0);fputc('\n',st_file);
  }
  st_previous[0]=chosen;st_previous[1]=stage[0];st_previous[2]=stage[1];st_previous[3]=resp;
  st_previous_region=chosen?entries[slot][3]:0;st_previous_miss=miss;st_previous_pa=pa;st_seen=true;
}
'''
    for label in sorted(values, key=len, reverse=True):
        helper = helper.replace(label, values[label])
    marker = 'static void sms_internal_snapshot(SSimTop *model)'
    assert cpp.count(marker) == 1 and cpp.count('  sms_internal_observe(model);') == 1
    cpp = cpp.replace(marker, helper+'\n'+marker).replace('  sms_internal_observe(model);',
                                                          '  sms_internal_observe(model);\n  sms_tlb_observe(model);')
    return cpp, dict(columns=COLUMNS, allow_TLB_during_S1_hit=allow_hit, is_evicted_equation=eviction,
                     TLB_guard_equation=line, generator_sha256=digest(Path(__file__)), native_counter_events=events,
                     bindings=bindings, source_sha256={str(p):digest(p) for p in [source,*paths]},
                     actual_eligibility_and_three_stage_commits_asserted=True, new_RTL_state_bits=0,
                     scope='Per-cycle eligible masks, selected slot, registered request, actual response and later update. '
                           'Addresses retain virtual/physical roles; same-slot generation must be joined to observed filter allocation. '
                           'No L2 residency, DRAM service, retirement equivalence or saved CPU cycles inferred.')
