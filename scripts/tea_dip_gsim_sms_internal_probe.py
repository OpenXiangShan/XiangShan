"""Observe SMS training, accepted generation and filter ownership using existing GSIM state."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest
from tea_dip_gsim_sms_request_probe import PREFIX

SMS = PREFIX + 'smsOpt__DOT__'
FILTER_FIELDS = [('region_tag', 11), ('region_addr', 40), ('region_bits', 16),
                 ('filter_bits', 16), ('paddr_valid', 1), ('decr_mode', 1)]
GEN_FIELDS = [('region_tag', 11), ('region_addr', 40), ('region_bits', 16),
              ('paddr_valid', 1), ('decr_mode', 1)]


def add_capture(root, header, cpp):
    declarations = dict(re.findall(r'uint(?:8|16|32|64)_t ([^;]+); // width = (\d+),', header))
    bindings = {}

    def field(name, width, array=None):
        declaration = name + (f'[{array}]' if array else '')
        assert declarations.get(declaration) == str(width), declaration
        bindings[declaration] = width
        return 'model->' + name

    def sms(name, width):
        return field(SMS + name, width)

    current, next_values = [], []
    for slot in range(16):
        current.append([sms(f'pf_filter__DOT__valids_{slot}', 1)] + [
            sms(f'pf_filter__DOT__entries_{slot}__DOT__{name}', width) for name, width in FILTER_FIELDS])
        next_values.append([sms(f'pf_filter__DOT__valids_{slot}$NEXT', 1)] + [
            sms(f'pf_filter__DOT__entries_{slot}__DOT__{name}$NEXT', width) for name, width in FILTER_FIELDS])
    gen = [sms('pf_filter__DOT__s1_gen_req__DOT__' + name + '$NEXT', width) for name, width in GEN_FIELDS]
    train = [field(SMS + 'train_filter__DOT__entries__DOT__' + name, width, 8) + '[slot]'
             for name, width in [('pc', 20), ('vaddr', 50), ('paddr', 48)]]
    expressions = {
        'TRAIN_VALID': sms('train_filter__DOT__io__DOT__trainReq__DOT__valid', 1),
        'TRAIN_SLOT': sms('train_filter__DOT__deqPtrExt__DOT__value', 3),
        'TRAIN_ARRAY_VALID': field(SMS + 'train_filter__DOT__valids', 1, 8) + '[slot]',
        'GEN_RAW': sms('_pf_filter_io_gen_req_valid_T_1', 1),
        'GEN_ACCEPTED': sms('pf_filter__DOT__s0_gen_req_valid', 1),
        'GEN_SLOT': sms('pf_filter__DOT__s0_access_way', 4),
        'GEN_HIT': sms('pf_filter__DOT__s0_hit', 1),
        'GEN_PHT': sms('pht_gen_valid', 1),
        'ALLOC_VALID': sms('pf_filter__DOT__s1_valid_r_last_REG', 1),
        'ALLOC_HIT': sms('pf_filter__DOT__s1_hit_r', 1),
        'ALLOC_MASK': sms('pf_filter__DOT__s1_replace_vec_r', 16),
        'SELECT_LOW': sms('pf_filter__DOT__pf_req_arb__DOT___finalSelLow_T_8', 1),
        'CHOSEN_LOW': sms('pf_filter__DOT__pf_req_arb__DOT__lowArb__DOT___chosenOH_T', 8),
        'CHOSEN_HIGH': sms('pf_filter__DOT__pf_req_arb__DOT__highArb__DOT___chosenOH_T', 8),
        'SMS_FIRE': field(PREFIX + 'l2_pf_arb__DOT__grant_2', 1) + ' && ' + sms('_io_l2_req_valid_T', 1),
        'SMS_ADDRESS': field(PREFIX + 'l2_pf_req_pf_to_l2cache_reg__DOT__stages_0__DOT__ram__DOT__addr$NEXT', 48),
    }
    helper = r'''
static uint64_t si_next[16][7]{},si_previous[16][7]{};
static unsigned si_prior_alloc=0;
static bool si_seen=false;
static FILE *si_train=nullptr,*si_gen=nullptr,*si_filter=nullptr;
static void si_open() {
  if(si_train) return;
  si_train=fopen("sms-training.csv","w");si_gen=fopen("sms-generation.csv","w");si_filter=fopen("sms-filter-state.csv","w");
  assert(si_train&&si_gen&&si_filter);
  fprintf(si_train,"core_cycle,clean,native_dump,kind,slot,pc_tag,virtual_address,physical_address\n");
  fprintf(si_gen,"core_cycle,clean,native_dump,kind,slot,hit,pht_valid,region_tag,region_address,region_bits,physical_valid,decreasing\n");
  fprintf(si_filter,"core_cycle,clean,native_dump,kind,slot,new_owner,allocated,region_tag,region_address,region_bits,filter_bits,physical_valid,decreasing\n");
}
static void si_filter_write(unsigned kind,unsigned slot,unsigned fresh,const uint64_t *r) {
  fprintf(si_filter,"%lu,%u,%u,%u,%u,%u,%lu,%lu,%lu,%lu,%lu,%lu,%lu\n",context.core,context.clean,context.dump,kind,slot,fresh,r[0],r[1],r[2],r[3],r[4],r[5],r[6]);
}
static void sms_internal_snapshot(SSimTop *model) {
SNAPSHOTS
}
static void sms_internal_observe(SSimTop *model) {
  if(!context.seen) return;
  si_open();
  const uint64_t current[16][7]={CURRENT};
  const bool force=!si_seen||context.clean||context.dump;
  if(context.clean||context.dump) {
    fprintf(si_train,"%lu,%u,%u,0,0,0,0,0\n",context.core,context.clean,context.dump);
    fprintf(si_gen,"%lu,%u,%u,0,0,0,0,0,0,0,0,0\n",context.core,context.clean,context.dump);
    const uint64_t zero[7]{};si_filter_write(0,0,0,zero);
  }
  for(unsigned i=0;i<16;++i) {
    assert(si_next[i][0]==current[i][0]);
    if(current[i][0]) for(unsigned j=1;j<7;++j) assert(si_next[i][j]==current[i][j]);
    if(!si_seen) assert(!current[i][0]);
    const unsigned fresh=(si_prior_alloc>>i)&1;
    if(fresh) assert(current[i][0]);
    if(si_seen&&!si_previous[i][0]&&current[i][0]) assert(fresh);
    uint64_t normalized[7]{};
    if(current[i][0]) std::memcpy(normalized,current[i],sizeof(normalized));
    if(force||fresh||std::memcmp(normalized,si_previous[i],sizeof(normalized))) si_filter_write(1,i,fresh,normalized);
    std::memcpy(si_previous[i],normalized,sizeof(normalized));
  }
  if(TRAIN_VALID) {
    const unsigned slot=TRAIN_SLOT;assert(slot<8&&TRAIN_ARRAY_VALID);
    fprintf(si_train,"%lu,%u,%u,1,%u,%lu,%lu,%lu\n",context.core,context.clean,context.dump,slot,uint64_t(TRAIN_PC),uint64_t(TRAIN_VA),uint64_t(TRAIN_PA));
  }
  if(GEN_RAW) {
    const bool accepted=GEN_ACCEPTED;
    const uint64_t r[5]={GEN_VALUES};
    fprintf(si_gen,"%lu,%u,%u,%u,%u,%u,%u,%lu,%lu,%lu,%lu,%lu\n",context.core,context.clean,context.dump,accepted?1:2,accepted?unsigned(GEN_SLOT):0,accepted?unsigned(GEN_HIT):0,unsigned(GEN_PHT),accepted?r[0]:0,accepted?r[1]:0,accepted?r[2]:0,accepted?r[3]:0,accepted?r[4]:0);
  }
  assert(!GEN_ACCEPTED||GEN_RAW);
  if(SMS_FIRE) {
    unsigned mask=SELECT_LOW?unsigned(CHOSEN_LOW):unsigned(CHOSEN_HIGH)<<8;
    assert(mask&&!(mask&(mask-1)));const unsigned slot=__builtin_ctz(mask);
    const uint64_t *r=current[slot];const uint64_t pending=r[3]&~r[4]&65535;
    assert(r[0]&&r[5]&&pending);
    const unsigned offset=r[6]?63-__builtin_clzll(pending):__builtin_ctzll(pending);
    assert((r[2]<<10|uint64_t(offset)<<6)==uint64_t(SMS_ADDRESS));
    si_filter_write(2,slot,0,r);
  }
  si_prior_alloc=(ALLOC_VALID&&!ALLOC_HIT)?unsigned(ALLOC_MASK):0;
  si_seen=true;
}
'''
    helper = helper.replace('SNAPSHOTS', '\n'.join(
        f'  si_next[{i}][{j}]={value};' for i, values in enumerate(next_values) for j, value in enumerate(values)))
    helper = helper.replace('CURRENT', ','.join('{' + ','.join('uint64_t(' + v + ')' for v in row) + '}' for row in current))
    helper = helper.replace('GEN_VALUES', ','.join('uint64_t(' + v + ')' for v in gen))
    expressions.update(zip(['TRAIN_PC', 'TRAIN_VA', 'TRAIN_PA'], train))
    # Longer names first so TRAIN_SLOT does not alter other placeholders.
    for label in sorted(expressions, key=len, reverse=True):
        helper = helper.replace(label, expressions[label])
    for old, new in [('static void sms_snapshot(SSimTop *model)', helper + '\nstatic void sms_snapshot(SSimTop *model)'),
                     ('  sms_snapshot(model);', '  sms_internal_snapshot(model);\n  sms_snapshot(model);'),
                     ('  sms_observe(model);', '  sms_observe(model);\n  sms_internal_observe(model);')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    sources = [root / 'src/main/scala/xiangshan/mem/prefetch' / name
               for name in ['SMSPrefetcher.scala', 'L1PrefetchComponent.scala', 'PrefetcherWrapper.scala']]
    sources.append(root / 'utility/src/main/scala/utility/FastArbiter.scala')
    generated = root / 'build/gsim-compile/model/SimTop129.cpp'
    text = generated.read_text()
    assert 'SMS_train_filter_deqCounter + ((uint64_t)' + expressions['TRAIN_VALID'].removeprefix('model->') + ')' in text
    assert 'sms_pf_filter_recv_reqCounter + ((uint64_t)' + expressions['GEN_RAW'].removeprefix('model->') + ')' in text
    assert expressions['GEN_ACCEPTED'].removeprefix('model->') in text
    return cpp, dict(training_PC_tag_bits=20, training_entries=8, filter_entries=16,
                     filter_region_bytes=1024, filter_blocks=16, block_bytes=64,
                     current_NEXT_commits_asserted=True, actual_filter_allocation_edges=True,
                     actual_arbiter_departure_owner_asserted=True, bindings=bindings,
                     source_sha256={str(p): digest(p) for p in [*sources, generated]}, new_RTL_state_bits=0,
                     scope='Actual training dequeue, raw/accepted filter generation, physical filter owner states and actual arbiter departures. Consecutive-tag-suppressed generation retains an explicit unknown payload. PC is a20-bit training tag, not full retired PC; generation causality from a particular training event is not inferred.')


def build(sms_build_proof, output):
    proof_path, output = Path(sms_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and previous['original_objects'] == 577
    assert previous['SMS_request_binding']['actual_pipeline_commit_and_forward_asserted']
    parent, root = proof_path.parent, Path(previous['root'])
    assert digest(parent / 'probe.cpp') == previous['probe_cpp_sha256'] and not output.exists()
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    cpp, binding = add_capture(root, (root / 'build/gsim-compile/model/SimTop.h').read_text(), (parent / 'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_bytes((parent / 'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as stdout, (output / f'build-{i}.stderr').open('wb') as stderr:
            subprocess.run(command, stdout=stdout, stderr=stderr, check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    result = dict(previous, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'), commands=commands,
                  probe_cpp_sha256=digest(output / 'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  base_SMS_build_proof_sha256=digest(proof_path), SMS_internal_binding=binding)
    (output / 'build-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--sms-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = build(args.sms_build_proof, args.output)
    print(json.dumps({k: result[k] for k in ['passed', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
