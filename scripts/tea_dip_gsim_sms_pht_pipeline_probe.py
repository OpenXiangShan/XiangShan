"""Trace existing PHT pipeline and prediction buffers on an immutable eight-CSV GSIM build."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest
from tea_dip_gsim_sms_internal_probe import SMS

COLUMNS = ['s1_valid', 's1_update', 's1_index', 's1_tag',
           's2_valid', 's2_update', 's2_index', 's2_tag', 's2_hits',
           's3_valid', 's3_update', 's3_index', 's3_tag', 's3_hits',
           's3_pattern', 's3_mask', 's3_region', 's4_valid_mask', 's4_next_mask',
           'pht_output', 'pht_enabled', 'filter_input', 'filter_accepted',
           'output_tag', 'output_region', 'output_bits', 'output_physical', 'output_decreasing']


def add_capture(root, header, cpp, previous):
    declarations = dict(re.findall(r'uint(?:8|16|32|64)_t ([^;]+); // width = (\d+),', header))
    bindings = {}

    def field(name, width):
        assert declarations.get(name) == str(width), name
        bindings[name] = width
        return 'model->' + name

    prefix = SMS + 'pht__DOT__'
    scalar = [('s1_valid_r', 1), ('s1_evict', 1), ('s1_ram_raddr', 5), ('s1_tag', 13),
              ('s2_valid_last_REG', 1), ('s2_evict', 1), ('s2_ram_waddr', 5), ('s2_tag', 13)]
    values = [field(prefix + name, width) for name, width in scalar]
    mask = lambda names: ' | '.join(f'(uint64_t({field(prefix + name, 1)}) << {i})' for i, name in enumerate(names))
    values.append(mask(['s2_hit_vec_0', 's2_hit_vec_1']))
    values += [field(prefix + name, width) for name, width in
               [('s3_valid_last_REG', 1), ('s3_evict', 1), ('s3_ram_waddr', 5), ('s3_pht_tag', 13)]]
    values.append(mask(['s3_hit_vec_0', 's3_hit_vec_1']))
    values += [field(prefix + name, width) for name, width in
               [('s3_hist_pf_gen', 30), ('s3_hist_update_mask', 30), ('s3_region_paddr', 40)]]
    banks = ['s4_pf_gen_' + branch + '_region' for branch in ['cur', 'incr', 'decr']]
    values += [mask([bank + '_valid' for bank in banks]), mask([bank + '_valid$NEXT' for bank in banks])]
    paths = [Path(p) for p in subprocess.check_output(
        ['rg', '-l', '-F', '-e', 'sms_pht_pf_genCounter + ', '-e', '_pf_filter_io_gen_req_valid_T_1 =',
         str(root / 'build/gsim-compile/model'), '--glob', '*.cpp'], text=True).splitlines()]
    generated = '\n'.join(p.read_text() for p in paths)
    raw, = set(re.findall(r'sms_pht_pf_genCounter \+ \(\(uint64_t\)([\w$]+)\)', generated))
    arbiter = prefix + 'pf_gen_req_arb__DOT__'
    grant = arbiter + 'grant_2'
    assert raw == arbiter + '_io_out_valid_T_1'
    assert raw + ' = (' + arbiter + '_io_out_valid_T | ' + prefix + banks[2] + '_valid);' in generated
    assert arbiter + '_io_out_valid_T = (' + grant + ' == 0x0);' in generated
    raw_expression = '(!' + field(grant, 1) + ' || ' + field(prefix + banks[2] + '_valid', 1) + ')'
    values += [raw_expression, field(SMS + 'pht_gen_valid', 1),
               field(SMS + '_pf_filter_io_gen_req_valid_T_1', 1), field(SMS + 'pf_filter__DOT__s0_gen_req_valid', 1)]
    payload = []
    for i, bank in enumerate(banks):
        payload.append([field(prefix + bank + '__DOT__' + name, width) for name, width in
                        [('region_tag', 11), ('region_addr', 40), ('region_bits', 16)]] +
                       [('1' if i == 0 else field(prefix + bank + '__DOT__paddr_valid', 1)), str(int(i == 2))])
    source = root / 'src/main/scala/xiangshan/mem/prefetch/SMSPrefetcher.scala'
    compact = re.sub(r'\s+', '', source.read_text())
    for text in ['s4_pf_gen_cur_region_valid:=s3_cur_region_valid',
                 's4_pf_gen_incr_region_valid:=s3_incr_region_valid||(!pf_gen_req_arb.io.in(1).ready&&s4_pf_gen_incr_region_valid)',
                 's4_pf_gen_decr_region_valid:=s3_decr_region_valid||(!pf_gen_req_arb.io.in(2).ready&&s4_pf_gen_decr_region_valid)',
                 'pf_gen_req_arb.io.out.ready:=true.B']:
        assert text in compact, text
    assert SMS + '_pf_filter_io_gen_req_valid_T_1 = (' + SMS + 'pht_gen_valid & ' + SMS + '_pf_filter_io_gen_req_valid_T);' in generated
    for name in ['region_tag', 'region_addr', 'region_bits', 'paddr_valid', 'decr_mode']:
        assert SMS + 'pf_filter__DOT__s1_gen_req__DOT__' + name + '$NEXT = ' + prefix + 'pf_gen_req_arb__DOT__io__DOT__out__DOT__bits__DOT__' + name + ';' in generated
    accepted = [field(SMS + 'pf_filter__DOT__s1_gen_req__DOT__' + name + '$NEXT', width)
                for name, width in [('region_tag', 11), ('region_addr', 40), ('region_bits', 16), ('paddr_valid', 1), ('decr_mode', 1)]]
    service = [previous['SMS_PHT_binding']['actual_counter_events'][name] for name in ['lookup_out', 'evict_out']]
    helper = r'''
static FILE *pp_file=nullptr;
static bool pp_seen=false;
static uint64_t pp_next_mask=0;
static void pht_pipeline_observe(SSimTop *model) {
  if(!context.seen) return;
  if(!pp_file) {
    pp_file=fopen("sms-pht-pipeline.csv","w");assert(pp_file);
    fprintf(pp_file,"HEADER\n");
  }
  uint64_t r[28]={VALUES};
  const uint64_t payload[3][5]={PAYLOAD},accepted[5]={ACCEPTED};
  const uint64_t valid=r[17],out=r[19];
  if(!pp_seen) assert(!(r[0]||r[4]||r[9]||valid));
  else assert(valid==pp_next_mask);
  assert(out==bool(valid)&&(!r[20]||out)&&(!r[21]||r[20])&&(!r[22]||r[21]));
  const bool prediction=r[9]&&!r[10]&&r[13];
  const uint64_t fresh=uint64_t(prediction&&bool(r[14]&r[15])) |
    (uint64_t(prediction&&bool((r[14]>>15)&~(r[15]>>15)&32767))<<1) |
    (uint64_t(prediction&&bool(r[14]&~r[15]&32767))<<2);
  const uint64_t next=(fresh&1) | (uint64_t(bool(fresh&2)||bool((valid&2)&&(valid&1)))<<1) |
    (uint64_t(bool(fresh&4)||bool((valid&4)&&(valid&3)))<<2);
  assert(next==r[18]);pp_next_mask=r[18];
  if(out) {
    const unsigned branch=__builtin_ctzll(valid);
    for(unsigned i=0;i<5;++i) r[23+i]=payload[branch][i];
    assert(r[23]==OUTPUT_TAG);
    if(r[22]) for(unsigned i=0;i<5;++i) assert(accepted[i]==r[23+i]);
  }
  for(unsigned kind=0;kind<2;++kind) {
    if(kind==0&&!(context.clean||context.dump)) continue;
    if(kind==1&&pp_seen&&!(r[0]||r[4]||r[9]||valid||SERVICE)) continue;
    fprintf(pp_file,"%lu,%u,%u,%u",context.core,context.clean,context.dump,kind);
    for(uint64_t value:r) fprintf(pp_file,",%lu",value);
    fputc('\n',pp_file);
  }
  pp_seen=true;
}
'''
    helper = helper.replace('HEADER', ','.join(['core_cycle', 'clean', 'native_dump', 'kind', *COLUMNS]))
    helper = helper.replace('VALUES', ','.join('uint64_t(' + v + ')' for v in values + ['0'] * 5))
    helper = helper.replace('PAYLOAD', ','.join('{' + ','.join('uint64_t(' + v + ')' for v in row) + '}' for row in payload))
    helper = helper.replace('ACCEPTED', ','.join('uint64_t(' + v + ')' for v in accepted))
    helper = helper.replace('OUTPUT_TAG', field(prefix + 'pf_gen_req_arb__DOT__io__DOT__out__DOT__bits__DOT__region_tag', 11))
    helper = helper.replace('SERVICE', ' || '.join(service))
    for old, new in [('static void pht_observe(SSimTop *model)', helper + '\nstatic void pht_observe(SSimTop *model)'),
                     ('  pht_observe(model);', '  pht_observe(model);\n  pht_pipeline_observe(model);')]:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp, dict(columns=COLUMNS, stages=3, prediction_buffers=3, PHT_only_filter_source=True,
                     current_NEXT_and_prediction_arbiter_asserted=True, accepted_payload_asserted=True,
                     all_inflight_tail_stages_captured=True, new_RTL_state_bits=0, bindings=bindings,
                     source_sha256={str(p): digest(p) for p in [source, *paths]},
                     scope='Actual queue-to-pipeline identity and three prediction-buffer fates. PHT RAM learning versions, retired-instruction identity and CPU causality are not captured.')


def build(proof_path, output):
    proof_path, output = Path(proof_path).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    parent, root = proof_path.parent, Path(previous['root'])
    assert previous['passed'] and previous['original_files_unchanged'] and not output.exists()
    assert digest(parent / 'probe.cpp') == previous['probe_cpp_sha256']
    for name, h in previous['original_sha256'].items():
        assert digest(Path(name)) == h, name
    cpp, binding = add_capture(root, (root / 'build/gsim-compile/model/SimTop.h').read_text(), (parent / 'probe.cpp').read_text(), previous)
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_bytes((parent / 'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in command] for command in previous['commands']]
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as out, (output / f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, h in previous['original_sha256'].items():
        assert digest(Path(name)) == h, name
    proof = dict(previous, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'), commands=commands,
                 probe_cpp_sha256=digest(output / 'probe.cpp'), generator_sha256=digest(Path(__file__)),
                 base_PHT_build_proof_sha256=digest(proof_path), SMS_PHT_pipeline_binding=binding,
                 scope='Nine source-qualified CSVs on the original immutable model; host-only pipeline/region-buffer observation. Own-source neutral and semantic calibration remains required.')
    (output / 'build-proof.json').write_text(json.dumps(proof, indent=2) + '\n')
    return proof


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--pht-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    proof = build(args.pht_build_proof, args.output)
    print(json.dumps({k: proof[k] for k in ['passed', 'source', 'binary', 'binary_sha256']}))


if __name__ == '__main__':
    main()
