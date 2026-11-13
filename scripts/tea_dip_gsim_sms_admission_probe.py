"""Observe SMS training offers, duplicate predicates and actual allocation without RTL state."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest
from tea_dip_gsim_sms_internal_probe import SMS


def add_capture(root, header, cpp):
    prefix = SMS + 'train_filter__DOT__'
    declarations = dict(re.findall(r'uint(?:8|16|32|64)_t ([^;]+); // width = (\d+),', header))
    bindings = {}

    def field(name, width, size=None):
        key = prefix + name + (f'[{size}]' if size else '')
        assert declarations.get(key) == str(width), key
        bindings[key] = width
        return 'model->' + prefix + name

    # These predicate nodes remain exported after GSIM eliminates local Cat/OR wires.
    need_nodes = [field(name, 1) for name in ['_needAlloc_0_T_1', '_needAlloc_1_T_3', '_needAlloc_2_T_3']]
    alloc_nodes = [field(f'_canAlloc_{i}_T_1', 1) for i in range(3)]
    mask = lambda nodes: '(' + '|'.join(f'(unsigned({node})<<{4-i})' for i, node in enumerate(nodes)) + ')'
    needed, allocated = mask(need_nodes), mask(alloc_nodes)
    valids = [field(f'ldReorderBufferedOpt__DOT__valid_{i}', 1) for i in range(3)]
    inputs = [[field(f'ldReorderBufferedOpt__DOT__bits__DOT__{name}_{i}', bits)
               for name, bits in [('pc', 20), ('vaddr', 50), ('paddr', 48)]] for i in range(3)]
    entries = [field('entries__DOT__' + name, bits, 8) for name, bits in [('pc', 20), ('vaddr', 50)]]
    valid = field('valids', 1, 8)
    src = root / 'src/main/scala/xiangshan/mem/prefetch'
    sms_source = (src / 'SMSPrefetcher.scala').read_text()
    assert 'train_filter.io.enable := true.B' in sms_source and 'train_filter.io.flush := false.B' in sms_source
    assert 'train_filter.io.trainReq.ready := true.B' in sms_source
    line, = [line for line in sms_source.splitlines() if 'val train_filter = Module(new TrainFilter(' in line]
    qualified = 'matchPc = true' in line
    helper = r'''
static FILE *sa_file=nullptr;
static unsigned sa_hash(uint64_t va) {
  const uint64_t block=va>>6;
  return unsigned(block&1023)|unsigned(((block>>10)^(block>>15)^(block>>20))&31)<<10;
}
static void sms_admission_observe(SSimTop *model) {
  if(!context.seen) return;
  if(!sa_file) {
    sa_file=fopen("sms-training-admission.csv","w");assert(sa_file);
    fprintf(sa_file,"core_cycle,clean,native_dump,kind,lane,pc_tag,virtual_address,physical_address,need_alloc,can_alloc,occupancy,entry_mask,entry_same_pc_mask,prev_mask,prev_same_pc_mask\n");
  }
  if(context.clean||context.dump) fprintf(sa_file,"%lu,%u,%u,0,0,0,0,0,0,0,0,0,0,0,0\n",context.core,context.clean,context.dump);
  const unsigned need=NEED,actual=ACTUAL;
  const bool valid[3]={VALID};
  const unsigned raw=(unsigned(valid[0])<<4)|(unsigned(valid[1])<<3)|(unsigned(valid[2])<<2);
  const uint64_t req[3][3]={INPUTS};
  assert(!(need&~raw)&&!(actual&~need));
  unsigned occupancy=0;for(unsigned j=0;j<8;++j) occupancy+=ENTRY_VALID[j];
  for(unsigned i=0;i<3;++i) {
    if(!valid[i]) continue;
    unsigned e=0,epc=0,p=0,ppc=0;
    for(unsigned j=0;j<8;++j) if(ENTRY_VALID[j]&&sa_hash(ENTRY_VA[j])==sa_hash(req[i][1])) {
      e|=1u<<j;if(ENTRY_PC[j]==req[i][0]) epc|=1u<<j;
    }
    for(unsigned j=0;j<i;++j) if(valid[j]&&sa_hash(req[j][1])==sa_hash(req[i][1])) {
      p|=1u<<j;if(req[j][0]==req[i][0]) ppc|=1u<<j;
    }
    const unsigned n=(need>>(4-i))&1,a=(actual>>(4-i))&1;
    assert(n==unsigned(!(QUALIFIED?(epc||ppc):(e||p))));
    fprintf(sa_file,"%lu,%u,%u,1,%u,%lu,%lu,%lu,%u,%u,%u,%u,%u,%u,%u\n",context.core,context.clean,context.dump,i,req[i][0],req[i][1],req[i][2],n,a,occupancy,e,epc,p,ppc);
  }
}
'''
    replacements = dict(NEED=needed, ACTUAL=allocated, VALID=','.join('bool('+v+')'for v in valids),
                        INPUTS=','.join('{' + ','.join('uint64_t(' + f + ')' for f in row) + '}' for row in inputs),
                        ENTRY_VALID=valid, ENTRY_PC=entries[0], ENTRY_VA=entries[1], QUALIFIED='true' if qualified else 'false')
    for key in sorted(replacements, key=len, reverse=True):
        helper = helper.replace(key, replacements[key])
    old = 'static void sms_internal_snapshot(SSimTop *model)'
    assert cpp.count(old) == 1
    cpp = cpp.replace(old, helper + '\n' + old)
    old = '  sms_internal_observe(model);'
    assert cpp.count(old) == 1
    cpp = cpp.replace(old, old + '\n  sms_admission_observe(model);')
    paths = subprocess.check_output(['rg', '-l', '-F', prefix + 'filtered_enq_pattern =', str(root / 'build/gsim-compile/model'), '--glob', '*.cpp'], text=True).splitlines()
    sources = [src / 'SMSPrefetcher.scala', src / 'L1PrefetchComponent.scala', *map(Path, paths)]
    return cpp, dict(input_lanes=5, active_load_lanes=3, inactive_store_lanes=2, training_entries=8, PC_qualified=qualified,
                     actual_masks_asserted=True, hash_and_PC_match_predicates_asserted=True, bindings=bindings,
                     source_sha256={str(p): digest(p) for p in sources}, new_RTL_state_bits=0,
                     scope='Buffered sorted load offers and their actual same-cycle predicates. Inactive store inputs are checked by the full raw mask. Different-PC-only hash duplicates are opportunities, not guaranteed useful predictions or recoverable CPU cycles.')


def build(internal_build_proof, output):
    path, output = Path(internal_build_proof).resolve(), Path(output).resolve()
    old = json.loads(path.read_text())
    assert old['passed'] and old['original_files_unchanged'] and old['SMS_internal_binding']['training_entries'] == 8
    assert digest(path.parent / 'probe.cpp') == old['probe_cpp_sha256'] and not output.exists()
    for name, expected in old['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    root = Path(old['root'])
    cpp, binding = add_capture(root, (root / 'build/gsim-compile/model/SimTop.h').read_text(), (path.parent / 'probe.cpp').read_text())
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_bytes((path.parent / 'objects.rsp').read_bytes())
    commands = [[arg.replace(str(path.parent), str(output)) for arg in command] for command in old['commands']]
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as out, (output / f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, expected in old['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    proof = dict(old, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'), commands=commands,
                 probe_cpp_sha256=digest(output / 'probe.cpp'), generator_sha256=digest(Path(__file__)),
                 base_SMS_internal_proof_sha256=digest(path), SMS_admission_binding=binding)
    (output / 'build-proof.json').write_text(json.dumps(proof, indent=2) + '\n')
    return proof


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--internal-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    proof = build(args.internal_build_proof, args.output)
    print(json.dumps({k: proof[k] for k in ['passed', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
