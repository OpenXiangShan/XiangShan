"""Relink an immutable GSIM model to distinguish source consumption and FIFO admission."""
import argparse
from pathlib import Path
import json
import re
from tea_dip_gsim_load_service_probe import build
from tea_dip_gsim_prefetch_queue_probe import add_capture, FIELDS

ADMISSION_FIELDS = ['source_fire', 'enqueue', 'full', 'line', 'source_id', 'req_source', 'need_t',
                    'selected_count', 'consumed_count', 'admitted_count', 'dropped_count']


def source(root, header, database, symbols):
    queue, binding = add_capture(root, header, '', standalone=True)
    calls = []
    for row in binding['bindings']:
        values = row['values']
        needle = f"    uint64_t row[{len(FIELDS)}]={{{','.join(values)}}};"
        full = f'({values[0]}=={values[1]} && {values[2]})'
        args = [str(row['bank']), row['raw_enqueue'], row['source_fire'], 'row[3]', full, *values[11:15]]
        assert queue.count(needle) == 1
        queue = queue.replace(needle, needle + '\n    admission_observe(' + ','.join(args) + ');')
        calls.append(dict(bank=row['bank'], values=args))
    helper = f'''
static FILE *admission_output=nullptr;
static uint64_t admission_counts[4][4]{{}};
static void admission_finish();
static void admission_write(unsigned kind,unsigned bank,const uint64_t *row) {{
  if(!admission_output) {{
    admission_output=fopen("prefetch-admission.csv","w"); assert(admission_output);
    fprintf(admission_output,"core_cycle,clean,native_dump,kind,bank,{','.join(ADMISSION_FIELDS)}\\n");
    std::atexit(admission_finish);
  }}
  fprintf(admission_output,"%lu,%u,%u,%u,%u",context.core,kind==2?0:context.clean,kind==2?0:context.dump,kind,bank);
  for(unsigned j=0;j<{len(ADMISSION_FIELDS)};++j) fprintf(admission_output,",%lu",row[j]);
  fprintf(admission_output,"\\n");
}}
static void admission_footer(unsigned kind,unsigned bank) {{
  uint64_t row[{len(ADMISSION_FIELDS)}]{{}};
  for(unsigned j=0;j<4;++j) row[7+j]=admission_counts[bank][j];
  admission_write(kind,bank,row);
}}
static void admission_finish() {{
  if(!admission_output) return;
  for(unsigned bank=0;bank<4;++bank) admission_footer(2,bank);
  fclose(admission_output);admission_output=nullptr;
}}
static void admission_observe(unsigned bank,bool valid,bool consumed,bool enqueued,bool full,
  uint64_t line,uint64_t source_id,uint64_t req_source,uint64_t need_t) {{
  assert(!consumed || valid); assert(!enqueued || consumed);
  admission_counts[bank][0]+=valid;admission_counts[bank][1]+=consumed;
  admission_counts[bank][2]+=enqueued;admission_counts[bank][3]+=consumed&&!enqueued;
  if(valid) {{
    const uint64_t row[{len(ADMISSION_FIELDS)}]={{{','.join(['uint64_t(consumed)','uint64_t(enqueued)','uint64_t(full)','line','source_id','req_source','need_t',*[f'admission_counts[bank][{j}]' for j in range(4)]])}}};
    admission_write(1,bank,row);
  }}
  if(context.clean || context.dump) admission_footer(0,bank);
}}
'''
    enum_source = root / 'utility/src/main/scala/utility/TLUtils/BusKeyField.scala'
    enumeration = re.findall(r'val (\w+) = Value\("(\w+)"\)', enum_source.read_text().split('object MemReqSource extends Enumeration', 1)[1].split('val ReqSourceCount', 1)[0])
    actual_ids = {name: i for i, (name, label) in enumerate(enumeration) if name == label}
    assert {name: actual_ids[name] for name in ['Prefetch2L2BOP', 'Prefetch2L2PBOP', 'Prefetch2L2TP', 'Prefetch2L2NL', 'Prefetch2L2CDP']} == dict(Prefetch2L2BOP=8, Prefetch2L2PBOP=9, Prefetch2L2TP=13, Prefetch2L2NL=15, Prefetch2L2CDP=16)
    step = '_ZN7SSimTop4stepEv'
    assert any(line.split()[-1:] == [step] for line in symbols.splitlines())
    writer, = [line.split()[-1] for line in symbols.splitlines() if 'InstTable0Writer' in line]
    signature, = re.findall(r'void InstTable0Writer\((.*?)\) \{', database, re.S)
    arguments = [value.strip().split()[-1].lstrip('*') for value in signature.split(',')]
    prefix = 'difftest__DOT__perfCtrl__DOT__'
    for field in ['clean', 'dump']:
        assert re.search(r'uint8_t ' + re.escape(prefix + field) + r'; // width = 1,', header)
    cpp = f'''#include "SimTop.h"
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
struct Context {{bool seen;uint64_t core;unsigned clean,dump;}};
static Context context{{}};
static bool in_step=false;
{helper}
{queue}
extern "C" void __real_{writer}({signature});
extern "C" void __wrap_{writer}({signature}) {{
  if(!reset) {{assert(in_step);if(context.seen) assert(context.core==stamp);context.seen=true;context.core=stamp;}}
  __real_{writer}({','.join(arguments)});
}}
extern "C" void __real_{step}(SSimTop *);
extern "C" void __wrap_{step}(SSimTop *model) {{
  assert(!in_step);context={{false,0,unsigned(model->{prefix}clean),unsigned(model->{prefix}dump)}};
  in_step=true;__real_{step}(model);in_step=false;
  assert(context.clean==model->{prefix}clean && context.dump==model->{prefix}dump);
  prefetch_queue_observe(model);
}}
'''
    return cpp, [step, writer], dict(kind='prefetch_admission', queue=binding,
        fields=ADMISSION_FIELDS, admission_bindings=calls, new_RTL_state_bits=0,
        scope='Source-bound overwrite/backpressure/drop-new/receiver-protection/CDP-victim policies. Actual FIFO ready/valid admission '
              'includes flow-through. Source handshakes can consume dropped hints; input-valid without '
              'consumption is a condition, not a unique request. NEXT pointers/data and pipeline owners '
              'are asserted; closed native/SQL neutrality and semantic calibration remain required.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--compiler', default='clang++')
    parser.add_argument('--dramsim3-library', type=Path, required=True)
    args = parser.parse_args()
    proof = build(args.root, args.output, args.compiler, args.dramsim3_library, source_factory=source)
    print(json.dumps({k: proof[k] for k in ['passed', 'source', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
