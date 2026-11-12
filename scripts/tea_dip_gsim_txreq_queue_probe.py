"""Relink an existing four-bank GSIM model with a read-only TXREQ queue observer.

Original RTL, header, model objects and binary remain unchanged. GNU linker wraps
SSimTop::step and the actual L2 trace callback: post-step queue pointers and
computed next pointers match that callback. Next pointers must commit next step.
The CSV is written in the guest working directory. Supported model: four 16-entry
TXREQ queues and the existing L2TxreqArbTraceWriter callback, without LTO.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shlex
import subprocess

PREFIX = 'cpu__DOT__l_soc__DOT__core_with_l2__DOT__l2top__DOT__inner__DOT__l2cache__DOT__slices_{}__DOT__txreq__DOT__queue__DOT__'
COUNT_HEADER = '''#pragma once
static unsigned queue_count(unsigned enq, unsigned deq, bool full) {
  return enq == deq ? (full ? 16u : 0u) : ((enq - deq) & 15u);
}
'''


def source(header, database, symbols):
    prefixes = [PREFIX.format(bank) for bank in range(4)]
    for prefix in prefixes:
        assert prefix + 'ram__DOT__addr[16];' in header, 'Only the observed 16-entry TXREQ queues are supported'
        for field, width in [('enq_ptr_value', 4), ('deq_ptr_value', 4), ('maybe_full', 1)]:
            for suffix in ['', '$NEXT']:
                assert re.search(r'uint8_t ' + re.escape(prefix + field + suffix) + r'; // width = ' + str(width) + r',', header), prefix + field + suffix
    step, = [line.split()[-1] for line in symbols.splitlines() if line.split()[-1:] == ['_ZN7SSimTop4stepEv']]
    trace, = [line.split()[-1] for line in symbols.splitlines() if 'L2TxreqArbTraceWriter' in line]
    signature, = re.findall(r'void L2TxreqArbTraceWriter\((.*?)\) \{', database, re.S)
    arguments = [parameter.strip().split()[-1].lstrip('*') for parameter in signature.split(',')]
    assert {'stamp', 'data_nativeCycle', 'data_clean', 'data_nativeDump', 'data_logEnable', 'data_events_validMask', 'data_events_readyMask'} <= set(arguments)
    real_step, wrapped_step = '__real_' + step, '__wrap_' + step
    real_trace, wrapped_trace = '__real_' + trace, '__wrap_' + trace
    snapshots = ',\n'.join('    {' + ','.join('model->' + prefix + field + '$NEXT' for field in ['enq_ptr_value','deq_ptr_value','maybe_full']) + '}' for prefix in prefixes)
    reads = '\n'.join(f'    before[{bank}][{index}] = model->{prefix}{field}; after[{bank}][{index}] = model->{prefix}{field}$NEXT;'
                      for bank, prefix in enumerate(prefixes)
                      for index, field in enumerate(['enq_ptr_value', 'deq_ptr_value', 'maybe_full']))
    text = '''#include "SimTop.h"
#include "queue_count.h"
#include <cstdio>
#include <cstdlib>
#include <cassert>
struct TraceRow { bool seen; uint64_t cycle; unsigned clean, dump, enabled, valid, ready; };
static TraceRow row{};
static bool in_step = false;
static FILE *output = nullptr;
extern "C" void REAL_STEP(SSimTop *);
extern "C" void REAL_TRACE(SIGNATURE);
extern "C" void WRAP_TRACE(SIGNATURE) {
  if (en && !reset) {
    assert(in_step && !row.seen && stamp == data_nativeCycle);
    row = {true, data_nativeCycle, data_clean, data_nativeDump, data_logEnable, data_events_validMask, data_events_readyMask};
  }
  REAL_TRACE(ARGUMENTS);
}
extern "C" void WRAP_STEP(SSimTop *model) {
  assert(!in_step);
  unsigned previous_next[4][3] = {
SNAPSHOTS
  };
  row.seen = false;
  in_step = true;
  REAL_STEP(model);
  in_step = false;
  unsigned before[4][3], after[4][3], occupancy[4], next_occupancy[4], enq_mask = 0, deq_mask = 0;
READS
  for (unsigned bank = 0; bank < 4; ++bank)
    for (unsigned field = 0; field < 3; ++field)
      assert(before[bank][field] == previous_next[bank][field]);
  if (!row.seen) {
    for (unsigned bank = 0; bank < 4; ++bank)
      assert(queue_count(before[bank][0], before[bank][1], before[bank][2]) == 0);
    return;
  }
  for (unsigned bank = 0; bank < 4; ++bank) {
    occupancy[bank] = queue_count(before[bank][0], before[bank][1], before[bank][2]);
    next_occupancy[bank] = queue_count(after[bank][0], after[bank][1], after[bank][2]);
    unsigned enqueued = (after[bank][0] - before[bank][0]) & 15u;
    unsigned dequeued = (after[bank][1] - before[bank][1]) & 15u;
    assert(enqueued <= 1 && dequeued <= 1);
    assert(bool(occupancy[bank]) == bool(row.valid & (1u << bank)));
    assert(dequeued == bool(row.ready & (1u << bank)));
    assert(next_occupancy[bank] + dequeued == occupancy[bank] + enqueued);
    enq_mask |= enqueued << bank; deq_mask |= dequeued << bank;
  }
  if (!output) {
    output = fopen("txreq-queue-probe.csv", "w");
    if (!output) { perror("txreq-queue-probe.csv"); abort(); }
    fprintf(output, "native_cycle,clean,native_dump,log_enable,valid_mask,ready_mask,q0,q1,q2,q3,next_q0,next_q1,next_q2,next_q3,enq_mask,deq_mask\\n");
  }
  fprintf(output, "%lu,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u\\n", row.cycle,row.clean,row.dump,row.enabled,row.valid,row.ready,occupancy[0],occupancy[1],occupancy[2],occupancy[3],next_occupancy[0],next_occupancy[1],next_occupancy[2],next_occupancy[3],enq_mask,deq_mask);
}
'''
    for key, value in [('REAL_STEP',real_step),('WRAP_STEP',wrapped_step),('REAL_TRACE',real_trace),('WRAP_TRACE',wrapped_trace),('SIGNATURE',signature),('ARGUMENTS',','.join(arguments)),('SNAPSHOTS',snapshots),('READS',reads)]:
        text = text.replace(key, value)
    return text, [step, trace]


def build(root, output, compiler, dramsim3_library):
    root, output = Path(root).resolve(), Path(output).resolve()
    dramsim3_library = Path(dramsim3_library).resolve()
    assert not output.exists()
    model = root / 'build/gsim-compile/model'
    original = root / 'build/gsim-compile/emu'
    header, database = model / 'SimTop.h', root / 'build/chisel_db.cpp'
    objects = sorted(model.glob('*.o')) + sorted((root / 'build/gsim-compile/other').glob('*.o'))
    assert objects
    def digest(path):
        with path.open('rb') as stream:
            return hashlib.file_digest(stream, 'sha256').hexdigest()
    originals = {str(p):digest(p) for p in [original,header,database,dramsim3_library,*objects]}
    symbols = subprocess.check_output(['nm','--defined-only',str(original)],text=True)
    cpp, wrapped = source(header.read_text(), database.read_text(), symbols)
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    (output/'queue_count.h').write_text(COUNT_HEADER)
    (output/'objects.rsp').write_text('\n'.join(shlex.quote(str(p)) for p in objects)+'\n')
    commands = [[compiler,'-std=c++17','-O2','-DGSIM','-fbracket-depth=2048','-I'+str(model),'-I'+str(output),'-c',str(output/'probe.cpp'),'-o',str(output/'probe.o')],
                [compiler,'@'+str(output/'objects.rsp'),str(output/'probe.o'),*['-Wl,--wrap='+name for name in wrapped],str(dramsim3_library),'-lsqlite3','-lz','-lzstd','-ldl','-o',str(output/'emu')]]
    for index, command in enumerate(commands):
        result = subprocess.run(command,capture_output=True,text=True)
        (output/f'command-{index}.stdout').write_text(result.stdout)
        (output/f'command-{index}.stderr').write_text(result.stderr)
        assert result.returncode == 0, result.stderr[-6000:]
    assert all(digest(Path(p))==value for p,value in originals.items())
    proof = dict(passed=True,root=str(root),binary=str(output/'emu'),binary_sha256=digest(output/'emu'),original_binary=str(original),original_binary_sha256=originals[str(original)],original_files_unchanged=True,
                 original_objects=len(objects),dramsim3_library=str(dramsim3_library),original_sha256=originals,commands=commands,wrapped_symbols=wrapped,probe_cpp_sha256=digest(output/'probe.cpp'),
                 scope='Relinked observer only. Exact original header, RTL producer binary and every compiled object remain unchanged. No RTL re-elaboration or new modeled state. Actual next-register commitment and pointer/valid/ready/SQL phase and execution neutrality still require guest calibration; this build proof alone does not establish accuracy or performance.')
    (output/'build-proof.json').write_text(json.dumps(proof,indent=2)+'\n')
    return proof


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    parser.add_argument('--compiler',default='clang++')
    parser.add_argument('--dramsim3-library',type=Path,required=True)
    args = parser.parse_args()
    result = build(args.root,args.output,args.compiler,args.dramsim3_library)
    print(json.dumps({key:result[key]for key in ['passed','binary','binary_sha256','original_objects','original_files_unchanged']}))


if __name__ == '__main__':
    main()
