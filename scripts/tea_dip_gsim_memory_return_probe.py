"""Relink an accepted L2 observer with read-only memory API and DRAM command capture."""
import argparse
import json
from pathlib import Path
import re
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest

COMMAND_SYMBOL = '_ZN8dramsim312ChannelState21UpdateTimingAndStatesERKNS_7CommandEm'
WRAPPED = ['memory_request', 'memory_response', COMMAND_SYMBOL]


def add_capture(cpp, header):
    for name, width, typename in [('addr',48,'uint64_t'),('id',18,'uint32_t')]:
        assert re.search(typename + r' cpu__DOT__memory__DOT__ram__DOT__rTrackers__DOT__' + name + r'\[64\]; // width = ' + str(width) + ',',header)
    helper = r'''
#include <vector>
#include <map>
#include "common.h"
struct MemoryEvent {
  unsigned kind, accepted, write, id;
  uint64_t address;
  unsigned controller, command;
  uint64_t dram_cycle;
  int channel, rank, bankgroup, bank, row, column;
  unsigned axi_id;
};
static SSimTop *memory_model = nullptr;
static uint64_t memory_step = 0, memory_last_core = 0;
static bool memory_inside = false, memory_have_core = false;
static std::vector<MemoryEvent> memory_pending;
static std::map<void *,unsigned> memory_controllers;
static FILE *memory_output = nullptr;
static void memory_write(const MemoryEvent &e, int64_t core, unsigned clean=0, unsigned dump=0) {
  if (!memory_output) {
    memory_output=fopen("load-memory-events.csv","w");
    if (!memory_output) { perror("load-memory-events.csv"); abort(); }
    fprintf(memory_output,"core_cycle,step,clean,native_dump,kind,accepted,is_write,id,address,controller,command,dram_cycle,channel,rank,bankgroup,bank,row,column,axi_id\n");
  }
  fprintf(memory_output,"%ld,%lu,%u,%u,%u,%u,%u,%u,%lu,%u,%u,%lu,%d,%d,%d,%d,%d,%d,%u\n",core,memory_step,clean,dump,e.kind,e.accepted,e.write,e.id,e.address,e.controller,e.command,e.dram_cycle,e.channel,e.rank,e.bankgroup,e.bank,e.row,e.column,e.axi_id);
}
static void memory_emit(const MemoryEvent &e) {
  if (memory_inside) memory_pending.push_back(e);
  else memory_write(e,memory_have_core ? int64_t(memory_last_core) : -1);
}
static void memory_observe() {
  if (context.seen) { memory_have_core=true; memory_last_core=context.core; }
  for (const auto &e : memory_pending) memory_write(e,context.seen ? int64_t(context.core) : -1);
  memory_pending.clear();
  if (context.seen && (context.clean || context.dump)) {
    const MemoryEvent boundary{}; memory_write(boundary,context.core,context.clean,context.dump);
  }
}
extern "C" bool __real_memory_request(uint64_t,uint32_t,bool);
extern "C" bool __wrap_memory_request(uint64_t address,uint32_t id,bool write) {
  assert(memory_inside && memory_model);
  if (!write) { assert(memory_model->cpu__DOT__memory__DOT__ram__DOT__rTrackers__DOT__id[id & 63u]==id); assert(memory_model->cpu__DOT__memory__DOT__ram__DOT__rTrackers__DOT__addr[id & 63u]==address); }
  const unsigned axi_id=write ? 0 : memory_model->cpu__DOT__memory__DOT__ram__DOT__rTrackers__DOT__id[id & 63u];
  const bool accepted=__real_memory_request(address,id,write);
  memory_emit({1,unsigned(accepted),unsigned(write),id,address,0,0,0,-1,-1,-1,-1,-1,-1,axi_id});
  return accepted;
}
extern "C" uint64_t __real_memory_response(bool);
extern "C" uint64_t __wrap_memory_response(bool write) {
  const uint64_t result=__real_memory_response(write);
  if (result>>32) {
    assert(memory_inside && memory_model);
    const unsigned id=unsigned(result);
    if (!write) assert(memory_model->cpu__DOT__memory__DOT__ram__DOT__rTrackers__DOT__id[id & 63u]==id);
    const uint64_t address=write ? 0 : memory_model->cpu__DOT__memory__DOT__ram__DOT__rTrackers__DOT__addr[id & 63u];
    const unsigned axi_id=write ? 0 : memory_model->cpu__DOT__memory__DOT__ram__DOT__rTrackers__DOT__id[id & 63u];
    memory_emit({2,1,unsigned(write),id,address,0,0,0,-1,-1,-1,-1,-1,-1,axi_id});
  }
  return result;
}
extern "C" void __real_COMMAND(void *,const dramsim3::Command &,uint64_t);
extern "C" void __wrap_COMMAND(void *self,const dramsim3::Command &cmd,uint64_t cycle) {
  auto found=memory_controllers.find(self);
  if (found==memory_controllers.end()) found=memory_controllers.emplace(self,memory_controllers.size()).first;
  memory_emit({3,0,unsigned(cmd.IsWrite()),0,cmd.hex_addr,found->second,unsigned(cmd.cmd_type),cycle,cmd.Channel(),cmd.Rank(),cmd.Bankgroup(),cmd.Bank(),cmd.Row(),cmd.Column(),0});
  __real_COMMAND(self,cmd,cycle);
}
'''.replace('COMMAND', COMMAND_SYMBOL)
    changes = [('static bool in_step = false;', helper + '\nstatic bool in_step = false;'),
               ('  assert(!in_step);', '  assert(!in_step);\n  ++memory_step; memory_inside=true; memory_model=model;'),
               ('; in_step = false;', '; in_step = false; memory_inside=false;\n  memory_observe();')]
    for old, new in changes:
        assert cpp.count(old) == 1, old
        cpp = cpp.replace(old, new)
    return cpp


def build(l2_build_proof, output):
    proof_path, output = Path(l2_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and previous['original_objects'] == 577
    assert previous['generator_sha256'] == digest(Path(__file__).with_name('tea_dip_gsim_l2_return_probe.py'))
    parent, root = proof_path.parent, Path(previous['root'])
    assert not output.exists() and digest(parent/'probe.cpp') == previous['probe_cpp_sha256']
    for name, value in previous['original_sha256'].items():
        assert digest(Path(name)) == value, name
    symbols = subprocess.check_output(['nm','--defined-only',str(root/'build/gsim-compile/emu')],text=True)
    for symbol in WRAPPED:
        assert any(line.split()[-1:] == [symbol] for line in symbols.splitlines()), symbol
    library, = [Path(name) for name in previous['original_sha256'] if Path(name).name == 'libdramsim3.a']
    dram = library.parent.parent
    source = dram/'src'
    common, controller, ram = source/'common.h', source/'controller.cc', root/'difftest/src/test/csrc/common/ram.cpp'
    assert 'channel_state_.UpdateTimingAndStates(cmd, clk_);' in controller.read_text()
    assert 'auto meta = new dramsim3_meta;' in ram.read_text()
    obj = dram/'build/CMakeFiles/dramsim3.dir/src/controller.cc.o'
    assert COMMAND_SYMBOL in subprocess.check_output(['nm','-u',str(obj)],text=True)
    model = root/'build/gsim-compile/model'
    target = 'cpu__DOT__memory__DOT__ram__DOT___read_resp_addr_T_5 ='
    paths = subprocess.check_output(['rg','-l','-F',target,str(model),'--glob','*.cpp'],text=True).splitlines()
    generated = '\n'.join(Path(name).read_text() for name in paths)
    assert target + ' (cpu__DOT__memory__DOT__ram__DOT__read_resp_id & 0x3f);' in generated
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(add_capture((parent/'probe.cpp').read_text(), (root/'build/gsim-compile/model/SimTop.h').read_text()))
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent),str(output)) for arg in command] for command in previous['commands']]
    commands[0].insert(1,'-I'+str(source))
    commands[1].extend('-Wl,--wrap='+symbol for symbol in WRAPPED)
    for i, command in enumerate(commands):
        with (output/f'build-{i}.stdout').open('wb')as out,(output/f'build-{i}.stderr').open('wb')as err:
            subprocess.run(command,stdout=out,stderr=err,check=True)
    for name, value in previous['original_sha256'].items():
        assert digest(Path(name)) == value, name
    result = dict(previous,binary=str(output/'emu'),binary_sha256=digest(output/'emu'),commands=commands,
                  probe_cpp_sha256=digest(output/'probe.cpp'),generator_sha256=digest(Path(__file__)),
                  base_L2_build_proof_sha256=digest(proof_path),memory_return_binding=dict(
                      wrapped_symbols=WRAPPED,original_DRAM_library=str(library),
                      source_sha256={str(p):digest(p)for p in [common,controller,ram,obj]},
                      command_call_is_external=True,model_objects=577,new_RTL_state_bits=0,
                      read_tracker_full_id_and_address_asserted=True,read_tracker_index_mask=63,
                      generated_index_source_sha256={name:digest(Path(name)) for name in paths},
                      scope='Original memory API returns and DRAM clock/order preserved. Model-step API events use the actual same-step core callback; outside-step DRAM commands use the latest confirmed core. Command controller IDs are observation identities, not guessed physical channels. Closure, clock alignment and execution neutrality still need real calibration.'))
    (output/'build-proof.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--l2-build-proof',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    result=build(args.l2_build_proof,args.output)
    print(json.dumps({k:result[k]for k in ['passed','binary','binary_sha256','original_objects']}))


if __name__=='__main__':
    main()
