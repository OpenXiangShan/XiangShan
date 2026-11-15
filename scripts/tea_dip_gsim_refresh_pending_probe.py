"""Observe real DRAM refresh enqueue and preparation calls without changing the model."""
import argparse
import json
from pathlib import Path
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest

CLOCK = '_ZN8dramsim310Controller9ClockTickEv'
ENQUEUE = '_ZN8dramsim312ChannelState15RankNeedRefreshEib'
FINISH = '_ZN8dramsim312CommandQueue13FinishRefreshEv'
FIELDS = ['core_cycle', 'step', 'clean', 'native_dump', 'event', 'controller',
          'command_controller', 'dram_cycle', 'rank', 'pending', 'front_rank',
          'command', 'valid', 'bankgroup', 'bank', 'need']


def add_capture(cpp):
    helper = r'''
#include "channel_state.h"
struct RefreshContext {
  unsigned id; uint64_t cycle;
  dramsim3::ChannelState *state;
  void *queue;
  int command_controller;
};
static std::map<void *,RefreshContext> refresh_contexts;
static RefreshContext *refresh_active=nullptr;
static FILE *refresh_output=nullptr;
static void refresh_write(int64_t core,unsigned clean,unsigned dump,unsigned event,
                          int controller,int command_controller,uint64_t cycle,int rank,
                          bool pending,int front,int command,bool valid,int bg,int bank,int need) {
  if(!refresh_output) {
    refresh_output=fopen("memory-refresh-pending.csv","w");
    if(!refresh_output) {perror("memory-refresh-pending.csv");abort();}
    fprintf(refresh_output,"core_cycle,step,clean,native_dump,event,controller,command_controller,dram_cycle,rank,pending,front_rank,command,valid,bankgroup,bank,need\n");
  }
  fprintf(refresh_output,"%ld,%lu,%u,%u,%u,%d,%d,%lu,%d,%u,%d,%d,%u,%d,%d,%d\n",
          core,memory_step,clean,dump,event,controller,command_controller,cycle,rank,
          unsigned(pending),front,command,unsigned(valid),bg,bank,need);
}
static void refresh_emit(unsigned event,int rank=-1,int need=-1,const dramsim3::Command *cmd=nullptr) {
  assert(refresh_active && !memory_inside);
  const auto &c=*refresh_active;
  const bool pending=c.state && c.state->IsRefreshWaiting();
  refresh_write(memory_have_core?int64_t(memory_last_core)+1:-1,0,0,event,c.id,
                c.command_controller,c.cycle,rank,pending,
                pending?c.state->PendingRefCommand().Rank():-1,
                cmd?int(cmd->cmd_type):-1,cmd && cmd->IsValid(),
                cmd && cmd->IsValid()?cmd->Bankgroup():-1,
                cmd && cmd->IsValid()?cmd->Bank():-1,need);
}
static void refresh_bind(void *state,unsigned controller,const dramsim3::Command &cmd,uint64_t cycle) {
  assert(refresh_active && cycle==refresh_active->cycle);
  auto *typed=static_cast<dramsim3::ChannelState *>(state);
  assert(!refresh_active->state || refresh_active->state==typed);
  assert(refresh_active->command_controller<0 || refresh_active->command_controller==int(controller));
  refresh_active->state=typed; refresh_active->command_controller=int(controller);
  refresh_emit(4,cmd.Rank(),-1,&cmd);
}
extern "C" void __real_CLOCK(void *);
extern "C" void __wrap_CLOCK(void *self) {
  assert(!refresh_active && !memory_inside);
  auto at=refresh_contexts.find(self);
  if(at==refresh_contexts.end()) at=refresh_contexts.emplace(self,RefreshContext{unsigned(refresh_contexts.size()),0,nullptr,nullptr,-1}).first;
  refresh_active=&at->second;
  auto *state=refresh_active->state;
  const int before=state?int(state->IsRefreshWaiting()):-1;
  const int before_rank=state && state->IsRefreshWaiting()?state->PendingRefCommand().Rank():-1;
  __real_CLOCK(self);
  state=refresh_active->state;
  if(state && (before!=int(state->IsRefreshWaiting()) ||
      before_rank!=(state->IsRefreshWaiting()?state->PendingRefCommand().Rank():-1))) refresh_emit(5);
  ++refresh_active->cycle; refresh_active=nullptr;
}
extern "C" void __real_ENQUEUE(void *,int,bool);
extern "C" void __wrap_ENQUEUE(void *self,int rank,bool need) {
  assert(refresh_active && !memory_inside);
  auto *typed=static_cast<dramsim3::ChannelState *>(self);
  assert(!refresh_active->state || refresh_active->state==typed);
  refresh_active->state=typed;
  __real_ENQUEUE(self,rank,need);
  refresh_emit(1,rank,int(need));
}
extern "C" dramsim3::Command __real_FINISH(void *);
extern "C" dramsim3::Command __wrap_FINISH(void *self) {
  assert(refresh_active && refresh_active->state && refresh_active->state->IsRefreshWaiting());
  assert(!refresh_active->queue || refresh_active->queue==self);
  refresh_active->queue=self;
  refresh_emit(2,refresh_active->state->PendingRefCommand().Rank());
  const auto cmd=__real_FINISH(self);
  refresh_emit(3,cmd.IsValid()?cmd.Rank():-1,-1,&cmd);
  return cmd;
}
'''.replace('CLOCK', CLOCK).replace('ENQUEUE', ENQUEUE).replace('FINISH', FINISH)
    old = 'static FILE *memory_output = nullptr;'
    assert cpp.count(old) == 1
    cpp = cpp.replace(old, old + '\n' + helper)
    old = '  memory_emit({3,0,unsigned(cmd.IsWrite()),0,cmd.hex_addr,found->second,unsigned(cmd.cmd_type),cycle,cmd.Channel(),cmd.Rank(),cmd.Bankgroup(),cmd.Bank(),cmd.Row(),cmd.Column(),0});'
    assert cpp.count(old) == 1
    cpp = cpp.replace(old, '  refresh_bind(self,found->second,cmd,cycle);\n' + old)
    old = 'const MemoryEvent boundary{}; memory_write(boundary,context.core,context.clean,context.dump);'
    assert cpp.count(old) == 1
    return cpp.replace(old, old + '\n    refresh_write(context.core,context.clean,context.dump,0,-1,-1,0,-1,false,-1,-1,false,-1,-1,-1);')


def build(memory_build_proof, output):
    proof_path, output = Path(memory_build_proof).resolve(), Path(output).resolve()
    previous = json.loads(proof_path.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and 'memory_return_binding' in previous
    parent = proof_path.parent
    assert not output.exists() and digest(parent/'probe.cpp') == previous['probe_cpp_sha256']
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    dram = Path(previous['memory_return_binding']['original_DRAM_library']).parent.parent
    src = dram/'src'
    controller, queue, state = [(src/name).read_text() for name in ['controller.cc', 'command_queue.cc', 'channel_state.h']]
    assert 'refresh_.ClockTick();' in controller and 'cmd = cmd_queue_.FinishRefresh();' in controller
    assert 'is_in_ref_ = true;' in queue and 'ref_q_indices_.find(queue_idx_)' in queue
    assert 'bool IsRefreshWaiting() const { return !refresh_q_.empty(); }' in state
    assert 'const Command& PendingRefCommand() const {return refresh_q_.front(); }' in state
    refs = {'controller': [FINISH], 'refresh': [ENQUEUE], 'dram_system': [CLOCK]}
    objects = []
    for name, symbols in refs.items():
        p = dram/f'build/CMakeFiles/dramsim3.dir/src/{name}.cc.o'
        undefined = subprocess.check_output(['nm', '-u', str(p)], text=True)
        assert all(symbol in undefined for symbol in symbols)
        objects.append(p)
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(add_capture((parent/'probe.cpp').read_text()))
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in cmd] for cmd in previous['commands']]
    assert '-I'+str(src) in commands[0]
    commands[0].insert(1, '-I'+str(dram/'ext/headers'))
    commands[1].extend('-Wl,--wrap='+symbol for symbol in [CLOCK, ENQUEUE, FINISH])
    for i, command in enumerate(commands):
        with (output/f'build-{i}.stdout').open('wb') as out, (output/f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    binding = dict(fields=FIELDS, wrapped_symbols=[CLOCK, ENQUEUE, FINISH],
                   source_sha256={str(p): digest(p) for p in [src/name for name in [
                       'controller.cc', 'command_queue.cc', 'channel_state.h', 'channel_state.cc', 'common.h']] + objects},
                   actual_public_pending_state=True, controller_cycle_command_asserted=True,
                   no_private_layout_access=True, new_RTL_state_bits=0,
                   scope='External enqueue calls, actual FinishRefresh enter/return and issued commands within the same Controller ClockTick. '
                         'RankNeedRefresh(false) may be local to channel_state.o; removal is observed via actual pending state after ClockTick. '
                         'Outside-step core labels use lastconfirmed+1; native boundaries use context.core. '
                         'Private is_in_ref is not directly read, and no counterfactual saving is inferred.')
    result = dict(previous, binary=str(output/'emu'), binary_sha256=digest(output/'emu'), commands=commands,
                  probe_cpp_sha256=digest(output/'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  parent_memory_build_proof_sha256=digest(proof_path), refresh_pending_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result, indent=2)+'\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--memory-build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = build(args.memory_build_proof, args.output)
    print(json.dumps({key: result[key] for key in ['passed', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
