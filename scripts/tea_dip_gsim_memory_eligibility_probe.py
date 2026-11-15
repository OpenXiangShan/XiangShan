"""Record actual command admission and ready queries for watched memory transactions."""
import argparse
import json
from pathlib import Path
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest

ADD = '_ZN8dramsim312CommandQueue10AddCommandENS_7CommandE'
READY = '_ZNK8dramsim312ChannelState15GetReadyCommandERKNS_7CommandEm'
FIELDS = ['watch', 'event', 'core_cycle', 'step', 'controller', 'dram_cycle',
          'address', 'input_command', 'valid', 'output_command', 'rank',
          'bankgroup', 'bank', 'row', 'column', 'pending', 'front_rank']


def add_capture(cpp, watches):
    entries = ',\n'.join('{%dULL,%d,%d,%d,%d}' % tuple(w[k] for k in [
        'address', 'begin_core', 'end_core', 'controller', 'rank']) for w in watches)
    helper = r'''
struct EligibilityWatch { uint64_t address; int64_t begin,end; int controller,rank; };
static const EligibilityWatch eligibility_watches[]={ENTRIES};
static FILE *eligibility_output=nullptr;
static void eligibility_emit(unsigned event,const dramsim3::Command &input,
                             bool valid,const dramsim3::Command *output) {
  assert(refresh_active && !memory_inside);
  if(!memory_have_core) return;
  const auto &c=*refresh_active;
  const int64_t core=int64_t(memory_last_core)+1;
  for(unsigned i=0;i<sizeof(eligibility_watches)/sizeof(*eligibility_watches);++i) {
    const auto &w=eligibility_watches[i];
    if(input.hex_addr!=w.address || core<w.begin || core>w.end) continue;
    assert(c.command_controller==w.controller && input.Rank()==w.rank);
    if(!eligibility_output) {
      eligibility_output=fopen("memory-command-eligibility.csv","w");
      if(!eligibility_output) {perror("memory-command-eligibility.csv");abort();}
      fprintf(eligibility_output,"HEADER\n");
    }
    const bool pending=c.state->IsRefreshWaiting();
    fprintf(eligibility_output,"%u,%u,%ld,%lu,%d,%lu,%lu,%d,%u,%d,%d,%d,%d,%d,%d,%u,%d\n",
            i,event,core,memory_step,c.command_controller,c.cycle,input.hex_addr,
            int(input.cmd_type),unsigned(valid),output?int(output->cmd_type):-1,
            input.Rank(),input.Bankgroup(),input.Bank(),input.Row(),input.Column(),
            unsigned(pending),pending?c.state->PendingRefCommand().Rank():-1);
  }
}
extern "C" bool __real_ADD(void *,dramsim3::Command);
extern "C" bool __wrap_ADD(void *self,dramsim3::Command cmd) {
  const bool accepted=__real_ADD(self,cmd);
  eligibility_emit(1,cmd,accepted,accepted?&cmd:nullptr);
  return accepted;
}
extern "C" dramsim3::Command __real_READY(const void *,const dramsim3::Command &,uint64_t);
extern "C" dramsim3::Command __wrap_READY(const void *self,const dramsim3::Command &cmd,uint64_t cycle) {
  assert(refresh_active && cycle==refresh_active->cycle);
  auto *state=const_cast<dramsim3::ChannelState *>(static_cast<const dramsim3::ChannelState *>(self));
  assert(!refresh_active->state || state==refresh_active->state);
  refresh_active->state=state;
  const auto result=__real_READY(self,cmd,cycle);
  eligibility_emit(2,cmd,result.IsValid(),result.IsValid()?&result:nullptr);
  return result;
}
'''.replace('ENTRIES', entries).replace('HEADER', ','.join(FIELDS)).replace('ADD', ADD).replace('READY', READY)
    # The inherited refresh helper needs all original memory globals before use.
    marker = 'extern "C" void __real_' + '_ZN8dramsim312ChannelState21UpdateTimingAndStatesERKNS_7CommandEm'
    at = cpp.index(marker)
    return cpp[:at] + helper + '\n' + cpp[at:]


def build(refresh_build_proof, watchlist, output):
    proof_path, watchlist, output = map(lambda p: Path(p).resolve(), [refresh_build_proof, watchlist, output])
    previous = json.loads(proof_path.read_text())
    watches = json.loads(watchlist.read_text())
    assert previous['passed'] and previous['original_files_unchanged'] and previous['refresh_pending_binding']['actual_public_pending_state']
    assert watches and all(w['begin_core'] <= w['end_core'] for w in watches)
    parent = proof_path.parent
    assert not output.exists() and digest(parent/'probe.cpp') == previous['probe_cpp_sha256']
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    dram = Path(previous['memory_return_binding']['original_DRAM_library']).parent.parent
    sources = [dram/'src'/n for n in ['controller.cc', 'command_queue.cc', 'channel_state.cc', 'channel_state.h']]
    refs = [('controller', ADD), ('command_queue', READY)]
    objects = []
    for name, symbol in refs:
        p = dram/f'build/CMakeFiles/dramsim3.dir/src/{name}.cc.o'
        assert symbol in subprocess.check_output(['nm', '-u', str(p)], text=True)
        objects.append(p)
    cpp = add_capture((parent/'probe.cpp').read_text(), watches)
    output.mkdir(parents=True)
    (output/'probe.cpp').write_text(cpp)
    (output/'objects.rsp').write_bytes((parent/'objects.rsp').read_bytes())
    commands = [[arg.replace(str(parent), str(output)) for arg in cmd] for cmd in previous['commands']]
    commands[1].extend('-Wl,--wrap='+symbol for symbol in [ADD, READY])
    for i, command in enumerate(commands):
        with (output/f'build-{i}.stdout').open('wb') as out, (output/f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, expected in previous['original_sha256'].items():
        assert digest(Path(name)) == expected, name
    binding = dict(fields=FIELDS, watches=watches, wrapped_symbols=[ADD, READY], watchlist_sha256=digest(watchlist),
                   source_sha256={str(p): digest(p) for p in sources+objects}, new_RTL_state_bits=0,
                   scope='Actual AddCommand returns and external GetReadyCommand queries for address/time/controller/rank watches. '
                         'No private layout, extra model queries or changed parameters/returns. Valid readiness does not prove arbitration grant; '
                         'unqueried commands are not classified as invalid. Windows are selected observations, not a global population.')
    result = dict(previous, binary=str(output/'emu'), binary_sha256=digest(output/'emu'), commands=commands,
                  probe_cpp_sha256=digest(output/'probe.cpp'), generator_sha256=digest(Path(__file__)),
                  parent_refresh_build_proof_sha256=digest(proof_path), memory_eligibility_binding=binding)
    (output/'build-proof.json').write_text(json.dumps(result, indent=2)+'\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--refresh-build-proof', type=Path, required=True)
    parser.add_argument('--watchlist', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = build(args.refresh_build_proof, args.watchlist, args.output)
    print(json.dumps({key: result[key] for key in ['passed', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
