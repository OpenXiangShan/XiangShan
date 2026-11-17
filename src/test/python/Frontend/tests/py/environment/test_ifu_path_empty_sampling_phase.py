"""WaitLastCommit uses live pins at a settled clock phase, never a stale snapshot."""
from types import SimpleNamespace
import pytest
from tests.py.jiabowen import test_ifu_owner_path_transition_v3_dut as case

class _Environment:
 def __init__(self):
  self.falling,self.post=[],[]; self.current_cycle=0
  self.dut=SimpleNamespace(reset=SimpleNamespace(value=0),io_backend_backendEmpty=SimpleNamespace(value=0),StepFal=self.falling.append)
  self.signals={'uncacheUnit.uncacheState':1,'isFirstInstr':0,'uncacheUnit.io_emptyAfter':0,'io_ibufferEmpty':1,'io_toIBuffer_valid':0}
  self.functional_coverage=SimpleNamespace(_lookup_dut_signal=self.lookup,_try_read_dut_signal=self.stale)
 def stale(self,*_args):
  raise AssertionError('settled gate must not reuse rising-edge snapshot')
 def lookup(self,_dut,path):
  prefix='Frontend_top.Frontend.inner_ifu.'
  if not path.startswith(prefix):return None
  value=self.signals.get(path[len(prefix):])
  return None if value is None else SimpleNamespace(value=value)
 def register_cycle_observer(self,callback):self.post.append(callback)
 def fall(self,cycle,backend,empty=None):
  self.dut.io_backend_backendEmpty.value=backend
  self.signals['uncacheUnit.io_emptyAfter']=backend if empty is None else empty
  self.current_cycle=cycle
  for callback in self.falling:callback(cycle)

@pytest.mark.parametrize('backend',[0,1])
def test_gate_reads_actual_settled_pins_without_the_rising_snapshot(backend):
 env=_Environment();_,gates,gate=case._capture_path_evidence(env,[]);env.falling.append(gate)
 env.fall(10,backend)
 assert gates[-1]['backend_empty']==gates[-1]['empty_after']==backend
 assert gates[-1]['sample_phase']=='post_step'
 assert all('__Vtogcov__' not in path for path in gates[-1]['signal_paths'].values())

@pytest.mark.parametrize('backend',[0,1])
def test_stale_optimized_child_port_is_not_used_as_live_gate(backend):
 env=_Environment();_,gates,gate=case._capture_path_evidence(env,[]);env.falling.append(gate)
 env.fall(10,backend,1-backend)
 assert gates[-1]['empty_after']==backend
 assert gates[-1]['empty_after_source']=='backend_and_ibuffer'


@pytest.mark.parametrize('release,allowed',[(0,False),(1,True)])
def test_transition_requires_observed_prior_settled_release(release,allowed):
 env=_Environment();_,gates,gate=case._capture_path_evidence(env,[]);env.falling.append(gate)
 env.fall(10,release);env.signals['uncacheUnit.uncacheState']=2
 if allowed:
  env.fall(11,release);assert gates[-1]['wait_last_commit_released'] is True
 else:
  with pytest.raises(AssertionError):env.fall(11,release)

def test_missing_gate_pin_remains_unavailable():
 env=_Environment();_,_,gate=case._capture_path_evidence(env,[]);env.falling.append(gate)
 del env.signals['io_ibufferEmpty']
 with pytest.raises(AssertionError):env.falling[0](10)

def test_reset_cannot_join_old_wait_state_to_new_request():
 env=_Environment();_,gates,gate=case._capture_path_evidence(env,[]);env.falling.append(gate)
 env.fall(10,0);env.dut.reset.value=1;env.fall(11,0)
 env.dut.reset.value=0;env.signals['uncacheUnit.uncacheState']=2;env.fall(12,0)
 assert 'wait_last_commit_released' not in gates[-1]


def test_post_step_observer_runs_after_simulation_step():
 events=[]
 class Env:
  current_cycle=0
  dut=SimpleNamespace(RefreshComb=lambda:events.append(("refresh",)))
  def step(self,cycles):
   events.append(("step",cycles))
   self.current_cycle+=cycles
 env=Env()
 observed=case._PostStepObserverEnv(env,lambda cycle:events.append(("observe",cycle)))
 observed.step(1)
 assert events==[("step",1),("refresh",),("observe",1)]
 with pytest.raises(ValueError):observed.step(2)


def test_current_design_and_generated_rtl_bind_empty_gate_to_both_inputs():
 import re
 from pathlib import Path
 root=Path(__file__).resolve().parents[7]
 scala=(root/'src/main/scala/xiangshan/frontend/ifu/Ifu.scala').read_text()
 rtl=(root/'build-frontend/rtl/Ifu.sv').read_text()
 assert re.search(r'uncacheUnit\.io\.emptyAfter\s*:=\s*io\.backendEmpty\s*&&\s*io\.ibufferEmpty',scala)
 assert re.search(r'\.io_emptyAfter\s*\(\s*io_backendEmpty\s*&\s*io_ibufferEmpty\s*\)',rtl)
