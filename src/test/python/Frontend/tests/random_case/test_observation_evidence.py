"""Pure Python: no emulator, DUT factory, stepping, compiler or input generation.
Set AUDIT_COORDINATOR_SOURCE only to review the ignored candidate in isolation.
After integration the same tests import the production module normally.
"""
import importlib.util
import os
from pathlib import Path
import sys
from types import SimpleNamespace as NS
import unittest
from unittest.mock import patch

RANDOM = next(p for p in Path(__file__).resolve().parents if (p / "src/test/python/Frontend/env/core/frontend_env.py").is_file())
FRONTEND = RANDOM / 'src/test/python/Frontend'
sys.path.insert(0, str(FRONTEND))
if os.getenv('AUDIT_COORDINATOR_SOURCE'):
    spec = importlib.util.spec_from_file_location('tests.random_case._review_observation_evidence', os.environ['AUDIT_COORDINATOR_SOURCE'])
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
else:
    from tests.random_case import scenario_coordinator as module

class ObservationEvidence(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Literal frozen control timing fixture; no seed generation or external input dependency.
        cls.timing = module.TimingPlan.from_dict({'backpressure_durations': [4, 7, 2, 7, 4, 7, 7, 4, 5, 3, 4, 3, 3, 4], 'icache_hit_latency': 2, 'icache_miss_latency': 32, 'icache_miss_rate': 0.04, 'icache_seed': 6033811949015794341, 'maximum_backpressure_windows': 14, 'periodic_response_stride': 21, 'policy': {'event_triggered': True, 'internal_dut_force': False, 'random_draws_resolved_before_run': True, 'trigger_source': 'observed_top_level_handshake'}, 'profile': 'default', 'relation_window_cycles': 2, 'schema_version': 1, 'timing_seed': 17566988105840682634, 'warmup_response_lines': 4})

    def coordinator(self):
        return module.ScenarioCoordinator(scenario_id='unit-only-observation', timing_plan=self.timing,
            program_map={'binary_text_verified':True,'binary_sha256':'unit-only','instructions':[]})

    def env(self, value=0):
        writes = []
        return NS(clock_reset=NS(reset=NS(value=0)),
            backend_to_ibuf_if=NS(decode_can_accept=NS(value=value)),
            backend_to_ftq_if=NS(redirect_valid=NS(value=0)),
            backend_model=NS(set_can_accept=writes.append), writes=writes)

    def test_missing_allowenq_is_unknown(self):
        with patch.object(module,'read_internal_signal',return_value=None):
            self.assertIsNone(module._read_ibuffer_full(NS(dut=object())))
    def test_missing_internal_reader_is_unknown(self):
        with patch.object(module,'read_internal_signal',None):
            self.assertIsNone(module._read_ibuffer_full(NS(dut=object())))
    def test_missing_wrapper_is_unknown(self):
        self.assertIsNone(module._read_ibuffer_full(NS()))
    def test_allowenq_one_is_measured_not_full(self):
        with patch.object(module,'read_internal_signal',return_value=1):
            self.assertEqual(module._read_ibuffer_full(NS(dut=object())),0)
    def test_allowenq_zero_is_measured_full(self):
        with patch.object(module,'read_internal_signal',return_value=0):
            self.assertEqual(module._read_ibuffer_full(NS(dut=object())),1)
    def test_invalid_internal_value_is_unknown(self):
        with patch.object(module,'read_internal_signal',return_value=2):
            self.assertIsNone(module._read_ibuffer_full(NS(dut=object())))
    def test_optional_wrapper_must_be_readable_binary(self):
        for value in (None,2,'bad'):
            env=NS(frontend_info_if=NS(io_frontendInfo_ibufFull=NS(value=value)))
            self.assertIsNone(module._read_ibuffer_full(env))
    def test_unreadable_signal_is_unknown(self):
        class Broken:
            @property
            def value(self):raise RuntimeError('unreadable pin')
        self.assertIsNone(module._read_optional_binary(Broken()))
    def test_missing_probe_does_not_emit_zero_or_full_relation(self):
        c=self.coordinator();c.observe_cycle(10,self.env())
        self.assertFalse(any(x['event']=='dut.ibuffer_full' for x in c._observations))
        self.assertNotIn('dut.ibuffer_full_x_backend_backpressure',c._realized_counts)
        event=next(x for x in c._timeline if x['type']=='audit.ibuffer_full_probe')
        self.assertEqual(event['payload'],{'available':False,'value':None})
    def test_probe_loss_and_return_are_explicit(self):
        c=self.coordinator();env=self.env()
        with patch.object(module,'_read_ibuffer_full',side_effect=[0,None,0]):
            for cycle in (10,11,12):c.observe_cycle(cycle,env)
        availability=[x['payload']['available'] for x in c._timeline if x['type']=='audit.ibuffer_full_probe']
        self.assertEqual(availability,[True,False,True])
        self.assertEqual([x['cycle'] for x in c._observations if x['event']=='dut.ibuffer_full'],[10,12])

    def sample(self, cycles, values):
        action={'start_cycle':10,'end_cycle_inclusive':13,'duration_cycles':4,'confirmed_low_cycles':0}
        for cycle,value in zip(cycles,values):module._record_pressure_sample(action,cycle,value)
        return action
    def test_full_consecutive_sample_window_passes(self):
        a=self.sample(range(10,14),[0]*4)
        self.assertTrue(a['pressure_window_confirmed']);self.assertEqual(a['confirmed_low_cycles'],4)
        self.assertEqual(a['observed_low_windows'],[{'start_cycle':10,'end_cycle_inclusive':13,'cycles':4}])
    def test_only_one_low_sample_is_not_a_window(self):
        self.assertFalse(self.sample(range(10,14),[1,0,1,1])['pressure_window_confirmed'])
    def test_gap_in_samples_does_not_infer_missing_low_cycle(self):
        a=self.sample([10,11,13],[0,0,0]);self.assertFalse(a['pressure_window_confirmed'])
        self.assertEqual([w['cycles'] for w in a['observed_low_windows']],[2,1])
    def test_high_cycle_splits_actual_windows(self):
        a=self.sample(range(10,14),[0,1,0,0]);self.assertFalse(a['pressure_window_confirmed'])
        self.assertEqual([w['cycles'] for w in a['observed_low_windows']],[1,2])
    def test_unknown_cycle_splits_actual_windows(self):
        a=self.sample(range(10,14),[0,None,0,0]);self.assertFalse(a['pressure_window_confirmed'])
        self.assertEqual([w['cycles'] for w in a['observed_low_windows']],[1,2])
    def test_duplicate_cycle_cannot_fill_duration(self):
        a=self.sample([10,11,11,13],[0]*4);self.assertFalse(a['pressure_window_confirmed'])
        self.assertEqual(a['confirmed_low_cycles'],2)
    def test_reordered_samples_rejected(self):
        self.assertFalse(self.sample([10,12,11,13],[0]*4)['pressure_window_confirmed'])
    def test_outside_requested_window_cannot_substitute(self):
        self.assertFalse(self.sample([11,12,13,14],[0]*4)['pressure_window_confirmed'])
    def test_wrong_binary_value_cannot_prove_pressure(self):
        self.assertFalse(self.sample(range(10,14),[0,2,0,0])['pressure_window_confirmed'])
    def test_postdrive_phase_is_not_claimed_as_handshake(self):
        self.assertIn('not edge handshake proof',self.sample([10],[0])['pressure_sampling_phase'])

    def drive_action(self, samples):
        c=self.coordinator();env=self.env();c.env=env
        c._start_backpressure(10,{'type':'icache.response','payload':{}})
        action=c._active_action
        self.assertEqual(action['duration_cycles'],4)
        for cycle,value in samples:
            env.backend_to_ibuf_if.decode_can_accept.value=value
            c.observe_cycle(cycle,env)
        return c,env,action
    def test_callback_full_window_is_realized(self):
        c,env,a=self.drive_action([(i,0) for i in range(10,14)])
        self.assertEqual(a['status'],'REALIZED');self.assertIsNone(c._active_action)
        self.assertEqual(env.writes,[0,1]);self.assertEqual(c._realized_counts['dut.backend_backpressure_window'],1)
    def test_callback_partial_low_is_not_realized(self):
        c,env,a=self.drive_action([(10,0),(11,1),(12,1),(13,1)])
        self.assertEqual(a['status'],'NOT_REALIZED');self.assertEqual(a['confirmed_low_cycles'],1)
        self.assertNotIn('dut.backend_backpressure_window',c._realized_counts)
        self.assertIn('not_fully_observed',a['failure_reason']);self.assertEqual(env.writes,[0,1])
    def test_callback_missing_pin_does_not_realize_pressure(self):
        c,env,a=self.drive_action([(i,None) for i in range(10,14)])
        self.assertEqual(a['status'],'NOT_REALIZED');self.assertEqual(a['confirmed_low_cycles'],0)
        self.assertNotIn('dut.backend_backpressure_window',c._realized_counts)
    def test_callback_late_observer_does_not_infer_start(self):
        c,_,a=self.drive_action([(11,0),(12,0),(13,0)])
        self.assertEqual(a['status'],'NOT_REALIZED');self.assertEqual(a['observed_low_windows'][0]['cycles'],3)
    def test_reset_discards_partial_pressure_and_probe_availability(self):
        c,env,a=self.drive_action([(10,0)])
        env.clock_reset.reset.value=1;c.observe_cycle(11,env)
        self.assertEqual(a['status'],'FAIL');self.assertIsNone(c._active_action)
        self.assertIsNone(c._last_ibuffer_probe_available)
        self.assertNotIn('dut.backend_backpressure_window',c._realized_counts)

if __name__=='__main__':unittest.main(verbosity=2)
