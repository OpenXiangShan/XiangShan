"""Exercise collection finishing before its prerequisite observer archive."""
import json
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import tea_dip_bpu_clock_follow as follow


class ClockFollowTest(unittest.TestCase):
    def run_follow(self, prior_ready):
        with TemporaryDirectory() as directory:
            root = Path(directory)
            launch, control, prior, output, audit, state = [root / name for name in
                ['launch.json', 'control', 'prior', 'output', 'audit.json', 'state']]
            launch.write_text('{}')
            prior.mkdir()
            prior_receipt = prior / 'closure-receipt.json'
            if prior_ready:
                prior_receipt.write_text('{}')
            args = ['follow', '--launch', str(launch), '--control-dir', str(control),
                    '--prior-observer-dir', str(prior), '--output-dir', str(output),
                    '--audit', str(audit), '--state-dir', str(state), '--retired-edges', '100']

            def collect(*_args, **_kwargs):
                output.mkdir(exist_ok=True)
                (output / 'closure-receipt.json').write_text('{}')
                return SimpleNamespace(returncode=0, stdout='{"ready":true}', stderr='')

            def analyze(*_args):
                self.assertTrue(prior_receipt.exists(), 'audit ran before the prerequisite archive closed')
                return dict(passed=True)

            def wait(_seconds):
                self.assertEqual(json.loads((state / 'state.json').read_text())['phase'],
                                 'waiting_prior_observer')
                prior_receipt.write_text('{}')

            with patch.object(follow.sys, 'argv', args), \
                    patch.object(follow.subprocess, 'run', side_effect=collect) as collector, \
                    patch.object(follow, 'analyze', side_effect=analyze) as analyzer, \
                    patch.object(follow.time, 'sleep', side_effect=wait) as sleeper:
                follow.main()
            self.assertEqual(analyzer.call_count, 1)
            self.assertEqual(collector.call_count, 1 if prior_ready else 2)
            self.assertEqual(sleeper.call_count, 0 if prior_ready else 1)
            self.assertTrue(json.loads(audit.read_text())['passed'])
            self.assertEqual(json.loads((state / 'state.json').read_text())['phase'], 'complete')

    def test_collection_closes_before_prior(self):
        self.run_follow(prior_ready=False)

    def test_both_archives_ready(self):
        self.run_follow(prior_ready=True)


if __name__ == '__main__':
    unittest.main()
