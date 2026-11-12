"""Check completion provenance, retries and pending-file exclusion."""
import copy
import unittest
from unittest.mock import patch

from tea_dip_ci_runner_closure import PROTOCOL, closed_receipt, runner_completion


class RunnerCompletionTest(unittest.TestCase):
    def setUp(self):
        self.point = 'mcf_12886_0.149435'
        self.identity = dict(run_id=43120494741, source='712abd35e823ea81d92161521f69cf5258526cd6', protocol_sha256=PROTOCOL)
        self.run = dict(id=self.identity['run_id'], head_sha=self.identity['source'], run_attempt=1, status='in_progress', conclusion=None)
        self.job = dict(run_id=self.run['id'], head_sha=self.run['head_sha'], run_attempt=1, status='in_progress', conclusion=None)
        self.log = ('2026-11-12 09:00:58,011 -  INFO - [1,0,0/145] Started mcf_12886_0.149435 on node039\n'
                    '2026-11-13 00:38:59,476 -  INFO - [145,143,0/145] mcf_12886_0.149435 succeeded \ton node039 \tin 15h 38m 01s\n')

    def assess(self, log=None):
        return runner_completion(self.identity, self.run, self.job, self.point, self.log if log is None else log)

    def test_completed_point_does_not_invent_pid_or_whole_CI_success(self):
        receipt = self.assess()
        self.assertTrue(receipt['ready'])
        self.assertFalse(receipt['original_process_observed'])
        self.assertNotIn('original_pid', receipt)
        self.assertNotIn('original_start_ticks', receipt)
        self.assertEqual(receipt['run_status_at_collection'], 'in_progress')

    def test_new_attempt_cannot_reuse_an_old_success(self):
        self.assertFalse(self.assess(self.log + self.log.splitlines()[0] + '\n')['ready'])

    def test_pending_point_never_touches_guest_files(self):
        with patch('tea_dip_ci_runner_closure.digest', side_effect=AssertionError('guest file read')):
            receipt = closed_receipt(self.identity, self.run, self.job, 'mcf_6753_0.218742', self.log)
        self.assertFalse(receipt['ready'])
        self.assertFalse(receipt['database_read'])

    def test_cancelled_job_cannot_supply_a_success_receipt(self):
        self.job.update(status='completed', conclusion='cancelled')
        self.assertFalse(self.assess()['ready'])

    def test_unknown_protocol_is_rejected(self):
        self.identity['protocol_sha256'] = copy.deepcopy(PROTOCOL)
        self.identity['protocol_sha256']['modules/server.py'] = 'unknown'
        with self.assertRaises(AssertionError):
            self.assess()

    def test_source_mismatch_is_rejected(self):
        self.job['head_sha'] = 'different'
        with self.assertRaises(AssertionError):
            self.assess()


if __name__ == '__main__':
    unittest.main()
