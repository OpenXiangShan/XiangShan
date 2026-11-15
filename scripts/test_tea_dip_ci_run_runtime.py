"""Transport failures must not erase other nodes or fabricate empty observations."""
import subprocess
import unittest
from tea_dip_ci_run_runtime import observe_hosts


class PartialObservationTests(unittest.TestCase):
    def test_timeout_preserves_healthy_nodes(self):
        def observe(host):
            if host == 'unreachable':
                raise subprocess.TimeoutExpired(['ssh', host], 50)
            return dict(host=host, processes=[dict(pid=1)])
        data, errors = observe_hosts(['worker', 'unreachable', 'runner'], observe)
        self.assertEqual([row['host'] if row else None for row in data], ['worker', None, 'runner'])
        self.assertEqual([row['host'] for row in errors], ['unreachable'])
        self.assertEqual(data[0]['processes'], [dict(pid=1)])

    def test_all_success_preserves_order(self):
        data, errors = observe_hosts(['worker', 'runner'], lambda host: dict(host=host))
        self.assertEqual(data, [dict(host='worker'), dict(host='runner')])
        self.assertEqual(errors, [])

    def test_failed_hosts_remain_unknown(self):
        def observe(host):
            raise subprocess.CalledProcessError(255, ['ssh', host])
        data, errors = observe_hosts(['worker', 'runner'], observe)
        self.assertEqual(data, [None, None])
        self.assertEqual([row['host'] for row in errors], ['worker', 'runner'])

    def test_semantic_failure_is_not_hidden(self):
        def observe(host):
            raise AssertionError('Wrong source configuration')
        with self.assertRaises(AssertionError):
            observe_hosts(['worker'], observe)


if __name__ == '__main__':
    unittest.main()
