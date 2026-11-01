#!/usr/bin/env python3
"""Split recovery constraints and independent finite lifecycle accounting."""
from pathlib import Path
import importlib.util
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
spec = importlib.util.spec_from_file_location('verify_recovery_overlap', ROOT / 'scripts/verify_regression.py')
verifier = importlib.util.module_from_spec(spec)
spec.loader.exec_module(verifier)


def csv(values):
    return ','.join(map(str, values))


CPP = r'''
#include "scenarios/shared.hpp"
#include <cassert>
int main() {
    for (auto profile : {"spec", "coverage", "corner", "cmo-stress"}) {
        auto c = memblock::scenarios::RandomConstraints::preset(profile);
        assert(c.stream_unresolved_overlap_per_mille == 250);
        for (auto rate : {0U, 1U, 250U, 1000U}) {
            c.apply("stream-unresolved-overlap=" + std::to_string(rate));
            c.validate();
            assert(c.summary().find("target_stream_unresolved_overlap=" + std::to_string(rate) + " ") != std::string::npos);
        }
        c.apply("stream-unresolved-overlap=1001");
        bool failed = false;
        try { c.validate(); } catch (const std::invalid_argument &) { failed = true; }
        assert(failed);
    }
}
'''


class StreamRecoveryOverlapTest(unittest.TestCase):
    def record(self):
        cold, recovery = [0] * 11, [0] * 20
        cold[2], recovery[8] = 2, 4
        return dict(constraint_schema=88, target_stream_unresolved_overlap=1000,
                    target_stream_unresolved_inflight=1000, target_stream_unresolved=1000,
                    stream_recovery_overlap=csv([2, 4, 4, 4, 2, 1, 1, 1, 2, 1, 2, 1]),
                    stream_recovery_inflight=csv(cold), stream_recovery=csv(recovery))

    def test_constraints_presets_endpoints_and_serialization(self):
        with tempfile.TemporaryDirectory(prefix='memblock-recovery-overlap-') as directory:
            source = Path(directory) / 'test.cpp'
            source.write_text(CPP)
            binary = source.with_suffix('')
            result = subprocess.run(['c++', '-std=c++20', '-O2', '-I', str(ROOT / 'cpp'),
                                     str(source), '-o', str(binary)], capture_output=True, text=True, timeout=60)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=10)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_lifecycle_phase_corruptions_missing_fields_disabled_and_legacy(self):
        check, record = verifier._check_stream_recovery_overlap, self.record()
        check(record)
        for index in range(12):
            values = list(map(int, record['stream_recovery_overlap'].split(',')))
            values[index] += 10
            with self.subTest(bin=index), self.assertRaises(verifier.VerificationError):
                check(dict(record, stream_recovery_overlap=csv(values)))
        for key in ('target_stream_unresolved_overlap', 'stream_recovery_overlap',
                    'stream_recovery_inflight', 'stream_recovery'):
            missing = dict(record)
            del missing[key]
            with self.subTest(missing=key), self.assertRaises(verifier.VerificationError):
                check(missing)
        for rate in (-1, 1001, True, '250'):
            with self.subTest(rate=rate), self.assertRaises(verifier.VerificationError):
                check(dict(record, target_stream_unresolved_overlap=rate))
        for key in ('target_stream_unresolved_overlap', 'target_stream_unresolved_inflight', 'target_stream_unresolved'):
            with self.subTest(disabled=key), self.assertRaises(verifier.VerificationError):
                check(dict(record, **{key: 0}))
        for values in ([0] * 11, [-1] + [0] * 11, [1 << 64] + [0] * 11):
            with self.subTest(values=values), self.assertRaises(verifier.VerificationError):
                check(dict(record, stream_recovery_overlap=csv(values)))
        check(dict(record, stream_recovery_overlap=csv([0] * 12)))
        check(dict(record, stream_recovery_overlap=csv([0] * 12), target_stream_unresolved_overlap=0))
        check(dict(constraint_schema=87))

    def test_uint64_boundary_is_valid_without_arithmetic_overflow(self):
        maximum = (1 << 64) - 1
        cold, recovery = [0] * 11, [0] * 20
        cold[2] = recovery[8] = maximum
        verifier._check_stream_recovery_overlap(dict(self.record(), stream_recovery_inflight=csv(cold),
            stream_recovery=csv(recovery), stream_recovery_overlap=csv([maximum] * 4 + [0] * 8)))


if __name__ == '__main__':
    unittest.main()
