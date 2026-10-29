#!/usr/bin/env python3
"""Check instruction-wide frontend metadata without changing unrelated RNG draws.

Ordinary tests compile the complete production vector builder. Segment tests
compile its actual FTQ assignment block; they do not execute the segment action,
its ports, or its translation/decomposition paths.
"""
from pathlib import Path
import hashlib
import json
import os
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


def method(text, signature):
    begin = text.index(signature)
    return text[begin:text.index('\n}\n', begin) + 3]


class VectorFrontendIdentityTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        parent = os.environ.get('MEMBLOCK_VECTOR_FRONTEND_TEST_OUT')
        if parent:
            Path(parent).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix='host-', dir=parent))
        else:
            cls.temporary = tempfile.TemporaryDirectory(prefix='memblock-vector-frontend-')
            cls.addClassCleanup(cls.temporary.cleanup)
            cls.work = Path(cls.temporary.name)
        cls.paths = [ROOT/'cpp/scenarios/mixed_vectors.cpp', ROOT/'cpp/scenarios/mixed_action_vector.cpp',
                     ROOT/'unit/vector_frontend_identity_fixture.cpp', Path(__file__).resolve()]
        cls.before = {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in cls.paths}
        ordinary = cls.paths[0].read_text()
        methods = [method(ordinary, signature) for signature in (
            'unsigned int MixedScenario::vector_shape_uop_count(',
            'unsigned int MixedScenario::vector_shape_flow_num(',
            'bool MixedScenario::choose_binary_class(',
            'std::vector<memblock::VectorMemoryTransaction> MixedScenario::make_random_vector_uops(')]
        cls.coupling = ('        transaction.ftq_ptr = uops.front().ftq_ptr;\n'
                        '        transaction.ftq_offset = uops.front().ftq_offset;\n')
        if methods[-1].count(cls.coupling) != 1:
            raise AssertionError('ordinary frontend coupling anchor changed')
        former = methods[-1].replace('MixedScenario::make_random_vector_uops(', 'MixedScenario::former_vector_uops(', 1)
        former = former.replace(cls.coupling, '', 1)
        segment = cls.paths[1].read_text()
        begin = segment.index('            segment_uop.ftq_ptr = random()')
        end = segment.index('            for (auto &byte : segment_uop.data)', begin)
        segment = segment[begin:end]
        coupling = ('            segment_uop.ftq_ptr = uops.front().ftq_ptr;\n'
                    '            segment_uop.ftq_offset = uops.front().ftq_offset;\n')
        if segment.count(coupling) != 1:
            raise AssertionError('segment frontend coupling anchor changed')
        source = cls.paths[2].read_text().replace('// PRODUCTION_BUILDERS', '\n'.join(methods + [former]))
        source = source.replace('// PRODUCTION_SEGMENT_METADATA', segment)
        source = source.replace('// FORMER_SEGMENT_METADATA', segment.replace(coupling, '', 1))
        cpp = cls.work/'fixture.cpp'
        cpp.write_text(source)
        cls.binary = cls.work/'fixture'
        cls.command = [os.environ.get('CXX', 'c++'), '-std=c++20', '-O0', '-I', str(ROOT/'cpp'),
                       str(cpp), '-o', str(cls.binary)]
        result = subprocess.run(cls.command, text=True, capture_output=True, timeout=60)
        (cls.work/'compile.log').write_text(result.stdout+result.stderr)
        if result.returncode:
            raise RuntimeError(result.stdout+result.stderr)
        cls.results = []

    @classmethod
    def tearDownClass(cls):
        after = {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in cls.paths}
        (cls.work/'summary.json').write_text(json.dumps(dict(source_hashes=cls.before,
            source_hashes_after=after, compile_command=cls.command, results=cls.results,
            scope='whole ordinary builder; segment FTQ assignment block; no DUT'), indent=2)+'\n')
        if after != cls.before:
            raise AssertionError('frontend identity inputs changed during tests')

    def run_case(self, mode):
        result = subprocess.run([str(self.binary), mode], text=True, capture_output=True, timeout=30)
        self.results.append(dict(mode=mode, returncode=result.returncode, stdout=result.stdout, stderr=result.stderr))
        return result

    def test_ordinary_siblings_share_frontend_identity_with_preserved_randomness(self):
        result = self.run_case('ordinary')
        self.assertEqual(result.returncode, 0, result.stdout+result.stderr)
        self.assertIn('instructions=2496', result.stdout)

    def test_segment_siblings_share_identity_and_preserve_rng_consumption(self):
        result = self.run_case('segment')
        self.assertEqual(result.returncode, 0, result.stdout+result.stderr)
        self.assertIn('instructions=640', result.stdout)

    def test_former_per_uop_frontend_draws_violate_instruction_identity(self):
        for mode in ('former-ordinary', 'former-segment'):
            with self.subTest(mode=mode):
                result = self.run_case(mode)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn('same-instruction uops have different frontend identities', result.stderr)


if __name__ == '__main__':
    unittest.main()
