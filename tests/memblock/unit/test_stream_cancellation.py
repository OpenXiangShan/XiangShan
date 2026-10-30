#!/usr/bin/env python3
"""Independent generation ledger and compiled cancellation-record contracts."""
import copy
from pathlib import Path
import random
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
import verify_regression as verifier


def csv(values):
    return ','.join(str(int(value)) for value in values)


def history(seed):
    rng = random.Random(seed)
    # A retired prefix, a canceled suffix, and independently generated replacements.
    kinds = [0, 1, 2, 3, 4]
    killed = [rng.randrange(5) for _ in range(rng.randrange(1, 9))]
    counts = [0] * 29
    counts[0] = counts[22] = counts[23] = 1
    counts[22] = len(killed)
    accepted = [0] * 6
    vectors = [0, 0]
    initial, retries, hits, misses = 0, 0, 0, 0
    selected_address, selected_data = 0, 0
    used_address, used_data = 0, 0
    records = []
    for kind in killed:
        counts[1 + kind] += 1
        record = [kind, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        address_edge = bool(rng.getrandbits(1))
        data_edge = kind == 2 and bool(rng.getrandbits(1))
        first_address = False
        if kind in (0, 4):
            issue = bool(rng.getrandbits(1))
            slot = 0 if kind == 0 else 5
            counts[6 + slot] += issue
            accepted[slot] += issue
            record[4] = issue
            first_address = issue
        elif kind in (1, 3):
            direction = kind == 3
            uops = rng.randrange(1, 9)
            issued = rng.randrange(uops + 1)
            restart = bool(rng.getrandbits(1))
            counts[12 + direction] += uops
            counts[9 + direction] += issued
            counts[14 + direction] += restart
            vectors[direction] += uops
            accepted[3 + direction] += issued
            record[1:3] = [uops, issued]
            record[7] = restart
            source_edges = {position for position in range(1, uops) if rng.getrandbits(1)}
            unconsumed = sum(position >= issued for position in source_edges) if kind == 1 else 0
            counts[26 + direction] += len(source_edges)
            counts[28] += unconsumed
            record[11:13] = [len(source_edges), unconsumed]
            first_address = issued != 0
        else:
            state = rng.randrange(4)
            address = state != 0
            retry = rng.randrange(4) if address else 0
            data = bool(rng.getrandbits(1))
            counts[7] += address
            counts[8] += data
            counts[21] += retry
            accepted[1] += address
            accepted[2] += data
            initial += address
            retries += retry
            hits += state == 1
            misses += retry + (state == 2)
            if address:
                counts[{1: 18, 2: 19, 3: 20}[state]] += 1
            record[3] = retry
            record[5:7] = [address, data]
            record[10] = state
            first_address = address
        if address_edge:
            selected_address += 1
            if first_address:
                used_address += 1
            else:
                counts[16] += 1
                record[8] = 1
        if data_edge:
            selected_data += 1
            if record[6]:
                used_data += 1
            else:
                counts[17] += 1
                record[9] = 1
        records.append(record)
    retired = len(kinds)
    admitted = retired + len(killed)
    counts[24] = admitted
    counts[25] = admitted > 32
    operations = [1] * 5
    # Two vector instructions, each with two uops, survive and retire.
    vectors = [n + 2 for n in vectors]
    issues = [accepted[i] + n for i, n in enumerate((1, 1, 1, 2, 2, 1))]
    result = dict(constraint_schema=75, transactions=retired,
        target_concurrent=1000, target_concurrent_stream=1000,
        target_stream_min_instructions=1, target_stream_max_instructions=256,
        target_stream_dependency=250, target_scalar_fp=200,
        target_ops=csv([1] * 15), concurrent=csv([0] * 5), concurrent_ops=csv([0] * 5),
        stream=csv([1, admitted, retired, 0, 0, 0, min(8, admitted), 1, sum(issues), 0, 0, retired]),
        stream_ops=csv(operations), stream_issue=csv(issues), stream_vector_uops=csv(vectors),
        stream_cancel=csv(counts), stream_scalar_retries=retries,
        stream_sta=csv([1 + initial, retries, 1 + hits, misses, 0, 0, 0, 0, 0, 0, 1]),
        stream_store_retirement='1,1,2,0,3,0,0,0,0,3',
        stream_vstart_flush=csv([counts[14], counts[15], 0, 0]),
        stream_history=csv([0, retired, 0, 0]), stream_waiting_head='0,0',
        stream_dependencies=csv([selected_address, selected_data, used_address, used_data,
            0, 0, used_address + used_data, 0, bool(selected_address + selected_data)]),
        stream_recovery=csv([0] * 20), vector='2,2',
        vector_operand_chains=csv([1 + counts[26], 1 + counts[27], 1 + counts[26] - counts[28]]))
    return result, records, counts


class StreamCancellationTest(unittest.TestCase):
    def test_independent_canceled_histories_and_corruption(self):
        for seed in range(256):
            result, _, _ = history(seed)
            verifier._check_stream_accounting(result)
            verifier._check_vector_operand_accounting(result)
        for slot in (1, 6, 7, 9, 12, 14, 16, 18, 20, 22, 23, 24, 25):
            result, _, counts = history(71)
            bad = copy.deepcopy(result)
            counts[slot] += 1
            bad['stream_cancel'] = csv(counts)
            with self.subTest(slot=slot), self.assertRaises(verifier.VerificationError):
                verifier._check_stream_accounting(bad)

    def test_review_counterexamples_preserve_phase_and_epoch_bounds(self):
        # Two retired instructions plus a wholly unissued canceled store.
        result = dict(constraint_schema=75, transactions=2,
            target_concurrent=1000, target_concurrent_stream=1000,
            target_stream_min_instructions=1, target_stream_max_instructions=256,
            target_stream_dependency=250, target_scalar_fp=0, target_ops=csv([1] * 15),
            concurrent=csv([0] * 5), concurrent_ops=csv([0] * 5),
            stream='1,3,2,0,0,1,3,1,3,0,0,2', stream_ops='1,0,1,0,0',
            stream_issue='1,1,1,0,0,0', stream_vector_uops='0,0', stream_scalar_retries=0,
            stream_sta='1,0,1,0,0,0,0,0,0,0,1', stream_store_retirement='1,0,0,0,1,0,0,0,0,1',
            stream_vstart_flush='0,0,0,0', stream_history='0,2,0,0',
            stream_waiting_head='0,0', stream_dependencies=csv([0] * 9), stream_recovery=csv([0] * 20))
        counts = [0] * 29
        counts[0] = counts[3] = counts[22] = counts[23] = 1
        counts[24] = 3
        result['stream_cancel'] = csv(counts)
        verifier._check_stream_accounting(result)
        corruptions = []
        bad = copy.deepcopy(result); wrong = counts.copy(); wrong[21] = 1
        bad.update(stream_cancel=csv(wrong), stream_scalar_retries=1, stream_sta='1,1,1,1,0,0,0,0,0,0,1')
        corruptions.append(bad)  # Canceled retry borrowed from the retired store.
        bad = copy.deepcopy(result); wrong = counts.copy(); wrong[8] = wrong[17] = 1
        bad.update(stream_cancel=csv(wrong), stream_issue='1,1,2,0,0,0',
            stream='1,3,2,0,0,1,3,1,4,0,0,2', stream_dependencies='0,1,0,0,0,0,0,0,1')
        corruptions.append(bad)  # Accepted STD cannot retain an unconsumed data edge.
        bad = copy.deepcopy(result); wrong = counts.copy(); wrong[7] = wrong[16] = wrong[18] = 1
        bad.update(stream_cancel=csv(wrong), stream_issue='1,2,1,0,0,0',
            stream='1,3,2,0,0,1,3,1,4,0,0,2', stream_sta='2,0,2,0,0,0,0,0,0,0,1',
            stream_dependencies='1,0,0,0,0,0,0,0,1')
        corruptions.append(bad)  # Accepted STA cannot retain an unconsumed address edge.
        bad = copy.deepcopy(result); wrong = counts.copy(); wrong[3] = wrong[22] = wrong[24] = 2
        bad.update(stream_cancel=csv(wrong), transactions=3, stream='3,5,3,0,0,0,2,1,3,0,0,3',
            stream_ops='3,0,0,0,0', stream_issue='3,0,0,0,0,0', stream_history='0,1,0,0',
            stream_sta=csv([0] * 11), stream_store_retirement=csv([0] * 10))
        corruptions.append(bad)  # Two cancellations plus a retirement need three generations.
        bad = copy.deepcopy(result); wrong = counts.copy()
        wrong[3] = 0; wrong[2] = wrong[9] = wrong[12] = wrong[22] = 2
        wrong[16] = 1; wrong[24] = 4
        bad.update(stream_cancel=csv(wrong), stream='1,4,2,0,0,1,4,1,5,0,0,2',
            stream_issue='1,1,1,2,0,0', stream_vector_uops='2,0',
            stream_dependencies='1,0,0,0,0,0,0,0,1')
        corruptions.append(bad)  # Two accepted one-uop vectors leave no unissued address consumer.
        roots = copy.deepcopy(bad); wrong = wrong.copy()
        wrong[9], wrong[12], wrong[16], wrong[26] = 9, 10, 0, 8
        roots.update(stream_cancel=csv(wrong), stream='1,4,2,0,0,1,4,1,12,0,0,2',
            stream_issue='1,1,1,9,0,0', stream_vector_uops='10,0', stream_dependencies=csv([0] * 9))
        corruptions.append(roots)  # Two accepted vector instructions need two source-chain roots.
        bad = copy.deepcopy(result); wrong = [0] * 29
        wrong[24], wrong[25] = 33, 1
        bad.update(stream_cancel=csv(wrong), transactions=67, stream='3,67,67,0,0,0,1,0,67,0,0,67',
            stream_ops='67,0,0,0,0', stream_issue='67,0,0,0,0,0', stream_history='2,33,0,0',
            stream_sta=csv([0] * 11), stream_store_retirement=csv([0] * 10))
        corruptions.append(bad)  # With no cancellation, admitted and retired extended epochs are identical.
        for index, bad in enumerate(corruptions):
            with self.subTest(counterexample=index), self.assertRaises(verifier.VerificationError):
                verifier._check_stream_accounting(bad)
        # This valid phase has no accepted ordinary STA and therefore no waiter peak.
        good = copy.deepcopy(corruptions[3]); wrong = list(map(int, good['stream_cancel'].split(',')))
        wrong[24] = 3; good['stream_cancel'] = csv(wrong)
        verifier._check_stream_accounting(good)

    def test_vector_chain_cancellation_retains_checked_waits(self):
        counts = [0] * 29
        counts[2], counts[12], counts[26], counts[28] = 1, 4, 3, 2
        result = dict(constraint_schema=75, stream_cancel=csv(counts),
            vector='2,0', vector_operand_chains='3,0,1')
        verifier._check_vector_operand_accounting(result)
        for chains in ('3,0,0', '3,0,2', '2,0,0', '6,0,4'):
            with self.subTest(chains=chains), self.assertRaises(verifier.VerificationError):
                verifier._check_vector_operand_accounting({**result, 'vector_operand_chains': chains})

    def test_compiled_records_match_independent_counts(self):
        source = r'''
#include "scenarios/mixed/stream_cancellation.hpp"
#include <iostream>
using memblock::scenarios::mixed::StreamCanceledGeneration;
int main() {
    unsigned n;
    while (std::cin >> n) {
        std::array<std::uint64_t, 29> result{};
        for (unsigned i=0; i<n; ++i) {
            StreamCanceledGeneration r;
            unsigned scalar,address,data,restart,addrdep,datadep,state;
            std::cin >> r.kind >> r.vector_uops >> r.accepted_vector_uops >> r.address_retries
                >> scalar >> address >> data >> restart >> addrdep >> datadep >> state
                >> r.vector_dependencies >> r.unconsumed_vector_dependencies;
            r.scalar_issued=scalar; r.address_issued=address; r.data_issued=data;
            r.unflushed_vstart=restart; r.unconsumed_address_dependency=addrdep;
            r.unconsumed_data_dependency=datadep;
            r.address_state=static_cast<StreamCanceledGeneration::AddressState>(state);
            r.sample(result);
        }
        for (auto value : result) std::cout << value << ' ';
        std::cout << '\n';
    }
}
'''
        rows, expected = [], []
        for seed in range(256):
            _, records, counts = history(seed)
            rows.append(str(len(records)) + '\n' + '\n'.join(' '.join(map(str, map(int, r))) for r in records))
            counts[0] = counts[22] = counts[23] = counts[24] = counts[25] = 0
            expected.append(counts)
        with tempfile.TemporaryDirectory(prefix='memblock-stream-cancel-') as directory:
            cpp = Path(directory) / 'records.cpp'; binary = cpp.with_suffix('')
            cpp.write_text(source)
            compiled = subprocess.run(['c++', '-std=c++20', '-I', str(ROOT / 'cpp'), str(cpp), '-o', str(binary)],
                capture_output=True, text=True, timeout=60)
            self.assertEqual(compiled.returncode, 0, compiled.stderr)
            run = subprocess.run([str(binary)], input='\n'.join(rows) + '\n', capture_output=True, text=True, timeout=10)
            self.assertEqual(run.returncode, 0, run.stderr)
            self.assertEqual([list(map(int, row.split())) for row in run.stdout.splitlines()], expected)

    def test_missing_and_malformed_current_schema(self):
        result, _, _ = history(1)
        del result['stream_cancel']
        with self.assertRaises(verifier.VerificationError):
            verifier._check_stream_accounting(result)
        for value in ('', '0,0', ','.join(['-1'] * 29)):
            with self.subTest(value=value), self.assertRaises(verifier.VerificationError):
                verifier._check_stream_accounting({**result, 'stream_cancel': value})


if __name__ == '__main__':
    unittest.main()
