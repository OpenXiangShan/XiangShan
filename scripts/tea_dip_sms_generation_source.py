"""Resolve SMS generation sources only when the bound optimized model has a direct PHT path."""
import argparse
import json
from pathlib import Path
import re

from tea_dip_gsim_sms_internal_probe import SMS, GEN_FIELDS
from tea_dip_gsim_lsu_s1_probe import digest
from tea_dip_l2_return_trace import read
from tea_dip_query_endpoint_evidence import verify
from tea_dip_sms_observer import analyze as targeted


def direct_pht_equations(text):
    raw = SMS + '_pf_filter_io_gen_req_valid_T_1'
    pht = SMS + 'pht_gen_valid'
    degree = SMS + '_pf_filter_io_gen_req_valid_T'
    expression = re.findall(r'\b' + re.escape(raw) + r'\s*=\s*([^;\n]+);', text)
    assert expression == ['(' + pht + ' & ' + degree + ')'], 'Generated raw-valid is not the qualified direct PHT path'
    connections = {}
    for field, _ in GEN_FIELDS:
        dest = SMS + 'pf_filter__DOT__s1_gen_req__DOT__' + field + '$NEXT'
        source = SMS + 'pht__DOT__pf_gen_req_arb__DOT__io__DOT__out__DOT__bits__DOT__' + field
        values = re.findall(r'\b' + re.escape(dest) + r'\s*=\s*([^;\n]+);', text)
        assert values == [source], 'Generated accepted payload has a different source: ' + field
        connections[field] = source
    return dict(raw_valid_expression=expression[0], accepted_payload_connections=connections)


def source_events(rows):
    events = [r for r in rows if r['kind'] != 0]
    assert all(r['kind'] in [1, 2] and r['pht_valid'] == 1 for r in events), 'Observed raw arrival disagrees with direct PHT source'
    accepted = [r for r in events if r['kind'] == 1]
    suppressed = len(events) - len(accepted)
    assert all(all(r[k] == 0 for k in ['slot', 'hit', 'region_tag', 'region_address', 'region_bits', 'physical_valid', 'decreasing'])
               for r in events if r['kind'] == 2), 'Suppressed generation payload is not unknown'
    return dict(raw_PHT_arrivals=len(events), accepted_PHT_arrivals=len(accepted),
                consecutive_tag_suppressed_PHT_arrivals=suppressed,
                first_accepted=accepted[0] if accepted else None)


def analyze(folder, receipt):
    folder = Path(folder)
    closure = json.loads(Path(receipt).read_text())
    assert closure['ready'] and not closure['original_process_live']
    verify(folder, closure['files'])
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert probe['closure_receipt_sha256'] == digest(Path(receipt))
    verify(folder, probe['files'])
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['source'] == closure['source'] and proof['original_files_unchanged']
    root = Path(proof['root'])
    header = root / 'build/gsim-compile/model/SimTop.h'
    assert digest(header) == proof['original_sha256'][str(header)]
    bound = proof['SMS_internal_binding']['source_sha256']
    source_hashes = {}
    code = []
    for name, expected in bound.items():
        path = Path(name)
        assert digest(path) == expected, name
        source_hashes[name] = expected
        if path.suffix == '.cpp':
            code.append(path.read_text())
    equations = direct_pht_equations('\n'.join(code))
    old = targeted(folder, Path(receipt))
    rows = read(folder / 'sms-generation.csv')
    phases = []
    for phase in old['phases']:
        begin, end = phase['begin_core_exclusive'], phase['end_core_inclusive']
        selected = [r for r in rows if begin < r['core_cycle'] <= end]
        counts = source_events(selected)
        assert counts['raw_PHT_arrivals'] == phase['SMS']['raw_generated']
        assert counts['accepted_PHT_arrivals'] + counts['consecutive_tag_suppressed_PHT_arrivals'] == counts['raw_PHT_arrivals']
        phases.append(dict(begin_core_exclusive=begin, end_core_inclusive=end, counts=counts))
    return dict(passed=True, source=proof['source'], receipt_sha256=digest(Path(receipt)),
                header_sha256=digest(header), generated_source_sha256=source_hashes,
                compiled_source='PHT', equations=equations, phases=phases,
                full_capture=source_events(rows), added_RTL_state_bits=0, new_guests=0,
                scope='Only this source-bound optimized model has a direct PHT port to the generation filter. '
                      'Generic Scala PHT-valid is not a source selector; models with different generated equations reject. '
                      'Native/targeted margins and original phase boundaries remain checked. Suppressed payload is unknown. '
                      'No PHT learning version,cur/inc/dec identity,training causality,L2 residency,critical load or saved CPU cycles are inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
