"""Compose existing GSIM load/L1/L2/directory captures into one host object and link."""
import argparse
import json
from pathlib import Path

import tea_dip_gsim_load_service_probe as load
from tea_dip_gsim_replay_state_probe import add_capture as replay
from tea_dip_gsim_mshr_return_probe import add_capture as l1
from tea_dip_gsim_l2_return_probe import add_capture as l2
from tea_dip_gsim_l2_merge_probe import add_capture as merge
from tea_dip_gsim_l2_demand_route_probe import add_capture as route
from tea_dip_gsim_l2_directory_probe import add_capture as directory
from tea_dip_query_endpoint_evidence import digest


def source(root, header, database, symbols):
    cpp, wrapped, binding = load.source(root, header, database, symbols, capture_wb=True)
    captures = {}
    cpp, captures['replay_state_binding'] = replay(header, cpp)
    for name, capture in [('MSHR_return_binding', l1), ('L2_return_binding', l2),
                          ('L2_merge_binding', merge), ('L2_demand_route_binding', route),
                          ('L2_directory_binding', directory)]:
        cpp, captures[name] = capture(root, header, cpp)
    binding['cache_service_captures'] = captures
    return cpp, wrapped, binding


def build(root, output, compiler, library):
    proof = load.build(root, output, compiler, library, source_factory=source)
    proof.update(proof['binding'].pop('cache_service_captures'))
    names = [Path(__file__).name, 'tea_dip_gsim_load_service_probe.py',
             'tea_dip_gsim_replay_state_probe.py', 'tea_dip_gsim_mshr_return_probe.py',
             'tea_dip_gsim_l2_return_probe.py', 'tea_dip_gsim_l2_merge_probe.py',
             'tea_dip_gsim_l2_demand_route_probe.py', 'tea_dip_gsim_l2_directory_probe.py']
    proof.update(generator_sha256=digest(Path(__file__)),
                 component_generator_sha256={name: digest(Path(__file__).with_name(name)) for name in names},
                 scope='One composed host object and link against unchanged GSIM objects. Existing signal/source '
                       'bindings and NEXT/handshake assertions retained. No RTL state or capacity added. '
                       'Closed native/SQL/old CSV neutrality and each component native/semantic gate remain required. '
                       'No DRAM command capture or address-only request attribution.')
    (Path(output) / 'build-proof.json').write_text(json.dumps(proof, indent=2) + '\n')
    return proof


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--compiler', default='clang++')
    parser.add_argument('--dramsim3-library', type=Path, required=True)
    args = parser.parse_args()
    value = build(args.root, args.output, args.compiler, args.dramsim3_library)
    print(json.dumps({k: value[k] for k in ['passed', 'source', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
