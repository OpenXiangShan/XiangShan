"""Build SMS observations, optionally with TLB and memory lineage, from an immutable GSIM model."""
import argparse
import json
from pathlib import Path
import shlex
import subprocess

from tea_dip_gsim_lsu_s1_probe import digest
from tea_dip_gsim_load_service_probe import source
from tea_dip_gsim_sms_request_probe import add_capture as pipeline
from tea_dip_gsim_sms_internal_probe import add_capture as internal
from tea_dip_gsim_sms_admission_probe import add_capture as admission


def build(root, output, compiler, library, *, capture_tlb=False, capture_memory=False):
    root, output, library = Path(root).resolve(), Path(output).resolve(), Path(library).resolve()
    assert not output.exists()
    git = lambda *args: subprocess.check_output(['git', '-C', str(root), *args], text=True).strip()
    identity = dict(source=git('rev-parse', 'HEAD'), cache_source=git('-C', 'XSCache', 'rev-parse', 'HEAD'), git_status=git('status', '--short','--branch'))
    model = root / 'build/gsim-compile/model'
    binary, header, database = root / 'build/gsim-compile/emu', model / 'SimTop.h', root / 'build/chisel_db.cpp'
    objects = sorted(model.glob('*.o')) + sorted((root / 'build/gsim-compile/other').glob('*.o'))
    originals = {str(p): digest(p) for p in [binary, header, database, library, *objects]}
    symbols = subprocess.check_output(['nm', '--defined-only', str(binary)], text=True)
    if capture_memory:
        cpp, wrapped, binding = source(root, header.read_text(), database.read_text(), symbols, capture_wb=True, capture_llptw=True)
    else:
        cpp, wrapped, binding = source(root, header.read_text(), database.read_text(), symbols, capture_wb=True)
    bindings = dict(binding=binding)
    if capture_memory:
        from tea_dip_gsim_replay_state_probe import add_capture as replay
        from tea_dip_gsim_mshr_return_probe import add_capture as mshr
        from tea_dip_gsim_l2_return_probe import add_capture as l2
        from tea_dip_gsim_memory_return_probe import add_capture as memory, binding_for_model, WRAPPED
        from tea_dip_gsim_l2_merge_probe import add_capture as merge
        from tea_dip_gsim_memory_lineage_probe import add_capture as lineage, route_proof
        cpp, bindings['replay_state_binding'] = replay(header.read_text(), cpp)
        for name, add in [('MSHR_return_binding', mshr), ('L2_return_binding', l2)]:
            cpp, bindings[name] = add(root, header.read_text(), cpp)
        cpp = memory(cpp, header.read_text())
        bindings['memory_return_binding'], dram_source = binding_for_model(root, library, symbols, len(objects))
        wrapped = [*wrapped, *WRAPPED]
        cpp, bindings['L2_merge_binding'] = merge(root, header.read_text(), cpp)
        cpp, bindings['memory_lineage_binding'] = lineage(root, header.read_text(), cpp)
        bindings['memory_lineage_binding']['AXI_ID_route'] = route_proof(root)
    for name, add in [('SMS_request_binding', pipeline), ('SMS_internal_binding', internal), ('SMS_admission_binding', admission)]:
        cpp, bindings[name] = add(root, header.read_text(), cpp)
    if capture_tlb:
        from tea_dip_gsim_sms_tlb_probe import add_capture
        cpp, bindings['SMS_TLB_binding'] = add_capture(root, header.read_text(), cpp)
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_text('\n'.join(shlex.quote(str(p)) for p in objects) + '\n')
    commands = [[compiler, '-std=c++17', '-O2', '-DGSIM', '-fbracket-depth=2048', '-I' + str(model),
                 '-c', str(output / 'probe.cpp'), '-o', str(output / 'probe.o')],
                [compiler, '@' + str(output / 'objects.rsp'), str(output / 'probe.o'),
                 *['-Wl,--wrap=' + symbol for symbol in wrapped], str(library), '-lsqlite3', '-lz', '-lzstd', '-ldl', '-o', str(output / 'emu')]]
    if capture_memory:
        commands[0].insert(1, '-I' + str(dram_source))
    for i, command in enumerate(commands):
        with (output / f'build-{i}.stdout').open('wb') as out, (output / f'build-{i}.stderr').open('wb') as err:
            subprocess.run(command, stdout=out, stderr=err, check=True)
    for name, value in originals.items():
        assert digest(Path(name)) == value, name
    assert git('rev-parse', 'HEAD') == identity['source'] and git('status', '--short', '--branch') == identity['git_status']
    proof = dict(passed=True, root=str(root), **identity, binary=str(output / 'emu'), binary_sha256=digest(output / 'emu'),
                 original_binary=str(binary), original_binary_sha256=originals[str(binary)], original_files_unchanged=True,
                 original_objects=len(objects), original_sha256=originals, commands=commands, wrapped_symbols=wrapped,
                 probe_cpp_sha256=digest(output / 'probe.cpp'), generator_sha256=digest(Path(__file__)), **bindings,
                 scope='One source-qualified host object and one link, using only this source model. Seven S2/WB/SMS CSVs; no LRQ/L2/LLC/NCB/DRAM observers or new RTL state. Closed own-source native/SQL/CSV neutrality, field semantics and host costs must still be calibrated.')
    if capture_memory:
        proof['scope'] = 'One host observer object and one link compose existing S2/WB/LLPTW/LRQ/L1/L2/LLC/NCB/DRAM/SMS captures against unchanged original objects. No new RTL state. Own-source native, all SQL and old CSV neutrality, existing marginal checks and host costs still require closed-guest calibration.'
    if capture_tlb:
        proof['scope'] += ' Additional sms-tlb.csv captures qualified translation ownership and pipeline, pending closed-source calibration.'
    (output / 'build-proof.json').write_text(json.dumps(proof, indent=2) + '\n')
    return proof


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--compiler', default='clang++')
    parser.add_argument('--dramsim3-library', type=Path, required=True)
    parser.add_argument('--capture-tlb', action='store_true')
    parser.add_argument('--capture-memory', action='store_true', help='Compose existing memory-owner and DRAM probes in the same host compilation')
    args = parser.parse_args()
    proof = build(args.root, args.output, args.compiler, args.dramsim3_library, capture_tlb=args.capture_tlb, capture_memory=args.capture_memory)
    print(json.dumps({name: proof[name] for name in ['passed', 'source', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
