"""Build only S2/WB plus SMS observations from this source's immutable GSIM model."""
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


def build(root, output, compiler, library):
    root, output, library = Path(root).resolve(), Path(output).resolve(), Path(library).resolve()
    assert not output.exists()
    git = lambda *args: subprocess.check_output(['git', '-C', str(root), *args], text=True).strip()
    identity = dict(source=git('rev-parse', 'HEAD'), cache_source=git('-C', 'XSCache', 'rev-parse', 'HEAD'), git_status=git('status', '--short','--branch'))
    model = root / 'build/gsim-compile/model'
    binary, header, database = root / 'build/gsim-compile/emu', model / 'SimTop.h', root / 'build/chisel_db.cpp'
    objects = sorted(model.glob('*.o')) + sorted((root / 'build/gsim-compile/other').glob('*.o'))
    originals = {str(p): digest(p) for p in [binary, header, database, library, *objects]}
    symbols = subprocess.check_output(['nm', '--defined-only', str(binary)], text=True)
    cpp, wrapped, binding = source(root, header.read_text(), database.read_text(), symbols, capture_wb=True)
    bindings = dict(binding=binding)
    for name, add in [('SMS_request_binding', pipeline), ('SMS_internal_binding', internal), ('SMS_admission_binding', admission)]:
        cpp, bindings[name] = add(root, header.read_text(), cpp)
    output.mkdir(parents=True)
    (output / 'probe.cpp').write_text(cpp)
    (output / 'objects.rsp').write_text('\n'.join(shlex.quote(str(p)) for p in objects) + '\n')
    commands = [[compiler, '-std=c++17', '-O2', '-DGSIM', '-fbracket-depth=2048', '-I' + str(model),
                 '-c', str(output / 'probe.cpp'), '-o', str(output / 'probe.o')],
                [compiler, '@' + str(output / 'objects.rsp'), str(output / 'probe.o'),
                 *['-Wl,--wrap=' + symbol for symbol in wrapped], str(library), '-lsqlite3', '-lz', '-lzstd', '-ldl', '-o', str(output / 'emu')]]
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
    (output / 'build-proof.json').write_text(json.dumps(proof, indent=2) + '\n')
    return proof


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--compiler', default='clang++')
    parser.add_argument('--dramsim3-library', type=Path, required=True)
    args = parser.parse_args()
    proof = build(args.root, args.output, args.compiler, args.dramsim3_library)
    print(json.dumps({name: proof[name] for name in ['passed', 'source', 'binary', 'binary_sha256', 'original_objects']}))


if __name__ == '__main__':
    main()
