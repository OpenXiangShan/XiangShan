"""Collect one successful, closed guest while its paired controller may still run."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import shlex
import subprocess
import tarfile


def collect(host, folder, output):
    assert not output.exists(), 'Preserve prior closed-guest collections'
    payload = 'folder=Path(' + repr(folder) + ')\n'
    payload = 'from pathlib import Path\n' + payload + '''
from datetime import datetime, timezone
import hashlib, json, socket
process = json.loads((folder / 'process.json').read_text())
assert process['host'] == socket.gethostname(), 'Wrong host for the recorded original process'
stat = Path('/proc', str(process['pid']), 'stat')
assert not stat.exists() or stat.read_text().rsplit(')', 1)[1].split()[19] != process['start_ticks'], 'Original guest remains live'
receipt = json.loads((folder / 'closure-receipt.json').read_text())
result = json.loads((folder / 'result.json').read_text())
assert receipt['ready'] and not receipt['original_process_live']
assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
assert all(result['sampler_flags'].values())
assert receipt['process'] == result['process'] == process
assert receipt['source'] == result['source']
files = dict(receipt['files'])
p = folder / 'closure-receipt.json'
files[p.name] = dict(bytes=p.stat().st_size, sha256=hashlib.sha256(p.read_bytes()).hexdigest())
print(json.dumps(dict(ready=True, utc=datetime.now(timezone.utc).isoformat(), host=socket.gethostname(),
    folder=str(folder), process=process, source=result['source'], binary_sha256=result['binary_sha256'],
    original_process_live=False, files=files)))
'''
    check = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=12',
                            host, 'python3', '-'], input=payload, text=True, capture_output=True, timeout=45)
    assert check.returncode == 0, check.stderr
    record = json.loads(check.stdout)
    output.mkdir(parents=True)
    archive = output / 'closed.tar.gz'
    command = 'tar -C ' + shlex.quote(folder) + ' -czf - -- ' + ' '.join(shlex.quote(n) for n in record['files'])
    with archive.open('wb') as stream:
        reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', host, command],
                               stdout=stream, stderr=subprocess.PIPE, timeout=180)
    assert reply.returncode == 0, reply.stderr.decode()
    with tarfile.open(archive) as stream:
        assert {m.name for m in stream.getmembers()} == set(record['files'])
        stream.extractall(output, filter='data')
    for name, expected in record['files'].items():
        path = output / name
        with path.open('rb') as stream:
            digest = hashlib.file_digest(stream, 'sha256').hexdigest()
        assert path.stat().st_size == expected['bytes'] and digest == expected['sha256'], name
    record.update(collector_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  database_read_while_live=False,
                  scope='Only this original guest is closed. Its paired guest/controller may still run. '
                        'All closure files are byte-verified; no pair gain, hidden-state identity or suite result is inferred.')
    (output / 'collection.json').write_text(json.dumps(record, indent=2) + '\n')
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--host', required=True)
    parser.add_argument('--folder', required=True)
    parser.add_argument('--output-dir', required=True, type=Path)
    args = parser.parse_args()
    result = collect(args.host, args.folder, args.output_dir.resolve())
    print(json.dumps(dict(ready=result['ready'], source=result['source'], files=len(result['files']),
                          output=str(args.output_dir.resolve()), original_process_live=False), indent=2))


if __name__ == '__main__':
    main()
