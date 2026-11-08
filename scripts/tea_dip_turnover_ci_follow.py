"""Archive and analyze newly closed points of the original turnover CI; preserve existing analyses."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import subprocess
import sys
import time

from tea_dip_query_endpoint_evidence import digest
from tea_dip_turnover_ci_point import NAMESPACE, RUN, SOURCE, context
from tea_dip_wb_protocol_micro import write


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--state-dir', type=Path, required=True)
    args = parser.parse_args()
    root, status_dir = args.report_dir.resolve(), args.state_dir.resolve()
    status_dir.mkdir(parents=True, exist_ok=True)
    assert not (status_dir / 'state.json').exists(), 'Follow the original CI collector'
    _, _, points, _ = context(root)
    expected = {row['point'] for row in points}
    tools = Path(__file__).resolve().parent
    scripts = ['tea_dip_collect_credit_ci.py', 'tea_dip_turnover_ci_point.py', 'tea_dip_turnover_ci_diagnostics.py', 'tea_dip_bpu_native_cohort.py']
    frozen = {name: digest(tools / name) for name in scripts}
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 run_id=RUN, source=SOURCE, phase='observing', attempts=0, expected_points=len(expected), tool_sha256=frozen)

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(status_dir / 'state.json', state)

    def execute(name, arguments, label):
        reply = subprocess.run([sys.executable, str(tools / name), *arguments], capture_output=True, text=True)
        (status_dir / (label + '.stdout')).write_text(reply.stdout)
        (status_dir / (label + '.stderr')).write_text(reply.stderr)
        return reply

    save()
    try:
        while True:
            assert all(digest(tools / name) == value for name, value in frozen.items()), 'Preserve active CI follow tools'
            attempt = state['attempts'] + 1
            save(attempts=attempt, phase='collecting')
            reply = execute(scripts[0], ['--report-dir', str(root), '--ci-evidence', NAMESPACE], f'collect-{attempt:04d}')
            if reply.returncode:
                save(phase='observation_error')
                time.sleep(60)
                continue
            closed = json.loads(reply.stdout)
            assert closed['run_id'] == RUN and closed['source'] == SOURCE
            names = {p.stem for p in (root / NAMESPACE / 'raw/closed-receipts').glob('*.json')}
            assert names <= expected
            completed = []
            for point in sorted(names):
                for script, folder, kind in [('tea_dip_turnover_ci_point.py', 'partial-analysis', 'point'),
                                             ('tea_dip_turnover_ci_diagnostics.py', 'partial-diagnostics', 'diagnostics'),
                                             ('tea_dip_bpu_native_cohort.py', 'partial-bpu-audit', 'bpu')]:
                    path = root / NAMESPACE / folder / (point + '.json')
                    if not path.exists():
                        save(phase='analyzing', point=point, analysis=kind)
                        reply = execute(script, ['--report-dir', str(root), point], point + '-' + kind)
                        assert reply.returncode == 0, 'Inspect closed point analysis failure: ' + point + '/' + kind
                    data = json.loads(path.read_text())
                    assert not data['pending']
                    assert (data['comparison']['point'] if kind == 'point' else data['point']) == point
                completed.append(point)
            save(phase='observing', closed_points=len(names), analyzed_points=len(completed))
            if len(completed) == len(expected):
                save(phase='all_points_analyzed_official_score_pending')
                return
            time.sleep(60)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
