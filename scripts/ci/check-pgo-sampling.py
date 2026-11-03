#!/usr/bin/env python3
# Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
# Licensed under Mulan PSL v2.
"""Validate and archive TEA/DIP sampling from the current PGO training run.

Usage: check-pgo-sampling.py BUILD_DIR REPORT_DIR TABLE [TABLE ...]
"""
import hashlib
import json
from pathlib import Path
import re
import shutil
import sqlite3
import subprocess
import sys

build, report = (Path(p).resolve() for p in sys.argv[1:3])
tables = sys.argv[3:]
logs = [p for p in build.rglob('*.log') if p.parent.name == 'pgo']
assert logs, 'PGO training stdout was not saved'
log = max(logs, key=lambda p: p.stat().st_mtime_ns)
text = log.read_text()
assert 'HIT CRITICAL' not in text, 'PGO training reported a critical error'
assert 'EXCEEDING CYCLE/INSTR LIMIT' in text or 'HIT GOOD TRAP' in text
match = re.search(r'instrCnt = ([\d,]+)', text)
assert match and int(match[1].replace(',', '')) >= 10000, 'CoreMark training did not run'
database = build / 'pgo-training.db'
periods = {}
with sqlite3.connect(database.as_uri() + '?mode=ro', uri=True) as db:
    counts = {table: db.execute('SELECT count(*) FROM ' + table).fetchone()[0] for table in tables}
    assert all(counts.values()), counts
    for sampler, table in [('tea', 'Tea_0'), ('dip', 'Dip_0')]:
        if table not in tables:
            continue
        assert re.search('^enable' + sampler.title() + '0 = 1$', text, re.M), sampler
        match = re.search('^' + sampler + r'SamplePeriod0 = (\d+)$', text, re.M)
        assert match, sampler
        periods[sampler] = int(match[1])
        if periods[sampler]:
            assert db.execute('SELECT count(*) FROM ' + table + ' WHERE CYCLE % ? != 0',
                              (periods[sampler],)).fetchone()[0] == 0
    if len(periods) == 2 and periods['tea'] == periods['dip']:
        assert counts['Tea_0'] == counts['Dip_0'], counts
result = dict(source=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
              scope='PGO training sampling; separate from SPEC correctness and performance',
              periods=periods, tables=counts,
              database_sha256=hashlib.sha256(database.read_bytes()).hexdigest(),
              stdout_sha256=hashlib.sha256(log.read_bytes()).hexdigest())
shutil.copyfile(database, report / 'pgo-training.db')
shutil.copyfile(log, report / 'pgo-training.log')
(report / 'pgo-training-audit.json').write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(result))
