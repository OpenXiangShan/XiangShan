#!/usr/bin/env python3
"""检查 PMA/PMP raw CSR FIFO 生产调用白名单与核心时序合同。"""
from pathlib import Path
import re

root = Path(__file__).resolve().parents[2]
functions = {}
for path in root.rglob('*.sv'):
    if 'sim' in path.relative_to(root).parts:
        continue
    source = path.read_text()
    # 仅查实际定义的函数体，不把 extern 声明当作 caller。
    for match in re.finditer(r'(?m)^\s*(?:virtual\s+)?(?:static\s+)?function\s+[^;]+;(?P<body>.*?)endfunction\s*(?::\s*\w+)?', source, re.S):
        header = match.group(0).split(';', 1)[0]
        if 'extern ' in header:
            continue
        name = re.findall(r'(\w+)\s*\(', header)
        if name:
            functions[(path.relative_to(root).as_posix(), name[-1])] = match.group('body')

rules = {
    'commit_raw_pma_pmp_csr_write_prefix': {'commit_pma_pmp_csr_replay'},
    'pop_raw_pma_pmp_csr_write': set(),
    'commit_csr_replay_batch': {'commit_pma_pmp_csr_replay', 'apply_csr_write'},
    'apply_csr_write': {'check_atomic_pma_pmp_capture'},
    'push_raw_pma_pmp_csr_write': {'push'},
}
errors = []
for api, allowed in rules.items():
    for (path, function), body in functions.items():
        if re.search(r'\b' + api + r'\s*\(', body) and function not in allowed:
            errors.append(f'{path}:{function}: unexpected caller of {api}')
# monitor 是 task，独立确认唯一生产 push 与 epoch 配对。
monitor = root / 'agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv'
mon = monitor.read_text()
assert 'raw_pma_pmp_csr_write.reset_epoch =' in mon
common = (root / 'seq/base_seq_help/common_data_transaction.sv').read_text()
for removed in ('apply_pma_pmp_csr_writes_before_request', 'get_generations_before_sample_with_replay'):
    assert removed not in common
assert 'published_sample - event_item.sample_seq < 2' in common
assert 'event_item.sample_seq += 2' in common
assert 'function bit precheck_pma_pmp_csr_end_state' in common
for name in ('service_issue_residency_ledger', 'end_test_check'):
    body = functions.get(('seq/base_seq_help/common_data_transaction.sv', name))
    if body is None:
        continue
    assert not re.search(r'\b(?:commit_pma_pmp_csr_replay|service_csr_pmp_pma_write_observations|pop_raw_pma_pmp_csr_write)\s*\(', body)
model = (root / 'seq/base_seq_help/memblock_pma_pmp_model.sv').read_text()
assert 'get_generations_before_sample_with_replay' not in model
assert 'write_sample <= request_sample' in model
directed = (root / 'seq/base_seq_help/soft_test/soft_test_pma_pmp_csr_fifo_sequence.sv').read_text()
assert 'precheck_pma_pmp_csr_end_state' in directed
assert 'uvm_report_catcher' not in directed
assert 'uvm_report_cb::' not in directed
assert '`uvm_error' not in directed.lower()
# 全文件扫描补上 task、裸队列 mutation，避免仅函数抽取漏检。
expected_files = {
    'commit_raw_pma_pmp_csr_write_prefix': {'common/memblock_common/src/memblock_sync_pkg.sv', 'seq/base_seq_help/common_data_transaction.sv'},
    'push_raw_pma_pmp_csr_write': {'common/memblock_common/src/memblock_sync_pkg.sv', 'agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv', 'seq/base_seq_help/soft_test/soft_test_pma_pmp_csr_fifo_sequence.sv'},
    'apply_csr_write': {
        'seq/base_seq_help/memblock_pma_pmp_model.sv',
        'seq/base_seq/soft_test/soft_test_rm_provenance_followup_sequence.sv',
    },
}
for api, allowed in expected_files.items():
    for path in root.rglob('*.sv'):
        rel = path.relative_to(root).as_posix()
        if 'sim' not in path.relative_to(root).parts and re.search(r'\b'+api+r'\s*\(', path.read_text()) and rel not in allowed:
            errors.append(f'{rel}: unexpected API reference {api}')
if errors:
    raise SystemExit('\n'.join(errors))
print(f'PASS: PMA/PMP CSR FIFO caller whitelist and C+2/history/audit contracts ({len(functions)} function bodies)')
