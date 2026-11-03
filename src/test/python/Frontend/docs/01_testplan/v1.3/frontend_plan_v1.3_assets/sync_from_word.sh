#!/usr/bin/env bash
set -euo pipefail

root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
docx="$root/Frontend_BT_验证方案_v1.3.docx"
pdf="$root/Frontend_BT_验证方案_v1.3.pdf"
markdown="$root/Frontend_BT_验证方案_v1.3.md"
repo_root="$(git -C "$root" rev-parse --show-toplevel)"
scratch_root="$repo_root/.cache/document-exports"
mkdir -p "$scratch_root"
export TMPDIR="$scratch_root"
cd "$root"

# V1.3 was reviewed in Word. Preserve that file's layout and embedded figures.
pandoc "$docx" --from=docx --to=gfm --wrap=none \
  --extract-media=frontend_plan_v1.3_assets -o "$markdown"
python3 - "$markdown" <<'PY'
from pathlib import Path
import re
import sys
p = Path(sys.argv[1])
lines = p.read_text().splitlines()
title = '香山昆明湖处理器 Frontend Block Test 验证方案'
assert lines[0] == '**' + title + '**'
lines[0] = '# ' + title
for i, line in enumerate(lines):
    match = re.match(r'^#+ (.+)$', line)
    if not match:
        continue
    text = match.group(1).removeprefix('**').removesuffix('**')
    if re.match(r'^[一二三四五六七八九十]+、', text):
        lines[i] = '## ' + text
    elif re.match(r'^\d+(?:\.\d+)+\s', text):
        number = text.split()[0]
        lines[i] = '#' * (2 + number.count('.')) + ' ' + text
content = '\n'.join(lines) + '\n'
# Word page breaks in empty bold runs have no Markdown text content.
content = re.sub(r'(?m)^\*\*  \n\*\*\n?', '', content)
# Normalize reference paths after the version-directory reorganization.
reference_paths = {
    'docs/01_testplan/Frontend_BT_验证方案_v1.0.md': 'docs/01_testplan/v1.0/Frontend_BT_验证方案_v1.0.md',
    'docs/01_testplan/Frontend_BT_验证方案_v1.2.md': 'docs/01_testplan/v1.2/Frontend_BT_验证方案_v1.2.md',
    'docs/01_testplan/memblock验证方案.docx': 'docs/01_testplan/references/memblock验证方案.docx',
    'docs/01_testplan/Frontend_BT_验证方案_v1.2.pdf': 'docs/01_testplan/v1.2/Frontend_BT_验证方案_v1.2.pdf',
    'docs/01_testplan/BPU_验证策略与参考模型建设方案_v0.1.md': 'docs/01_testplan/bpu/v0.1/BPU_验证策略与参考模型建设方案_v0.1.md',
    'docs/01_testplan/Frontend_BT_验证方案.md': 'docs/01_testplan/v1.1/Frontend_BT_验证方案.md',
    'docs/01_testplan/Frontend_BT_验证设计_场景推导与测试点分解_v0.1.md': 'docs/01_testplan/verification_design/v0.1/Frontend_BT_验证设计_场景推导与测试点分解_v0.1.md',
    'docs/01_testplan/Frontend_BT_验证方案_v1.3.md': 'docs/01_testplan/v1.3/Frontend_BT_验证方案_v1.3.md',
    'docs/01_testplan/BPU_验证策略与参考模型建设方案_v0.1.pdf': 'docs/01_testplan/bpu/v0.1/BPU_验证策略与参考模型建设方案_v0.1.pdf',
    'docs/01_testplan/Frontend_BT_验证方案_v1.3.pdf': 'docs/01_testplan/v1.3/Frontend_BT_验证方案_v1.3.pdf',
    'docs/01_testplan/BPU_验证策略与参考模型建设方案_v0.1.docx': 'docs/01_testplan/bpu/v0.1/BPU_验证策略与参考模型建设方案_v0.1.docx',
    'docs/01_testplan/Frontend_BT_验证方案_v1.3.docx': 'docs/01_testplan/v1.3/Frontend_BT_验证方案_v1.3.docx',
    'docs/01_testplan/Frontend_BT_验证方案_v1.2.docx': 'docs/01_testplan/v1.2/Frontend_BT_验证方案_v1.2.docx',
}
for old_path, new_path in reference_paths.items():
    content = content.replace(old_path, new_path)
p.write_text(content)
PY

mkdir -p "$scratch_root"
out="$(mktemp -d "$scratch_root/frontend-v13-export.XXXXXX")"
remote=""
cleanup() {
  rm -rf -- "$out"
  if [ -n "$remote" ]; then ssh open19 "rm -rf -- '$remote'"; fi
}
trap cleanup EXIT
if command -v libreoffice >/dev/null 2>&1; then
  libreoffice -env:UserInstallation="file://$out/lo" --headless \
    --convert-to pdf --outdir "$out" "$docx" >/dev/null
  mv "$out/Frontend_BT_验证方案_v1.3.pdf" "$pdf"
else
  remote="$(mktemp -d "$scratch_root/frontend-v13-export-remote.XXXXXX")"
  ssh open19 "mkdir -p '$remote/output'"
  scp "$docx" "open19:$remote/input.docx" >/dev/null
  ssh open19 "TMPDIR='$remote' libreoffice -env:UserInstallation=file://$remote/lo --headless --convert-to pdf --outdir '$remote/output' '$remote/input.docx' >/dev/null"
  scp "open19:$remote/output/input.pdf" "$pdf" >/dev/null
fi
echo 'Synchronized V1.3 Word content to Markdown and PDF.'
