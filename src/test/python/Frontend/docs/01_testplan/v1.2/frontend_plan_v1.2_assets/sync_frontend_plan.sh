#!/usr/bin/env bash
set -euo pipefail

root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
assets="$root/frontend_plan_v1.2_assets"
source_md="${1:-$root/Frontend_BT_验证方案_v1.2.md}"
source_md="$(realpath -- "$source_md")"
docx="${source_md%.md}.docx"
pdf="${source_md%.md}.pdf"
docx_name="$(basename -- "$docx")"
pdf_name="$(basename -- "$pdf")"
repo_root="$(git -C "$root" rev-parse --show-toplevel)"
scratch_root="$repo_root/.cache/document-exports"
mkdir -p "$scratch_root"
export TMPDIR="$scratch_root"

# Markdown is the content source. export_word.py applies the MemBlock layout
# and embeds the selected CJK fonts before the compatibility normalization.
if [ "$source_md" = "$root/Frontend_BT_验证方案_v1.2.md" ]; then
  python3 "$assets/export_word.py"
else
  pandoc "$source_md" --from=markdown --to=docx \
    --reference-doc="$root/../references/memblock验证方案.docx" -o "$docx"
  python3 "$assets/apply_memblock_format.py" "$docx"
  python3 "$assets/embed_fonts.py" "$docx"
fi

if command -v libreoffice >/dev/null 2>&1; then
  lo=libreoffice
elif command -v soffice >/dev/null 2>&1; then
  lo=soffice
else
  lo=""
fi

if [ -n "$lo" ]; then
  mkdir -p "$scratch_root"
  out="$(mktemp -d "$scratch_root/frontend-plan-export.XXXXXX")"
  trap 'rm -rf "$out"' EXIT
  "$lo" -env:UserInstallation="file://$out/lo" --headless \
    --convert-to docx:"Office Open XML Text" --outdir "$out" "$docx" >/dev/null
  mv "$out/$docx_name" "$docx"
  "$lo" -env:UserInstallation="file://$out/lo-pdf" --headless \
    --convert-to pdf --outdir "$out" "$docx" >/dev/null
  mv "$out/$pdf_name" "$pdf"
else
  # Both hosts share this repository path; scratch stays inside the repository.
  command -v ssh >/dev/null 2>&1 || { echo 'LibreOffice and ssh are unavailable' >&2; exit 1; }
  mkdir -p "$scratch_root"
  remote="$(mktemp -d "$scratch_root/frontend-plan-export-remote.XXXXXX")"
  trap 'rm -rf -- "$remote"' EXIT
  ssh open19 "mkdir -p '$remote' '$remote/normalized'"
  scp "$docx" "open19:$remote/input.docx" >/dev/null
  ssh open19 "TMPDIR='$remote' libreoffice -env:UserInstallation=file://$remote/lo --headless --convert-to docx:'Office Open XML Text' --outdir '$remote/normalized' '$remote/input.docx' >/dev/null && TMPDIR='$remote' libreoffice -env:UserInstallation=file://$remote/lo-pdf --headless --convert-to pdf --outdir '$remote/normalized' '$remote/normalized/input.docx' >/dev/null"
  scp "open19:$remote/normalized/input.docx" "$docx" >/dev/null
  scp "open19:$remote/normalized/input.pdf" "$pdf" >/dev/null
  ssh open19 "rm -rf '$remote'"
fi

echo "Synchronized Markdown -> $docx and $pdf"
