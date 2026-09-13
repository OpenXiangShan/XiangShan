import json
import subprocess
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set

TIMEOUT_SECONDS = 10


def _normalize_pc(pc: str) -> str:
    if not pc:
        return ""
    text = pc.strip()
    try:
        if text.lower().startswith("0x"):
            return f"0x{int(text, 16):x}"
        return f"0x{int(text):x}"
    except ValueError:
        return text


def _unknown_symbol(pc: str) -> Dict[str, object]:
    return {
        "pc": pc,
        "function": "<unknown>",
        "file": "<unknown>",
        "line": 0,
        "location": "<unknown>:0",
        "symbol_offset": "0x0",
    }


TEXT_SYMBOL_TYPES = {"T", "t", "W", "w"}


def _collect_symbols(elf: Path) -> List[tuple[int, str]]:
    try:
        result = subprocess.run(
            ["nm", "-n", "--defined-only", "--", str(elf)],
            check=True,
            text=True,
            capture_output=True,
            timeout=TIMEOUT_SECONDS,
        )
    except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
        return []
    symbols: List[tuple[int, str]] = []
    for line in result.stdout.splitlines():
        parts = line.strip().split()
        if len(parts) >= 3 and parts[1] in TEXT_SYMBOL_TYPES:
            try:
                addr = int(parts[0], 16)
            except ValueError:
                continue
            symbols.append((addr, parts[2]))
    return symbols


def _resolve_locations(elf: Path, pcs: Iterable[str]) -> Dict[str, dict[str, object]]:
    pcs = [pc for pc in pcs if pc]
    if not pcs:
        return {}
    try:
        result = subprocess.run(
            ["addr2line", "-f", "-C", "-e", str(elf), "--", *pcs],
            check=True,
            text=True,
            capture_output=True,
            timeout=TIMEOUT_SECONDS,
        )
    except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
        return {}
    lines_iter = iter(result.stdout.splitlines())
    resolved: Dict[str, dict[str, object]] = {}
    for pc in pcs:
        try:
            func = next(lines_iter).strip()
        except StopIteration:
            func = "<unknown>"
        try:
            location = next(lines_iter).strip()
        except StopIteration:
            location = "<unknown>:0"
        file, line_num = "<unknown>", 0
        if ":" in location:
            file_part, line_part = location.rsplit(":", 1)
            file_candidate = file_part.strip() or "<unknown>"
            if file_candidate == "??":
                file_candidate = "<unknown>"
            file = file_candidate
            try:
                line_num = int(line_part)
            except ValueError:
                line_num = 0
        if file == "??":
            file = "<unknown>"
        if not location or location.startswith("??"):
            file = "<unknown>"
            line_num = 0
        resolved[pc] = {
            "pc": pc,
            "function": func if func != "??" else "<unknown>",
            "file": file,
            "line": line_num,
            "location": f"{file}:{line_num}",
            "symbol_offset": "0x0",
        }
    return resolved


class ElfResolver:
    def __init__(self, elf: Path, cache_path: Optional[Path] = None):
        self.elf = Path(elf)
        self.cache_path = Path(cache_path) if cache_path else None
        self._cache: Dict[str, dict[str, object]] = self._load_cache()

    def _load_cache(self) -> Dict[str, dict[str, object]]:
        if not self.cache_path or not self.cache_path.exists():
            return {}
        try:
            data = json.loads(self.cache_path.read_text(encoding="utf-8"))
            if isinstance(data, dict):
                return {str(k): dict(v) for k, v in data.items()}
        except (json.JSONDecodeError, OSError):
            pass
        return {}

    def _store_cache(self) -> None:
        if not self.cache_path:
            return
        self.cache_path.parent.mkdir(parents=True, exist_ok=True)
        try:
            self.cache_path.write_text(json.dumps(self._cache, indent=2), encoding="utf-8")
        except OSError:
            pass

    def resolve_many(self, pcs: List[str]) -> Dict[str, dict[str, object]]:
        normalized = [_normalize_pc(pc) for pc in pcs]
        missing: List[str] = []
        seen: Set[str] = set()
        for pc in normalized:
            if not pc or pc in self._cache or pc in seen:
                continue
            missing.append(pc)
            seen.add(pc)
        if missing:
            resolved = self._resolve_missing(missing)
            self._cache.update(resolved)
            self._store_cache()
        return {pc: self._cache.get(pc, _unknown_symbol(pc)) for pc in normalized}

    def _resolve_missing(self, pcs: List[str]) -> Dict[str, dict[str, object]]:
        symbols = _collect_symbols(self.elf)
        locations = _resolve_locations(self.elf, pcs)
        resolved: Dict[str, dict[str, object]] = {}
        sorted_symbols = sorted(symbols)
        for pc in pcs:
            try:
                pc_val = int(pc, 16) if pc.lower().startswith("0x") else int(pc)
            except ValueError:
                pc_val = 0
            symbol_entry = None
            for addr, name in sorted_symbols:
                if addr > pc_val:
                    break
                symbol_entry = (addr, name)
            entry = locations.get(pc, _unknown_symbol(pc))
            function = entry.get("function", "<unknown>")
            symbol_offset = "0x0"
            if symbol_entry:
                symbol_addr, nm_function = symbol_entry
                if function == "<unknown>" and nm_function and nm_function != "<unknown>":
                    function = nm_function
                if symbol_addr <= pc_val:
                    symbol_offset = f"0x{pc_val - symbol_addr:x}"
            resolved[pc] = {
                "pc": pc,
                "function": function,
                "file": entry.get("file", "<unknown>"),
                "line": entry.get("line", 0),
                "location": entry.get("location", "<unknown>:0"),
                "symbol_offset": symbol_offset,
            }
        return resolved


def symbolize_rows(rows: List[dict], resolver: ElfResolver, pc_field: str) -> List[dict]:
    canonical_pcs: List[str] = []
    seen: Set[str] = set()
    for row in rows:
        raw_pc = row.get(pc_field)
        if not raw_pc:
            continue
        canonical = _normalize_pc(raw_pc)
        if canonical and canonical not in seen:
            seen.add(canonical)
            canonical_pcs.append(canonical)
    canonical_pcs.sort()
    resolved = resolver.resolve_many(canonical_pcs)
    result: List[dict] = []
    for row in rows:
        raw_pc = row.get(pc_field)
        canonical = _normalize_pc(raw_pc) if raw_pc else ""
        symbol = resolved.get(canonical)
        if symbol is None:
            symbol = _unknown_symbol(raw_pc)
        result.append({**row, **symbol})
    return result
