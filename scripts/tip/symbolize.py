import json
import subprocess
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set

TIMEOUT_SECONDS = 10
XLEN = 64
DEFAULT_PC_BITS = 39
DEFAULT_PC_MODE = "sign"
PC_MODES = ("sign", "zero")


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


def _lookup_pc(pc: str, pc_bits: int, pc_mode: str) -> str:
    normalized = _normalize_pc(pc)
    try:
        value = int(normalized, 16)
    except ValueError:
        return normalized
    mask = (1 << pc_bits) - 1
    value &= mask
    if pc_mode == "sign" and value & (1 << (pc_bits - 1)):
        value |= ((1 << XLEN) - 1) ^ mask
    return f"0x{value:x}"


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


def _collect_symbols(elf: Path) -> List[tuple[int, int, str]]:
    try:
        result = subprocess.run(
            ["nm", "-n", "-S", "--defined-only", "--", str(elf)],
            check=True,
            text=True,
            capture_output=True,
            timeout=TIMEOUT_SECONDS,
        )
    except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
        return []
    symbols: List[tuple[int, int, str]] = []
    for line in result.stdout.splitlines():
        parts = line.strip().split(maxsplit=3)
        if len(parts) == 3:
            parts.insert(1, "0")  # nm omits the size for size-less symbols.
        if len(parts) == 4 and parts[2] in TEXT_SYMBOL_TYPES:
            try:
                addr = int(parts[0], 16)
                size = int(parts[1], 16)
            except ValueError:
                continue
            symbols.append((addr, size, parts[3]))
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
                line_num = int(line_part.split(maxsplit=1)[0])
            except ValueError:
                line_num = 0
        if file == "??":
            file = "<unknown>"
        if not location or location.startswith("??"):
            file = "<unknown>"
            line_num = 0
            location = "<unknown>:0"
        resolved[pc] = {
            "pc": pc,
            "function": func if func != "??" else "<unknown>",
            "file": file,
            "line": line_num,
            "location": location,
            "symbol_offset": "0x0",
        }
    return resolved


class ElfResolver:
    def __init__(
        self,
        elf: Path,
        cache_path: Optional[Path] = None,
        pc_bits: int = DEFAULT_PC_BITS,
        pc_mode: str = DEFAULT_PC_MODE,
    ):
        if not 1 <= pc_bits <= XLEN:
            raise ValueError(f"pc_bits must be between 1 and {XLEN}")
        if pc_mode not in PC_MODES:
            raise ValueError(f"pc_mode must be one of: {', '.join(PC_MODES)}")
        self.elf = Path(elf).resolve()
        self.cache_path = Path(cache_path) if cache_path else None
        self.pc_bits = pc_bits
        self.pc_mode = pc_mode
        self._elf_identity = self._identity()
        self._cache_identity = f"{self._elf_identity}|pc_bits={pc_bits}|pc_mode={pc_mode}"
        self._cache_file: Dict[str, object] = {"version": 3, "elfs": {}}
        self._cache: Dict[str, dict[str, object]] = self._load_cache()

    def _identity(self) -> str:
        try:
            stat = self.elf.stat()
            return f"{self.elf}|{stat.st_size}|{stat.st_mtime_ns}"
        except OSError:
            return str(self.elf)

    def _load_cache(self) -> Dict[str, dict[str, object]]:
        if not self.cache_path or not self.cache_path.exists():
            return {}
        try:
            data = json.loads(self.cache_path.read_text(encoding="utf-8"))
            if isinstance(data, dict) and data.get("version") == 3 and isinstance(data.get("elfs"), dict):
                self._cache_file = data
                entries = data["elfs"].get(self._cache_identity, {})
                if isinstance(entries, dict):
                    return {str(k): dict(v) for k, v in entries.items()}
        except (json.JSONDecodeError, OSError):
            pass
        return {}

    def _store_cache(self) -> None:
        if not self.cache_path:
            return
        self.cache_path.parent.mkdir(parents=True, exist_ok=True)
        self._cache_file["elfs"][self._cache_identity] = self._cache
        try:
            self.cache_path.write_text(json.dumps(self._cache_file, indent=2), encoding="utf-8")
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
        lookup_pcs = [_lookup_pc(pc, self.pc_bits, self.pc_mode) for pc in pcs]
        locations = _resolve_locations(self.elf, lookup_pcs)
        resolved: Dict[str, dict[str, object]] = {}
        sorted_symbols = sorted(symbols)
        for pc, lookup_pc in zip(pcs, lookup_pcs):
            try:
                pc_val = int(lookup_pc, 16)
            except ValueError:
                pc_val = 0
            symbol_entry = None
            for addr, size, name in sorted_symbols:
                if addr > pc_val:
                    break
                if pc_val == addr or pc_val < addr + size:
                    symbol_entry = (addr, name)
            entry = locations.get(lookup_pc, _unknown_symbol(pc))
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
