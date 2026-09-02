#!/usr/bin/env python3
"""Extract and validate the generated MemBlock SystemVerilog interface."""

from __future__ import annotations

import argparse
import dataclasses
import hashlib
import json
import re
import sys
from collections import Counter
from pathlib import Path
from typing import Any, Iterable


SCHEMA_VERSION = 1
DECLARATION_RE = re.compile(
    r"^(input|output|inout)\s+"
    r"(?:(?:wire|logic|reg)\s+)?"
    r"(?:(signed|unsigned)\s+)?"
    r"(?:\[\s*([^:\]]+)\s*:\s*([^\]]+)\s*\]\s+)?"
    r"([A-Za-z_$][A-Za-z0-9_$]*)$"
)
TILELINK_RE = re.compile(
    r"^(?P<interface>.+)_(?P<channel>[abcde])_"
    r"(?P<leaf>ready|valid|bits(?:_.+)?)$"
)


class ManifestError(RuntimeError):
    """Raised when RTL or its configured contract is invalid."""


@dataclasses.dataclass(frozen=True)
class Port:
    name: str
    direction: str
    width: int | None
    packed_range: str | None
    signedness: str | None
    group: str
    protocol: str
    interface: str | None
    channel: str | None
    role: str
    input_policy: str | None


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def complete_rtl_sha256(rtl_path: Path) -> str:
    """Hash the ordered generated RTL sources, including split memory helpers."""
    source_dir = rtl_path.resolve().parent
    filelist = source_dir / "filelist.f"
    files: list[Path] = []
    if filelist.is_file():
        for raw_line in filelist.read_text(encoding="utf-8").splitlines():
            line = raw_line.split("#", 1)[0].strip()
            if not line or line.startswith(("+", "-")):
                continue
            candidate = Path(line)
            path = (candidate if candidate.is_absolute() else source_dir / candidate).resolve()
            if path.name == "XSTop.sv" or path.suffix not in (".sv", ".v"):
                continue
            if path.is_file() and path not in files:
                files.append(path)
        for path in sorted(source_dir.glob("*_ext.v")):
            path = path.resolve()
            if path not in files:
                files.append(path)
    else:
        files = [rtl_path.resolve()]

    digest = hashlib.sha256()
    for path in files:
        digest.update(str(path.relative_to(source_dir)).encode("utf-8"))
        digest.update(b"\0")
        with path.open("rb") as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(chunk)
        digest.update(b"\0")
    return digest.hexdigest()


def strip_comments(text: str) -> str:
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)
    return re.sub(r"//[^\n]*", "", text)


def find_module_ports(text: str, module: str) -> str:
    clean = strip_comments(text)
    match = re.search(rf"\bmodule\s+{re.escape(module)}\s*\(", clean)
    if not match:
        raise ManifestError(f"module {module!r} was not found")

    start = match.end() - 1
    depth = 0
    for index in range(start, len(clean)):
        char = clean[index]
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return clean[start + 1 : index]
    raise ManifestError(f"unterminated port list for module {module!r}")


def split_top_level_commas(text: str) -> list[str]:
    fields: list[str] = []
    start = 0
    paren_depth = 0
    bracket_depth = 0
    brace_depth = 0
    for index, char in enumerate(text):
        if char == "(":
            paren_depth += 1
        elif char == ")":
            paren_depth -= 1
        elif char == "[":
            bracket_depth += 1
        elif char == "]":
            bracket_depth -= 1
        elif char == "{":
            brace_depth += 1
        elif char == "}":
            brace_depth -= 1
        elif char == "," and not (paren_depth or bracket_depth or brace_depth):
            fields.append(text[start:index].strip())
            start = index + 1
    tail = text[start:].strip()
    if tail:
        fields.append(tail)
    return fields


def parse_literal_width(msb: str | None, lsb: str | None) -> tuple[int | None, str | None]:
    if msb is None or lsb is None:
        return 1, None
    packed_range = f"[{msb.strip()}:{lsb.strip()}]"
    try:
        return abs(int(msb, 0) - int(lsb, 0)) + 1, packed_range
    except ValueError:
        return None, packed_range


def classify_group(name: str, config: dict[str, Any]) -> str:
    for rule in config.get("group_rules", []):
        if name in rule.get("exact", []):
            return rule["name"]
        if any(name.startswith(prefix) for prefix in rule.get("prefixes", [])):
            return rule["name"]
        if any(re.search(pattern, name) for pattern in rule.get("patterns", [])):
            return rule["name"]
    return config.get("default_group", "misc")


def signal_role(leaf: str) -> str:
    if leaf == "valid":
        return "valid"
    if leaf == "ready":
        return "ready"
    return "payload"


def protocol_metadata(
    name: str,
    group: str,
    valid_bases: set[str],
    ready_bases: set[str],
) -> tuple[str, str | None, str | None, str]:
    tl_match = TILELINK_RE.match(name)
    if group.endswith("_tilelink") and tl_match:
        return (
            "tilelink",
            tl_match.group("interface"),
            tl_match.group("channel"),
            signal_role(tl_match.group("leaf")),
        )

    direct_match = re.match(r"^(.*)_(valid|ready)$", name)
    if direct_match and direct_match.group(1) in valid_bases | ready_bases:
        base, leaf = direct_match.groups()
        protocol = "decoupled" if base in valid_bases and base in ready_bases else "valid"
        return protocol, base, None, signal_role(leaf)

    payload_bases = [base for base in valid_bases if name.startswith(base + "_bits")]
    if payload_bases:
        base = max(payload_bases, key=len)
        protocol = "decoupled" if base in ready_bases else "valid"
        return protocol, base, None, "payload"

    if name == "clock":
        return "clock", None, None, "clock"
    if "reset" in name.lower() or name.endswith("rst_n"):
        return "reset", None, None, "reset"
    return "signal", None, None, "value"


def choose_input_policy(name: str, protocol: str, role: str) -> str:
    if name == "clock":
        return "clock"
    if name == "reset":
        return "reset_active_high"
    if name.endswith("rst_n"):
        return "one"
    if protocol == "reset":
        return "zero"
    if role == "ready":
        return "one"
    return "zero"


def parse_ports(text: str, module: str, config: dict[str, Any]) -> list[Port]:
    declarations = split_top_level_commas(find_module_ports(text, module))
    raw_ports: list[tuple[str, str, int | None, str | None, str | None]] = []
    for declaration in declarations:
        normalized = " ".join(declaration.split())
        match = DECLARATION_RE.match(normalized)
        if not match:
            raise ManifestError(f"unsupported port declaration: {declaration!r}")
        direction, signedness, msb, lsb, name = match.groups()
        width, packed_range = parse_literal_width(msb, lsb)
        raw_ports.append((name, direction, width, packed_range, signedness))

    names = {item[0] for item in raw_ports}
    if len(names) != len(raw_ports):
        raise ManifestError("duplicate port name in module interface")
    valid_bases = {name[: -len("_valid")] for name in names if name.endswith("_valid")}
    ready_bases = {name[: -len("_ready")] for name in names if name.endswith("_ready")}

    ports: list[Port] = []
    for name, direction, width, packed_range, signedness in raw_ports:
        group = classify_group(name, config)
        protocol, interface, channel, role = protocol_metadata(
            name, group, valid_bases, ready_bases
        )
        input_policy = (
            choose_input_policy(name, protocol, role) if direction == "input" else None
        )
        ports.append(
            Port(
                name=name,
                direction=direction,
                width=width,
                packed_range=packed_range,
                signedness=signedness,
                group=group,
                protocol=protocol,
                interface=interface,
                channel=channel,
                role=role,
                input_policy=input_policy,
            )
        )
    return ports


def nested_set(target: dict[str, Any], dotted_key: str, value: Any) -> None:
    cursor = target
    parts = dotted_key.split(".")
    for part in parts[:-1]:
        cursor = cursor.setdefault(part, {})
    cursor[parts[-1]] = value


def derive_dimensions(ports: Iterable[Port], config: dict[str, Any]) -> dict[str, Any]:
    port_list = list(ports)
    dimensions: dict[str, Any] = {}
    for key, pattern in config.get("lane_dimensions", {}).items():
        lane_re = re.compile(pattern)
        lanes = {
            int(match.group(1))
            for port in port_list
            if (match := lane_re.fullmatch(port.name))
        }
        count = max(lanes) + 1 if lanes else 0
        if lanes != set(range(count)):
            raise ManifestError(f"dimension {key} has non-contiguous lanes: {sorted(lanes)}")
        nested_set(dimensions, key, count)

    by_name = {port.name: port for port in port_list}
    for key, name in config.get("width_dimensions", {}).items():
        if name not in by_name:
            raise ManifestError(f"width dimension {key} references missing port {name}")
        nested_set(dimensions, key, by_name[name].width)
    for key, value in config.get("fixed_dimensions", {}).items():
        if not isinstance(value, int) or isinstance(value, bool) or value < 1:
            raise ManifestError(f"fixed dimension {key} must be a positive integer")
        nested_set(dimensions, key, value)
    return dimensions


def stats_for(ports: Iterable[Port]) -> dict[str, Any]:
    port_list = list(ports)
    return {
        "port_count": len(port_list),
        "by_direction": dict(sorted(Counter(port.direction for port in port_list).items())),
        "by_group": dict(sorted(Counter(port.group for port in port_list).items())),
        "by_protocol": dict(sorted(Counter(port.protocol for port in port_list).items())),
        "input_policy": dict(
            sorted(
                Counter(
                    port.input_policy
                    for port in port_list
                    if port.input_policy is not None
                ).items()
            )
        ),
    }


def lookup_dotted(value: dict[str, Any], dotted_key: str) -> Any:
    cursor: Any = value
    for part in dotted_key.split("."):
        if not isinstance(cursor, dict) or part not in cursor:
            raise ManifestError(f"missing derived value {dotted_key}")
        cursor = cursor[part]
    return cursor


def validate_contract(
    stats: dict[str, Any], dimensions: dict[str, Any], config: dict[str, Any]
) -> None:
    errors: list[str] = []
    expected_stats = config.get("expected_stats", {})
    for key, expected in expected_stats.items():
        actual = lookup_dotted(stats, key)
        if actual != expected:
            errors.append(f"stats.{key}: expected {expected!r}, got {actual!r}")
    for key, expected in config.get("expected_dimensions", {}).items():
        actual = lookup_dotted(dimensions, key)
        if actual != expected:
            errors.append(f"dimensions.{key}: expected {expected!r}, got {actual!r}")
    if errors:
        raise ManifestError("interface contract mismatch:\n  " + "\n  ".join(errors))


def build_manifest(rtl_path: Path, config_path: Path) -> dict[str, Any]:
    rtl_bytes = rtl_path.read_bytes()
    config_bytes = config_path.read_bytes()
    config = json.loads(config_bytes)
    module = config.get("module", "MemBlock")
    ports = parse_ports(rtl_bytes.decode("utf-8"), module, config)
    stats = stats_for(ports)
    dimensions = derive_dimensions(ports, config)
    validate_contract(stats, dimensions, config)
    return {
        "schema_version": SCHEMA_VERSION,
        "module": module,
        "source": config.get("rtl", str(rtl_path)),
        "rtl_sha256": sha256_bytes(rtl_bytes),
        "complete_rtl_sha256": complete_rtl_sha256(rtl_path),
        "config_sha256": sha256_bytes(config_bytes),
        "stats": stats,
        "dimensions": dimensions,
        "ports": [dataclasses.asdict(port) for port in ports],
    }


def json_text(manifest: dict[str, Any]) -> str:
    return json.dumps(manifest, indent=2, sort_keys=True) + "\n"


def flatten_dict(value: dict[str, Any], prefix: str = "") -> list[tuple[str, Any]]:
    rows: list[tuple[str, Any]] = []
    for key in sorted(value):
        dotted = f"{prefix}.{key}" if prefix else key
        child = value[key]
        if isinstance(child, dict):
            rows.extend(flatten_dict(child, dotted))
        else:
            rows.append((dotted, child))
    return rows


def markdown_text(manifest: dict[str, Any]) -> str:
    stats = manifest["stats"]
    lines = [
        "# MemBlock Generated RTL Interface",
        "",
        "This file is generated by `scripts/rtl_ports.py`; do not edit it manually.",
        "",
        f"- Module: `{manifest['module']}`",
        f"- RTL: `{manifest['source']}`",
        f"- RTL SHA-256: `{manifest['rtl_sha256']}`",
        f"- Total ports: {stats['port_count']}",
        "",
        "## Port Groups",
        "",
        "| Group | Ports |",
        "| --- | ---: |",
    ]
    lines.extend(f"| `{key}` | {value} |" for key, value in stats["by_group"].items())
    lines.extend(
        [
            "",
            "## Protocol Classification",
            "",
            "| Protocol | Ports |",
            "| --- | ---: |",
        ]
    )
    lines.extend(
        f"| `{key}` | {value} |" for key, value in stats["by_protocol"].items()
    )
    lines.extend(
        [
            "",
            "## Derived Dimensions",
            "",
            "| Dimension | Value |",
            "| --- | ---: |",
        ]
    )
    lines.extend(f"| `{key}` | {value} |" for key, value in flatten_dict(manifest["dimensions"]))
    lines.extend(
        [
            "",
            "## Idle Input Policy",
            "",
            "| Policy | Inputs |",
            "| --- | ---: |",
        ]
    )
    lines.extend(
        f"| `{key}` | {value} |" for key, value in stats["input_policy"].items()
    )
    lines.extend(
        [
            "",
            "The complete per-port inventory, including widths, directions, bundle membership,",
            "protocol roles, and drive policy, is stored in `config/expected_ports.json`.",
            "",
        ]
    )
    return "\n".join(lines)


def write_if_changed(path: Path, content: str) -> None:
    if path.exists() and path.read_text(encoding="utf-8") == content:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def check_file(path: Path, expected: str, label: str) -> None:
    if not path.exists():
        raise ManifestError(f"{label} is missing: {path}")
    actual = path.read_text(encoding="utf-8")
    if actual != expected:
        raise ManifestError(
            f"{label} is stale: {path}; run the manifest generation target and review the diff"
        )


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("generate", "check", "summary"))
    parser.add_argument("--rtl", type=Path, required=True)
    parser.add_argument("--config", type=Path, required=True)
    parser.add_argument("--json", type=Path)
    parser.add_argument("--markdown", type=Path)
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(sys.argv[1:] if argv is None else argv)
    try:
        manifest = build_manifest(args.rtl, args.config)
        rendered_json = json_text(manifest)
        rendered_markdown = markdown_text(manifest)
        if args.action in ("generate", "check") and (not args.json or not args.markdown):
            raise ManifestError("--json and --markdown are required for generate/check")
        if args.action == "generate":
            write_if_changed(args.json, rendered_json)
            write_if_changed(args.markdown, rendered_markdown)
        elif args.action == "check":
            check_file(args.json, rendered_json, "JSON manifest")
            check_file(args.markdown, rendered_markdown, "Markdown summary")
        else:
            print(rendered_json, end="")
    except (ManifestError, OSError, UnicodeError, json.JSONDecodeError) as error:
        print(f"rtl_ports.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
