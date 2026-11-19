#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import shutil
import subprocess
from contextlib import contextmanager
from pathlib import Path
from typing import Iterable, Iterator, TextIO


TOP_SCOPE = "TOP.Frontend_top"
CFVEC_FIELDS = (
    "valid",
    "ready",
    "bits_pc",
    "bits_instr",
    "bits_isRvc",
    "bits_predTaken",
    "bits_fixedTaken",
    "bits_ftqPtr_flag",
    "bits_ftqPtr_value",
    "bits_ftqOffset",
    "bits_isLastInFtqEntry",
)
REDIRECT_FIELDS = (
    "valid",
    "bits_pc",
    "bits_target",
    "bits_ftqIdx_flag",
    "bits_ftqIdx_value",
    "bits_ftqOffset",
)
FROM_FTQ_FIELDS = (
    "wen",
    "ftqIdx",
    "startPc_addr",
)


def _wanted_relative_names() -> set[str]:
    names = {"clock"}
    names.update(
        f"io_backend_cfVec_{slot}_{field}"
        for slot in range(8)
        for field in CFVEC_FIELDS
    )
    names.update(f"io_backend_toFtq_redirect_{field}" for field in REDIRECT_FIELDS)
    names.update(f"io_backend_fromFtq_{field}" for field in FROM_FTQ_FIELDS)
    return names


def _decode_vcd_value(raw: str) -> int | None:
    normalized = raw.lower()
    if any(bit in normalized for bit in "xz"):
        return None
    return int(normalized, 2)


def _parse_header(
    lines: Iterator[str],
) -> tuple[dict[str, str], dict[str, str]]:
    wanted = _wanted_relative_names()
    scopes: list[str] = []
    relative_by_code: dict[str, str] = {}
    code_by_relative: dict[str, str] = {}

    for raw_line in lines:
        line = raw_line.strip()
        if line.startswith("$scope "):
            tokens = line.split()
            if len(tokens) >= 3:
                scopes.append(tokens[2])
            continue
        if line.startswith("$upscope"):
            if scopes:
                scopes.pop()
            continue
        if line.startswith("$var "):
            tokens = line.split()
            if len(tokens) < 6:
                continue
            code = tokens[3]
            reference = tokens[4]
            full_name = ".".join((*scopes, reference))
            prefix = f"{TOP_SCOPE}."
            if not full_name.startswith(prefix):
                continue
            relative_name = full_name[len(prefix) :]
            if relative_name not in wanted:
                continue
            previous = code_by_relative.get(relative_name)
            if previous is not None and previous != code:
                raise ValueError(
                    f"VCD signal has multiple identifiers: {relative_name}: "
                    f"{previous!r}, {code!r}"
                )
            code_by_relative[relative_name] = code
            relative_by_code[code] = relative_name
            continue
        if line.startswith("$enddefinitions"):
            break
    else:
        raise ValueError("VCD header does not contain $enddefinitions")

    if "clock" not in code_by_relative:
        raise ValueError(f"VCD does not contain {TOP_SCOPE}.clock")
    return relative_by_code, code_by_relative


def _apply_value_change(
    line: str,
    relative_by_code: dict[str, str],
    values: dict[str, int | None],
) -> None:
    if not line:
        return
    first = line[0]
    if first in "01xXzZ":
        code = line[1:].strip()
        relative = relative_by_code.get(code)
        if relative is not None:
            values[relative] = _decode_vcd_value(first)
        return
    if first in "bB":
        tokens = line.split()
        if len(tokens) != 2:
            return
        relative = relative_by_code.get(tokens[1])
        if relative is not None:
            values[relative] = _decode_vcd_value(tokens[0][1:])


def _structured_snapshot(time: int, values: dict[str, int | None]) -> dict:
    redirect = {
        field: values.get(f"io_backend_toFtq_redirect_{field}")
        for field in REDIRECT_FIELDS
    }
    from_ftq = {
        field: values.get(f"io_backend_fromFtq_{field}")
        for field in FROM_FTQ_FIELDS
    }
    start_pc_addr = from_ftq.get("startPc_addr")
    from_ftq["start_pc"] = (
        None if start_pc_addr is None else int(start_pc_addr) << 1
    )
    cfvec = []
    for slot in range(8):
        cfvec.append(
            {
                "slot": slot,
                **{
                    field: values.get(f"io_backend_cfVec_{slot}_{field}")
                    for field in CFVEC_FIELDS
                },
            }
        )
    return {
        "time": int(time),
        "clock": values.get("clock"),
        "redirect": redirect,
        "from_ftq": from_ftq,
        "cfvec": cfvec,
    }


def iter_rising_edge_snapshots(
    lines: Iterable[str],
    *,
    start_time: int = 0,
    end_time: int | None = None,
) -> Iterator[dict]:
    iterator = iter(lines)
    relative_by_code, _code_by_relative = _parse_header(iterator)
    values: dict[str, int | None] = {}
    current_time: int | None = None
    clock_before_time: int | None = None

    def finish_time() -> dict | None:
        if current_time is None:
            return None
        clock_after_time = values.get("clock")
        if clock_before_time != 0 or clock_after_time != 1:
            return None
        if int(current_time) < int(start_time):
            return None
        if end_time is not None and int(current_time) > int(end_time):
            return None
        return _structured_snapshot(int(current_time), values)

    for raw_line in iterator:
        line = raw_line.strip()
        if line.startswith("#"):
            snapshot = finish_time()
            if snapshot is not None:
                yield snapshot
            next_time = int(line[1:])
            if end_time is not None and current_time is not None and next_time > int(end_time):
                return
            current_time = next_time
            clock_before_time = values.get("clock")
            continue
        _apply_value_change(line, relative_by_code, values)

    snapshot = finish_time()
    if snapshot is not None:
        yield snapshot


@contextmanager
def _vcd_stream(path: Path) -> Iterator[TextIO]:
    path = path.resolve()
    if path.suffix.lower() != ".fst":
        with path.open("r", encoding="ascii", errors="strict") as stream:
            yield stream
        return

    converter = shutil.which("fst2vcd")
    if converter is None:
        raise RuntimeError("fst2vcd is required to audit an FST waveform")
    process = subprocess.Popen(
        [converter, "-f", str(path)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="ascii",
    )
    assert process.stdout is not None
    try:
        yield process.stdout
    finally:
        process.stdout.close()
        if process.poll() is None:
            process.terminate()
        assert process.stderr is not None
        stderr = process.stderr.read()
        process.wait()
        if process.returncode not in (0, -15):
            raise RuntimeError(
                f"fst2vcd failed with exit code {process.returncode}: {stderr.strip()}"
            )


def load_rising_edge_snapshots(
    path: Path,
    *,
    start_time: int = 0,
    end_time: int | None = None,
) -> list[dict]:
    with _vcd_stream(path) as stream:
        return list(
            iter_rising_edge_snapshots(
                stream,
                start_time=int(start_time),
                end_time=None if end_time is None else int(end_time),
            )
        )


def _hex(value: int | None) -> str:
    return "x" if value is None else f"0x{int(value):x}"


def _text_lines(snapshots: Iterable[dict], *, include_idle: bool) -> Iterator[str]:
    for snapshot in snapshots:
        redirect = dict(snapshot["redirect"])
        from_ftq = dict(snapshot["from_ftq"])
        valid_cfvec = [entry for entry in snapshot["cfvec"] if entry.get("valid") == 1]
        if (
            not include_idle
            and redirect.get("valid") != 1
            and from_ftq.get("wen") != 1
            and not valid_cfvec
        ):
            continue
        parts = [f"time={int(snapshot['time'])}"]
        if redirect.get("valid") == 1:
            parts.append(
                "redirect=(pc={pc},target={target},ftq=({flag},{value}),offset={offset})".format(
                    pc=_hex(redirect.get("bits_pc")),
                    target=_hex(redirect.get("bits_target")),
                    flag=redirect.get("bits_ftqIdx_flag"),
                    value=redirect.get("bits_ftqIdx_value"),
                    offset=redirect.get("bits_ftqOffset"),
                )
            )
        if from_ftq.get("wen") == 1:
            parts.append(
                "from_ftq=(idx={idx},start_pc={start_pc})".format(
                    idx=from_ftq.get("ftqIdx"),
                    start_pc=_hex(from_ftq.get("start_pc")),
                )
            )
        packets = []
        for entry in valid_cfvec:
            packets.append(
                "s{slot}(pc={pc},instr={instr},rvc={rvc},pred={pred},"
                "ftq=({flag},{value}),offset={offset},last={last})".format(
                    slot=int(entry["slot"]),
                    pc=_hex(entry.get("bits_pc")),
                    instr=_hex(entry.get("bits_instr")),
                    rvc=entry.get("bits_isRvc"),
                    pred=entry.get("bits_predTaken"),
                    flag=entry.get("bits_ftqPtr_flag"),
                    value=entry.get("bits_ftqPtr_value"),
                    offset=entry.get("bits_ftqOffset"),
                    last=entry.get("bits_isLastInFtqEntry"),
                )
            )
        if packets:
            parts.append("cfvec=[" + ",".join(packets) + "]")
        yield " ".join(parts)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Audit top-level Frontend redirect/cfVec values after each VCD rising-edge timestamp"
    )
    parser.add_argument("waveform", type=Path, help="input .fst or .vcd waveform")
    parser.add_argument("--start-time", type=int, default=0)
    parser.add_argument("--end-time", type=int)
    parser.add_argument("--format", choices=("text", "json"), default="text")
    parser.add_argument("--include-idle", action="store_true")
    args = parser.parse_args()

    snapshots = load_rising_edge_snapshots(
        args.waveform,
        start_time=args.start_time,
        end_time=args.end_time,
    )
    if args.format == "json":
        print(json.dumps(snapshots, indent=2, sort_keys=True))
    else:
        for line in _text_lines(snapshots, include_idle=bool(args.include_idle)):
            print(line)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
