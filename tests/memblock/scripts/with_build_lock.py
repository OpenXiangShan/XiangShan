#!/usr/bin/env python3
"""Run one command while holding a persistent advisory file lock."""

from __future__ import annotations

import argparse
import fcntl
import os
import sys
from pathlib import Path
from typing import Iterable


def main(argv: Iterable[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lock", type=Path, required=True)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args(list(argv) if argv is not None else None)

    command = list(args.command)
    if command[:1] == ["--"]:
        command.pop(0)
    if not command:
        parser.error("a command is required after --")

    args.lock.parent.mkdir(parents=True, exist_ok=True)
    lock_fd = os.open(args.lock, os.O_CREAT | os.O_RDWR, 0o666)
    try:
        fcntl.flock(lock_fd, fcntl.LOCK_EX)
        os.set_inheritable(lock_fd, True)
        os.execvp(command[0], command)
    except OSError as error:
        print(
            f"{parser.prog}: cannot execute {command[0]!r}: {error}",
            file=sys.stderr,
        )
        return 127
    finally:
        os.close(lock_fd)


if __name__ == "__main__":
    raise SystemExit(main())
