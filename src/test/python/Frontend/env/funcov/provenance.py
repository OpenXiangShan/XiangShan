from __future__ import annotations

import hashlib
import json
from functools import lru_cache
from pathlib import Path
from typing import Any

from ..runtime.artifact_provenance import file_sha256
from .registry import _frontend_root


@lru_cache(maxsize=None)
def file_sha256_cached(path_text: str) -> str:
    return file_sha256(Path(path_text))


def json_sha256(value: Any) -> str:
    payload = json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def funcov_sampler_paths() -> dict[str, Path]:
    root = Path(__file__).resolve().parent
    return {
        "funcov/recorder.py": root / "recorder.py",
        "funcov/registry.py": root / "registry.py",
        "funcov/provenance.py": root / "provenance.py",
        "funcov/__init__.py": root / "__init__.py",
        "funcov/py/ftq/sampler.py": root / "py" / "ftq" / "sampler.py",
        "funcov/py/ftq/two_fetch_funcov.py": root / "py" / "ftq" / "two_fetch_funcov.py",
        "funcov/py/ftq/ftq_request_funcov.py": root / "py" / "ftq" / "ftq_request_funcov.py",
        "funcov/py/ftq/waylookup_funcov.py": root / "py" / "ftq" / "waylookup_funcov.py",
        "funcov/py/ftq/mainpipe_funcov.py": root / "py" / "ftq" / "mainpipe_funcov.py",
        "funcov/py/ftq/ifu_delivery_funcov.py": root / "py" / "ftq" / "ifu_delivery_funcov.py",
        "funcov/py/ftq/checker_funcov.py": root / "py" / "ftq" / "checker_funcov.py",
        "funcov/py/icache/__init__.py": root / "py" / "icache" / "__init__.py",
        "funcov/py/icache/signal_contract.py": root / "py" / "icache" / "signal_contract.py",
        "funcov/py/icache/icache_mainpipe_funcov.py": root / "py" / "icache" / "icache_mainpipe_funcov.py",
        "funcov/py/icache/icache_prefetchpipe_funcov.py": root / "py" / "icache" / "icache_prefetchpipe_funcov.py",
        "funcov/py/icache/icache_missunit_funcov.py": root / "py" / "icache" / "icache_missunit_funcov.py",
        "funcov/py/icache/icache_waylookup_funcov.py": root / "py" / "icache" / "icache_waylookup_funcov.py",
        "funcov/py/icache/icache_hitmiss_funcov.py": root / "py" / "icache" / "icache_hitmiss_funcov.py",
        "funcov/py/ifu/sampler.py": root / "py" / "ifu" / "sampler.py",
        "funcov/py/ifu/cfvec_funcov.py": root / "py" / "ifu" / "cfvec_funcov.py",
        "funcov/py/ifu/compact_funcov.py": root / "py" / "ifu" / "compact_funcov.py",
        "funcov/py/ifu/owner_v3_funcov.py": root / "py" / "ifu" / "owner_v3_funcov.py",
        "funcov/py/ifu/mmio_v3_funcov.py": root / "py" / "ifu" / "mmio_v3_funcov.py",
        "funcov/py/ifu/mmio_nc_owner_funcov.py": root / "py" / "ifu" / "mmio_nc_owner_funcov.py",
        "funcov/py/ifu/cacheable_pipeline_funcov.py": root / "py" / "ifu" / "cacheable_pipeline_funcov.py",
        **{
            f"funcov/py/{path.relative_to(root / 'py').as_posix()}": path
            for path in sorted((root / "py").rglob("*.py"))
            if path.name.endswith("_toffee.py")
        },
        "funcov/toffee_bridge.py": root / "toffee_bridge.py",
    }


def current_funcov_sampler_sha256() -> str:
    return json_sha256(
        {
            label: file_sha256_cached(str(path))
            for label, path in funcov_sampler_paths().items()
        }
    )


def verification_environment_paths() -> dict[str, Path]:
    frontend_root = _frontend_root()
    env_root = frontend_root / "env"
    paths = [
        path
        for path in env_root.rglob("*")
        if path.is_file() and path.suffix in {".py", ".sv"}
    ]
    paths.extend(
        path
        for path in (
            frontend_root / "Frontend_api.py",
            frontend_root / "Frontend_env.py",
            frontend_root / "conftest.py",
            frontend_root / "tests" / "conftest.py",
        )
        if path.is_file()
    )
    return {
        path.relative_to(frontend_root).as_posix(): path
        for path in sorted(set(paths))
    }


def current_verification_environment_sha256() -> str:
    return json_sha256(
        {
            label: file_sha256_cached(str(path))
            for label, path in verification_environment_paths().items()
        }
    )


__all__ = [
    "current_funcov_sampler_sha256",
    "current_verification_environment_sha256",
    "file_sha256_cached",
    "funcov_sampler_paths",
    "json_sha256",
    "verification_environment_paths",
]
