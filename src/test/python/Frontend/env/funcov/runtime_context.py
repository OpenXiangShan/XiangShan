from __future__ import annotations

from .registry import (
    CoverageBinDef,
    decode_signal_inventory_name,
    default_pilot_csv_path,
)
from .recorder import (
    FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS,
    UNCACHE_EVENT_SAMPLER_BIN_KEYS,
    FrontendFuncovSampleHub,
    current_funcov_sampler_sha256,
    current_verification_environment_sha256,
    funcov_sampler_paths,
    verification_environment_paths,
)

_decode_signal_inventory_name = decode_signal_inventory_name

# Compatibility alias for the first SampleHub extraction cut.
FrontendFuncovRuntimeContext = FrontendFuncovSampleHub

__all__ = [
    "CoverageBinDef",
    "FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS",
    "FrontendFuncovRuntimeContext",
    "FrontendFuncovSampleHub",
    "UNCACHE_EVENT_SAMPLER_BIN_KEYS",
    "decode_signal_inventory_name",
    "_decode_signal_inventory_name",
    "default_pilot_csv_path",
    "current_funcov_sampler_sha256",
    "current_verification_environment_sha256",
    "funcov_sampler_paths",
    "verification_environment_paths",
]
