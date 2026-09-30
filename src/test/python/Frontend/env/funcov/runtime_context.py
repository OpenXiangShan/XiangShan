from __future__ import annotations

from .recorder import (
    FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS,
    UNCACHE_EVENT_SAMPLER_BIN_KEYS,
    FrontendFuncovSampleHub,
    CoverageBinDef,
    current_funcov_sampler_sha256,
    current_verification_environment_sha256,
    default_pilot_csv_path,
    funcov_sampler_paths,
    verification_environment_paths,
)

# Compatibility alias for the first SampleHub extraction cut.
FrontendFuncovRuntimeContext = FrontendFuncovSampleHub

__all__ = [
    "CoverageBinDef",
    "FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS",
    "FrontendFuncovRuntimeContext",
    "FrontendFuncovSampleHub",
    "UNCACHE_EVENT_SAMPLER_BIN_KEYS",
    "current_funcov_sampler_sha256",
    "current_verification_environment_sha256",
    "default_pilot_csv_path",
    "funcov_sampler_paths",
    "verification_environment_paths",
]
