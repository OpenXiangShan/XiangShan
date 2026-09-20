from __future__ import annotations

from .sample_hub import FrontendFuncovSampleHub

# Compatibility alias for the first SampleHub extraction cut.
FrontendFuncovRuntimeContext = FrontendFuncovSampleHub

__all__ = ["FrontendFuncovRuntimeContext", "FrontendFuncovSampleHub"]
