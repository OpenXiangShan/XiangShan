from __future__ import annotations

from .recorder import FunctionalCoverageRecorder


class FrontendFuncovRuntimeContext(FunctionalCoverageRecorder):
    """Shared state/snapshot context for the canonical Toffee runtime.

    The class temporarily inherits the established state implementation while
    fixture ownership is split from the legacy artifact recorder.  Formal
    Toffee runs must never write legacy artifacts through this object.
    """

    def write_artifacts(self) -> dict:
        raise RuntimeError(
            "FrontendFuncovRuntimeContext cannot write legacy funcov artifacts"
        )


__all__ = ["FrontendFuncovRuntimeContext"]
