from __future__ import annotations

from dataclasses import dataclass

from ..bundles import BackendFromFtqBundle, FrontendInfoBundle, bind_bundle_optional
from ..model.backend_runtime import BackendObservationSnapshot


@dataclass(frozen=True)
class BackendStartupObservation:
    startup_progress: bool = False
    from_ftq_ftq_idx: int = 0
    from_ftq_start_pc_addr: int = 0
    ibuf_full: int = 0


class BackendObserveMonitor:
    def __init__(self) -> None:
        self.from_ftq_if = None
        self.frontend_info_if = None

    @staticmethod
    def _read(signal, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return default if value is None else int(value)
        except Exception:
            return default

    def bind(self, target) -> None:
        self.from_ftq_if = bind_bundle_optional(BackendFromFtqBundle, target)
        self.frontend_info_if = bind_bundle_optional(FrontendInfoBundle, target)

    def observe_startup(self) -> BackendStartupObservation:
        from_ftq_wen = self._read(getattr(self.from_ftq_if, "io_backend_fromFtq_wen", None))
        return BackendStartupObservation(
            startup_progress=bool(from_ftq_wen),
            from_ftq_ftq_idx=self._read(getattr(self.from_ftq_if, "io_backend_fromFtq_ftqIdx", None)),
            from_ftq_start_pc_addr=self._read(getattr(self.from_ftq_if, "io_backend_fromFtq_startPc_addr", None)),
            ibuf_full=self._read(getattr(self.frontend_info_if, "io_frontendInfo_ibufFull", None)),
        )

    def startup_progressed(self) -> bool:
        return bool(self.observe_startup().startup_progress)

    def snapshot(self) -> BackendObservationSnapshot:
        startup = self.observe_startup()
        return BackendObservationSnapshot(
            from_ftq_wen=self._read(getattr(self.from_ftq_if, "io_backend_fromFtq_wen", None)),
            from_ftq_ftq_idx=int(startup.from_ftq_ftq_idx),
            from_ftq_start_pc_addr=int(startup.from_ftq_start_pc_addr),
            ibuf_full=int(startup.ibuf_full),
        )


__all__ = ["BackendObserveMonitor", "BackendStartupObservation"]
