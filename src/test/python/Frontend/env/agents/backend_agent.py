from __future__ import annotations

from typing import Iterable, Mapping, Optional

from ..model.backend_state import FtqEntry, ResolveEntry
from ..signal_utils import set_sig


class BackendAgent:
    def __init__(self) -> None:
        self.dut = None

    def bind(self, dut) -> None:
        self.dut = dut

    @staticmethod
    def _encode_backend_addr(addr: int) -> int:
        return int(addr) >> 1

    def clear_one_shot_signals(self) -> None:
        assert self.dut is not None
        set_sig(self.dut, "io_backend_toFtq_redirect_valid", 0)
        for channel in range(3):
            set_sig(self.dut, f"io_backend_toFtq_resolve_{channel}_valid", 0)

    def start_cycle(self, can_accept: int) -> None:
        assert self.dut is not None
        set_sig(self.dut, "io_backend_canAccept", int(can_accept))
        self.clear_one_shot_signals()

    def drive_commit(self, entry: Optional[FtqEntry]) -> None:
        assert self.dut is not None
        set_sig(self.dut, "io_backend_toFtq_commit_valid", 0)
        if entry is None:
            return
        set_sig(self.dut, "io_backend_toFtq_commit_bits_flag", int(entry.ftq_flag))
        set_sig(self.dut, "io_backend_toFtq_commit_bits_value", int(entry.ftq_value))
        set_sig(self.dut, "io_backend_toFtq_commit_valid", 1)

    def drive_resolves(self, entries: Iterable[ResolveEntry]) -> None:
        assert self.dut is not None
        for channel, entry in enumerate(list(entries)[:3]):
            set_sig(self.dut, f"io_backend_toFtq_resolve_{channel}_bits_ftqIdx_flag", int(entry.ftq_flag))
            set_sig(self.dut, f"io_backend_toFtq_resolve_{channel}_bits_ftqIdx_value", int(entry.ftq_value))
            set_sig(self.dut, f"io_backend_toFtq_resolve_{channel}_bits_ftqOffset", int(entry.ftq_offset))
            set_sig(self.dut, f"io_backend_toFtq_resolve_{channel}_bits_pc_addr", self._encode_backend_addr(entry.pc))
            set_sig(
                self.dut,
                f"io_backend_toFtq_resolve_{channel}_bits_target_addr",
                self._encode_backend_addr(entry.target),
            )
            set_sig(self.dut, f"io_backend_toFtq_resolve_{channel}_bits_taken", int(entry.taken))
            set_sig(self.dut, f"io_backend_toFtq_resolve_{channel}_bits_mispredict", int(entry.mispredict))
            set_sig(
                self.dut,
                f"io_backend_toFtq_resolve_{channel}_bits_attribute_branchType",
                int(entry.branch_type),
            )
            set_sig(
                self.dut,
                f"io_backend_toFtq_resolve_{channel}_bits_attribute_rasAction",
                int(entry.ras_action),
            )
            set_sig(self.dut, f"io_backend_toFtq_resolve_{channel}_valid", 1)

    def drive_redirect(self, payload: Mapping[str, int]) -> None:
        assert self.dut is not None
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_pc", int(payload.get("pc", 0)))
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_target", int(payload.get("target_pc", 0)))
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_taken", int(payload.get("taken", 1)))
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_ftqIdx_flag", int(payload.get("ftq_flag", 0)))
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_ftqIdx_value", int(payload.get("ftq_value", 0)))
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_ftqOffset", int(payload.get("ftq_offset", 0)))
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_isRVC", int(payload.get("is_rvc", 0)))
        set_sig(
            self.dut,
            "io_backend_toFtq_redirect_bits_attribute_branchType",
            int(payload.get("branch_type", 0)),
        )
        set_sig(
            self.dut,
            "io_backend_toFtq_redirect_bits_attribute_rasAction",
            int(payload.get("ras_action", 0)),
        )
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_level", int(payload.get("level", 0)))
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_backendIGPF", 0)
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_backendIPF", 0)
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_backendIAF", 0)
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_debugIsCtrl", 1)
        set_sig(self.dut, "io_backend_toFtq_redirect_bits_debugIsMemVio", 0)
        set_sig(self.dut, "io_backend_toFtq_redirect_valid", 1)
