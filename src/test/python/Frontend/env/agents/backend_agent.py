from __future__ import annotations

from typing import Iterable, Mapping, Optional

from ..bundles import BackendCtrlBundle, bind_bundle_optional
from ..model.backend_state import FtqEntry, ResolveEntry


class BackendAgent:
    def __init__(self) -> None:
        self.interface = None

    @staticmethod
    def _write(signal, value: int) -> None:
        try:
            signal.value = int(value)
        except Exception:
            return

    def bind(self, target) -> None:
        if isinstance(target, BackendCtrlBundle):
            self.interface = target
            return
        self.interface = bind_bundle_optional(BackendCtrlBundle, target)

    @staticmethod
    def _encode_backend_addr(addr: int) -> int:
        return int(addr) >> 1

    def clear_one_shot_signals(self) -> None:
        assert self.interface is not None
        self._write(self.interface.redirect_valid, 0)
        for channel in range(3):
            self._write(self.interface.resolve_valid[channel], 0)

    def start_cycle(self, can_accept: int) -> None:
        assert self.interface is not None
        self._write(self.interface.can_accept, int(can_accept))
        self.clear_one_shot_signals()

    def drive_commit(self, entry: Optional[FtqEntry]) -> None:
        assert self.interface is not None
        self._write(self.interface.commit_valid, 0)
        if entry is None:
            return
        self._write(self.interface.commit_bits_flag, int(entry.ftq_flag))
        self._write(self.interface.commit_bits_value, int(entry.ftq_value))
        self._write(self.interface.commit_valid, 1)

    def drive_resolves(self, entries: Iterable[ResolveEntry]) -> None:
        assert self.interface is not None
        for channel, entry in enumerate(list(entries)[:3]):
            self._write(self.interface.resolve_bits_ftq_idx_flag[channel], int(entry.ftq_flag))
            self._write(self.interface.resolve_bits_ftq_idx_value[channel], int(entry.ftq_value))
            self._write(self.interface.resolve_bits_ftq_offset[channel], int(entry.ftq_offset))
            self._write(self.interface.resolve_bits_pc_addr[channel], self._encode_backend_addr(entry.pc))
            self._write(self.interface.resolve_bits_target_addr[channel], self._encode_backend_addr(entry.target))
            self._write(self.interface.resolve_bits_taken[channel], int(entry.taken))
            self._write(self.interface.resolve_bits_mispredict[channel], int(entry.mispredict))
            self._write(self.interface.resolve_bits_attribute_branch_type[channel], int(entry.branch_type))
            self._write(self.interface.resolve_bits_attribute_ras_action[channel], int(entry.ras_action))
            self._write(self.interface.resolve_valid[channel], 1)

    def drive_redirect(self, payload: Mapping[str, int]) -> None:
        assert self.interface is not None
        self._write(self.interface.redirect_bits_pc, int(payload.get("pc", 0)))
        self._write(self.interface.redirect_bits_target, int(payload.get("target_pc", 0)))
        self._write(self.interface.redirect_bits_taken, int(payload.get("taken", 1)))
        self._write(self.interface.redirect_bits_ftq_idx_flag, int(payload.get("ftq_flag", 0)))
        self._write(self.interface.redirect_bits_ftq_idx_value, int(payload.get("ftq_value", 0)))
        self._write(self.interface.redirect_bits_ftq_offset, int(payload.get("ftq_offset", 0)))
        self._write(self.interface.redirect_bits_is_rvc, int(payload.get("is_rvc", 0)))
        self._write(self.interface.redirect_bits_attribute_branch_type, int(payload.get("branch_type", 0)))
        self._write(self.interface.redirect_bits_attribute_ras_action, int(payload.get("ras_action", 0)))
        self._write(self.interface.redirect_bits_level, int(payload.get("level", 0)))
        self._write(self.interface.redirect_bits_backend_igpf, 0)
        self._write(self.interface.redirect_bits_backend_ipf, 0)
        self._write(self.interface.redirect_bits_backend_iaf, 0)
        self._write(self.interface.redirect_bits_debug_is_ctrl, 1)
        self._write(self.interface.redirect_bits_debug_is_mem_vio, 0)
        self._write(self.interface.redirect_valid, 1)
