from __future__ import annotations

from typing import Iterable, Mapping, Optional

from ..bundles import BackendCtrlBundle
from ..model.backend_state import FtqEntry, ResolveEntry


class BackendAgent:
    def __init__(self) -> None:
        self.interface = None
        self._drive_if = None

    @staticmethod
    def _write(signal, value: int) -> None:
        try:
            signal.value = int(value)
        except Exception:
            return

    def bind(self, target) -> None:
        if not isinstance(target, BackendCtrlBundle):
            raise TypeError(f"BackendAgent.bind requires a backend control interface, got {type(target).__name__}")
        self.interface = target
        self._drive_if = target

    @staticmethod
    def _encode_backend_addr(addr: int) -> int:
        return int(addr) >> 1

    def _assert_ftq_idx_ahead_unused(self) -> None:
        assert self._drive_if is not None
        value = getattr(self._drive_if.ftq_idx_ahead_0_valid, "value", 0)
        if int(value) != 0:
            raise AssertionError("ftqIdxAhead(0) is not modeled by the frontend env drive path")

    def clear_one_shot_signals(self) -> None:
        assert self._drive_if is not None
        self._write(self._drive_if.redirect_valid, 0)
        for channel in range(3):
            self._write(self._drive_if.resolve_valid[channel], 0)
        for lane in range(8):
            self._write(self._drive_if.call_ret_commit_valid[lane], 0)
            self._write(self._drive_if.call_ret_commit_bits_ras_action[lane], 0)
            self._write(self._drive_if.call_ret_commit_bits_ftq_ptr_value[lane], 0)
        self._write(self._drive_if.ftq_idx_ahead_0_valid, 0)

    def start_cycle(self, can_accept: int) -> None:
        assert self._drive_if is not None
        self._assert_ftq_idx_ahead_unused()
        self._write(self._drive_if.can_accept, int(can_accept))
        self.clear_one_shot_signals()

    def drive_commit(self, entry: Optional[FtqEntry]) -> None:
        assert self._drive_if is not None
        self._write(self._drive_if.commit_valid, 0)
        if entry is None:
            return
        self._write(self._drive_if.commit_bits_flag, int(entry.ftq_flag))
        self._write(self._drive_if.commit_bits_value, int(entry.ftq_value))
        self._write(self._drive_if.commit_valid, 1)

    def drive_resolves(self, entries: Iterable[ResolveEntry]) -> None:
        assert self._drive_if is not None
        for channel, entry in enumerate(list(entries)[:3]):
            self._write(self._drive_if.resolve_bits_ftq_idx_flag[channel], int(entry.ftq_flag))
            self._write(self._drive_if.resolve_bits_ftq_idx_value[channel], int(entry.ftq_value))
            self._write(self._drive_if.resolve_bits_ftq_offset[channel], int(entry.ftq_offset))
            self._write(self._drive_if.resolve_bits_pc_addr[channel], self._encode_backend_addr(entry.pc))
            self._write(self._drive_if.resolve_bits_target_addr[channel], self._encode_backend_addr(entry.target))
            self._write(self._drive_if.resolve_bits_taken[channel], int(entry.taken))
            self._write(self._drive_if.resolve_bits_mispredict[channel], int(entry.mispredict))
            self._write(self._drive_if.resolve_bits_attribute_branch_type[channel], int(entry.branch_type))
            self._write(self._drive_if.resolve_bits_attribute_ras_action[channel], int(entry.ras_action))
            self._write(self._drive_if.resolve_valid[channel], 1)

    def drive_call_ret_commit(self, group: Iterable[object]) -> None:
        assert self._drive_if is not None
        for lane, inst in enumerate(list(group)[:8]):
            self._write(self._drive_if.call_ret_commit_bits_ras_action[lane], int(getattr(inst, "ras_action", 0)))
            self._write(self._drive_if.call_ret_commit_bits_ftq_ptr_value[lane], int(getattr(inst, "ftq_value", 0)))
            self._write(self._drive_if.call_ret_commit_valid[lane], 1)

    def drive_redirect(self, payload: Mapping[str, int]) -> None:
        assert self._drive_if is not None
        self._write(self._drive_if.redirect_bits_pc, int(payload.get("pc", 0)))
        self._write(self._drive_if.redirect_bits_target, int(payload.get("target_pc", 0)))
        self._write(self._drive_if.redirect_bits_taken, int(payload.get("taken", 1)))
        self._write(self._drive_if.redirect_bits_ftq_idx_flag, int(payload.get("ftq_flag", 0)))
        self._write(self._drive_if.redirect_bits_ftq_idx_value, int(payload.get("ftq_value", 0)))
        self._write(self._drive_if.redirect_bits_ftq_offset, int(payload.get("ftq_offset", 0)))
        self._write(self._drive_if.redirect_bits_is_rvc, int(payload.get("is_rvc", 0)))
        self._write(self._drive_if.redirect_bits_attribute_branch_type, int(payload.get("branch_type", 0)))
        self._write(self._drive_if.redirect_bits_attribute_ras_action, int(payload.get("ras_action", 0)))
        self._write(self._drive_if.redirect_bits_level, int(payload.get("level", 0)))
        self._write(self._drive_if.redirect_bits_backend_igpf, int(payload.get("backend_igpf", 0)))
        self._write(self._drive_if.redirect_bits_backend_ipf, int(payload.get("backend_ipf", 0)))
        self._write(self._drive_if.redirect_bits_backend_iaf, int(payload.get("backend_iaf", 0)))
        self._write(self._drive_if.redirect_bits_debug_is_ctrl, 1)
        self._write(self._drive_if.redirect_bits_debug_is_mem_vio, 0)
        self._write(self._drive_if.redirect_valid, 1)
