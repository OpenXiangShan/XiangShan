from __future__ import annotations

from types import SimpleNamespace

from env import api as env_api
from env.runtime.pylib import frontend_offset_path


class _ApiTargetCursorEnv:
    def __init__(self) -> None:
        self.backend_model = SimpleNamespace()


class _Pin:
    def __init__(self, value: int) -> None:
        self.value = int(value)


class _SnapshotDut:
    def __init__(self) -> None:
        self.signals: dict[str, _Pin] = {}
        self.read_names: list[str] = []

    def set(self, name: str, value: int) -> None:
        self.signals[str(name)] = _Pin(value)

    def __getattr__(self, name: str):
        self.read_names.append(str(name))
        try:
            return self.signals[str(name)]
        except KeyError as exc:
            raise AttributeError(name) from exc

    def GetInternalSignal(self, name: str):  # noqa: N802 - DUT API spelling
        self.read_names.append(str(name))
        try:
            return self.signals[str(name)]
        except KeyError as exc:
            raise KeyError(name) from exc


def _current_s2_snapshot_dut() -> _SnapshotDut:
    dut = _SnapshotDut()
    prefix = "Frontend_top.Frontend.inner_ifu."
    scalar_values = {
        "s2_ready": 1,
        "s2_valid_valid": 1,
        "s2_fire": 1,
        "s2_fetchBlock_0_valid": 1,
        "s2_fetchBlock_1_valid": 1,
        "s2_fetchBlock_0_ftqIdx_flag": 0,
        "s2_fetchBlock_0_ftqIdx_value": 7,
        "s2_fetchBlock_1_ftqIdx_flag": 1,
        "s2_fetchBlock_1_ftqIdx_value": 9,
        "s2_fetchBlock_0_startVAddr_addr": 0x100,
        "s2_fetchBlock_1_startVAddr_addr": 0x120,
        "s2_alignedInstrValid": (1 << 35) | 0b111,
        "s2_fixedInstrValid": (1 << 35) | 0b011,
        "s2_prevEndIsHalfRviInfo_valid": 1,
        "s2_firstEndHalfRvi_valid": 0,
        "s2_totalEndHalfRvi_valid": 1,
        "io_toIBuffer_bits_enqEnable_0": (1 << 35) | 0b011,
    }
    for name, value in scalar_values.items():
        dut.set(prefix + name, value)
    for lane in range(35):
        dut.set(prefix + f"s2_alignedInstrVec_{lane}_invalidTaken", int(lane == 2))
        dut.set(prefix + f"s2_alignedInstrVec_{lane}_blockSel", int(lane == 1))
        dut.set(prefix + f"s2_alignedInstrVec_{lane}_isCrossBlockInstr", int(lane == 0))
    return dut


def _snapshot_env(dut: _SnapshotDut) -> SimpleNamespace:
    return SimpleNamespace(
        backend_model=SimpleNamespace(),
        dut=dut,
        monitor=None,
    )


def test_run_until_golden_complete_passes_target_cursor_env_var(monkeypatch) -> None:
    captured = {}

    def fake_run_until_golden_trace_complete(env, **kwargs):
        captured["env"] = env
        captured.update(kwargs)
        return SimpleNamespace(
            ok=True,
            completed=False,
            status="cursor_target",
            cycles_run=5,
            cursor=12,
            total_entries=99,
            pending_work=0,
            monitor_error_count=0,
        )

    monkeypatch.setenv("TB_TRACE_TARGET_CURSOR", "12")
    monkeypatch.setattr(env_api, "run_until_golden_trace_complete", fake_run_until_golden_trace_complete)

    env = _ApiTargetCursorEnv()
    result = env_api.api_Frontend_run_until_golden_complete(env, max_cycles=100)

    assert result is True
    assert captured["env"] is env
    assert captured["target_cursor"] == 12


def test_stall_snapshot_uses_registered_s2_alignment_and_effective_owner() -> None:
    dut = _current_s2_snapshot_dut()

    snapshot = env_api.api_Frontend_capture_frontend_stall_snapshot(_snapshot_env(dut))
    ifu = snapshot["ifu_runtime"]

    assert ifu["probe_contract"] == {"complete": True, "missing": []}
    assert ifu["s2_valid"] == 1
    assert ifu["s2_start_pc"] == 0x200
    assert ifu["s2_second_start_pc"] == 0x240
    assert ifu["s2_block_valid_mask"] == 0b11
    assert ifu["s2_block_ftq"] == [{"flag": 0, "value": 7}, {"flag": 1, "value": 9}]
    assert ifu["s2_instr_valid_mask"] == (1 << 35) | 0b111
    assert ifu["s2_fixed_instr_valid_mask"] == (1 << 35) | 0b011
    assert ifu["s2_invalid_taken_mask"] == 0b100
    assert ifu["s2_raw_block_sel_mask"] == 0b010
    assert ifu["s2_cross_block_mask"] == 0b001
    assert ifu["s2_effective_owner_mask"] == 0b011
    assert ifu["s2_effective_owner_bits"][0] == 1
    assert ifu["s2_raw_block_sel_bits"][0] == 0
    assert ifu["s2_cross_block_bits"][0] == 1
    assert ifu["s2_half_rvi_valid"] == {"previous": 1, "first": 0, "total": 1}

    formatted = env_api._format_stall_snapshot(snapshot)
    assert "s2=[0x200,0x240]" in formatted
    assert "raw_block=0x2 cross_block=0x1 effective_owner=0x3" in formatted
    assert "contract=ok" in formatted
    assert not any("s3_align" in name for name in dut.read_names)
    assert not any("fixedTwoFetchRange" in name for name in dut.read_names)


def test_stall_snapshot_reports_missing_current_owner_probe() -> None:
    dut = _current_s2_snapshot_dut()
    missing = "Frontend_top.Frontend.inner_ifu.s2_alignedInstrVec_0_isCrossBlockInstr"
    dut.signals.pop(missing)

    snapshot = env_api.api_Frontend_capture_frontend_stall_snapshot(_snapshot_env(dut))
    contract = snapshot["ifu_runtime"]["probe_contract"]

    assert contract["complete"] is False
    assert contract["missing"] == [missing]
    assert f"contract=missing[{missing}]" in env_api._format_stall_snapshot(snapshot)


def test_stall_snapshot_signal_contract_matches_current_inventory() -> None:
    offset = frontend_offset_path()
    assert offset.is_file(), "Verilator DUT signal inventory is required"
    inventory = offset.read_text(encoding="utf-8")
    prefix = "Frontend_top.Frontend.inner_ifu."
    required = {
        prefix + "s2_ready",
        prefix + "s2_valid_valid",
        prefix + "s2_fire",
        prefix + "s2_alignedInstrValid",
        prefix + "s2_fixedInstrValid",
        prefix + "s2_fetchBlock_0_startVAddr_addr",
        prefix + "s2_fetchBlock_1_startVAddr_addr",
        prefix + "s2_prevEndIsHalfRviInfo_valid",
        prefix + "s2_firstEndHalfRvi_valid",
        prefix + "s2_totalEndHalfRvi_valid",
        prefix + "io_toIBuffer_bits_enqEnable_0",
    }
    for block in range(2):
        required.update(
            {
                prefix + f"s2_fetchBlock_{block}_valid",
                prefix + f"s2_fetchBlock_{block}_ftqIdx_flag",
                prefix + f"s2_fetchBlock_{block}_ftqIdx_value",
            }
        )
    for lane in range(35):
        required.update(
            {
                prefix + f"s2_alignedInstrVec_{lane}_invalidTaken",
                prefix + f"s2_alignedInstrVec_{lane}_blockSel",
            }
        )
        if lane < 34:
            required.add(prefix + f"s2_alignedInstrVec_{lane}_isCrossBlockInstr")

    missing = sorted(name for name in required if f"  - name: {name}\n" not in inventory)
    assert not missing, {"missing_stall_snapshot_signals": missing}
    assert prefix + "s3_alignInstrValid_" not in inventory
    assert prefix + "s3_alignInvalidTaken_" not in inventory
    assert prefix + "s3_alignCompactInfo_selectBlock_" not in inventory
    assert "predChecker.io_resp_stage1Out_fixedTwoFetchRange_" not in inventory
