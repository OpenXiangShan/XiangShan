"""Sequential path transitions driven by executable CFI bytes and a golden trace."""

from __future__ import annotations

from types import SimpleNamespace

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.sequences import LoadProgramSequence, RunUntilGoldenTraceCompleteSequence
from env.support import PmpPmaConfig, fold_pc
from tests.py.support import uncache_scenarios as uncache


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_CACHEABLE_VA = uncache._NORMAL_ALT_BASE
_NC_VA = uncache._NORMAL_BASE
_MMIO_VA = uncache._MMIO_BASE
_MMIO_PA = uncache._MMIO_BASE
_PAGE_SIZE = uncache._SV39_PAGE_SIZE
_CACHEABLE_REGION_BASE = 0x8000_0000
_NOP = 0x00000013
_PATH_PAGES = {
    "cacheable": (_CACHEABLE_VA, uncache._NORMAL_ALT_PHYS_BASE, 0),
    "nc": (_NC_VA, uncache._NORMAL_PHYS_BASE, 1),
    "mmio": (_MMIO_VA, _MMIO_PA, 2),
}
_PATH_SEQUENCE = ("nc", "cacheable", "mmio", "nc", "cacheable", "nc", "mmio")


def _jump_words(pc: int, target: int) -> tuple[int, int]:
    """AUIPC x6 + JALR x0,x6,lo12; x6 carries no RAS call/return hint."""
    delta = int(target) - int(pc)
    if target & 1 or not -(1 << 31) <= delta < (1 << 31):
        raise ValueError("path jump target is outside the aligned AUIPC/JALR range")
    high = (delta + 0x800) >> 12
    low = delta - (high << 12)
    return (((high & 0xFFFFF) << 12) | (6 << 7) | 0x17,
            ((low & 0xFFF) << 20) | (6 << 15) | 0x67)


def _path_transition_program():
    """Build the original seven path visits at distinct executable offsets."""
    images = {path: bytearray(_NOP.to_bytes(4, "little") * (_PAGE_SIZE // 4))
              for path in _PATH_PAGES}
    counts = {path: 0 for path in _PATH_PAGES}
    phases = []
    for path in _PATH_SEQUENCE:
        va, pa, pbmt = _PATH_PAGES[path]
        offset = counts[path] * 64
        counts[path] += 1
        phases.append({"path": path, "pc": va + offset, "pa": pa + offset,
                       "offset": offset, "pbmt": pbmt, "pmp_mmio": int(path == "mmio")})
    entries = []
    for index, phase in enumerate(phases):
        pc, offset = phase["pc"], phase["offset"]
        for word in range(8):
            entries.append(TraceEntry(len(entries), pc + word * 4, _NOP, 4))
        branch_pc = pc + 32
        if index + 1 < len(phases):
            target = phases[index + 1]["pc"]
            auipc, jalr = _jump_words(branch_pc, target)
            images[phase["path"]][offset + 32:offset + 40] = (
                auipc.to_bytes(4, "little") + jalr.to_bytes(4, "little"))
            entries.append(TraceEntry(len(entries), branch_pc, auipc, 4))
            entries.append(TraceEntry(len(entries), branch_pc + 4, jalr, 4,
                                      "jump_indirect", True, target))
        else:
            # End the finite checked prefix on a complete CFI/FTQ boundary.
            # The executable program may loop here; no artificial redirect or
            # backend queue deletion is used to claim trace completion.
            images[phase["path"]][offset + 32:offset + 36] = (0x6F).to_bytes(4, "little")
            entries.append(TraceEntry(len(entries), branch_pc, 0x6F, 4, "jump", True, branch_pc))
    return {path: bytes(data) for path, data in images.items()}, phases, GoldenTrace(entries)


def _prepare_three_path_stream(env):
    images, phases, trace = _path_transition_program()
    env.page_table.clear()
    env.memory.mmio_ranges.append((_MMIO_PA, _MMIO_PA + _PAGE_SIZE - 1))
    for path, (va, pa, pbmt) in _PATH_PAGES.items():
        env.page_table.map_page(va >> 12, pa >> 12, v=1, r=1, x=1, pbmt=pbmt)
        LoadProgramSequence(image=ProgramImage(payload=images[path], base_addr=pa),
                            step_cycles=0).run(env)
    env.ptw_agent.configure(mode="sv39", response_source="model", compare_drive_source="model")
    return images, phases, trace


def _configure_path_attributes(env, trace):
    uncache._configure_exec_attrs_16k(env, base_addr=_CACHEABLE_REGION_BASE)
    env.write_pmp_entry(1, PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                        _MMIO_PA, size=_PAGE_SIZE, settle_cycles=4)
    env.write_pma_entry(1, PmpPmaConfig(match="napot", read=True, write=True, execute=True,
                                     cacheable=False),
                        _MMIO_PA, size=_PAGE_SIZE, settle_cycles=4)
    env.backend_model.set_golden_trace(trace)



def _initialize_path_stream(env, trace):
    """Keep PTW request handshakes blocked until real post-reset CSR writes land."""
    env.ptw_agent.configure(req_ready_strategy="periodic", req_ready_high_cycles=0,
                            req_ready_low_cycles=1)
    baseline = env.ptw_agent.get_stats()
    native = {}
    native_sources = {}

    def observe_permissions(_cycle, active_env):
        recorder = active_env.functional_coverage
        for kind in ("pmp", "pma"):
            # Read the actual packed CSR registers. Short io_pma names may
            # resolve to toggle-history storage in MEM_DIRECT; they are not
            # evidence of the current permission bits.
            prefix = "Frontend_top.Frontend.inner_pmp."
            cfg_path = prefix + kind + "Mapping_cfgMerged_0"
            cfg_word = recorder._try_read_dut_signal(active_env.dut, cfg_path)
            assert cfg_word is not None, {"missing_native_path_permission": cfg_path}
            native_sources[cfg_path] = int(cfg_word)
            for index in (0, 1):
                cfg_byte = (int(cfg_word) >> (8 * index)) & 0xFF
                fields = {"cfg_a": (cfg_byte >> 3) & 3, "cfg_r": cfg_byte & 1,
                          "cfg_w": (cfg_byte >> 1) & 1, "cfg_x": (cfg_byte >> 2) & 1}
                if kind == "pma":
                    fields["cfg_c"] = (cfg_byte >> 6) & 1
                for field, value in fields.items():
                    native[f"io_{kind}_{index}_{field}"] = value
                addr_path = prefix + f"{kind}Mapping_addr_{index}"
                addr = recorder._try_read_dut_signal(active_env.dut, addr_path)
                assert addr is not None, {"missing_native_path_permission": addr_path}
                native_sources[addr_path] = int(addr)
                native[f"io_{kind}_{index}_addr"] = int(addr)
        native["reset"] = int(active_env.dut.reset.value)

    env.register_cycle_observer(observe_permissions)
    env.initialize(reset_vector=_NC_VA, bare_mode=False, reset_cycles=20)
    # The frontend reset tree also needs to release. No PTW request can be
    # accepted while the configured public ready policy holds ready low.
    env.step(8)
    assert int(env.dut.reset.value) == native["reset"] == 0, native
    _configure_path_attributes(env, trace)
    after = env.ptw_agent.get_stats()
    assert (after["req_count"], after["resp_count"]) == (
        baseline["req_count"], baseline["resp_count"]), (baseline, after)
    for kind in ("pmp", "pma"):
        for index, (base, size) in enumerate(((_CACHEABLE_REGION_BASE, 0x4000),
                                             (_MMIO_PA, _PAGE_SIZE))):
            expected = {"cfg_a": 3, "cfg_r": 1, "cfg_w": 1, "cfg_x": 1,
                        "addr": (base >> 2) | ((size >> 3) - 1)}
            if kind == "pma":
                expected["cfg_c"] = int(index == 0)
            for field, value in expected.items():
                assert native[f"io_{kind}_{index}_{field}"] == value, (kind, index, expected, native)
    env._emit_event("ifu.path_transition_post_reset_permissions", {
        "native": dict(native), "native_register_sources": dict(native_sources),
        "ptw_before": baseline, "ptw_after": after,
        "contract": "post-reset distributed CSR writes precede first PTW handshake",
    })
    env.ptw_agent.configure(req_ready_strategy="always")


def _capture_path_evidence(env, phases):
    deliveries, gates = {}, []
    by_pc = {phase["pc"]: phase for phase in phases}
    previous_gate = None

    def capture_gate(cycle):
        nonlocal previous_gate
        if int(env.dut.reset.value):
            previous_gate = None
            return
        recorder = env.functional_coverage

        def read_live(dut, path):
            signal = recorder._lookup_dut_signal(dut, path)
            value = None if signal is None else getattr(signal, "value", None)
            return None if value is None else int(value)

        # Sample after env.step returns, when the simulator has completed the
        # cycle. The callback does not reuse a rising-edge snapshot.
        live = SimpleNamespace(_try_read_dut_signal=read_live)
        paths = {}

        def read(stem):
            value, path = _read_ifu_internal_with_path(live, env.dut, stem)
            assert value is not None and path is not None, {"missing_path_transition_probe": stem}
            paths[stem] = path
            return int(value)

        state = read("uncacheUnit.uncacheState")
        if state not in (1, 2):
            previous_gate = None
            return
        first = read("isFirstInstr")
        # The generated RTL connects emptyAfter to these two inputs. Its
        # optimized child-port storage is not a reliable cycle probe.
        backend = int(env.dut.io_backend_backendEmpty.value)
        ibuffer = read("io_ibufferEmpty")
        assert backend in (0, 1) and ibuffer in (0, 1)
        empty = backend & ibuffer
        gate = {"cycle": int(cycle), "state": state, "first": first,
                "backend_empty": backend, "ibuffer_empty": ibuffer, "empty_after": empty,
                "sample_phase": "post_step", "empty_after_source": "backend_and_ibuffer",
                "signal_paths": paths}
        if previous_gate is not None and previous_gate["state"] == 1 and state == 2:
            assert int(cycle) == previous_gate["cycle"] + 1, previous_gate
            assert previous_gate["first"] == 1 or previous_gate["empty_after"] == 1, previous_gate
            gate["wait_last_commit_released"] = True
        gates.append(gate)
        previous_gate = gate

    def capture(cycle, active_env):
        if int(active_env.dut.reset.value):
            return
        recorder = active_env.functional_coverage
        def read(stem):
            value, _path = _read_ifu_internal_with_path(recorder, active_env.dut, stem)
            assert value is not None, {"missing_path_transition_probe": stem}
            return int(value)
        if read("io_toIBuffer_valid") != 1 or read("io_toIBuffer_ready") != 1:
            return
        valid, enq = read("io_toIBuffer_bits_valid"), read("io_toIBuffer_bits_enqEnable")
        assert 0 <= enq < (1 << 36) and enq & ~valid == 0
        for lane in range(36):
            if not ((valid & enq) >> lane) & 1:
                continue
            pc = read(f"s2_alignedInstrPcVec_{lane}_addr") << 1
            if pc not in by_pc or pc in deliveries:
                continue
            phase = by_pc[pc]
            pbmt, mmio = read("s2_icacheMeta_0_itlbPbmt"), read("s2_icacheMeta_0_pmpMmio")
            assert (pbmt, mmio) == (phase["pbmt"], phase["pmp_mmio"]), (phase, pbmt, mmio)
            assert read("s2_prevEndIsHalfRviInfo_valid") == 0
            assert read(f"io_toIBuffer_bits_instrs_{lane}") == _NOP
            assert read(f"io_toIBuffer_bits_foldpc_{lane}") == fold_pc(pc)
            assert (read("io_toIBuffer_bits_exceptionMask") & valid & enq) == 0 or read("io_toIBuffer_bits_exceptionType_value") == 0
            deliveries[pc] = {"cycle": int(cycle), "pc": pc, "path": phase["path"],
                              "pbmt": pbmt, "pmp_mmio": mmio, "lane": lane,
                              "ftq": [read(f"io_toIBuffer_bits_ftqPtr_{lane}_{f}")
                                      for f in ("flag", "value")]}
    env.register_cycle_observer(capture)
    return deliveries, gates, capture_gate


class _PostStepObserverEnv:
    """Observe completed simulation steps without changing the shared runtime."""

    def __init__(self, env, observer):
        self._env = env
        self._observer = observer

    def __getattr__(self, name):
        return getattr(self._env, name)

    def step(self, cycles):
        if int(cycles) != 1:
            raise ValueError("path gate evidence requires single-cycle steps")
        result = self._env.step(1)
        # Inputs driven in clock callbacks need a combinational evaluation
        # before comparing them with an internal derived DUT signal.
        self._env.dut.RefreshComb()
        self._observer(self._env.current_cycle)
        return result


@pytest.mark.funcov_bins("BIN-957")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_ifu_owner_sequential_cacheable_nc_mmio_transitions(env) -> None:
    _images, phases, trace = _prepare_three_path_stream(env)
    env.icache_agent.configure(hit_latency=8, miss_latency=8, miss_rate=0.0, seed=0x957)
    env.uncache_agent.configure(latency=8, mmio_latency=8)
    _initialize_path_stream(env, trace)
    deliveries, gates, observe_gate = _capture_path_evidence(env, phases)
    observed_env = _PostStepObserverEnv(env, observe_gate)
    result = RunUntilGoldenTraceCompleteSequence(max_cycles=24000, stagnant_cycles_limit=8000).run(observed_env)
    assert result.status == "completed" and result.completed is True, result
    assert result.cursor == len(trace.entries) and result.pending_work == 0, result
    assert result.monitor_error_count == 0
    assert set(deliveries) == {phase["pc"] for phase in phases}, (phases, deliveries)
    assert [deliveries[p["pc"]]["cycle"] for p in phases] == sorted(
        deliveries[p["pc"]]["cycle"] for p in phases)
    assert any(g.get("wait_last_commit_released") for g in gates), gates
    observed = {int(item.pc) for item in env.monitor.observations}
    assert {phase["pc"] for phase in phases} <= observed
    owner_state = env.functional_coverage._ifu_mmio_nc_owner_state
    assert env.functional_coverage.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_059"), {
        "seen": sorted(owner_state.get("nc_seen", ())),
        "path_transition_observations": owner_state.get("path_transition_observations"),
        "deliveries": deliveries, "gates": gates,
    }
    env._emit_event("ifu.path_transition_architectural_completion", {
        "phases": phases, "deliveries": list(deliveries.values()), "wait_last_commit": gates,
        "trace_cursor": result.cursor, "trace_entries": len(trace.entries),
        "completion_pending_work": result.pending_work,
        "stimulus": "executed AUIPC/JALR sequence; no explicit redirect or queue clearing",
    })
    assert env.translation_oracle.get_stats()["error_count"] == 0
    assert not env.monitor.get_errors()
    assert not env.get_errors()
