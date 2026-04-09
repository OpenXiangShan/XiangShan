from __future__ import annotations

import logging
from pathlib import Path
from types import SimpleNamespace
from typing import Callable, Dict, Optional

from .agents.backend_agent import BackendAgent
from .agents.icache_agent import ICacheAgent
from .agents.ptw_agent import PTWAgent
from .agents.uncache_agent import UncacheAgent
from .backend_model import BackendModel
from .bundles import (
    BackendCtrlBundle,
    BackendFromFtqBundle,
    BackendObserveBundle,
    CSRControlBundle,
    ClockResetBundle,
    DFTControlBundle,
    FrontendInfoBundle,
    ICacheBundle,
    PTWBundle,
    UncacheBundle,
    bind_bundle_optional,
    bind_bundle_required,
)
from .env_config import DEFAULT_ENV_CONFIG, EnvConfig
from .logging_utils import configure_env_logging, parse_log_level
from .model import GoldenTrace, MemoryModel, PageTableModel
from .model.branch_checker import BranchChecker
from .monitors.backend_observe_monitor import BackendObserveMonitor
from .monitors.frontend_monitor import FrontendMonitor


class FrontendEnv:
    def __init__(
        self,
        dut,
        memory_model: Optional[MemoryModel] = None,
        page_table_model: Optional[PageTableModel] = None,
        register_callbacks: bool = True,
        log_level: Optional[str] = None,
        event_sink: Optional[Callable[[Dict], None]] = None,
        config: Optional[EnvConfig] = None,
        bp_ctrl_ubtb_enable: int = 1,
        bp_ctrl_abtb_enable: int = 1,
        bp_ctrl_mbtb_enable: int = 1,
        bp_ctrl_tage_enable: int = 1,
        bp_ctrl_sc_enable: int = 1,
        bp_ctrl_ittage_enable: int = 1,
    ) -> None:
        configure_env_logging(log_level)
        self.logger = logging.getLogger("env.frontend_env")
        self.dut = dut
        self.waveform_path: Optional[str] = None
        self.event_sink = event_sink
        self.config = config or DEFAULT_ENV_CONFIG
        self.current_cycle = 0
        self.bp_ctrl_ubtb_enable = 1 if int(bp_ctrl_ubtb_enable) else 0
        self.bp_ctrl_abtb_enable = 1 if int(bp_ctrl_abtb_enable) else 0
        self.bp_ctrl_mbtb_enable = 1 if int(bp_ctrl_mbtb_enable) else 0
        self.bp_ctrl_tage_enable = 1 if int(bp_ctrl_tage_enable) else 0
        self.bp_ctrl_sc_enable = 1 if int(bp_ctrl_sc_enable) else 0
        self.bp_ctrl_ittage_enable = 1 if int(bp_ctrl_ittage_enable) else 0
        self.memory = memory_model or MemoryModel()
        self.page_table = page_table_model or PageTableModel(mode=self.config.ptw.mode)
        self._create_interfaces()
        self._apply_collaborators(self._create_collaborators())
        self._configure_collaborators(explicit_page_table=page_table_model is not None)
        self._bind_collaborators()
        self._connect_collaborators()

        self._init_inputs()
        self._register_callbacks(register_callbacks)
        self.logger.info("frontend env initialized")
        self._emit_event("session.init", {"register_callbacks": bool(register_callbacks)})

    def _create_collaborators(self) -> Dict[str, object]:
        branch_checker = BranchChecker()
        return {
            "branch_checker": branch_checker,
            "monitor": FrontendMonitor(memory=self.memory, branch_checker=branch_checker),
            "backend_observe_monitor": BackendObserveMonitor(),
            "icache_agent": ICacheAgent(self.memory),
            "uncache_agent": UncacheAgent(self.memory),
            "ptw_agent": PTWAgent(self.page_table),
            "backend_agent": BackendAgent(),
            "backend_model": BackendModel(
                ftq_size=self.config.backend.ftq_size,
                ibuf_watchdog_threshold=self.config.backend.ibuf_watchdog_threshold,
                safe_pc=self.config.backend.safe_pc,
                resolve_min_delay=self.config.backend.resolve_min_delay,
                resolve_max_delay=self.config.backend.resolve_max_delay,
                auto_redirect_on_golden_mispredict=self.config.backend.auto_redirect_on_golden_mispredict,
            ),
        }

    def _apply_collaborators(self, collaborators: Dict[str, object]) -> None:
        self.branch_checker = collaborators["branch_checker"]
        self.monitor = collaborators["monitor"]
        self.backend_observe_monitor = collaborators["backend_observe_monitor"]
        self.icache_agent = collaborators["icache_agent"]
        self.uncache_agent = collaborators["uncache_agent"]
        self.ptw_agent = collaborators["ptw_agent"]
        self.backend_agent = collaborators["backend_agent"]
        self.backend_model = collaborators["backend_model"]

    def _configure_collaborators(self, explicit_page_table: bool = False) -> None:
        self.icache_agent.configure(
            hit_latency=self.config.icache.hit_latency,
            miss_latency=self.config.icache.miss_latency,
            miss_rate=self.config.icache.miss_rate,
            seed=self.config.icache.seed,
        )
        self.uncache_agent.configure(
            latency=self.config.uncache.latency,
            mmio_latency=self.config.uncache.mmio_latency,
        )
        self.ptw_agent.configure(
            latency=self.config.ptw.latency,
            mode=(None if explicit_page_table else self.config.ptw.mode),
        )

    @staticmethod
    def _write(signal, value: int) -> None:
        try:
            signal.value = int(value)
        except Exception:
            return

    def _backend_observe_bind_target(self):
        return SimpleNamespace(
            io_backend_fromFtq_wen=self.backend_from_ftq_if.io_backend_fromFtq_wen,
            io_backend_fromFtq_ftqIdx=self.backend_from_ftq_if.io_backend_fromFtq_ftqIdx,
            io_backend_fromFtq_startPc_addr=self.backend_from_ftq_if.io_backend_fromFtq_startPc_addr,
            io_frontendInfo_ibufFull=self.frontend_info_if.io_frontendInfo_ibufFull,
        )

    def _create_interfaces(self) -> None:
        self.clock_reset = bind_bundle_required(ClockResetBundle, self.dut)
        self.csr_ctrl_if = bind_bundle_required(CSRControlBundle, self.dut)
        self.dft_ctrl_if = bind_bundle_required(DFTControlBundle, self.dut)
        self.icache_if = bind_bundle_optional(ICacheBundle, self.dut)
        self.uncache_if = bind_bundle_optional(UncacheBundle, self.dut)
        self.ptw_if = bind_bundle_optional(PTWBundle, self.dut)
        self.backend_ctrl_if = bind_bundle_required(BackendCtrlBundle, self.dut)
        self.backend_observe_if = bind_bundle_optional(BackendObserveBundle, self.dut)
        self.backend_from_ftq_if = bind_bundle_optional(BackendFromFtqBundle, self.dut)
        self.frontend_info_if = bind_bundle_optional(FrontendInfoBundle, self.dut)

    def _bind_collaborators(self) -> None:
        self.icache_agent.bind(self.icache_if)
        self.uncache_agent.bind(self.uncache_if)
        self.ptw_agent.bind(self.ptw_if)
        self.backend_agent.bind(self.backend_ctrl_if)
        self.backend_observe_monitor.bind(self._backend_observe_bind_target())
        self.monitor.bind(self.backend_observe_if)
        if hasattr(self.backend_model, "bind_interfaces"):
            self.backend_model.bind_interfaces(
                drive_if=self.backend_ctrl_if,
                observe_if=self.backend_observe_if,
                from_ftq_if=self.backend_from_ftq_if,
                frontend_info_if=self.frontend_info_if,
            )
        else:
            self.backend_model.bind(self.dut)
        for collaborator in self._bindable_collaborators():
            collaborator.set_event_sink(self.event_sink)

    def _connect_collaborators(self) -> None:
        self.monitor.attach_backend_model(self.backend_model)
        self.backend_model.attach_env(self)
        self.backend_model.attach_monitor(self.monitor)
        self.backend_model.attach_branch_checker(self.branch_checker)

    def _register_callbacks(self, register_callbacks: bool) -> None:
        if register_callbacks:
            self.dut.StepRis(self._on_clock_edge)

    def _bindable_collaborators(self) -> tuple:
        return (
            self.icache_agent,
            self.uncache_agent,
            self.ptw_agent,
            self.monitor,
            self.backend_model,
        )

    def _drive_backend_cycle(self, cycle: int) -> None:
        self.backend_model.begin_cycle(cycle)
        observation = self.backend_observe_monitor.snapshot()
        self.backend_model.consume_backend_observation(observation)
        actions = self.backend_model.plan_cycle_actions()
        self.backend_agent.start_cycle(actions.can_accept)
        self.backend_agent.drive_commit(actions.commit_entry)
        self.backend_agent.drive_resolves(actions.resolve_entries)
        if actions.redirect_payload is not None:
            self.backend_agent.drive_redirect(actions.redirect_payload)

    def _emit_event(self, event_type: str, payload: Dict, level: str = "INFO") -> None:
        if self.event_sink is None:
            return
        evt = {
            "type": event_type,
            "source": "frontend_env",
            "cycle": int(self.current_cycle),
            "level": level,
            "payload": payload,
        }
        self.event_sink(evt)

    def _init_inputs(self) -> None:
        self.clock_reset.drive_idle(reset_vector_addr=0x40000000)
        self.csr_ctrl_if.drive_idle(
            ubtb_enable=self.bp_ctrl_ubtb_enable,
            abtb_enable=self.bp_ctrl_abtb_enable,
            mbtb_enable=self.bp_ctrl_mbtb_enable,
            tage_enable=self.bp_ctrl_tage_enable,
            sc_enable=self.bp_ctrl_sc_enable,
            ittage_enable=self.bp_ctrl_ittage_enable,
        )
        self.dft_ctrl_if.drive_idle()
        self.icache_if.drive_idle()
        self.uncache_if.drive_idle()
        self.ptw_if.drive_idle()
        self.backend_ctrl_if.drive_idle()

    def _on_clock_edge(self, cycle: int) -> None:
        self.current_cycle = int(cycle)
        self.icache_agent.on_clock_edge(cycle)
        self.uncache_agent.on_clock_edge(cycle)
        self.ptw_agent.on_clock_edge(cycle)
        self._drive_backend_cycle(cycle)
        self.monitor.on_clock_edge(cycle)
        self._emit_event("clock.tick", {"cycle": int(cycle)}, level="DEBUG")

    def step(self, cycles: int = 1) -> int:
        return self.dut.Step(int(cycles))

    def Step(self, cycles: int = 1) -> int:
        return self.step(cycles)

    def reset(self, cycles: int = 20) -> None:
        self.logger.info("reset dut: cycles=%d", int(cycles))
        self._emit_event("control.reset", {"cycles": int(cycles)})
        self._write(self.clock_reset.reset, 1)
        self.step(cycles)
        self._write(self.clock_reset.reset, 0)
        self.step(1)

    def initialize(self, reset_vector: int = 0x80000000, bare_mode: bool = True, reset_cycles: int = 20) -> None:
        self.logger.info(
            "initialize env: reset_vector=0x%x bare_mode=%s reset_cycles=%d",
            int(reset_vector),
            bool(bare_mode),
            int(reset_cycles),
        )
        self._write(self.clock_reset.io_reset_vector_addr, int(reset_vector) >> 1)
        if bare_mode:
            self.page_table.set_mode("bare")
            self._write(self.csr_ctrl_if.io_tlbCsr_satp_mode, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_vsatp_mode, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_hgatp_mode, 0)
        self.reset(reset_cycles)
        self.monitor.set_expected_pc(int(reset_vector))

    def load_program(self, data: bytes, base_addr: int) -> None:
        self.memory.load_bin(data, int(base_addr))
        self.logger.info("program loaded: base=0x%x size=%d", int(base_addr), len(data))
        self._emit_event("control.load_program", {"base_addr": int(base_addr), "size": len(data)})

    def load_program_file(self, path: str, base_addr: int) -> int:
        size = Path(path).stat().st_size
        self.memory.load_file(path, int(base_addr))
        self.logger.info("program file loaded: path=%s base=0x%x size=%d", path, int(base_addr), size)
        self._emit_event(
            "control.load_program",
            {"path": str(path), "base_addr": int(base_addr), "size": int(size)},
        )
        return int(size)

    def load_golden_trace_file(self, path: str) -> int:
        trace = GoldenTrace.from_file(str(path))
        self.backend_model.set_golden_trace(trace)
        self.logger.info("golden trace loaded: path=%s entries=%d", str(path), len(trace.entries))
        self._emit_event(
            "control.load_golden_trace",
            {"path": str(path), "entries": int(len(trace.entries))},
        )
        return int(len(trace.entries))

    def set_bp_ctrl_enable(
        self,
        ubtb_enable: Optional[int] = None,
        abtb_enable: Optional[int] = None,
        mbtb_enable: Optional[int] = None,
        tage_enable: Optional[int] = None,
        sc_enable: Optional[int] = None,
        ittage_enable: Optional[int] = None,
    ) -> dict:
        if ubtb_enable is not None:
            self.bp_ctrl_ubtb_enable = 1 if int(ubtb_enable) else 0
        if abtb_enable is not None:
            self.bp_ctrl_abtb_enable = 1 if int(abtb_enable) else 0
        if mbtb_enable is not None:
            self.bp_ctrl_mbtb_enable = 1 if int(mbtb_enable) else 0
        if tage_enable is not None:
            self.bp_ctrl_tage_enable = 1 if int(tage_enable) else 0
        if sc_enable is not None:
            self.bp_ctrl_sc_enable = 1 if int(sc_enable) else 0
        if ittage_enable is not None:
            self.bp_ctrl_ittage_enable = 1 if int(ittage_enable) else 0

        self._write(self.csr_ctrl_if.io_csrCtrl_bp_ctrl_ubtbEnable, self.bp_ctrl_ubtb_enable)
        self._write(self.csr_ctrl_if.io_csrCtrl_bp_ctrl_abtbEnable, self.bp_ctrl_abtb_enable)
        self._write(self.csr_ctrl_if.io_csrCtrl_bp_ctrl_mbtbEnable, self.bp_ctrl_mbtb_enable)
        self._write(self.csr_ctrl_if.io_csrCtrl_bp_ctrl_tageEnable, self.bp_ctrl_tage_enable)
        self._write(self.csr_ctrl_if.io_csrCtrl_bp_ctrl_scEnable, self.bp_ctrl_sc_enable)
        self._write(self.csr_ctrl_if.io_csrCtrl_bp_ctrl_ittageEnable, self.bp_ctrl_ittage_enable)

        cfg = {
            "ubtb_enable": int(self.bp_ctrl_ubtb_enable),
            "abtb_enable": int(self.bp_ctrl_abtb_enable),
            "mbtb_enable": int(self.bp_ctrl_mbtb_enable),
            "tage_enable": int(self.bp_ctrl_tage_enable),
            "sc_enable": int(self.bp_ctrl_sc_enable),
            "ittage_enable": int(self.bp_ctrl_ittage_enable),
        }
        self.logger.info("bp ctrl config updated: %s", cfg)
        self._emit_event("control.bp_ctrl_enable", cfg)
        return cfg

    def set_log_level(self, level: str) -> int:
        value = parse_log_level(level)
        logging.getLogger("env").setLevel(value)
        self.logger.info("env log level set to %s", str(level).upper())
        return value

    def set_waveform_path(self, path: str) -> str:
        p = Path(path)
        p.parent.mkdir(parents=True, exist_ok=True)
        self.dut.SetWaveform(str(p))
        self.waveform_path = str(p)
        self.logger.info("waveform path set: %s", self.waveform_path)
        self._emit_event("waveform.set_path", {"path": self.waveform_path})
        return self.waveform_path

    def resume_waveform_dump(self) -> bool:
        self.dut.ResumeWaveformDump()
        self.logger.info("waveform dump resumed")
        self._emit_event("waveform.resume", {})
        return True

    def pause_waveform_dump(self) -> bool:
        self.dut.PauseWaveformDump()
        self.logger.info("waveform dump paused")
        self._emit_event("waveform.pause", {})
        return True

    def flush_waveform(self) -> bool:
        self.dut.FlushWaveform()
        self.logger.info("waveform flushed")
        self._emit_event("waveform.flush", {})
        return True

    def waveform_paused(self) -> Optional[int]:
        if not hasattr(self.dut, "WaveformPaused"):
            return None
        return int(self.dut.WaveformPaused())

    def enable_fst_dump(self, path: Optional[str] = None) -> Optional[str]:
        if path:
            self.set_waveform_path(path)
        self.resume_waveform_dump()
        return self.waveform_path

    def disable_fst_dump(self, flush: bool = False) -> bool:
        if flush:
            self.flush_waveform()
        self.pause_waveform_dump()
        return True

    def get_errors(self) -> list:
        return self.monitor.get_errors()

    def get_stats(self) -> dict:
        out = {
            "monitor": self.monitor.get_stats(),
            "icache": self.icache_agent.get_stats(),
            "uncache": self.uncache_agent.get_stats(),
            "ptw": self.ptw_agent.get_stats(),
            "backend": self.backend_model.get_stats(),
            "branch": self.branch_checker.get_stats(),
        }
        return out

    def finish(self) -> None:
        self.logger.info("finish dut")
        self.dut.Finish()
