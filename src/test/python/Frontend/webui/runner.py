from __future__ import annotations

import logging
import threading
import time
from pathlib import Path
from typing import Any, Dict, Optional

from Frontend import DUTFrontend

from env.frontend_env import FrontendEnv
from env.logging_utils import configure_env_logging
from env.nemu_trace_pipeline import generate_nemu_trace_from_bin
from env.trace import GoldenTrace

from .event_bus import EventBus


PACKAGE_ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = PACKAGE_ROOT.parents[3]
READY_TO_RUN_DIR = REPO_ROOT / "ready-to-run"
MICROBENCH_BIN_PATH = READY_TO_RUN_DIR / "microbench.bin"
MICROBENCH_TRACE_PATH = REPO_ROOT / "NEMU" / "logs" / "microbench.trace.jsonl"
MICROBENCH_BASE_ADDR = 0x80000000


class SimulationRunner:
    def __init__(self, event_bus: EventBus) -> None:
        self.logger = logging.getLogger("Frontend.webui.runner")
        self.event_bus = event_bus
        self.lock = threading.RLock()
        self._thread: Optional[threading.Thread] = None
        self._stop_flag = threading.Event()
        self._run_flag = threading.Event()
        self._run_interval_s = 0.0

        self.dut: Optional[DUTFrontend] = None
        self.env: Optional[FrontendEnv] = None
        self.state = "idle"
        self.last_error: Optional[str] = None
        self.fixed_program: Optional[Dict[str, Any]] = None

    def _publish(self, event_type: str, payload: Dict[str, Any], level: str = "INFO") -> None:
        cycle = 0
        if self.env is not None:
            cycle = int(getattr(self.env, "current_cycle", 0))
        self.event_bus.publish(
            {
                "type": event_type,
                "source": "runner",
                "cycle": cycle,
                "level": level,
                "payload": payload,
            }
        )

    def _ensure_thread(self) -> None:
        if self._thread and self._thread.is_alive():
            return
        self._stop_flag.clear()
        self._thread = threading.Thread(target=self._run_loop, name="tb-runner", daemon=True)
        self._thread.start()

    def _fixed_program_snapshot(self) -> Dict[str, Any]:
        if self.fixed_program is None:
            return {
                "active": False,
                "name": None,
                "bin_path": None,
                "trace_path": None,
                "trace_entries": 0,
                "manual_redirect_enabled": True,
            }
        return dict(self.fixed_program)

    def _prepare_microbench_trace(self) -> Dict[str, Any]:
        trace_path = MICROBENCH_TRACE_PATH
        if trace_path.is_file():
            trace = GoldenTrace.from_file(str(trace_path))
            return {
                "bin_path": str(MICROBENCH_BIN_PATH),
                "trace_output_path": str(trace_path),
                "trace_entries": int(len(trace.entries)),
                "trace_cached": True,
            }

        out = generate_nemu_trace_from_bin(
            bin_path=str(MICROBENCH_BIN_PATH),
            trace_output_path=str(trace_path),
        )
        out["trace_cached"] = False
        return out

    def _run_loop(self) -> None:
        self.logger.info("runner loop started")
        while not self._stop_flag.is_set():
            if not self._run_flag.is_set():
                time.sleep(0.005)
                continue
            env = self.env
            if env is None:
                self._run_flag.clear()
                self.state = "idle"
                continue

            try:
                with self.lock:
                    env.step(1)
                if self._run_interval_s > 0:
                    time.sleep(self._run_interval_s)
            except Exception as ex:
                self.last_error = str(ex)
                self.state = "error"
                self._run_flag.clear()
                self._publish("runner.error", {"error": str(ex)}, level="ERROR")
                self.logger.exception("runner step failed")
                time.sleep(0.05)
        self.logger.info("runner loop stopped")

    def _release_env(self) -> None:
        if self.env is not None:
            try:
                self.env.finish()
            except Exception:
                self.logger.exception("env finish failed")
        self.env = None
        self.dut = None

    def close(self) -> None:
        self._run_flag.clear()
        self._stop_flag.set()
        if self._thread and self._thread.is_alive():
            self._thread.join(timeout=2.0)
        with self.lock:
            self._release_env()
        self.state = "stopped"

    def start_session(self, config: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        cfg = dict(config or {})
        configure_env_logging(cfg.get("log_level"))

        with self.lock:
            self._run_flag.clear()
            self._release_env()
            self.fixed_program = None

            dut = DUTFrontend()
            dut.InitClock("clock")

            wave_path = cfg.get("waveform_path")
            if wave_path:
                Path(str(wave_path)).parent.mkdir(parents=True, exist_ok=True)
                dut.SetWaveform(str(wave_path))

            env = FrontendEnv(
                dut,
                log_level=cfg.get("log_level"),
                event_sink=self.event_bus.publish,
                register_callbacks=True,
            )
            env.initialize(
                reset_vector=int(cfg.get("reset_vector", 0x80000000)),
                bare_mode=bool(cfg.get("bare_mode", True)),
                reset_cycles=int(cfg.get("reset_cycles", 20)),
            )

            program = cfg.get("program_words")
            base_addr = int(cfg.get("program_base_addr", 0x80000000))
            if program:
                raw = bytearray()
                for w in program:
                    raw.extend((int(w) & 0xFFFFFFFF).to_bytes(4, "little"))
                env.load_program(bytes(raw), base_addr)

            self.dut = dut
            self.env = env
            self.state = "paused"
            self.last_error = None

        self._ensure_thread()
        self._publish("session.started", {"config": cfg})
        return self.snapshot()

    def load_fixed_microbench(self) -> Dict[str, Any]:
        trace_out = self._prepare_microbench_trace()
        self.start_session(
            {
                "reset_vector": MICROBENCH_BASE_ADDR,
                "bare_mode": True,
            }
        )
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            bin_size = int(self.env.load_program_file(str(MICROBENCH_BIN_PATH), MICROBENCH_BASE_ADDR))
            loaded_trace_entries = int(self.env.load_golden_trace_file(str(trace_out["trace_output_path"])))
            self.fixed_program = {
                "active": True,
                "name": "microbench",
                "bin_path": str(MICROBENCH_BIN_PATH.resolve()),
                "trace_path": str(Path(str(trace_out["trace_output_path"])).resolve()),
                "bin_size": int(bin_size),
                "trace_entries": int(loaded_trace_entries),
                "trace_cached": bool(trace_out.get("trace_cached", False)),
                "manual_redirect_enabled": False,
            }
        self._publish(
            "control.load_fixed_microbench",
            {
                "bin_path": str(MICROBENCH_BIN_PATH.resolve()),
                "trace_path": str(Path(str(trace_out["trace_output_path"])).resolve()),
                "trace_entries": int(loaded_trace_entries),
                "trace_cached": bool(trace_out.get("trace_cached", False)),
            },
        )
        return self.snapshot()

    def reset(self, reset_vector: int = 0x80000000, bare_mode: bool = True, reset_cycles: int = 20) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.initialize(
                reset_vector=int(reset_vector),
                bare_mode=bool(bare_mode),
                reset_cycles=int(reset_cycles),
            )
            self.state = "paused"
        self._publish("session.reset", {"reset_vector": int(reset_vector)})
        return self.snapshot()

    def step(self, cycles: int = 1) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.step(int(cycles))
            self.state = "paused"
        self._publish("control.step", {"cycles": int(cycles)})
        return self.snapshot()

    def run(self, interval_ms: int = 0) -> Dict[str, Any]:
        if self.env is None:
            raise RuntimeError("session not started")
        self._run_interval_s = max(0.0, float(interval_ms) / 1000.0)
        self._run_flag.set()
        self.state = "running"
        self._ensure_thread()
        self._publish("control.run", {"interval_ms": int(interval_ms)})
        return self.snapshot()

    def pause(self) -> Dict[str, Any]:
        self._run_flag.clear()
        self.state = "paused" if self.env is not None else "idle"
        self._publish("control.pause", {})
        return self.snapshot()

    def inject_redirect(self, target_pc: int, reason: str) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.backend_model.inject_redirect(int(target_pc), str(reason), delay_cycles=1)
        self._publish("control.inject_redirect", {"target_pc": int(target_pc), "reason": str(reason)})
        return self.snapshot()

    def inject_exception(self, cause: int, tval: int, pc: int) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.backend_model.inject_exception(int(cause), int(tval), int(pc), delay_cycles=1)
        self._publish(
            "control.inject_exception",
            {"cause": int(cause), "tval": int(tval), "pc": int(pc)},
        )
        return self.snapshot()

    def config_icache(self, hit_latency: int, miss_latency: int, miss_rate: float, seed: int = 1) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.icache_agent.configure(
                hit_latency=int(hit_latency),
                miss_latency=int(miss_latency),
                miss_rate=float(miss_rate),
                seed=int(seed),
            )
        self._publish("control.config_icache", {"hit_latency": int(hit_latency), "miss_latency": int(miss_latency), "miss_rate": float(miss_rate)})
        return self.snapshot()

    def config_uncache(self, latency: int, mmio_latency: Optional[int] = None) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.uncache_agent.configure(latency=int(latency), mmio_latency=mmio_latency)
        self._publish("control.config_uncache", {"latency": int(latency), "mmio_latency": mmio_latency})
        return self.snapshot()

    def config_ptw(self, latency: int, mode: Optional[str] = None) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.ptw_agent.configure(latency=int(latency), mode=mode)
        self._publish("control.config_ptw", {"latency": int(latency), "mode": mode})
        return self.snapshot()

    def set_log_level(self, level: str) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.set_log_level(level)
        self._publish("control.log_level", {"level": str(level)})
        return self.snapshot()

    def waveform_set_path(self, path: str) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            final_path = self.env.set_waveform_path(str(path))
        self._publish("control.waveform_set_path", {"path": final_path})
        return self.snapshot()

    def waveform_enable(self, path: Optional[str] = None) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            final_path = self.env.enable_fst_dump(path)
        self._publish("control.waveform_enable", {"path": final_path})
        return self.snapshot()

    def waveform_pause(self, flush: bool = False) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            if bool(flush):
                self.env.flush_waveform()
            self.env.pause_waveform_dump()
        self._publish("control.waveform_pause", {"flush": bool(flush)})
        return self.snapshot()

    def waveform_flush(self) -> Dict[str, Any]:
        with self.lock:
            if self.env is None:
                raise RuntimeError("session not started")
            self.env.flush_waveform()
        self._publish("control.waveform_flush", {})
        return self.snapshot()

    def snapshot(self) -> Dict[str, Any]:
        with self.lock:
            env = self.env
            if env is None:
                return {
                    "state": self.state,
                    "has_session": False,
                    "last_error": self.last_error,
                    "fixed_program": self._fixed_program_snapshot(),
                    "recent_events": self.event_bus.recent(200),
                }
            return {
                "state": self.state,
                "has_session": True,
                "cycle": int(getattr(env, "current_cycle", 0)),
                "running": bool(self._run_flag.is_set()),
                "last_error": self.last_error,
                "stats": env.get_stats(),
                "recent_pcs": env.monitor.recent_pcs(limit=32),
                "recent_backend_events": env.backend_model.recent_events(limit=16),
                "waveform": {
                    "path": env.waveform_path,
                    "paused": env.waveform_paused(),
                },
                "fixed_program": self._fixed_program_snapshot(),
                "recent_events": self.event_bus.recent(200),
            }
