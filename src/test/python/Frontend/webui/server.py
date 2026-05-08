from __future__ import annotations

import asyncio
import logging
from pathlib import Path
from typing import Optional

from fastapi import FastAPI, HTTPException, WebSocket, WebSocketDisconnect
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field

from .event_bus import EventBus
from .runner import SimulationRunner


logger = logging.getLogger("Frontend.webui.server")

app = FastAPI(title="Frontend Interactive Console", version="0.1.0")
bus = EventBus(queue_size=1024, history_size=4096)
runner = SimulationRunner(bus)

STATIC_DIR = Path(__file__).resolve().parent / "static"
app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")


class SessionStartRequest(BaseModel):
    reset_vector: int = 0x80000000
    bare_mode: bool = True
    reset_cycles: int = 20
    log_level: Optional[str] = None
    waveform_path: Optional[str] = None
    program_words: list[int] = Field(default_factory=list)
    program_base_addr: int = 0x80000000


class StepRequest(BaseModel):
    cycles: int = 1


class RunRequest(BaseModel):
    interval_ms: int = 0


class RedirectRequest(BaseModel):
    target_pc: int
    reason: str = "manual_redirect"


class ExceptionRequest(BaseModel):
    cause: int
    tval: int = 0
    pc: int


class ICacheConfigRequest(BaseModel):
    hit_latency: int = 1
    miss_latency: int = 20
    miss_rate: float = 0.0
    seed: int = 1


class UncacheConfigRequest(BaseModel):
    latency: int = 2
    mmio_latency: Optional[int] = None


class PTWConfigRequest(BaseModel):
    latency: int = 3
    latency_max: Optional[int] = None
    mode: Optional[str] = None
    response_source: str = "model"
    compare_drive_source: str = "nemu"
    nemu_ptw_adapter: str = ""
    req_ready_strategy: str = "always"
    req_ready_probability: float = 1.0
    req_ready_high_cycles: int = 1
    req_ready_low_cycles: int = 0
    seed: int = 1
    flush_pending_on_sfence: bool = True
    strict_bare_mode: bool = False


class LogLevelRequest(BaseModel):
    level: str


class WaveformPathRequest(BaseModel):
    path: str


class WaveformEnableRequest(BaseModel):
    path: Optional[str] = None


class WaveformPauseRequest(BaseModel):
    flush: bool = False


@app.on_event("startup")
async def _startup() -> None:
    bus.attach_loop(asyncio.get_running_loop())
    logger.info("web console startup")


@app.on_event("shutdown")
async def _shutdown() -> None:
    runner.close()
    logger.info("web console shutdown")


@app.get("/")
async def index() -> FileResponse:
    return FileResponse(str(STATIC_DIR / "index.html"))


@app.get("/api/state/snapshot")
async def state_snapshot() -> dict:
    return runner.snapshot()


@app.post("/api/session/start")
async def session_start(req: SessionStartRequest) -> dict:
    try:
        return runner.start_session(req.model_dump())
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/session/reset")
async def session_reset(req: SessionStartRequest) -> dict:
    try:
        return runner.reset(req.reset_vector, req.bare_mode, req.reset_cycles)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/program/load_fixed_microbench")
async def load_fixed_microbench() -> dict:
    try:
        return runner.load_fixed_microbench()
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/clock/step")
async def clock_step(req: StepRequest) -> dict:
    try:
        return runner.step(req.cycles)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/clock/run")
async def clock_run(req: RunRequest) -> dict:
    try:
        return runner.run(req.interval_ms)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/clock/pause")
async def clock_pause() -> dict:
    return runner.pause()


@app.post("/api/inject/redirect")
async def inject_redirect(req: RedirectRequest) -> dict:
    try:
        return runner.inject_redirect(req.target_pc, req.reason)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/inject/exception")
async def inject_exception(req: ExceptionRequest) -> dict:
    try:
        return runner.inject_exception(req.cause, req.tval, req.pc)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/agent/icache/config")
async def config_icache(req: ICacheConfigRequest) -> dict:
    try:
        return runner.config_icache(req.hit_latency, req.miss_latency, req.miss_rate, req.seed)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/agent/uncache/config")
async def config_uncache(req: UncacheConfigRequest) -> dict:
    try:
        return runner.config_uncache(req.latency, req.mmio_latency)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/agent/ptw/config")
async def config_ptw(req: PTWConfigRequest) -> dict:
    try:
        return runner.config_ptw(
            req.latency,
            req.mode,
            latency_max=req.latency_max,
            response_source=req.response_source,
            compare_drive_source=req.compare_drive_source,
            nemu_ptw_adapter=req.nemu_ptw_adapter,
            req_ready_strategy=req.req_ready_strategy,
            req_ready_probability=req.req_ready_probability,
            req_ready_high_cycles=req.req_ready_high_cycles,
            req_ready_low_cycles=req.req_ready_low_cycles,
            seed=req.seed,
            flush_pending_on_sfence=req.flush_pending_on_sfence,
            strict_bare_mode=req.strict_bare_mode,
        )
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/log/level")
async def set_log_level(req: LogLevelRequest) -> dict:
    try:
        return runner.set_log_level(req.level)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/waveform/set_path")
async def waveform_set_path(req: WaveformPathRequest) -> dict:
    try:
        return runner.waveform_set_path(req.path)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/waveform/enable")
async def waveform_enable(req: WaveformEnableRequest) -> dict:
    try:
        return runner.waveform_enable(req.path)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/waveform/pause")
async def waveform_pause(req: WaveformPauseRequest) -> dict:
    try:
        return runner.waveform_pause(req.flush)
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.post("/api/waveform/flush")
async def waveform_flush() -> dict:
    try:
        return runner.waveform_flush()
    except Exception as ex:
        raise HTTPException(status_code=400, detail=str(ex)) from ex


@app.websocket("/ws/events")
async def ws_events(ws: WebSocket) -> None:
    await ws.accept()
    q = await bus.subscribe()
    try:
        for evt in bus.recent(100):
            await ws.send_json(evt)
        while True:
            evt = await q.get()
            await ws.send_json(evt)
    except WebSocketDisconnect:
        pass
    finally:
        await bus.unsubscribe(q)
