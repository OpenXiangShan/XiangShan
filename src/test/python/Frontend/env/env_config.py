from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True)
class ICacheConfig:
    hit_latency: int = 1
    miss_latency: int = 20
    miss_rate: float = 0.0
    seed: int = 1


@dataclass(frozen=True)
class UncacheConfig:
    latency: int = 2
    mmio_latency: int = 4


@dataclass(frozen=True)
class PTWConfig:
    latency: int = 3
    latency_max: int = 3
    mode: str = "bare"
    priv_imode: int = 3
    priv_virt: int = 0
    satp_mode: int = 0
    satp_asid: int = 0
    satp_ppn: int = 0
    vsatp_mode: int = 0
    vsatp_asid: int = 0
    vsatp_ppn: int = 0
    hgatp_mode: int = 0
    hgatp_vmid: int = 0
    hgatp_ppn: int = 0
    response_source: str = "nemu"
    compare_drive_source: str = "nemu"
    nemu_ptw_adapter: str = "env.nemu_ptw_adapter_template:build_ptw_resp"
    req_ready_strategy: str = "always"
    req_ready_probability: float = 1.0
    req_ready_high_cycles: int = 1
    req_ready_low_cycles: int = 0
    seed: int = 1
    flush_pending_on_sfence: bool = True
    strict_bare_mode: bool = False


@dataclass(frozen=True)
class BackendConfig:
    ftq_size: int = 64
    ibuf_watchdog_threshold: int = 32
    safe_pc: int = 0x80000000
    instruction_commit_width: int = 8
    resolve_min_delay: int = 3
    resolve_max_delay: int = 8
    redirect_min_delay: int = 5
    redirect_max_delay: int = 8
    commit_min_delay: int = 3
    commit_max_delay: int = 10
    auto_redirect_on_golden_mispredict: bool = True


@dataclass(frozen=True)
class TraceConfig:
    nemu_image_base_addr: int = 0x80000000


@dataclass(frozen=True)
class SequenceConfig:
    reset_vector: int = 0x80000000
    reset_cycles: int = 20
    run_until_commit_max_cycles: int = 10000
    inject_redirect_max_cycles: int = 1000
    check_pc_sequence_max_cycles: int = 5000


@dataclass(frozen=True)
class EnvConfig:
    icache: ICacheConfig = field(default_factory=ICacheConfig)
    uncache: UncacheConfig = field(default_factory=UncacheConfig)
    ptw: PTWConfig = field(default_factory=PTWConfig)
    backend: BackendConfig = field(default_factory=BackendConfig)
    trace: TraceConfig = field(default_factory=TraceConfig)
    sequence: SequenceConfig = field(default_factory=SequenceConfig)


BAREMODE_ENV_CONFIG = EnvConfig()
SV39_ENV_CONFIG = EnvConfig(
    ptw=PTWConfig(
        mode="sv39",
        priv_imode=1,
        satp_mode=8,
    )
)
DEFAULT_ENV_CONFIG = BAREMODE_ENV_CONFIG
