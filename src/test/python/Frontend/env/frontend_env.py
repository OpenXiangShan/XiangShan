from __future__ import annotations

import logging
import os
from importlib import import_module
from pathlib import Path
from types import SimpleNamespace
from typing import Any, Callable, Dict, Optional

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
from .checkers import PTWFullPpnChecker, PTWRespInputChecker
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
        self.line_coverage_path: Optional[str] = None
        self.functional_coverage = None
        self.event_sink = event_sink
        self.config = config or DEFAULT_ENV_CONFIG
        self._nemu_sync_module: Optional[Any] = None
        self._nemu_satp_override: Optional[int] = None
        self._nemu_vsatp_override: Optional[int] = None
        self._nemu_hgatp_override: Optional[int] = None
        self.current_cycle = 0
        self._cycle_observers = []
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

    def set_event_sink(self, sink: Optional[Callable[[Dict], None]]) -> None:
        self.event_sink = sink
        for collaborator in self._bindable_collaborators():
            collaborator.set_event_sink(sink)

    def register_cycle_observer(self, observer: Callable[[int, "FrontendEnv"], None]) -> None:
        self._cycle_observers.append(observer)

    def _create_collaborators(self) -> Dict[str, object]:
        backend_random_seed = os.getenv("TB_BACKEND_RANDOM_SEED", "").strip()
        branch_checker = BranchChecker()
        return {
            "branch_checker": branch_checker,
            "monitor": FrontendMonitor(memory=self.memory, page_table=self.page_table, branch_checker=branch_checker),
            "backend_observe_monitor": BackendObserveMonitor(),
            "icache_agent": ICacheAgent(self.memory),
            "uncache_agent": UncacheAgent(self.memory),
            "ptw_agent": PTWAgent(self.page_table),
            "ptw_full_ppn_checker": PTWFullPpnChecker(),
            "ptw_resp_input_checker": PTWRespInputChecker(),
            "backend_agent": BackendAgent(),
            "backend_model": BackendModel(
                ftq_size=self.config.backend.ftq_size,
                ibuf_watchdog_threshold=self.config.backend.ibuf_watchdog_threshold,
                safe_pc=self.config.backend.safe_pc,
                instruction_commit_width=self.config.backend.instruction_commit_width,
                resolve_min_delay=self.config.backend.resolve_min_delay,
                resolve_max_delay=self.config.backend.resolve_max_delay,
                redirect_min_delay=self.config.backend.redirect_min_delay,
                redirect_max_delay=self.config.backend.redirect_max_delay,
                commit_min_delay=self.config.backend.commit_min_delay,
                commit_max_delay=self.config.backend.commit_max_delay,
                auto_redirect_on_golden_mispredict=self.config.backend.auto_redirect_on_golden_mispredict,
                random_seed=(None if not backend_random_seed else int(backend_random_seed, 0)),
            ),
        }

    def _apply_collaborators(self, collaborators: Dict[str, object]) -> None:
        self.branch_checker = collaborators["branch_checker"]
        self.monitor = collaborators["monitor"]
        self.backend_observe_monitor = collaborators["backend_observe_monitor"]
        self.icache_agent = collaborators["icache_agent"]
        self.uncache_agent = collaborators["uncache_agent"]
        self.ptw_agent = collaborators["ptw_agent"]
        self.ptw_full_ppn_checker = collaborators["ptw_full_ppn_checker"]
        self.ptw_resp_input_checker = collaborators["ptw_resp_input_checker"]
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
            latency_max=self.config.ptw.latency_max,
            mode=(None if explicit_page_table else self.config.ptw.mode),
            response_source=self.config.ptw.response_source,
            compare_drive_source=self.config.ptw.compare_drive_source,
            nemu_ptw_adapter=self.config.ptw.nemu_ptw_adapter,
            req_ready_strategy=self.config.ptw.req_ready_strategy,
            req_ready_probability=self.config.ptw.req_ready_probability,
            req_ready_high_cycles=self.config.ptw.req_ready_high_cycles,
            req_ready_low_cycles=self.config.ptw.req_ready_low_cycles,
            seed=self.config.ptw.seed,
            flush_pending_on_sfence=self.config.ptw.flush_pending_on_sfence,
            strict_bare_mode=self.config.ptw.strict_bare_mode,
        )

    @staticmethod
    def _write(signal, value: int) -> None:
        try:
            signal.value = int(value)
        except Exception:
            return

    @staticmethod
    def _read(signal, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return default if value is None else int(value)
        except Exception:
            return default

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
        self.ptw_agent.set_request_context_provider(self._build_ptw_request_context)
        self.ptw_agent.set_nemu_sync_hook(self._sync_nemu_ptw_state)
        self.ptw_full_ppn_checker.bind_env(self)
        self.ptw_resp_input_checker.bind_env(self)
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
            self.ptw_full_ppn_checker,
            self.ptw_resp_input_checker,
            self.monitor,
            self.backend_model,
        )

    @staticmethod
    def _compose_nemu_satp(mode: int, asid: int, ppn: int) -> int:
        return ((int(mode) & 0xF) << 60) | ((int(asid) & 0xFFFF) << 44) | (int(ppn) & ((1 << 44) - 1))

    @staticmethod
    def _compose_nemu_hgatp(mode: int, vmid: int, ppn: int) -> int:
        return ((int(mode) & 0xF) << 60) | ((int(vmid) & 0x3FFF) << 44) | (int(ppn) & ((1 << 44) - 1))

    def _build_ptw_request_context(self) -> Dict[str, int]:
        if self.ptw_if is None or self.csr_ctrl_if is None:
            return {}
        satp_mode = self._read(self.csr_ctrl_if.io_tlbCsr_satp_mode, 0)
        satp_asid = self._read(self.csr_ctrl_if.io_tlbCsr_satp_asid, 0)
        satp_ppn = self._read(self.csr_ctrl_if.io_tlbCsr_satp_ppn, 0)
        vsatp_mode = self._read(self.csr_ctrl_if.io_tlbCsr_vsatp_mode, 0)
        vsatp_asid = self._read(self.csr_ctrl_if.io_tlbCsr_vsatp_asid, 0)
        vsatp_ppn = self._read(self.csr_ctrl_if.io_tlbCsr_vsatp_ppn, 0)
        hgatp_mode = self._read(self.csr_ctrl_if.io_tlbCsr_hgatp_mode, 0)
        hgatp_vmid = self._read(self.csr_ctrl_if.io_tlbCsr_hgatp_vmid, 0)
        hgatp_ppn = self._read(self.csr_ctrl_if.io_tlbCsr_hgatp_ppn, 0)
        priv_virt = self._read(self.csr_ctrl_if.io_tlbCsr_priv_virt, 0)
        satp_full = self._nemu_satp_override
        if satp_full is None:
            satp_full = self._compose_nemu_satp(satp_mode, satp_asid, satp_ppn)
        vsatp_full = self._nemu_vsatp_override
        if vsatp_full is None:
            vsatp_full = self._compose_nemu_satp(vsatp_mode, vsatp_asid, vsatp_ppn)
        hgatp_full = self._nemu_hgatp_override
        if hgatp_full is None:
            hgatp_full = self._compose_nemu_hgatp(hgatp_mode, hgatp_vmid, hgatp_ppn)
        return {
            "cycle": int(self.current_cycle),
            "priv_imode": self._read(self.csr_ctrl_if.io_tlbCsr_priv_imode, 3),
            "satp_mode": satp_mode,
            "vsatp_mode": vsatp_mode,
            "hgatp_mode": hgatp_mode,
            "sfence_valid": self._read(self.ptw_if.sfence_valid, 0),
            "sfence_bits_rs1": self._read(self.ptw_if.sfence_bits_rs1, 0),
            "sfence_bits_rs2": self._read(self.ptw_if.sfence_bits_rs2, 0),
            "sfence_bits_addr": self._read(self.ptw_if.sfence_bits_addr, 0),
            "sfence_bits_id": self._read(self.ptw_if.sfence_bits_id, 0),
            "sfence_bits_hv": self._read(self.ptw_if.sfence_bits_hv, 0),
            "sfence_bits_hg": self._read(self.ptw_if.sfence_bits_hg, 0),
            "satp": int(satp_full),
            "vsatp": int(vsatp_full),
            "hgatp": int(hgatp_full),
            "virt": int(priv_virt),
        }

    def _get_nemu_sync_module(self) -> Any:
        if self._nemu_sync_module is not None:
            return self._nemu_sync_module
        adapter_spec = str(self.config.ptw.nemu_ptw_adapter or "")
        if ":" not in adapter_spec:
            raise RuntimeError(f"invalid NEMU PTW adapter spec: {adapter_spec!r}")
        module_name, _ = adapter_spec.split(":", 1)
        module = import_module(module_name)
        required = ("sync_memory", "sync_sv39_page_table", "sync_sv39x4_page_table")
        if any(not hasattr(module, name) for name in required):
            raise RuntimeError(
                f"NEMU PTW adapter module {module_name!r} does not expose "
                "sync_memory/sync_sv39_page_table/sync_sv39x4_page_table"
            )
        self._nemu_sync_module = module
        return module

    def _iter_memory_ranges(self):
        mem_items = sorted((int(addr), int(val) & 0xFF) for addr, val in self.memory.mem.items())
        if not mem_items:
            return
        pmem_base = int(os.environ.get("FRONTEND_NEMU_PMEM_BASE", "0x80000000"), 0)
        start = None
        prev = None
        buf = bytearray()
        for addr, value in mem_items:
            if addr < pmem_base:
                continue
            if start is None:
                start = addr
                prev = addr
                buf = bytearray([value])
                continue
            if addr == prev + 1:
                buf.append(value)
            else:
                yield start, bytes(buf)
                start = addr
                buf = bytearray([value])
            prev = addr
        if start is not None and buf:
            yield start, bytes(buf)

    @staticmethod
    def _align_up(value: int, alignment: int) -> int:
        alignment = max(1, int(alignment))
        return (int(value) + alignment - 1) & ~(alignment - 1)

    def _pick_nemu_page_table_base(self, *, slot: int = 0, alignment: int = 0x1000, stride: int = 0x400000) -> int:
        override = os.environ.get("FRONTEND_NEMU_PGTABLE_BASE", "").strip()
        if override:
            base = int(override, 0)
            return self._align_up(base + int(slot) * int(stride), int(alignment))
        if not self.memory.mem:
            base = 0x81000000
            return self._align_up(base + int(slot) * int(stride), int(alignment))
        high_addr = max(int(addr) for addr in self.memory.mem.keys())
        base = self._align_up(high_addr + 0x1000, int(alignment))
        return self._align_up(base + int(slot) * int(stride), int(alignment))

    def _pick_nemu_page_table_roots(self) -> Dict[str, int]:
        return {
            "satp": self._pick_nemu_page_table_base(slot=0, alignment=0x1000, stride=0x400000),
            "vsatp": self._pick_nemu_page_table_base(slot=0, alignment=0x1000, stride=0x400000),
            "hgatp": self._pick_nemu_page_table_base(slot=1, alignment=0x4000, stride=0x400000),
        }

    def _clear_nemu_ptw_overrides(self) -> None:
        self._nemu_satp_override = None
        self._nemu_vsatp_override = None
        self._nemu_hgatp_override = None

    @staticmethod
    def _page_table_entries(pte_map) -> list[dict]:
        entries = []
        for vpn, pte in sorted(pte_map.items()):
            entries.append(
                {
                    "vpn": int(vpn),
                    "ppn": int(pte.ppn),
                    "v": int(pte.v),
                    "r": int(pte.r),
                    "w": int(pte.w),
                    "x": int(pte.x),
                    "u": int(pte.u),
                    "g": int(pte.g),
                    "a": int(pte.a),
                    "d": int(pte.d),
                    "level": int(pte.level),
                }
            )
        return entries

    def _sync_nemu_ptw_state(self, **_request: Any) -> None:
        if self.page_table.mode != "sv39" or self.csr_ctrl_if is None:
            self._clear_nemu_ptw_overrides()
            return
        module = self._get_nemu_sync_module()

        self._clear_nemu_ptw_overrides()
        roots = self._pick_nemu_page_table_roots()
        stage1_entries = self._page_table_entries(self.page_table.pte_map)
        stage2_entries = self._page_table_entries(self.page_table.stage2_pte_map)

        satp_mode = self._read(self.csr_ctrl_if.io_tlbCsr_satp_mode, 0)
        satp_asid = self._read(self.csr_ctrl_if.io_tlbCsr_satp_asid, 0)
        vsatp_mode = self._read(self.csr_ctrl_if.io_tlbCsr_vsatp_mode, 0)
        vsatp_asid = self._read(self.csr_ctrl_if.io_tlbCsr_vsatp_asid, 0)
        hgatp_mode = self._read(self.csr_ctrl_if.io_tlbCsr_hgatp_mode, 0)
        hgatp_vmid = self._read(self.csr_ctrl_if.io_tlbCsr_hgatp_vmid, 0)

        stage1_root_ppn = None
        if int(satp_mode) != 0 or int(vsatp_mode) != 0:
            stage1_root_ppn = module.sync_sv39_page_table(stage1_entries, root_paddr=roots["satp"])

        if int(satp_mode) != 0 and stage1_root_ppn is not None:
            self._nemu_satp_override = self._compose_nemu_satp(satp_mode, satp_asid, stage1_root_ppn)
        if int(vsatp_mode) != 0 and stage1_root_ppn is not None:
            self._nemu_vsatp_override = self._compose_nemu_satp(vsatp_mode, vsatp_asid, stage1_root_ppn)
        if int(hgatp_mode) != 0:
            stage2_root_ppn = module.sync_sv39x4_page_table(stage2_entries, root_paddr=roots["hgatp"])
            self._nemu_hgatp_override = self._compose_nemu_hgatp(hgatp_mode, hgatp_vmid, stage2_root_ppn)

        self.logger.debug(
            "synced NEMU PTW state: s1_entries=%d s2_entries=%d satp=0x%x vsatp=0x%x hgatp=0x%x",
            len(stage1_entries),
            len(stage2_entries),
            int(self._nemu_satp_override or 0),
            int(self._nemu_vsatp_override or 0),
            int(self._nemu_hgatp_override or 0),
        )
        for paddr, payload in self._iter_memory_ranges():
            module.sync_memory(int(paddr), payload)

    def _drive_backend_cycle(self, cycle: int) -> None:
        self.backend_model.begin_cycle(cycle)
        observation = self.backend_observe_monitor.snapshot()
        self.backend_model.consume_backend_observation(observation)
        actions = self.backend_model.plan_cycle_actions()
        self.backend_agent.start_cycle(actions.can_accept)
        self.backend_agent.drive_commit(actions.commit_entry)
        if hasattr(self.backend_model, "commit_entry_driven"):
            self.backend_model.commit_entry_driven(actions.commit_entry)
        self.backend_agent.drive_resolves(actions.resolve_entries)
        self.backend_agent.drive_call_ret_commit(actions.call_ret_commit_group)
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
        self._init_unbound_top_inputs()

    def _init_unbound_top_inputs(self) -> None:
        # FrontendTop currently exposes more top-level inputs than the Python env
        # bundles model. Drive the unbound ones to deterministic reset values so
        # the DUT does not depend on simulator-specific input initialization.
        scalar_defaults = {
            "io_hartId": 0,
            "io_sfence_bits_flushPipe": 0,
            "io_backend_wfi_wfiSafe": 0,
            "io_softPrefetch_0_valid": 0,
            "io_softPrefetch_0_bits_vaddr": 0,
            "io_softPrefetch_1_valid": 0,
            "io_softPrefetch_1_bits_vaddr": 0,
            "io_softPrefetch_2_valid": 0,
            "io_softPrefetch_2_bits_vaddr": 0,
            "io_testOnlyPtwReqCtrlEnable": 0,
            "io_testOnlyPtwReqCtrlFire": 0,
            "io_testOnlyPtwReqCtrlBitsVpn": 0,
            "io_testOnlyPtwReqCtrlBitsS2xlate": 0,
            "io_testOnlyPtwReqCtrlBitsGetGpa": 0,
            "io_testOnlyPtwReqCtrlBitsMemidxIsLd": 0,
            "io_testOnlyPtwReqCtrlBitsMemidxIsSt": 0,
            "io_testOnlyPtwReqCtrlBitsMemidxIdx": 0,
            "io_testOnlyPtwReqReady": 0,
            "io_tlbCsr_mbmc_KEYIDEN": 0,
            "io_tlbCsr_mbmc_BME": 0,
            "io_tlbCsr_mbmc_CMODE": 0,
            "io_tlbCsr_mbmc_BCLEAR": 0,
            "io_tlbCsr_mbmc_BMA": 0,
            "io_tlbCsr_priv_mxr": 0,
            "io_tlbCsr_priv_sum": 0,
            "io_tlbCsr_priv_vmxr": 0,
            "io_tlbCsr_priv_vsum": 0,
            "io_tlbCsr_priv_spvp": 0,
            "io_tlbCsr_priv_dmode": 0,
            "io_tlbCsr_mPBMTE": 0,
            "io_tlbCsr_hPBMTE": 0,
            "io_tlbCsr_pmm_mseccfg": 0,
            "io_tlbCsr_pmm_menvcfg": 0,
            "io_tlbCsr_pmm_henvcfg": 0,
            "io_tlbCsr_pmm_hstatus": 0,
            "io_tlbCsr_pmm_senvcfg": 0,
            "io_csrCtrl_pf_ctrl_l2_pf_enable": 0,
            "io_csrCtrl_pf_ctrl_l1D_pf_enable": 0,
            "io_csrCtrl_pf_ctrl_l1D_pf_train_on_hit": 0,
            "io_csrCtrl_pf_ctrl_l1D_pf_enable_agt": 0,
            "io_csrCtrl_pf_ctrl_l1D_pf_enable_pht": 0,
            "io_csrCtrl_pf_ctrl_l1D_pf_active_threshold": 0,
            "io_csrCtrl_pf_ctrl_l1D_pf_active_stride": 0,
            "io_csrCtrl_pf_ctrl_l1D_pf_enable_stride": 0,
            "io_csrCtrl_pf_ctrl_l2_pf_store_only": 0,
            "io_csrCtrl_pf_ctrl_l2_pf_recv_enable": 0,
            "io_csrCtrl_pf_ctrl_l2_pf_pbop_enable": 0,
            "io_csrCtrl_pf_ctrl_l2_pf_vbop_enable": 0,
            "io_csrCtrl_pf_ctrl_l2_pf_tp_enable": 0,
            "io_csrCtrl_pf_ctrl_l2_pf_delay_latency": 0,
            "io_csrCtrl_pf_ctrl_berti_enable": 0,
            "io_csrCtrl_lvpred_disable": 0,
            "io_csrCtrl_no_spec_load": 0,
            "io_csrCtrl_storeset_wait_store": 0,
            "io_csrCtrl_storeset_no_fast_wakeup": 0,
            "io_csrCtrl_lvpred_timeout": 0,
            "io_csrCtrl_bp_ctrl_rasEnable": 1,
            "io_csrCtrl_sbuffer_timeout": 0,
            "io_csrCtrl_sbuffer_threshold": 0,
            "io_csrCtrl_ldld_vio_check_enable": 0,
            "io_csrCtrl_soft_prefetch_enable": 0,
            "io_csrCtrl_cache_error_enable": 0,
            "io_csrCtrl_uncache_write_outstanding_enable": 0,
            "io_csrCtrl_hd_misalign_st_enable": 0,
            "io_csrCtrl_hd_misalign_ld_enable": 0,
            "io_csrCtrl_power_down_enable": 0,
            "io_csrCtrl_flush_l2_enable": 0,
            "io_csrCtrl_fusion_enable": 0,
            "io_csrCtrl_wfi_enable": 0,
            "io_csrCtrl_distribute_csr_w_bits_addr": 0,
            "io_csrCtrl_distribute_csr_w_bits_data": 0,
            "io_csrCtrl_singlestep": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_valid": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_addr": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_timing": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_execute": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_store": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_load": 0,
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2": 0,
            "io_csrCtrl_frontend_trigger_tEnableVec_0": 0,
            "io_csrCtrl_frontend_trigger_tEnableVec_1": 0,
            "io_csrCtrl_frontend_trigger_tEnableVec_2": 0,
            "io_csrCtrl_frontend_trigger_tEnableVec_3": 0,
            "io_csrCtrl_frontend_trigger_debugMode": 0,
            "io_csrCtrl_frontend_trigger_triggerCanRaiseBpExp": 0,
            "io_csrCtrl_mem_trigger_tUpdate_valid": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_addr": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_select": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_timing": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_action": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_execute": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_store": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_load": 0,
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2": 0,
            "io_csrCtrl_mem_trigger_tEnableVec_0": 0,
            "io_csrCtrl_mem_trigger_tEnableVec_1": 0,
            "io_csrCtrl_mem_trigger_tEnableVec_2": 0,
            "io_csrCtrl_mem_trigger_tEnableVec_3": 0,
            "io_csrCtrl_mem_trigger_debugMode": 0,
            "io_csrCtrl_mem_trigger_triggerCanRaiseBpExp": 0,
            "io_csrCtrl_virtMode": 0,
            "io_debugTopDown_robHeadVaddr_valid": 0,
            "io_debugTopDown_robHeadVaddr_bits_addr": 0,
            "io_dft_ram_ctl": 0,
        }
        for name, value in scalar_defaults.items():
            self._write(getattr(self.dut, name, None), value)

        for idx in range(8):
            self._write(getattr(self.dut, f"io_backend_cfVec_{idx}_ready", None), 1)

    def _on_clock_edge(self, cycle: int) -> None:
        self.current_cycle = int(cycle)
        self.icache_agent.on_clock_edge(cycle)
        self.uncache_agent.on_clock_edge(cycle)
        self.ptw_agent.on_clock_edge(cycle)
        self.ptw_full_ppn_checker.on_clock_edge(cycle)
        self.ptw_resp_input_checker.on_clock_edge(cycle)
        self._drive_backend_cycle(cycle)
        self.monitor.on_clock_edge(cycle)
        for observer in list(self._cycle_observers):
            observer(int(cycle), self)
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
            self._write(self.csr_ctrl_if.io_tlbCsr_priv_virt, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_satp_mode, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_satp_asid, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_satp_ppn, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_vsatp_mode, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_vsatp_asid, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_vsatp_ppn, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_hgatp_mode, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_hgatp_vmid, 0)
            self._write(self.csr_ctrl_if.io_tlbCsr_hgatp_ppn, 0)
        self.reset(reset_cycles)
        self.monitor.set_expected_pc(int(reset_vector))

    def load_program(self, data: bytes, base_addr: int) -> None:
        self.memory.load_bin(data, int(base_addr))
        self.backend_model.set_explicit_injection_enabled(True)
        self.logger.info("program loaded: base=0x%x size=%d", int(base_addr), len(data))
        self._emit_event("control.load_program", {"base_addr": int(base_addr), "size": len(data)})

    def load_program_file(self, path: str, base_addr: int) -> int:
        size = Path(path).stat().st_size
        self.memory.load_file(path, int(base_addr))
        self.backend_model.set_explicit_injection_enabled(
            False,
            reason=f"bin/program-file mode active ({str(path)})",
        )
        self.logger.info("program file loaded: path=%s base=0x%x size=%d", path, int(base_addr), size)
        self._emit_event(
            "control.load_program",
            {"path": str(path), "base_addr": int(base_addr), "size": int(size)},
        )
        return int(size)

    def load_golden_trace_file(self, path: str, start_index: int = 0) -> int:
        trace = GoldenTrace.from_file(str(path))
        self.backend_model.set_golden_trace(trace, start_cursor=int(start_index))
        self.logger.info(
            "golden trace loaded: path=%s start_index=%d entries=%d",
            str(path),
            int(start_index),
            len(trace.entries),
        )
        self._emit_event(
            "control.load_golden_trace",
            {"path": str(path), "entries": int(len(trace.entries)), "start_index": int(start_index)},
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

    def pulse_tlb_csr_changes(
        self,
        *,
        satp: bool = False,
        vsatp: bool = False,
        hgatp: bool = False,
        priv_virt: bool = False,
        cycles: int = 1,
    ) -> int:
        asserted = []
        if priv_virt:
            asserted.append(self.csr_ctrl_if.io_tlbCsr_priv_virt_changed)
        if satp:
            asserted.append(self.csr_ctrl_if.io_tlbCsr_satp_changed)
        if vsatp:
            asserted.append(self.csr_ctrl_if.io_tlbCsr_vsatp_changed)
        if hgatp:
            asserted.append(self.csr_ctrl_if.io_tlbCsr_hgatp_changed)
        for signal in asserted:
            self._write(signal, 1)
        if asserted:
            step_cycles = max(1, int(cycles))
            self.step(step_cycles)
            for signal in asserted:
                self._write(signal, 0)
            self._emit_event(
                "control.tlb_csr_changed",
                {
                    "satp": bool(satp),
                    "vsatp": bool(vsatp),
                    "hgatp": bool(hgatp),
                    "priv_virt": bool(priv_virt),
                    "cycles": step_cycles,
                },
            )
            return step_cycles
        return 0

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
            "ptw_resp_input_checker": self.ptw_resp_input_checker.get_stats(),
            "backend": self.backend_model.get_stats(),
            "branch": self.branch_checker.get_stats(),
        }
        return out

    def finish(self) -> None:
        self.logger.info("finish dut")
        self.dut.Finish()
