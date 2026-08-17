from __future__ import annotations

from dataclasses import dataclass, field
from typing import Literal, Optional, Tuple

from ..core.transactions import ProgramImage
from ..support.pmp_pma import PmpPmaConfig, csr_addresses_for_entry, encode_pmp_pma_addr, encode_pmp_pma_cfg


_PAGE_SIZE = 0x1000
_SV39_MODE = 8
_S2XLATE_NONE = 0
_S2XLATE_ONLY_STAGE1 = 1
_S2XLATE_ONLY_STAGE2 = 2
_S2XLATE_ALL_STAGE = 3


@dataclass(frozen=True)
class TranslationPte:
    """Complete PTE fields used by one translation stage."""

    v: int = 1
    r: int = 1
    w: int = 0
    x: int = 1
    u: int = 0
    g: int = 0
    a: int = 1
    d: int = 0
    n: int = 0
    level: int = 0
    asid: int = 0
    vmid: int = 0
    pbmt: int = 0

    def as_mapping_kwargs(self) -> dict:
        return {
            "v": int(self.v),
            "r": int(self.r),
            "w": int(self.w),
            "x": int(self.x),
            "u": int(self.u),
            "g": int(self.g),
            "a": int(self.a),
            "d": int(self.d),
            "n": int(self.n),
            "level": int(self.level),
            "asid": int(self.asid),
            "vmid": int(self.vmid),
            "pbmt": int(self.pbmt),
        }


@dataclass(frozen=True)
class TranslationPmpPmaEntry:
    kind: Literal["pmp", "pma"]
    index: int
    config: PmpPmaConfig
    addr: int
    size: Optional[int] = None


@dataclass(frozen=True)
class TranslationScenario:
    """One reproducible Sv39 translation setup shared by model and DUT."""

    scenario_id: str
    va: int
    pa: int
    payload: bytes
    page_count: int = 1
    mode: str = "sv39"
    s1_pte: TranslationPte = field(default_factory=TranslationPte)
    s2_pte: TranslationPte = field(default_factory=TranslationPte)
    gpa: Optional[int] = None
    s2xlate: int = _S2XLATE_NONE
    get_gpa: int = 0
    satp_asid: int = 0
    satp_ppn: int = 0
    vsatp_asid: int = 0
    vsatp_ppn: int = 0
    hgatp_vmid: int = 0
    hgatp_ppn: int = 0
    priv_imode: int = 1
    priv_virt: int = 0
    pmp_entries: Tuple[TranslationPmpPmaEntry, ...] = ()
    pma_entries: Tuple[TranslationPmpPmaEntry, ...] = ()
    expected_path: Literal["cacheable", "uncache", "fault"] = "cacheable"
    expected_result: Literal["hit", "miss_refill", "page_fault", "access_fault", "guest_fault", "normal"] = "normal"

    def program_image(self) -> ProgramImage:
        return ProgramImage(payload=bytes(self.payload), base_addr=int(self.pa))


@dataclass(frozen=True)
class TranslationScenarioState:
    scenario: TranslationScenario
    expected_ptw_request: dict
    expected_outcome: dict
    context: dict
    pmp_writes: Tuple[dict, ...]
    pma_writes: Tuple[dict, ...]


class TranslationScenarioBuilder:
    """Apply one Sv39 scenario to the page-table model, memory and DUT controls."""

    def __init__(self, env) -> None:
        self.env = env

    @staticmethod
    def _is_sv39_canonical(va: int) -> bool:
        value = int(va)
        if not 0 <= value < (1 << 64):
            return False
        sign = (value >> 38) & 1
        upper = value >> 39
        return upper == ((1 << 25) - 1 if sign else 0)

    @staticmethod
    def _page_count_needed(va: int, payload_size: int) -> int:
        return max(1, ((int(va) & (_PAGE_SIZE - 1)) + max(1, int(payload_size)) + _PAGE_SIZE - 1) // _PAGE_SIZE)

    @staticmethod
    def _validate_pte(pte: TranslationPte, stage: str) -> None:
        if int(pte.level) != 0:
            raise ValueError(f"{stage} PTE level {pte.level} is unsupported; only Sv39 level-0 pages are supported")
        for name, value in pte.as_mapping_kwargs().items():
            if name == "level":
                continue
            if int(value) < 0:
                raise ValueError(f"{stage} PTE field {name} must be non-negative")
            if name not in {"asid", "vmid", "pbmt"} and int(value) not in {0, 1}:
                raise ValueError(f"{stage} PTE field {name} must be 0 or 1")
        if not 0 <= int(pte.pbmt) < 4:
            raise ValueError(f"{stage} PTE PBMT must fit the two-bit PTW response field")

    def validate(self, scenario: TranslationScenario) -> None:
        if not str(scenario.scenario_id):
            raise ValueError("translation scenario_id must be non-empty")
        if str(scenario.mode).lower() != "sv39":
            raise ValueError(f"unsupported translation mode: {scenario.mode}; only Sv39 is supported")
        if not self._is_sv39_canonical(scenario.va):
            raise ValueError(f"Sv39 non-canonical VA is unsupported: 0x{int(scenario.va):x}")
        if int(scenario.page_count) < 1:
            raise ValueError("translation page_count must be positive")
        if not scenario.payload:
            raise ValueError("translation payload must be non-empty")
        if int(scenario.va) & (_PAGE_SIZE - 1) != int(scenario.pa) & (_PAGE_SIZE - 1):
            raise ValueError("VA and PA must have the same page offset")
        if int(scenario.page_count) < self._page_count_needed(scenario.va, len(scenario.payload)):
            raise ValueError("translation page_count does not cover the payload")
        if int(scenario.s2xlate) not in {
            _S2XLATE_NONE,
            _S2XLATE_ONLY_STAGE1,
            _S2XLATE_ONLY_STAGE2,
            _S2XLATE_ALL_STAGE,
        }:
            raise ValueError(f"unsupported s2xlate value: {scenario.s2xlate}")
        if int(scenario.s2xlate) == _S2XLATE_ALL_STAGE:
            if scenario.gpa is None:
                raise ValueError("all-stage translation requires gpa")
            if int(scenario.va) & (_PAGE_SIZE - 1) != int(scenario.gpa) & (_PAGE_SIZE - 1):
                raise ValueError("VA and GPA must have the same page offset")
            if int(scenario.gpa) & (_PAGE_SIZE - 1) != int(scenario.pa) & (_PAGE_SIZE - 1):
                raise ValueError("GPA and PA must have the same page offset")
            if int(scenario.s1_pte.vmid) != int(scenario.hgatp_vmid):
                raise ValueError("all-stage stage-1 PTE VMID must match hgatp_vmid")
        self._validate_pte(scenario.s1_pte, "stage-1")
        self._validate_pte(scenario.s2_pte, "stage-2")
        for entry in scenario.pmp_entries:
            if entry.kind != "pmp":
                raise ValueError("pmp_entries must contain PMP entries")
            csr_addresses_for_entry(entry.kind, entry.index)
            encode_pmp_pma_cfg(entry.config)
            encode_pmp_pma_addr(entry.addr, entry.config, size=entry.size)
        for entry in scenario.pma_entries:
            if entry.kind != "pma":
                raise ValueError("pma_entries must contain PMA entries")
            csr_addresses_for_entry(entry.kind, entry.index)
            encode_pmp_pma_cfg(entry.config)
            encode_pmp_pma_addr(entry.addr, entry.config, size=entry.size)

    @staticmethod
    def _map_stage1_pages(env, scenario: TranslationScenario, target: int) -> None:
        pte_kwargs = scenario.s1_pte.as_mapping_kwargs()
        for page in range(int(scenario.page_count)):
            env.page_table.map_page(
                (int(scenario.va) >> 12) + page,
                (int(target) >> 12) + page,
                **pte_kwargs,
            )

    @staticmethod
    def _map_stage2_pages(env, scenario: TranslationScenario, target: int) -> None:
        pte_kwargs = scenario.s2_pte.as_mapping_kwargs()
        pte_kwargs.pop("asid")
        for page in range(int(scenario.page_count)):
            env.page_table.map_stage2_page(
                (int(target) >> 12) + page,
                (int(scenario.pa) >> 12) + page,
                **pte_kwargs,
            )

    def build(self, scenario: TranslationScenario) -> TranslationScenarioState:
        self.validate(scenario)

        self.env.page_table.clear()
        s2xlate = int(scenario.s2xlate)
        if s2xlate == _S2XLATE_ONLY_STAGE2:
            self._map_stage2_pages(self.env, scenario, int(scenario.va))
        elif s2xlate == _S2XLATE_ALL_STAGE:
            self._map_stage1_pages(self.env, scenario, int(scenario.gpa))
            self._map_stage2_pages(self.env, scenario, int(scenario.gpa))
        else:
            self._map_stage1_pages(self.env, scenario, int(scenario.pa))

        self.env.ptw_agent.configure(mode="sv39", response_source="model", compare_drive_source="model")
        expected_pa, expected_ok, expected_metadata = self.env.page_table.translate(
            int(scenario.va),
            s2xlate=s2xlate,
            priv_imode=int(scenario.priv_imode),
        )
        self.env.load_program(scenario.payload, int(scenario.pa))
        context = self.env.update_translation_context(
            satp_mode=_SV39_MODE,
            satp_asid=int(scenario.satp_asid),
            satp_ppn=int(scenario.satp_ppn),
            vsatp_mode=(_SV39_MODE if s2xlate == _S2XLATE_ALL_STAGE else None),
            vsatp_asid=(int(scenario.vsatp_asid) if s2xlate == _S2XLATE_ALL_STAGE else None),
            vsatp_ppn=(int(scenario.vsatp_ppn) if s2xlate == _S2XLATE_ALL_STAGE else None),
            hgatp_mode=(_SV39_MODE if s2xlate in {_S2XLATE_ONLY_STAGE2, _S2XLATE_ALL_STAGE} else None),
            hgatp_vmid=(int(scenario.hgatp_vmid) if s2xlate in {_S2XLATE_ONLY_STAGE2, _S2XLATE_ALL_STAGE} else None),
            hgatp_ppn=(int(scenario.hgatp_ppn) if s2xlate in {_S2XLATE_ONLY_STAGE2, _S2XLATE_ALL_STAGE} else None),
            priv_imode=int(scenario.priv_imode),
            priv_virt=int(scenario.priv_virt),
        )
        pmp_writes = tuple(
            self.env.write_pmp_entry(entry.index, entry.config, entry.addr, size=entry.size)
            for entry in scenario.pmp_entries
        )
        pma_writes = tuple(
            self.env.write_pma_entry(entry.index, entry.config, entry.addr, size=entry.size)
            for entry in scenario.pma_entries
        )
        return TranslationScenarioState(
            scenario=scenario,
            expected_ptw_request={
                "scenario_id": str(scenario.scenario_id),
                "vpn": int(scenario.va) >> 12,
                "s2xlate": s2xlate,
                "get_gpa": int(scenario.get_gpa),
            },
            expected_outcome={"pa": expected_pa, "ok": expected_ok, **expected_metadata},
            context=context,
            pmp_writes=pmp_writes,
            pma_writes=pma_writes,
        )


__all__ = [
    "TranslationPmpPmaEntry",
    "TranslationPte",
    "TranslationScenario",
    "TranslationScenarioBuilder",
    "TranslationScenarioState",
]
