from __future__ import annotations

import random
from dataclasses import dataclass, field
from typing import Literal, Optional, Tuple

from ..core.transactions import ProgramImage
from ..model.pmp_pma_model import PmpPmaPermissionModel
from ..support.pmp_pma import PmpPmaConfig, csr_addresses_for_entry, encode_pmp_pma_addr, encode_pmp_pma_cfg


_PAGE_SIZE = 0x1000
_SV39_MODE = 8
_SV48_MODE = 9
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
class TranslationPtwResponseOverride:
    """Patch one model-built PTW response identified by its request fields."""

    vpn: int
    s2xlate: int
    get_gpa: int = 0
    patch: Tuple[Tuple[str, object], ...] = ()

    def as_agent_config(self) -> dict:
        return {
            "vpn": int(self.vpn),
            "s2xlate": int(self.s2xlate),
            "get_gpa": int(self.get_gpa),
            "patch": dict(self.patch),
        }


@dataclass(frozen=True)
class TranslationSectorLane:
    """One S-stage level-0 lane's valididx/PPN declaration in a PTW response."""

    lane: int
    ppn: int
    valid: int = 1
    pte_present: int = 1


@dataclass(frozen=True)
class TranslationPermissionProbe:
    """One logical fetch range, optionally split at PMP/PMA attribute boundaries."""

    va: int
    size: int = 8
    segment_sizes: Tuple[int, ...] = ()


@dataclass(frozen=True)
class TranslationScenario:
    """One reproducible Sv39 translation setup shared by model and DUT."""

    scenario_id: str
    va: int
    pa: int
    payload: bytes
    page_count: int = 1
    mode: str = "sv39"
    stage2_mode: Optional[str] = None
    s1_pte: TranslationPte = field(default_factory=TranslationPte)
    s2_pte: TranslationPte = field(default_factory=TranslationPte)
    gpa: Optional[int] = None
    s2xlate: int = _S2XLATE_NONE
    get_gpa: int = 0
    ptw_response_latency: int = 3
    ptw_response_latency_max: Optional[int] = None
    ptw_response_seed: int = 1
    ptw_req_ready_strategy: Literal["always", "periodic", "random"] = "always"
    ptw_req_ready_probability: float = 1.0
    ptw_req_ready_high_cycles: int = 1
    ptw_req_ready_low_cycles: int = 0
    ptw_flush_pending_on_sfence: bool = True
    ptw_strict_bare_mode: bool = False
    ptw_response_source: Literal["model", "nemu", "compare"] = "model"
    ptw_compare_drive_source: Literal["model", "nemu"] = "model"
    ptw_nemu_adapter: str = ""
    ptw_machine_pbmte: Optional[int] = None
    ptw_hypervisor_pbmte: Optional[int] = None
    ptw_response_overrides: Tuple[TranslationPtwResponseOverride, ...] = ()
    s1_sector_lanes: Tuple[TranslationSectorLane, ...] = ()
    max_ptw_requests_per_key: Optional[int] = None
    satp_asid: int = 0
    satp_ppn: int = 0
    vsatp_asid: int = 0
    vsatp_ppn: int = 0
    hgatp_vmid: int = 0
    hgatp_ppn: int = 0
    priv_imode: int = 1
    priv_virt: int = 0
    s1_pf: int = 0
    s1_af: int = 0
    s2_gpf: int = 0
    s2_gaf: int = 0
    pmp_entries: Tuple[TranslationPmpPmaEntry, ...] = ()
    pma_entries: Tuple[TranslationPmpPmaEntry, ...] = ()
    permission_probes: Tuple[TranslationPermissionProbe, ...] = ()
    expected_path: Literal["cacheable", "uncache", "fault"] = "cacheable"
    expected_result: Literal["hit", "miss_refill", "page_fault", "access_fault", "guest_fault", "normal"] = "normal"

    def program_image(self) -> ProgramImage:
        return ProgramImage(payload=bytes(self.payload), base_addr=int(self.pa))


@dataclass(frozen=True)
class TranslationScenarioState:
    scenario: TranslationScenario
    translation_epoch: int
    expected_ptw_request: dict
    expected_outcome: dict
    expected_page_outcomes: Tuple[dict, ...]
    context: dict
    pmp_writes: Tuple[dict, ...]
    pma_writes: Tuple[dict, ...]
    expected_permission_probes: Tuple[dict, ...]


@dataclass(frozen=True)
class TranslationGeneratedScenario:
    """One reproducible constrained-random translation scenario."""

    seed: int
    ordinal: int
    scenario: TranslationScenario


class TranslationScenarioRandomizer:
    """Generate legal, replayable translation scenarios for regression streams."""

    _TRANSLATION_KINDS = ("bare", "stage1", "stage2", "all_stage", "sector", "superpage")

    def __init__(self, seed: int) -> None:
        self.seed = int(seed)
        self._random = random.Random(self.seed)

    def _pte(self, *, vmid: int = 0, allow_fault: bool = True) -> TranslationPte:
        fault = self._random.choice(("normal", "x", "a", "invalid_wr")) if allow_fault else "normal"
        return TranslationPte(
            r=0 if fault == "invalid_wr" else 1,
            w=1 if fault == "invalid_wr" else 0,
            x=0 if fault == "x" else 1,
            a=0 if fault == "a" else 1,
            u=self._random.randrange(2),
            vmid=int(vmid),
            pbmt=self._random.choice((0, 0, 1, 2)),
        )

    def _permission_entries(self, pa: int, *, pages: int, ordinal: int) -> tuple[Tuple[TranslationPmpPmaEntry, ...], Tuple[TranslationPmpPmaEntry, ...]]:
        pmp_entries = []
        pma_entries = []
        for page in range(pages):
            execute = bool(self._random.randrange(2))
            pmp_entries.append(
                TranslationPmpPmaEntry(
                    "pmp",
                    page,
                    PmpPmaConfig(match="napot", read=True, execute=execute),
                    int(pa) + page * _PAGE_SIZE,
                    size=_PAGE_SIZE,
                )
            )
            pma_entries.append(
                TranslationPmpPmaEntry(
                    "pma",
                    page,
                    PmpPmaConfig(match="napot", read=True, execute=True, cacheable=bool(self._random.randrange(2))),
                    int(pa) + page * _PAGE_SIZE,
                    size=_PAGE_SIZE,
                )
            )
        style = int(ordinal) % 3
        if style == 1:
            pmp_entries.append(
                TranslationPmpPmaEntry(
                    "pmp",
                    len(pmp_entries),
                    PmpPmaConfig(match="napot", read=True, execute=not bool(pmp_entries[0].config.execute)),
                    int(pa),
                    size=_PAGE_SIZE,
                )
            )
        elif style == 2:
            first_execute = bool(pmp_entries[0].config.execute)
            pmp_entries[0] = TranslationPmpPmaEntry(
                "pmp", 0, PmpPmaConfig(match="off"), int(pa), size=None
            )
            pmp_entries.insert(
                1,
                TranslationPmpPmaEntry(
                    "pmp", 1, PmpPmaConfig(match="tor", read=True, execute=first_execute), int(pa) + _PAGE_SIZE, size=None
                ),
            )
            for entry_index in range(2, len(pmp_entries)):
                entry = pmp_entries[entry_index]
                pmp_entries[entry_index] = TranslationPmpPmaEntry(
                    entry.kind, entry_index, entry.config, entry.addr, entry.size
                )
        return tuple(pmp_entries), tuple(pma_entries)

    def _generate_one(self, ordinal: int) -> TranslationGeneratedScenario:
        index = int(ordinal)
        if index < 0:
            raise ValueError("translation random scenario ordinal must be non-negative")
        kind = self._TRANSLATION_KINDS[index % len(self._TRANSLATION_KINDS)]
        mode = self._random.choice(("sv39", "sv48"))
        stage2_mode = self._random.choice(("sv39", "sv48"))
        page_count = 2 if self._random.randrange(4) == 0 else 1
        offset = 0xFF8 if page_count == 2 else self._random.randrange(0, 0xF80, 8)
        va = 0x8020_0000 + offset
        pa = 0x8040_0000 + offset
        gpa = 0x8060_0000 + offset
        priv_imode = self._random.choice((0, 1, 2))
        # A 1KiB stream carries 256 RVI instructions. Preserve the random
        # page offset, then extend the declared mapping if that stream crosses
        # into the next page.
        payload = b"\x13\x00\x00\x00" * 256
        page_count = max(page_count, (offset + len(payload) + _PAGE_SIZE - 1) // _PAGE_SIZE)
        pmp_entries, pma_entries = self._permission_entries(pa - offset, pages=page_count, ordinal=index)
        common = {
            "scenario_id": f"translation-random-s{self.seed}-n{index}",
            "va": va,
            "pa": pa,
            "payload": payload,
            "page_count": page_count,
            "ptw_response_latency": self._random.randrange(0, 6),
            "ptw_response_seed": self._random.randrange(1, 1 << 31),
            "priv_imode": priv_imode,
            "pmp_entries": pmp_entries,
            "pma_entries": pma_entries,
        }
        if page_count == 2:
            common["permission_probes"] = (
                TranslationPermissionProbe(va=va, size=16, segment_sizes=(8, 8)),
            )
        if kind == "bare":
            bare_common = {**common, "mode": "bare", "va": pa, "pa": pa}
            if page_count == 2:
                bare_common["permission_probes"] = (
                    TranslationPermissionProbe(va=pa, size=16, segment_sizes=(8, 8)),
                )
            scenario = TranslationScenario(**bare_common)
        elif kind == "stage1":
            response_fault = self._random.choice(("none", "page", "access"))
            only_stage1 = bool(self._random.randrange(2))
            scenario = TranslationScenario(
                **{
                    **common,
                    "mode": mode,
                    "s2xlate": _S2XLATE_ONLY_STAGE1 if only_stage1 else _S2XLATE_NONE,
                    "priv_virt": int(only_stage1),
                    "s1_pte": self._pte(),
                    "s1_pf": int(response_fault == "page"),
                    "s1_af": int(response_fault == "access"),
                }
            )
        elif kind == "stage2":
            response_fault = self._random.choice(("none", "guest_page", "guest_access"))
            scenario = TranslationScenario(
                **{
                    **common,
                    "mode": "bare",
                    "stage2_mode": stage2_mode,
                    "s2xlate": _S2XLATE_ONLY_STAGE2,
                    "priv_virt": 1,
                    "s2_pte": self._pte(),
                    "s2_gpf": int(response_fault == "guest_page"),
                    "s2_gaf": int(response_fault == "guest_access"),
                }
            )
        else:
            if kind == "sector":
                scenario = TranslationScenario(
                    **{
                        **common,
                        "mode": mode,
                        "s1_pte": self._pte(allow_fault=False),
                        "s1_sector_lanes": (
                            TranslationSectorLane(
                                lane=1,
                                ppn=(pa >> 12) + 1,
                                valid=self._random.randrange(2),
                                pte_present=1,
                            ),
                        ),
                    }
                )
            elif kind == "superpage":
                scenario = TranslationScenario(
                    **{
                        **common,
                        "mode": mode,
                        "s1_pte": TranslationPte(
                            **{**self._pte(allow_fault=False).as_mapping_kwargs(), "level": 1}
                        ),
                    }
                )
            else:
                vmid = self._random.randrange(1, 16)
                s1_fault = self._random.choice(("none", "page", "access"))
                s2_fault = self._random.choice(("none", "guest_page", "guest_access"))
                scenario = TranslationScenario(
                    **{
                        **common,
                        "mode": mode,
                        "stage2_mode": stage2_mode,
                        "s2xlate": _S2XLATE_ALL_STAGE,
                        "gpa": gpa,
                        "priv_virt": 1,
                        "s1_pte": self._pte(vmid=vmid),
                        "s2_pte": self._pte(vmid=vmid),
                        "hgatp_vmid": vmid,
                        "s1_pf": int(s1_fault == "page"),
                        "s1_af": int(s1_fault == "access"),
                        "s2_gpf": int(s2_fault == "guest_page"),
                        "s2_gaf": int(s2_fault == "guest_access"),
                    }
                )
        return TranslationGeneratedScenario(seed=self.seed, ordinal=index, scenario=scenario)

    def next(self, ordinal: int) -> TranslationGeneratedScenario:
        """Replay one ordinal without requiring callers to restore RNG state."""

        index = int(ordinal)
        if index < 0:
            raise ValueError("translation random scenario ordinal must be non-negative")
        replay = TranslationScenarioRandomizer(self.seed)
        generated = None
        for current in range(index + 1):
            generated = replay._generate_one(current)
        assert generated is not None
        return generated

    def generate(self, count: int) -> Tuple[TranslationGeneratedScenario, ...]:
        if int(count) < 0:
            raise ValueError("translation random scenario count must be non-negative")
        replay = TranslationScenarioRandomizer(self.seed)
        return tuple(replay._generate_one(ordinal) for ordinal in range(int(count)))


class TranslationScenarioBuilder:
    """Apply one Sv39 scenario to the page-table model, memory and DUT controls."""

    def __init__(self, env) -> None:
        self.env = env

    @staticmethod
    def _is_canonical(va: int, bits: int) -> bool:
        value = int(va)
        if not 0 <= value < (1 << 64):
            return False
        sign = (value >> (int(bits) - 1)) & 1
        upper = value >> int(bits)
        return upper == ((1 << (64 - int(bits))) - 1 if sign else 0)

    @staticmethod
    def _mode_name(mode: str, stage: str, *, allow_bare: bool) -> str:
        normalized = str(mode).lower()
        supported = {"sv39", "sv48"}
        if allow_bare:
            supported.add("bare")
        if normalized not in supported:
            allowed = "/".join(sorted(supported))
            raise ValueError(f"unsupported {stage} translation mode: {mode}; expected {allowed}")
        return normalized

    @staticmethod
    def _mode_csr_value(mode: str) -> int:
        return {"bare": 0, "sv39": _SV39_MODE, "sv48": _SV48_MODE}[str(mode).lower()]

    @staticmethod
    def _canonical_bits(mode: str) -> int:
        return {"sv39": 39, "sv48": 48}[str(mode).lower()]

    @staticmethod
    def _stage2_gpa_bits(mode: str) -> int:
        return {"sv39": 41, "sv48": 50}[str(mode).lower()]

    @staticmethod
    def _page_count_needed(va: int, payload_size: int) -> int:
        return max(1, ((int(va) & (_PAGE_SIZE - 1)) + max(1, int(payload_size)) + _PAGE_SIZE - 1) // _PAGE_SIZE)

    @staticmethod
    def _validate_pte(pte: TranslationPte, stage: str, mode: str) -> None:
        max_level = 3 if str(mode).lower() == "sv48" else 2
        if not 0 <= int(pte.level) <= max_level:
            raise ValueError(f"{stage} PTE level {pte.level} is outside the {mode} leaf range")
        for name, value in pte.as_mapping_kwargs().items():
            if name == "level":
                continue
            if int(value) < 0:
                raise ValueError(f"{stage} PTE field {name} must be non-negative")
            if name not in {"asid", "vmid", "pbmt"} and int(value) not in {0, 1}:
                raise ValueError(f"{stage} PTE field {name} must be 0 or 1")
        if not 0 <= int(pte.pbmt) < 4:
            raise ValueError(f"{stage} PTE PBMT must fit the two-bit PTW response field")

    @staticmethod
    def _validate_response_faults(scenario: TranslationScenario) -> None:
        fault_bits = {
            "s1_pf": scenario.s1_pf,
            "s1_af": scenario.s1_af,
            "s2_gpf": scenario.s2_gpf,
            "s2_gaf": scenario.s2_gaf,
        }
        for name, value in fault_bits.items():
            if int(value) not in {0, 1}:
                raise ValueError(f"response fault {name} must be 0 or 1")
        if int(scenario.s1_pf) and int(scenario.s1_af):
            raise ValueError("S-stage response cannot assert both s1_pf and s1_af")
        if int(scenario.s2_gpf) and int(scenario.s2_gaf):
            raise ValueError("G-stage response cannot assert both s2_gpf and s2_gaf")
        if int(scenario.s2xlate) == _S2XLATE_ONLY_STAGE2 and (int(scenario.s1_pf) or int(scenario.s1_af)):
            raise ValueError("only-stage2 translation cannot inject S-stage response faults")
        if int(scenario.s2xlate) in {_S2XLATE_NONE, _S2XLATE_ONLY_STAGE1} and (
            int(scenario.s2_gpf) or int(scenario.s2_gaf)
        ):
            raise ValueError("translation without G-stage cannot inject G-stage response faults")

    @staticmethod
    def _validate_superpage_target(va: int, target: int, pte: TranslationPte, stage: str, page_count: int) -> None:
        level = int(pte.level)
        if level == 0:
            return
        page_mask = (1 << (level * 9)) - 1
        va_vpn = int(va) >> 12
        target_ppn = int(target) >> 12
        if (va_vpn & page_mask) != (target_ppn & page_mask):
            raise ValueError(f"{stage} superpage VA and target must share the leaf page offset")
        if (va_vpn & page_mask) + int(page_count) > page_mask + 1:
            raise ValueError(f"{stage} superpage does not cover the declared page_count")

    def validate(self, scenario: TranslationScenario) -> None:
        if not str(scenario.scenario_id):
            raise ValueError("translation scenario_id must be non-empty")
        s2xlate = int(scenario.s2xlate)
        stage1_mode = self._mode_name(scenario.mode, "stage-1", allow_bare=True)
        stage2_mode = self._mode_name(scenario.stage2_mode or "sv39", "stage-2", allow_bare=False)
        if stage1_mode == "bare" and s2xlate not in {_S2XLATE_NONE, _S2XLATE_ONLY_STAGE2}:
            raise ValueError("bare translation scenarios require s2xlate=0 or s2xlate=2")
        if stage1_mode == "bare" and s2xlate == _S2XLATE_NONE and int(scenario.va) != int(scenario.pa):
            raise ValueError("bare translation scenarios require VA and PA to match")
        if stage1_mode != "bare" and s2xlate != _S2XLATE_ONLY_STAGE2 and not self._is_canonical(scenario.va, self._canonical_bits(stage1_mode)):
            raise ValueError(f"{stage1_mode} non-canonical VA is unsupported: 0x{int(scenario.va):x}")
        if int(scenario.page_count) < 1:
            raise ValueError("translation page_count must be positive")
        if not scenario.payload:
            raise ValueError("translation payload must be non-empty")
        if int(scenario.va) & (_PAGE_SIZE - 1) != int(scenario.pa) & (_PAGE_SIZE - 1):
            raise ValueError("VA and PA must have the same page offset")
        if int(scenario.page_count) < self._page_count_needed(scenario.va, len(scenario.payload)):
            raise ValueError("translation page_count does not cover the payload")
        if s2xlate not in {
            _S2XLATE_NONE,
            _S2XLATE_ONLY_STAGE1,
            _S2XLATE_ONLY_STAGE2,
            _S2XLATE_ALL_STAGE,
        }:
            raise ValueError(f"unsupported s2xlate value: {scenario.s2xlate}")
        if int(scenario.ptw_response_latency) < 0:
            raise ValueError("PTW response latency must be non-negative")
        if scenario.ptw_response_latency_max is not None and int(scenario.ptw_response_latency_max) < int(scenario.ptw_response_latency):
            raise ValueError("PTW response latency_max must be at least latency")
        if str(scenario.ptw_req_ready_strategy).lower() not in {"always", "periodic", "random"}:
            raise ValueError("unsupported PTW request ready strategy")
        if not 0.0 <= float(scenario.ptw_req_ready_probability) <= 1.0:
            raise ValueError("PTW request ready probability must be within [0, 1]")
        if int(scenario.ptw_req_ready_high_cycles) < 0 or int(scenario.ptw_req_ready_low_cycles) < 0:
            raise ValueError("PTW request ready cycle counts must be non-negative")
        if str(scenario.ptw_response_source).lower() not in {"model", "nemu", "compare"}:
            raise ValueError("unsupported PTW response source")
        if str(scenario.ptw_compare_drive_source).lower() not in {"model", "nemu"}:
            raise ValueError("unsupported PTW compare drive source")
        if str(scenario.ptw_response_source).lower() in {"nemu", "compare"} and not str(scenario.ptw_nemu_adapter).strip():
            raise ValueError("NEMU/compare PTW response source requires a NEMU adapter")
        for name, value in {
            "ptw_machine_pbmte": scenario.ptw_machine_pbmte,
            "ptw_hypervisor_pbmte": scenario.ptw_hypervisor_pbmte,
        }.items():
            if value is not None and int(value) not in {0, 1}:
                raise ValueError(f"{name} must be 0, 1 or None")
        if scenario.max_ptw_requests_per_key is not None and int(scenario.max_ptw_requests_per_key) < 1:
            raise ValueError("max_ptw_requests_per_key must be positive")
        override_keys = set()
        for override in scenario.ptw_response_overrides:
            key = (int(override.vpn), int(override.s2xlate), int(override.get_gpa))
            if key in override_keys:
                raise ValueError(f"duplicate PTW response override for request {key}")
            override_keys.add(key)
            if not override.patch:
                raise ValueError("PTW response override patch must not be empty")
        self.env.ptw_agent.validate_response_overrides(
            tuple(override.as_agent_config() for override in scenario.ptw_response_overrides)
        )
        if scenario.s1_sector_lanes:
            if stage1_mode == "bare" or s2xlate == _S2XLATE_ONLY_STAGE2 or int(scenario.s1_pte.level) != 0:
                raise ValueError("S-stage sector lanes require a non-bare level-0 stage-1 translation")
            seen_lanes = set()
            target_ppn = (
                int(scenario.gpa)
                if s2xlate == _S2XLATE_ALL_STAGE and scenario.gpa is not None
                else int(scenario.pa)
            ) >> 12
            for entry in scenario.s1_sector_lanes:
                if not 0 <= int(entry.lane) <= 7:
                    raise ValueError(f"sector lane {entry.lane} is outside the eight-lane response")
                if int(entry.lane) in seen_lanes:
                    raise ValueError(f"duplicate sector lane {entry.lane}")
                seen_lanes.add(int(entry.lane))
                if int(entry.ppn) < 0:
                    raise ValueError("sector lane PPN must be non-negative")
                if int(entry.valid) not in {0, 1} or int(entry.pte_present) not in {0, 1}:
                    raise ValueError("sector lane valid and pte_present must be 0 or 1")
                if int(entry.valid) and not int(entry.pte_present):
                    raise ValueError("a valid sector lane requires a present PTE")
                if int(entry.pte_present) and (int(entry.ppn) >> 3) != (target_ppn >> 3):
                    raise ValueError("sector lane PPN must share the response PPN high bits")
        if s2xlate == _S2XLATE_ALL_STAGE:
            if scenario.gpa is None:
                raise ValueError("all-stage translation requires gpa")
            if int(scenario.va) & (_PAGE_SIZE - 1) != int(scenario.gpa) & (_PAGE_SIZE - 1):
                raise ValueError("VA and GPA must have the same page offset")
            if int(scenario.gpa) & (_PAGE_SIZE - 1) != int(scenario.pa) & (_PAGE_SIZE - 1):
                raise ValueError("GPA and PA must have the same page offset")
            if int(scenario.s1_pte.vmid) != int(scenario.hgatp_vmid):
                raise ValueError("all-stage stage-1 PTE VMID must match hgatp_vmid")
        if s2xlate in {_S2XLATE_ONLY_STAGE2, _S2XLATE_ALL_STAGE}:
            stage2_input = int(scenario.gpa) if s2xlate == _S2XLATE_ALL_STAGE else int(scenario.va)
            if not 0 <= stage2_input < (1 << self._stage2_gpa_bits(stage2_mode)):
                raise ValueError(f"{stage2_mode}x4 GPA is unsupported: 0x{stage2_input:x}")
        self._validate_pte(scenario.s1_pte, "stage-1", stage1_mode)
        self._validate_pte(scenario.s2_pte, "stage-2", stage2_mode)
        stage1_target = int(scenario.gpa) if s2xlate == _S2XLATE_ALL_STAGE else int(scenario.pa)
        stage2_target = int(scenario.pa)
        if s2xlate != _S2XLATE_ONLY_STAGE2:
            self._validate_superpage_target(scenario.va, stage1_target, scenario.s1_pte, "stage-1", scenario.page_count)
        if s2xlate in {_S2XLATE_ONLY_STAGE2, _S2XLATE_ALL_STAGE}:
            stage2_va = int(scenario.gpa) if s2xlate == _S2XLATE_ALL_STAGE else int(scenario.va)
            self._validate_superpage_target(stage2_va, stage2_target, scenario.s2_pte, "stage-2", scenario.page_count)
        self._validate_response_faults(scenario)
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
        for probe in scenario.permission_probes:
            if int(probe.size) < 1:
                raise ValueError("translation permission probe size must be positive")
            if int(probe.va) < int(scenario.va) or int(probe.va) + int(probe.size) > int(scenario.va) + len(scenario.payload):
                raise ValueError("translation permission probe must be covered by the scenario payload")
            if probe.segment_sizes:
                if any(int(size) < 1 for size in probe.segment_sizes):
                    raise ValueError("translation permission probe segment sizes must be positive")
                if sum(int(size) for size in probe.segment_sizes) != int(probe.size):
                    raise ValueError("translation permission probe segment sizes must cover the complete probe")

    @staticmethod
    def _map_stage1_pages(
        env,
        scenario: TranslationScenario,
        target: int,
        *,
        skip_vpns: set[int] | None = None,
    ) -> None:
        pte_kwargs = scenario.s1_pte.as_mapping_kwargs()
        level = int(scenario.s1_pte.level)
        if level:
            page_mask = (1 << (level * 9)) - 1
            env.page_table.map_page(
                int(scenario.va) >> 12,
                (int(target) >> 12) & ~page_mask,
                **pte_kwargs,
            )
            return
        skipped = {int(vpn) for vpn in (skip_vpns or set())}
        for page in range(int(scenario.page_count)):
            if ((int(scenario.va) >> 12) + page) in skipped:
                continue
            env.page_table.map_page(
                (int(scenario.va) >> 12) + page,
                (int(target) >> 12) + page,
                **pte_kwargs,
            )

    @staticmethod
    def _map_stage2_pages(env, scenario: TranslationScenario, target: int) -> None:
        pte_kwargs = scenario.s2_pte.as_mapping_kwargs()
        pte_kwargs.pop("asid")
        level = int(scenario.s2_pte.level)
        if level:
            page_mask = (1 << (level * 9)) - 1
            env.page_table.map_stage2_page(
                int(target) >> 12,
                (int(scenario.pa) >> 12) & ~page_mask,
                **pte_kwargs,
            )
            return
        for page in range(int(scenario.page_count)):
            env.page_table.map_stage2_page(
                (int(target) >> 12) + page,
                (int(scenario.pa) >> 12) + page,
                **pte_kwargs,
            )

    def _expected_page_outcome(self, scenario: TranslationScenario, va: int) -> dict:
        expected_pa, expected_ok, expected_metadata = self.env.page_table.translate(
            int(va),
            s2xlate=int(scenario.s2xlate),
            priv_imode=int(scenario.priv_imode),
        )
        if expected_ok:
            permission = PmpPmaPermissionModel.check_instruction(
                expected_pa,
                pmp_entries=scenario.pmp_entries,
                pma_entries=scenario.pma_entries,
                priv_imode=int(scenario.priv_imode),
                pmp_enabled=bool(scenario.pmp_entries),
                pma_enabled=bool(scenario.pma_entries),
            )
            expected_metadata["permission"] = permission.as_dict()
            if not permission.execute_allowed:
                expected_ok = False
                expected_metadata.update(
                    {
                        "outcome": "instruction_access_fault",
                        "fault": "access_fault",
                        "reason": permission.reason,
                    }
                )

        if not expected_ok:
            expected_path = "fault"
        elif expected_metadata.get("fetch_path") != "pma":
            expected_path = "uncache"
        else:
            expected_path = "uncache" if expected_metadata["permission"]["pma_mmio"] else "cacheable"
        return {"va": int(va), "pa": expected_pa, "ok": expected_ok, "expected_path": expected_path, **expected_metadata}

    def _expected_permission_probes(self, scenario: TranslationScenario) -> Tuple[dict, ...]:
        probes = []
        for probe_index, probe in enumerate(scenario.permission_probes):
            segment_sizes = probe.segment_sizes or (int(probe.size),)
            offset = 0
            for segment_index, segment_size in enumerate(segment_sizes):
                segment_va = int(probe.va) + offset
                offset += int(segment_size)
                pa, translated, metadata = self.env.page_table.translate(
                    segment_va,
                    s2xlate=int(scenario.s2xlate),
                    priv_imode=int(scenario.priv_imode),
                )
                record = {
                    "va": segment_va,
                    "size": int(segment_size),
                }
                if probe.segment_sizes:
                    record.update(
                        {
                            "probe_index": int(probe_index),
                            "segment_index": int(segment_index),
                            "logical_va": int(probe.va),
                            "logical_size": int(probe.size),
                        }
                    )
                if not translated:
                    probes.append(
                        {
                            **record,
                            "pa": int(pa),
                            "translated": False,
                            "outcome": str(metadata.get("outcome", "fault")),
                        }
                    )
                    continue
                permission = PmpPmaPermissionModel.check_instruction(
                    int(pa),
                    pmp_entries=scenario.pmp_entries,
                    pma_entries=scenario.pma_entries,
                    priv_imode=int(scenario.priv_imode),
                    size=int(segment_size),
                    pmp_enabled=bool(scenario.pmp_entries),
                    pma_enabled=bool(scenario.pma_entries),
                )
                probes.append(
                    {
                        **record,
                        "pa": int(pa),
                        "end": int(pa) + int(segment_size) - 1,
                        "translated": True,
                        "permission": permission.as_dict(),
                    }
                )
        return tuple(probes)

    @staticmethod
    def _apply_response_fault_override(outcome: dict, scenario: TranslationScenario, page: int) -> dict:
        request_vpn = (int(scenario.va) >> 12) + int(page)
        request_key = (
            int(request_vpn),
            int(scenario.s2xlate),
            int(scenario.get_gpa),
        )
        override = next(
            (
                item
                for item in scenario.ptw_response_overrides
                if (int(item.vpn), int(item.s2xlate), int(item.get_gpa)) == request_key
            ),
            None,
        )
        if override is None:
            return outcome
        patch = dict(override.patch)
        if int(patch.get("s1_af", 0)):
            fault = "instruction_access_fault"
            fault_name = "access_fault"
        elif int(patch.get("s1_pf", 0)):
            fault = "instruction_page_fault"
            fault_name = "page_fault"
        elif int(patch.get("s2_gaf", 0)):
            fault = "instruction_access_fault"
            fault_name = "access_fault"
        elif int(patch.get("s2_gpf", 0)):
            fault = "instruction_guest_page_fault"
            fault_name = "guest_fault"
        else:
            return outcome
        return {
            **outcome,
            "ok": False,
            "expected_path": "fault",
            "outcome": fault,
            "fault": fault_name,
            "reason": "ptw_response_override",
        }

    def build(self, scenario: TranslationScenario) -> TranslationScenarioState:
        self.validate(scenario)

        self.env.page_table.clear()
        self.env.page_table.set_ptw_pbmte_policy(
            machine=scenario.ptw_machine_pbmte,
            hypervisor=scenario.ptw_hypervisor_pbmte,
            reset=True,
        )
        s2xlate = int(scenario.s2xlate)
        missing_sector_vpns = {
            (int(scenario.va) >> 12) + int(entry.lane)
            for entry in scenario.s1_sector_lanes
            if not int(entry.pte_present)
        }
        if s2xlate == _S2XLATE_ONLY_STAGE2:
            self._map_stage2_pages(self.env, scenario, int(scenario.va))
        elif s2xlate == _S2XLATE_ALL_STAGE:
            self._map_stage1_pages(self.env, scenario, int(scenario.gpa), skip_vpns=missing_sector_vpns)
            self._map_stage2_pages(self.env, scenario, int(scenario.gpa))
        elif str(scenario.mode).lower() != "bare":
            self._map_stage1_pages(self.env, scenario, int(scenario.pa), skip_vpns=missing_sector_vpns)
        if scenario.s1_sector_lanes:
            sector_base = (int(scenario.va) >> 12) & ~0x7
            lane_pte = scenario.s1_pte.as_mapping_kwargs()
            for entry in scenario.s1_sector_lanes:
                if not int(entry.pte_present):
                    continue
                self.env.page_table.map_page(
                    sector_base + int(entry.lane),
                    int(entry.ppn),
                    **{**lane_pte, "v": int(entry.pte_present)},
                )
            self.env.page_table.enable_stage1_sector_response(
                int(scenario.va) >> 12,
                lane_valid={int(entry.lane): int(entry.valid) for entry in scenario.s1_sector_lanes},
            )

        if int(scenario.s1_pf) or int(scenario.s1_af):
            self.env.page_table.set_stage1_response_fault(
                int(scenario.va) >> 12,
                page_fault=int(scenario.s1_pf),
                access_fault=int(scenario.s1_af),
            )
        if int(scenario.s2_gpf) or int(scenario.s2_gaf):
            stage2_vpn = int(scenario.gpa if s2xlate == _S2XLATE_ALL_STAGE else scenario.va) >> 12
            self.env.page_table.set_stage2_response_fault(
                stage2_vpn,
                guest_page_fault=int(scenario.s2_gpf),
                guest_access_fault=int(scenario.s2_gaf),
            )

        self.env.ptw_agent.configure(
            latency=int(scenario.ptw_response_latency),
            latency_max=(
                None if scenario.ptw_response_latency_max is None else int(scenario.ptw_response_latency_max)
            ),
            seed=int(scenario.ptw_response_seed),
            mode=str(scenario.mode).lower(),
            req_ready_strategy=str(scenario.ptw_req_ready_strategy),
            req_ready_probability=float(scenario.ptw_req_ready_probability),
            req_ready_high_cycles=int(scenario.ptw_req_ready_high_cycles),
            req_ready_low_cycles=int(scenario.ptw_req_ready_low_cycles),
            flush_pending_on_sfence=bool(scenario.ptw_flush_pending_on_sfence),
            strict_bare_mode=bool(scenario.ptw_strict_bare_mode),
            response_source=str(scenario.ptw_response_source),
            compare_drive_source=str(scenario.ptw_compare_drive_source),
            nemu_ptw_adapter=str(scenario.ptw_nemu_adapter),
            response_overrides=tuple(override.as_agent_config() for override in scenario.ptw_response_overrides),
        )
        stage2_mode = str(scenario.stage2_mode or "sv39").lower()
        self.env.page_table.set_stage2_mode(stage2_mode)
        expected_page_outcomes = tuple(
            self._apply_response_fault_override(
                self._expected_page_outcome(
                    scenario,
                    int(scenario.va) if page == 0 else (int(scenario.va) & ~(_PAGE_SIZE - 1)) + page * _PAGE_SIZE,
                ),
                scenario,
                page,
            )
            for page in range(int(scenario.page_count))
        )
        expected_permission_probes = self._expected_permission_probes(scenario)
        self.env.load_program(scenario.payload, int(scenario.pa))
        context = self.env.update_translation_context(
            satp_mode=(self._mode_csr_value(scenario.mode) if s2xlate not in {_S2XLATE_ONLY_STAGE1, _S2XLATE_ONLY_STAGE2} else 0),
            satp_asid=int(scenario.satp_asid),
            satp_ppn=int(scenario.satp_ppn),
            vsatp_mode=(self._mode_csr_value(scenario.mode) if s2xlate in {_S2XLATE_ONLY_STAGE1, _S2XLATE_ALL_STAGE} else None),
            vsatp_asid=(int(scenario.vsatp_asid) if s2xlate in {_S2XLATE_ONLY_STAGE1, _S2XLATE_ALL_STAGE} else None),
            vsatp_ppn=(int(scenario.vsatp_ppn) if s2xlate in {_S2XLATE_ONLY_STAGE1, _S2XLATE_ALL_STAGE} else None),
            hgatp_mode=(self._mode_csr_value(stage2_mode) if s2xlate in {_S2XLATE_ONLY_STAGE2, _S2XLATE_ALL_STAGE} else None),
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
            translation_epoch=int(self.env.translation_epoch),
            expected_ptw_request={
                "scenario_id": str(scenario.scenario_id),
                "vpn": self.env.page_table.normalize_ptw_vpn(int(scenario.va) >> 12),
                "s2xlate": s2xlate,
                "get_gpa": int(scenario.get_gpa),
            },
            expected_outcome=dict(expected_page_outcomes[0]),
            expected_page_outcomes=expected_page_outcomes,
            context=context,
            pmp_writes=pmp_writes,
            pma_writes=pma_writes,
            expected_permission_probes=expected_permission_probes,
        )


__all__ = [
    "TranslationPmpPmaEntry",
    "TranslationPtwResponseOverride",
    "TranslationPermissionProbe",
    "TranslationSectorLane",
    "TranslationGeneratedScenario",
    "TranslationPte",
    "TranslationScenarioRandomizer",
    "TranslationScenario",
    "TranslationScenarioBuilder",
    "TranslationScenarioState",
]
