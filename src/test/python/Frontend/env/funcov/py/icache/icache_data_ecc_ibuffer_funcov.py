"""Observe Data parity injection Conditions, independently of ECC/Hwe results.

Four point-level leaves own 47 bank-set distribution bins. Only real refill,
SRAM readback, physical reads and data/code establish the input scenario.
Corrupt, exceptionType and exceptionMask are deliberately not inputs here;
DataEccRequestChecker remains responsible for those Checkpoints.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass, field
import re

from ....support.icache_data_ecc import (
    BankRead, SCENARIO_CASES, SCENARIO_POINTS, fault_banks, parity,
    request_bank_masks, scenario_bin_name,
)
from ...native_toffee import BinEvidence


GROUP = "icache_data_ecc_ibuffer"
BIN_POINTS = {
    scenario_bin_name(kind, fault_banks(kind, banks)): SCENARIO_POINTS[kind]
    for kind, cases in SCENARIO_CASES.items() for banks in cases
}
ICACHE_DATA_ECC_IBUFFER_SAMPLER_BIN_KEYS = frozenset((GROUP, name) for name in BIN_POINTS)

ROOT = "Frontend_top.Frontend."
ICACHE = ROOT + "inner_icache."
MAIN = ICACHE + "mainPipe."
DATA = ICACHE + "dataArray."
IFU = ROOT + "inner_ifu."
REQ = ICACHE + "__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_"
OUT = ICACHE + "__Vtogcov__io_toIfu_req_bits_info_0_icacheMeta_"

# All aliases below are ports/retained wires in the current V3 generated ABI.
SIGNALS = {
    "reset": ("reset",),
    "global_flush": (ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",),
    "refill_valid": (MAIN + "io_missResp_valid",),
    "refill_block": (MAIN + "__Vtogcov__io_missResp_bits_blkPAddr",),
    "refill_set": (MAIN + "__Vtogcov__io_missResp_bits_vSetIdx",),
    "refill_waymask": (ICACHE + "missUnit.__Vtogcov__io_resp_bits_waymask",),
    "refill_data": (MAIN + "io_missResp_bits_data",),
    "refill_corrupt": (MAIN + "__Vtogcov__io_missResp_bits_corrupt",),
    "refill_denied": (MAIN + "__Vtogcov__io_missResp_bits_denied",),
    "meta_write": (ICACHE + "missUnit.io_metaWrite_req_valid",),
    # The ctrlUnit port is the CSR injection source, not the refill write.
    # ICache.sv selects the actual DataArray write after that arbitration.
    "data_write": (DATA + "__Vtogcov__io_write_req_valid",),
    "data_write_ready": (DATA + "io_write_req_ready",),
    "s0_fire": (MAIN + "s0_fire",),
    "s0_flush": (MAIN + "s0_flush",),
    "ecc_enable": (ICACHE + "ctrlUnitOpt.__Vtogcov__io_eccEnable",),
    "req1_valid": (ICACHE + "__Vtogcov__io_toFtq_fromMainPipe_realTwoFetchValid",),
    "start_pc": (REQ + "vAddr_0_addr",),
    "end_position": (REQ + "endPosition",),
    "s1_valid": (MAIN + "s1_valid",),
    "s1_fire": (MAIN + "s1_fire",),
    "s1_flush": (MAIN + "s1_flush",),
    "s1_pc": (MAIN + "s1_req_0_vAddr_0_addr",),
    "response_valid": (MAIN + "s1_sramRespValid",),
    "handoff_exception": (OUT + "exception_value",),
    "handoff_mmio": (OUT + "pmpMmio",),
    "handoff_pbmt": (OUT + "itlbPbmt",),
    "handoff_paddr": (OUT + "pAddr_addr",),
    "handoff_backend": (OUT + "isBackendException",),
    "s2_pc": (MAIN + "s2_vAddr_0_0_addr",),
    "s2_cross": (MAIN + "s2_isCrossLine_0",),
    "s2_ptag": (MAIN + "s2_pTag",),
    "ifu_s1_valid": (IFU + "s1_valid",),
    # Ifu.scala uses ValidHold for s2_valid; Ifu.sv retains that register
    # as s2_valid_valid. It also gates toIBuffer.valid with s2_flush/uncache.
    "ifu_s2_valid": (IFU + "s2_valid_valid", IFU + "__Vtogcov__s2_valid_valid"),
    "ifu_s1_flush": (IFU + "s1_flush",),
    "ifu_s2_flush": (IFU + "__Vtogcov__s2_flush",),
    "ibuffer_valid": (IFU + "__Vtogcov__io_toIBuffer_valid",),
    "ibuffer_ready": (IFU + "__Vtogcov__io_toIBuffer_ready",),
    "previous_half": (IFU + "s2_prevEndIsHalfRviInfo_valid",),
}
for prefix in (REQ + "ftqIdx", MAIN + "s1_req_0_ftqIdx",
               IFU + "s1_fetchBlock_0_ftqIdx", IFU + "s2_fetchBlock_0_ftqIdx"):
    for suffix in ("_flag", "_value"):
        SIGNALS[prefix + suffix] = (prefix + suffix,)
for line in range(2):
    for path in (REQ + f"bankSel_{line}", MAIN + f"s1_mshrValidReg_0_{line}",
                 MAIN + f"s2_sramHits_0_{line}", MAIN + f"s2_wayMask_0_{line}",
                 MAIN + f"s2_wayLookupEntry_0_maybeRvcMap_{line}",
                 MAIN + f"s2_wayLookupEntry_0_metaCodes_{line}"):
        SIGNALS[path] = (path,)
for bank in range(8):
    paths = [DATA + f"banks_{bank}.io_read_req_valid",
             DATA + f"banks_{bank}.io_read_req_ready",
             DATA + f"banks_{bank}.__Vtogcov__io_read_req_bits_waymask",
             DATA + f"__Vtogcov__io_read_resp_0_datas_{bank}",
             DATA + f"__Vtogcov__io_read_resp_0_codes_{bank}"]
    paths += [DATA + f"banks_{bank}.__Vtogcov__io_read_req_bits_setIdx_{way}" for way in range(4)]
    if bank < 7:
        paths += [MAIN + f"s1_bankSramValid_0_{bank}"]
    for path in paths:
        SIGNALS[path] = (path,)


class MissingProbe(RuntimeError):
    pass


class RejectedScenario(ValueError):
    pass


def _need(condition, reason):
    if not condition:
        raise RejectedScenario(reason)


@dataclass
class Candidate:
    identity: tuple
    start_pc: int
    masks: tuple
    ways: tuple
    words: dict
    metas: tuple
    faults: frozenset
    read_cycle: int
    evidence: dict
    response_cycle: int | None = None
    handoff_cycle: int | None = None
    metadata_cycle: int | None = None


@dataclass
class DataEccScenarioCoverage:
    """Input-only transaction state; usable without a DUT in predicate tests."""

    refills: dict = field(default_factory=dict)
    pending: dict = field(default_factory=dict)
    diagnostics: list = field(default_factory=list)

    def reject(self, cycle, reason, identity=None):
        if identity is not None:
            self.pending.pop(tuple(identity), None)
        self.diagnostics.append(dict(cycle=cycle, reason=str(reason), identity=identity))
        self.diagnostics[:] = self.diagnostics[-32:]

    def start(self, cycle, r):
        identity = tuple(r["identity"])
        _need(identity not in self.pending, "FTQ identity reused before completion")
        masks = request_bank_masks(r["start_pc"], r["end_position"])
        _need(tuple(r["request_masks"]) == masks, "bankSel differs from address range")
        _need(masks in ((255, 0), (240, 15)), "outside directed fetch geometry")
        _need(r["req1_valid"] == 0 and r["ecc_enable"] == 1 and r["flush"] == 0,
              "dual request, ECC disabled or flushed")
        selected = {(line, bank) for line, mask in enumerate(masks)
                    for bank in range(8) if mask & (1 << bank)}
        faults = frozenset(r["mutations"])
        _need(bool(faults) and faults <= selected, "no fault or injection outside request")
        base = r["start_pc"] & ~63
        for line, mask in enumerate(masks):
            if not mask:
                continue
            addr, way = base + 64 * line, r["ways"][line]
            refill = self.refills.get(((addr >> 6) & 255, way))
            _need(refill is not None and refill["paddr"] == addr and refill["cycle"] < cycle,
                  "missing same-line/set/way real refill")
            meta = r["metas"][line]
            _need(meta >> 33 == addr >> 12 and (meta >> 1).bit_count() & 1 == meta & 1,
                  "Meta tag or parity is not clean")
            for bank in range(8):
                data = (refill["data"] >> (64 * bank)) & ((1 << 64) - 1)
                clean = (data << 2) | (parity(data) << 1)
                key = (line, bank)
                raw = r["words"][key]
                _need(raw == clean ^ (2 if key in faults else 0), "unrelated SRAM content changed")
                if key in faults:
                    m = r["mutations"][key]
                    _need(m["before"] == clean and m["after"] == raw and m["mask"] == 2
                          and m["paddr"] & ~63 == addr and m["way"] == way
                          and m["row"] == ((addr >> 6) & 255), "invalid parity-only mutation")
        for bank, actual in enumerate(r["banks"]):
            lines = [line for line, mask in enumerate(masks) if mask & (1 << bank)]
            if not lines:
                _need(actual.valid == 0, "unexpected physical bank read")
                continue
            line = lines[0]
            way = r["ways"][line]
            _need(actual.valid == actual.ready == 1 and actual.waymask == 1 << way
                  and actual.sets[way] == (((base >> 6) + line) & 255),
                  "required physical bank/set/way not read")
        self.pending[identity] = Candidate(
            identity, r["start_pc"], masks, tuple(r["ways"]), dict(r["words"]),
            tuple(r["metas"]), faults, cycle,
            {"read": {"cycle": cycle, **r}, "sampling_phase": "post_drive"},
        )

    def advance(self, cycle, phase, value):
        c = self.pending.get(tuple(value["identity"]))
        if c is None:
            return None
        if phase == "response":
            _need(cycle == c.read_cycle + 1 and c.response_cycle is None,
                  "SRAM response cycle/identity mismatch")
            bad = set()
            for (line, bank), raw in c.words.items():
                if not c.masks[line] & (1 << bank):
                    continue
                _need(value["sram_valid"][bank] == 1 and value["mshr_valid"][line] == 0,
                      "refill bypass")
                _need(value["data"][bank] == raw >> 2 and value["codes"][bank] == (raw >> 1) & 1,
                      "parity injection overwritten/not sampled")
                if parity(value["data"][bank]) != value["codes"][bank]:
                    bad.add((line, bank))
            _need(bad == c.faults, "returned faults differ from SRAM readback")
            c.response_cycle = cycle
        elif phase == "handoff":
            _need(c.response_cycle is not None and c.handoff_cycle is None, "handoff without response")
            _need(value["start_pc"] == c.start_pc and value["paddr"] == c.start_pc,
                  "handoff PC/physical address mismatch")
            _need(value["exception"] == value["mmio"] == value["pbmt"] == value["backend"] == 0
                  and tuple(value["mshr_valid"]) == (0, 0), "other exception or uncache/refill source")
            c.handoff_cycle = cycle
        elif phase == "metadata":
            _need(c.handoff_cycle is not None and cycle == c.handoff_cycle + 1,
                  "metadata cycle/identity mismatch")
            _need(value["start_pc"] == c.start_pc and value["cross"] == int(bool(c.masks[1])),
                  "metadata request mismatch")
            for line, mask in enumerate(c.masks):
                if mask:
                    _need(value["hits"][line] == 1 and value["waymasks"][line] == 1 << c.ways[line]
                          and value["metas"][line] == c.metas[line], "Meta error, multi-way or stale context")
            c.metadata_cycle = cycle
        elif phase == "delivery":
            _need(c.metadata_cycle is not None and value["previous_half"] == 0,
                  "missing metadata or previous half instruction")
            # The handshake closes the continuous Condition observation window.
            # Its exceptionType/mask/PC payload is checked only by the checker.
            kind = ("single" if not c.masks[1] else "both" if {line for line, _ in c.faults} == {0, 1}
                    else "line1" if any(line for line, _ in c.faults) else "line0")
            name = scenario_bin_name(kind, c.faults)
            _need(name in BIN_POINTS, "bank set has no distribution bin")
            evidence = {**c.evidence, "delivery": {"cycle": cycle, **value}}
            self.pending.pop(c.identity)
            return name, evidence
        c.evidence[phase] = {"cycle": cycle, **value}
        return None

    def observe(self, cycle, frame):
        hits = []
        if frame.get("reset") == 1:
            self.pending.clear()
            self.refills.clear()
            return hits
        if frame.get("global_flush") != 0:
            self.pending.clear()
        r = frame.get("refill")
        if r is not None:
            key = (r["vset"], r["waymask"].bit_length() - 1)
            self.refills.pop(key, None)
            if (r["corrupt"] == r["denied"] == 0 and r["meta_write"] == r["data_write"]
                    == r["ready"] == 1 and r["waymask"] in (1, 2, 4, 8)):
                self.refills[key] = {**r, "cycle": cycle}
        for identity, flush in frame.get("stages", ()):
            if flush != 0:
                self.reject(cycle, "target stage flushed or unobservable", identity)
        for phase in ("response", "handoff", "metadata", "delivery", "read"):
            value = frame.get(phase)
            if value is None:
                continue
            try:
                if phase == "read":
                    _need(frame.get("global_flush") == 0, "global flush")
                    self.start(cycle, value)
                else:
                    result = self.advance(cycle, phase, value)
                    if result is not None:
                        hits.append(result)
            except (RejectedScenario, AssertionError, KeyError, TypeError, ValueError) as exc:
                self.reject(cycle, exc, value.get("identity"))
        for identity, c in list(self.pending.items()):
            if cycle - c.read_cycle > 4096:
                self.reject(cycle, "incomplete observation window", identity)
        return hits


class Probes:
    def __init__(self, recorder):
        self.recorder = recorder
        self.paths = {}

    def read(self, key):
        names = SIGNALS[key]
        for name in names:
            value = self.recorder._read_first_dut_signal(self.recorder.env.dut, (name,))
            if value is not None:
                self.paths[key] = name
                return int(value)
        raise MissingProbe(key)

    def identity(self, prefix):
        return self.read(prefix + "_flag"), self.read(prefix + "_value")

    def capture_read(self, env):
        pc = self.read("start_pc") << 1
        masks = request_bank_masks(pc, self.read("end_position"))
        base = pc & ~63
        mutations = {}
        agent = env.icache_ecc_agent
        for m in agent.mutations:
            line = ((m.paddr & ~63) - base) // 64
            if m.restored or m.kind != "data_code" or line not in (0, 1) or not masks[line]:
                continue
            match = re.search(r"dataArray\.banks_(\d+)\.ways_", m.path)
            if match is None:
                raise RejectedScenario("unknown DataArray mutation path")
            key = (line, int(match[1]))
            if key in mutations:
                raise RejectedScenario("multiple mutations of the same SRAM word")
            mutations[key] = asdict(m)
        if not mutations:
            return None
        ways, metas, words = [0, 0], [0, 0], {}
        for line, mask in enumerate(masks):
            if mask:
                addr = base + line * 64
                resident = agent.resident_ways(addr)
                _need(len(resident) == 1, "target is not uniquely resident")
                ways[line] = resident[0]
                metas[line] = agent.read_resident_meta(addr)
                for bank in range(8):
                    words[line, bank] = agent.read_resident_bank(addr, bank=bank)
        banks = tuple(BankRead(
            self.read(DATA + f"banks_{b}.io_read_req_valid"),
            self.read(DATA + f"banks_{b}.io_read_req_ready"),
            self.read(DATA + f"banks_{b}.__Vtogcov__io_read_req_bits_waymask"),
            tuple(self.read(DATA + f"banks_{b}.__Vtogcov__io_read_req_bits_setIdx_{w}") for w in range(4)),
        ) for b in range(8))
        return dict(identity=self.identity(REQ + "ftqIdx"), start_pc=pc,
                    end_position=self.read("end_position"),
                    request_masks=tuple(self.read(REQ + f"bankSel_{line}") for line in range(2)),
                    req1_valid=self.read("req1_valid"), ecc_enable=self.read("ecc_enable"),
                    flush=self.read("s0_flush"), ways=ways, metas=metas, words=words,
                    mutations=mutations, banks=banks)

    def frame(self, env, state, cycle):
        f = dict(reset=self.read("reset"), global_flush=self.read("global_flush"), stages=[])
        if f["reset"]:
            return f
        if self.read("refill_valid"):
            f["refill"] = dict(paddr=self.read("refill_block") << 6,
                               vset=self.read("refill_set"), waymask=self.read("refill_waymask"),
                               data=self.read("refill_data"), corrupt=self.read("refill_corrupt"),
                               denied=self.read("refill_denied"), meta_write=self.read("meta_write"),
                               data_write=self.read("data_write"), ready=self.read("data_write_ready"))
        if state.pending:
            s1 = self.identity(MAIN + "s1_req_0_ftqIdx")
            c = state.pending.get(s1)
            if self.read("s1_valid") and c is not None:
                f["stages"].append((s1, self.read("s1_flush")))
                mshr = tuple(self.read(MAIN + f"s1_mshrValidReg_0_{line}") for line in range(2))
                if cycle == c.read_cycle + 1:
                    _need(self.read("response_valid") == 1, "missing SRAM response")
                    _need(self.read("s1_pc") << 1 == c.start_pc, "response PC mismatch")
                    f["response"] = dict(identity=s1,
                        data=tuple(self.read(DATA + f"__Vtogcov__io_read_resp_0_datas_{b}") for b in range(8)),
                        codes=tuple(self.read(DATA + f"__Vtogcov__io_read_resp_0_codes_{b}") for b in range(8)),
                        sram_valid=tuple(self.read(MAIN + f"s1_bankSramValid_0_{b}") if b < 7
                                         else self.read("response_valid") for b in range(8)), mshr_valid=mshr)
                if self.read("s1_fire"):
                    f["handoff"] = dict(identity=s1, start_pc=self.read("s1_pc") << 1,
                        exception=self.read("handoff_exception"), mmio=self.read("handoff_mmio"),
                        pbmt=self.read("handoff_pbmt"), backend=self.read("handoff_backend"),
                        paddr=self.read("handoff_paddr") << 1, mshr_valid=mshr)
            for stage in (1, 2):
                if self.read(f"ifu_s{stage}_valid"):
                    ident = self.identity(IFU + f"s{stage}_fetchBlock_0_ftqIdx")
                    f["stages"].append((ident, self.read(f"ifu_s{stage}_flush")))
                    c = state.pending.get(ident)
                    if c is None:
                        continue
                    if stage == 1 and c.handoff_cycle is not None and cycle == c.handoff_cycle + 1:
                        ptag = self.read("s2_ptag")
                        f["metadata"] = dict(identity=ident, start_pc=self.read("s2_pc") << 1,
                            cross=self.read("s2_cross"),
                            hits=tuple(self.read(MAIN + f"s2_sramHits_0_{line}") for line in range(2)),
                            waymasks=tuple(self.read(MAIN + f"s2_wayMask_0_{line}") for line in range(2)),
                            metas=tuple((ptag << 33)
                                | (self.read(MAIN + f"s2_wayLookupEntry_0_maybeRvcMap_{line}") << 1)
                                | self.read(MAIN + f"s2_wayLookupEntry_0_metaCodes_{line}") for line in range(2)))
                    if stage == 2 and self.read("ibuffer_valid") and self.read("ibuffer_ready"):
                        f["delivery"] = dict(identity=ident, previous_half=self.read("previous_half"))
        if self.read("s0_fire") and any(not m.restored and m.kind == "data_code"
                                       for m in getattr(env.icache_ecc_agent, "mutations", ())):
            f["read"] = self.capture_read(env)
        return f


def reset_icache_data_ecc_ibuffer_state(recorder):
    recorder = getattr(recorder, "_runtime", recorder)
    recorder._icache_data_ecc_ibuffer_state = DataEccScenarioCoverage()


def evaluate_icache_data_ecc_ibuffer_coverage(recorder, env, cycle):
    # Keep one state on SampleHub: native wrappers delegate reads, but ordinary
    # attribute writes would otherwise create a second, hidden state there.
    recorder = getattr(recorder, "_runtime", recorder)
    flags = {key: False for key in ICACHE_DATA_ECC_IBUFFER_SAMPLER_BIN_KEYS}
    evidence = BinEvidence()
    if not recorder.sampler_domain_enabled("icache"):
        return flags, evidence
    targets = set(recorder.coverage_targets.get("bin_ids", ()))
    ours = {d.bin_id for d in recorder.definitions if d.coverage_group == GROUP}
    if targets and not targets.intersection(ours):
        return flags, evidence
    if not hasattr(recorder, "_icache_data_ecc_ibuffer_state"):
        reset_icache_data_ecc_ibuffer_state(recorder)
    state = recorder._icache_data_ecc_ibuffer_state
    probes = getattr(recorder, "_icache_data_ecc_ibuffer_probes", None)
    if probes is None:
        probes = recorder._icache_data_ecc_ibuffer_probes = Probes(recorder)
    try:
        frame = recorder.domain_cycle_view(GROUP, lambda: probes.frame(env, state, int(cycle)))
        for name, detail in state.observe(int(cycle), frame):
            flags[GROUP, name] = True
            evidence.capture(GROUP, name, {**detail, "probe_paths": dict(probes.paths)})
    except (MissingProbe, RejectedScenario, AssertionError, ValueError) as exc:
        state.pending.clear()
        state.reject(cycle, exc)
        if targets.intersection(ours) and isinstance(exc, MissingProbe):
            recorder.record_contract_error("icache_data_ecc_missing_probe", cycle, {"probe": str(exc)})
    return flags, evidence
