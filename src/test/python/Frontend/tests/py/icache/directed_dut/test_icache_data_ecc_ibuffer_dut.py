"""Real DataArray parity faults must reach the owning request at IBuffer.

Each parameter runs a clean baseline and a faulted refetch. The oracle joins
FTQ requests, physical SRAM bank reads, returned parity and IBuffer handshakes
at the environment's post-drive observation boundary. These four leaves have
no canonical coverage bins yet: JSON results are checker evidence, not HITs.
"""

from __future__ import annotations

import json
import os
import re
import traceback
from dataclasses import asdict, is_dataclass
from itertools import product
from pathlib import Path

import pytest

from env.checkers.icache_data_ecc_checker import (
    BankRead,
    DataEccRequestChecker,
    parity,
)
from env.runtime.frontend_init import FrontendInitConfig, ProgramImage, initialize_frontend
from env.runtime.pylib import frontend_build_root_path
from tests.py.icache.directed_dut.programs import CNOP, build_idle_loop_program
from tests.py.icache.directed_dut.support import (
    cycle_limit,
    make_program_init_config,
    read_dut_signal,
    wait_until,
)


pytestmark = [
    pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT"),
    pytest.mark.parametrize("frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True),
]

_ROOT = "Frontend_top.Frontend."
_ICACHE = _ROOT + "inner_icache."
_MAIN = _ICACHE + "mainPipe."
_DATA = _ICACHE + "dataArray."
_IFU = _ROOT + "inner_ifu."
_BASE = 0x80040000
_SAFE = _BASE + 0x1000
_TITLES = {
    "single": "单行请求的Data bank注错传递Hwe至IBuffer",
    "line0": "跨行请求仅cacheline0的Data bank注错传递Hwe至IBuffer",
    "line1": "跨行请求仅cacheline1的Data bank注错传递Hwe至IBuffer",
    "both": "跨行请求两条cacheline的Data bank同时注错传递Hwe至IBuffer",
}


class _Probes:
    """Use live pins throughout; never mix in the coverage read-once snapshot."""

    def __init__(self, env):
        self.env = env
        self.paths = {}

    def read(self, path, *fallbacks):
        key = (path, *fallbacks)
        if key not in self.paths:
            for candidate in key:
                if read_dut_signal(self.env, candidate) is not None:
                    self.paths[key] = candidate
                    break
            else:
                raise AssertionError({"reason": "required ECC probe missing", "paths": key})
        return read_dut_signal(self.env, self.paths[key], required=True)

    def identity(self, prefix):
        return self.read(prefix + "_flag"), self.read(prefix + "_value")

    def bank_reads(self):
        return tuple(BankRead(
            self.read(_DATA + f"banks_{bank}.io_read_req_valid"),
            self.read(_DATA + f"banks_{bank}.io_read_req_ready"),
            self.read(_DATA + f"banks_{bank}.__Vtogcov__io_read_req_bits_waymask"),
            tuple(self.read(_DATA + f"banks_{bank}.__Vtogcov__io_read_req_bits_setIdx_{way}")
                  for way in range(4)),
        ) for bank in range(8))

    def request(self):
        prefix = _ICACHE + "__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_"
        return dict(
            start_pc=self.read(prefix + "vAddr_0_addr") << 1,
            end_position=self.read(prefix + "endPosition"),
            identity=self.identity(prefix + "ftqIdx"),
            request_masks=tuple(self.read(prefix + f"bankSel_{line}") for line in range(2)),
            # The optimized MainPipe signal is exported through this output.
            req1_valid=self.read(_ICACHE + "__Vtogcov__io_toFtq_fromMainPipe_realTwoFetchValid"),
        )

    def delivery(self):
        enq = self.read(_IFU + "__Vtogcov__io_toIBuffer_bits_enqEnable")
        valid = self.read(_IFU + "__Vtogcov__io_toIBuffer_bits_valid")
        active = valid & enq
        slots = {}
        for slot in range(active.bit_length()):
            if active & (1 << slot):
                slots[slot] = dict(
                    identity=self.identity(_ROOT + f"_inner_ifu_io_toIBuffer_bits_ftqPtr_{slot}"),
                    # FetchToIBuffer.pc is wired directly to this retained vector.
                    pc=self.read(_IFU + f"s2_alignedInstrPcVec_{slot}_addr") << 1,
                )
        assert self.read(_IFU + "__Vtogcov__io_toIBuffer_bits_isBackendException") == 0
        assert self.read(_IFU + "__Vtogcov__io_toIBuffer_bits_exceptionCrossPage") == 0
        return dict(
            exception=self.read(_IFU + "__Vtogcov__io_toIBuffer_bits_exceptionType_value"),
            exception_mask=self.read(_IFU + "__Vtogcov__io_toIBuffer_bits_exceptionMask"),
            valid_mask=valid, enq_mask=enq, slots=slots,
            previous_half=self.read(_IFU + "s2_prevEndIsHalfRviInfo_valid"),
        )


class _ScenarioObserver:
    def __init__(self, env, target):
        self.env, self.target = env, target
        self.probes = _Probes(env)
        self.checker = None
        self.error = None
        self.tail = []
        env.register_cycle_observer(self.observe)

    def observe(self, cycle, env):
        if self.error is not None or self.checker is None or self.checker.delivery is not None:
            return
        try:
            self._observe(cycle, env)
        except Exception as exc:
            # Clock wrappers may swallow callback exceptions; rethrow in wait().
            self.error = exc
            env.logger.error("Data ECC observer failed at cycle %d: %s", cycle, exc)

    def _observe(self, cycle, env):
        if int(env.clock_reset.reset.value):
            return
        p, checker = self.probes, self.checker
        if p.read(_MAIN + "s0_fire"):
            req = p.request()
            self.tail.append(dict(cycle=cycle, **req))
            self.tail = self.tail[-16:]
            if req["start_pc"] == self.target and checker.identity is None:
                for (line, bank), raw in checker.words.items():
                    actual = env.icache_ecc_agent.read_resident_bank(_BASE + 64 * line, bank=bank)
                    assert actual == raw, ("SRAM word changed before read", line, bank)
                checker.check_read(
                    cycle=cycle, **req,
                    # Generated ICache.sv wires this output to mainPipe.io_eccEnable.
                    ecc_enable=p.read(_ICACHE + "ctrlUnitOpt.__Vtogcov__io_eccEnable"),
                    flush=p.read(_MAIN + "s0_flush"), banks=p.bank_reads(),
                )
        if checker.identity is None:
            return
        assert p.read(_ICACHE + "__Vtogcov__io_fromFtq_redirectFlush") == 0, "global flush during target request"
        s1_identity = p.identity(_MAIN + "s1_req_0_ftqIdx")
        if s1_identity == checker.identity:
            assert p.read(_MAIN + "s1_flush") == 0, "target MainPipe request flushed"
            if cycle == checker.read_cycle + 1:
                response_valid = p.read(_MAIN + "s1_sramRespValid")
                assert response_valid == 1, "missing SRAM response valid"
                checker.check_response(
                    cycle=cycle, identity=s1_identity,
                    start_pc=p.read(_MAIN + "s1_req_0_vAddr_0_addr") << 1,
                    data=tuple(p.read(_DATA + f"__Vtogcov__io_read_resp_0_datas_{bank}") for bank in range(8)),
                    codes=tuple(p.read(_DATA + f"__Vtogcov__io_read_resp_0_codes_{bank}") for bank in range(8)),
                    # Bank7 always belongs to line0 in this ABI; generated
                    # Helpers.getBankValid aliases its valid to s1_sramRespValid.
                    sram_valid=tuple(p.read(_MAIN + f"s1_bankSramValid_0_{bank}")
                                     if bank < 7 else response_valid for bank in range(8)),
                    mshr_valid=tuple(p.read(_MAIN + f"s1_mshrValidReg_0_{line}") for line in range(2)),
                )
            if p.read(_MAIN + "s1_fire"):
                out = _ICACHE + "__Vtogcov__io_toIfu_req_bits_info_0_icacheMeta_"
                checker.check_handoff(
                    cycle=cycle, identity=s1_identity,
                    exception=p.read(out + "exception_value"), mmio=p.read(out + "pmpMmio"),
                    pbmt=p.read(out + "itlbPbmt"),
                    mshr_valid=tuple(p.read(_MAIN + f"s1_mshrValidReg_0_{line}") for line in range(2)),
                )
        if checker.handoff_cycle is None:
            return
        if cycle == checker.handoff_cycle + 1:
            checker.check_ecc(
                cycle=cycle, identity=p.identity(_IFU + "s1_fetchBlock_0_ftqIdx"),
                start_pc=p.read(_MAIN + "s2_vAddr_0_0_addr") << 1,
                cross_line=p.read(_MAIN + "s2_isCrossLine_0"),
                hits=tuple(p.read(_MAIN + f"s2_sramHits_0_{line}") for line in range(2)),
                corrupt=tuple(p.read(_ICACHE + f"__Vtogcov__io_toIfu_corrupt_0_{line}") for line in range(2)),
            )
        for stage in (1, 2):
            if p.identity(_IFU + f"s{stage}_fetchBlock_0_ftqIdx") == checker.identity:
                assert p.read(_IFU + ("s1_flush" if stage == 1 else "__Vtogcov__s2_flush")) == 0, "target IFU request flushed"
        if (p.read(_IFU + "__Vtogcov__io_toIBuffer_valid")
                and p.read(_IFU + "__Vtogcov__io_toIBuffer_ready")
                and p.identity(_IFU + "s2_fetchBlock_0_ftqIdx") == checker.identity):
            checker.check_delivery(cycle=cycle, identity=checker.identity,
                                   valid=1, ready=1, **p.delivery())

    def wait(self, predicate, label):
        def finished():
            if self.error is not None:
                raise self.error
            return predicate()
        wait_until(self.env, finished,
                   max_cycles=cycle_limit("TB_ICACHE_DATA_ECC_WAIT", 4096), label=label,
                   diagnostics=lambda: {"read_tail": self.tail,
                       "checker": None if self.checker is None else self.checker.evidence})


def _read_words(env, masks):
    agent = env.icache_ecc_agent
    ways, words = [0, 0], {}
    for line, mask in enumerate(masks):
        if not mask:
            continue
        addr = _BASE + 64 * line
        ways[line] = agent.wait_resident(addr)
        meta = agent.read_resident_meta(addr)
        assert (meta >> 1).bit_count() & 1 == meta & 1, "Meta parity is not clean"
        # Include unselected banks to prove their contents remain unchanged.
        for bank in range(8):
            words[line, bank] = agent.read_resident_bank(addr, bank=bank)
    return tuple(ways), words


def _redirect(env, target):
    assert not env.get_errors(), env.get_errors()
    env.monitor.set_expected_pc(target)
    env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)


def _park(env, observer):
    observer.checker = None
    start = int(env.current_cycle)
    _redirect(env, _SAFE)
    observer.wait(lambda: any(o.pc == _SAFE and o.cycle > start
                             for o in env.monitor.observations[-64:]), "safe loop before target refetch")
    env.step(8)


def _json_value(value):
    if is_dataclass(value):
        return asdict(value)
    raise TypeError(f"not JSON serializable: {type(value).__name__}")


def _run_scenario(env, request, kind, faults):
    cross = kind != "single"
    target = _BASE + (0x20 if cross else 0)
    masks = (0xF0, 0x0F) if cross else (0xFF, 0)
    baseline = measured = observer = None
    result = {"testpoint": _TITLES[kind], "fault_banks": faults, "target": target,
              "pytest_nodeid": request.node.nodeid, "bank_masks": masks}
    manifest = frontend_build_root_path() / f'frontend_build_manifest.{os.getenv("TB_FRONTEND_SIM", "verilator")}.json'
    if manifest.is_file():
        result["dut_build"] = json.loads(manifest.read_text())
    try:
        initialize_frontend(env, make_program_init_config(
            env.config, reset_vector=target,
            images=(ProgramImage(CNOP.to_bytes(2, "little") * 512, _BASE),
                    ProgramImage(build_idle_loop_program(), _SAFE)),
            icache_parameters={"hit_latency": 8, "miss_latency": 8, "miss_rate": 1.0, "seed": 0xECC},
        ))
        observer = _ScenarioObserver(env, target)
        for line, mask in enumerate(masks):
            if mask:
                env.icache_ecc_agent.wait_resident(_BASE + 64 * line, max_cycles=4096)
        # Warm through the normal FTQ/refill path. An immediate soft-prefetch
        # pulse after top-level reset can be lost while inner reset is asserted.
        # Parking also drains the initial miss before either measured refetch.
        _park(env, observer)
        ways, clean_words = _read_words(env, masks)
        for raw in clean_words.values():
            assert parity(raw >> 2) == (raw >> 1) & 1, "baseline data parity is not clean"
        baseline = DataEccRequestChecker(target, masks, ways, clean_words)
        observer.checker = baseline
        _redirect(env, target)
        observer.wait(lambda: baseline.delivery is not None, "clean target IBuffer delivery")
        baseline.assert_complete()

        _park(env, observer)
        assert _read_words(env, masks) == (ways, clean_words), "target SRAM changed before injection"
        mutations = []
        for line, bank in faults:
            item = env.icache_ecc_agent.inject_data_ecc(_BASE + 64 * line, bank=bank)
            assert item.before == clean_words[line, bank] and item.before ^ item.after == 2
            mutations.append(item)
        for item in mutations:
            env.icache_ecc_agent.verify_persisted(item)
        bad_ways, bad_words = _read_words(env, masks)
        assert bad_ways == ways
        for key, before in clean_words.items():
            assert bad_words[key] == before ^ (2 if key in faults else 0), ("unexpected SRAM change", key)
        measured = DataEccRequestChecker(target, masks, ways, bad_words, faults)
        observer.checker = measured
        _redirect(env, target)
        observer.wait(lambda: measured.delivery is not None, "faulted target Hwe at IBuffer")
        measured.assert_complete()
        assert not env.get_errors(), env.get_errors()
        result["outcome"] = "passed"
    except Exception:
        result["outcome"] = "failed"
        result["failure"] = traceback.format_exc()
        raise
    finally:
        if observer is not None:
            observer.checker = None
            result["probe_paths"] = {str(key): value for key, value in observer.probes.paths.items()}
            result["read_tail"] = observer.tail
        result["baseline"] = None if baseline is None else baseline.evidence
        result["faulted"] = None if measured is None else measured.evidence
        result["checker_errors"] = env.get_errors()
        try:
            env.icache_ecc_agent.restore_all()
        finally:
            directory = Path(env.waveform_path).parent / "checks"
            directory.mkdir(parents=True, exist_ok=True)
            name = re.sub(r"[^A-Za-z0-9_.=-]", "_", request.node.name)
            path = directory / f"{name}.data_ecc.json"
            path.write_text(json.dumps(result, ensure_ascii=False, indent=2, default=_json_value) + "\n")
            request.node.user_properties.append(("data_ecc_evidence", str(path)))


def _bank_id(banks):
    return "banks_" + "_".join(map(str, banks))


_SINGLE_BANKS = [(bank,) for bank in range(8)] + [(0, 1), (0, 7), tuple(range(8))]
_LINE0_BANKS = [(bank,) for bank in range(4, 8)] + [(6, 7), tuple(range(4, 8))]
_LINE1_BANKS = [(bank,) for bank in range(4)] + [(1, 2), tuple(range(4))]
# All 16 individual bank pairs, followed by the remaining cardinality classes.
_BOTH_BANKS = [((left,), (right,)) for left, right in product(range(4, 8), range(4))]
_BOTH_BANKS += [pair for pair in product(((6,), (6, 7), tuple(range(4, 8))),
                                       ((1,), (0, 1, 2), tuple(range(4))))
                if pair not in _BOTH_BANKS]


@pytest.mark.parametrize("banks", _SINGLE_BANKS, ids=_bank_id)
def test_icache_data_ecc_ibuffer_single_line(env, request, banks):
    _run_scenario(env, request, "single", tuple((0, bank) for bank in banks))


@pytest.mark.parametrize("banks", _LINE0_BANKS, ids=_bank_id)
def test_icache_data_ecc_ibuffer_cross_line0(env, request, banks):
    _run_scenario(env, request, "line0", tuple((0, bank) for bank in banks))


@pytest.mark.parametrize("banks", _LINE1_BANKS, ids=_bank_id)
def test_icache_data_ecc_ibuffer_cross_line1(env, request, banks):
    _run_scenario(env, request, "line1", tuple((1, bank) for bank in banks))


@pytest.mark.parametrize("banks", _BOTH_BANKS,
                         ids=lambda pair: "line0_" + _bank_id(pair[0]) + "-line1_" + _bank_id(pair[1]))
def test_icache_data_ecc_ibuffer_cross_both(env, request, banks):
    faults = tuple((line, bank) for line, selected in enumerate(banks) for bank in selected)
    _run_scenario(env, request, "both", faults)
