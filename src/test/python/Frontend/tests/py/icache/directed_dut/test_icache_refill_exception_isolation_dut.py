"""Check exception isolation between adjacent TileLink refill transactions."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from tests.py.icache.directed_dut.programs import build_nop_program
from tests.py.icache.directed_dut.support import (
    SetBackendCanAccept,
    SetIcachePrefetch,
    make_program_init_config,
    wait_until,
)
from tests.py.support.dut_support import read_dut_signal


pytestmark = pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT",
)
_BASE = 0x8020_0000


def _read(env, suffix: str, *, block: str = "missUnit") -> int:
    path = "inner_icache." + block + "." + suffix
    return read_dut_signal(
        env, (path, "Frontend_top.Frontend." + path),
        required=True, wide=True,
    )


def _observe(env) -> list[dict]:
    samples = []

    def capture(cycle, active):
        if active.clock_reset.reset.value:
            return
        interface = active.icache_if
        sample = {
            "cycle": int(cycle),
            "reset": int(active.clock_reset.reset.value),
            "fencei": int(active.clock_reset.io_fencei.value),
            # Ftq.scala selects backendRedirect over ifuRedirect; either valid
            # input therefore asserts the MissUnit's combinational flush.
            "flush": int(any(read_dut_signal(active,
                "Frontend_top.Frontend.inner_ftq." + name, required=True,
            ) for name in ("backendRedirect_valid", "ifuRedirect_valid"))),
            "d_valid": int(interface.d_valid.value),
            "d_source": int(interface.d_bits_source.value),
            "d_data": int(interface.d_bits_data.value),
            "d_denied": int(interface.d_bits_denied.value),
            "d_corrupt": int(interface.d_bits_corrupt.value),
            "last_fire_next": _read(active, "lastFireNext"),
            "id_next": _read(active, "idNext"),
            "resp_valid": _read(active, "io_resp_valid"),
            "corrupt": _read(active, "corruptReg"),
            "denied": _read(active, "deniedReg"),
        }
        if sample["resp_valid"]:
            for field in ("blkPAddr", "vSetIdx", "waymask", "data"):
                sample[field] = _read(active, "io_resp_bits_" + field)
            for port in ("metaWrite", "dataWrite"):
                sample[port] = _read(active, f"io_{port}_req_valid")
                sample[port + "_vset"] = _read(active, f"io_{port}_req_bits_vSetIdx")
                sample[port + "_waymask"] = _read(active, f"io_{port}_req_bits_waymask")
            sample["array_write"] = _read(active, "io_write_req_valid", block="dataArray.banks_0")
            sample["write_data"] = sum(
                _read(active, "io_write_req_bits_entry_data", block=f"dataArray.banks_{bank}") << (64 * bank)
                for bank in range(8)
            )
        sample["mshrs"] = {
            source: {
                field: _read(active, f"allMshr_{source}." + field)
                for field in ("valid", "issue", "blkPAddr", "vSetIdx", "way")
            }
            for source in {record["source"] for record in active.icache_agent.request_records}
        }
        samples.append(sample)

    # Read the D inputs before the agent drives the following beat. This also
    # samples native DUT outputs at one consistent boundary for every cycle.
    env.register_pre_drive_cycle_observer(capture)
    return samples


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True,
)
@pytest.mark.parametrize("denied,beat", [
    pytest.param(0, 0, id="beat0-corrupt", marks=pytest.mark.funcov_bins("BIN-1187")),
    pytest.param(0, 1, id="beat1-corrupt", marks=pytest.mark.funcov_bins("BIN-1188")),
    pytest.param(0, None, id="both-beats-corrupt", marks=pytest.mark.funcov_bins("BIN-1189")),
    pytest.param(1, 0, id="beat0-denied", marks=pytest.mark.funcov_bins("BIN-1190")),
    pytest.param(1, 1, id="beat1-denied", marks=pytest.mark.funcov_bins("BIN-1191")),
    pytest.param(1, None, id="both-beats-denied", marks=pytest.mark.funcov_bins("BIN-1192")),
])
def test_icache_fault_then_clean_back_to_back_dut(env, denied, beat) -> None:
    _check_refill_pair(env, denied=denied, beat=beat)


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True,
)
def test_icache_clean_then_clean_back_to_back_dut(env) -> None:
    """Control case for the same traffic, probes, and transaction checks."""
    _check_refill_pair(env, denied=None, beat=None)


def _check_refill_pair(env, *, denied: int | None, beat: int | None) -> None:
    # A 32-bit instruction beginning at byte 62 needs both cache lines, so the
    # second demand miss does not depend on the first (possibly faulted) refill.
    data = bytearray(build_nop_program(16))
    data.extend((0x00100013).to_bytes(4, "little") * 16)
    data[62:66] = (0x00000013).to_bytes(4, "little")
    if denied is not None:
        env.icache_agent.inject_response_fault_at(_BASE, corrupt=1, denied=denied, beat=beat)
    samples = _observe(env)
    initialize_frontend(env, make_program_init_config(
        env.config,
        reset_vector=_BASE + 62,
        images=(ProgramImage(bytes(data), _BASE),),
        icache_parameters={
            "hit_latency": 64, "miss_latency": 64, "miss_rate": 0.0,
            "seed": 0xB2B, "back_to_back_rate": 1.0,
        },
        before_reset=(SetIcachePrefetch(False),),
        after_reset=(SetBackendCanAccept(False),),
    ))
    agent = env.icache_agent
    wait_until(
        env, lambda: len(agent.response_records) >= 4,
        max_cycles=2000, label="two complete cache-line responses",
        diagnostics=lambda: {"requests": agent.request_records, "responses": agent.response_records},
    )
    env.step(4)

    requests = agent.request_records[:2]
    assert [record["address"] for record in requests] == [_BASE, _BASE + 64], requests
    sources = [record["source"] for record in requests]
    assert sources[0] != sources[1], requests
    records = agent.response_records[:4]
    assert [record["source"] for record in records] == [sources[0]] * 2 + [sources[1]] * 2
    assert [record["beat_idx"] for record in records] == [0, 1, 0, 1]
    start = records[0]["cycle"]
    assert [record["cycle"] for record in records] == list(range(start, start + 4)), records
    assert requests[1]["cycle"] < start, requests

    d_beats = [sample for sample in samples if sample["d_valid"]][:4]
    assert len(d_beats) == 4, samples[-8:]
    assert [sample["cycle"] for sample in d_beats] == list(range(d_beats[0]["cycle"], d_beats[0]["cycle"] + 4))
    assert [sample["d_source"] for sample in d_beats] == [sources[0]] * 2 + [sources[1]] * 2
    for index, sample in enumerate(d_beats):
        fault = denied is not None and index < 2 and (beat is None or index == beat)
        assert (sample["d_corrupt"], sample["d_denied"]) == (int(fault), denied if fault else 0)
        assert sample["d_data"] == int.from_bytes(data[index * 32:(index + 1) * 32], "little")

    window = [sample for sample in samples if d_beats[0]["cycle"] <= sample["cycle"] <= d_beats[-1]["cycle"] + 2]
    assert all(not sample["reset"] and not sample["fencei"] and not sample["flush"] for sample in window), window
    responses = [sample for sample in window if sample["resp_valid"]]
    assert len(responses) == 2, responses
    if denied is not None and env.functional_coverage is not None:
        location = "both" if beat is None else f"beat{beat}"
        kind = "denied" if denied else "corrupt"
        assert env.functional_coverage.key_hit(
            "icache_missunit_refill", f"fault_then_clean_b2b_{kind}_{location}",
        ), "back-to-back input scenario was not sampled by native Toffee coverage"
    env.logger.info(
        "refill exception isolation: denied=%s beat=%s requests=%s D=%s responses=%s",
        denied, beat, requests,
        [{key: sample[key] for key in ("cycle", "d_source", "d_corrupt", "d_denied")} for sample in d_beats],
        [{key: sample[key] for key in (
            "cycle", "id_next", "blkPAddr", "corrupt", "denied", "metaWrite", "dataWrite", "array_write",
        )} for sample in responses],
    )
    for index, response in enumerate(responses):
        source = sources[index]
        address = requests[index]["address"]
        prior = next(sample for sample in reversed(samples) if sample["cycle"] < d_beats[index * 2]["cycle"])
        mshr = prior["mshrs"][source]
        assert mshr["valid"] == mshr["issue"] == 1, mshr
        assert mshr["blkPAddr"] == address >> 6, mshr
        assert response["last_fire_next"] == 1 and response["id_next"] == source, response
        assert response["blkPAddr"] == address >> 6, response
        assert response["vSetIdx"] == mshr["vSetIdx"]
        assert response["waymask"] == 1 << mshr["way"]
        expected_data = int.from_bytes(data[index * 64:(index + 1) * 64], "little")
        assert response["data"] == response["write_data"] == expected_data, response
        fault = denied is not None and index == 0
        assert (response["corrupt"], response["denied"]) == (int(fault), denied if fault else 0), {
            "reason": "refill exception state crossed transaction boundary",
            "source": source, "address": hex(address), "responses": responses,
        }
        assert response["metaWrite"] == response["dataWrite"] == int(not fault), response
        assert response["array_write"] == int(not fault), response
        for port in ("metaWrite", "dataWrite"):
            assert response[port + "_vset"] == mshr["vSetIdx"]
            assert response[port + "_waymask"] == 1 << mshr["way"]
        released = next(sample for sample in samples if sample["cycle"] == response["cycle"] + 1)
        assert released["mshrs"][source]["valid"] == 0, released
    assert responses[0]["mshrs"][sources[1]]["valid"] == 1, responses[0]
    assert not env.monitor.get_errors()
