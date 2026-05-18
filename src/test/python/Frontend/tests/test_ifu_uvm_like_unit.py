from __future__ import annotations

from env.model.ifu_reference_model import IFUFetchMonitorAdapter, SequentialIFUReferenceModel
from env.monitors.frontend_monitor import Observation
from env.sequences import BaremodeSequentialIFUScenario


def test_baremode_sequential_ifu_scenario_defines_program_and_expected_pcs() -> None:
    scenario = BaremodeSequentialIFUScenario(base_addr=0x80000000, words=8, expected_fetches=4)

    image = scenario.program_image()

    assert image.base_addr == 0x80000000
    assert len(image.payload) == 32
    assert scenario.expected_pcs() == (0x80000000, 0x80000004, 0x80000008, 0x8000000C)


def test_ifu_fetch_adapter_and_reference_model_match_sequential_stream() -> None:
    observations = [
        Observation(cycle=1, slot=0, pc=0x80000000, instr=0x13, is_rvc=False, pred_taken=False),
        Observation(cycle=1, slot=1, pc=0x80000004, instr=0x13, is_rvc=False, pred_taken=False),
        Observation(cycle=2, slot=0, pc=0x80000008, instr=0x13, is_rvc=False, pred_taken=False),
    ]

    txns = IFUFetchMonitorAdapter().from_observations(observations)
    result = SequentialIFUReferenceModel(expected_pcs=(0x80000000, 0x80000004, 0x80000008)).compare(txns)

    assert txns[0].fetch_path == "icache_seq"
    assert result.passed is True
    assert result.checked == 3
