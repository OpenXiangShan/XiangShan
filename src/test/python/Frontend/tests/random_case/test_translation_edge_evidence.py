from types import SimpleNamespace as NS

import pytest

from .scenario_coordinator import ScenarioCoordinator, TimingPlan


def signal(value=0):
    return NS(value=value)


def setup():
    coordinator = ScenarioCoordinator(
        scenario_id="translation-edge", timing_plan=TimingPlan.from_seed(1, "translation_timing_cross"),
        program_map={"binary_text_verified": True, "binary_sha256": "fixture", "instructions": []},
    )
    fields = ("valid", "foldpc", "ftq_ptr_flag", "ftq_ptr_value", "ftq_offset",
              "is_rvc", "is_last_in_ftq_entry", "cross_page_ipf_fix")
    observation = NS(**{"cfvec_" + name: [signal() for _ in range(8)] for name in fields})
    for bit in (1, 2, 12, 19, 20):
        setattr(observation, f"cfvec_exception_vec_{bit}", [signal() for _ in range(8)])
    observation.cfvec_valid[3].value = 1
    observation.cfvec_foldpc[3].value = 37
    observation.cfvec_ftq_ptr_flag[3].value = 1
    observation.cfvec_ftq_ptr_value[3].value = 7
    observation.cfvec_exception_vec_12[3].value = 1
    control = NS(redirect_valid=signal(1),
                 **{"redirect_bits_" + k: signal(v) for k, v in {
                     "ftq_idx_flag": 1, "ftq_idx_value": 6, "ftq_offset": 2,
                     "target": 0x40001000, "pc": 0x40000000, "level": 0,
                 }.items()})
    env = NS(clock_reset=NS(reset=signal()), backend_to_ftq_if=control, backend_cfvec_if=observation,
             backend_to_ibuf_if=NS(decode_can_accept=signal(0)),
             backend_from_ftq_if=NS(io_backend_fromFtq_wen=signal(1),
                                    io_backend_fromFtq_ftqIdx=signal(7),
                                    io_backend_fromFtq_startPc_addr=signal(0x40000000)))
    return coordinator, env


def test_edge_evidence_preserves_identity_and_held_exception_without_claiming_delivery():
    coordinator, env = setup()
    coordinator.observe_translation_edge(12, env)
    env.backend_to_ibuf_if.decode_can_accept.value = 1
    env.backend_to_ftq_if.redirect_bits_target.value = 0
    timeline = coordinator.build_timeline(execution_status="FAIL")
    assert [e["type"] for e in timeline["timeline"]] == [
        "audit.native_ftq_write", "audit.native_redirect", "audit.native_cfvec_exception",
    ]
    packet = timeline["timeline"][-1]["payload"]
    assert (packet["slot"], packet["ftq_flag"], packet["ftq_value"], packet["folded_pc"]) == (3, 1, 7, 37)
    assert packet["can_accept"] == 0
    assert packet["redirect_valid"] == 1
    assert "pc" not in packet
    assert timeline["timeline"][1]["payload"]["target_raw"] == 0x40001000
    assert timeline["realized_events"] == []


def test_edge_evidence_is_bounded_and_drops_are_explicit():
    coordinator, env = setup()
    coordinator.maximum_timeline_events = 1
    coordinator.observe_translation_edge(12, env)
    timeline = coordinator.build_timeline(execution_status="FAIL")
    assert len(timeline["timeline"]) == 1
    assert timeline["dropped_timeline_events"] == 2


def test_edge_evidence_missing_native_signal_fails_instead_of_defaulting_to_zero():
    coordinator, env = setup()
    del env.backend_cfvec_if.cfvec_foldpc
    with pytest.raises(AttributeError):
        coordinator.observe_translation_edge(12, env)


def test_edge_evidence_requires_each_split_exception_signal():
    coordinator, env = setup()
    del env.backend_cfvec_if.cfvec_exception_vec_12
    with pytest.raises(AttributeError):
        coordinator.observe_translation_edge(12, env)


@pytest.mark.parametrize("dummy_present", [False, True])
def test_edge_evidence_marks_optional_ftq_boundary_as_unavailable(dummy_present):
    coordinator, env = setup()
    if dummy_present:
        env.backend_cfvec_if._missing_optional_attrs = {"cfvec_is_last_in_ftq_entry"}
    else:
        del env.backend_cfvec_if.cfvec_is_last_in_ftq_entry
    coordinator.observe_translation_edge(12, env)
    packet = coordinator.build_timeline(execution_status="FAIL")["timeline"][-1]["payload"]
    assert packet["is_last"] is None
    assert packet["is_last_available"] is False


def test_translation_observer_is_registered_before_regular_cycle_sampling():
    coordinator, env = setup()
    hooks = []
    env.event_sink = None
    env.set_event_sink = lambda callback: None
    env.register_pre_drive_cycle_observer = lambda callback: hooks.append(("pre", callback))
    env.register_cycle_observer = lambda callback: hooks.append(("post", callback))
    env.icache_agent = NS(configure=lambda **kwargs: None)
    coordinator.install(env)
    assert hooks == [("pre", coordinator.observe_translation_edge), ("post", coordinator.observe_cycle)]
    env.clock_reset.reset.value = 1
    coordinator.observe_translation_edge(1, env)
    assert coordinator.build_timeline(execution_status="FAIL")["timeline"] == []
