from __future__ import annotations

from dataclasses import replace
import os

import pytest

from env.core.transactions import BackendRedirectClass, RedirectTxn
from env.sequences import (
    InjectRedirectSequence,
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
)
from env.support import PmpPmaConfig, fold_pc


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_PA = 0x8040_0F00
_SINGLE_PAGE_VA = 0x8020_0800
_SINGLE_PAGE_PA = 0x8040_0800
_PAGE_SIZE = 0x1000
_PAYLOAD = b"\x13\x00\x00\x00" * 64
_CROSS_PAGE_PAYLOAD = b"\x13\x00\x00\x00" * 512
_OBSERVED_EXCEPTION_BITS = (1, 2, 12, 19, 20)


def _entry(
    kind: str,
    index: int,
    config: PmpPmaConfig,
    addr: int,
    size: int | None = None,
) -> TranslationPmpPmaEntry:
    return TranslationPmpPmaEntry(kind, index, config, addr, size=size)


def _scenario(
    scenario_id: str,
    *,
    pmp_entries: tuple[TranslationPmpPmaEntry, ...],
    pma_entries: tuple[TranslationPmpPmaEntry, ...],
    va: int = _SINGLE_PAGE_VA,
    pa: int = _SINGLE_PAGE_PA,
    payload: bytes = _PAYLOAD,
    page_count: int = 1,
    priv_imode: int = 1,
    expected_fault: str | None = "instruction_access_fault",
    s1_pte: TranslationPte = TranslationPte(),
    ptw_machine_pbmte: int | None = None,
) -> TranslationScenario:
    return TranslationScenario(
        scenario_id=scenario_id,
        va=va,
        pa=pa,
        payload=payload,
        page_count=page_count,
        mode="sv39",
        priv_imode=priv_imode,
        s1_pte=s1_pte,
        ptw_machine_pbmte=ptw_machine_pbmte,
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
        expected_path="fault" if expected_fault is not None else "cacheable",
        expected_result="access_fault" if expected_fault is not None else "normal",
    )


def _wait_until(env, predicate, *, description: str, max_cycles: int = 2000) -> None:
    for _ in range(max_cycles):
        if predicate():
            return
        env.step(1)
    if not predicate():
        raise AssertionError(f"timed out waiting for {description}")


def _assert_exact_oracle_fault(env, *, pc: int, expected_fault: str) -> None:
    exception_records = [
        record
        for record in env.translation_oracle.get_stats()["records"]
        if record["kind"] == "cfvec_exception"
    ]
    assert any(
        record["fault"] == expected_fault
        and (
            int(record["pc"]) == int(pc)
            or (
                int(record["pc"]) == 0
                and int(record["folded_pc"]) == fold_pc(pc)
            )
        )
        for record in exception_records
    ), exception_records


def _capture_backend_fault_recovery(env) -> dict[str, list[dict]]:
    records: dict[str, list[dict]] = {"redirects": [], "cfvec": []}

    def capture(cycle: int, active_env) -> None:
        ctrl = active_env.backend_ctrl_if
        if int(ctrl.redirect_valid.value) == 1:
            records["redirects"].append(
                {
                    "cycle": int(cycle),
                    "pc": int(ctrl.redirect_bits_pc.value),
                    "target_pc": int(ctrl.redirect_bits_target.value),
                    "ftq_flag": int(ctrl.redirect_bits_ftq_idx_flag.value),
                    "ftq_value": int(ctrl.redirect_bits_ftq_idx_value.value),
                    "ftq_offset": int(ctrl.redirect_bits_ftq_offset.value),
                    "level": int(ctrl.redirect_bits_level.value),
                    "backend_iaf": int(ctrl.redirect_bits_backend_iaf.value),
                    "backend_ipf": int(ctrl.redirect_bits_backend_ipf.value),
                    "backend_igpf": int(ctrl.redirect_bits_backend_igpf.value),
                    "debug_is_ctrl": int(ctrl.redirect_bits_debug_is_ctrl.value),
                    "debug_is_mem_vio": int(ctrl.redirect_bits_debug_is_mem_vio.value),
                }
            )

        observe = active_env.backend_observe_if
        for slot in range(8):
            if int(observe.cfvec_valid[slot].value) != 1:
                continue
            backend_exception = getattr(
                active_env.dut,
                f"io_backend_cfVec_{slot}_bits_backendException",
                None,
            )
            assert backend_exception is not None
            records["cfvec"].append(
                {
                    "cycle": int(cycle),
                    "slot": int(slot),
                    "pc": int(observe.cfvec_pc[slot].value),
                    "foldpc": int(observe.cfvec_foldpc[slot].value),
                    "ftq_flag": int(observe.cfvec_ftq_ptr_flag[slot].value),
                    "ftq_value": int(observe.cfvec_ftq_ptr_value[slot].value),
                    "ftq_offset": int(observe.cfvec_ftq_offset[slot].value),
                    "is_rvc": int(observe.cfvec_is_rvc[slot].value),
                    "backend_exception": int(backend_exception.value or 0),
                    "exception_bits": tuple(
                        bit
                        for bit in _OBSERVED_EXCEPTION_BITS
                        if int(observe.cfvec_exception_vec[slot][bit].value or 0) == 1
                    ),
                }
            )

    env.register_cycle_observer(capture)
    return records


_PMP_ALLOW = _entry(
    "pmp",
    0,
    PmpPmaConfig(match="napot", read=True, execute=True),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMA_ALLOW = _entry(
    "pma",
    0,
    PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMA_UNCACHE_ALLOW = _entry(
    "pma",
    0,
    PmpPmaConfig(match="napot", read=True, execute=True, cacheable=False),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMP_DENY = _entry(
    "pmp",
    0,
    PmpPmaConfig(match="napot", read=True, execute=False),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMA_UNCACHE_DENY = _entry(
    "pma",
    0,
    PmpPmaConfig(match="napot", read=True, execute=False, cacheable=False),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMA_DENY = _entry(
    "pma",
    0,
    PmpPmaConfig(match="napot", read=True, execute=False, cacheable=True),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)


_PERMISSION_CASES = (
    pytest.param(
        _scenario(
            "fetch-permission-pmp-napot-deny",
            pmp_entries=(
                _entry(
                    "pmp",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=False),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
            ),
            pma_entries=(_PMA_ALLOW,),
        ),
        "instruction_access_fault",
        id="pmp-napot-execute-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pma-napot-deny",
            pmp_entries=(_PMP_ALLOW,),
            pma_entries=(
                _entry(
                    "pma",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=False, cacheable=True),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
            ),
        ),
        "instruction_access_fault",
        id="pma-napot-execute-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pma-tor-deny",
            pmp_entries=(_PMP_ALLOW,),
            pma_entries=(
                _entry("pma", 0, PmpPmaConfig(match="off"), _PA & ~(_PAGE_SIZE - 1)),
                _entry(
                    "pma",
                    1,
                    PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True),
                    (_PA & ~(_PAGE_SIZE - 1)) + 0x2000,
                ),
            ),
        ),
        "instruction_access_fault",
        id="pma-tor-execute-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-overlap-first-match",
            pmp_entries=(
                _entry(
                    "pmp",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=False),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
                _entry(
                    "pmp",
                    1,
                    PmpPmaConfig(match="napot", read=True, execute=True),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
            ),
            pma_entries=(_PMA_ALLOW,),
        ),
        "instruction_access_fault",
        id="pmp-overlap-first-match",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-locked-mmode-deny",
            pmp_entries=(
                _entry(
                    "pmp",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=False, locked=True),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
            ),
            pma_entries=(_PMA_ALLOW,),
            priv_imode=2,
        ),
        "instruction_access_fault",
        id="pmp-locked-mmode-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-only-uncache-deny",
            pmp_entries=(_PMP_DENY,),
            pma_entries=(_PMA_UNCACHE_ALLOW,),
            s1_pte=TranslationPte(pbmt=1),
            ptw_machine_pbmte=1,
        ),
        "instruction_access_fault",
        id="pmp-only-uncache-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pma-only-uncache-deny",
            pmp_entries=(_PMP_ALLOW,),
            pma_entries=(_PMA_UNCACHE_DENY,),
            s1_pte=TranslationPte(pbmt=1),
            ptw_machine_pbmte=1,
        ),
        "instruction_access_fault",
        id="pma-only-uncache-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-both-uncache-deny",
            pmp_entries=(_PMP_DENY,),
            pma_entries=(_PMA_UNCACHE_DENY,),
            s1_pte=TranslationPte(pbmt=1),
            ptw_machine_pbmte=1,
        ),
        "instruction_access_fault",
        id="both-uncache-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-both-cacheable-deny",
            pmp_entries=(_PMP_DENY,),
            pma_entries=(_PMA_DENY,),
        ),
        "instruction_access_fault",
        id="both-cacheable-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-napot-lower-boundary",
            va=0x8020_1000,
            pa=0x8040_1000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False), 0x8040_1000, 0x1000),),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_1000, 0x1000),),
        ),
        "instruction_access_fault",
        id="napot-lower-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-napot-upper-boundary",
            va=0x8020_1C00,
            pa=0x8040_1C00,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False), 0x8040_1000, 0x1000),),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_1000, 0x1000),),
        ),
        "instruction_access_fault",
        id="napot-upper-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-interior-boundary",
            pmp_entries=(_PMP_ALLOW,),
            pma_entries=(
                _entry("pma", 0, PmpPmaConfig(match="off"), _PA & ~(_PAGE_SIZE - 1)),
                _entry("pma", 1, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), (_PA & ~(_PAGE_SIZE - 1)) + 0x2000),
            ),
        ),
        "instruction_access_fault",
        id="tor-interior-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-lower-boundary",
            va=0x8020_0000,
            pa=0x8040_0000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, 0x1000),),
            pma_entries=(
                _entry("pma", 0, PmpPmaConfig(match="off"), 0x8040_0000),
                _entry("pma", 1, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), 0x8040_1000),
                _entry("pma", 2, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), 0x8040_2000),
            ),
        ),
        "instruction_access_fault",
        id="tor-lower-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-upper-boundary",
            va=0x8020_1000,
            pa=0x8040_1000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, 0x4000),),
            pma_entries=(
                _entry("pma", 0, PmpPmaConfig(match="off"), 0x8040_0000),
                _entry("pma", 1, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), 0x8040_1000),
            ),
        ),
        "instruction_access_fault",
        id="tor-upper-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-tor-lower-boundary",
            va=0x8020_0000,
            pa=0x8040_0000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="off"), 0x8040_0000),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=False), 0x8040_1000),
            ),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, 0x1000),),
        ),
        "instruction_access_fault",
        id="pmp-tor-lower-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-tor-interior-boundary",
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="off"), _PA & ~(_PAGE_SIZE - 1)),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=False), (_PA & ~(_PAGE_SIZE - 1)) + 0x2000),
            ),
            pma_entries=(_PMA_ALLOW,),
        ),
        "instruction_access_fault",
        id="pmp-tor-interior-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-tor-upper-boundary",
            va=0x8020_1000,
            pa=0x8040_1000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="tor", read=True, execute=False), 0x8040_1000),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=False), 0x8040_2000),
            ),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, 0x4000),),
        ),
        "instruction_access_fault",
        id="pmp-tor-upper-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-napot-upper-boundary-allow",
            va=0x8020_1C00,
            pa=0x8040_1C00,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_1000, 0x1000),),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_1000, 0x1000),),
            expected_fault=None,
        ),
        None,
        id="napot-upper-boundary-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-lower-boundary-allow",
            va=0x8020_0000,
            pa=0x8040_0000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="off"), 0x8040_0000),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=True), 0x8040_1000),
            ),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, 0x1000),),
            expected_fault=None,
        ),
        None,
        id="tor-lower-boundary-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-upper-boundary-allow",
            va=0x8020_1000,
            pa=0x8040_1000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="tor", read=True, execute=False), 0x8040_1000),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=True), 0x8040_2000),
            ),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, 0x4000),),
            expected_fault=None,
        ),
        None,
        id="tor-upper-boundary-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-interior-boundary-allow",
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="off"), _PA & ~(_PAGE_SIZE - 1)),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=True), (_PA & ~(_PAGE_SIZE - 1)) + 0x2000),
            ),
            pma_entries=(_PMA_ALLOW,),
            expected_fault=None,
        ),
        None,
        id="tor-interior-boundary-allow",
    ),
)


@pytest.mark.parametrize("scenario,expected_fault", _PERMISSION_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_instruction_fetch_permission_boundary(
    env,
    scenario: TranslationScenario,
    expected_fault: str,
) -> None:
    sequence = TranslationScenarioSequence(
        actions=(TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),)
    )
    sequence.initialize_first_phase(env)
    phases = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phases) == 1
    state = phases[0]["state"]
    expected_outcome = "normal" if expected_fault is None else expected_fault
    assert state.expected_page_outcomes[0]["outcome"] == expected_outcome
    if expected_fault is None:
        assert any(int(observation.pc) == scenario.va for observation in env.monitor.observations)
    else:
        _assert_exact_oracle_fault(env, pc=scenario.va, expected_fault=expected_fault)
    assert not env.get_errors()


def _backend_fault_recovery_scenarios(fault_kind: str) -> tuple[TranslationScenario, TranslationScenario]:
    region_base = _PA & ~0x3FFF
    second_page_va = (_VA & ~(_PAGE_SIZE - 1)) + _PAGE_SIZE
    second_page_pa = (_PA & ~(_PAGE_SIZE - 1)) + _PAGE_SIZE
    normal = _scenario(
        f"backend-{fault_kind}-redirect-recovery-target",
        va=_VA,
        pa=_PA,
        payload=_CROSS_PAGE_PAYLOAD,
        page_count=2,
        pmp_entries=(
            _entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), region_base, 0x4000),
        ),
        pma_entries=(
            _entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), region_base, 0x4000),
        ),
        expected_fault=None,
    )
    if fault_kind == "iaf":
        fault = replace(
            normal,
            scenario_id="backend-iaf-redirect-source",
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), _PA & ~(_PAGE_SIZE - 1), _PAGE_SIZE),
                _entry("pmp", 1, PmpPmaConfig(match="napot", read=True, execute=False), second_page_pa, _PAGE_SIZE),
            ),
            expected_path="fault",
            expected_result="access_fault",
        )
    elif fault_kind == "ipf":
        fault = replace(
            normal,
            scenario_id="backend-ipf-redirect-source",
            ptw_response_overrides=(
                TranslationPtwResponseOverride(
                    vpn=second_page_va >> 12,
                    s2xlate=0,
                    patch=(("s1_pf", 1),),
                ),
            ),
            expected_path="fault",
            expected_result="page_fault",
        )
    elif fault_kind == "igpf":
        normal = replace(
            normal,
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            priv_virt=1,
        )
        fault = replace(
            normal,
            scenario_id="backend-igpf-redirect-source",
            ptw_response_overrides=(
                TranslationPtwResponseOverride(
                    vpn=second_page_va >> 12,
                    s2xlate=2,
                    patch=(("s2_gpf", 1),),
                ),
            ),
            expected_path="fault",
            expected_result="guest_fault",
        )
    else:
        raise ValueError(f"unsupported backend fault kind: {fault_kind}")
    return fault, normal


@pytest.mark.parametrize(
    ("fault_kind", "fault_bit", "redirect_faults"),
    (
        pytest.param("iaf", 1, {"backend_iaf": 1}, id="instruction-access-fault"),
        pytest.param("ipf", 12, {"backend_ipf": 1}, id="instruction-page-fault"),
        pytest.param("igpf", 20, {"backend_igpf": 1}, id="instruction-guest-page-fault"),
    ),
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_backend_fault_redirect_recovery(
    env,
    fault_kind: str,
    fault_bit: int,
    redirect_faults: dict[str, int],
) -> None:
    fault, normal = _backend_fault_recovery_scenarios(fault_kind)
    fault_pc = (fault.va & ~(_PAGE_SIZE - 1)) + _PAGE_SIZE
    builder = TranslationScenarioBuilder(env)
    records = _capture_backend_fault_recovery(env)

    env.initialize(reset_vector=fault_pc, bare_mode=False)

    def arm_fault_before_reset_release() -> None:
        fault_state = builder.build(fault)
        env.monitor.clear()
        env.monitor.set_expected_pc(fault_pc)
        env.translation_oracle.clear()
        env.arm_translation_scenario(fault_state, page_indexes=(1,))

    env.reset(before_release=arm_fault_before_reset_release)
    _wait_until(
        env,
        lambda: any(
                record["pc"] == fault_pc
            and record["exception_bits"] == (fault_bit,)
            and record["backend_exception"] == 0
            for record in records["cfvec"]
        ),
        description=f"frontend {fault_kind} cfVec source",
    )
    env.assert_translation_scenario()
    source = next(
        record
        for record in records["cfvec"]
        if record["pc"] == fault_pc
        and record["exception_bits"] == (fault_bit,)
        and record["backend_exception"] == 0
    )

    reuses_source_ftq = source["ftq_offset"] == 0 or (
        source["ftq_offset"] == 1 and source["is_rvc"] == 0
    )
    assert reuses_source_ftq
    InjectRedirectSequence(
        RedirectTxn(
            source_pc=source["pc"],
            source_ftq_flag=source["ftq_flag"],
            source_ftq_value=source["ftq_value"],
            source_ftq_offset=source["ftq_offset"],
            target_pc=fault_pc,
            reason=f"backend-{fault_kind}-recovery",
            level=1,
            redirect_class=BackendRedirectClass.OTHER,
            **redirect_faults,
        )
    ).inject(env)
    builder.build(normal)
    env.translation_oracle.disarm()
    env.monitor.set_expected_pc(fault_pc)

    _wait_until(env, lambda: bool(records["redirects"]), description=f"backend {fault_kind} redirect")
    redirect = records["redirects"][-1]
    expected_fault_encoding = {
        "backend_iaf": int(fault_kind == "iaf"),
        "backend_ipf": int(fault_kind == "ipf"),
        "backend_igpf": int(fault_kind == "igpf"),
    }
    assert redirect == {
        "cycle": redirect["cycle"],
        "pc": source["pc"],
        "target_pc": fault_pc,
        "ftq_flag": source["ftq_flag"],
        "ftq_value": source["ftq_value"],
        "ftq_offset": source["ftq_offset"],
        "level": 1,
        **expected_fault_encoding,
        "debug_is_ctrl": 0,
        "debug_is_mem_vio": 0,
    }

    if reuses_source_ftq:
        expected_new_ftq = (source["ftq_flag"], source["ftq_value"])
    else:
        expected_new_ftq = (
            source["ftq_flag"] ^ int(source["ftq_value"] == 63),
            (source["ftq_value"] + 1) % 64,
        )
    _wait_until(
        env,
        lambda: any(
            record["cycle"] >= redirect["cycle"] + 2
            and (record["pc"] == fault_pc or record["foldpc"] == fold_pc(fault_pc))
            and (record["ftq_flag"], record["ftq_value"]) == expected_new_ftq
            and record["exception_bits"] == (fault_bit,)
            and record["backend_exception"] == 1
            for record in records["cfvec"]
        ),
        description=f"backend {fault_kind} recovery cfVec",
    )
    assert not env.get_errors()


_PMP_LOCK_MODE_CASES = (
    pytest.param(
        _scenario(
            "fetch-permission-pmp-locked-mmode-allow",
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True, locked=True), _PA & ~0x1FFF, 0x2000),),
            pma_entries=(_PMA_ALLOW,),
            priv_imode=2,
            expected_fault=None,
        ),
        id="pmp-locked-mmode-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-locked-sumode-allow",
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True, locked=True), _PA & ~0x1FFF, 0x2000),),
            pma_entries=(_PMA_ALLOW,),
            expected_fault=None,
        ),
        id="pmp-locked-sumode-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-locked-sumode-deny",
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False, locked=True), _PA & ~0x1FFF, 0x2000),),
            pma_entries=(_PMA_ALLOW,),
        ),
        id="pmp-locked-sumode-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-unlocked-mmode-bypass",
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False), _PA & ~0x1FFF, 0x2000),),
            pma_entries=(_PMA_ALLOW,),
            priv_imode=2,
            expected_fault=None,
        ),
        id="pmp-unlocked-mmode-bypass",
    ),
)


@pytest.mark.parametrize("scenario", _PMP_LOCK_MODE_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_pmp_lock_mode(env, scenario: TranslationScenario) -> None:
    sequence = TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),))
    sequence.initialize_first_phase(env)
    phases = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phases) == 1
    if scenario.expected_path == "fault":
        assert env.monitor.exception_mark_count > 0
    else:
        assert any(int(observation.pc) == scenario.va for observation in env.monitor.observations)
    assert not env.get_errors()
