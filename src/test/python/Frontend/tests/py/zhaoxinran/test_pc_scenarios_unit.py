from __future__ import annotations

import random
from dataclasses import replace
from types import SimpleNamespace

import pytest

from env.model.golden_trace import GoldenTrace
from env.model.memory_model import MemoryModel
from env.model.page_table_model import PageTableModel
from env.sequences import TranslationScenarioRandomizer
from env.support import fold_pc, scenario_rng
from tests.py.support import uncache_scenarios as uncache
from tests.py.support.instructions import encode_jal_x0
from tests.py.support.translation_context_switch import (
    same_vpn_context_scenarios,
    satp_switch_scenarios,
)
from tests.py.support.translation_scenarios import FAULT_NOP_PAYLOAD, randomize_translation_fault_entry
from tests.py.zhaoxinran.support.branch import jal_forward_program
from tests.py.zhaoxinran.support.exception import exception_entry
from tests.py.zhaoxinran.support.mmio import (
    mmio_non_crossing_rvi_stream,
    mmio_rvc_beat_tail_entry,
    mmio_rvi_entry,
    prepare_mmio_cnop_stream,
    prepare_mmio_cross_beat_stream,
    prepare_mmio_jalr_stream,
    prepare_mmio_page_tail_stream,
)
from tests.py.zhaoxinran.support.translation import (
    _sfence_stage_scenario,
    randomize_gstage_entry,
    randomize_permission_entry,
    randomize_translation_layout,
)
from tests.py.zhaoxinran.support.uncache import (
    prepare_sv39_cfi_stream,
    prepare_sv39_dual_stream,
    sv39_layout_entry,
)
from tests.py.zhaoxinran.support.vtype import vtype_entry
from tests.py.zhaoxinran.translation.test_address_translation_fault import _FAULT_CASES
from tests.py.zhaoxinran.translation.test_address_translation_normal import (
    _NORMAL_CASES,
    _randomize_normal_ptw_timing,
)
from tests.py.zhaoxinran.translation.test_address_translation_pte_permission import _PTE_PERMISSION_CASES
from tests.py.zhaoxinran.translation.test_address_translation_ptw_timing import (
    _TIMING_CASES,
    _randomize_ptw_timing,
)
from tests.py.zhaoxinran.translation.test_gstage_faults import _PROVENANCE_CASES
from tests.py.zhaoxinran.translation.test_instruction_fetch_permission_boundary import (
    _PERMISSION_CASES,
    _PMP_LOCK_MODE_CASES,
)


def test_mmio_entry_points_to_generated_rvi() -> None:
    placements = set()
    for seed in range(30):
        placement, payload = mmio_rvi_entry(random.Random(seed), cross_beat=False)
        offset = placement.pc - uncache._MMIO_BASE
        placements.add(offset)
        assert placement.pa == placement.pc
        assert placement.memory.mark_mmio
        assert offset // 8 < 15 and offset % 8 in (0, 2, 4)
        assert len(payload) == 128
        assert int.from_bytes(payload[offset:offset + 4], "little") == uncache._ADDI_X0_X0_0
    assert len(placements) > 1


def test_mmio_non_crossing_rvi_stream_links_random_pages() -> None:
    beat_bytes = uncache._UNCACHE_BEAT_BYTES
    page_size = uncache._SV39_PAGE_SIZE
    page_counts, target_counts, target_offsets, jump_offsets = set(), set(), set(), set()
    uneven_split = False
    for seed in range(32):
        images, trace = mmio_non_crossing_rvi_stream(random.Random(seed))
        assert (images, trace) == mmio_non_crossing_rvi_stream(random.Random(seed))
        pages = [image.base_addr // page_size for image in images]
        assert 2 <= len(images) == len(set(pages)) <= 8
        page_counts.add(len(images))
        memory = MemoryModel()
        for image in images:
            assert image.base_addr % page_size + len(image.payload) <= page_size
            memory.load_bin(image.payload, image.base_addr)
        jumps = []
        for index, entry in enumerate(trace):
            source = GoldenTrace.from_memory(memory, entry.pc, 1).entries[0]
            assert (source.instr, source.size, source.kind, source.target_pc) == (
                entry.instr, entry.size, entry.kind, entry.target_pc,
            )
            if source.kind == "jump":
                assert source.instr & 0xFFF == 0x06F  # JAL x0
                assert entry.target_pc == trace[index + 1].pc
                assert entry.pc // page_size != entry.target_pc // page_size
                jumps.append(entry)
                jump_offsets.add(entry.pc % beat_bytes)
            elif index + 1 < len(trace):
                assert trace[index + 1].pc == entry.pc + entry.size
            if entry.size == 4:
                assert entry.pc % beat_bytes in (0, 2, 4)
        assert len(jumps) == len(images) - 1
        assert [trace[0].pc // page_size, *(entry.target_pc // page_size for entry in jumps)] == pages
        page_targets = []
        for image in images:
            pcs = [
                entry.pc for entry in trace
                if entry.size == 4 and entry.kind == "normal" and entry.pc // page_size == image.base_addr // page_size
            ]
            assert pcs
            first_beat = image.base_addr // beat_bytes
            assert [pc // beat_bytes for pc in pcs] == list(range(first_beat, first_beat + len(pcs)))
            target_offsets.update(pc % beat_bytes for pc in pcs)
            page_targets.append(len(pcs))
        assert 16 <= sum(page_targets) <= 100
        target_counts.add(sum(page_targets))
        uneven_split |= max(page_targets) - min(page_targets) > 1
    assert len(page_counts) > 1 and len(target_counts) > 1
    assert target_offsets == jump_offsets == {0, 2, 4}
    assert uneven_split


def test_mmio_cross_beat_entry_points_to_generated_rvi() -> None:
    placements = set()
    for seed in range(30):
        placement, payload = mmio_rvi_entry(random.Random(seed), cross_beat=True)
        offset = placement.pc - uncache._MMIO_BASE
        placements.add(offset)
        assert placement.pa == placement.pc
        assert offset // 8 < 15 and offset % 8 == 6
        assert int.from_bytes(payload[offset:offset + 4], "little") == uncache._ADDI_X0_X0_0
    assert len(placements) > 1


def test_mmio_rvc_beat_tail_keeps_next_instruction_in_second_beat() -> None:
    placements = set()
    for seed in range(30):
        placement, payload = mmio_rvc_beat_tail_entry(random.Random(seed))
        offset = placement.pc - uncache._MMIO_BASE
        placements.add(offset)
        assert placement.pa == placement.pc
        assert offset // 8 < 15 and offset % 8 == 6
        assert payload[offset:offset + 4] == uncache._CNOP.to_bytes(2, "little") * 2
    assert len(placements) > 1


@pytest.mark.parametrize("jalr", (False, True))
def test_mmio_stream_entry_loads_original_payload_at_selected_pc(jalr: bool) -> None:
    cnop = uncache._CNOP.to_bytes(2, "little")
    jalr_instr = 0x00008067
    expected = jalr_instr.to_bytes(4, "little") + cnop * 128 if jalr else cnop * 256

    def load(seed: int) -> int:
        programs = []
        env = SimpleNamespace(
            memory=SimpleNamespace(mmio_ranges=[]),
            load_program=lambda payload, base: programs.append((payload, base)),
        )
        rng = random.Random(seed)
        pc = prepare_mmio_jalr_stream(env, rng, jalr_instr) if jalr else prepare_mmio_cnop_stream(env, rng)
        assert pc % 64 == 0
        assert uncache._MMIO_BASE <= pc <= uncache._MMIO_BASE + 0x1000 - len(expected)
        assert programs == [(expected, pc)]
        assert env.memory.mmio_ranges == [(pc, pc + len(expected))]
        return pc

    entries = [load(seed) for seed in range(20)]
    assert entries[0] == load(0)
    assert len(set(entries)) > 1


@pytest.mark.parametrize("tail", ("beat-rvi", "page-rvi", "page-rvc"))
def test_mmio_tail_stream_moves_payload_and_preserves_boundary(tail: str) -> None:
    pcs = set()
    for seed in range(16):
        programs = []
        env = SimpleNamespace(
            memory=SimpleNamespace(mmio_ranges=[]),
            load_program=lambda payload, base: programs.append((payload, base)),
        )
        rng = random.Random(seed)
        if tail == "beat-rvi":
            pc, base = prepare_mmio_cross_beat_stream(env, rng)
            assert pc == base + 6 and base % 64 == 0
            assert len(programs[0][0]) > 0x100  # Redirect target is in the same image.
        else:
            pc = prepare_mmio_page_tail_stream(env, rng, is_rvc=tail == "page-rvc")
            base = pc & ~0xFFF
            assert pc == base + 0xFFE and (base - uncache._MMIO_BASE) % 0x8000 == 0
        ((payload, load_base),) = programs
        assert load_base == base
        assert env.memory.mmio_ranges == [(base, base + len(payload))]
        offset = pc - base
        if tail == "page-rvc":
            assert payload[offset:offset + 4] == uncache._CNOP.to_bytes(2, "little") * 2
        else:
            assert payload[offset:offset + 4] == uncache._ADDI_X0_X0_0.to_bytes(4, "little")
        pcs.add(pc)
    assert len(pcs) > 1


def test_sv39_dual_stream_keeps_relative_mappings_and_pma_window() -> None:
    pcs = set()
    for seed in range(16):
        programs = []
        env = SimpleNamespace(
            page_table=PageTableModel(mode="sv39"),
            ptw_agent=SimpleNamespace(configure=lambda **kwargs: None),
            load_program=lambda payload, base: programs.append((payload, base)),
        )
        entry = sv39_layout_entry(random.Random(seed))
        expected, cacheable_pcs = prepare_sv39_dual_stream(env, entry)
        assert entry.va % 0x8000 == 0
        assert [pc + 0x2000 for pc, _, _ in expected] == cacheable_pcs
        assert expected[0][0] == entry.va
        assert programs == [(programs[0][0], entry.pa), (programs[0][0], entry.pa + 0x1000)]
        assert (entry.pa & ~0x3FFF) <= entry.pa < entry.pa + 0x2000 <= (entry.pa & ~0x3FFF) + 0x4000
        for va, pa, pbmt in ((entry.va, entry.pa, uncache._PBMT_NC),
                             (entry.va + 0x2000, entry.pa + 0x1000, uncache._PBMT_PMA)):
            pte = env.page_table.pte_map[va >> 12]
            assert (pte.ppn, pte.pbmt, pte.x) == (pa >> 12, pbmt, 1)
        pcs.add(entry.va)
    assert len(pcs) > 1


@pytest.mark.parametrize("ordinal", range(6))
def test_generated_translation_layout_keeps_scenario_constraints(monkeypatch, ordinal: int) -> None:
    original = TranslationScenarioRandomizer(0x5A390001).next(ordinal).scenario
    stride = 0x20_0000 if ordinal == 5 else 0x8000
    pcs = set()
    for seed in range(1, 9):
        monkeypatch.setenv("TB_SEED", str(seed))
        kwargs = {"scenario_key": f"test/generated/{ordinal}/pc", "stride": stride}
        scenario = randomize_translation_layout(SimpleNamespace(), original, **kwargs)
        assert scenario == randomize_translation_layout(SimpleNamespace(), original, **kwargs)
        delta = scenario.va - original.va
        assert scenario.pa - original.pa == delta and delta % stride == 0
        assert scenario.payload == original.payload and scenario.page_count == original.page_count
        assert scenario.gpa == (None if original.gpa is None else original.gpa + delta)
        assert scenario.s1_sector_lanes == tuple(
            replace(lane, ppn=lane.ppn + (delta >> 12)) for lane in original.s1_sector_lanes
        )
        assert scenario.permission_probes == tuple(
            replace(probe, va=probe.va + delta) for probe in original.permission_probes
        )
        for items in ("pmp_entries", "pma_entries"):
            assert getattr(scenario, items) == tuple(
                replace(item, addr=item.addr + delta) for item in getattr(original, items)
            )
        pcs.add(scenario.va)
    assert len(pcs) > 1


@pytest.mark.parametrize("pbmt,instr_count", ((uncache._PBMT_IO, 256), (uncache._PBMT_NC, 4096)))
def test_sv39_cfi_stream_moves_payload_and_page_table_with_pc(pbmt: int, instr_count: int) -> None:
    def prepare(seed: int | None):
        programs = []
        env = SimpleNamespace(
            memory=SimpleNamespace(mmio_ranges=[]),
            page_table=PageTableModel(mode="sv39"),
            ptw_agent=SimpleNamespace(configure=lambda **kwargs: None),
            load_program=lambda payload, base: programs.append((payload, base)),
        )
        if seed is None:
            expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
                env, vaddr=uncache._NORMAL_BASE, paddr=uncache._NORMAL_PHYS_BASE,
                pbmt=pbmt, instr_count=instr_count,
            )
        else:
            expected, mapping = prepare_sv39_cfi_stream(
                env, random.Random(seed), pbmt=pbmt, instr_count=instr_count,
            )
        return expected, mapping, programs, env.page_table.pte_map

    original_expected, original, original_programs, _ = prepare(None)
    starts = set()
    for seed in range(16):
        expected, mapping, programs, pt = prepare(seed)
        delta = mapping.vaddr - original.vaddr
        assert delta % 0x8000 == 0
        assert mapping.paddr - original.paddr == delta
        assert expected == [(pc + delta, instr, is_rvc) for pc, instr, is_rvc in original_expected]
        assert programs == [(payload, pa + delta) for payload, pa in original_programs]
        pmp_base = mapping.paddr & ~0x3FFF
        assert pmp_base <= mapping.paddr < mapping.paddr + mapping.size <= pmp_base + 0x4000
        for page_index, pa in enumerate(mapping.paddr_pages):
            pte = pt[(mapping.vaddr >> 12) + page_index]
            assert (pte.ppn, pte.pbmt, pte.x) == (pa >> 12, pbmt, 1)
        starts.add(mapping.vaddr)
    assert len(starts) > 1
    assert prepare(0)[1] == prepare(0)[1]


def test_forward_jal_program_follows_randomized_start_pc() -> None:
    entries = [jal_forward_program(random.Random(seed)) for seed in range(20)]
    assert entries[0] == jal_forward_program(random.Random(0))
    assert len({placement.pc for placement, _, _ in entries}) > 1
    for placement, payload, target_pc in entries:
        assert placement.pc == placement.pa
        assert placement.pc % 64 == 0
        assert placement.pc % 0x1000 + len(payload) <= 0x1000
        assert 8 <= target_pc - placement.pc <= 64
        assert int.from_bytes(payload[:4], "little") == encode_jal_x0(target_pc - placement.pc)


def test_vtype_entry_is_reproducible_and_keeps_program_offsets(monkeypatch) -> None:
    entries = set()
    folded_pcs = set()
    for seed in range(1, 17):
        monkeypatch.setenv("TB_SEED", str(seed))
        env = SimpleNamespace()
        base = vtype_entry(env, "instruction_order/plain_vsetvl_plain/0")
        assert base == vtype_entry(SimpleNamespace(), "instruction_order/plain_vsetvl_plain/0")
        assert 0x80000000 <= base < 0x80020000
        assert base % 0x1000 == 0
        assert env.random_scenario_records[0]["parameters"] == {"pc": base, "pa": base}
        assert (base + 4 * 17) % 64 == 4
        folded_pcs.add(fold_pc(base + 4 * 17 - 8))
        assert base + 4 * (256 + 16 + 33 + 1024) < 0x80024000
        entries.add(base)
    assert len(entries) > 1
    assert len(folded_pcs) > 1


@pytest.mark.parametrize("kind,offset", (("cross64", 0x3E), ("illegal_rvc", 0x3E), ("trigger", 0)))
def test_exception_entry_preserves_instruction_boundary(kind: str, offset: int) -> None:
    entries = [exception_entry(random.Random(seed), kind) for seed in range(20)]
    assert entries[0] == exception_entry(random.Random(0), kind)
    assert len({entry.pc for entry in entries}) > 1
    for entry in entries:
        assert entry.pa == entry.pc
        assert entry.pc & 0xFFF == offset
        assert entry.memory.pma_cacheable
        if kind == "cross64":
            assert (entry.pc & 0x3F) + 4 > 0x40
        elif kind == "illegal_rvc":
            assert (entry.pc + 2) & 0x3F == 0


@pytest.mark.parametrize("original,expected_fault", (*_FAULT_CASES, *_PTE_PERMISSION_CASES))
def test_translation_fault_entry_keeps_fault_and_mapping(original, expected_fault, monkeypatch) -> None:
    entries = set()
    group = "pte-permission" if original.scenario_id.startswith("translation-pte-") else "fault"
    scenario_key = f"zhaoxinran/translation/{group}/{original.scenario_id}"
    for seed in range(1, 5):
        monkeypatch.setenv("TB_SEED", str(seed))
        env = SimpleNamespace()
        scenario = randomize_translation_fault_entry(
            env,
            original,
            scenario_key=scenario_key,
            expected_fault=expected_fault,
        )
        assert scenario == randomize_translation_fault_entry(
            SimpleNamespace(),
            original,
            scenario_key=scenario_key,
            expected_fault=expected_fault,
        )
        assert scenario.payload == FAULT_NOP_PAYLOAD
        assert scenario.page_count == 2
        assert (scenario.va & 0xFFF) == 0xF00
        assert (scenario.va & 0xFFF) + len(scenario.payload) > 0x1000
        assert scenario.pa - scenario.va == 0x20_0000
        assert scenario.gpa == (scenario.va + 0x40_0000 if original.gpa is not None else None)
        assert (scenario.va >> 12) & 0x1FF < 511
        assert (scenario.va >> 12) & 1 == 0
        assert scenario.pmp_entries == tuple(
            replace(item, addr=item.addr + scenario.pa - original.pa)
            for item in original.pmp_entries
        )
        assert scenario.pma_entries == tuple(
            replace(item, addr=item.addr + scenario.pa - original.pa)
            for item in original.pma_entries
        )
        assert all(item.addr <= scenario.pa and scenario.pa + len(scenario.payload) <= item.addr + item.size
                   for item in (*scenario.pmp_entries, *scenario.pma_entries))
        assert env.random_scenario_records[0]["parameters"]["va"] == scenario.va
        assert env.random_scenario_records[0]["parameters"]["gpa"] == scenario.gpa
        entries.add(scenario.va)
    assert len(entries) > 1


def test_satp_switch_pair_tracks_entry_mapping_and_permissions(monkeypatch) -> None:
    entries = set()
    for seed in range(1, 9):
        monkeypatch.setenv("TB_SEED", str(seed))
        env = SimpleNamespace()
        old, new = satp_switch_scenarios(env)
        assert (old, new) == satp_switch_scenarios(SimpleNamespace())
        assert (new.va - old.va, new.pa - old.pa) == (0x2000, 0x2000)
        assert old.va & 0x3FFF == 0xF00
        assert old.pa - old.va == 0x20_0000
        assert old.payload == new.payload
        assert old.satp_asid == old.s1_pte.asid == 3
        assert new.satp_asid == new.s1_pte.asid == 9
        for scenario in (old, new):
            assert scenario.pmp_entries[0].addr == (scenario.pa & ~0x3FFF)
            assert scenario.pma_entries[0].addr == (scenario.pa & ~0x3FFF)
            assert scenario.pa + len(scenario.payload) <= scenario.pma_entries[0].addr + 0x4000
        assert env.random_scenario_records[0]["parameters"]["new_va"] == new.va
        entries.add(old.va)
    assert len(entries) > 1


def test_same_vpn_context_pair_tracks_pte_signature(monkeypatch) -> None:
    entries = set()
    for seed in range(1, 9):
        monkeypatch.setenv("TB_SEED", str(seed))
        old, new = same_vpn_context_scenarios(SimpleNamespace())
        assert (old, new) == same_vpn_context_scenarios(SimpleNamespace())
        assert (old.va, old.pa, old.payload) == (new.va, new.pa, new.payload)
        assert old.va & 0x3FFF == 0xF00
        assert old.s1_pte.pbmt == 0 and new.s1_pte.pbmt == 1
        assert old.satp_asid == old.s1_pte.asid == 3
        assert new.satp_asid == new.s1_pte.asid == 9
        assert old.pmp_entries == new.pmp_entries
        assert old.pma_entries == new.pma_entries
        entries.add(old.va)
    assert len(entries) > 1


@pytest.mark.parametrize("s2xlate", (0, 1, 2, 3))
def test_sfence_stage_entry_tracks_translation_and_permissions(monkeypatch, s2xlate: int) -> None:
    entries = set()
    for seed in range(1, 9):
        monkeypatch.setenv("TB_SEED", str(seed))
        env = SimpleNamespace()
        scenario = _sfence_stage_scenario(env, f"sfence-stage-{s2xlate}", s2xlate=s2xlate)
        assert scenario == _sfence_stage_scenario(SimpleNamespace(), f"sfence-stage-{s2xlate}", s2xlate=s2xlate)
        assert scenario.va & 0x3FFF == 0xF00
        assert scenario.pa - scenario.va == 0x20_0000
        assert scenario.gpa == (scenario.va + 0x40_0000 if s2xlate in (2, 3) else None)
        assert scenario.page_count == 2
        assert scenario.payload[0:4] == b"\x13\x00\x00\x00"
        assert scenario.va % 0x1000 + len(scenario.payload) > 0x1000
        for permission in (*scenario.pmp_entries, *scenario.pma_entries):
            assert permission.addr == scenario.pa & ~0x3FFF
            assert scenario.pa + len(scenario.payload) <= permission.addr + permission.size
        assert env.random_scenario_records[0]["parameters"]["va"] == scenario.va
        assert env.random_scenario_records[0]["parameters"]["gpa"] == scenario.gpa
        entries.add(scenario.va)
    assert len(entries) > 1


@pytest.mark.parametrize("original,expected_fault", _TIMING_CASES)
def test_ptw_timing_entry_relocates_mapping_and_sector_lanes(monkeypatch, original, expected_fault) -> None:
    entries = set()
    for seed in range(1, 5):
        monkeypatch.setenv("TB_SEED", str(seed))
        env = SimpleNamespace()
        scenario = _randomize_ptw_timing(env, original, expected_fault)
        assert scenario == _randomize_ptw_timing(SimpleNamespace(), original, expected_fault)
        assert scenario.va & 0x3FFF == 0xF00
        assert scenario.pa - scenario.va == 0x20_0000
        assert scenario.gpa == (original.gpa + scenario.va - original.va if original.gpa is not None else None)
        assert scenario.payload == original.payload
        assert scenario.page_count == original.page_count
        pa_delta = scenario.pa - original.pa
        assert scenario.pmp_entries == tuple(replace(item, addr=item.addr + pa_delta)
                                             for item in original.pmp_entries)
        assert scenario.pma_entries == tuple(replace(item, addr=item.addr + pa_delta)
                                             for item in original.pma_entries)
        assert scenario.s1_sector_lanes == tuple(replace(lane, ppn=lane.ppn + (pa_delta >> 12))
                                                 for lane in original.s1_sector_lanes)
        assert env.random_scenario_records[0]["parameters"]["va"] == scenario.va
        entries.add(scenario.va)
    assert len(entries) > 1


@pytest.mark.parametrize("original,expected_paths", _NORMAL_CASES)
def test_normal_entry_preserves_page_offset_permissions_and_lanes(monkeypatch, original, expected_paths) -> None:
    entries = set()
    for seed in range(1, 5):
        monkeypatch.setenv("TB_SEED", str(seed))
        env = SimpleNamespace()
        scenario = _randomize_normal_ptw_timing(env, original, expected_paths)
        assert scenario == _randomize_normal_ptw_timing(SimpleNamespace(), original, expected_paths)
        assert scenario.va - original.va == scenario.pa - original.pa
        assert (scenario.va - original.va) % 0x8000 == 0
        assert scenario.va % 0x8000 == original.va % 0x8000
        assert scenario.gpa == (original.gpa + scenario.va - original.va if original.gpa is not None else None)
        assert scenario.payload == original.payload
        assert scenario.page_count == original.page_count
        assert scenario.pa + len(scenario.payload) <= (scenario.pa & ~0x1F_FFFF) + 0x20_0000
        delta = scenario.pa - original.pa
        assert scenario.pmp_entries == tuple(replace(item, addr=item.addr + delta)
                                             for item in original.pmp_entries)
        assert scenario.pma_entries == tuple(replace(item, addr=item.addr + delta)
                                             for item in original.pma_entries)
        assert scenario.s1_sector_lanes == tuple(replace(lane, ppn=lane.ppn + (delta >> 12))
                                                 for lane in original.s1_sector_lanes)
        assert env.random_scenario_records[0]["parameters"]["va"] == scenario.va
        entries.add(scenario.va)
    assert len(entries) > 1


def test_normal_pc_placement_does_not_consume_ptw_timing_rng(monkeypatch) -> None:
    original, expected_paths = _NORMAL_CASES[-1].values
    monkeypatch.setenv("TB_SEED", "1")
    scenario_key = f"zhaoxinran/translation/normal/{original.scenario_id}"
    _, _, rng = scenario_rng(scenario_key)
    expected = (rng.randint(1, 5), rng.randint(1, 2), rng.randint(1, 4))
    scenario = _randomize_normal_ptw_timing(SimpleNamespace(), original, expected_paths)
    assert (scenario.ptw_response_latency, scenario.ptw_req_ready_high_cycles,
            scenario.ptw_req_ready_low_cycles) == expected


@pytest.mark.parametrize("original,expected_fault", _PROVENANCE_CASES)
def test_gstage_provenance_entry_tracks_override_vpn(monkeypatch, original, expected_fault) -> None:
    entries = set()
    for seed in range(1, 9):
        monkeypatch.setenv("TB_SEED", str(seed))
        env = SimpleNamespace()
        scenario = randomize_gstage_entry(env, original)
        assert scenario == randomize_gstage_entry(SimpleNamespace(), original)
        delta = scenario.va - original.va
        assert delta == scenario.pa - original.pa
        assert delta % 0x4000 == 0
        assert scenario.gpa == original.gpa + delta
        assert scenario.payload == original.payload
        assert scenario.ptw_response_overrides == tuple(
            replace(item, vpn=item.vpn + (delta >> 12)) for item in original.ptw_response_overrides
        )
        assert scenario.ptw_response_overrides[0].vpn == scenario.va >> 12
        assert env.random_scenario_records[0]["parameters"]["va"] == scenario.va
        entries.add(scenario.va)
    assert len(entries) > 1


@pytest.mark.parametrize("original", [*(case.values[0] for case in _PERMISSION_CASES),
                                     *(case.values[0] for case in _PMP_LOCK_MODE_CASES)])
def test_permission_entry_preserves_pmp_pma_boundaries(monkeypatch, original) -> None:
    entries = set()
    for seed in range(1, 5):
        monkeypatch.setenv("TB_SEED", str(seed))
        env = SimpleNamespace()
        scenario = randomize_permission_entry(env, original)
        assert scenario == randomize_permission_entry(SimpleNamespace(), original)
        delta = scenario.pa - original.pa
        assert scenario.va - original.va == delta
        assert delta % 0x4000 == 0
        assert scenario.va & 0x3FFF == original.va & 0x3FFF
        assert scenario.pa + len(scenario.payload) <= (scenario.pa & ~0x1F_FFFF) + 0x20_0000
        assert scenario.pmp_entries == tuple(replace(item, addr=item.addr + delta) for item in original.pmp_entries)
        assert scenario.pma_entries == tuple(replace(item, addr=item.addr + delta) for item in original.pma_entries)
        assert scenario.payload == original.payload
        assert env.random_scenario_records[0]["parameters"]["pa"] == scenario.pa
        entries.add(scenario.va)
    assert len(entries) > 1
