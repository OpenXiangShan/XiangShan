"""MMIO redirect scenarios shared by directed tests."""

import random

from env.core.transactions import BackendRedirectClass, ProgramImage
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from env.model.golden_trace import TraceEntry
from env.sequences import LoadProgramSequence
from env.support import PcBoundary, PcMapping, RandomPc, random_pc
from tests.py.support.dut_support import _wait_for_req_delta, _wait_for_resp_delta, run_until
from tests.py.support.instructions import encode_jal_x0
from tests.py.support import uncache_scenarios as uncache


def load_mmio_payload(env, payload: bytes, *, base: int = uncache._MMIO_BASE) -> None:
    env.memory.mmio_ranges.append((base, base + len(payload)))
    LoadProgramSequence(
        image=ProgramImage(payload=bytes(payload), base_addr=base),
        step_cycles=0,
    ).run(env)


def load_random_mmio_stream(env, rng: random.Random, payload: bytes) -> int:
    page_size = uncache._SV39_PAGE_SIZE
    block_size = uncache._FETCH_BLOCK_SIZE
    placement = random_pc(
        rng,
        mappings=(PcMapping(uncache._MMIO_BASE, uncache._MMIO_BASE, page_size),),
        entry_offsets=tuple(range(0, page_size - len(payload) + 1, block_size)),
        same_address=True,
        memory="mmio",
    )
    load_mmio_payload(env, payload, base=placement.pa)
    return placement.pc


def prepare_mmio_cnop_stream(env, rng: random.Random) -> int:
    return load_random_mmio_stream(env, rng, uncache._CNOP.to_bytes(2, "little") * 256)


def prepare_normal_and_mmio_cnop_stream(env, rng: random.Random) -> tuple[int, int]:
    payload = uncache._CNOP.to_bytes(2, "little") * 256
    mmio_pc = load_random_mmio_stream(env, rng, payload)
    normal_pc = random_pc(
        rng,
        va_range=(uncache._NORMAL_BASE, uncache._NORMAL_BASE + 0x10_0000),
        pa_range=(uncache._NORMAL_BASE, uncache._NORMAL_BASE + 0x10_0000),
        offsets=(0,), same_address=True,
    ).pc
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=normal_pc), step_cycles=0).run(env)
    return normal_pc, mmio_pc


def mmio_page_tail_entry(rng: random.Random, *, is_rvc: bool, bare: bool = True) -> RandomPc:
    page_size = uncache._SV39_PAGE_SIZE
    return random_pc(
        rng,
        mappings=(PcMapping(uncache._MMIO_BASE if bare else uncache._NORMAL_BASE,
                            uncache._MMIO_BASE, 0x10_0000),),
        entry_offsets=tuple(offset + page_size - 2 for offset in range(0, 0x10_0000, 8 * page_size)),
        boundaries=(PcBoundary(page_size, width=2 if is_rvc else 4,
                               relation="within" if is_rvc else "cross"),),
        instruction_bytes=2 if is_rvc else 4,
        same_address=bare,
        memory="mmio",
    )


def prepare_mmio_page_tail_stream(env, rng: random.Random, *, is_rvc: bool = False) -> int:
    pc = mmio_page_tail_entry(rng, is_rvc=is_rvc).pc
    load_mmio_payload(env, uncache._page_tail_payload(is_rvc=is_rvc), base=uncache._page_align_down(pc))
    return pc


def prepare_sv39_mmio_page_tail(env, rng: random.Random) -> tuple[int, int, bytes]:
    placement = mmio_page_tail_entry(rng, is_rvc=False, bare=False)
    payload = uncache._ADDI_X0_X0_0.to_bytes(4, "little") + uncache._CNOP.to_bytes(2, "little") * 8
    first_page = uncache._page_align_down(placement.pa)
    env.memory.mmio_ranges.append((first_page, first_page + 2 * uncache._SV39_PAGE_SIZE))
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=placement.pa), step_cycles=0).run(env)
    return placement.va, placement.pa, payload


def prepare_mmio_jalr_stream(env, rng: random.Random, instruction: int) -> int:
    payload = instruction.to_bytes(4, "little") + uncache._CNOP.to_bytes(2, "little") * 128
    return load_random_mmio_stream(env, rng, payload)


def mmio_rvi_entry(rng: random.Random, *, cross_beat: bool) -> tuple[RandomPc, bytes]:
    beat_bytes = uncache._UNCACHE_BEAT_BYTES
    image_size = 16 * beat_bytes
    entry_offsets = tuple(
        beat * beat_bytes + offset
        for beat in range(image_size // beat_bytes - 1)
        for offset in ((beat_bytes - 2,) if cross_beat else (0, 2, 4))
    )
    placement = random_pc(
        rng,
        mappings=(PcMapping(uncache._MMIO_BASE, uncache._MMIO_BASE, image_size),),
        entry_offsets=entry_offsets,
        boundaries=(PcBoundary(beat_bytes, width=4, relation="cross" if cross_beat else "within"),),
        instruction_bytes=4,
        same_address=True,
        memory="mmio",
    )
    payload = bytearray(int(uncache._CNOP).to_bytes(2, "little") * (image_size // 2))
    offset = placement.pa - uncache._MMIO_BASE
    payload[offset:offset + 4] = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    return placement, bytes(payload)


def mmio_non_crossing_rvi_stream(
    rng: random.Random,
) -> tuple[list[ProgramImage], list[TraceEntry]]:
    """Link compact runs of single-beat RVI targets on distinct MMIO pages."""
    beat_bytes = uncache._UNCACHE_BEAT_BYTES
    page_size = uncache._SV39_PAGE_SIZE
    beats_per_page = page_size // beat_bytes
    # Bound simulation work while always exercising a cross-page jump.
    target_count = rng.randint(16, 100)
    page_count = rng.randint(2, 8)
    splits = [0, *sorted(rng.sample(range(1, target_count), page_count - 1)), target_count]
    # A 1 MiB window keeps any pair of entries within the signed JAL displacement.
    pages = rng.sample(range(0x10_0000 // page_size), page_count)
    placements = []
    for page, start, end in zip(pages, splits, splits[1:]):
        targets = end - start
        first_beat = rng.randrange(beats_per_page - (targets + 2) + 1)
        base = uncache._MMIO_BASE + page * page_size + first_beat * beat_bytes
        pcs = [base + beat * beat_bytes + rng.choice((0, 2, 4)) for beat in range(targets)]
        placements.append((base, pcs))

    images = []
    trace = []
    for index, (base, pcs) in enumerate(placements):
        image_beats = len(pcs) + 2
        payload = bytearray(uncache._CNOP.to_bytes(2, "little") * (image_beats * beat_bytes // 2))
        cursor = pcs[0]
        for pc in pcs:
            for padding_pc in range(cursor, pc, 2):
                trace.append(TraceEntry(len(trace), padding_pc, uncache._CNOP, 2))
            offset = pc - base
            payload[offset:offset + 4] = uncache._ADDI_X0_X0_0.to_bytes(4, "little")
            trace.append(TraceEntry(len(trace), pc, uncache._ADDI_X0_X0_0, 4))
            cursor = pc + 4

        if index + 1 < page_count:
            target_pc = placements[index + 1][1][0]
            jump_pc = rng.choice([
                pc for pc in range(cursor, base + (image_beats - 1) * beat_bytes, 2)
                if pc % beat_bytes <= beat_bytes - 4
            ])
            for padding_pc in range(cursor, jump_pc, 2):
                trace.append(TraceEntry(len(trace), padding_pc, uncache._CNOP, 2))
            jump = encode_jal_x0(target_pc - jump_pc)
            offset = jump_pc - base
            payload[offset:offset + 4] = jump.to_bytes(4, "little")
            trace.append(TraceEntry(len(trace), jump_pc, jump, 4, kind="jump", taken=True, target_pc=target_pc))
        else:
            # Keep the monitor active on the cycle when the final RVI consumes its trace entry.
            trace.append(TraceEntry(len(trace), cursor, uncache._CNOP, 2))
        images.append(ProgramImage(payload=bytes(payload), base_addr=base))
    return images, trace


def mmio_rvc_beat_tail_entry(rng: random.Random) -> tuple[RandomPc, bytes]:
    beat_bytes = uncache._UNCACHE_BEAT_BYTES
    image_size = 16 * beat_bytes
    placement = random_pc(
        rng,
        mappings=(PcMapping(uncache._MMIO_BASE, uncache._MMIO_BASE, image_size),),
        entry_offsets=tuple(beat * beat_bytes + beat_bytes - 2 for beat in range(15)),
        boundaries=(PcBoundary(beat_bytes, width=2, relation="within"),),
        instruction_bytes=2,
        same_address=True,
        memory="mmio",
    )
    payload = int(uncache._CNOP).to_bytes(2, "little") * (image_size // 2)
    return placement, payload


def mmio_cross_beat_rvi_stream(env, rng: random.Random) -> tuple[int, int, int, bytes]:
    placement, payload = mmio_rvi_entry(rng, cross_beat=True)
    load_mmio_payload(env, payload)
    first_beat = placement.pc & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    return placement.pc, first_beat, first_beat + uncache._UNCACHE_BEAT_BYTES, payload


def prepare_mmio_cross_beat_stream(env, rng: random.Random) -> tuple[int, int]:
    cnop = uncache._CNOP.to_bytes(2, "little")
    payload = cnop * 3 + uncache._ADDI_X0_X0_0.to_bytes(4, "little") + cnop * 128
    first_beat = load_random_mmio_stream(env, rng, payload)
    return first_beat + uncache._UNCACHE_BEAT_BYTES - 2, first_beat


def prepare_non_crossing_rvi_offsets(env, rng: random.Random) -> tuple[int, int, int]:
    offsets = (0, 10, 20)
    payload = bytearray(uncache._CNOP.to_bytes(2, "little") * 128)
    for offset in offsets:
        payload[offset:offset + 4] = uncache._ADDI_X0_X0_0.to_bytes(4, "little")
    base = load_random_mmio_stream(env, rng, bytes(payload))
    return base + offsets[0], base + offsets[1], base + offsets[2]


def configure_random_mmio_latency(
    env, rng: random.Random, *, minimum: int = 1, maximum: int = 16,
) -> int:
    latency = rng.randint(minimum, maximum)
    env.uncache_agent.configure(latency=latency)
    return latency


def capture_mmio_snapshots(env) -> list[dict[str, int | None]]:
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        sample = owner_funcov._snapshot(active_env.functional_coverage, active_env.dut)
        sample["cycle"] = int(cycle)
        snapshots.append(sample)

    env.register_cycle_observer(capture)
    return snapshots


def _run_mmio_redirect_cancels_wait_last_commit_before_request(
    env,
    *,
    pc: int,
    target_pc: int,
    latency: int | None = None,
):
    """Redirect a non-first MMIO while it is still commit ordered."""
    if latency is not None:
        env.uncache_agent.configure(latency=latency)
    env.backend_model.set_can_accept(1)
    env.backend_model.commit_min_delay = 1000
    env.backend_model.commit_max_delay = 1000
    env.backend_model.backend_empty_for_dut = lambda: 0
    snapshots = capture_mmio_snapshots(env)
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert _wait_for_req_delta(env, env.uncache_agent)
    assert _wait_for_resp_delta(env, env.uncache_agent)
    assert run_until(
        env,
        lambda: bool(snapshots) and snapshots[-1]["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT,
        max_cycles=8000,
    ), {
        "snapshots": snapshots[-64:]
    }

    old_req_count = int(env.uncache_agent.get_stats().get("req_count", 0))
    observations_before_redirect = len(env.monitor.observations)
    source = next(
        entry
        for entry in env.backend_model._cfvec_queue
        if int(entry.pc) == pc
    )
    env.backend_model.inject_redirect_from_cfvec(
        source_pc=int(source.pc),
        source_ftq_flag=int(source.ftq_flag),
        source_ftq_value=int(source.ftq_value),
        source_ftq_offset=int(source.ftq_offset),
        target_pc=target_pc,
        reason="mmio_wait_last_commit_cancel",
        redirect_class=BackendRedirectClass.MEMORY_VIOLATION,
        delay_cycles=0,
    )
    env.step(8)

    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT
        and sample["backend_redirect"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT
        and sample["ifu_flush"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == old_req_count

    env.backend_model.backend_empty_for_dut = lambda: 1
    assert uncache._wait_for_request_addr(env, target_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not any(
        int(item.pc) == pc
        for item in env.monitor.observations[observations_before_redirect:]
    )
    assert not env.monitor.get_errors()
