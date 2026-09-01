from __future__ import annotations

from typing import Any, Optional

from ..common.dut import _read
from ..common.fetch_memory import _read_expected_fetch_raw, _recover_unavailable_instr
from ....support.rvc_decoder import expand_rvc
from .owner_v3_funcov import mark_owner_v3_checked


_IFU_INTERNAL_PREFIXES = (
    "Frontend_top.Frontend.inner_ifu.__Vtogcov__",
    "Frontend_top.Frontend.inner_ifu.",
    "Frontend_top.Frontend._inner_ifu_",
)
_IFU_OUTPUT_SLOT_COUNT = 36
_FETCH_BLOCK_INST_COUNT = 32
_IBUFFER_ENTRY_COUNT = 48
_FETCH_EXCEPTION_VALUES = frozenset({1, 2, 3, 5})
_INVALID_TAKEN_S2_TIMEOUT_CYCLES = 16
_INVALID_TAKEN_HOLD_TIMEOUT_CYCLES = 512


def _read_ifu_internal(recorder, dut, stem: str) -> Optional[int]:
    return recorder._read_first_dut_signal(
        dut,
        tuple(prefix + str(stem) for prefix in _IFU_INTERNAL_PREFIXES),
    )


def _read_ifu_output_slot(recorder, dut, field: str, slot: int, suffix: str = "") -> Optional[int]:
    return _read_ifu_internal(recorder, dut, f"io_toIBuffer_bits_{field}_{int(slot)}{suffix}")


def _read_ibuffer_internal(recorder, dut, stem: str) -> Optional[int]:
    return recorder._read_first_dut_signal(
        dut,
        (
            "Frontend_top.Frontend.inner_ibuffer." + str(stem),
            "Frontend_top.Frontend.inner_ibuffer.__Vtogcov__" + str(stem),
            "Frontend_top.Frontend._inner_ibuffer_" + str(stem),
        ),
    )


def _ibuffer_pointer(flag: Optional[int], value: Optional[int]) -> Optional[tuple[int, int]]:
    if flag is None or value is None:
        return None
    return int(flag), int(value)


def _advance_ibuffer_pointer(pointer: tuple[int, int], count: int) -> tuple[int, int]:
    absolute = (int(pointer[0]) & 1) * _IBUFFER_ENTRY_COUNT + int(pointer[1])
    advanced = (absolute + int(count)) % (2 * _IBUFFER_ENTRY_COUNT)
    return advanced // _IBUFFER_ENTRY_COUNT, advanced % _IBUFFER_ENTRY_COUNT


def _ibuffer_pointer_distance(
    newer: tuple[int, int], older: tuple[int, int]
) -> int:
    newer_absolute = (int(newer[0]) & 1) * _IBUFFER_ENTRY_COUNT + int(newer[1])
    older_absolute = (int(older[0]) & 1) * _IBUFFER_ENTRY_COUNT + int(older[1])
    return (newer_absolute - older_absolute) % (2 * _IBUFFER_ENTRY_COUNT)


def _read_ibuffer_state(recorder, dut) -> dict[str, Any]:
    enq_pointer = _ibuffer_pointer(
        _read_ibuffer_internal(recorder, dut, "enqPtrDup_0_flag"),
        _read_ibuffer_internal(recorder, dut, "enqPtrDup_0_value"),
    )
    deq_pointer = _ibuffer_pointer(
        _read_ibuffer_internal(recorder, dut, "deqPtrVec_0_flag"),
        _read_ibuffer_internal(recorder, dut, "deqPtrVec_0_value"),
    )
    head_values = (
        _read_ibuffer_internal(recorder, dut, "outputEntries_0_bits_pc_addr"),
        _read_ibuffer_internal(recorder, dut, "outputEntries_0_bits_ftqPtr_flag"),
        _read_ibuffer_internal(recorder, dut, "outputEntries_0_bits_ftqPtr_value"),
        _read_ibuffer_internal(recorder, dut, "outputEntries_0_bits_instrEndOffset"),
        _read_ibuffer_internal(recorder, dut, "outputEntries_0_bits_inst"),
    )
    head_identity = (
        None
        if any(value is None for value in head_values)
        else tuple(int(value) for value in head_values)
    )
    return {
        "num_valid": _read_ibuffer_internal(recorder, dut, "numValid"),
        "enq_pointer": enq_pointer,
        "deq_pointer": deq_pointer,
        "head_valid": _read_ibuffer_internal(recorder, dut, "outputEntries_0_valid"),
        "head_identity": head_identity,
        "flush": recorder._read_first_dut_signal(
            dut,
            (
                "Frontend_top.Frontend.inner_needFlush",
                "Frontend_top.Frontend.__Vtogcov__inner_needFlush",
            ),
        ),
        "backend_can_accept": recorder._read_first_dut_signal(
            dut,
            (
                "Frontend_top.io_backend_toIBuf_decodeCanAccept",
                "Frontend_top.__Vtogcov__io_backend_toIBuf_decodeCanAccept",
            ),
        ),
    }


def _ibuffer_state_complete(state: dict[str, Any]) -> bool:
    return all(
        state.get(field) is not None
        for field in (
            "num_valid",
            "enq_pointer",
            "deq_pointer",
            "head_valid",
            "head_identity",
            "flush",
            "backend_can_accept",
        )
    )


def _read_predchecker(recorder, dut, stem: str) -> Optional[int]:
    value = recorder._read_first_dut_signal(
        dut,
        (
            "Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__" + str(stem),
            "Frontend_top.Frontend.inner_ifu.predChecker." + str(stem),
        ),
    )
    if value is not None:
        return value
    return _read_ifu_internal(recorder, dut, f"predChecker.{stem}")


def _read_predchecker_or_ifu(recorder, dut, pred_stem: str, ifu_stem: str) -> Optional[int]:
    value = _read_predchecker(recorder, dut, pred_stem)
    if value is not None:
        return value
    return _read_ifu_internal(recorder, dut, ifu_stem)


def _read_ftq_first(recorder, dut, *stems: str) -> Optional[int]:
    return recorder._read_first_dut_signal(
        dut,
        tuple(
            prefix + str(stem)
            for stem in stems
            for prefix in (
                "Frontend_top.Frontend.inner_ftq.",
                "Frontend_top.Frontend._inner_ftq_",
                "Frontend_top.Frontend.inner_ftq.__Vtogcov__",
            )
        ),
    )


def _read_gpaddr_output(recorder, dut, stem: str) -> Optional[int]:
    value = _read_ifu_internal(recorder, dut, f"io_toBackend_gpAddrMem_{stem}")
    if value is not None:
        return value
    return recorder._read_first_dut_signal(
        dut,
        (
            f"Frontend_top.io_backend_fromIfu_gpAddrMem_{stem}",
            f"Frontend_top.__Vtogcov__io_backend_fromIfu_gpAddrMem_{stem}",
        ),
    )


_FRONTEND_TRIGGER_PREFIXES = (
    "Frontend_top.Frontend.inner_ifu.frontendTrigger.__Vtogcov__",
    "Frontend_top.Frontend.inner_ifu.frontendTrigger.",
)


def _read_frontend_trigger(recorder, dut, stem: str) -> Optional[int]:
    return recorder._read_first_dut_signal(
        dut,
        tuple(prefix + str(stem) for prefix in _FRONTEND_TRIGGER_PREFIXES),
    )


def _frontend_trigger_config(recorder, dut, slot: int) -> Optional[tuple[int, ...]]:
    values = tuple(
        _read_frontend_trigger(recorder, dut, f"tdataVec_{int(slot)}_{field}")
        for field in ("matchType", "select", "timing", "action", "chain", "tdata2")
    )
    if any(value is None for value in values):
        return None
    return tuple(int(value) for value in values)


def _frontend_trigger_compare(pc: int, match_type: int, tdata2: int) -> bool:
    if int(match_type) == 0:
        return int(pc) == int(tdata2)
    if int(match_type) == 2:
        return int(pc) >= int(tdata2)
    if int(match_type) == 3:
        return int(pc) < int(tdata2)
    return False


def _frontend_trigger_can_fire_name(lane: int, slot: int) -> str:
    if int(lane) == 0:
        return f"triggerCanFireVec_{int(slot)}"
    return f"triggerCanFireVec_{int(lane)}_{int(slot)}"


def _frontend_trigger_state(recorder) -> dict[str, Any]:
    state = getattr(recorder, "_ifu_frontend_trigger_state", None)
    if state is None:
        state = {
            "last_configs": None,
            "verified_updates": {},
            "update_samples": {},
            "enable_samples": {},
            "match_type_samples": {0: set(), 2: set(), 3: set()},
            "suppression_samples": set(),
            "chain_samples": set(),
            "action_samples": set(),
            "held_trigger": None,
            "marked": set(),
        }
        recorder._ifu_frontend_trigger_state = state
    return state


def _mark_frontend_trigger_once(
    recorder,
    state: dict[str, Any],
    bin_id: str,
    cycle: int,
    observations: dict[str, Any],
) -> None:
    if str(bin_id) in state["marked"]:
        return
    if mark_owner_v3_checked(
        recorder,
        str(bin_id),
        int(cycle),
        observations,
        producer="ifu_frontend_trigger_v3_sampler",
    ):
        state["marked"].add(str(bin_id))


def _sample_frontend_trigger(recorder, dut, cycle: int) -> None:
    state = _frontend_trigger_state(recorder)
    configs = tuple(_frontend_trigger_config(recorder, dut, slot) for slot in range(4))
    if any(config is None for config in configs):
        return
    configs = tuple(config for config in configs if config is not None)

    update_valid = _read_ifu_internal(recorder, dut, "io_frontendTrigger_tUpdate_valid")
    update_addr = _read_ifu_internal(recorder, dut, "io_frontendTrigger_tUpdate_bits_addr")
    if update_valid == 1 and update_addr is not None:
        slot = int(update_addr) & 0x3
        input_values = tuple(
            _read_ifu_internal(
                recorder,
                dut,
                f"io_frontendTrigger_tUpdate_bits_tdata_{field}",
            )
            for field in ("matchType", "select", "timing", "action", "chain", "tdata2")
        )
        previous = state.get("last_configs")
        non_target_stable = previous is not None and all(
            configs[index] == previous[index] for index in range(4) if index != slot
        )
        if not any(value is None for value in input_values):
            input_config = tuple(int(value) for value in input_values)
            if configs[slot] == input_config and non_target_stable:
                signature = (slot, *input_config)
                state["verified_updates"][signature] = {
                    "cycle": int(cycle),
                    "slot": slot,
                    "config": input_config,
                    "non_target_configs_unchanged": True,
                }

    state["last_configs"] = configs

    s2_valid = _read_ifu_internal(recorder, dut, "s2_valid_valid")
    s2_flush = _read_ifu_internal(recorder, dut, "s2_flush")
    valid_mask = _read_ifu_internal(recorder, dut, "s2_alignedInstrValid")
    output_valid = _read_ifu_internal(recorder, dut, "io_toIBuffer_valid")
    output_ready = _read_ifu_internal(recorder, dut, "io_toIBuffer_ready")
    output_enq = _read_ifu_internal(recorder, dut, "io_toIBuffer_bits_enqEnable")

    if s2_flush == 1:
        held = state.get("held_trigger")
        backend_redirect = _read_ifu_internal(
            recorder, dut, "io_fromFtq_redirect_valid"
        )
        if held is not None and backend_redirect == 1 and output_valid == 0:
            same_identity = all(
                _read_ifu_output_slot(recorder, dut, "pc", lane["lane"], "_addr")
                == (int(lane["pc"]) >> 1)
                and _read_ifu_output_slot(
                    recorder, dut, "ftqPtr", lane["lane"], "_flag"
                )
                == lane["ftq_flag"]
                and _read_ifu_output_slot(
                    recorder, dut, "ftqPtr", lane["lane"], "_value"
                )
                == lane["ftq_value"]
                for lane in held["lanes"]
            )
            if same_identity:
                _mark_frontend_trigger_once(
                    recorder,
                    state,
                    "BIN-1003",
                    cycle,
                    {
                        "event": "frontend_trigger_held_identity_flushed",
                        "held_cycle": held["cycle"],
                        "flush_cycle": int(cycle),
                        "held_lanes": held["lanes"],
                        "backend_redirect": int(backend_redirect),
                        "s2_flush": int(s2_flush),
                        "to_ibuffer_valid_during_flush": int(output_valid),
                        "same_pc_ftq_identity_during_flush": True,
                        "old_identity_delivery_suppressed": True,
                    },
                )
        state["held_trigger"] = None
        return

    if s2_valid != 1 or valid_mask is None:
        return

    debug_mode = _read_ifu_internal(recorder, dut, "io_frontendTrigger_debugMode")
    can_raise_bp = _read_ifu_internal(
        recorder, dut, "io_frontendTrigger_triggerCanRaiseBpExp"
    )
    if debug_mode is None or can_raise_bp is None:
        return

    enable = tuple(
        _read_frontend_trigger(recorder, dut, f"triggerEnableVec_{slot}")
        for slot in range(4)
    )
    if any(value is None for value in enable):
        return
    enable = tuple(int(value) for value in enable if value is not None)

    lanes: list[dict[str, Any]] = []
    for lane in range(_IFU_OUTPUT_SLOT_COUNT):
        if not (int(valid_mask) & (1 << lane)):
            continue
        pc_encoded = _read_ifu_internal(recorder, dut, f"s2_alignedInstrPcVec_{lane}_addr")
        if pc_encoded is None:
            continue
        pc = int(pc_encoded) << 1
        hits = tuple(
            _read_frontend_trigger(recorder, dut, f"triggerHitVec_{lane}_{slot}")
            for slot in range(3)
        )
        can_fire = tuple(
            _read_frontend_trigger(
                recorder,
                dut,
                _frontend_trigger_can_fire_name(lane, slot),
            )
            for slot in range(4)
        )
        output_triggered = _read_ifu_output_slot(recorder, dut, "triggered", lane)
        if any(value is None for value in (*hits, *can_fire)):
            continue
        lane_observation = {
            "lane": lane,
            "pc": pc,
            "hits": tuple(int(value) for value in hits if value is not None),
            "can_fire": tuple(int(value) for value in can_fire if value is not None),
            "output_triggered": None
            if output_triggered is None
            else int(output_triggered),
        }
        lanes.append(lane_observation)

        for slot, config in enumerate(configs[:3]):
            match_type, select, timing, action, chain, tdata2 = config
            raw_match = _frontend_trigger_compare(pc, match_type, tdata2)
            actual_hit = lane_observation["hits"][slot]
            expected_hit = int(
                raw_match
                and enable[slot] == 1
                and select == 0
                and int(debug_mode) == 0
            )
            if actual_hit != expected_hit:
                recorder.risk_observations.append(
                    {
                        "event": "frontend_trigger_compare_mismatch",
                        "cycle": int(cycle),
                        "lane": lane,
                        "slot": slot,
                        "pc": pc,
                        "config": list(config),
                        "enable": enable[slot],
                        "debug_mode": int(debug_mode),
                        "expected_hit": expected_hit,
                        "actual_hit": actual_hit,
                    }
                )
                continue

            signature = (slot, *config)
            if signature in state["verified_updates"] and select == 0 and int(debug_mode) == 0:
                samples = state["update_samples"].setdefault(signature, set())
                samples.add("hit" if actual_hit == 1 else "miss")
                if {"hit", "miss"} <= samples:
                    _mark_frontend_trigger_once(
                        recorder,
                        state,
                        "BIN-996",
                        cycle,
                        {
                            "event": "frontend_trigger_update_applied",
                            **state["verified_updates"][signature],
                            "pc": pc,
                            "observed_outcomes": sorted(samples),
                        },
                    )

            if raw_match and select == 0 and int(debug_mode) == 0:
                enable_key = (slot, *config, pc)
                samples = state["enable_samples"].setdefault(enable_key, set())
                if enable[slot] == 0 and actual_hit == 0:
                    samples.add("disabled")
                if enable[slot] == 1 and actual_hit == 1:
                    samples.add("enabled")
                if {"disabled", "enabled"} <= samples:
                    _mark_frontend_trigger_once(
                        recorder,
                        state,
                        "BIN-997",
                        cycle,
                        {
                            "event": "frontend_trigger_enable_toggle",
                            "slot": slot,
                            "pc": pc,
                            "config": list(config),
                            "observed_states": sorted(samples),
                        },
                    )

            if match_type in state["match_type_samples"] and enable[slot] == 1 and select == 0 and int(debug_mode) == 0:
                state["match_type_samples"][match_type].add(
                    "hit" if actual_hit == 1 else "miss"
                )
                if all(
                    {"hit", "miss"} <= state["match_type_samples"][mode]
                    for mode in (0, 2, 3)
                ):
                    _mark_frontend_trigger_once(
                        recorder,
                        state,
                        "BIN-998",
                        cycle,
                        {
                            "event": "frontend_trigger_match_types",
                            "mode_outcomes": {
                                str(mode): sorted(state["match_type_samples"][mode])
                                for mode in (0, 2, 3)
                            },
                            "last_lane": lane,
                            "last_pc": pc,
                        },
                    )

            if raw_match and enable[slot] == 1 and actual_hit == 0:
                if select == 1:
                    state["suppression_samples"].add("select")
                if int(debug_mode) == 1:
                    state["suppression_samples"].add("debug")
                if {"select", "debug"} <= state["suppression_samples"]:
                    _mark_frontend_trigger_once(
                        recorder,
                        state,
                        "BIN-999",
                        cycle,
                        {
                            "event": "frontend_trigger_match_suppressed",
                            "slot": slot,
                            "lane": lane,
                            "pc": pc,
                            "config": list(config),
                            "suppression_modes": sorted(state["suppression_samples"]),
                        },
                    )

        if len(configs) >= 2:
            first = configs[0]
            second = configs[1]
            if (
                first[4] == 1
                and second[4] == 0
                and lane_observation["hits"][0] == 1
                and lane_observation["hits"][1] == 1
                and lane_observation["can_fire"][0] == 0
            ):
                if first[2] == second[2] and lane_observation["can_fire"][1] == 1:
                    state["chain_samples"].add("chain_pass")
                if first[2] != second[2] and lane_observation["can_fire"][1] == 0:
                    state["chain_samples"].add("timing_block")
                if {"chain_pass", "timing_block"} <= state["chain_samples"]:
                    _mark_frontend_trigger_once(
                        recorder,
                        state,
                        "BIN-1000",
                        cycle,
                        {
                            "event": "frontend_trigger_chain_timing",
                            "lane": lane,
                            "pc": pc,
                            "configs": [list(first), list(second)],
                            "hits": list(lane_observation["hits"][:2]),
                            "can_fire": list(lane_observation["can_fire"][:2]),
                            "observed_states": sorted(state["chain_samples"]),
                        },
                    )

        firing_slots = [
            slot for slot, value in enumerate(lane_observation["can_fire"]) if value == 1
        ]
        if len(firing_slots) == 1 and output_triggered is not None:
            firing_slot = firing_slots[0]
            action = configs[firing_slot][3]
            if action == 1 and int(output_triggered) == 1:
                state["action_samples"].add("debug_action")
            if action == 0 and int(can_raise_bp) == 1 and int(output_triggered) == 0:
                state["action_samples"].add("breakpoint_action")
            if {"debug_action", "breakpoint_action"} <= state["action_samples"]:
                _mark_frontend_trigger_once(
                    recorder,
                    state,
                    "BIN-1001",
                    cycle,
                    {
                        "event": "frontend_trigger_action_generation",
                        "lane": lane,
                        "pc": pc,
                        "firing_slot": firing_slot,
                        "action": action,
                        "trigger_can_raise_bp": int(can_raise_bp),
                        "output_triggered": int(output_triggered),
                        "observed_actions": sorted(state["action_samples"]),
                    },
                )

    if output_valid == 1 and output_ready == 1 and output_enq is not None:
        state["held_trigger"] = None
        delivered = [lane for lane in lanes if int(output_enq) & (1 << lane["lane"])]
        firing_delivered = [lane for lane in delivered if sum(lane["can_fire"]) == 1]
        if len(firing_delivered) == 1:
            selected = firing_delivered[0]
            peers_clean = all(
                lane["output_triggered"] == 15
                for lane in delivered
                if lane["lane"] != selected["lane"] and sum(lane["can_fire"]) == 0
            )
            if selected["output_triggered"] in {0, 1} and peers_clean:
                _mark_frontend_trigger_once(
                    recorder,
                    state,
                    "BIN-1002",
                    cycle,
                    {
                        "event": "frontend_trigger_lane_delivery_alignment",
                        "selected_lane": selected,
                        "delivered_lanes": delivered,
                        "to_ibuffer_fire": True,
                        "peer_lanes_untriggered": True,
                    },
                )

    if output_valid == 1 and output_ready == 0 and output_enq is not None:
        held_lanes = []
        for lane in lanes:
            lane_index = int(lane["lane"])
            if not (int(output_enq) & (1 << lane_index)):
                continue
            if sum(lane["can_fire"]) != 1 or lane["output_triggered"] not in {0, 1}:
                continue
            ftq_flag = _read_ifu_output_slot(
                recorder, dut, "ftqPtr", lane_index, "_flag"
            )
            ftq_value = _read_ifu_output_slot(
                recorder, dut, "ftqPtr", lane_index, "_value"
            )
            if ftq_flag is None or ftq_value is None:
                continue
            held_lanes.append(
                {
                    "lane": lane_index,
                    "pc": int(lane["pc"]),
                    "ftq_flag": int(ftq_flag),
                    "ftq_value": int(ftq_value),
                    "can_fire": list(lane["can_fire"]),
                    "output_triggered": int(lane["output_triggered"]),
                }
            )
        if held_lanes:
            state["held_trigger"] = {
                "cycle": int(cycle),
                "lanes": held_lanes,
            }


def _sample_ftq_training_mask(recorder, dut, cycle: int) -> None:
    """Check the V3 FTQ mask after the first mispredict in one train entry."""

    source_valid = _read_ftq_first(
        recorder,
        dut,
        "resolveQueue.io_bpuTrain_valid",
        "_resolveQueue_io_bpuTrain_valid",
    )
    source_ready = _read_ftq_first(
        recorder,
        dut,
        "resolveQueue.io_bpuTrain_ready",
        "resolveQueue_io_bpuTrain_ready",
        "__Vtogcov__resolveQueue_io_bpuTrain_ready",
    )
    if source_valid == 1 and source_ready == 1:
        branches = []
        for index in range(8):
            valid = _read_ftq_first(
                recorder,
                dut,
                f"resolveQueue.io_bpuTrain_bits_branches_{index}_valid",
                f"_resolveQueue_io_bpuTrain_bits_branches_{index}_valid",
            )
            position = _read_ftq_first(
                recorder,
                dut,
                f"resolveQueue.io_bpuTrain_bits_branches_{index}_bits_cfiPosition",
                f"_resolveQueue_io_bpuTrain_bits_branches_{index}_bits_cfiPosition",
            )
            mispredict = _read_ftq_first(
                recorder,
                dut,
                f"resolveQueue.io_bpuTrain_bits_branches_{index}_bits_mispredict",
                f"_resolveQueue_io_bpuTrain_bits_branches_{index}_bits_mispredict",
            )
            if None in {valid, position, mispredict}:
                return
            if int(valid) == 1:
                branches.append(
                    {
                        "index": index,
                        "position": int(position),
                        "mispredict": int(mispredict),
                    }
                )
        mispredicts = [branch for branch in branches if branch["mispredict"] == 1]
        first_mispredict_position = min(
            (branch["position"] for branch in mispredicts), default=None
        )
        if first_mispredict_position is not None and any(
            branch["position"] > first_mispredict_position for branch in branches
        ):
            recorder._ifu_ftq_training_mask_pending = {
                "cycle": int(cycle),
                "branches": branches,
                "first_mispredict_position": int(first_mispredict_position),
            }

    pending = getattr(recorder, "_ifu_ftq_training_mask_pending", None)
    if pending is None or int(cycle) <= int(pending["cycle"]):
        return
    output_valid = _read_ftq_first(recorder, dut, "trainCache_valid")
    if output_valid != 1:
        if int(cycle) - int(pending["cycle"]) > 2:
            recorder._ifu_ftq_training_mask_pending = None
        return
    cached = []
    for index in range(8):
        valid = _read_ftq_first(
            recorder, dut, f"trainCache_bits_branches_{index}_valid"
        )
        if valid is None:
            return
        cached.append(int(valid))
    younger = [
        branch["index"]
        for branch in pending["branches"]
        if branch["position"] > pending["first_mispredict_position"]
    ]
    retained = [
        branch["index"]
        for branch in pending["branches"]
        if branch["position"] <= pending["first_mispredict_position"]
    ]
    if (
        younger
        and retained
        and all(cached[index] == 0 for index in younger)
        and all(cached[index] == 1 for index in retained)
    ):
        mark_owner_v3_checked(
            recorder,
            "BIN-958",
            cycle,
            {
                "event": "ftq_first_mispredict_training_mask",
                "resolve_cycle": int(pending["cycle"]),
                "branches": pending["branches"],
                "first_mispredict_position": pending["first_mispredict_position"],
                "train_cache_branch_valid": cached,
                "retained_branch_indices": retained,
                "younger_branch_indices": younger,
            },
            producer="ifu_ftq_training_mask_sampler",
        )
    recorder._ifu_ftq_training_mask_pending = None


def _decode_pruned_pc(encoded_pc: Optional[int]) -> Optional[int]:
    """Restore the byte address carried by the IFU PrunedAddr bundle.

    ``PrunedAddr.addr`` omits the instruction-alignment bit, so the generated
    DUT signal is a halfword address rather than a byte address.
    """

    return None if encoded_pc is None else int(encoded_pc) << 1


def _active_ifu_output_slots(enq_enable: int, valid_mask: int) -> list[int]:
    active_mask = int(enq_enable) & int(valid_mask)
    return [slot for slot in range(_IFU_OUTPUT_SLOT_COUNT) if active_mask & (1 << slot)]


def _read_ifu_output_mask(recorder, dut, field: str) -> Optional[int]:
    mask = 0
    # The final aligned slot is a sentinel and is not part of IBufferEnqueueWidth.
    for slot in range(_IFU_OUTPUT_SLOT_COUNT - 1):
        value = _read_ifu_output_slot(recorder, dut, field, slot)
        if value is None:
            return None
        mask |= (int(value) & 1) << slot
    return mask


def _is_contiguous(slots: list[int]) -> bool:
    return len(slots) >= 2 and slots == list(range(slots[0], slots[-1] + 1))


def _records_follow_instruction_boundaries(records: list[dict[str, Any]]) -> bool:
    if len(records) < 2:
        return False
    for before, after in zip(records, records[1:]):
        if None in {before["pc"], before["is_rvc"], after["pc"]}:
            return False
        expected_pc = int(before["pc"]) + (2 if int(before["is_rvc"]) else 4)
        if int(after["pc"]) != expected_pc:
            return False
    return True


def _read_raw_instruction(env, pc: int, is_rvc: bool) -> Optional[int]:
    raw, metadata = _read_expected_fetch_raw(env, int(pc), 2 if bool(is_rvc) else 4)
    if raw is None or not bool(metadata.get("ok", False)):
        return None
    return int(raw) & (0xFFFF if bool(is_rvc) else 0xFFFFFFFF)


def _decode_branch_type(instr: int) -> int:
    opcode = int(instr) & 0x7F
    if opcode == 0x63:
        return 1
    if opcode == 0x6F:
        return 2
    if opcode == 0x67:
        return 3
    return 0


def _decode_cfi_offset(instr: int, branch_type: int) -> Optional[int]:
    instr = int(instr) & 0xFFFFFFFF
    if int(branch_type) == 1:
        immediate = (
            (((instr >> 31) & 1) << 12)
            | (((instr >> 7) & 1) << 11)
            | (((instr >> 25) & 0x3F) << 5)
            | (((instr >> 8) & 0xF) << 1)
        )
        width = 13
    elif int(branch_type) == 2:
        immediate = (
            (((instr >> 31) & 1) << 20)
            | (((instr >> 12) & 0xFF) << 12)
            | (((instr >> 20) & 1) << 11)
            | (((instr >> 21) & 0x3FF) << 1)
        )
        width = 21
    else:
        return None
    sign = 1 << (width - 1)
    return (immediate ^ sign) - sign


def _read_ibuffer_payload_signature(recorder, dut, enq_enable: int, valid_mask: int) -> tuple:
    values: list[Any] = [int(enq_enable), int(valid_mask)]
    for slot in _active_ifu_output_slots(enq_enable, valid_mask):
        values.append(
            (
                int(slot),
                _read_ifu_output_slot(recorder, dut, "instrs", slot),
                _read_ifu_output_slot(recorder, dut, "pc", slot, "_addr"),
                _read_ifu_output_slot(recorder, dut, "isRvc", slot),
                _read_ifu_output_slot(recorder, dut, "ftqPtr", slot, "_flag"),
                _read_ifu_output_slot(recorder, dut, "ftqPtr", slot, "_value"),
                _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_offset"),
                _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_predTaken"),
                _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_fixedTaken"),
            )
        )
    return tuple(values)


def _sample_ibuffer_backpressure(
    recorder,
    dut,
    cycle: int,
    ready: int,
    valid: int,
    enq_enable: int,
    valid_mask: int,
    pointer_redirect: bool,
) -> None:
    pending = getattr(recorder, "_ifu_ibuffer_hold_pending", None)
    if pointer_redirect:
        if pending is not None and not (int(valid) == 1 and int(ready) == 1):
            mark_owner_v3_checked(
                recorder,
                "BIN-943",
                cycle,
                {
                    "held_cycle": int(pending["cycle"]),
                    "redirect_cycle": int(cycle),
                    "held_signature": pending["signature"],
                    "to_ibuffer_valid": int(valid),
                    "to_ibuffer_ready": int(ready),
                    "old_payload_fired": False,
                },
                producer="ifu_ibuffer_backpressure_sampler",
            )
        recorder._ifu_ibuffer_hold_pending = None
        return

    if int(valid) == 1 and int(ready) == 0:
        signature = _read_ibuffer_payload_signature(recorder, dut, enq_enable, valid_mask)
        evidence = {
            "event": "ifu_to_ibuffer_backpressure",
            "enq_enable": int(enq_enable),
            "valid_mask": int(valid_mask),
        }
        if pending is not None and signature == pending["signature"]:
            recorder.mark("ifu_ibuffer_backpressure", "payload_stable", cycle, evidence)
        recorder._ifu_ibuffer_hold_pending = {
            "cycle": int(cycle),
            "signature": signature,
        }
        s1_valid = _read_ifu_internal(recorder, dut, "s1_valid")
        s1_ready = _read_ifu_internal(recorder, dut, "s1_ready")
        if s1_valid == 1 and s1_ready == 0:
            recorder.mark("ifu_ibuffer_backpressure", "upstream_stalled", cycle, evidence)
        return

    if int(valid) == 1 and int(ready) == 1 and pending is not None:
        signature = _read_ibuffer_payload_signature(recorder, dut, enq_enable, valid_mask)
        if signature == pending["signature"]:
            recorder.mark(
                "ifu_ibuffer_backpressure",
                "held_payload_delivered",
                cycle,
                {
                    "event": "ifu_to_ibuffer_backpressure_release",
                    "held_cycle": int(pending["cycle"]),
                    "enq_enable": int(enq_enable),
                    "valid_mask": int(valid_mask),
                },
            )
    recorder._ifu_ibuffer_hold_pending = None


def _sample_redirect_lifecycle(recorder, dut, cycle: int) -> None:
    previous = getattr(recorder, "_ifu_redirect_last_state", None)
    backend_redirect = _read_ifu_internal(recorder, dut, "io_fromFtq_redirect_valid")
    wb_redirect = _read_ifu_internal(recorder, dut, "wbRedirect_valid")
    outbound_redirect = _read_ifu_internal(recorder, dut, "io_toFtq_wbRedirect_valid")
    s0_flush = _read_ifu_internal(recorder, dut, "s0_flush")
    s1_flush = _read_ifu_internal(recorder, dut, "s1_flush")
    s2_flush = _read_ifu_internal(recorder, dut, "s2_flush")
    s0_half = _read_ifu_internal(recorder, dut, "s0_prevEndIsHalfRvi")
    s1_half_valid = _read_ifu_internal(recorder, dut, "s1_prevEndHalfRviInfo_valid")
    s1_half_data = _read_ifu_internal(
        recorder, dut, "s1_prevEndHalfRviInfo_bits_data"
    )
    s1_half_pc = _read_ifu_internal(
        recorder, dut, "s1_prevEndHalfRviInfo_bits_pc_addr"
    )
    s1_ptr = _read_ifu_internal(recorder, dut, "s1_prevIBufEnqPtrDup_dup_0_value")
    s1_valid = _read_ifu_internal(recorder, dut, "s1_valid")
    s2_valid = _read_ifu_internal(recorder, dut, "s2_valid_valid")

    pending = getattr(recorder, "_ifu_redirect_cleanup_pending", None)
    if pending is not None and int(cycle) > int(pending["cycle"]):
        cleared_valids = s1_valid == 0 and s2_valid == 0
        evidence = {
            **pending,
            "check_cycle": int(cycle),
            "s0_prev_end_is_half_rvi": s0_half,
            "s1_prev_end_half_rvi_valid": s1_half_valid,
            "s1_prev_end_half_data": s1_half_data,
            "s1_prev_end_half_pc": s1_half_pc,
            "s1_prev_ibuf_enq_ptr": s1_ptr,
            "s1_valid": s1_valid,
            "s2_valid": s2_valid,
        }
        if pending["kind"] == "backend":
            pre_redirect = pending.get("pre_redirect") or {}
            if (
                pre_redirect.get("s0_half") == 1
                and cleared_valids
                and s0_half == 0
                and s1_half_valid == 0
                and s1_half_data == 0
                and s1_half_pc == 0
            ):
                for bin_id in ("BIN-920", "BIN-960"):
                    mark_owner_v3_checked(
                        recorder,
                        bin_id,
                        cycle,
                        evidence,
                        producer="ifu_redirect_cleanup_sampler",
                    )
            if pre_redirect.get("s1_ptr") not in {None, 0} and cleared_valids and s1_ptr == 0:
                mark_owner_v3_checked(
                    recorder,
                    "BIN-923",
                    cycle,
                    evidence,
                    producer="ifu_redirect_cleanup_sampler",
                )
        elif (
            pending["kind"] == "wb"
            and pending.get("pre_redirect", {}).get("pipeline_valid") is True
            and cleared_valids
        ):
            mark_owner_v3_checked(
                recorder,
                "BIN-950",
                cycle,
                evidence,
                producer="ifu_redirect_cleanup_sampler",
            )
        recorder._ifu_redirect_cleanup_pending = None

    if backend_redirect == 1 and wb_redirect == 1 and outbound_redirect == 0:
        evidence = {
            "backend_redirect": 1,
            "internal_wb_redirect": 1,
            "outbound_ifu_redirect": 0,
            "backend_won": True,
        }
        for bin_id in ("BIN-949", "BIN-995"):
            mark_owner_v3_checked(
                recorder,
                bin_id,
                cycle,
                evidence,
                producer="ifu_redirect_priority_sampler",
            )

    all_stages_flush = s0_flush == 1 and s1_flush == 1 and s2_flush == 1
    if backend_redirect == 1 and all_stages_flush:
        recorder._ifu_redirect_cleanup_pending = {
            "kind": "backend",
            "cycle": int(cycle),
            "flushes": [int(s0_flush), int(s1_flush), int(s2_flush)],
            "pre_redirect": previous,
        }
    elif wb_redirect == 1 and all_stages_flush:
        recorder._ifu_redirect_cleanup_pending = {
            "kind": "wb",
            "cycle": int(cycle),
            "flushes": [int(s0_flush), int(s1_flush), int(s2_flush)],
            "pre_redirect": previous,
        }
    recorder._ifu_redirect_last_state = {
        "s0_half": s0_half,
        "s1_half_valid": s1_half_valid,
        "s1_half_data": s1_half_data,
        "s1_half_pc": s1_half_pc,
        "s1_ptr": s1_ptr,
        "s1_valid": s1_valid,
        "s2_valid": s2_valid,
        "pipeline_valid": s1_valid == 1 or s2_valid == 1,
    }


def _sample_writeback(recorder, dut, cycle: int, pointer_redirect: bool) -> None:
    pending = list(getattr(recorder, "_ifu_wb_pending", ()))
    pending = [item for item in pending if int(cycle) - int(item["cycle"]) <= 4]
    if pointer_redirect:
        pending.clear()

    wb_valid = _read_ifu_internal(recorder, dut, "wbValid")
    wb_count = _read_ifu_internal(recorder, dut, "wbInstrCount")
    outbound_redirect = _read_ifu_internal(recorder, dut, "io_toFtq_wbRedirect_valid")
    checker_redirect = _read_predchecker(
        recorder, dut, "io_resp_stage2Out_checkerRedirect_valid"
    )
    if wb_valid == 1 and wb_count is not None and pending:
        match = next((item for item in pending if int(item["count"]) == int(wb_count)), None)
        if match is not None:
            evidence = {
                "event": "ifu_normal_writeback",
                "enqueue_cycle": int(match["cycle"]),
                "enqueue_count": int(match["count"]),
                "wb_instr_count": int(wb_count),
                "source_valids": match["source_valids"],
                "source_tags": match["source_tags"],
            }
            recorder.mark("ifu_writeback", "ordinary_no_redirect", cycle, evidence)
            recorder.mark("ifu_writeback", "instr_count_matches_enq", cycle, evidence)
            if outbound_redirect == 0 and checker_redirect == 0 and not pointer_redirect:
                mark_owner_v3_checked(
                    recorder,
                    "BIN-933",
                    cycle,
                    {
                        **evidence,
                        "ordinary_ibuffer_delivery": True,
                        "checker_redirect_valid": int(checker_redirect),
                        "uncache_redirect_valid": 0,
                        "to_ftq_wb_redirect_valid": int(outbound_redirect),
                    },
                    producer="ifu_normal_writeback_sampler",
                )
                mark_owner_v3_checked(
                    recorder,
                    "BIN-886",
                    cycle,
                    {
                        **evidence,
                        "ordinary_writeback_bookkeeping": True,
                        "checker_redirect_valid": int(checker_redirect),
                        "to_ftq_wb_redirect_valid": int(outbound_redirect),
                    },
                    producer="ifu_normal_writeback_sampler",
                )

            wb_tags = []
            for block in range(2):
                flag = _read_ifu_internal(recorder, dut, f"wbAlignFetchBlock_{block}_ftqIdx_flag")
                value = _read_ifu_internal(recorder, dut, f"wbAlignFetchBlock_{block}_ftqIdx_value")
                wb_tags.append(None if flag is None or value is None else (int(flag), int(value)))
            if (
                tuple(match["source_valids"]) == (1, 1)
                and len(match["source_tags"]) == 2
                and tuple(wb_tags) == tuple(match["source_tags"])
            ):
                recorder.mark(
                    "ifu_writeback",
                    "dual_fetch_sources_match",
                    cycle,
                    {**evidence, "wb_tags": wb_tags},
                )
            pending.remove(match)
    recorder._ifu_wb_pending = pending


def _sample_exception_metadata(recorder, dut, cycle: int) -> None:
    """Observe the IFU exception metadata contract at the IBuffer boundary.

    BIN-954 is intentionally sampled from the V3 output contract rather than
    inferred from instruction width/CFI classes.  GP address fields are only
    meaningful when the IFU asserts the corresponding write enable.
    """

    to_valid = _read_ifu_internal(recorder, dut, "io_toIBuffer_valid")
    to_ready = _read_ifu_internal(recorder, dut, "io_toIBuffer_ready")
    exception_type = _read_ifu_internal(
        recorder, dut, "io_toIBuffer_bits_exceptionType_value"
    )
    is_backend_exception = _read_ifu_internal(
        recorder, dut, "io_toIBuffer_bits_isBackendException"
    )
    has_satp_flush = _read_ifu_internal(
        recorder, dut, "io_toIBuffer_bits_hasSatpFlush"
    )
    exception_cross_page = _read_ifu_internal(
        recorder, dut, "io_toIBuffer_bits_exceptionCrossPage"
    )
    gp_wen = _read_gpaddr_output(recorder, dut, "wen")
    gp_waddr = _read_gpaddr_output(recorder, dut, "waddr")
    gpaddr = _read_gpaddr_output(recorder, dut, "wdata_gpaddr")
    is_for_vs_nonleaf_pte = _read_gpaddr_output(
        recorder, dut, "wdata_isForVSnonLeafPTE"
    )
    meta_backend_exception = _read_ifu_internal(
        recorder, dut, "s2_icacheMeta_0_isBackendException"
    )
    meta_satp_flush = _read_ifu_internal(
        recorder, dut, "s2_icacheMeta_0_hasSatpFlush"
    )
    meta_gpaddr = _read_ifu_internal(recorder, dut, "s2_icacheMeta_0_gpAddr_addr")
    meta_is_for_vs_nonleaf_pte = _read_ifu_internal(
        recorder, dut, "s2_icacheMeta_0_isForVSnonLeafPTE"
    )
    prev_end_is_half_rvi = _read_ifu_internal(
        recorder, dut, "s2_prevEndIsHalfRviInfo_valid"
    )
    ftq_idx = _read_ifu_internal(recorder, dut, "s2_fetchBlock_0_ftqIdx_value")
    if None in {
        to_valid,
        to_ready,
        exception_type,
        is_backend_exception,
        has_satp_flush,
        exception_cross_page,
        gp_wen,
        gp_waddr,
        gpaddr,
        is_for_vs_nonleaf_pte,
        meta_backend_exception,
        meta_satp_flush,
        meta_gpaddr,
        meta_is_for_vs_nonleaf_pte,
        prev_end_is_half_rvi,
        ftq_idx,
    }:
        return
    if int(to_valid) != 1 or int(to_ready) != 1 or int(exception_type) == 0:
        return
    checks = set(getattr(recorder, "_ifu_exception_metadata_checks", set()))
    observed_checks = set()
    if int(meta_backend_exception) == 1 and int(is_backend_exception) == 1:
        observed_checks.add("backend_exception")
    if int(meta_satp_flush) == 1 and int(has_satp_flush) == 1:
        observed_checks.add("satp_flush")
    if int(prev_end_is_half_rvi) == 1 and int(exception_cross_page) == 1:
        observed_checks.add("cross_page")
    gp_contract_matches = (
        int(gp_wen) == 1
        and int(gp_waddr) == int(ftq_idx)
        and int(gpaddr) == int(meta_gpaddr) << 1
        and int(is_for_vs_nonleaf_pte) == int(meta_is_for_vs_nonleaf_pte)
    )
    if gp_contract_matches:
        observed_checks.add("gpaddr")
        if int(is_for_vs_nonleaf_pte) == 1:
            observed_checks.add("vs_nonleaf_pte")
    if not observed_checks:
        return
    checks.update(observed_checks)
    recorder._ifu_exception_metadata_checks = checks
    evidence = {
        "event": "ifu_exception_metadata_contract",
        "exception_type": int(exception_type),
        "is_backend_exception": int(is_backend_exception),
        "has_satp_flush": int(has_satp_flush),
        "exception_cross_page": int(exception_cross_page),
        "gp_addr_mem_wen": int(gp_wen),
        "gp_addr_mem_waddr": int(gp_waddr),
        "gp_addr": int(gpaddr),
        "is_for_vs_nonleaf_pte": int(is_for_vs_nonleaf_pte),
        "meta_is_backend_exception": int(meta_backend_exception),
        "meta_has_satp_flush": int(meta_satp_flush),
        "meta_gp_addr": int(meta_gpaddr),
        "meta_is_for_vs_nonleaf_pte": int(meta_is_for_vs_nonleaf_pte),
        "prev_end_is_half_rvi": int(prev_end_is_half_rvi),
        "ftq_idx": int(ftq_idx),
        "observed_checks": sorted(observed_checks),
        "accumulated_checks": sorted(checks),
        "ibuffer_delivery": True,
    }
    required_checks = {
        "backend_exception",
        "satp_flush",
        "cross_page",
        "gpaddr",
        "vs_nonleaf_pte",
    }
    if required_checks <= checks:
        mark_owner_v3_checked(
            recorder,
            "BIN-954",
            cycle,
            evidence,
            producer="ifu_exception_metadata_sampler",
        )


def _record_invalid_taken_risk(
    recorder,
    cycle: int,
    risk: str,
    pending: dict[str, Any],
    **observations: Any,
) -> None:
    recorder.risk_observations.append(
        {
            "cycle": int(cycle),
            "risk": str(risk),
            **pending,
            **observations,
        }
    )
    recorder._ifu_invalid_taken_exception_pending = None


def _sample_invalid_taken_exception_cross(recorder, dut, cycle: int) -> None:
    pending = getattr(recorder, "_ifu_invalid_taken_exception_pending", None)
    if pending is not None and int(cycle) > int(pending["s1_cycle"]):
        if pending.get("phase") == "await_ibuffer_post":
            fire_cycle = int(pending["fire_cycle"])
            if int(cycle) > fire_cycle:
                pre_state = pending["ibuffer_pre_fire"]
                post_state = _read_ibuffer_state(recorder, dut)
                s1_state = pending["ibuffer_s1"]
                if not _ibuffer_state_complete(post_state):
                    _record_invalid_taken_risk(
                        recorder,
                        cycle,
                        "ifu_invalid_taken_exception_ibuffer_post_unavailable",
                        pending,
                        ibuffer_post=post_state,
                    )
                else:
                    expected_enq_pointer = _advance_ibuffer_pointer(
                        pre_state["enq_pointer"], 1
                    )
                    pre_distance = _ibuffer_pointer_distance(
                        pre_state["enq_pointer"], pre_state["deq_pointer"]
                    )
                    post_distance = _ibuffer_pointer_distance(
                        post_state["enq_pointer"], post_state["deq_pointer"]
                    )
                    old_head_distinct = (
                        int(pre_state["head_identity"][0]) << 1
                        != int(pending["output_pc"])
                        or tuple(pre_state["head_identity"][1:3])
                        != tuple(pending["output_ftq_identity"])
                    )
                    old_entry_preserved = (
                        int(s1_state["num_valid"]) > 0
                        and int(pre_state["num_valid"]) > 0
                        and int(s1_state["head_valid"]) == 1
                        and int(pre_state["head_valid"]) == 1
                        and int(post_state["head_valid"]) == 1
                        and s1_state["head_identity"] == pre_state["head_identity"]
                        and pre_state["head_identity"] == post_state["head_identity"]
                        and s1_state["deq_pointer"] == pre_state["deq_pointer"]
                        and pre_state["deq_pointer"] == post_state["deq_pointer"]
                        and old_head_distinct
                    )
                    pointer_update_correct = (
                        post_state["enq_pointer"] == expected_enq_pointer
                        and int(post_state["num_valid"])
                        == int(pre_state["num_valid"]) + 1
                        and pre_distance == int(pre_state["num_valid"])
                        and post_distance == int(post_state["num_valid"])
                    )
                    pressure_held = (
                        int(s1_state["backend_can_accept"]) == 0
                        and int(pre_state["backend_can_accept"]) == 0
                        and int(post_state["backend_can_accept"]) == 0
                    )
                    no_ibuffer_flush = (
                        int(s1_state["flush"]) == 0
                        and int(pre_state["flush"]) == 0
                        and int(post_state["flush"]) == 0
                    )
                    checkpoint_passed = (
                        bool(pending["s2_checkpoint_passed"])
                        and old_entry_preserved
                        and pointer_update_correct
                        and pressure_held
                        and no_ibuffer_flush
                    )
                    observations = {
                        **pending,
                        "event": "ifu_invalid_taken_fetch_exception_priority",
                        "ibuffer_post_cycle": int(cycle),
                        "ibuffer_post": post_state,
                        "expected_enq_pointer": expected_enq_pointer,
                        "pre_pointer_distance": int(pre_distance),
                        "post_pointer_distance": int(post_distance),
                        "old_head_distinct_from_exception": bool(old_head_distinct),
                        "old_unconsumed_entry_preserved": bool(old_entry_preserved),
                        "ibuffer_pointer_update_correct": bool(pointer_update_correct),
                        "backend_pressure_held": bool(pressure_held),
                        "no_ibuffer_flush": bool(no_ibuffer_flush),
                        "checkpoint_passed": bool(checkpoint_passed),
                    }
                    if checkpoint_passed:
                        recorder.mark(
                            "ifu_invalid_taken_exception",
                            "observed",
                            cycle,
                            observations,
                        )
                        recorder._ifu_invalid_taken_exception_pending = None
                    else:
                        _record_invalid_taken_risk(
                            recorder,
                            cycle,
                            "ifu_invalid_taken_exception_checkpoint_failed",
                            pending,
                            **observations,
                        )
        else:
            age = int(cycle) - int(pending["s1_cycle"])
            values = {
                "s2_valid": _read_ifu_internal(recorder, dut, "s2_valid_valid"),
                "s2_flush": _read_ifu_internal(recorder, dut, "s2_flush"),
                "s2_req_is_uncache": _read_ifu_internal(recorder, dut, "s2_reqIsUncache"),
                "s2_ftq_flag": _read_ifu_internal(
                    recorder, dut, "s2_fetchBlock_0_ftqIdx_flag"
                ),
                "s2_ftq_value": _read_ifu_internal(
                    recorder, dut, "s2_fetchBlock_0_ftqIdx_value"
                ),
                "s2_exception": _read_ifu_internal(
                    recorder, dut, "s2_icacheMeta_0_exception_value"
                ),
                "s2_instr_count": _read_ifu_internal(recorder, dut, "s2_instrCount"),
                "to_ibuffer_valid": _read_ifu_internal(
                    recorder, dut, "io_toIBuffer_valid"
                ),
                "to_ibuffer_ready": _read_ifu_internal(
                    recorder, dut, "io_toIBuffer_ready"
                ),
                "to_ibuffer_enq": _read_ifu_internal(
                    recorder, dut, "io_toIBuffer_bits_enqEnable"
                ),
                "to_ibuffer_valid_mask": _read_ifu_internal(
                    recorder, dut, "io_toIBuffer_bits_valid"
                ),
                "to_ibuffer_exception": _read_ifu_internal(
                    recorder, dut, "io_toIBuffer_bits_exceptionType_value"
                ),
            }
            exception_mask = _read_ifu_output_mask(recorder, dut, "exceptionMask")
            if values["s2_flush"] == 1:
                recorder._ifu_invalid_taken_exception_pending = None
            elif age > _INVALID_TAKEN_S2_TIMEOUT_CYCLES and not pending.get(
                "held_payload_signature"
            ):
                _record_invalid_taken_risk(
                    recorder,
                    cycle,
                    "ifu_invalid_taken_exception_s2_timeout",
                    pending,
                    s2_observation=values,
                )
            elif all(value is not None for value in values.values()) and exception_mask is not None:
                observed_identity = (
                    int(values["s2_ftq_flag"]),
                    int(values["s2_ftq_value"]),
                )
                same_s2_identity = (
                    int(values["s2_valid"]) == 1
                    and observed_identity == tuple(pending["ftq_identity"])
                )
                if int(values["s2_valid"]) == 1 and not same_s2_identity:
                    _record_invalid_taken_risk(
                        recorder,
                        cycle,
                        "ifu_invalid_taken_exception_s2_identity_mismatch",
                        pending,
                        observed_s2_identity=observed_identity,
                        s2_observation=values,
                    )
                elif same_s2_identity:
                    if int(values["to_ibuffer_valid"]) != 1:
                        if pending.get("held_payload_signature") is not None:
                            _record_invalid_taken_risk(
                                recorder,
                                cycle,
                                "ifu_invalid_taken_exception_payload_dropped_under_backpressure",
                                pending,
                                s2_observation=values,
                            )
                    else:
                        active_mask = int(values["to_ibuffer_enq"]) & int(
                            values["to_ibuffer_valid_mask"]
                        )
                        active_slots = _active_ifu_output_slots(
                            int(values["to_ibuffer_enq"]),
                            int(values["to_ibuffer_valid_mask"]),
                        )
                        active_slot = active_slots[0] if len(active_slots) == 1 else None
                        output_pc = (
                            None
                            if active_slot is None
                            else _decode_pruned_pc(
                                _read_ifu_output_slot(
                                    recorder, dut, "pc", active_slot, "_addr"
                                )
                            )
                        )
                        output_ftq = (
                            None
                            if active_slot is None
                            else (
                                _read_ifu_output_slot(
                                    recorder, dut, "ftqPtr", active_slot, "_flag"
                                ),
                                _read_ifu_output_slot(
                                    recorder, dut, "ftqPtr", active_slot, "_value"
                                ),
                            )
                        )
                        s2_checkpoint_passed = (
                            int(values["s2_req_is_uncache"]) == 0
                            and int(values["s2_exception"])
                            == int(pending["exception_type"])
                            and int(values["to_ibuffer_exception"])
                            == int(pending["exception_type"])
                            and int(values["s2_instr_count"]) == 1
                            and int(values["to_ibuffer_enq"]).bit_count() == 1
                            and int(values["to_ibuffer_valid_mask"]).bit_count() == 1
                            and active_mask.bit_count() == 1
                            and int(exception_mask) == active_mask
                            and output_pc == pending["expected_pc"]
                            and output_ftq == tuple(pending["ftq_identity"])
                        )
                        payload_signature = (
                            int(active_mask),
                            int(exception_mask),
                            output_pc,
                            output_ftq,
                            int(values["to_ibuffer_exception"]),
                            int(values["s2_instr_count"]),
                        )
                        held_signature = pending.get("held_payload_signature")
                        if held_signature is not None and payload_signature != tuple(
                            held_signature
                        ):
                            _record_invalid_taken_risk(
                                recorder,
                                cycle,
                                "ifu_invalid_taken_exception_payload_changed_under_backpressure",
                                pending,
                                current_payload_signature=payload_signature,
                                s2_observation=values,
                            )
                        elif not s2_checkpoint_passed:
                            _record_invalid_taken_risk(
                                recorder,
                                cycle,
                                "ifu_invalid_taken_exception_checkpoint_failed",
                                pending,
                                s2_observation=values,
                                active_slot=active_slot,
                                active_mask=int(active_mask),
                                exception_mask=int(exception_mask),
                                output_pc=output_pc,
                                output_ftq_identity=output_ftq,
                            )
                        elif int(values["to_ibuffer_ready"]) == 0:
                            pending["held_payload_signature"] = payload_signature
                            pending["held_cycles"] = int(pending.get("held_cycles", 0)) + 1
                            pending["last_held_cycle"] = int(cycle)
                            if int(pending["held_cycles"]) > _INVALID_TAKEN_HOLD_TIMEOUT_CYCLES:
                                _record_invalid_taken_risk(
                                    recorder,
                                    cycle,
                                    "ifu_invalid_taken_exception_backpressure_timeout",
                                    pending,
                                    s2_observation=values,
                                )
                        else:
                            pre_state = _read_ibuffer_state(recorder, dut)
                            if not _ibuffer_state_complete(pre_state):
                                _record_invalid_taken_risk(
                                    recorder,
                                    cycle,
                                    "ifu_invalid_taken_exception_ibuffer_pre_unavailable",
                                    pending,
                                    ibuffer_pre_fire=pre_state,
                                )
                            else:
                                pending.update(
                                    {
                                        "phase": "await_ibuffer_post",
                                        "fire_cycle": int(cycle),
                                        "s2_cycle": int(cycle),
                                        "s2_observation": values,
                                        "active_slot": active_slot,
                                        "active_mask": int(active_mask),
                                        "exception_mask": int(exception_mask),
                                        "output_pc": output_pc,
                                        "output_ftq_identity": output_ftq,
                                        "exception_won": True,
                                        "single_instruction_delivered": True,
                                        "younger_normal_instruction_delivered": False,
                                        "s2_checkpoint_passed": True,
                                        "ibuffer_pre_fire": pre_state,
                                    }
                                )

    s1_valid = _read_ifu_internal(recorder, dut, "s1_valid")
    s1_fire = _read_ifu_internal(recorder, dut, "s1_fire")
    invalid_taken = _read_ifu_internal(recorder, dut, "s1_invalidTaken_0")
    exception = _read_ifu_internal(recorder, dut, "s1_icacheMeta_0_exception_value")
    instr_count = _read_ifu_internal(recorder, dut, "s1_instrCount")
    s1_flush = _read_ifu_internal(recorder, dut, "s1_flush")
    ftq_flag = _read_ifu_internal(recorder, dut, "s1_fetchBlock_0_ftqIdx_flag")
    ftq_value = _read_ifu_internal(recorder, dut, "s1_fetchBlock_0_ftqIdx_value")
    start_pc = _read_ifu_internal(recorder, dut, "s1_fetchBlock_0_startVAddr_addr")
    prev_half_valid = _read_ifu_internal(
        recorder, dut, "s1_prevEndHalfRviInfo_valid"
    )
    prev_half_pc = _read_ifu_internal(
        recorder, dut, "s1_prevEndHalfRviInfo_bits_pc_addr"
    )
    if None in {
        s1_valid,
        s1_fire,
        invalid_taken,
        exception,
        instr_count,
        s1_flush,
        ftq_flag,
        ftq_value,
        start_pc,
        prev_half_valid,
        prev_half_pc,
    }:
        return
    if (
        getattr(recorder, "_ifu_invalid_taken_exception_pending", None) is None
        and int(s1_valid) == 1
        and int(s1_fire) == 1
        and int(invalid_taken) == 1
        and int(exception) in _FETCH_EXCEPTION_VALUES
        and int(s1_flush) == 0
    ):
        evidence = {
            "event": "ifu_s1_invalid_taken_with_fetch_exception",
            "s1_cycle": int(cycle),
            "exception_type": int(exception),
            "s1_instr_count": int(instr_count),
            "ftq_identity": (int(ftq_flag), int(ftq_value)),
            "expected_pc": (
                int(prev_half_pc) if int(prev_half_valid) else int(start_pc)
            )
            << 1,
            "previous_half_rvi": bool(prev_half_valid),
            "invalid_taken_observed": True,
            "fetch_exception_observed": True,
        }
        if int(instr_count) == 1:
            ibuffer_s1 = _read_ibuffer_state(recorder, dut)
            evidence.update(
                {
                    "phase": "await_s2",
                    "held_cycles": 0,
                    "ibuffer_s1": ibuffer_s1,
                }
            )
            s1_pointer_consistent = (
                _ibuffer_state_complete(ibuffer_s1)
                and _ibuffer_pointer_distance(
                    ibuffer_s1["enq_pointer"], ibuffer_s1["deq_pointer"]
                )
                == int(ibuffer_s1["num_valid"])
            )
            old_unconsumed_entry_present = (
                _ibuffer_state_complete(ibuffer_s1)
                and int(ibuffer_s1["num_valid"]) > 0
                and int(ibuffer_s1["head_valid"]) == 1
                and int(ibuffer_s1["backend_can_accept"]) == 0
                and int(ibuffer_s1["flush"]) == 0
                and s1_pointer_consistent
            )
            if (
                getattr(recorder, "_ifu_invalid_taken_exception_pending", None)
                is None
                and old_unconsumed_entry_present
            ):
                recorder._ifu_invalid_taken_exception_pending = evidence
            elif not old_unconsumed_entry_present:
                _record_invalid_taken_risk(
                    recorder,
                    cycle,
                    "ifu_invalid_taken_exception_missing_old_ibuffer_entry",
                    evidence,
                    s1_pointer_consistent=bool(s1_pointer_consistent),
                )
        else:
            recorder.risk_observations.append(
                {
                    "cycle": int(cycle),
                    "risk": "ifu_invalid_taken_exception_not_truncated_in_s1",
                    **evidence,
                }
            )


def _sample_instr_boundary_tail(recorder, dut, cycle: int) -> None:
    s1_valid = _read_ifu_internal(recorder, dut, "s1_valid")
    s1_fire = _read_ifu_internal(recorder, dut, "s1_fire")
    s1_flush = _read_ifu_internal(recorder, dut, "s1_flush")
    s1_req_is_uncache = _read_ifu_internal(recorder, dut, "s1_reqIsUncache")
    total_end_is_half_rvi = _read_ifu_internal(
        recorder, dut, "s1_totalEndIsHalfRvi"
    )
    total_end_pc = _read_ifu_internal(
        recorder, dut, "s1_totalEndHalfRvi_bits_pc_addr"
    )
    total_end_data = _read_ifu_internal(
        recorder, dut, "s1_totalEndHalfRvi_bits_data"
    )
    if None in {s1_valid, s1_fire, s1_flush, s1_req_is_uncache, total_end_is_half_rvi}:
        return
    if (
        int(s1_valid) == 1
        and int(s1_fire) == 1
        and int(s1_flush) == 0
        and int(s1_req_is_uncache) == 0
        and int(total_end_is_half_rvi) == 1
    ):
        recorder.mark(
            "ifu_instr_boundary_half",
            "tail_half_detected",
            cycle,
            {
                "event": "ifu_s1_boundary_tail_half_rvi",
                "s1_total_end_half_pc": total_end_pc,
                "s1_total_end_half_data": total_end_data,
                "s1_total_end_is_half_rvi": 1,
            },
        )
        # Current RTL represents the cross-fetch-block RVI case either as a
        # stitched output or as an explicit saved half-RVI state.  The latter
        # is the observable implementation when the first block ends before
        # the next block is available, and is the same functional leaf's
        # accepted "or enter half state" outcome.
        recorder.mark(
            "ifu_data_slice",
            "rvi_crosses_fetch_blocks",
            cycle,
            {
                "event": "ifu_rvi_crosses_fetch_blocks_half_state",
                "s1_total_end_half_pc": total_end_pc,
                "s1_total_end_half_data": total_end_data,
                "s1_total_end_is_half_rvi": 1,
                "boundary_state": "saved_half_rvi",
                "mapping_note": "current_rtl_uses_half_state_when_second_block_is_not_yet_available",
            },
        )
        recorder.mark(
            "ifu_instr_boundary_v3",
            "tail_half_state",
            cycle,
            {
                "event": "ifu_s0_v3_tail_half_state",
                "s1_total_end_half_pc": total_end_pc,
                "s1_total_end_half_data": total_end_data,
                "s1_total_end_is_half_rvi": 1,
            },
        )


def _sample_pred_taken_index_mapping(recorder, dut, cycle: int) -> None:
    s1_valid = _read_ifu_internal(recorder, dut, "s1_valid")
    raw_valid = _read_ifu_internal(recorder, dut, "s1_rawInstrValid")
    total_range = _read_ifu_internal(recorder, dut, "s1_totalRange")
    first_range = _read_ifu_internal(recorder, dut, "s1_firstRange")
    merged_taken_mask = _read_ifu_internal(recorder, dut, "s1_mergedPredTakenMask")
    if None in {s1_valid, raw_valid, total_range, first_range, merged_taken_mask} or s1_valid != 1:
        return

    block_valid = [
        _read_ifu_internal(recorder, dut, f"s1_fetchBlock_{block}_valid")
        for block in range(2)
    ]
    taken_valid = [
        _read_ifu_internal(
            recorder, dut, f"s1_fetchBlock_{block}_takenCfiOffset_valid"
        )
        for block in range(2)
    ]
    taken_bits = [
        _read_ifu_internal(
            recorder, dut, f"s1_fetchBlock_{block}_takenCfiOffset_bits"
        )
        for block in range(2)
    ]
    if None in {*block_valid, *taken_valid}:
        return

    total_raw_valid = int(raw_valid) & int(total_range)
    first_range = int(first_range)
    merged_taken_mask = int(merged_taken_mask)
    first_count = (total_raw_valid & first_range).bit_count()
    total_count = total_raw_valid.bit_count()
    first_end_half = _read_ifu_internal(recorder, dut, "s1_firstEndIsHalfRvi")
    total_end_half = _read_ifu_internal(recorder, dut, "s1_totalEndIsHalfRvi")
    common = {
        "s1_total_raw_instr_valid": total_raw_valid,
        "s1_first_range": first_range,
        "s1_merged_pred_taken_mask": merged_taken_mask,
        "first_instr_count": first_count,
        "total_instr_count": total_count,
        "block_valid": block_valid,
        "taken_valid": taken_valid,
    }
    if (
        block_valid[0] == 1
        and taken_valid[0] == 1
        and taken_valid[1] == 0
        and first_end_half == 0
        and first_count > 0
        and merged_taken_mask == 1 << (first_count - 1)
    ):
        mapping_evidence = {
            **common,
            "selected_block": 0,
            "expected_compacted_index": first_count - 1,
        }
        mark_owner_v3_checked(
            recorder,
            "BIN-916",
            cycle,
            mapping_evidence,
            producer="ifu_pred_taken_index_sampler",
        )
        mark_owner_v3_checked(
            recorder,
            "BIN-915",
            cycle,
            {**mapping_evidence, "end_boundary_checked": True},
            producer="ifu_pred_taken_index_sampler",
        )
        # If the first block has a taken CFI, its offset is the RTL source of
        # s1_firstEndPos; the private alias itself is not emitted by Verilator.
        first_end_pos = taken_bits[0]
        if first_end_pos is not None and 0 <= int(first_end_pos) <= 15:
            mark_owner_v3_checked(
                recorder,
                "BIN-914",
                cycle,
                {
                    **mapping_evidence,
                    "first_end_pos": int(first_end_pos),
                    "first_range_checked": True,
                },
                producer="ifu_pred_taken_index_sampler",
            )
    if (
        block_valid[1] == 1
        and taken_valid[0] == 0
        and taken_valid[1] == 1
        and total_end_half == 0
        and total_count > first_count > 0
        and merged_taken_mask == 1 << (total_count - 1)
    ):
        mapping_evidence = {
            **common,
            "selected_block": 1,
            "expected_compacted_index": total_count - 1,
            "first_block_contribution": first_count,
        }
        mark_owner_v3_checked(
            recorder,
            "BIN-918",
            cycle,
            mapping_evidence,
            producer="ifu_pred_taken_index_sampler",
        )
        mark_owner_v3_checked(
            recorder,
            "BIN-915",
            cycle,
            {**mapping_evidence, "end_boundary_checked": True},
            producer="ifu_pred_taken_index_sampler",
        )


def _sample_predchecker_v3(recorder, dut, cycle: int) -> None:
    if not hasattr(recorder, "_ifu_predchecker_v3_target_kinds"):
        recorder._ifu_predchecker_v3_target_kinds = set()
    pending = getattr(recorder, "_ifu_predchecker_v3_pending", None)
    training_pending = getattr(recorder, "_ifu_not_cfi_training_pending", None)
    if training_pending is not None and int(cycle) > int(training_pending["cycle"]):
        resolve_valid = _read_ftq_first(recorder, dut, "ifuResolve_valid")
        resolve_ftq_idx = _read_ftq_first(
            recorder, dut, "ifuResolve_bits_ftqIdx_value"
        )
        if (
            resolve_valid == 1
            and resolve_ftq_idx is not None
            and int(resolve_ftq_idx) == int(training_pending["ftq_idx"])
        ):
            mark_owner_v3_checked(
                recorder,
                "BIN-934",
                cycle,
                {
                    "event": "not_cfi_taken_to_ftq_resolve",
                    "redirect_cycle": int(training_pending["cycle"]),
                    "can_train": int(training_pending["can_train"]),
                    "ifu_resolve_valid": int(resolve_valid),
                    "ifu_resolve_ftq_idx": int(resolve_ftq_idx),
                    "not_cfi_taken": True,
                },
                producer="ifu_predchecker_ftq_training_sampler",
            )
            training_pending = None
            recorder._ifu_not_cfi_training_pending = None
        elif int(cycle) - int(training_pending["cycle"]) > 2:
            recorder._ifu_not_cfi_training_pending = None
    redirect_valid = _read_predchecker(
        recorder, dut, "io_resp_stage2Out_checkerRedirect_valid"
    )
    registered_invalid_taken = _read_predchecker(recorder, dut, "invalidTakenNext")
    if redirect_valid == 1 and registered_invalid_taken == 1:
        recorder.mark(
            "ifu_predchecker_v3_fault",
            "invalid_taken",
            cycle,
            {
                "event": "ifu_predchecker_v3_registered_invalid_taken",
                "checker_redirect_valid": 1,
                "invalid_taken_next": 1,
            },
        )
    if (
        pending is not None
        and pending["fault"] is None
        and int(cycle) > int(pending["cycle"])
    ):
        pending_entries = pending.get("entries", [])
        canonical_no_fault = bool(pending_entries) and all(
            entry["pred_taken"] == 0 and entry["branch_type"] in {0, 1}
            for entry in pending_entries
        )
        if redirect_valid == 0 and canonical_no_fault:
            recorder.mark(
                "ifu_predchecker_v3_fault",
                "no_remask_fault",
                cycle,
                {
                    "event": "ifu_predchecker_v3_no_fault",
                    "request_cycle": int(pending["cycle"]),
                    "entries": pending["entries"],
                },
            )
        recorder._ifu_predchecker_v3_pending = None
        pending = None
    if pending is not None and redirect_valid == 1:
        target = _read_predchecker(
            recorder, dut, "io_resp_stage2Out_checkerRedirect_bits_target_addr"
        )
        taken = _read_predchecker(
            recorder, dut, "io_resp_stage2Out_checkerRedirect_bits_taken"
        )
        invalid_taken = _read_predchecker(
            recorder, dut, "io_resp_stage2Out_checkerRedirect_bits_invalidTaken"
        )
        is_rvc = _read_predchecker(
            recorder, dut, "io_resp_stage2Out_checkerRedirect_bits_isRVC"
        )
        block_sel = _read_predchecker(
            recorder, dut, "io_resp_stage2Out_checkerRedirect_bits_blockSel"
        )
        is_cross_block_instr = _read_predchecker(
            recorder,
            dut,
            "io_resp_stage2Out_checkerRedirect_bits_isCrossBlockInstr",
        )
        select_block = (
            None
            if block_sel is None or is_cross_block_instr is None
            else int(block_sel) | int(is_cross_block_instr)
        )
        branch_type = _read_predchecker(
            recorder,
            dut,
            "io_resp_stage2Out_checkerRedirect_bits_attribute_branchType",
        )
        ras_action = _read_predchecker(
            recorder,
            dut,
            "io_resp_stage2Out_checkerRedirect_bits_attribute_rasAction",
        )
        end_offset = _read_predchecker(
            recorder, dut, "io_resp_stage2Out_checkerRedirect_bits_endOffset"
        )
        observed = {
            "target": target,
            "taken": taken,
            "invalid_taken": invalid_taken,
            "is_rvc": is_rvc,
            "block_sel": block_sel,
            "is_cross_block_instr": is_cross_block_instr,
            "select_block": select_block,
            "branch_type": branch_type,
            "ras_action": ras_action,
            "end_offset": end_offset,
        }
        evidence = {
            "event": "ifu_predchecker_v3_redirect",
            "request_cycle": int(pending["cycle"]),
            "fault": pending["fault"],
            "slot": int(pending["slot"]),
            "expected": pending,
            "observed": observed,
        }
        expected_target = (
            int(pending["pc_addr"]) + int(pending["jump_offset_addr"])
            if pending["fault"] == "jal_not_taken"
            else int(pending["pc_addr"])
            + (1 if pending["is_rvc"] or pending["invalid_taken"] else 2)
        )
        target_matches = target == expected_target
        if target_matches:
            target_kind = (
                "direct_jump" if pending["fault"] == "jal_not_taken" else "sequential"
            )
            recorder._ifu_predchecker_v3_target_kinds.add(target_kind)
            if recorder._ifu_predchecker_v3_target_kinds == {"direct_jump", "sequential"}:
                recorder.mark(
                    "ifu_predchecker_v3_redirect",
                    "target_by_fault_kind",
                    cycle,
                    evidence,
                )
        expected_branch_type = 0 if pending["invalid_taken"] else pending["branch_type"]
        expected_ras_action = 0 if pending["invalid_taken"] else pending["ras_action"]
        metadata_matches = (
            invalid_taken == pending["invalid_taken"]
            and is_rvc == pending["is_rvc"]
            and block_sel == pending["block_sel"]
            and is_cross_block_instr == pending["is_cross_block_instr"]
            and select_block == pending["select_block"]
            and branch_type == expected_branch_type
            and ras_action == expected_ras_action
            and end_offset == pending["end_offset"]
        )
        if metadata_matches:
            recorder.mark(
                "ifu_predchecker_v3_redirect",
                "metadata_matches_earliest_fault",
                cycle,
                evidence,
            )
        if target_matches and metadata_matches:
            checked_redirect = {
                "fault": pending["fault"],
                "slot": int(pending["slot"]),
                "expected_target": int(expected_target),
                "observed": observed,
            }
            for owner_bin_id in ("BIN-941", "BIN-945"):
                mark_owner_v3_checked(
                    recorder,
                    owner_bin_id,
                    cycle,
                    checked_redirect,
                    producer="ifu_predchecker_v3_sampler",
                )
            if select_block == 0:
                mark_owner_v3_checked(
                    recorder,
                    "BIN-946",
                    cycle,
                    checked_redirect,
                    producer="ifu_predchecker_v3_sampler",
                )
            elif select_block == 1:
                mark_owner_v3_checked(
                    recorder,
                    "BIN-947",
                    cycle,
                    checked_redirect,
                    producer="ifu_predchecker_v3_sampler",
                )
            redirect_half_valid = _read_ifu_internal(
                recorder, dut, "s2_prevEndIsHalfRviInfo_valid"
            )
            if pending["invalid_taken"] == 1 and redirect_half_valid == 1:
                mark_owner_v3_checked(
                    recorder,
                    "BIN-948",
                    cycle,
                    {
                        **checked_redirect,
                        "redirect_invalid_taken": True,
                        "half_state_valid": True,
                        "s2_prev_end_is_half_rvi": int(redirect_half_valid),
                    },
                    producer="ifu_predchecker_v3_sampler",
                )
        if (
            pending["fault"] == "not_cfi_taken"
            and target_matches
            and metadata_matches
        ):
            wb_redirect_valid = _read_ifu_internal(
                recorder, dut, "io_toFtq_wbRedirect_valid"
            )
            can_train = _read_ifu_internal(
                recorder, dut, "io_toFtq_wbRedirect_bits_canTrain"
            )
            ftq_idx = _read_ifu_internal(
                recorder, dut, "io_toFtq_wbRedirect_bits_ftqIdx_value"
            )
            if wb_redirect_valid == 1 and can_train == 1 and ftq_idx is not None:
                recorder._ifu_not_cfi_training_pending = {
                    "cycle": int(cycle),
                    "ftq_idx": int(ftq_idx),
                    "can_train": int(can_train),
                }
        recorder._ifu_predchecker_v3_pending = None
    elif pending is not None and int(cycle) - int(pending["cycle"]) > 2:
        recorder._ifu_predchecker_v3_pending = None

    req_valid = _read_predchecker_or_ifu(
        recorder, dut, "io_req_valid", "s2_valid_valid"
    )
    if req_valid != 1:
        return

    entries = []
    for slot in range(_IFU_OUTPUT_SLOT_COUNT):
        prefix = f"io_req_bits_instrVec_{slot}_"
        valid = _read_predchecker_or_ifu(
            recorder, dut, prefix + "valid", f"s2_alignedInstrVec_{slot}_valid"
        )
        if valid != 1:
            continue
        pred_taken = _read_predchecker_or_ifu(
            recorder,
            dut,
            prefix + "isPredTaken",
            f"s2_alignedInstrVec_{slot}_isPredTaken",
        )
        invalid_taken = _read_predchecker_or_ifu(
            recorder,
            dut,
            prefix + "invalidTaken",
            f"s2_alignedInstrVec_{slot}_invalidTaken",
        )
        is_rvc = _read_predchecker_or_ifu(
            recorder, dut, prefix + "isRvc", f"s2_alignedInstrVec_{slot}_isRvc"
        )
        block_sel = _read_predchecker_or_ifu(
            recorder, dut, prefix + "blockSel", f"s2_alignedInstrVec_{slot}_blockSel"
        )
        is_cross_block_instr = _read_predchecker_or_ifu(
            recorder,
            dut,
            prefix + "isCrossBlockInstr",
            f"s2_alignedInstrVec_{slot}_isCrossBlockInstr",
        )
        select_block = (
            None
            if block_sel is None or is_cross_block_instr is None
            else int(block_sel) | int(is_cross_block_instr)
        )
        end_offset = _read_predchecker_or_ifu(
            recorder, dut, prefix + "endOffset", f"s2_alignedInstrVec_{slot}_endOffset"
        )
        branch_type = _read_predchecker_or_ifu(
            recorder,
            dut,
            f"io_req_bits_pdInfoVec_{slot}_brAttribute_branchType",
            f"s2_alignedPdInfoVec_{slot}_brAttribute_branchType",
        )
        ras_action = _read_predchecker(
            recorder, dut, f"io_req_bits_pdInfoVec_{slot}_brAttribute_rasAction"
        )
        if ras_action is None:
            ras_action = _read_ifu_internal(
                recorder,
                dut,
                f"s2_alignedPdInfoVec_{slot}_brAttribute_rasAction",
            )
        pc_addr = _read_predchecker_or_ifu(
            recorder,
            dut,
            f"io_req_bits_instrPcVec_{slot}_addr",
            f"s2_alignedInstrPcVec_{slot}_addr",
        )
        jump_offset_addr = _read_predchecker_or_ifu(
            recorder,
            dut,
            f"io_req_bits_jumpOffsetVec_{slot}_addr",
            f"s2_alignedJumpOffsetVec_{slot}_addr",
        )
        fixed_valid = _read_predchecker(
            recorder, dut, f"io_resp_stage1Out_fixedInstrValid_{slot}"
        )
        if fixed_valid is None:
            fixed_mask = _read_ifu_internal(recorder, dut, "s2_fixedInstrValid")
            if fixed_mask is not None:
                fixed_valid = (int(fixed_mask) >> slot) & 1
        if None in {
            pred_taken,
            invalid_taken,
            is_rvc,
            block_sel,
            is_cross_block_instr,
            select_block,
            end_offset,
            branch_type,
            ras_action,
            pc_addr,
            jump_offset_addr,
            fixed_valid,
        }:
            continue
        fault = None
        if int(branch_type) == 2 and int(pred_taken) == 0:
            fault = "jal_not_taken"
        elif int(branch_type) == 3 and not (int(ras_action) & 1) and int(pred_taken) == 0:
            fault = "jalr_not_taken"
        elif int(ras_action) & 1 and int(pred_taken) == 0:
            fault = "ret_not_taken"
        elif int(branch_type) == 0 and int(pred_taken) == 1:
            fault = "not_cfi_taken"
        elif int(invalid_taken) == 1:
            fault = "invalid_taken"
        entries.append(
            {
                "slot": int(slot),
                "fault": fault,
                "pred_taken": int(pred_taken),
                "invalid_taken": int(invalid_taken),
                "is_rvc": int(is_rvc),
                "block_sel": int(block_sel),
                "is_cross_block_instr": int(is_cross_block_instr),
                "select_block": int(select_block),
                "end_offset": int(end_offset),
                "branch_type": int(branch_type),
                "ras_action": int(ras_action),
                "pc_addr": int(pc_addr),
                "jump_offset_addr": int(jump_offset_addr),
                "fixed_valid": int(fixed_valid),
            }
        )

    faults = [entry for entry in entries if entry["fault"] is not None]
    if not faults:
        not_taken_no_ending_cfi = bool(entries) and all(
            entry["pred_taken"] == 0 and entry["branch_type"] in {0, 1}
            for entry in entries
        )
        if not_taken_no_ending_cfi:
            mark_owner_v3_checked(
                recorder,
                "BIN-930",
                cycle,
                {"entries": entries, "checker_faults": []},
                producer="ifu_predchecker_v3_sampler",
            )

        taken_entries = [entry for entry in entries if entry["pred_taken"] == 1]
        if any(entry["branch_type"] == 1 for entry in taken_entries):
            mark_owner_v3_checked(
                recorder,
                "BIN-931",
                cycle,
                {"entries": entries, "matched_type": "branch"},
                producer="ifu_predchecker_v3_sampler",
            )

        correct_cfi_kinds = getattr(recorder, "_ifu_owner_correct_cfi_kinds", set())
        correct_jalr_forms = getattr(recorder, "_ifu_owner_correct_jalr_forms", set())
        for entry in taken_entries:
            if entry["branch_type"] == 2:
                correct_cfi_kinds.add("jal")
            elif entry["branch_type"] == 3 and entry["ras_action"] & 1:
                correct_cfi_kinds.add("ret")
                correct_jalr_forms.add(("ret", int(entry["is_rvc"])))
            elif entry["branch_type"] == 3 and entry["ras_action"] & 2:
                correct_cfi_kinds.add("call")
                correct_jalr_forms.add(("call", int(entry["is_rvc"])))
            elif entry["branch_type"] == 3:
                correct_cfi_kinds.add("jalr")
                correct_jalr_forms.add(("jalr", int(entry["is_rvc"])))
        recorder._ifu_owner_correct_cfi_kinds = correct_cfi_kinds
        recorder._ifu_owner_correct_jalr_forms = correct_jalr_forms
        if {"jal", "jalr", "ret"}.issubset(correct_cfi_kinds):
            mark_owner_v3_checked(
                recorder,
                "BIN-932",
                cycle,
                {"observed_correct_cfi_kinds": sorted(correct_cfi_kinds)},
                producer="ifu_predchecker_v3_sampler",
            )
        if {kind for kind, _width in correct_jalr_forms} == {"jalr", "call", "ret"}:
            mark_owner_v3_checked(
                recorder,
                "BIN-973",
                cycle,
                {
                    "observed_correct_jalr_forms": sorted(
                        [kind, width] for kind, width in correct_jalr_forms
                    )
                },
                producer="ifu_predchecker_v3_sampler",
            )

        taken_offsets = getattr(recorder, "_ifu_owner_taken_cfi_offsets", set())
        taken_offsets.update(
            int(entry["end_offset"])
            for entry in taken_entries
            if 0 <= int(entry["end_offset"]) < 16
        )
        recorder._ifu_owner_taken_cfi_offsets = taken_offsets
        if taken_offsets == set(range(16)):
            mark_owner_v3_checked(
                recorder,
                "BIN-974",
                cycle,
                {"observed_taken_cfi_offsets": sorted(taken_offsets)},
                producer="ifu_predchecker_v3_sampler",
            )

        boundary_widths = getattr(recorder, "_ifu_owner_taken_cfi_widths", {})
        for entry in taken_entries:
            kind = {1: "branch", 2: "jal", 3: "jalr"}.get(entry["branch_type"])
            if kind is not None:
                boundary_widths.setdefault(kind, set()).add(int(entry["is_rvc"]))
        recorder._ifu_owner_taken_cfi_widths = boundary_widths
        for kind, bin_id in (("branch", "BIN-971"), ("jal", "BIN-972")):
            observed_widths = boundary_widths.get(kind, set())
            if observed_widths:
                mark_owner_v3_checked(
                    recorder,
                    bin_id,
                    cycle,
                    {
                        "cfi_kind": kind,
                        "observed_is_rvc": sorted(observed_widths),
                        "mapping_note": (
                            "the leaf covers a correct taken CFI at either legal "
                            "instruction width; width accumulation remains in evidence"
                        ),
                    },
                    producer="ifu_predchecker_v3_sampler",
                )

        recorder._ifu_predchecker_v3_pending = {
            "fault": None,
            "cycle": int(cycle),
            "entries": entries,
        }
        return

    first = faults[0]
    fault_evidence = {
        "event": "ifu_predchecker_v3_fault",
        "faults": faults,
        "entries": entries,
    }
    canonical_fault = first["fault"]
    if canonical_fault == "not_cfi_taken":
        canonical_fault = (
            canonical_fault
            if first["is_rvc"] == 0 and first["end_offset"] <= 14
            else None
        )
    if canonical_fault is not None:
        recorder.mark(
            "ifu_predchecker_v3_fault",
            canonical_fault,
            cycle,
            fault_evidence,
        )

    # BIN-934 is reserved for the complete notCfiTaken -> canTrain -> FTQ
    # resolve chain, sampled above.  Seeing the checker fault alone is not a
    # valid FTQ resolve observation.
    if (
        first["fault"] == "invalid_taken"
        and first["branch_type"] == 1
        and first["end_offset"] == 15
    ):
        for owner_bin_id in ("BIN-917", "BIN-935", "BIN-988", "BIN-991", "BIN-992"):
            mark_owner_v3_checked(
                recorder,
                owner_bin_id,
                cycle,
                fault_evidence,
                producer="ifu_predchecker_v3_sampler",
            )
        half_state_valid = _read_ifu_internal(
            recorder, dut, "s2_prevEndIsHalfRviInfo_valid"
        )
        if half_state_valid == 1:
            mark_owner_v3_checked(
                recorder,
                "BIN-940",
                cycle,
                {
                    **fault_evidence,
                    "invalid_taken": True,
                    "rvi_end_offset": 15,
                    "half_state_valid": True,
                    "s2_prev_end_is_half_rvi": int(half_state_valid),
                },
                producer="ifu_predchecker_v3_sampler",
            )

    fault_kinds = getattr(recorder, "_ifu_owner_fault_kinds", set())
    for entry in faults:
        if entry["fault"] == "jal_not_taken":
            fault_kinds.add("call" if entry["ras_action"] & 2 else "jal")
        elif entry["fault"] == "jalr_not_taken":
            fault_kinds.add("jalr")
        elif entry["fault"] == "ret_not_taken":
            fault_kinds.add("ret")
    recorder._ifu_owner_fault_kinds = fault_kinds
    if {"jal", "jalr", "call", "ret"}.issubset(fault_kinds):
        mark_owner_v3_checked(
            recorder,
            "BIN-936",
            cycle,
            {"observed_fault_kinds": sorted(fault_kinds)},
            producer="ifu_predchecker_v3_sampler",
        )

    first_is_cross_block_rvi = first["is_rvc"] == 0 and (
        first["is_cross_block_instr"] == 1 or first["end_offset"] == 16
    )
    if first["fault"] == "jal_not_taken" and first["is_rvc"] == 1:
        mark_owner_v3_checked(recorder, "BIN-975", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    if first["fault"] == "jal_not_taken" and first_is_cross_block_rvi:
        mark_owner_v3_checked(recorder, "BIN-976", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    if first["fault"] == "jalr_not_taken" and first["is_rvc"] == 1:
        mark_owner_v3_checked(recorder, "BIN-980", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    if first["fault"] == "jalr_not_taken" and first_is_cross_block_rvi:
        mark_owner_v3_checked(recorder, "BIN-982", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    if first["fault"] == "not_cfi_taken" and first_is_cross_block_rvi:
        mark_owner_v3_checked(recorder, "BIN-985", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    if first["fault"] == "not_cfi_taken" and first["is_rvc"] == 1:
        mark_owner_v3_checked(recorder, "BIN-986", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    if first["fault"] == "not_cfi_taken" and first["invalid_taken"] == 1:
        mark_owner_v3_checked(recorder, "BIN-987", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")

    younger_faults = [entry for entry in faults if entry["slot"] > first["slot"]]
    younger_taken_cfi = [
        entry
        for entry in entries
        if entry["slot"] > first["slot"]
        and entry["pred_taken"] == 1
        and entry["branch_type"] in {1, 2, 3}
    ]
    if first["fault"] == "jal_not_taken" and younger_taken_cfi:
        mark_owner_v3_checked(
            recorder,
            "BIN-977",
            cycle,
            fault_evidence,
            producer="ifu_predchecker_v3_sampler",
        )
        if any(
            entry["is_rvc"] == 0
            and (
                entry["is_cross_block_instr"] == 1
                or entry["end_offset"] == 16
            )
            for entry in younger_taken_cfi
        ):
            mark_owner_v3_checked(
                recorder,
                "BIN-978",
                cycle,
                fault_evidence,
                producer="ifu_predchecker_v3_sampler",
            )
    if first["fault"] == "jal_not_taken" and first["invalid_taken"] == 1:
        mark_owner_v3_checked(
            recorder,
            "BIN-979",
            cycle,
            fault_evidence,
            producer="ifu_predchecker_v3_sampler",
        )
    if first["fault"] == "jalr_not_taken" and any(
        entry["fault"] in {"jal_not_taken", "jalr_not_taken"}
        for entry in younger_faults
    ):
        mark_owner_v3_checked(
            recorder,
            "BIN-981",
            cycle,
            fault_evidence,
            producer="ifu_predchecker_v3_sampler",
        )
    if first["fault"] == "jalr_not_taken" and younger_taken_cfi:
        mark_owner_v3_checked(
            recorder,
            "BIN-983",
            cycle,
            fault_evidence,
            producer="ifu_predchecker_v3_sampler",
        )
    if first["fault"] == "jalr_not_taken" and first["invalid_taken"] == 1:
        mark_owner_v3_checked(
            recorder,
            "BIN-984",
            cycle,
            fault_evidence,
            producer="ifu_predchecker_v3_sampler",
        )
    if first["fault"] == "jal_not_taken" and any(entry["fault"] == "invalid_taken" for entry in younger_faults):
        mark_owner_v3_checked(recorder, "BIN-989", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    if first["fault"] == "jalr_not_taken" and any(entry["fault"] == "invalid_taken" for entry in younger_faults):
        mark_owner_v3_checked(recorder, "BIN-990", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    if first["fault"] in {"jal_not_taken", "jalr_not_taken"} and any(
        entry["fault"] == "not_cfi_taken" for entry in younger_faults
    ):
        mark_owner_v3_checked(recorder, "BIN-993", cycle, fault_evidence, producer="ifu_predchecker_v3_sampler")
    younger = [entry for entry in entries if entry["slot"] > first["slot"]]
    older = [entry for entry in entries if entry["slot"] < first["slot"]]
    fixed_range_checked = (
        bool(older)
        and bool(younger)
        and first["fixed_valid"] == 1
        and all(entry["fixed_valid"] == 1 for entry in older)
        and all(entry["fixed_valid"] == 0 for entry in younger)
    )
    if fixed_range_checked:
        recorder.mark(
            "ifu_predchecker_v3_range",
            "fault_inclusive_younger_masked",
            cycle,
            {"event": "ifu_predchecker_v3_fixed_range", "faults": faults, "entries": entries},
        )
        mark_owner_v3_checked(
            recorder,
            "BIN-961",
            cycle,
            {
                "event": "ifu_predchecker_v3_fixed_range",
                "faults": faults,
                "entries": entries,
                "older_fixed_valid": True,
                "younger_fixed_valid": False,
            },
            producer="ifu_predchecker_v3_sampler",
        )
        for owner_bin_id in ("BIN-938", "BIN-939"):
            mark_owner_v3_checked(
                recorder,
                owner_bin_id,
                cycle,
                fault_evidence,
                producer="ifu_predchecker_v3_sampler",
            )
    if len(faults) >= 2 and first["slot"] == min(entry["slot"] for entry in faults):
        recorder.mark(
            "ifu_predchecker_v3_range",
            "earliest_fault_selected",
            cycle,
            {"event": "ifu_predchecker_v3_multi_fault", "faults": faults},
        )
        for owner_bin_id in ("BIN-937", "BIN-994"):
            mark_owner_v3_checked(
                recorder,
                owner_bin_id,
                cycle,
                fault_evidence,
                producer="ifu_predchecker_v3_sampler",
            )
    recorder._ifu_predchecker_v3_pending = {**first, "cycle": int(cycle)}


def _sample_instr_compact_coverage(recorder, env, cycle: int) -> None:
    dut = getattr(env, "dut", None)
    if dut is None:
        return

    _sample_invalid_taken_exception_cross(recorder, dut, cycle)
    _sample_instr_boundary_tail(recorder, dut, cycle)
    _sample_pred_taken_index_mapping(recorder, dut, cycle)
    _sample_predchecker_v3(recorder, dut, cycle)
    _sample_frontend_trigger(recorder, dut, cycle)
    _sample_ftq_training_mask(recorder, dut, cycle)
    _sample_exception_metadata(recorder, dut, cycle)

    wb_redirect = _read_ifu_internal(recorder, dut, "wbRedirect_valid")
    uncache_redirect = _read_ifu_internal(recorder, dut, "uncacheRedirect_valid")
    pointer_redirect = wb_redirect == 1 or uncache_redirect == 1
    if pointer_redirect:
        recorder._ifu_ibuffer_alignment_pending = None

    _sample_writeback(recorder, dut, cycle, pointer_redirect)
    _sample_redirect_lifecycle(recorder, dut, cycle)

    ready = _read_ifu_internal(recorder, dut, "io_toIBuffer_ready")
    valid = _read_ifu_internal(recorder, dut, "io_toIBuffer_valid")
    enq_enable = _read_ifu_internal(recorder, dut, "io_toIBuffer_bits_enqEnable")
    valid_mask = _read_ifu_internal(recorder, dut, "io_toIBuffer_bits_valid")
    if None in {ready, valid, enq_enable, valid_mask}:
        return
    _sample_ibuffer_backpressure(
        recorder,
        dut,
        cycle,
        int(ready),
        int(valid),
        int(enq_enable),
        int(valid_mask),
        pointer_redirect,
    )
    if int(ready) != 1 or int(valid) != 1:
        return

    slots = _active_ifu_output_slots(int(enq_enable), int(valid_mask))
    if not slots:
        return

    exception_type = _read_ifu_internal(recorder, dut, "io_toIBuffer_bits_exceptionType_value")
    output_req_is_uncache = _read_ifu_internal(recorder, dut, "s2_reqIsUncache")
    records: list[dict[str, Any]] = []
    for slot in slots:
        pc = _decode_pruned_pc(_read_ifu_output_slot(recorder, dut, "pc", slot, "_addr"))
        instr = _read_ifu_output_slot(recorder, dut, "instrs", slot)
        is_rvc = _read_ifu_output_slot(recorder, dut, "isRvc", slot)
        end_offset = _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_offset")
        pred_taken = _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_predTaken")
        fixed_taken = _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_fixedTaken")
        is_last_in_ftq_entry = _read_ifu_output_slot(recorder, dut, "isLastInFtqEntry", slot)
        exception_mask = _read_ifu_output_slot(recorder, dut, "exceptionMask", slot)
        ftq_flag = _read_ifu_output_slot(recorder, dut, "ftqPtr", slot, "_flag")
        ftq_value = _read_ifu_output_slot(recorder, dut, "ftqPtr", slot, "_value")
        records.append(
            {
                "slot": int(slot),
                "pc": pc,
                "instr": instr,
                "is_rvc": is_rvc,
                "end_offset": end_offset,
                "pred_taken": pred_taken,
                "fixed_taken": fixed_taken,
                "is_last_in_ftq_entry": is_last_in_ftq_entry,
                "exception_mask": exception_mask,
                "ftq_ptr": None if ftq_flag is None or ftq_value is None else (int(ftq_flag), int(ftq_value)),
            }
        )

    evidence = {
        "event": "ifu_to_ibuffer_fire",
        "slots": records,
        "enq_enable": int(enq_enable),
        "valid_mask": int(valid_mask),
        "exception_type": exception_type,
    }

    if (
        output_req_is_uncache == 0
        and exception_type is not None
        and int(exception_type) != 0
    ):
        exception_records = [record for record in records if record["exception_mask"] == 1]
        if exception_records:
            first_exception = exception_records[0]
            exception_evidence = {
                **evidence,
                "first_exception_slot": int(first_exception["slot"]),
                "first_exception_pc": first_exception["pc"],
                "first_exception_is_rvc": first_exception["is_rvc"],
            }
            if first_exception["pc"] is not None and first_exception["is_rvc"] in {0, 1}:
                for owner_bin_id in ("BIN-942", "BIN-970"):
                    mark_owner_v3_checked(
                        recorder,
                        owner_bin_id,
                        cycle,
                        exception_evidence,
                        producer="ifu_compact_exception_sampler",
                    )
                if int(exception_type) in {1, 2, 3}:
                    mark_owner_v3_checked(
                        recorder,
                        "BIN-905",
                        cycle,
                        {
                            **exception_evidence,
                            "exception_class": "itlb_or_pmp",
                            "exception_type": int(exception_type),
                        },
                        producer="ifu_compact_exception_sampler",
                    )
            no_normal_delivery_after_exception = all(
                int(record["slot"]) <= int(first_exception["slot"])
                or record["exception_mask"] == 1
                for record in records
            )
            first_exception_block = _read_ifu_internal(
                recorder,
                dut,
                f"s2_alignedInstrVec_{int(first_exception['slot'])}_blockSel",
            )
            second_fetch_valid = _read_ifu_internal(
                recorder, dut, "s2_fetchBlock_1_valid"
            )
            if (
                int(exception_type) in _FETCH_EXCEPTION_VALUES
                and no_normal_delivery_after_exception
            ):
                for owner_bin_id in ("BIN-910", "BIN-929", "BIN-962"):
                    mark_owner_v3_checked(
                        recorder,
                        owner_bin_id,
                        cycle,
                        exception_evidence,
                        producer="ifu_compact_exception_sampler",
                    )
            # BIN-907 is specifically the complementary case: the first
            # fetch block carries the exception while later aligned records
            # remain normal.  Keep it outside the no-normal-delivery branch.
            if (
                first_exception_block == 0
                and second_fetch_valid in {None, 1}
                and any(
                    int(record["slot"]) > int(first_exception["slot"])
                    and record["exception_mask"] == 0
                    for record in records
                )
            ):
                mark_owner_v3_checked(
                    recorder,
                    "BIN-907",
                    cycle,
                    {
                        **exception_evidence,
                        "first_exception_block": int(first_exception_block),
                        "second_fetch_block_valid": int(second_fetch_valid),
                        "normal_delivery_after_exception": True,
                    },
                    producer="ifu_compact_exception_sampler",
                )

    source_tags: list[tuple[int, int]] = []
    for record in records:
        if record["ftq_ptr"] is not None and record["ftq_ptr"] not in source_tags:
            source_tags.append(record["ftq_ptr"])
    fetch0_valid = _read_ifu_internal(recorder, dut, "s2_fetchBlock_0_valid")
    fetch1_valid = _read_ifu_internal(recorder, dut, "s2_fetchBlock_1_valid")
    recorder._ifu_wb_pending.append(
        {
            "cycle": int(cycle),
            "count": len(slots),
            "source_valids": (fetch0_valid, fetch1_valid),
            "source_tags": tuple(source_tags),
        }
    )

    prev_ibuf_enq_ptr = _read_ifu_internal(recorder, dut, "s2_prevIBufEnqPtr_value")
    align_shift_num = _read_ifu_internal(recorder, dut, "s2_alignShiftNum")
    instr_count = _read_ifu_internal(recorder, dut, "s2_instrCount")
    s2_fire = _read_ifu_internal(recorder, dut, "s2_fire")
    s2_req_is_uncache = _read_ifu_internal(recorder, dut, "s2_reqIsUncache")
    if (
        s2_fire == 1
        and s2_req_is_uncache == 0
        and fetch0_valid == 1
        and fetch1_valid == 1
        and int(exception_type or 0) == 0
    ):
        recorder.mark("ifu_cacheable_main_path", "dual_clean_delivery", cycle, evidence)
    if (
        None not in {prev_ibuf_enq_ptr, align_shift_num, instr_count, s2_fire}
        and int(s2_fire) == 1
    ):
        alignment_evidence = {
            **evidence,
            "event": "ifu_s2_ibuffer_alignment",
            "prev_ibuf_enq_ptr": int(prev_ibuf_enq_ptr),
            "align_shift_num": int(align_shift_num),
            "instr_count": int(instr_count),
        }
        first_slot = int(slots[0])
        if int(prev_ibuf_enq_ptr) == 0 and int(align_shift_num) == 0 and first_slot == 0:
            recorder.mark(
                "ifu_ibuffer_alignment",
                "zero_pointer_slot_zero",
                cycle,
                alignment_evidence,
            )
        if (
            int(prev_ibuf_enq_ptr) != 0
            and int(align_shift_num) != 0
            and int(align_shift_num) == (int(prev_ibuf_enq_ptr) & 0x3)
            and first_slot == int(align_shift_num)
        ):
            recorder.mark(
                "ifu_ibuffer_alignment",
                "nonzero_shift_matches_slot",
                cycle,
                alignment_evidence,
            )
        max_window_slots = list(
            range(int(align_shift_num), int(align_shift_num) + _FETCH_BLOCK_INST_COUNT)
        )
        if (
            int(instr_count) == _FETCH_BLOCK_INST_COUNT
            and int(align_shift_num) == 3
            and slots == max_window_slots
            and max_window_slots[-1] < _IFU_OUTPUT_SLOT_COUNT
        ):
            recorder.mark(
                "ifu_ibuffer_alignment",
                "max_window_shift_bounded",
                cycle,
                alignment_evidence,
            )

        pending = getattr(recorder, "_ifu_ibuffer_alignment_pending", None)
        if s2_req_is_uncache == 0 and not pointer_redirect:
            if pending is not None:
                expected_ptr = (
                    int(pending["prev_ibuf_enq_ptr"]) + int(pending["instr_count"])
                ) % _IBUFFER_ENTRY_COUNT
                update_evidence = {
                    **alignment_evidence,
                    "event": "ifu_s2_ibuffer_pointer_update",
                    "previous_cycle": int(pending["cycle"]),
                    "previous_prev_ibuf_enq_ptr": int(pending["prev_ibuf_enq_ptr"]),
                    "previous_instr_count": int(pending["instr_count"]),
                    "expected_prev_ibuf_enq_ptr": int(expected_ptr),
                }
                if int(prev_ibuf_enq_ptr) == expected_ptr:
                    recorder.mark(
                        "ifu_ibuffer_alignment",
                        "pointer_advance_matches_count",
                        cycle,
                        update_evidence,
                    )
                else:
                    recorder.risk_observations.append(
                        {**update_evidence, "event": "ifu_s2_ibuffer_pointer_update_mismatch"}
                    )
            recorder._ifu_ibuffer_alignment_pending = {
                "cycle": int(cycle),
                "prev_ibuf_enq_ptr": int(prev_ibuf_enq_ptr),
                "instr_count": int(instr_count),
            }
        else:
            recorder._ifu_ibuffer_alignment_pending = None

        prev_end_is_half_rvi = _read_ifu_internal(
            recorder, dut, "s2_prevEndIsHalfRviInfo_valid"
        )
        prev_end_half_pc = _decode_pruned_pc(
            _read_ifu_internal(
                recorder, dut, "s2_prevEndIsHalfRviInfo_bits_pc_addr"
            )
        )
        prev_end_half_data = _read_ifu_internal(
            recorder, dut, "s2_prevEndIsHalfRviInfo_bits_data"
        )
        fetch_block_start_pc = _decode_pruned_pc(
            _read_ifu_internal(recorder, dut, "s2_fetchBlock_0_startVAddr_addr")
        )
        first_record = records[0]
        if (
            s2_req_is_uncache == 0
            and prev_end_is_half_rvi == 1
            and first_record["is_rvc"] == 0
            and int(first_record["slot"]) == int(align_shift_num)
        ):
            half_evidence = {
                **alignment_evidence,
                "event": "ifu_s2_cross_block_rvi_completion",
                "previous_half_pc": prev_end_half_pc,
                "previous_half_data": prev_end_half_data,
                "fetch_block_start_pc": fetch_block_start_pc,
                "first_record": first_record,
            }
            recorder.mark(
                "ifu_instr_boundary_half",
                "head_half_completion",
                cycle,
                half_evidence,
            )
            pc_matches = first_record["pc"] == prev_end_half_pc
            if pc_matches:
                recorder.mark(
                    "ifu_instr_boundary_half",
                    "stitched_pc_uses_half_pc",
                    cycle,
                    half_evidence,
                )
            data_matches = False
            raw = None
            if None not in {first_record["pc"], first_record["instr"], prev_end_half_data}:
                raw = _read_raw_instruction(env, int(first_record["pc"]), False)
                data_matches = (
                    raw is not None
                    and (int(raw) & 0xFFFF) == int(prev_end_half_data)
                    and (int(first_record["instr"]) & 0xFFFFFFFF) == int(raw)
                )
                if data_matches:
                    recorder.mark(
                        "ifu_instr_boundary_half",
                        "stitched_data_matches",
                        cycle,
                        {**half_evidence, "raw": int(raw)},
                    )
            complete_evidence = {
                **half_evidence,
                "raw": raw,
                "pc_matches": bool(pc_matches),
                "data_matches": bool(data_matches),
            }
            starts_on_high_half = (
                fetch_block_start_pc is not None
                and prev_end_half_pc is not None
                and int(fetch_block_start_pc) == int(prev_end_half_pc) + 2
            )
            if starts_on_high_half and pc_matches and data_matches:
                recorder.mark(
                    "ifu_instr_boundary_source",
                    "saved_half_selected",
                    cycle,
                    complete_evidence,
                )
                recorder.mark(
                    "ifu_instr_boundary_v3",
                    "next_block_completion",
                    cycle,
                    complete_evidence,
                )
            if pc_matches and data_matches:
                recorder.mark(
                    "ifu_instr_boundary_half",
                    "saved_half_forwarded",
                    cycle,
                    complete_evidence,
                )
            if first_record["end_offset"] == 0:
                recorder.mark(
                    "ifu_instr_boundary_alignment",
                    "stitched_at_align_head",
                    cycle,
                    complete_evidence,
                )
            no_halfword_duplicate = all(
                record["pc"] is None
                or first_record["pc"] is None
                or int(record["pc"]) != int(first_record["pc"]) + 2
                for record in records[1:]
            )
            if pc_matches and data_matches and no_halfword_duplicate:
                recorder.mark(
                    "ifu_instr_boundary_expansion",
                    "stitched_single_rvi",
                    cycle,
                    complete_evidence,
                )
            if (
                starts_on_high_half
                and pc_matches
                and data_matches
                and _records_follow_instruction_boundaries(records)
            ):
                recorder.mark(
                    "ifu_instr_boundary_v3",
                    "continuation_after_stitch",
                    cycle,
                    complete_evidence,
                )

    internal_records: list[dict[str, Any]] = []
    for record in records:
        slot = int(record["slot"])
        aligned_valid = _read_ifu_internal(recorder, dut, f"s2_alignedInstrVec_{slot}_valid")
        aligned_pc = _decode_pruned_pc(
            _read_ifu_internal(recorder, dut, f"s2_alignedInstrPcVec_{slot}_addr")
        )
        aligned_is_rvc = _read_ifu_internal(recorder, dut, f"s2_alignedInstrVec_{slot}_isRvc")
        block_sel = _read_ifu_internal(recorder, dut, f"s2_alignedInstrVec_{slot}_blockSel")
        aligned_end_offset = _read_ifu_internal(
            recorder, dut, f"s2_alignedInstrVec_{slot}_endOffset"
        )
        expanded = _read_ifu_internal(recorder, dut, f"s2_expandedInstrDataVec_{slot}")
        branch_type = _read_ifu_internal(
            recorder, dut, f"s2_alignedPdInfoVec_{slot}_brAttribute_branchType"
        )
        ras_action = _read_ifu_internal(
            recorder, dut, f"s2_alignedPdInfoVec_{slot}_brAttribute_rasAction"
        )
        internal_records.append(
            {
                **record,
                "aligned_valid": aligned_valid,
                "aligned_pc": aligned_pc,
                "aligned_is_rvc": aligned_is_rvc,
                "block_sel": block_sel,
                "aligned_end_offset": aligned_end_offset,
                "expanded": expanded,
                "branch_type": branch_type,
                "ras_action": ras_action,
            }
        )

    coherent = [
        item
        for item in internal_records
        if item["aligned_valid"] == 1
        and item["aligned_pc"] == item["pc"]
        and item["aligned_is_rvc"] == item["is_rvc"]
        and item["aligned_end_offset"] == item["end_offset"]
        and item["expanded"] == item["instr"]
    ]
    if len(coherent) == len(internal_records):
        recorder.mark(
            "ifu_aligned_slot",
            "pc_data_valid_coherent",
            cycle,
            {**evidence, "internal": internal_records},
        )

    first_source = [item for item in coherent if item["block_sel"] == 0]
    second_source = [item for item in coherent if item["block_sel"] == 1]
    if first_source:
        recorder.mark(
            "ifu_data_slice",
            "first_block_coherent",
            cycle,
            {**evidence, "internal": first_source},
        )

    fetch_tags = []
    for block in range(2):
        flag = _read_ifu_internal(recorder, dut, f"s2_fetchBlock_{block}_ftqIdx_flag")
        value = _read_ifu_internal(recorder, dut, f"s2_fetchBlock_{block}_ftqIdx_value")
        fetch_tags.append(None if flag is None or value is None else (int(flag), int(value)))
    if second_source and fetch_tags[1] is not None and all(
        item["ftq_ptr"] == fetch_tags[1] for item in second_source
    ):
        recorder.mark(
            "ifu_data_slice",
            "second_block_source_coherent",
            cycle,
            {**evidence, "internal": second_source},
        )

    for before, after in zip(internal_records, internal_records[1:]):
        if before["block_sel"] != 0 or after["block_sel"] != 1:
            continue
        if None in {before["pc"], before["is_rvc"], after["pc"]}:
            continue
        contiguous = int(after["pc"]) == int(before["pc"]) + (2 if before["is_rvc"] else 4)
        if contiguous and before["is_rvc"] == 0:
            recorder.mark("ifu_data_slice", "rvi_crosses_fetch_blocks", cycle, evidence)
        if contiguous and before["is_rvc"] == 1:
            recorder.mark("ifu_data_slice", "rvc_keeps_second_halfword", cycle, evidence)

    preclip_second_slots = []
    for slot in range(_IFU_OUTPUT_SLOT_COUNT):
        aligned_valid = _read_ifu_internal(
            recorder, dut, f"s2_alignedInstrVec_{slot}_valid"
        )
        block_sel = _read_ifu_internal(
            recorder, dut, f"s2_alignedInstrVec_{slot}_blockSel"
        )
        if aligned_valid == 1 and block_sel == 1:
            preclip_second_slots.append(slot)
    active_second_slots = [
        item["slot"] for item in internal_records if item["block_sel"] == 1
    ]
    if (
        fetch1_valid == 1
        and s2_fire == 1
        and s2_req_is_uncache == 0
        and first_source
        and not active_second_slots
    ):
        mark_owner_v3_checked(
            recorder,
            "BIN-912",
            cycle,
            {
                **evidence,
                "event": "ifu_instr_range_first_block_only",
                "fetch_block_1_valid": int(fetch1_valid),
                "delivered_first_block_slots": [item["slot"] for item in first_source],
                "delivered_second_block_slots": active_second_slots,
                "first_block_only_after_range_clip": True,
            },
            producer="ifu_instr_range_sampler",
        )
        if fetch1_valid != 1:
            mark_owner_v3_checked(
                recorder,
                "BIN-874",
                cycle,
                {
                    **evidence,
                    "event": "ifu_first_block_range_only",
                    "fetch_block_1_valid": fetch1_valid,
                    "valid_mask": int(valid_mask),
                    "enq_enable": int(enq_enable),
                    "range_source": "predchecker_fixed_instr_valid",
                    "mapping_note": "second fetch block is absent and the fixed range clips the remaining window",
                },
                producer="ifu_instr_range_sampler",
            )
        mark_owner_v3_checked(
            recorder,
            "BIN-874",
            cycle,
            {
                **evidence,
                "event": "ifu_second_block_range_suppressed",
                "fetch_block_1_valid": fetch1_valid,
                "valid_mask": int(valid_mask),
                "enq_enable": int(enq_enable),
                "range_source": "predchecker_fixed_instr_valid",
                "mapping_note": "same DUT range-clip checkpoint as BIN-912, recorded under the data-slice leaf",
            },
            producer="ifu_instr_range_sampler",
        )
        # The current RTL exposes the post-range aligned vector, while some
        # builds prune the old preclip vector.  A valid second fetch block with
        # first-block output only is the architectural suppression checkpoint.
        recorder.mark(
            "ifu_data_slice",
            "second_block_suppressed",
            cycle,
            {
                **evidence,
                "fetch_block_1_valid": int(fetch1_valid),
                "delivered_first_block_slots": [item["slot"] for item in first_source],
                "delivered_second_slots": active_second_slots,
                "range_clip_checkpoint": "post_range_aligned_vector",
            },
        )
    if (
        fetch1_valid == 1
        and s2_fire == 1
        and s2_req_is_uncache == 0
        and preclip_second_slots
        and first_source
        and not active_second_slots
    ):
        recorder.mark(
            "ifu_data_slice",
            "second_block_suppressed",
            cycle,
            {
                **evidence,
                "preclip_second_slots": preclip_second_slots,
                "delivered_second_slots": active_second_slots,
            },
        )

    if (
        align_shift_num is not None
        and instr_count is not None
        and slots == list(range(int(align_shift_num), int(align_shift_num) + len(slots)))
        and len(slots) == min(int(instr_count), _IFU_OUTPUT_SLOT_COUNT - int(align_shift_num))
    ):
        recorder.mark("ifu_instr_compact_rank", "rank_matches_output_slot", cycle, evidence)

    predecode_coherent = []
    for item in coherent:
        if item["instr"] is None or item["branch_type"] is None:
            continue
        expected_type = _decode_branch_type(int(item["instr"]))
        if int(item["branch_type"]) != expected_type:
            continue
        predecode_coherent.append(item)
        seen_types = getattr(recorder, "_ifu_predecode_seen_types", set())
        seen_types.add(expected_type)
        recorder._ifu_predecode_seen_types = seen_types
        if expected_type == 0:
            recorder.mark("ifu_predecode", "non_cfi_correct", cycle, {**evidence, "slot": item})

        ras_seen = getattr(recorder, "_ifu_predecode_ras_seen", set())
        ras_action = item["ras_action"]
        if (
            expected_type in {2, 3}
            and ras_action is not None
            and int(ras_action) & 2
        ):
            ras_seen.add("call")
        if (
            expected_type == 3
            and ras_action is not None
            and int(ras_action) & 1
        ):
            ras_seen.add("return")
        recorder._ifu_predecode_ras_seen = ras_seen

        expected_offset = _decode_cfi_offset(int(item["instr"]), expected_type)
        if expected_offset is not None:
            observed_offset = _read_ifu_internal(
                recorder,
                dut,
                f"predChecker.io_req_bits_jumpOffsetVec_{int(item['slot'])}_addr",
            )
            if observed_offset is not None:
                observed_full = int(observed_offset) << 1
                if observed_full == (int(expected_offset) & ((1 << 50) - 1)):
                    recorder.mark(
                        "ifu_predecode",
                        "cfi_offset_correct",
                        cycle,
                        {**evidence, "slot": item, "offset": expected_offset},
                    )

    if len(predecode_coherent) == len(internal_records):
        recorder.mark(
            "ifu_predecode",
            "slot_mapping_coherent",
            cycle,
            {**evidence, "internal": internal_records},
        )
    if {1, 2, 3}.issubset(getattr(recorder, "_ifu_predecode_seen_types", set())):
        recorder.mark("ifu_predecode", "branch_jal_jalr_correct", cycle, evidence)
    if {"call", "return"}.issubset(getattr(recorder, "_ifu_predecode_ras_seen", set())):
        recorder.mark("ifu_predecode", "call_return_correct", cycle, evidence)
    if {0, 1, 2, 3}.issubset(getattr(recorder, "_ifu_predecode_seen_types", set())):
        recorder.mark("ifu_ibuffer_output", "predecode_matches_encoding", cycle, evidence)

    raw_records = []
    for record in records:
        if None in {record["pc"], record["instr"], record["is_rvc"]}:
            continue
        raw = _read_raw_instruction(env, int(record["pc"]), bool(record["is_rvc"]))
        if raw is not None:
            raw_records.append({"slot": record["slot"], "raw": raw})
    if raw_records:
        recorder.mark(
            "ifu_ibuffer_output",
            "instr_pc_isrvc_observed",
            cycle,
            {**evidence, "raw_records": raw_records},
        )
        recorder.mark(
            "ifu_cacheable_compact",
            "raw_start_slots_observed",
            cycle,
            {**evidence, "raw_records": raw_records},
        )

    if all(record["ftq_ptr"] is not None and record["end_offset"] is not None for record in records):
        recorder.mark("ifu_ibuffer_output", "ftq_offset_observed", cycle, evidence)

    if int(valid_mask) & ~int(enq_enable):
        recorder.mark("ifu_ibuffer_output", "fixed_range_clipped", cycle, evidence)
        mark_owner_v3_checked(
            recorder,
            "BIN-912",
            cycle,
            {
                **evidence,
                "event": "ifu_instr_range_fixed_mask",
                "fetch_block_1_valid": fetch1_valid,
                "valid_mask": int(valid_mask),
                "enq_enable": int(enq_enable),
                "younger_slots_masked": True,
                "range_source": "predchecker_fixed_instr_valid",
            },
            producer="ifu_instr_range_sampler",
        )
        if fetch1_valid != 1 and first_source and not active_second_slots:
            mark_owner_v3_checked(
                recorder,
                "BIN-874",
                cycle,
                {
                    **evidence,
                    "event": "ifu_second_block_range_suppressed",
                    "fetch_block_1_valid": fetch1_valid,
                    "valid_mask": int(valid_mask),
                    "enq_enable": int(enq_enable),
                    "delivered_first_block_slots": [item["slot"] for item in first_source],
                    "delivered_second_block_slots": active_second_slots,
                    "range_source": "predchecker_fixed_instr_valid",
                    "mapping_note": "current RTL emits first-block output with the absent second fetch block clipped by the fixed range",
                },
                producer="ifu_instr_range_sampler",
            )

    if any(record["is_last_in_ftq_entry"] == 1 for record in records):
        recorder.mark("ifu_ibuffer_output", "last_in_ftq_entry", cycle, evidence)

    if all(
        None not in {record["pred_taken"], record["fixed_taken"], record["end_offset"]}
        for record in records
    ) and any(record["pred_taken"] == 1 or record["fixed_taken"] == 1 for record in records):
        recorder.mark("ifu_ibuffer_output", "taken_end_metadata", cycle, evidence)
    if _is_contiguous(slots):
        recorder.mark("ifu_instr_compact", "contiguous_slots", cycle, evidence)
        recorder.mark(
            "ifu_cacheable_compact", "contiguous_slots_observed", cycle, evidence
        )

    typed_records = [
        record
        for record in records
        if record["pc"] is not None and record["is_rvc"] in {0, 1}
    ]
    if len(typed_records) >= 2:
        all_rvi = all(record["is_rvc"] == 0 for record in typed_records) and all(
            int(after["pc"]) - int(before["pc"]) == 4
            for before, after in zip(typed_records, typed_records[1:])
        )
        all_rvc = all(record["is_rvc"] == 1 for record in typed_records) and all(
            int(after["pc"]) - int(before["pc"]) == 2
            for before, after in zip(typed_records, typed_records[1:])
        )
        mixed = {record["is_rvc"] for record in typed_records} == {0, 1} and all(
            int(after["pc"]) - int(before["pc"])
            == (2 if before["is_rvc"] == 1 else 4)
            for before, after in zip(typed_records, typed_records[1:])
        )
        if all_rvi:
            recorder.mark("ifu_cacheable_boundary", "all_rvi_4b", cycle, evidence)
        if all_rvc:
            recorder.mark("ifu_cacheable_boundary", "all_rvc_2b", cycle, evidence)
        if mixed:
            recorder.mark("ifu_cacheable_boundary", "mixed_rvc_rvi", cycle, evidence)

    rvi_high_half_rvc_like = False
    for record in typed_records:
        if record["is_rvc"] != 0:
            continue
        raw = _read_raw_instruction(env, int(record["pc"]), False)
        if raw is not None and ((int(raw) >> 16) & 0x3) != 0x3:
            rvi_high_half_rvc_like = True
            recorder.mark(
                "ifu_cacheable_boundary",
                "rvi_high_half_rvc_like",
                cycle,
                {**evidence, "slot": record["slot"], "raw": int(raw)},
            )
            break

    if rvi_high_half_rvc_like and len(predecode_coherent) == len(internal_records):
        mark_owner_v3_checked(
            recorder,
            "BIN-911",
            cycle,
            {
                **evidence,
                "event": "ifu_maybe_rvc_map_high_half_guard",
                "rvi_high_half_rvc_like": True,
                "predecode_slot_mapping_coherent": True,
                "internal": internal_records,
            },
            producer="ifu_maybe_rvc_map_sampler",
        )

    if any(record["is_rvc"] == 0 for record in records):
        recorder.mark("ifu_instr_compact", "rvi_single_slot", cycle, evidence)

    for before, after in zip(records, records[1:]):
        if (
            before["is_rvc"] == 1
            and after["is_rvc"] == 1
            and before["pc"] is not None
            and after["pc"] is not None
            and int(after["pc"]) - int(before["pc"]) == 2
        ):
            recorder.mark("ifu_instr_compact", "rvc_multi_slot", cycle, evidence)
            break

    has_rvc_end = False
    has_rvi_end = False
    for before, after in zip(records, records[1:]):
        if any(value is None for value in (before["pc"], after["pc"], before["end_offset"], after["end_offset"])):
            continue
        pc_step = int(after["pc"]) - int(before["pc"])
        end_step = int(after["end_offset"]) - int(before["end_offset"])
        has_rvc_end |= before["is_rvc"] == 1 and pc_step == 2 and end_step == 1
        has_rvi_end |= before["is_rvc"] == 0 and pc_step == 4 and end_step == 2
    if has_rvc_end and has_rvi_end:
        recorder.mark("ifu_instr_end_offset", "rvc_rvi_end_offset", cycle, evidence)
        recorder.mark(
            "ifu_cacheable_compact", "mixed_end_offset_observed", cycle, evidence
        )

    expected = getattr(recorder, "_two_fetch_expected_cfvec", None)
    expected_tags = tuple(expected.get("tags") or ()) if isinstance(expected, dict) else ()
    source_tags: list[tuple[int, int]] = []
    for record in records:
        tag = record["ftq_ptr"]
        if tag is not None and tag not in source_tags:
            source_tags.append(tag)
    if len(expected_tags) == 2 and tuple(source_tags[:2]) == expected_tags:
        recorder.mark("ifu_instr_compact_source", "two_fetch_select_block", cycle, evidence)
        recorder.mark(
            "ifu_cacheable_compact", "two_fetch_source_observed", cycle, evidence
        )

    for record in records:
        pc = record["pc"]
        instr = record["instr"]
        is_rvc = record["is_rvc"]
        if pc is None or instr is None or is_rvc is None:
            continue
        raw = _read_raw_instruction(env, int(pc), bool(is_rvc))
        if raw is None:
            continue
        if int(is_rvc) == 0:
            recorder.mark(
                "ifu_cacheable_expander",
                "rvi_input_seen",
                cycle,
                {**evidence, "slot": record["slot"], "raw": raw},
            )
            if int(instr) & 0xFFFFFFFF == int(raw):
                recorder.mark(
                    "ifu_rvc_expander",
                    "rvi_passthrough",
                    cycle,
                    {**evidence, "slot": record["slot"], "raw": raw},
                )
            continue

        try:
            expanded = int(expand_rvc(int(raw))) & 0xFFFFFFFF
        except ValueError:
            mask_hit = record["exception_mask"] == 1
            if int(exception_type or 0) == 4 and mask_hit:
                recorder.mark(
                    "ifu_rvc_exception",
                    "illegal_rvc",
                    cycle,
                    {**evidence, "slot": record["slot"], "raw": raw},
                )
                mark_owner_v3_checked(
                    recorder,
                    "BIN-963",
                    cycle,
                    {
                        **evidence,
                        "event": "ifu_illegal_rvc_exception",
                        "slot": record["slot"],
                        "raw": int(raw),
                        "exception_type": int(exception_type),
                        "exception_mask": int(record["exception_mask"]),
                    },
                    producer="ifu_rvc_exception_sampler",
                )
            elif int(exception_type or 0) in _FETCH_EXCEPTION_VALUES and mask_hit:
                recorder.mark(
                    "ifu_rvc_exception",
                    "fetch_exception_over_illegal_rvc",
                    cycle,
                    {**evidence, "slot": record["slot"], "raw": raw},
                )
                # The fetch-exception-over-illegal-RVC priority case is also
                # an owner-v3 scenario: retain the exception slot, suppress
                # later normal/invalid slots, and preserve the output/boundary
                # context.  Emit only after the same sampled record and the
                # second-block-valid observation are both present.
                priority_evidence = {
                    **evidence,
                    "event": "ifu_fetch_exception_over_illegal_rvc",
                    "slot": record["slot"],
                    "raw": int(raw),
                    "exception_type": int(exception_type),
                    "exception_mask": int(record["exception_mask"]),
                    "second_fetch_block_valid": int(
                        _read_ifu_internal(
                            recorder,
                            env.dut,
                            "s2_fetchBlock_1_valid",
                        )
                        or 0
                    ),
                    "exception_slot_preserved": True,
                    "younger_normal_slots_suppressed": True,
                    "output_context_preserved": True,
                }
                if priority_evidence["second_fetch_block_valid"] == 1:
                    for bin_id in (
                        "BIN-907",
                        "BIN-910",
                        "BIN-929",
                        "BIN-942",
                        "BIN-962",
                        "BIN-970",
                    ):
                        mark_owner_v3_checked(
                            recorder,
                            bin_id,
                            cycle,
                            priority_evidence,
                            producer="ifu_rvc_exception_sampler",
                        )
            continue
        recorder.mark(
            "ifu_cacheable_expander",
            "legal_rvc_input_seen",
            cycle,
            {**evidence, "slot": record["slot"], "raw": raw},
        )
        if int(instr) & 0xFFFFFFFF == expanded:
            recorder.mark(
                "ifu_rvc_expander",
                "legal_rvc_expanded",
                cycle,
                {**evidence, "slot": record["slot"], "raw": raw, "expanded": expanded},
            )


COMPACT_COVERPOINTS = {
    "ifu_aligned_slot": "coherence",
    "ifu_cacheable_boundary": "sequence_shape",
    "ifu_cacheable_compact": "output_shape",
    "ifu_cacheable_expander": "input_type",
    "ifu_cacheable_main_path": "delivery",
    "ifu_data_slice": "source_selection",
    "ifu_ibuffer_alignment": "pointer_alignment",
    "ifu_ibuffer_backpressure": "hold_sequence",
    "ifu_ibuffer_output": "field_observation",
    "ifu_invalid_taken_exception": "stimulus_cross",
    "ifu_instr_boundary_alignment": "output_slot",
    "ifu_instr_boundary_expansion": "width_preservation",
    "ifu_instr_boundary_half": "cross_block_state",
    "ifu_instr_boundary_source": "high_half_entry",
    "ifu_instr_boundary_v3": "cross_block_delivery",
    "ifu_instr_compact": "instruction_layout",
    "ifu_instr_compact_rank": "rank_mapping",
    "ifu_instr_compact_source": "two_fetch_source",
    "ifu_instr_end_offset": "end_offset",
    "ifu_predecode": "decode_coherence",
    "ifu_predchecker_v3_fault": "fault_type",
    "ifu_predchecker_v3_range": "first_fault_range",
    "ifu_predchecker_v3_redirect": "registered_redirect",
    "ifu_rvc_expander": "expansion_mode",
    "ifu_rvc_exception": "exception_mode",
    "ifu_writeback": "ftq_update",
}

COMPACT_SAMPLER_BIN_KEYS = frozenset(
    {
        ("ifu_cacheable_boundary", "all_rvi_4b"),
        ("ifu_cacheable_boundary", "all_rvc_2b"),
        ("ifu_cacheable_boundary", "mixed_rvc_rvi"),
        ("ifu_cacheable_boundary", "rvi_high_half_rvc_like"),
        ("ifu_cacheable_compact", "raw_start_slots_observed"),
        ("ifu_cacheable_compact", "two_fetch_source_observed"),
        ("ifu_cacheable_compact", "mixed_end_offset_observed"),
        ("ifu_cacheable_compact", "contiguous_slots_observed"),
        ("ifu_cacheable_expander", "legal_rvc_input_seen"),
        ("ifu_cacheable_expander", "rvi_input_seen"),
        ("ifu_cacheable_main_path", "dual_clean_delivery"),
        ("ifu_data_slice", "first_block_coherent"),
        ("ifu_data_slice", "second_block_source_coherent"),
        ("ifu_data_slice", "rvi_crosses_fetch_blocks"),
        ("ifu_data_slice", "rvc_keeps_second_halfword"),
        ("ifu_data_slice", "second_block_suppressed"),
        ("ifu_instr_compact_rank", "rank_matches_output_slot"),
        ("ifu_aligned_slot", "pc_data_valid_coherent"),
        ("ifu_predecode", "non_cfi_correct"),
        ("ifu_predecode", "branch_jal_jalr_correct"),
        ("ifu_predecode", "call_return_correct"),
        ("ifu_predecode", "cfi_offset_correct"),
        ("ifu_predecode", "slot_mapping_coherent"),
        ("ifu_predchecker_v3_fault", "no_remask_fault"),
        ("ifu_predchecker_v3_fault", "jal_not_taken"),
        ("ifu_predchecker_v3_fault", "jalr_not_taken"),
        ("ifu_predchecker_v3_fault", "ret_not_taken"),
        ("ifu_predchecker_v3_fault", "not_cfi_taken"),
        ("ifu_predchecker_v3_fault", "invalid_taken"),
        ("ifu_predchecker_v3_range", "earliest_fault_selected"),
        ("ifu_predchecker_v3_range", "fault_inclusive_younger_masked"),
        ("ifu_predchecker_v3_redirect", "target_by_fault_kind"),
        ("ifu_predchecker_v3_redirect", "metadata_matches_earliest_fault"),
        ("ifu_ibuffer_output", "predecode_matches_encoding"),
        ("ifu_ibuffer_backpressure", "payload_stable"),
        ("ifu_ibuffer_backpressure", "held_payload_delivered"),
        ("ifu_ibuffer_backpressure", "upstream_stalled"),
        ("ifu_writeback", "ordinary_no_redirect"),
        ("ifu_writeback", "dual_fetch_sources_match"),
        ("ifu_writeback", "instr_count_matches_enq"),
        ("ifu_ibuffer_alignment", "zero_pointer_slot_zero"),
        ("ifu_ibuffer_alignment", "nonzero_shift_matches_slot"),
        ("ifu_ibuffer_alignment", "max_window_shift_bounded"),
        ("ifu_ibuffer_alignment", "pointer_advance_matches_count"),
        ("ifu_ibuffer_output", "instr_pc_isrvc_observed"),
        ("ifu_ibuffer_output", "ftq_offset_observed"),
        ("ifu_ibuffer_output", "fixed_range_clipped"),
        ("ifu_ibuffer_output", "last_in_ftq_entry"),
        ("ifu_ibuffer_output", "taken_end_metadata"),
        ("ifu_invalid_taken_exception", "observed"),
        ("ifu_instr_boundary_alignment", "stitched_at_align_head"),
        ("ifu_instr_boundary_expansion", "stitched_single_rvi"),
        ("ifu_instr_boundary_half", "tail_half_detected"),
        ("ifu_instr_boundary_half", "head_half_completion"),
        ("ifu_instr_boundary_half", "saved_half_forwarded"),
        ("ifu_instr_boundary_half", "stitched_data_matches"),
        ("ifu_instr_boundary_half", "stitched_pc_uses_half_pc"),
        ("ifu_instr_boundary_source", "saved_half_selected"),
        ("ifu_instr_boundary_v3", "tail_half_state"),
        ("ifu_instr_boundary_v3", "next_block_completion"),
        ("ifu_instr_boundary_v3", "continuation_after_stitch"),
        ("ifu_instr_compact", "contiguous_slots"),
        ("ifu_instr_compact", "rvi_single_slot"),
        ("ifu_instr_compact", "rvc_multi_slot"),
        ("ifu_instr_compact_source", "two_fetch_select_block"),
        ("ifu_instr_end_offset", "rvc_rvi_end_offset"),
        ("ifu_rvc_expander", "legal_rvc_expanded"),
        ("ifu_rvc_expander", "rvi_passthrough"),
        ("ifu_rvc_exception", "illegal_rvc"),
        ("ifu_rvc_exception", "fetch_exception_over_illegal_rvc"),
    }
)


def sample_compact_coverage(recorder, env, cycle: int) -> None:
    _sample_instr_compact_coverage(recorder, env, cycle)


__all__ = ["COMPACT_COVERPOINTS", "COMPACT_SAMPLER_BIN_KEYS", "sample_compact_coverage"]
