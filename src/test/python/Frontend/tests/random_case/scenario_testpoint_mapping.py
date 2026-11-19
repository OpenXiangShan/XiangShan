"""Offline links from random scenario families to reviewed testpoints."""

import hashlib
import json

# These are explicit, reviewable links into the canonical testpoint catalog.
# They intentionally describe a family exploration envelope; a DUT run must
# still prove which individual scenarios and bins were actually realized.
FAMILY_TESTPOINT_IDS: dict[str, tuple[str, ...]] = {
    "cfg_layout_mix": (
        "TP-A3C71554887D",  # page-tail fall-through crosses a 4 KiB page
        "TP-FF13D2B05681",  # repeated direct-jump prediction/training
        "TP-DBD778207C98",  # taken conditional branch
        "TP-B23F955C179D",  # repeated direct-jump ABTB lookup
        "TP-31CBDD333D48",  # repeated conditional-branch ABTB training
        "TP-7AE61813536F",  # sequential s1 prediction path
        "TP-A5400D81695B",  # repeated loop branch / history update
        "TP-3AB632F5F0B9",  # mixed RVI/RVC delivery
        "TP-9E9B3ECAF7B8",  # RVI instruction across a fetch-block boundary
    ),
    "ras_indirect_history": (
        "TP-A3C71554887D",  # page-tail fall-through crosses a 4 KiB page
        "TP-6F04E38C634F",  # return prediction with uRAS target
        "TP-759F46DF1522",  # pending call followed by matching return
        "TP-CEB632761EF2",  # call followed by matching return
        "TP-D864E8FF06A4",  # nested calls followed by returns
        "TP-4D59CD84F689",  # speculative RAS push/pop ordering
        "TP-BDEA2E1B8FF3",  # committed call/return RAS update
        "TP-8E71FC4A3420",  # commit and speculative RAS activity overlap
        "TP-3AB632F5F0B9",  # mixed RVI/RVC delivery
    ),
    "ibuffer_pressure_cross": (
        "TP-5E43742AE3A9",  # IBuffer ready low holds s2 output payload stable
        "TP-E81830286280",  # release resumes the held prediction block
        "TP-7E0192010E33",  # backpressure protects against a new ICache response
        "TP-D3534BCC613A",  # redirect during backpressure discards old payload
        "TP-DABDD5BB59D1",  # path x stall/flush/redirect cross
        "TP-9E9B3ECAF7B8",  # RVI instruction across a fetch-block boundary
        "TP-A3C71554887D",  # page-tail fall-through crosses a 4 KiB page
    ),
    "translation_timing_cross": (
        "TP-BE3B82462DA0",
        "TP-A029A53CC159",
        "TP-685EA095D6E4",
        "TP-6FFECC3E6934",
        "TP-4ACCBD47410B",
        "TP-D8B773CC9DE8",
        "TP-3E4BC9F46B44",
        "TP-142F09E5B933",
        "TP-97662EFDD77C",
        "TP-6E294F3B7153",
        "TP-D9648EC87124",
        "TP-04C69F96DAC3",
    ),
}

# Reviewed catalog move at bbb2dd2c1: only hierarchy changed. Keep historical
# scenario identities intact; new intents require exact condition/checkpoint/
# object/coverage equality. This mapping never transfers status or HIT evidence.
REVIEWED_TESTPOINT_MOVES = {
    "TP-9E9B3ECAF7B8": ("TP-F0A234CD3E54", "4923135ad887c2b0992511dd7b75b995cabf3c4a5fa217efbc53245a82809bba"),
}


def _resolve_testpoint_ids(testpoint_ids, index):
    result = []
    for testpoint_id in testpoint_ids:
        if testpoint_id in index:
            result.append(testpoint_id)
            continue
        move = REVIEWED_TESTPOINT_MOVES.get(testpoint_id)
        if move is None or move[0] not in index:
            result.append(testpoint_id)  # existing missing-ID gate rejects it
            continue
        replacement, expected = move
        item = index[replacement]
        semantic = {key: item.get(key) for key in ("condition", "checkpoint", "object", "coverage")}
        actual = hashlib.sha256(json.dumps(
            semantic, ensure_ascii=False, sort_keys=True, separators=(",", ":"),
        ).encode()).hexdigest()
        if actual != expected:
            raise ValueError("reviewed random testpoint move changed semantics: " + replacement)
        result.append(replacement)
    return tuple(result)
