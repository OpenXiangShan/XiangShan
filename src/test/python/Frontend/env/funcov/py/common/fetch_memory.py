from __future__ import annotations

from typing import Optional

from ....rvc_decoder import expand_rvc


def _translate_fetch_addr(env, va: int) -> tuple[Optional[int], dict]:
    if env is None or getattr(env, "page_table", None) is None:
        return int(va), {"mode": "bare", "va": int(va), "pa": int(va), "ok": True}
    pa, ok, info = env.page_table.translate(int(va))
    meta = dict(info or {})
    meta["va"] = int(va)
    meta["ok"] = bool(ok)
    if ok:
        meta["pa"] = int(pa)
        return int(pa), meta
    return None, meta


def _read_expected_fetch_raw(env, pc: int, size: int) -> tuple[Optional[int], dict]:
    if env is None or getattr(env, "memory", None) is None:
        return None, {"ok": False, "reason": "no_memory"}
    value = 0
    last_meta: dict = {"ok": True, "mode": "bare", "va": int(pc), "pa": int(pc)}
    for off in range(int(size)):
        pa, meta = _translate_fetch_addr(env, int(pc) + int(off))
        last_meta = meta
        if pa is None:
            return None, meta
        value |= (int(env.memory.read_u8(int(pa))) & 0xFF) << (8 * int(off))
    return int(value), last_meta


def _recover_unavailable_instr(env, pc: int, instr: int, is_rvc: bool, ex_sum: int) -> int:
    if int(instr) != 0:
        return int(instr)
    fetch_size = 2 if bool(is_rvc) else 4
    raw_fetch, fetch_meta = _read_expected_fetch_raw(env, int(pc), fetch_size)
    if raw_fetch is None or not bool(fetch_meta.get("ok", False)):
        return int(instr)
    if bool(is_rvc):
        raw16 = int(raw_fetch) & 0xFFFF
        try:
            return int(expand_rvc(raw16)) & 0xFFFFFFFF
        except ValueError:
            return int(instr)
    return int(raw_fetch) & 0xFFFFFFFF
