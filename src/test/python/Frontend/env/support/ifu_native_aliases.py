"""Same-cycle projections of frozen IFU/ICache RTL, using native probes only.

A projection preserves selected exception cause or a source-defined output;
it does not reconstruct raw TileLink input bits or invent an exported pin.
"""
from dataclasses import dataclass
from typing import Callable

_READER = Callable[[str], int | None]
_IFU = "Frontend_top.Frontend.inner_ifu."
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
LINE0_TL_EXCEPTION_PATHS = (_MAIN + "s1_tlException_reg_value",
                           "inner_icache.mainPipe.s1_tlException_reg_value")
LINE0_PRIOR_EXCEPTION_PATHS = (_MAIN + "s1_exception_0_value",
                              "inner_icache.mainPipe.s1_exception_0_value")
IFU_ENQUEUE_PATHS = (
    _IFU + "io_toIBuffer_bits_enqEnable",
    "inner_ifu.io_toIBuffer_bits_enqEnable",
    _IFU + "io_toIBuffer_bits_enqEnable_0",
    "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_enqEnable",
)
LAST0_PARENT_PATHS = (IFU_ENQUEUE_PATHS[2], _IFU + "select_0", _IFU + "select_1")
IFU_ENQUEUE_WIDTH = 36


@dataclass(frozen=True)
class NativeProjection:
    value: int
    expression: str
    source_values: dict[str, int]


def _read_native(reader: _READER, paths: tuple[str, ...]):
    for path in paths:
        assert "__Vtogcov__" not in path
        value = reader(path)
        if value is not None:
            return int(value), path
    return None, None


def read_line0_tl_cause(reader: _READER) -> NativeProjection | None:
    value, path = _read_native(reader, LINE0_TL_EXCEPTION_PATHS)
    if value is None or value not in (0, 3, 5):
        return None
    # A matching merged exception code alone cannot prove bus attribution:
    # itlb/pmp wins even when its code equals the bus cause. Require the
    # actual same-cycle higher-priority input to be visibly None.
    prior, prior_path = _read_native(reader, LINE0_PRIOR_EXCEPTION_PATHS)
    if prior != 0:
        return None
    # ExceptionType.None/Af/Hwe; reg || new preserves the first nonzero cause.
    return NativeProjection(value, "s1_exception(0)=None; s1_tlException(0).value",
                            {path: value, prior_path: prior})


def read_last_in_ftq(reader: _READER, slot: int) -> NativeProjection | None:
    if not 0 <= slot < IFU_ENQUEUE_WIDTH:
        raise ValueError("slot outside frozen DefaultConfig IFU enqueue width")
    value, path = _read_native(reader, (
        _IFU + f"io_toIBuffer_bits_isLastInFtqEntry_{slot}",
        "inner_ifu." + f"io_toIBuffer_bits_isLastInFtqEntry_{slot}",
        "Frontend_top.Frontend._inner_ifu_" + f"io_toIBuffer_bits_isLastInFtqEntry_{slot}",
    ))
    if value is not None:
        return NativeProjection(value, "native isLastInFtqEntry", {path: value}) if value in (0, 1) else None
    enq, enq_path = _read_native(reader, IFU_ENQUEUE_PATHS)
    if enq is None or not 0 <= enq < (1 << IFU_ENQUEUE_WIDTH):
        return None
    sources = {enq_path: enq}
    if slot == IFU_ENQUEUE_WIDTH - 1:
        return NativeProjection((enq >> slot) & 1, f"enq[{slot}]", sources)
    selected = []
    for index in (slot, slot + 1):
        value, path = _read_native(reader, (_IFU + f"select_{index}", "inner_ifu." + f"select_{index}"))
        if value not in (0, 1):
            return None
        selected.append(value)
        sources[path] = value
    result = ((enq >> slot) & 1) ^ (int(selected[0] == selected[1]) & ((enq >> (slot + 1)) & 1))
    return NativeProjection(result, f"enq[{slot}] ^ ((select[{slot}] == select[{slot + 1}]) & enq[{slot + 1}])", sources)
