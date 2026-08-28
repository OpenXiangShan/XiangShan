from __future__ import annotations


def fold_pc(pc: int, width: int = 10) -> int:
    """Match RTL XORFold(pc(VAddrBits - 1, 1), width)."""
    mask = (1 << int(width)) - 1
    value = int(pc) >> 1
    folded = 0
    while value:
        folded ^= value & mask
        value >>= int(width)
    return int(folded) & mask


__all__ = ["fold_pc"]
