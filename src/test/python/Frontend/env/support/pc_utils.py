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


def pc_from_ftq_start(start_pc: int, ftq_offset: int, is_rvc: bool) -> int:
    """Recover an instruction PC from the architectural FTQ context."""
    return (
        int(start_pc)
        + (int(ftq_offset) << 1)
        - (0 if bool(is_rvc) else 2)
    ) & 0xFFFFFFFFFFFFFFFF


def require_matching_foldpc(pc: int, observed_foldpc: int) -> int:
    expected = fold_pc(int(pc))
    if expected != int(observed_foldpc):
        raise AssertionError(
            "cfVec foldpc does not match FTQ-derived PC: "
            f"pc=0x{int(pc):x} expected_foldpc=0x{expected:x} "
            f"observed_foldpc=0x{int(observed_foldpc):x}"
        )
    return int(pc)


__all__ = ["fold_pc", "pc_from_ftq_start", "require_matching_foldpc"]
