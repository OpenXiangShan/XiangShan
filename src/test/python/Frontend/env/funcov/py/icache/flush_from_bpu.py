from __future__ import annotations

from dataclasses import dataclass
from typing import Optional


FtqPtr = tuple[int, int]


@dataclass(frozen=True)
class BpuS3Flush:
    valid: Optional[int]
    flag: Optional[int]
    value: Optional[int]

    @property
    def active(self) -> bool:
        return self.valid is not None and int(self.valid) != 0

    @property
    def ptr(self) -> Optional[FtqPtr]:
        if self.flag is None or self.value is None:
            return None
        return int(self.flag), int(self.value)


def ftq_ptr_is_after(left: FtqPtr, right: FtqPtr) -> bool:
    """Mirror XiangShan CircularQueuePtr.isAfter for comparable FTQ pointers."""
    left_flag, left_value = int(left[0]), int(left[1])
    right_flag, right_value = int(right[0]), int(right[1])
    return bool((left_flag != right_flag) ^ (left_value > right_value))


def ftq_ptr_matches_or_before(flush: BpuS3Flush, current: Optional[FtqPtr]) -> Optional[bool]:
    """Return True when a BPU s3 flush targets current or an older FTQ entry."""
    if not flush.active:
        return False
    flush_ptr = flush.ptr
    if flush_ptr is None or current is None:
        return None
    return not ftq_ptr_is_after(flush_ptr, current)


def ftq_ptr_is_strictly_after_current(
    flush: BpuS3Flush, current: Optional[FtqPtr]
) -> Optional[bool]:
    if not flush.active:
        return False
    flush_ptr = flush.ptr
    if flush_ptr is None or current is None:
        return None
    return ftq_ptr_is_after(flush_ptr, current)
