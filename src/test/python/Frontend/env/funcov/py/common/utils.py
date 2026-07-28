from __future__ import annotations


def circular_distance(
    newer_flag: int,
    newer_value: int,
    older_flag: int,
    older_value: int,
    size: int,
) -> int:
    size = max(1, int(size))
    modulo = size * 2
    newer = (int(newer_flag) & 1) * size + (int(newer_value) % size)
    older = (int(older_flag) & 1) * size + (int(older_value) % size)
    return (newer - older) % modulo
