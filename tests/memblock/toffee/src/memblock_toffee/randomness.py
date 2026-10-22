from __future__ import annotations

import hashlib
import random


class RandomStreams:
    """Create stable, independent random streams from one replay seed."""

    def __init__(self, seed: int) -> None:
        if seed < 0:
            raise ValueError("seed must be non-negative")
        self.seed = seed
        self._streams: dict[str, random.Random] = {}

    def stream(self, name: str) -> random.Random:
        if not name or name.strip() != name:
            raise ValueError("random stream name must be non-empty and trimmed")
        if name not in self._streams:
            digest = hashlib.sha256(
                f"{self.seed}\0{name}".encode("utf-8")
            ).digest()
            stream_seed = int.from_bytes(digest[:16], "big")
            self._streams[name] = random.Random(stream_seed)
        return self._streams[name]
