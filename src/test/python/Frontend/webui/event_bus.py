from __future__ import annotations

import asyncio
import threading
import time
from collections import deque
from typing import Any, Deque, Dict, Optional, Set


class EventBus:
    def __init__(self, queue_size: int = 1024, history_size: int = 2048) -> None:
        self.queue_size = int(queue_size)
        self.history: Deque[Dict[str, Any]] = deque(maxlen=int(history_size))
        self._loop: Optional[asyncio.AbstractEventLoop] = None
        self._subs: Set[asyncio.Queue] = set()
        self._lock = threading.Lock()

    def attach_loop(self, loop: asyncio.AbstractEventLoop) -> None:
        self._loop = loop

    def recent(self, limit: int = 200) -> list[Dict[str, Any]]:
        if limit <= 0:
            return []
        return list(self.history)[-limit:]

    async def subscribe(self) -> asyncio.Queue:
        q: asyncio.Queue = asyncio.Queue(maxsize=self.queue_size)
        with self._lock:
            self._subs.add(q)
        return q

    async def unsubscribe(self, q: asyncio.Queue) -> None:
        with self._lock:
            if q in self._subs:
                self._subs.remove(q)

    def publish(self, evt: Dict[str, Any]) -> None:
        event = dict(evt)
        event.setdefault("ts", time.time())
        self.history.append(event)

        loop = self._loop
        if loop is None:
            return
        loop.call_soon_threadsafe(self._publish_async, event)

    def _publish_async(self, evt: Dict[str, Any]) -> None:
        with self._lock:
            subs = list(self._subs)
        for q in subs:
            if q.full():
                try:
                    q.get_nowait()
                except Exception:
                    pass
            try:
                q.put_nowait(evt)
            except Exception:
                pass

