import logging
import threading
import time
from pathlib import Path


class Lock:
    def try_acquire(self) -> bool:
        raise NotImplementedError

    def release(self) -> None:
        raise NotImplementedError

class FakeLock(Lock):
    def try_acquire(self) -> bool:
        return True

    def release(self) -> None:
        pass

class Heartbeat(Lock):
    def __init__(self, name: str, parent_path: Path, interval: float):
        if not name.isidentifier():
            raise ValueError(f"Name {name} is not a valid identifier")
        if not parent_path.is_dir():
            raise ValueError(f"Parent directory of {parent_path} does not exist")
        if not type(interval) in (int, float):
            raise ValueError(f"Interval {interval} is not a number")
        if interval <= 0:
            raise ValueError(f"Interval {interval} must be positive")

        self.name = name
        self.lock_path = parent_path / f".{name}.lock"
        self.heartbeat_path = parent_path / f".{name}.heartbeat"
        self.interval = interval
        self.thread = None
        self.stop_event = threading.Event()
        self.lock_owned = False
        # If lock dir exists but heartbeat file is not yet written, treat as alive briefly.
        self.acquire_grace = max(1.0, float(interval))

    def __is_alive(self) -> bool:
        # check the write timestamp of the heartbeat file
        curr = time.time()
        if self.heartbeat_path.exists():
            last = self.heartbeat_path.stat().st_mtime
            return curr - last < 2 * self.interval

        # Grace window: lock dir may be freshly acquired before heartbeat is written.
        if self.lock_path.exists():
            lock_last = self.lock_path.stat().st_mtime
            return curr - lock_last < self.acquire_grace

        return False

    def __heartbeat(self) -> None:
        try:
            self.heartbeat_path.touch(exist_ok=True)
        except OSError:
            logging.warning("Failed to update heartbeat file: %s", self.heartbeat_path)

    def __loop(self) -> None:
        # Should be called only by threading.Thread
        while not self.stop_event.is_set():
            self.__heartbeat()
            if self.stop_event.wait(self.interval):
                break

    def __start(self) -> None:
        # Should be called only after acquiring the lock
        self.stop_event.clear()
        self.thread = threading.Thread(
            target=self.__loop,
            name="heartbeat",
            daemon=True,
        )
        self.thread.start()

    def __stop(self) -> None:
        # Should be called only after acquiring the lock
        if self.thread is not None:
            self.stop_event.set()
            self.thread.join()
            self.thread = None

        self.heartbeat_path.unlink(missing_ok=True)

    def try_acquire(self) -> bool:
        if self.lock_owned:
            return True

        while True:
            try:
                self.lock_path.mkdir()
                # Publish liveness immediately after lock acquisition to avoid stale reclaim races.
                self.__heartbeat()
                self.__start()
                self.lock_owned = True
                return True
            except FileExistsError:
                if self.__is_alive():
                    return False
                # The old owner appears stale; try to reclaim lock and retry.
                try:
                    self.lock_path.rmdir()
                except (
                    FileNotFoundError
                ):  # Someone else removed the lock; retry acquisition.
                    pass
                except OSError:  # Cannot safely reclaim the lock.
                    return False
                # Loop will retry acquiring the lock.

    def release(self) -> None:
        if not self.lock_owned:
            return

        try:
            self.__stop()
            self.lock_path.rmdir()
        except FileNotFoundError:
            pass
        except OSError:
            logging.warning("Failed to remove heartbeat lock dir: %s", self.lock_path)
        finally:
            self.lock_owned = False
