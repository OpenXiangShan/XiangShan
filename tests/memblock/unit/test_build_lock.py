from __future__ import annotations

import os
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WRAPPER = ROOT / "scripts" / "with_build_lock.py"
MAKEFILE = ROOT / "Makefile"


def locked_command(lock: Path, *command: str) -> list[str]:
    return [sys.executable, str(WRAPPER), "--lock", str(lock), "--", *command]


class BuildLockTest(unittest.TestCase):
    def test_makefile_locks_all_shared_picker_build_stages(self) -> None:
        makefile = MAKEFILE.read_text(encoding="utf-8")
        self.assertIn(
            "PICKER_BUILD_LOCK := $(PREPARED_DIR)/.picker-build.lock", makefile
        )
        self.assertIn(
            "scripts/with_build_lock.py --lock $(PICKER_BUILD_LOCK) -- "
            "$(MAKE) --no-print-directory MEMBLOCK_PICKER_BUILD_LOCK_HELD=1",
            makefile,
        )
        self.assertIn(
            "\nexport-picker:\n\t+$(PICKER_LOCKED_MAKE) _export-picker\n",
            makefile,
        )
        self.assertIn(
            "\npicker-model:\n\t+$(PICKER_LOCKED_MAKE) _picker-model\n", makefile
        )
        self.assertIn(
            "\npicker-harness $(PICKER_TEST_BINARY):\n"
            "\t+$(PICKER_LOCKED_MAKE) _picker-harness\n",
            makefile,
        )
        self.assertIn(
            "\nfreeze-runtime:\n\t+$(PICKER_LOCKED_MAKE) _freeze-runtime\n",
            makefile,
        )
        self.assertIn(
            "export-picker _export-picker: $(PICKER_EXPORT_STAMP)", makefile
        )
        self.assertIn(
            "picker-model _picker-model: $(PICKER_MODEL_STAMP)", makefile
        )
        self.assertIn(
            "picker-harness _picker-harness: $(PICKER_TEST_BINARY)", makefile
        )
        self.assertIn(
            "freeze-runtime _freeze-runtime: $(RUNTIME_METADATA) "
            "$(FROZEN_RTL_METADATA)",
            makefile,
        )
        for target in (
            "stress-regression",
            "extended-regression",
            "final-regression",
            "long-final-regression",
            "endurance-regression",
            "benchmark-tests",
        ):
            self.assertIn(f"\n{target}: freeze-runtime\n", makefile)
        self.assertIn("--build-threads $(JOBS)", makefile)
        self.assertIn("compile NPROC=$(JOBS)", makefile)

    def test_propagates_command_status(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            lock = Path(directory) / "build.lock"
            result = subprocess.run(
                locked_command(lock, sys.executable, "-c", "raise SystemExit(23)"),
                check=False,
            )
            self.assertEqual(result.returncode, 23)
            self.assertTrue(lock.is_file())

    def test_two_commands_do_not_overlap(self) -> None:
        worker = (
            "import os,time; from pathlib import Path; "
            "path=Path(os.environ['MEMBLOCK_LOCK_TEST_LOG']); "
            "handle=path.open('a'); "
            "handle.write('start\\n'); handle.flush(); os.fsync(handle.fileno()); "
            "time.sleep(0.2); "
            "handle.write('end\\n'); handle.flush(); os.fsync(handle.fileno())"
        )
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            lock = root / "build.lock"
            log = root / "events.log"
            environment = os.environ.copy()
            environment["MEMBLOCK_LOCK_TEST_LOG"] = str(log)
            processes = [
                subprocess.Popen(
                    locked_command(lock, sys.executable, "-c", worker),
                    env=environment,
                )
                for _ in range(2)
            ]
            self.assertEqual([process.wait() for process in processes], [0, 0])
            self.assertEqual(
                log.read_text(encoding="utf-8").splitlines(),
                ["start", "end", "start", "end"],
            )

    def test_waiting_make_rechecks_target_after_lock(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            lock = root / "build.lock"
            (root / "source").write_text("source\n", encoding="utf-8")
            makefile = root / "fixture.mk"
            makefile.write_text(
                "artifact: source\n"
                "\t$(PYTHON) -c \"import time; from pathlib import Path; "
                "time.sleep(0.2); Path('runs').open('a').write('run\\\\n'); "
                "Path('artifact').write_text('done\\\\n')\"\n",
                encoding="utf-8",
            )
            command = locked_command(
                lock,
                "make",
                "--no-print-directory",
                "-f",
                str(makefile),
                f"PYTHON={sys.executable}",
                "artifact",
            )
            processes = [
                subprocess.Popen(command, cwd=root) for _ in range(2)
            ]
            self.assertEqual([process.wait() for process in processes], [0, 0])
            self.assertEqual(
                (root / "runs").read_text(encoding="utf-8").splitlines(),
                ["run"],
            )
            self.assertEqual(
                (root / "artifact").read_text(encoding="utf-8"), "done\n"
            )

    def test_sigkill_releases_lock(self) -> None:
        worker = (
            "import os,time; from pathlib import Path; "
            "Path(os.environ['MEMBLOCK_LOCK_TEST_READY']).write_text('ready'); "
            "time.sleep(60)"
        )
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            lock = root / "build.lock"
            ready = root / "ready"
            environment = os.environ.copy()
            environment["MEMBLOCK_LOCK_TEST_READY"] = str(ready)
            process = subprocess.Popen(
                locked_command(lock, sys.executable, "-c", worker),
                env=environment,
            )
            try:
                deadline = time.monotonic() + 5
                while not ready.exists() and time.monotonic() < deadline:
                    time.sleep(0.01)
                self.assertTrue(ready.exists())
                os.kill(process.pid, signal.SIGKILL)
                self.assertEqual(process.wait(timeout=5), -signal.SIGKILL)
                result = subprocess.run(
                    locked_command(lock, sys.executable, "-c", "pass"),
                    check=False,
                    timeout=5,
                )
                self.assertEqual(result.returncode, 0)
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait()


if __name__ == "__main__":
    unittest.main()
