#!/usr/bin/env python3

from __future__ import annotations

import contextlib
import io
import json
import os
import subprocess
import sys
import tempfile
import textwrap
import threading
import time
import unittest
from unittest import mock
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import run_regression  # noqa: E402


class RunRegressionTest(unittest.TestCase):
    def test_cancellation_stops_long_running_process(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            pid_path = Path(temporary) / "child.pid"
            cancellation_event = threading.Event()
            process_pids: list[int] = []

            def cancel_after_child_starts() -> None:
                deadline = time.monotonic() + 5.0
                while time.monotonic() < deadline:
                    try:
                        pids = [
                            int(value)
                            for value in pid_path.read_text(
                                encoding="utf-8"
                            ).split()
                        ]
                        if len(pids) != 2:
                            raise ValueError("expected leader and descendant PIDs")
                    except (FileNotFoundError, ValueError):
                        time.sleep(0.01)
                    else:
                        process_pids.extend(pids)
                        cancellation_event.set()
                        return

            canceller = threading.Thread(target=cancel_after_child_starts)
            canceller.start()
            started = time.monotonic()
            try:
                returncode, output, timed_out = run_regression._run_process(
                    [
                        sys.executable,
                        "-c",
                        textwrap.dedent(
                            """
                            import os
                            import pathlib
                            import signal
                            import subprocess
                            import sys
                            import time

                            descendant = subprocess.Popen(
                                [sys.executable, "-c", "import time; time.sleep(30)"]
                            )

                            def terminate(signum, frame):
                                descendant.wait(timeout=5)
                                signal.signal(signal.SIGTERM, signal.SIG_DFL)
                                os.kill(os.getpid(), signal.SIGTERM)

                            signal.signal(signal.SIGTERM, terminate)
                            marker = pathlib.Path(sys.argv[1])
                            pending = marker.with_suffix(".pending")
                            pending.write_text(
                                f"{os.getpid()} {descendant.pid}", encoding="utf-8"
                            )
                            pending.replace(marker)
                            time.sleep(30)
                            """
                        ),
                        str(pid_path),
                    ],
                    10.0,
                    None,
                    cancellation_event,
                )
            finally:
                canceller.join()

            elapsed = time.monotonic() - started
            self.assertTrue(cancellation_event.is_set())
            self.assertFalse(timed_out)
            self.assertIsNotNone(returncode)
            self.assertNotEqual(returncode, 0)
            self.assertLess(elapsed, 3.0)
            self.assertEqual(output, "")

            self.assertEqual(len(process_pids), 2)
            for process_pid in process_pids:
                with self.assertRaises(ProcessLookupError):
                    os.kill(process_pid, 0)

    def test_timeout_bytes_are_recorded_without_type_error(self) -> None:
        with mock.patch.object(
            run_regression,
            "_run_process",
            return_value=(None, "partialdiagnostic", True),
        ):
            result = run_regression.run_seed(
                Path("/bin/true"), 7, "constrained-random", 128, 1.0, True
            )
        self.assertEqual(result["status"], "timeout")
        self.assertIn("partialdiagnostic", result["output"])

    def test_default_worker_count_uses_measured_host_scaling(self) -> None:
        self.assertEqual(run_regression.DEFAULT_JOBS, 8)

    def test_parallel_workers_reject_a_shared_mem_direct_trace(self) -> None:
        environment = {"MEMBLOCK_MEM_DIRECT_TRACE_FILE": "shared-trace.log"}
        with self.assertRaisesRegex(
            run_regression.RegressionError, "unsafe with --jobs > 1"
        ):
            run_regression.validate_worker_environment(2, environment)
        run_regression.validate_worker_environment(1, environment)
        run_regression.validate_worker_environment(2, {})

    def test_main_rejects_parallel_trace_before_creating_results(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "results.json"
            stderr = io.StringIO()
            argv = [
                "run_regression.py",
                "--binary",
                "/bin/true",
                "--output",
                str(output),
                "--jobs",
                "2",
            ]
            with (
                mock.patch.object(sys, "argv", argv),
                mock.patch.dict(
                    os.environ,
                    {"MEMBLOCK_MEM_DIRECT_TRACE_FILE": "shared-trace.log"},
                    clear=False,
                ),
                contextlib.redirect_stderr(stderr),
            ):
                returncode = run_regression.main()
            self.assertEqual(returncode, 2)
            self.assertIn("unsafe with --jobs > 1", stderr.getvalue())
            self.assertFalse(output.exists())

    def test_constraint_interface_is_forwarded_only_to_constrained_random(self) -> None:
        with mock.patch.object(
            run_regression,
            "_run_process",
            return_value=(
                0,
                "MEMBLOCK_CONSTRAINED_RANDOM_PASS seed=3 transactions=128\n",
                False,
            ),
        ):
            result = run_regression.run_seed(
                Path("/bin/true"),
                3,
                "constrained-random",
                128,
                1.0,
                True,
                constraint_profile="spec",
                constraint_overrides=("tlb-flush=40", "latency=compact"),
            )
        self.assertIn("--constraints", result["command"])
        self.assertIn("spec", result["command"])
        self.assertIn("--constraint", result["command"])
        self.assertIn("tlb-flush=40", result["command"])

        with mock.patch.object(
            run_regression,
            "_run_process",
            return_value=(
                0,
                "MEMBLOCK_FRONTEND_BRIDGE_PASS seed=3 transactions=128\n",
                False,
            ),
        ):
            directed = run_regression.run_seed(
                Path("/bin/true"),
                3,
                "frontend-bridge",
                128,
                1.0,
                True,
                constraint_profile="corner",
                constraint_overrides=("tlb-flush=100",),
            )
        self.assertNotIn("--constraints", directed["command"])
        self.assertNotIn("--constraint", directed["command"])

    def test_constrained_random_has_no_prefix_action_floor(self) -> None:
        self.assertFalse(hasattr(run_regression, "MINIMUM_MIXED_TRANSACTIONS"))
        self.assertEqual(run_regression.DEFAULT_ACTIONS, 16384)
        self.assertEqual(run_regression.DEFAULT_TIMEOUT_SECONDS, 1800)

    def test_parses_constrained_random_summary(self) -> None:
        parsed = run_regression.parse_summary(
            "MEMBLOCK_CONSTRAINED_RANDOM_PASS seed=9 transactions=64 cycle=1200 "
            "lq=55+1/56 sq=30+0/30\n"
        )
        self.assertEqual(parsed["status"], "pass")
        self.assertEqual(parsed["seed"], 9)
        self.assertEqual(parsed["transactions"], 64)
        self.assertEqual(parsed["lq"], "55+1/56")

    def test_parses_frontend_bridge_summary(self) -> None:
        parsed = run_regression.parse_summary(
            "MEMBLOCK_FRONTEND_BRIDGE_PASS seed=13 transactions=32 cycle=441 "
            "requests=96 responses=128 request_stalls=161 response_stalls=106 "
            "source_credit_stalls=379 field_checks=3599\n",
            expected_scenario="frontend-bridge",
            expected_seed=13,
            expected_transactions=32,
        )
        self.assertEqual(parsed["status"], "pass")
        self.assertEqual(parsed["requests"], 96)
        self.assertEqual(parsed["source_credit_stalls"], 379)

    def test_parses_directed_boundary_failures(self) -> None:
        parsed = run_regression.parse_summary(
            "MEMBLOCK_VECTOR_GUEST_FAULT_BOUNDARY_FAIL seed=9 transactions=37 "
            "failures=4\n",
            expected_scenario="vector-guest-fault-boundary",
        )
        self.assertEqual(parsed["status"], "fail")
        self.assertEqual(parsed["transactions"], 37)
        self.assertEqual(parsed["failures"], 4)

    def test_rejects_missing_summary(self) -> None:
        with self.assertRaises(run_regression.RegressionError):
            run_regression.parse_summary("ordinary simulator output")

    def test_default_scenarios_are_constrained_random(self) -> None:
        self.assertEqual(set(run_regression.DEFAULT_SCENARIOS), {"constrained-random"})

    def test_rejects_conflicting_terminal_summaries(self) -> None:
        output = (
            "MEMBLOCK_CONSTRAINED_RANDOM_FAIL seed=7 transactions=20\n"
            "MEMBLOCK_CONSTRAINED_RANDOM_PASS seed=7 transactions=20\n"
        )
        with self.assertRaisesRegex(run_regression.RegressionError, "expected one"):
            run_regression.parse_summary(output, expected_scenario="constrained-random")

    def test_rejects_summary_for_wrong_scenario(self) -> None:
        output = "MEMBLOCK_FRONTEND_BRIDGE_PASS seed=7 transactions=20\n"
        with self.assertRaisesRegex(run_regression.RegressionError, "does not match"):
            run_regression.parse_summary(
                output,
                expected_scenario="constrained-random",
                expected_seed=7,
                expected_transactions=20,
            )

    def test_rejects_summary_without_expected_seed(self) -> None:
        output = "MEMBLOCK_CONSTRAINED_RANDOM_PASS transactions=20\n"
        with self.assertRaisesRegex(run_regression.RegressionError, "seed"):
            run_regression.parse_summary(
                output,
                expected_scenario="constrained-random",
                expected_seed=7,
                expected_transactions=20,
            )

    def test_campaigns_keep_sidecars_across_shared_parent_and_rerun(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            binary = root / "simulator"
            binary.write_text(
                "#!/usr/bin/env python3\n"
                "import sys\n"
                "args = sys.argv\n"
                "seed = args[args.index('--seed') + 1]\n"
                "actions = args[args.index('--actions') + 1]\n"
                "print(f'MEMBLOCK_SMOKE_PASS seed={seed} transactions={actions}')\n",
                encoding="utf-8",
            )
            binary.chmod(0o755)
            artifacts = [{"role": "binary", "path": binary.name}]
            for role in ("model", "rtl_metadata", "xspcomm"):
                (root / role).write_text("fixture", encoding="utf-8")
                artifacts.append({"role": role, "path": role})
            metadata = root / "runtime.json"
            metadata.write_text(json.dumps({
                "schema_version": 1,
                "artifacts": artifacts,
                "external_dependencies": [],
            }), encoding="utf-8")
            common = [
                "--runtime-metadata", str(metadata), "--scenarios", "smoke",
                "--actions", "256", "--jobs", "1",
            ]
            outputs = [root / name for name in ("first.json", "second.json", "bench.json")]
            commands = [
                [sys.executable, str(MEMBLOCK_ROOT / "scripts" / script),
                 "--output", str(output), *common, *options]
                for script, output, options in (
                    ("run_regression.py", outputs[0], ["--seeds", "1"]),
                    ("run_regression.py", outputs[1], ["--seeds", "1"]),
                    ("benchmark_tests.py", outputs[2], []),
                )
            ]
            environment = os.environ.copy()
            environment.pop("MEMBLOCK_MEM_DIRECT_TRACE_FILE", None)
            processes = []
            try:
                for command in commands:
                    processes.append(subprocess.Popen(
                        command, env=environment, stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT, text=True,
                    ))
                for process in processes:
                    output, _ = process.communicate(timeout=30)
                    self.assertEqual(process.returncode, 0, output)
            finally:
                for process in processes:
                    if process.poll() is None:
                        process.kill()
                    process.communicate()

            reports = {}
            run_ids = set()
            for output in outputs:
                document = json.loads(output.read_text(encoding="utf-8"))
                run_ids.add(document["run_id"])
                result = document["results"][0]
                report_path = Path(result["coverage_report"])
                report = json.loads(report_path.read_text(encoding="utf-8"))
                self.assertEqual(report_path.parent.name, document["run_id"])
                self.assertEqual(report["run_id"], document["run_id"])
                for name in ("scenario", "seed", "status", "summary", "command"):
                    self.assertEqual(report[name], result[name])
                reports[report_path] = report_path.read_bytes()
            self.assertEqual(len(run_ids), 3)
            self.assertEqual(len(reports), 3)

            # Reusing an aggregate filename must retain its previous reports.
            replay = subprocess.run(commands[0], env=environment, capture_output=True,
                                    text=True, timeout=30, check=False)
            self.assertEqual(replay.returncode, 0, replay.stdout + replay.stderr)
            rerun = json.loads(outputs[0].read_text(encoding="utf-8"))
            self.assertNotIn(rerun["run_id"], run_ids)
            for report_path, contents in reports.items():
                self.assertEqual(report_path.read_bytes(), contents)
            self.assertEqual(len(list((root / "coverage-reports").glob("*/*.json"))), 4)

    def test_coverage_report_rejects_invalid_campaign_namespace(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "reports"
            for run_id in ("", "../escape", "a" * 31, "G" * 32, "a" * 31 + "/"):
                with self.subTest(run_id=run_id), self.assertRaisesRegex(
                    run_regression.RegressionError, "valid campaign run id"
                ):
                    run_regression.write_coverage_report({}, root, 0, run_id=run_id)
            self.assertFalse(root.exists())

    def test_verifies_frozen_runtime_layout(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            artifacts = []
            for role, name in (
                ("binary", "memblock_sim"),
                ("model", "libUTMemBlock.so"),
                ("rtl_metadata", "rtl.json"),
                ("xspcomm", "libxspcomm.so.0.0.1"),
            ):
                artifact = root / name
                artifact.write_bytes(role.encode())
                if role == "binary":
                    artifact.chmod(0o555)
                artifacts.append(
                    {
                        "role": role,
                        "path": name,
                    }
                )
            dependency = root / "system-lib.so"
            dependency.write_bytes(b"system")
            metadata = root / "runtime.json"
            metadata.write_text(
                json.dumps(
                    {
                        "schema_version": 1,
                        "artifacts": artifacts,
                        "external_dependencies": [
                            {
                                "path": str(dependency.resolve()),
                            }
                        ],
                    }
                )
            )

            verified = run_regression.verify_runtime_metadata(metadata)
            self.assertEqual(verified["binary"], root / "memblock_sim")
            self.assertEqual(set(verified["artifacts"]), run_regression.RUNTIME_ROLES)

            (root / "libUTMemBlock.so").write_bytes(b"changed")
            verified = run_regression.verify_runtime_metadata(metadata)
            self.assertEqual(verified["binary"], root / "memblock_sim")

    def test_runtime_rejects_artifact_path_escape(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            outside = root.with_name(root.name + "-outside")
            outside.write_bytes(b"outside")
            try:
                metadata = root / "runtime.json"
                metadata.write_text(
                    json.dumps(
                        {
                            "schema_version": 1,
                            "artifacts": [
                                {
                                    "role": "binary",
                                    "path": "../" + outside.name,
                                }
                            ],
                            "external_dependencies": [],
                        }
                    )
                )
                with self.assertRaisesRegex(
                    run_regression.RegressionError, "local filename"
                ):
                    run_regression.verify_runtime_metadata(metadata)
            finally:
                os.unlink(outside)

if __name__ == "__main__":
    unittest.main()
