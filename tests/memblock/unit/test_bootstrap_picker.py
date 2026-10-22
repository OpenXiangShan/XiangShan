#!/usr/bin/env python3

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import bootstrap_picker  # noqa: E402


class BootstrapPickerTest(unittest.TestCase):
    def test_toolchain_lock_records_full_commits(self) -> None:
        tools = bootstrap_picker.load_toolchain()
        self.assertEqual(tools["picker"]["ref"], "refs/tags/v2.1.0")
        self.assertEqual(
            tools["picker"]["commit"],
            "7499f797b56be57cf950cd40f3d14e9eba133449",
        )
        self.assertEqual(tools["xcomm"]["ref"], "refs/tags/v1.0.0")
        self.assertEqual(
            tools["xcomm"]["commit"],
            "a5f5dae14f3cdc8f7b35dab7b5dc8c21576b4dbd",
        )
        self.assertEqual(tools["toffee"]["ref"], "master")
        self.assertEqual(
            tools["toffee"]["commit"],
            "f57dd6ffc295b9b8bad3e3b3ce040b8a849f5c91",
        )
        self.assertEqual(tools["toffee_test"]["ref"], "master")
        self.assertEqual(
            tools["toffee_test"]["commit"],
            "abec0edd93cba9f3d91ab1ef7e2fc30bf807eb00",
        )

    def test_missing_checkout_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaisesRegex(bootstrap_picker.BootstrapError, "not a git"):
                bootstrap_picker.checkout_revision(
                    Path(temporary) / "missing", "fixture", "HEAD"
                )

    def test_dirty_checkout_is_not_updated(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            checkout = Path(temporary) / "checkout"
            subprocess.run(["git", "init", "-q", str(checkout)], check=True)
            subprocess.run(
                ["git", "config", "user.email", "memblock@example.invalid"],
                cwd=checkout,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "MemBlock Test"],
                cwd=checkout,
                check=True,
            )
            (checkout / "tracked").write_text("content\n", encoding="utf-8")
            subprocess.run(["git", "add", "tracked"], cwd=checkout, check=True)
            subprocess.run(["git", "commit", "-qm", "fixture"], cwd=checkout, check=True)
            (checkout / "tracked").write_text("dirty\n", encoding="utf-8")
            with self.assertRaisesRegex(
                bootstrap_picker.BootstrapError, "dirty fixture checkout"
            ):
                bootstrap_picker.checkout_revision(checkout, "fixture", "HEAD")

    def test_remote_branch_is_fetched_and_resolved_to_expected_commit(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            origin = root / "origin.git"
            source = root / "source"
            checkout = root / "checkout"
            subprocess.run(["git", "init", "--bare", "-q", str(origin)], check=True)
            subprocess.run(["git", "init", "-q", str(source)], check=True)
            subprocess.run(
                ["git", "config", "user.email", "memblock@example.invalid"],
                cwd=source,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "MemBlock Test"],
                cwd=source,
                check=True,
            )
            (source / "tracked").write_text("first\n", encoding="utf-8")
            subprocess.run(["git", "add", "tracked"], cwd=source, check=True)
            subprocess.run(["git", "commit", "-qm", "first"], cwd=source, check=True)
            subprocess.run(["git", "branch", "-M", "master"], cwd=source, check=True)
            subprocess.run(
                ["git", "remote", "add", "origin", str(origin)],
                cwd=source,
                check=True,
            )
            subprocess.run(["git", "push", "-qu", "origin", "master"], cwd=source, check=True)
            subprocess.run(
                [
                    "git",
                    "--git-dir",
                    str(origin),
                    "symbolic-ref",
                    "HEAD",
                    "refs/heads/master",
                ],
                check=True,
            )
            subprocess.run(["git", "clone", "-q", str(origin), str(checkout)], check=True)

            (source / "tracked").write_text("second\n", encoding="utf-8")
            subprocess.run(["git", "add", "tracked"], cwd=source, check=True)
            subprocess.run(["git", "commit", "-qm", "second"], cwd=source, check=True)
            subprocess.run(["git", "push", "-q", "origin", "master"], cwd=source, check=True)
            expected = subprocess.run(
                ["git", "rev-parse", "HEAD"],
                cwd=source,
                check=True,
                capture_output=True,
                text=True,
            ).stdout.strip()

            actual = bootstrap_picker.checkout_revision(
                checkout, "fixture", "refs/remotes/origin/master", expected
            )
            self.assertEqual(actual, expected)
            self.assertEqual(
                expected,
                subprocess.run(
                    ["git", "rev-parse", "HEAD"],
                    cwd=checkout,
                    check=True,
                    capture_output=True,
                    text=True,
                ).stdout.strip(),
            )

    def test_metadata_is_not_rewritten_when_contents_match(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            metadata = Path(temporary) / "picker.json"
            self.assertTrue(bootstrap_picker.write_if_changed(metadata, "{}\n"))
            self.assertFalse(bootstrap_picker.write_if_changed(metadata, "{}\n"))
            self.assertEqual(metadata.read_text(encoding="utf-8"), "{}\n")


if __name__ == "__main__":
    unittest.main()
