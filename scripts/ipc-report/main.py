"""Downloads ipc / score artifact from Github actions and generates a report"""

import argparse
from dataclasses import dataclass
from itertools import count
import logging
from pathlib import Path
import re
from typing import TYPE_CHECKING, Callable, Literal
from zipfile import ZipFile

from modules.spec import is_spec

if TYPE_CHECKING:
    from modules.github import GitHub

OWNER = "OpenXiangShan"
REPO = "XiangShan"
EMU_WORKFLOW = "EMU Performance Test"


@dataclass
class Result:
    """Result of a testcase"""

    score: float
    coverage: float | None = None

    def diff_str(self, base: "Result") -> str:
        """Get the diff of the result compared to a base result"""
        if self.coverage != base.coverage:
            return "N/A (Coverage mismatch)"
        if base.score == 0:
            return "N/A"
        return f"{(self.score - base.score) / base.score * 100:.3f}%"

    def score_str(self) -> str:
        """Get the score as a string"""
        return f"{self.score:.3f}"


@dataclass
class Report:
    """Report of a workflow run"""

    run_id: int | None
    commit_sha: str | None
    results: dict[str, Result]

    def get_results(
        self, filter_func: Callable[[str], bool] | None = None
    ) -> dict[str, Result]:
        """Get the results of the report, optionally filtered by a function"""
        if filter_func is None:
            return self.results.copy()
        result = {k: v for k, v in self.results.items() if filter_func(k)}
        return result


def geomean(results: list[Result]) -> Result:
    """Calculate the geometric mean of a list of values"""
    score_prod = 1.0
    for r in results:
        score_prod *= r.score
    score = score_prod ** (1 / len(results)) if results else 0.0
    coverage = min(
        [r.coverage for r in results if r.coverage is not None], default=None
    )
    return Result(score=score, coverage=coverage)


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Downloads ipc / score artifact from Github actions and generates a report"
    )
    parser.add_argument(
        "--token", help="Github token to access the artifacts"
    )
    parser.add_argument(
        "--logging-level",
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        help="Set the logging level",
    )
    parser.add_argument("base", help="Base commit SHA / run id")
    parser.add_argument(
        "compare",
        nargs="+",
        help="List of commit SHAs / run ids to compare against the base",
    )
    return parser.parse_args()


def parse_emu_ipc_artifact(artifact: ZipFile) -> dict[str, Result]:
    """Parses the ipc artifact from Github actions and returns a dictionary of testcases and their IPC scores."""
    result: dict[str, Result] = {}
    for name in artifact.namelist():
        if not name.startswith("ipc-"):
            continue
        testcase = name.replace("ipc-", "")
        with artifact.open(name) as f:
            ipc = float(f.read().decode("utf-8").strip())
        # ipc does not care about coverage, set to 1.0 for unified interface
        result[testcase] = Result(score=ipc)

    return result


def parse_perf_score_txt(txt: str) -> dict[str, Result]:
    """Parses the score.txt from Github actions and returns a dictionary of testcases and their scores."""
    result: dict[str, Result] = {}
    for line in txt.splitlines():
        # match "id.name time ref_time score coverage"
        m = re.match(
            r"^\s*(\d+\.\w+)\s+[\d\.]+\s+[\d\.]+\s+([\d\.]+)\s+([\d\.]+)",
            line,
        )
        if m:
            testcase = m.group(1)
            score = float(m.group(2))
            coverage = float(m.group(3))
            result[testcase] = Result(score=score, coverage=coverage)

    return result

def parse_perf_score_artifact(artifact: ZipFile) -> dict[str, Result]:
    """Parses the score artifact from Github actions and returns a dictionary of testcases and their scores."""
    for name in artifact.namelist():
        if not (name.startswith("score") and name.endswith(".txt")):
            continue
        with artifact.open(name) as f:
            return parse_perf_score_txt(f.read().decode("utf-8").strip())
    logging.error("No score.txt found in the score artifact")
    return {}


def render_metadata(base: Report, compare: list[Report]) -> str:
    """Render a metadata table as markdown format"""
    rows = [
        "## Metadata",
        "",
        "| Revision | commit | Run ID |",
        "| --- | --- | --- |",
        f"| Base | {base.commit_sha} | {base.run_id} |",
    ]
    for i, meta in enumerate(compare):
        rows.append(f"| Compare {i+1} | {meta.commit_sha} | {meta.run_id} |")
    return "\n".join(rows)


def render_table(
    base: Report,
    compare: list[Report],
    filter_func: Callable[[str], bool] | None = None,
    mode: Literal["ipc", "score"] = "ipc",
) -> str:
    """Render a report table as markdown format"""

    def get_results(report: Report) -> dict[str, Result]:
        results = report.get_results(filter_func)
        geomean_all = geomean(list(results.values()))
        if mode == "score":
            results["GEOMEAN-int"] = geomean(
                [v for k, v in results.items() if is_spec(k, "int")]
            )
            results["GEOMEAN-fp"] = geomean(
                [v for k, v in results.items() if is_spec(k, "fp")]
            )
        results["GEOMEAN"] = geomean_all
        return results

    base_results = get_results(base)
    compare_results_list = [get_results(c) for c in compare]

    for testcase in base_results.keys():
        for compare_results in compare_results_list:
            if testcase not in compare_results:
                logging.warning(
                    "Testcase %s not found in compare result, skipping", testcase
                )

    header = ["Testcase", "Base"]
    for i in range(len(compare)):
        header.append(f"Compare {i+1}")
        header.append(f"Diff {i+1}")

    rows = []
    for testcase in base_results.keys():
        row = [testcase, base_results[testcase].score_str()]
        for compare_results in compare_results_list:
            if testcase in compare_results:
                row.append(compare_results[testcase].score_str())
                row.append(compare_results[testcase].diff_str(base_results[testcase]))
            else:
                row.append("N/A")
                row.append("N/A")
        rows.append("| " + " | ".join(row) + " |")

    return "\n".join(
        [
            "## Report",
            "",
            "| " + " | ".join(header) + " |",
            "| " + " | ".join(["---"] * len(header)) + " |",
        ]
        + rows
    )


@dataclass
class Worker:
    target: Literal["ipc", "score"]
    gh: "GitHub | None" = None

    @staticmethod
    def from_args(args: argparse.Namespace) -> "Worker":
        gh = None
        if args.token:
            from modules.github import GitHub

            gh = GitHub(args.token)

        if not args.base.isdigit():
            path = Path(args.base)
            if path.exists() and path.is_file() and path.suffix == ".txt":
                return Worker(target="score", gh=gh)
            return Worker(target="ipc", gh=gh)

        base_run_id = int(args.base)

        worker = Worker(target="score", gh=gh)
        artifact = worker.get_artifact_id(base_run_id)
        if artifact != -1:
            return worker

        worker = Worker(target="ipc", gh=gh)
        artifact = worker.get_artifact_id(base_run_id)
        if artifact != -1:
            return worker

        raise ValueError(f"Could not infer mode of artifact for base run {args.base}")

    def get_emu_run_id(self, commit_sha: str) -> int:
        """Get the workflow run id for a given commit SHA."""
        if self.gh is None:
            raise ValueError("--token must be provided for non-local mode")
        runs = self.gh.actions.list_workflow_runs(OWNER, REPO, head_sha=commit_sha)[
            "workflow_runs"
        ]
        runs = list(filter(lambda run: run["name"] == EMU_WORKFLOW, runs))
        for run in runs:
            if self.get_artifact_id(run["id"]) != -1:
                return run["id"]
        logging.error("No EMU performance run found for commit %s", commit_sha)
        return -1

    def get_artifact_id(self, run_id: int) -> int:
        """A wrapper to get all artifacts for a workflow run, handling pagination"""
        if self.gh is None:
            raise ValueError("--token must be provided for non-local mode")
        artifacts = []
        for artifact_page in count(1):
            artifacts_page = self.gh.actions.list_workflow_run_artifacts(
                OWNER,
                REPO,
                run_id,
                page=artifact_page,
            )["artifacts"]
            if not artifacts_page:
                break
            artifacts.extend(artifacts_page)
        artifacts = list(
            filter(
                lambda x: not x["expired"]
                and x["name"].startswith(self.target),
                artifacts,
            )
        )
        if not artifacts:
            logging.error("No artifacts found for run id %s", run_id)
            return -1
        return artifacts[0]["id"]

    def resolve_ident(self, ident: str) -> tuple[ZipFile | bytes, int, str]:
        """Get the (artifact, run_id, commit_sha) tuple for a given ident(either local path, commit sha, or run_id)."""
        # is local path?
        if not ident.isdigit():
            path = Path(ident)
            if path.exists() and path.is_file() and path.suffix == ".txt":
                return path.read_bytes(), -1, ident

        if self.gh is None:
            raise ValueError("--token must be provided for non-local mode")

        # is commit sha or run id?
        if not ident.isdigit():
            ident = self.gh.commits.get_commit(OWNER, REPO, ident)["sha"]
            run_id = self.get_emu_run_id(ident)
            if run_id == -1:
                raise ValueError(f"Could not find run id for commit {ident}")
        else:
            run_id = int(ident)
            ident = self.gh.actions.get_workflow_run(OWNER, REPO, run_id)["head_sha"]

        artifact = self.get_artifact_id(run_id)
        if artifact != -1:
            return self.gh.actions.download_artifact(OWNER, REPO, artifact), run_id, ident

        raise ValueError(f"Could not infer mode of artifact for run {ident}")

    def get_report(self, ident: str) -> Report:
        artifact, run_id, commit_sha = self.resolve_ident(ident)
        match self.target:
            case "ipc":
                if isinstance(artifact, ZipFile):
                    return Report(run_id=run_id, commit_sha=commit_sha, results=parse_emu_ipc_artifact(artifact))
                else:
                    raise ValueError(f"Expected a ZipFile for ipc artifact, got {type(artifact)}")
            case "score":
                if isinstance(artifact, bytes):
                    return Report(run_id=run_id, commit_sha=commit_sha, results=parse_perf_score_txt(artifact.decode("utf-8").strip()))
                elif isinstance(artifact, ZipFile):
                    return Report(run_id=run_id, commit_sha=commit_sha, results=parse_perf_score_artifact(artifact))
                else:
                    raise ValueError(f"Expected a ZipFile or bytes for score artifact, got {type(artifact)}")

def main() -> None:
    """Entry point"""
    args = parse_args()
    logging.basicConfig(level=getattr(logging, args.logging_level))
    worker = Worker.from_args(args)

    base = worker.get_report(args.base)
    compare = [worker.get_report(c) for c in args.compare]

    print(render_metadata(base, compare))
    print()

    if any(testcase.startswith("legacy-") for testcase in base.results.keys()):
        print("<details>")
        print("<summary>Legacy Report</summary>")
        print()
        print(
            render_table(
                base,
                compare,
                filter_func=lambda k: k.startswith("legacy-"),
                mode=worker.target,
            )
        )
        print()
        print("</details>")
        print()

    print(
        render_table(
            base,
            compare,
            filter_func=lambda k: not k.startswith("legacy-"),
            mode=worker.target,
        )
    )
    print()


if __name__ == "__main__":
    main()
