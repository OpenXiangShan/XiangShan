import argparse
from enum import Enum
import hashlib
import json
import logging
from math import isnan
from multiprocessing import Process, Queue
import os
from pathlib import Path
import random
import re
import time

from modules.gcpt import GCPT
from modules.lock import Heartbeat, FakeLock
from modules.server import Server
from modules.spec import Spec, get_benchmarks, get_int_benchmarks, get_fp_benchmarks
from modules.types import EmuConfig, FreeCoreInfo
from modules.tracker import Tracker
from modules.utils import geomean

SERVER_POOL = [
    "node003",
    "node004",
    "node005",
    "node006",
    "node007",
    "node008",
    "node009",
    "node020",
    "node021",
    "node022",
    "node023",
    "node024",
    "node025",
    "node026",
    "node027",
    "node028",
    "node034",
    "node036",
    "node037",
    "node038",
    "node039",
    "node040",
    "node041",
    "node042",
    "open06",
    "open07",
    "open08",
    "open09",
    "open10",
    "open11",
    "open12",
    "open13",
    "open14",
    "open15",
    "open23",
    "open24",
    "open25",
    "open26",
    "open27",
]

HEARTBEAT_INTERVAL = 60
PROFILE_DIR = Path(__file__).parent / "profile" / "data"

RETRY_LIMIT = 3


class XiangShan:
    class Mode(Enum):
        CKPT_JSON = 0
        WORKLOAD_LIST = 2

    def __init__(
        self,
        json_path: Path,
        result_path: Path,
        benchmarks: str,
        gcpt_path: Path | None = None,
    ):
        self.gcpt_path = gcpt_path
        self.json_path = json_path
        self.result_path = result_path
        self.servers: list[Server] = []

        # No checkpoint directory: json_path is a workload list (one image path per line)
        if gcpt_path is None:
            self.mode = XiangShan.Mode.WORKLOAD_LIST
            logging.info(
                "GCPT path is not provided, treating %s as a workload list", json_path
            )
            self.__init_from_workload_list(json_path)
            return

        self.mode = XiangShan.Mode.CKPT_JSON
        self.ckpt_version = self.__infer_ckpt_version()

        with json_path.open("r", encoding="utf-8") as f:
            self.benchmarks = json.load(f)

        profile_path = PROFILE_DIR / f"{self.ckpt_version}.json"
        if profile_path.is_file():
            with profile_path.open("r", encoding="utf-8") as f:
                self.profile = json.load(f)
        else:
            self.profile = {}

        if benchmarks != "":
            benchmark_filter = benchmarks.replace(" ", "").split(",")

            # expand alias
            if m := re.match(r"int(\d\d)", " ".join(benchmark_filter)):
                benchmark_filter.extend(get_int_benchmarks(Spec.Version(m.group(1))))
            if m := re.match(r"fp(\d\d)", " ".join(benchmark_filter)):
                benchmark_filter.extend(get_fp_benchmarks(Spec.Version(m.group(1))))

            self.benchmarks = {
                k: v
                for k, v in self.benchmarks.items()
                if any(k.startswith(prefix) for prefix in benchmark_filter)
            }

        self.checkpoints: list[GCPT] = []
        for benchmark_name, benchmark_config in self.benchmarks.items():
            benchmark_profile = self.profile.get(benchmark_name, {})
            for point, weight in benchmark_config["points"].items():
                self.checkpoints.append(
                    GCPT(
                        gcpt_path=gcpt_path,
                        result_path=result_path,
                        benchmark=benchmark_name,
                        checkpoint=point,
                        weight=float(weight),
                        eta=benchmark_profile.get(point, 0),
                    )
                )

    def __init_from_workload_list(self, workload_list: Path) -> None:
        self.ckpt_version = None
        self.benchmarks = {}
        self.checkpoints = []
        seen: set[str] = set()
        with workload_list.open("r", encoding="utf-8") as f:
            for lineno, raw in enumerate(f, 1):
                line = raw.strip()
                if line == "" or line.startswith("#"):
                    continue
                image = Path(line).expanduser()
                if not image.is_file():
                    raise FileNotFoundError(
                        f"workload list {workload_list}:{lineno} is not a file: {image}"
                    )
                gcpt = GCPT.from_image(image, self.result_path)
                name = str(gcpt)
                if name in seen:
                    raise ValueError(f"duplicate workload in list: {image}")
                seen.add(name)
                self.checkpoints.append(gcpt)

        if not self.checkpoints:
            raise ValueError(f"workload list has no images: {workload_list}")

        logging.info("Loaded %d workloads", len(self.checkpoints))

    def __infer_ckpt_version(self) -> str:
        if self.gcpt_path is None:
            raise ValueError(
                "__infer_ckpt_version should not be called when gcpt_path is None"
            )
        return self.gcpt_path.parent.name

    def __infer_spec_version(self) -> Spec.Version | None:
        if self.gcpt_path is None:
            raise ValueError(
                "__infer_ckpt_version should not be called when gcpt_path is None"
            )

        # try infer from dir name in gcpt_path
        if m := re.search(r"spec(\d\d)", str(self.gcpt_path)):
            return Spec.Version(m.group(1))

        # try infer from benchmarks
        with self.json_path.open("r", encoding="utf-8") as f:
            benchmarks = json.load(f)
        for version in Spec.Version:
            if any(probe in benchmarks for probe in get_benchmarks(version)):
                return version

        return None

    def __init_servers(
        self,
        emu_path: Path,
        nemu_so_path: Path | None,
        server_list: str,
    ) -> None:
        # Do not use open servers unless explicitly specified, as they are too slow
        if server_list == "all":
            server_pool = list(filter(lambda s: s.startswith("node"), SERVER_POOL))
        elif server_list == "open":
            server_pool = list(filter(lambda s: s.startswith("open"), SERVER_POOL))
            desired_server_num = min(len(self.checkpoints) // 64 + 1, len(server_pool))
            server_pool = random.sample(server_pool, k=desired_server_num)
        elif server_list == "node" or server_list == "":
            server_pool = list(filter(lambda s: s.startswith("node"), SERVER_POOL))
            desired_server_num = min(len(self.checkpoints) // 64 + 1, len(server_pool))
            server_pool = random.sample(server_pool, k=desired_server_num)
        else:
            server_pool = server_list.replace(" ", "").split(",")
            for server in server_pool:
                if server not in SERVER_POOL:
                    raise ValueError(f"Server {server} is not in the server pool")

        self.servers = [
            Server(hostname, emu_path, self.tracker, nemu_so_path)
            for hostname in server_pool
        ]

        open_server = [s for s in self.servers if s.hostname.startswith("open")]
        if open_server and self.mode == XiangShan.Mode.WORKLOAD_LIST:
            logging.warning(
                "Workload-list images are used as-is on remote hosts; "
                "open servers do not share node NFS. Ensure every listed path "
                "is visible on the selected servers."
            )
        if open_server:
            logging.info("Using open servers, initializing binaries and libs...")
            target_result_path = Path(
                str(self.result_path).replace(
                    "/nfs/home/cirunner", "/nfs/home/ci-runner"
                )
            )
            target_emu_path = target_result_path / emu_path.name
            target_nemu_so_path = None

            open_server[0].initialize_open(emu_path, target_emu_path)

            if nemu_so_path is not None:
                target_nemu_so_path = target_result_path / nemu_so_path.name
                open_server[0].initialize_open(nemu_so_path, target_nemu_so_path)

            for server in open_server:
                server.emu_path = target_emu_path
                server.nemu_so_path = target_nemu_so_path

    def __init_tracker(self):
        self.tracker = Tracker(
            total=len(self.checkpoints),
            keys=["assigned", "completed", "failed"],
            with_keys=False,
        )

    def __reorder_by_eta(self, checkpoints: list[GCPT] | None = None):
        if not hasattr(self, "profile") or not self.profile:
            return
        checkpoints = self.checkpoints if checkpoints is None else checkpoints
        logging.info(
            "Profile found for %s, sorting checkpoints by ETA", self.ckpt_version
        )
        logging.debug(
            "Origin first 5:\n\t%s",
            "\n\t".join([f"{str(c)} ({c.eta})" for c in checkpoints[:5]]),
        )
        checkpoints.sort(key=lambda x: x.eta, reverse=True)
        logging.debug(
            "Sorted first 5:\n\t%s",
            "\n\t".join([f"{str(c)} ({c.eta})" for c in checkpoints[:5]]),
        )

    def __run(
        self,
        emu_config: EmuConfig,
    ) -> None:
        self.tracker.info(
            "Start running %d checkpoints on %d servers",
            len(self.checkpoints),
            len(self.servers),
        )
        self.tracker.debug(
            "Server list: %s", ", ".join(map(lambda s: s.hostname, self.servers))
        )
        unassigned_checkpoints = self.checkpoints.copy()
        last_pending_count = 0

        def poll_servers() -> bool:
            nonlocal last_pending_count
            pending = False
            all_pending = []
            for server in self.servers:
                success, fail, pending_list = server.poll()
                all_pending.extend(pending_list)
                retry_count = 0
                for task in fail:
                    gcpt = task.gcpt
                    if gcpt is None:
                        self.tracker.error(
                            "Failed task %s has no checkpoint reference", task.name
                        )
                        continue
                    gcpt.clear_state()
                    match gcpt.refresh_state():
                        case GCPT.State.NONE | GCPT.State.RUNNING:
                            if not gcpt.retry(RETRY_LIMIT):
                                self.tracker.error(
                                    "%s has exceeded retry limit (%d), treat as failed, rerun the script if needed",
                                    gcpt,
                                    RETRY_LIMIT,
                                )
                                continue
                            self.tracker.warning(
                                "%s seems to have failed with retryable error (e.g. network failure), retrying",
                                gcpt,
                            )
                            retry_count += 1
                            # insert to the front to retry it ASAP
                            unassigned_checkpoints.insert(0, gcpt)
                        case _:
                            self.tracker.error(
                                "%s failed with non-retryable error", gcpt
                            )

                self.tracker.step("completed", len(success) + len(fail) - retry_count)
                self.tracker.step("failed", len(fail) - retry_count)
                self.tracker.step("assigned", -retry_count)

                if len(pending_list) > 0:
                    pending = True

            pending_count = len(all_pending)
            if (
                not unassigned_checkpoints
                and 0 < len(all_pending) <= 5
                and last_pending_count != pending_count
            ):
                self.tracker.info(
                    "Remaining checkpoints: %s",
                    ", ".join(t.name for t in all_pending),
                )
            last_pending_count = pending_count
            return pending

        def assign(gcpt: GCPT) -> None:
            # check state from disk
            state = gcpt.refresh_state()
            if emu_config.dry_run:
                state = GCPT.State.NONE  # ignore existing state in dry-run mode
            match state:
                case GCPT.State.RUNNING:
                    self.tracker.warning(
                        "%s is RUNNING, resetting it",
                        gcpt,
                    )
                    state = GCPT.State.NONE

                case GCPT.State.FINISHED | GCPT.State.ABORTED:
                    self.tracker.info(
                        "%s is %s, skipping",
                        gcpt,
                        state,
                    )
                    self.tracker.step("assigned", 1)
                    self.tracker.step("completed", 1)
                    if state == GCPT.State.ABORTED:
                        self.tracker.step("failed", 1)
                    return

            # loop until task is assigned
            assigned = False
            while not assigned:
                # check completion
                poll_servers()
                # assign task to the first available server
                free_server = None
                free_cores = FreeCoreInfo.none()
                # check if a server has enough cached free core
                for server in self.servers:
                    free_cores = server.get_cached_free_cores(emu_config.threads)
                    if free_cores.free:
                        free_server = server
                        self.tracker.debug("Get cached free cores")
                        break
                # no, check if a server can alloc enough free core
                else:
                    for server in self.servers:
                        free_cores = server.get_free_cores(emu_config.threads)
                        if free_cores.free:
                            self.tracker.debug("Allocated free cores")
                            free_server = server
                            break
                # still no, wait and retry
                if free_server is None:
                    self.tracker.debug("No available server, waiting for 60 seconds...")
                    time.sleep(60)
                    continue

                # start job
                free_server.run_gcpt(gcpt, emu_config, free_cores)
                # shuffle for load balancing
                random.shuffle(self.servers)
                assigned = True
                self.tracker.step("assigned", 1)

        pending = True
        while unassigned_checkpoints or pending:
            if unassigned_checkpoints:
                assign(unassigned_checkpoints.pop(0))
                if not unassigned_checkpoints:
                    self.tracker.info(
                        "All checkpoints assigned, waiting for completion..."
                    )

            pending = poll_servers()
            if not unassigned_checkpoints and pending:
                self.tracker.debug("Still running, checking again in 60 seconds...")
                time.sleep(60)

        self.tracker.info("All checkpoints completed")

    def __stop(self):
        logging.info("Stopping all servers...")
        for server in self.servers:
            server.stop()

    def run(
        self,
        emu_path: Path,
        nemu_so_path: Path | None,
        server_list: str,
        emu_config: EmuConfig,
    ) -> None:
        try:
            self.__init_tracker()
            self.__init_servers(
                emu_path,
                nemu_so_path,
                server_list,
            )
            self.__reorder_by_eta()
            self.__run(emu_config)
        except KeyboardInterrupt as e:
            logging.info("SIGINT received")
            self.__stop()
            raise KeyboardInterrupt("Run interrupted by user") from e
        except Exception as e:
            logging.critical("Critical failure")
            self.__stop()
            raise e

    def report(
        self,
        frequency: float,
        override_version: str | None = None,
    ) -> None:
        # collect checkpoint -> benchmark, i.e. astar_biglakes_2972, astar_biglakes_3421 -> astar_biglakes
        result_queue = Queue()
        failed_queue = Queue()

        def collect_checkpoints(checkpoints: list[GCPT]) -> None:
            weighted_cpi_sum = 0.0
            weight_sum = 0.0

            for gcpt in checkpoints:
                cpi = gcpt.get_cpi()
                if cpi is None:
                    logging.warning("No valid result for checkpoint %s, skipping", gcpt)
                    failed_queue.put(
                        str(gcpt)
                        if self.mode == XiangShan.Mode.CKPT_JSON
                        else str(gcpt.bin_path)
                    )
                    continue
                # do not need lock as each threading is responsible for a different benchmark
                weighted_cpi_sum += cpi * gcpt.weight
                weight_sum += gcpt.weight

            # in CKPT_JSON mode, checkpoints[] are guaranteed to be in the same benchmark, so we can just use checkpoints[0].benchmark
            # in WORKLOAD_LIST mode, we don't care about it
            result_queue.put((checkpoints[0].benchmark, weighted_cpi_sum, weight_sum))

        if self.mode == XiangShan.Mode.CKPT_JSON:
            processes = [
                Process(
                    target=collect_checkpoints,
                    args=(
                        [
                            ckpt
                            for ckpt in self.checkpoints
                            if ckpt.benchmark == benchmark
                        ],
                    ),
                )
                for benchmark in self.benchmarks.keys()
            ]
        else:
            threads = (
                len(self.checkpoints) // 16 + 1
            )  # magic number: each process handles 16 checkpoints
            processes = [
                Process(
                    target=collect_checkpoints, args=(self.checkpoints[i::threads],)
                )
                for i in range(threads)
            ]
        for p in processes:
            p.start()
        for p in processes:
            p.join()

        failed_checkpoints = []
        while not failed_queue.empty():
            failed_checkpoints.append(failed_queue.get())

        if self.mode == XiangShan.Mode.CKPT_JSON:
            benchmark_weighted_cpis = {}
            benchmark_weights = {}
            while not result_queue.empty():
                benchmark, weighted_cpi, weight = result_queue.get()
                if weight == 0:
                    logging.warning(
                        "Total weight is 0 for benchmark %s, skipping", benchmark
                    )
                benchmark_weighted_cpis[benchmark] = weighted_cpi
                benchmark_weights[benchmark] = weight

            benchmark_times = {}
            for benchmark, weight in benchmark_weights.items():
                if weight == 0:
                    benchmark_times[benchmark] = float("nan")
                    continue

                benchmark_times[benchmark] = (  # weighted_avg_cpi * inst / freq
                    benchmark_weighted_cpis[benchmark]
                    / weight
                    * float(self.benchmarks[benchmark]["insts"])
                    / (frequency * 1e9)
                )

            version = (
                Spec.Version(override_version)
                if override_version is not None
                else self.__infer_spec_version()
            )

            spec = Spec(version) if version is not None else None

            def collect_benchmark_group(
                group: str,
            ) -> tuple[
                float, float, float, float
            ]:  # run_time, ref_time, score, coverage
                run_time = 0.0
                ref_time = spec.get_ref_time(group) if spec is not None else None
                if ref_time is None:
                    ref_time = float("nan")

                weighted_coverage_sum = 0.0
                instruction_sum = 0.0

                for benchmark in self.benchmarks.keys():
                    if not benchmark.startswith(group):
                        continue
                    run_time += benchmark_times[benchmark]
                    weighted_coverage_sum += benchmark_weights[benchmark] * float(
                        self.benchmarks[benchmark]["insts"]
                    )
                    instruction_sum += float(self.benchmarks[benchmark]["insts"])

                if instruction_sum == 0:
                    logging.warning(
                        "Total instruction count is 0 for benchmark group %s, skipping",
                        group,
                    )
                    return run_time, ref_time, float("nan"), float("nan")

                coverage = weighted_coverage_sum / instruction_sum
                score = ref_time / run_time / frequency

                return run_time, ref_time, score, coverage

            def render_line(
                name: str,
                run_time: float,
                ref_time: float,
                score: float,
                coverage: float,
            ) -> None:
                if spec is not None:
                    print(
                        f"{name:<19s} {run_time:>8.3f} {ref_time:>8.0f} {score:>8.3f} {coverage:>8.3f}",
                    )
                else:
                    print(f"{name:<19s} {run_time:>8.3f} {coverage:>8.3f}")

            def render_groups(
                name: str, group_names: list[str]
            ) -> tuple[list[float], list[float]]:
                scores = []
                coverages = []
                for group in group_names:
                    fullname = (
                        spec.get_benchmark_fullname(group)
                        if spec is not None
                        else group
                    )
                    run_time, ref_time, score, coverage = collect_benchmark_group(group)
                    scores.append(score)
                    coverages.append(coverage)
                    render_line(fullname, run_time, ref_time, score, coverage)
                render_line(
                    f"{name}/GHz",
                    float("nan"),
                    float("nan"),
                    geomean(scores),
                    float("nan"),
                )
                return scores, coverages

            print("======================== Score ========================")
            if spec is not None:
                print("                        time ref_time    score coverage")
                int_scores, int_coverages = render_groups(
                    spec.get_int_name(), spec.get_int_benchmarks()
                )
                fp_scores, fp_coverages = render_groups(
                    spec.get_fp_name(), spec.get_fp_benchmarks()
                )
                final_name = spec.get_name()
                final_geomean = geomean(int_scores + fp_scores)
                render_line(
                    final_name, float("nan"), float("nan"), final_geomean, float("nan")
                )
                print()
                print(f"{final_name}/GHz:    {final_geomean:.3f}")
                print(
                    f"{final_name}@{frequency:2.1f}GHz: {final_geomean * frequency:.3f}"
                )
                final_coverage = min(
                    c for c in int_coverages + fp_coverages if not isnan(c)
                )
            else:
                print("                        time coverage")
                scores, coverages = render_groups(
                    "All Benchmarks", list(self.benchmarks.keys())
                )
                final_coverage = min(c for c in coverages if not isnan(c))
            print()
            print("================ Other Information ===============")
            final_checkpoints = len(self.checkpoints)
            final_success_checkpoints = final_checkpoints - len(failed_checkpoints)
            print(f"Checkpoint Json : {self.json_path}")
            print(f"Ckpt Version    : {self.ckpt_version}")
            if spec is not None:
                print(f"SPEC Version    : {spec.version.name}")
            print(f"DRAMSIM3 Config : {self.checkpoints[0].get_dramsim3_config()}")
            print(f"Data Directory  : {self.result_path}")
            print(f"Minimal Coverage: {final_coverage:.2f}/1.00")
            print(f"Checkpoints     : {final_success_checkpoints}/{final_checkpoints}")
            print()
        else:
            print("================ Information ===============")
            final_checkpoints = len(self.checkpoints)
            final_success_checkpoints = final_checkpoints - len(failed_checkpoints)
            print(f"Workload List  : {self.json_path}")
            print(f"DRAMSIM3 Config: {self.checkpoints[0].get_dramsim3_config()}")
            print(f"Data Directory : {self.result_path}")
            print(f"Workloads      : {final_success_checkpoints}/{final_checkpoints}")
            print()
        print("=============== Failed Workloads ===============")
        print(json.dumps(failed_checkpoints, indent=2, separators=(",", ": ")))

    def reset_running_gcpt(self):
        num = 0
        for gcpt in self.checkpoints:
            state = gcpt.refresh_state()
            if state == GCPT.State.RUNNING:
                logging.info("Resetting GCPT %s", gcpt)
                num += 1
                gcpt.stdout_path.unlink()
                gcpt.stderr_path.unlink()
                gcpt.db_path.unlink(missing_ok=True)
                gcpt.result_path.rmdir()
                gcpt.clear_state()
        logging.info("Reset %d RUNNING GCPTs", num)


def main():
    parser = argparse.ArgumentParser(description="Performance regression script")

    # general task configs
    parser.add_argument(
        "--gcpt-path",
        type=str,
        help="Checkpoint directory. Omit to treat --json-path as a workload list",
    )
    parser.add_argument(
        "--json-path",
        type=str,
        required=True,
        help="Checkpoint JSON, or workload list if --gcpt-path is omitted",
    )
    parser.add_argument(
        "--result-path", type=str, required=True, help="Path to store the results"
    )

    # run emu configs
    parser.add_argument("--emu-path", type=str, help="Path to the emu")
    parser.add_argument("--nemu-so-path", type=str, help="Path to NEMU diff so")
    parser.add_argument(
        "--warmup", "-W", default=20000000, type=int, help="warmup instr count"
    )
    parser.add_argument(
        "--max-instr", "-I", default=40000000, type=int, help="max instr count"
    )
    parser.add_argument(
        "--max-cycles",
        "-C",
        default=None,
        type=int,
        help="max cycle count (emu -C). Omit to leave unlimited",
    )
    parser.add_argument(
        "--threads", "-T", default=8, type=int, help="number of emu threads"
    )
    parser.add_argument(
        "--cst-file",
        type=str,
        default="",
        help="Path to custom constantin file path (empty for default init setting)",
    )
    parser.add_argument(
        "--dump-db",
        action="store_true",
        help="Run emu with --dump-db and store simulator.db in each checkpoint result directory",
    )
    parser.add_argument(
        "--save-dramsim3",
        action="store_true",
        help="Store DRAMSim3 statistics in each checkpoint result directory",
    )

    # report configs
    parser.add_argument(
        "--frequency",
        type=float,
        default=3.0,
        help="CPU frequency in GHz for performance score calculation, default 3.0GHz",
    )
    parser.add_argument(
        "--spec-version",
        type=str,
        default=None,
        help="Specify SPEC version for report, empty for auto inference from gcpt_path",
    )

    # autorun
    parser.add_argument(
        "--server-list",
        type=str,
        default="",
        help="Comma-separated list of servers to use, leave empty to use all",
    )
    parser.add_argument(
        "--benchmarks",
        default="",
        type=str,
        help="Comma-separated list of benchmarks to run, leave empty to run all",
    )

    # function options
    parser.add_argument(
        "--run",
        action="store_true",
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Run emu with only 2000 instructions to check the function",
    )

    parser.add_argument(
        "--report",
        action="store_true",
    )

    parser.add_argument(
        "--reset-running",
        action="store_true",
        help="Reset checkpoints in RUNNING state by removing their output files",
    )

    # debug options
    parser.add_argument(
        "--log-level",
        type=str,
        default="INFO",
        help="Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)",
    )

    args = parser.parse_args()

    gcpt_path = Path(args.gcpt_path).resolve() if args.gcpt_path else None
    json_path = Path(args.json_path).resolve()
    result_path = Path(args.result_path).resolve()

    result_path.mkdir(parents=True, exist_ok=True)

    log_path = result_path / f"runner_{time.strftime('%Y-%m-%d_%H-%M-%S')}.log"
    # if log path is not writable, disable logging to file,
    # to prevent the whole script from failing due to logging error
    if not os.access(result_path, os.W_OK):
        log_path = None

    # setup logging
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s - %(levelname)5s - %(message)s",
        handlers=[
            logging.StreamHandler(),
        ]
        + (
            [logging.FileHandler(log_path, encoding="utf-8")]
            if log_path is not None
            else []
        ),
    )
    for handler in logging.root.handlers:
        if isinstance(handler, logging.StreamHandler):
            handler.setLevel(args.log_level.upper())
        if isinstance(handler, logging.FileHandler):
            handler.setLevel(logging.DEBUG)

    if log_path is None:
        logging.warning(
            "Log path %s is not writable, logging to file is disabled", result_path
        )

    # pre-checks
    if not json_path.is_file():
        raise FileNotFoundError(f"json_path is not a file: {json_path}")
    if gcpt_path is not None:
        if not gcpt_path.is_dir():
            raise FileNotFoundError(f"gcpt_path is not a directory: {gcpt_path}")

    # if only args.report is specified, no need to acquire lock as it is read-only operation.
    need_lock = args.run or args.dry_run or args.reset_running
    if not need_lock:
        logging.info("Only report is requested, skipping lock acquisition")
    # add lock per (result_path, gcpt_path) pair
    # to prevent the same checkpoint from being run by multiple instances with same result_path simultaneously
    if gcpt_path is not None:
        lock_src = str(gcpt_path)
    else:
        lock_src = str(json_path)
    gcpt_hash = hashlib.sha256(lock_src.encode()).hexdigest()[:8]
    lock = (
        Heartbeat(f"trigger_{gcpt_hash}", result_path, HEARTBEAT_INTERVAL)
        if need_lock
        else FakeLock()
    )
    while not lock.try_acquire():
        logging.info(
            "Another instance is running in the same directory (%s), waiting for %d seconds...",
            result_path,
            HEARTBEAT_INTERVAL,
        )
        time.sleep(HEARTBEAT_INTERVAL)

    if need_lock:
        logging.info("Lock %s acquired for %s", gcpt_hash, result_path)

    try:
        xiangshan = XiangShan(
            json_path=json_path,
            result_path=result_path,
            benchmarks=args.benchmarks,
            gcpt_path=gcpt_path,
        )

        if args.reset_running:
            xiangshan.reset_running_gcpt()

        if args.run or args.dry_run:
            emu_path = Path(args.emu_path).resolve() if args.emu_path else None
            nemu_so_path = (
                Path(args.nemu_so_path).resolve() if args.nemu_so_path else None
            )
            cst_file = Path(args.cst_file).resolve() if args.cst_file else None

            if emu_path is None:
                raise ValueError("emu_path is required for --run")
            if not emu_path.is_file():
                raise FileNotFoundError(f"emu_path is not a file: {emu_path}")
            if nemu_so_path is not None and not nemu_so_path.is_file():
                raise FileNotFoundError(f"nemu_so_path is not a file: {nemu_so_path}")
            if cst_file is not None and not cst_file.is_file():
                raise FileNotFoundError(f"cst_file does not exist: {cst_file}")
            xiangshan.run(
                emu_path=emu_path,
                nemu_so_path=nemu_so_path,
                server_list=args.server_list,
                emu_config=EmuConfig(
                    warmup=args.warmup,
                    max_instr=args.max_instr,
                    threads=args.threads,
                    cst_file=cst_file,
                    dump_db=args.dump_db,
                    dry_run=args.dry_run,
                    max_cycles=args.max_cycles,
                    save_dramsim3=args.save_dramsim3,
                ),
            )

        if args.report:
            xiangshan.report(args.frequency, args.spec_version)

    finally:
        lock.release()


if __name__ == "__main__":
    main()
