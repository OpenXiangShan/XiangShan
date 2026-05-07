import argparse
import os
import re
import sqlite3
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple

import numpy as np


class HelpFormatter(argparse.ArgumentDefaultsHelpFormatter, argparse.RawDescriptionHelpFormatter):
    pass


EPILOG = """Examples:
  python3 scripts/rolling/rolling.py plot run.db --perf-name ipc --output ipc.png
  python3 scripts/rolling/rolling.py plot run.db --hart all --include-prefix td_ --output td.png
  python3 scripts/rolling/rolling.py diff run1.db run2.db --perf-name ipc
  python3 scripts/rolling/rolling.py corr run.db --include-prefix td_ --csv /tmp/ipc_corr.csv
"""

PNG_SAVE_DPI = 300


ROLLING_TABLE_PATTERN = re.compile(r"^(?P<perf_name>\w+)_rolling_(?P<hart>\d+)$")


@dataclass(frozen=True)
class RollingSeries:
    perf_name: str
    hart: int
    table_name: str
    xdata: np.ndarray
    ydata: np.ndarray

    @property
    def sample_count(self) -> int:
        return int(self.xdata.size)


@dataclass(frozen=True)
class TableStats:
    perf_name: str
    hart: int
    table_name: str
    sample_count: int
    x_min: Optional[int]
    x_max: Optional[int]
    y_min: Optional[int]
    y_max: Optional[int]
    stamp_min: Optional[int]
    stamp_max: Optional[int]


@dataclass(frozen=True)
class CorrelationResult:
    perf_name: str
    hart: int
    samples: int
    align: str
    corr: float

    @property
    def abs_corr(self) -> float:
        return abs(self.corr)


def err_exit(msg: str) -> None:
    print(msg)
    sys.exit(1)


def load_perf_file(path: Optional[str]) -> List[str]:
    if not path:
        return []
    with open(path) as fp:
        return [
            line.strip()
            for line in fp
            if line.strip() and not line.lstrip().startswith(("//", "#"))
        ]


def normalize_progress(xdata: np.ndarray) -> np.ndarray:
    if xdata.size <= 1:
        return np.linspace(0.0, 1.0, num=max(xdata.size, 1))
    x_float = xdata.astype(np.float64)
    x_min = float(x_float[0])
    x_max = float(x_float[-1])
    span = x_max - x_min
    if span <= 0.0:
        return np.linspace(0.0, 1.0, num=xdata.size)
    return (x_float - x_min) / span


def parse_hart(value: str) -> Optional[int]:
    if value.lower() in {"all", "*"}:
        return None
    try:
        return int(value)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("hart must be an integer or 'all'") from exc


def is_sqlite_db(path: str) -> bool:
    try:
        with open(path, "rb") as fp:
            return fp.read(16) == b"SQLite format 3\x00"
    except OSError:
        return False


def read_db_list_file(path: str) -> List[str]:
    with open(path) as fp:
        return [
            line.strip()
            for line in fp
            if line.strip() and not line.lstrip().startswith(("//", "#"))
        ]


def resolve_diff_db_paths(raw_paths: List[str]) -> List[str]:
    if len(raw_paths) != 1:
        return raw_paths
    single = raw_paths[0]
    if is_sqlite_db(single):
        return [single]
    return read_db_list_file(single)


def derive_output_stem(perf_names: List[str]) -> str:
    if not perf_names:
        return "rolling"
    if len(perf_names) == 1:
        return perf_names[0]
    if len(perf_names) <= 4:
        return "_".join(perf_names)
    return "_".join(perf_names[:4]) + f"_n{len(perf_names)}"


def resolve_output_path(output: Optional[str], perf_names: List[str], suffix: str = ".png") -> Path:
    if output:
        return Path(output)
    return Path(f"{derive_output_stem(perf_names)}{suffix}")


class DataSet:

    def __init__(self, db_path: str):
        self.db_path = db_path
        self.conn = sqlite3.connect(db_path)
        self.cursor = self.conn.cursor()

    def close(self) -> None:
        self.conn.close()

    def _query_table_names(self) -> List[str]:
        sql = (
            "SELECT name FROM sqlite_master "
            "WHERE type = 'table' AND name LIKE '%_rolling_%' "
            "ORDER BY name"
        )
        self.cursor.execute(sql)
        return [row[0] for row in self.cursor.fetchall()]

    def list_rolling_tables(self, hart: Optional[int] = None) -> List[Tuple[str, int, str]]:
        tables = []
        for table_name in self._query_table_names():
            match = ROLLING_TABLE_PATTERN.match(table_name)
            if not match:
                continue
            table_hart = int(match.group("hart"))
            if hart is not None and table_hart != hart:
                continue
            tables.append((match.group("perf_name"), table_hart, table_name))
        return tables

    def derive(self, perf_name: str, aggregate: int, clk_itval: int, hart: int) -> Tuple[List[float], List[float]]:
        series = self.load_series(perf_name, hart)
        aggcnt = 0
        recordcnt = 0
        aggydata = 0.0
        aggxdata = 0.0
        xplot: List[float] = []
        yplot: List[float] = []

        if clk_itval == -1:
            # normal mode:
            #   db log in normal mode: (xAxis, ydata)
            #   xAxis is current x position, ydata is the increment value in this window
            for x_pt, y_pt in zip(series.xdata, series.ydata):
                aggcnt += 1
                aggydata += float(y_pt)
                if aggcnt == aggregate:
                    xplot.append(float(x_pt))
                    x_gap = float(x_pt) - aggxdata
                    yplot.append(0.0 if x_gap == 0.0 else aggydata / x_gap)
                    aggcnt = 0
                    aggydata = 0.0
                    aggxdata = float(x_pt)
        else:
            # interval-based mode:
            #   db log: (xdata, ydata)
            #   xdata/ydata are the accumulated values in a fixed event interval
            for x_pt, y_pt in zip(series.xdata, series.ydata):
                aggcnt += 1
                aggxdata += float(x_pt)
                aggydata += float(y_pt)
                if aggcnt == aggregate:
                    xplot.append(float(clk_itval * aggregate) * (recordcnt + 1))
                    yplot.append(0.0 if aggydata == 0.0 else aggxdata / aggydata)
                    aggcnt = 0
                    aggxdata = 0.0
                    aggydata = 0.0
                    recordcnt += 1
        return xplot, yplot

    def load_series(self, perf_name: str, hart: int) -> RollingSeries:
        if not re.fullmatch(r"\w+", perf_name):
            err_exit(f"invalid perf name `{perf_name}`")
        table_name = f"{perf_name}_rolling_{hart}"
        self.cursor.execute(f'SELECT XAXISPT, YAXISPT FROM "{table_name}" ORDER BY ID')
        rows = self.cursor.fetchall()
        if not rows:
            err_exit(f"table `{table_name}` is empty")
        xdata = np.array([int(row[0]) for row in rows], dtype=np.int64)
        ydata = np.array([int(row[1]) for row in rows], dtype=np.int64)
        return RollingSeries(perf_name, hart, table_name, xdata, ydata)

    def get_table_stats(self, perf_name: str, hart: int) -> TableStats:
        if not re.fullmatch(r"\w+", perf_name):
            err_exit(f"invalid perf name `{perf_name}`")
        table_name = f"{perf_name}_rolling_{hart}"
        self.cursor.execute(
            f'SELECT COUNT(*), MIN(XAXISPT), MAX(XAXISPT), MIN(YAXISPT), MAX(YAXISPT), MIN(STAMP), MAX(STAMP) '
            f'FROM "{table_name}"'
        )
        row = self.cursor.fetchone()
        return TableStats(
            perf_name=perf_name,
            hart=hart,
            table_name=table_name,
            sample_count=int(row[0]),
            x_min=row[1],
            x_max=row[2],
            y_min=row[3],
            y_max=row[4],
            stamp_min=row[5],
            stamp_max=row[6],
        )


def iter_perf_names(args: argparse.Namespace, dataset: DataSet) -> List[str]:
    perf_names: List[str] = []
    seen = set()

    def add(name: str) -> None:
        if name and name not in seen:
            perf_names.append(name)
            seen.add(name)

    for name in load_perf_file(args.perf_file):
        add(name)
    if getattr(args, "perf_name", None):
        if isinstance(args.perf_name, list):
            for name in args.perf_name:
                add(name)
        else:
            add(args.perf_name)

    tables = dataset.list_rolling_tables(args.hart)
    if not perf_names:
        perf_names.extend(perf_name for perf_name, _, _ in tables)

    if getattr(args, "include_prefix", None):
        prefixes = tuple(args.include_prefix)
        perf_names = [name for name in perf_names if name.startswith(prefixes)]

    if getattr(args, "exclude_prefix", None):
        prefixes = tuple(args.exclude_prefix)
        perf_names = [name for name in perf_names if not name.startswith(prefixes)]

    if getattr(args, "exclude_perf", None):
        excluded = set(args.exclude_perf)
        perf_names = [name for name in perf_names if name not in excluded]

    return perf_names


def get_pyplot(output: Optional[str]):
    if output or not os.environ.get("DISPLAY"):
        mplconfigdir = os.environ.get("MPLCONFIGDIR")
        if not mplconfigdir:
            mplconfigdir_path = Path(tempfile.gettempdir()) / "matplotlib"
            mplconfigdir_path.mkdir(parents=True, exist_ok=True)
            os.environ["MPLCONFIGDIR"] = str(mplconfigdir_path)
        os.environ.setdefault("XDG_CACHE_HOME", tempfile.gettempdir())
        import matplotlib

        matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    return plt


def plot_dataset(plt, path: str, perf_names: List[str], aggregate: int, clk_itval: int,
                 hart: Optional[int], label_suffix: str = "") -> int:
    dataset = DataSet(path)
    plotted = 0

    try:
        table_harts = {}
        if hart is None:
            for perf_name, table_hart, _ in dataset.list_rolling_tables(None):
                table_harts.setdefault(perf_name, []).append(table_hart)
        for perf in perf_names:
            harts = table_harts.get(perf, []) if hart is None else [hart]
            for table_hart in harts:
                try:
                    xplot, yplot = dataset.derive(perf, aggregate, clk_itval, table_hart)
                except sqlite3.OperationalError:
                    continue
                if not xplot:
                    continue
                if hart is None:
                    label = f"{perf}@h{table_hart}{label_suffix}"
                else:
                    label = perf + label_suffix
                plt.plot(xplot, yplot, lw=1, ls="-", label=label)
                plotted += 1
    finally:
        dataset.close()
    return plotted


def finalize_plot(plt, output: Optional[str]) -> None:
    plt.legend()
    plt.tight_layout()
    if output:
        output_path = Path(output)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        save_kwargs = {"bbox_inches": "tight"}
        if output_path.suffix.lower() in {"", ".png"}:
            save_kwargs["dpi"] = PNG_SAVE_DPI
        plt.savefig(output_path, **save_kwargs)
        print(f"saved plot to {output_path}")
    else:
        if not os.environ.get("DISPLAY"):
            err_exit("no GUI display detected; use --output to save the plot")
        plt.show()
    plt.close()


def align_by_index(ref: RollingSeries, target: RollingSeries) -> Tuple[np.ndarray, np.ndarray]:
    samples = min(ref.sample_count, target.sample_count)
    if samples == 0:
        return np.array([]), np.array([])
    return (
        ref.ydata[:samples].astype(np.float64),
        target.ydata[:samples].astype(np.float64),
    )


def align_by_xaxis(ref: RollingSeries, target: RollingSeries) -> Tuple[np.ndarray, np.ndarray]:
    common_x, ref_idx, target_idx = np.intersect1d(
        ref.xdata, target.xdata, assume_unique=False, return_indices=True
    )
    if common_x.size == 0:
        return np.array([]), np.array([])
    return (
        ref.ydata[ref_idx].astype(np.float64),
        target.ydata[target_idx].astype(np.float64),
    )


def align_by_progress(ref: RollingSeries, target: RollingSeries, points: Optional[int]) -> Tuple[np.ndarray, np.ndarray]:
    if ref.sample_count == 0 or target.sample_count == 0:
        return np.array([]), np.array([])
    sample_points = points if points else min(ref.sample_count, target.sample_count)
    if sample_points <= 1:
        return np.array([]), np.array([])
    grid = np.linspace(0.0, 1.0, num=sample_points)
    ref_progress = normalize_progress(ref.xdata)
    target_progress = normalize_progress(target.xdata)
    ref_y = np.interp(grid, ref_progress, ref.ydata.astype(np.float64))
    target_y = np.interp(grid, target_progress, target.ydata.astype(np.float64))
    return ref_y, target_y


def align_series(ref: RollingSeries, target: RollingSeries, align: str,
                 progress_points: Optional[int]) -> Tuple[np.ndarray, np.ndarray]:
    if align == "index":
        return align_by_index(ref, target)
    if align == "xaxis":
        return align_by_xaxis(ref, target)
    if align == "progress":
        return align_by_progress(ref, target, progress_points)
    err_exit(f"unsupported align mode `{align}`")
    return np.array([]), np.array([])


def safe_corrcoef(lhs: np.ndarray, rhs: np.ndarray) -> Optional[float]:
    if lhs.size < 2 or rhs.size < 2:
        return None
    if np.allclose(lhs, lhs[0]) or np.allclose(rhs, rhs[0]):
        return None
    corr = float(np.corrcoef(lhs, rhs)[0, 1])
    if np.isnan(corr):
        return None
    return corr


def check_plot_args(args: argparse.Namespace) -> None:
    if args.aggregate <= 0:
        err_exit("aggregation ratio must be no less than 1")
    if args.interval < -1 or args.interval == 0:
        err_exit("interval must be -1 or a positive integer")


def handle_plot(args: argparse.Namespace) -> None:
    check_plot_args(args)
    dataset = DataSet(args.db_path)
    try:
        perf_names = iter_perf_names(args, dataset)
    finally:
        dataset.close()
    if not perf_names:
        err_exit("no rolling counters matched the selection")
    output = resolve_output_path(getattr(args, "output", None), perf_names)
    plt = get_pyplot(output)
    if plot_dataset(plt, args.db_path, perf_names, args.aggregate, args.interval, args.hart) == 0:
        err_exit("no rolling counters could be plotted")
    finalize_plot(plt, output)


def handle_diff(args: argparse.Namespace) -> None:
    check_plot_args(args)
    db_paths = resolve_diff_db_paths(args.db_paths)
    if not db_paths:
        err_exit("no db paths provided for diff")

    ref_dataset = DataSet(db_paths[0])
    try:
        perf_names = iter_perf_names(args, ref_dataset)
    finally:
        ref_dataset.close()

    if not perf_names:
        err_exit("no rolling counters matched the selection")

    output = resolve_output_path(getattr(args, "output", None), perf_names)
    plt = get_pyplot(output)
    plotted = 0
    for db_path in db_paths:
        db_label = "@" + Path(db_path).name
        plotted += plot_dataset(plt, db_path, perf_names, args.aggregate, args.interval, args.hart, db_label)
    if plotted == 0:
        err_exit("no rolling counters could be plotted")
    finalize_plot(plt, output)


def handle_list(args: argparse.Namespace) -> None:
    dataset = DataSet(args.db_path)
    try:
        tables = dataset.list_rolling_tables(args.hart)
        if not tables:
            err_exit("no rolling tables found in db")
        print("perf_name\thart\tsamples\tx_min\tx_max\ty_min\ty_max\tstamp_min\tstamp_max")
        for perf_name, hart, _ in tables:
            stats = dataset.get_table_stats(perf_name, hart)
            print(
                f"{stats.perf_name}\t{stats.hart}\t{stats.sample_count}\t"
                f"{stats.x_min}\t{stats.x_max}\t{stats.y_min}\t{stats.y_max}\t"
                f"{stats.stamp_min}\t{stats.stamp_max}"
            )
    finally:
        dataset.close()


def handle_corr(args: argparse.Namespace) -> None:
    if args.hart is None:
        err_exit("corr requires a concrete hart id")
    dataset = DataSet(args.db_path)
    try:
        ref_series = dataset.load_series(args.ref, args.hart)
        perf_names = iter_perf_names(args, dataset)
        perf_names = [name for name in perf_names if name != args.ref]

        results: List[CorrelationResult] = []
        for perf_name in perf_names:
            try:
                target_series = dataset.load_series(perf_name, args.hart)
            except sqlite3.OperationalError:
                continue
            ref_aligned, target_aligned = align_series(
                ref_series, target_series, args.align, args.progress_points
            )
            corr = safe_corrcoef(ref_aligned, target_aligned)
            if corr is None:
                continue
            results.append(
                CorrelationResult(
                    perf_name=perf_name,
                    hart=args.hart,
                    samples=int(ref_aligned.size),
                    align=args.align,
                    corr=corr,
                )
            )

        if not results:
            err_exit("no comparable rolling counters found")

        results.sort(key=lambda item: item.abs_corr, reverse=True)

        if args.csv:
            csv_path = Path(args.csv)
            csv_path.parent.mkdir(parents=True, exist_ok=True)
            with csv_path.open("w") as fp:
                fp.write("perf_name,hart,samples,align,corr,abs_corr\n")
                for item in results:
                    fp.write(
                        f"{item.perf_name},{item.hart},{item.samples},{item.align},"
                        f"{item.corr:.8f},{item.abs_corr:.8f}\n"
                    )

        limit = args.top if args.top > 0 else len(results)
        print(f"reference={args.ref} hart={args.hart} align={args.align} compared={len(results)}")
        print("rank\tperf_name\tcorr\tabs_corr\tsamples")
        for idx, item in enumerate(results[:limit], start=1):
            print(
                f"{idx}\t{item.perf_name}\t{item.corr:+.6f}\t"
                f"{item.abs_corr:.6f}\t{item.samples}"
            )
    finally:
        dataset.close()


def add_common_perf_selection_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--perf-name",
        action="append",
        default=None,
        help="select one counter by name; can be specified multiple times",
    )
    parser.add_argument(
        "--perf-file",
        "-F",
        default=None,
        help="read selected counters from a file; lines starting with // or # are ignored",
    )
    parser.add_argument(
        "--include-prefix",
        action="append",
        default=None,
        help="keep only counters whose name starts with this prefix; can be specified multiple times",
    )
    parser.add_argument(
        "--exclude-prefix",
        action="append",
        default=None,
        help="drop counters whose name starts with this prefix; can be specified multiple times",
    )
    parser.add_argument(
        "--exclude-perf",
        action="append",
        default=None,
        help="drop one named counter; can be specified multiple times",
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Inspect, plot, diff, and correlate rolling counters from XiangShan SQLite DB dumps.",
        epilog=EPILOG,
        formatter_class=HelpFormatter,
    )
    subparsers = parser.add_subparsers(
        title="subcommands",
        dest="subcommand",
        help="plot, diff, inspect rolling tables, or analyze correlation",
        metavar="COMMAND",
    )
    subparsers.required = True

    cmd_plot = subparsers.add_parser(
        "plot",
        help="plot counters from one DB",
        description="Plot one or more rolling counters from a single SQLite DB dump.",
        formatter_class=HelpFormatter,
    )
    cmd_plot.add_argument("db_path", metavar="db_path", type=str, help="path to chiseldb file")
    cmd_plot.add_argument("--aggregate", "-A", default=1, type=int, help="aggregation ratio")
    cmd_plot.add_argument("--interval", "-I", default=-1, type=int, help="interval value in interval-based mode")
    cmd_plot.add_argument("--hart", default=0, type=parse_hart, help="hart id to analyze, or 'all' to overlay every hart")
    add_common_perf_selection_args(cmd_plot)
    cmd_plot.add_argument(
        "--output",
        "-O",
        default=argparse.SUPPRESS,
        type=str,
        help="save plot to this file instead of opening a GUI; if omitted, the filename is derived from the selected counter name(s)",
    )

    cmd_diff = subparsers.add_parser(
        "diff",
        help="plot counters from multiple DBs",
        description="Plot the same counters across multiple SQLite DB dumps for comparison.",
        formatter_class=HelpFormatter,
    )
    cmd_diff.add_argument(
        "db_paths",
        nargs="+",
        metavar="db_path",
        type=str,
        help="one or more chiseldb files, or a single file containing newline-separated db paths",
    )
    cmd_diff.add_argument("--aggregate", "-A", default=1, type=int, help="aggregation ratio")
    cmd_diff.add_argument("--interval", "-I", default=-1, type=int, help="interval value in interval-based mode")
    cmd_diff.add_argument("--hart", default=0, type=parse_hart, help="hart id to analyze, or 'all' to overlay every hart")
    add_common_perf_selection_args(cmd_diff)
    cmd_diff.add_argument(
        "--output",
        "-O",
        default=argparse.SUPPRESS,
        type=str,
        help="save plot to this file instead of opening a GUI; if omitted, the filename is derived from the selected counter name(s)",
    )

    cmd_list = subparsers.add_parser(
        "list",
        help="list counters in one DB",
        description="List rolling tables and their sample ranges from one SQLite DB dump.",
        formatter_class=HelpFormatter,
    )
    cmd_list.add_argument("db_path", metavar="db_path", type=str, help="path to chiseldb file")
    cmd_list.add_argument("--hart", default=None, type=parse_hart, help="filter by hart id; omit or use 'all' to list every hart")

    cmd_corr = subparsers.add_parser(
        "corr",
        help="correlate counters against a reference",
        description="Rank rolling counters by correlation with a reference counter in one SQLite DB dump.",
        formatter_class=HelpFormatter,
    )
    cmd_corr.add_argument("db_path", metavar="db_path", type=str, help="path to chiseldb file")
    cmd_corr.add_argument("--ref", default="ipc", type=str, help="reference rolling counter name")
    cmd_corr.add_argument("--hart", default=0, type=parse_hart, help="hart id to analyze")
    cmd_corr.add_argument(
        "--align",
        default="progress",
        choices=["index", "xaxis", "progress"],
        help="series alignment method before correlation",
    )
    cmd_corr.add_argument(
        "--progress-points",
        default=None,
        type=int,
        help="number of interpolation points in progress alignment mode",
    )
    cmd_corr.add_argument("--top", default=20, type=int, help="print top-N counters, <=0 means print all")
    cmd_corr.add_argument("--csv", default=None, type=str, help="optional csv output path")
    add_common_perf_selection_args(cmd_corr)
    return parser


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    if args.subcommand == "plot":
        handle_plot(args)
    elif args.subcommand == "diff":
        handle_diff(args)
    elif args.subcommand == "list":
        handle_list(args)
    elif args.subcommand == "corr":
        handle_corr(args)
    else:
        err_exit("invalid command")
    return 0


if __name__ == "__main__":
    sys.exit(main())
