import argparse
import importlib
import sys
from pathlib import Path

if __package__ in (None, ""):
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    import queries
    import symbolize
else:
    from . import queries, symbolize


_PLOT_MODULE = None


def _plot_module():
    global _PLOT_MODULE
    if _PLOT_MODULE is None:
        if __package__ in (None, ""):
            _PLOT_MODULE = importlib.import_module("plot")
        else:
            _PLOT_MODULE = importlib.import_module(".plot", package=__package__)
    return _PLOT_MODULE


def print_rows(title: str, rows: list[dict]) -> None:
    print(f"== {title} ==")
    if not rows:
        print("(empty)")
        return
    keys = list(rows[0].keys())
    print(", ".join(keys))
    for row in rows:
        print(", ".join(str(row[key]) for key in keys))
    print()


def resolve_db_path(db_arg: str | None) -> Path:
    if db_arg:
        return Path(db_arg)
    return queries.latest_db_path()


def export_symbolized_commits(
    conn,
    hart: int,
    out_dir: Path,
    elf_path: Path,
    sym_cache: Path,
    top: int,
) -> list[dict]:
    rows = queries.fetch_commit_event_rows(conn, hart, top=top)
    resolver = symbolize.ElfResolver(elf_path, cache_path=sym_cache)
    symbolized = symbolize.symbolize_rows(rows, resolver, pc_field="pc")
    queries.write_csv(out_dir / "symbolized_commits.csv", symbolized)
    print_rows("Symbolized Commits", symbolized)
    return symbolized


def export_summary(conn, hart: int, out_dir: Path) -> dict:
    state_rows = queries.fetch_state_summary(conn, hart)
    commit_width_rows = queries.fetch_commit_width_summary(conn, hart)
    commit_type_rows = queries.fetch_commit_type_summary(conn, hart)
    redirect_rows = queries.fetch_redirect_summary(conn, hart)
    replay_rows = queries.fetch_replay_summary(conn, hart)
    queries.write_csv(out_dir / "state_summary.csv", state_rows)
    queries.write_csv(out_dir / "commit_width_summary.csv", commit_width_rows)
    queries.write_csv(out_dir / "commit_type_summary.csv", commit_type_rows)
    queries.write_csv(out_dir / "redirect_summary.csv", redirect_rows)
    queries.write_csv(out_dir / "replay_summary.csv", replay_rows)
    print_rows("State Summary", state_rows)
    print_rows("Commit Width Summary", commit_width_rows)
    print_rows("Commit Type Summary", commit_type_rows)
    print_rows("Redirect Summary", redirect_rows)
    print_rows("Replay Summary", replay_rows)
    return {
        "state": state_rows,
        "commit_width": commit_width_rows,
        "commit_type": commit_type_rows,
        "redirect": redirect_rows,
        "replay": replay_rows,
    }


def export_hotspots(conn, hart: int, out_dir: Path, top: int) -> dict:
    pc_rows = queries.fetch_pc_hotspots(conn, hart, top=top)
    pc_commit_rows = queries.fetch_pc_commit_type_hotspots(conn, hart, top=top)
    redirect_rows = queries.fetch_redirect_targets(conn, hart, top=top)
    replay_rows = queries.fetch_replay_hotspots(conn, hart, top=top)
    queries.write_csv(out_dir / "pc_hotspots.csv", pc_rows)
    queries.write_csv(out_dir / "pc_commit_type_hotspots.csv", pc_commit_rows)
    queries.write_csv(out_dir / "redirect_targets.csv", redirect_rows)
    queries.write_csv(out_dir / "replay_hotspots.csv", replay_rows)
    print_rows("PC Hotspots", pc_rows)
    print_rows("PC CommitType Hotspots", pc_commit_rows)
    print_rows("Redirect Targets", redirect_rows)
    print_rows("Replay Hotspots", replay_rows)
    return {
        "pc": pc_rows,
        "pc_commit": pc_commit_rows,
        "redirect_targets": redirect_rows,
        "replay_hotspots": replay_rows,
    }


def export_plots(summary_data: dict, hotspot_data: dict, out_dir: Path) -> None:
    plot = _plot_module()
    plot.plot_state_distribution(summary_data["state"], out_dir / "state_distribution.png")
    plot.plot_commit_width_distribution(summary_data["commit_width"], out_dir / "commit_width_distribution.png")
    plot.plot_pc_hotspots(hotspot_data["pc"], out_dir / "top_pc_hotspots.png")
    plot.plot_redirect_targets(hotspot_data["redirect_targets"], out_dir / "redirect_targets.png")


def build_parser() -> argparse.ArgumentParser:
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--db", default=None, help="path to sqlite db; default uses latest build/*.db")
    common.add_argument("--hart", default=0, type=int, help="hart id, default 0")
    common.add_argument("--out-dir", default="scripts/tip/results", help="output directory")
    common.add_argument("--top", default=20, type=int, help="top N hotspots to export")
    common.add_argument(
        "--elf",
        help="path to ELF binary for symbolizing addresses (required for 'symbols'; optional for 'all')",
    )
    common.add_argument("--sym-cache", default="scripts/tip/.symbol-cache.json", help="JSON cache for symbol resolutions")
    parser = argparse.ArgumentParser(description="TIP analysis for XiangShan ChiselDB output", parents=[common])
    subparsers = parser.add_subparsers(dest="subcommand", required=True)
    subparsers.add_parser("summary", help="print summary and write summary CSVs", parents=[common])
    subparsers.add_parser("hotspots", help="print hotspots and write hotspot CSVs", parents=[common])
    subparsers.add_parser("plot", help="generate PNG charts", parents=[common])
    subparsers.add_parser(
        "symbols",
        help="export symbolized commit hotspots from an ELF binary",
        parents=[common],
    )
    subparsers.add_parser("all", help="run summary, hotspots, and plot together", parents=[common])
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.top <= 0:
        parser.error("--top must be positive")
    requires_elf = args.subcommand == "symbols"
    if requires_elf and not args.elf:
        parser.error("--elf is required for 'symbols'")

    include_symbolized = args.subcommand == "symbols" or (args.subcommand == "all" and args.elf)
    db_path = resolve_db_path(args.db)
    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    conn = queries.open_db(db_path)
    queries.ensure_tip_table(conn, args.hart)

    summary_data = None
    hotspot_data = None
    symbolized_rows = None
    elf_path = Path(args.elf) if args.elf else None
    sym_cache_path = Path(args.sym_cache)
    if args.subcommand in ("summary", "all", "plot"):
        summary_data = export_summary(conn, args.hart, out_dir)
    if args.subcommand in ("hotspots", "all", "plot"):
        hotspot_data = export_hotspots(conn, args.hart, out_dir, args.top)
    if include_symbolized:
        if elf_path is None:
            parser.error("--elf is required for symbolized reports")
        symbolized_rows = export_symbolized_commits(
            conn,
            args.hart,
            out_dir,
            elf_path,
            sym_cache_path,
            args.top,
        )
    if args.subcommand in ("plot", "all"):
        if summary_data is None:
            summary_data = export_summary(conn, args.hart, out_dir)
        if hotspot_data is None:
            hotspot_data = export_hotspots(conn, args.hart, out_dir, args.top)
        export_plots(summary_data, hotspot_data, out_dir)

    conn.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
