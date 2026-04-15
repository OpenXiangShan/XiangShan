from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt


def _bar_plot(labels, values, title: str, xlabel: str, ylabel: str, out_path: Path | str) -> None:
    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.bar(range(len(labels)), values)
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_xticks(range(len(labels)))
    ax.set_xticklabels(labels, rotation=45, ha="right")
    fig.tight_layout()
    fig.savefig(out_path)
    plt.close(fig)


def plot_state_distribution(rows: list[dict], out_path: Path | str) -> None:
    _bar_plot(
        [row["state_name"] for row in rows],
        [row["count"] for row in rows],
        "TIP State Distribution",
        "State",
        "Cycles",
        out_path,
    )


def plot_commit_width_distribution(rows: list[dict], out_path: Path | str) -> None:
    _bar_plot(
        [str(row["commit_width"]) for row in rows],
        [row["count"] for row in rows],
        "Commit Width Distribution",
        "Commit Width",
        "Cycles",
        out_path,
    )


def plot_pc_hotspots(rows: list[dict], out_path: Path | str) -> None:
    _bar_plot(
        [row["pc"] for row in rows],
        [row["count"] for row in rows],
        "Top PC Hotspots",
        "PC",
        "Count",
        out_path,
    )


def plot_redirect_targets(rows: list[dict], out_path: Path | str) -> None:
    _bar_plot(
        [row["target"] for row in rows],
        [row["count"] for row in rows],
        "Redirect Targets",
        "Target",
        "Count",
        out_path,
    )
