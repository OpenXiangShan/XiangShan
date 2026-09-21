from __future__ import annotations

from dataclasses import dataclass, field

from toffee.funcov import CovGroup

from .icache_prefetchpipe_funcov import (
    ICACHE_PREFETCHPIPE_COVERPOINTS,
    ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
    evaluate_icache_prefetchpipe_coverage,
    reset_icache_prefetchpipe_coverage_state,
)


@dataclass
class _PrefetchpipeCycleView:
    flags: dict[str, bool] = field(default_factory=dict)


def _point_definitions() -> dict[tuple[str, str], tuple[str, ...]]:
    definitions: dict[tuple[str, str], list[str]] = {}
    for group_name, bin_name in sorted(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS):
        point_name = ICACHE_PREFETCHPIPE_COVERPOINTS[group_name]
        definitions.setdefault((group_name, point_name), []).append(bin_name)
    return {key: tuple(value) for key, value in definitions.items()}


class ICachePrefetchpipeToffeeCoverage:
    """Native Toffee CovGroup sampling for the ICache PrefetchPipe domain."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        self._runtime = runtime
        self._sink = sink
        self._audit_recorder = audit_recorder
        self.env = runtime.env
        self._view = _PrefetchpipeCycleView(
            flags={bin_name: False for _, bin_name in ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS}
        )
        self._groups: dict[str, CovGroup] = {}
        self.cov_groups: list[CovGroup] = []
        self._build_native_groups()
        if sink is not None and hasattr(sink, "install_native_groups"):
            sink.install_native_groups(self.cov_groups, model=self)

    def _build_native_groups(self) -> None:
        for (group_name, point_name), bin_names in _point_definitions().items():
            group = CovGroup(group_name, disable_sample_when_point_hinted=False)
            predicates = {
                bin_name: (
                    lambda view, expected=bin_name: bool(view.flags.get(expected, False))
                )
                for bin_name in bin_names
            }
            group.add_watch_point(
                self._view,
                predicates,
                name=point_name,
                once=False,
            )
            self._groups[group_name] = group
            self.cov_groups.append(group)

    def _read_first_dut_signal(self, dut, names):
        return self._runtime._read_first_dut_signal(dut, names)

    def _read_dut_signal(self, dut, name, default=0):
        return self._runtime._read_dut_signal(dut, name, default)

    def _try_read_dut_signal(self, dut, name):
        return self._runtime._try_read_dut_signal(dut, name)

    def on_cycle(self, cycle: int) -> None:
        dut = self.env.dut
        if self._runtime._read_dut_signal(dut, "reset", 0) == 1:
            reset_icache_prefetchpipe_coverage_state(self)
            self._view.flags = {
                bin_name: False for _, bin_name in ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS
            }
            return
        self.sample(cycle)

    def sample(self, cycle: int) -> None:
        flags, evidence = evaluate_icache_prefetchpipe_coverage(self, self.env, int(cycle))
        self._view.flags = {
            bin_name: bool(flags.get(bin_name, False))
            for _, bin_name in ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS
        }
        for group in self.cov_groups:
            group.sample()
        if self._sink is not None:
            self._sink.record_native_hits(
                {
                    (group_name, bin_name): bool(self._view.flags.get(bin_name, False))
                    for group_name, bin_name in ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS
                },
                ICACHE_PREFETCHPIPE_COVERPOINTS,
                cycle,
                evidence,
            )
        if self._audit_recorder is not None:
            for group_name, bin_name in ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS:
                if not self._view.flags.get(bin_name, False):
                    continue
                self._audit_recorder.mark(
                    group_name,
                    bin_name,
                    cycle,
                    evidence,
                    coverpoint=ICACHE_PREFETCHPIPE_COVERPOINTS[group_name],
                    forward_to_toffee=False,
                )

    def report(self):
        return [group.as_dict() for group in self.cov_groups]

    def hit_counts(self):
        return {
            (group["name"], point["name"], item["name"]): int(item["hints"])
            for group in self.report()
            for point in group["points"]
            for item in point["bins"]
        }


__all__ = ["ICachePrefetchpipeToffeeCoverage"]
