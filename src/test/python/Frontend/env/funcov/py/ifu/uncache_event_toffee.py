from __future__ import annotations

from dataclasses import dataclass, field

from toffee.funcov import CovGroup

from ...recorder import UNCACHE_EVENT_SAMPLER_BIN_KEYS


UNCACHE_EVENT_COVERPOINTS = {
    "uncache_ordering": "mmio_commit_gate",
    "uncache_path_switch": "redirect_recovery",
    "fetch_path_switch": "redirect_recovery",
}


@dataclass
class _UncacheCycleView:
    flags: dict[tuple[str, str], bool] = field(default_factory=dict)


@dataclass
class _FlagMarkTarget:
    flags: dict[tuple[str, str], bool]
    evidence: dict = field(default_factory=dict)

    def mark(self, group, bin_name, cycle, evidence=None, *, coverpoint=None, **kwargs):
        del cycle, coverpoint, kwargs
        self.flags[(str(group), str(bin_name))] = True
        if isinstance(evidence, dict):
            self.evidence.clear()
            self.evidence.update(evidence)
        return True


def _point_definitions() -> dict[tuple[str, str], tuple[str, ...]]:
    definitions: dict[tuple[str, str], list[str]] = {}
    for group_name, bin_name in sorted(UNCACHE_EVENT_SAMPLER_BIN_KEYS):
        point_name = UNCACHE_EVENT_COVERPOINTS[group_name]
        definitions.setdefault((group_name, point_name), []).append(bin_name)
    return {key: tuple(value) for key, value in definitions.items()}


def evaluate_uncache_event_coverage(runtime, env, cycle: int):
    flags = {
        (group_name, bin_name): False
        for group_name, bin_name in UNCACHE_EVENT_SAMPLER_BIN_KEYS
    }
    target = _FlagMarkTarget(flags=flags)
    runtime._sample_uncache_cycle_state(env.dut, int(cycle), env, mark_target=target)
    return target.flags, target.evidence


class UncacheEventToffeeCoverage:
    """Native Toffee CovGroup sampling for uncache cycle and path-switch events."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        self._runtime = runtime
        self._sink = sink
        self._audit_recorder = audit_recorder
        self.env = runtime.env
        self._view = _UncacheCycleView(
            flags={
                (group_name, bin_name): False
                for group_name, bin_name in UNCACHE_EVENT_SAMPLER_BIN_KEYS
            }
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
                    lambda view, g=group_name, b=bin_name: bool(
                        view.flags.get((g, b), False)
                    )
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
            self._view.flags = {
                (group_name, bin_name): False
                for group_name, bin_name in UNCACHE_EVENT_SAMPLER_BIN_KEYS
            }
            return
        self.sample(cycle)

    def sample(self, cycle: int) -> None:
        flags, evidence = evaluate_uncache_event_coverage(self._runtime, self.env, int(cycle))
        self._view.flags = {
            (group_name, bin_name): bool(flags.get((group_name, bin_name), False))
            for group_name, bin_name in UNCACHE_EVENT_SAMPLER_BIN_KEYS
        }
        for group in self.cov_groups:
            group.sample()
        if self._sink is not None:
            self._sink.record_native_hits(
                self._view.flags, UNCACHE_EVENT_COVERPOINTS, cycle, evidence
            )
        if self._audit_recorder is not None:
            for group_name, bin_name in UNCACHE_EVENT_SAMPLER_BIN_KEYS:
                if not self._view.flags.get((group_name, bin_name), False):
                    continue
                self._audit_recorder.mark(
                    group_name,
                    bin_name,
                    cycle,
                    evidence,
                    coverpoint=UNCACHE_EVENT_COVERPOINTS[group_name],
                    forward_to_toffee=False,
                )
        self._view.flags = {
            (group_name, bin_name): False
            for group_name, bin_name in UNCACHE_EVENT_SAMPLER_BIN_KEYS
        }

    def mark(self, coverage_group, bin_name, cycle, evidence=None, *, coverpoint=None):
        """Event-driven path-switch hits sample the native group immediately."""
        del coverpoint
        key = (str(coverage_group), str(bin_name))
        if key not in self._view.flags:
            raise KeyError(f"unknown uncache coverage bin: {coverage_group}::{bin_name}")
        self._view.flags[key] = True
        group = self._groups.get(str(coverage_group))
        if group is None:
            raise KeyError(f"unknown uncache coverage group: {coverage_group}")
        group.sample()
        self._view.flags[key] = False
        if self._sink is not None:
            self._sink.record_native_hits(
                {key: True}, UNCACHE_EVENT_COVERPOINTS, cycle, evidence
            )
        if self._audit_recorder is not None:
            self._audit_recorder.mark(
                coverage_group,
                bin_name,
                cycle,
                evidence,
                coverpoint=UNCACHE_EVENT_COVERPOINTS[str(coverage_group)],
                forward_to_toffee=False,
            )
        return True

    def report(self):
        return [group.as_dict() for group in self.cov_groups]

    def hit_counts(self):
        return {
            (group["name"], point["name"], item["name"]): int(item["hints"])
            for group in self.report()
            for point in group["points"]
            for item in point["bins"]
        }


__all__ = [
    "UNCACHE_EVENT_COVERPOINTS",
    "UncacheEventToffeeCoverage",
    "evaluate_uncache_event_coverage",
]
