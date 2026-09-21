from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Iterable, Mapping

from toffee.funcov import CovGroup


@dataclass
class FlagCycleView:
    flags: dict[tuple[str, str], bool] = field(default_factory=dict)


@dataclass
class FlagMarkTarget:
    """Intercept recorder.mark() into same-cycle flags/evidence."""

    flags: dict[tuple[str, str], bool]
    evidence: dict[str, Any] = field(default_factory=dict)

    def mark(self, group, bin_name, cycle, evidence=None, *, coverpoint=None, **kwargs):
        del cycle, coverpoint, kwargs
        self.flags[(str(group), str(bin_name))] = True
        if isinstance(evidence, dict):
            self.evidence.clear()
            self.evidence.update(evidence)
        return True


class EvaluateFlagRecorder:
    """Delegate state/attrs to a recorder while capturing mark() as flags."""

    def __init__(self, recorder, flags: dict[tuple[str, str], bool], evidence: dict[str, Any]):
        object.__setattr__(self, "_recorder", recorder)
        object.__setattr__(self, "_flags", flags)
        object.__setattr__(self, "_evidence", evidence)

    def __getattr__(self, name):
        return getattr(self._recorder, name)

    def __setattr__(self, name, value):
        setattr(self._recorder, name, value)

    def mark(self, group, bin_name, cycle, evidence=None, *, coverpoint=None, **kwargs):
        del coverpoint, kwargs
        key = (str(group), str(bin_name))
        if key not in self._flags:
            sink = getattr(self._recorder, "_sink", None)
            if sink is None:
                raise KeyError(f"coverage mark is outside the active native domain: {key}")
            return sink.mark_native(group, bin_name, cycle, evidence)
        self._flags[key] = True
        if isinstance(evidence, dict):
            self._evidence.clear()
            self._evidence.update(evidence)
        return True


def point_definitions_from_keys(
    keys: Iterable[tuple[str, str]],
    coverpoints: Mapping[str, str],
    *,
    sink=None,
) -> dict[tuple[str, str], tuple[str, ...]]:
    definitions: dict[tuple[str, str], list[str]] = {}
    for group_name, bin_name in sorted(keys):
        point_name = (
            sink.point_name(group_name, bin_name)
            if sink is not None
            else coverpoints[group_name]
        )
        definitions.setdefault((group_name, point_name), []).append(bin_name)
    return {key: tuple(value) for key, value in definitions.items()}


class NativeDomainToffeeCoverage:
    """Shared native CovGroup wrapper for domain evaluate/sample/event marks."""

    def __init__(
        self,
        runtime,
        *,
        keys: Iterable[tuple[str, str]],
        coverpoints: Mapping[str, str],
        evaluate: Callable[..., tuple[dict[tuple[str, str], bool], dict[str, Any]]] | None = None,
        reset=None,
        sink=None,
        audit_recorder=None,
        clear_flags_after_cycle: bool = True,
    ) -> None:
        self._runtime = runtime
        self._sink = sink
        self._audit_recorder = audit_recorder
        self.env = runtime.env
        self._keys = frozenset((str(g), str(b)) for g, b in keys)
        self._coverpoints = {str(k): str(v) for k, v in coverpoints.items()}
        self._coverpoints_by_key = {
            key: (
                sink.point_name(*key)
                if sink is not None
                else self._coverpoints[key[0]]
            )
            for key in self._keys
        }
        self._evaluate = evaluate
        self._reset = reset
        self._clear_flags_after_cycle = clear_flags_after_cycle
        self._view = FlagCycleView(flags={key: False for key in self._keys})
        self._groups: dict[str, CovGroup] = {}
        self.cov_groups: list[CovGroup] = []
        self._build_native_groups()
        if sink is not None and hasattr(sink, "install_native_groups"):
            sink.install_native_groups(self.cov_groups, model=self)

    def _build_native_groups(self) -> None:
        grouped: dict[str, list[tuple[str, tuple[str, ...]]]] = {}
        for (group_name, point_name), bin_names in point_definitions_from_keys(
            self._keys, self._coverpoints, sink=self._sink
        ).items():
            grouped.setdefault(group_name, []).append((point_name, bin_names))
        for group_name, point_specs in grouped.items():
            group = CovGroup(group_name, disable_sample_when_point_hinted=False)
            for point_name, bin_names in point_specs:
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

    def __getattr__(self, name):
        return getattr(self._runtime, name)

    def _empty_flags(self) -> dict[tuple[str, str], bool]:
        return {key: False for key in self._keys}

    def on_cycle(self, cycle: int) -> None:
        if self._evaluate is None:
            return
        dut = self.env.dut
        if self._runtime._read_dut_signal(dut, "reset", 0) == 1:
            if self._reset is not None:
                self._reset(self)
            self._view.flags = self._empty_flags()
            return
        self.sample(cycle)

    def sample(self, cycle: int) -> None:
        if self._evaluate is None:
            raise RuntimeError(f"{type(self).__name__} has no cycle evaluate path")
        flags, evidence = self._evaluate(self, self.env, int(cycle))
        self._view.flags = {
            key: bool(flags.get(key, False)) for key in self._keys
        }
        for group in self.cov_groups:
            group.sample()
        if self._sink is not None:
            self._sink.record_native_hits(
                self._view.flags, self._coverpoints_by_key, cycle, evidence
            )
        if self._audit_recorder is not None:
            for group_name, bin_name in self._keys:
                if not self._view.flags.get((group_name, bin_name), False):
                    continue
                self._audit_recorder.mark(
                    group_name,
                    bin_name,
                    cycle,
                    evidence,
                    coverpoint=(
                        self._sink.point_name(group_name, bin_name)
                        if self._sink is not None
                        else self._coverpoints[group_name]
                    ),
                    forward_to_toffee=False,
                )
        if self._clear_flags_after_cycle:
            self._view.flags = self._empty_flags()

    def mark(self, coverage_group, bin_name, cycle, evidence=None, *, coverpoint=None, **kwargs):
        """Immediate native sample for event/source-derived hits."""
        del coverpoint, kwargs
        key = (str(coverage_group), str(bin_name))
        if key not in self._keys:
            raise KeyError(f"unknown native coverage bin: {coverage_group}::{bin_name}")
        self._view.flags[key] = True
        group = self._groups.get(str(coverage_group))
        if group is None:
            raise KeyError(f"unknown native coverage group: {coverage_group}")
        group.sample()
        self._view.flags[key] = False
        if self._sink is not None:
            self._sink.record_native_hits(
                {key: True}, self._coverpoints_by_key, cycle, evidence
            )
        if self._audit_recorder is not None:
            audit_kwargs = {
                "coverpoint": (
                    self._sink.point_name(coverage_group, bin_name)
                    if self._sink is not None
                    else self._coverpoints[str(coverage_group)]
                ),
                "forward_to_toffee": False,
            }
            try:
                self._audit_recorder.mark(
                    coverage_group,
                    bin_name,
                    cycle,
                    evidence,
                    derive_owner=False,
                    **audit_kwargs,
                )
            except TypeError:
                self._audit_recorder.mark(
                    coverage_group,
                    bin_name,
                    cycle,
                    evidence,
                    **audit_kwargs,
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
    "EvaluateFlagRecorder",
    "FlagCycleView",
    "FlagMarkTarget",
    "NativeDomainToffeeCoverage",
    "point_definitions_from_keys",
]
