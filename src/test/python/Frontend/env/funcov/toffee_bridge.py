from __future__ import annotations

from dataclasses import dataclass, field
import csv
import json
from pathlib import Path
from typing import Any, Iterable, Mapping

from toffee.funcov import CovGroup

@dataclass
class _PointPulse:
    active_bins: set[str] = field(default_factory=set)


class ToffeeSessionCoverage:
    """Accumulate native CovGroup reports for one pytest session."""

    def __init__(self) -> None:
        self._groups: list[str] = []

    def add(self, groups: Iterable[CovGroup]) -> None:
        self.add_groups(group.as_dict() for group in groups)

    def add_groups(self, groups: Iterable[Mapping[str, Any]]) -> None:
        self._groups.extend(
            json.dumps(dict(group), ensure_ascii=False, sort_keys=True)
            for group in groups
        )

    def report(self) -> dict[str, Any]:
        from toffee_test.reporter import __update_func_coverage__

        return __update_func_coverage__(self._groups)

    def write(self, path: Path) -> Path:
        output = Path(path)
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(
            json.dumps(self.report(), ensure_ascii=False, indent=2, sort_keys=True),
            encoding="utf-8",
        )
        return output


def merge_native_toffee_reports(paths: Iterable[Path], output: Path) -> Path:
    collector = ToffeeSessionCoverage()
    for path in paths:
        raw = json.loads(Path(path).read_text(encoding="utf-8"))
        groups = raw.get("groups")
        if not isinstance(groups, list):
            raise ValueError(f"invalid native Toffee report: {path}")
        collector.add_groups(groups)
    return collector.write(output)


class ToffeeCoverageSink:
    """Route existing sampler events into native Toffee coverage groups."""

    def __init__(
        self,
        points: Mapping[tuple[str, str], Iterable[str]],
        *,
        bin_ids: Mapping[tuple[str, str, str], str] | None = None,
    ) -> None:
        self._sample_hub = None
        self.cov_groups: list[CovGroup] = []
        self._groups: dict[str, CovGroup] = {}
        self._point_targets: dict[tuple[str, str], _PointPulse] = {}
        self._point_by_group_bin: dict[tuple[str, str], str] = {}
        self._pending_cycle: int | None = None
        self._dirty_groups: set[str] = set()
        self._native_group_names: set[str] = set()
        self._native_models_by_group: dict[str, Any] = {}
        self._owner_model = None
        self.bin_ids = {
            (str(group), str(point), str(bin_name)): str(bin_id)
            for (group, point, bin_name), bin_id in (bin_ids or {}).items()
        }
        self._key_by_bin_id = {value: key for key, value in self.bin_ids.items()}

        grouped: dict[str, list[tuple[str, tuple[str, ...]]]] = {}
        for (group_name, point_name), bin_names in points.items():
            bins = tuple(str(name) for name in bin_names)
            if not group_name or not point_name or not bins:
                raise ValueError("Toffee coverage points require group, point, and bins")
            grouped.setdefault(str(group_name), []).append((str(point_name), bins))

        for group_name, point_specs in grouped.items():
            group = CovGroup(group_name, disable_sample_when_point_hinted=False)
            for point_name, bin_names in point_specs:
                target = _PointPulse()
                predicates = {}
                for bin_name in bin_names:
                    key = (group_name, bin_name)
                    if key in self._point_by_group_bin:
                        raise ValueError(f"duplicate Toffee coverage bin: {key}")
                    self._point_by_group_bin[key] = point_name
                    predicates[bin_name] = (
                        lambda current, expected=bin_name: expected in current.active_bins
                    )
                group.add_watch_point(
                    target,
                    predicates,
                    name=point_name,
                    once=False,
                )
                self._point_targets[(group_name, point_name)] = target
            self._groups[group_name] = group
            self.cov_groups.append(group)

    def attach_sample_hub(self, sample_hub) -> None:
        self._sample_hub = sample_hub

    def attach_owner_model(self, owner_model) -> None:
        self._owner_model = owner_model

    def __getattr__(self, name: str):
        hub = self.__dict__.get("_sample_hub")
        if hub is None:
            raise AttributeError(name)
        return getattr(hub, name)

    def install_native_groups(self, groups: Iterable[CovGroup], *, model=None) -> None:
        """Replace pulse-bridge groups with already-sampled native CovGroups."""
        for group in groups:
            group_name = str(getattr(group, "name", "") or "")
            if not group_name:
                raise ValueError("native CovGroup requires a name")
            self._native_group_names.add(group_name)
            if model is not None:
                self._native_models_by_group[group_name] = model
            self._groups[group_name] = group
            replaced = False
            updated_groups: list[CovGroup] = []
            for existing in self.cov_groups:
                if str(getattr(existing, "name", "")) == group_name:
                    updated_groups.append(group)
                    replaced = True
                else:
                    updated_groups.append(existing)
            if not replaced:
                updated_groups.append(group)
            self.cov_groups = updated_groups
            for point_key in [
                key for key in self._point_targets if key[0] == group_name
            ]:
                self._point_targets.pop(point_key, None)
            self._dirty_groups.discard(group_name)

    def mark_native(
        self,
        coverage_group: str,
        bin_name: str,
        cycle: int,
        evidence: Mapping[str, Any] | None = None,
    ) -> bool:
        model = self._native_models_by_group.get(str(coverage_group))
        if model is None:
            raise KeyError(
                f"native Toffee model is not registered: {coverage_group}::{bin_name}"
            )
        group_name = str(coverage_group)
        bin_name = str(bin_name)
        key = (group_name, bin_name)
        flags = model._view.flags
        flag_key = key if key in flags else bin_name
        if flag_key not in flags:
            raise KeyError(f"unknown native Toffee model bin: {group_name}::{bin_name}")
        group = model._groups.get(group_name)
        if group is None:
            raise KeyError(f"unknown native Toffee model group: {group_name}")

        flags[flag_key] = True
        group.sample()
        flags[flag_key] = False
        point_name = self.point_name(group_name, bin_name)
        self.record_native_hits(
            {key: True}, {key: point_name}, cycle, evidence
        )

        audit_recorder = getattr(model, "_audit_recorder", None)
        if audit_recorder is not None:
            kwargs = {
                "coverpoint": point_name,
                "forward_to_toffee": False,
            }
            try:
                audit_recorder.mark(
                    group_name,
                    bin_name,
                    cycle,
                    evidence,
                    derive_owner=False,
                    **kwargs,
                )
            except TypeError:
                audit_recorder.mark(group_name, bin_name, cycle, evidence, **kwargs)
        return True

    @classmethod
    def from_registry(cls, csv_path: Path) -> "ToffeeCoverageSink":
        points: dict[tuple[str, str], list[str]] = {}
        bin_ids: dict[tuple[str, str, str], str] = {}
        with Path(csv_path).open(encoding="utf-8-sig", newline="") as handle:
            for row in csv.DictReader(handle):
                group = str(row.get("Coverage_Group") or "").strip()
                point = str(row.get("Coverpoint") or "").strip()
                bin_name = str(row.get("Bin_Name") or "").strip()
                bin_id = str(row.get("Bin_ID") or "").strip()
                if not point:
                    continue
                key = (group, point)
                points.setdefault(key, []).append(bin_name)
                bin_ids[(group, point, bin_name)] = bin_id
        return cls(points, bin_ids=bin_ids)

    def mark(
        self,
        coverage_group: str,
        bin_name: str,
        cycle: int,
        evidence: Mapping[str, Any] | None = None,
        *,
        coverpoint: str | None = None,
    ) -> bool:
        cycle = int(cycle)
        if self._pending_cycle is not None and cycle != self._pending_cycle:
            self.flush_cycle(self._pending_cycle)
        self._pending_cycle = cycle
        group_name = str(coverage_group)
        bin_name = str(bin_name)
        if group_name in self._native_group_names:
            raise RuntimeError(
                f"native Toffee group rejects mark-bridge hits: {group_name}::{bin_name}"
            )
        expected_point = self._point_by_group_bin.get((group_name, bin_name))
        if expected_point is None:
            raise KeyError(f"unknown Toffee coverage bin: {group_name}::{bin_name}")
        point_name = (
            str(coverpoint)
            if coverpoint is not None
            else expected_point
        )
        if point_name != expected_point:
            raise KeyError(
                "Toffee coverage point mismatch: "
                f"{group_name}::{point_name}::{bin_name}; expected {expected_point}"
            )

        target = self._point_targets.get((group_name, point_name))
        group = self._groups.get(group_name)
        if target is None or group is None:
            raise KeyError(
                f"unknown Toffee coverage point: {group_name}::{point_name}::{bin_name}"
            )

        target.active_bins.add(bin_name)
        self._dirty_groups.add(group_name)
        return True

    def on_cycle(self, cycle: int) -> None:
        """Keep a common callback contract for cycle-driven Toffee sinks."""
        del cycle

    def flush_cycle(self, cycle: int) -> None:
        cycle = int(cycle)
        if self._pending_cycle is None or cycle != self._pending_cycle:
            return
        for group_name in sorted(self._dirty_groups):
            if group_name in self._native_group_names:
                continue
            self._groups[group_name].sample()
        for (group_name, _point_name), target in self._point_targets.items():
            if group_name in self._native_group_names:
                continue
            target.active_bins.clear()
        self._dirty_groups.clear()
        self._pending_cycle = None

    def flush_pending(self) -> None:
        if self._pending_cycle is not None:
            self.flush_cycle(self._pending_cycle)

    def report(self) -> list[dict[str, Any]]:
        self.flush_pending()
        return [group.as_dict() for group in self.cov_groups]

    def hit_counts(self) -> dict[tuple[str, str, str], int]:
        return {
            (group["name"], point["name"], item["name"]): int(item["hints"])
            for group in self.report()
            for point in group["points"]
            for item in point["bins"]
        }

    def point_name(self, group_name: str, bin_name: str) -> str:
        point_name = self._point_by_group_bin.get((str(group_name), str(bin_name)))
        if point_name is None:
            raise KeyError(f"unknown Toffee coverage bin: {group_name}::{bin_name}")
        return point_name

    def pending_hit_keys(self) -> set[tuple[str, str, str]]:
        pending: set[tuple[str, str, str]] = set()
        for (group_name, point_name), target in self._point_targets.items():
            if group_name in self._native_group_names:
                continue
            for bin_name in target.active_bins:
                pending.add((group_name, point_name, str(bin_name)))
        return pending

    def _flushed_hint_count(self, key: tuple[str, str, str]) -> int:
        group_name, point_name, bin_name = key
        group = self._groups.get(group_name)
        if group is None:
            return 0
        point = group.cover_point(point_name)
        return int(point["hints"].get(bin_name, 0))

    def hit_count_by_bin_id(self, bin_id: str) -> int:
        key = self._key_by_bin_id.get(str(bin_id))
        if key is None:
            return 0
        count = self._flushed_hint_count(key)
        if key in self.pending_hit_keys():
            count += 1
        return count

    def record_native_hits(
        self,
        flags: Mapping[tuple[str, str], bool],
        coverpoints: Mapping[Any, str],
        cycle: int,
        evidence: Mapping[str, Any] | None = None,
    ) -> None:
        active_items = [key for key, active in flags.items() if active]
        if not active_items or not self.bin_ids:
            return

        for group_name, bin_name in active_items:
            point_name = self.point_name(group_name, bin_name)
            declared_point = coverpoints.get((str(group_name), str(bin_name)))
            if declared_point is None:
                declared_point = coverpoints.get(str(group_name))
            if declared_point is not None and str(declared_point) != point_name:
                raise ValueError(
                    "native Toffee point mismatch: "
                    f"{group_name}::{bin_name} declares {declared_point}, "
                    f"registry requires {point_name}"
                )
            key = (str(group_name), point_name, str(bin_name))
            if key not in self.bin_ids:
                raise KeyError(f"unknown native Toffee evidence key: {key}")
            if self._owner_model is not None:
                self._owner_model.derive_from_source(
                    self.bin_ids[key], int(cycle), evidence
                )

    def key_hit(
        self,
        coverage_group: str,
        bin_name: str,
        *,
        coverpoint: str | None = None,
    ) -> bool:
        group_name = str(coverage_group)
        bin_name = str(bin_name)
        point_name = (
            self._point_by_group_bin.get((group_name, bin_name))
            if coverpoint is None
            else str(coverpoint)
        )
        if point_name is None:
            return False
        key = (group_name, point_name, bin_name)
        if key in self.pending_hit_keys():
            return True
        self.flush_pending()
        point = self._groups[group_name].cover_point(point_name)
        return int(point["hints"].get(bin_name, 0)) > 0
