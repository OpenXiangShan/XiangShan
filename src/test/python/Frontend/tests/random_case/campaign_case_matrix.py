"""Pure selection of per-family quotas for the random campaign."""
from __future__ import annotations

if __package__:
    from .generate_random_asm import FAMILIES
else:
    from generate_random_asm import FAMILIES


def _parse_family_count_overrides(values: list[str]) -> dict[str, int]:
    """Parse explicit per-family quotas without making generation implicit."""

    overrides: dict[str, int] = {}
    for raw_value in values:
        family, separator, raw_count = str(raw_value).partition("=")
        if separator != "=" or not family or not raw_count:
            raise ValueError(
                "--family-count must use FAMILY=COUNT, for example "
                "--family-count cfg_layout_mix=33"
            )
        if family not in FAMILIES:
            raise ValueError(f"unsupported --family-count family: {family}")
        try:
            count = int(raw_count, 0)
        except ValueError as exc:
            raise ValueError(f"invalid --family-count value: {raw_value}") from exc
        if count <= 0:
            raise ValueError(f"--family-count must be > 0: {raw_value}")
        if family in overrides:
            raise ValueError(f"duplicate --family-count family: {family}")
        overrides[family] = count
    return overrides


def _resolve_family_case_counts(
    requested_families: list[str],
    overrides: dict[str, int],
    default_count: int,
) -> tuple[list[str], dict[str, int]]:
    """Return the exact, ordered generation matrix for a new campaign."""

    if default_count <= 0:
        raise ValueError("default family count must be > 0")
    if requested_families:
        families = list(requested_families)
        if len(set(families)) != len(families):
            raise ValueError("--family must not name the same family more than once")
    elif overrides:
        # A quota-only invocation should generate exactly the requested
        # families. This avoids accidentally adding every family at --count.
        families = list(overrides)
    else:
        families = list(FAMILIES)
    unexpected = sorted(set(overrides) - set(families))
    if unexpected:
        raise ValueError(
            "--family-count names a family not selected by --family: "
            + ", ".join(unexpected)
        )
    return families, {family: int(overrides.get(family, default_count)) for family in families}
