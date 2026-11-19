"""Deterministic physical-attribute plans for random translation cases.

The plan is part of the random scenario contract rather than an incidental
property of the DUT builder.  Keeping it as a small pure module makes a seed
replay explainable: the ordinal selects one reviewed profile, and the profile
expands to the concrete PMA page pattern once the binary size is known.
"""

from __future__ import annotations

from dataclasses import dataclass


ATTRIBUTE_PLAN_SCHEMA_VERSION = 1
PBMT_PMA = 0
PBMT_NC = 1
PBMT_IO = 2


@dataclass(frozen=True)
class TranslationAttributePlan:
    """One legal, executable attribute combination for a random case."""

    profile: str
    pbmt: int
    pbmte: int
    pma_pattern: tuple[bool, ...]
    pbmt_pattern: tuple[int, ...] = ()

    def validate(self) -> None:
        if not self.profile.strip():
            raise ValueError("translation attribute profile must be non-empty")
        if int(self.pbmt) not in {PBMT_PMA, PBMT_NC, PBMT_IO}:
            raise ValueError(f"unsupported translation PBMT: {self.pbmt}")
        if int(self.pbmte) not in {0, 1}:
            raise ValueError("translation PBMTE must be 0 or 1")
        if int(self.pbmt) != PBMT_PMA and int(self.pbmte) != 1:
            raise ValueError("non-PMA PBMT requires PBMTE enabled")
        if not self.pma_pattern:
            raise ValueError("translation PMA pattern must not be empty")
        if any(int(value) not in {PBMT_PMA, PBMT_NC, PBMT_IO} for value in self.pbmt_pattern):
            raise ValueError("translation PBMT pattern contains an unsupported value")
        if any(int(value) != PBMT_PMA for value in self.pbmt_pattern) and int(self.pbmte) != 1:
            raise ValueError("non-PMA PBMT pattern requires PBMTE enabled")

    def for_pages(self, pages: int) -> tuple[bool, ...]:
        """Expand the reviewed pattern to exactly ``pages`` physical pages."""

        self.validate()
        count = int(pages)
        if count < 1:
            raise ValueError("translation page count must be positive")
        pattern = tuple(bool(value) for value in self.pma_pattern)
        return tuple(pattern[index % len(pattern)] for index in range(count))

    def pbmt_for_pages(self, pages: int) -> tuple[int, ...]:
        """Expand the optional per-page PBMT contract for one replay."""

        self.validate()
        count = int(pages)
        if count < 1:
            raise ValueError("translation PBMT page count must be positive")
        pattern = self.pbmt_pattern or (int(self.pbmt),)
        return tuple(int(pattern[index % len(pattern)]) for index in range(count))

    def to_dict(self) -> dict[str, object]:
        self.validate()
        payload = {
            "schema_version": ATTRIBUTE_PLAN_SCHEMA_VERSION,
            "profile": self.profile,
            "pbmt": int(self.pbmt),
            "pbmte": int(self.pbmte),
            "pma_pattern": [int(value) for value in self.pma_pattern],
        }
        if self.pbmt_pattern:
            payload["pbmt_pattern"] = [int(value) for value in self.pbmt_pattern]
        return payload


# The first four profiles isolate PMA/PBMT precedence.  The last two exercise
# page transitions while retaining a normal, executable mapping on every page.
_PLANS: tuple[TranslationAttributePlan, ...] = (
    TranslationAttributePlan("pma_cacheable", PBMT_PMA, 0, (True,)),
    TranslationAttributePlan("pma_non_cacheable", PBMT_PMA, 0, (False,)),
    TranslationAttributePlan("pbmt_nc_over_cacheable_pma", PBMT_NC, 1, (True,)),
    TranslationAttributePlan("pbmt_io_over_cacheable_pma", PBMT_IO, 1, (True,)),
    TranslationAttributePlan("pma_cacheable_to_non_cacheable", PBMT_PMA, 0, (True, False)),
    TranslationAttributePlan("pma_non_cacheable_to_cacheable", PBMT_PMA, 0, (False, True)),
)


def translation_attribute_plan(ordinal: int) -> TranslationAttributePlan:
    """Return the deterministic profile selected by a case ordinal."""

    index = int(ordinal)
    if index < 0:
        raise ValueError("translation attribute ordinal must be non-negative")
    plan = _PLANS[index % len(_PLANS)]
    plan.validate()
    return plan


def attribute_plan_from_dict(payload: dict[str, object]) -> TranslationAttributePlan:
    """Parse a scenario-contract plan and reject incompatible schemas."""

    if int(payload.get("schema_version", 0)) != ATTRIBUTE_PLAN_SCHEMA_VERSION:
        raise ValueError("unsupported translation attribute plan schema")
    plan = TranslationAttributePlan(
        profile=str(payload["profile"]),
        pbmt=int(payload["pbmt"]),
        pbmte=int(payload["pbmte"]),
        pma_pattern=tuple(bool(int(value)) for value in payload["pma_pattern"]),
        pbmt_pattern=tuple(int(value) for value in payload.get("pbmt_pattern", ())),
    )
    plan.validate()
    return plan


__all__ = [
    "ATTRIBUTE_PLAN_SCHEMA_VERSION",
    "PBMT_PMA",
    "PBMT_NC",
    "PBMT_IO",
    "TranslationAttributePlan",
    "attribute_plan_from_dict",
    "translation_attribute_plan",
]
