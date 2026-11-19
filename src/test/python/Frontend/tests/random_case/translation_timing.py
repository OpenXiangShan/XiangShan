"""DUT-side construction for the replayable translation random family.

The assembly binary remains linked at the NEMU virtual address.  During a DUT
run its bytes are installed at the translated physical address and the same
program seed/timing seed pair selects the translation mode and transport
disturbance.  This keeps the golden trace authoritative while making the
translation controls real DUT inputs.
"""

from __future__ import annotations

from dataclasses import replace

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioRandomizer,
)
from env.support import PmpPmaConfig
from env.support.pmp_pma import PMP_PMA_ENTRY_COUNT

try:
    from .translation_attributes import (
        PBMT_PMA,
        attribute_plan_from_dict,
        translation_attribute_plan,
    )
except ImportError:
    from translation_attributes import PBMT_PMA, attribute_plan_from_dict, translation_attribute_plan


PAGE_SIZE = 0x1000
TRANSLATION_SCHEMA_VERSION = 1


def _page_count(base: int, payload_size: int) -> int:
    offset = int(base) & (PAGE_SIZE - 1)
    return max(1, (offset + max(1, int(payload_size)) + PAGE_SIZE - 1) // PAGE_SIZE)


def _executable_pte(
    pte: TranslationPte,
    *,
    vmid: int | None = None,
    pbmt: int = 0,
) -> TranslationPte:
    return replace(
        pte,
        v=1,
        r=1,
        w=0,
        x=1,
        u=0,
        a=1,
        d=0,
        vmid=int(pte.vmid if vmid is None else vmid),
        pbmt=int(pbmt),
    )


def _physical_permissions(
    physical_base: int,
    pages: int,
    *,
    pma_pattern: tuple[bool, ...],
    layout: str = "per_page_v1",
):
    if layout not in {"per_page_v1", "exact_napot_v1"}:
        raise ValueError(f"unsupported translation permission layout: {layout}")
    if int(physical_base) < 0 or int(physical_base) % PAGE_SIZE or int(pages) < 1:
        raise ValueError("translation physical permissions require an aligned base and positive pages")
    if not pma_pattern:
        raise ValueError("translation PMA pattern must not be empty")
    pmp = tuple(
        TranslationPmpPmaEntry(
            "pmp",
            index,
            PmpPmaConfig(match="napot", read=True, execute=True),
            int(physical_base) + index * PAGE_SIZE,
            size=PAGE_SIZE,
        )
        for index in range(int(pages))
    )
    pma = tuple(
        TranslationPmpPmaEntry(
            "pma",
            index,
            PmpPmaConfig(
                match="napot",
                read=True,
                execute=True,
                cacheable=bool(pma_pattern[index % len(pma_pattern)]),
            ),
            int(physical_base) + index * PAGE_SIZE,
            size=PAGE_SIZE,
        )
        for index in range(int(pages))
    )
    if layout == "exact_napot_v1":
        pmp, pma = _coalesce_page_entries(pmp), _coalesce_page_entries(pma)
        for entries in (pmp, pma):
            if len(entries) > PMP_PMA_ENTRY_COUNT:
                raise ValueError(
                    f"translation {entries[0].kind} layout requires {len(entries)} entries; "
                    f"limit={PMP_PMA_ENTRY_COUNT}; cannot merge across attribute boundaries"
                )
    return pmp, pma


def _coalesce_page_entries(entries: tuple[TranslationPmpPmaEntry, ...]):
    """Exact byte-region cover of ordered page entries with equal attributes.

    This opt-in stimulus layout changes CSR indices and same-attribute entry
    boundaries, not the checker. It is not equivalence for arbitrary accesses
    spanning old PMP boundaries, nor suitable for entry-priority tests.
    """
    result = []
    first = 0
    while first < len(entries):
        entry = entries[first]
        end = first + 1
        while (end < len(entries) and entries[end].config == entry.config
               and entries[end].addr == entries[end - 1].addr + PAGE_SIZE):
            end += 1
        addr = entry.addr
        remaining = (end - first) * PAGE_SIZE
        while remaining:
            size = 1 << (remaining.bit_length() - 1)
            while addr % size:
                size //= 2
            result.append(replace(entry, index=len(result), addr=addr, size=size))
            addr += size
            remaining -= size
        first = end
    return tuple(result)


def build_translation_scenario(
    scenario_spec,
    payload: bytes,
    *,
    virtual_base: int = 0x80000000,
    physical_base: int = 0x80400000,
    guest_physical_base: int = 0x80600000,
) -> TranslationScenario:
    """Build a successful, long-running scenario from a scenario contract.

    The underlying randomizer contains fault variants for directed translation
    tests.  A long golden-trace case must complete, so this family keeps the
    address-space and timing dimensions random while normalizing permission
    faults; fault variants remain covered by the dedicated directed suites.
    """

    raw = scenario_spec.parameters.get("translation")
    if not isinstance(raw, dict) or int(raw.get("schema_version", 0)) != TRANSLATION_SCHEMA_VERSION:
        raise ValueError("translation_timing_cross requires a versioned translation profile")
    seed = int(raw["randomizer_seed"])
    # The scenario contract carries the ordinal as the authoritative replay
    # coordinate.  Older/generated payloads also keep a copy under
    # parameters.translation; prefer the contract field so callers cannot
    # accidentally replay every case as ordinal zero.
    ordinal = int(getattr(scenario_spec, "ordinal", raw["ordinal"]))
    # Avoid the bare/no-stage variant: it cannot exercise a PTW request while
    # still sharing the NEMU virtual reset address.
    source_ordinal = ordinal + 1
    if source_ordinal % 6 == 0:
        source_ordinal += 1
    generated = TranslationScenarioRandomizer(seed).next(source_ordinal).scenario
    pages = _page_count(virtual_base, len(payload))
    raw_plan = raw.get("attribute_plan")
    plan = (
        attribute_plan_from_dict(raw_plan)
        if isinstance(raw_plan, dict)
        else translation_attribute_plan(ordinal)
    )
    pma_pattern = plan.for_pages(pages)
    pbmt_pattern = plan.pbmt_for_pages(pages)
    pmp_entries, pma_entries = _physical_permissions(
        physical_base,
        pages,
        pma_pattern=pma_pattern,
        layout=str(raw.get("permission_layout", "per_page_v1")),
    )

    # The current Frontend DUT integration has a verified Sv39/non-virtualized
    # entry path (the same path used by the existing translation timing tests).
    # Keep this first random family on that path while still varying PTW
    # latency/ready and physical attributes.  Virtualized/all-stage variants
    # remain in TranslationScenarioRandomizer and will be promoted after a
    # separate DUT smoke proves their reset/CSR contract.
    s2xlate = 0
    s1_pte = replace(_executable_pte(generated.s1_pte, pbmt=pbmt_pattern[0]), level=0)
    s2_pte = _executable_pte(generated.s2_pte, vmid=0, pbmt=pbmt_pattern[0])
    # Preserve the generated translation kind and transport policy, but make
    # every selected page executable so the long trace can reach completion.
    return replace(
        generated,
        scenario_id=str(scenario_spec.scenario_id),
        va=int(virtual_base),
        pa=int(physical_base),
        payload=bytes(payload),
        page_count=int(pages),
        s1_pte=s1_pte,
        s1_page_pbmt=(pbmt_pattern if len(set(pbmt_pattern)) > 1 else ()),
        s2_pte=s2_pte,
        mode="sv39",
        stage2_mode=None,
        gpa=None,
        priv_virt=0,
        s2xlate=s2xlate,
        s1_pf=0,
        s1_af=0,
        s2_gpf=0,
        s2_gaf=0,
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
        s1_sector_lanes=(),
        permission_probes=(),
        # The long program is entered in supervisor mode.  Keeping this
        # explicit avoids randomizer-generated user-mode PTE combinations
        # turning a timing case into an immediate permission-fault case.
        priv_imode=1,
        # PBMTE only enables interpretation of non-PMA PBMT encodings.  A
        # PBMT.PMA PTE is independent of that CSR, so do not pretend to drive
        # a zero through a generated DUT that does not export the pin.
        ptw_machine_pbmte=(
            None if all(int(pbmt) == PBMT_PMA for pbmt in pbmt_pattern) else int(plan.pbmte)
        ),
        expected_path=(
            "cacheable" if pbmt_pattern[0] == PBMT_PMA and pma_pattern[0] else "uncache"
        ),
        expected_result="normal",
    )


__all__ = ["build_translation_scenario"]
