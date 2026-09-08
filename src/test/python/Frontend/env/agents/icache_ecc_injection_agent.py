"""Fault injection into real ICache SRAM words in the V3 parity configuration.

The MEM_DIRECT array API is used, never coverage flags or pipeline outputs.
Dimensions and packing are checked before writing. Unsupported builds fail
closed; they must supply a matching SRAM adapter instead of guessing offsets.
"""

from __future__ import annotations

from dataclasses import dataclass


class ICacheInjectionUnavailable(RuntimeError):
    pass


@dataclass
class SRAMMutation:
    path: str
    row: int
    mask: int
    before: int
    after: int
    kind: str
    paddr: int
    way: int
    restored: bool = False


class ICacheECCInjectionAgent:
    """Explicit, reversible code-bit corruption for 256 sets / 4 ways / 8 banks."""

    ROOT = "Frontend_top.Frontend.inner_icache."
    META_BITS = 69  # {phyTag[35:0], maybeRvcMap[31:0], code}
    DATA_BITS = 66  # {data[63:0], code, padding}

    def __init__(self, env) -> None:
        self.env = env
        self._arrays = {}
        self.mutations: list[SRAMMutation] = []

    def _event(self, kind: str, payload: dict) -> None:
        self.env._emit_event("injection.icache_sram." + kind, payload)
        recorder = getattr(self.env, "functional_coverage", None)
        if recorder is not None:
            recorder.risk_observations.append({"cycle": int(self.env.current_cycle),
                "risk": "explicit_sram_fault_injection", "action": kind, **payload})

    def _array(self, path: str, *, rows: int, width: int):
        if path not in self._arrays:
            getter = getattr(self.env.dut, "GetInternalSignal", None)
            if not callable(getter):
                raise ICacheInjectionUnavailable("DUT has no internal SRAM access API")
            try:
                pins = getter(path, is_array=True)
                if not isinstance(pins, (list, tuple)) or len(pins) != rows:
                    raise ValueError(f"expected {rows} SRAM rows")
                if any(int(pin.W()) != width for pin in pins):
                    raise ValueError(f"expected {width}-bit SRAM words")
                if any(not callable(getattr(pin, "ImmSet", None)) for pin in pins):
                    raise ValueError("SRAM words do not support immediate deposit")
            except Exception as exc:
                raise ICacheInjectionUnavailable(f"SRAM layout/access unavailable: {path}: {exc}") from exc
            self._arrays[path] = pins
        return self._arrays[path]

    @staticmethod
    def _set(paddr: int, vaddr: int | None) -> int:
        for address in (paddr, paddr if vaddr is None else vaddr):
            if not 0 <= int(address) < (1 << 48):
                raise ValueError("ICache address must fit the supported 48-bit layout")
        return ((int(paddr) if vaddr is None else int(vaddr)) >> 6) & 255

    def _meta(self, vset: int, way: int):
        if not 0 <= int(way) < 4:
            raise ValueError("way must be 0..3")
        path = self.ROOT + (
            f"metaArray.banks_{vset & 1}.tagArray.array_0_{way // 2}_0.array.array_ext.Memory"
        )
        row = vset >> 1
        return path, row, self._array(path, rows=128, width=138)[row], (way % 2) * self.META_BITS

    def _valid_mask(self, vset: int) -> int:
        path = self.ROOT + f"metaArray.banks_{vset & 1}.validArray_{vset >> 1}"
        try:
            pin = self.env.dut.GetInternalSignal(path)
            if pin is None or int(pin.W()) != 4:
                raise ValueError("expected a 4-bit valid mask")
            return int(pin.value)
        except Exception as exc:
            raise ICacheInjectionUnavailable(f"valid array unavailable: {path}: {exc}") from exc

    def resident_ways(self, paddr: int, *, vaddr: int | None = None) -> tuple[int, ...]:
        vset = self._set(paddr, vaddr)
        valid = self._valid_mask(vset)
        matches = []
        for way in range(4):
            _, _, pin, shift = self._meta(vset, way)
            tag = (int(pin.value) >> (shift + 33)) & ((1 << 36) - 1)
            if (valid >> way) & 1 and tag == int(paddr) >> 12:
                matches.append(way)
        return tuple(matches)

    def wait_resident(self, paddr: int, *, vaddr: int | None = None, max_cycles: int = 2048) -> int:
        for _ in range(int(max_cycles) + 1):
            ways = self.resident_ways(paddr, vaddr=vaddr)
            if len(ways) == 1:
                return ways[0]
            if len(ways) > 1:
                raise AssertionError(f"line already has multiple matching ways: {ways}")
            if _ < int(max_cycles):
                self.env.step(1)
        raise AssertionError(f"cache line 0x{int(paddr):x} did not become resident")

    def read_resident_line(self, paddr: int, *, vaddr: int | None = None) -> bytes:
        """Read back the data words paired with the line's unique resident tag."""
        ways = self.resident_ways(paddr, vaddr=vaddr)
        if len(ways) != 1:
            raise AssertionError(f"data readback requires one resident way, got {ways}")
        vset = self._set(paddr, vaddr)
        words = []
        for bank in range(8):
            path = self.ROOT + f"dataArray.banks_{bank}.ways_{ways[0]}.array.array_ext.Memory"
            raw = int(self._array(path, rows=256, width=self.DATA_BITS)[vset].value)
            # ICacheDataEntry packs {data[63:0], code, padding}; V3 parity
            # has one bit each for code and padding below the data field.
            words.append((raw >> 2) & ((1 << 64) - 1))
        return b"".join(word.to_bytes(8, "little") for word in words)

    def _mutate(self, pin, *, path, row, mask, new_bits, kind, paddr, way) -> SRAMMutation:
        before = int(pin.value)
        after = (before & ~mask) | (int(new_bits) & mask)
        if before == after:
            raise ValueError("injection must change SRAM contents")
        pin.ImmSet(after)
        if int(pin.value) != after:
            raise ICacheInjectionUnavailable(f"SRAM deposit did not read back: {path}[{row}]")
        mutation = SRAMMutation(path, row, mask, before, after, kind, int(paddr), int(way))
        self.mutations.append(mutation)
        self._event("deposit", vars(mutation).copy())
        return mutation

    def inject_meta_ecc(self, paddr: int, *, vaddr: int | None = None) -> SRAMMutation:
        ways = self.resident_ways(paddr, vaddr=vaddr)
        if len(ways) != 1:
            raise AssertionError(f"Meta ECC requires one resident way, got {ways}")
        path, row, pin, shift = self._meta(self._set(paddr, vaddr), ways[0])
        mask = 1 << shift
        return self._mutate(pin, path=path, row=row, mask=mask, new_bits=int(pin.value) ^ mask,
                            kind="meta_code", paddr=paddr, way=ways[0])

    def inject_data_ecc(self, paddr: int, *, bank: int, vaddr: int | None = None) -> SRAMMutation:
        if not 0 <= int(bank) < 8:
            raise ValueError("data bank must be 0..7")
        ways = self.resident_ways(paddr, vaddr=vaddr)
        if len(ways) != 1:
            raise AssertionError(f"Data ECC requires one resident way, got {ways}")
        path = self.ROOT + f"dataArray.banks_{int(bank)}.ways_{ways[0]}.array.array_ext.Memory"
        row = self._set(paddr, vaddr)
        pin = self._array(path, rows=256, width=self.DATA_BITS)[row]
        return self._mutate(pin, path=path, row=row, mask=2, new_bits=int(pin.value) ^ 2,
                            kind="data_code", paddr=paddr, way=ways[0])

    def clone_meta_to_second_way(self, paddr: int, *, dest_way: int, vaddr: int | None = None) -> SRAMMutation:
        """Create a multi-hit using an already valid way; valid bits remain RTL-owned."""
        vset = self._set(paddr, vaddr)
        ways = self.resident_ways(paddr, vaddr=vaddr)
        if len(ways) != 1 or dest_way == ways[0] or not 0 <= int(dest_way) < 4:
            raise ValueError("clone requires one resident source and a distinct destination way")
        if not ((self._valid_mask(vset) >> dest_way) & 1):
            raise AssertionError("warm a second same-set line before cloning metadata")
        _, _, src, src_shift = self._meta(vset, ways[0])
        path, row, dst, shift = self._meta(vset, dest_way)
        mask = (1 << self.META_BITS) - 1
        bits = (int(src.value) >> src_shift) & mask
        return self._mutate(dst, path=path, row=row, mask=mask << shift, new_bits=bits << shift,
                            kind="meta_multiway", paddr=paddr, way=dest_way)

    def verify_persisted(self, mutation: SRAMMutation) -> None:
        pin = self._arrays[mutation.path][mutation.row]
        if int(pin.value) & mutation.mask != mutation.after & mutation.mask:
            raise AssertionError(f"SRAM injection was overwritten: {mutation.path}[{mutation.row}]")
        self._event("persisted", vars(mutation).copy())

    def restore_all(self) -> None:
        """Restore only unchanged injected fields, preserving intervening RTL refills."""
        for item in reversed(self.mutations):
            if item.restored:
                continue
            pin = self._arrays[item.path][item.row]
            current = int(pin.value)
            # A different tag/data means this word was replaced; never put old bits back.
            field_mask = ((1 << self.META_BITS) - 1) << ((item.way % 2) * self.META_BITS) if "meta" in item.kind else (1 << self.DATA_BITS) - 1
            if (current & field_mask) == (item.after & field_mask):
                pin.ImmSet((current & ~item.mask) | (item.before & item.mask))
                if int(pin.value) & item.mask != item.before & item.mask:
                    raise ICacheInjectionUnavailable("SRAM restoration did not read back")
                self._event("restore", vars(item).copy())
            else:
                self._event("restore_skipped_replaced_word", vars(item).copy())
            item.restored = True
