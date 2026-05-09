from __future__ import annotations

from dataclasses import dataclass, field
from importlib import import_module
from typing import Any, Dict


@dataclass(frozen=True)
class PTWRequestSnapshot:
    sequence_id: int
    cycle: int
    vpn: int
    s2xlate: int
    get_gpa: int
    memidx_is_ld: int
    memidx_is_st: int
    memidx_idx: int
    priv_imode: int
    satp_mode: int
    vsatp_mode: int
    hgatp_mode: int
    sfence_valid: int
    sfence_bits_rs1: int
    sfence_bits_rs2: int
    sfence_bits_addr: int
    sfence_bits_id: int
    sfence_bits_hv: int
    sfence_bits_hg: int
    extra: Dict[str, int] = field(default_factory=dict)

    def as_dict(self) -> Dict[str, Any]:
        data = {
            "sequence_id": int(self.sequence_id),
            "cycle": int(self.cycle),
            "vpn": int(self.vpn),
            "s2xlate": int(self.s2xlate),
            "get_gpa": int(self.get_gpa),
            "memidx_is_ld": int(self.memidx_is_ld),
            "memidx_is_st": int(self.memidx_is_st),
            "memidx_idx": int(self.memidx_idx),
            "priv_imode": int(self.priv_imode),
            "satp_mode": int(self.satp_mode),
            "vsatp_mode": int(self.vsatp_mode),
            "hgatp_mode": int(self.hgatp_mode),
            "sfence_valid": int(self.sfence_valid),
            "sfence_bits_rs1": int(self.sfence_bits_rs1),
            "sfence_bits_rs2": int(self.sfence_bits_rs2),
            "sfence_bits_addr": int(self.sfence_bits_addr),
            "sfence_bits_id": int(self.sfence_bits_id),
            "sfence_bits_hv": int(self.sfence_bits_hv),
            "sfence_bits_hg": int(self.sfence_bits_hg),
        }
        data.update({str(key): int(value) for key, value in dict(self.extra).items()})
        return data


class NemuPtwResponseSource:
    def __init__(self, adapter_spec: str) -> None:
        spec = str(adapter_spec or "")
        if ":" not in spec:
            raise RuntimeError(f"invalid NEMU PTW adapter spec: {spec!r}")
        module_name, builder_name = spec.split(":", 1)
        if not module_name or not builder_name:
            raise RuntimeError(f"invalid NEMU PTW adapter spec: {spec!r}")
        module = import_module(module_name)
        builder = getattr(module, builder_name, None)
        if not callable(builder):
            raise RuntimeError(f"NEMU PTW adapter builder is not callable: {spec!r}")
        self.adapter_spec = spec
        self.module = module
        self.builder = builder

    def build_response(self, snapshot: PTWRequestSnapshot) -> dict:
        response = self.builder(snapshot)
        if response is None:
            raise RuntimeError(f"NEMU PTW adapter returned None: {self.adapter_spec}")
        return dict(response)


__all__ = ["NemuPtwResponseSource", "PTWRequestSnapshot"]
