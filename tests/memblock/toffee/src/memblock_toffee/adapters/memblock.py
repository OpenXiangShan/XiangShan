from __future__ import annotations

import json
import importlib
import os
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from .picker import PickerDutAdapter, ToffeePin


DEFAULT_DUT_PACKAGE = "MemBlock"
DEFAULT_DUT_CLASS = "DUTMemBlock"


@dataclass(frozen=True, slots=True)
class PortSpec:
    name: str
    direction: str
    width: int
    input_policy: str | None
    protocol: str
    role: str
    group: str


LOGICAL_PINS = {
    "reset": "reset",
    "l2_flush_enable": "io_ooo_to_mem_csrCtrl_flush_l2_enable",
    "l2_flush_done": "io_l2_flush_done",
    "outer_l2_flush_enable": "io_outer_l2_flush_en",
    "backend_l2_flush_done": "io_mem_to_ooo_topToBackendBypass_l2FlushDone",
    "backend_reset": "io_reset_backend",
    "uncache_a_valid": "auto_inner_buffers_out_a_valid",
    "frontend_instr_uncache_in_d_valid":
        "auto_inner_frontendBridge_instr_uncache_in_d_valid",
    "frontend_instr_uncache_out_a_valid":
        "auto_inner_frontendBridge_instr_uncache_out_a_valid",
    "frontend_icachectrl_in_d_valid":
        "auto_inner_frontendBridge_icachectrl_in_d_valid",
    "frontend_icachectrl_out_a_valid":
        "auto_inner_frontendBridge_icachectrl_out_a_valid",
    "frontend_icache_in_d_valid": "auto_inner_frontendBridge_icache_in_d_valid",
    "frontend_icache_out_a_valid": "auto_inner_frontendBridge_icache_out_a_valid",
    "ptw_to_l2_a_valid": "auto_inner_ptw_to_l2_buffer_out_a_valid",
    "dcache_a_valid": "auto_inner_dcache_client_out_a_valid",
    "dcache_c_valid": "auto_inner_dcache_client_out_c_valid",
    "dcache_e_valid": "auto_inner_dcache_client_out_e_valid",
    "scalar_load_writeback_0_valid": "io_mem_to_ooo_writebackLda_0_valid",
    "scalar_load_writeback_1_valid": "io_mem_to_ooo_writebackLda_1_valid",
    "scalar_load_writeback_2_valid": "io_mem_to_ooo_writebackLda_2_valid",
    "scalar_store_address_0_valid": "io_mem_to_ooo_writebackSta_0_valid",
    "scalar_store_address_1_valid": "io_mem_to_ooo_writebackSta_1_valid",
    "scalar_store_data_0_valid": "io_mem_to_ooo_writebackStd_0_valid",
    "scalar_store_data_1_valid": "io_mem_to_ooo_writebackStd_1_valid",
    "vector_writeback_0_valid": "io_mem_to_ooo_writebackVldu_0_valid",
    "vector_writeback_1_valid": "io_mem_to_ooo_writebackVldu_1_valid",
}


def default_manifest_path() -> Path:
    configured = os.environ.get("MEMBLOCK_TOFFEE_PORT_MANIFEST")
    if configured:
        return Path(configured)
    for parent in Path(__file__).resolve().parents:
        candidate = parent / "config" / "expected_ports.json"
        if candidate.is_file():
            return candidate
    return Path(__file__).resolve().parents[4] / "config" / "expected_ports.json"


def load_port_manifest(path: Path | None = None) -> dict[str, PortSpec]:
    manifest_path = default_manifest_path() if path is None else path
    document = json.loads(manifest_path.read_text(encoding="utf-8"))
    ports: dict[str, PortSpec] = {}
    for record in document["ports"]:
        port = PortSpec(
            name=record["name"],
            direction=record["direction"],
            width=int(record["width"]),
            input_policy=record.get("input_policy"),
            protocol=record["protocol"],
            role=record["role"],
            group=record["group"],
        )
        ports[port.name] = port
    return ports


def load_dut_class(package_root: Path) -> type[Any]:
    if not package_root.is_dir():
        raise RuntimeError(f"generated Picker package root is missing: {package_root}")
    package_name = os.environ.get("MEMBLOCK_TOFFEE_DUT_PACKAGE", DEFAULT_DUT_PACKAGE)
    class_name = os.environ.get("MEMBLOCK_TOFFEE_DUT_CLASS", DEFAULT_DUT_CLASS)
    root_text = str(package_root.resolve())
    if root_text not in sys.path:
        sys.path.insert(0, root_text)
    importlib.invalidate_caches()
    module = importlib.import_module(package_name)
    try:
        return getattr(module, class_name)
    except AttributeError as error:
        raise RuntimeError(
            f"generated Picker package has no DUT class {class_name}"
        ) from error


class MemBlockDutAdapter(PickerDutAdapter):
    """The only MemBlock component allowed to know generated signal names."""

    def __init__(self, raw: Any, manifest_path: Path | None = None) -> None:
        self.ports = load_port_manifest(manifest_path)
        super().__init__(raw, LOGICAL_PINS, clock_pin="clock")
        self._initialize_inputs()

    def _initialize_inputs(self) -> None:
        for port in self.ports.values():
            if port.direction != "input" or port.protocol == "clock":
                continue
            value = 1 if port.input_policy == "one" else 0
            wrapper = ToffeePin(self.raw_signal(port.name), self.event)
            wrapper.SetWriteMode(wrapper.xdata.Imme)
            wrapper.value = value

    def drive(self, logical_name: str, value: int) -> None:
        if logical_name not in LOGICAL_PINS:
            raise KeyError(f"no write adapter for logical pin {logical_name!r}")
        getattr(self, logical_name).value = value

    def sample(self, logical_name: str) -> int:
        if logical_name not in LOGICAL_PINS:
            raise KeyError(f"no read adapter for logical pin {logical_name!r}")
        return int(getattr(self, logical_name).value)

    @property
    def port_count(self) -> int:
        return len(self.ports)
