from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Dict, Optional

from ..bundles import ICacheControlBundle


_PUT_FULL_DATA = 0
_PUT_PARTIAL_DATA = 1
_GET = 4
_ACCESS_ACK = 0
_ACCESS_ACK_DATA = 1
_CONTROL_BASE = 0x38022080
_CONTROL_END = 0x380220FF
_BEAT_SIZE = 3
_FULL_MASK = 0xFF


@dataclass
class _ControlRequest:
    opcode: int
    address: int
    size: int
    source: int
    mask: int
    data: int
    driven: bool = False
    accepted: bool = False
    accepted_cycle: Optional[int] = None


class ICacheControlAgent:
    def __init__(self) -> None:
        self.interface = None
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.outstanding: Optional[_ControlRequest] = None
        self.accepted_request_count = 0
        self.response_count = 0
        self.requests: list[dict] = []
        self.responses: list[dict] = []

    @staticmethod
    def _read(signal, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return default if value is None else int(value)
        except Exception:
            return default

    @staticmethod
    def _write(signal, value: int) -> None:
        signal.value = int(value)

    def bind(self, target) -> None:
        if not isinstance(target, ICacheControlBundle):
            raise TypeError(
                "ICacheControlAgent.bind requires an ICache control interface, "
                f"got {type(target).__name__}"
            )
        self.interface = target

    def set_event_sink(self, sink: Optional[Callable[[Dict], None]]) -> None:
        self.event_sink = sink

    def _emit(self, cycle: int, event_type: str, payload: Dict) -> None:
        if self.event_sink is None:
            return
        self.event_sink(
            {
                "type": event_type,
                "source": "icache_control_agent",
                "cycle": int(cycle),
                "level": "DEBUG",
                "payload": dict(payload),
            }
        )

    @staticmethod
    def _validate_request(*, opcode: int, address: int, size: int, source: int, mask: int) -> None:
        if opcode not in {_PUT_FULL_DATA, _PUT_PARTIAL_DATA, _GET}:
            raise ValueError(f"unsupported ICache control opcode: {opcode}")
        if size != _BEAT_SIZE:
            raise ValueError(f"ICache control size must be {_BEAT_SIZE}, got {size}")
        if address & 0x7:
            raise ValueError(f"ICache control address must be 8-byte aligned: 0x{address:x}")
        if not _CONTROL_BASE <= address <= _CONTROL_END:
            raise ValueError(f"ICache control address is outside 0x{_CONTROL_BASE:x}-0x{_CONTROL_END:x}: 0x{address:x}")
        if not 0 <= source < 32:
            raise ValueError(f"ICache control source must fit 5 bits: {source}")
        if opcode in {_GET, _PUT_FULL_DATA} and mask != _FULL_MASK:
            raise ValueError(f"ICache control opcode {opcode} requires mask 0xff, got 0x{mask:x}")
        if opcode == _PUT_PARTIAL_DATA and not 0 < mask <= _FULL_MASK:
            raise ValueError(f"ICache control partial write mask must be nonzero and fit 8 bits: 0x{mask:x}")

    def request(
        self,
        *,
        opcode: int,
        address: int,
        size: int = _BEAT_SIZE,
        source: int = 0,
        mask: int = _FULL_MASK,
        data: int = 0,
    ) -> None:
        if self.outstanding is not None:
            raise RuntimeError("ICache control permits only one outstanding request")
        values = {
            "opcode": int(opcode),
            "address": int(address),
            "size": int(size),
            "source": int(source),
            "mask": int(mask),
            "data": int(data),
        }
        self._validate_request(**{key: values[key] for key in ("opcode", "address", "size", "source", "mask")})
        self.outstanding = _ControlRequest(**values)

    def reset(self) -> None:
        self.outstanding = None
        if self.interface is not None:
            self.interface.drive_idle()

    def _drive_request(self, cycle: int) -> None:
        request = self.outstanding
        if (
            request is not None
            and request.driven
            and not request.accepted
            and self._read(self.interface.a_ready) == 1
        ):
            request.accepted = True
            request.accepted_cycle = int(cycle)
            self.accepted_request_count += 1
            record = {
                "cycle": int(cycle),
                "opcode": request.opcode,
                "size": request.size,
                "source": request.source,
                "address": request.address,
                "mask": request.mask,
                "data": request.data,
            }
            self.requests.append(record)
            self._emit(cycle, "handshake.icache_control_a", record)

        self._write(self.interface.a_valid, int(request is not None and not request.accepted))
        if request is None or request.accepted:
            return
        self._write(self.interface.a_bits_opcode, request.opcode)
        self._write(self.interface.a_bits_size, request.size)
        self._write(self.interface.a_bits_source, request.source)
        self._write(self.interface.a_bits_address, request.address)
        self._write(self.interface.a_bits_mask, request.mask)
        self._write(self.interface.a_bits_data, request.data)
        request.driven = True

    def _check_response(self, cycle: int) -> None:
        request = self.outstanding
        ready = int(
            request is not None
            and request.accepted
            and request.accepted_cycle is not None
            and int(cycle) > request.accepted_cycle
        )
        valid = self._read(self.interface.d_valid)
        if valid != 1:
            self._write(self.interface.d_ready, ready)
            return
        if request is None or not request.accepted:
            raise AssertionError("ICache control response arrived without an outstanding request")
        if ready != 1:
            self._write(self.interface.d_ready, 0)
            return
        actual = {
            "opcode": self._read(self.interface.d_bits_opcode),
            "size": self._read(self.interface.d_bits_size),
            "source": self._read(self.interface.d_bits_source),
        }
        expected = {
            "opcode": _ACCESS_ACK_DATA if request.opcode == _GET else _ACCESS_ACK,
            "size": request.size,
            "source": request.source,
        }
        for field, expected_value in expected.items():
            if actual[field] != expected_value:
                raise AssertionError(
                    f"ICache control response {field} mismatch: expected {expected_value}, got {actual[field]}"
                )
        record = {
            "cycle": int(cycle),
            **actual,
            "data": self._read(self.interface.d_bits_data),
        }
        self.responses.append(record)
        self.response_count += 1
        self.outstanding = None
        self._write(self.interface.d_ready, 1)
        self._emit(cycle, "handshake.icache_control_d", record)

    def on_clock_edge(self, cycle: int) -> None:
        if self.interface is None:
            return
        self._drive_request(cycle)
        self._check_response(cycle)

    def get_stats(self) -> dict:
        return {
            "accepted_request_count": self.accepted_request_count,
            "response_count": self.response_count,
            "outstanding": self.outstanding is not None,
            "requests": list(self.requests),
            "responses": list(self.responses),
        }


__all__ = ["ICacheControlAgent"]
