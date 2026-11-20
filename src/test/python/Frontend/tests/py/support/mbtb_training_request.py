"""Native MainBtb request and its exact one-edge write-buffer launch binding."""
from dataclasses import dataclass

from env.support.bpu_v3_contract import BpuV3SignalUnavailable


TRAINING_REQUEST_FIELDS = {
    "valid": "io_write_req_valid",
    "need_write": "io_write_req_bits_needWrite",
    "mispredict_valid": "io_write_req_bits_mispredictInfo_valid",
    "start_pc": "io_write_req_bits_startPc_addr",
    "position": "io_write_req_bits_mispredictInfo_bits_cfiPosition",
    "target": "io_write_req_bits_mispredictInfo_bits_target_addr",
    "branch_type": "io_write_req_bits_mispredictInfo_bits_attribute_branchType",
    "ras_action": "io_write_req_bits_mispredictInfo_bits_attribute_rasAction",
}


def decoded_source_registration(model, entry, program):
    """Check the native six-bit fromFtq index without inventing its wrap bit.

    fromFtq.ftqIdx is UInt(6.W); cfVec carries flag and value separately.
    The cache is populated by native fromFtq writes and therefore has index
    keys only. This checks source geometry, not an exact FTQ occurrence.
    """
    value, flag = int(entry.ftq_value), int(entry.ftq_flag)
    assert 0 <= value < 64 and flag in (0, 1), vars(entry)
    start_pc = model._ftq_start_pc_cache.get(value)
    assert start_pc == program["source_pc"], {
        "reason": "decoded source lacks the native fromFtq start PC",
        "entry": vars(entry), "native_ftq_index": value, "start_pc": start_pc,
    }
    assert int(start_pc) + 2 * int(entry.ftq_offset) - 2 == int(entry.pc), {
        "reason": "decoded RVI source does not match native fetch geometry",
        "entry": vars(entry), "start_pc": start_pc,
    }
    return int(start_pc)


@dataclass(frozen=True)
class MbtbTrainingRequest:
    valid: int
    need_write: int
    mispredict_valid: int
    start_pc: int
    position: int
    target: int
    branch_type: int
    ras_action: int
    signal_paths: dict[str, str]
    source_values: dict[str, int]

    def require_source(self, program, incoming):
        assert self.valid == self.need_write == self.mispredict_valid == 1, self
        assert self.start_pc == program["source_pc"] and self.position == program["position"], self
        assert self.branch_type == incoming.branch_type == 3 and self.ras_action == incoming.ras_action == 0, self
        assert self.target in (program["initial_target"], program["changed_target"]), self
        assert (self.target >> 1) & ((1 << 20) - 1) == incoming.target_lower, self
        assert incoming.target_carry == program["target_carry"], self
        assert incoming.identity == (program["set_idx"], program["tag"], program["position"]), self


@dataclass(frozen=True)
class MbtbWriteLaunch:
    """One native bank input sample before its registered WriteBuffer input.

    MainBtbInternalBank.scala registers valid and payload exactly once. This
    object is never an occurrence-level FTQ identity: that ID is absent from
    this native request interface, even though startPC/position are available.
    """
    cycle: int
    align_bank: int
    internal_bank: int
    request: MbtbTrainingRequest
    bank_write_valid: int
    port_write_valid: tuple[int, ...]
    port_valid: tuple[int, ...]
    signal_paths: dict[str, str]
    source_values: dict[str, int]

    def bind_event(self, event, program, *, event_cycle):
        assert int(event_cycle) == self.cycle + 1, {
            "reason": "MBTB event must match exactly the previous native launch edge",
            "launch_cycle": self.cycle, "event_cycle": event_cycle,
        }
        assert (self.align_bank, self.internal_bank) == (event.align_bank, event.internal_bank), self
        assert (self.align_bank, self.internal_bank) == (program["align_bank"], program["internal_bank"]), self
        assert self.bank_write_valid == 1, self
        assert 0 <= event.port < 4, event
        # valid can also come from a flush. Require the genuine entry-write
        # branch, which wins the payload mux, and the exact selected port.
        assert self.port_write_valid[event.port] == self.port_valid[event.port] == 1, self
        self.request.require_source(program, event.incoming)
        return self.request


def sample_mbtb_training_request(reader, align_bank):
    # This program places its source in alignBank0. The frozen inventory has
    # all exact request probes there; bank1 lacks several native request aliases.
    # Refuse to pretend this fixture provides a generic two-bank contract.
    assert align_bank == 0, "far-JALR fixture requires native alignBank0 request probes"
    base = f"Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_{align_bank}."
    values, paths, sources = {}, {}, {}
    for key, suffix in TRAINING_REQUEST_FIELDS.items():
        path = base + suffix
        value = reader(path)
        if value is None:
            raise BpuV3SignalUnavailable("mbtb_training_" + key, (path,))
        sources[path] = int(value)
        values[key] = int(value) << 1 if key in ("start_pc", "target") else int(value)
        paths[key] = path
    return MbtbTrainingRequest(**values, signal_paths=paths, source_values=sources)


def sample_mbtb_write_launch(reader, *, cycle, align_bank, internal_bank):
    request = sample_mbtb_training_request(reader, align_bank)
    assert internal_bank in range(4)
    root = f"Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_{align_bank}.internalBanks_{internal_bank}."
    paths, sources = dict(request.signal_paths), dict(request.source_values)

    def read(key, suffix):
        path = root + suffix
        value = reader(path)
        if value is None:
            raise BpuV3SignalUnavailable("mbtb_launch_" + key, (path,))
        value = int(value)
        assert value in (0, 1), {"path": path, "value": value}
        paths[key], sources[path] = path, value
        return value

    bank_valid = read("bank_write_valid", "io_writeEntry_req_valid")
    write_valid, valid = [], []
    for port in range(4):
        suffix = "" if port == 0 else f"_{port}"
        write_valid.append(read(f"port{port}_write_valid", "writeValid" + suffix))
        valid.append(read(f"port{port}_valid", "valid" + suffix))
    return MbtbWriteLaunch(int(cycle), int(align_bank), int(internal_bank), request,
                          bank_valid, tuple(write_valid), tuple(valid), paths, sources)
