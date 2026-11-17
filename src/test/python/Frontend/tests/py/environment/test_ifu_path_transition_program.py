"""Independently execute the directed path program bytes; never run a DUT."""

from copy import deepcopy
from types import SimpleNamespace

import pytest

from env.model import MemoryModel, PageTableModel
from env.support.pmp_pma import encode_pmp_pma_addr, encode_pmp_pma_cfg
from tests.py.jiabowen import test_ifu_owner_path_transition_v3_dut as case


def _signed(value, width):
    return value - (1 << width) if value & (1 << (width - 1)) else value


def _execute_bytes(images, trace):
    """Subset interpreter derives targets from bytes/registers, never trace."""
    registers = [0] * 32
    pc = case._NC_VA
    visited = []
    for index, entry in enumerate(trace.entries):
        page = next(path for path, (va, _pa, _pbmt) in case._PATH_PAGES.items()
                    if va <= pc < va + case._PAGE_SIZE)
        va = case._PATH_PAGES[page][0]
        word = int.from_bytes(images[page][pc - va:pc - va + 4], "little")
        assert entry.index == index and entry.pc == pc and entry.instr == word
        assert entry.size == 4
        visited.append(pc)
        opcode, rd, rs1 = word & 127, (word >> 7) & 31, (word >> 15) & 31
        target, kind = None, "normal"
        successor = pc + 4
        if word == 0x13:
            pass
        elif opcode == 0x17:
            registers[rd] = (pc + _signed(word & 0xFFFFF000, 32)) & ((1 << 64) - 1)
        elif opcode == 0x67:
            assert (word >> 12) & 7 == 0 and rd == 0 and rs1 == 6
            successor = (registers[rs1] + _signed(word >> 20, 12)) & ~1
            target, kind = successor, "jump_indirect"
        elif opcode == 0x6F:
            immediate = (((word >> 31) & 1) << 20 | ((word >> 21) & 1023) << 1
                         | ((word >> 20) & 1) << 11 | ((word >> 12) & 255) << 12)
            successor = pc + _signed(immediate, 21)
            target, kind = successor, "jump"
        else:
            raise AssertionError(f"unsupported instruction: {word:#x}")
        assert entry.kind == kind and entry.taken is (target is not None)
        assert entry.target_pc == target
        registers[0] = 0
        pc = successor
    assert pc == trace.entries[-1].pc  # final executable JAL x0,0
    return visited


def test_path_program_executes_original_seven_visits_and_all_four_transitions():
    images, phases, trace = case._path_transition_program()
    visited = _execute_bytes(images, trace)
    assert [p["path"] for p in phases] == ["nc", "cacheable", "mmio", "nc", "cacheable", "nc", "mmio"]
    assert len({p["pc"] for p in phases}) == 7
    assert len(trace.entries) == 69
    indices = [visited.index(p["pc"]) for p in phases]
    assert indices == sorted(indices)
    transitions = {a["path"] + "_to_" + b["path"] for a, b in zip(phases, phases[1:])}
    assert {"cacheable_to_nc", "cacheable_to_mmio", "nc_to_cacheable", "nc_to_mmio"} <= transitions
    assert all(p["pc"] & 63 == 0 and p["pc"] & 4095 < 4096 - 40 for p in phases)


@pytest.mark.parametrize("change", ["claimed_target", "wrong_rs1", "wrong_auipc", "wrong_pc"])
def test_path_oracle_rejects_target_or_source_not_proved_by_instruction_bytes(change):
    images, _phases, trace = case._path_transition_program()
    trace = deepcopy(trace)
    index = next(i for i, entry in enumerate(trace.entries) if entry.kind == "jump_indirect")
    if change == "claimed_target":
        trace.entries[index].target_pc += 64
    elif change == "wrong_pc":
        trace.entries[index + 1].pc += 4
    else:
        victim = index if change == "wrong_rs1" else index - 1
        entry = trace.entries[victim]
        path = next(p for p, (va, _pa, _pbmt) in case._PATH_PAGES.items()
                    if va <= entry.pc < va + case._PAGE_SIZE)
        offset = entry.pc - case._PATH_PAGES[path][0]
        word = entry.instr ^ (1 << (15 if change == "wrong_rs1" else 12))
        image = bytearray(images[path])
        image[offset:offset + 4] = word.to_bytes(4, "little")
        images[path] = bytes(image)
        entry.instr = word  # byte match alone is insufficient; target must execute.
    with pytest.raises(AssertionError):
        _execute_bytes(images, trace)


class _Environment:
    def __init__(self):
        self.page_table = PageTableModel()
        self.memory = MemoryModel()
        self.programs, self.pmp, self.pma = {}, {}, {}
        self.ptw_agent = SimpleNamespace(configure=lambda **kw: setattr(self, "ptw", kw))
        self.backend_model = SimpleNamespace(set_golden_trace=lambda trace: setattr(self, "trace", trace))

    def load_program(self, payload, base_addr):
        self.programs[base_addr] = payload
        self.memory.load_bin(payload, base_addr)

    def write_pmp_entry(self, index, config, addr, *, size, settle_cycles):
        encode_pmp_pma_cfg(config)
        encode_pmp_pma_addr(addr, config, size=size)
        self.pmp[index] = (config, addr, size)

    def write_pma_entry(self, index, config, addr, *, size, settle_cycles):
        encode_pmp_pma_cfg(config)
        encode_pmp_pma_addr(addr, config, size=size)
        self.pma[index] = (config, addr, size)


def test_all_program_pages_have_executable_pte_pbmt_and_physical_permissions():
    env = _Environment()
    images, phases, trace = case._prepare_three_path_stream(env)
    case._configure_path_attributes(env, trace)
    env.page_table.set_mode("sv39")
    assert env.trace is trace
    assert env.ptw == {"mode": "sv39", "response_source": "model", "compare_drive_source": "model"}
    for path, (va, pa, pbmt) in case._PATH_PAGES.items():
        translated, accepted, metadata = env.page_table.translate(va)
        pte = env.page_table.pte_map[va >> 12]
        assert accepted and translated == pa and pte.pbmt == pbmt
        assert env.programs[pa] == images[path]
        assert pte.x == pte.r == pte.v == 1
        index = int(path == "mmio")
        pmp, pmp_addr, pmp_size = env.pmp[index]
        pma, pma_addr, pma_size = env.pma[index]
        assert pmp.execute and pmp.read and pmp.write
        assert pma.execute and pma.read and pma.write
        assert pma.cacheable is (path != "mmio")
        assert pmp_addr <= pa < pmp_addr + pmp_size
        assert pma_addr <= pa < pma_addr + pma_size
    assert all(p["pa"] in range(case._PATH_PAGES[p["path"]][1],
                                case._PATH_PAGES[p["path"]][1] + 4096) for p in phases)


@pytest.mark.parametrize("pc,target", [(0x1000, 0x1001), (0, 1 << 31), (1 << 32, 0)])
def test_jump_encoder_rejects_unrepresentable_or_unaligned_target(pc, target):
    with pytest.raises(ValueError):
        case._jump_words(pc, target)


@pytest.mark.parametrize("first,backend,ibuffer,allowed", [
    (0, 0, 1, False), (0, 1, 0, False), (0, 1, 1, True), (1, 0, 1, True),
])
def test_wait_last_commit_observer_requires_actual_prior_release_gate(first, backend, ibuffer, allowed):
    callbacks = []
    gate_callbacks = []
    values = {"uncacheUnit.uncacheState": 1, "isFirstInstr": first,
              "uncacheUnit.io_emptyAfter": backend & ibuffer, "io_ibufferEmpty": ibuffer,
              "io_toIBuffer_valid": 0}
    prefix = "Frontend_top.Frontend.inner_ifu."
    def lookup(_dut, path):
        return values.get(path[len(prefix):]) if path.startswith(prefix) else None
    def lookup_signal(dut, path):
        value = lookup(dut, path)
        return None if value is None else SimpleNamespace(value=value)
    env = SimpleNamespace(
        dut=SimpleNamespace(reset=SimpleNamespace(value=0), StepFal=gate_callbacks.append,
                            io_backend_backendEmpty=SimpleNamespace(value=backend)),
        functional_coverage=SimpleNamespace(_try_read_dut_signal=lookup, _lookup_dut_signal=lookup_signal),
        register_cycle_observer=callbacks.append,
    )
    _deliveries, gates, observe_gate = case._capture_path_evidence(env, [])
    gate_callbacks.append(observe_gate)
    gate_callbacks[0](1)
    values["uncacheUnit.uncacheState"] = 2
    if allowed:
        gate_callbacks[0](2)
        assert gates[-1]["wait_last_commit_released"] is True
    else:
        with pytest.raises(AssertionError):
            gate_callbacks[0](2)


def test_path_program_never_manually_clears_backend_or_drives_empty():
    # Check control APIs as a constraint on this stimulus, independently of
    # success assertions: execution must remain the natural trace policy.
    import ast
    import inspect
    tree = ast.parse(inspect.getsource(case))
    forbidden = {"inject_redirect", "inject_redirect_from_cfvec", "_force_redirect_to",
                 "_clear_cfvec_queue_state", "_initialize_epoch_state",
                 "set_explicit_injection_enabled"}
    assert not {n.func.attr for n in ast.walk(tree)
                if isinstance(n, ast.Call) and isinstance(n.func, ast.Attribute)} & forbidden
    assert not any(isinstance(n, ast.Attribute) and isinstance(n.ctx, ast.Store)
                   and n.attr in {"backend_empty", "backendEmpty", "io_backend_backendEmpty"}
                   for n in ast.walk(tree))


class _StartupEnvironment(_Environment):
    def __init__(self, fault=None):
        super().__init__()
        self.fault = fault
        self.observers, self.native, self.events = [], {}, []
        self.current_cycle = 0
        self.req_count = self.resp_count = 0
        self.dut = SimpleNamespace(reset=SimpleNamespace(value=1))
        self.ready_policy = "always"
        self.ptw_agent = SimpleNamespace(configure=self.configure_ptw, get_stats=lambda: {
            "req_count": self.req_count, "resp_count": self.resp_count,
        })
        self.functional_coverage = SimpleNamespace(_try_read_dut_signal=self.read_native)

    def configure_ptw(self, **settings):
        self.events.append(("ptw", settings))
        self.ready_policy = settings["req_ready_strategy"]
        if self.ready_policy == "periodic":
            assert settings["req_ready_high_cycles"] == 0
            assert settings["req_ready_low_cycles"] > 0

    def register_cycle_observer(self, callback):
        self.observers.append(callback)

    def initialize(self, *, reset_vector, bare_mode, reset_cycles):
        assert reset_vector == case._NC_VA and bare_mode is False
        assert reset_cycles == 20
        self.events.append(("initialize", None))
        self.dut.reset.value = int(self.fault == "reset_stuck")
        self.step(1)

    def step(self, cycles):
        for _ in range(cycles):
            self.current_cycle += 1
            if self.fault == "early_ptw":
                self.req_count += 1
            for callback in self.observers:
                callback(self.current_cycle, self)

    def read_native(self, _dut, name):
        prefix = "Frontend_top.Frontend.inner_pmp."
        if not name.startswith(prefix):
            return None
        stem = name[len(prefix):]
        if self.fault == "missing_native" and stem == "pmpMapping_cfgMerged_0":
            return None
        value = self.native.get(stem, 0)
        if self.fault == "dropped_csr" and stem == "pmpMapping_cfgMerged_0":
            value &= ~4
        return value

    def _write_entry(self, kind, index, config, addr, size):
        assert self.dut.reset.value == 0
        assert self.ready_policy == "periodic"
        self.events.append((kind, index))
        stem = f"{kind}Mapping_cfgMerged_0"
        word = self.native.get(stem, 0)
        byte_mask = 0xFF << (index * 8)
        self.native[stem] = (word & ~byte_mask) | (encode_pmp_pma_cfg(config) << (index * 8))
        self.native[f"{kind}Mapping_addr_{index}"] = encode_pmp_pma_addr(addr, config, size=size)
        self.step(5)

    def write_pmp_entry(self, index, config, addr, *, size, settle_cycles):
        self._write_entry("pmp", index, config, addr, size)
        super().write_pmp_entry(index, config, addr, size=size, settle_cycles=settle_cycles)

    def write_pma_entry(self, index, config, addr, *, size, settle_cycles):
        self._write_entry("pma", index, config, addr, size)
        super().write_pma_entry(index, config, addr, size=size, settle_cycles=settle_cycles)

    def _emit_event(self, kind, payload):
        self.events.append((kind, payload))


def test_startup_programs_native_permissions_after_reset_before_ptw_handshake():
    env = _StartupEnvironment()
    _images, _phases, trace = case._path_transition_program()
    case._initialize_path_stream(env, trace)
    assert env.trace is trace and env.ready_policy == "always"
    assert [e[0] for e in env.events] == [
        "ptw", "initialize", "pmp", "pma", "pmp", "pma",
        "ifu.path_transition_post_reset_permissions", "ptw",
    ]
    assert env.req_count == env.resp_count == 0


@pytest.mark.parametrize("fault", ["early_ptw", "dropped_csr", "reset_stuck", "missing_native"])
def test_startup_rejects_unqualified_native_permissions_or_early_translation(fault):
    env = _StartupEnvironment(fault)
    _images, _phases, trace = case._path_transition_program()
    with pytest.raises(AssertionError):
        case._initialize_path_stream(env, trace)
    assert env.ready_policy == "periodic"
