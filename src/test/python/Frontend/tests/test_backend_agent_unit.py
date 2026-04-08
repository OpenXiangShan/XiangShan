from env.agents.backend_agent import BackendAgent
from env.bundles import BackendCtrlBundle, bind_bundle_optional
from env.model.backend_state import FtqEntry, ResolveEntry


class _Pin:
    def __init__(self, name: str, log: list[tuple[str, int]]) -> None:
        self.name = str(name)
        self.log = log
        self._value = 0

    @property
    def value(self) -> int:
        return self._value

    @value.setter
    def value(self, value: int) -> None:
        self._value = int(value)
        self.log.append((self.name, self._value))


class _DummyDut:
    def __init__(self) -> None:
        self.log: list[tuple[str, int]] = []
        self._pins: dict[str, _Pin] = {}

    def __getattr__(self, name: str) -> _Pin:
        if name.startswith("_"):
            raise AttributeError(name)
        pin = self._pins.get(name)
        if pin is None:
            pin = _Pin(name, self.log)
            self._pins[name] = pin
        return pin

    def clear_log(self) -> None:
        self.log.clear()


def _make_resolve(flag: int, value: int, offset: int, pc: int, target: int) -> ResolveEntry:
    return ResolveEntry(
        ready_cycle=3,
        inst_pc=pc,
        pc=pc,
        target=target,
        taken=True,
        mispredict=False,
        ftq_flag=flag,
        ftq_value=value,
        ftq_offset=offset,
        branch_type=1,
        ras_action=0,
    )


def test_start_cycle_drives_can_accept_before_clearing_one_shot_valids():
    dut = _DummyDut()
    dut.io_backend_toFtq_redirect_valid.value = 1
    dut.io_backend_toFtq_resolve_0_valid.value = 1
    dut.io_backend_toFtq_resolve_1_valid.value = 1
    dut.io_backend_toFtq_resolve_2_valid.value = 1
    dut.clear_log()

    agent = BackendAgent()
    agent.bind(dut)

    agent.start_cycle(can_accept=0)

    assert int(dut.io_backend_canAccept.value) == 0
    assert int(dut.io_backend_toFtq_redirect_valid.value) == 0
    assert int(dut.io_backend_toFtq_resolve_0_valid.value) == 0
    assert int(dut.io_backend_toFtq_resolve_1_valid.value) == 0
    assert int(dut.io_backend_toFtq_resolve_2_valid.value) == 0
    assert dut.log[:5] == [
        ("io_backend_canAccept", 0),
        ("io_backend_toFtq_redirect_valid", 0),
        ("io_backend_toFtq_resolve_0_valid", 0),
        ("io_backend_toFtq_resolve_1_valid", 0),
        ("io_backend_toFtq_resolve_2_valid", 0),
    ]


def test_drive_commit_clears_valid_when_idle_then_asserts_payload_for_entry():
    dut = _DummyDut()
    agent = BackendAgent()
    agent.bind(dut)

    agent.drive_commit(None)
    assert int(dut.io_backend_toFtq_commit_valid.value) == 0

    dut.clear_log()
    entry = FtqEntry(ftq_flag=1, ftq_value=5)
    agent.drive_commit(entry)

    assert int(dut.io_backend_toFtq_commit_valid.value) == 1
    assert int(dut.io_backend_toFtq_commit_bits_flag.value) == 1
    assert int(dut.io_backend_toFtq_commit_bits_value.value) == 5
    assert dut.log[0] == ("io_backend_toFtq_commit_valid", 0)
    assert dut.log[-1] == ("io_backend_toFtq_commit_valid", 1)


def test_drive_resolves_populates_requested_channels_only():
    dut = _DummyDut()
    agent = BackendAgent()
    agent.bind(dut)
    agent.start_cycle(can_accept=1)
    dut.clear_log()

    entries = [
        _make_resolve(0, 1, 2, 0x80000020, 0x80000040),
        _make_resolve(1, 3, 0, 0x80000080, 0x800000A0),
    ]
    agent.drive_resolves(entries)

    assert int(dut.io_backend_toFtq_resolve_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_resolve_1_valid.value) == 1
    assert int(dut.io_backend_toFtq_resolve_2_valid.value) == 0
    assert int(dut.io_backend_toFtq_resolve_0_bits_pc_addr.value) == (0x80000020 >> 1)
    assert int(dut.io_backend_toFtq_resolve_1_bits_target_addr.value) == (0x800000A0 >> 1)


def test_drive_redirect_asserts_payload_and_next_cycle_clears_valid():
    dut = _DummyDut()
    agent = BackendAgent()
    agent.bind(dut)

    agent.drive_redirect(
        {
            "pc": 0x80000010,
            "target_pc": 0x80000040,
            "taken": 1,
            "ftq_flag": 1,
            "ftq_value": 2,
            "ftq_offset": 3,
            "is_rvc": 0,
            "branch_type": 2,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert int(dut.io_backend_toFtq_redirect_valid.value) == 1
    assert int(dut.io_backend_toFtq_redirect_bits_pc.value) == 0x80000010
    assert int(dut.io_backend_toFtq_redirect_bits_target.value) == 0x80000040
    assert int(dut.io_backend_toFtq_redirect_bits_ftqIdx_flag.value) == 1
    assert int(dut.io_backend_toFtq_redirect_bits_ftqIdx_value.value) == 2

    agent.start_cycle(can_accept=1)

    assert int(dut.io_backend_toFtq_redirect_valid.value) == 0


def test_backend_agent_accepts_explicit_backend_ctrl_bundle_binding():
    dut = _DummyDut()
    interface = bind_bundle_optional(BackendCtrlBundle, dut)
    agent = BackendAgent()

    agent.bind(interface)
    agent.start_cycle(can_accept=1)

    assert agent.interface is interface
    assert int(dut.io_backend_canAccept.value) == 1
