import env.frontend_env as frontend_env_module


class _Pin:
    def __init__(self, value=0):
        self.value = int(value)


class _DummyDut:
    def __init__(self):
        self.reset = _Pin(0)
        self.clock = _Pin(0)
        self.io_reset_vector_addr = _Pin(0)
        self.io_tlbCsr_satp_mode = _Pin(-1)
        self.io_tlbCsr_vsatp_mode = _Pin(-1)
        self.io_tlbCsr_hgatp_mode = _Pin(-1)
        self.step_calls = []
        self.step_ris_callbacks = []

    def StepRis(self, callback):
        self.step_ris_callbacks.append(callback)

    def Step(self, cycles):
        self.step_calls.append(int(cycles))
        return sum(self.step_calls)


class _FakePageTable:
    def __init__(self, mode="sv39"):
        self.mode = mode
        self.mode_calls = []

    def set_mode(self, mode):
        self.mode_calls.append(str(mode))
        self.mode = str(mode)


class _FakeBranchChecker:
    def __init__(self):
        self.attached = []

    def get_stats(self):
        return {"branch": 0}


class _FakeAgent:
    def __init__(self, name, dependency, call_log=None):
        self.name = name
        self.dependency = dependency
        self.call_log = call_log if call_log is not None else []
        self.bind_calls = []
        self.event_sinks = []
        self.configure_calls = []

    def configure(self, **kwargs):
        self.configure_calls.append(dict(kwargs))

    def bind(self, dut):
        self.bind_calls.append(dut)

    def set_event_sink(self, sink):
        self.event_sinks.append(sink)

    def on_clock_edge(self, cycle):
        self.call_log.append((self.name, int(cycle)))

    def get_stats(self):
        return {"name": self.name}


class _FakeMonitor(_FakeAgent):
    def __init__(self, memory, branch_checker, call_log=None):
        super().__init__("monitor", memory, call_log=call_log)
        self.memory = memory
        self.branch_checker = branch_checker
        self.backend_models = []
        self.expected_pcs = []

    def attach_backend_model(self, backend_model):
        self.backend_models.append(backend_model)

    def set_expected_pc(self, pc):
        self.expected_pcs.append(int(pc))

    def get_errors(self):
        return []


class _FakeBackend(_FakeAgent):
    def __init__(self, call_log=None, **kwargs):
        super().__init__("backend", kwargs, call_log=call_log)
        self.config = dict(kwargs)
        self.envs = []
        self.monitors = []
        self.branch_checkers = []

    def attach_env(self, env):
        self.envs.append(env)

    def attach_monitor(self, monitor):
        self.monitors.append(monitor)

    def attach_branch_checker(self, branch_checker):
        self.branch_checkers.append(branch_checker)


class _InjectedFrontendEnv(frontend_env_module.FrontendEnv):
    def __init__(self, *args, injected_collaborators=None, **kwargs):
        self._injected_collaborators = injected_collaborators or {}
        super().__init__(*args, **kwargs)

    def _create_collaborators(self):
        return dict(self._injected_collaborators)


def _make_fake_collaborators(memory, page_table, call_log=None):
    collaborators = {
        "branch_checker": _FakeBranchChecker(),
        "icache_agent": _FakeAgent("icache", memory, call_log=call_log),
        "uncache_agent": _FakeAgent("uncache", memory, call_log=call_log),
        "ptw_agent": _FakeAgent("ptw", page_table, call_log=call_log),
        "backend_model": _FakeBackend(call_log=call_log),
    }
    collaborators["monitor"] = _FakeMonitor(memory, collaborators["branch_checker"], call_log=call_log)
    return collaborators


def test_frontend_env_constructs_split_module_collaborators():
    assert frontend_env_module.FrontendMonitor.__module__ == "env.monitors.frontend_monitor"
    assert frontend_env_module.BranchChecker.__module__ == "env.model.branch_checker"
    assert frontend_env_module.ICacheAgent.__module__ == "env.agents.icache_agent"
    assert frontend_env_module.UncacheAgent.__module__ == "env.agents.uncache_agent"
    assert frontend_env_module.PTWAgent.__module__ == "env.agents.ptw_agent"

    dut = _DummyDut()
    env = frontend_env_module.FrontendEnv(dut, register_callbacks=False)

    assert type(env.monitor) is frontend_env_module.FrontendMonitor
    assert type(env.branch_checker) is frontend_env_module.BranchChecker
    assert type(env.icache_agent) is frontend_env_module.ICacheAgent
    assert type(env.uncache_agent) is frontend_env_module.UncacheAgent
    assert type(env.ptw_agent) is frontend_env_module.PTWAgent


def test_frontend_env_allows_injected_collaborators_for_clock_edge_order():
    dut = _DummyDut()
    page_table = _FakePageTable()
    call_log = []
    collaborators = _make_fake_collaborators(memory=object(), page_table=page_table, call_log=call_log)

    env = _InjectedFrontendEnv(
        dut,
        memory_model=object(),
        page_table_model=page_table,
        register_callbacks=False,
        injected_collaborators=collaborators,
    )

    assert env.monitor is collaborators["monitor"]
    assert env.branch_checker is collaborators["branch_checker"]
    assert env.icache_agent is collaborators["icache_agent"]
    assert env.uncache_agent is collaborators["uncache_agent"]
    assert env.ptw_agent is collaborators["ptw_agent"]

    env._on_clock_edge(7)

    assert call_log == [
        ("icache", 7),
        ("uncache", 7),
        ("ptw", 7),
        ("backend", 7),
        ("monitor", 7),
    ]


def test_initialize_keeps_reset_defaults_and_top_level_helpers():
    dut = _DummyDut()
    page_table = _FakePageTable(mode="sv39")
    collaborators = _make_fake_collaborators(memory=object(), page_table=page_table)
    env = _InjectedFrontendEnv(
        dut,
        memory_model=object(),
        page_table_model=page_table,
        register_callbacks=False,
        injected_collaborators=collaborators,
    )
    reset_calls = []
    env.reset = lambda cycles=20: reset_calls.append(int(cycles))

    env.initialize()

    assert int(dut.io_reset_vector_addr.value) == 0x80000000 >> 1
    assert int(dut.io_tlbCsr_satp_mode.value) == 0
    assert int(dut.io_tlbCsr_vsatp_mode.value) == 0
    assert int(dut.io_tlbCsr_hgatp_mode.value) == 0
    assert page_table.mode == "bare"
    assert page_table.mode_calls == ["bare"]
    assert reset_calls == [20]
    assert collaborators["monitor"].expected_pcs == [0x80000000]
