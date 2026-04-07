from env import FrontendEnv
from env import ICacheAgent, PTWAgent, UncacheAgent
from env.model import MemoryModel, PageTableModel, GoldenTrace, BranchChecker


def test_import_compat_symbols_exist():
    assert FrontendEnv is not None
    assert ICacheAgent is not None
    assert PTWAgent is not None
    assert UncacheAgent is not None
    assert MemoryModel is not None
    assert PageTableModel is not None
    assert GoldenTrace is not None
    assert BranchChecker is not None
