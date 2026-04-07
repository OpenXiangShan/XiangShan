from env.env_config import DEFAULT_ENV_CONFIG
from env.frontend_env import FrontendEnv
from env.memory_model import PageTableModel
from env.transactions import RedirectTxn, CommitTarget


def test_default_env_config_has_expected_frontend_defaults():
    assert DEFAULT_ENV_CONFIG.icache.hit_latency == 1
    assert DEFAULT_ENV_CONFIG.uncache.latency >= 0
    assert DEFAULT_ENV_CONFIG.ptw.latency >= 0
    assert DEFAULT_ENV_CONFIG.backend.resolve_min_delay >= 0


def test_transactions_are_plain_value_objects():
    redirect = RedirectTxn(target_pc=0x80000000, reason="unit")
    commit = CommitTarget(target_count=8, max_cycles=100)
    assert redirect.target_pc == 0x80000000
    assert commit.target_count == 8


class _DummyDut:
    def StepRis(self, callback):
        self.callback = callback


def test_frontend_env_preserves_explicit_page_table_mode():
    page_table = PageTableModel(mode="sv39")

    env = FrontendEnv(
        _DummyDut(),
        page_table_model=page_table,
        config=DEFAULT_ENV_CONFIG,
        register_callbacks=False,
    )

    assert env.page_table.mode == "sv39"
