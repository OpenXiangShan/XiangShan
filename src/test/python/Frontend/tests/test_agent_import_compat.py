from env.agents.icache_agent import ICacheAgent
from env.agents.ptw_agent import PTWAgent
from env.agents.uncache_agent import UncacheAgent


def test_agent_modules_export_expected_classes():
    assert ICacheAgent is not None
    assert UncacheAgent is not None
    assert PTWAgent is not None
    assert ICacheAgent.__module__ == "env.agents.icache_agent"
    assert UncacheAgent.__module__ == "env.agents.uncache_agent"
    assert PTWAgent.__module__ == "env.agents.ptw_agent"


def test_agent_package_and_legacy_shim_exports_match_split_modules():
    from env.agents import ICacheAgent as PackageICacheAgent
    from env.agents import PTWAgent as PackagePTWAgent
    from env.agents import UncacheAgent as PackageUncacheAgent
    from env.agents_legacy import ICacheAgent as LegacyICacheAgent
    from env.agents_legacy import PTWAgent as LegacyPTWAgent
    from env.agents_legacy import UncacheAgent as LegacyUncacheAgent

    assert PackageICacheAgent is ICacheAgent
    assert PackageUncacheAgent is UncacheAgent
    assert PackagePTWAgent is PTWAgent
    assert LegacyICacheAgent is ICacheAgent
    assert LegacyUncacheAgent is UncacheAgent
    assert LegacyPTWAgent is PTWAgent
