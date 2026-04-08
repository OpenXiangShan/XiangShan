# coding=utf-8

import Frontend_api
import Frontend_env
from env import api as env_api
from env import fixtures
from env.frontend_env import FrontendEnv


def test_frontend_api_re_exports_env_api_and_fixtures():
    assert Frontend_api.create_dut is fixtures.create_dut
    assert Frontend_api.dut is fixtures.dut
    assert Frontend_api.api_Frontend_load_program is env_api.api_Frontend_load_program
    assert Frontend_api.api_Frontend_load_program_file is env_api.api_Frontend_load_program_file
    assert Frontend_api.api_Frontend_load_golden_trace is env_api.api_Frontend_load_golden_trace
    assert Frontend_api.api_Frontend_prepare_program_and_nemu_trace is env_api.api_Frontend_prepare_program_and_nemu_trace
    assert Frontend_api.api_Frontend_run_until_commit is env_api.api_Frontend_run_until_commit
    assert Frontend_api.api_Frontend_inject_redirect is env_api.api_Frontend_inject_redirect
    assert Frontend_api.api_Frontend_check_pc_sequence is env_api.api_Frontend_check_pc_sequence
    assert Frontend_api.api_Frontend_get_branch_stats is env_api.api_Frontend_get_branch_stats
    assert Frontend_api.api_Frontend_set_log_level is env_api.api_Frontend_set_log_level
    assert Frontend_api.api_Frontend_set_bp_ctrl_enable is env_api.api_Frontend_set_bp_ctrl_enable
    assert Frontend_api.api_Frontend_enable_fst_dump is env_api.api_Frontend_enable_fst_dump
    assert Frontend_api.api_Frontend_pause_fst_dump is env_api.api_Frontend_pause_fst_dump
    assert Frontend_api.api_Frontend_flush_fst_dump is env_api.api_Frontend_flush_fst_dump
    assert sorted(Frontend_api.__all__) == [
        "api_Frontend_check_pc_sequence",
        "api_Frontend_enable_fst_dump",
        "api_Frontend_flush_fst_dump",
        "api_Frontend_get_branch_stats",
        "api_Frontend_inject_redirect",
        "api_Frontend_load_golden_trace",
        "api_Frontend_load_program",
        "api_Frontend_load_program_file",
        "api_Frontend_pause_fst_dump",
        "api_Frontend_prepare_program_and_nemu_trace",
        "api_Frontend_run_until_commit",
        "api_Frontend_set_bp_ctrl_enable",
        "api_Frontend_set_log_level",
        "create_dut",
        "dut",
    ]


def test_frontend_env_re_exports_env_objects():
    assert Frontend_env.FrontendEnv is FrontendEnv
    assert Frontend_env.env is fixtures.env
    assert Frontend_env.full_env is fixtures.full_env
    assert sorted(Frontend_env.__all__) == ["FrontendEnv", "env", "full_env"]
