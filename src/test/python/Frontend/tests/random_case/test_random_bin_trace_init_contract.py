from __future__ import annotations

import ast
from pathlib import Path


def _test_random_bin_trace_function() -> tuple[ast.Module, ast.FunctionDef]:
    path = Path(__file__).with_name("test_random_bin_trace_dut.py")
    tree = ast.parse(path.read_text(encoding="utf-8"))
    function = next(
        node
        for node in tree.body
        if isinstance(node, ast.FunctionDef) and node.name == "test_random_bin_trace"
    )
    return tree, function


def test_random_bin_trace_defers_fixture_initialization_for_explicit_sv39_boot():
    tree, function = _test_random_bin_trace_function()
    calls = [
        node
        for node in ast.walk(function)
        if isinstance(node, ast.Call)
        and isinstance(node.func, ast.Attribute)
        and isinstance(node.func.value, ast.Name)
        and node.func.value.id == "env"
        and node.func.attr == "initialize"
    ]
    assert [arg.arg for arg in function.args.args] == ["env", "frontend_init_config"]
    assert len(calls) == 2
    assert any(
        any(
            keyword.arg == "bare_mode"
            and not (isinstance(keyword.value, ast.Constant) and keyword.value.value is True)
            for keyword in call.keywords
        )
        for call in calls
    )

    defer_marks = []
    for decorator in function.decorator_list:
        if not isinstance(decorator, ast.Call):
            continue
        if not isinstance(decorator.func, ast.Attribute) or decorator.func.attr != "parametrize":
            continue
        if not decorator.args or not isinstance(decorator.args[0], ast.Constant):
            continue
        if decorator.args[0].value != "frontend_init_config" or len(decorator.args) != 2:
            continue
        items = decorator.args[1]
        if not isinstance(items, (ast.List, ast.Tuple)) or len(items.elts) != 1:
            continue
        config = items.elts[0]
        if not isinstance(config, ast.Call) or len(config.args) or len(config.keywords) != 1:
            continue
        keyword = config.keywords[0]
        if keyword.arg != "defer" or not isinstance(keyword.value, ast.Constant):
            continue
        indirect = next(
            (item.value for item in decorator.keywords if item.arg == "indirect"), None
        )
        if isinstance(indirect, ast.Constant) and indirect.value is True and keyword.value.value is True:
            defer_marks.append(decorator)
    assert len(defer_marks) == 1

    imports_frontend_init = any(
        isinstance(node, ast.ImportFrom)
        and node.module == "env.runtime.frontend_init"
        and any(alias.name == "FrontendInitConfig" for alias in node.names)
        for node in tree.body
    )
    assert imports_frontend_init
