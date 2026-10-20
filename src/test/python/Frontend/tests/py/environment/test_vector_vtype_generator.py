import sys
from pathlib import Path

from tests.asm_cases import generate_vector_vtype_cases


def test_vector_vtype_generator_is_deterministic_and_covers_config_forms(
    tmp_path: Path, monkeypatch
) -> None:
    case_name = "vector_vtype_test"
    output_dir = tmp_path / "generated"
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "generate_vector_vtype_cases.py",
            "--out-dir",
            str(output_dir),
            "--case-name",
            case_name,
        ],
    )
    assert generate_vector_vtype_cases.main() == 0

    output_path = output_dir / f"{case_name}.S"
    rendered = output_path.read_text(encoding="utf-8")
    assert rendered == generate_vector_vtype_cases._render(case_name)
    assert "li t2, 0x600\n    csrs mstatus, t2\n" in rendered
    instructions = [
        line.strip()
        for line in rendered.splitlines()
        if line.strip().startswith(("vsetvli ", "vsetivli "))
    ]
    assert instructions == 2 * [instruction for _name, instruction in generate_vector_vtype_cases.CASES] + [
        "vsetvli t0, zero, e32, m1, ta, ma",
    ]
    assert [instruction for _name, instruction in generate_vector_vtype_cases.CASES] == [
        "vsetvli t0, zero, e32, m1, ta, ma",
        "vsetivli t0, 4, e16, m2, tu, mu",
        "vsetvli t0, t1, e64, m4, ta, mu",
        "vsetivli zero, 0, e8, mf2, tu, ma",
    ]
    for _name, instruction in generate_vector_vtype_cases.EDGE_CASES:
        assert instruction in rendered
    assert (
        ".word 0x1d0072d7\n    nop\n"
        "    vsetvli t0, zero, e32, m1, ta, ma\n"
        "    vadd.vv v8, v16, v24\n    addi s0, s0, 1\n"
    ) in rendered
    assert rendered.count("    .rept 64\n") == 4
    assert all(f"    {instruction}\n" in rendered for instruction in (
        "vadd.vv v8, v16, v24", "vsub.vv v16, v8, v24",
        "vxor.vv v24, v8, v16", "vand.vv v8, v24, v16", "vor.vv v16, v8, v24",
    ))


def test_vector_vsetvl_generator_sets_source_registers(tmp_path: Path, monkeypatch) -> None:
    monkeypatch.setattr(sys, "argv", [
        "generate_vector_vtype_cases.py", "--out-dir", str(tmp_path),
        "--case-name", "vector_vsetvl_test", "--variant", "vsetvl",
    ])
    assert generate_vector_vtype_cases.main() == 0
    rendered = (tmp_path / "vector_vsetvl_test.S").read_text(encoding="utf-8")
    assert rendered == generate_vector_vtype_cases._render_vsetvl("vector_vsetvl_test")
    assert "vsetvli t0, zero, e32, m1, ta, ma\n    li t1, 0x49\n    vsetvl t3, t0, t1\n" in rendered
    assert "vsetvl t3, t0, t1\n    vadd.vv v6, v2, v4\n" in rendered
    assert rendered.count("    .rept 64\n") == 4
    assert rendered.index("vsetvl t3, t0, t1") < rendered.index("vadd.vv v8, v16, v24")
    assert rendered.index("vadd.vv v8, v16, v24") < rendered.index("vsetivli t0, 4, e16, m2, tu, mu")
