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
    assert instructions == [instruction for _name, instruction in generate_vector_vtype_cases.CASES]
    assert [instruction for _name, instruction in generate_vector_vtype_cases.CASES] == [
        "vsetvli t0, zero, e32, m1, ta, ma",
        "vsetivli t0, 4, e16, m2, tu, mu",
        "vsetvli t0, t1, e64, m4, ta, mu",
        "vsetivli zero, 0, e8, mf2, tu, ma",
    ]
    for _name, instruction in generate_vector_vtype_cases.EDGE_CASES:
        assert instruction in rendered
