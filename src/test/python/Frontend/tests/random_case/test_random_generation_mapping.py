from __future__ import annotations

import json
import shutil
import subprocess
from pathlib import Path

import pytest

from .generate_random_asm import CaseSpec, generate_case, write_cases
from .program_map import build_program_map, parse_disassembly

FRONTEND_ROOT = Path(__file__).resolve().parents[2]


def test_generator_is_deterministic_and_seed_sensitive() -> None:
    spec = CaseSpec(
        family="cfg_layout_mix",
        seed=0x1234,
        blocks=24,
        iterations=3,
    )
    source_a, metadata_a = generate_case(spec)
    source_b, metadata_b = generate_case(spec)
    source_c, metadata_c = generate_case(
        CaseSpec(family="cfg_layout_mix", seed=0x1235, blocks=24, iterations=3)
    )

    assert source_a == source_b
    assert metadata_a == metadata_b
    assert source_a != source_c
    assert metadata_a["source_sha256"] != metadata_c["source_sha256"]
    assert all(phase in source_a for phase in ("warmup", "train", "perturb", "stress", "drain"))
    assert ".fill 2047, 2, 0x0001" in source_a
    trap_index = source_a.index(".word 0x0005006b")
    drain_padding_index = source_a.index(".rept 128", trap_index)
    assert drain_padding_index > trap_index
    assert "c.nop" in source_a[drain_padding_index:]
    assert set(metadata_a["kind_counts"]) == {
        "branch_rvi",
        "branch_rvc",
        "jal",
        "c_j",
        "jalr",
        "call_ret",
    }


def test_history4_indirect_targets_are_opt_in_and_have_distinct_artifact_names() -> None:
    fixed = CaseSpec(family="cfg_layout_mix", seed=0x1234, blocks=48)
    varied = CaseSpec(family="cfg_layout_mix", seed=0x1234, blocks=48, indirect_target_mode="history4")
    old_source, _ = generate_case(fixed)
    source, metadata = generate_case(varied)
    replay, _ = generate_case(varied)
    assert source == replay
    assert source != old_source
    assert varied.name == fixed.name + "_ih4"
    assert "_target_0:" not in old_source
    assert "srli t1, s3, 7" in source
    assert "slli t1, t1, 5" in source
    episodes = [e for e in metadata["episodes"] if e["kind"] == "jalr"]
    assert episodes
    assert all(e["parameters"]["target_count"] == 4 for e in episodes)
    assert source.count("_target_3:") == len(episodes)
    assert metadata["spec"]["indirect_target_mode"] == "history4"


def test_history4_rejects_families_that_do_not_implement_it() -> None:
    with pytest.raises(ValueError, match="require a CFG or translation family"):
        generate_case(CaseSpec(family="ras_indirect_history", seed=1, indirect_target_mode="history4"))


def test_ras_family_contains_nested_and_indirect_control_flow() -> None:
    source, metadata = generate_case(
        CaseSpec(
            family="ras_indirect_history",
            seed=0x5678,
            blocks=16,
            iterations=3,
            max_call_depth=5,
        )
    )

    assert "ras_chain_000_d00" in source
    assert "jalr zero, 0(ra)" in source
    assert "jal t0," in source
    assert "jalr zero, 0(t0)" in source
    assert metadata["max_generated_call_depth"] <= 5
    assert metadata["max_generated_call_depth"] >= 2
    assert metadata["logical_static_instructions"] > 500


def test_ibuffer_pressure_family_contains_long_boundary_bursts() -> None:
    source, metadata = generate_case(
        CaseSpec(
            family="ibuffer_pressure_cross",
            seed=0x9876,
            blocks=16,
            iterations=3,
        )
    )

    assert "ibp_main_loop" in source
    assert "jalr zero, 0(t1)" in source
    assert "jalr zero, 0(ra)" in source
    assert {14, 28, 30, 60, 62} & set(metadata["boundary_offsets"])
    assert min(metadata["pressure_burst_lengths"]) >= 32
    assert metadata["maximum_pressure_burst"] >= 64
    assert metadata["logical_static_instructions"] > 800


def test_write_cases_records_replay_metadata(tmp_path: Path) -> None:
    records = write_cases(
        [CaseSpec(family="cfg_layout_mix", seed=7, blocks=16, iterations=2)],
        tmp_path,
    )

    assert len(records) == 1
    record = records[0]
    source_path = Path(record["source_path"])
    sidecar_path = Path(record["generator_manifest_path"])
    assert source_path.is_file()
    assert sidecar_path.is_file()
    assert json.loads(sidecar_path.read_text(encoding="ascii"))["source_sha256"] == record[
        "source_sha256"
    ]
    assert record["episodes"]
    assert record["episodes"][0]["entry_labels"]


@pytest.mark.skipif(
    shutil.which("riscv64-unknown-elf-gcc") is None,
    reason="RISC-V assembler is not installed",
)
@pytest.mark.parametrize(
    "family",
    (
        "cfg_layout_mix",
        "ras_indirect_history",
        "ibuffer_pressure_cross",
        "translation_timing_cross",
    ),
)
def test_generated_source_assembles(family: str, tmp_path: Path) -> None:
    source, _ = generate_case(
        CaseSpec(
            family=family,
            seed=0xA55A,
            blocks=16,
            iterations=2,
            max_call_depth=4,
        )
    )
    asm_path = tmp_path / f"{family}.S"
    elf_path = tmp_path / f"{family}.elf"
    asm_path.write_text(source, encoding="ascii")
    subprocess.run(
        [
            "riscv64-unknown-elf-gcc",
            "-march=rv64gc_zicsr_zifencei",
            "-mabi=lp64",
            "-static",
            "-no-pie",
            "-nostdlib",
            "-nostartfiles",
            "-Wl,--build-id=none",
            "-Wl,-Ttext=0x80000000",
            "-Wl,-e,_start",
            "-o",
            str(elf_path),
            str(asm_path),
        ],
        check=True,
    )
    assert elf_path.is_file()


def test_objdump_parser_preserves_instruction_width_and_labels() -> None:
    instructions, labels = parse_disassembly(
        """
0000000080000000 <_start>:
    80000000:  0001                 c.addi  x0,0
    80000002:  008000ef             jal     x1,8000000a <callee>
000000008000000a <callee>:
    8000000a:  00008067             jalr    x0,0(x1)
"""
    )

    assert labels == {"_start": 0x80000000, "callee": 0x8000000A}
    assert [item["size"] for item in instructions] == [2, 4, 4]
    assert instructions[0]["labels"] == ["_start"]
    assert instructions[-1]["labels"] == ["callee"]


@pytest.mark.parametrize(
    "second_pc",
    [0x80000000, 0x80000002],
)
def test_objdump_parser_rejects_duplicate_or_overlapping_instructions(second_pc) -> None:
    with pytest.raises(ValueError, match="out-of-order, overlapping, or duplicate"):
        parse_disassembly(
            f"""
    80000000:  00000013             addi    x0,x0,0
    {second_pc:x}:  0001                 c.addi  x0,0
"""
        )


def test_objdump_parser_rejects_out_of_order_instructions() -> None:
    with pytest.raises(ValueError, match="out-of-order"):
        parse_disassembly(
            """
    80000004:  0001                 c.addi  x0,0
    80000000:  0001                 c.addi  x0,0
"""
        )


def test_objdump_parser_rejects_conflicting_label_addresses() -> None:
    with pytest.raises(ValueError, match="conflicting disassembly label"):
        parse_disassembly(
            """
0000000080000000 <same>:
    80000000:  0001                 c.addi  x0,0
0000000080000002 <same>:
    80000002:  0001                 c.addi  x0,0
"""
        )


@pytest.mark.skipif(
    any(
        shutil.which(tool) is None
        for tool in (
            "riscv64-unknown-elf-gcc",
            "riscv64-unknown-elf-objcopy",
            "riscv64-unknown-elf-objdump",
        )
    ),
    reason="RISC-V ELF tools are not installed",
)
def test_program_map_is_verified_against_actual_binary(tmp_path: Path) -> None:
    source, metadata = generate_case(
        CaseSpec(family="cfg_layout_mix", seed=0xC0FFEE, blocks=16, iterations=2)
    )
    source_path = tmp_path / "case.S"
    elf_path = tmp_path / "case.elf"
    binary_path = tmp_path / "case.bin"
    source_path.write_text(source, encoding="ascii")
    subprocess.run(
        [
            "riscv64-unknown-elf-gcc",
            "-march=rv64gc_zicsr_zifencei",
            "-mabi=lp64",
            "-static",
            "-no-pie",
            "-nostdlib",
            "-nostartfiles",
            "-Wl,--build-id=none",
            "-Wl,-Ttext=0x80000000",
            "-Wl,-e,_start",
            "-o",
            str(elf_path),
            str(source_path),
        ],
        check=True,
    )
    subprocess.run(
        [
            "riscv64-unknown-elf-objcopy",
            "-O",
            "binary",
            "-j",
            ".text",
            str(elf_path),
            str(binary_path),
        ],
        check=True,
    )

    program_map = build_program_map(
        source_path,
        binary_path,
        metadata,
        link_address=0x80000000,
        binary_base=0x80000000,
    )

    assert program_map["binary_text_verified"] is True
    assert program_map["summary"]["episode_region_count"] >= 17
    assert program_map["summary"]["feature_counts"]["crosses_page"] == 1
    assert any(item["episode_id"] == "cfg-page-tail-probe" for item in program_map["instructions"])


def test_random_sources_are_not_discovered_by_directed_asm_suite() -> None:
    suite_script = FRONTEND_ROOT / "scripts" / "run_baremode_asm_suite.sh"
    listed = subprocess.run(
        ["bash", str(suite_script), "--list"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.splitlines()

    assert listed
    assert all("/tests/asm_cases/" in path for path in listed)
    assert all("/tests/random_case/" not in path for path in listed)
