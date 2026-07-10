from __future__ import annotations

from env.nemu_trace_converter import convert_nemu_log_lines


def test_convert_nemu_log_lines_keeps_debug_after_interleaved_execute_lines() -> None:
    lines = [
        "[src/cpu/cpu-exec.c:490,execute] end_of_loop: prev pc = 0x80000078, pc = 0x8000007c",
        "[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080000078:   73 90 07 30     csrrw      $0,csr_0x300,a5",
        "[src/cpu/cpu-exec.c:469,execute] prev pc = 0x8000007c, pc = 0x8000007e",
        "[src/cpu/cpu-exec.c:80,debug_hook] (M)0x000000008000007c:   fd 57           c_li       a5,$0,-1",
        "[src/cpu/cpu-exec.c:490,execute] end_of_loop: prev pc = 0x80000082, pc = 0x80000082",
        "[src/cpu/cpu-exec.c:80,debug_hook] (M)0x000000008000007e:   73 90 07 3b     csrrw      $0,csr_0x3b0,a5",
        "[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80000082, pc = 0x80000086",
        "[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080000082:   83 37 84 fe     ld         a5,-24(s0)",
    ]

    rows = convert_nemu_log_lines(lines)

    pcs = [int(row["pc"], 16) for row in rows]
    assert pcs == [0x80000078, 0x8000007C, 0x8000007E, 0x80000082]


def test_convert_execute_only_log_decodes_instructions_from_bin() -> None:
    base = 0x80000000
    addi = 0x00100093
    jal = 0x0080006F
    image = addi.to_bytes(4, "little") + jal.to_bytes(4, "little") + bytes(8)
    lines = [
        "[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80000000, pc = 0x80000004",
        "[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80000004, pc = 0x8000000c",
    ]

    rows = convert_nemu_log_lines(lines, binary_image=image, memory_base=base)

    assert rows == [
        {
            "index": 0,
            "pc": "0x80000000",
            "instr": hex(addi),
            "size": 4,
            "kind": "normal",
            "taken": False,
            "target_pc": None,
        },
        {
            "index": 1,
            "pc": "0x80000004",
            "instr": hex(jal),
            "size": 4,
            "kind": "jump",
            "taken": True,
            "target_pc": "0x8000000c",
        },
    ]
