from env.nemu_trace_converter import convert_nemu_log_lines


def test_convert_nemu_log_lines_infers_branch_and_jump_targets():
    lines = [
        "\x1b[1;34m[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80001000, pc = 0x80001004\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080001000:   63 84 20 00     beq        0x80001008,s0,s1\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80001004, pc = 0x80000ff8\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080001004:   e3 1a 95 fe     bne        0x80000ff8,a0,s1\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80000ff8, pc = 0x80001010\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080000ff8:   ef 00 80 01     p_jal      ra,0x80001010\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80001010, pc = 0x80002000\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080001010:   82 99           c_jalr     ra,s3\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80002000, pc = 0x80002004\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080002000:   13 00 00 00     p_li_0     ra\x1b[0m",
    ]

    out = convert_nemu_log_lines(lines)
    assert len(out) == 5

    beq = out[0]
    assert beq["kind"] == "branch"
    assert beq["taken"] is False
    assert beq["target_pc"] == "0x80001004"

    bne = out[1]
    assert bne["kind"] == "branch"
    assert bne["taken"] is True
    assert bne["target_pc"] == "0x80000ff8"

    jal = out[2]
    assert jal["kind"] == "jump"
    assert jal["taken"] is True
    assert jal["target_pc"] == "0x80001010"

    jalr = out[3]
    assert jalr["kind"] == "jump_indirect"
    assert jalr["taken"] is True
    assert jalr["target_pc"] == "0x80002000"

    normal = out[4]
    assert normal["kind"] == "normal"
    assert normal["taken"] is False
    assert normal["target_pc"] is None


def test_convert_nemu_log_lines_uses_explicit_target_for_direct_compressed_jump():
    lines = [
        "\x1b[1;34m[src/cpu/cpu-exec.c:490,execute] end_of_loop: prev pc = 0x80001f78, pc = 0x80002d9a\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080001f78:   82 80           p_ret     \x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:469,execute] prev pc = 0x80002d9e, pc = 0x800027c2\x1b[0m",
        "\x1b[1;34m[src/cpu/cpu-exec.c:80,debug_hook] (M)0x0000000080002d9e:   15 b4           c_j        ,0x800027c2\x1b[0m",
    ]

    out = convert_nemu_log_lines(lines)

    ret = out[0]
    assert ret["kind"] == "jump_indirect"
    assert ret["taken"] is True
    assert ret["target_pc"] == "0x80002d9a"

    jump = out[1]
    assert jump["kind"] == "jump"
    assert jump["taken"] is True
    assert jump["target_pc"] == "0x800027c2"
