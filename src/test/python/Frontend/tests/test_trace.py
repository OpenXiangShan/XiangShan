from env.model import GoldenTrace, MemoryModel


def test_trace_from_memory_linear_program():
    mem = MemoryModel()
    # addi x0, x0, 0
    mem.write_u32(0x80000000, 0x00000013)
    mem.write_u32(0x80000004, 0x00100093)

    trace = GoldenTrace.from_memory(mem, 0x80000000, 2)
    assert len(trace.entries) == 2
    assert trace.entries[0].pc == 0x80000000
    assert trace.entries[1].pc == 0x80000004
    assert trace.entries[0].size == 4


def test_trace_cursor():
    mem = MemoryModel()
    mem.write_u32(0x0, 0x00000013)
    tr = GoldenTrace.from_memory(mem, 0x0, 1)
    assert tr.peek() is not None
    assert tr.next_entry() is not None
    assert tr.next_entry() is None


def test_trace_from_file_accepts_hex_string_fields(tmp_path):
    trace_path = tmp_path / "golden.json"
    trace_path.write_text(
        """
[
  {
    "index": 0,
    "pc": "0x80000000",
    "instr": "0x13",
    "size": 4,
    "kind": "normal",
    "taken": false,
    "target_pc": null,
    "exception": null
  },
  {
    "index": 1,
    "pc": "0x80000004",
    "instr": "0x6f",
    "size": 4,
    "kind": "jump",
    "taken": true,
    "target_pc": "0x80000010",
    "exception": "0xb"
  }
]
""".strip(),
        encoding="utf-8",
    )

    trace = GoldenTrace.from_file(str(trace_path))

    assert len(trace.entries) == 2
    assert trace.entries[0].pc == 0x80000000
    assert trace.entries[0].instr == 0x13
    assert trace.entries[1].pc == 0x80000004
    assert trace.entries[1].instr == 0x6F
    assert trace.entries[1].target_pc == 0x80000010
    assert trace.entries[1].exception == 0xB
