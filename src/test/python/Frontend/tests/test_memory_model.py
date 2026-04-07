from env.model import MemoryModel, PageTableModel


def test_memory_load_and_read_u32():
    mem = MemoryModel()
    mem.load_bin(bytes([0x13, 0x05, 0x00, 0x00]), 0x80000000)
    assert mem.read_u32(0x80000000) == 0x00000513


def test_cacheline_two_beat_layout():
    mem = MemoryModel()
    for i in range(64):
        mem.write_u8(0x1000 + i, i)
    beat0, beat1 = mem.read_cacheline(0x1000)
    assert beat0 & 0xFF == 0
    assert (beat0 >> (31 * 8)) & 0xFF == 31
    assert beat1 & 0xFF == 32
    assert (beat1 >> (31 * 8)) & 0xFF == 63


def test_page_table_translate_modes():
    pt = PageTableModel(mode="bare")
    pa, valid, _ = pt.translate(0x80001234)
    assert valid and pa == 0x80001234

    pt.set_mode("sv39")
    pt.map_page(vpn=0x80001, ppn=0x90001, v=1, x=1, r=1)
    pa, valid, _ = pt.translate(0x80001008)
    assert valid
    assert pa == 0x90001008
