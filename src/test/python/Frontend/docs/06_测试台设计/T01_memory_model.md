# T01 内存与页表模型设计

## 1. 目标与范围

T01 提供所有 agent/monitor 的共享数据底座：

- `MemoryModel`：统一物理地址空间，支持 bin 加载与 cacheline 读取。
- `PageTableModel`：统一地址翻译入口，支持 Bare/Sv39 两类模式。

非目标：

- 不模拟真实 cache 替换策略。
- 不实现复杂 MMU side effect（如 A/D 位自动写回）。

## 2. 核心类

### 2.1 `MemoryModel`

建议数据结构：

```text
mem: dict[int, int]   # byte-addressable, addr -> byte(0..255)
mmio_ranges: list[tuple[int, int]]
```

核心接口：

- `load_bin(bin_data: bytes, base_addr: int) -> None`
- `load_file(path: str, base_addr: int) -> None`
- `read_u32(addr: int) -> int`
- `read_block(addr: int, size: int, default_byte: int = 0) -> bytes`
- `read_cacheline(addr: int, line_bytes: int = 64) -> tuple[int, int]`
- `is_mmio(addr: int) -> bool`

约定：

- `read_cacheline()` 自动按 `line_bytes` 对齐。
- 返回值为 little-endian 拼装的两个 256-bit 整数 `(beat0, beat1)`，对应 64B line。
- 未映射地址默认返回 `0x13 0x00 0x00 0x00` 模式（NOP 填充）用于指令流连续性验证。

### 2.2 `PageTableModel`

建议数据结构：

```text
mode: str  # "bare" | "sv39"
pte_map: dict[int, PTE]   # vpn -> pte
```

`PTE` 最小字段：

- `ppn`, `v`, `r`, `w`, `x`, `u`, `g`, `a`, `d`, `level`

核心接口：

- `set_mode(mode: str) -> None`
- `map_page(vpn: int, ppn: int, *, v=1, r=1, w=0, x=1, level=0) -> None`
- `translate(va: int) -> tuple[int, bool, dict]`
- `build_ptw_resp(vpn: int) -> dict`

## 3. 翻译语义

### 3.1 Bare

- `pa = va`
- `valid = True`
- `attr.mode = "bare"`

### 3.2 Sv39（黑盒简化版）

- 以 `vpn = va >> 12` 查表。
- 命中且 `v=1 且 x=1`：`pa = (ppn << 12) | (va & 0xFFF)`。
- 否则返回 `valid=False`，并在 `attr` 标注 `page_fault=True`。

## 4. MMIO 地址规划

默认建议：

- DRAM：`[0x8000_0000, 0xBFFF_FFFF]`
- MMIO：`[0x0000_0000, 0x0FFF_FFFF]`

允许通过构造参数覆盖，供 `UncacheAgent` 直接调用 `is_mmio()`。

## 5. 与其他模块交互

- T02/T03 调用 `read_cacheline()` 或 `read_block()` 生成总线响应。
- T04 调用 `read_u32(pc)` 校验 cfVec 指令编码。
- T06 调用内存读取生成静态 Golden Trace。

## 6. 错误模型与日志

统一抛出：

- `AddressError`：地址越界/未对齐（可配置是否严格）
- `PageFaultError`：Sv39 翻译失败

统一事件日志字段：

- `cycle`, `kind`, `addr`, `detail`

## 7. 验收标准

单元测试最小集合：

1. `load_bin` 后 `read_u32` 与原始编码一致。
2. `read_cacheline` 的 beat0/beat1 字节顺序正确。
3. Bare/Sv39 翻译结果符合预期。
4. MMIO 判定边界地址正确。
