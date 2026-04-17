# T06 Golden Trace 与比对设计

## 1. 目标

建立统一的参考指令流模型，用于和 `FrontendMonitor` 观测结果逐条对齐：

- 支持从内存静态构建 Trace。
- 支持从外部文件加载 Trace（JSON/二进制）。
- 支持与 `BackendModel` 联动，触发分支更新与重定向。

## 2. 数据结构

```text
TraceEntry {
  index: int
  pc: int
  instr: int
  size: int         # 2 or 4
  kind: str         # normal/branch/jump/call/ret/exception/interrupt
  taken: bool
  target_pc: int | None
  exception: int | None
}
```

`GoldenTrace` 状态：

- `entries: list[TraceEntry]`
- `cursor: int`

## 3. 核心接口

- `from_memory(mem, start_pc, count) -> GoldenTrace`
- `from_file(path) -> GoldenTrace`
- `peek() -> TraceEntry | None`
- `next_entry() -> TraceEntry | None`
- `reset(cursor=0) -> None`

## 4. 构建流程

### 4.1 from_memory

1. 从 `start_pc` 连续读取指令。
2. 判定是否 RVC（低 2bit != `0b11`）。
3. 简化解码分支/JAL/JALR 类型并填充 `kind/taken/target_pc`。
4. 生成固定长度 trace（`count` 条）或遇到终止标记停止。

### 4.2 from_file

建议 JSON schema：

- `pc`, `instr`, `size`, `kind`, `taken`, `target_pc`, `exception`

## 5. 比对策略

逐条比对字段：

- 强一致：`pc`, `instr`, `size`
- 条件一致：`target_pc`（仅 branch/jump）
- 事件一致：`exception` 类型与触发位置

容错窗口：

- 允许 redirect 后若干周期内出现重取指重叠；由 monitor 状态机吸收。

## 6. 与 BackendModel 协同

- 当 `kind` 为 branch 且实际观测不一致，`BackendModel` 触发 update/redirect。
- 当 `kind` 为 exception/interrupt，`BackendModel` 注入异常重定向。

## 7. 验收标准

1. 线性程序 trace 与实际取指序列 100% 匹配。
2. 包含分支的程序可正确识别错误并驱动 redirect。
3. 外部 JSON trace 可加载并参与比较。
