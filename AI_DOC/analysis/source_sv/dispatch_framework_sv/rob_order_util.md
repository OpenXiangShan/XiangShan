# rob_order_util.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/rob_order_util.sv`

## 1. 文件定位与使用场景

ROB/LQ/SQ key 工具。它不保存状态，负责把 DUT 的环形指针语义统一封装起来，避免各处用普通整数比较导致回绕错误。

输入是 `{flag,value}` key 或 redirect payload；输出是关联数组 key、推进后的 key、年龄比较结果或 flush 判断结果。它直接影响 send priority tie-break、redirect flush 范围和 active map 索引。

函数：

- `check_rob_key()`：内部合法性检查，确认 ROB value 未超过 ROB size。
- `rob_to_map_key()`、`lq_to_map_key()`、`sq_to_map_key()`：压缩为关联数组 key。
- `rob_advance(base,step)`：ROB 环形推进，回绕时翻转 flag。
- `rob_is_after(left,right)`：按 `CircularQueuePtr.>` 语义比较 younger。
- `rob_need_flush(uop_rob,redirect)`：实现 `flush_itself && same` 或 younger flush。

## 2. 字段与函数/task 设计原理

`rob_order_util` 是 ROB/LQ/SQ 环形指针的小工具 class。它不保存状态，只统一处理 flag/value 转 key 和 ROB 年龄比较。

| 函数 | 参数 | 功能和设计原理 |
|---|---|---|
| `check_rob_key(key, caller)` | ROB key、调用者名字 | 只做 ROB value 合法性检查，不再承担 key 构造职责。 |
| `rob_to_map_key(key)`、`lq_to_map_key(key)`、`sq_to_map_key(key)` | ROB/LQ/SQ key | 把结构化 key 转成关联数组索引，统一 map key 格式；ROB key 会先检查 value 范围。 |
| `rob_advance(base,step)` | 起始 ROB key、步数 | 处理 ROB 环形递增和 flag 翻转，主表生成时可用于连续 ROB 分配。 |
| `rob_is_after(left,right)` | 两个 ROB key | 判断环形 ROB 年龄关系。redirect flush、issue tie-break 都依赖同一规则。 |
| `rob_need_flush(uop_rob,redirect)` | uid 的 ROB、redirect payload | 判断某 uid 是否位于 redirect 之后或本身需要 flush。集中实现可以保证 recovery handler 和调度器使用同一 flush 语义。 |
