# issue_field_assigner.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/issue_field_assigner.sv`

## 1. 文件定位与使用场景

发射前字段赋值 helper，目标是 `lintsissue_agent_agent_xaction`。scheduler 只选出“发谁”，这个 helper 负责把主表/status/queue item 翻译成 DUT lintsissue port 上的每个信号。

输入是 issue item、主表 transaction、status 和配置权重。输出是 xaction 的 port 0/1/2 load、3/4 STA、5/6 STD payload。

控制逻辑字段主要是 `valid`、load wait/StoreSet 相关字段、`isFirstIssue` 和 write enable。地址、ROB/LQ/SQ、pc/ftq/pdest 等更多是随接口携带的 payload。当前源码里 STD 只填主字段，没有第二类依赖字段和第三类后端 meta 字段。

关键函数：

- `clear_lintsissue_xaction(tr)`：清 0/1/2 load、3/4 STA、5/6 STD 所有 valid/payload。
- `assign_load_main_fields()`：load pipe 0/1/2 赋 `valid/fuOpType/src_0/imm/robIdx/lqIdx/sqIdx`。
- `assign_sta_main_fields()`：STA pipe 3/4 赋 `valid/fuType/fuOpType/src_0/imm/robIdx/sqIdx`。
- `assign_std_main_fields()`：STD pipe 5/6 赋 `valid/fuType/fuOpType/src_0/robIdx/sqIdx`。
- `assign_main_issue_fields()`：按 target 分派上述函数。
- `assign_issue_dep_fields()`：为 load 赋 `loadWaitBit/waitForRobIdx/storeSetHit/loadWaitStrict`；为 STA 赋 `isFirstIssue/storeSetHit/ssid`；STD 当前无第二类字段。
- `assign_backend_meta_fields()`：为 load/STA 赋 `pdest/rfWen/fpWen/pc/isRVC/ftqIdx/ftqOffset`；STD 当前无第三类字段。
- `assign_issue_item_fields()`：依次调用主字段、依赖字段、后端 meta。
- `derive_wen()`：INT load 和 AMO 写 int，FP load 写 fp，store/prefetch 不写回。
- `select_prior_store_for_load()`：为 load wait 选择之前 active/enq 的 store。
- `deterministic_percent_hit()`：基于 uid/salt 的确定性概率，用于 MDP/RVC 等字段。

## 2. 字段与函数/task 设计原理

`issue_field_assigner` 的设计重点是把“调度选择”和“接口赋值”彻底分开。scheduler 只选 item；assigner 根据 uid 从主表/状态表取字段，填到 lintsissue xaction 的正确 port。

关键函数：

| 函数 | 参数 | 功能和设计原理 |
|---|---|---|
| `ensure_data()` | 无 | 绑定公共 owner。 |
| `deterministic_percent_hit(uid,percent,salt)` | uid、百分比、salt | 用 uid 和 salt 得到确定性概率命中，不依赖随机调用顺序，保证回归可复现。 |
| `is_valid_pipe_idx(target,pipe_idx)`、`check_pipe_idx(target,pipe_idx)` | target、pipe index | 根据 LOAD/STA/STD 各自 pipe 数检查 port 合法性，防止越界赋值。 |
| `select_prior_store_for_load(uid,store_uid)` | load uid、输出 store uid | 给 load wait/MDP 字段选择前序 store，避免等待未来 store 造成非法激励。 |
| `compute_is_rvc(uid)` | uid | 按权重生成 RVC 元信息，不影响 MemBlock 主调度，但随发射下发给后端/调试路径。 |
| `compute_pc(uid)` | uid | 使用 `pc_base + uid * pc_stride` 生成可追踪 PC，便于日志和 waveform 对齐。 |
| `compute_ftq_flag/value/offset(uid)` | uid | 生成 FTQ 元信息，作为第三类后端字段随流水线传递。 |
| `compute_pdest(uid,has_wb)` | uid、是否写回 | 只有写回有效时生成物理目的寄存器，避免无写回指令携带误导性 pdest。 |
| `derive_wen(main_tr,rfWen,fpWen)` | 主表 tr、输出 int/fp 写使能 | 根据 op_class 推导 `rfWen/fpWen`。这些字段在 MemBlock 侧主要随流水线传递，对 load 写回端和后端 RF 写回有意义。 |
| `clear_lintsissue_xaction(tr)` | xaction | 每拍清空所有 LOAD/STA/STD port，避免上一拍 valid 或 payload 残留。 |
| `assign_load_main_fields(tr,pipe_idx,item,main_tr,status)` | xaction、pipe、item、主表、状态 | 填 load port 的主控制字段，如 fuType/fuOpType/addr/ROB/LQ。 |
| `assign_sta_main_fields(...)`、`assign_std_main_fields(...)` | xaction、pipe、item、主表、状态 | 分别填 STA/STD port 字段，store 拆 target 后各自赋值。 |
| `assign_main_issue_fields(tr,item,pipe_idx)` | xaction、issue item、pipe | 根据 target 分派到 load/STA/STD 主字段赋值函数。 |
| `assign_issue_dep_fields(tr,item,pipe_idx)` | xaction、issue item、pipe | 发射前补第二类字段，如 `loadWaitBit/waitForRobIdx/storeSetHit/loadWaitStrict/isFirstIssue`。 |
| `assign_backend_meta_fields(tr,item,pipe_idx)` | xaction、issue item、pipe | 发射前补第三类后端字段，如 `pc/isRVC/ftqIdx/ftqOffset/pdest/rfWen/fpWen`。 |
| `assign_issue_item_fields(tr,item,pipe_idx)` | xaction、issue item、pipe | 单个 item 的总赋值入口：先主字段，再依赖字段，再后端元信息。 |

这样设计的好处是，后续如果某个字段赋值规则需要调整，只改 assigner；如果调度规则调整，只改 scheduler，两者不会互相污染。
