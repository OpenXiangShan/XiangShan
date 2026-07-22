# `issue_field_assigner.sv` 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv`

## 1. 文件职责

`issue_field_assigner` 是 scalar split issue 的字段翻译入口。scheduler 只决定本拍选择哪些
`memblock_issue_q_item_t`，本 helper 根据 item 的 `uid/target/pipe_idx` 从主表和状态表取得字段，
写入 `lintsissue_agent_agent_xaction` 的 V2 `issueLda/issueSta/issueStd` 端口。

它不改变 issue queue 仲裁、redirect/replay 生命周期或 pass/fail/terminal。字段写完后是否真正
fire，仍由 driver 的 `valid && ready` 和 sequence 的 fired-mask 回填决定。

## 2. V2 端口与字段边界

| target | V2 端口 | 本 helper 写入的关键字段 | 不存在或不得伪造的字段 |
|---|---|---|---|
| LOAD | `issueLda_0..2` | ROB/LQ/SQ 完整 key、`fuOpType`、src/imm、FTQ、PC、依赖字段、写回元信息 | `fuType`、`numLsElem` |
| STA | `issueSta_0..1` | `fuType[34:0]`、`fuOpType`、ROB/SQ 完整 key、src/imm、pdest/rfWen | LQ、FTQ、`numLsElem` |
| STD | `issueStd_0..1` | `fuType[34:0]`、`fuOpType`、ROB value、SQ 完整 key、store data | `robIdx_flag`、LQ、FTQ、`numLsElem` |

STA/STD 的 DUT-facing FuType 由 `encode_and_fit_dut_futype()` 无损转换。内部 36-bit FuType
容器不能直接裁剪后写入 V2 35-bit 端口。FTQ value/offset 分别消费
`MEMBLOCK_FTQ_PTR_VALUE_W` 和 `MEMBLOCK_FTQ_OFFSET_W`，不保留局部固定切片。

## 3. 合法矩阵

`assign_issue_item_fields()` 是唯一总入口。它先确认 split profile、target pipe 范围和主表项，
再复用 `lsq_ctrl_model::derive_op_behavior()` 与 classifier 检查如下矩阵：

| `fuType/fuOpType` | behavior | 合法 target | 结果 |
|---|---|---|---|
| LDU + 普通 load | `LOAD`，只 `route_load` | LOAD | 写 `issueLda` |
| LDU + software prefetch | `PREFETCH/is_prefetch=1`，只 `route_load` | LOAD | 写 `issueLda` |
| STU + scalar store | 同时 `route_sta/route_std` | STA 或 STD | 写对应 split port |
| MOU/AMO、CBO、vector LS、未知组合 | unsupported | 无 | 字段赋值前 `uvm_fatal` |

software prefetch 与普通 load 的 classifier 互斥，因此 scalar-only gate 不会误杀合法 prefetch。
本轮不把 V2 STA 物理上可承载的 CBO/AMO 当作普通 store；这些路径尚无完整 completion 闭环。

## 4. 关键函数

| 函数 | 输入/输出 | 功能和副作用 |
|---|---|---|
| `get_target_pipe_limit(target)` | target；返回 compile-time pipe 数 | LOAD/STA/STD 分别读取 `MEMBLOCK_DUT_*_PIPE_NUM`，非法 target fatal。 |
| `check_pipe_idx(target, pipe_idx, caller)` | target、局部 pipe、调用者 | 检查局部 pipe 小于该 target 的物理数量；只检查，不改状态。 |
| `check_target_futype_fuoptype(main_tr, behavior, target)` | 主表项、统一 behavior、target | 检查 FuType、fuOpType、route 和 target 一致；不复制第二套 operation classifier。 |
| `clear_lintsissue_xaction(tr)` | xaction | 清全部 V2 split issue valid/payload，避免上一拍残留。 |
| `assign_load_main_fields()` | xaction、主表项、item、局部 pipe | 写 LDA 主字段。 |
| `assign_sta_main_fields()` | 同上 | 无损编码 FuType 后写 STA 主字段。 |
| `assign_std_main_fields()` | 同上 | 无损编码 FuType 后写 STD 主字段，ROB 只写 value。 |
| `assign_issue_dep_fields()` | xaction、item、局部 pipe | 仅 LOAD 写 MDP/StoreSet 字段；STA/STD 不写依赖字段，V2 端口不存在这些字段。 |
| `assign_backend_meta_fields()` | xaction、item、局部 pipe | LOAD 写 FTQ/PC/pdest/WEN，STA 写 pdest/rfWen，STD 不写额外 meta。 |
| `assign_issue_item_fields()` | xaction、item、局部 pipe | 完成所有前置检查，再依次调用主字段、依赖字段和后端元信息 helper。 |

## 5. 总入口文字伪代码

```text
assign_issue_item_fields(tr, item, pipe_idx)：
  绑定 common_data；tr 为空则 fatal；
  用 item.uid 取得主表项，缺失则 fatal；
  当前 profile 不是 split issue 则 fatal；
  根据 item.target 的 compile-time pipe 数检查 pipe_idx；
  vector LS 直接以 scalar-scope 错误 fatal；
  调用 derive_op_behavior 取得统一 operation behavior；
  调用 check_target_futype_fuoptype，确认 FuType、fuOpType、route 与 target 一致；
  调用 assign_main_issue_fields，写对应 V2 split port 的主 payload；
  调用 assign_issue_dep_fields，只给真实存在该字段的 target 写依赖元信息；
  调用 assign_backend_meta_fields，只给真实存在该字段的 target 写后端元信息；
  本函数不删除 queue item、不置 dispatched，也不生成 pass/fail。
```

## 6. 支持边界

- 本轮只支持 scalar load、software prefetch 和 scalar store。
- `issueVldu` 由独立 vecissue agent 承载；scalar testcase 不启动其随机 default sequence，
  vecissue driver 收到非零 valid 时 fail-fast。
- MOU/AMO/CBO 和 vector LS 不会静默映射到 LDA/STA/STD。
- STD real writeback、IQ feedback、redirect/replay 和最终状态收敛由各自 flow owner 处理。
