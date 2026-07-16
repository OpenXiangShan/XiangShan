# mem_ut V2 Int Writeback 测试框架适配 Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| 关联执行 plan | `mem_ut_v2_int_wb_writeback_framework_adapt_execution_plan_20260708.md` |
| 适配原则 | 只让 V2 真实来源字段进入 raw event；缺失 key 只能由公共状态反查或 fail-fast，不从未连接字段伪造 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 split int writeback 进入公共状态流时需要解决的问题。每个问题均说明 V2 问题、修改原因、最终方案、修改的逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- `writebackLda_0/1/2`、`writebackSta_0/1`、`writebackStd_0/1` raw event 采样。
- `dispatch_raw_int_wb_t -> memblock_wb_event_t -> writeback_status_handler` 字段来源自洽。
- `writebackStd` 只有 `robIdx_value` 时的受限 active ROB 反查。
- V2 无来源字段的删除、默认值或 fail-fast 策略。

本轮不支持：

- RM、scoreboard、checker 或 coverage。
- monitor analysis port producer 恢复。
- 从未接入顶层 writeback output 的内部 wire 直接补 SQ key。
- 将 atomic/MOU 正向闭环并入普通 LOAD writeback。

## 2. 问题一：V2 split writeback 不再等同 V3 聚合 intWriteback

### V2 问题

V2 `MemBlock.sv` 的 int writeback 已拆成：

```text
writebackLda_0/1/2
writebackSta_0/1
writebackStd_0/1
```

旧逻辑仍按 V3 聚合 `intWriteback_*` 语义处理部分字段，容易把不存在的 `robIdx_flag`、`lqIdx_flag/value`、`sqIdx_flag/value` 或异常位写入 `dispatch_raw_int_wb_t`。

### 修改原因

公共状态表依赖 ROB/LQ/SQ key 定位 uid、issue epoch 和 replay sequence。raw event 若携带伪 key，会导致 writeback 归属错误、stale event 过滤错误、fault/pass 更新错误，甚至让主动 flow 卡死。

### 修改方案与修改逻辑

`dispatch_raw_int_wb_t` 使用 `source_kind + port_id` 表示 V2 物理来源：

| `source_kind` | `port_id` | V2 port |
|---|---:|---|
| `LDA` | 0..2 | `writebackLda_0/1/2` |
| `STA` | 0..1 | `writebackSta_0/1` |
| `STD` | 0..1 | `writebackStd_0/1` |

monitor 只复制 V2 RTL 真实存在的字段。对不进入 `dispatch_raw_int_wb_t` 且无 V2 来源的字段，从 interface、xaction、monitor 局部变量和 X/Z 检查中删除。

### 文字伪代码

```text
monitor 采样 V2 writeback：
  按固定顺序检查 LDA0/1/2、STA0/1、STD0/1；
  如果 valid 为 X/Z，uvm_fatal；
  如果 valid 为 0，不生成 raw；
  如果 valid 为 1：
    调用 build_raw_int_wb_from_v2_port() 构造 raw；
    builder 按 source_kind 和 port_id 校验合法 V2 lane；
    builder 从 make_empty_raw_int_wb() 开始清零全部字段；
    builder 只复制该 lane 真实存在的 ROB、exception、trigger、flushPipe、replayInst 字段；
    builder 保存同拍 sample_flush_epoch 和 cycle；
    mon_data() 对每个 valid lane 恰好 push 一次 raw；
```

## 3. 问题二：LDA/STA 缺少 LQ/SQ key 不能从伪字段读取

### V2 问题

V2 LDA/STA port 有真实 ROB key，但 LDA 顶层不携带 LQ key，STA 顶层不携带 SQ key。旧逻辑若把缺失字段置 valid 或从未连接字段读取，会生成错误的 `has_lq/has_sq` event。

### 修改原因

LQ/SQ key 是 target metadata，不是 LDA/STA raw 的物理身份来源。LDA/STA 必须先通过真实 ROB key 定位当前 active uid，再从 issue/enqueue 阶段保存的 status 中补齐 LQ/SQ key 和发射代。

### 修改方案与修改逻辑

LDA/STA normal/fault raw 在 converter 入口先调用 `attach_current_issue_snapshot()`：

- 输入为 raw 中真实 ROB key、source kind、target 类型和当前 active status map。
- 输出为 uid、完整 ROB/LQ/SQ key、`issue_epoch/replay_seq`。
- 失败时固定 fail-fast，不新增历史 event 猜测或旧状态恢复路径。

`normalize_v2_int_wb_key()` 不再负责从 raw 伪 LQ/SQ key 构造身份，只校验 attach 后的 current snapshot 与 raw 物理来源一致。

### 文字伪代码

```text
转换 LDA/STA raw：
  检查 raw.source_kind 是 LDA 或 STA；
  检查 raw.rob_valid=1 且 ROB flag/value 来自真实 V2 port；
  调用 attach_current_issue_snapshot(raw.rob_key, source_kind)；
    helper 通过 active ROB map 定位唯一 current uid；
    helper 读取该 uid 的当前 status、main transaction 和 issue metadata；
    helper 检查该 uid 未被 kill、redirect、flush 或进入不匹配 replay 代；
    helper 按 source kind 补齐 LDA 的 LQ key 或 STA 的 SQ key；
    helper 返回 uid、完整 key 和 issue_epoch/replay_seq；
  如果 helper 失败：
    uvm_fatal，不生成误导性 writeback event；
  调用 normalize_v2_int_wb_key()：
    只校验 current snapshot 与 raw ROB/source_kind 一致；
    不从缺失 raw key 伪造 LQ/SQ；
  生成 memblock_wb_event_t 并交给 writeback_status_handler；
```

## 4. 问题三：STD 只有 ROB value，不能默认 ROB flag 或 SQ key

### V2 问题

`writebackStd_0/1` 当前只确认有 `valid` 和 `robIdx_value`，没有真实 `robIdx_flag`、`sqIdx_flag` 或 `sqIdx_value`。旧逻辑若默认 `robIdx_flag=0` 或 `sq_valid=1`，会把 store data writeback 归属到错误 uid。

### 修改原因

STD real writeback 是 store data pass 闭环的关键事件。若 key 不完整却继续生成 pass event，会导致错误通过；若静默 drop，又可能让 `MEMBLOCK_STD_REAL_WB_PASS_EN=1` 的 flow 永远等待。

### 修改方案与修改逻辑

STD 使用受限 value-only 反查：

1. monitor 保存 `rob_value_only_without_flag=1`，保持 `rob_valid=0/sq_valid=0`。
2. adapter 对 `{flag=0,value}` 和 `{flag=1,value}` 两个 ROB key 做 active map probe。
3. 只有唯一 active STD candidate 且生命周期检查通过，才从 status 补完整 ROB/SQ key。
4. 若反查失败，`MEMBLOCK_STD_REAL_WB_PASS_EN=1` 时 fatal；关闭 real-WB pass 时允许 warning+计数 drop。

### 文字伪代码

```text
转换 STD raw：
  检查 raw.source_kind=STD；
  检查 raw.rob_value_only_without_flag=1；
  生成两个候选 ROB key：
    candidate0 = {flag=0, value=raw.rob_value};
    candidate1 = {flag=1, value=raw.rob_value};
  通过 active ROB map 分别 probe 两个候选；
  过滤不是 active STD、已 kill、已 redirect、已 flush 或发射代不匹配的候选；
  如果唯一候选存在：
    从 status 补完整 ROB key、SQ key、uid 和发射代；
    生成 STD writeback event；
  如果无候选或多候选：
    如果 get_std_real_wb_pass_en() 为 1，uvm_fatal；
    否则 uvm_warning 并增加 drop 计数，不生成 pass event；
```

## 5. 问题四：LDA replayInst 与 LDA0 atomic owner 不属于普通 LOAD pass

### V2 问题

V2 LDA port 会暴露 `replayInst`，且 `writebackLda_0` 可能被 AtomicsUnit 覆盖。旧逻辑若把所有 LDA raw 都当作普通 LOAD writeback，会把当前未支持的 replay/atomic producer 混入 LOAD pass。

### 修改原因

当前受支持 scalar LOAD producer 域没有已证实的 `replayInst=1` 正向来源。`writebackLda_0` 的 atomic owner 也不属于本 plan 的普通 LOAD 功能域，当前框架没有完成 AMO/MOU 正向闭环。

### 修改方案与修改逻辑

- monitor 原样保存 LDA `replayInst`。
- converter 在 key attach、owner 检查和 `real_wb_valid` 赋值前先检查 `replayInst`；若为 1，固定 fatal。
- 对 `writebackLda_0`，先用真实 ROB key 解析 uid，再读取主表 `op_class`；若为 `MEMBLOCK_OP_CLASS_AMO`，在 LOAD/LQ 生命周期判断前固定命中 unsupported atomic fatal。

### 文字伪代码

```text
处理 LDA raw：
  如果 raw.replay_inst=1：
    uvm_fatal，标记 INT_WB_REPLAY_INST_UNREACHABLE；
    不 attach key，不生成 replay event，不更新状态；
  如果 raw.source_kind=LDA 且 raw.port_id=0：
    通过真实 ROB key 查 active uid；
    读取 main_control_transaction.op_class；
    如果 op_class 是 MEMBLOCK_OP_CLASS_AMO：
      uvm_fatal，标记 INT_WB_UNSUPPORTED_ATOMIC；
      不进入普通 LOAD target 或 LQ key 检查；
  其它 LDA raw 才进入普通 LDA attach 和 normalize flow；
```

## 6. 验收标准

1. `io_mem_to_ooo_int_wb_agent` 不再读取 V2 RTL 无来源字段。
2. `dispatch_raw_int_wb_t` 中每个 valid 字段都有 V2 RTL 来源、公共状态来源或明确 default/fail-fast 规则。
3. LDA/STA 不从 raw 伪造 LQ/SQ key，必须通过真实 ROB key attach 当前 issue snapshot。
4. STD 只允许 value-only 双 flag active-map probe，不能默认 ROB flag 或 SQ key。
5. `MEMBLOCK_STD_REAL_WB_PASS_EN=1` 路径具备真实 STD writeback 闭环能力。
6. LDA `replayInst=1` 和 LDA0 AMO owner 不进入普通 LOAD pass。
7. 通过 `git diff --check -- AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_int_wb_writeback_framework_adapt_plan_20260708.md`。
