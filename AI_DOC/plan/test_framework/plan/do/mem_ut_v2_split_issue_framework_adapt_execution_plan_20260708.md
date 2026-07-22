# mem_ut V2 Split Issue 测试框架适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | 已完成 coding、文档同步、VCS 编译、load smoke 和最终独立 review；已归档 |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `memblock_issue_dispatch_base_sequence::drive_dispatch_issue_loop()`、`issue_field_assigner::assign_issue_item_fields()` |
| 适配原则 | 只适配 V2 scalar split issue 字段、pipe/mask 和 scalar-only vector 边界，不改变 scheduler 主队列模型和 replay/redirect 生命周期 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 `issueLda/issueSta/issueStd` split issue 适配时需要解决的问题。每个问题均说明修改原因、最终方案、修改的原有逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- V2 scalar split issue：`issueLda[0..2]`、`issueSta[0..1]`、`issueStd[0..1]`。
- ordinary load、software prefetch、ordinary store 的 STA/STD 双 issue。
- compile profile 提供的 pipe 数、port base、mask width 和 DUT-facing FuType width。
- V2 profile 当前全局关闭 vector issue stimulus，并在 vecissue driver 边界 fail-fast；VSTU feedback 和
  VLD writeback 的 output fail-fast 只记录跨专项验收合同。
- issue 主循环 no-progress 诊断改为基于 queue size 的 O(1) pending 判断。

本轮不支持：

- 不实现 `issueVldu` 或 vector LS 主流程。
- 不支持 MOU/AMO/CBO 的正向 issue 闭环。
- 不实现 RM、checker、scoreboard 或 coverage。
- 不改变 `issue_queue_scheduler` 的主队列模型、最老优先仲裁、redirect/replay requeue 和 pass/fail/terminal 主体算法。

执行 coding 前必须确认：

```bash
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

## 2. 问题一：pipe 数、port base 和 fired-mask 仍有固定 V3/旧结构常量

### V2 问题

当前 issue 路径中仍可能存在固定 3/2/2、`[6:0]`、STA offset `+3`、STD offset `+5` 和 `7'h7f` 等结构常量。它们在当前 V2 刚好对应 3 LDA、2 STA、2 STD，但不能作为第二套权威散落在 sequence、scheduler、driver 和 xaction 中。

### 修改原因

pipe 数、port base 和 mask width 是编译期结构参数。若一部分代码消费 compile profile，另一部分硬编码旧常量，V2/V3 或后续 profile 修改时会出现 driver fired-mask 与 sequence item 标记不一致。

### 修改方案与修改逻辑

删除 `is_valid_pipe_idx()` 这一层固定 3/2/2 的第二权威，新增唯一 helper：

```text
get_target_pipe_limit(target)
```

`check_pipe_idx()` 只调用该 helper。

port 布局统一消费 compile/width plan 提供的 localparam：

```text
LOAD 区间 = [MEMBLOCK_DUT_LOAD_PORT_BASE,
             MEMBLOCK_DUT_LOAD_PORT_BASE + MEMBLOCK_DUT_LOAD_PIPE_NUM)
STA 区间  = [MEMBLOCK_DUT_STA_PORT_BASE,
             MEMBLOCK_DUT_STA_PORT_BASE + MEMBLOCK_DUT_STA_PIPE_NUM)
STD 区间  = [MEMBLOCK_DUT_STD_PORT_BASE,
             MEMBLOCK_DUT_STD_PORT_BASE + MEMBLOCK_DUT_STD_PIPE_NUM)
mask 宽度 = MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W
总端口数 = MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM
```

`memblock_dispatch_fired_mask`、`mark_fired_items()`、driver ready bit 回填、fire report、redirect abort partial-fire 和 blocking candidate 覆盖检查全部使用同源常量；blocking 不再生成 all-ones mask。

### 文字伪代码

```text
get_target_pipe_limit(target)：
  如果 target=LOAD，返回 MEMBLOCK_DUT_LOAD_PIPE_NUM；
  如果 target=STA，返回 MEMBLOCK_DUT_STA_PIPE_NUM；
  如果 target=STD，返回 MEMBLOCK_DUT_STD_PIPE_NUM；
  其它 target 直接 uvm_fatal；

check_pipe_idx(target, pipe_idx, caller)：
  limit = get_target_pipe_limit(target)；
  如果 pipe_idx >= limit：
    uvm_fatal，指出 target、pipe_idx、limit 和 caller；
  合法时不修改 item、queue、mask 或状态；

port_idx_for_item(item)：
  如果 item.target=LOAD，port = LOAD_PORT_BASE + item.uop_index；
  如果 item.target=STA，port = STA_PORT_BASE + item.uop_index；
  如果 item.target=STD，port = STD_PORT_BASE + item.uop_index；
  如果 port >= SCALAR_ISSUE_PORT_NUM，uvm_fatal；
  返回 port；

blocking mask：
  candidate_mask = 根据本拍 selected item 的 target/base/local_pipe 构造；
  要求 driver_fired_mask 覆盖 candidate_mask；
  不再用 all-ones mask 代替真实 fire；

compile profile：
  当前 interface/xaction/driver 只显式展开 LDA/STA/STD=3/2/2；
  split style 下若任一 pipe count 超过该展开能力，在 check_compile_param_consistency() 中立即 fatal；
  小于等于展开上限时仍由 compile count 门控实际使用量。
```

## 3. 问题二：target/FuType/fuOpType 合法矩阵需要唯一检查入口

### V2 问题

V2 split issue 不是 V3 `intIssue` 的简单改名。`issueLda`、`issueSta`、`issueStd` 必须分别只接收对应 target。旧逻辑如果只看 FuType one-hot，可能把 software prefetch、CBO、AMO/MOU、vector LS 或 target mismatch 混入错误 issue port。

### 修改原因

当前测试框架的行为分类 owner 是 `lsq_ctrl_model::derive_op_behavior()`。issue 字段赋值前必须同时检查 `main_tr.fuType`、`main_tr.fuOpType`、derive 出的 behavior 和 issue item target，避免 `issue_field_assigner` 复制第二套 classifier。

### 修改方案与修改逻辑

在 `issue_field_assigner.sv` 中新增唯一纯检查 helper：

```text
check_target_futype_fuoptype(main_tr, behavior, target)
```

合法矩阵：

| FuType 与 fuOpType | behavior | 合法 target | 本轮结果 |
|---|---|---|---|
| V2 LDU 且 load fuOpType | `kind=LOAD`，只 `route_load` | LOAD | 驱动 `issueLda` |
| V2 LDU 且 prefetch fuOpType | `kind=PREFETCH`、`is_prefetch=1`、只 `route_load` | LOAD | 驱动 `issueLda` |
| V2 STU 且 store fuOpType | 同时 `route_sta/route_std` | STA | 驱动 `issueSta` |
| V2 STU 且 store fuOpType | 同时 `route_sta/route_std` | STD | 驱动 `issueStd` |
| MOU、AMO 或 `behavior.is_atomic` | unsupported | 任意 | `uvm_fatal` |
| vector LS | unsupported | 任意 | `uvm_fatal` |
| CBO 或其它错配 | unsupported | 任意 | `uvm_fatal` |

STA 和 STD 不新增目标专用 fuOpType classifier；二者共同复用 store 分类后由 target 区分。

### 文字伪代码

```text
check_target_futype_fuoptype(main_tr, behavior, target)：
  如果 main_tr 为空，uvm_fatal；
  如果 lsq_ctrl_model::is_vector_ls_futype(main_tr.fuType)，uvm_fatal；
  如果 main_tr 是 MOU/AMO 或 behavior.is_atomic，uvm_fatal；

  如果 main_tr.fuType 是 LDU：
    要求 behavior.route_load=1 且 route_sta/route_std=0；
    要求 target=LOAD；
    如果 lsq_ctrl_model::is_load_fuoptype(main_tr.fuOpType)：
      要求 behavior.kind=LOAD 且 !behavior.is_prefetch；
    否则如果 lsq_ctrl_model::is_prefetch_fuoptype(main_tr.fuOpType)：
      要求 behavior.kind=PREFETCH 且 behavior.is_prefetch=1；
    否则 uvm_fatal；

  否则如果 main_tr.fuType 是 STU：
    要求 lsq_ctrl_model::is_store_fuoptype(main_tr.fuOpType)=1；
    要求 !lsq_ctrl_model::is_cbo_fuoptype(main_tr.fuOpType)；
    要求 behavior.route_load=0 且 route_sta=1 且 route_std=1；
    要求 target=STA 或 STD；

  否则：
    uvm_fatal；

  函数只做检查，不修改 transaction、behavior、queue、map 或状态；
  所有分类调用都使用 lsq_ctrl_model:: 既有 static helper。
```

## 4. 问题三：V2 split issue 字段语义和 DUT-facing 位宽需要收敛

### V2 问题

V2 `issueLda`、`issueSta`、`issueStd` 的字段不同：

- LDA 无 `fuType`，有 FTQ ptr/offset、PC、dependency、LQ/SQ 等字段。
- STA 有 `fuType[34:0]`、完整 ROB、完整 SQ key。
- STD 有 `fuType[34:0]`、ROB value-only、完整 SQ key，无 ROB flag。

旧逻辑若复用 V3 字段或统一写 payload，会伪造不存在字段。FTQ offset 和 FuType 宽度也不能靠局部裁剪完成。

### 修改原因

内部公共 FuType 容器与 DUT-facing V2 35-bit port 是两个层次。写 STA/STD 前必须通过唯一 `encode_and_fit_dut_futype()` 检查无损编码。LDA/STA/STD 没有 `numLsElem` 字段，不能从 LSQ enqueue 语义迁移过来。

### 修改方案与修改逻辑

字段策略：

| target/port | V2 真实字段 | 固定约束 |
|---|---|---|
| LDA | 完整 ROB、LQ/SQ key、`fuOpType`、FTQ ptr/offset、PC、dependency、pdest/rfWen/fpWen | 不增加伪 `fuType`；FTQ value/offset 使用 profile 宽度 |
| STA | `fuType[34:0]`、`fuOpType`、完整 ROB、完整 SQ key、src/imm/pdest/rfWen | 不写 LQ/FTQ/dependency |
| STD | `fuType[34:0]`、`fuOpType`、ROB value-only、完整 SQ key、src | 不伪造 `robIdx_flag` |

`assign_issue_item_fields()` 保持现有 wrapper，不允许调用点绕过它直接调用三段字段 helper。

### 文字伪代码

```text
assign_issue_item_fields(tr, item, pipe_idx)：
  ensure_data()；
  如果 tr 为空，uvm_fatal；
  main_tr = data.get_main_transaction(item.uid)；
  behavior = lsq_ctrl_model::derive_op_behavior(main_tr)；
  check_target_futype_fuoptype(main_tr, behavior, item.target)；
  check_pipe_idx(item.target, pipe_idx, "assign_issue_item_fields")；
  如果当前 profile 不是 MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT，uvm_fatal；
  按原顺序调用：
    assign_main_issue_fields(tr, item, pipe_idx)；
    assign_issue_dep_fields(tr, item, pipe_idx)；
    assign_backend_meta_fields(tr, item, pipe_idx)；

compute_ftq_value(uid)：
  返回 bit[MEMBLOCK_FTQ_PTR_VALUE_W-1:0]；

compute_ftq_offset(uid)：
  返回 bit[MEMBLOCK_FTQ_OFFSET_W-1:0]；

assign_sta_main_fields / assign_std_main_fields：
  dut_futype = encode_and_fit_dut_futype(main_tr.fuType, caller)；
  写入 V2 STA/STD 对应 pipe 的 35-bit fuType；
  STD 只写 robIdx_value，不新增 robIdx_flag；
  issue 端口不写 numLsElem；
```

## 5. 问题四：scalar 模式必须关闭 vector 默认入口并在边界 fail-fast

### V2 问题

`tc_base`、`tc_dispatch_real_smoke` 和 `soft_test_tc_dispatch_smoke` 都可能为 `vecissue_agent_agent` 配置默认 sequence。即使主表侧拒绝 vector LS，默认 sequence 仍可能随机驱动 `issueVldu`。同时，VSTU feedback 和 VLD writeback monitor 边界如果只 drop，会让 unsupported vector 活动悄悄进入或绕过公共状态。

### 修改原因

本 V2 profile 明确不支持 vector LS / `issueVldu`。所有当前 V2 testcase 应在默认调度入口、driver 输入边界和 DUT
output monitor 边界同时封住 vector 活动；不能依赖后续 adapter 静默丢弃。三个边界的行为合同一致，
但源码 owner 必须唯一，不能由 split issue plan 重复修改 output monitor。

### 修改方案与修改逻辑

三层 gate 及唯一 owner：

1. 删除三处 vecissue default-sequence 配置，不设置 idle 替代 sequence，也不关闭 agent/driver；未来 vector 专项必须显式解除该 V2 profile gate。
2. `vecissue_agent_agent_driver::send_pkt()` 收到 transaction 后，只要 `issueVldu_0_valid` 或 `issueVldu_1_valid` 不是确定 0，立即 fatal；二者为 0 时只驱动 `DRV_0` idle。
3. output monitor 边界只作为跨专项依赖：
   - VSTU feedback fatal 由 IQ feedback/replay plan 在
     `io_mem_to_ooo_iq_feedback_agent_agent_monitor::mon_data()` 中唯一实现。
   - `writebackVldu` fatal 由 monitor output plan 在
     `io_mem_to_ooo_vec_wb_agent_agent_monitor::mon_data()` 中唯一实现。

本 split issue plan 只 coding 第 1、2 层，不修改上述两个 output monitor。

### 文字伪代码

```text
tc_base::build_phase()：
  保留其它 default_sequence 配置；
  删除 env.u_vecissue_agent_agent.sqr.main_phase 的 default_sequence set；
  不设置 idle 替代 sequence；

tc_dispatch_real_smoke::configure_real_smoke_default_sequences()：
  保留其它 agent generic default_sequence；
  删除 vecissue generic default_sequence；

soft_test_tc_dispatch_smoke::configure_software_smoke_default_sequences()：
  保留 LSQ、lintsissue 和其它 soft smoke 配置；
  删除 vecissue generic default_sequence；

vecissue_agent_agent_driver::send_pkt(tr)：
  如果 tr 为空，uvm_fatal；
  如果 issueVldu_0_valid !== 0 或 issueVldu_1_valid !== 0：
    uvm_fatal；
  drive_idle(DRV_0)；
  return；

以下两段是跨专项验收合同，不是本 split issue plan 的 coding 落点：

IQ feedback/replay专项的IQ feedback monitor：
  reset 完成后，如果任一 VSTU feedback valid !== 0：
    uvm_fatal；
  合法 scalar STA feedback 继续既有 raw queue 生产；

monitor output专项的vector-WB monitor：
  reset 完成后，如果任一 writebackVldu valid !== 0：
    uvm_fatal；
  不写 status、pass/fail 或 terminal；
```

## 6. 问题五：issue 主循环 no-progress 不能每拍全表扫描，也不能作为正常退出

### V2 问题

主动 issue loop 每拍调用，如果为了判断 pending work 扫描 `main_trans_num` 或 status 全表，会随着 transaction 数放大。若把 no-progress 阈值当作正常退出条件，也会把等待 writeback/commit/deq/terminal 的合法 drain 错判为完成或失败。

### 修改原因

issue queue 已经是待发工作项的 owner。高频路径只需要知道 LOAD/STA/STD queue 是否非空，应使用 queue size O(1) 判断。正常完成仍由 terminal/global-stop 合同收口；no-progress 只做 stall 诊断和错误报告。

### 修改方案与修改逻辑

新增：

```text
issue_queue_scheduler::has_pending_issue_work()
```

该函数只读：

```text
data.load_issue_q.size()
data.sta_issue_q.size()
data.std_issue_q.size()
```

不扫描主表、status、queue item 或 active map。

`drive_dispatch_issue_loop()` 保持每拍顺序：

```text
route_all_ready_uids()
send_issue_cycle(cycle_idx, has_fire)
advance_issue_queue_delays()
has_pending_issue_work()
检查 global_stop_requested
更新 no-progress idle_count
```

`route_all_ready_uids()` 的每拍有限扫描上限改为 compile owner 提供的 `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`，不再使用 runtime 物理宽度 getter。

### 文字伪代码

```text
issue_queue_scheduler::has_pending_issue_work()：
  如果 data 为空，uvm_fatal；
  如果 load_issue_q.size() != 0，返回 1；
  如果 sta_issue_q.size() != 0，返回 1；
  如果 std_issue_q.size() != 0，返回 1；
  返回 0；
  函数不读取 uid/item 内容，不写 queue/map/counter/status；

route_all_ready_uids()：
  推进 terminal 前缀并取得 active window；
  scan_limit = MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM；
  从窗口起点按 uid 递增扫描，最多处理 scan_limit 个 uid；
  对 ready uid 调用原 route_uid() 写 LOAD/STA/STD queue；
  global flush 阻塞、窗口为空或达到上限时返回；

drive_dispatch_issue_loop()：
  cycle_idx = 0；
  idle_count = 0；
  forever：
    issue_sched.route_all_ready_uids()；
    send_issue_cycle(cycle_idx, has_fire)；
    issue_sched.advance_issue_queue_delays()；
    pending_issue_work = issue_sched.has_pending_issue_work()；

    如果 data.is_global_stop_requested()：
      正常退出 loop；

    如果 has_fire：
      idle_count = 0；
    否则如果 pending_issue_work：
      idle_count++；
      如果 no_progress_warn_cycles 非 0 且命中正整数倍：
        uvm_error，报告 terminal_done_uid、main_trans_num 和三个 queue size；
      不清零、不 break、不 return；
    否则：
      queue 全空，合法等待 WB/commit/deq/terminal；
      idle_count = 0；

    cycle_idx++；
```

## 7. 问题六：send/driver fired-mask 必须只标记真实 fire

### V2 问题

若 blocking 模式继续使用固定 `7'h7f`，或 redirect abort 后按候选全部 mark fired，会让未握手 item 被错误推进到 dispatched 状态。driver report 若用 `<=2`、`<=4` 或 `-3/-5` 推导类别，也会与 profile base/limit 分裂。

### 修改原因

issue item 的状态推进必须以真实 fire 为准。ready/fire 是 driver 从 V2 split port 看到的物理握手，sequence 只能消费 driver 回填的参数化 fired-mask。

### 修改方案与修改逻辑

`send_issue_cycle()` 保持两参数签名：

```systemverilog
extern virtual task send_issue_cycle(input int unsigned cycle_idx,
                                     output bit has_fire);
```

行为：

- 创建并清零 xaction 和参数化 fired-mask。
- 按原 scheduler 顺序选择 LOAD/STA/STD candidate。
- `start_item/finish_item` 交给 driver。
- driver 根据 ready 回填 fired-mask。
- redirect abort 或 flush epoch 变化时，只 mark 已确认 fire 的 item，未 fire 保持 queue 状态。
- blocking 路径也只消费 driver 回填的真实 fired-mask；driver 结束时额外检查该 mask 覆盖全部候选，
  不能用 all-ones 掩盖 mapping/ready 缺口。

driver `clear_ready_dispatch_issue_ports()`：

- 对每个 V2 展开 LDA/STA/STD port 逐一检查 valid；ready 为 X/Z 时 fatal，只有 `ready === 1'b1`
  才算 fire。
- 清对应 valid。
- 置 `mask[PORT_BASE + local_pipe]`。
- 调 `report_dispatch_issue_fire()`。

### 文字伪代码

```text
send_issue_cycle(cycle_idx, has_fire)：
  has_fire = 0；
  创建 xaction，失败 fatal；
  tr.memblock_dispatch_fired_mask = '0；
  按原 scheduler 选择 LOAD/STA/STD candidate 并调用 assign_issue_item_fields()；
  start_item/finish_item；

  根据 fired_items 调 port_idx_for_item() 构造 candidate_mask；
  检查 driver 返回的 fired-mask 不包含 candidate 之外的 bit；
  如果是 blocking 且没有 abort/flush，要求 fired-mask 覆盖 candidate_mask，否则 fatal；
  任何模式都先用 driver 实际 fired-mask 调 mark_fired_items()；
  如果 effective_mask 非零，先完成上述 marking 并置 has_fire=1；

  如果 driver 标记 redirect abort：
    effective_mask = tr.memblock_dispatch_fired_mask 与 candidate_mask 的交集；
    返回；

  如果 finish 后 flush epoch 变化：
    已确认的 fired-mask 已经标记；只取消尚未确认 fire 的 item；
    返回；

clear_ready_dispatch_issue_ports(tr)：
  如果 tr 为空，uvm_fatal；
  对每个 LDA local_pipe：
    如果 valid 且 ready 为 X/Z，uvm_fatal；
    如果 valid && ready === 1'b1：
      清 valid；
      mask[LOAD_PORT_BASE + local_pipe] = 1；
      report_dispatch_issue_fire(port_idx)；
  对 STA 和 STD 执行同样逻辑；
  任一 port_idx >= SCALAR_ISSUE_PORT_NUM 时 uvm_fatal；

report_dispatch_issue_fire(port_idx)：
  如果 port_idx 落在 LOAD 区间，报告 LOAD/local_pipe；
  否则如果落在 STA 区间，报告 STA/local_pipe；
  否则如果落在 STD 区间，报告 STD/local_pipe；
  否则 uvm_fatal；
  函数只写日志和读取必要 debug，不修改 queue/status。
```

## 8. Coding 落点汇总

| 文件 | 对应问题与修改 |
|---|---|
| `mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv` | 问题二、三：合法矩阵、wrapper 顺序、FTQ/FuType 字段赋值 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv` | 问题五：`has_pending_issue_work()`、route scan 上限改 compile slot |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv` | 问题一、五、六：port index、drive loop、send cycle、mark fired |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv` | 问题二：复用已有 behavior 和 classifier，不新增第二套分类 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv` | 问题一、三：compile localparam、mask width、FuType encode helper 消费 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv` | 问题一：检查 split issue 显式物理字段展开上限，避免宏超出 driver/interface 能力后延迟 fatal |
| `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv` | 问题一、六：参数化 fired-mask 宽度 |
| `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv` | 问题一、六：ready 回填和 fire report 使用 base/limit |
| `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/lintsissue_agent_agent_pkg.sv` | 问题一：显式 include compile profile，保证 agent package 自包含 |
| `mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_driver.sv` | 问题四：vector valid 非零 fatal，合法零 valid 只 idle |
| `mem_ut/ver/ut/memblock/tc/src/tc_base.sv` | 问题四：删除 vecissue default sequence |
| `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv` | 问题四：删除 real smoke vecissue generic sequence |
| `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_dispatch_smoke.sv` | 问题四：删除 soft smoke vecissue generic sequence |
| `mem_ut/ver/ut/memblock/tb/lintsissue_agent_connect.sv` | 问题三：确认 V2 split issue 真实字段连接 |
| `mem_ut/ver/ut/memblock/tb/vecissue_agent_connect.sv` | 问题四：确认 vector port 保持 idle/fail-fast 边界 |

明确不修改：

```text
issue_queue_scheduler 主队列模型
跨 queue 最老优先仲裁
ready/fire mark 的生命周期 owner
redirect/replay requeue 规则
pass/fail/terminal 主体算法
RM/checker/coverage
VLD/vector LS 正向闭环
io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv中的VSTU fatal（由IQ feedback/replay专项唯一coding）
io_mem_to_ooo_vec_wb_agent_agent_monitor.sv中的writebackVldu fatal（由monitor output专项唯一coding）
```

## 9. 修改类型与原逻辑对比总结

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| pipe 数检查 | 字段/参数适配 | `is_valid_pipe_idx()` 固定 3/2/2 | 与 compile profile 形成第二权威 | `get_target_pipe_limit()` 消费 compile pipe num |
| fired-mask 宽度 | 字段/参数适配 | `[6:0]`、`7'h7f` | V2/V3 profile 可能分裂 | mask width 由 compile localparam；运行期只消费 driver 实际 mask，不再生成固定 full mask |
| port base | 字段/参数适配 | STA `+3`、STD `+5` | base 应由 profile owner 统一 | LOAD/STA/STD base+local_pipe 计算 |
| 合法矩阵 | 功能逻辑修改 | 可能只按 FuType 判断 | target/fuOpType/behavior 必须一致 | `check_target_futype_fuoptype()` 唯一检查 |
| software prefetch | 功能逻辑保持/收敛 | 收紧 scalar 时可能误禁用 | V2 LoadUnit 支持 prefetch 走 LDA | LDU+prefetch 合法 target=LOAD |
| AMO/MOU/CBO/vector | unsupported gate | 可能晚报错或错 route | 当前 V2 主 flow 未闭环 | 字段赋值前 fatal |
| LDA/STA/STD 字段 | 字段适配 | 可能复用不存在字段 | V2 split port 字段不同 | LDA 无 FuType，STA 完整 ROB/SQ，STD ROB value-only |
| FuType DUT-facing | 字段适配 | 局部裁剪风险 | 内部容器和 V2 DUT 宽度不同 | STA/STD 写前调用 `encode_and_fit_dut_futype()` |
| vecissue 默认入口 | 配置逻辑修改 | 三处可能启动默认 vector sequence | V2 profile 全局不支持 issueVldu | 删除三处 default sequence，不设置 idle 替代；未来专项须显式解除 gate |
| vecissue driver | unsupported gate，本 plan 唯一 coding | transaction 可能继续驱动 | 防止 vector issue 进入 DUT | valid 非 0 fatal，确定为 0 时只 idle |
| VSTU/vector-WB monitor | 跨专项依赖，本 plan 无源码修改 | 可能 drop 或继续 | 防止 vector output 进入公共状态 | VSTU fatal由IQ专项唯一coding；`writebackVldu` fatal由monitor output专项唯一coding |
| pending issue 判断 | 性能逻辑修改 | 可能每拍全表判断 | 高频路径必须 O(1) | 只读三个 queue size |
| no-progress 退出 | 运行期逻辑修改 | 阈值可能混入正常退出 | 正常完成应由 terminal/global-stop | pending 且无 fire 才报错，queue 空合法 drain |
| route scan 上限 | 性能/参数适配 | runtime 物理宽度 getter | 硬件结构是 compile 参数 | 每拍有限扫描上限用 `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM` |
| send/mark fired | 功能逻辑修改 | blocking 可用 full mask，epoch race 可能丢真实 mask | 任何模式都只能标记真实 fire，flush 只取消未 fire | 消费 driver 实际 mask；blocking 检查候选完整覆盖；epoch/redirect 后先保留已确认 fire |

## 执行中补充/修正（IMPLEMENTATION_DELTA）

以下内容是在 coding 和独立 review 中根据当前源码/验证结果补充的实现约束，原始问题边界不变：

| 标记 | 原 plan | 实现调整 | 原因与影响 |
|---|---|---|---|
| `[IMPLEMENTATION_DELTA] package include` | 只要求各 consumer 消费 compile-time 宏 | `lintsissue_agent_agent_pkg.sv` 在 interface/xaction/driver 前显式 include `memblock_compile_params.svh` | `tb.f` 未启用 `-mfcu`，不能依赖其他 compilation unit 的宏副作用；不改变运行期行为。 |
| `[IMPLEMENTATION_DELTA] launch epoch gate` | 重点描述 ready 等待期间的 flush | driver 首次 send 前也检查 `dispatch_flush_in_progress/flush_epoch`；失效 transaction 只驱动 idle 并置 abort | 防止 sequence 生成 item 后、首次上 VIF 前发生 redirect 时仍 launch 旧请求。 |
| `[IMPLEMENTATION_DELTA] confirmed fire 保留` | epoch 变化分支容易直接跳过所有 marking | sequence 先构造 candidate mask、消费 driver 实际 fired-mask，再处理 abort/epoch；只取消未 fire item | 同拍或 finish 后发生 redirect 时，已确认 `valid&&ready` 的 item 不会被重复 issue。 |
| `[IMPLEMENTATION_DELTA] blocking mask 校验` | 原方案允许 blocking 使用参数化 all-ones | blocking 正常返回要求实际 fired-mask 覆盖全部候选，缺 bit 直接 fatal | 保留原 blocking 等待语义，同时不让 all-ones 掩盖 driver/port mapping 缺口。 |
| `[IMPLEMENTATION_DELTA] ready X/Z` | 只描述 valid/ready 二值握手 | valid=1 且 ready 为 X/Z 时 fatal；只有 `ready === 1'b1` 才记 fire | 将未知 ready 变成明确协议错误，避免静默等待到 timeout。 |
| `[IMPLEMENTATION_DELTA] no-progress 单位` | 文案称周期 | 日志改称 issue-loop iteration；queue size 的 O(1) 判断和退出语义不变 | blocking driver 一次调用可能跨多个 DUT clock，避免诊断数字被误解为物理周期。 |
| `[IMPLEMENTATION_DELTA] stale item handle` | driver 循环依赖 `try_next_item()` 返回空句柄 | 每轮调用前显式 `req = null`，再调用 `try_next_item(req)` | 不依赖 UVM 实现对 output 参数的隐式清理，禁止无 item 时复用上一轮句柄；不改变 item 生命周期。 |
| `[IMPLEMENTATION_DELTA] split expansion guard` | 只由 pipe count 派生逻辑 port/mask | `check_compile_param_consistency()` 在 split style 下拒绝超过显式 LDA/STA/STD=3/2/2 的 profile | 当前 interface/xaction/driver 只展开这些物理字段；将结构不一致提前变成 compile/runtime 初始化错误，而不是发射时才 fatal。 |

本节补充均属于 split issue driver/sequence 的边界防护；没有新增 runtime plus，也没有改变
issue queue、replay/redirect、writeback/commit/deq 或 pass/fail/terminal 主体算法。

保持不变的主体逻辑：issue queue 数据结构、candidate 选择、最老优先、queue delay、redirect/replay requeue、writeback/commit/deq/terminal 收口和公共状态 owner。
