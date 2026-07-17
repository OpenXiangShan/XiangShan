# mem_ut V2 测试框架适配总控最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待各专项 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv`、`build_memblock/rtl/filelist.f` |
| Plan 类型 | V2 测试框架运行期适配总控，不替代专项 execution plan |
| 适配原则 | 只记录 V2 适配的关键问题、专项 owner、修改逻辑边界和文字伪代码；不保留历史讨论和长 checklist |
| 创建/修订日期 | 2026-07-17 |

## 1. 范围与边界

本文是 `mem_ut_uvm_v2` 分支的 V2 测试框架适配总控 plan。它不替代 DUT interface 字段适配 plan，也
不重复每个专项的完整函数合同。本文只回答：

- V2 适配有哪些运行期问题。
- 每类问题为什么必须修改。
- 修改方案改变了哪些测试框架逻辑。
- coding 时应落到哪个专项 owner。

属于本文总控范围：

- 版本 profile、compile 参数、V2/V3 宽度和 capability 的单一权威。
- 主表生成、split issue、LSQ enqueue、int-WB、IQ feedback/replay、ROB/LSQ commit/deq。
- L2TLB responder、CSR/sfence、DCache L2 sideband、monitor output、MMIO/status sideband。
- V2-only output 的职责分类和不支持功能的 fail-fast 边界。

不属于本文实现范围：

- 机械 DUT/interface/connect 字段逐项修复；这些由 DUT/interface 适配专项负责。
- RM、scoreboard、checker、coverage。
- 任何未被专项授权的 flow 文档、analysis 文档、rule/profile 文档同步。
- V3 运行期功能补齐。

所有 V2 专项执行前必须先确认：

```text
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

`build/rtl` 只能辅助比对，`MemBlockTop.sv` 不是当前 V2 profile 的权威输入。

## 2. 专项 owner 总览

| 适配域 | 唯一 coding owner |
|---|---|
| compile 参数、宽度、FuType、ROB/LQ/SQ key | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_compile_param_and_width_adapt_execution_plan_20260708.md`，已归档完成；后续 LSQ delta 由 LSQ enqueue 最终 plan 维护 |
| 自动主表 VADDR 窗口 | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_main_table_vaddr_generation_adapt_execution_plan_20260713.md` |
| DCache L2 sideband known-zero | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_dcache_l2_sideband_responder_adapt_execution_plan_20260712.md` |
| LSQ enqueue | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_lsq_enqueue_framework_adapt_final_plan_20260714.md`，coding、文档同步、冻结验证和最终独立review均已完成；真实load已闭环，store admission已覆盖，store终态仍由后续SQ deq专项闭环 |
| split issue | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_split_issue_framework_adapt_execution_plan_20260708.md` |
| IQ feedback/replay | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_iq_feedback_replay_framework_adapt_execution_plan_20260711.md` |
| int-WB/writeback | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_int_wb_writeback_framework_adapt_execution_plan_20260708.md` |
| CSR/sfence/runtime snapshot | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_csr_control_runtime_semantic_review_execution_plan_20260708.md` |
| L2TLB response/permission/ready gate | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md` |
| LSQ MMIO/status | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md` |
| pending-MMIO load/store sideband | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md` |
| monitor output 分类 | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md` |

本文中每个问题只给出总控级最终方案。coding 时以对应专项 owner 的文件清单和函数合同为准，不得从
本文自行扩展修改范围。

## 3. 问题一：V2/V3 编译期结构仍可能存在第二权威

### V2 问题

V2 和 V3 的 ROB value width、FuType width、LSQ enqueue slot、split issue port、SQ deq pointer
presence 等是编译期结构。旧逻辑中仍可能存在固定 V3 literal、runtime plus 镜像或同义参数，导致
V2 worktree 编译通过但运行期按错误结构截断、驱动或解析。

### 修改原因

interface 数组维度、端口 presence 和 key/FuType 位宽在 elaboration 前固定，不能由 runtime plus
改变。测试框架高频路径只能消费一个 compile profile 权威，否则 candidate、driver、monitor 和
状态表会产生不同宽度语义。

### 修改方案与修改逻辑

compile/width 基线已完成。后续专项必须继续遵守：

- `tb.f -> memblock_compile_params.svh -> memblock_dispatch_types.sv ->
  seq_csr_common::check_compile_param_consistency()` 是结构参数主链。
- 业务 helper 直接使用 package localparam 或 compile macro，不新增 `get_compile_*()` 第二入口。
- runtime plus 只限制行为使用量，不改变物理 slot、pipe、port、key width 或 presence。
- AMO/MOU、CBO、vector LS 本轮没有 scalar capability；默认权重为 0，显式非 0 或 manual/fixed 生成
  在主表落表/admission 前 fail-fast。
- 最终 LSQ enqueue plan 已复用 V2 compile baseline，补齐 LSQ 派生宏、无 response 的 clock-first
  streaming、load/store 6/4 gate和pending-sample时序；未增加新的profile selector或固定retry guard。

### 文字伪代码

```text
编译当前 V2 worktree：
  tb.f 只选择 V2 profile；
  memblock_compile_params.svh 定义 V2 ROB/LQ/SQ/FuType/slot/port/presence tuple；
  如果缺宏、宏冲突或 tuple 与 V2 profile 不一致，编译或初始化 fatal；

运行 seq_csr_common::validate_and_clamp()：
  先调用 check_compile_param_consistency()；
  check helper 只检查 compile tuple，不修改 runtime 字段；
  validate_and_clamp() 再把 runtime enqueue/pipe/资源使用量 clamp 到 compile 上限；
  如果用户显式配置本轮不支持的 AMO/MOU/CBO/vector 运行期权重，fatal；

构造 directed ROB value：
  manual builder 和 software smoke 都调用 seq_pkg::fit_directed_rob_value_or_fatal()；
  helper 比较未截断输入是否落入当前 ROB value width；
  合法后才做 sized cast；
  禁止固定 slice、隐式截断或 class-local wrapper。
```

## 4. 问题二：自动主表虚拟地址窗口与物理映射窗口耦合

### V2 问题

旧 `apply_legal_addr_template()` 使用 `MEMBLOCK_PADDR_BASE/RANGE` 生成 issue 虚拟地址，TLB builder 又用
同一组参数选择 PADDR。translated flow 中，VA 在翻译前被物理窗口重复限制。

### 修改原因

主表 virtual address 和 TLB physical address 是两个不同语义。把它们绑定在同一组参数上，会让地址
激励空间受错误窗口限制，也让后续调试难以判断异常来自 VA 生成还是 PADDR 映射。

### 修改方案与修改逻辑

主表 VADDR 专项新增 `MEMBLOCK_MAIN_VADDR_BASE/RANGE`，完整接入
`plus.sv -> seq_csr_common -> getter -> default.cfg`。`apply_legal_addr_template()` 只读取 MAIN_VADDR
getter；`tlb_map_builder::choose_paddr()` 继续唯一消费 `MEMBLOCK_PADDR_BASE/RANGE`。

### 文字伪代码

```text
初始化 runtime 参数：
  读取 MAIN_VADDR_BASE/RANGE；
  检查 range 非 0；
  检查 base+range 不溢出；
  检查窗口处于 Sv39 positive-canonical 子空间；
  任一非法配置 fatal，不 fallback 到 PADDR 窗口。

apply_legal_addr_template(main_tr)：
  根据 op_class/fuOpType 计算访问 size 和对齐要求；
  在 MAIN_VADDR 窗口内选择 64B 对齐候选槽；
  要求完整访问跨度落在 MAIN_VADDR 窗口内；
  找不到合法槽时 fatal；
  写 main_tr.src_0/imm/vaddr；
  不读取 PADDR getter。

tlb_map_builder::choose_paddr()：
  继续使用 PADDR_BASE/RANGE；
  映射算法和 entry 生命周期不改。
```

## 5. 问题三：V2 split issue 与 vector LS 边界不清

### V2 问题

V2 使用 `issueLda/issueSta/issueStd/issueVldu` split port。旧框架容易把 V3 聚合 issue 语义、固定
fired-mask 或 vector 默认 sequence 沿用到 V2，导致不支持的 vector LS 被当成 scalar 路径处理。

### 修改原因

测试框架若声称模拟 DUT issue flow，就不能生成 V2 Scala 不会产生的 target/FuType/fuOpType/port
组合。vector LS 主流程本轮不支持，必须 fail-fast 或禁止生成，不能静默 drop。

### 修改方案与修改逻辑

split issue 专项负责：

- 建立 `MEMBLOCK_ISSUE_TARGET_LOAD/STA/STD` 到 V2 `issueLda/Sta/Std` 的权威映射。
- V2 FuType 使用 bit15/16/17 表示 scalar LDU/STU/MOU；禁止把 V3 36-bit literal 低位截断。
- fired-mask width、LOAD/STA/STD base offset、full-mask、driver ready 映射全部从 compile port count 派生。
- 删除 scalar testcase 中随机/generic vecissue default sequence 配置。
- vecissue transaction、VSTU feedback、`writebackVldu` 任一 valid 在本轮 scalar flow 中 fatal。
- issue loop 只有 driver fired-mask 确认的真实 fire 才计 progress；queue blocked、delay、route 尝试不计。

### 文字伪代码

```text
构造 issue item：
  读取 main_tr 的 op_class、fuType、fuOpType、lsq_flow；
  如果是 scalar LOAD：
    target=LOAD；
    route 到 issueLda 可用 port；
  如果是 scalar STA：
    target=STA；
    route 到 issueSta 可用 port；
  如果是 scalar STD：
    target=STD；
    route 到 issueStd 可用 port；
  如果是 vector LS、MOU/AMO、CBO 或本轮不支持组合：
    在主表 validate 或 issue 入口 fatal；

生成 fired-mask：
  compile localparam 给出 LOAD/STA/STD port count 和 base offset；
  根据实际 port 置位；
  禁止固定 +3/+5 或 7'h7f；

drive_dispatch_issue_loop()：
  每轮尝试发射；
  只有 driver 返回真实 fired-mask 且至少一个 item fire，has_progress=1；
  queue 空且 terminal 前缀完成可正常 drain；
  长时间无真实 fire 在阈值整数倍报 uvm_error，不清计数、不 break；
  永久停滞由 UVM timeout 暴露。
```

## 6. 问题四：LSQ enqueue V2 request、capacity gate 和时序不同于 V3

### V2 问题

V2 LSQ enqueue 使用 6 个物理 slot、load/store 单拍上限 6/4，并带 `exceptionVec/trigger/fuOpType/
flushPipe/lastUop` 等 request 字段。V2 顶层没有 LSQ enqueue `canAccept/response`，旧 flow 若等待
ready/response 会没有完成条件。

### 修改原因

V2 6/4 表示单拍 load/store element 端口能力，不是软件模型必须长期保留的 LQ/SQ 空项数。V2 request
从 driver clocking 边界 launch 后，到下一边界才有 DUT sample 机会；软件 allocation 必须在 launch 后
立即预留，`issue_ready` 则必须延后到下一边界，才能同时保持 pointer 连续和每拍一批的 streaming 吞吐。

### 修改方案与修改逻辑

唯一 owner 是 LSQ enqueue 最终 plan。总控只保留以下边界：

- V2 scalar LDU/STU request 固定 `uopIdx=0`、`lastUop=1`、`numLsElem=1`。
- request setter 从 `main_tr + behavior + predicted key` 一次构造完整 slot payload。
- candidate 保持连续 UID 前缀，只在局部预览 pointer/free count，不修改公共状态。
- V2 capacity gate 使用 `tentative load <= 6`、`tentative store <= 4`，并分别不超过实际 LQ/SQ free count；
  不要求额外 reserve 6/4，也不先要求 base free 始终达到 6/4。
- V2 driver 使用 clock-first streaming，不调用 `wait_lsq_can_accept()` 或 response sample；每个边界先让 DUT
  采样上一批，再 launch 当前批并立即 `item_done()`。
- launch 后立即调用唯一 `commit_allocate()` 预留资源；上一批在下一 driver边界通过
  `complete_v2_pending_sample()` 开放 issue route。
- collect、driver launch 和 confirm 分别复用现有 global flush/epoch gate；不增加固定5-cycle retry guard。
- 随机 enqueue 数量支持 ZERO/MIDDLE/MAX 三类权重；返回0时只发送idle，不消费next uid或修改LSQ资源。

### 文字伪代码

```text
collect_lsq_candidates()：
  如果 global flush gate 有效，返回空；
  每拍调用一次get_enq_per_cycle取得runtime目标；
  如果目标为0，在读取uid/pointer/free count前返回空，并由上层发送全零idle；
  保存当前LQ/SQ pointer和free count到局部变量；
  复制 LQ/SQ enqueue pointer 到局部变量；
  load_elem_count=0，store_elem_count=0；
  从 next-admit uid 开始预览连续前缀：
    遇非 LSQ、已有状态、unsupported op 或 slot 上限时停止；
    derive_op_behavior() 得到scalar load/store element 数，本轮要求num_ls_elem=1；
    tentative 计数超过 6/4 时停止；
    tentative 计数超过对应实际free count时停止；
    保存 uid、tr、behavior、预测 key 到等长 queue；
    只推进局部 pointer 和局部 element count；
  返回 queue 是否非空。

send_lsqenq_cycle()：
  先处理 pending cancel；
  上一批pending且下一uid是non-LSQ时，先发送idle边界完成上一批sample；
  先尝试 non-LSQ admission；
  收集 LSQ candidates；
  无 candidate 时发送一个全零 idle item；
  有 candidate 时：
    clear xaction；
    对每个 candidate 调用唯一 setter 构造完整 V2 request；
    start_item/finish_item 交给clock-first driver；
    finish_item返回后先complete上一批pending sample；
    当前批未abort且epoch未变时，preview key重新核对后调用唯一commit_allocate()预留资源；
    当前批保存为pending sample，下一driver边界才设置issue_ready；
  V2 不等待 canAccept/response。
```

## 7. 问题五：int-WB 和 IQ feedback 缺少 V2 raw 保真与 current snapshot

### V2 问题

V2 `writebackLda/Sta/Std` 和 `staIqFeedback` 都是 split output。旧 raw 可能携带 V2 不存在的
ROB/LQ/SQ key，或在 replay 后缺少 `issue_epoch/replay_seq`，导致 normalize drop gen1 事件。

### 修改原因

monitor raw 必须先保真表达 DUT 端口；adapter 再通过 active map 和 current status 补 canonical key
与 generation。高频 event 路径不能全表扫描，也不能用历史 token 猜测。

### 修改方案与修改逻辑

int-WB 专项负责 split WB raw 和 STD value-only 反查；IQ feedback/replay 专项负责 STA IQ SQ-only raw、
current snapshot attach、STA 单向阶段状态和按 cycle timeline 处理。

总控固定以下组合：

- STA IQ raw 只保留真实 SQ。
- LDA/STA raw 只保留真实 ROB。
- STD raw 使用 int-WB 专项的 ROB value-only 双 flag probe，不进入 STA current snapshot helper。
- LDA/STA/IQ 进入 normalize 前必须已经附加 UID、canonical key、`issue_epoch/replay_seq`。
- current snapshot 来源是现有 status，不新增 generation token/tombstone。
- raw queue 积压时按 `raw.cycle` 分组，同 cycle IQ 先于 WB，当前 cycle ctrl apply/recovery 后才处理下一 cycle。

### 文字伪代码

```text
monitor raw：
  STA IQ valid -> SQ-only raw；
  LDA/STA WB valid -> ROB-only raw；
  STD WB valid -> value-only raw，交给 int-WB 专项固定双flag probe；

adapter convert：
  对 STA IQ/LDA/STA partial event 调用 attach_current_issue_snapshot()；
  helper 用真实 SQ 或 ROB key 做 O(1) active map 查询；
  读取 current status；
  核对 active、target dispatched、未 kill/redirect/flush/replay；
  核对 required canonical LQ/SQ owner；
  写 UID、canonical key、issue_epoch、replay_seq；
  attach 失败固定 fatal；

normalize：
  如果 event 是 STA IQ、LDA WB 或 STA WB：
    要求 has_uid/has_issue_epoch/has_replay_seq 完整；
    缺失 fatal，不从 status fallback；

batch timeline：
  冻结本次 service 的 IQ/WB/ctrl raw；
  循环按三个 queue 头部最小 cycle 分组；
  同 cycle 转换顺序为 IQ -> WB -> memoryViolation；
  redirect-first 后 allowed event 才进入 stage check/handler/commit；
  本 cycle full ctrl raw apply 后调用 replay/redirect recovery；
  再处理下一 cycle。
```

## 8. 问题六：ROB/LSQ commit、MMIO status 和 SQ deq pointer 语义混杂

### V2 问题

V2 只有 `sqDeq` count，没有 `sqDeqPtr`。同时 `pendingPtr/pendingst/pendingMMIOld/scommit` 由
lsqcommit driver 驱动，语义上依赖当前 ROB head。旧逻辑容易把 fault 当 normal commit、把 batch tail
当 pending head、或把 ctrl raw deq 过早应用删除同批 event 的 active map。

### 修改原因

V2 LSQ/ROB sideband 既影响 DUT 输入，也影响测试框架 terminal 收敛。必须分清 normal commit、
fault convergence、真实 LSQ deq 和 output tag producer，否则会掩盖 deadlock 或伪造 progress。

### 修改方案与修改逻辑

LSQ MMIO/status 专项负责：

- 独立 modeled ROB head 驱动 `pendingPtr/pendingst/pendingMMIOld`。
- normal batch 和 fault convergence 互斥。
- fault token 只置框架 `rob_commit`，真实 LSQ mapping 释放并形成 fault terminal 后才 rebase head。
- `apply_raw_ctrl_deq(raw)` 是 LQ/SQ/SB full-raw 唯一 owner。
- V2 SQ deq 使用 count-only 分支，全部预检查通过后才 release。
- semantic event claim/handler 完成后才 apply deferred ctrl raw。
- active driver no-item/gap 保持 level sideband，只清 `scommit/flushSb` pulse。
- progress 只由 normal commit、fault token、fault terminal rebase、真实 deq event 和 flushSb 边沿组成。

### 文字伪代码

```text
build_lsqcommit_xaction()：
  第一步 sync_modeled_head_after_fault_terminal()；
  clear xaction，并用 modeled head 填 pendingPtr；
  如果 fault waiting：
    pendingst/pendingMMIOld/scommit=0；
    返回空 normal/fault candidate；
  解析 sideband head uid；
  选择 normal-only batch；
  如果 normal batch 非空：
    用 head behavior 派生 pendingst/pendingMMIOld；
    scommit=normal batch 中 scalar store 数；
    返回 normal batch；
  否则选择 fault head candidate；
  如果 fault 命中：
    只返回 fault uid，sideband 只保留 pendingPtr；
  否则：
    无 commit 时仍按当前 head 派生 pendingst/pendingMMIOld；

apply_raw_ctrl_deq(raw)：
  先 update_sb_is_empty；
  检查 sq_deq_ptr_valid 与 profile capability；
  LQ nonzero 走 pointer helper；
  SQ nonzero 且 profile 有 pointer 时走 pointer helper；
  SQ nonzero 且 V2 无 pointer 时走 count-only helper；
  count-only helper 先预检查连续 SQ head owner 和 rob_commit；
  全部通过后才 release pointer、删除 map、try_retire，并递增 sq_deq_event_seq；

collect_monitor_event_batch()：
  collect writeback/IQ semantic events；
  collect ctrl semantic events，并把完整 raw 存入 deferred queue；
  process semantic batch；
  apply_deferred_ctrl_updates_batch(deferred queue)；
  task 返回后再执行既有 redirect/replay apply。
```

## 9. 问题七：L2TLB response permission 和 request ready 不能只做字段连接

### V2 问题

V2 有 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 等 response permission 字段。仅把字段机械接到
interface 不足以保证 active takeover 路径完整。同时 L2TLB request ready 若没有 single-outstanding
gate，responder 可能接受多笔 request 或在 sequence disabled 时握手无人消费。

### 修改原因

L2TLB agent 在 memblock 中代替 DTLB -> L2TLB request / L2TLB -> DTLB response responder。request
ready 是运行期 backpressure，不是 plus/cfg，也不是 sfence freeze 或 ack。

### 修改方案与修改逻辑

L2TLB 专项负责：

- `s1/s2 entry_perm_g/u` 字段从 entry -> xaction -> interface -> driver -> connect -> RTL internal wire
  全链审计；当前 s1/s2 同源 `pte_g/pte_u` 只是字段链完整，独立 PTE 来源是后续 TODO。
- `memblock_sync_pkg::l2tlb_request_accept_enable` 是唯一 ready gate，初值 0。
- driver reset 第一项功能动作清 gate，早于 `super.reset_phase()` 和任何 idle drive。
- driver 所有 ready 赋值都受 `l2tlb_responder_active && l2tlb_request_accept_enable` 约束。
- responder disabled 时 gate=0；context、takeover 和 PTW tracking start 全部成功后才开 gate。
- request fire 定义为采样沿 `valid && ready`；fire 后立即 outstanding 0->1、清 gate、删除独立
  ready item。
- response 完成后 outstanding 1->0；非 stopping 路径才重开 gate。

### 文字伪代码

```text
driver reset_phase()：
  l2tlb_request_accept_enable=0；
  调用 super.reset_phase()；
  raise objection；
  执行原 reset wait/idle；
  drop objection。

driver ready 计算：
  requested_ready 来自 item 或 idle mode；
  effective_ready = l2tlb_responder_active && l2tlb_request_accept_enable && requested_ready；
  如果当前拍 request_fire=valid && effective_ready：
    下一拍 ready 强制安排为 0；
  任何 drv mode 都不能绕过 gate。

responder body()：
  如果 cfg disabled：
    gate=0；
    退出；
  ensure_context()；
  检查 takeover；
  start_l2tlb_ptw_tracking()，要求 outstanding count 为 0；
  gate=1；
  进入 drive_l2tlb_loop()。

send_l2tlb_cycle()：
  如果 request_fire：
    先锁存完整 request payload；
    mark_l2tlb_ptw_request_begin()，count 0->1；
    gate=0；
    构造 response item，pre_pkt_gap=choose_latency()+1，post_pkt_gap=1；
    发送 response；
    mark_l2tlb_ptw_request_done()，count 1->0；
    如果非 stopping，gate=1；

idle-stop：
  达到 idle stop 时先 stopping=1 且 gate=0；
  过渡拍若仍发生 final fire，必须完成 response 但不得 rearm；
  只有 ready=0 且 outstanding count=0 时退出。
```

## 10. 问题八：CSR/sfence、debug 和 DCache L2 sideband 不能按普通随机输入处理

### V2 问题

V2 CSR control 命名与 V3 不同，包含 misalign、priv debug、branch predictor enable 等字段。
DCache 的 `io_l2_hint_*` 和 `io_l2_flush_done` 是 DUT input，机械接入后若沿用 generic idle/random
mode，可能触发未建模 MSHR hint 或 CSR flush done 行为。

### 修改原因

这些字段不是普通可随机输入。当前主 flow 没有合法非零 DCache L2 sideband producer；sfence
`flushPipe=1` 必须在 standalone 条件下唯一驱动，不能与主 dispatch/LSQ/L2TLB 业务并发。

### 修改方案与修改逻辑

CSR 专项负责：

- `hd_misalign_ld/st_enable` 和 `tlbCsr_priv_debug` 采样到 raw CSR/runtime snapshot，但本轮不进入
  sequence、pass/fail、terminal 或 L2TLB lookup key。
- sfence quiescent provider 由 `memblock_env::connect_phase()` 无条件注册，single-owner。
- `flushPipe=1` 只允许 `tc=basicTest ts=memblock_sfence_flushpipe_directed_vseq` standalone 运行；
  vseq 在唯一 child 前检查 main/dispatch/queue/flushSb/redirect/gate/outstanding 全空。

DCache L2 sideband 专项负责：

- interface 四字段 time-zero 初始化为 0。
- xaction constraint、`new()`、idle builder、response builder 都显式写 0。
- `drive_idle()` 任意 mode 后都无条件驱这四字段为 0。
- `send_pkt()` 首个 vif 赋值前检查四字段，任一非 0 或 X 以 `DCACHE_L2_SIDEBAND_UNSUPPORTED` fatal。

### 文字伪代码

```text
CSR monitor：
  采样 misalign 和 priv_debug；
  写入 raw CSR/runtime snapshot；
  branch predictor enable 只做观察，不进入 TLB lookup 或 pass/fail；

sfence standalone vseq：
  build_phase 解析 VSEQ_MAIN；
  只有精确命中 directed vseq 时设置 standalone mode；
  env build 在创建子组件前校验所有 agent default sequence 和 drv mode；
  vseq body 在唯一 uvm_do_on 前检查：
    main/dispatch 状态为空；
    active map 和 raw queue 为空；
    flushSb/redirect/flush/freeze 为空；
    L2TLB gate=0，outstanding=0；
  child item 固定 valid=1、flushPipe=1、pre_pkt_gap=0、post_pkt_gap=1；

DCache driver：
  drive_idle(mode)：
    保留原 TL A/B/C/D/E idle 逻辑；
    分支结束后无条件驱 l2_hint_valid/sourceId/isKeyword 和 l2_flush_done 为 0；
  send_pkt(tr)：
    在任何 vif 赋值前检查四个 sideband；
    任一非零或 X，fatal 且不产生部分驱动；
    合法时保留原 TL payload 赋值，并把四个 sideband 明确驱 0。
```

## 11. 问题九：monitor analysis port 与 V2-only output 容易被误当 runtime raw

### V2 问题

当前多个 agent monitor 有 env analysis FIFO 和 RM blocking-get consumer，但 monitor producer
普遍未执行 `mon_item_port.write()`。同时 V2-only output 如 L2 TLB/PMP response、outer prefetch
control、WFI safe 等未分类，容易被误接到 runtime raw 或内部 L2TLB agent。

### 修改原因

raw queue 足够支撑 dispatch 主 flow，不等价于 RM transaction 闭环。恢复 analysis producer 必须与
RM/checker/scoreboard 成对设计，不能在 V2 interface 适配中批量打开。

### 修改方案与修改逻辑

monitor output 专项负责：

- 20 个 monitor 同时给出 runtime 角色和 RM analysis 角色。
- 当前 analysis producer 统一记录为 `ANALYSIS_PORT_DEFERRED`，本轮不批量恢复。
- 只有 CSR、sfence、ctrl、int-WB、IQ-feedback 等 raw producer 进入公共状态。
- `io_l2_tlb_req_resp_*` 和 `io_l2_pmp_resp_*` 不接内部 `L2TLB_agent`。
- V2-only output 固定七组；`externalInterrupt_debug` 归属 ctrl agent 字段级
  `OUTPUT_OBSERVATION_XZ`，不进入 raw、CSR snapshot、status、pass/fail、terminal、redirect 或 replay。
- `io_outer_cpu_halt` 只作为 halt/status output，不再称为 `cpuWfi`；`reset_backend_done` 仍是 testbench
  同步标志，不等同 DUT `io_reset_backend` output。

### 文字伪代码

```text
分类一个 monitor 字段：
  如果字段已进入 CSR/sfence/ctrl/int-WB/IQ raw producer：
    标记为 runtime raw owner，并说明写入 queue 和 downstream consumer；
  否则如果字段只需要 X/Z 或 debug 观察：
    标记为 OUTPUT_OBSERVATION_XZ 或 DEBUG_ONLY；
  否则如果后续 RM/checker 需要 standard transaction：
    标记为 ANALYSIS_PORT_DEFERRED；
    不在本轮调用 mon_item_port.write()；
  如果字段是外部 L2TLB/PMP response：
    不接内部 L2TLB agent；
  如果字段是 externalInterrupt_debug：
    只补 interface/xaction/connect/monitor/XZ 同名字段链；
    不写 raw/status/terminal。
```

## 12. 修改顺序

V2 专项之间存在硬依赖，coding 应按以下顺序执行：

1. 确认 V2 RTL 权威路径存在。
2. 以已归档 compile/width 基线为前提，执行仍需追加 profile tuple 的专项 delta。
3. 执行主表 VADDR 和不支持 op fail-fast，确保后续 flow 不生成本轮不支持激励。
4. 执行 split issue 和 LSQ enqueue，建立正确激励入口。
5. 执行 int-WB 与 IQ feedback/replay，建立 monitor raw 到 current status 的事件链。
6. 执行 pending-MMIO producer/query 与 LSQ MMIO/status，同一原子批次完成 ctrl raw producer 和 consumer。
7. 执行 L2TLB、CSR/sfence、DCache sideband 和 monitor output 专项。
8. 每个专项完成后按各自 plan 运行静态检查、远端 compile/smoke，并生成对应 implementation review。

### 文字伪代码

```text
执行一个 V2 专项：
  先确认当前分支是 mem_ut_uvm_v2；
  读取该专项 plan 和它声明的硬前置；
  如果专项依赖的 macro/raw field/helper 缺失：
    停止该专项 coding，不写 fallback；
  只修改专项 owner 列出的源码、cfg 和文档；
  高频路径使用 cursor、map、queue、bounded snapshot；
  禁止每拍或每 event 全表扫描；
  完成后运行专项 plan 指定静态检查；
  需要远端验证时从 mem_ut/ver/ut/memblock/sim 使用 eda_* 目标；
  生成 implementation review；
  不自动 push。
```

## 13. 验证与 smoke 边界

总体验证入口保持：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

专项还必须运行各自 plan 定义的 directed smoke，例如：

- split issue：真实 fired-mask、no-progress error 和 vector fail-fast。
- LSQ enqueue：V2 clock-first streaming、launch reservation/下一边界 sample、6/4 capacity gate、随机idle和redirect epoch路径。
- IQ feedback/replay：STA IQ/WB 正向和独立 expected-fatal。
- LSQ MMIO/status：normal pendingst/scommit、fault-at-tail、V2 SQ count-only、driver active idle hold。
- L2TLB：single-outstanding request fire、response latency、idle-stop。
- DCache L2 sideband：任意非零 sideband 在首个 vif 赋值前 fatal。

本总控不新增 RM/checker/coverage 验证要求。coverage/checker/RM 后续只消费专项留下的字段、事件或标签。

## 14. RM 协同支持

本 plan 不实现 RM、checker 或 scoreboard。

后续 RM/checker 可使用：

- current snapshot 后的 UID、ROB/LQ/SQ canonical key、`issue_epoch/replay_seq`。
- LSQ commit/deq 后的 terminal 状态和 deq event sequence。
- pending-MMIO 专项落表的 MMIO tag。
- monitor output 专项恢复的 standard transaction producer。

这些字段只能作为后续组件输入；本 plan 不定义 DUT 正确性比较算法。

## 15. 功能覆盖率协同支持

本 plan 不实现 coveragent/covergroup。

后续 coverage 可采样：

- V2 split issue target、FuType/fuOpType route。
- LSQ enqueue slot 占用、load/store batch element 数、随机idle类别和redirect launch/sample时点。
- STA IQ hit/miss、real-WB、expected-fatal 类型。
- normal commit、fault convergence、V2 SQ count-only deq。
- L2TLB ready gate/outstanding 状态和 response latency bucket。

覆盖率实现必须另建专项，不得混入本总控或当前测试框架激励主流程。

## 16. 修改方案总结

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| 版本结构参数 | 编译期结构 | 固定 V3 值、runtime 镜像或同义参数可能并存 | V2/V3 结构必须 elaboration 前固定 | compile profile 是唯一权威，runtime 只限制行为使用量 |
| 主表 VADDR | 参数语义 | VA 生成复用 PADDR 窗口 | VA/PADDR 语义不同 | MAIN_VADDR 与 PADDR 参数解耦 |
| split issue | 激励生成 | 聚合 issue 语义和固定 fired-mask 残留 | V2 是 LDA/STA/STD split port | 由 compile port count 派生 route/mask，vector 本轮 fail-fast |
| LSQ enqueue | 激励生成/driver 时序 | V3 slot/response 假设残留，allocation和issue-ready同拍 | V2 无 accept-response，字段更多且launch后下一边界才完成sample | 完整request setter、load/store 6/4实际free gate、clock-first每拍streaming、launch reservation与下一边界issue-ready分层；不增加固定retry guard |
| int-WB/IQ raw | monitor event | 伪造不存在 key，replay 后缺 snapshot | V2 raw 必须保真且 current event 必须带 generation | raw 保真 + current status snapshot attach + cycle timeline |
| ROB/LSQ status | 状态生命周期 | fault 混入 normal commit，SQ pointer 默认 0 被误用 | V2 fault 不产生 normal commit，V2 无 sqDeqPtr | normal/fault 分流、full-raw owner、count-only deq |
| L2TLB | responder 生命周期 | ready 可能由 item/default 绕过 | 需要 single-outstanding backpressure | 唯一 gate + fire 后清 gate + response done 后 rearm |
| CSR/sfence | runtime snapshot | 近义字段可能混入 lookup/pass-fail | V2 字段语义分层 | snapshot-only、standalone flushPipe、provider single-owner |
| DCache sideband | DUT input 防御 | generic idle/random 可能驱非零 | 当前无合法 producer | 全生命周期 known-zero，非零首赋值前 fatal |
| monitor output | 观察链分类 | raw producer 与 analysis producer 容易混淆 | RM transaction 尚未闭环 | runtime/raw、OUTPUT_OBSERVATION_XZ、ANALYSIS_PORT_DEFERRED 分层 |
