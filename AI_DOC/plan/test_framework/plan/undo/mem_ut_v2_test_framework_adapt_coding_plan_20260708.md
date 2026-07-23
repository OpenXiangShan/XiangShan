# mem_ut V2 测试框架适配总控 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，monitor output、L2TLB、LSQ MMIO/status 和 pending-MMIO 专项均已完成并归档；L2Cache responder 等剩余专项按 owner 继续执行 |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv`、`build_memblock/rtl/filelist.f` |
| Plan 类型 | V2 测试框架运行期适配总控，不替代专项 execution plan |
| 适配原则 | 只记录 V2 适配的关键问题、专项 owner、修改逻辑边界和文字伪代码；不保留历史讨论和长 checklist |
| 创建/修订日期 | 2026-07-21 |

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
| 既有 compile 参数、宽度、FuType、ROB/LQ/SQ key | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_compile_param_and_width_adapt_execution_plan_20260708.md`，该范围已归档完成，只作为公共基线 |
| SQ deq/cancel count width 与 redirect/cancel latency compile delta | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md`；已在统一compile header新增宏和派生检查，是这些参数的唯一coding owner，不回写到既有compile plan |
| 自动主表 VADDR 窗口 | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_main_table_vaddr_generation_adapt_execution_plan_20260713.md`，源码审计、地址复用跨度修复、远端验证和两轮独立 review 已完成 |
| DCache 轻量 L2 response/hint/Probe，flush_done zero-only | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2cache_response_hint_probe_model_coding_plan_20260717.md` |
| LSQ enqueue | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_lsq_enqueue_framework_adapt_final_plan_20260714.md`，coding、文档同步、冻结验证和最终独立review均已完成；真实load已闭环，store admission已覆盖，store终态仍由后续SQ deq专项闭环 |
| split issue、vector stimulus/driver gate | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_split_issue_framework_adapt_execution_plan_20260708.md`，已完成并归档；只拥有vecissue默认入口关闭和driver valid fatal，不修改vector output monitor |
| IQ feedback/replay、VSTU gate | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_iq_feedback_replay_framework_adapt_execution_plan_20260711.md`；已完成STA SQ-only raw、active SQ/current snapshot attach、同拍IQ-first、deferred ctrl、严格STA real-WB顺序和VSTU valid fatal |
| int-WB/writeback | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_int_wb_writeback_framework_adapt_execution_plan_20260708.md` |
| CSR/sfence payload字段与runtime语义 | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_csr_control_runtime_semantic_review_execution_plan_20260708.md`；拥有CSR字段采样/消费语义；L2TLB专项只拥有不受semantic capture gate控制的latest发布 plumbing，并复用同一raw类型、统一seq和公共state，不复制字段模型 |
| L2TLB response/permission、多 outstanding lifecycle | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md`；唯一拥有 permission 字段链、独立runtime CSR latest发布与统一seq、request-time CSR 冻结、per-fire token、pending queue、ordered/reorder 调度、三档最早 due、ready、reset/non-destructive flush event/stop 生命周期 |
| LSQ MMIO/status、cancel output 对账 | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md`；已完成per-epoch record、独立cancel snapshot/redirect anchor sideband源码与consumer、software/observed直接对账和global-stop gate，observed count不接入第二个SQ deq/free-count owner |
| pending-MMIO load/store sideband | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md`；已完成MMIO raw、active uid tag/provenance、head query和directed owner-contract闭环 |
| monitor output分类、vector-WB gate | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md`；已完成ctrl snapshot/redirect anchor的monitor职责分类和`writebackVldu` valid fatal；具体sideband类型、queue、producer源码、consumer和reconcile由已归档LSQ MMIO/status专项唯一coding |

本文对已有 owner 的问题只给出总控级摘要。coding 时以对应专项 owner 的文件清单和函数合同为准，
不得从本文自行扩展修改范围。owner 表明确标为“当前没有子 plan coding owner”的内容只登记缺口，
不属于可执行 coding 方案。

### 2.1 子计划执行记录

| 子计划 | 状态 | 本功能记录 | Review/验证 |
|---|---|---|---|
| compile 参数与宽度适配 | 已完成并归档 | 建立 V2 compile profile 单一真源，参数化 ROB/LQ/SQ key、FuType、物理 slot/port/capability，并移除硬件结构 runtime plus 镜像。 | 以 `plan/do/mem_ut_v2_compile_param_and_width_adapt_execution_plan_20260708.md` 的归档记录为准。 |
| LSQ enqueue 适配 | 已完成并归档 | 完成 V2 6-slot、load/store 6/4 过滤、完整 enqueue 字段、无 response 的 clock-first streaming、launch reservation 与下一采样边界 issue-ready。 | 以 `plan/do/mem_ut_v2_lsq_enqueue_framework_adapt_final_plan_20260714.md` 及对应 implementation review 为准。 |
| 自动主表 VADDR 窗口 | 已完成并归档 | 自动 normal 主表改用独立 `MAIN_VADDR` 参数，初始化时拒绝非法 Sv39 正规范窗口，按完整访问跨度选择 64B 对齐虚拟地址；地址复用最终尺寸越界时保留参考地址并按 ref size 收敛合法 opcode，无参考 fallback 按最终类型重新选址；TLB builder 的 PADDR 映射保持独立。 | 干净远端编译通过；默认及 VA/PA 不同窗口的 smoke 通过；窄窗口场景实际触发 helper，后续 SQ deq mismatch 已明确归属后续 owner；第二轮独立 review 通过。review 位于 `review_doc/do/mem_ut_v2_main_table_vaddr_generation_adapt_implementation_review_20260721.md`。 |
| V2 split issue | 已完成并归档 | 建立 LOAD/STA/STD 到 `issueLda/issueSta/issueStd` 的 target-local pipe 映射；字段写入前检查 FuType/fuOpType/behavior 合法矩阵；driver 只记录真实 `valid&&ready` fired-mask，redirect/epoch 仅取消未 fire item；关闭 scalar testcase 的 vector 默认入口并在 vecissue driver fail-fast。 | VCS 编译通过；正确 preset 的 `tc_dispatch_real_smoke` 通过，`UVM_ERROR=0`、`UVM_FATAL=0`；implementation review 最后一轮 subagent `FINAL PASS`。review 位于 `review_doc/undo/mem_ut_v2_split_issue_framework_adapt_implementation_review_20260722.md`。 |
| Int-WB/writeback | 已完成并归档 | V2 split `writebackLda/Sta/Std` 按 source/lane 采样；LDA/STA 用真实 ROB key 补 current issue snapshot；STD 用 ROB value-only 双 flag 过滤后唯一反查并从 status 补 SQ；只有真实 STD writeback 设置完成状态；target flush epoch 和 capability guard 防止 stale/unsupported event。 | 干净远端 VCS/Verdi 编译 `0 error(s), 0 warning(s)`；`tc_sanity` 通过；真实 store smoke 已闭环 STD/STA WB 与 ROB commit，后续 SQ deq pointer mismatch 明确归属下游 owner。review 位于 `review_doc/undo/mem_ut_v2_int_wb_writeback_framework_adapt_implementation_review_20260722.md`；plan 已移至 `plan/do`。 |
| IQ feedback/replay | 已完成并归档 | STA monitor只生成真实SQ-only raw；adapter用active SQ map/current status补UID、ROB、issue epoch和replay sequence；同拍IQ先于int-WB进入同一redirect-first batch；ctrl deq延后应用；严格模式要求STA real-WB前已有IQ hit；VSTU/STD IQ保持fail-fast边界。 | 全量VCS/Verdi编译`0 error(s), 0 warning(s)`；真实store路径已经过STD WB、STA IQ/WB和ROB commit，后续SQ deq mismatch归下游owner；software replay受既有int-WB inactive STD X/Z和disabled-monitor connect问题阻塞。review位于`review_doc/undo/mem_ut_v2_iq_feedback_replay_framework_adapt_implementation_review_20260722.md`；plan已移至`plan/do`。 |
| CSR/sfence runtime 语义 | 已完成并归档 | 完成 misalign/priv_debug 的 `1/1/0` snapshot-only 链路、CSR re-arm epoch、sfence `flushPipe` 透明驱动与四态 valid/XZ 边界；不改变 TLB key、standalone flush、pass/fail 或 terminal 主流程。 | V2 远端 compile/elaboration/link 和 `tc_sanity` smoke 通过，`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`；最终独立 reviewer `FINAL PASS`。plan 已归档到 `plan/do`；implementation review 保留在 `review_doc/undo/mem_ut_v2_csr_control_runtime_semantic_review_implementation_review_20260722.md`。 |
| L2TLB response/permission 与多 outstanding lifecycle | 已完成并归档 | 保持 DTLB -> L2TLB request、L2TLB -> DTLB response 语义；补齐 V2 S1/S2 G/U 字段链，按每次真实 request fire 建 token，使用 bounded pending queue + driving slot，支持默认顺序/可配乱序回复、三档 due latency、CSR latest、flush/reset/stop 生命周期和唯一 owner。driver 最终采用 owner=0 idle、owner=1 阻塞取当拍必有 item 的握手，并补齐 `do_kill/get_owned_item_or_abort/phase_ended` 强制停序清理；独立 S1/S2 权限模型仍留在 TODO。 | r7 staged-only compile、disabled和tc_sanity active smoke通过；r8强制停序补强后的compile与active basic smoke通过，且文档未把它误写为callback动态命中。独立 reviewer核对staged代码、UVM 1.2调用链、r7/r8日志和共享文件提交边界后给出`FINAL PASS`。implementation review位于`review_doc/undo/mem_ut_v2_l2tlb_response_permission_adapt_implementation_review_20260722.md`，plan已移动到`plan/do`。 |
| Monitor output 分类与 vector-WB gate | 已完成并归档 | 保持现有output observation与analysis-port deferred边界；`writebackVldu` valid为1或X/Z时fail-fast，不生产第二套scalar raw/event；cancel snapshot和redirect anchor只定义monitor职责，状态owner仍由LSQ专项实现。 | 独立review `FINAL PASS`；commit `cf63e12ebd`；plan位于`plan/do/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md`，review位于`review_doc/undo/mem_ut_v2_monitor_output_framework_adapt_implementation_review_20260722.md`。 |
| LSQ MMIO/status 与 cancel 对账 | 已完成并归档 | 建立modeled ROB head及表尾watermark，分离normal commit、fault convergence和LQ/SQ physical deq；参数化`sqDeq` count-only链路；active idle保持level sideband；redirect epoch按software count与DUT snapshot直接对账且只由software路径回退free count；deferred raw FIFO、singleton owner和runtime drain闭环；`pendingst/scommit`按V2 scalar ROB store分类接受STORE/CBO。 | 最终VCS/KDB compile通过；default real smoke、real cancel reconcile和pending-MMIO directed均`TEST_PASS`且未捕获error/fatal为0；CBO分类日志通过；最后一轮独立review `FINAL PASS`。plan位于`plan/do/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md`，review位于`review_doc/undo/mem_ut_v2_lsq_mmio_status_framework_adapt_implementation_review_20260722.md`。 |
| pending-MMIO load/store sideband | 已完成并归档 | ctrl monitor采集load/store MMIO raw与ROB value；resolver结合完整active key、动态epoch和LOAD sample provenance归一化uid；status保存canonical tag/source，LSQ owner只查询tag生成head `pendingMMIOld`；stale旧owner丢弃，无法证明归属或新owner重叠fail-fast；directed vseq覆盖tag/provenance、fault head、owner reset和global-stop raw drain。 | `v2_lsq_mmio_cbo_final_20260723`下directed为`TEST_PASS`，`UVM_ERROR=0`、未捕获`UVM_FATAL=0`、精确caught fatal=1；相邻real smoke/cancel也通过；最后一轮独立review `FINAL PASS`。plan位于`plan/do/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md`，review位于`review_doc/undo/mem_ut_v2_pending_mmio_load_sideband_implementation_review_20260722.md`。 |

后续每完成一个子计划，必须在本表追加其实际功能、归档路径和验证结果，并保持每个子计划一个独立
本地 git commit。

## 3. 问题一：V2/V3 编译期结构仍可能存在第二权威

### V2 问题

V2 和 V3 的 ROB value width、FuType width、LSQ enqueue slot、split issue port、`sqDeq` count width、
SQ deq pointer presence 等是编译期结构。旧逻辑中仍可能存在固定 V3 literal、runtime plus 镜像或
同义参数；其中 ctrl raw/interface/xaction/monitor/XZ 仍把 `sqDeq` 重复写为 `[1:0]`/宽度 `2`，
会让 V2 worktree 编译通过但运行期按错误结构截断、驱动或解析。

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
- `MEMBLOCK_DUT_ENSBUFFER_WIDTH` 是 `sqDeq` 最大计数的 profile 主参数；
  `MEMBLOCK_SQ_DEQ_COUNT_W=$clog2(MEMBLOCK_DUT_ENSBUFFER_WIDTH+1)` 是唯一派生宽度，
  raw/interface/xaction/monitor/XZ 公共 consumer 全链消费该宽度，不保留固定 `[1:0]` 或 `2`；
  xaction 若被默认 sequence 随机化，合法值限制为 `0..MEMBLOCK_DUT_ENSBUFFER_WIDTH`。
- 现有 `default_io_mem_to_ooo_sqDeq_cons` 必须实现为以下约束；该约束只限制随机 transaction，
  不改变 DUT raw observed flow：

  ```systemverilog
  io_mem_to_ooo_sqDeq inside {[0:`MEMBLOCK_DUT_ENSBUFFER_WIDTH]};
  ```
- `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR` 只控制 `sqDeqPtr` presence 和 pointer/count-only 分支，不能推导或
  替代 `sqDeq` count width；count 总线在 capability 为 0 或 1 时都存在。
- `dut_inst.sv` 继续保留当前 RTL 逐端口展开的具体 `[1:0]`，不作为公共参数第二权威；connect
  继续同宽直连，不新增宽度声明。
- AMO/MOU、CBO、vector LS 本轮没有 scalar capability；默认权重为 0，显式非 0 或 manual/fixed 生成
  在主表落表/admission 前 fail-fast。
- 最终 LSQ enqueue plan 已复用 V2 compile baseline，补齐 LSQ 派生宏、无 response 的 clock-first
  streaming、load/store 6/4 gate和pending-sample时序；未增加新的profile selector或固定retry guard。

### 文字伪代码

```text
编译当前 V2 worktree：
  当前 V2 分支通过 tb.f 的 compile include 链加载 V2 compile header 默认值；
  memblock_compile_params.svh 定义 V2 ROB/LQ/SQ/FuType/slot/port/presence tuple；
  profile 定义 MEMBLOCK_DUT_ENSBUFFER_WIDTH=2，并按 RTL 公式唯一派生
    MEMBLOCK_SQ_DEQ_COUNT_W=2；
  ctrl raw/interface/xaction/monitor/XZ 全部消费该派生宽度；
  dut_inst 保留当前 RTL 展开宽度，connect 只做同宽连接；
  HAS_SQ_DEQ_PTR 只选择 pointer presence，不参与 count width 计算；
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

相关专项按源码边界拆分唯一 owner：

- split issue专项建立 `MEMBLOCK_ISSUE_TARGET_LOAD/STA/STD` 到 V2 `issueLda/Sta/Std` 的权威映射。
- split issue专项使用V2 FuType bit15/16/17表示scalar LDU/STU/MOU，禁止把V3 36-bit literal低位截断。
- split issue专项让fired-mask width、LOAD/STA/STD base offset、full-mask、driver ready映射全部从compile port count派生。
- split issue专项删除scalar testcase中的随机/generic vecissue default sequence，并唯一实现vecissue transaction valid fatal。
- IQ feedback/replay专项唯一实现VSTU feedback valid fatal。
- monitor output专项唯一实现`writebackVldu` valid fatal。
- split issue专项保证issue loop只有driver fired-mask确认的真实fire才计progress；queue blocked、delay、route尝试不计。

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
- collect和driver launch复用现有global flush/epoch gate形成launch前abort；confirm只依据driver
  `request_launched/aborted_by_redirect`，真实launch后不得被当前flush/epoch二次否决；不增加固定5-cycle retry guard。
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
    当前批request_launched=1且未标记launch前abort时，preview key重新核对后调用唯一commit_allocate()预留资源；
    confirm时即使epoch已变也必须预留，保存该epoch只用于下一sample边界的issue gate；
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
current snapshot attach、STA 单向阶段状态和单 service batch 内的 deferred ctrl 处理。

总控固定以下组合：

- STA IQ raw 只保留真实 SQ。
- LDA/STA raw 只保留真实 ROB。
- STD raw 使用 int-WB 专项的 ROB value-only 双 flag probe，不进入 STA current snapshot helper。
- LDA/STA/IQ 进入既有 generic normalize 前，converter 本地必须已经附加并检查 UID、canonical key、
  `issue_epoch/replay_seq`；本轮不修改 `normalize_feedback_event()` 的全局 fallback。
- current snapshot 来源是现有 status，不新增 generation token/tombstone。
- 每次 `service_monitor_once()` 只处理一个采样 cycle；本次可见的 IQ/WB/ctrl raw 必须同 cycle，出现
  mixed-cycle 直接 fatal。semantic batch 完成后才按原顺序 apply 本次 deferred ctrl，service 尾部只执行
  一次既有 recovery；不新增跨 cycle 排序或逐 cycle recovery 循环。

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

converter 本地完整性检查：
  如果 event 是 STA IQ、LDA WB 或 STA WB：
    要求 has_uid/has_issue_epoch/has_replay_seq 完整；
    缺失 fatal，不把该职责下沉到 generic normalize fallback；

single-service batch：
  本次 service drain 当前可见 IQ/WB/ctrl raw；
  第一条 raw 固化本 batch sample_cycle；
  后续任一 raw.cycle 不等于 sample_cycle 时 fatal；
  ctrl raw 先保存到 deferred_ctrl，不立即删除 active map；
  semantic events 继续交给既有 redirect-first batch handler；
  semantic batch 完成后按原顺序 apply deferred_ctrl；
  返回 service_monitor_once，由既有调用点执行一次 replay/redirect recovery；
  不冻结三个 queue，不按最小 cycle 排序，不在 collector 内逐 cycle 调 recovery。
```

## 8. 问题六：ROB/LSQ commit、MMIO status 和 SQ deq pointer 语义混杂

### V2 问题

V2 只有 `sqDeq` count，没有 `sqDeqPtr`。当前 `sqDeq[1:0]` 是
`log2Ceil(EnsbufferWidth+1)` 在 `EnsbufferWidth=2` 下的派生结果，不是独立固定协议宽度。同时
`pendingPtr/pendingst/pendingMMIOld/scommit` 由 lsqcommit driver 驱动，语义上依赖当前 ROB head。
旧逻辑容易把 fault 当 normal commit、把 batch tail 当 pending head、把 ctrl raw deq 过早应用删除同批
event 的 active map，或让 raw/interface/monitor 使用不同的 `sqDeq` count width。

### 修改原因

V2 LSQ/ROB sideband 既影响 DUT 输入，也影响测试框架 terminal 收敛。必须分清 normal commit、
fault convergence、真实 LSQ deq 和 output tag producer，否则会掩盖 deadlock 或伪造 progress。

本问题还必须明确 `scommit` 与 `sqDeq` 不是同一事件：

| 语义 | V2 端口 | 方向 | 计数单位 | 框架影响 |
|---|---|---|---|---|
| ROB scalar store commit | `io_ooo_to_mem_lsqio_scommit` | ROB -> MemBlock | normal commit batch 中 scalar store 子集的数量 | normal batch 全部 UID 写 `status.rob_commit`，该字段只传递 scalar store 子集数量；不推进 `sq_deq_ptr` |
| SQ physical deq | `io_mem_to_ooo_sqDeq` | MemBlock -> 后端 | 本拍连续离开 SQ 的 entry 数 | 消费 raw ctrl，释放 SQ mapping、`sq_deq_ptr` 和 free count；不写 `status.rob_commit` |

完整 core 的下游 Dispatch/Scheduler 可能把 MemBlock `sqDeq` 接入名为 `scommit` 的 Bundle 字段，
但那不是 ROB 输入 `scommit`。ROB 输出 `scommit` 经过一次 `RegNext`，StoreQueue 内部消费前还有
一次 `GatedRegNext`；`sqDeqCnt` 则经 `RegNext` 形成 MemBlock 输出。两者可能跨周期、同拍但数值
不等，或只出现其中一个；总控不得要求同拍相等，也不得用 ROB `scommit` 直接模拟 SQ deq。

### 修改方案与修改逻辑

LSQ MMIO/status 专项负责：

- 独立 modeled ROB head 驱动 `pendingPtr/pendingst/pendingMMIOld`。
- normal batch 和 fault convergence 互斥。
- fault token 只置框架 `rob_commit`，与真实 LQ/SQ deq 可按任意顺序独立到达；只有 token、fault 状态和
  LSQ mapping释放全部收敛形成fault terminal后才rebase head。
- `apply_raw_ctrl_deq(raw)` 是 LQ/SQ/SB full-raw 唯一 owner。
- `MEMBLOCK_DUT_ENSBUFFER_WIDTH` 从 V2 profile 获取物理最大 count，
  `MEMBLOCK_SQ_DEQ_COUNT_W=$clog2(MEMBLOCK_DUT_ENSBUFFER_WIDTH+1)` 只派生一次；
  `dispatch_raw_ctrl_t.sq_deq`、ctrl interface/xaction/monitor/XZ 全部消费该宽度；`dut_inst.sv`
  保持 RTL 展开的具体宽度，connect 保持同宽直连。handler 在消费前检查
  `raw.sq_deq<=MEMBLOCK_DUT_ENSBUFFER_WIDTH`。
- `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR` 只控制 pointer payload presence 和 pointer/count-only 分支，不能替代
  count width；无 pointer profile 仍保留完整 `sqDeq` count 总线。
- `lqCancelCnt/sqCancelCnt` 的interface/xaction/monitor/snapshot宽度分别从LQ/SQ容量派生；xaction现有
  `72/56` literal范围改为`MEMBLOCK_DUT_LQ_SIZE/SQ_SIZE`，monitor observed和record software
  累计同样按对应容量检查，禁止位宽参数化后继续保留容量第二权威。
- V2 SQ deq 使用 count-only 分支，全部预检查通过后才 release；预检查只确认当前 active
  SQ owner，不把 `status.rob_commit` 作为 raw deq 硬门槛。`try_retire_committed_uid()` 仍负责
  在 `rob_commit` 到达后收口 success/terminal。
- `scommit` 和 `sqDeq` 由两个独立 consumer 消费：前者只更新 commit 状态/sideband，后者只更新
  SQ mapping、`sq_deq_ptr` 和 free count；二者不互相补造、不互相推进。
- 写者合同固定为：`lq_deq_ptr/sq_deq_ptr` 只由 reset 和成功消费真实 `lqDeq/sqDeq` 的
  `release_lq/release_sq()` 更新；`lq_free_count/sq_free_count` 才由 reset、enqueue allocation、
  redirect cancel 和真实 `lqDeq/sqDeq` release 更新。
- 每个 redirect 由 `request_redirect_flush()` 创建一个 reconcile record；monitor 不创建第二个 redirect
  状态机。`advance_active_redirect()`必须等待anchor、internal T0 sample和main service ctrl drain后，才调用
  唯一`apply_redirect_flush_range()`扫描/清理；`try_retire_committed_uid()`命中active redirect只defer，
  不旁路scan直接prepare。reservation ledger只补充sample sequence、same-cycle enqueue和防重复epoch。
- 保持`commit_allocate()`现有void签名；真实launch的allocation完成后调用
  `begin_lsq_reservation_launch(uid)`生成稳定reservation launch epoch，pending UID queue升级为
  `(uid,reservation_launch_epoch)`token queue；原pending batch dispatch/flush epoch独立保留，只负责
  `complete_admission()` gate，不得与reservation launch epoch合并。下一次
  `finish_item()`返回后只取一次统一sample sequence并调用`mark_lsq_reservation_sampled()`。即使flush epoch
  失效，也先把token对应实例标为DUT_VISIBLE，再拒绝issue-ready；missing/duplicate/重发实例mismatch均
  fatal，不能按当前UID status反查。launch epoch只在全表reset清零；redirect clear或真实deq只清
  visibility state/sample并保留epoch，防止旧callback命中新实例。
- `confirm_lsq_candidates()`只有`request_launched=0`的launch前abort可以不建reservation；
  `request_launched=1`后删除按confirm时global flush/epoch mismatch提前返回的旧分支，始终allocation并建token，
  保存的batch dispatch/flush epoch只在下一sample边界禁止`complete_admission()`。
- `status_transaction::snapshot_from_main()`保持只复制主表UID和ROB/LQ/SQ静态key，不得清reservation
  metadata；`init_status_for_uid()/status.reset()`只用于首次建表或`reset_all_tables()`全局重置，不得用于
  redirect reissue。调用顺序固定为`commit_allocate -> activate_uid/snapshot_from_main -> begin launch`，
  因此reissue和deq都保留旧launch epoch，只有新真实launch递增动态实例token。
- record 中的 `software_cancel_lq/sq_count` 是软件回退的唯一来源；
  `apply_pending_lsq_cancels()`只消费`software_count_finalized=1`的record，并按record各调用一次
  `cancel_lq/cancel_sq()`，后者仍是 enqueue pointer 和
  free count 的唯一 cancel 写者。旧 `pending_lq/sq_cancel_count` 若保留，只能是未应用 record 的派生和。
- ctrl monitor 每个 post-reset sample 把 `lqCancelCnt/sqCancelCnt` 写入独立 0/非0 snapshot sideband；
  redirect monitor 只在顶层 `io_redirect_valid` sample 时写接口可观测的`level/robIdx`和sample-sequence
  anchor，不伪造`flush_itself`。cancel output
  没有 valid 且保持旧值，禁止按 nonzero、value-change 或 semantic ctrl event 生成 observation。
- anchor按FIFO绑定最老未锚定record并比较接口可观测投影；已完成state flush但仍等待delayed output的
  旧record可与下一笔active redirect record并存，有界FIFO按各自target sample支持连续相同count。
  V2 redirect-to-LSQ latency=1定义expected cutoff：早于internal T0是allocated，等于internal T0是
  same-cycle enqueue。DUT update latency=2只描述RTL更新边界；现有clocking monitor offset=1，实际
  snapshot compare读取唯一派生observe latency=3。所有时序参数都只相对顶层anchor使用，不能相对
  driver/service时点。
  reconcile 在每个 record 的 exact target sample 直接比较一次 software/observed，software=0 也必须比较 0/0；
  observed 不再次调用 `cancel_lq/cancel_sq()`。
- 对账记录覆盖已完成 admission 和 DUT-visible pending sample，排除 launch 前 abort；software 与 observed
  不等、target snapshot缺失、非target出现未解释的新level变化、anchor/payload不匹配或超时均
  `uvm_fatal`。reconcile不改变单个UID的`terminal_done`，但record、anchor、snapshot和software rollback
  未收敛时，`request_global_stop_if_done()`不得置位全局退出。
- semantic event claim/handler 完成后才 apply deferred ctrl raw。
- `memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()` 是唯一调度点：每轮
  drain cancel snapshot和redirect anchor，完成原semantic batch/recovery，再调用一次reconcile；
  lsqcommit/LSQ enqueue子sequence不得建立第二个reconcile service loop。
- `get_dut_sample_seq($time)`只允许clocking sample路径调用；negedge readiness/deadline使用纯只读
  `peek_latest_dut_sample_seq()`和adapter维护的latest-drained watermark，不得额外递增sample sequence。
- software-only smoke只验证ledger；新增basicTest real cancel-reconcile vseq使用automatic phase objection，
  以3-entry manual table和年轻load/store的既有issue delay建立确定性DUT_VISIBLE barrier，再经redirect agent、
  anchor、ctrl snapshot和main service驱动非零LQ/SQ cancel。DCache/SBuffer/redirect responder只在real-smoke
  active、global stop且无inflight时自然退出；场景检查victim reissue、后台完成握手和最终全状态收敛。
- active driver no-item/gap 保持 level sideband，只清 `scommit/flushSb` pulse。
- `has_progress` 保持既有轻量 activity 语义，只由 normal commit、当拍新驱动的 flushSb 和
  `flushsb_busy()` 组成；fault token、fault terminal rebase 和真实 LQ/SQ deq 不要求单独计入。
  该值只服务 idle/debug watchdog，不进入 pass/fail、terminal、pointer 或 global-stop 判定；本轮不新增
  `lq_deq_event_seq/sq_deq_event_seq`、pointer snapshot 或 edge helper。

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
  把 raw.sq_deq 作为 MEMBLOCK_SQ_DEQ_COUNT_W 宽的无符号 count 读取；
  如果 count 超过 MEMBLOCK_DUT_ENSBUFFER_WIDTH，fatal；
  检查 sq_deq_ptr_valid 与 profile capability；
  LQ nonzero 走 pointer helper；
  SQ nonzero 且 profile 有 pointer 时走 pointer helper；
  SQ nonzero 且 V2 无 pointer 时走 count-only helper；
  count-only helper 先预检查连续 SQ head owner 和 active mapping，不要求 rob_commit；
  全部通过后才 release pointer、删除 map并调用try_retire；
  不新增或递增仅为has_progress服务的deq event sequence；

scommit/sqDeq 解耦：
  commit batch -> 全部 UID 标记 rob_commit；
  再计算其中 scalar store 子集数量 -> 驱动 `io_ooo_to_mem_lsqio_scommit`；
  不调用 release_sq，不修改 sq_deq_ptr/free count；
  raw ctrl -> 读取 `io_mem_to_ooo_sqDeq` -> 按 SQ head/count 释放 mapping 和软件 SQ pointer；
  不设置 rob_commit，不推进 commit_cursor/modeled_rob_deq_ptr；
  sq_deq_ptr 只由 reset 和成功消费真实 sqDeq 的 release_sq() 写；
  lq_free_count/sq_free_count 由 reset、enqueue allocation、redirect cancel 和真实 lqDeq/sqDeq release 写；
  `scommit=0/sqDeq=0`（可能是 load-only commit）、`scommit=0/sqDeq>0`、
  `scommit>0/sqDeq=0` 和两者同拍均为可处理状态，禁止同拍相等断言；

deq-before-commit 状态收口：
  raw sqDeq 命中当前 active SQ owner -> release_uid_sq_mapping(uid)；
  保留 status.active=1、status.rob_commit=0，设置/保持 status.lsq_deq；
  try_retire_committed_uid(uid) 因 rob_commit=0 暂不置 success/terminal_done；
  后续 mark_rob_commit_uid(uid) 后再次调用 try_retire_committed_uid(uid)；
  不因 mapping 释放提前推进 commit_cursor/modeled_rob_deq_ptr，不因 raw deq 直接完成 terminal；

collect_monitor_event_batch()：
  collect writeback/IQ semantic events；
  collect ctrl semantic events，并把deq/memoryViolation/SBuffer字段的完整raw存入deferred queue；
  semantic raw ctrl不保存cancel count；
  process semantic batch；
  apply_deferred_ctrl_updates_batch(deferred queue)；

advance_active_redirect()：
  driver done后仍等待record anchor有效；
  用peek只读sample seq和adapter latest-drained watermark确认都已到internal T0；
  条件满足后才调用apply_redirect_flush_range唯一扫描active window；
  try_retire命中active redirect时只等待该scan，不直接prepare；

apply_pending_lsq_cancels()：
  取最老software未应用record；
  software_count未finalized则停止；
  finalized后按record调用cancel_lq/cancel_sq一次并置software_applied；

service_monitor_once()：
  获取本轮service cycle；
  收集CSR/sfence等runtime context；
  adapter drain逐拍cancel snapshot和redirect sample anchor到common data sideband buffer；
  调用collect_monitor_event_batch完成原semantic batch；
  调用exception_redirect_replay_task完成原redirect/replay apply；
  调用service_cancel_reconcile按record target sample直接比较software/observed，不修改deq/free count；
  返回外层service_real_dispatch_flow；

service_real_dispatch_flow() 的既有退出调用点：
  每拍只在service_monitor_once和route_all_issue_queues之后调用一次all_transactions_terminal_done()；
  all_transactions_terminal_done内部调用唯一request_global_stop_if_done()；
  request_global_stop_if_done同时检查transaction、record、anchor、raw/local snapshot和software-pending；
  任一cancel状态未收敛则保持主service运行，但不回退已经完成的UID terminal状态；
  禁止service_monitor_once直接再次调用request_global_stop_if_done。
```

## 9. 问题七：L2TLB 串行阻塞 responder 会漏采 V2 多 outstanding request

### V2 问题

V2 有 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 等 response permission 字段，同时
`PTWNewFilter` 内部保存多笔 `vpn/s2xlate` request。当前 sequence 只按 `request_valid()` 串行发送
ready item 和带 `pre_pkt_gap` 的 response item；gap 期间 driver idle 仍把 ready 置1，新的 request 可以
真实 fire，但 sequence 正阻塞且没有 pending queue，存在丢请求风险。sequence 关闭或 idle-stop 退出后，
driver 继续 active idle 也有同类风险。

### 修改原因

V2 `PtwRespS2` 没有 request ID，但 `PTWFilterEntry` 对全部有效 entry 按
`s2xlate + hit(vpn/asid/vasid/vmid)` 匹配；L2TLB 又可从 cache、PTW FSM 和 LLPTW 三条不同延迟路径
仲裁返回，因此 response 可按内容乱序命中。测试框架必须保存所有已握手 request，并在保留默认顺序模式
的同时提供显式乱序模式。response 延迟不能继续阻塞 driver，而要成为每笔 queue record 的 due sample。

### 修改方案与修改逻辑

扩展后的 L2TLB 专项唯一负责：

- 检查 `s2_entry_perm_g/u` 从 `entry.pte_g/pte_u` 经 sequence、xaction、driver、interface、active connect
  到 RTL internal wire 的完整字段链。
- 保持内部 `DTLB -> L2TLB_agent -> DTLB` responder 方向，不接顶层 `io_l2_tlb_req_*` 或下游模型。
- 增加 compile `DFILTER_SIZE=32/FLUSH_HOLD_CYCLES=4` 及 `memblock_dispatch_types.sv` 同名typed
  localparam，其中4拍来自顶层 CSR/sfence 到 internal
  filter 的两级 `RegNext` 加 filter 内部 `fenceDelay=2`；runtime `MAX_OUTSTANDING`、`RESP_REORDER_EN`、
  1拍/中/长最早可响应延迟值和三档权重；删除旧 `MIN/MAX_LATENCY`，避免两套延迟权威。
- 用 `pending_q + driving slot` 保存全部 accepted request；request fire 时用显式 `copy_from()` 冻结
  CSR/key/entry snapshot/response，
  response sample 确认后才更新 uid TLB record。
- legacy `tc_base` default sequence和`basicTest + VSEQ_MAIN`显式vseq是两种分别合法的启动拓扑；用
  package try-claim/release拒绝hybrid并发建立第二份pending queue，package只返回状态，UVM fatal由
  sequence报告；sequencer item arbitration不能替代lifecycle owner唯一性。
- 相同 key 的每次真实 request fire 都建立独立 token 并各返回一次；不得因较早 response 可同时 refill
  多个 DTLB filter entry 而合并、删除或跳过后续已接受 token。
- ordered 模式只允许最老 due request 回复；reorder 模式在全部 due request 中用 `std::randomize` 选择。
- 逐拍 cycle item 驱动 ready/response，driver 的 `pre_pkt_gap/post_pkt_gap` 必须为0；queue-full、reset、
  sfence/CSR non-destructive flush event snapshot、global stop 和 idle-stop 生命周期均由该专项闭环。
- 首份latest runtime CSR未有效前保持ready=0且不累计idle；active阶段新flush event必须与当前sample
  同时，迟到event在任何queue/counter变化前fatal，只有ready从未开放的startup/reset允许旧baseline保守hold。
- CSR monitor在post-reset sample无条件发布non-destructive runtime CSR latest snapshot；逐拍baseline只由
  monitor维护并更新，semantic `push_raw_csr()`继续受capture gate控制；两条latest视图复用同一snapshot
  sequence并幂等写同一`mmu_csr_state`，semantic clear后下一gate sample按valid/seq mismatch重发，保证
  legacy `tc_base` default responder也能取得首份CSR。
- idle-stop在构造下一cycle item前进入stopping并发送最终inactive item；强制kill/stop/phase jump后的
  同仿真owner handoff不支持，正常handoff只允许自然release。
- 三档 latency 只决定 `due_sample`，即最早可响应拍；端口竞争或 ordered head blocking 可以使实际
  `complete_sample` 更晚，始终要求 `complete_sample>=due_sample`。
- 当前 s1/s2 permission 继续同源 `entry.pte_*`；两套 PTE 独立建模仍属于 TODO。

### 文字伪代码

```text
L2TLB request/response service：
  每个sample边界先锁存request valid/vpn/s2xlate和接口实际ready；
  等待NBA region后一次读取runtime CSR latest和non-destructive flush event_seq/sample_time snapshot；
  先只读判定reset/backend；reset路径吸收latest flush baseline并清账，不执行active freshness fatal；
  post-reset active阶段再校验新event.sample_time等于当前$time，迟到或超前均在任何状态变化前fatal；
  校验通过后确认上一driving response，再按统一runtime CSR snapshot序号幂等同步runtime CSR；
  合法event_seq前进时取消旧pending并按FLUSH_HOLD_CYCLES=4建立ready hold，即使CSR尚无效也推进event；
  active同sample首次观察到新event时，旧ready形成的真实fire分配token并计入accepted/canceled；
  startup/reset旧baseline因ready从未开放，只建立保守hold，不创建killed token；
  CSR无效却观察到fire则fatal；CSR有效的真实request fire生成内部token，显式复制snapshot字段和
  CSR/key/entry snapshot并冻结response后入pending_q；
  相同key的每次fire仍各自入queue，禁止token合并；
  处理global stop和idle-stop；CSR无效时保持inactive、不累计idle，但global stop仍可正常release退出；
  CSR有效时按due sample和ordered/reorder模式最多选择一笔response；
  pending与driving总数达到runtime上限时，下一cycle item把ready置0；
  stop关闭新ready但排空已接受请求；在构造item前已经完成stop/idle-stop决定；
  构造唯一gap=0 cycle item，idle-stop/CSR无效退出路径只能构造ready=0/resp=0；
  driver驱动该item，不在driver内部等待延迟；
  sequence在ready生效前try-claim唯一owner，最终inactive item后自然release；hybrid第二实例fatal；
  response只在下一sample边界被确认后更新uid TLB record，reset/flush按专项event snapshot规则清理；
  due只表示最早可响应拍，拥塞时complete允许晚于due但禁止早于due。
  每次更新复查accepted=completed+flush/reset canceled+pending+driving。

permission 字段链审计：
  从entry.pte_g/pte_u开始逐层检查s2_entry_perm_g/u；
  active takeover路径任一层缺声明、缺搬运或被常量0替代时，只修复该断裂层；
  inactive takeover分支可保持0，但必须说明它不是被动观察模式；
  request继续来自内部DTLB request的vpn/s2xlate；
  response继续返回内部DTLB，不接顶层L2/PMP端口；
  共享entry.pte_*仍同时填S1/S2，独立两阶段权限留在TODO。
```

## 10. 问题八：CSR/sfence、debug 和 DCache L2 sideband 的默认与消费语义不同

### V2 问题

V2 CSR control 命名与 V3 不同，包含 misalign、priv debug、branch predictor enable 等字段。
`sfence_bits_flushPipe` 是需要构造、驱动和采样的真实接口字段，但它不是当前测试框架行为 owner。
DCache 的 `io_l2_hint_*` 和 `io_l2_flush_done` 是 DUT input，机械接入后若沿用 generic idle/random
mode，可能触发未建模 MSHR hint 或 CSR flush done 行为。

### 修改原因

CSR snapshot-only 字段只需要保存 runtime 真值；DCache hint 的合法非零 producer 只允许来自轻量
L2 responder 已接受的 `AcquireBlock -> GrantData`，`io_l2_flush_done` 当前仍没有合法非零 producer。
`sfence_bits_flushPipe` 则只需默认 `0` 并透明传递：取值为 `0` 或 `1` 都不改变
软件 TLB、DTLB miss responder、redirect、LSQ 或 terminal 行为，不需要 standalone 边界。

### 修改方案与修改逻辑

CSR 专项负责：

- `hd_misalign_ld/st_enable` 和 `tlbCsr_priv_debug` 采样到 raw CSR/runtime snapshot，但本轮不进入
  sequence、pass/fail、terminal 或 L2TLB lookup key。
- `fence_agent_agent_xaction` 为 `sfence_bits_flushPipe` 提供 soft 默认 `0`，并在 debug 文本和
  custom `compare()` 中覆盖该字段；directed item 可覆盖为 `1`。
- fence driver 按 item 原值驱动，idle 驱 `0`；不增加 quiescent provider、standalone cfg/vseq、
  保护窗口或驱动前行为 gate。
- fence monitor 在 `sfence_valid=1` 时采样并检查该 payload；raw sfence 和软件失效 flow 不保存、
  不读取该位，仍只由 `sfence_valid` 触发既有 TLB entry invalidation。

DCache L2 sideband 专项负责：

- interface 四字段 time-zero 初始化为 0。
- generic xaction random、`new()`、idle builder 和 `drive_idle()` 都保持四字段为 0。
- 专用 responder 只对已接受的 `AcquireBlock -> GrantData` 按参数产生一次合法 hint，payload 取
  A source 和 `echo_isKeyword`；Grant、CBOAck、ReleaseAck 和非 DCache client 不产生 hint。
- `send_pkt()` 首个 vif 赋值前检查 hint 无效拍 payload 为 0，并拒绝任意非零 `io_l2_flush_done`。

### 文字伪代码

```text
CSR monitor：
  采样 misalign 和 priv_debug；
  写入 raw CSR/runtime snapshot；
  branch predictor enable 只做观察，不进入 TLB lookup 或 pass/fail；

sfence flushPipe 接口 flow：
  xaction soft 默认 flushPipe=0；
  default sequence 复用该默认，不重复建立第二个约束 owner；
  directed item 如需覆盖为1，优先使用hard inline constraint；显式赋值必须在randomize后、finish_item前完成；
  psdisplay打印该位，custom compare把仅该位不同的item判为不相等；
  driver 无条件按 item 原值驱动 DUT，不查询 quiescent、不等待 drain、不写全局状态；
  monitor 在 valid=1 时对 flushPipe 做 payload X/Z 检查；
  raw sfence 不携带 flushPipe，软件 TLB invalidation 行为保持不变；
  在valid和其它payload相同的前提下，只切换flushPipe不改变DTLB hit/miss或responder行为；

DCache driver：
  drive_idle(mode)：
    保留原 TL A/B/C/D/E idle 逻辑；
    分支结束后无条件驱 l2_hint_valid/sourceId/isKeyword 和 l2_flush_done 为 0；
  send_pkt(tr)：
    在任何 vif 赋值前检查sideband自洽；
    l2_flush_done非0或X时fatal；
    hint_valid=0时要求sourceId/isKeyword均为0，否则fatal；
    hint_valid=1时允许透传专用responder已经按AcquireBlock保存的sourceId/isKeyword；
    driver不重复查询pending request，也不建立第二份hint owner；
    合法时保留原TL payload并按item透传hint，flush_done明确驱0。
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

monitor output 专项负责分类和 vector-WB unsupported gate：

- 20 个 monitor 同时给出 runtime 角色和 RM analysis 角色。
- 当前 analysis producer 统一记录为 `ANALYSIS_PORT_DEFERRED`，本轮不批量恢复。
- 保留 CSR latest snapshot、sfence、ctrl、int-WB、IQ-feedback 五条 semantic runtime 输入路径。
- 规定 `lqCancelCnt/sqCancelCnt` 必须使用独立逐拍 snapshot sideband、redirect input 必须使用独立
  sample-anchor sideband；两者不进入 semantic batch，也不是 RM analysis transaction。具体类型、queue、
  monitor producer源码、consumer、sample sequence和reconcile由LSQ MMIO/status专项唯一coding。
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
  如果字段是lqCancelCnt/sqCancelCnt：
    分类合同要求每个post-reset sample保存0/非0snapshot和统一sample sequence；
    不按nonzero/value-change生成event，不复制到semantic raw ctrl；
    具体producer实现只落在LSQ MMIO/status专项；
  如果字段是顶层redirect valid/payload：
    分类合同要求valid sample只保存cancel latency anchor；
    不反灌成第二个recovery/status事件。
    具体producer实现只落在LSQ MMIO/status专项。
```

## 12. 修改顺序

V2 专项之间存在硬依赖，coding 应按以下顺序执行：

1. 确认 V2 RTL 权威路径存在。
2. 以已归档 compile/width 基线为前提，执行仍需追加 profile tuple 的专项 delta。
3. 执行主表 VADDR 和不支持 op fail-fast，确保后续 flow 不生成本轮不支持激励。
4. 执行 split issue 和 LSQ enqueue，建立正确激励入口。
5. 执行 int-WB 与 IQ feedback/replay，建立 monitor raw 到 current status 的事件链。
6. 执行 monitor output 的 cancel snapshot/redirect anchor producer边界，并与 LSQ MMIO/status 的
   sideband consumer、record、主service和global-stop gate作为同一原子批次完成；不得只落producer。
7. 执行 pending-MMIO producer/query、L2TLB response/permission 与多 outstanding lifecycle、CSR/sfence、
   DCache sideband和其余monitor output分类；L2TLB源码修改只按其唯一专项执行，不从总控复制状态机。
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
- L2TLB responder：检查 `s2_entry_perm_g/u` active字段链、内部DTLB/L2TLB方向和
  `vpn/s2xlate` lookup；以bounded queue保存多outstanding，默认保序、可配置乱序，三档权重延迟，
  request-time CSR冻结，相同key按每次request fire独立记账，response sample后更新uid record，并闭环
  ready/reset/non-destructive flush event/stop；三档值只约束最早due，拥塞时complete可以更晚。
- DCache 轻量 L2 responder：默认 cfg 的 hint/Probe 为 0；专项 cfg 检查 request-bound hint、
  small/medium/large delay、map-backed Probe 和 Release/GrantAck 生命周期；任意非零
  `io_l2_flush_done` 在首个 vif 赋值前 fatal。

本总控不新增 RM/checker/coverage 验证要求。coverage/checker/RM 后续只消费专项留下的字段、事件或标签。

## 14. RM 协同支持

本 plan 不实现 RM、checker 或 scoreboard。

后续 RM/checker 可使用：

- current snapshot 后的 UID、ROB/LQ/SQ canonical key、`issue_epoch/replay_seq`。
- LSQ commit/deq 后的 terminal 状态和真实 deq 状态；本轮不为RM新增deq event sequence。
- pending-MMIO 专项落表的 MMIO tag。
- 后续独立 monitor/RM 专项成对恢复并完成字段合同的 standard transaction producer；当前 monitor
  output 专项保持 `ANALYSIS_PORT_DEFERRED`，本轮没有该producer。

这些字段只能作为后续组件输入；本 plan 不定义 DUT 正确性比较算法。

## 15. 功能覆盖率协同支持

本 plan 不实现 coveragent/covergroup。

后续 coverage 可采样：

- V2 split issue target、FuType/fuOpType route。
- LSQ enqueue slot 占用、load/store batch element 数、随机idle类别和redirect launch/sample时点。
- STA IQ hit/miss、real-WB、expected-fatal 类型。
- normal commit、fault convergence、V2 SQ count-only deq。
- L2TLB response permission、request token、ordered/reorder mode、latency bucket、queue high-water、
  flush drop和response complete sample；本总控不实现对应covergroup。

覆盖率实现必须另建专项，不得混入本总控或当前测试框架激励主流程。

## 与初步 plan 差异说明

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| 版本结构参数 | 编译期结构 | 固定 V3 值、runtime 镜像或同义参数可能并存；`sqDeq` raw/interface/monitor/XZ 固定 `[1:0]`/`2` | V2/V3 结构必须 elaboration 前固定；当前 2 bit 只是 `EnsbufferWidth=2` 的派生结果 | 已归档compile plan继续拥有既有结构；MMIO/status undo plan在同一header唯一新增`MEMBLOCK_DUT_ENSBUFFER_WIDTH`并派生`MEMBLOCK_SQ_DEQ_COUNT_W`及cancel/latency宏，runtime只限制行为使用量，`HAS_SQ_DEQ_PTR`只控制pointer presence |
| 主表 VADDR | 参数语义 | VA 生成复用 PADDR 窗口 | VA/PADDR 语义不同 | MAIN_VADDR 与 PADDR 参数解耦 |
| split issue | 激励生成 | 聚合 issue 语义和固定 fired-mask 残留 | V2 是 LDA/STA/STD split port | 由 compile port count 派生 route/mask，vector 本轮 fail-fast |
| LSQ enqueue | 激励生成/driver 时序 | V3 slot/response 假设残留，allocation和issue-ready同拍 | V2 无 accept-response，字段更多且launch后下一边界才完成sample | 完整request setter、load/store 6/4实际free gate、clock-first每拍streaming、launch reservation与下一边界issue-ready分层；不增加固定retry guard |
| int-WB/IQ raw | monitor event | 伪造不存在 key，replay 后缺 snapshot，ctrl/deq可能提前删除同拍active map | V2 raw 必须保真且 current event 必须带 generation；当前service合同每次只消费一个采样cycle | raw保真+current status snapshot attach；converter本地完整性检查；mixed-cycle fatal；semantic batch后apply deferred ctrl，service尾部只recovery一次 |
| ROB/LSQ status | 状态生命周期与 deq 顺序 | fault 混入 normal commit，SQ pointer 默认 0 被误用；count-only 草案把 `rob_commit` 当作 deq 前置条件；`scommit` 与 `sqDeq` 语义未分离 | V2 fault 不产生 normal commit，V2 无 `sqDeqPtr`，且两类信号方向、延迟、计数单位不同，MMIO/CBO 可能 SQ deq 先于 ROB commit | normal/fault 分流、full-raw owner、count-only deq；独立消费 `scommit`/`sqDeq`，deq 释放 mapping 与最终 retire 解耦 |
| `sqDeq` count 字段链 | 字段/编译期参数适配，合法运行期功能不变 | `dispatch_raw_ctrl_t`、ctrl interface/xaction/monitor/XZ 分别固定 2 bit | count width 来自 `log2Ceil(EnsbufferWidth+1)`，与 SQ pointer presence、SQ size、store enqueue width 和 commit width均不同 | 公共 consumer 全链消费 `MEMBLOCK_SQ_DEQ_COUNT_W`，`dut_inst` 保留当前 RTL 展开宽度；既有 pointer/count-only release、pass/fail 和 terminal 逻辑不变 |
| xaction `sqDeq` 合法范围 | 字段约束适配，不改变 DUT observed flow | `default_io_mem_to_ooo_sqDeq_cons` 为空，随机 transaction 可生成超出 V2 `EnsbufferWidth` 的值 | 默认 sequence 仍必须表达合法结构范围，即使 driver 不主动驱动该 output | 约束 `io_mem_to_ooo_sqDeq inside {[0:MEMBLOCK_DUT_ENSBUFFER_WIDTH]}`；只限制随机 transaction，不修改 raw 消费和终态逻辑 |
| `sqDeq` count 合法范围 | 失败策略新增 | 2-bit 容器可编码 3，handler 没有按 `EnsbufferWidth=2` 拒绝该 RTL 不可能值 | packed width 与合法最大 count 是两个概念 | release 前检查 `count<=MEMBLOCK_DUT_ENSBUFFER_WIDTH`，越界 `uvm_fatal`；合法 0..max 流程不变 |
| cancel count 合法范围 | 字段约束与失败策略适配 | xaction固定72/56，monitor与record累计没有同源容量检查 | count packed宽度可表示容量外值，固定literal无法跨profile | xaction、monitor observed及record software累计统一按LQ/SQ compile容量检查；越界fatal，合法cancel/free-count/terminal合同不变 |
| `scommit`/`sqDeq` 计数与指针 | 功能逻辑修正 | 可能把 ROB commit 数当成 SQ 释放数，或用 `scommit` 推进 `sq_deq_ptr`；load-only commit 边界不清 | ROB commit 与 SQ physical deq 是不同阶段，寄存器延迟和计数单位不同，`sqDeq=2` 只表示两个 SQ entry | normal batch 全部 UID 写 commit 状态，`scommit` 只传 scalar store 子集；`sqDeq` 只释放 SQ mapping；`sq_deq_ptr` 只由 reset/真实 deq release 更新，`sq_free_count` 独立按 reset/allocation/cancel/deq 更新；不建立同拍相等关系 |
| redirect cancel record、reservation生命周期、free count 与 DUT 对账 | 功能逻辑新增/跨专项接口适配 | 聚合cancel回退；driver done即可早扫active map；pending batch只有UID与flush epoch、无稳定launch token；若reissue复用status reset会把旧token清零；cancel只采样/XZ | 早扫会被未消费deq污染；顶层/LSQ双T0和clocking monitor可见拍不同；token必须跨reissue/deq区分动态实例，且不能替代batch flush gate；observed不能写软件状态 | record创建后等待anchor、T0_lsq和ctrl drain再唯一扫描；pending UID升级为reservation token，原batch dispatch/flush epoch独立保留；void allocation后由begin/mark helper维护单调token和sample事实；snapshot只复制静态key，status reset仅用于首次建表/全表reset；sample早于/等于T0_lsq分别计allocated/same-cycle，晚于cutoff fatal；T0+3比较snapshot，rollback只消费finalized record，software/observed独立收敛 |
| cancel directed验证 | 新增真实DUT场景和退出闭环 | software-only fault smoke直接改状态；既有basicTest real-smoke vseq记录过同拍结束，mapping后victim也可能先issue | DUT observed对账必须经过真实VIF且可重复制造LQ/SQ非零victim，后台sequence不能依赖phase强杀 | software-only只测ledger；新vseq使用automatic objection、3-entry table、victim issue delay和DUT_VISIBLE barrier，经redirect driver/anchor/snapshot/reconcile后要求LQ/SQ非零match、reissue、后台自然退出和终态收敛 |
| L2TLB permission与lifecycle | 字段链适配+功能逻辑新增 | `request_valid`后串行ready/阻塞gap/response，无queue；gap和sequence退出时ready仍可接受无人记录请求；延迟值被driver gap当作串行等待；hybrid启动可建立两个状态owner；live entry handle不构成不可变快照；legacy default responder取不到gated raw CSR | V2 DTLB filter有32-entry多inflight并按内容匹配，L2TLB多路径允许返回次序变化；重复key可能跨filter产生多次request fire，真实L2TLB仍按每次fire记账；顶层flush到filter清空有2级RegNext+2拍fenceDelay；sequencer只能仲裁item不能合并queue；entry未注册UVM copy字段；lookup必须等待runtime CSR | 唯一专项实现package try-claim/release、独立runtime CSR latest发布并与semantic raw共享seq、`pending_q+driving slot`、显式entry `copy_from()`与request-time CSR冻结、CSR-ready gate、相同key每fire独立token、默认保序/可配乱序、三档最早due权重、逐拍driver、queue-full backpressure及reset/non-destructive flush event/4拍hold/stop；active迟到flush event状态变更前fatal，idle-stop先发最终inactive item；实际complete可晚于due，permission字段链保持最小适配 |
| CSR/sfence | runtime snapshot/接口透传 | 近义字段可能混入 lookup/pass-fail，flushPipe 默认值和观测链不完整 | V2 字段语义分层，flushPipe 不影响当前框架行为 | snapshot-only；flushPipe 默认0、原值驱动、仅观测不消费 |
| DCache L2 sideband | DUT input/responder | generic idle/random 可能驱无归属 hint 或 flush done | hint 只允许关联 GrantData，flush done 当前无合法 producer | request-bound hint；generic/idle known-zero；flush done 非零首赋值前 fatal |
| monitor output | 观察链分类与时序sideband新增 | raw producer与analysis producer容易混淆，held cancel没有合法event valid，redirect monitor只有XZ回看 | RM transaction尚未闭环；cancel对账需要逐拍value和顶层采样锚点，但不能污染semantic batch/recovery | 5条semantic runtime路径保持；OUTPUT_OBSERVATION_XZ、ANALYSIS_PORT_DEFERRED分层；另增cancel snapshot/redirect anchor sideband，禁止当semantic raw或RM producer |

跨专项关键 helper 差异：`begin_lsq_reservation_launch(uid)` 在现有 void allocation 后返回每UID单调
launch token，`mark_lsq_reservation_sampled(uid,token,sample_seq)` 在下一driver边界只写DUT-visible sample
事实；两者都不修改pointer/free count。`cancel_redirect_scan_ready()` 只读anchor与sample/drain watermark，
决定`advance_active_redirect()`是否可调用唯一active scan。`service_cancel_reconcile()` 输入
`software_count_finalized` record
和bounded snapshot，输出observed进度/debug计数且不写LSQ状态。新cancel vseq覆盖phase、background和core
三个virtual task，输入固定3-entry main table，输出真实redirect对账及自然退出结果。其它专项新增/修改
helper的输入、输出、副作用和完整伪代码以各专项执行plan正文为准；总控不再复制第二实现。

### 审稿用四要素与差异影响

```text
修改目的：
  将V2各flow的接口字段、参数和新增状态逻辑路由到唯一专项owner，避免同一字段或状态被两份plan重复coding。
修改前逻辑行为：
  V3 literal/接口残留分散；LSQ allocation与sample同拍；raw可能伪造key；commit/deq/cancel职责交叠；
  monitor output、L2TLB、CSR和DCache sideband边界不完整。
修改后逻辑行为：
  compile、VADDR、enqueue、issue、WB/IQ、MMIO/status、L2TLB responder、CSR、DCache和monitor分别由owner表执行；
  L2TLB request/response lifecycle由扩展后的L2TLB专项唯一实现，总控不复制queue或driver状态机；
  跨专项只通过公开字段/helper合同交接，任一非owner不得复制状态机或写者。
差异影响：
  改变V2字段来源、失败边界、部分driver/monitor时序和状态收敛；不改变各owner声明保持不变的主表顺序、
  合法pass/fail定义及RM/checker/coverage职责。文档同步和验证范围按各专项执行plan完成。
```

### 跨专项 Helper 审稿伪代码

本总控不新增任何源码 helper；下列内容只用于核对 owner 输入、输出和副作用，不作为第二实现。

```text
begin_lsq_reservation_launch(uid) / mark_lsq_reservation_sampled(uid,token,sample_seq)：
  owner为MMIO/status专项；前者在唯一allocation后返回单调token，只写reservation metadata；
  后者在下一driver边界校验token并写DUT_VISIBLE，不改pointer/free count，随后才由batch flush epoch决定issue。

cancel_redirect_scan_ready(record) / service_cancel_reconcile()：
  owner为MMIO/status专项；前者只读anchor和sample/drain watermark；后者直接比较finalized software count与target snapshot observed count；
  二者均不调用release/cancel；software rollback由MMIO/status专项在现有enqueue sequence的
  `apply_pending_lsq_cancels()`中消费software_count_finalized record。

encode_and_fit_dut_futype() / fit_directed_rob_value_or_fatal()：
  owner为已归档compile专项；输入内部编码或directed value，输出无损DUT值；不可表示时fatal，无运行期状态副作用。

各issue/WB/IQ/L2TLB responder/CSR/DCache helper：
  只由owner表对应专项新增或修改；总控只检查输入来自真实raw/current snapshot、输出进入既有queue/driver，
  以及unsupported路径fail-fast，不复制其候选循环、状态更新或fallback实现。
```
