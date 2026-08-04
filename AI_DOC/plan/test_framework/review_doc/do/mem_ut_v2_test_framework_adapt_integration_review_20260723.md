# mem_ut V2 测试框架适配总控集成 Review

| 项目 | 内容 |
|---|---|
| 关联总控 Plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_test_framework_adapt_coding_plan_20260708.md` |
| 目标版本 | V2，分支 `mem_ut_uvm_v2` |
| Review 范围 | 总控 owner 覆盖、子 plan 归档、跨 flow 文档同步、提交边界和集成验证 |
| Review 日期 | 2026-07-23 |
| 当前结论 | `FINAL PASS`；独立 subagent `Popper` 已完成最后复核 |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应对象或落点 | 示例 |
|---|---|---|---|
| `owner` | 对某一字段、状态或生命周期拥有唯一写者和 coding 责任的专项 | 总控第 2 节 owner 表、各 execution plan | `pendingMMIOld` 由 pending-MMIO/LSQ status 合同共同交接，不由 DCache plan 重复维护 |
| `raw` | monitor 在接口采样边界保存的原始字段，保留 valid、X/Z 和采样顺序 | raw queue、snapshot、sample record | `lqCancelCnt` 先保存 raw snapshot，再由 cancel owner 对账 |
| `semantic` | 已通过字段完整性和 owner 归属转换后的状态事件 | adapter、status table、dispatch progress | semantic commit 才能推进 modeled ROB head |
| `fail-fast` | 发现不支持组合、未知值或无法证明归属时立即 `uvm_error/uvm_fatal`，不静默降级 | driver、adapter、sequence gate | vector LS 不转成 scalar；无唯一 ROB value-only 命中时停止当前必需事件 |
| `drain` | global stop 后只完成已经握手或已建立的在途生命周期，不再创建新工作 | L2TLB/DCache/LSQ responder stop loop | DCache pending D、GrantAck、C assembly 清空后才报告 done |
| `integration baseline` | 所有已完成子 plan 按依赖顺序叠加后的同一 V2 代码树 | `mem_ut_uvm_v2` HEAD 和最后专项验证日志 | DCache smoke 在前序 LSQ、WB、L2TLB、MMIO 等提交之上运行 |

本 review 只审查总控是否把所有 V2 适配问题路由到唯一子 plan，并确认子 plan 的实现、文档和验证
能够组成一条闭环。它不复制子 plan 的状态机，也不重新实现任何 sequence、driver、monitor 或
reference model。总控关闭动作只修改 plan 归档状态、历史 plan 路径和集成审计文档。

## 2. Owner 与提交闭环

下表按总控依赖顺序列出每个 coding owner。提交 hash 是本地提交，用于证明该专项没有停留在 plan
层；详细源码差异和专项伪代码以对应 implementation review 为准。

| 适配专项 | Plan | 主要提交 | Review/验证结论 |
|---|---|---|---|
| compile 参数与宽度 | `plan/do/mem_ut_v2_compile_param_and_width_adapt_execution_plan_20260708.md` | `35e994a0a6`、`63470bc7b1` | compile profile、key/FuType/port 宽度和 runtime/compile 边界已归档 |
| 主表 VADDR | `plan/do/mem_ut_v2_main_table_vaddr_generation_adapt_execution_plan_20260713.md` | `f1756a4833` | VADDR/PADDR 解耦、完整访问跨度和窄窗口路径验证通过 |
| LSQ enqueue | `plan/do/mem_ut_v2_lsq_enqueue_framework_adapt_final_plan_20260714.md` | `bd813bc3ed` | V2 clock-first、load/store gate、reservation/sample 边界已实现 |
| split issue | `plan/do/mem_ut_v2_split_issue_framework_adapt_execution_plan_20260708.md` | `642147364d` | LOAD/STA/STD target-local route 和 vector fail-fast 已实现 |
| int-WB | `plan/do/mem_ut_v2_int_wb_writeback_framework_adapt_execution_plan_20260708.md` | `a3e626988c` | V2 split WB、ROB value-only 反查和真实 STD WB 闭环已实现 |
| IQ feedback/replay | `plan/do/mem_ut_v2_iq_feedback_replay_framework_adapt_execution_plan_20260711.md` | `887e0e6de5` | STA SQ-only raw、current snapshot、IQ-first 和 deferred ctrl 已实现 |
| CSR/sfence | `plan/do/mem_ut_v2_csr_control_runtime_semantic_review_execution_plan_20260708.md` | `bf6598541d` | misalign/priv_debug snapshot、flushPipe 透明边界和四态检查已实现 |
| monitor output | `plan/do/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md` | `cf63e12ebd` | output observation、deferred analysis-port 和 vector-WB gate 已实现 |
| L2TLB lifecycle | `plan/do/mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md` | `e374f39c5d` | permission 字段链、多 outstanding、ordered/reorder 和 stop 清理已实现 |
| LSQ MMIO/status | `plan/do/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md` | `64bed78edc` | commit/deq/cancel 分离、count width、软件/DUT cancel 对账已实现 |
| pending-MMIO | `plan/do/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md` | `7c25383b9a` | MMIO raw、provenance、head query 和 stale owner 处理已实现 |
| DCache L2 response/hint/Probe | `plan/do/mem_ut_v2_l2cache_response_hint_probe_model_coding_plan_20260717.md` | `bace94b6ef` | coherent response、Hint/Probe、GrantAck/E、C assembly 和 drain 已实现 |

每一行的 owner 唯一性均已检查：同一状态没有在总控或相邻专项中复制第二套写者；例如 `scommit`
只作为 ROB 输入，`sqDeq` 只释放 SQ mapping，DUT cancel observed 只做对账，不能再次回退软件
free count；L2TLB agent 仍保持 DTLB -> L2TLB request、L2TLB -> DTLB response 方向。

## 3. 逻辑变化分类复核

### 3.1 字段、宽度和参数适配

以下修改不改变测试框架的主控制目标，主要是把 V2 的接口结构接入同一字段链：

| 范围 | 原有问题 | V2 适配后的结果 |
|---|---|---|
| ROB/LQ/SQ key、FuType、issue port、`sqDeq` count | 固定 V3 literal 或重复 `[1:0]` 宽度 | 从 compile profile 派生，raw/interface/xaction/monitor/XZ 共用同一宏；runtime 只限制行为用量 |
| 主表地址 | issue VA 复用 PADDR 窗口 | `MAIN_VADDR` 只控制虚拟地址生成，TLB PADDR 映射保持独立 |
| V2 split signal 命名 | 聚合 issue/WB 命名和固定 lane 假设 | 采用 `issueLda/Sta/Std`、`writebackLda/Sta/Std` 的 target/lane 映射 |
| CSR、MMIO、Hint sideband | 字段缺失、常量化或 X/Z 被折叠 | 补齐 interface/xaction/raw snapshot 链，按 owner 定义已知值和消费边界 |

### 3.2 新增或改变的功能逻辑

这些修改确实改变了原测试框架的运行期状态流，但都由对应子 plan 明确授权，且没有改变不相关
V3 主流程：

| 功能 | 原有逻辑 | 变更原因 | 修改后逻辑 |
|---|---|---|---|
| LSQ admission | allocation、入队和 issue-ready 近似同拍，并依赖 V3 response 假设 | V2 无 canAccept/response，必须区分 DUT launch 与下一采样边界可见 | 每拍 streaming；按 load/store 6/4 gate 过滤；launch 建 reservation，下一边界完成 sample 后才开放 issue |
| split issue/真实 fired-mask | 聚合 route 或按 valid 推进，redirect 可能误取消已 fire item | V2 有独立 target-local pipe 和真实 handshake | 只有 `valid&&ready` 才记录 fired；已 fire item 不被后续 epoch 回退，未 fire item 才取消 |
| WB/IQ/replay | raw key 缺失时可能伪造事件，ctrl 与 WB 同拍顺序不稳定 | V2 raw 必须保真，并需把动态 issue snapshot 绑定到当前 uid | 先采 raw/current snapshot，再按 IQ-first、deferred ctrl 和 redirect epoch 生成 semantic batch；无法证明必需事件归属时 fatal |
| commit/deq/cancel | fault 混入 normal commit，`scommit` 可能被误当 SQ deq，cancel 可能重复回退 | V2 ROB commit、SQ physical deq 和 cancel 是不同方向/延迟/计数单位 | normal commit、fault convergence、LQ/SQ deq、software cancel 和 DUT observed cancel 分离；各自只有一个状态 owner |
| MMIO pending | MMIO 信号只作为常量或 debug 观察，无法从 ROB head 生成 `pendingMMIOld` | V2 uncache MMIO request 需要 head sideband 和 provenance | monitor raw 通过 active ROB value/epoch 反查 uid，status 保存 tag/source；head query 只输出 sideband，不提前 pass/fail/deq/terminal |
| L2TLB responder | 串行 responder 用 gap 阻塞，可能漏采多笔 request | V2 filter 可多 outstanding，response 可按配置乱序 | 每次真实 request fire 建 token，bounded pending queue 保存 request-time CSR/权限快照，按 ordered/reorder 和 due latency 调度，reset/flush/stop 清理唯一 owner |
| DCache responder | 简化 A-to-D、sideband 随机/常量、无 C/E/Probe 生命周期 | V2 coherent DCache 需要合法 Grant/Release/CBO 和 backpressure | 单 A/D owner、GrantAck/E、request-bound Hint、cached-line Probe、C assembly、四态 fail-fast 和 global-stop drain |

上述功能变化的详细“修改前 -> 原因 -> 修改后 -> 伪代码”均在对应 execution plan 和 implementation
review 中展开；本总控没有发现功能 owner 缺失或同一状态被重复实现。

## 4. Flow 与文档覆盖复核

按用户要求逐项检查了主表生成、非对齐、LSQ/store 地址复用、DCache 回复、ROB commit/LQ/SQ deq、
SBuffer flush、正常/异常 WB、redirect/replay 和 sfence：

| Flow | 适配结论 | 当前落点 |
|---|---|---|
| 主表生成、虚拟地址和非对齐 | 已覆盖；VA 窗口与物理映射解耦，完整访问跨度参与合法地址选择 | main-table VADDR plan/flow |
| LSQ enqueue、LS/store 地址复用 | 已覆盖；V2 slot、6/4 gate、reservation/sample 和不支持组合 fail-fast 已接入 | LSQ enqueue plan/flow |
| issue、正常/异常 writeback | 已覆盖；split target、真实 fired-mask、ROB value-only 反查和 fault 分流已接入 | split/int-WB/IQ plans/flows |
| ROB commit、LQ/SQ commit/deq | 已覆盖；`scommit`、`pending*`、`sqDeq` count 和 pointer presence 解耦 | LSQ MMIO/status plan/flow |
| redirect/replay/cancel | 已覆盖；software/DUT cancel 只对账一次，epoch、snapshot、anchor 和 drain 有 owner | IQ/replay 与 LSQ MMIO/status plans/flows |
| SBuffer flush | 已覆盖当前 V2 smoke 的状态/退出边界；完整 denied/corrupt response 注入仍为 TODO | flushSb flow、LSQ status plan、TODO |
| DCache 回复 | 已覆盖当前轻量 coherent 范围；GrantAck/E、Probe、Release 和 Hint 均有生命周期 | DCache plan/flow、共享 responder flow |
| sfence/CSR | 已覆盖 snapshot、payload 透传和 standalone TLB invalidation；完整 core `flushAfter` 属于 ROB/CtrlBlock，不是 MemBlock standalone TODO | CSR plan、sfence flow、V2 RTL flushPipe flow |

共享 `mem_base_sequence`、DCache driver、virtual sequence drain、同步包和参数入口的修改已同步到
对应的 flow/analysis 文档；旧 plan 已明确“被替代”或移动到 `plan/do`，不存在继续指向旧
`plan/undo` 当前入口的有效引用。

## 5. 集成验证

已复核最后专项的集成日志目录：
`mem_ut/ver/ut/memblock/sim/v2_l2cache_lockstep_20260723/log/`。

| 检查 | 结果 |
|---|---|
| VCS/KDB compile | 通过，0 error；工具日志保留 `LCA_FEATURES_ENABLED` warning，非 DUT/UVM error |
| canonical real smoke | `TEST_PASS`，`UVM_ERROR=0`，`UVM_FATAL=0` |
| Hint=100 directed smoke | `TEST_PASS`，`UVM_ERROR=0`，`UVM_FATAL=0` |
| Probe=100 directed smoke | `TEST_PASS`，`UVM_ERROR=0`，`UVM_FATAL=0` |
| legacy `tc_dispatch_real_smoke` | `TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0` |
| staged diff/旧路径扫描 | `git diff --check` 通过；旧 DCache/总控 `plan/undo` 入口已清理 |

## 6. 保留的 TODO 与已关闭职责边界

以下是明确不属于本轮 scalar V2 测试框架适配的后续工作，不构成总控遗漏：

- vector LS/`issueVldu` 专项；当前 scalar flow 对不支持组合 fail-fast，不静默当作 scalar。
- L2TLB S1/S2 独立 PTE G/U 权限模型；当前最小链路由 `tlb_entry.pte_g/pte_u` 同源驱动。
- DCache/SBuffer `denied/corrupt/PBMT/permission` response 注入和对应 RM/terminal 语义。
- 完整 analysis-port standard transaction producer、RM、scoreboard 和 coverage。

`sfence_bits_flushPipe` 不在上述 TODO 中。完整 core 由 ROB 在提交点产生 `flushAfter`，MemBlock 不从
该 payload 位本地暂停 LSQ 或清理年轻状态；当前 standalone 仅做字段保真和
`sfence.valid` 驱动的 TLB invalidation，已经满足本轮适配范围。未来只有在验证对象扩展到真实
ROB/CtrlBlock/global redirect 时才需要全核集成验证，不应为当前 MemBlock 新建补偿状态机。

## 7. 最终结论

总控 owner、flow、字段链、功能逻辑、文档同步和提交边界均已闭环；当前工作区中与本专项无关的
规则增强、RTL 知识文档和历史 review 批量迁移未被纳入总控提交。独立 subagent `Popper` 已核对
7 个 staged 文件、owner 覆盖、TODO 边界、旧路径和验证事实，结论为 `FINAL PASS`，未发现 blocker。
后续针对 `sfence_bits_flushPipe` 从“未来 TODO”纠正为“MemBlock standalone 已关闭职责边界”的修订，
独立 subagent `Dalton` 再次对照 V2 Decode/Fence FU/ROB/TLB/LSQ Scala 源码复核，结论同样为
`FINAL PASS`：完整 core `flushAfter` 属于 ROB/CtrlBlock，当前测试框架不需要补偿状态机。
