# dispatch_plan_v2 当前实现 review

审查时间：2026-05-19

审查基准：

- 目标方案：`AI_DOC/plan/test_framework/review_doc/undo/dispatch_plan_v2_review_annotated.md`
- 实现记录：`AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`
- 实现范围：`mem_ut/ver/ut/memblock` 下 dispatch 公共测试框架相关 sequence、testcase、plus、connect、rule 文档。

## 总体结论

当前实现已经形成可工作的 dispatch 公共测试框架骨架，覆盖了公共数据 owner、主表/状态表/TLB 表、plus 参数快照、软件 LSQ 模型、TLB builder、load/STA/STD 发射队列、issue 字段赋值、真实 agent sequence、monitor event adapter、writeback/replay/redirect handler、commit handler、L2TLB responder，以及一组软件和真实 DUT smoke testcase。按 `dispatch_plan_v2_review_annotated.md` 的模块目标看，基础闭环和 staged smoke 验证已经达到较高完成度，适合作为后续专项扩展的公共底座。

但当前还不是完整 v2 目标的全覆盖实现。主要缺口集中在真实 DUT replay/redirect/memory violation 闭环、异常/TLB fault/DCache denied/corrupt 真实专项、特殊访存类别的系统性覆盖，以及 flush/cancel/refetch 等边界协议验证。现有真实 smoke 更偏向正常路径、真实入队/发射/写回/提交的严格或兼容闭环；软件 replay smoke 已覆盖状态机 replay-only 逻辑，但不能替代真实 DUT replay/redirect/memory violation 验收。

## 已实现功能清单

### 公共数据、主表、状态表、TLB 表

- `common_data_transaction` 已作为全局数据 owner，维护主表、状态表、TLB 表、CSR runtime snapshot、发射队列、ROB/LQ/SQ/TLB lookup 以及 flush/replay 全局状态，见 `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv:11`。
- `reset_all_tables()` 会重置主表容量、状态数组、TLB 数组、发射队列、raw monitor queue、ROB/LQ/SQ/TLB 映射和 CSR runtime baseline，见 `common_data_transaction.sv:60`。
- target 级状态条件更新 API 已实现 issue_epoch/replay_seq 防迟到反馈机制，`conditional_set_target_status_field()` 对 stale 事件直接忽略；状态表不再维护 uid 级粗粒度 issue snapshot。
- replay 状态支持按目标入口清除局部完成状态并保留另一侧状态，例如 STA replay 只清 STA dispatch/writeback/pass，不清 STD 状态，见 `common_data_transaction.sv:469`。
- 发射队列管理、去重、删除、end check 已实现，见 `common_data_transaction.sv:1212` 和 `common_data_transaction.sv:1346`。

### plus 和 seq_csr_common

- `plus.sv` 已新增 dispatch framework 的 `MEMBLOCK_*` 参数解析，包括真实 issue/enq/commit/L2TLB、STA/STD real writeback strict 开关、real smoke step 上限等，见 `mem_ut/ver/ut/memblock/env/plus.sv:210`。
- `seq_csr_common` 已做静态参数快照、初始化保护、reload、validate/clamp，以及大量 getter 入口，见 `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv:11` 和 `seq_csr_common.sv:110`。
- `MEMBLOCK_STA_REAL_WB_PASS_EN`、`MEMBLOCK_STD_REAL_WB_PASS_EN` 已从 plus 读入并进入 `seq_csr_common` 快照，见 `plus.sv:216`、`seq_csr_common.sv:89`、`seq_csr_common.sv:213`。

### 主表生成和基础 sequence

- `memblock_dispatch_base_sequence` 承担公共初始化、随机主表生成、手动主表导入、地址复用、TLB 构建、队列路由、issue fire、writeback/replay/redirect/commit task 包装等职责，实现在 `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`。
- `main_control_transaction` 已承载 op class、ROB/LQ/SQ、地址、TLB/异常控制位、delay、send priority 等主表一类字段，见 `mem_ut/ver/ut/memblock/seq/base_seq/main_control_transaction.sv`。当前主表不保存 `pc/isRVC/ftqIdx/ftqOffset/pdest/rfWen/fpWen/loadWaitBit` 等第二/第三类后端派生字段；这些字段在发射前由 `issue_field_assigner` 根据 uid、plus 权重或默认策略派生，见 `mem_ut/ver/ut/memblock/seq/base_seq/issue_field_assigner.sv:94` 和 `issue_field_assigner.sv:507`。
- 软件 directed smoke 固定生成 load/store/AMO 三条主表，覆盖公共路径的基本 load、store STA/STD 和 AMO 路由，见 `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv:55`。

### LSQ 模型

- `lsq_ctrl_model` 已实现 load/store/prefetch/CBO/AMO 的行为分类、LQ/SQ 指针和 free count 镜像、LSQ admission 和 commit/deq 辅助。操作分类见 `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv:143`。
- scalar load 分配 LQ，scalar store 分配 SQ 并拆分 STA/STD，AMO 走 STA/STD issue 但当前不消耗 LSQ，见 `lsq_ctrl_model.sv:155`、`lsq_ctrl_model.sv:170`、`lsq_ctrl_model.sv:186`。
- vector LS 当前显式不支持并 fatal，见 `lsq_ctrl_model.sv:149`。

### TLB builder 和 L2TLB responder

- `tlb_map_builder` 已按 uid 生成 TLB entry，设置 vaddr/paddr、PTE bits、CSR runtime state 并注册 lookup key，见 `mem_ut/ver/ut/memblock/seq/base_seq/tlb_map_builder.sv:22`。
- `memblock_l2tlb_base_sequence` 已实现 L2TLB/PTW responder：等待主表、采样 DUT DTLB/L2TLB request 的 `vpn/s2xlate`，查公共 TLB 表并返回 response，见 `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv:81` 和 `memblock_l2tlb_base_sequence.sv:123`。
- `L2tlb_agent_connect.sv` 当前由编译期宏 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制 DTLB/L2TLB response 通路是否接管，默认值为 1；`MEMBLOCK_L2TLB_SEQ_EN` 只控制 responder sequence 是否主动回包，不再控制 connect force/takeover。需要纯观察 DUT 原始 response 时，必须编译期覆盖 takeover 宏为 0。

### 发射队列和字段赋值

- `issue_queue_scheduler` 已实现 route ready 判断、replay target gating、load/STA/STD 三队列选择、send priority、issue fire 标记和 replay_seq 过滤，见 `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv:81`、`issue_queue_scheduler.sv:149`、`issue_queue_scheduler.sv:221`。
- `issue_field_assigner` 已按 load pipe 0-2、STA pipe 3-4、STD pipe 5-6 写入 lintsissue xaction，并补充 PC、ftq、pdest、wait/mdp、store-set 等派生字段，见 `mem_ut/ver/ut/memblock/seq/base_seq/issue_field_assigner.sv`。

### 真实 agent sequence

- `memblock_lsqenq_dispatch_sequence` 可在 plus 使能后等待公共主表，按 `canAccept` 驱动 LSQ admission，完成后构建 TLB 并路由 issue queue，见 `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv:95` 和 `memblock_lsqenq_dispatch_sequence.sv:129`。
- `memblock_lintsissue_dispatch_sequence` 可在 plus 使能后等待主表，驱动 lintsissue item，并在默认兼容路径为普通 scalar store STD 生成 synthetic issue-accept pass；严格路径由 `MEMBLOCK_STD_REAL_WB_PASS_EN` 关闭 synthetic pass，见 `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv:56` 和 `memblock_lintsissue_dispatch_sequence.sv:189`。
- `memblock_lsqcommit_dispatch_sequence` 可驱动当前已暴露的 lsqcommit/pendingPtr 风格 commit xaction，并更新 ROB commit 状态，见 `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv:48` 和 `memblock_lsqcommit_dispatch_sequence.sv:103`。

### monitor adapter、writeback/replay/redirect handler

- `dispatch_monitor_event_adapter` 已将 raw int writeback、IQ feedback、redirect、memory violation、CSR runtime、LQ/SQ deq 转换到公共 handler 或 commit handler，见 `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv:76`、`dispatch_monitor_event_adapter.sv:127`、`dispatch_monitor_event_adapter.sv:183`、`dispatch_monitor_event_adapter.sv:208`。
- STA strict real writeback 路径只过滤 normal STA IQ pass，过滤条件为 `raw.is_sta && raw.hit && MEMBLOCK_STA_REAL_WB_PASS_EN`；IQ feedback 按 `hit=0` 形成 replay，`flush_state` 只作为 TLB/PTW-back 状态元信息，不单独触发 replay，见 `dispatch_monitor_event_adapter.sv:153`。
- `writeback_status_handler` 已区分 normal pass、fault、replay、redirect，并将 replay/redirect/fault 放入 feedback queue 给异常/replay handler 处理，见 `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv:60`。
- `exception_redirect_replay_handler` 已处理 replay/fault/redirect 队列，redirect 选最老事件，replay 调 `mark_replay_pending()`，见 `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv:93` 和 `exception_redirect_replay_handler.sv:121`。

### commit handler

- `lsq_commit_handler` 已被软件 smoke、真实 commit sequence 和 monitor adapter 复用，用于构建 lsqcommit xaction、ROB commit 标记、真实 lq/sq deq 反馈清理。入口在 `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`，调用点包括 `soft_test_memblock_dispatch_smoke_sequence.sv:139`、`memblock_lsqcommit_dispatch_sequence.sv:103`、`dispatch_monitor_event_adapter.sv:238`。

### testcase smoke 和 package 接入

- 软件 smoke `tc_dispatch_smoke` 继承 `tc_smoke`，通过 hook `run_dispatch_smoke_sequence()` 启动软件 sequence，并保留默认安全 sequence 配置，见 `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_dispatch_smoke.sv:19` 和 `tc_dispatch_smoke.sv:24`。
- 软件 replay smoke `tc_dispatch_replay_smoke` 继承 `tc_dispatch_smoke`，只 override hook，不重复启动父类 smoke，见 `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_dispatch_replay_smoke.sv:19`。
- `soft_test_memblock_dispatch_replay_smoke_sequence` 已通过 writeback/replay handler 注入 replay，而不是直接改状态；并检查 STA replay 后只重发 STA、STD pass 保留、旧 issue_epoch/旧 replay_seq stale pass 被忽略，见 `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv:63`、`:68`、`:70`、`:75`。
- 真实 smoke family 已存在 `tc_dispatch_real_smoke`、store smoke、store wb smoke、STA/STD strict smoke、multi-store smoke、mixed load/store smoke 等。当前 sequence/helper 由 `mem_ut/ver/ut/memblock/seq/seq_pkg.sv` 管理，testcase 由 `mem_ut/ver/ut/memblock/tc/tc_pkg.sv` 管理，`cfg/tb.f` 保证 `seq.f` 先于 `tc.f` 编译。
- rule 文档 `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` 已记录 dispatch plus 参数、真实 smoke testcase、STA/STD strict writeback 和 replay smoke 语义，见 `plus_demo_migration_plan.md:85` 和 `plus_demo_migration_plan.md:156`。

## 未实现或未充分实现功能清单

- 真实 DUT replay/redirect/memory violation 闭环尚未完整验收。代码已有 raw monitor 转换和 handler 状态更新，但现有真实 smoke 主要验证 normal path；真实 DUT 触发 replay、redirect、memory violation 后的 refetch、flush、cancel、重发和最终 retire 闭环仍缺专项 testcase。相关框架入口在 `dispatch_monitor_event_adapter.sv:183`、`dispatch_monitor_event_adapter.sv:208`、`exception_redirect_replay_handler.sv:121`，但缺真实 directed 验收。
- memory violation 目前按 redirect 事件进入 handler，缺少真实 load-store 冲突场景、DUT `memoryViolation` 触发、younger/flushItself 覆盖和最终恢复成功的系统性测试。
- redirect/flush 后的 lqCancel/sqCancel、issue freeze ack、发射前二次检查、队列残留清理等边界协议未看到完整真实专项。`end_test_check()` 会检查最终 idle，但不能证明中间协议完整正确。
- 特殊访存覆盖仍有限。AMO/LR/SC/CBO 有分类和 directed AMO smoke 基础，但没有覆盖 AMO/LR/SC/CBO 的真实专项矩阵；MMIO、NC/PBMT、prefetch、跨页/权限组合也未形成真实回归闭环。
- vector load/store 当前明确不支持：`lsq_ctrl_model` 遇到 vector LS fatal，IQ adapter 也 drop vector feedback，见 `lsq_ctrl_model.sv:149`、`dispatch_monitor_event_adapter.sv:133`。
- 异常/TLB fault/DCache denied/corrupt 真实专项未完成。主表和 TLB transaction 携带 `tlbAF/tlbPF/tlbGPF/pmaAF/denied/corrupt` 等字段，writeback handler 可处理 fault，但缺从真实 TLB/DCache 返回到异常/写回/commit 的 directed 验收。
- 异常/DCache 控制字段当前没有随机注入闭环。`main_control_transaction` 定义了 `tlbAF/tlbPF/tlbGPF/pmaAF/corrupt/denied`，但随机主表生成在 `memblock_dispatch_base_sequence::randomize_main_transaction()` 中强制清 0，见 `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv:444`；`plus.sv` 也尚未落完整异常和 DCache response 权重参数。因此当前 smoke 默认只覆盖正常返回，不覆盖异常/DCache 控制组合。
- DCache/sbuffer memory response 仍偏 smoke 支撑，未覆盖 denied/corrupt/replay/uncache/MMIO 等真实响应组合。
- CSR runtime 与 L2TLB responder 已有基础，但 SFENCE、CSR 变化期间 request snapshot、vmid/asid mismatch、strict lookup fault/fatal policy 的边界测试不足。
- 主表合法性校验和 coverage 仍偏轻量。`main_control_transaction` 提供字段与随机化入口，但尚未实现方案中期望的完整字段合法性/约束矩阵和覆盖统计。

## 与原方案存在差异的功能

- 原方案倾向把公共 task 收敛在 base sequence；当前实现采用薄 `memblock_dispatch_base_sequence` 加多个 helper class 的拆分方式。差异原因是降低单文件复杂度并让 handler/model/adapter 可被真实 agent sequence 复用；风险是跨 helper 状态约定更分散，需要 package include 顺序和文档持续同步。该差异整体合理。
- `seq_csr_common::init()` 不只在公共 base sequence 中调用，多个真实 agent sequence 也会自行调用，例如 `memblock_lsqenq_dispatch_sequence.sv:95`、`memblock_lintsissue_dispatch_sequence.sv:56`、`memblock_lsqcommit_dispatch_sequence.sv:48`、`memblock_l2tlb_base_sequence.sv:81`。原因是真实 agent default sequence 可能独立启动；参数 preset 由 Makefile `cfg=<cfg_name>` 在仿真命令中提供，testcase build phase 通过 `seq_csr_common::reload_from_plus()` 刷新快照。当前有 semaphore 和 initialized guard，做法合理。
- 真实 strict writeback testcase 不再通过 testcase build phase 强制 plus 默认值；strict 参数由对应 `seq/plus_cfg/<cfg_name>.cfg` 提供，运行命令或回归 ini 必须显式写 `cfg=<cfg_name>`。Makefile 会把 cfg 文件内容展开成 runtime plusargs，再追加用户 `plus_arg`，让 testcase 源码保持无 preset 逻辑且用户仍可覆盖同名参数。
- 普通 scalar store STD 当前默认等待真实 writeback；只有显式设置 `MEMBLOCK_STD_REAL_WB_PASS_EN=0` 时才使用 synthetic issue-accept pass，见 `memblock_lintsissue_dispatch_sequence.sv:189`。兼容路径仍可用于早期 bring-up，但默认不再掩盖真实 store writeback 路径。
- STA IQ feedback 在 `MEMBLOCK_STA_REAL_WB_PASS_EN=1` 时过滤 `hit=1` 的 normal pass，但 `hit=0` replay 不过滤，见 `dispatch_monitor_event_adapter.sv:153`。该差异是为避免 STA normal IQ feedback 误当真实 writeback，同时保留 replay 信息；`flush_state` 不再单独决定 replay，避免 `hit=1 && flush_state=1` 被误判。
- `memblock_lsqcommit_dispatch_sequence` 当前驱动的是已有 pendingPtr/commit 风格接口，而不是完整方案中所有 commit/release 端口。原因是 TB/RTL 暴露接口限制；风险是真实 commit 协议覆盖不足，但作为 smoke 阶段可接受。
- L2TLB responder 当前连接 DTLB/L2TLB 上游交互点，按 request `vpn/s2xlate` 查表，不再把 L2TLB 写成下游 memory/PTW 模型。该实现与现行 rule 文档一致，和早期口径相比是合理修正。
- send priority 调度实现是 deterministic 的。方案要求 `global_send_pri_en=1` 时取全局最大优先级，相同最大值且分属 load/STA/STD 时可并发；同一队列超过流水线数量时方案允许随机筛选或按较小 `robIdx` 筛选。当前 `issue_queue_scheduler` 先找全局最大 `send_pri`，再分别在 load/STA/STD 队列中选择该优先级候选，队列内 tie-break 由 `item_is_better()` 固定按 ROB oldness/uid，见 `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv:259`、`issue_queue_scheduler.sv:309`、`issue_queue_scheduler.sv:340`。该实现满足“全局最大优先级”和“跨队列并发”的功能语义，但牺牲了同优先级候选的随机性覆盖。
- 主表字段拆分与方案中的字段分层一致但实现位置不同：`pc/isRVC/ftqIdx/loadWaitBit` 等不在 `main_control_transaction` 中持久保存，而是在 `issue_field_assigner` 发射前派生。这样减少主表状态面，但风险是后续若要 debug 或复现第二/第三类字段，需要额外记录派生快照或在日志中增强输出。
- 异常/DCache 控制字段目前是“结构已预留、随机关闭”的状态。主表字段存在，但随机生成强制清零且 plus 权重未落地，见 `memblock_dispatch_base_sequence.sv:444`。这比方案中异常和 DCache response 可配置注入的目标更窄，当前对 normal smoke 合理，但不能作为异常专项完成依据。

## 其他问题和风险

- 文档实现记录很完整，但 `dispatch_plan_v2_review_annotated.md` 的目标范围远大于当前实现。后续使用时应明确“已完成公共框架与 smoke，不代表真实 replay/redirect/memory violation/异常专项已完成”。
- 命名仍有历史痕迹：`mem_base_sequence.sv` 仍是 legacy 文件名，多个新文件头部仍写 `Discribution`。这不影响编译，但会降低文档和代码搜索一致性。
- 历史实现中 `L2tlb_agent_connect.sv` 曾依赖硬编码 `UVM_TESTNAME` 字符串和 `MEMBLOCK_L2TLB_SEQ_EN` 打开 responder 接管路径，新增真实 dispatch testcase 时容易漏加。当前已改为 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 编译期宏统一控制 connect-time 接管，testcase 只通过 cfg/plus 打开 runtime `MEMBLOCK_L2TLB_SEQ_EN`。
- `dispatch_monitor_event_adapter` 对没有 active uid 的 raw 事件默认 drop 并打印 info。该策略对迟到反馈友好，但如果真实事件 lookup key 缺字段或映射漏注册，可能被低噪声吞掉，需要在严格专项中增加统计/断言。
- replay/redirect handler 现阶段缺真实多事件同拍、多个 redirect/replay 优先级、redirect 与 replay 交织的 directed 验收。
- redirect/flush 恢复当前是简化模型。`exception_redirect_replay_handler::process_pending_events()` 在选中 redirect 后立即 `request_redirect_flush()`、设置 `issue_freeze_ack` 并同一 task 内 `apply_redirect_flush()`，见 `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv:131`；`apply_redirect_flush()` 随后直接清理并释放全局 flush 状态，见 `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv:612`。这没有建模真实 issue freeze ack 的下一拍协同，也没有基于真实 `lqCancelCnt/sqCancelCnt` 完整恢复 LQ/SQ 资源，因此只能视为软件状态清理模型。
- L2TLB lookup 目前对 smoke 做了容错 fallback。`lookup_tlb_uid_by_req()` 先按 runtime CSR 生成 full key 查找，失败后扫描 `vpn/s2xlate/csr_update_seq` 匹配项，弱化了 asid/vmid 区分，见 `common_data_transaction.sv:1161`。这对当前单地址空间 smoke 可接受；在 strict ASID/VMID/SFENCE 专项前应收紧为 full key 匹配，或至少对 fallback 命中加断言/统计。
- `seq_csr_common` 已有 semaphore，但 `reload_from_plus()` 是 function 且未加同一 semaphore。当前仅在 testcase build phase 用于刷新 Makefile cfg/plusarg 快照，风险低；若后续运行期动态 reload，需要重新审视并发安全。
- `plus_demo_migration_plan.md` 已记录大多数新增参数和 testcase，但建议补充一段“当前未覆盖项/非目标项”，避免读者把 smoke 验收误读为完整专项完成。

## 文档状态和提交注意事项

- 本 review 文档是本轮新增文件，当前在 git 状态中表现为 untracked：`AI_DOC/plan/test_framework/review_doc/undo/dispatch_plan_v2_implementation_review.md`。父 agent 后续如果要保存该结论，需要显式 `git add AI_DOC/plan/test_framework/review_doc/undo/dispatch_plan_v2_implementation_review.md` 并纳入对应 commit。
- 本轮只审查并记录现状，没有修改 `mem_ut` 实现代码；文档中的风险和差异项不代表已修复。

## 建议后续任务优先级

### P0

- 新增真实 DUT replay smoke：由 DUT raw IQ feedback 或真实 backend replay 触发 replay，检查只重发目标入口、旧 issue_epoch/replay_seq pass 被忽略、最终真实写回/commit 成功。
- 新增真实 redirect/memory violation smoke：构造可触发 memoryViolation 或 redirect 的场景，验收 flush/cancel/refetch、younger 清理、队列残留、最终 end_test_check。
- 把 strict real store STA/STD smoke、mixed load/store strict smoke、软件 replay smoke 纳入固定验收命令，防止 synthetic path 回退掩盖真实写回路径。

### P1

- 异常/TLB fault/DCache denied/corrupt 真实专项：分别覆盖 TLB AF/PF/GPF、pmaAF、DCache denied/corrupt 到 writeback/fault/commit 的闭环。
- 扩展特殊访存 directed regression：AMO、LR/SC、CBO、prefetch、MMIO、NC/PBMT，至少先覆盖 scalar real smoke 的关键组合。
- 强化 L2TLB responder 边界：strict lookup、missing entry policy、CSR update_seq/asid/vmid 变化、SFENCE/refetch 期间 lookup 行为。

### P2

- 引入 coverage/scoreboard 统计：按 op class、target、pass/fault/replay/redirect、真实/合成来源分类统计。
- 清理文档和命名一致性：修正 `Discribution`、整理 legacy `mem_base_sequence` 与 dispatch base sequence 的边界说明。
- 若后续支持更高并发 sequence，统一 `seq_csr_common` init/reload 的并发约束。
- 规划 vector load/store 支持；当前为显式 unsupported，应在文档中持续标明。

## 建议验收命令

以下命令建议从 `mem_ut/ver/ut/memblock/sim` 执行，优先使用远端 flow：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_dispatch_smoke mode=base_fun
make eda_run tc=tc_dispatch_replay_smoke mode=base_fun
make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke
make eda_run tc=tc_dispatch_real_store_smoke mode=base_fun cfg=tc_dispatch_real_store_smoke
make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke
make eda_run tc=tc_dispatch_real_store_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_sta_wb_smoke
make eda_run tc=tc_dispatch_real_multi_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_multi_store_wb_smoke
make eda_run tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke
make eda_run tc=tc_dispatch_real_mixed_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_sta_wb_smoke
```

通过标准：每个 testcase 日志中 `TEST CASE PASSED`，且 `UVM_ERROR`、`UVM_FATAL` 均为 0。P0 新增真实 replay/redirect/memory violation 后，应补充对应 testcase 并加入同一远端回归清单。
