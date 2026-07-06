# seq_csr_common.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`

## 1. 文件定位与使用场景

该 class 是 plus 参数快照和合法性检查入口。它存在的原因是：dispatch 框架很多地方都需要同一组参数，例如主表数量、op 权重、issue pipe 数、TLB PTE bit 权重、real smoke timeout。如果每个 sequence 自己读 plus，很容易出现读到的值不一致或缺少合法性检查。

输入来自 `plus.sv` 已解析出的 `plus::MEMBLOCK_*` 命令行参数，这些参数可以来自
`default.cfg`、运行命令指定的 testcase preset cfg，或用户追加的 `plus_arg`。输出不是
transaction，而是一组静态配置字段和 getter，供主表生成、LSQ 入队、issue 调度、L2TLB
responder、real smoke loop 使用。

控制逻辑字段包括 `main_trans_num/enq_per_cycle/real_lsq_enq_max/load_pip_num_limit/sta_pip_num_limit/std_pip_num_limit/*_pip_num_random_en/send_pri_mode_en/global_send_pri_en_wt/*_seq_en/*_timeout`。payload 生成字段包括 op 权重、PTE bit 权重、`level` 模式、`pc_base/pc_stride/pdest_base/paddr_base/paddr_range`，它们主要影响 transaction 里携带什么值。

字段按用途分组：

- 初始化：`initialized`、`init_sem`，保证 `init()` 幂等和并发保护。
- 主表/流水线：`main_trans_num`、`use_manual_main_table`、`enq_per_cycle`、`real_lsq_enq_max`、`load_pip_num_limit`、`sta_pip_num_limit`、`std_pip_num_limit`、`*_pip_num_random_en`。
- 真实端口上限：`real_enq_width`、`real_load_pipe_num`、`real_sta_pipe_num`、`real_std_pipe_num`，用于 clamp。
- op 权重：`op_class_int_load_wt`、`op_class_fp_load_wt`、`op_class_store_wt`、`op_class_prefetch_wt`、`op_class_amo_wt`。
- 调度优先级：`send_pri_mode_en`、`global_send_pri_en_wt`、`send_pri_default`、`send_pri_*_wt`、`send_pri_std_default`、`send_pri_std_*_wt`。
- 地址复用：`addr_reuse_en_*_wt`、四类 `addr_reuse_*_wt`、`addr_ref_window_*`，用于随机主表生成期 recent-window 地址复用。
- delay/MDP/元数据：`delay_*_wt`、`mdp_load_wait_wt`、`mdp_storeset_hit_wt`、`load_wait_strict_wt`、`rvc_wt`、`pc_base`、`pc_stride`、`ftq_idx_base`、`pdest_base`、`pdest_range`。
- TLB/PTE 权重：`tlb_pte_{r,w,x,u,g,a,d,n,v}_{1,0}_wt`、`tlb_level_mode`、`tlb_level_fixed_value`、`tlb_level_random_low/high`、`tlb_pte_mode`。
- 地址范围：`paddr_base`、`paddr_range`。
- real smoke 控制：`dispatch_issue_seq_en`、`dispatch_issue_nonblocking_en`、`redirect_seq_en`、`lsqenq_seq_en`、`lsqcommit_seq_en`、`l2tlb_seq_en`、主动主流程 no-progress warning 阈值、ready/drive/flush timeout、`flushsb_seq_en/request_cycle/timeout`、`replay_wait_ptw_en/timeout`、`sta_real_wb_pass_en`、`std_real_wb_pass_en`。

函数/task：

- `init()`：semaphore 保护，首次调用 `load_from_plus()` 和 `validate_and_clamp()`。
- `reload_from_plus()`：在 testcase build phase 中把 Makefile cfg/plus_arg 已解析出的
  `plus::MEMBLOCK_*` 重新快照到 `seq_csr_common`；它只刷新快照，不选择或解析 cfg 文件。
- `is_initialized()`、`check_initialized(caller)`：用于 transaction/randomize 和 getter 防误用。
- `load_from_plus()`：从 `plus::MEMBLOCK_*` 复制配置，并对 int 参数做非负入口检查。
- `validate_and_clamp()`：检查非零、地址溢出、真实端口上限、权重组全 0；对优先级、概率、流水线数量、timeout 做 clamp 或 warning。
- `get_*()`：所有配置读取必须经 getter，getter 内部都会 `check_initialized()`。

## 2. 字段与函数/task 设计原理

`seq_csr_common` 是所有公共参数的快照层。它不是 DUT CSR 虚拟化模型，也不保存运行时 MMU CSR；运行时 CSR 在 `mmu_csr_runtime_state` 中维护。它的设计目标是：testcase 或 plusarg 改完参数后，所有 helper 看到同一份已校验配置。

关键字段按用途分组：

| 字段 | 含义 | 设计原理 |
|---|---|---|
| `initialized`、`init_sem` | 初始化标志和互斥信号量 | 防止多个 sequence 并发调用 `init()` 时重复加载或看到半初始化参数。 |
| `main_trans_num`、`use_manual_main_table` | 主表条目数、是否手动导入主表 | 同一个入口支持随机主表和 directed 主表，便于 smoke 和专项复用。 |
| `enq_per_cycle`、`enq_per_cycle_rand_en`、`load_pip_num_limit`、`sta_pip_num_limit`、`std_pip_num_limit`、`*_pip_num_random_en` | 每拍软件侧期望入队数量和发射数量上限 | 固定模式下 `enq_per_cycle` 必须落在 `[1:real_enq_width]` 内，pipe LIMIT 会被真实 pipe 数 clamp；随机模式下 `get_enq_per_cycle()` 每次在 `[1:real_enq_width]` 内均匀随机，`sample_*_pip_num()` 每拍在 `[1:LIMIT]` 内随机。 |
| `real_lsq_enq_max` | 当前 DUT 每拍 LSQ enqueue 总 slot 统一上限，默认 8 | 作为 LSQ enqueue 队列/slot 宽度镜像，也用于 route 阶段扫描预算；所有相关队列应跟随该统一上限。 |
| `real_enq_width`、`real_load_pipe_num`、`real_sta_pipe_num`、`real_std_pipe_num` | DUT 真实接口宽度 | `real_enq_width` 是历史兼容字段，必须等于 `real_lsq_enq_max`；其他字段分别约束 load/STA/STD 发射 pipe 数。 |
| `op_class_*_wt` | load/store/prefetch/AMO 类型权重 | 主表生成时先随机 op_class，再生成合法 fuType/fuOpType，避免 fuType 和 fuOpType 被独立随机成非法组合。 |
| `send_pri_mode_en`、`global_send_pri_en_wt`、`send_pri_*`、`send_pri_std_*` | 是否启用 send_pri 仲裁、global priority filter 权重、优先级默认值和权重 | `send_pri_mode_en=0` 时主表使用默认 priority，issue 只按 ROB age；`send_pri_mode_en=1` 时主表按权重随机 priority，issue 会比较 priority；`global_send_pri_en_wt` 决定每拍是否进一步跨 LOAD/STA/STD 过滤全局最大 priority。 |
| `addr_reuse_en_1_wt/addr_reuse_en_0_wt`、四类 `addr_reuse_*_wt`、`addr_ref_window_*` | 主表生成期地址复用控制 | 每条随机 transaction 先按 enable 权重决定是否尝试复用，再按 LOAD/STORE_AFTER_LOAD/STORE 四类权重选择场景；recent queue 只保留 uid 距离窗口内候选，避免旧全表扫描选到过远 transaction。 |
| `delay_*_wt` | issue queue ready delay 权重 | 用软件 delay 模拟不同指令进入发射队列后的等待时间，覆盖排队、乱序选择和延迟发射。 |
| `mdp_*`、`rvc_wt`、`pc_base/stride`、`ftq_idx_base`、`pdest_*` | 第二/第三类字段生成参数 | 这些字段不主导 LSQ 入队，但会在发射前补到 agent transaction 中，便于覆盖后端元信息和 MDP 字段。 |
| `tlb_pte_*_wt`、`paddr_base/range` | TLB/PTE 生成参数 | PTE bit 用权重随机，再由 `memblock_tlb_entry::fixup_pte_legal()` 修正合法性；物理地址范围统一控制。 |
| `tlb_level_mode`、`tlb_level_fixed_value`、`tlb_level_random_low/high` | TLB response `level` 生成参数 | 支持固定值、随机和简化推导三种模式，避免把 `level` 永久写死为 `0`。 |
| `tlb_pte_mode` | TLB PTE 修正模式 | 控制 `fixup_pte_legal()` 偏向正常合法页，还是保留更多异常/非法组合。 |
| `dispatch_issue_seq_en`、`lsqenq_seq_en`、`lsqcommit_seq_en`、`l2tlb_seq_en` | real sequence 开关 | software smoke 可以只验证公共状态机，real smoke 再打开真实 agent 驱动。 |
| `dispatch_issue_nonblocking_en` | lintsissue 非阻塞发射开关 | 为 1 时 driver 每个 issue xaction 只采样一次 ready，未 fire item 保留在 issue queue，下一轮重新仲裁；默认 0 保持旧阻塞等待行为。 |
| `active_seq_no_progress_warn_cycles` | 主动主流程无进展 warning 阈值 | LSQENQ、LINTSISSUE、LSQCOMMIT 在 global stop 前常驻运行；连续无 progress 只 warning，不作为退出条件。 |
| `dispatch_ready_timeout`、`lsqenq_ready_timeout`、`redirect_*_timeout`、`flushsb_timeout`、`replay_wait_ptw_timeout` | ready/drive/flush/replay 等待保护 | 这些 timeout 保护真实握手或恢复等待，不承担主动主流程正常退出。 |
| `l2tlb_idle_stop_cycle` | 被动 L2TLB responder 空闲退出阈值 | L2TLB 是被动 responder，不知道总请求数；当前仍用连续 idle 周期作为自身退出条件。 |
| `redirect_seq_en`、`redirect_drive_timeout`、`redirect_freeze_timeout` | redirect 回灌和恢复控制 | memoryViolation/redirect event 需要先真实驱动 `io.redirect`，再 apply 软件 flush；timeout 使用 dispatch service-cycle。若关闭 redirect sequence 后仍出现 redirect event，recovery handler 会 fatal。 |
| `flushsb_seq_en`、`flushsb_request_cycle`、`flushsb_timeout` | directed flushSb 控制 | `flushsb_seq_en` 只授权机制；`flushsb_request_cycle` 非 0 时最小化触发一次 request，并在公共数据层登记 scheduled pending，阻止 real smoke 提前结束；timeout 等待 `sbIsEmpty`。若 LSQ commit sequence 未开启则忽略 request；LSQ commit loop 会把 scheduled/pending `flushSb` 视为 pending work，避免到点前提前 idle 退出。 |
| `replay_wait_ptw_en`、`replay_wait_ptw_timeout` | PTW-back replay 等待控制 | 默认关闭。开启后 STA `hit=0 && flush_state=1` 可先等待 L2TLB response done 或 timeout 再重发。 |
| `lsq_resync_on_mismatch` | LSQ deq mismatch 时 warning 继续或 fatal | 默认 fatal，保证模型与 DUT 不一致时尽早暴露；debug 时可临时放宽。 |
| `tlb_level_*`、`tlb_pte_mode` | TLB entry 细粒度生成模式 | 给 `memblock_tlb_entry` 提供 payload 级控制入口，便于正常页、混合页和异常偏置页共用一套 builder。 |

主要函数/task：

| 函数/task | 参数 | 功能和设计原理 |
|---|---|---|
| `init()` | 无 | 第一次调用时加锁、从 `plus` 加载参数、执行合法性检查。使用 task 是因为内部使用 semaphore。 |
| `reload_from_plus()` | 无 | testcase build phase 在 Makefile cfg/plus_arg 已解析后主动刷新快照。它不加 semaphore，适合 build_phase 中单线程调用；testcase 不应在调用前写 `plus::MEMBLOCK_*` 默认值。 |
| `load_from_plus()` | 无 | 把 `plus` 中的大写配置拷贝到本 class 小写静态字段，并对 int 做非负检查。这样后续 helper 不直接读 plus。 |
| `validate_and_clamp()` | 无 | 对宽度、权重、范围、timeout 做统一检查；非法必然影响正确性的配置 fatal，可自动收敛的配置 clamp 并 warning。 |
| `get_non_negative_int(name, value)` | 配置名、原值 | 统一拦截负数配置，避免负数被 unsigned 转换后变成很大的值。 |
| `fatal_if_zero(name, value)` | 配置名、值 | 对必须非 0 的数量做启动期检查，例如主表大小、真实 pipe 数。 |
| `fatal_if_all_zero2/3/4/5(name, weights...)` | 权重组名、权重值 | 防止随机权重全 0 导致随机选择无定义。 |
| `clamp_int(name, value, min_v, max_v)` | 配置名、可修改值、上下界 | 把用户配置限制在 DUT 或模型支持范围内，并打印 warning 说明被修改。 |
| `get_*()` | 无 | 所有 getter 都先 `check_initialized()`。设计目的是让 helper 必须在参数初始化后工作，避免默认值误用。 |
