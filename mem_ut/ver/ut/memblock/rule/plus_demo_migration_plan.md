# plus.sv parameter control migration plan

## Scope

参考 `/nfs/home/lixiangrui/work/xsv_new/scenario/kmh_ut/L2Tlb` 中的
`plus.sv`、`plus_pkg.sv` 和 VCS makefile 接入方式，在 memblock UT 中支持
一套独立的 plusarg 参数化控制入口。

本机制只用于参数化控制，例如 testcase、sequence、model 后续通过
`plus::<field>` 读取运行参数。它不用于配置 memblock 环境组件，不覆盖
`memblock_env_cfg`，也不替代 `user_cfg.local.sv`。

环境组件配置继续使用：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.sv
```

## Implemented Files

新增 plus 参数定义：

```text
mem_ut/ver/ut/memblock/env/plus.sv
```

新增 package wrapper：

```text
mem_ut/ver/ut/memblock/env/plus_pkg.sv
```

新增默认 plusarg cfg：

```text
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
```

更新编译 filelist：

```text
mem_ut/ver/ut/memblock/cfg/tb.f
```

更新 package/test 接入：

```text
mem_ut/ver/ut/memblock/env/memblock_env_pkg.sv
mem_ut/ver/ut/memblock/tc/tc_pkg.sv
mem_ut/ver/ut/memblock/tc/src/tc_base.sv
```

更新 VCS makefile 参数传递：

```text
mem_ut/scr/verif/project_cfg.mk
mem_ut/scr/verif/project_cfg_vcs.mk
mem_ut/ver/ut/memblock/sim/Makefile
mem_ut/ver/ut/memblock/sim/remote_eda_make.sh
```

## Current Plus Fields

当前保留 memblock 通用 demo 参数，同时新增 dispatch framework 使用的
`MEMBLOCK_*` 参数。所有参数仍只用于 testcase、sequence 和 model 参数化，
不接入 agent/env cfg。

| 字段 | 类型 | 默认值 | 说明 |
| --- | --- | --- | --- |
| `plus_memblock_demo_enable` | `bit` | `0` | demo 参数总使能 |
| `plus_memblock_demo_depth` | `int` | `0` | demo 深度参数 |
| `plus_memblock_demo_addr` | `bit [63:0]` | `64'h0` | demo 地址参数 |
| `plus_memblock_demo_data` | `bit [63:0]` | `64'h0` | demo 数据参数 |
| `plus_memblock_demo_stride` | `int` | `1` | demo 步进参数 |
| `UVM_VERBOSITY` | `string` | `"UVM_HIGH"` | 同步读取 UVM verbosity plusarg |

后续新增 testcase/sequence/model 参数时，应添加到 `plus.sv` 和相应
`plus_cfg/*.cfg` 文件中；不要把 agent 开关、driver mode、monitor mode
这类环境组件配置放入 plus。

Dispatch framework 参数分组如下：

| 分组 | 字段 |
| --- | --- |
| 主表与模式 | `MEMBLOCK_MAIN_TRANS_NUM`、`MEMBLOCK_USE_MANUAL_MAIN_TABLE` |
| 入队与流水线runtime使用量 | `MEMBLOCK_ENQ_PER_CYCLE`、`MEMBLOCK_ENQ_PER_CYCLE_RAND_EN`、`MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT`、`MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT`、`MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT`、`MEMBLOCK_LOAD_PIP_NUM_LIMIT`、`MEMBLOCK_STA_PIP_NUM_LIMIT`、`MEMBLOCK_STD_PIP_NUM_LIMIT`、`MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN`、`MEMBLOCK_STA_PIP_NUM_RANDOM_EN`、`MEMBLOCK_STD_PIP_NUM_RANDOM_EN` |
| op class 权重 | `MEMBLOCK_OP_CLASS_INT_LOAD_WT`、`MEMBLOCK_OP_CLASS_FP_LOAD_WT`、`MEMBLOCK_OP_CLASS_STORE_WT`、`MEMBLOCK_OP_CLASS_PREFETCH_WT`、`MEMBLOCK_OP_CLASS_AMO_WT`、`MEMBLOCK_OP_CLASS_CBO_WT` |
| fuOpType 权重 | `MEMBLOCK_LOAD_FUOP_*_WT`、`MEMBLOCK_STORE_FUOP_*_WT`、`MEMBLOCK_PREFETCH_FUOP_*_WT`、`MEMBLOCK_CBO_FUOP_*_WT`、`MEMBLOCK_AMO_FUOP_*_WT` |
| boundary profile 生成 | `MEMBLOCK_BOUNDARY_PROFILE_GEN_EN`、`MEMBLOCK_BOUNDARY_*_WT`、`MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN` |
| LSQ debug | `MEMBLOCK_LSQ_RESYNC_ON_MISMATCH` |
| 发射优先级 | `MEMBLOCK_SEND_PRI_MODE_EN`、`MEMBLOCK_GLOBAL_SEND_PRI_EN_WT`、`MEMBLOCK_SEND_PRI_DEFAULT`、`MEMBLOCK_SEND_PRI_LOW_WT`、`MEMBLOCK_SEND_PRI_MID_WT`、`MEMBLOCK_SEND_PRI_HIGH_WT`、`MEMBLOCK_SEND_PRI_STD_DEFAULT`、`MEMBLOCK_SEND_PRI_STD_LOW_WT`、`MEMBLOCK_SEND_PRI_STD_MID_WT`、`MEMBLOCK_SEND_PRI_STD_HIGH_WT` |
| 地址复用 | `MEMBLOCK_ADDR_REUSE_EN_1_WT`、`MEMBLOCK_ADDR_REUSE_EN_0_WT`、`MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT`、`MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT`、`MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT`、`MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT`、`MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_*_WT`、`MEMBLOCK_ADDR_REF_WINDOW_*`、`MEMBLOCK_ROB_START_*` |
| delay 权重 | `MEMBLOCK_DELAY_0_WT`、`MEMBLOCK_DELAY_1_20_WT`、`MEMBLOCK_DELAY_21_50_WT` |
| 发射前补充字段 | `MEMBLOCK_MDP_LOAD_WAIT_WT`、`MEMBLOCK_MDP_STORESET_HIT_WT`、`MEMBLOCK_LOAD_WAIT_STRICT_WT`、`MEMBLOCK_RVC_WT`、`MEMBLOCK_PC_BASE`、`MEMBLOCK_PC_STRIDE`、`MEMBLOCK_FTQ_IDX_BASE`、`MEMBLOCK_PDEST_BASE`、`MEMBLOCK_PDEST_RANGE` |
| TLB/PTE 权重 | `MEMBLOCK_TLB_PTE_*_WT` |
| 自动主表虚拟地址范围 | `MEMBLOCK_MAIN_VADDR_BASE`、`MEMBLOCK_MAIN_VADDR_RANGE` |
| TLB物理地址映射范围 | `MEMBLOCK_PADDR_BASE`、`MEMBLOCK_PADDR_RANGE` |
| 主动主流程 sequence debug | `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` |
| lintsissue dispatch sequence | `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN`、`MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN`、`MEMBLOCK_DISPATCH_READY_TIMEOUT` |
| lsqenq admission sequence | `MEMBLOCK_LSQENQ_SEQ_EN`、`MEMBLOCK_LSQENQ_READY_TIMEOUT` |
| lsqcommit pendingPtr sequence | `MEMBLOCK_LSQCOMMIT_SEQ_EN` |
| redirect/recovery sequence | `MEMBLOCK_REDIRECT_SEQ_EN`、`MEMBLOCK_REDIRECT_DRIVE_TIMEOUT`、`MEMBLOCK_REDIRECT_FREEZE_TIMEOUT` |
| directed flushSb/PTW replay | `MEMBLOCK_FLUSHSB_SEQ_EN`、`MEMBLOCK_FLUSHSB_REQUEST_CYCLE`、`MEMBLOCK_FLUSHSB_TIMEOUT`、`MEMBLOCK_REPLAY_WAIT_PTW_EN`、`MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT` |
| L2TLB/PTW responder sequence | `MEMBLOCK_L2TLB_SEQ_EN`、`MEMBLOCK_L2TLB_MIN_LATENCY`、`MEMBLOCK_L2TLB_MAX_LATENCY`、`MEMBLOCK_L2TLB_IDLE_STOP_CYCLE` |

`seq_csr_common.sv` 是 sequence 使用的正式读取入口。它从 `plus.sv`
读取最终值后执行合法性检查：非负整型检查、必需非零检查、权重组全0
fatal、优先级/地址复用概率clamp，以及`apply_runtime_resource_limits()`按编译期真实端口数量
集中处理enqueue/pipe runtime行为上限。

DUT物理LSQ enqueue slot和LOAD/STA/STD pipe数量不属于plus参数，统一由
`cfg/memblock_compile_params.svh`的`MEMBLOCK_DUT_*`宏配置，并由
`memblock_dispatch_types.sv`暴露同名typed localparam。五个旧`MEMBLOCK_REAL_*`硬件镜像从
字段定义、加载、default cfg、runtime快照、getter和consumer完整删除，不保留旧名称检测或
兼容wrapper。物理循环直接读取compile localparam，runtime plus只能调节本testcase使用量。

`MEMBLOCK_MAIN_VADDR_BASE/RANGE` 只供 `apply_legal_addr_template()` 生成自动主表 normal
虚拟地址，`MEMBLOCK_PADDR_BASE/RANGE` 继续只供 `tlb_map_builder` 生成翻译后的物理 PPN。
两组默认值相同只用于兼容既有 Bare smoke，不表示 VA 和 PA 窗口继续耦合。manual directed
和 boundary profile 地址生成不受 MAIN_VADDR 参数全局限制。

`MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 是主动主流程 driver 的统一无进展
debug 阈值，覆盖 LSQ enqueue、lintsissue dispatch issue 和 LSQ commit sequence。
达到阈值只打印 `uvm_warning` 并继续运行，不作为正常退出条件；真正卡死由 testcase /
UVM timeout 兜底。

`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN` 默认必须为 0。启用后，
`memblock_issue_dispatch_base_sequence` 会替换 lintsissue agent 的安全默认
sequence，从 `common_data_transaction` 中的 load/STA/STD issue queue 选择候选，
填充 `lintsissue_agent_agent_xaction` 并通过 sequencer 交给 driver。
真实 issue loop 不再使用固定 `max_cycles`、idle stop 或 start timeout 作为正常退出条件；
global stop 前常驻运行，global stop 后退出。等待主表期间也只按
`MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 周期性 warning，不主动 fatal。
`MEMBLOCK_DISPATCH_READY_TIMEOUT` 限制 dispatch 专用 lintsissue item 等待
DUT ready 的最大周期数，避免非法 backpressure 造成 sequence 永久阻塞。
`MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN` 默认必须为 0。置 1 后，
lintsissue driver 每个 xaction 只采样一次 DUT ready，只把真实 `valid&&ready`
的 port 写入 `memblock_dispatch_fired_mask`；未 ready item 不从 issue queue
删除，下一轮继续参与仲裁。该模式下单个 xaction 不再触发
`MEMBLOCK_DISPATCH_READY_TIMEOUT` 长等待，长期无 fire 由主动主流程 no-progress
warning 提示。

`MEMBLOCK_LSQENQ_SEQ_EN` 默认必须为 1，与 `plus.sv` 和 `default.cfg` 保持一致。
`tc_base` 默认安装 `memblock_lsqenq_dispatch_base_sequence`，因此任何构建 dispatch 主表的
testcase 都会自动获得真实 LSQ admission，不需要在每个 directed cfg 中重复打开。没有构建主表时，
sequence 只在 `wait_for_main_table()` 中保持接口 idle，既不随机驱动 LSQENQ，也不修改公共状态；
testcase 主 phase 结束时由 UVM 统一结束 default sequence。需要明确关闭该主动 admission flow 的
bring-up 或隔离场景，可以在对应 cfg/plusarg 中显式置 0；关闭后 sequence 保持 idle 并返回，
不回退父类随机 default sequence。
从已经生成的 `common_data_transaction` 主表按 uid 顺序完成 admission。本轮 V2 scalar
load/store 先用本地 `lsq_ctrl_model` 预览 LQ/SQ key，再由 clock-first driver 把 request
发送一次；V2 顶层没有 LSQ enqueue `canAccept/response`，因此不等待 ready，也不把软件
预测 key 当成 DUT response。launch 后立即预留软件 LSQ 资源，下一 driver 边界才设置
`issue_ready` 并路由 LOAD/STA/STD issue queue。redirect 覆盖 pending batch 时不开放 issue，
仍由原全局 redirect/cancel owner 回退资源。公共参数层仍解析
`MEMBLOCK_LSQENQ_READY_TIMEOUT` 并检查非负，但 V2 sequence 不读取该 getter，也不等待 ready。

`MEMBLOCK_ENQ_PER_CYCLE_RAND_EN=1` 时，ZERO/MIDDLE/MAX 三类权重分别控制主动 idle、
`1..SLOT_NUM-1` 和物理最大 slot 数的类别总概率。MIDDLE 的 `-1` 是 AUTO sentinel，
有效权重派生为 `SLOT_NUM-1`。ZERO类别允许成为唯一非零权重，`1/0/0`表示idle-only；只有三类
全0属于非法配置。zero-only不消费uid或产生terminal，只用于存在外部结束条件的场景；其它随机目标
仍会被连续 uid、V2 load/store 6/4 和实际 LQ/SQ free count继续截断。

`MEMBLOCK_LSQCOMMIT_SEQ_EN` 默认必须为 0。启用后，
`memblock_lsqcommit_dispatch_base_sequence` 会替换 lsqcommit agent 的安全默认
sequence，从 `common_data_transaction` 中选择已 pass 且未 commit 的连续 ROB
条目，把当前可驱动的 `pendingPtr` 推进到本批最后一条。当前 RTL/TB 未暴露
`lcommit/scommit/commit/pendingPtrNext`，因此该参数只控制 pendingPtr 驱动，不伪造
缺失端口。LSQ commit 不再使用固定 `max_cycles`、pending-work 扫描、idle stop 或
start timeout 作为正常退出条件；global stop 前常驻运行，commit candidate 选择仍由
`lsq_commit_handler` 的顺序推进逻辑完成。global stop 后如果还有 scheduled/pending
`flushSb` 或 waiting empty 状态，则继续 drain；flushSb 状态清空后退出。

`MEMBLOCK_REDIRECT_SEQ_EN` 默认必须为 1。该 sequence 是后端 redirect 闭环的一部分，
只在 `common_data_transaction` 的 redirect drive queue 中存在 payload 时才真实驱动
`io.redirect`，空闲时保持 idle，因此默认开启不会主动制造 redirect。关闭该参数只用于
临时隔离 redirect responder，本质上会让 memory violation/redirect directed 场景缺少
DUT 侧回灌闭环。`MEMBLOCK_REDIRECT_DRIVE_TIMEOUT` 是 redirect sequence 本地等待
active payload 的 idle loop timeout；redirect freeze 后等待真实 drive/apply 的保护使用
`MEMBLOCK_REDIRECT_FREEZE_TIMEOUT`，按 dispatch service-cycle 计数。

`MEMBLOCK_FLUSHSB_SEQ_EN` 默认必须为 0。该开关只控制
`memblock_flushsb_base_sequence` 周期 producer 是否运行，不再作为
`common_data_transaction::push_flushsb_request()` 的全局 gate。真正触发 flushSb 有两种方式：
其它 testcase/helper 直接调用 `push_flushsb_request()` 不定时入队，或设置非 0 的
`MEMBLOCK_FLUSHSB_REQUEST_CYCLE` 让周期 producer 按间隔入队。队列消费依赖
`MEMBLOCK_LSQCOMMIT_SEQ_EN=1`，因为 `flushSb` 与 pendingPtr 由同一个
lsqcommit agent 驱动，避免多个 sequence 同时控制同一接口。
`MEMBLOCK_FLUSHSB_REQUEST_CYCLE=0` 表示不自动周期触发，不禁止 directed 入队。
`MEMBLOCK_FLUSHSB_TIMEOUT` 用于 active flushSb 等待 `sbIsEmpty=1` 的
dispatch service-cycle warning；timeout 到达只 warning，不退出 sequence。

`MEMBLOCK_REPLAY_WAIT_PTW_EN` 默认必须为 0。启用后，带 PTW-back 语义的后端 replay
可以先进入 `ptw_wait_replay_q`，等待对应 uid 的 L2TLB responder 真实回过 response
并把 `uid_tlb_record_by_uid[uid].pte_valid` 回填为 1，或达到
`MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT` 后再重新入 issue queue。该机制只节流后端 replay
重新入队，不模拟 MemBlock 内部 `LoadQueueReplay`，也不把 `flush_state` 单独解释成
replay 条件。

`MEMBLOCK_L2TLB_SEQ_EN` 默认值为 1。启用后，
`memblock_l2tlb_base_sequence` 从 L2TLB/PTW agent 采样 `vpn/s2xlate`
request，并通过 `common_data_transaction` 中的 TLB 表回填 response transaction。
当前 `L2tlb_agent_connect.sv` 已连接到 DUT 内部 DTLB/L2TLB 交互点：
`inner_dtlbRepeater.io_ptw_req_0` 和 `inner_ptw.io_tlb_1`。
connect 是否接管由编译期宏 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制，默认值为 1，
表示 mem_ut 默认由 L2TLB agent 接管 DTLB/L2TLB response 通路。默认
`MEMBLOCK_L2TLB_SEQ_EN=1` 时，由 agent driver 模拟 L2TLB 侧 ready/response；
显式设置 `MEMBLOCK_L2TLB_SEQ_EN=0` 时，driver idle 会保持 `ready=0/resp_valid=0`，
不会主动响应 request。
编译期覆盖 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0` 表示关闭 L2TLB agent 接管，
agent interface 保持非激活默认值；该模式不是只观察 DUT 原始 L2TLB/PTW response。
如后续需要保留并观察 DUT 原始 response 通路，应另建 passive monitor 或 mirror 方案。
`MEMBLOCK_L2TLB_MIN_LATENCY` 和
`MEMBLOCK_L2TLB_MAX_LATENCY` 控制 response 延迟范围；查不到表项时由
`get_or_create_tlb_entry_by_req()` 自动创建 by-key TLB entry，不再提供额外的
fault/fatal/idle 缺项策略参数。
`MEMBLOCK_L2TLB_IDLE_STOP_CYCLE` 控制 responder 连续空闲停止条件，连续 idle 超过该阈值才停止。L2TLB responder
不再使用固定 `max_cycles` 退出，不等待 `main_table_ready`，只在 DUT request
到来时查/建 TLB entry 并回包。

`tc_dispatch_real_smoke`、`tc_dispatch_real_store_smoke`、
`tc_dispatch_real_store_wb_smoke`、`tc_dispatch_real_store_sta_wb_smoke`、
`tc_dispatch_real_multi_store_wb_smoke`、`tc_dispatch_real_mixed_wb_smoke` 和
`tc_dispatch_real_mixed_sta_wb_smoke` 是真实联动专用 testcase。运行时应通过 Makefile
`cfg=<cfg_name>` 指定对应 `seq/plus_cfg/<cfg_name>.cfg`，由该 cfg 打开
`MEMBLOCK_L2TLB_SEQ_EN`。connect 层不再解析
`UVM_TESTNAME` 或 `MEMBLOCK_L2TLB_SEQ_EN`；默认接管策略由编译期宏
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 提供。

示例：

```bash
make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke
```

V2 普通 scalar store 的 STD 入口禁止 issue sequence 合成 issue-accept pass，STD pass
必须来自真实 `writebackStd_0/1` monitor fact。V2 不再提供
`MEMBLOCK_STD_REAL_WB_PASS_EN` 参数，也不保留 synthetic pass 兼容路径；真实 STD writeback
无法唯一归一化时由 adapter `uvm_fatal`。
`tc_dispatch_real_multi_store_wb_smoke` 继承严格 store writeback 路径，并固定生成两条
普通 scalar store，用于验证连续 uid/ROB/SQ 的真实 writeback、ROB commit 和 SQ deq 基础流。
`tc_dispatch_real_mixed_wb_smoke` 同样继承严格 store writeback 路径，但通过手动主表固定生成
一条 load 和一条普通 scalar store，用于同时验证 LQ/SQ 分配、真实 load writeback、真实
STD writeback、ROB commit 和 LQ/SQ deq 的组合闭环。

`MEMBLOCK_STA_REAL_WB_PASS_EN` 默认必须为 1。默认路径中，普通 scalar store 的 STA
入口会过滤 STA IQ feedback hit 转成的 normal pass，STA pass 必须来自真实
`io_mem_to_ooo_int_wb` 的 V2 `writebackSta_0/1` monitor
fact。IQ feedback 的 replay 判定按 XiangShan IssueQueue 语义只由 `hit=0` 触发，
`flush_state` 仅作为 TLB/PTW-back 状态元信息，不单独触发 replay。
需要兼容 STA IQ feedback pass 路径时，必须在对应 testcase cfg 或用户 `plus_arg` 中显式设置
`MEMBLOCK_STA_REAL_WB_PASS_EN=0`。
`tc_dispatch_real_store_sta_wb_smoke` 显式保持
`MEMBLOCK_STA_REAL_WB_PASS_EN=1`，并使用 V2 严格 STD real-WB 语义，用于验证普通 store 的
STA/STD 两个入口都来自真实 writeback monitor。
`tc_dispatch_real_mixed_sta_wb_smoke` 在 mixed load/store 场景中强制打开 STA real-WB，
并使用严格 STD real-WB 语义，用于验证同一个 testcase 内 load 真实写回、store STA 真实 writeback、store STD 真实
writeback、ROB commit 和 LQ/SQ deq 的组合闭环。

`tc_dispatch_replay_smoke` 是软件级 dispatch replay 闭环 testcase，不启用真实 DUT
LSQ/L2TLB/lintsissue 联动。它复用软件 directed load/store/AMO 主表，手动注入一次
store STA replay feedback，验证 `replay_seq` 递增、STA 重新入队、旧
`issue_epoch/replay_seq` pass 被忽略、重发后 pass 重新收敛以及最终 commit/deq 清理。
该 testcase 用于保护公共状态机 replay-only 路径，不代表真实 memory violation
redirect/flush 场景。

`memblock_main_dispatch_auto_build_main_table_base_sequence` 只负责生成主表、按 service clock 下降沿
drain monitor fact、处理 replay/redirect 和等待公共状态表收敛；真实 LSQ admission、
lintsissue、lsqcommit 和 L2TLB response 仍由对应 agent default sequence 在 plus
使能后并行执行。真实 smoke 主控循环不再使用 `MEMBLOCK_REAL_SMOKE_MAX_SERVICE_STEPS`
这类软件轮询上限参数。

sequence 参数添加规则：

- base sequence 和 transaction 提取出的场景可调参数可以进入 `plus.sv`。
- 默认 cfg 统一放在 `mem_ut/ver/ut/memblock/seq/plus_cfg`。
- 新增 directed 增强或破坏旧 flow 的使能类参数在 `default.cfg` 中必须默认为 0；用于补齐真实接口闭环且空闲时只 drive idle 的 responder 可以默认开启，但必须在本文档中写清楚不会主动制造激励。
- 新增或删除 `plus.sv` 字段需要重新编译；只修改 `seq/plus_cfg/*.cfg` 或 runtime `plus_arg` 时可在仿真运行阶段生效。
- 每次新增 sequence 参数，都必须同步检查 `mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md` 和对应设计 plan。

## VCS Runtime Flow

memblock 默认 plus cfg 路径：

```text
plus_file=../seq/plus_cfg
cfg=default
```

VCS 运行参数会在 cfg 文件存在时，把 cfg 文件中的 `+...` 行展开成真实 runtime plusargs：

```text
+MEMBLOCK_MAIN_TRANS_NUM=...
+MEMBLOCK_LSQENQ_SEQ_EN=...
```

同时支持额外临时参数：

```text
plus_arg="+plus_memblock_demo_depth=32"
```

`plus_arg` 会被追加到 simv runtime options。

同名 plusarg 的有效优先级为：

```text
plus_arg > ${plus_file}/${cfg}.cfg > plus.sv 内建默认值
```

makefile 会先展开 cfg 文件，再拼接 `plus_arg`。当 `plus_arg` 与
`${plus_file}/${cfg}.cfg` 中存在同名 plusarg 时，以 `plus_arg` 的取值作为
用户显式覆盖值。

远端 `eda_*` flow 会透传：

```text
cfg
plus_file
plus_arg
```

## Usage

默认运行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_sanity mode=base_fun
```

选择其他 plus cfg：

```bash
make eda_run tc=tc_sanity mode=base_fun cfg=my_cfg
```

追加一次性参数：

```bash
make eda_run tc=tc_sanity mode=base_fun plus_arg="+plus_memblock_demo_depth=32"
```

在 SV 中读取：

```systemverilog
if(plus::plus_memblock_demo_enable) begin
    int depth = plus::plus_memblock_demo_depth;
end

seq_csr_common::init();
int unsigned main_num = seq_csr_common::get_main_trans_num();
```

## Non-Goals

- 不复制 L2Tlb 特有的 page-table、TLB、PMP/PMA、LLPTW 或 sequence 控制字段
- 不通过 plus 配置 `memblock_env_cfg`
- 不通过 plus 控制 agent 的 `sqr_sw`、`drv_sw`、`mon_sw`、`xz_sw` 或 `drv_mode`
- 不恢复 `+memblock_user_*` 命令行配置；组件配置仍由 `user_cfg.local.sv` 管理
- 不把 sequence SV 文件放入 `seq/plus_cfg`；该目录只保存 plusarg cfg 文本

## Validation

完成 plus 接入后至少执行：

```bash
rg -n "plus_pkg|class plus|MEMBLOCK_MAIN_TRANS_NUM|seq_csr_common" mem_ut/ver/ut/memblock/env mem_ut/ver/ut/memblock/tc mem_ut/ver/ut/memblock/seq
cd mem_ut/ver/ut/memblock/sim
make -n batch_run tc=tc_sanity mode=base_fun
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun plus_arg="+MEMBLOCK_MAIN_TRANS_NUM=8"
```
