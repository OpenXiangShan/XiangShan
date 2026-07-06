# memblock_lintsissue_dispatch_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`

## 1. 文件定位与使用场景

真实 lintsissue 发射驱动 sequence，继承 `lintsissue_agent_agent_default_sequence`。它负责把 issue queue 里的候选转成 DUT intIssue port transaction。它不决定 uid 是否已经入队或 TLB 是否已建，这些由 scheduler/status 保证；它只在每拍选择候选、填字段、等待 ready，然后通知 scheduler 这些项已 fire。

输入是三类 issue queue 和 `issue_field_assigner`。输出是 `lintsissue_agent_agent_xaction` 和 status 中的 dispatched/issue_epoch/replay mask 更新。

控制逻辑字段包括 pipe 数、非阻塞 issue 开关、ready timeout、queue item 的 target/priority/delay/replay_seq、driver wait ready 开关。xaction 里的地址、ROB/LQ/SQ、pc、pdest 等是 DUT payload。

关键 task/function：

- `drive_dispatch_issue_loop()`：global stop 前每拍 route ready uid、send issue cycle、递减 delay；global stop 后退出。
- `send_issue_cycle()`：创建 `lintsissue_agent_agent_xaction`，清空，设置 wait ready 和 timeout，选择候选，填字段，start/finish item。
- `assign_issue_items()`：按数组 index 作为 pipe_idx；load 对应 0..2，STA 0..1 映射到 port 3/4，STD 0..1 映射到 port 5/6。
- `mark_fired_items()`：根据 driver 回填的 `memblock_dispatch_fired_mask` 标记已 fire 端口；redirect 中止和非阻塞正常返回时都只标记已经 ready/fire 的端口，未 fire 项留在队列中等待后续 flush/replay 或下一轮仲裁处理。
- `item_needs_issue_accept_pass()`：当 `MEMBLOCK_STD_REAL_WB_PASS_EN=0` 且是普通 store STD 时，发射接受后注入兼容 STD issue feedback success。
- `make_issue_accept_pass_event()`、`submit_issue_accept_pass()`：构造 `STD_FEEDBACK + iq_feedback_hit` 事件；handler 在 real STD writeback pass 关闭时才把它转换为 target pass。

## 2. 调度关系与参数数据流

该 sequence 是 LOAD/STA/STD 真正进入 MemBlock 流水线入口的驱动者。它挂在 `env.u_lintsissue_agent_agent.sqr.main_phase`，是否工作由 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN` 控制。

调度关系：

| 阶段 | 输入 | 动作 | 输出 |
|---|---|---|---|
| route ready uid | `common_data_transaction` 状态表 | 调用 scheduler 把 ready uid 放入 LOAD/STA/STD 队列 | issue queue item |
| 仲裁候选 | issue queue、pipe 数、`send_pri`、ROB 年龄、delay | 选择本拍可发射的 item | fired item 列表 |
| 字段赋值 | 主表、状态表、TLB/LQ/SQ key、第二/第三类字段 | `issue_field_assigner` 填 `lintsissue_agent_agent_xaction` | DUT issue payload |
| 驱动接口 | xaction valid bits | driver 等待对应 ready 后完成 item | issue fire |
| 更新状态 | fired item + fire mask | `mark_issue_fire()` 或 `mark_issue_fire_already_accepted()` 更新 dispatched/issue_epoch 并删除队列项 | 后续等待 writeback/feedback |

参数数据流：

- `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN`：总开关。
- `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN`：非阻塞 issue drive 开关；为 1 时每个 xaction 只采样一次 ready，正常返回也以真实 `fired_mask` 为准。
- `MEMBLOCK_LOAD_PIP_NUM_LIMIT`、`MEMBLOCK_STA_PIP_NUM_LIMIT`、`MEMBLOCK_STD_PIP_NUM_LIMIT`：LOAD/STA/STD 每拍最多发射数量上限，会被真实 pipe 数 clamp；调度时通过 `sample_*_pip_num()` 得到本拍数量。
- `MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN`、`MEMBLOCK_STA_PIP_NUM_RANDOM_EN`、`MEMBLOCK_STD_PIP_NUM_RANDOM_EN`：为 0 时本拍数量固定等于对应 LIMIT；为 1 时每拍在 `[1:LIMIT]` 内随机。
- `MEMBLOCK_SEND_PRI_MODE_EN`：开启后主表随机 `send_pri/send_pri_std`，issue 仲裁先比较 priority，再按 ROB age。
- `MEMBLOCK_GLOBAL_SEND_PRI_EN_WT`：send_pri 模式下每拍启用 global priority filter 的权重；采样为 0 时只做各 target 内部 priority 仲裁。
- `MEMBLOCK_DISPATCH_READY_TIMEOUT`：控制 dispatch 专用 item 等待 DUT ready 的最大周期数。
- `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`：控制 issue loop 连续无 fire 时的 warning 周期；只用于 debug，不作为退出条件。正常退出只看顶层置位的 `global_stop_requested`。
- `MEMBLOCK_STA_REAL_WB_PASS_EN`、`MEMBLOCK_STD_REAL_WB_PASS_EN` 不改变发射 payload，但影响发射后 pass 是否可由 feedback/issue accept 兼容闭环。
- `memblock_dispatch_flush_epoch`、`memblock_dispatch_nonblocking_issue` 和 `memblock_dispatch_fired_mask` 是 xaction 内部协同字段。`flush_epoch` 让 driver 在等待或采样 ready 时识别 redirect/flush epoch 变化；`nonblocking_issue` 决定 driver 是阻塞等待所有 valid port，还是只采样一次 ready；`fired_mask` 告诉 sequence 哪些端口已经被 DUT 接受。

DUT payload 数据来源：

- 主表：`fuType/fuOpType/src/imm/vaddr/robIdx/pdest/rfWen/fpWen/pc/isRVC/ftqIdx/ftqOffset` 等。
- LSQ/TLB 状态：`lqIdx/sqIdx`、active key、issue epoch、replay seq。
- 第二类字段：MDP/load wait/store set/first issue 等发射前补充字段。

边界：

- 它不负责 LSQ admission；没有被 `memblock_lsqenq_dispatch_sequence` 激活和路由的 uid 不应进入这里。
- 它不负责判断最终 pass；load/store pass 由 monitor/writeback handler 或兼容 pass 事件推进。
- 普通 store STD 在 `MEMBLOCK_STD_REAL_WB_PASS_EN=0` 时会生成 `STD_FEEDBACK` synthetic issue feedback success，并由 handler 作为兼容 pass 闭环；严格模式下必须等待真实 writeback。
- 若 driver 等待或非阻塞采样 ready 期间发生 redirect，未 ready 端口会被清 valid，已 ready 端口通过 fire mask 补记 dispatch。这样可以避免“已经被 DUT 接受但 TB 没标记”或“未被接受却被错误删除队列”的两类冲突。非阻塞正常路径同样只按真实 `fired_mask` 删除队列项；未 ready item 不分配 issue epoch、不置 dispatched，后续继续参与仲裁。
