# strict-NC redirect 后旧 NC 写回分析

日期：2026-10-29。母工作区：`.codex_mother_integration_20261028`，生产提交：`c9763bd7d`。
状态：`STOPPED_RTL_CONFIRMED`；两轮独立 review 确认 NC 输出寄存器未按 redirect 清除，责任为 RTL。
已登记 W44 RTL Bug；未获得 RTL coding 授权，未修改 RTL，也不放宽框架写回身份检查。

## 1. 用例与证据

`tc=basicTest`，`ts=memblock_dispatch_real_smoke_vseq`，`cfg=tc_dispatch_real_mmu_sv39_strict_nc_100k`，
seed=`846339364`，临时参数 `MAIN_TRANS_NUM=1000`、`CHECK_TRIGGER_EN=0`，timeout=`1000000000ns`。

母目录 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/` 下：

- 原始批次：`mem_ut/ver/ut/memblock/sim/mother_basic_matrix_20261029_results/tc_dispatch_real_mmu_sv39_strict_nc_100k/`。
  controller 1 FAIL、5 ABORTED、4 NOT_RUN，`remote_unknown_jobs=0`。
- debug 单 seed：`mem_ut/ver/ut/memblock/sim/mother_strict_nc_wb_debug_20261029_results/`，compile PASS，
  1/1 FAIL，在相同 `787.300ns` 复现 `INT_WB_ATTACH no active uid for ROB key=0/14`。
- FSDB：`mem_ut/ver/ut/memblock/sim/mother_strict_nc_wb_debug_20261029/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_strict_nc_100k_seed=846339364_rtl_eda_reg_20261029_121505_3132208_32761.fsdb`。
- 转换 VCD：`mem_ut/ver/ut/memblock/sim/mother_strict_nc_wb_debug_20261029_results/strict_nc_seed846339364.vcd`。
  在 eda01 用 Verdi `platform/linux64/bin/fsdb2vcd` 转换成功；本地 reader 无法打开且 wrapper 缺库。

## 2. 时序

以下取自 VCD，时刻为时钟上升沿后可见值；单位 ns。

| 时刻 | 观测 |
|---:|---|
| 705.3 起 | `top_tb.U_MEMBLOCK._inner_lsq_io_ncOut_1_valid=1`，ROB=0/14，LoadUnit1 nc 输入 ready=0 |
| 745.3 | 框架因 UID0 fault 发起 flush-itself redirect；此前 UID0 已完成匹配的 access-fault 比较 |
| 755.3 | `top_tb.U_MEMBLOCK.io_redirect_valid=1`，ROB=0/0，level=1 |
| 760.3 | `top_tb.U_MEMBLOCK.inner_LoadUnit_1.io_redirect_valid=1`；nc 输入仍 valid，ready=0 |
| 765.3 | NC ROB0/14 输入 valid=1 且 ready=1；内部 redirect 已为 0，S1 kill 仍是 1 |
| 770.3 | NC 已进入 S1：`s1_nc_with_data=1`、`s1_valid=1`、`s1_kill=0` |
| 775.3 | NC 进入 S2：`s2_nc_with_data=1`、`s2_valid=1`、`s2_kill=0` |
| 780.3 | NC 进入 S3，ROB=0/14；LoadUnit1 ldout 与顶层 LDA1 valid |
| 787.3 | adapter 消费整数写回 raw，在 active ROB map 中找不到旧 ROB0/14，报 fatal |

波形证明写回来自 redirect 前已滞留的 NC 返回，并非新的框架 admission/issue 请求。

主 agent 再次直接解析 VCD 中
`top_tb.U_MEMBLOCK.inner_lsq.loadQueue.uncacheBuffer.pipelineReg_3`：700.3ns `io_in_valid=1`，
705.3ns 至 765.3ns `pipeline_reg_valid=io_out_valid=1`、stored ROB value=14；
755.3ns/760.3ns redirect 期间仍保持 valid，765.3ns `io_out_ready=1`，770.3ns 才因握手清除 valid。
因此积压位置已从外围 NC valid 收敛到实际输出寄存器，不只是 monitor 延迟消费旧 raw 事件。
额外核对局部 redirect：`top_tb.U_MEMBLOCK.inner_lsq.loadQueue.uncacheBuffer.io_redirect_valid`
在 755.3ns 仍为 0、760.3ns 为 1（ROB value=0）、765.3ns 返回 0。不能要求该局部寄存器在顶层
redirect 的 755.3ns 提前响应；实际漏取消的局部边界是 760.3ns，旧 valid 随后仍保持到握手。

## 3. 源码候选

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala:493`：NC 输出使用
  `AddPipelineReg(ncOut(i), io.ncOut(i), false.B)`，输出寄存器没有 redirect flush。
- 同文件 `:124`：`canSendReq` 对 `req.nc` 直接允许，只有 MMIO 必须等待 `pendingld && ROB==pendingPtr`。
  所以 ROB14 在 ROB0 fault 之前发出 NC 请求本身合法，不能通过强制 NC 等待队首规避该场景。
- 生成 RTL `build/rtl/LoadQueueUncache.sv:10720,11847`：`pipelineReg_3` 对应 NC lane1 输出，类型为
  `PipelineRegModule_2`；该模块 `build/rtl/PipelineRegModule_2.sv:175` 的 valid 寄存器只由 reset、
  输入/输出握手更新，flush 输入已被常量优化消失，与此次编译使用的 DUT 波形一致。
- `src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala:325,340,513,836`：NC 返回低于正常请求等来源的
  优先级，允许长期 backpressure；返回进入 S0 后按现有流水线推进。
- 同文件 `:1062`：S1 只检查当前和上一拍 redirect；长期被阻塞的旧 NC 在窗口结束后进入时不会被杀。
- `src/main/scala/xiangshan/backend/CtrlBlock.scala:125,154`：写回过滤也是有限拍 redirect 判断，未看到
  全历史取消事实过滤；不能据此声称任意迟到旧 ROB 写回都可安全忽略。
- `src/main/scala/xiangshan/backend/rob/Rob.scala:1033`：写回计数按 ROB value 匹配；真实影响需结合
  allocation 复用和写回到达时机判断，当前 standalone 复现只证明旧写回越过 MemBlock flush。
- `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv:379`：LDA current snapshot
  attach 需要真实 active UID。未确认 RTL/TB 合同前不能新增无条件 stale-drop 或忽略缺失 owner。

## 4. 独立 review 归档

两位 reviewer 均只读，没有修改源码或新建 worktree，日期均为 2026-10-29。

- Galileo，agent=`01a1815b-d3fc-7c41-84b6-6fbe81f2332e`：检查 adapter/活动 owner/redirect 合同，
  独立核对 staging 与最终 LDA 的 ROB0/14、LQ0/7、rfWen=1、pdest=15；当前 UID 数量未触发 pdest
  复用，不能夸大为已观测到污染新指令。检查 WbArbiter/CtrlBlock 后未找到持久旧 WB 消解机制。
  结论：NC 提前执行合法；取消后越过窗口的旧 WB 为 RTL 问题，不能以历史 owner 路径正常放行。
- Dewey，agent=`01a1815b-d437-7d31-822f-fdf7e2e98355`：独立检查 NC entry、输出 staging、LoadUnit
  和 backend 过滤链，确认 RTL 责任；进一步 review 只替换 NC lanes 的局部 valid/data 方案，确认
  stored kill 组合屏蔽 valid、合法 input.fire 优先替换、incoming kill 丢弃，以及无新增组合环。
- 主 agent 重读相应源码并直接解析 VCD 的 `pipelineReg_3`，独立确认 payload 跨 redirect 保持和
  随后进入 LoadUnit 的事实。没有通过修改 cfg、强制 NC 等队首、延长任意窗口或吞事件规避失败。

## 5. 正式记录与下一步

正式 Bug 和源码级方案：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/AI_DOC/buglist/rtl/v2/rtl_buglist_2026-W44_20261026.md`。
当前现有授权只覆盖测试框架；必须取得该 NC staging RTL 修改及必要生成输入的明确 coding 授权，
或由 RTL owner 提供修复版本后，才能重新编译原 seed 专项并恢复矩阵。当前没有活动仿真，覆盖率未收集。
