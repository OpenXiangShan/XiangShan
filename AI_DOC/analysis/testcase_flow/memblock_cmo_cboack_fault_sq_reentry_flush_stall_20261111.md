# V2 冷 CMO 错误响应后的 SQ 重入与排空阻塞分析

日期：2026-11-11。版本：`mem_ut_uvm_v2`，当前 worktree 的 Scala 与 `build/rtl/StoreQueue.sv`。本文记录专项真实 DUT 用例的实际波形与责任边界，不是 RTL 知识库更新或正式 Buglist 定责；未修改 RTL。

## 1. 术语与分析范围

| 术语 | 本文含义 | 对应对象与例子 |
|---|---|---|
| CMO / CBOAck | cache-block 操作及其下游完成响应 | 本次目标是 uid0 的 clean，目标行 `0x80002000`，响应 denied |
| owner / epoch | 指令动态身份及恢复后实例代次 | 当前 CMO 保持原身份，年轻指令由框架恢复；RM 不创建 epoch |
| SQ | StoreQueue，保存 Store 类操作及其进度 | 目标 CMO 占 SQ0，年轻 Store 占 SQ1 |
| pendingst | 后端声明当前 ROB 头为 Store 类操作 | 本次故障完成后仍保持到 430.3ns；不能把软件观察时间当成 DUT 输入切换时间 |
| rdataPtr / deqPtr | SQ 数据读取位置与实际出队位置 | 第二次错误关联的完成把 rdataPtr 推到2，但年轻 Store 的 deqPtr 仍为1 |
| flushSb / CHECK_STORE | 请求排空实际写路径后做最终 Store-table 比较 | 最后 marker 已发出 flushSb，但观测到 `sbIsEmpty=0`，因此没有最终通过 |
| isolated | 无 writer 的三指令辅助场景 | 只保留 CMO 和两个年轻 Load，不含 CHECK_STORE；覆盖范围小于原场景 |

结论：已证实当前 DUT 在 CMO 故障完成后重新进入 SQ 的请求状态，随后发生保留 CMO 身份与年轻 Store 地址混用、重复完成及读取指针越过恢复后 Store，最终 flushSb 不空。CMO 请求、错误异常及年轻指令的 RM 指令级比较已经通过，原含 Store/CHECK_STORE 场景整体验收仍受阻。

波形能确认 DUT 内部现象及因果链；尚未完成真实后端 `pendingst` 撤销契约与框架采样/驱动延迟的独立对照，不能据此宣称上游测试框架时序已经完全免责。正式责任分类仍需独立 review，本文不登记“已确认 RTL 缺陷”。

## 2. 复现入口及证据

原场景：uid0 CMO=`0x80002000`，uid1 LD=`0x80003000`，uid2 Store=`0x80004000`，uid3 CHECK_STORE。年轻访问刻意使用不同物理行，使目标 CMO 的首次真实下游访问有机会冻结错误结果，避免同一行较早 Load 永久冻结 CLEAN。

已有同 mode 编译产物时，在主仿真目录复现：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=basicTest ts=memblock_dispatch_real_cbo_fault_cold_vseq cfg=tc_dispatch_real_cbo_cold_denied mode=cmo_plans_20261111 seed=666666 wave=on
```

corrupt 入口仅将 `cfg` 改为 `tc_dispatch_real_cbo_cold_corrupt`。本次两例由主 agent 定点停止，不能以没有 UVM_ERROR 代替 TEST CASE PASSED。

真实用例状态见 [CMO 真实 DUT 清单](../../seq/memblock_cmo_rm_real_dut_testcases_20261111.csv) 中 `cmo_cold_cbo_ack_denied`、`cmo_cold_cbo_ack_corrupt` 的 BLOCKED 记录。denied 完整日志暂存于 `/tmp/cmo_cold_denied_run_20261111.log`。

波形路径：

```text
mem_ut/ver/ut/memblock/sim/cmo_plans_20261111/wave/tc=basicTest_ts=memblock_dispatch_real_cbo_fault_cold_vseq_cfg=tc_dispatch_real_cbo_cold_denied_seed=666666_rtl.fsdb
```

本次读取遵循 [fsdb-reader skill](/nfs/home/lixiangrui/.codex/skills/fsdb-reader/SKILL.md) 与 [eda-sim skill](/nfs/home/lixiangrui/.codex/skills/eda-sim/SKILL.md)。实际采用 eda01 上 Verdi NPI Python reader，先 source 当前 worktree 上级 `.envrc`，读取 1ps 时间单位波形：

```bash
ssh eda01 'source /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.envrc; python3 /nfs/home/lixiangrui/.codex/skills/eda-sim/scripts/fsdb_reader.py /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/cmo_plans_20261111/wave/tc=basicTest_ts=memblock_dispatch_real_cbo_fault_cold_vseq_cfg=tc_dispatch_real_cbo_cold_denied_seed=666666_rtl.fsdb --signal top_tb.U_MEMBLOCK.inner_lsq.storeQueue.mmioState --start 380ns --end 620ns'
```

已提取数据暂存 `/tmp/cmo_cold_sq_wave_20261111.txt`、`/tmp/cmo_cold_reg4_wave_20261111.txt`、`/tmp/cmo_cold_sq_late_wave_20261111.txt`；对应提取脚本以 `/tmp/read_cold_` 开头。表中状态值为 idle=0、req=1、resp=2、wb=3、wait=4。

## 3. 已证实的时间链

下表内部信号统一省略 `top_tb.U_MEMBLOCK.inner_lsq.storeQueue.` 前缀。波形时间与框架 monitor 日志时间分别列出，不能互相替代。

| 时间/ns | 波形或日志事实 | 含义 |
|---|---|---|
| 405.3–410.3 | `io_cmoOpResp_valid=1`，denied=1 | 目标真实 CMO 响应产生故障 |
| 410.3 | `mmioState=wb`，`uncacheUop_exceptionVec_7=1`，`io_mmioStout_valid=1` | 原 CMO 故障完成；身份仍为 ROB0、fuOpType=`0x00c` |
| 415.3 | 第一次完成后 `mmioState=idle`，rdataPtr=1；deqPtr仍为0 | 数据读取位置已前进，但出队位置尚处 CMO |
| 415.3–420.3 | `_GEN_370=idle && REG_4=1`；`REG_4` 至425.3才变0 | 上一拍锁存的启动条件再次命中 |
| 420.3 | `mmioState=req`，CMO exception 位清零；deqPtr变为1 | 再次启动使用旧 CMO 身份，当前出队位置随后变成年轻 Store |
| 425.3–430.3 | `io_uncache_req_valid && ready=1`，addr=`0x80004000`，robIdx=2，nc=0 | 原普通 cacheable Store 的地址被错误送入普通 Uncache 请求 |
| 430.3 | `io_rob_pendingst` 从1变0 | 框架抑制 fault head 的结果此时才到 DUT 输入；比重入晚 |
| 445.3–450.3 | `io_brqRedirect_valid=1`，ROB0 | 后续异常 redirect 到 SQ；重入已发生 |
| 460.3 / 495.3 | responder 日志记录地址 `0x80004000` 的 A.fire / D.fire | responder 已完整响应这次请求，没有占住 outstanding 不返回 |
| 520.3–525.3 | 第二次 `io_mmioStout_valid=1`；保留身份仍 ROB0、clean；异常为0 | 同一保留 CMO 身份产生非架构的迟到 normal 完成 |
| 525.3 | rdataPtr变2，`completed_1=1`，state=wait | 第二次完成推进了当前 SQ1 的状态与读取指针 |
| 555.3 | 年轻 Store 恢复入 SQ1，allocated=1，completed重新为0 | 新实例落在已经被读取指针越过的位置 |
| 1540.3–1542.3 | 两条年轻指令正常提交并通过 RM | 指令级正常结果比较通过，不代表实际 Store 已排空 |
| 1555.3 | SQ1 committed=1、completed=0；deqPtr=1、rdataPtr=2 | 年轻 Store 仍在 SQ，读取位置已越过它 |
| 1450–1800 | `io_sbuffer_0_valid=0`、`io_sqDeq=0`、顶层 sbIsEmpty=0 | 没有后续实际出队或 SBuffer 请求完成它 |
| 1555.3 / 6555.3 | flushSb sendover / 超时 warning，last_sb_is_empty=0 | CHECK_STORE 无法开始最终 Store-table 比较 |

此外，框架在422.8ns采集 fault STA0，430.3ns提交异常 redirect intent，431.3ns RM 对异常 `0x80` 比较 PASS；532.8ns由既有严格身份匹配分支吸收迟到 normal STA0。该吸收规则不构成第二次架构提交，也不能修复 SQ 内部读取位置。

## 4. 源码链与责任边界

### 4.1 故障完成后重新启动

抽象功能描述：SQ 的 MMIO/CMO 状态机依据后端头指令资格启动实际请求，保存该指令身份，在下游返回后发出最终 Store 类完成。

源码位置：`src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:845` 附近的 `switch(mmioState)`；生成 RTL 对应 `build/rtl/StoreQueue.sv:11461`、`:41854`、`:53236` 附近。

关键实现是 idle 启动判断使用 `RegNext(pendingst && ROB匹配 && pending && allocated && datavalid && addrvalid && !hasException)`，fault WB fire 直接回 idle。生成的 `REG_4` 每拍锁存启动资格，没有按请求生命周期消耗该资格。故障 response 只设置 `uncacheUop.exceptionVec`，本次 `hasException_0` 始终为0，不能阻止上述启动条件。

内部控制流：第一次故障完成使状态回 idle；仍为真的锁存资格在下一拍重入 req，并清空保留指令的异常；当前 deqPtr 随后前进到普通 Store，`mmioReq.valid` 的“当前 deq 不是 nonzero CBO”条件变真；请求数据和地址来自数据读取位置，而保留的完成身份仍为旧 CMO。实测后续 Uncache 请求与迟到完成符合该链条。

### 4.2 重复完成推进读取位置，redirect 未恢复该位置

抽象功能描述：SQ 的读取位置控制下一条应被送到数据通路的 entry，出队位置只越过已分配且完成的 entry；redirect 取消年轻未提交 entry。

源码位置：同文件 `:318` 附近 `readyReadGoVec`、`:338` 附近 `readyDeqVec`、`:1069` 附近 `io.mmioStout`、`:1496` 附近 `needCancel`。

内部控制流：每次 `mmioStout.fire` 均允许推进 rdataPtr，并把当前 deqPtr 标记 completed；因此第二次保留旧身份的完成仍推进到 rdataPtr=2。redirect 分支清除年轻 entry 的 allocated/completed，当前这段恢复逻辑没有将已越过的读取位置退回。恢复 Store 再入 SQ1 后 completed=0，后续提交也不能让它自动完成，最终实际出队与排空受阻。本文未修改任何上述源码。

### 4.3 上游 pendingst 时序仍需独立核对

抽象功能描述：框架 `lsq_commit_handler` 将软件观察到的 ROB 头状态转换为下一次实际驱动的后端控制输入，fault head 不应继续以正常 pending Store 表示。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv:695` 附近。现有代码明确仅在 `has_head && !has_fault_head && !fault_head_waiting` 等条件成立时置 pendingst。日志表明 fault head 已被识别后停止软件驱动，但 monitor 观察与 driver 到达 DUT 输入存在延迟，本次 DUT pendingst 实际到430.3ns才撤销。

已确认的结论是“当前输入时序下，DUT 出现重复启动并形成错误请求/完成”。尚缺真实后端对该输入的撤销时间要求、fault WB 对 pendingst 的组合/寄存反馈关系以及框架相应延迟是否符合接口约束的独立比对。责任分类应保留为 DUT 状态机与上游时序契约待复核，不能为了令用例通过而让 RM 忽略请求、提前结束排空或直接修改 pendingst 采样。

## 5. 隔离用例与验收范围

第一版 isolated 用例把年轻 Store 改为第二条 Load但保留 CHECK_STORE。实际 CMO 和两条 Load 的 RM 比较均通过，仍在1570.3ns发送 flushSb 后、6570.3ns观测超时 last_sb_is_empty=0。该现象说明原失败不能全部简化为“年轻 Store 写值错误”；无 Store 时还存在排空残留，其完整内部根因本轮没有继续扩展定位。

当前三指令 isolated 用例删除尾 CHECK_STORE，使用既有DISABLED静态Sv39拓扑（MAIN_TRANS_NUM=3，CONTROL_WORKER_TOPOLOGY_MODE=0）。denied/corrupt两例已由主agent真实VCS执行通过，均确认实际CBOAck首访错误及CMO/两条Load比较、UVM_ERROR/FATAL为0。该精简场景只证明首次 CBOAck 错误选择、CMO 请求身份、最终异常比较及年轻 Load 恢复，不证明 Store 恢复或 flushSb 排空。不得把其 PASS 回填到原四指令场景，原 BLOCKED CSV 项必须保留。

## 6. 对三个方案实现收口的独立意见

CMO RM、统一 denied/corrupt 与首次物理行加权历史的功能代码可按独立软件检查、已完成正常/Uncache真实测试及 isolated 故障检查分别记录实现状态。原故障组合受阻不应通过 RM 特判、清空实际队列或跳过 Store-table 比较规避。

若主 agent 确认三项方案的编码、文档和必要验证均已完成，并明确把“故障后年轻 Store + flushSb 完整恢复”登记为保留的集成阻塞，可以依规则将 plan 作为“编码完成，存在已复现集成阻塞”收口；归档本身不代表全部真实 DUT 验收通过。若原 plan 将这两个四指令场景或完整故障排空列为必须通过的验收条件，则该条件仍未满足，必须保留未满足项，不能仅凭 isolated 通过宣称计划完全验收。

正式 Buglist 需先完成独立责任分类；本文仅提供波形证据、源代码链和待确认接口边界。后续修复需按实际责任取得授权，并回到原 denied/corrupt 四指令 seed 完整复测。
