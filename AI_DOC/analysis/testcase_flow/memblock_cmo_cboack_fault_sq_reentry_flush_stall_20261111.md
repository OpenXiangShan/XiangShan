# V2 冷 CMO 错误响应后的 SQ 重入与排空阻塞分析

首次发现日期：2026-11-11；后端时序补核与责任分类日期：2026-11-12。版本：`mem_ut_uvm_v2`。本文保留专项真实 DUT 用例的原始波形链，并补充独立责任分类结论；不是 RTL 知识库更新，未修改 RTL。正式缺陷记录见 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/buglist/rtl/v2/rtl_buglist_2026-W46_20261109.md`。

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

2026-11-11 初版将责任保留为“后端 pendingst 契约待核对”。2026-11-12 已完成真实后端寄存流水与框架实测输入的两轮独立对照，结论收敛为 StoreQueue 的 CMO 请求资格未消费导致重入，满足 RTL 缺陷正式登记门禁。该结论只覆盖本例；完整后端时序来自源码推导，不能宣称已完成整核复现或全部框架时序验证。独立 review 记录见第7节。

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

### 4.3 后端 pendingst 与异常 redirect 时序补核（2026-11-12）

抽象功能描述：真实后端接收顶层写回，更新 ROB 完成/异常状态后阻止正常提交，并将异常 redirect 送回 MemBlock。框架模拟上述控制接口；判断框架是否违规应以 DUT 输入波形和后端流水为依据，不能要求其在 SQ 内部完成同拍反馈。

本例实测：SQ 内部 `mmioStout.valid` 在410.3～415.3ns有效，于415.3ns握手；顶层 `io_mem_to_ooo_writebackSta_0_valid` 在415.3～420.3ns有效，异常位7为1、ROB0。框架于422.8ns处理该异常，430.3ns实际撤销顶层 pendingst 并提交异常 redirect 意图；顶层 redirect 在440.3ns有效，SQ在445.3ns收到 ROB0、level1 的异常 redirect。没有漏发 redirect，也没有 pendingst 永久未更新。

真实后端源码依据：

- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala:1385`：`otherStoutConnect` 将 SQ 内部写回寄存一拍后交给顶层 STA0。
- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/backend/datapath/WbArbiter.scala:355`、`:375`：内存写回以实际 fire 交给 CtrlBlock，没有在此额外寄存。
- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/backend/CtrlBlock.scala:125`、`:765`：写回延迟一拍后交给 ROB；`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/backend/rob/Rob.scala:254`、`:1107`、`:781`、`:848` 分别对应寄存的队首组、写回状态更新、阻止提交和再次 RegNext 的 pendingst 输出。
- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/backend/rob/ExceptionGen.scala:102`、`:108`、`:154`：异常 s0/s1/current 流水；`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/backend/rob/Rob.scala:635` 产生 flushOut，`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/backend/CtrlBlock.scala:104` 寄存 ROB redirect，`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala:405` 再寄存到 SQ。

内部控制流推导：在本例时钟边沿、无额外背压且队首其它条件就绪时，顶层写回在420.3ns交付，CtrlBlock 延迟结果自该边沿后有效，ROB 队首状态在425.3ns更新，pendingst 最早约430.3ns撤销。ExceptionGen 三阶段约在425.3/430.3/435.3ns推进，顶层redirect约440.3ns、SQ redirect约445.3ns。该推导与本例实测相符，但属于源码时序推导，不是整核仿真实测。

更直接的因果判断：415.3ns时旧启动条件已经被REG_4锁存为真；即使外部pendingst在该边沿之后立即拉低，420.3ns仍可消费旧REG_4。因此不能依赖加快测试框架反馈消除本次重入。

框架依据：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv:695` 排除fault head/fault waiting后才发布pendingst；`:169` 的 `request_fault_head_redirect()` 构造完整 ROB、level1、flush-itself1；`:1095` 调用该函数时没有把 terminal_ready 作为必要条件，不存在已证实的“先排空才允许redirect”循环等待。fault软件终态标记也不等于正常scommit。`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv:104`、`:124` 分别为驱动与间隙保持电平逻辑。框架不是完整逐拍ROB模型，但本例未发现造成首个重入的时序违规。

新增顶层边界提取原文件为 `/tmp/cmo_backend_boundary_20261112.txt`；该文件、原有三个SQ提取文件、两份控制台日志和两个原FSDB均已另存到 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/cmo_plans_20261111_results/cmo_fault_sq_reentry_20261112`，完整来源及SHA-256见 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/cmo_plans_20261111_results/cmo_fault_sq_reentry_20261112/evidence_manifest.json`。原路径仍保留，固定副本用于避免后续复跑覆盖。

## 5. 隔离用例与验收范围

第一版 isolated 用例把年轻 Store 改为第二条 Load但保留 CHECK_STORE。实际 CMO 和两条 Load 的 RM 比较均通过，仍在1570.3ns发送 flushSb 后、6570.3ns观测超时 last_sb_is_empty=0。该现象说明原失败不能全部简化为“年轻 Store 写值错误”；无 Store 时还存在排空残留，其完整内部根因本轮没有继续扩展定位。

当前三指令 isolated 用例删除尾 CHECK_STORE，使用既有DISABLED静态Sv39拓扑（MAIN_TRANS_NUM=3，CONTROL_WORKER_TOPOLOGY_MODE=0）。denied/corrupt两例已由主agent真实VCS执行通过，均确认实际CBOAck首访错误及CMO/两条Load比较、UVM_ERROR/FATAL为0。该精简场景只证明首次 CBOAck 错误选择、CMO 请求身份、最终异常比较及年轻 Load 恢复，不证明 Store 恢复或 flushSb 排空。不得把其 PASS 回填到原四指令场景，原 BLOCKED CSV 项必须保留。

## 6. 对三个方案实现收口的独立意见

CMO RM、统一 denied/corrupt 与首次物理行加权历史的功能代码可按独立软件检查、已完成正常/Uncache真实测试及 isolated 故障检查分别记录实现状态。原故障组合受阻不应通过 RM 特判、清空实际队列或跳过 Store-table 比较规避。

若主 agent 确认三项方案的编码、文档和必要验证均已完成，并明确把“故障后年轻 Store + flushSb 完整恢复”登记为保留的集成阻塞，可以依规则将 plan 作为“编码完成，存在已复现集成阻塞”收口；归档本身不代表全部真实 DUT 验收通过。若原 plan 将这两个四指令场景或完整故障排空列为必须通过的验收条件，则该条件仍未满足，必须保留未满足项，不能仅凭 isolated 通过宣称计划完全验收。

2026-11-12 已完成两轮独立责任分类，按 RTL 类别正式登记。本轮没有新的仿真或修复，原 denied/corrupt 四指令场景仍未通过。后续 RTL 修改仍需用户明确授权，并回到原 cfg/seed 完整复测，不能用 isolated 通过或 RM 吞事件代替排空验收。

## 7. 独立责任分类 review 与最小修复建议（2026-11-12）

核验提交为 `4f1cbd818969587616ce565a1380e35d8b0b6cb2`，仅为当前源码核验基线，不将其等同于此前仿真的编译提交。两轮review均为只读独立核查，没有新编译或新仿真。

### 7.1 第一轮独立 review

Reviewer：`/root/cmo_bug_classify_r1`；日期：2026-11-12；责任分类门禁：PASS，归类RTL。

核验范围：StoreQueue Scala与生成RTL的REG_4、实际SQ/顶层输入提取、真实CtrlBlock/ROB/ExceptionGen/MemBlock流水以及框架fault提交/redirect路径。独立结论：420.3ns重入早于真实后端最早可反馈的时刻；旧pending资格未消费、fault只保存在uncacheUop、fault WB直接回idle共同形成重复请求。实测430.3ns撤销pendingst、440.3ns顶层redirect、445.3nsSQ redirect，不能作为框架漏反馈证据。

初次建议还包含idle入口当前资格与completed门控；经最短握手流水及地址补写优先级补核，reviewer明确修正：全live门控不是本例最小修复的必需项。主方案在现有请求消费分支补入cmoOpReq.fire，足以使资格在返回idle前传过RegNext。此修正保留为方案收敛过程，不伪称初次即无分歧。

### 7.2 第二轮独立 review

Reviewer：`/root/cmo_bug_classify_r2`；日期：2026-11-12；责任分类门禁：PASS，归类RTL；正式记录草稿复核亦为PASS。该轮在第一轮结束后开展，独立重读源码并尝试反证，不依赖上一轮责任结论。

独立推导：CtrlBlock写回寄存、ROB队首寄存和pendingst的RegNext，使真实后端最早约430.3ns撤销pendingst；异常选择、ROB flush和两级redirect寄存使SQ最早约445.3ns收到redirect。即使不依赖这些精确时刻，真实后端必须先接收415.3～420.3ns顶层异常，已经不能阻止420.3ns重入。

排除项：框架确有异常redirect；fault软件rob_commit不等于物理scommit；422.8ns之后的fault token等待不能造成更早的420.3ns重入；driver保持pending level不等于违规；responder已应答额外请求。另核对 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/backend/decode/DecodeUnit.scala:479` 和第103/104行默认值，clean未设置noSpec/blockBack，不能用该译码禁止年轻Store来否定场景。

最小方案独立意见为在nonzero CMO的io.cmoOpReq.fire分支清pending，或等价地合并进现有mmioDoReq清理条件；不扩展到zero的每段资格门控。该意见为静态建议，尚无修复后验证。

### 7.3 建议修改与验证边界

功能职责：实际接受一次特殊请求时消费一次启动资格，等待响应和写回期间继续持有请求身份及异常，不允许同一动态项重新从idle启动。

建议源码位置：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:1054`，将现有 `when(mmioDoReq)` 清pending条件扩为 `when(mmioDoReq || io.cmoOpReq.fire)`，其他状态与payload逻辑保持。内部时序：E0请求握手并清pending、进入resp；最早E1响应握手进入wb且REG_4已变0；最早E2写回握手回idle；E3不再有旧启动资格。只在fault WB清pending则不足以排空上一拍寄存条件。

保持边界：deqCanDoCbo不依赖pending，不会因消费资格而关闭response.ready；仅fire清理，背压时保持；普通MMIO保留原机制；zero多分段由resp直接回req，不在每段检查pending；正常CMO仍等scommit；异常、redirect与真实写路径不被吞掉或伪造。地址补写先于正常CMO启动，同拍消费清零优先，不增加未经证明必要的idle组合路径。

当前状态为待修复。后续须取得RTL修改授权，再在原denied/corrupt四项主表、seed666666上证明单次请求/完成、无年轻Store额外Uncache请求、恢复后Store正常排空和CHECK_STORE实际通过，并覆盖正常CMO、MMIO、zero分段、最短响应、背压和redirect边界。本轮不改变原CSV的未通过结果。

正式周记录：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/buglist/rtl/v2/rtl_buglist_2026-W46_20261109.md`。日志、FSDB与提取的防覆盖副本清单为 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/cmo_plans_20261111_results/cmo_fault_sq_reentry_20261112/evidence_manifest.json`，保留原始发现时间、原始路径与源码核验hash；详细逐拍证据和修复建议以该周记录为正式入口。
