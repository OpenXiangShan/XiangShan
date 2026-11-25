# Frontend MMIO / Non-MMIO 切换验证

本文维护 cacheable、PBMT.NC、PBMT.IO 和 PMA MMIO 之间切换时的验证要求。
具体路径需结合有效翻译、PMP/PMA 配置与所选 DUT 的接口合同确认。

## 源码入口

- `src/main/scala/xiangshan/frontend/ifu/Ifu.scala`
- `src/main/scala/xiangshan/frontend/ifu/IfuUncacheUnit.scala`
- `src/main/scala/xiangshan/frontend/instruncache/InstrUncacheEntry.scala`
- `src/main/scala/xiangshan/frontend/ftq/BackendRedirectReceiver.scala`
- `src/main/scala/xiangshan/frontend/ftq/IfuRedirectReceiver.scala`

基础边界见 [InstrUncache 说明](frontend-uncache-boundaries.md)。

## 路径和提交门控

| 场景 | 检查要求 |
| --- | --- |
| Cacheable | 确认有效属性选择 ICache 路径，核对对应 refill/hit 与交付 |
| PBMT.NC 且非 PMA MMIO | 确认进入 uncache，检查可绕过 WaitLastCommit 的请求 |
| PBMT.IO 或 PMA MMIO | 检查 WaitLastCommit，等待首指令条件或 backend/IBuffer 均空 |
| 页表属性与 PMA 组合 | 独立计算有效属性，检查请求携带的实际属性与路径 |

`IfuUncacheUnit` 以 `isMmio || Pbmt.isIO(pbmt)` 决定是否等待提交，
通过 `isFirstInstr || emptyAfter` 离开等待态。
`emptyAfter` 来自 `backendEmpty && ibufferEmpty`。
验证等待态退出、后续 SendReq 及 TL-A fire 的各自周期。

构造 `SEND_REQ && ifuStall` 时选择合法可达的状态。
IBuffer 空与反压条件之间的关系需按实际 RTL 方程核对。
backend 接受控制应根据已观察到的 held cfVec 调整，检查释放反压后的交付顺序。

## Redirect 与在途事务

IFU 的 uncache 事务在 S2 管理，`s2_flush` 传入 `IfuUncacheUnit`。
backend redirect、IFU checker redirect、uncache redirect 的作用级与 FTQ 仲裁
需分别核对，记录内部候选、真正选中的 redirect 和实际生效周期。

每次切换都检查以下身份链。

1. 旧路径 FTQ/PC、已握手的 ICache 或 uncache 请求。
2. redirect 来源、FTQ flag/index、offset、target 和实际驱动周期。
3. flush 后仍需返回的旧总线响应及其丢弃边界。
4. 恢复路径首次请求、属性、响应和有效指令交付。
5. 恢复后连续控制流及 checker/monitor 结果。

覆盖旧 D 与 redirect 同拍、A 等待 ready、自动 resend 等待第二 beat、
响应呈现且 IFU stall、checker 与 backend redirect 同拍等组合。
同拍定义必须包含采样相位和实际 DUT 信号。

## 属性与旧状态隔离

`IfuUncacheUnit` 的 flush 会清除 FSM、data、exception、needResend、paddr 和
finish，`isMmio/itlbPbmt` 在新请求到来时覆盖。
检查有效发送只使用对应请求的新属性，并检查 TL-A 等待期间属性稳定。

`memBackTypeMM = !isMmio`，`memPageTypeNC = (pbmt == NC)`。
对配置存在的 TL user 位进行实读，记录实际路径。
各类请求的数据、异常、needResend 和完成脉冲都需保持事务归属。

## 跨页切换

在页末放置 RVI 低半字，为下一虚拟页配置独立物理页和属性。
覆盖 cacheable、NC、IO、MMIO 之间的双向切换，以及第二页 fault。
页表和 PMP/PMA 配置按测试初始化顺序生效，TLB context 变更使用正确的
SFENCE 范围并确认失效已发生。

跨页检查同时跟踪原始 PC、half-valid/data/PC、FTQ、下一页翻译和最终交付。
第二页物理地址独立解析，异常检查包含原始指令 PC、异常类型、跨页标志和 GPA。
reset 后再应用会被 reset 清除的属性或 trigger 配置。

## 控制流与目标

对 uncache 返回的 JAL、JALR、CALL、RET 分别建立独立指令语义预期。
IFU 内部顺序恢复目标与 backend 基于实际执行计算的架构目标分别检查，
跟踪 source-bound resolve/redirect 以及最终正确目标路径。

backend/checker 同拍仲裁需要核对候选和选中来源，检查年轻交付被清理、
half-RVI 状态有效性和恢复指令连续性。对反压输出按实际接受条件统计消费。

## PBMT.NC 构造与观测

- 配置支持 PBMT 的翻译模式和对应 PBMTE，建立具有目标 PBMT 的有效 PTE。
- 为请求设置可执行 PMP/PMA 属性，并独立计算生效的翻译/权限预期。
- 配置 satp/vsatp/hgatp、ASID/VMID 与 SFENCE 范围，等待目标上下文生效。
- 观察实际 ITLB/PMP/PMA 结果、ICache meta 与 uncache 请求。
- 验证下一页、redirect 和上下文更新后的新请求重新采用正确属性。

观测能力依赖所选构建。必要探针缺失时保留诊断，按对应场景的证据要求处理。

## 用例维护与证据

用例位于 `src/test/python/Frontend/tests/py/zhaoxinran/mmio/`、
`src/test/python/Frontend/tests/py/zhaoxinran/uncache/` 和
`src/test/python/Frontend/tests/py/zhaoxinran/translation/`。
跨域恢复入口为 `test_redirect_flush_recovery_scenarios.py`。

用例保留 seed、程序/trace、DUT manifest、实际探针、采样相位及同一 run 的
日志和波形，存入仓库内的独立运行目录。检查实际 Condition、事务归属、
异常/恢复 Checkpoint 和覆盖目标，按 canonical 规范记录 HIT/CLOSED。
