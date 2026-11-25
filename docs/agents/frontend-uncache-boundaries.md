# Frontend InstrUncache 边界行为说明

本文说明 InstrUncache 与 IFU uncache 路径的验证边界。端口名、参数和内部探针
需结合所选 DUT 的 manifest、signal inventory 与生成 RTL 核对。

## 源码入口

- `src/main/scala/xiangshan/frontend/instruncache/InstrUncacheEntry.scala`
- `src/main/scala/xiangshan/frontend/instruncache/Bundles.scala`
- `src/main/scala/xiangshan/frontend/instruncache/Parameters.scala`
- `src/main/scala/xiangshan/frontend/ifu/IfuUncacheUnit.scala`
- `src/main/scala/xiangshan/frontend/ifu/Ifu.scala`
- `src/main/scala/xiangshan/frontend/Bundles.scala` 中的 `ExceptionType`

## 请求与响应归属

Entry 状态为 `Invalid -> RefillReq -> RefillResp -> SendResp`。
只在 `Invalid` 接收新请求，`req.fire` 保存地址与内存属性，TL-A fire 后等待
TL-D，响应消费或 flush 丢弃完成后回到 `Invalid`。

默认 `MmioBusBytes=8`、`nMmioEntry=1`。多 entry 配置下，使用实际 source ID
和仲裁规则关联响应。每笔事务记录原始取指 PC、物理地址、完整 FTQ flag/index、
TL source、属性以及各级 fire，地址相同的重复访问仍需区分事务。

## 地址单位与跨 beat

取指 PC 和 TL 地址按字节解释。`PrunedAddr` 的内部编码与导出位宽需按信号合同
解码。内部 `alignedAddr` 保存去掉 beat offset 的地址，输出 TL-A 时补回低位。
默认配置发出 8B 对齐的 Get，`lgSize=3`。

| 指令位置与类型 | 验证预期 |
| --- | --- |
| RVC 位于 beat 内或末尾半字 | 首次响应即可提供该指令 |
| RVI 未跨 beat | 从对应 offset 取得完整 32 位指令 |
| RVI 起于 beat 最后半字，且未跨页 | 首次响应无 corrupt 时自动请求下一 beat，拼接后半字 |
| RVI 起于页末半字 | 交由 IFU 重新取下一页，检查新的翻译与权限 |

自动 resend 要求跨 beat、未跨页、无 corrupt、非 RVC 且尚未 resending。
检查第二次 A 地址、source、D 归属及两半字拼接，覆盖第二 beat 的异常。

## 跨页与 half-RVI

页边界由 `PageOffsetWidth` 决定。4KiB 页中，`PC & 0xfff == 0xffe`
表示最后一个半字。跨页 RVI 首次响应无 corrupt 时输出 `needResend`。
下一虚拟页可能映射到任意物理页，应重新执行 ITLB/PMP/PMA/PBMT 检查。

IFU 在 S2 保存原始指令 PC，并通过 saved-half 信息完成拼接。
将 half-valid、data、PC 作为有身份的状态跟踪，检查恢复后的 instruction、PC、
FTQ、endOffset 和异常元数据。第二页 fault 需保留原始跨页指令身份。
覆盖第一/第二页的属性组合、RVC/RVI、翻译 fault 和访问权限 fault。

## Corrupt 与 denied

`ExceptionType.fromTileLink` 规定以下关系。

| corrupt | denied | 结果 |
| --- | --- | --- |
| 0 | 0 | 无 TL 异常 |
| 1 | 0 | Hwe |
| 1 | 1 | Af |
| 0 | 1 | 违反响应合同，触发断言 |

分别检查首 beat 与 resend beat，并检查异常后的新请求使用自己的数据和异常状态。
总线异常会阻止正常 resend，异常交付仍需核对 PC、FTQ 与指令 lane。

## Flush 与在途响应

Entry 在 `RefillReq/RefillResp` 收到 flush 时保存 `needFlush`，继续完成总线事务，
到 `SendResp` 丢弃旧响应。已经握手的 A 仍需合法的 D 响应。

flush 与 `SendResp` 同拍的行为还需结合 IFU 接收端检查。
`needFlush` 的寄存路径不能单独证明所有同拍旧响应都被抑制。
覆盖 A 未握手、D 等待、D fire、SendResp 与 flush 的相邻和同拍组合，
检查旧响应没有进入恢复路径的有效交付。

## WFI 与流控

- `wfiReq` 在 `RefillReq` 阻止新的 TL-A valid。
- `wfiSafe` 在 `RefillResp` 为低，表示仍有总线响应待返回。
- A valid 且 ready 为低时，核对地址、size、source 和存在的 user 位稳定。
- IFU 的 `ifuStall` 门控发给 InstrUncache 的请求 valid。
- 恢复 ready 后检查请求最终完成，并用新事务证明状态可继续推进。

## MMIO 提交门控与属性

`IfuUncacheUnit` 包含 `Idle/WaitLastCommit/SendReq/WaitResp`。
`isMmio || Pbmt.isIO(pbmt)` 的请求进入 `WaitLastCommit`。
`isFirstInstr` 或 `emptyAfter` 允许转入 `SendReq`，其中
`emptyAfter = backendEmpty && ibufferEmpty`。PBMT.NC 在 `isMmio=0` 时可直接
进入 `SendReq`。

`memBackTypeMM = !isMmio`，`memPageTypeNC = (pbmt == NC)`。
TL user 字段由配置决定是否存在，缺失时记录实际观测能力。
flush 重置 uncache FSM、data、exception、needResend、paddr 与 finish。
`isMmio/itlbPbmt` 由新请求覆盖，应检查旧属性不会参与新请求的有效发送。

## 验证入口与覆盖

定向场景位于 `src/test/python/Frontend/tests/py/zhaoxinran/uncache/` 和
`src/test/python/Frontend/tests/py/zhaoxinran/mmio/`，共享准备代码位于
`src/test/python/Frontend/tests/py/support/uncache_scenarios.py`。

汇编可构造真实控制流和指令布局。反压、响应异常、flush/WFI 碰撞等场景还需
通过端口驱动与独立 checker 证明实际发生。观察信号时记录采样相位，使用相同
事务的 A/D、IFU 和 IBuffer 证据，避免跨事务拼接子条件。

功能覆盖采用 SampleHub 与 Toffee，定义和目标映射以 canonical registry 为准。
有效 Condition 可形成 HIT，正确性 Checkpoint 与人工验收决定 CLOSED。
具体规则见 [funcov 说明](../../src/test/python/Frontend/env/funcov/README.md)。

切换场景见 [MMIO / Non-MMIO 切换验证](frontend-mmio-nonmmio-switch-risks.md)。
