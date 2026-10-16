# Landing pad 的 Chisel 源码阅读顺序

更新日期：2026-10-16。相关代码已补中文注释；搜 `LPAD 阅读` 可以定位入口。本次源码修改均为注释，功能基线不变。

先固定一个例子，假设 LPE=1、x10 指向 4 字节对齐的 target，前端预测目标正确：

```asm
    lui  x7, 0x12345
    jalr x1, 0(x10)
target:
    lpad 0x12345
    add  x12, x13, x14
```

带着这三个问题读代码：谁让“下一条必须是 LPAD”？谁取得 x7 并比较标签？如果中断或误预测，谁保存/恢复期待状态？

## 第一遍：沿着主线读，先看链接附近的中文注释

| 顺序 | 代码入口 | 读完应能回答 |
| --- | --- | --- |
| 1 | [Bundle.scala：ZicfilpInfo](../src/main/scala/xiangshan/Bundle.scala#L87) | `ZicfilpJalr` 和 `ZicfilpLPADValid` 分别表示什么？它们为何没有标签数据？ |
| 2 | [DecodeStage.scala：编码识别与控制选择](../src/main/scala/xiangshan/backend/vector/Decoder/DecodeStage.scala#L214)，再向前看 [Spec 输入接线](../src/main/scala/xiangshan/backend/vector/Decoder/DecodeStage.scala#L123) | 哪些 JALR 要求 LPAD？AUIPC rd=0 如何变成读 x7 的检查 uop？valid/fire 为什么限定 firstUop？ |
| 3 | [SpecZicfilp.scala：从接口读到寄存器更新](../src/main/scala/xiangshan/backend/decode/SpecZicfilp.scala#L8) | 哪个是寄存器？两条 Wire 链分别干什么？同拍和阻塞例子的状态是什么？ |
| 4 | [Rename.scala：RAT 返回的编号](../src/main/scala/xiangshan/backend/rename/Rename.scala#L556)，然后看 [同拍编号旁路](../src/main/scala/xiangshan/backend/rename/Rename.scala#L752) | LPAD 怎么绑定正确的 x7 版本 p42？`jalr x7,0(x10)` 后为什么应读返回地址？ |
| 5 | [BypassNetwork.scala：操作数数据选择](../src/main/scala/xiangshan/backend/datapath/BypassNetwork.scala#L205)，再看后面的 `isLPAD/immBJU` | 哪根线是实际 x7 数据？标签立即数为什么不能叠加 PC offset？ |
| 6 | [JumpUnit.scala：输入、异常比较与分支门控](../src/main/scala/xiangshan/backend/fu/wrapper/JumpUnit.scala#L12) | 非零标签如何比较？零标签怎么通配？LPAD 为什么不会产生跳转/训练？ |
| 7 | [archZicfilp.scala：提交侧寄存器](../src/main/scala/xiangshan/backend/archZicfilp.scala#L8)，再看 [CtrlBlock 的提交接线](../src/main/scala/xiangshan/backend/CtrlBlock.scala#L459) | specELP=0 时，为什么 archELP 仍可能=1？多条提交怎么依序更新？ |
| 8 | [Rob.scala：异常边界快照](../src/main/scala/xiangshan/backend/rob/Rob.scala#L908)，然后看 [NewCSR 的快照选择](../src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala#L873) | 清零实时 archELP 后，CSR 怎么仍能保存 trap 前的 1？ |
| 9 | [NewCSR.scala：返回恢复值](../src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala#L1191)，然后看 [CtrlBlock 的 redirect 恢复](../src/main/scala/xiangshan/backend/CtrlBlock.scala#L523) | xRET 使用哪个模式的 LPE？JALR 误预测后为什么重新设置 specELP？ |

第 2 步看 Decode 的整体输入/输出，第 3 步进入状态模块；这两处可以来回对照。第 4 步的 Rename 文件较长，先读两处链接附近的注释和紧接的几条赋值，不必通读整个模块。

## 第二遍：把主线中的具体连线补齐

| 想继续追的细节 | 代码入口 | 重点 |
| --- | --- | --- |
| AUIPC 的基础译码 | [ScalaUopTable.scala](../src/main/scala/xiangshan/backend/vector/Decoder/Uop/ScalaUopTable.scala)，[DecodeChannels.scala](../src/main/scala/xiangshan/backend/vector/Decoder/DecodeChannels.scala) | AUIPC 命中 Simple；当前 Pseudo 表无 LPAD；后处理再选 JumpUnit |
| falseInsts | [CanRobCompressField.scala](../src/main/scala/xiangshan/backend/vector/Decoder/DecodeFields/SimpleDecodeChannel/CanRobCompressField.scala) | 它是某个控制字段取 0 的表项集合，不是非法指令或通道选择 |
| stall 的保持条件 | [DecodeChannels.scala](../src/main/scala/xiangshan/backend/vector/Decoder/DecodeChannels.scala)，[CtrlBlock.scala](../src/main/scala/xiangshan/backend/CtrlBlock.scala) 中的 `decodePipeRename`，[PipelineConnect.scala](../utility/src/main/scala/utility/PipelineConnect.scala#L100) | buffer 写使能、输入 ready、payload 锁存；组合 LPADValid 持续为 1 不等于重复接收 |
| 等待 x7 就绪 | [BusyTable.scala](../src/main/scala/xiangshan/backend/rename/BusyTable.scala)，[EntryBundles.scala](../src/main/scala/xiangshan/backend/issue/EntryBundles.scala) | psrc 对应的 busy 位、srcState、wakeup 与发射条件；这是单条依赖等待 |
| 实际寄存器读 | [DataPath.scala](../src/main/scala/xiangshan/backend/datapath/DataPath.scala) 中的 `intRFReadArbiter` | psrc 作为物理寄存器读地址，使用已有端口仲裁 |
| 标签立即数 | [DecodeUnit.scala](../src/main/scala/xiangshan/backend/decode/DecodeUnit.scala) 的 `Imm_U`，[FuConfig.scala](../src/main/scala/xiangshan/backend/fu/FuConfig.scala) 的 `NJmpCfg` | `0x12345017 → 0x12345 → 0x12345000 → [31:12]` |
| 执行身份不丢失 | [Bundles.scala](../src/main/scala/xiangshan/backend/Bundles.scala) 的 `ZicfilpInfos`，[ExeUnit.scala](../src/main/scala/xiangshan/backend/exu/ExeUnit.scala) | uop 自己携带 LPADValid；手工组装接口时也要显式连上 |
| 异常如何到 ROB | [ExceptionGen.scala](../src/main/scala/xiangshan/backend/rob/ExceptionGen.scala) 的 `jmp_wb` | JumpUnit 比较错误加入最老异常选择，错误路径异常可被取消 |
| ROB 如何记住类别 | [RobBundles.scala](../src/main/scala/xiangshan/backend/rob/RobBundles.scala)，[CompressUnit.scala](../src/main/scala/xiangshan/backend/rename/CompressUnit.scala) | 入队/提交传递分类，不能压掉多个 ELP 改变边界 |
| trap 保存及 xRET 出口 | [TrapEntryMEvent.scala](../src/main/scala/xiangshan/backend/fu/NewCSR/CSREvents/TrapEntryMEvent.scala)，[CSR.scala](../src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala) | 快照写 MPELP，tval=2；合法返回携带已算好的恢复值 |
| enable 从哪里来 | [NewCSR.scala](../src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala) 的 `enableZicfilp`、`zicfilpLPEChange` | 当前模式 LPE 选择；LPE 改变时 flush 已译码的年轻指令 |

## 读代码时先分清这四种量

| 代码中的量 | 含义 | 示例 |
| --- | --- | --- |
| `speculativeELP` / `archELP` | 两个不同边界的 1 位状态 | 0 或 1 |
| `lsrc(0)` / `psrc(0)` | 架构编号 / 物理编号 | x7 / p42 |
| JumpUnit 的 `src` / `imm` | 实际 x7 数据 / 重建的立即数 | `0x12345000` / `0x12345000` |
| `ZicfilpLPADValid` | 属于这条 uop 的检查控制位 | Decode 接收时锁存，之后随 uop 传递 |

当你看到 `when`，先看被赋值的对象：Wire/输出上的覆盖产生本拍组合选择；Reg 的赋值决定下一时钟沿的状态。看到 `for/foldLeft`，先按同拍展开的组合电路理解。看到 `fire`，只表示对应接口完成一次接收，不表示已经执行或提交。
