# Backend Resolve Notes

## 主题结论

`resolve` 在 backend 中表示 branch/jump 指令执行后得到的真实控制流结果。  
它是执行完成事件，不是 ROB commit、FTQ commit 或 RAS commit 事件。

## 接口语义

`CtrlBlock` 对 frontend/FTQ 输出：

```scala
val resolve = Vec(backendParams.BrhCnt, Valid(new Resolve))
```

`Resolve` 结构携带的核心字段包括：

- `ftqIdx`
- `ftqOffset`
- `pc`
- `target`
- `taken`
- `mispredict`
- `attribute`

因此从 backend 视角看，`resolve` 回答的是：这条 CFI 实际属于哪个 FTQ 位置、真实跳到哪里、是否 taken、以及它是否错判。

## 产生位置

`resolve` 不是由 ROB 产生，而是由控制流执行单元在执行完成时直接产生。

条件分支来自 `BranchUnit`：

```scala
io.toFrontendBJUResolve.get.valid := io.out.valid
```

跳转来自 `JumpUnit`：

```scala
io.toFrontendBJUResolve.get.valid := io.out.valid && !JumpOpType.jumpOpisAuipc(func)
```

直接结论：

- branch 执行完成就会上报 `resolve`
- jump/jal/jalr 执行完成就会上报 `resolve`
- `auipc` 不走这条 `resolve` 路径
- `resolve` 不是“只有 mispredict 才有”

## backend 内部路径

每个 branch/jump FU 先产生各自的 `toFrontendBJUResolve`。  
随后在 `ExeUnit` 内汇聚为每个执行单元一条 `resolve`：

```scala
val resolveVec = VecInit(bjus.map(_.io.toFrontendBJUResolve.get))
resolve.valid := resolveVec.map(_.valid).reduce(_ || _)
resolve.bits := Mux1H(resolveVec.map(_.valid), resolveVec.map(_.bits))
```

再往上是：

1. `ExeUnit` 汇聚
2. `ExuBlock` 拼接
3. `Region` 透传
4. `CtrlBlock` 直接输出

`CtrlBlock` 的关键行为是基本直通：

```scala
io.frontend.toFtq.resolve := io.fromBJUResolve
```

所以这条路径的特点很明确：

- 不按 ROB commit 对齐
- 不额外打一拍
- 不做复杂仲裁
- 尽快把执行结果往前端送

## 与 redirect 的关系

`resolve` 和 `redirect` 常常同源，但不是同一件事。

可以用一句话区分：

- `resolve`：真实结果上报
- `redirect`：立刻修正前端路径的动作

对条件分支，`resolve.valid` 比 `redirect.valid` 更宽：

```scala
redirect.valid := io.out.valid && (isMisPred || redirect.bits.hasBackendFault)
io.toFrontendBJUResolve.get.valid := io.out.valid
```

这意味着：

- 预测正确时，经常只有 `resolve`，没有 `redirect`
- 预测错误时，可能同拍同时有 `resolve` 和 `redirect`

对 jump，`resolve.bits.mispredict` 与 `redirect.bits.isMisPred` 也不完全等价，因为训练条件和立即 redirect 条件已经分叉。

## 时序语义

从 backend 内部看，`resolve` 的时间基准是 FU 执行完成，而不是提交阶段。

一个实用时间线是：

1. CFI 进入 branch/jump FU
2. FU 算出真实 `taken/target`
3. 同拍生成 `resolve`
4. 若需要修正路径，同拍也可能生成局部 `redirect`
5. `resolve` 快速汇聚并由 `CtrlBlock` 送出
6. 更晚阶段，这条指令若真正 ROB commit，才会出现 `callRetCommit`

因此通常可以认为：

- `resolve` 早于 `callRetCommit`
- `resolve` 也通常早于 ROB 级 flush/commit 处理

## resolve ordering 的正确理解

`resolve` 反映的是执行完成顺序，而不是程序序、ROB 序或 FTQ 提交序。

因此如果同一个 FTQ entry 里有多个 CFI：

- 更年轻的 CFI 可以先执行完成
- 它的 `resolve` 可以先出现
- 它的 `mispredict` 字段保留它自己的训练结果

这件事本身不依赖更老 CFI 是否已经 resolve。

## Verification Model

如果验证环境要近似真实 backend，`resolve` 是最适合做范围随机 delay 的接口。

原因是：

- 真实 backend 里 `resolve` 来源于执行完成事件
- 它不和 ROB commit 对齐
- 多条 CFI 的 `resolve` 顺序本来就可能受执行完成顺序影响

因此验证环境中可以接受下面的建模策略：

- 为每条 CFI 分配一个区间随机 delay
- 允许更年轻的 CFI 先于更老的 CFI 发出 `resolve`
- 允许 `resolve` 和最终采用的 `redirect` 不同拍

但必须维持下面的不变量：

- `resolve` 不能早于该 CFI 在模型中被识别/发射
- `resolve.pc` / `target` / `ftqIdx` / `ftqOffset` 必须已经是这条 CFI 的真实结果，而不是占位值
- 如果某条 `redirect` 是由该条 `resolve` 的 mispredict 推导出来，则这个 `redirect` 不能早于该 `resolve`
- 不要把 `resolve` 绑定成严格程序序输出

如果讨论的是 Backend Agent 的规范语义，还需要额外满足
`docs/agents/frontend-backend-agent.md` 中的要求：

- 正确路径上的每条 CFI 都必须最终产生 `resolve`
- 错误路径上的 CFI 在被 `redirect` flush 之前可以发，也可以不发
- `resolve` 本身不要求严格保序

## Current Env Mapping

若需要查看当前 Python env 的具体实现，请直接阅读
`src/test/python/Frontend/env/` 下的代码；但实现细节不应覆盖
`docs/agents/frontend-backend-agent.md` 定义的语义约束。

阅读顺序建议是：

1. 先按本文理解 RTL 中 `resolve` 的来源、时序和与 `redirect` 的关系
2. 再按 `docs/agents/frontend-backend-agent.md` 理解验证环境必须保持的 `resolve` 语义
3. 最后再核对当前 env 的实现是否满足这些语义

## 不要把 resolve 理解成什么

下面这些理解都是不准确的：

- `resolve` 是 ROB commit 的另一种表示
- `resolve` 只有 mispredict 时才出现
- `resolve` 必须按程序序出现
- `resolve` 与最终采用的 `redirect` 一定同拍、同顺序

## 源文档

- `docs/backend_resolve_timing.md`
- `docs/backend_resolve_redirect_relationship.md`
- `docs/backend_resolve_redirect_ordering_case.md`
