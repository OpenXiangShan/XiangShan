# Frontend-Backend Control-Flow Notes

这组文档整理自 `docs/` 中与 frontend / backend 控制流交互直接相关的 markdown，目的是提供一个稳定、可导航的入口，方便后续 agent 或开发者快速建立整体认知。

## 范围

这里聚焦以下主题：

- backend 产生的 `resolve`
- backend 产生并仲裁的 `redirect`
- backend 发给 frontend 的 FTQ-entry `commit`，以及 FTQ 到 BPU/RAS 的 commit meta
- frontend 直接消费 backend redirect 的路径与风险
- frontend Python 验证环境应如何近似这些 backend 激励

这里不覆盖完整 BPU、FTQ、ICache、ITLB、ROB 设计，也不替代源码阅读。

## 文档地图

- `backend-resolve.md`
  说明 `resolve` 的接口语义、产生位置、汇聚路径、与 `redirect` 的区别，以及它为什么是执行完成事件而不是提交事件；同时说明验证环境中哪些 `resolve` delay 可以随机。
- `backend-redirect.md`
  说明 backend 中 `redirect` 的来源、仲裁方式、`robFlushRedirect` 的触发条件、`level` 语义，以及 `resolve ordering` 与 `redirect ordering` 的真实关系；同时说明验证环境中 `redirect` 应如何受约束地建模。
- `../frontend-backend-model.md`
  说明 instruction commit、FTQ-entry commit 和 FTQ 到 BPU/RAS 的提交反馈。
- `../submodule-design/frontend-ftq.md`
  说明 FTQ 指针、commit meta 和实际 RTL 消费路径。
- `frontend-backend-redirect-risks.md`
  说明 frontend 直接接 backend redirect 的主路径，并整理当前实现中的主要功能正确性风险、env 已经能硬约束的子集，以及仍需靠 DUT/RTL 验证的问题。

## 建议阅读顺序

1. `backend-resolve.md`
2. `backend-redirect.md`
3. `../frontend-backend-model.md`
4. `frontend-backend-redirect-risks.md`

CFI 的执行完成和 redirect 仲裁看前两篇。提交条件及 BPU/RAS 的 commit meta
看第三篇和 FTQ 说明。redirect 接收路径的验证关注点看第四篇。

如果你的问题是“验证环境里这三组接口该怎么打激励、delay 能不能随机”，先看
`docs/agents/frontend-backend-model.md` 的语义约束，再把这里的
`Verification Model` 小节当作补充背景。

如果你的问题是“`backend_model.py` 太长，先从哪里读、当前实现有哪些高风险点”，
请直接看：

- `docs/agents/frontend-backend-model.md`

## 源码入口

- `src/main/scala/xiangshan/backend/CtrlBlock.scala`
- `src/main/scala/xiangshan/frontend/ftq/Ftq.scala`
- `src/main/scala/xiangshan/frontend/ftq/BackendRedirectReceiver.scala`
- `src/main/scala/xiangshan/frontend/ftq/ResolveQueue.scala`

## 一页结论

- `resolve` 是 branch/jump 执行完成后的真实结果上报，不是 ROB / FTQ 提交事件。
- `redirect` 是 backend 认为必须立刻修正前端路径时发出的控制动作，不等同于 `resolve`。
- 同一条 CFI 常常同时产生 `resolve` 和局部 `redirect`，但 backend 最终采用的 `redirect` 还要经过全局仲裁。
- backend 不会因为“还有更老的 branch 尚未 resolve”就必然压住更年轻 CFI 的 redirect；它只在当前已经产生的 redirect 候选里选最老者。
- backend `commit` 推进 FTQ 的提交边界，FTQ 按 entry 发送保存的 commit meta 给 BPU/RAS。
- 对 BackendModel 而言，`commit` / `resolve` / `redirect` 的语义边界由 `docs/agents/frontend-backend-model.md` 统一定义；这里的控制流文档主要补充 RTL 语境和时序背景。
- frontend 对 backend redirect 的接收路径存在几类高价值风险点，尤其是 `ftqIdxAhead` 配对、backend flag 归属和 `ResolveQueue` 的固定 3 拍隔离窗口。
- 对验证环境来说，`resolve` 适合做区间随机 delay，`redirect` 只适合做受因果约束的小范围 delay，`commit` 一般不应建成独立随机 delay 通道。
