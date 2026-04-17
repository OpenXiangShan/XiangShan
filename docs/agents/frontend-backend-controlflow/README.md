# Frontend-Backend Control-Flow Notes

这组文档整理自 `docs/` 中与 frontend / backend 控制流交互直接相关的 markdown，目的是提供一个稳定、可导航的入口，方便后续 agent 或开发者快速建立整体认知。

## 范围

这里聚焦以下主题：

- backend 产生的 `resolve`
- backend 产生并仲裁的 `redirect`
- backend 发给 frontend 的 `commit` / `callRetCommit`
- frontend 直接消费 backend redirect 的路径与风险
- frontend Python 验证环境应如何近似这些 backend 激励

这里不覆盖完整 BPU、FTQ、ICache、ITLB、ROB 设计，也不替代源码阅读。

## 文档地图

- `backend-resolve.md`
  说明 `resolve` 的接口语义、产生位置、汇聚路径、与 `redirect` 的区别，以及它为什么是执行完成事件而不是提交事件；同时说明验证环境中哪些 `resolve` delay 可以随机。
- `backend-redirect.md`
  说明 backend 中 `redirect` 的来源、仲裁方式、`robFlushRedirect` 的触发条件、`level` 语义，以及 `resolve ordering` 与 `redirect ordering` 的真实关系；同时说明验证环境中 `redirect` 应如何受约束地建模。
- `backend-commit-callret.md`
  说明 `commit` 与 `callRetCommit` 的职责分工、发送时机、frontend 消费路径，以及 flush 场景下二者为什么不严格绑定。若涉及 Backend Agent 语义约束，以 `docs/agents/frontend-backend-agent.md` 为准。
- `frontend-backend-redirect-risks.md`
  说明 frontend 直接接 backend redirect 的主路径，并整理当前实现中的主要功能正确性风险、env 已经能硬约束的子集，以及仍需靠 DUT/RTL 验证的问题。

## 建议阅读顺序

1. `backend-resolve.md`
2. `backend-redirect.md`
3. `backend-commit-callret.md`
4. `frontend-backend-redirect-risks.md`

如果你的问题是“某个 CFI 为什么先 resolve / 后 redirect”，先看前两篇。  
如果你的问题是“为什么某条 call/ret 已经上报，但 FTQ commit 还没推进”，先看第三篇。  
如果你的问题是“frontend 接 redirect 后哪里最脆弱”，先看第四篇。

如果你的问题是“验证环境里这三组接口该怎么打激励、delay 能不能随机”，先看
`docs/agents/frontend-backend-agent.md` 的语义约束，再把这里的
`Verification Model` 小节当作补充背景。

如果你的问题是“`backend_model.py` 太长，先从哪里读、当前实现有哪些高风险点”，
请直接看：

- `docs/agents/frontend-backend-model-review.md`

## 源文档对应关系

本目录内容主要整理自下列源文档：

- `docs/backend_resolve_timing.md`
- `docs/backend_resolve_redirect_relationship.md`
- `docs/backend_resolve_redirect_ordering_case.md`
- `docs/backend_frontend_callret_commit_timing.md`
- `docs/rob_flush_redirect_conditions.md`
- `docs/backend_redirect_level.md`
- `docs/frontend_backend_redirect_risks.md`

## 一页结论

- `resolve` 是 branch/jump 执行完成后的真实结果上报，不是 ROB / FTQ 提交事件。
- `redirect` 是 backend 认为必须立刻修正前端路径时发出的控制动作，不等同于 `resolve`。
- 同一条 CFI 常常同时产生 `resolve` 和局部 `redirect`，但 backend 最终采用的 `redirect` 还要经过全局仲裁。
- backend 不会因为“还有更老的 branch 尚未 resolve”就必然压住更年轻 CFI 的 redirect；它只在当前已经产生的 redirect 候选里选最老者。
- 普通 `commit` 负责推进 frontend 的 FTQ 提交边界；`callRetCommit` 负责把 ROB 已提交指令里的 call/ret 信息上报给 frontend/BPU/RAS。
- 对 Backend Agent 而言，`commit` / `callRetCommit` / `resolve` / `redirect` 的语义边界由 `docs/agents/frontend-backend-agent.md` 统一定义；这里的控制流文档主要补充 RTL 语境和时序背景。
- frontend 对 backend redirect 的接收路径存在几类高价值风险点，尤其是 `ftqIdxAhead` 配对、backend fault 归属和 `ResolveQueue` 的固定 3 拍隔离窗口。
- 对验证环境来说，`resolve` 适合做区间随机 delay，`redirect` 只适合做受因果约束的小范围 delay，`commit` 一般不应建成独立随机 delay 通道。
