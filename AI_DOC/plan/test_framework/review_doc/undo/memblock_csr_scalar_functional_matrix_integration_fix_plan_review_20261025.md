# Memblock CSR Scalar 10k 功能覆盖矩阵集成修复 Plan Review

## 1. 专有名词与评审范围

- **plus-file directory**：runner 读取 plus cfg 的源目录，不是可由用户直接指定的任意 cfg 文件。
- **canonical path**：消除相对段和 symlink 后的物理绝对路径，用于目录与 cfg containment 检查。
- **frozen cfg**：结果目录内的只读 cfg 快照，compile 和 simulation 只消费该副本。
- **profile**：覆盖矩阵中受控的目录与 runtime override 策略组合。

评审对象：

`AI_DOC/plan/test_framework/plan/do/memblock_csr_scalar_functional_matrix_integration_fix_plan_20261025.md`

评审重点：`--plus-file` 安全边界、默认兼容、cfg 冻结和审计、矩阵 26+4 路由、CSR scalar 精确计数、
dry-run/fake 测试、shell 可移植性，以及禁止修改 RTL/Scala/SystemVerilog 的范围约束。

## 2. 独立 Review 调用记录

2026-10-25 已创建三个独立只读 Codex review session：

- `01a17303-ee0c-7ca1-9390-b8e912a7633c`
- `01a17304-625d-7bd2-bae1-dfda8380d9ab`
- `01a17305-2273-7b70-8f6d-ad8037be6399`

三个 session 均在输出任何评审结论前因 provider `429 Too Many Requests` 中止。该状态不构成
`REVIEW_PASS` 或 `REVIEW_FAIL`，不得用于解除 coding 门禁。后续必须继续发起独立 review，直到获得
明确结论。

## 3. 主 Agent 预审

主 agent 的本地预审不替代独立 review，仅记录当前已满足项：

1. plan 第一正文章包含全文术语表和修改对象抽象职责。
2. `normalize_plus_file_dir()`、`resolve_cfg_file()`、`resolve_case_profile()` 和两个主流程均先给出抽象
   功能，再给出源码级或顺序伪代码。
3. cfg 输入固定为“已验证目录 + 受限 cfg 名 + `.cfg`”，并计划检查真实文件目标父目录，未开放任意
   文件名入口。
4. compile/simulation 继续只消费冻结副本；`config.txt` 增加 source/frozen 审计链。
5. 普通 26 项与最后四项的 `TRANS_NUM` 策略分离，四个精确值均列入验收。
6. 测试覆盖默认目录、绝对/相对目录、目录逃逸、四个 dry-run、矩阵参数路由和既有 controller 回归。
7. plan 明确禁止修改 RTL、Scala、SystemVerilog、cfg、sequence 和 RM。

## 4. 当前结论

2026-10-25 后续独立只读 review 尝试：

- Claude Sonnet/Haiku 均完成进程启动，但上游返回 `502 Upstream access forbidden`，未形成 verdict。
- Codex 默认模型和显式 `gpt-5.4` 均在请求阶段返回 `429 Too Many Requests`，未形成 verdict。
- Qwen CLI 返回 rc=1 且没有 reviewer 输出；eda01 上的 CLI 因节点 glibc/libstdc++ 版本不足无法启动。

上述均为 reviewer 基础设施失败，不构成 `REVIEW_PASS` 或 `REVIEW_FAIL`。主 agent 的 checklist 结果只能
作为预审记录，不能替代独立 reviewer verdict，也不能标记为“静态 closure”或解除 review 门禁。

**结论**：`NO_VERDICT`。本文件不再使用机械 `REVIEW_PASS`；后续实现已由用户显式授予测试框架 coding
权限，但最终合入仍必须取得独立 implementation `REVIEW_PASS`。

**门禁状态**：实现已完成，等待修正独立 implementation review 指出的问题并复审。
