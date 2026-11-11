# XiangShan 贡献指南

[English](CONTRIBUTING.md) | 中文

## 目录

- [XiangShan 贡献指南](#xiangshan-贡献指南)
  - [目录](#目录)
  - [贡献前须知](#贡献前须知)
  - [Bug 报告](#bug-报告)
    - [部分已知问题说明](#部分已知问题说明)
  - [功能请求](#功能请求)
  - [Pull Request](#pull-request)
    - [提交前](#提交前)
    - [提交信息](#提交信息)
    - [Review/CI 流程](#reviewci-流程)
  - [编译与使用问题](#编译与使用问题)
  - [其余疑问](#其余疑问)
  - [AI 使用](#ai-使用)

## 贡献前须知

首先，感谢您对 XiangShan 的关注和支持！在提交 issue 或 pull request 之前，建议您先完成以下检查：

- 阅读相关文档和手册，例如 [XiangShan 设计文档](https://docs.xiangshan.cc/projects/design/zh-cn) 及 [RISC-V 指令集手册](https://github.com/riscv/riscv-isa-manual)。
- 关于香山维护的多个分支，请参考 [README](README.zh-cn.md#分支维护情况) 了解分支维护和开发状态。
- 搜索已有 issue，尤其是[标签为 `type: example`](https://github.com/OpenXiangShan/XiangShan/issues?q=sort%3Aupdated-desc%20state%3Aclosed%20label%3A%22type%3A%20example%22) 的 FAQ 与示例问题。
- 我们接受中文或英文反馈；出于国际协作的考虑，建议使用英文。
- 我们接受 AI 工具辅助的内容，但存在前提条件，请务必阅读 [AI 使用](#ai-使用) 章节。
- 我们主要通过 [GitHub Issues](https://github.com/OpenXiangShan/XiangShan/issues) 和 [Pull Requests](https://github.com/OpenXiangShan/XiangShan/pulls) 接收贡献、回答疑问。如果您不希望公开，也可以发送到邮件列表 [all@xiangshan.cc](mailto:all@xiangshan.cc)。

## Bug 报告

如果您发现了 **XiangShan RTL 中的缺陷**（例如 difftest 失败），请使用 [`Bug report` 模板](https://github.com/OpenXiangShan/XiangShan/issues/new?template=1-bug_report.yml)。

请尽可能完整、准确地填写 issue 模板要求的内容，包括分支、commit、现象、环境、测试用例等，帮助我们更快地定位问题和提供反馈。

您可以使用 `./scripts/bug-report.sh` 脚本生成所需附件。该脚本会收集系统信息（包括操作系统、内存大小、编译工具链版本等）；上传前请先审查 `bug-report.tar.gz` 中的内容，确认不包含敏感信息。

如 issue 模板中所要求的，请尽量查找是否已有 issue 或 PR 描述/解决了相同的问题。如果有，请在现有 issue 或 PR 中补充信息，而不要新开一个 issue。重复的 issue 将会被关闭，恕不另作说明。

我们欢迎您通过 PR 提交修复，另请参考 [Pull Request](#pull-request) 章节的建议。

### 部分已知问题说明

目前 `kunminghu-v3` 主线分支正处于快速迭代阶段，我们当前主要关注 RVA23 profile 的功能正确性和性能表现。以下方向在现阶段暂不是最高优先级；您仍然可以提交相关问题，但我们可能无法立即处理，敬请理解。

- 侧信道安全（如计时攻击等）。建议作为 [功能请求](#功能请求) 提出，而不是作为 Bug 报告提交。
- 可配置性。我们无法保证所有参数组合都能稳定编译或运行，请尽量使用 `DefaultConfig` 进行测试。
- 向量单元正确性。`kunminghu-v3` 的向量单元经历了完整重构，当前以提升性能为主要目标，在部分边界情况下可能仍存在功能问题。若您遇到相关问题，请先确认其在最新主线上是否仍可复现，并检查是否已有待合入 PR 可解决该问题。

另外，对于模糊测试，请注意程序自修改行为可能引起 difftest 失败。这是因为香山 RTL 的 ICache 采用软件一致性，需要通过 `fence.i` 指令显式同步；而 NEMU 没有实现 cache 结构，存数与取指都会直接访问内存。因此，所有自动生成的 store 类指令都需要满足以下条件之一：1）不修改代码段；或 2）在修改代码段后执行 `fence.i`。否则，出现 difftest 失败属于符合预期的行为，请不要将其作为 Bug 提交。

本节内容时效性较强，如果您认为内容可能过时，欢迎在 issue 中提出，我们会尽快更新。

更新日期：2026/06/30

## 功能请求

如果您希望提出 **新特性、设计改进或流程优化建议**，请使用 [`Feature request` 模板](https://github.com/OpenXiangShan/XiangShan/issues/new?template=2-feature_request.yaml)。

我们欢迎您通过 PR 提交新特性，另请参考 [Pull Request](#pull-request) 章节的建议。

## Pull Request

### 提交前

1. 在正式提交 PR 之前，请先提交一个 issue 与我们和社区讨论，以避免重复工作。
   1. 修复 bug 的 PR -> [Bug 报告 issue](#bug-报告)
   2. 实现新特性的 PR -> [功能请求 issue](#功能请求)
2. 和 issue 的要求类似，请搜索确认是否已有 PR 解决了或正在解决相同的问题。如果有，请在现有 PR 中补充信息，而不要新开一个 PR。重复的 PR 将会被关闭，恕不另作说明。
   1. 例外情况：如果您认为现有 PR 并未以合适的方式解决问题，您可以在与社区讨论后提交自己的 PR。同时，请在新 PR 中附上现有 PR 的编号（单起一行 `Supersede #123456`），并说明为什么新方案更好。
3. 请在 commit 前运行 `make reformat` 自动格式化代码。
4. 请将每个 commit / PR 的改动控制在单一主题内，避免把多个不相关的功能修改混在同一个 PR 中。
   1. 单个 PR 可以包含功能修复/改进及其必要的前置重构（如整理端口结构），但请将它们拆分到不同的 commit 中以便 review。
5. 请将您的 git `user.name` 和 `user.email` 配置为您在 GitHub 上的用户名和邮箱（使用 GitHub 匿名邮箱也可以）。
   1. 请参考 [官方文档](https://docs.github.com/en/account-and-profile/how-tos/email-preferences/setting-your-commit-email-address)。
   2. 运行 `git config user.{name,email}` 检查当前配置。
   3. 如果需要修改配置，请运行 `git config user.name "Your Name"` 和 `git config user.email "your.email@example.com"`。

### 提交信息

本仓库基本采用[约定式提交](https://www.conventionalcommits.org/zh-hans/v1.0.0/)。commit 与 PR 标题应遵循 `type(scope): description` 的格式。

常见的 `type` 包括但不限于：

- `feat`：新增特性。
- `fix`：修复功能或性能问题。
- `perf`：性能调优（注意：性能特性通常应归为 `feat`，性能缺陷修复通常应归为 `fix`，仅参数调优等少数情况建议使用 `perf`）。
- `area` / `timing` / `power`：面向 PPA 的专项优化。
- `build` / `ci` / `chore`：构建、持续集成与杂项维护。
- `docs`：文档更新。
- `style`：样式调整，如换行、缩进等不影响功能的修改。
- `refactor`：代码重构，如信号重命名、端口结构重构等。
- `test`：测试相关修改。
- `submodule`：子模块更新。

分支命名同样建议使用 `type-scope-description` 的格式。仓库中的 `labeler` 会基于分支名前缀自动为 PR 添加标签，便于维护者快速筛选。

此外，请在 commit message 和 PR 描述中对改动进行充分说明：

- 包括改动原因和预期效果等。
- 如果 PR 有相关 issues，请单起一行 `Fixes #ISSUE_NUMBER`，以便 GitHub 自动关联。
- 如果有其它附加信息（例如在线讨论、技术报告、论文等外部链接），请在尾部使用 `Link:` 标签说明。
- 如果有 AI 工具辅助生成内容，请参考 [AI 使用](#ai-使用) 章节的要求，在尾部使用 `Assisted-by:` 标签说明。
- 亦可以按您的实际情况在尾部使用 `Signed-off-by`、`Co-authored-by` 等标签。

一个完整的 commit message 示例：

```text
fix(some-module): fix some bug

The original design has a bug that blabla.

This commit fixes that by doing blabla.

Fixes #123456

Link: url.to.related.information
Assisted-by: Some-AI-Agent:Model-Version Some-MCP-TOOL
Co-authored-by: Another Author <another.author@example.com>
```

### Review/CI 流程

建议优先创建 `Draft Pull Request`，待 rebase 到最新主线、自测结果符合预期、CI 通过后再转为 `Ready for review`。

来自外部贡献者的 PR 可能无法自动触发 CI；您可以参考 [CODEOWNERS](.github/CODEOWNERS) 文件 `@` 相应维护者协助手动触发。

PR 进入 ready 状态后，我们会开始 review；在此之前，您仍可继续完善内容。

## 编译与使用问题

如果您遇到的是 **编译失败、EMU 运行问题、环境配置问题或使用流程问题**，请使用 [`Problem` 模板](https://github.com/OpenXiangShan/XiangShan/issues/new?template=3-problem.yaml)。

如果您判断问题来自某个环境仓库或外部工具，建议直接在对应仓库提交 issue，例如：

- [GSIM](https://github.com/OpenXiangShan/GSIM/issues)
- [NEMU](https://github.com/OpenXiangShan/NEMU/issues)
- [difftest](https://github.com/OpenXiangShan/difftest/issues)
- [xs-env 环境及 docker 开发环境](https://github.com/OpenXiangShan/xs-env/issues)

## 其余疑问

如果您有其他不属于上述类别的问题，欢迎使用 [`Other Question` 模板](https://github.com/OpenXiangShan/XiangShan/issues/new?template=4-other_question.yml)。

## AI 使用

我们鼓励负责任地使用 AI 以提升协作效率，请注意：

- 贡献者应对最终提交内容负责。
- 请人工复核 AI 生成的代码、文档、测试与结论。
- 请确保提交内容符合项目规范，并能通过必要的构建、测试与评审。

此外，请根据以下情况披露 AI 的使用方式与范围：

- 使用 AI 做翻译、语言润色、行内补全等简单辅助性工作时，无需额外说明。
- 使用 AI 生成完整的 Bug 报告等文本内容时，请在 issue 正文中说明。
- 使用 AI 生成完整代码时，请在 commit message 尾部使用 `Assisted-by: AGENT_NAME:MODEL_VERSION [TOOL1] [TOOL2]` 标签标明：
  - `AGENT_NAME`：AI 工具/框架/agent 名称，如 Claude、Copilot。
  - `MODEL_VERSION`：AI 模型及版本，如 GPT-4、GPT-3.5。
  - `[TOOL]`：额外使用的工具（例如 MCP，skills），如未使用则留空。
    - 上面的`[]`符号表示此项可选，不应该实际出现在 commit message 中。
    - 注意，不应该包括 Agent 框架的内置功能或工具如 `exec_command`、`apply_patch` 或 `web_search` 等，也不应该包括构建工具链如 `make`、`mill`、`clang` 或 `verilator` 等。
  - 例：`Assisted-by: Codex:GPT-6.1-Sol-xhigh github-mcp-server`。
