# Contributing to XiangShan

English | [中文](CONTRIBUTING.zh-cn.md)

## Table of Contents

- [Before contributing](#before-contributing)
- [Bug Report](#bug-report)
- [Feature Request](#feature-request)
- [Pull Request](#pull-request)
- [Build and Usage Problems](#build-and-usage-problems)
- [Other Question](#other-question)
- [AI Use](#ai-use)

## Before contributing

First of all, thank you for your interest in and support for XiangShan. Before opening an issue or submitting a pull request, we recommend that you complete the following checks:

- Read the relevant documents and manuals, such as the [XiangShan design documents](https://docs.xiangshan.cc/projects/design/en) and the [RISC-V ISA Manual](https://github.com/riscv/riscv-isa-manual).
- For the maintenance and development status of XiangShan branches, please refer to the [README](README.md#branch-maintenance-status).
- Search existing issues, especially the FAQ and example issues under the [`type: example`](https://github.com/OpenXiangShan/XiangShan/issues?q=sort%3Aupdated-desc%20state%3Aclosed%20label%3A%22type%3A%20example%22) label.
- We accept reports in either Chinese or English; however, for broader international collaboration, English is recommended.
- We accept content assisted by AI tools, but there are conditions. Please read the [AI Use](#ai-use) section carefully.
- We primarily use [GitHub Issues](https://github.com/OpenXiangShan/XiangShan/issues) and [Pull Requests](https://github.com/OpenXiangShan/XiangShan/pulls) to receive contributions and answer questions. If you prefer not to disclose information publicly, you can also contact us via the mailing list at [all@xiangshan.cc](mailto:all@xiangshan.cc).

## Bug Report

If you find a **bug in XiangShan RTL** (for example, a difftest failure), please use the [`Bug report` template](https://github.com/OpenXiangShan/XiangShan/issues/new?template=1-bug_report.yaml).

Please fill in the issue template as completely and accurately as possible, including the branch, commit, observed behavior, environment, test workload, and other requested information. This helps us locate the problem and provide feedback more efficiently.

You can use `./scripts/bug-report.sh` to generate the required attachments. This script collects system information, including the operating system, memory size, and toolchain versions. Please review the contents of `bug-report.tar.gz` before uploading it to ensure that no sensitive information is included.

We also welcome pull requests that fix bugs. Please refer to the [Pull Request](#pull-request) section below.

### Known issues

The `kunminghu-v3` main branch is under rapid development. At the moment, our primary focus is the functional correctness of RVA23 profile and performance. The following areas are currently not among our highest priorities. You are still welcome to report related issues, but we may not be able to address them immediately.

- Side-channel security issues, such as timing attacks. Please consider reporting them as a [Feature Request](#feature-request) rather than a bug report.
- Configurability. We cannot guarantee stable compilation or execution for every parameter combination. Please use `DefaultConfig` whenever possible for testing.
- Vector unit correctness. The vector unit in `kunminghu-v3` has been fully reworked, and our current focus is performance improvement. Functional issues may still exist in some corner cases. If you encounter one, please first confirm that it is still reproducible on the latest mainline and check whether there is already a pending PR that addresses it.

In addition, for fuzz testing, please note that self-modifying programs may legitimately cause difftest failures. This is because the XiangShan RTL ICache uses software-managed coherence and requires explicit synchronization with the `fence.i` instruction, while NEMU does not implement a cache hierarchy and directly accesses memory for both stores and instruction fetches. Therefore, all automatically generated store-type instructions must satisfy at least one of the following conditions: 1) they do not modify the code section; or 2) they execute `fence.i` after modifying the code section. Otherwise, a difftest failure is expected and should not be reported as a bug.

This section is time-sensitive. If you believe any of the information here is outdated, please let us know in an issue and we will update it as soon as possible.

Last updated: 2026/06/30

## Feature Request

If you would like to propose a **new feature, design improvement, or process enhancement**, please use the [`Feature request` template](https://github.com/OpenXiangShan/XiangShan/issues/new?template=2-feature_request.yaml).

We also welcome pull requests that implement new features. Please refer to the [Pull Request](#pull-request) section below.

## Pull Request

Before submitting a PR for a bug fix or a new feature, we recommend opening the corresponding type of issue first and discussing it with us and the community to avoid duplicated work.

Please run `make reformat` before committing to automatically format the code.

Please keep each PR focused on a single topic. Avoid mixing multiple unrelated changes in the same PR. A PR may include the main functional change together with necessary preparatory refactoring, such as reorganizing port structures, but we recommend splitting them into separate commits to make review easier.

This repository generally follows conventional commits. Commit messages and PR titles should follow the `type(scope): description` format, and the description should clearly explain what changed and why.

Common `type` values include, but are not limited to:

- `feat`: introduce a new feature.
- `fix`: fix a functional or performance-related bug.
- `perf`: performance tuning. Note that performance features usually belong to `feat`, and performance bug fixes usually belong to `fix`. Use `perf` only for a limited set of cases such as parameter tuning.
- `area` / `timing` / `power`: targeted PPA optimizations.
- `build` / `ci` / `chore`: build system changes, CI updates, or miscellaneous maintenance.
- `docs`: documentation updates.
- `style`: non-functional style-only changes, such as formatting, indentation, or line breaks.
- `refactor`: code restructuring, such as signal renaming or port organization changes.
- `test`: test-related changes.
- `submodule`: submodule updates.

Branch names are also recommended to follow the `type-scope-description` format. The repository `labeler` uses branch name prefixes to automatically add labels to PRs, which helps maintainers triage them more quickly.

In addition, please describe your changes sufficiently in the commit message and the PR description:

- Include the motivation for the change and its expected effect.
- If the PR is related to one or more issues, put `Fixes #ISSUE_NUMBER` on a separate line so that GitHub can automatically link and close them.
- If there is additional information, such as online discussions, technical reports, papers, or other external references, add it at the end with a `Link:` tag.
- If AI tools were used to assist in generating content, follow the requirements in the [AI Use](#ai-use) section and add an `Assisted-by:` tag at the end.
- You may also use other footer tags such as `Signed-off-by` or `Co-authored-by` when appropriate.

A complete example of a commit message:

```
fix(some-module): fix some bug

The original design has a bug that blabla.

This commit fixes that by doing blabla.

Fixes #123456

Assisted-by: Some-AI-Agent:Model-Version
Co-authored-by: Another Author <another.author@example.com>
Link: url.to.related.information
```

We recommend creating a `Draft Pull Request` first, and converting it to `Ready for review` only after rebasing onto the latest mainline, completing self-testing, and getting CI to pass. PRs from external contributors may not be able to trigger CI automatically. In that case, you can refer to [CODEOWNERS](.github/CODEOWNERS) and `@` the relevant maintainers for help triggering it manually. Once the PR is marked ready, we will start the review process; until then, you can continue refining it.

## Build and Usage Problems

If you encounter **build failures, EMU runtime issues, environment setup problems, or usage workflow problems**, please use the [`Problem` template](https://github.com/OpenXiangShan/XiangShan/issues/new?template=3-problem.yaml).

If you believe the issue comes from a specific environment repository or external tool, please consider filing the issue directly in the corresponding repository, for example:

- [GSIM](https://github.com/OpenXiangShan/GSIM/issues)
- [NEMU](https://github.com/OpenXiangShan/NEMU/issues)
- [difftest](https://github.com/OpenXiangShan/difftest/issues)
- [xs-env and Docker-based development environment](https://github.com/OpenXiangShan/xs-env/issues)

## Other Question

If your question does not fit into any of the categories above, please use the [`Other Question` template](https://github.com/OpenXiangShan/XiangShan/issues/new?template=4-other_question.yaml).

## AI Use

We encourage responsible use of AI to improve collaboration efficiency. Please keep the following in mind:

- Contributors are responsible for the final content they submit.
- Please manually review any AI-generated code, documentation, tests, or conclusions.
- Please ensure that the submitted content follows project conventions and passes the necessary build, test, and review processes.

In addition, please disclose the use and scope of AI in the following situations:

- No extra disclosure is needed when AI is only used for simple auxiliary tasks such as translation, language polishing, or inline completion.
- If AI is used to generate a complete bug report or similar textual content, please state that in the issue body.
- If AI is used to generate complete code, please follow the [Linux kernel](https://docs.kernel.org/process/coding-assistants.html#attribution) guidance and add an `Assisted-by: AGENT_NAME:MODEL_VERSION [TOOL1] [TOOL2]` tag at the end of the commit message:
  - `AGENT_NAME`: the AI tool, framework, or agent name, such as Claude or Copilot.
  - `MODEL_VERSION`: the model name and version, such as GPT-4 or GPT-3.5.
