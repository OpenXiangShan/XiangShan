# Repository Guidelines for Agents

## Project Structure & Module Organization

Core Scala/Chisel lives in `src/main/scala/xiangshan/`; integration/helpers in sibling `top/`, `system/`, `device/`, and `utils/`. Unit tests: `src/test/scala/`; resources: `src/main/resources/`; tools: `scripts/`; images: `images/`. Submodules include `XSCache/`, `yunsuan/`, `difftest/`, and `ready-to-run/`. Generated RTL: `build/rtl/`.

## Resource Checks

Before building or running simulations, agents must check available RAM/swap (`free -h`), allocated CPUs/topology (`nproc`, `lscpu -e`), and load (`uptime`), accounting for container/job limits. Building `kunminghu-v3` `DefaultConfig` needs about 128 GB RAM; 64 GB RAM + 64 GB swap with zswap also works, slightly slower. `-j` controls parallel Clang compilation; single-threaded Verilator/GSIM model generation still needs ~100 GB memory. Treat this as a hard minimum even at `-j1`. Size build parallelism and simulation threads to available resources. Prefer `numactl` binding to available physical cores and local memory; see [`scripts/xiangshan.py`](scripts/xiangshan.py), `--numa`, and `--threads`.

## Build, Test, and Development Commands

Only `NOOP_HOME` is required by XiangShan: set it to the repository's absolute path. Optional `NEMU_HOME` points to OpenXiangShan/NEMU when building your own golden reference; `AM_HOME` points to OpenXiangShan/nexus-am when compiling test cases. Neither is needed with prebuilt `ready-to-run/` workloads/references. See [xs-env](https://github.com/OpenXiangShan/xs-env) for bare-metal/Nix/Docker setup and environment variables.

Install Mill and a simulator; run from the repository root:

- `make init`: initialize submodules.
- `make comp`: compile Scala using `build.mill`.
- `make verilog`: generate synthesis RTL; `make sim-verilog`: generate simulation RTL.
- `make emu -j8`: build with [Verilator](https://verilator.org/guide/latest/).
- `make gsim GSIM=1 -j8`: build `build/emu` with [GSIM](https://github.com/OpenXiangShan/GSIM); retain `GSIM=1` for its difftest configuration.
- Also see `Makefile`, `difftest/verilator.mk`, and `difftest/gsim.mk` for details.
- `./build/emu -i ready-to-run/coremark-2-iteration.bin --diff ready-to-run/riscv64-nemu-interpreter-so`: run CoreMark with differential checking.
- `./build/emu --help` for more runtime options.
- `make reformat` / `make check-format`: apply/check Scalafmt.

Prefer `DefaultConfig`; regenerate artifacts after configuration changes.

## Coding Style & Naming Conventions

Use two-space indentation, PascalCase classes, and camelCase methods/values. Preserve license headers; follow `.scalafmt.conf` (120 columns, scoped formatting) and `scalastyle*-config.xml`. Reformat before committing.

## Testing Guidelines

Prefer focused regression workloads written with [nexus-am](https://github.com/OpenXiangShan/nexus-am) (`make ARCH=riscv64-xs`), or simulate `ready-to-run/coremark-2-iteration.bin` and `ready-to-run/linux.bin` with differential checking. ScalaTest/Chisel is another, less maintained method: `mill -i xiangshan.test.test`; suites follow `*Test.scala` under matching packages. No numeric coverage threshold is documented.

## Issue & Pull Request Guidelines

Read CONTRIBUTING and [Known issues](CONTRIBUTING.md#known-issues) ([中文](CONTRIBUTING.zh-cn.md#部分已知问题说明)); search issues/PRs. Choose `.github/ISSUE_TEMPLATE/` forms: Bug report for RTL, Problem for build/EMU/setup, Feature request for proposals, Other Question otherwise. Reproduce on latest default-branch code. Supply branch/commits, environment, commands, expected/observed behavior, minimal workloads, and attached logs. `./scripts/bug-report.sh` regenerates `bug-report/` and `bug-report.tar.gz`; review system information (to prevent submitting private information) and diffs before uploading.

If a proposed issue or PR matches a listed known issue or deprioritized area (e.g. `kunminghu-v3` vector correctness), agents must notify the user before submission, explain the relevant guidance and priority. Agents must not submit issues or PRs that are known to be deprioritized without user consent.

Discuss issues first; keep PRs focused. Use `type(scope): description` commits/titles and `type-scope-description` branches. Explain motivation/effects/validation; add `Fixes #123` and relevant `Link:` footers. Open drafts; rebase, self-test, and pass CI before review. Consult `.github/CODEOWNERS` for CI assistance.

## AI Policy

Before contributing to the repository (including generating/submitting issues, making commits intended for the repository, or generating/submitting PRs), agents must explicitly ask users to personally read [AI Use](CONTRIBUTING.md#ai-use) ([中文](CONTRIBUTING.zh-cn.md#ai-使用)). Personal development work—such as implementing features, building/running, debugging, or analyzing logs—does not require that unless the work is intended to be submitted as a repository contribution. When making this request, offer the README/CONTRIBUTING in English or `.zh-cn.md` according to the user's language.

Contributors must review AI output, disclose AI-generated reports in issues with model/framework information, and add `Assisted-by: AGENT_NAME:MODEL_VERSION [TOOL1] [TOOL2]` commit footers for AI-generated code.
