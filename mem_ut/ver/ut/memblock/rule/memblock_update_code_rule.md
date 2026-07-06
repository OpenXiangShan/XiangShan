# memblock 更新最新代码规则

## 触发条件

当用户要求更新最新代码、同步最新代码、拉取最新代码、更新到最新
`kunminghu-v2` 或 `kunminghu-v3`，或表达类似含义时，必须使用本规则。

本规则保存通用流程。具体上游分支、工作分支和已验证基线由当前版本 profile 定义：

```text
mem_ut/ver/ut/memblock/rule/version/v2/branch_policy.md
mem_ut/ver/ut/memblock/rule/version/v3/branch_policy.md
```

## Profile 选择

更新前必须先确定版本：

- 当前分支为 `mem_ut_uvm` 时，默认使用 V3 profile。
- 当前分支为 `mem_ut_uvm_v2` 时，默认使用 V2 profile。
- 用户显式指定 V2 或 V3 时，以用户指定版本为准。
- 当前分支与用户显式指定版本冲突时，停止并向用户确认。

## 更新前检查

必须先在 XiangShan 仓库根目录运行：

```bash
git status
```

如果 `git status` 显示存在任何已修改、已暂存或未跟踪文件，必须停止更新流程并反馈用户：

```text
当前工作区存在未提交修改。请先选择：
1. git 提交本地所有修改
2. 执行 git stash 暂存本地修改

工作区干净后再继续更新最新代码。
```

在工作区未干净前，不要执行会改变当前基线的 `rebase`、`merge` 或 `pull`。

## 更新命令

只有当 `git status` 确认工作区干净后，才按当前 branch profile 选择上游分支并依次执行：

```bash
git fetch origin <profile-upstream-branch>
git rebase FETCH_HEAD
```

执行顺序不能调换。不要用 `git pull` 替代上述两步。

## rebase 后 RTL 刷新

`git rebase FETCH_HEAD` 成功后，必须参考：

```text
AI_DOC/memblock_rtl生成规则.md
mem_ut/ver/ut/memblock/rule/version/<v2|v3>/memblock_rtl_profile.md
```

并按当前版本 profile 重新生成或刷新 memblock RTL。默认优先使用：

```bash
scripts/generate_memblock_rtl.sh
```

生成成功标准以当前版本 profile 的产物清单为准。

## rebase 后 DUT 适配检查

RTL 刷新成功后，必须继续参考：

```text
mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md
mem_ut/ver/ut/memblock/rule/version/<v2|v3>/dut_interface_baseline.md
```

从 `mem_ut/ver/ut/memblock/tb/top_tb.sv` 展开的所有 RTL 交接 interface 入手，
检查最新 Verilog DUT 顶层端口和内部模块接口是否变化。

如果发现变化，必须同步修改对应：

- `tb/dut_inst.sv`
- `tb/*_agent_connect.sv`
- agent interface
- agent transaction / xaction
- agent driver 驱动字段
- agent monitor 采集字段
- 受影响的 env、RM、sequence、cfg 文件

## 失败处理

如果 `git fetch`、`git rebase` 或 RTL 生成失败：

- 停止后续流程
- 保留现场，不要自动回滚用户修改
- 汇报失败命令、关键错误信息和当前状态
