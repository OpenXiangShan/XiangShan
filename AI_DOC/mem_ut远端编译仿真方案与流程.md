# mem_ut 远端编译仿真方案与流程

## 1. 文档目的

本文档用于说明当前 XiangShan worktree 中 `mem_ut` 环境下的远端编译与仿真机制，明确：

- 工程目录位置
- 本地节点与 `eda01` 的职责划分
- 编译/仿真的调用入口
- 远端环境初始化方式
- 实际执行 flow
- 当前已验证状态
- 当前已知阻塞点

本文档是面向人阅读的方案文档。

## 2. 环境位置

`mem_ut` 当前位于：

- `mem_ut`

memblock UVM 环境位于：

- `mem_ut/ver/ut/memblock`

主仿真目录位于：

- `mem_ut/ver/ut/memblock/sim`

## 3. 方案概述

本项目采用“本地控制，远端执行”的双节点方案：

- 当前节点：
  - 代码编辑
  - 大模型分析
  - 触发编译/仿真
  - 读取共享日志
- `eda01`：
  - 实际加载 `vcs` / `verdi`
  - 实际执行 `make compile` / `make run`

也就是说：

- 本地不要求能直接运行 `vcs`
- 真正的 EDA 编译仿真工作发生在 `eda01`
- 触发入口统一保留在 `sim/Makefile`

## 4. 关键文件

该方案由以下文件实现：

- `mem_ut/ver/ut/memblock/sim/Makefile`
- `mem_ut/ver/ut/memblock/scripts/remote_eda_make.sh`
- `mem_ut/ver/ut/memblock/scripts/eda01_entry.sh`

职责分别如下：

- `Makefile`
  - 暴露 `eda_compile`、`eda_run` 等统一入口
- `remote_eda_make.sh`
  - 在本地执行
  - 收集 make 参数
  - 通过 ssh 触发 `eda01`
- `eda01_entry.sh`
  - 在 `eda01` 上执行
  - 进入正确的 `sim` 目录
  - 设置 `MEMBLOCK_XS_HOME` 和 `MEMBLOCK_PROJECT`
  - 真正执行 `make compile` / `make run`

## 5. 编译仿真 Flow

### 5.1 编译 flow

推荐从以下目录执行：

```bash
cd mem_ut/ver/ut/memblock/sim
```

执行命令：

```bash
make eda_compile tc=tc_sanity mode=base_fun
```

实际 flow 如下：

1. 用户或 Codex 在本地 `sim` 目录执行 `make eda_compile`
2. `Makefile` 调用本地脚本 `remote_eda_make.sh`
3. `remote_eda_make.sh` 通过 ssh 连接 `eda01`
4. 在 `eda01` 上执行 `eda01_entry.sh compile ...`
5. `eda01_entry.sh` 基于自身路径进入正确的 `sim` 目录
6. `eda01_entry.sh` 导出 `MEMBLOCK_XS_HOME` 和 `MEMBLOCK_PROJECT`
7. `eda01` 上实际执行 `make compile`
8. `vcs` 在 `eda01` 上启动
9. 编译日志写回共享目录

### 5.2 仿真 flow

同步运行：

```bash
make eda_run tc=tc_sanity mode=base_fun
```

后台运行：

```bash
make eda_run_bg tc=tc_sanity mode=base_fun
```

查看状态：

```bash
make eda_status tc=tc_sanity mode=base_fun
```

查看后台 launcher 日志：

```bash
make eda_tail tc=tc_sanity mode=base_fun
```

## 6. 远端环境初始化规则

当前 `Makefile` 中已经固化远端 bootstrap：

```bash
source /usr/share/Modules/init/bash >/dev/null 2>&1 || true; \
module load synopsys/vcs/Q-2020.03-SP2 license; \
module load synopsys/verdi/R-2020.12-SP1 license
```

作用如下：

- 初始化 `module`
- 加载 `vcs`
- 加载 `verdi`

如果将来再次出现以下报错：

```text
vcs: command not found
```

优先检查：

- `eda01` 上 `module` 初始化路径是否变化
- `synopsys/vcs/Q-2020.03-SP2` 是否仍可用
- `synopsys/verdi/R-2020.12-SP1` 是否仍可用

## 7. 测试框架与 RTL 路径配置

`MEMBLOCK_XS_HOME` 指向当前测试框架所在的 XiangShan worktree，`MEMBLOCK_PROJECT`
保留为它的上一级目录。独立的 `MEMBLOCK_RTL_HOME` 指向生成 RTL 的 XiangShan 仓库根目录；
`cfg/rtl.f` 使用 `$MEMBLOCK_RTL_HOME/build/rtl/filelist.f`，公共验证源码仍从
`MEMBLOCK_XS_HOME/mem_ut` 读取。

未设置 `MEMBLOCK_RTL_HOME` 时，默认沿用 `MEMBLOCK_XS_HOME`。设置后，Makefile 导出该变量，
`remote_eda_make.sh` 将其显式传给 eda01，避免 SSH 环境丢失导致使用其他 RTL。

当前用户本地 V2 配置为：

```bash
export MEMBLOCK_RTL_HOME=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/RTL/XiangShan
```

该赋值保存在仓库外的 `~/.config/memblock/v2_env.sh`，由 `~/.bashrc`、`~/.zshrc`
和 V2 项目上一级 `.envrc` 加载。新终端自动生效；已经打开的终端执行：

```bash
source ~/.config/memblock/v2_env.sh
```

检查路径和产物：

```bash
printf '%s\n' "$MEMBLOCK_RTL_HOME"
test -r "$MEMBLOCK_RTL_HOME/build/rtl/filelist.f"
```

本次最新 V2 顶层适配的两个固定 CSR 输入按硬件复位值配置：`TSO_ENABLE=0`、
`PF_REFRESH_ENABLE=1`。PF 表示页故障；这两个输入目前由 `tb/dut_inst.sv` 初始化，
尚未加入 CSR agent 的动态随机控制。

## 8. 当前已验证结论

该方案已经验证到以下阶段：

1. 本地 `make eda_compile tc=tc_sanity mode=base_fun`
2. 正常 ssh 触发 `eda01`
3. 正常加载 `vcs` 和 `verdi`
4. `eda01` 上真实启动 `vcs`
5. 共享目录生成编译日志
6. 搬迁到当前 worktree 的 `mem_ut` 后仍然可用

这说明当前“本地控制 + `eda01` 实际编译”的方案已经打通。

## 9. 当前真实阻塞点

当前编译失败已经不是环境问题，而是工程源码问题。

已知阻塞点为：

- 文件：`mem_ut/ver/ut/memblock/tb/dut_inst.sv`
- 错误类型：VCS 语法错误
- 报错 token：`.`
- 相关信号：
  - `.auto_inner_frontendBridge_instr_uncache_in_a_ready(...)`

因此，后续如果再次验证环境：

- 只要还能稳定复现到这个错误点
- 就说明远端编译方案仍然是正常的

## 10. 推荐验证命令

默认验证命令：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

判定标准：

- 如果报 `vcs: command not found`，说明远端环境又坏了
- 如果能进入 `dut_inst.sv` 当前语法错误，说明远端编译链仍正常
- 如果越过该错误，则说明源码问题也已被修复

## 11. 后续建议

后续 Codex 或人工在本项目中应遵循以下原则：

- 优先在 `sim` 目录运行 `eda_*` 目标
- 不要默认在本地直接执行 `make compile`
- 遇到环境问题优先检查远端 bootstrap
- 遇到当前 `dut_inst.sv` 语法错误时，不要误判为远端方案失效
- Git 操作默认只做本地 `commit`
- 未经明确要求，不要自动 `push` 到远端仓库
