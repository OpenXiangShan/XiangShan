# memblock VCS/NFS `rm` 报错分析与处理方案（2026-08-28）

## 1. 现象

在 V2 worktree 的 `mem_ut/ver/ut/memblock/sim` 目录执行基础编译时，终端出现以下报错：

```text
rm: cannot remove ‘.../base_fun/exec/simv.daidir/work.lib++/oh.etc/.nfs000000003c194cf3000073ce’: Device or resource busy
rm: cannot remove ‘.../base_fun/exec/simv.daidir/work.lib++/oh.etc/.nfs0000000037dfa37d0000e0f0’: Device or resource busy
rm: cannot remove ‘.../base_fun/exec/simv.daidir/work.lib++/oh.etc/.nfs00000000397511490000314b’: Device or resource busy
```

触发命令为：

```text
make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun
```

报错发生在 VCS `elabcom`/partition compile 的内部清理阶段，目标目录是 VCS 生成的
`work.lib++/oh.etc`，不是工程源代码目录。

## 2. 证据和归类

1. 同一次编译随后完成了 partition compile、stitch、link 和 Verdi KDB 生成。
   `base_fun/exec/simv` 已生成且可执行，KDB 最终报告：

   ```text
   Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)
   ```

2. `base_fun/log/vcs_compile_rtl.log` 中没有 SystemVerilog `Error-[...]`；该日志末尾保留了
   VCS 成功 link 的记录。终端中的 `rm` 信息来自 VCS 子 make/清理命令，因此不一定进入主编译日志。

3. 编译结束后检查 `work.lib++/oh.etc`，发现三个约 50 MB 的 `.nfs*` 文件。`.nfs*` 是 NFS
   在“文件已被 unlink 但仍被进程打开”时生成的临时占位文件。编译结束后使用 `fuser` 检查
   没有本地存活进程继续占用这些文件，说明它们来自旧的或并发的 VCS 增量作业，而不是当前
   RTL 运行时行为。

4. 因此该现象归类为 **VCS partition/KDB 增量产物在 NFS 上的清理竞争或遗留占用**，不是
   RM、UVM sequence、SystemVerilog 语法或 DUT/RTL 功能错误。不能通过修改 RTL 解决，也不能
   把该提示当作本次 RM mismatch 的证据。

## 3. 最优处理方案

### 3.1 本次回归采用的方案

对需要稳定复现和长时间运行的 RM 回归，使用独立的 `mode` 目录并关闭 VCS partition compile：

```text
make eda_compile tc=basicTest ts=virtual_base_sequence \
  mode=rmfix_sticky_20260828 partcmp_op=off wave=off
```

随后在同一个独立 `mode` 下运行仿真。这样 VCS 不再访问共享的
`mode/partitionlib` 和 `work.lib++` 增量数据库，避免旧 `.nfs*` 清理和并发分区编译相互影响；
`partcmp_op=off` 只改变编译缓存策略，不改变 RTL、UVM transaction 或测试语义。

### 3.2 清理边界

- 只在确认没有 `vcs`/`simv`/`verdi` 进程占用后，清理对应 `mode` 下的 `exec`、`partitionlib`
  等生成目录；不删除源码、RTL、sequence、cfg 或用户日志。
- 不使用无条件跨 worktree 的 `rm -rf`，也不强制 kill 不属于当前任务的进程。
- 如果必须复用 `partcmp_op=on`，应为每次任务分配独立 mode，或把 VCS 中间文件放到远端
  本地盘；发现 `.nfs*` 正在被占用时只等待并重试，不能把失败的 `rm` 重试升级成强制删除。

### 3.3 为什么不修改 Makefile 默认值或 RTL

工程现有默认 `partcmp_op=on` 是通用增量编译策略，直接改成全局关闭会增加所有普通回归的
编译时间，也会扩大本次 RM 任务的变更范围。最小且可复现的处理是通过本次验证命令覆盖
`partcmp_op` 和 `mode`；该处理已经足以消除本次 NFS 清理路径，不需要任何 RTL/Scala 修改。

## 4. 按方案重新验证

重新验证时应记录：

- 独立 mode 和完整命令。
- 编译退出码、VCS/KDB error/warning 数量。
- 新 mode 下是否再次出现 `rm: cannot remove ... .nfs*`。
- 若后续仿真失败，单独分析 UVM/RM 日志和 FSDB，不能把本节的 NFS 提示归因于 RTL。

本文件只记录工具环境问题及其处理，不改变任何 RTL/Scala 文件。

本次按方案重新编译的实际结果：`rmfix_sticky_20260828` 使用 `partcmp_op=off`，远端 VCS
完成 251 个模块编译、elaboration 和 link，退出码为 0；输出中未再出现
`rm: cannot remove ... .nfs*`。该结果证明本次报错可以通过隔离增量目录和关闭 partition compile
规避，后续目标回归沿用该 mode/参数组合。
