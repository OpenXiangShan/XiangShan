# V2 已验证状态

## 当前状态

| 项目 | 状态 |
|---|---|
| 从最新 `origin/kunminghu-v2` 创建分支 | 已在 `2acbf327cf7fb514593acc00d4c41117ec499e08` 完成 |
| 规则/profile 路由 | 已由 `8003cc084 docs(mem_ut): add versioned v2 v3 rule profiles` 完成 |
| V2 RTL 生成 flow | 已由 `cead7d6fc build(memblock): add v2 rtl generation flow` 添加 |
| V2 RTL 生成结果 | 修复 submodule 并迁移 V2 独立 wrapper 后已通过，见 `AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md` |
| mem_ut 基础环境迁移 | 已由 `d555ee14a mem_ut: port base environment to v2 rtl path` 完成 |
| V2 DUT agent/interface 适配 | 已按最新 `build/rtl/MemBlock.sv` 完成顶层端口、agent/interface、transaction、driver/monitor 和测试框架逻辑闭合，见 `1464c5066b`、`765c7f0c07`、`85201f2505`、`625cfb5885`、`2b64636780` |
| 远端 VCS 编译与仿真 | 已于 20260811 通过 `tc_sanity/base_fun`，`TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0` |

## 验证标准

V2 RTL 生成成功且基础 mem_ut 环境可用后，优先编译以下目标：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

本次专项 V2 DUT 适配已完成并通过 `tc_sanity` runtime 验收，详细结果见下文。

## 20260811 最新 V2 RTL 适配验证

本次验证以当前 worktree 的 `build/rtl/MemBlock.sv` 为 DUT 权威来源，测试环境统一通过
`mem_ut/ver/ut/memblock/sim` 的远端 EDA flow 编译和运行。

执行命令：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_sanity mode=base_fun
```

首次增量编译遇到 VCS 明确报告的生成缓存
`base_fun/exec/simv.daidir/work.lib++/tdc.sdb` 损坏。该文件不是 RTL、UVM 源码或配置文件；按工具提示
仅删除该可再生缓存后重试，重新编译完成，未出现 DUT 端口、interface、transaction、constraint 或
层级路径错误。

重试后的仿真日志为：

```text
mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_sanity_ts=virtual_base_sequence_cfg=default_seed=666666_rtl_.log
```

在 `1400000.000ns` 结束，结果如下：

```text
TEST CASE PASSED
UVM_ERROR :    0
UVM_FATAL :    0
```

`tc_sanity` 的无 dispatch 主表拓扑仍会输出 LSQ enqueue/commit 等待告警；该类告警不包含
`UVM_ERROR` 或 `UVM_FATAL`，且不影响本次通过判定。此前的
`L2 flush request was withdrawn before DRAIN completed` 已不再出现。

当前 sanity 验证覆盖默认 CSR random transaction 不再发出无 owner 的 L2 flush level 请求。真正的
L2 flush stimulus 仍应由后续专用 sequence 独占 request/done 生命周期，并另行进行专项仿真验证。
