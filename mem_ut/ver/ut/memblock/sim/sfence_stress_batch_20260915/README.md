# SFENCE sequence 压力批量验证

本目录是独立压力入口，不修改 sim/Makefile，通过现有 eda_compile 与 eda_batch_run 调用 VCS。

## 运行内容

- tc_sfence_stress_1k.cfg：1000 个主表 slot，SFENCE 间隔为 1，使用 1001～1010 共 10 个 seed。
- tc_sfence_stress_10k.cfg：10000 个主表 slot，使用 seed 20260915 长跑。
- 两档均复用 basicTest、memblock_dispatch_real_smoke_vseq 和 AUTO control topology。
- 脚本先编译一次，再批量复用 simv；每个 seed 的日志由 note 区分。

## 使用

```bash
cd /nfs/home/wangyan/Memblock_env_20260908/XiangShan/mem_ut/ver/ut/memblock/sim/sfence_stress_batch_20260915
./run_sfence_stress_batch.sh
```

状态文件只写本目录 .eda_remote；仿真结果由现有 Makefile 写入 sim/sfence_stress_20260915/{log,wave,exec,cov}。

编译阶段通过脚本的 `udf` 临时补入 `build/rtl/array*_ext.v` memory model。它们只作为本次压力 simv 的编译输入，不修改公共 `rtl.f` 或 RTL。

脚本默认 `WAVE=on`，编译时接入 Verdi NOVAS PLI（`novas.tab`/`pli.a`）并导出 FSDB；可通过 `WAVE=off` 关闭波形。

## 结果检查

每个 seed 应检查：无 uvm_fatal、timeout、C0 mismatch 或 worker hang；action、raw C0 和 effective 数量一致；valid、flushPipe、rs1、rs2 均为 1；下一采样周期八个字段全零；hv/hg 只出现 00、10、01；owner、generation、event、epoch 无重复或回退；所有队列 drain 且 worker 正常退出。

SFENCE 关闭的既有 testcase 仍需单独执行一次非回归。

## 2026-09-15 FSDB 重跑结果

已通过 SSH `172.28.10.101 (eda01)` 完成带波形批量重跑：1K 的 10 个 seed 与 10K 的 seed `20260915` 共生成 **11 个 FSDB**，无 `UCLI-FSDB-LOAD-FAIL`，相关仿真均 `TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0`。

上述日志、FSDB、simv 与编译缓存已在结果确认后清理；重新运行脚本会在 `../sfence_stress_20260915/` 下重新生成。

## 2026-09-15 实际结果

已在 `172.28.10.101 (eda01)` 完成实际 VCS 编译和仿真：

- 1K：seed 1001～1010，10/10 `TEST_PASS`；每个 seed 均产生 999 笔 SFENCE effective，`UVM_ERROR=0`、`UVM_FATAL=0`。
- 10K：seed 20260915，`TEST_PASS`；产生 9999 笔 SFENCE effective，`UVM_ERROR=0`、`UVM_FATAL=0`。
- 结果文件已清理，当前仓库只保留可复现的配置、seed 列表与批处理脚本。
