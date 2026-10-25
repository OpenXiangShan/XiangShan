# MemBlock sparse PPN manager 可达性实现 Review

## 1. Review 结论

独立只读 reviewer `Anscombe` 于 `2026-10-25` 完成源码、生成 RTL 和历史日志审查，结论为
`REVIEW_PASS`：根因是测试框架 sparse PPN 生成未约束到真实 TL manager，不是 Uncache、TLBuffer
或 TLXbar RTL 功能错误；推荐生成端确定性重映射，拒绝 TB fallback responder。

## 2. 根因证据

- `MAIN_MEM_RANGES_EN=0` 的旧算法使用全空间 XOR，未做 manager serviceability 检查。
- UID462 最终地址为 `0x311368c0`；Uncache input buffer 接受 source0 后，TLXbar out0/out1
  均无 A.valid，也没有 D response。
- `SoC.APLICRange` 是 `0x31100000-0x3113ffff` 的粗粒度描述；实际 M/S domain 只覆盖
  `0x31100000-0x31107fff` 和 `0x31120000-0x31127fff`。
- `TLXbar_6.auto_in_a_ready` 只 OR 两个 route predicate；空洞地址两个 predicate 均为 0。

## 3. 实现检查

`tlb_map_builder.sv` 把 sparse hash 限制在 `0x0080_0000_0000-0x00ff_ffff_ffff`。
该范围同时被外部 memory port 和 cacheable PMA 覆盖，并按 512 GiB 对齐。修改保持
`MAIN_MEM_RANGES_EN=1` 路径不变，保留原 deterministic hash 和后续 level alignment。

`soft_test_l2tlb_ppn_reuse_sequence.sv` 检查历史 VPN、S1/S2、level 0-3、确定性和对齐；
`tc_l2tlb_ppn_reuse_smoke.cfg` 显式打开 sparse mode，确保 directed 实际覆盖新分支。

## 4. Reviewer 风险意见与处理

| reviewer 意见 | 处理 |
| --- | --- |
| 不要把 PMA 范围当作 manager 真值 | 方案同时核对 `HaveAXI4MemPort.memRange` 和 TLXbar 高地址 decode |
| 不要增加 fallback responder | 未增加 responder；空洞由生成端避免 |
| superpage/NAPOT 需在映射后对齐 | 保留既有 level/NAPOT 流程，directed 覆盖 level 0-3 |
| PPN reuse 不得重新注入 hole PPN | reuse history 来自同运行已完成响应；directed cfg 强制 sparse 分支并检查生成结果 |
| MMIO 覆盖不能依赖 hash | 明确归属显式 MMIO directed 场景 |

## 5. 验证记录

- 当前完整 worktree VCS compile：PASS。
- software directed `tc_l2tlb_ppn_reuse_smoke`：1/1 PASS。
- 历史代表 seed `1242164593`：PASS，`UVM_ERROR=0`、`UVM_FATAL=0`，完成 1000 transactions。
- 其余历史 seeds `165489856,898041954,37617171`：串行 3/3 PASS，`fail=0 infra=0`
  `incomplete=0 aborted=0 remote_unknown_jobs=0`。
- 并行首次尝试因 regression controller 的 remote identity probe 误判出现 ABORTED；该批次不作为
  功能验证证据，改用新 mode、`jobs=1` 重新编译串行运行并全部通过。
- 隔离 worktree
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_sparse_ppn_isolation_20261025`
  只应用三个实现文件，并链接同版本生成 RTL。隔离源码重新编译 PASS，software directed 1/1 PASS，
  历史 seed `1242164593` 在原 1000 transaction 条件下 PASS。
- 隔离 `simv` 上直接并发运行 20 个新固定随机 seed，避开已复现的 regression controller 多作业
  identity probe 缺陷。结果 `20/20 PASS`，每条 `rc=0`、`UVM_ERROR=0`、`UVM_FATAL=0`，均观察到
  `$finish`。结果目录：
  `mem_ut/ver/ut/memblock/sim/sparse_ppn_isolation_random20_r2/`。
- 最终独立 implementation review 结论 `REVIEW_PASS`，无 blocking finding。Reviewer 确认 manager、
  PMA、S2 38-bit PPN、level 0-3、NAPOT、MAIN=1 和 runtime reuse 路径均无当前遗漏。

当前实现 review 状态：`REVIEW_PASS`。隔离编译、directed、历史 4 seeds 和 20 个新随机 seed 全部通过。
