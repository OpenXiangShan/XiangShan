# memblock legacy real-smoke redirect responder 拓扑修复计划

状态：coding、独立 review、原失败 seed、22 个有效历史失败 seed和 cancel-reconcile 随机专项均已完成，
已归档到 `plan/do`。显式 real-smoke VSEQ 随机专项中 7 个已收敛 seed PASS，3 个 seed 暴露独立的
uncache replay livelock 后保留日志并中止；它们未复现 cancel snapshot depth fatal。原始结果中
`default.cfg` 的 5 个 seed 已确认属于独立回归配置冲突，不计入本问题。

## 1. 问题摘要

原回归多个 legacy real-smoke cfg 命中：

```text
[LSQ_CANCEL] local cancel snapshot depth=15 exceeds compile bound=14
```

专项诊断确认溢出队列为 sample `68..82` 的 15 条全零 baseline，且不存在已锚定、待 observation 的
cancel record；阻塞者是一个长期未绑定 redirect monitor anchor 的 record。

## 2. 最终根因

1. `tc_base` 在 redirect sequencer 的 `main_phase` 安装
   `memblock_redirect_dispatch_base_sequence`。
2. `tc_dispatch_real_smoke::configure_real_smoke_default_sequences()` 又把同一路径覆盖为通用
   `tcnt_default_sequence_base#(redirect_agent_agent_xaction)`。
3. recovery handler 创建 cancel record 并把 payload 放入 `pending_redirect_drive_q` 后，没有 sequence 调用
   `try_pop_redirect_drive()` 和 `mark_redirect_drive_done()`。
4. DUT redirect 未驱动，monitor anchor 未产生；reconcile 为保护 future exact target 保留 baseline，最终
   queue depth 到 15。

因此 queue overflow 是 redirect responder 缺失的次生症状，不应修改 queue 深度、snapshot 压缩或
adapter service 顺序。

## 3. 修改方案

删除 `tc_dispatch_real_smoke::configure_real_smoke_default_sequences()` 对 redirect agent 的错误 override，
让 legacy real-smoke 继承 `tc_base` 已安装的 `memblock_redirect_dispatch_base_sequence`。

保持以下行为不变：

- cancel snapshot queue 深度 14 及 fatal；
- raw/local drain 顺序；
- exact target sample 和 deadline；
- software cancel count 与 redirect-deleted owner 语义；
- redirect driver、monitor、DUT RTL。

排查期间尝试的 incremental reconcile、held-tail 压缩和扩展诊断全部撤销，不进入最终提交。

## 4. 拓扑边界

- legacy `tc_dispatch_real_smoke`：继承 `tc_base`，由 agent default sequence 持有 redirect responder。
- 显式 `basicTest + memblock_dispatch_real_smoke_vseq`：`basicTest` 不继承 `tc_base`，不安装 legacy
  redirect default；VSEQ 显式启动唯一 queue-driven responder。
- 两种拓扑互斥，不允许同一 sequencer 同时存在两个 queue consumer。

## 5. 验证计划

1. 重新编译。
2. 原失败 `tc_dispatch_real_store_smoke seed=893303783`：确认出现 redirect drive/anchor、exact cancel compare，
   且 `UVM_ERROR=0/UVM_FATAL=0`。
3. 复跑原结果中属于本根因的 22 个 cancel-depth seed；原集合中 `default.cfg` 的 5 个 seed 单独处理。
4. `tc_dispatch_real_cancel_reconcile_smoke` 至少 20 个随机 seed。
5. 分别覆盖 legacy testcase 和显式 `basicTest + memblock_dispatch_real_smoke_vseq`，确认无双 responder。
6. 检查 end audit：pending redirect drive、cancel record、anchor、snapshot queue 全部清空。

## 6. 非目标

本问题不处理 STD value-only ROB 歧义、PBMT exception、strict-NC late owner、ROB admission prefix、L2TLB
directed sequence 或其他后续首错。
