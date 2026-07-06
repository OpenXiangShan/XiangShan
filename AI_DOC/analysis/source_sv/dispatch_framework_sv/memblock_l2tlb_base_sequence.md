# memblock_l2tlb_base_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`

## 1. 文件定位与使用场景

L2TLB/PTW responder sequence。当前语义是响应 DTLB/L2TLB 上游 PTW request，不是 L2TLB 到 cache/memory 的下游模型。它的职责是：DUT 发出 `vpn/s2xlate` request 后，按 request 和 runtime CSR 生成 lookup key，在 `tlb_entry_by_key` 中命中或自动创建 entry，然后把 PPN、权限、异常等组成 response 发回 DUT。

输入是 DUT request 侧采样的 `vpn/s2xlate` 和 runtime CSR 生成的 `asid/vmid`。输出是 `L2tlb_agent_agent_xaction` 的 ready/resp 字段，并按 key 回填所有匹配的 `uid_tlb_record_by_uid[]` pending record。

控制逻辑字段包括 seq enable、responder active、latency、连续 idle 停止阈值。response 中的 PPN/PTE/PBMT/pf/af/gpf 是 payload。注意这里不按 `paddr` 查表，`paddr` 只用于生成返回 PPN。当前环境默认 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1`，所以 connect 默认接管 DTLB/L2TLB response；`MEMBLOCK_L2TLB_SEQ_EN` 只决定该 sequence 是否主动响应 request。

关键字段：

- `l2tlb_vif`：从 config_db 取得的 virtual interface。
- `enable`、`min_latency/max_latency`、`idle_stop_cycle`。
- `send_count`：只在 `drive_l2tlb_loop()` 内部维护，表示已经成功处理过多少次 L2TLB request。它不参与功能判定，只用于日志和 xaction 命名，避免连续 idle 场景下继续复用 `idle_count` 造成“序号”和“空闲计数”混淆。

关键 task/function：

- `body()`：`MEMBLOCK_L2TLB_SEQ_EN=0` 直接 return；否则要求 `memblock_sync_pkg::l2tlb_responder_active=1`，随后直接进入 responder loop。
- `request_valid()`/`request_fire()`：检查 reset/backend done 和 request valid/ready。
- `send_l2tlb_cycle()`：看到 request valid 后采样 `vpn/s2xlate`，先发 ready item，再给 response item 设置 `pre_pkt_gap=latency`，由 driver 消费 gap 后返回 response。
- `sample_request_fields(vpn,s2xlate)`：从 vif 采样 DTLB request 的 `vpn/s2xlate`。
- `fill_dtlb_resp_from_entry()`：把 `memblock_tlb_entry` 填成 PTW response，包括 S1/S2 tag、asid/vmid、权限、ppn、pf/af/gpf。
- `choose_latency()`：在 min/max 间随机。

## 2. 调度关系与参数数据流

该 sequence 挂在 `env.u_L2tlb_agent_agent.sqr.main_phase`，作为 L2TLB 对上游 DTLB 的 responder。它不是 L2TLB 到 L2Cache/PTW/memory 下游访问模型。

调度关系：

| 阶段 | 条件 | 动作 | 输出 |
|---|---|---|---|
| 使能检查 | `MEMBLOCK_L2TLB_SEQ_EN=1` | 要求 `memblock_sync_pkg::l2tlb_responder_active=1` | 允许 force responder |
| 采样 request | DUT request valid/fire | 从 vif 采样 `vpn/s2xlate` | lookup key 的 request 部分 |
| runtime CSR 同步 | 每次 lookup 前 | 同步 latest CSR snapshot，更新 ASID/VMID/update_seq | lookup key 的 CSR 部分 |
| 查/建 TLB entry | `vpn/s2xlate/asid/vmid` | `common_data_transaction.get_or_create_tlb_entry_by_req()` | lookup key、entry、created |
| 发送 response | entry 可用 | `fill_dtlb_resp_from_entry()` 填 `L2tlb_agent_agent_xaction` | DUT PTW/L2TLB response |
| 回填 uid record | response entry 已确定 | 调用 `common_data_transaction::update_uid_tlb_records_by_entry(key, entry)` | 所有匹配 pending uid record 的 PTE 备份完成 |

参数数据流：

- `MEMBLOCK_L2TLB_SEQ_EN`：responder sequence 运行开关；默认 1 时 sequence 主动响应 L2TLB request，显式设为 0 时 sequence 直接返回，不主动发 response。
- `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN`：编译期接管开关，默认 1；为 0 时该 sequence 即使被 runtime 打开也会 fatal，因为 agent 没有接管 response 通路。
- `MEMBLOCK_L2TLB_MIN_LATENCY`、`MEMBLOCK_L2TLB_MAX_LATENCY`：request fire 后到 response 的随机延迟范围。
- `MEMBLOCK_L2TLB_IDLE_STOP_CYCLE`：控制 responder loop 的连续空闲退出阈值，默认 5000。L2TLB responder 不再使用固定 `max_cycles` 退出；只要有 request/response progress，`idle_count` 清零；连续 idle 超过该阈值后退出。

查表 miss 行为：

- 当前实现不再保留 fallback fault response 或 idle-no-response 策略。
- `get_or_create_tlb_entry_by_req()` 若未返回有效 entry，sequence 直接 `uvm_fatal`。
- 这样可以尽早暴露 TLB 建表、runtime CSR snapshot 或 lookup key 不一致问题，避免用“补 fault response”掩盖前面建表链路的错误。

TLB 数据来源：

- L2TLB request 到来时才按 `vpn/s2xlate` 和 runtime CSR 建/查 `tlb_entry_by_key`。
- L2TLB request 不携带 `paddr`，查表必须用 request 的 `vpn/s2xlate` 加 runtime CSR 的 `asid/vmid`。
- uid 发射后会预登记 `uid_tlb_record_by_uid[uid]`；responder 确定 entry 后回填所有 key 匹配且 `pte_valid=0` 的 record，PTW-back replay 等待以该 record 的 `pte_valid` 判断。

边界：

- 该 sequence 只回填 DTLB/L2TLB 上游 response，不建主表、不分配 LSQ、不推进 commit。
- 该 sequence 不等待 `main_table_ready`。它只在实际 DTLB request 到来时查/建 TLB entry，并尝试回填已存在的 pending uid record。
- runtime CSR 变化不再清 TLB 索引或拒绝命中；上下文变化自然体现在 lookup key 的 ASID/VMID 字段中，真正失效由后续 `sfence/hfence` entry 级逻辑负责。
