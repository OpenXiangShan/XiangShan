# l2tlb_base_sequence.sv Implementation Plan

## 1. 目标

实现 `memblock_l2tlb_base_sequence.sv`，用于对 DTLB/L2TLB 侧的翻译请求进行响应建模：

1. sequence 从 `common_data_transaction.sv` 获取 TLB 表。
2. 根据采集到的 DTLB request `vpn/s2xlate` 和 runtime CSR 快照生成查询索引。
3. 从 by-key live TLB 表中读取对应 entry；若 miss，则通过公共 API 自动创建新 entry。
4. 将 TLB entry 字段回填到发往 agent driver 的 transaction。
5. 通过 sequencer/driver 发射响应 transaction。

Task15 已按本方案实现可编译 responder skeleton、plus 参数和 common data 查表 API。
Task18 已把 `L2tlb_agent_connect.sv` 接到真实 DUT 内部 DTLB/L2TLB 交互点：
`inner_dtlbRepeater.io_ptw_req_0` 和 `inner_ptw.io_tlb_1`。

## 2. 命名和边界确认

当前计划中的 `l2tlb_base_sequence.sv` 负责的是 L2TLB/PTW response 模型，不负责生成前端/后端的 DTLB 请求激励。

当前实现使用现有 `L2tlb_agent_agent`，sequence 继承
`L2tlb_agent_agent_default_sequence`，transaction 类型为
`L2tlb_agent_agent_xaction`。

当前 `L2tlb_agent_connect.sv` 已接入 `memblock_connect.sv`。connect 是否接管由编译期宏
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制，默认值为 1，即 mem_ut 默认由
`L2TLB_agent` 接管 DTLB/L2TLB response 通路。`MEMBLOCK_L2TLB_SEQ_EN` 默认值为 1，
responder sequence 默认主动回包；显式设置 `+MEMBLOCK_L2TLB_SEQ_EN=0` 时不主动
回包，此时 driver idle 保持 `ready=0/resp_valid=0`。若需要只观察 DUT 原始
PTW/L2TLB response，应在编译期覆盖
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`。

注意：当前 DTLB/L2TLB request 接口携带 `vpn`、`s2xlate`，不携带 `paddr`。因此查询
以 `{vpn, asid, vmid, s2xlate}` 为主；`paddr` 是 TLB 表项内容和 debug/一致性检查字段，
不作为当前 request 直接采样字段。

## 3. common_data_transaction.sv 中需要的数据结构

`common_data_transaction.sv` 中新增或预留 TLB 表和请求上下文表。

### 3.1 TLB entry

建议定义 `tlb_entry_t`，字段覆盖 driver response transaction 需要的所有字段：

```systemverilog
typedef struct {
    bit        valid;
    bit [47:0] paddr;
    bit [37:0] vpn;
    bit [1:0]  s2xlate;

    bit [34:0] s1_entry_tag;
    bit [15:0] s1_entry_asid;
    bit [13:0] s1_entry_vmid;
    bit        s1_entry_n;
    bit [1:0]  s1_entry_pbmt;
    bit        s1_entry_perm_d;
    bit        s1_entry_perm_a;
    bit        s1_entry_perm_g;
    bit        s1_entry_perm_u;
    bit        s1_entry_perm_x;
    bit        s1_entry_perm_w;
    bit        s1_entry_perm_r;
    bit [1:0]  s1_entry_level;
    bit        s1_entry_v;
    bit [40:0] s1_entry_ppn;
    bit [2:0]  s1_addr_low;
    bit [2:0]  s1_ppn_low[8];
    bit        s1_valididx[8];
    bit        s1_pteidx[8];
    bit        s1_pf;
    bit        s1_af;

    bit [37:0] s2_entry_tag;
    bit [13:0] s2_entry_vmid;
    bit        s2_entry_n;
    bit [1:0]  s2_entry_pbmt;
    bit [37:0] s2_entry_ppn;
    bit        s2_entry_perm_d;
    bit        s2_entry_perm_a;
    bit        s2_entry_perm_x;
    bit        s2_entry_perm_w;
    bit        s2_entry_perm_r;
    bit [1:0]  s2_entry_level;
    bit        s2_gpf;
    bit        s2_gaf;
} tlb_entry_t;
```

### 3.2 表组织

当前实现使用 by-key TLB 主表和 per-uid record 追踪：

```systemverilog
memblock_tlb_entry      tlb_entry_by_key[memblock_tlb_lookup_key_t];
memblock_uid_tlb_record uid_tlb_record_by_uid[memblock_uid_t];
```

其中：

- `tlb_entry_by_key[key]` 是 live TLB cache，同 `{vpn, asid, vmid, s2xlate}` 复用同一 entry。
- `uid_tlb_record_by_uid[uid]` 只保存 uid 发射时的上下文快照和后续 PTE 回填结果。
- `common_data_transaction::get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)` 用 runtime CSR 生成 key，命中则返回已有 entry，miss 则自动创建并插入 `tlb_entry_by_key`。
- 当前 L2TLB req 接口没有 paddr，因此不按 paddr request context 查表。

### 3.3 request context

如后续 directed case 需要把同一 `vpn/s2xlate` 下的多条 in-flight request 与 ROB 或
地址上下文强绑定，可以扩展请求上下文：

```systemverilog
typedef struct {
    bit        valid;
    bit [47:0] paddr;
    bit [37:0] vpn;
    bit [1:0]  s2xlate;
    bit [8:0]  robIdx;
} tlb_req_context_t;
```

可选表：

```systemverilog
tlb_req_context_t tlb_req_context_by_vpn[bit [37:0]];
tlb_req_context_t tlb_req_context_by_robidx[bit [8:0]];
```

当前实现暂不新增 request context 表。sequence 查询优先级：

1. 通过采集到的 `vpn/s2xlate` 和 runtime CSR 生成 lookup key。
2. 查 `tlb_entry_by_key[]`。
3. 若 entry miss，自动创建新 entry。
4. response entry 确定后，按 key 回填所有 `pte_valid=0` 的匹配 uid record。

## 4. l2tlb_base_sequence.sv 行为

### 4.1 类定义

建议：

```systemverilog
class memblock_l2tlb_base_sequence extends L2tlb_agent_agent_default_sequence;
```

### 4.2 成员

需要包含：

- `virtual L2tlb_agent_agent_interface l2tlb_vif`
- `common_data_transaction data`
- `bit enable`
- `int unsigned min_latency`
- `int unsigned max_latency`
- `int unsigned idle_stop_cycle`

配置项后续从 `seq_csr_common.sv` 或 plus 参数进入，不在 sequence 内写死。

### 4.3 body 主流程

流程：

1. `seq_csr_common::init()` 并读取 `MEMBLOCK_L2TLB_*` 参数。
2. 若 `MEMBLOCK_L2TLB_SEQ_EN=0`，直接返回，不发送 L2TLB item。当前 agent 原始 default sequence 会随机发送 item，因此不能作为默认关闭路径。
3. 从 `uvm_config_db` 获取 virtual interface。
4. 获取 `common_data_transaction::get()` 单例。
5. 等待 reset 释放和 `memblock_sync_pkg::reset_backend_done`。
6. 每轮先发送一拍 ready transaction：
   - `req_ready = 1`
   - `resp_valid = 0`
7. 发送 ready item 后采样请求：
   - reset 已释放
   - `memblock_sync_pkg::reset_backend_done = 1`
   - `req_valid = 1`
   - `req_ready` 由本 sequence 持续驱动为 1，不读取 vif 上旧 ready 作为 fire 条件
8. 若采样到请求，记录 `vpn`、`s2xlate`。
9. 按配置 latency 继续发送若干拍 ready/idle transaction，保持 `req_ready=1`、`resp_valid=0`。
10. 用 common data lookup 查询 TLB 表，并根据查表结果构造 response transaction。
11. 发射 response transaction。

### 4.4 查表流程

建议封装 task/function：

```systemverilog
function bit lookup_tlb_entry_by_req(
    input  bit [37:0] vpn,
    input  bit [1:0]  s2xlate,
    output memblock_tlb_entry entry
);
```

内部顺序：

1. `common_data.get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)`。
2. 用 entry 填 response。
3. 调 `common_data.update_uid_tlb_records_by_entry(key, entry)` 回填 uid record。

## 5. response 字段回填规则

命中 TLB 表时，transaction 字段直接从 entry 回填：

- `io_ptw_resp_valid = 1`
- `io_ptw_resp_bits_s2xlate = entry.s2xlate`
- `io_ptw_resp_bits_s1_entry_* = entry.s1_entry_*`
- `io_ptw_resp_bits_s1_addr_low = entry.s1_addr_low`
- `io_ptw_resp_bits_s1_ppn_low_0..7 = entry.s1_ppn_low[0..7]`
- `io_ptw_resp_bits_s1_valididx_0..7 = entry.s1_valididx[0..7]`
- `io_ptw_resp_bits_s1_pteidx_0..7 = entry.s1_pteidx[0..7]`
- `io_ptw_resp_bits_s1_pf = entry.s1_pf`
- `io_ptw_resp_bits_s1_af = entry.s1_af`
- `io_ptw_resp_bits_s2_entry_* = entry.s2_entry_*`
- `io_ptw_resp_bits_s2_gpf = entry.s2_gpf`
- `io_ptw_resp_bits_s2_gaf = entry.s2_gaf`

请求侧字段建议保留采集信息，便于 debug：

- `io_ptw_req_0_valid = sampled_req_valid`
- `io_ptw_req_0_bits_vpn = sampled_vpn`
- `io_ptw_req_0_bits_s2xlate = sampled_s2xlate`
- `io_ptw_req_0_ready = 1`

## 6. miss 或非法映射处理

当前最终方案中，L2TLB request 查不到 entry 时不再进入独立 miss policy 分支，而是由
`common_data_transaction::get_or_create_tlb_entry_by_req()` 自动创建 `tlb_entry_by_key`
表项并返回 response。这样可以支持进程切换后 `A -> B -> A` 的 key 复用，也避免
per-uid TLB 表导致同一 `{vpn,asid,vmid,s2xlate}` 被重复建多份。

因此旧的缺项策略参数已删除，不再提供 fault/fatal/idle 三种 miss policy。非法映射、
权限异常和严格一致性检查后续应通过 PTE 权重、PTE 合法性修正以及 RM check 机制处理，
而不是通过 L2TLB miss policy 处理。

后续可通过 plus 控制：

- `+MEMBLOCK_L2TLB_SEQ_EN=0/1`
- `+MEMBLOCK_L2TLB_MIN_LATENCY=N`
- `+MEMBLOCK_L2TLB_MAX_LATENCY=N`
- `+MEMBLOCK_L2TLB_IDLE_STOP_CYCLE=N`

## 7. 信号合法性约束

TLB 表生成或 entry 修正阶段需要保证：

- `s1_entry_v = 0` 时，权限位不能被当作有效翻译使用。
- `s1_pf`、`s1_af`、`s2_gpf`、`s2_gaf` 的组合需要来自约束或异常计划，不允许完全无约束随机。
- `s2_entry_*` 仅在 `s2xlate` 有效路径下起作用；非 s2 翻译时可填默认值但不能影响 DUT 判断。
- `ppn`、`paddr`、`level` 要一致：
  - 4KB 页默认 `paddr[47:12]` 对应 PPN。
  - superpage 需要按 `level` mask 低位。
- `s1_ppn_low[]`、`s1_valididx[]`、`s1_pteidx[]` 需要和页表 walk 结果一致，不能随意填。

## 8. 与其他 task 的协同

`l2tlb_base_sequence.sv` 在 request 到来时触发 live TLB entry 建/查。uid record 由 issue fire 预登记，entry 确定后回填。

协同关系：

- TLB entry builder：负责根据 request `vpn/s2xlate`、runtime CSR 和 PTE 权重生成 `memblock_tlb_entry`。
- L2TLB connect：负责把 `inner_dtlbRepeater.io_ptw_req_0` 采样到 interface，并按
  编译期宏 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 决定是否把 driver response force 到
  `inner_ptw.io_tlb_1`；runtime `MEMBLOCK_L2TLB_SEQ_EN` 只控制 sequence 是否主动回包。
- l2tlb base sequence：按 request `vpn/s2xlate` 和 runtime CSR 建/查 entry，填 response，并回填 uid record。
- sfence/hfence 失效：fence monitor 采到 `io_ooo_to_mem_sfence_*` 后进入 raw queue，由 `dispatch_monitor_event_adapter::drain_sfence_events()` 调用公共数据层删除命中的 live `tlb_entry_by_key`；uid record 不删除，后续同 key request 再来时会 miss 并自动重建 entry。
- replay/redirect/flush task：若发生 flush，需要清理或标记 request context，避免旧请求被响应。

## 9. 集成文件清单

后续 coding 预计涉及：

1. `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
   - 新增 `get_or_create_tlb_entry_by_req()` 和 uid record 回填 API。
2. `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`
   - 实现 base sequence。
3. `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
   - include `memblock_l2tlb_base_sequence.sv`。
4. `mem_ut/ver/ut/memblock/cfg/tb.f`
   - 若 L2TLB agent 尚未纳入编译，需要添加对应 filelist。
5. `mem_ut/ver/ut/memblock/env/plus.sv`、`seq/plus_cfg/default.cfg`、
   `seq/base_seq/seq_csr_common.sv`
   - 新增 `MEMBLOCK_L2TLB_*` 参数。

注意：`tb/L2tlb_agent_connect.sv` 当前连接的是 `inner_dtlbRepeater` 与
`inner_ptw.io_tlb_1`。若后续 RTL 重新生成导致层级名变化，需要按
`memblock_latest_dut_adapt_rule.md` 重新确认层级路径、位宽和方向。

## 10. 验证计划

### 10.1 静态检查

- package include 顺序正确。
- `common_data_transaction.sv` 在 sequence include 前可见。
- agent package 在 `tc_pkg.sv` 中已 import。
- sequence transaction 类型与 driver/sequencer 类型一致。

### 10.2 基础用例

1. 单条 load 触发 DTLB miss。
2. `inner_dtlbRepeater.io_ptw_req_0_valid` 发起 `vpn/s2xlate` 请求。
3. TLB 表存在命中 entry。
4. `l2tlb_base_sequence` 通过 `inner_ptw.io_tlb_1_resp_*` 返回合法 response。
5. DUT 不再保持 TLB miss 状态。

### 10.3 异常用例

- TLB 表查不到 entry 时，通过 `get_or_create_tlb_entry_by_req()` 自动创建 entry 并返回 response。
- `s1_pf` 命中，检查后续异常路径。
- `s2_gpf` 命中，检查 guest page fault 路径。
- flush 后旧 request context 被清理，不产生过期响应。

### 10.4 当前验证状态

本轮 sfence/hfence entry 级失效实现后已完成基础编译和 sanity 验证：

- `make eda_compile tc=tc_sanity mode=base_fun`：通过。
- `make eda_run tc=tc_sanity mode=base_fun`：`TEST CASE PASSED`。
- `UVM_ERROR=0`，`UVM_FATAL=0`。

该验证覆盖了代码可编译、L2TLB responder 可正常启动/idle 退出、基础仿真流程未被 raw sfence queue 和 by-key TLB 逻辑破坏。尚未覆盖 directed sfence/hfence 命中删除、`g` 位保护和指定 ASID/VMID/addr 的细粒度行为，后续需要补 directed testcase。

## 11. 验收标准

1. `l2tlb_base_sequence.sv` 能从 common TLB 表读 entry，不自行随机生成关键响应字段。
2. 当前实现查询路径以采集到的 `vpn/s2xlate` 和 runtime CSR key 为主，直接查/建 `tlb_entry_by_key`，不扫描 per-uid TLB 表兜底。
3. response transaction 字段完整回填到 driver。
4. request miss 由 `get_or_create_tlb_entry_by_req()` 自动建表；fault/permission 行为由 PTE 权重和 entry 合法性修正控制，不再由 L2TLB missing-entry policy 控制。
5. sequence 与 replay/redirect/flush 的 request context 生命周期不冲突。
6. L2TLB connect 默认 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1`，由 L2TLB agent 接管
   DTLB/L2TLB response 通路；`MEMBLOCK_L2TLB_SEQ_EN` 默认 1，responder sequence 默认主动
   回包。需要纯观察 DUT 原始 response 时，必须编译期覆盖 takeover 宏为 0；启用接管时
   只驱动 DTLB/L2TLB 交互点，不接 L2Cache 下游路径。
7. sfence/hfence 只删除 live `tlb_entry_by_key` 中匹配的 entry，不能删除主表、状态表或 `uid_tlb_record_by_uid`。
