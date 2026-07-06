# sfence/hfence TLB entry 级失效修改 review 清单

## 1. 背景和目标

本轮修改实现 `sfence/hfence` 对 `common_data_transaction::tlb_entry_by_key` 的 entry 级失效。

目标行为：

- fence monitor 只采集 DUT 接口 raw fact，不直接改公共表。
- raw sfence/hfence 事件进入 `memblock_sync_pkg` FIFO。
- `dispatch_monitor_event_adapter` 在 runtime CSR 同步后 drain sfence 事件。
- `common_data_transaction` 统一 decode、match、delete。
- 只删除 live TLB cache：`tlb_entry_by_key`。
- 不删除 `main_table_by_uid[]`、`status_by_uid[]`、`uid_tlb_record_by_uid[]`。

当前验证：

```bash
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

结果：编译通过，`tc_sanity/base_fun` 通过，`UVM_ERROR=0`，`UVM_FATAL=0`。

## 2. Review 总表

| 编号 | 修改点 | 需要 review 的问题 | 当前结论 |
|---|---|---|---|
| R1 | fence monitor 采 raw sfence | 是否只采 DUT output raw fact，不直接改公共状态 | 符合，monitor 只 push raw queue |
| R2 | `memblock_sync_pkg` raw queue | sfence 是否必须 FIFO，而不是 latest snapshot | 符合，sfence 是离散失效事件，不能覆盖 |
| R3 | adapter drain 顺序 | sfence 是否在 runtime CSR 同步后处理 | 符合，统一 service loop 通过 `collect_runtime_context_events()` 显式先 `drain_csr_events()`，再 `drain_sfence_events()` |
| R4 | entry 匹配语义 | 普通 sfence、hfence_v、hfence_g、`g` 位和地址匹配是否合理 | 当前实现覆盖基础语义，directed testcase 待补 |
| R5 | 删除范围 | 是否只删 `tlb_entry_by_key`，不破坏 uid 追踪 | 符合 |
| R6 | VCS 兼容修复 | 是否只修语法兼容，不改变行为 | 符合 |
| R7 | 文档同步 | 方案和源码分析是否写清实现状态与验证状态 | 已同步 |

## 3. 修改逻辑和源码片段

### 3.1 fence monitor：采集 raw sfence

文件：

- `mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_monitor.sv`

修改逻辑：

- monitor 每拍从 `fence_agent_agent_interface` 采 `io_ooo_to_mem_sfence_*`。
- reset 完成且 `sfence_valid=1` 时构造 `dispatch_raw_sfence_t`。
- 调用 `memblock_sync_pkg::push_raw_sfence(raw_sfence)`。
- monitor 不直接调用 `common_data_transaction`，避免 monitor 绕过公共状态 owner。

关键源码：

```systemverilog
if(this.vif.rst_n==1'b1 &&
   memblock_sync_pkg::reset_backend_done==1'b1 &&
   io_ooo_to_mem_sfence_valid==1'b1) begin
    raw_sfence = memblock_sync_pkg::make_empty_raw_sfence();
    raw_sfence.valid = 1'b1;
    raw_sfence.rs1   = io_ooo_to_mem_sfence_bits_rs1;
    raw_sfence.rs2   = io_ooo_to_mem_sfence_bits_rs2;
    raw_sfence.addr  = io_ooo_to_mem_sfence_bits_addr;
    raw_sfence.id    = io_ooo_to_mem_sfence_bits_id;
    raw_sfence.hv    = io_ooo_to_mem_sfence_bits_hv;
    raw_sfence.hg    = io_ooo_to_mem_sfence_bits_hg;
    raw_sfence.cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    memblock_sync_pkg::push_raw_sfence(raw_sfence);
end
```

Review 点：

- `push_raw_sfence()` 内部受 `dispatch_monitor_capture_en` 控制，testcase 收尾后不会继续积累 raw event。
- `raw_sfence` 声明放在 task 声明区，避免老 VCS 对过程块中间声明敏感。

### 3.2 memblock_sync_pkg：新增 raw sfence FIFO

文件：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`

修改逻辑：

- 新增 `dispatch_raw_sfence_t` 保存接口 raw 字段。
- 新增 `raw_sfence_q[$]`。
- 新增 `make_empty_raw_sfence()`、`push_raw_sfence()`、`pop_raw_sfence()`。
- `clear_raw_monitor_queues()` 和 `raw_monitor_queue_size()` 同步纳入 sfence queue。

关键源码：

```systemverilog
typedef struct {
    bit               valid;
    bit               rs1;
    bit               rs2;
    bit [49:0]        addr;
    bit [15:0]        id;
    bit               hv;
    bit               hg;
    longint unsigned  cycle;
} dispatch_raw_sfence_t;

dispatch_raw_sfence_t raw_sfence_q[$];
```

```systemverilog
function void push_raw_sfence(input dispatch_raw_sfence_t item);
    if (dispatch_monitor_capture_en && item.valid) begin
        raw_sfence_q.push_back(item);
    end
endfunction:push_raw_sfence

function bit pop_raw_sfence(output dispatch_raw_sfence_t item);
    if (raw_sfence_q.size() == 0) begin
        item = make_empty_raw_sfence();
        return 1'b0;
    end
    item = raw_sfence_q.pop_front();
    return 1'b1;
endfunction:pop_raw_sfence
```

Review 点：

- sfence/hfence 是离散失效事件，必须 FIFO 保存，不能像 CSR snapshot 一样只保留 latest。
- `clear_raw_monitor_queues()` 删除 `raw_sfence_q`，避免 reset/end check 后残留。

### 3.3 dispatch_monitor_event_adapter：独立 drain CSR 与 sfence

文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`

修改逻辑：

- `drain_csr_events()` 只同步 latest runtime CSR。
- `drain_sfence_events()` 独立消费 raw sfence queue。
- 统一 service loop 通过 `memblock_dispatch_base_sequence::collect_runtime_context_events()` 显式先调用 `drain_csr_events()`，再调用 `drain_sfence_events()`。
- `drain_sfence_events()` 只桥接 raw queue 到公共数据 API，不手写匹配规则。

`drain_csr_events()` 的作用：

- 它是 monitor raw event 到公共状态表之间的同步入口。
- 每次被调度时，先从 `memblock_sync_pkg` 取出最新一份 raw CSR snapshot，并调用
  `common_data_transaction::apply_raw_csr_runtime()` 更新 `mmu_csr_runtime_state`。
- 该入口不再消费 `raw_sfence_q`，避免 L2TLB lookup、writeback/ctrl drain 等 CSR-only 路径隐式处理 sfence/hfence。
- 这样做的原因是 `sfence/hfence` 匹配可能需要当前 CSR 上下文，例如
  `priv_virt` 判断普通 `sfence` 属于非虚拟化页表还是虚拟化页表，
  `hgatp_vmid` 判断 `hfence.vvma` 或虚拟态 `sfence` 应该命中哪个 VMID。
- 因此 `drain_csr_events()` 不只是“处理 CSR event”，也承担了
  “保证后续 sfence/hfence 看到最新 CSR 上下文”的调度屏障作用。

关键源码：

```systemverilog
function void drain_csr_events();
    memblock_sync_pkg::dispatch_raw_csr_t raw_csr;
    int unsigned raw_csr_seq;

    ensure_handles();
    if (memblock_sync_pkg::get_latest_raw_csr(raw_csr, raw_csr_seq)) begin
        data.apply_raw_csr_runtime(raw_csr, raw_csr_seq);
    end
    drain_sfence_events();
endfunction:drain_csr_events

function void drain_sfence_events();
    memblock_sync_pkg::dispatch_raw_sfence_t raw_sfence;

    ensure_handles();
    while (memblock_sync_pkg::pop_raw_sfence(raw_sfence)) begin
        void'(data.apply_raw_sfence(raw_sfence));
    end
endfunction:drain_sfence_events
```

Review 点：

- drain 顺序保证 sfence/hfence 处理时看到最新 runtime CSR。
- adapter 不直接删除表，删除集中在 `common_data_transaction`。

### 3.4 memblock_dispatch_types：内部 sfence payload

文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`

修改逻辑：

- 新增 `memblock_sfence_payload_t`，作为公共数据层内部 payload。
- raw `rs1` decode 成 `ignore_addr`。
- raw `rs2` decode 成 `ignore_id`。

关键源码：

```systemverilog
typedef struct {
    bit               valid;
    bit               ignore_addr;
    bit               ignore_id;
    bit [49:0]        addr;
    bit [15:0]        id;
    bit               hv;
    bit               hg;
    longint unsigned  cycle;
} memblock_sfence_payload_t;
```

Review 点：

- payload 不带 uid，因为 sfence/hfence 失效对象是 TLB entry cache，不是单条 transaction。
- 使用非 packed struct，避免包含 `longint` 时的工具兼容风险。

### 3.5 common_data_transaction：entry 级匹配和删除

文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

修改逻辑：

- `decode_raw_sfence()`：raw event 转内部 payload。
- `sfence_vpn_match()`：按 entry `level` 做 VPN tag 匹配。
- `sfence_match_entry()`：判断某个 `tlb_entry_by_key[key]` 是否被本次 sfence/hfence 命中。
- `apply_sfence_invalidate()`：收集命中 key 后删除 `tlb_entry_by_key` entry。
- `apply_raw_sfence()`：adapter 调用入口。

关键源码：

```systemverilog
function memblock_sfence_payload_t decode_raw_sfence(input memblock_sync_pkg::dispatch_raw_sfence_t raw);
    memblock_sfence_payload_t payload;

    payload = '{default:'0};
    payload.valid       = raw.valid;
    payload.ignore_addr = raw.rs1;
    payload.ignore_id   = raw.rs2;
    payload.addr        = raw.addr;
    payload.id          = raw.id;
    payload.hv          = raw.hv;
    payload.hg          = raw.hg;
    payload.cycle       = raw.cycle;
    return payload;
endfunction:decode_raw_sfence
```

```systemverilog
function bit sfence_vpn_match(input bit [51:0] entry_vpn,
                              input bit [1:0] entry_level,
                              input bit [49:0] addr);
    bit [51:0] addr_vpn;

    addr_vpn = {14'b0, addr[49:12]};
    case (entry_level)
        2'd0: return entry_vpn[37:0]  == addr_vpn[37:0];
        2'd1: return entry_vpn[37:9]  == addr_vpn[37:9];
        2'd2: return entry_vpn[37:18] == addr_vpn[37:18];
        default: return entry_vpn[37:27] == addr_vpn[37:27];
    endcase
endfunction:sfence_vpn_match
```

```systemverilog
function bit sfence_match_entry(input memblock_sfence_payload_t payload,
                                input memblock_tlb_lookup_key_t key,
                                input memblock_tlb_entry entry);
    if (!payload.valid) begin
        return 1'b0;
    end
    if (entry == null) begin
        `uvm_fatal("COMMON_DATA", "sfence_match_entry got null entry")
    end
    if (!payload.ignore_addr && !sfence_vpn_match(key.vpn, entry.level, payload.addr)) begin
        return 1'b0;
    end

    if (payload.hg) begin
        if (!(key.s2xlate == 2'd2 || key.s2xlate == 2'd3)) begin
            return 1'b0;
        end
        if (!payload.ignore_id && key.vmid != payload.id) begin
            return 1'b0;
        end
        return 1'b1;
    end

    if (payload.hv) begin
        if (!(key.s2xlate == 2'd1 || key.s2xlate == 2'd3)) begin
            return 1'b0;
        end
        if (key.s2xlate == 2'd3 &&
            mmu_csr_state != null &&
            key.vmid != mmu_csr_state.hgatp_vmid) begin
            return 1'b0;
        end
        if (!payload.ignore_id) begin
            if (entry.pte_g) begin
                return 1'b0;
            end
            if (key.asid != payload.id) begin
                return 1'b0;
            end
        end
        return 1'b1;
    end

    if (key.s2xlate == 2'd2) begin
        return 1'b0;
    end
    if (!payload.ignore_id) begin
        if (entry.pte_g) begin
            return 1'b0;
        end
        if (key.asid != payload.id) begin
            return 1'b0;
        end
    end
    return 1'b1;
endfunction:sfence_match_entry
```

```systemverilog
function int unsigned apply_sfence_invalidate(input memblock_sfence_payload_t payload);
    memblock_tlb_lookup_key_t delete_keys[$];

    if (!payload.valid) begin
        return 0;
    end
    foreach (tlb_entry_by_key[key]) begin
        if (sfence_match_entry(payload, key, tlb_entry_by_key[key])) begin
            delete_keys.push_back(key);
        end
    end
    foreach (delete_keys[idx]) begin
        tlb_entry_by_key.delete(delete_keys[idx]);
    end
    return delete_keys.size();
endfunction:apply_sfence_invalidate
```

```systemverilog
function int unsigned apply_raw_sfence(input memblock_sync_pkg::dispatch_raw_sfence_t raw);
    return apply_sfence_invalidate(decode_raw_sfence(raw));
endfunction:apply_raw_sfence
```

Review 点：

- 删除前先收集 key，避免遍历关联数组时同时删除造成工具或行为风险。
- 删除范围只限 `tlb_entry_by_key`。
- `uid_tlb_record_by_uid` 保留，便于已发射 uid 的历史 PTE/debug 追踪。
- 当前基础 sanity 没有覆盖 directed sfence/hfence 命中删除，需要后续 testcase 补：
  - 普通 sfence 指定 ASID。
  - 普通 sfence 全局 flush。
  - `entry.pte_g=1` 时 ASID 精确 flush 不误删。
  - `hfence_v` 指定 ASID/VMID。
  - `hfence_g` 指定 VMID。
  - 指定 addr 时不同 `entry.level` 的 VPN tag 匹配。

### 3.6 VCS 兼容修复：函数返回值切片

文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_tlb_entry.sv`

修改逻辑：

- VCS Q-2020.03 不接受 `function_call()[1:0]`。
- 改成临时变量保存返回值，再切片。

关键源码：

```systemverilog
MEMBLOCK_TLB_LEVEL_FIXED: begin
    fixed_level = seq_csr_common::get_tlb_level_fixed_value();
    return fixed_level[1:0];
end
```

Review 点：

- 只改变写法，不改变 fixed level 的取值语义。

### 3.7 VCS 兼容修复：extern task 参数名

文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`

修改逻辑：

- VCS 要求 extern method declaration 与 definition 参数名一致。
- 将声明中的 `idle_count` 改为实现使用的 `send_count`。

关键源码：

```systemverilog
extern virtual task send_l2tlb_cycle(input int unsigned send_count,
                                     output bit has_progress);
```

Review 点：

- 只修声明一致性，不改变 send/idle 计数逻辑。

## 4. 待补 directed 验证

当前 `tc_sanity` 只证明基础编译和 sanity flow 未被破坏。建议后续新增 directed testcase 覆盖：

1. 建立两个不同 ASID 的同 VPN entry，普通 sfence 指定 ASID 只删目标 ASID。
2. 建立 `pte_g=1` entry，普通 sfence 指定 ASID 不删除该 entry。
3. `ignore_id=1` 普通 sfence 删除非 stage2 相关 entry。
4. `hfence_g` 指定 VMID 删除 stage2/G-stage entry。
5. `hfence_v` 指定 ASID，并在 G-stage 场景下检查当前 VMID 约束。
6. 删除后同 key request 再来，`get_or_create_tlb_entry_by_req()` miss 并自动重建 entry。

## 5. 提交前检查清单

- [ ] `memblock_tlb_entry.sv` 作为新增文件纳入 git。
- [ ] `AI_DOC/plan/test_framework/plan/do/tlb_lookup_epoch_issue_and_fix_plan_20260614.md` 作为新增文件纳入 git。
- [ ] 确认 `git diff --check` 无空白错误。
- [ ] 确认 `make eda_compile tc=tc_sanity mode=base_fun` 通过。
- [ ] 确认 `make eda_run tc=tc_sanity mode=base_fun` 通过。
- [ ] 后续 directed testcase 完成后，把本文件第 4 节的待验证项逐项关闭。
