# CSR Runtime Sync Flow

本文档说明 mem_ut 测试框架中 MMU CSR runtime mirror 的真实同步链路。CSR runtime 是 latest snapshot，不是 FIFO 事件：monitor 每拍采样 CSR interface，只有 payload 变化或软件 reset 要求 re-arm 时更新 `memblock_sync_pkg::latest_raw_csr`；service loop 或 L2TLB responder 通过 `drain_csr_events()` 把最新 snapshot 同步到 `common_data_transaction.mmu_csr_state`。V2 的 `hd_misalign_ld_enable`、`hd_misalign_st_enable` 和 `priv_debug` 也沿该链保存，但它们当前是 snapshot-only 字段，不进入 TLB key、主表、异常生成或 pass/fault/terminal。

## 1. 函数调用 Flow 图

### 1.1 术语与抽象功能说明

| 英文术语 | 当前 flow 中的中文含义 | 代码对象/状态落点 | 示例 |
|---|---|---|---|
| `latest snapshot` | 只保留最近一份 CSR 状态，不按拍排成 FIFO | `memblock_sync_pkg::latest_raw_csr` | CSR payload 变化时覆盖旧快照并递增 `latest_raw_csr_seq` |
| `raw struct` | monitor 采到的原始字段容器，只负责跨 monitor 和 adapter 传递 | `dispatch_raw_csr_t` | monitor 填 raw，adapter 再交给 runtime mirror |
| `snapshot-only` | 当前只保存和复制、不参与行为判断的字段 | `hd_misalign_ld/st_enable`、`priv_debug` | 字段改变会更新 snapshot，但不改变 TLB key 或 terminal |
| `semantic field` | 会改变翻译/权限上下文的字段 | satp/vsatp/hgatp、既有 privilege/PBMT 字段 | 这些字段改变才递增 `update_seq` |
| `re-arm epoch` | 清空 latest 后使 monitor 本地去重基线失效的代号 | `raw_csr_rearm_epoch` | reset 前后 CSR 相同时仍重新发布首份 snapshot |
| `capture` | 是否允许 monitor 把采样值发布到共享 raw 状态的开关，不控制 interface 采样本身 | `dispatch_monitor_capture_en` | capture=0 时仍每拍采样和按条件诊断，但 `push_raw_csr()` 不更新 latest |
| `payload baseline` | monitor 本地保存的上一份 CSR payload，用于去重比较 | `last_raw_csr`、`has_last_raw_csr` | re-arm 时清除 baseline，下一份相同 payload 也会发布 |
| `X/Z diagnosis` | 对四态 interface 值的 error-only 诊断，不是 raw drop/fatal gate | `TCNT_CHECK_SIG_XZ` | 发现 X/Z 时报告 `uvm_error`，后续 raw 发布条件仍独立判断 |
| `service loop` | 周期性消费 raw 事件并更新公共状态的软件循环 | `service_monitor_once()`、`drain_csr_events()` | 每轮先同步 CSR，再处理其它 runtime 事件 |

### 1.2 重点函数的抽象功能

| 函数/task | 抽象功能描述 |
|---|---|
| `csr_ctrl_agent_agent_monitor::mon_data()` | 每拍采样 CSR；只有 capture 开启且 reset/backend ready 时才发布 latest raw snapshot，不直接改 status。 |
| `memblock_sync_pkg::raw_csr_payload_changed()` | 判断当前 raw 是否需要覆盖 latest snapshot，不解释字段的行为语义。 |
| `memblock_sync_pkg::push_raw_csr()` | 保存有效 latest snapshot 并递增 snapshot 序号，不建立 CSR FIFO。 |
| `dispatch_monitor_event_adapter::drain_csr_events()` | 读取 latest snapshot 并交给公共数据层按序号幂等应用。 |
| `common_data_transaction::apply_raw_csr_runtime()` | 将 raw CSR 同步到 runtime mirror，不生成异常或终态事件。 |
| `mmu_csr_runtime_state::update_from_raw_csr()` | 复制完整 CSR 状态，仅用 semantic field 变化决定 `update_seq`。 |

```mermaid
flowchart TD
    A[csr_ctrl_agent_agent_monitor::run_phase] --> B[mon_data]
    V[clear_raw_monitor_queues] --> W[raw_csr_rearm_epoch++]
    W --> B
    B --> C{rst_n && reset_backend_done && dispatch_monitor_capture_en}
    C -->|no| D[清 has_last_raw_csr 或等待下一拍]
    C -->|yes| E[memblock_sync_pkg::make_empty_raw_csr]
    E --> F[填 raw_csr: satp/vsatp/hgatp/priv/pbmt/misalign/priv_debug]
    F --> G{raw_csr_payload_changed、首次采样或re-arm}
    G -->|yes| H[memblock_sync_pkg::push_raw_csr]
    G -->|no| I[只更新 last_raw_csr]
    H --> J[latest_raw_csr/latest_raw_csr_seq 更新]
    J --> K[service_real_dispatch_flow]
    K --> L[service_monitor_once]
    L --> M[collect_runtime_context_events]
    M --> N[dispatch_monitor_event_adapter::drain_csr_events]
    N --> O[memblock_sync_pkg::get_latest_raw_csr]
    O --> P[common_data_transaction::apply_raw_csr_runtime]
    P --> Q[mmu_csr_runtime_state::update_from_raw_csr]
    Q --> R[更新 mmu_csr_state；语义字段变化才更新 update_seq]
    R --> S[TLB key/build/uid record 使用最新 CSR]

    T[memblock_l2tlb_base_sequence::send_l2tlb_cycle] --> U[drain_csr_runtime_events]
    U --> N
```

### 1.3 函数调用 Flow 图整体文字伪代码

```text
CSR runtime sync 主流程：

1. CSR monitor 采样阶段：
   csr_ctrl_agent_agent_monitor::run_phase 调用 mon_data。
   mon_data 每拍从 csr_ctrl interface 采样 satp/vsatp/hgatp/priv/pbmt，以及 misalign enable/priv_debug；
   X/Z 诊断只由 xz_sw、rst_n 和 reset_backend_done 控制，不依赖 capture 开关。
   如果 raw_csr_rearm_epoch 变化：
     清空本地 has_last_raw_csr 和 last_raw_csr，强制下一份完整 snapshot 发布。
   如果 reset 未完成或 dispatch_monitor_capture_en 关闭：
     清空本地 last_raw_csr 有效标记，避免下一次 capture 误认为旧 snapshot 仍连续。
   如果 reset 完成且 capture 打开：
     创建 raw_csr。
     把当前 DUT CSR 信号写入 raw_csr。
     如果是首次/re-arm 采样，或 raw_csr_payload_changed 判断 payload 发生变化：
       调用 push_raw_csr 更新 latest_raw_csr 和 latest_raw_csr_seq。
     最后更新 monitor 本地 last_raw_csr。

2. service loop 同步阶段：
   service_real_dispatch_flow 每拍调用 service_monitor_once。
   service_monitor_once 先调用 collect_runtime_context_events。
   collect_runtime_context_events 只 drain_csr_events。
   返回 `service_monitor_once()` 后，唯一的 `service_l2tlb_sfence_events()` 另行处理 raw fence；它不属于 CSR runtime mirror 更新。
   drain_csr_events 从 memblock_sync_pkg 读取 latest raw CSR snapshot。
   如果 snapshot 有效且 seq 未重复：
     common_data_transaction::apply_raw_csr_runtime 更新 mmu_csr_state。
     mmu_csr_runtime_state::update_from_raw_csr 更新全部 snapshot 字段；只有既有翻译/权限语义字段变化时递增 update_seq。
     misalign enable/priv_debug 单独变化只更新 snapshot，不改变 TLB key 或行为版本。

3. L2TLB responder 同步阶段：
   L2TLB responder 收到 DTLB request 后，在建 TLB key 前调用 drain_csr_runtime_events。
   该路径只同步 CSR latest snapshot，不消费 sfence FIFO。
   后续 make_tlb_key_by_req / build_tlb_entry_for_key 使用最新 mmu_csr_state 选择 asid/vmid/s2xlate 相关 key。
```

## 2. `csr_ctrl_agent_agent_monitor::mon_data()`

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv`

抽象功能描述：`mon_data()` 是 CSR interface 的连续采样入口；它每拍读取 interface，X/Z 诊断独立于 capture，只有 raw 发布受 capture、reset/backend ready 和 payload 去重条件限制。

真实逻辑摘要：

```systemverilog
if (memblock_sync_pkg::raw_csr_rearm_epoch != last_raw_csr_rearm_epoch) begin
    has_last_raw_csr = 1'b0;
    last_raw_csr = memblock_sync_pkg::make_empty_raw_csr();
    last_raw_csr_rearm_epoch = memblock_sync_pkg::raw_csr_rearm_epoch;
end
if (memblock_sync_pkg::dispatch_monitor_capture_en != last_capture_en) begin
    has_last_raw_csr = 1'b0;
    last_capture_en = memblock_sync_pkg::dispatch_monitor_capture_en;
end
if (this.vif.rst_n!=1'b1 || memblock_sync_pkg::reset_backend_done!=1'b1) begin
    has_last_raw_csr = 1'b0;
end
if(this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1 &&
   memblock_sync_pkg::dispatch_monitor_capture_en==1'b1) begin
    raw_csr = memblock_sync_pkg::make_empty_raw_csr();
    raw_csr.valid             = 1'b1;
    raw_csr.satp_mode         = io_ooo_to_mem_tlbCsr_satp_mode;
    raw_csr.satp_asid         = io_ooo_to_mem_tlbCsr_satp_asid;
    raw_csr.vsatp_mode        = io_ooo_to_mem_tlbCsr_vsatp_mode;
    raw_csr.hgatp_mode        = io_ooo_to_mem_tlbCsr_hgatp_mode;
    raw_csr.hgatp_vmid        = io_ooo_to_mem_tlbCsr_hgatp_vmid;
    raw_csr.priv_virt         = io_ooo_to_mem_tlbCsr_priv_virt;
    raw_csr.priv_dmode        = io_ooo_to_mem_tlbCsr_priv_dmode;
    raw_csr.hd_misalign_ld_enable = io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
    raw_csr.hd_misalign_st_enable = io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
    raw_csr.priv_debug        = io_ooo_to_mem_tlbCsr_priv_debug;
    raw_csr.m_pbmt_en         = io_ooo_to_mem_tlbCsr_mPBMTE;
    raw_csr.h_pbmt_en         = io_ooo_to_mem_tlbCsr_hPBMTE;
    raw_csr.cycle             = $time;
    if (!has_last_raw_csr ||
        memblock_sync_pkg::raw_csr_payload_changed(last_raw_csr, raw_csr)) begin
        memblock_sync_pkg::push_raw_csr(raw_csr);
        has_last_raw_csr = 1'b1;
    end
    last_raw_csr = raw_csr;
end
```

中文伪代码：

```text
每个 monitor clock：
  采样 DUT 输出的 satp/vsatp/hgatp/priv/pbmt CSR 和 misalign enable/priv_debug 信号。
  X/Z 检查条件满足时诊断三个 snapshot-only 字段；宏只报告 uvm_error，不作为 raw 发布 gate。
  如果 raw_csr_rearm_epoch 变化：
    清空 has_last_raw_csr 和 last_raw_csr，记录新 epoch，使软件清表前的去重基线失效。
  如果 capture enable 状态变化：
    清空本地 has_last_raw_csr，保证下一次 capture 会推送完整 snapshot。
  如果 reset 未完成：
    清空本地 has_last_raw_csr，不发布 reset 阶段的 snapshot。
  如果 reset 完成且 capture 打开：
    调用 make_empty_raw_csr 创建有确定默认值的 raw_csr。
    把当前 CSR 信号写入 raw_csr，并记录采样时间。
    如果本地没有上一份 raw_csr，或 raw_csr_payload_changed 判断 payload 已变化：
      调用 push_raw_csr，把这份 snapshot 写成全局 latest CSR，并递增全局 snapshot 序号。
      置 has_last_raw_csr=1。
    保存 last_raw_csr，用于下一拍变化比较。
```

功能解释：

该 monitor 是 CSR runtime 的连续采样入口。它每拍读取 interface；在 `xz_sw`、reset/backend ready 满足时执行 X/Z 诊断，而只有 capture 打开时才在首次、re-arm 或 payload 变化时发布 latest snapshot。

输入/输出：

- 输入：`csr_ctrl_agent_agent_interface` 上的 `io_ooo_to_mem_tlbCsr_*`、`io_ooo_to_mem_csrCtrl_hd_misalign_*_enable` 信号、`rst_n`、`reset_backend_done`、`dispatch_monitor_capture_en`、`raw_csr_rearm_epoch`。
- 输出：调用 `memblock_sync_pkg::push_raw_csr()` 更新 `latest_raw_csr`。

内部子调用：

- `make_empty_raw_csr()`：生成默认无效 CSR raw struct，避免未赋字段残留。
- `raw_csr_payload_changed()`：比较关心的 CSR payload 和 changed pulse。
- `push_raw_csr()`：写全局 latest snapshot 并递增 sequence。
- `raw_csr_rearm_epoch`：由 `clear_raw_monitor_queues()` 递增，要求 monitor 丢弃本地去重 baseline。

## 3. `memblock_sync_pkg::raw_csr_payload_changed()`

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`

抽象功能描述：该函数比较上一份和当前份 CSR raw，决定是否需要发布新的 latest snapshot；它不决定字段是否进入 TLB key 或异常模型。

真实逻辑摘要：

```systemverilog
function bit raw_csr_payload_changed(input dispatch_raw_csr_t prev,
                                     input dispatch_raw_csr_t cur);
    return
        prev.satp_mode         != cur.satp_mode         ||
        prev.satp_asid         != cur.satp_asid         ||
        prev.vsatp_mode        != cur.vsatp_mode        ||
        prev.vsatp_asid        != cur.vsatp_asid        ||
        prev.hgatp_mode        != cur.hgatp_mode        ||
        prev.hgatp_vmid        != cur.hgatp_vmid        ||
        prev.priv_virt         != cur.priv_virt         ||
        prev.priv_dmode        != cur.priv_dmode        ||
        prev.hd_misalign_ld_enable != cur.hd_misalign_ld_enable ||
        prev.hd_misalign_st_enable != cur.hd_misalign_st_enable ||
        prev.priv_debug        != cur.priv_debug        ||
        prev.m_pbmt_en         != cur.m_pbmt_en         ||
        prev.h_pbmt_en         != cur.h_pbmt_en         ||
        (cur.satp_changed      && !prev.satp_changed)   ||
        (cur.vsatp_changed     && !prev.vsatp_changed)  ||
        (cur.hgatp_changed     && !prev.hgatp_changed)  ||
        (cur.priv_virt_changed && !prev.priv_virt_changed);
endfunction:raw_csr_payload_changed
```

中文伪代码：

```text
比较上一份和当前 CSR snapshot：
  依次比较 satp/vsatp/hgatp 的 mode、asid/vmid、ppn 等稳定字段。
  再比较 priv、PBMT 和 snapshot-only 的 misalign enable/priv_debug 字段。
  再检查 satp/vsatp/hgatp/priv_virt changed pulse 是否从 0 上升为 1。
  任一条件成立就返回 true，要求 monitor 发布新的 latest snapshot。
  所有条件均不成立则返回 false，保留现有 latest snapshot 和序号。
```

功能解释：

该函数决定 monitor 是否需要推送新的 CSR snapshot。它既比较稳定 CSR 字段，也比较 changed pulse 的上升语义。

输入/输出：

- 输入：上一份 raw CSR、当前 raw CSR。
- 输出：返回是否需要更新 latest snapshot。

## 4. `memblock_sync_pkg::push_raw_csr()` / `get_latest_raw_csr()`

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`

抽象功能描述：这两个函数分别写入和读取 latest CSR snapshot；写入侧受 capture/valid 约束，读取侧只返回当前最新值和序号。

真实逻辑摘要：

```systemverilog
function void push_raw_csr(input dispatch_raw_csr_t item);
    if (dispatch_monitor_capture_en && item.valid) begin
        latest_raw_csr = item;
        latest_raw_csr_valid = 1'b1;
        latest_raw_csr_seq++;
    end
endfunction:push_raw_csr

function bit get_latest_raw_csr(output dispatch_raw_csr_t item,
                                output int unsigned seq);
    seq = latest_raw_csr_seq;
    if (!latest_raw_csr_valid) begin
        item = make_empty_raw_csr();
        return 1'b0;
    end
    item = latest_raw_csr;
    return 1'b1;
endfunction:get_latest_raw_csr
```

中文伪代码：

```text
push_raw_csr：
  如果 capture 打开且 item 有效：
    用 item 覆盖 latest_raw_csr，只保留最新 CSR 状态。
    标记 latest_raw_csr_valid=1。
    latest_raw_csr_seq 加一，使 consumer 能识别这是一份尚未应用的新 snapshot。
  否则不修改 latest snapshot 或序号。

get_latest_raw_csr：
  先把当前 latest_raw_csr_seq 写给调用者。
  如果 latest snapshot 无效：
    调用 make_empty_raw_csr 输出确定的空结构并返回 false。
  如果有效：
    输出 latest_raw_csr 并返回 true；读取不会删除或改变 snapshot。
```

功能解释：

CSR runtime 使用 latest snapshot 模型。`push_raw_csr()` 覆盖旧 snapshot 并递增 seq，`get_latest_raw_csr()` 返回当前最新值。

输入/输出：

- 输入：raw CSR snapshot。
- 输出：`latest_raw_csr`、`latest_raw_csr_valid`、`latest_raw_csr_seq`；清空路径另递增 `raw_csr_rearm_epoch`。

## 5. `dispatch_monitor_event_adapter::drain_csr_events()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`

抽象功能描述：该 adapter task 从共享 sync package 取得 latest CSR，并将它交给公共数据对象按序号去重应用；它不直接修改 TLB entry。

真实逻辑摘要：

```systemverilog
function void drain_csr_events();
    memblock_sync_pkg::dispatch_raw_csr_t raw_csr;
    int unsigned raw_csr_seq;

    ensure_handles();
    if (memblock_sync_pkg::get_latest_raw_csr(raw_csr, raw_csr_seq)) begin
        data.apply_raw_csr_runtime(raw_csr, raw_csr_seq);
    end
endfunction:drain_csr_events
```

中文伪代码：

```text
调用 ensure_handles，保证 adapter 已取得唯一公共 common_data_transaction；该调用不消费事件。
调用 get_latest_raw_csr 读取 latest raw CSR 和序号：
  如果函数返回 false，说明当前没有可应用 snapshot，直接结束且不修改 runtime mirror。
  如果函数返回 true，取得 raw_csr 和 raw_csr_seq。
调用 data.apply_raw_csr_runtime：
  由公共数据对象按 valid 和序号去重，再把 snapshot 应用到 CSR runtime mirror。
```

功能解释：

adapter 从 sync_pkg 读取 latest CSR snapshot，并把它同步到 `common_data_transaction` 的 runtime CSR mirror。

输入/输出：

- 输入：`latest_raw_csr` 和 `latest_raw_csr_seq`。
- 输出：可能更新 `data.mmu_csr_state`。

内部子调用：

- `ensure_handles()`：保证 `common_data_transaction` 可用。
- `get_latest_raw_csr()`：读取 latest snapshot，不出队 FIFO。
- `apply_raw_csr_runtime()`：按 seq 去重后更新 runtime mirror。

## 6. `common_data_transaction::apply_raw_csr_runtime()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

抽象功能描述：该函数是 runtime mirror 的公共写入口，过滤无效或重复 snapshot 后调用 runtime state 更新；它不把 snapshot-only 字段转成主表或终态行为。

真实逻辑摘要：

```systemverilog
function void apply_raw_csr_runtime(input memblock_sync_pkg::dispatch_raw_csr_t raw,
                                    input int unsigned raw_csr_seq);
    if (!raw.valid) begin
        return;
    end
    if (raw_csr_seq == last_applied_raw_csr_seq) begin
        return;
    end
    if (mmu_csr_state == null) begin
        mmu_csr_state = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
        mmu_csr_state.reset();
    end
    mmu_csr_state.update_from_raw_csr(raw);
    last_applied_raw_csr_seq = raw_csr_seq;
endfunction:apply_raw_csr_runtime
```

中文伪代码：

```text
如果 raw.valid=0：
  直接返回，不创建 runtime state，也不记录序号。
如果 raw_csr_seq 已等于 last_applied_raw_csr_seq：
  直接返回，避免多个 service 调用重复应用同一 latest snapshot。
如果 mmu_csr_state 尚未创建：
  创建对象并调用 reset，建立确定的 CSR 默认值。
调用 update_from_raw_csr：
  复制完整 raw CSR，并由 runtime state 自己区分语义字段与 snapshot-only 字段。
最后记录 last_applied_raw_csr_seq，使后续相同序号被过滤。
```

功能解释：

该函数是公共 data owner 应用 CSR snapshot 的唯一落点。它用 `last_applied_raw_csr_seq` 防止同一个 latest snapshot 在多个 service 调用中重复应用。

输入/输出：

- 输入：raw CSR snapshot、raw CSR seq。
- 输出：`mmu_csr_state` 创建/更新，`last_applied_raw_csr_seq` 更新。

## 7. `mmu_csr_runtime_state::update_from_raw_csr()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv`

抽象功能描述：该函数复制 raw CSR 的全部字段，并把翻译/权限语义字段变化与 snapshot-only 字段变化分开；只有前者推进 `update_seq`。

真实逻辑摘要：

```systemverilog
changed =
    satp_mode  != raw.satp_mode         ||
    satp_asid  != raw.satp_asid         ||
    vsatp_mode != raw.vsatp_mode        ||
    vsatp_asid != raw.vsatp_asid        ||
    hgatp_mode != raw.hgatp_mode        ||
    hgatp_vmid != raw.hgatp_vmid        ||
    priv_virt  != raw.priv_virt         ||
    raw.satp_changed                    ||
    raw.vsatp_changed                   ||
    raw.hgatp_changed                   ||
    raw.priv_virt_changed;

satp_mode  = raw.satp_mode;
satp_asid  = raw.satp_asid;
vsatp_mode = raw.vsatp_mode;
vsatp_asid = raw.vsatp_asid;
hgatp_mode = raw.hgatp_mode;
hgatp_vmid = raw.hgatp_vmid;
priv_virt  = raw.priv_virt;
hd_misalign_ld_enable = raw.hd_misalign_ld_enable;
hd_misalign_st_enable = raw.hd_misalign_st_enable;
priv_debug = raw.priv_debug;
if (changed) begin
    update_seq++;
end
```

中文伪代码：

```text
如果 raw 无效，函数在片段之前直接返回，不改变 mirror。
按源码列出的 satp/vsatp/hgatp/priv/PBMT 字段和 changed pulse 计算 changed：
  这些字段代表翻译或权限上下文，任一变化都使 changed=1。
按源码顺序把 raw 中全部 CSR 字段复制到 runtime mirror：
  包括 hd_misalign_ld_enable、hd_misalign_st_enable 和 priv_debug。
如果 changed=1：
  update_seq 加一，记录一次翻译/权限语义版本变化。
如果只有三个 snapshot-only 字段变化：
  mirror 仍保存新值，但 changed=0，因此 update_seq 保持不变。
```

功能解释：

runtime mirror 保存当前 MMU CSR 状态，并用 `update_seq` 记录会影响当前翻译/权限上下文的语义变化次数。三个 snapshot-only 字段也保存在同一对象中，但不参与 `changed`，因此单独变化不会改变 `update_seq`。后续 TLB key、uid TLB record 和 responder 建表虽然持有该对象或其副本，当前只读取既有翻译字段。

输入/输出：

- 输入：raw CSR snapshot。
- 输出：`satp/vsatp/hgatp/priv/pbmt/misalign/priv_debug` 字段更新；仅语义字段变化时 `update_seq++`。

### 7.1 `mmu_csr_runtime_state::reset()`、`update_from_csr_ctrl()` 与 `copy_from()` 辅助链路

这三个 helper 不构成 monitor raw 主路径的额外事件源，但属于同一 runtime state 的字段完整性边界：`reset()` 提供默认值，`update_from_csr_ctrl()` 保留直接 transaction 兼容入口，`copy_from()` 把已同步的快照复制给 uid/TLB entry。它们都不直接修改主表、status、pass/fail 或 terminal。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv:46-129,246-273`。

`reset()` 关键片段：

```systemverilog
function void reset();
    satp_mode = '0;
    priv_imode = 2'd3;
    priv_dmode = 2'd3;
    hd_misalign_ld_enable = 1'b1;
    hd_misalign_st_enable = 1'b1;
    priv_debug = 1'b0;
    update_seq = 0;
endfunction:reset
```

中文伪代码：

```text
清空翻译地址空间和权限/PBMT runtime 字段；
按源码默认 privilege mode 初始化；
设置 misalign load/store=1/1、priv_debug=0；
清零 update_seq；
不发布任何 raw 或 TLB 事件。
```

`update_from_csr_ctrl()` 关键片段：

```systemverilog
function void update_from_csr_ctrl(input csr_ctrl_agent_agent_xaction csr_tr);
    bit changed;
    if (csr_tr == null) begin
        `uvm_fatal("MMU_CSR", "update_from_csr_ctrl got null transaction")
    end
    changed = satp_mode != csr_tr.io_ooo_to_mem_tlbCsr_satp_mode ||
              priv_virt != csr_tr.io_ooo_to_mem_tlbCsr_priv_virt ||
              csr_tr.io_ooo_to_mem_tlbCsr_satp_changed ||
              csr_tr.io_ooo_to_mem_tlbCsr_priv_virt_changed;
    hd_misalign_ld_enable = csr_tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
    hd_misalign_st_enable = csr_tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
    priv_debug = csr_tr.io_ooo_to_mem_tlbCsr_priv_debug;
    if (changed) update_seq++;
endfunction:update_from_csr_ctrl
```

中文伪代码：

```text
空 transaction 直接 fatal；
比较直接 transaction 中的语义字段和 changed pulse；
复制完整 CSR（包括三个 snapshot-only 字段）；
只有语义字段变化才递增 update_seq；
当前 monitor 主链路不调用该入口，它不产生额外行为事件。
```

`copy_from()` 关键片段：

```systemverilog
function void copy_from(input mmu_csr_runtime_state rhs);
    if (rhs == null) begin
        `uvm_fatal("MMU_CSR", "copy_from got null rhs")
    end
    hd_misalign_ld_enable = rhs.hd_misalign_ld_enable;
    hd_misalign_st_enable = rhs.hd_misalign_st_enable;
    priv_debug = rhs.priv_debug;
    update_seq = rhs.update_seq;
endfunction:copy_from
```

中文伪代码：

```text
源 snapshot 为空则 fatal；
否则复制完整 runtime CSR 和已有 update_seq；
复制不重新计算或递增版本号，也不反向修改公共 runtime；
目标快照供 uid/TLB entry 保存上下文，sfence 不因复制动作删除它。
```

## 8. `make_lookup_key()` / `expected_s2xlate()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv`

抽象功能描述：这些函数把当前 runtime 翻译上下文转换为 L2TLB lookup 所需的 key/阶段值；它们不读取 snapshot-only 字段。

真实逻辑摘要：

```systemverilog
function bit [1:0] expected_s2xlate(input bit is_hypervisor_inst);
    if (!(priv_virt || is_hypervisor_inst)) begin
        return 2'd0;
    end
    if (vsatp_mode != 4'd0 && hgatp_mode != 4'd0) begin
        return 2'd3;
    end
    if (vsatp_mode == 4'd0) begin
        return 2'd2;
    end
    if (hgatp_mode == 4'd0) begin
        return 2'd1;
    end
    return 2'd0;
endfunction:expected_s2xlate

function memblock_tlb_lookup_key_t make_lookup_key(input bit [63:0] vpn,
                                                   input bit [1:0] s2xlate);
    memblock_tlb_lookup_key_t key;

    key.vpn     = vpn[51:0];
    key.asid    = current_asid(s2xlate);
    key.vmid    = current_vmid(s2xlate);
    key.s2xlate = s2xlate;
    return key;
endfunction:make_lookup_key
```

中文伪代码：

```text
expected_s2xlate：
  如果当前既不是虚拟化状态，也不是 hypervisor 指令，返回 0。
  否则如果 vsatp 和 hgatp 都开启，返回 3，表示两阶段翻译。
  否则如果 vsatp 关闭，返回 2，表示只走 G-stage。
  否则如果 hgatp 关闭，返回 1，表示只走 VS-stage。
  其它情况返回 0。

make_lookup_key：
  key.vpn 取输入 vpn 低 52 位。
  调用 current_asid，根据传入 s2xlate 从 runtime CSR 选择当前 ASID。
  调用 current_vmid，根据传入 s2xlate 从 runtime CSR 选择当前 VMID。
  保存接口 request 提供的 s2xlate，不使用 update_seq 或 snapshot-only 字段。
  返回完整 lookup key，供 live TLB cache 查询或建表。
```

功能解释：

这组函数把 runtime CSR 转成 TLB 使用的上下文：`expected_s2xlate()` 用于 uid TLB record 预期路径，`make_lookup_key()` 用接口 request 的 `s2xlate` 生成 `{vpn, asid, vmid, s2xlate}` key。

输入/输出：

- 输入：runtime CSR 字段、vpn、s2xlate、是否 hypervisor 指令。
- 输出：预期 s2xlate 或 TLB lookup key。

## 9. 队列和状态说明

- `latest_raw_csr`：全局 latest snapshot，只保留最新 CSR runtime，不是 FIFO。
- `latest_raw_csr_seq`：每次 latest snapshot 更新时递增，`apply_raw_csr_runtime()` 用它去重。
- `raw_csr_rearm_epoch`：每次 `clear_raw_monitor_queues()` 递增；monitor 见变化后强制下一拍重新发布完整 snapshot。
- `mmu_csr_state`：`common_data_transaction` 内的运行时 CSR 镜像，TLB 建表和 uid TLB record 都从这里读实时 CSR。
- `update_seq`：CSR runtime 语义变化计数，目前用于 debug/追踪，不再作为 TLB key 命中强制条件。
- `hd_misalign_ld/st_enable`、`priv_debug`：当前只保存和复制；没有 sequence、主表、权限或终态 consumer。
- `raw_sfence_q`：独立 FIFO，和 CSR latest snapshot 分开；只由
  `dispatch_monitor_event_adapter::service_l2tlb_sfence_events()` 内部的 `drain_l2tlb_sfence_events()` 消费，并在 C4
  通过 `apply_due_sfence_invalidate()` 删除 logical live entry。

## 10. 分支优先级

1. monitor 先看 reset/capture，未打开 capture 时不推送 CSR。
2. monitor 只在首次、re-arm 或 payload changed 时 push，避免每拍重复刷新 latest snapshot，同时不丢软件 reset 后首份状态。
3. adapter 只读取 latest snapshot，不消费 sfence FIFO。
4. `apply_raw_csr_runtime()` 先按 valid/seq 去重，再更新 runtime mirror。
5. L2TLB responder 在建 key 前只 drain CSR，保证 request 使用最新 CSR，同时不抢先消费 sfence 事件。

## 11. 端到端行为总结

```text
场景 A：CSR payload 变化
  csr_ctrl monitor
  -> raw_csr_payload_changed=true
  -> push_raw_csr 更新 latest_raw_csr/latest_raw_csr_seq
  -> collect_runtime_context_events
  -> drain_csr_events
  -> apply_raw_csr_runtime
  -> update_from_raw_csr
  -> mmu_csr_state 更新

场景 B：CSR payload 未变化
  csr_ctrl monitor
  -> raw_csr_payload_changed=false
  -> 不 push_raw_csr
  -> latest_raw_csr_seq 不变
  -> apply_raw_csr_runtime 即使被调用也不会产生新变化

场景 C：L2TLB responder 建表前同步 CSR
  DTLB request valid
  -> send_l2tlb_cycle
  -> drain_csr_runtime_events
  -> drain_csr_events
  -> apply_raw_csr_runtime
  -> make_tlb_key_by_req 使用最新 asid/vmid/s2xlate 上下文

场景 D：只有 snapshot-only 字段变化
  -> raw_csr_payload_changed=true，latest_raw_csr_seq递增
  -> apply_raw_csr_runtime保存新值
  -> update_seq、TLB key、pass/fault/terminal保持不变

场景 E：软件 reset，CSR payload 未变化
  -> clear_raw_monitor_queues清空latest并递增raw_csr_rearm_epoch
  -> monitor丢弃本地last_raw_csr去重基线
  -> 下一拍重新push完整snapshot
  -> runtime不再停留在reset默认值
```

### 11.1 端到端文字伪代码

```text
场景 A：
  当 DUT CSR 输出变化时，monitor 把当前 CSR 信号封装成 raw_csr。
  raw_csr_payload_changed 返回 true 后，push_raw_csr 覆盖 latest snapshot 并递增 seq。
  service loop 下一拍先调用 drain_csr_events。
  drain_csr_events 读取 latest snapshot 并调用 apply_raw_csr_runtime。
  apply_raw_csr_runtime 按 seq 去重后更新 mmu_csr_state。
  后续 TLB key、uid record 和 responder 查表都读这个最新 runtime mirror。

场景 B：
  如果 CSR payload 没有变化，monitor 不 push。
  latest_raw_csr_seq 不递增。
  因此重复调用 drain_csr_events 不会造成重复 update_seq 或旧值覆盖。

场景 C：
  L2TLB responder 收到 DTLB request 后先同步 CSR latest snapshot。
  然后使用 request 的 s2xlate 和 runtime CSR 的 asid/vmid 生成 key。
  该路径不消费 sfence FIFO，sfence 仍由统一 service loop 顺序处理。

场景 E：
  reset_all_tables调用clear_raw_monitor_queues时，raw_csr_rearm_epoch递增。
  monitor即使一直看到capture enable=1，也会因为epoch变化清除本地去重baseline。
  下一拍当前CSR值即使与reset前相同，仍作为首份snapshot重新发布。

场景 D：
  misalign enable或priv_debug变化时，monitor仍发布新latest snapshot，避免后续读取到旧值。
  runtime mirror复制新值，但不会把纯观测变化计为翻译语义版本变化。
  当前任何sequence、主表或状态处理函数都不读取这些字段。
```
