# V2 CSR/control runtime 语义适配 Implementation Review

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `latest snapshot` | 只保留当前最新 CSR 状态的快照，不是按拍排队的 FIFO | `memblock_sync_pkg::latest_raw_csr` | CSR 从 `1/1/0` 变为 `0/1/0` 时覆盖旧快照并递增 `latest_raw_csr_seq` |
| `raw struct` | monitor 采样的原始字段容器，只负责跨 monitor/service loop 传递 | `dispatch_raw_csr_t`、`dispatch_raw_sfence_t` | monitor 先填 raw，再由 adapter 转入公共状态；raw 不直接改 status |
| `snapshot-only` | 当前只保存和复制、暂不被行为判断消费的字段 | `hd_misalign_ld_enable`、`hd_misalign_st_enable`、`priv_debug` | 字段变化更新 runtime mirror，但不改变 TLB key、pass/fail 或 terminal |
| `semantic field` | 会改变翻译或权限上下文的字段 | `satp_*`、`vsatp_*`、`hgatp_*`、`priv_*`、PBMT 字段 | 这些字段变化才使 `mmu_csr_runtime_state::update_seq` 递增 |
| `payload` | valid 伴随的实际控制字段集合 | sfence 的 `rs1/rs2/addr/id/hv/hg/flushPipe` | `sfence_valid=0` 时 payload 不进入 raw 失效事件 |
| `valid gating` | 只有 valid 明确为 1 才消费或检查 payload | `fence_agent_agent_monitor::mon_data()` | 无效 sfence 拍不产生 raw event，也不因无效 payload 的 X/Z 误报 |
| `transparent drive` | driver 不解释字段语义，只把 transaction 原值写入 interface | `fence_agent_agent_driver::send_pkt()` | transaction 的 `flushPipe=1` 就驱动 DUT 为 1，不触发额外状态机 |
| `analysis port` | monitor 向 RM/checker/scoreboard 发布 transaction 的独立出口 | agent base 的 `mon_item_port` | 本子计划不恢复该出口；monitor output 专项负责其分类和 consumer |
| `semantic version` | 表示翻译/权限上下文变化的 runtime 版本 | `mmu_csr_runtime_state::update_seq` | 单独改变 snapshot-only 字段不产生新 semantic version |
| `re-arm epoch` | 软件清空 latest CSR 后，要求 monitor 丢弃本地去重基线并重新发布首份 snapshot 的代号 | `memblock_sync_pkg::raw_csr_rearm_epoch` | `reset_all_tables()` 清表但 capture 仍为 1 时，下一拍相同 CSR payload 也必须重新发布 |

### 1.1 重点函数的抽象功能

| 函数/task | 抽象功能描述 |
|---|---|
| `csr_ctrl_agent_agent_monitor::mon_data()` | 从 CSR interface 采样 runtime 字段，在首次采样、re-arm 或 payload 改变时发布一份 latest raw snapshot；不直接更新公共 status。 |
| `memblock_sync_pkg::raw_csr_payload_changed()` | 判断当前 CSR 快照是否值得发布，包含 snapshot-only 字段，但不负责决定它们是否影响 TLB 语义。 |
| `memblock_sync_pkg::push_raw_csr()` | 在 capture 开启且 raw 有效时覆盖全局 latest CSR snapshot，并递增 snapshot 序号；不建立 CSR FIFO。 |
| `memblock_sync_pkg::clear_raw_monitor_queues()` | 清空 raw queue/latest CSR，并递增 re-arm epoch，保证 monitor 的本地去重状态同步失效。 |
| `common_data_transaction::apply_raw_csr_runtime()` | 按 snapshot 序号幂等地把 raw CSR 交给 runtime mirror；不负责生成异常、主表或终态。 |
| `mmu_csr_runtime_state::update_from_raw_csr()` | 保存所有 runtime CSR 字段，并只用 semantic field 的变化决定 `update_seq` 是否递增。 |
| `fence_agent_agent_xaction::compare()` | 比较 fence transaction 的全部字段，避免仅 `flushPipe` 不同时被误判相等。 |
| `fence_agent_agent_monitor::mon_data()` | 采样 sfence payload，在 valid=1 时检查和生成既有 raw sfence；只观测 `flushPipe`，不把它送入软件失效状态。 |

## 2. Review 范围与基线

关联执行计划：

`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_csr_control_runtime_semantic_review_execution_plan_20260708.md`

本轮审查的源码范围：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv`
- `mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_xaction.sv`
- `mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_xaction.sv`
- `mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_monitor.sv`

同步审查的 flow/analysis/TODO 文档：

- `AI_DOC/mem_ut_flow_doc/csr_runtime_sync_flow.md`
- `AI_DOC/mem_ut_flow_doc/sfence_flow.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_sync_pkg.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/mmu_csr_runtime_state.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md`
- `AI_DOC/plan/test_framework/plan/undo/mem_ut_test_framework_todo_20260614.md`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md`

工作区中 `AI_DOC/plan/test_framework/review_doc/undo` 到 `do` 的历史文档搬迁属于既有无关变更，未纳入本次实现，也未回滚。

## 3. CSR snapshot-only 字段链路

### 3.1 修改前逻辑、修改后逻辑和正确性检查

修改前，V2 interface 和 CSR transaction 已经有三个字段，但 raw CSR struct、latest snapshot 和 runtime mirror 没有保存它们；`DRV_0` 还会把两个 misalign enable 驱成 0。这样 monitor 看到的 DUT 真值无法被后续 runtime 查询，且默认驱动值与用户确认的 `1/1/0` 边界不一致。

修改后，三个字段的链路是：

```text
csr_ctrl_agent interface
  -> csr_ctrl xaction/driver 默认与 directed 原值
  -> csr_ctrl monitor X/Z 诊断和采样
  -> dispatch_raw_csr_t
  -> latest_raw_csr/latest_raw_csr_seq
  -> common_data_transaction::apply_raw_csr_runtime()
  -> mmu_csr_runtime_state 的 snapshot 字段
```

正确性检查如下：

| 检查项 | 结果 |
|---|---|
| 三个字段是否都在 interface、xaction、driver、monitor 中出现 | 通过；interface/connect 原有字段保持同名直连 |
| raw 是否保存并在变化时发布 | 通过；`make_empty_raw_csr()`、monitor 填充和 `raw_csr_payload_changed()` 均覆盖 |
| 软件 reset 清空 latest 后是否重新发布 | 通过；`raw_csr_rearm_epoch` 使 monitor 强制丢弃本地 baseline |
| runtime reset/update/copy 是否完整 | 通过；三处 update 和 `copy_from()` 均覆盖 |
| 是否进入 TLB key | 未进入；`make_lookup_key()` 只读取既有 ASID/VMID/S2 上下文字段 |
| 是否进入主表、异常、pass/fail、terminal | 未进入；本轮没有新增 consumer |
| branch predictor enable 是否混入 raw/runtime | 未混入；只保留在 CSR interface/transaction 的旁路字段 |

### 3.2 raw 类型、默认值和变化检测

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:111-140,238-313`。

抽象功能描述：`dispatch_raw_csr_t` 保存 monitor 发布的一份完整 CSR snapshot；`make_empty_raw_csr()` 为未赋值字段提供确定默认；`raw_csr_payload_changed()` 决定是否覆盖 latest snapshot。它们不负责解释权限或改变状态表。

关键源码片段：

```systemverilog
bit hd_misalign_ld_enable;
bit hd_misalign_st_enable;
bit priv_debug;

item.hd_misalign_ld_enable = 1'b1;
item.hd_misalign_st_enable = 1'b1;
item.priv_debug = 1'b0;

prev.hd_misalign_ld_enable != cur.hd_misalign_ld_enable ||
prev.hd_misalign_st_enable != cur.hd_misalign_st_enable ||
prev.priv_debug != cur.priv_debug
```

中文伪代码：

```text
定义 raw CSR 的三个 snapshot-only 字段；
创建空 raw 时把 misalign load/store 设为 1，把 priv_debug 设为 0；
比较上一份和当前份时，如果三个字段任一变化，返回“需要发布”；
该返回值只决定 latest snapshot 是否更新，不直接决定 TLB 命中或异常结果。
```

关键副作用：CSR 使用 latest snapshot 模型，不会因为每拍采样而累积 CSR FIFO；变化时由 `push_raw_csr()` 覆盖旧值并递增 `latest_raw_csr_seq`。这与 sfence 的离散 FIFO 语义不同。

### 3.3 CSR transaction 的默认值、展示和比较

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_xaction.sv:102-111,643-652,673-769,771-1230`。

抽象功能描述：CSR transaction 是 driver 的输入对象。新增 soft constraint 提供普通随机 transaction 的默认 `1/1/0`；`psdisplay()` 提供调试可见性；custom `compare()` 保证三个字段参与 transaction 比较。

关键源码片段：

```systemverilog
constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable_cons{
    soft io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable == 1'b1;
}
constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_csrCtrl_hd_misalign_st_enable_cons{
    soft io_ooo_to_mem_csrCtrl_hd_misalign_st_enable == 1'b1;
}
constraint csr_ctrl_agent_agent_xaction::default_io_ooo_to_mem_tlbCsr_priv_debug_cons{
    soft io_ooo_to_mem_tlbCsr_priv_debug == 1'b0;
}

function string csr_ctrl_agent_agent_xaction::psdisplay(string prefix = "");
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_hd_misalign_ld_enable = 0x%0h ",
                        pkt_str, this.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_csrCtrl_hd_misalign_st_enable = 0x%0h ",
                        pkt_str, this.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable);
    pkt_str = $sformatf("%sio_ooo_to_mem_tlbCsr_priv_debug = 0x%0h ",
                        pkt_str, this.io_ooo_to_mem_tlbCsr_priv_debug);
    return pkt_str;
endfunction:psdisplay

if(this.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable !=
   rhs_.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable) begin
    super_result = 0;
end
if(this.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable !=
   rhs_.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable) begin
    super_result = 0;
end
if(this.io_ooo_to_mem_tlbCsr_priv_debug != rhs_.io_ooo_to_mem_tlbCsr_priv_debug) begin
    super_result = 0;
end
```

中文伪代码：

```text
普通 randomize 时优先得到 misalign=1/1、priv_debug=0；
directed item 可以用更强的 inline constraint 覆盖 soft 默认；
psdisplay 按字段把三个值追加到 debug 字符串；
custom compare 逐个比较三个字段，任一不同就把结果置为不相等。
```

这是 transaction 保真补充，不是新的行为模型：它不会读取 common data，也不会改变主表或状态表。

### 3.4 CSR driver 的字段驱动

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv:84-180,182-568`。

抽象功能描述：`send_pkt()` 负责将 CSR transaction 原值写入 VIF；`drive_idle()` 为不同测试驱动模式提供确定的 interface 值。它不负责消费 runtime snapshot。

关键源码片段：

```systemverilog
vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <=
    tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <=
    tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <=
    tr.io_ooo_to_mem_tlbCsr_priv_debug;

if (drv_mode == tcnt_dec_base::DRV_0) begin
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= 1'b1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= 1'b1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= 1'b0;
end
else if (drv_mode == tcnt_dec_base::DRV_1) begin
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= '1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= '1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= '1;
end
else if (drv_mode == tcnt_dec_base::DRV_X) begin
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= 'x;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= 'x;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= 'x;
end
else if (drv_mode == tcnt_dec_base::DRV_RAND) begin
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= $urandom;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= $urandom;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= $urandom;
end
else if (drv_mode == tcnt_dec_base::DRV_LST) begin
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= '0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= '0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= '0;
end
```

中文伪代码：

```text
send_pkt 读取当前 transaction 的三个字段并原值驱动 interface；
没有 item 时，drive_idle 按模式显式赋值：DRV_0 为 1/1/0，DRV_1 为 1/1/1，DRV_X 为 X/X/X，DRV_RAND 为三个独立随机值，DRV_LST 为 0/0/0；
每个模式都覆盖三个字段，避免复用上一拍值；
driver 不根据这些字段暂停 sequence，也不向 pass/fail 或 terminal 写状态。
```

`DRV_LST` 的全零是该压力模式的既有语义，不改变普通 `DRV_0` 的默认边界；后续 directed testcase 若需要具体值，应通过 transaction 显式设置。

### 3.5 CSR monitor 采样与 raw 发布

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv:34-369`。

抽象功能描述：`mon_data()` 每拍采样 CSR interface；X/Z 诊断由 `xz_sw`、reset 和 backend-ready 条件控制，独立于 capture；只有 raw 发布还需要 capture 开启。它在首次采样、re-arm 或 payload 变化时更新 latest snapshot，不直接更新 status。

关键源码片段：

```systemverilog
io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable =
    this.vif.mon_mp.mon_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
io_ooo_to_mem_csrCtrl_hd_misalign_st_enable =
    this.vif.mon_mp.mon_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
io_ooo_to_mem_tlbCsr_priv_debug =
    this.vif.mon_mp.mon_cb.io_ooo_to_mem_tlbCsr_priv_debug;

raw_csr.hd_misalign_ld_enable = io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
raw_csr.hd_misalign_st_enable = io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
raw_csr.priv_debug = io_ooo_to_mem_tlbCsr_priv_debug;
```

中文伪代码：

```text
每个 monitor clock 从 mon_cb 读取三个 interface 值；
当 xz_sw 打开且 reset/backend ready 时，对三个值执行 X/Z 诊断；诊断不阻断后续 raw 发布；
无论 capture 是否打开，monitor 都继续采样 interface；
从 make_empty_raw_csr 开始构造 raw，并填入三个采样值；
首次采样、re-arm 或 raw_csr_payload_changed 返回真时调用 push_raw_csr；
保存 last_raw_csr，供下一拍比较；
如果 reset 或 capture 关闭，清除本地“已有上一份 snapshot”标志，下一次重新发布完整 snapshot。
```

monitor 的 `mon_item_port.write()` 仍未启用；这是 monitor output 专项的责任边界，本子计划只处理 raw/latest snapshot。

### 3.6 runtime mirror 与 `update_seq`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv:46-188,246-273`。

抽象功能描述：`mmu_csr_runtime_state` 是公共 runtime mirror。`reset()` 建立确定默认；两个 update 函数从 transaction/raw 更新所有字段；`copy_from()` 为 TLB entry 或 uid snapshot 复制完整状态。它不负责生成新的异常或 commit 事件。

关键源码片段：

```systemverilog
changed =
    satp_mode != raw.satp_mode ||
    satp_asid != raw.satp_asid ||
    satp_ppn != raw.satp_ppn ||
    vsatp_mode != raw.vsatp_mode ||
    vsatp_asid != raw.vsatp_asid ||
    vsatp_ppn != raw.vsatp_ppn ||
    hgatp_mode != raw.hgatp_mode ||
    hgatp_vmid != raw.hgatp_vmid ||
    hgatp_ppn != raw.hgatp_ppn ||
    priv_virt != raw.priv_virt ||
    priv_spvp != raw.priv_spvp ||
    priv_imode != raw.priv_imode ||
    priv_dmode != raw.priv_dmode ||
    priv_mxr != raw.priv_mxr ||
    priv_sum != raw.priv_sum ||
    priv_vmxr != raw.priv_vmxr ||
    priv_vsum != raw.priv_vsum ||
    m_pbmt_en != raw.m_pbmt_en ||
    h_pbmt_en != raw.h_pbmt_en ||
    raw.satp_changed || raw.vsatp_changed ||
    raw.hgatp_changed || raw.priv_virt_changed;

hd_misalign_ld_enable = raw.hd_misalign_ld_enable;
hd_misalign_st_enable = raw.hd_misalign_st_enable;
priv_debug = raw.priv_debug;
if (changed) begin
    update_seq++;
end
```

中文伪代码：

```text
先比较既有翻译/权限字段和 changed pulse，计算 semantic changed；
无论 semantic changed 是否成立，都把 raw 中三个 snapshot-only 字段复制到 runtime mirror；
只有 semantic changed 为真时才递增 update_seq；
copy_from 再把三个字段和 update_seq 一并复制给下游快照；
make_lookup_key 仍只使用 vpn、asid、vmid、s2xlate，因而 snapshot-only 字段变化不会改变查表 key。
```

这里的关键设计是把“snapshot 有新值”和“翻译语义版本变化”分开：`raw_csr_payload_changed()` 会使 latest snapshot 更新，但单独改变三个 snapshot-only 字段不会伪造一次 TLB 语义切换。

#### 3.6.1 `mmu_csr_runtime_state::reset()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv:46-70`。

抽象功能描述：`reset()` 为公共 runtime mirror 建立确定的初始 CSR 上下文。它由对象构造和公共 runtime 初始化路径调用，不消费 raw event，也不修改主表或 TLB cache。

关键源码片段：

```systemverilog
function void reset();
    satp_mode  = '0;
    satp_asid  = '0;
    satp_ppn   = '0;
    vsatp_mode = '0;
    hgatp_mode = '0;
    priv_virt  = 1'b0;
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
把 satp/vsatp/hgatp 的地址空间字段和 privilege/PBMT 字段清到确定默认值；
把普通运行时权限模式设为源码定义的初始值；
把 misalign load/store 设为 1/1，把 priv_debug 设为 0；
把 semantic update_seq 清零；
返回已初始化的 runtime mirror，不发布 raw、TLB 或 status 事件。
```

调用者和副作用：`new()` 直接调用该函数；`apply_raw_csr_runtime()` 在首次创建 mirror 时也先调用它。该函数只改变 `mmu_csr_state` 自身字段。

#### 3.6.2 `mmu_csr_runtime_state::update_from_csr_ctrl()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv:72-129`。

抽象功能描述：该函数保留从 CSR transaction 直接更新 runtime mirror 的兼容入口。当前实际 monitor 主链路使用 `update_from_raw_csr()`；本函数没有独立 active caller，但必须与 raw 入口保持同样的字段分类和默认语义。

关键源码片段：

```systemverilog
function void update_from_csr_ctrl(input csr_ctrl_agent_agent_xaction csr_tr);
    bit changed;
    if (csr_tr == null) begin
        `uvm_fatal("MMU_CSR", "update_from_csr_ctrl got null transaction")
    end
    changed =
        satp_mode != csr_tr.io_ooo_to_mem_tlbCsr_satp_mode ||
        vsatp_mode != csr_tr.io_ooo_to_mem_tlbCsr_vsatp_mode ||
        hgatp_mode != csr_tr.io_ooo_to_mem_tlbCsr_hgatp_mode ||
        priv_virt != csr_tr.io_ooo_to_mem_tlbCsr_priv_virt ||
        m_pbmt_en != csr_tr.io_ooo_to_mem_tlbCsr_mPBMTE ||
        csr_tr.io_ooo_to_mem_tlbCsr_satp_changed ||
        csr_tr.io_ooo_to_mem_tlbCsr_vsatp_changed ||
        csr_tr.io_ooo_to_mem_tlbCsr_hgatp_changed ||
        csr_tr.io_ooo_to_mem_tlbCsr_priv_virt_changed;
    // 源码随后复制完整 CSR 和 snapshot-only 字段
    hd_misalign_ld_enable = csr_tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
    hd_misalign_st_enable = csr_tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
    priv_debug = csr_tr.io_ooo_to_mem_tlbCsr_priv_debug;
    if (changed) update_seq++;
endfunction:update_from_csr_ctrl
```

中文伪代码：

```text
如果传入 transaction 为空，报告 uvm_fatal，避免用空对象更新 runtime；
比较 transaction 中的翻译/权限字段和 changed pulse，计算 semantic changed；
按源码顺序复制 satp/vsatp/hgatp、priv、PBMT 及三个 snapshot-only 字段；
如果 semantic changed 为真，递增 update_seq；
即使只有 misalign/priv_debug 变化，也保存新值但不递增 update_seq；
该兼容入口不生成 raw、TLB invalidation、pass/fail 或 terminal 事件。
```

调用关系与副作用：当前 `rg` 审计未发现 active caller，raw monitor 通过 `apply_raw_csr_runtime()` 走 raw 入口；保留此函数是为了直接 transaction 路径与 raw 路径的字段语义一致。

#### 3.6.3 `mmu_csr_runtime_state::copy_from()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv:246-273`。

抽象功能描述：`copy_from()` 将公共 runtime mirror 的完整快照复制到 uid/TLB entry 使用的局部 snapshot。它由 `common_data_transaction` 创建 uid TLB record 时调用，不重新计算语义、不产生新的 `update_seq`。

关键源码片段：

```systemverilog
function void copy_from(input mmu_csr_runtime_state rhs);
    if (rhs == null) begin
        `uvm_fatal("MMU_CSR", "copy_from got null rhs")
    end
    satp_mode  = rhs.satp_mode;
    vsatp_mode = rhs.vsatp_mode;
    hgatp_vmid = rhs.hgatp_vmid;
    priv_virt  = rhs.priv_virt;
    m_pbmt_en  = rhs.m_pbmt_en;
    h_pbmt_en  = rhs.h_pbmt_en;
    hd_misalign_ld_enable = rhs.hd_misalign_ld_enable;
    hd_misalign_st_enable = rhs.hd_misalign_st_enable;
    priv_debug = rhs.priv_debug;
    update_seq = rhs.update_seq;
endfunction:copy_from
```

中文伪代码：

```text
如果源 snapshot 为空，报告 uvm_fatal；
否则按源码顺序复制完整翻译、权限、PBMT 和 snapshot-only 字段；
同时复制源对象已有的 update_seq，不在 copy 过程中重新比较或递增；
目标 snapshot 只供对应 uid/TLB entry 保存上下文，不反向修改公共 runtime 或主表。
```

调用关系与副作用：`common_data_transaction::get_mmu_csr_snapshot()` 先创建目标 snapshot 并调用该函数；`memblock_uid_tlb_record::init_context()` 再把 snapshot 复制到 uid TLB record。复制结果用于发射时的历史上下文/debug，sfence 不因该复制动作删除它。

### 3.7 latest CSR reset re-arm 生命周期

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:14-22,395-405`；`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv:130-145,327-340`。

抽象功能描述：`clear_raw_monitor_queues()` 清除全局 latest CSR 时发布新的 re-arm epoch；CSR monitor 发现 epoch 改变后清除自身的 payload 去重基线。该机制只恢复 reset 后首份 snapshot，不改变正常 payload change 规则。

关键源码片段：

```systemverilog
int unsigned raw_csr_rearm_epoch = 0;

function void clear_raw_monitor_queues();
    raw_int_wb_q.delete();
    raw_iq_feedback_q.delete();
    raw_ctrl_q.delete();
    raw_sfence_q.delete();
    latest_raw_csr = make_empty_raw_csr();
    latest_raw_csr_valid = 1'b0;
    latest_raw_csr_seq = 0;
    raw_csr_rearm_epoch++;
    dispatch_service_cycle = 0;
endfunction:clear_raw_monitor_queues
```

中文伪代码：

```text
软件 reset 或收尾调用 clear_raw_monitor_queues；
函数清空所有 raw FIFO 和 latest CSR 有效状态；
把 raw_csr_rearm_epoch 加一，通知 CSR monitor 旧的本地去重 baseline 已失效；
该函数不直接伪造一份 CSR snapshot，也不修改 TLB key。
```

关键源码片段：

```systemverilog
if (memblock_sync_pkg::raw_csr_rearm_epoch != last_raw_csr_rearm_epoch) begin
    has_last_raw_csr = 1'b0;
    last_raw_csr = memblock_sync_pkg::make_empty_raw_csr();
    last_raw_csr_rearm_epoch = memblock_sync_pkg::raw_csr_rearm_epoch;
end
```

中文伪代码：

```text
每个 monitor 边界比较全局 re-arm epoch 和本地记录；
如果不同，清除 has_last_raw_csr 并把 last_raw_csr 恢复为空；
记录新 epoch；
本拍后续正常构造当前 raw CSR，因为 has_last_raw_csr=0，会无条件调用 push_raw_csr；
因此 reset 前后 payload 完全相同也不会丢失 runtime 首份 snapshot。
```

## 4. `sfence_bits_flushPipe` 接口保真适配

### 4.1 修改前逻辑、修改后逻辑和正确性检查

修改前，fence transaction 已有 `flushPipe` 字段但没有确定 soft 默认、debug 展示和 custom compare；driver 的部分 idle mode 也没有显式给该位赋值，monitor 没有在有效 payload 中检查该位。

修改后：

1. transaction 默认 `flushPipe=0`，directed item 可以覆盖为 1；
2. driver `send_pkt()` 原值透传，所有 idle mode 显式赋值；
3. monitor 每拍采样该位，只有 `sfence_valid===1'b1` 时诊断全部 payload；
4. `dispatch_raw_sfence_t`、`decode_raw_sfence()`、`apply_sfence_invalidate()` 不增加该字段。

正确性检查：

| 场景 | 预期 | 结果 |
|---|---|---|
| 普通随机 fence item | `flushPipe=0` | soft constraint 已提供 |
| directed item 覆盖为 1 | DUT interface 得到 1 | `send_pkt()` 原值驱动 |
| valid=0 且 payload 为 X | 不产生 raw sfence；不把无效 payload 当事件 | monitor 使用 valid gate |
| valid=1 且 flushPipe 为 X/Z | X/Z 诊断报 `uvm_error`，但 flushPipe 不进入 raw | monitor 保持公共 error-only 策略 |
| 只切换 flushPipe，其他 sfence 字段不变 | 软件 TLB invalidation 结果不变 | raw 类型和失效函数未改变 |

### 4.2 transaction 默认、展示和 compare

源码位置：`mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_xaction.sv:22-50,82-182`。

抽象功能描述：fence xaction 负责保存一拍 fence request 的字段并提供默认随机约束、日志和比较；它不实现 pipeline flush。

关键源码片段：

```systemverilog
constraint fence_agent_agent_xaction::default_io_ooo_to_mem_sfence_bits_flushPipe_cons{
    soft io_ooo_to_mem_sfence_bits_flushPipe == 1'b0;
}

function string fence_agent_agent_xaction::psdisplay(string prefix = "");
    pkt_str = $sformatf("%sio_ooo_to_mem_sfence_bits_flushPipe = 0x%0h ",
                        pkt_str, this.io_ooo_to_mem_sfence_bits_flushPipe);
    return pkt_str;
endfunction:psdisplay

if(this.io_ooo_to_mem_sfence_bits_flushPipe !=
   rhs_.io_ooo_to_mem_sfence_bits_flushPipe) begin
    super_result = 0;
end
```

中文伪代码：

```text
普通 randomize 时 flushPipe 默认取 0；
directed sequence 需要 1 时使用更强的 inline constraint 或 randomize 后显式赋值；
psdisplay 打印实际 flushPipe 值；
custom compare 把 flushPipe 作为完整 transaction 的比较字段；
这些操作不读取公共状态，也不触发 flush。
```

### 4.3 driver 原值驱动和 idle 清理

源码位置：`mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_driver.sv:84-155`。

抽象功能描述：`send_pkt()` 只负责将 transaction 的 `flushPipe` 写到 DUT interface；`drive_idle()` 在没有 item 时清理该位，避免上一笔 transaction 残留。

关键源码片段：

```systemverilog
vif.drv_mp.drv_cb.io_ooo_to_mem_sfence_bits_flushPipe <=
    tr.io_ooo_to_mem_sfence_bits_flushPipe;
```

中文伪代码：

```text
收到 transaction 时，把 flushPipe 原值写入 driver clocking block；
DRV_0/DRV_LST 驱 0，DRV_1 驱 1，DRV_X 驱 X，DRV_RAND 驱随机值；
driver 不查询 quiescent、queue、redirect 或 L2TLB 状态，也不创建 flush epoch。
```

### 4.4 monitor valid gate 和 raw 行为边界

源码位置：`mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_monitor.sv:34-100`。

抽象功能描述：fence monitor 负责采样和过滤有效 sfence 事件。`flushPipe` 只参与有效 payload 的 X/Z 诊断，不进入 raw sfence；既有 `sfence_valid` 才是软件 TLB 失效事件的入口。X/Z 宏只报错，不是 payload drop gate。

关键源码片段：

```systemverilog
`TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_valid,io_ooo_to_mem_sfence_valid,1);
if (io_ooo_to_mem_sfence_valid===1'b1) begin
    `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_flushPipe,io_ooo_to_mem_sfence_bits_flushPipe,1);
end

if (io_ooo_to_mem_sfence_valid===1'b1) begin
    raw_sfence = memblock_sync_pkg::make_empty_raw_sfence();
    raw_sfence.valid = 1'b1;
    raw_sfence.rs1 = io_ooo_to_mem_sfence_bits_rs1;
    raw_sfence.hg = io_ooo_to_mem_sfence_bits_hg;
    memblock_sync_pkg::push_raw_sfence(raw_sfence);
end
```

中文伪代码：

```text
每拍先诊断 valid；
只有 valid 明确为 1 时才诊断 rs1/rs2/addr/id/hv/hg/flushPipe；
valid 为 0 或未知时不构造 raw sfence；
valid 为 1 时即使其它 payload 的 X/Z 诊断已报错，现有逻辑仍复制二态 raw 并入 raw_sfence_q；
flushPipe 故意不复制到 raw；
后续 adapter 仍按原 sfence 字段执行 TLB entry invalidation。
```

这保证了接口字段保真与行为建模边界分离：本轮没有把 `flushPipe=1` 误当成 standalone 的 LSQ pause、年轻 uid kill、redirect 或 terminal flush。

## 5. 与原测试框架主体逻辑的对比和修改类型总结

| 修改项 | 原有逻辑 | 当前逻辑 | 修改类型 | 是否改变主体控制行为 |
|---|---|---|---|---|
| misalign/priv_debug 默认 | transaction 字段存在但默认和 idle 驱动不明确，raw/runtime 不保存 | 默认 `1/1/0`，所有 idle mode 显式驱动，raw/runtime/copy 完整保存 | V2 字段默认与 snapshot 链适配 | 否；当前无行为 consumer |
| CSR raw 变化检测 | 只比较已有 CSR 字段 | 额外比较三个 snapshot-only 字段 | 新增观测状态同步 | 否；只更新 latest snapshot |
| CSR reset/re-arm | 清全局 latest 后 monitor 本地去重基线可能保留 | reset epoch 使 monitor 强制重发首份 snapshot | reset 生命周期修复 | 否；只恢复正确 runtime 真值 |
| `update_seq` | 反映既有 CSR 语义字段变化 | 继续只反映 semantic field 变化 | 语义边界澄清 | 否 |
| CSR transaction compare/debug | UVM field macro 存在，但手工 compare/日志漏字段 | 手工 compare/日志补齐 | transaction 保真 | 否 |
| fence `flushPipe` | 可通过部分路径驱动，但默认/idle/compare/XZ 不完整 | 默认 0、原值驱动、idle 清理、valid payload 检查、日志/compare 完整 | 接口字段适配 | 否 |
| sfence raw/失效 | raw 只保存 valid/rs1/rs2/addr/id/hv/hg | 保持不变，flushPipe 不进入 raw/失效 | 明确保持原逻辑 | 否 |

未修改的主体 flow：主表生成、LSQ enqueue、issue、writeback、ROB/SQ commit/deq、redirect/replay、异常 pass/fail、terminal、L2TLB request/response 和 TLB entry invalidation 的原有控制顺序均未增加分支。

## 6. 验证结果

### 6.1 静态检查

- `git diff --check`：通过。
- 字段审计：三个 CSR 字段在 interface、xaction、driver、monitor、raw、runtime reset/update/copy 中均有引用。
- 消费审计：branch predictor enable 未出现在 `dispatch_raw_csr_t`、`mmu_csr_runtime_state` 或 `make_lookup_key()`；`flushPipe` 未出现在 `dispatch_raw_sfence_t`、`decode_raw_sfence()` 或 `apply_sfence_invalidate()`。
- 工作区范围审计：本次 review 未纳入既有 review 文档搬迁产生的删除/新增文件。

### 6.2 V2 远端编译和 smoke

执行入口：

```text
cd mem_ut/ver/ut/memblock/sim
make eda_batch_run tc=tc_sanity mode=base_fun
```

最终 re-arm 修复版本结果：

- 受影响 V2 partition 的 VCS/Verdi compile、partcomp stitch、elaboration 和 link 成功；
- `TEST CASE PASSED`；
- `UVM_ERROR : 0`；
- `UVM_FATAL : 0`；
- 退出码 `0`；
- 日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_sanity_ts=virtual_base_sequence_cfg=default_seed=666666_rtl_.log`。

该 smoke 主要证明公共环境能编译、启动并正常结束；它不专门覆盖 `flushPipe=1` directed item、重复软件 reset 或动态切换 snapshot-only 字段后的 `update_seq` 数值断言，这些是明确记录的残余覆盖风险。运行中观察到 `tc_sanity` 的空 `virtual_base_sequence` 使 LSQ sequence 打印等待主表的既有 warning，但最终测试正常结束且没有 UVM error/fatal；该 warning 不归因于本子计划。

## 7. Plan 对齐检查

### 7.1 执行前 Plan 与实现逐项对照

| Plan 要求 | 实现检查 |
|---|---|
| monitor -> raw CSR -> runtime snapshot 保存三个 CSR 字段 | 已实现并逐层核对 |
| reset 后重新发布首份 latest CSR | 已增加 re-arm epoch，并经最终独立 reviewer 复查通过 |
| 默认 misalign=1/1、priv_debug=0 | xaction soft constraint、`DRV_0`、raw factory、runtime reset 均一致 |
| branch predictor 不进入 TLB key/主表判断 | 已通过源码检索确认 |
| `flushPipe` 默认 0、directed 可覆盖、driver 透传 | 已实现 |
| 只有 valid=1 诊断 sfence payload X/Z | 已实现 error-only 诊断，且 raw event 使用 case-equality valid gate |
| `flushPipe` 不进入 raw sfence/行为状态 | 已实现 |
| 不新增 standalone pipeline flush、redirect、LSQ 或 terminal 逻辑 | 已确认无相关代码修改 |
| monitor analysis port 不由本子计划扩展 | 已保持原边界，并在 plan/flow/review 中记录 |

### 7.2 实现与 Plan 不一致项

相对执行前原始 plan，存在以下八项执行中补充或修正。它们不是“原 plan 已完整覆盖”的内容，均已回写执行 plan 的 `IMPLEMENTATION_DELTA`，并在下面逐项同时给出：原 plan 方案、当前实现、补充原因、源码/文档位置、中文伪代码和最终处理结论。

#### 7.2.1 CSR transaction 手工 compare 补齐

原 plan：只要求三个 CSR 字段进入 xaction、driver、monitor 和 runtime snapshot，未明确手工 `psdisplay()`/`compare()` 的覆盖。

当前实现：在已有 UVM field macro 之外，`psdisplay()` 输出三个字段，custom `compare()` 逐项比较三个字段。

补充原因：手工比较和调试输出不会自动继承 field macro；若不补齐，只有新增字段不同的 transaction 可能被误判相等或日志不可见。

功能：防止 custom compare 在仅三个新增字段不同时丢失差异。

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_xaction.sv:1213-1226`。

```systemverilog
if(this.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable!=rhs_.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable) begin
    super_result = 0;
end
if(this.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable!=rhs_.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable) begin
    super_result = 0;
end
if(this.io_ooo_to_mem_tlbCsr_priv_debug!=rhs_.io_ooo_to_mem_tlbCsr_priv_debug) begin
    super_result = 0;
end
```

中文伪代码：

```text
custom compare 逐个读取两个 transaction 的三个新增字段；
任一字段不同就把比较结果置为失败；
该分支只影响 transaction 相等判断和日志，不写公共 status，也不触发行为 flow。
```

处理结论：已回写 plan 的 `IMPLEMENTATION_DELTA/5.1`，无需新增行为 consumer。

#### 7.2.2 snapshot 与 semantic update_seq 分离

原 plan：要求 raw/runtime 保存三个字段，但没有明确 snapshot 序号与翻译语义 `update_seq` 的边界。

当前实现：三个字段进入 runtime mirror，但 `changed` 只由既有翻译/权限字段和 changed pulse 决定；单独 snapshot-only 变化不递增 `update_seq`。

补充原因：避免把观测字段变化误当成 TLB 语义切换，保持原有 lookup/invalidation 控制逻辑。

功能：允许 snapshot-only 字段更新，同时不把观察值变化伪装成 TLB 语义版本变化。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv:138-187`。

```systemverilog
changed =
    satp_mode != raw.satp_mode ||
    satp_asid != raw.satp_asid ||
    priv_mxr  != raw.priv_mxr  ||
    m_pbmt_en != raw.m_pbmt_en ||
    raw.satp_changed || raw.vsatp_changed ||
    raw.hgatp_changed || raw.priv_virt_changed;
hd_misalign_ld_enable = raw.hd_misalign_ld_enable;
hd_misalign_st_enable = raw.hd_misalign_st_enable;
priv_debug = raw.priv_debug;
if (changed) begin
    update_seq++;
end
```

中文伪代码：

```text
先只比较既有翻译/权限字段和 changed pulse，得到 semantic changed；
无论该结果是否为真，都把三个 snapshot-only 字段复制到 runtime mirror；
只有 semantic changed 为真才递增 update_seq，因此单独改变 misalign/priv_debug 不改变 TLB 语义版本。
```

处理结论：已回写 plan 的 `IMPLEMENTATION_DELTA/5.2`，属于状态版本边界，不改变主控制行为。

#### 7.2.3 sfence valid 的四态 gate

原 plan：要求有效 sfence payload 做 X/Z 检查，但没有明确 valid 的四态判定方式。

当前实现：raw event 条件使用 `io_ooo_to_mem_sfence_valid===1'b1`；valid 为 0 或 X/Z 时不生成 raw，X/Z 宏仍只做 error-only 诊断。

补充原因：普通 `==` 在四态值下可能产生不确定控制结果，不能让未知 valid 形成误导性的 TLB invalidation event。

功能：避免 valid 为 X/Z 时把不确定 payload 静默生成 raw sfence event。

源码位置：`mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_monitor.sv:59-83`。

```systemverilog
if (io_ooo_to_mem_sfence_valid===1'b1) begin
    `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_flushPipe,io_ooo_to_mem_sfence_bits_flushPipe,1);
end
if(this.vif.rst_n==1'b1 &&
   memblock_sync_pkg::reset_backend_done==1'b1 &&
   io_ooo_to_mem_sfence_valid===1'b1) begin
    raw_sfence = memblock_sync_pkg::make_empty_raw_sfence();
    raw_sfence.valid = 1'b1;
    memblock_sync_pkg::push_raw_sfence(raw_sfence);
end
```

中文伪代码：

```text
valid 明确为 1 时才检查关联 payload；
reset/backend ready 且 valid 明确为 1 时才创建并推送 raw sfence；
valid 为 0 或 X/Z 时不生成 raw event，合法 0/1 的行为保持不变。
```

处理结论：已回写 plan 的 `IMPLEMENTATION_DELTA/5.3`，只收紧四态输入边界。

#### 7.2.4 latest CSR reset re-arm

原 plan：要求清空 latest CSR，但未规定 monitor 本地 payload 去重 baseline 如何随软件清表失效。

当前实现：`clear_raw_monitor_queues()` 递增 `raw_csr_rearm_epoch`，monitor 看到 epoch 变化后清除 `has_last_raw_csr` 和 baseline，下一拍重新发布完整 snapshot。

补充原因：若 reset 前后 payload 相同，仅清全局 latest 会使 monitor 认为“没有变化”，runtime mirror 会停留在 reset 默认值。

功能：解决软件清表后 CSR payload 未变化导致 monitor 本地去重基线阻止首份 snapshot 重发的问题。

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:395-405`。

```systemverilog
latest_raw_csr_valid = 1'b0;
latest_raw_csr_seq = 0;
raw_csr_rearm_epoch++;
```

中文伪代码：

```text
清空全局 latest CSR 和序号；
递增 re-arm epoch，向 monitor 发布“旧 baseline 已失效”的通知；
monitor 下一拍清除 has_last_raw_csr，即使 payload 相同也重新 push 首份 snapshot。
```

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv:327-331`。

```systemverilog
if (memblock_sync_pkg::raw_csr_rearm_epoch != last_raw_csr_rearm_epoch) begin
    has_last_raw_csr = 1'b0;
    last_raw_csr = memblock_sync_pkg::make_empty_raw_csr();
    last_raw_csr_rearm_epoch = memblock_sync_pkg::raw_csr_rearm_epoch;
end
```

中文伪代码：

```text
monitor 每个采样边界比较全局和本地 epoch；
发现不同就清除本地去重标志和上一份 raw，并记录新 epoch；
本拍后续的首次采样分支无条件发布当前 snapshot，不改变 TLB key 或 status。
```

处理结论：已回写 plan 的 `IMPLEMENTATION_DELTA/5.4`，闭环 reset 生命周期。

#### 7.2.5 X/Z 宏的 error-only 语义

原 plan：要求监测有效 payload 的 X/Z，但没有明确诊断宏是否会阻断 raw 发布。

当前实现：`TCNT_CHECK_SIG_XZ` 只调用 `uvm_error`，不执行 return、drop 或 fatal；valid=1 时 monitor 继续构造二态 raw。

补充原因：保持公共 monitor 的既有错误报告策略，避免把诊断宏错误地升级为新的行为 gate。

功能：明确诊断报错与 raw 发布 gate 是两件事。

源码位置：`mem_ut/ver/common/tcnt_base/src/tcnt_macro_define_base.sv:4`。

```systemverilog
`define TCNT_CHECK_SIG_XZ(SIG,VAR,WID) if(^(VAR)===1'bx|^(VAR)===1'bz) `uvm_error(get_type_name(),$psprintf(`"MON_XZ_CHECK: SIG %0d'h%0x`",(WID),(VAR)));
```

中文伪代码：

```text
宏检测变量归约值是否为 X 或 Z；
命中时只调用 uvm_error 记录诊断，不执行 return、drop 或 fatal；
因此 monitor 可能继续把四态值折叠到二态 raw，最终由 testcase 的 UVM error 使测试失败。
```

处理结论：已回写 plan 的 `IMPLEMENTATION_DELTA/5.5`，保持公共 monitor 的既有策略。

#### 7.2.6 web callgraph 文档同步

原 plan：要求同步有效分析文档，但未逐点列出 active web callgraph 中的旧语义。

当前实现：更新 re-arm、snapshot-only 和 `update_seq` 摘要，并明确 lookup key 不依赖 `update_seq`；同步修正 L2TLB sequence 分析表和 S1/S2 TODO 中把 `update_seq` 列作 key 字段的旧表述。

补充原因：网页、L2TLB sequence 分析文档和后续 S1/S2 TODO 都会被使用，保留旧描述会与源码及 flow 文档冲突。

功能：同步 active web callgraph 对 re-arm、snapshot-only 字段和 `update_seq` 的摘要，避免网页文档继续描述“清 latest 但无 re-arm”或“任意 CSR 变化都递增 update_seq”。

文档位置：`AI_DOC/web/memblock_dispatch_control_flow_callgraph.md:441,445,737,1156,1299`；`AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md:42`；`AI_DOC/plan/test_framework/plan/undo/mem_ut_test_framework_todo_20260614.md:725`。

文字伪代码：

```text
网页函数表把 clear_raw_monitor_queues 描述为清 latest 并递增 re-arm epoch；
把 update_from_raw_csr 描述为只有 semantic field 变化才递增 update_seq；
把 mmu_csr_state 标注为含 snapshot-only 字段但当前不消费；
明确 L2TLB lookup key 只使用 vpn/asid/vmid/s2xlate，不依赖 update_seq；L2TLB 分析表和 S1/S2 TODO 都把 update_seq 改为语义追踪值，不再列为 key 字段。
```

处理结论：已回写 plan 的 `IMPLEMENTATION_DELTA/5.6`；第三轮发现的 active 文档 owner/前置条件修正另回写到 `IMPLEMENTATION_DELTA/5.8`，并将 web、L2TLB 分析文档和 S1/S2 TODO 纳入本 review 范围；不涉及源码行为。

#### 7.2.7 flow 文档结构和四态语义同步

原 plan：要求同步 CSR/sfence flow，但未明确按 flow 文档规则为每个源码片段补齐术语、抽象功能和紧邻伪代码。

当前实现：两份 flow 补齐术语表、抽象功能、源码后的中文伪代码，并准确区分采样、X/Z 诊断、capture 发布和 `valid===1'b1` gate。

补充原因：避免 flow 文档把 capture 当作采样前置条件，或把四态 valid 误写成普通二态比较；该补充只改变文档表达，不新增运行期 consumer。

功能：让当前有效的 CSR/sfence flow 文档与源码及项目文档规则一致。

文档位置：`AI_DOC/mem_ut_flow_doc/csr_runtime_sync_flow.md`、`AI_DOC/mem_ut_flow_doc/sfence_flow.md`。

文字伪代码：

```text
两个 flow 的首章先定义 latest snapshot、raw event、snapshot-only、re-arm epoch、valid gate 等术语；
每个源码实现章节先写抽象功能描述，再给源码片段和紧邻中文伪代码；
sfence flow 使用 valid===1'b1 的真实四态 gate，并明确 monitor 每拍采样、XZ 诊断和 capture 发布的不同条件；
这些修改只同步文档，不新增源码路径或行为 consumer。
```

处理结论：已回写 plan 的 `IMPLEMENTATION_DELTA/5.7`，flow 文档与源码语义一致。

#### 7.2.8 active L2TLB 文档调用链和 key owner 纠正

原 plan：要求 active web/analysis 文档与 CSR runtime、L2TLB lookup key 语义同步，但没有逐项审计 web 中的历史索引名称和 `main_table_ready` 前置条件。

当前实现：

- web 只保留源码真实存在的 `tlb_entry_by_key` 和 `uid_tlb_record_by_uid`，删除不存在的 `uid_by_tlb_key` 描述；
- web 将 `main_table_ready` 限定为 LSQ/issue/commit 等依赖主表的 sequence 条件，明确 L2TLB responder 不等待该标志；
- CSR runtime analysis 改为“CSR 上下文参与 live TLB entry 查/建”，明确 uid record 由公共数据层按 `{vpn,asid,vmid,s2xlate}` 扫描回填，而不是由 CSR runtime 直接查 uid。

补充原因：源码 `common_data_transaction` 没有 key->uid 强绑定表，L2TLB sequence 的 `body()` 也没有等待 `main_table_ready`；继续保留旧描述会误导后续 responder/lifecycle owner 实现。

源码和文档证据：

```text
common_data_transaction：
  源码：mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:26-27,1585-1601,1798-1828。
  tlb_entry_by_key[key] 是 live TLB entry 主存储；
  uid_tlb_record_by_uid[uid] 是 uid 历史/待回填记录；
  update_uid_tlb_records_by_entry() 遍历 uid record，按 vpn/s2xlate/asid/vmid 匹配并回填。

memblock_l2tlb_base_sequence::body()：
  源码：mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv:58-74。
  init/configure/enable/context 检查后直接进入 drive_l2tlb_loop；
  没有 main_table_ready 等待分支。

active 文档：
  web：AI_DOC/web/memblock_dispatch_control_flow_callgraph.md:1151-1164；
  CSR analysis：AI_DOC/analysis/source_sv/dispatch_framework_sv/mmu_csr_runtime_state.md:9-11；
  L2TLB analysis：AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md:64-71。
```

中文伪代码：

```text
L2TLB request 到来时，用 request 的 vpn/s2xlate 和最新 runtime CSR 生成 lookup key；
在 tlb_entry_by_key 中命中或创建 live entry；
response 确定后，遍历 uid_tlb_record_by_uid，按完整 key 字段回填所有匹配 pending record；
不要建立或引用 uid_by_tlb_key；
不要让 L2TLB responder 因 main_table_ready 未置位而停住，主表依赖仅属于对应 LSQ/issue/commit sequence。
```

处理结论：已回写 plan 的 `IMPLEMENTATION_DELTA/5.8`；只修正文档 owner 和调用链，不修改源码行为。

## 8. 非本次修改的逻辑分析

本 review 按当前 `git status --short` 将工作区其它修改分为以下类别；它们没有被本子计划 stage、修改或提交：

| 类别 | 当前路径/范围 | 处理结论 |
|---|---|---|
| 项目规则与入口 | `AGENTS.md`、`AI_DOC/project_management/mem_ut_code_review_document_rule.md`、`mem_ut_flow_document_rule.md`、`mem_ut_test_framework_logic_build_rule.md`、`mem_ut_test_framework_plan_review_rule.md` | 属于项目规则维护，影响文档格式和执行约束；本 review 只按其规则检查，不分析其逻辑正确性。 |
| 历史 review 文档搬迁 | `AI_DOC/plan/test_framework/review_doc/undo/*.md` 的删除及对应 `review_doc/do/*.md` 的新增 | 属于既有归档整理，不是 CSR runtime coding；不回滚、不纳入本 commit。 |
| 其它功能 plan/review | `AI_DOC/plan/test_framework/plan/do` 和其它 `review_doc` 中已有文档 | 属于其它 V2 flow 的历史资料，本 review 不改变其内容。 |
| 仿真/生成产物 | 当前 status 中未发现需要纳入本 review 的 tracked 仿真产物 | smoke 日志仅作为验证证据，不纳入源码 commit。 |

上述范围没有发现会改变本 CSR 子计划行为的其它 `mem_ut/ver/ut/memblock` 源码修改。

## 9. Subagent Review 状态

本轮经历多轮独立只读 review，所有 finding 均已逐项修复并重新复查：

1. `Mencius` 首轮发现 CSR re-arm、X/Z error-only 语义和 directed coverage 记录缺口；已增加 `raw_csr_rearm_epoch`，修正文档并保留覆盖边界。
2. `Sagan`、`Raman` 复查发现 flow 术语/抽象功能、四态 gate、web lookup key 语义和 review delta 结构缺口；已补齐 8 项 `IMPLEMENTATION_DELTA` 对照及每个源码片段后的中文伪代码。
3. `Maxwell`、`Kant` 复查发现 runtime helper、idle mode、psdisplay 证据和 active 文档旧语义；已补充 reset/update/copy、五种 driver mode、两个 psdisplay 以及对应路径行号。
4. `Archimedes` 后续复查发现 `uid_by_tlb_key`、`main_table_ready`、CSR 直接查 uid、caller 类归属、web 参数名和过期行号；已按真实源码修正 active web/analysis/review。
5. 最终独立 reviewer `Archimedes`（agent `019f8961-378e-7320-87f9-2c2218acbb71`）结论：未发现问题，明确输出 `FINAL PASS`。

本 agent 已独立执行源码检索、文档结构检查、`git diff --check` 和验证结果核对；满足归档和提交条件。

## 10. 残余风险与最终结论

### 10.1 残余风险

- `hd_misalign_ld/st_enable` 和 `priv_debug` 目前只保存，不驱动 directed 异常、权限或 debug-mode 行为；这些功能仍需独立专项 plan。
- `sfence_bits_flushPipe` 目前只完成接口字段保真，不模拟完整 core ROB 提交点 pipeline flush；年轻 uid kill、queue 回滚和 terminal 重收敛仍是 TODO。
- smoke 不专门断言 directed `flushPipe=1`、重复软件 reset 和 snapshot-only 字段动态变化；这些场景当前依赖静态源码审计，未声称已被 testcase 命中。

### 10.2 结论

re-arm 修复、最终 compile/smoke、文档同步和独立 review 均已完成；最终 reviewer 明确 `FINAL PASS`，本 agent 复核未发现 blocker。本子计划可以归档到 `plan/do` 并提交。
