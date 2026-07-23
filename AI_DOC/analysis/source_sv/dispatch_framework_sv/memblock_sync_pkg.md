# memblock_sync_pkg.sv 源码分析

本文档对应源码：

- mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv

memblock_sync_pkg 是 UVM monitor、dispatch service 和 L2TLB responder 之间的共享同步包。它保存 raw queue、runtime latest snapshot、非破坏性 flush sideband、dispatch service cycle 和 lifecycle owner 状态；它不直接驱动 DUT，也不直接修改主表 pass/fail/terminal。

## 1. 术语与抽象职责

| 术语 | 当前 package 中的含义 | 状态落点 |
|---|---|---|
| raw queue | monitor采集的离散事件 FIFO，必须由对应 semantic consumer pop | raw_int_wb_q、raw_ctrl_q、raw_sfence_q等 |
| runtime latest | CSR monitor发布的最新状态快照，可由多个consumer重复读取 | runtime_csr_snapshot |
| semantic raw latest | dispatch semantic flow使用的gated CSR latest视图 | latest_raw_csr |
| flush event | L2TLB responder使用的非破坏性失效通知 | l2tlb_flush_event_seq/sample_time/valid |
| owner | 当前唯一拥有L2TLB responder lifecycle的sequence | l2tlb_lifecycle_owner_claimed/name |
| service cycle | dispatch 软件服务循环的单调计数 | dispatch_service_cycle |

抽象职责分为两条边界：

1. monitor把真实接口值转换成 raw event 或 runtime snapshot；
2. consumer从对应视图读取并负责后续状态更新。L2TLB responder只读 runtime latest 和 flush latest，不抢占 dispatch raw queue 的消费权。

## 2. 全局同步状态

| 字段 | 含义 | 主要使用者 |
|---|---|---|
| reset_backend_done | backend reset/初始化完成 | driver、monitor、sequence |
| dispatch_flush_in_progress | dispatch redirect/flush正在处理 | issue/LSQ dispatch flow |
| dispatch_monitor_capture_en | semantic raw queue采集开关 | 各monitor的push_raw_* |
| l2tlb_responder_active | connect takeover是否把L2TLB response交给agent | L2TLB sequence和driver |
| dispatch_real_smoke_active | 当前是否运行dispatch real smoke | smoke相关driver |
| dcache_responder_done | DCache 已完成 terminal idle 并自然返回 | legacy real-smoke testcase phase objection |
| dispatch_flushsb_waiting_empty | flushSb已发出且等待sbIsEmpty | ctrl monitor和flushSb flow |
| dispatch_flush_epoch | dispatch flush版本 | LSQ admission和redirect |
| dispatch_service_cycle | 软件service周期 | debug、timeout、TLB record时间戳 |
| raw_csr_rearm_epoch | semantic CSR clear后的重新发布代号 | CSR monitor |
| l2tlb_lifecycle_owner_claimed/name | 唯一L2TLB responder owner | sequence claim/release |

l2tlb_lifecycle_owner_* 不由 DUT reset 清除。只有 owner sequence 发送最终 inactive cycle item并自然退出后，调用者名称匹配的 release 才能清除它。

`dcache_responder_done` 不是 DUT 状态，也不参与 pass/fail/terminal 判断。DCache responder 每次 body
启动时清零，global stop 后把全部 in-flight 排空并发送最后 safe idle 后置一；canonical vseq 仍以
`wait fork` 为主，只有 legacy `tc_dispatch_real_smoke` 用该标志防止 phase 提前结束。

## 3. Raw 类型和队列

### 3.1 raw event 类型

| 类型 | 生产者 | 主要字段 | 消费者 |
|---|---|---|---|
| dispatch_raw_int_wb_t | int-WB monitor | V2 source_kind、lane内port、ROB/LQ/SQ key、metadata、exception和采样flush epoch | dispatch_monitor_event_adapter |
| dispatch_raw_iq_feedback_t | IQ feedback monitor | target、key、hit、flush state和exception | dispatch monitor adapter |
| dispatch_raw_ctrl_t | ctrl monitor | LQ/SQ deq、memory violation、sbIsEmpty等 | LSQ/redirect/flushSb handler |
| dispatch_raw_sfence_t | fence monitor | valid、rs1/rs2、addr/id、hv/hg和service cycle | semantic sfence adapter |
| dispatch_raw_csr_t | CSR monitor | satp/vsatp/hgatp、权限、PBMT和snapshot-only字段 | runtime snapshot及CSR semantic flow |

dispatch_raw_csr_t 中的 hd_misalign_ld_enable、hd_misalign_st_enable 和 priv_debug 被 monitor 采样并保存；它们属于 runtime snapshot字段，本 package不把它们混入 L2TLB lookup key，也不直接改变主表 pass/fault判断。

### 3.2 raw queue API

抽象功能描述：push/pop API让 monitor采集和semantic状态更新解耦；除CSR latest以外，离散事件都用FIFO保存，避免事件被后一个事件覆盖。

核心行为：

~~~text
make_empty_raw_*：
  返回安全空结构，避免monitor复用旧字段；

push_raw_int_wb/iq_feedback/ctrl/sfence：
  只有capture gate开启且事件有效时入队；

pop_raw_*：
  FIFO为空返回空结构和0；
  非空从队头取出一条事件；

clear_raw_monitor_queues：
  清所有semantic raw FIFO和latest_raw_csr；
  清latest_raw_csr_seq并把dispatch_service_cycle清零；
  递增raw_csr_rearm_epoch；
  不清runtime_csr_snapshot、runtime_csr_snapshot_seq、l2tlb_flush_event_seq或owner。
~~~

package API不直接更新 common_data_transaction，由 dispatch_monitor_event_adapter、LSQ handler、redirect owner 或其他明确 consumer 负责。

## 4. Runtime CSR latest

### 4.1 publish_runtime_csr_snapshot()

源码位置：

- mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv
- mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv

抽象功能描述：CSR monitor维护自己的逐拍 baseline，并把首份或发生变化的完整 CSR payload发布为公共 latest。该视图独立于 dispatch_monitor_capture_en，用于保证 legacy L2TLB default sequence也能取得真实CSR。

真实逻辑摘要：

~~~systemverilog
runtime_payload_changed =
    !has_last_runtime_csr ||
    memblock_sync_pkg::raw_csr_payload_changed(last_runtime_csr, raw_csr);
memblock_sync_pkg::publish_runtime_csr_snapshot(raw_csr,
                                                 runtime_payload_changed);

function void publish_runtime_csr_snapshot(input dispatch_raw_csr_t item,
                                           input bit payload_changed);
    if (item.valid && payload_changed) begin
        runtime_csr_snapshot = item;
        runtime_csr_snapshot_valid = 1'b1;
        runtime_csr_snapshot_seq++;
    end
endfunction
~~~

文字伪代码：

~~~text
post-reset monitor每拍构造raw_csr；
首份sample或payload变化时调用publisher；
publisher覆盖runtime_csr_snapshot并递增统一seq；
payload未变化时保留原latest和seq；
publisher不pop raw queue，不修改TLB table、ready或主表状态。
~~~

raw_csr_payload_changed() 比较查表和异常建模需要的 MMU CSR、权限、PBMT以及 hd_misalign_ld/st_enable、priv_debug。branch predictor enable 等字段不进入该比较，也不进入 L2TLB lookup key。

### 4.2 get_latest_runtime_csr_snapshot()

抽象功能描述：该函数返回当前 runtime latest 的副本和统一序号，不消费或清除 snapshot。

~~~systemverilog
seq = runtime_csr_snapshot_seq;
if (!runtime_csr_snapshot_valid) begin
    item = make_empty_raw_csr();
    return 1'b0;
end
item = runtime_csr_snapshot;
return 1'b1;
~~~

L2TLB sequence每拍读取该接口，并把同一 seq 传给 common_data_transaction::apply_raw_csr_runtime()。多个consumer读取不会互相争抢；同一seq的重复apply由公共数据层幂等抑制。

### 4.3 push_raw_csr() 与 get_latest_raw_csr()

这两个函数服务 dispatch semantic raw flow，不替代 runtime latest：

~~~text
push_raw_csr：
  只有capture gate、raw valid且runtime snapshot已建立时工作；
  当semantic latest无效或seq不同，把raw写入latest_raw_csr；
  使用runtime_csr_snapshot_seq作为统一版本，不单独制造第二个CSR版本号；

get_latest_raw_csr：
  返回gated semantic latest及其seq；
  不消费runtime snapshot。
~~~

clear_raw_monitor_queues() 清掉 latest_raw_csr_valid 后，只要capture仍开启，下一次 push_raw_csr() 可按无效标志重新发布同一runtime版本。runtime latest本身保持不变。

## 5. L2TLB flush lifecycle sideband

### 5.1 note_l2tlb_flush_event()

源码位置：

- CSR monitor
- fence monitor
- memblock_sync_pkg.sv

抽象功能描述：记录一个会影响 DTLB filter生命周期的最新事件，不承担 semantic sfence entry删除，也不直接清 pending queue。

~~~systemverilog
function void note_l2tlb_flush_event(input time sample_time);
    l2tlb_flush_event_seq++;
    l2tlb_flush_sample_time = sample_time;
    l2tlb_flush_event_valid = 1'b1;
endfunction
~~~

发布规则：

- CSR monitor在 satp_changed、vsatp_changed、hgatp_changed 或 priv_virt_changed 发生时发布。
- fence monitor对每个post-reset有效 sfence sample发布。
- event seq在一次仿真中单调递增，不因semantic raw clear回退。
- event sideband不受 dispatch_monitor_capture_en 控制。

### 5.2 get_latest_l2tlb_flush_event()

抽象功能描述：向 L2TLB lifecycle owner提供当前latest event的只读副本。

~~~systemverilog
event_seq = l2tlb_flush_event_seq;
sample_time = l2tlb_flush_sample_time;
valid = l2tlb_flush_event_valid;
~~~

sequence保存自己的 last_seen_flush_event_seq。它在等待NBA后读取该接口，发现seq前进才处理一次；读取不会pop raw_sfence_q，也不会替代 apply_raw_sfence() 对 live TLB entry的semantic失效。

## 6. L2TLB lifecycle owner API

### 6.1 try_claim_l2tlb_lifecycle_owner()

抽象功能描述：在任何ready开放前，尝试把唯一 responder lifecycle所有权交给一个sequence实例。

~~~text
输出当前owner名称；
如果lifecycle_owner_claimed为1：
  返回0，不修改状态；
否则：
  保存调用者名称，置claimed为1，返回1。
~~~

package helper只返回状态，不调用UVM report。sequence收到0后负责 uvm_fatal，避免把两个queue owner交给sequencer item arbitration隐式交错。

### 6.2 try_release_l2tlb_lifecycle_owner()

抽象功能描述：只允许当前owner在自然排空后释放所有权。

~~~text
输出当前owner；
若未claimed或调用者名称不匹配：
  返回0且保持状态；
否则：
  清claimed和owner name，返回1。
~~~

DUT reset不清owner。支持的交接顺序是：最终inactive item完成、sequence自然退出、release成功，随后新实例claim。强制 kill 后在同一仿真重新handoff不属于当前支持范围。

## 7. Sample 与公共时间边界

L2TLB responder 自己的 `sample_seq` 是 sequence 私有状态，不由本 package 维护，也不提供
`get_dut_sample_seq()` 接口。当前 package 只维护 dispatch service cycle；未来 cancel/redirect
对账专项若需要 DUT sample watermark，应由该专项明确新增写者和生命周期，不能把尚不存在的 API
当作当前 package 能力。

### 7.1 service cycle API

tick_dispatch_service_cycle() 和 get_dispatch_service_cycle() 维护 dispatch software service cycle。它用于日志、timeout和UID TLB record时间戳，不代替 L2TLB responder自己的 sample_seq。

## 8. L2TLB 相关状态不变量

- L2TLB sequence只能读取 runtime_csr_snapshot 和 l2tlb_flush_event，不能pop dispatch raw queue。
- runtime_csr_snapshot_seq 与 latest_raw_csr_seq共享版本语义，但两者是不同消费视图。
- clear_raw_monitor_queues()不应让L2TLB丢失已发布的runtime CSR或重复处理旧flush event。
- owner状态只由try-claim/release修改；package不负责检查 outstanding，sequence在release前检查。
- package不生成request token、不创建pending record、不选择response、不修改主表pass/fail/terminal。
- l2tlb_responder_active只表示 DTLB/L2TLB上游response takeover，不表示 L2Cache/PTW下游模型。
- dispatch_raw_csr_t中的 misalign 和 priv_debug字段目前只保存snapshot；是否被异常激励消费者使用由专项sequence/plan决定。
- 当前 package 不定义 cancel snapshot、redirect anchor 或公共 DUT sample sequence；这些属于后续 LSQ
  MMIO/status 与 redirect/cancel 专项的边界，不能在本分析中作为现有 API 描述。
