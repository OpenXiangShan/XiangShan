# mem_ut V2 LSQ MMIO/Status 框架适配 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 Plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md` |
| Review 基线 | `mem_ut_uvm_v2`，`HEAD=e374f39c5d` 加当前未提交工作区 |
| Review 日期 | 2026-07-23 |
| Review 状态 | 两轮blocker已修复，最后一轮独立终审 `FINAL PASS` |
| 当前结论 | coding、文档同步、最终工作区编译/专项回归和归档均已完成；legacy soft testcase受既有monitor X/Z阻塞，未作为通过证据 |

## 1. Review 范围

### 1.1 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `batch` | 同一拍连续选择并提交的一组 UID；这里只表示 ROB normal commit batch，不等于 SQ 物理出队数量 | `select_rob_commit_batch()`、`mark_rob_commit_batch()` | UID1 load 与 UID2 store 可同批 normal commit，但 `scommit` 只计 UID2 |
| `modeled ROB head` | 测试框架当前认为位于 ROB 提交队头的完整 `flag/value` key | `modeled_rob_deq_ptr`、`modeled_head_valid` | cursor 指向 UID2 时，从 UID2 status 读取完整 ROB key |
| `watermark` | 最后一批已成功 normal commit 的 tail ROB key，只用于满足 DUT 已提交范围比较，不代表新的 active head | `committed_rob_watermark` | 表尾 store 已 commit、尚未 `sqDeq` 时继续发布其 ROB key |
| `level sideband` | 每拍持续表达当前状态、不能只发一次脉冲的输入 | `pendingPtr`、`pendingst`、`pendingMMIOld` | active driver 无 item 时保持最近 head sideband |
| `pulse` | 只在当前 transaction 当拍有效、下一 active idle 必须清零的输入 | `scommit`、`flushSb` | 同一 store commit 不能在气泡拍重复计数 |
| `commit cursor` | 按 UID 顺序寻找下一 ROB head 的软件游标 | `commit_cursor_uid` | 只能跨过连续 `terminal_done` 前缀或成功处理的当前 head |
| `fault token` | 对当前 fault head 已被框架接受的 synthetic retire 记录；不是 normal commit batch 成员 | `fault_head_waiting`、`fault_head_uid` | token 和真实 LQ/SQ deq 可任意先后，完整 fault tuple 后才 rebase |
| `deq` | DUT 报告 LQ/SQ entry 已物理离队的事件，不等同于 ROB commit | `lqDeq`、`sqDeq`、`apply_raw_ctrl_deq()` | `scommit=1,sqDeq=0` 与 `scommit=0,sqDeq=1` 均合法 |
| `count-only` | raw 只携带出队数量、没有对应 pointer payload 的接口能力 | V2 `sqDeq`、`MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=0` | 从软件 `sq_deq_ptr` 解析连续 owner |
| `preflight` | 在任何 pointer、free count、map 被修改前，先验证整批 owner 和 key | `preflight_dut_*_deq()` | 同一 raw 的 SQ owner 失败时，LQ 侧也不得部分释放 |
| `owner` | 对某类状态生命周期拥有唯一写权限的对象 | singleton `lsq_commit_handler`、`common_data_transaction`、`lsq_ctrl_model` | adapter 不另建第二个 commit cursor |
| `reservation` | 软件模型对一次真实 LSQ launch 动态实例的预留和 DUT 可见性记录 | `lsq_reservation_*`、`memblock_lsq_reservation_token_t` | launch 后建 token，下一 sample 边界转为 `DUT_VISIBLE` |
| `sample sequence` | 同一 DUT clocking sample 共享的单调序号，不等同于 service cycle 或 `$time` 数值 | `get_dut_sample_seq()` | redirect anchor 与 ctrl cancel snapshot 用序号计算精确目标拍 |
| `anchor` | redirect monitor 实际采到 DUT redirect 输入的时序锚点 | `dispatch_raw_redirect_anchor_t` | framework record 按 FIFO 与相同 level/ROB payload 绑定 |
| `snapshot` | ctrl monitor 每个可见 sample 保存的 held cancel count | `dispatch_raw_cancel_snapshot_t` | 即使 LQ/SQ count 都为 0 也入队 |
| `epoch` | 区分 redirect 批次或同 UID 动态实例的版本号 | `redirect_epoch`、`dynamic_epoch`、reservation launch epoch | 同 UID reissue 后旧 token 不能命中新实例 |
| `reconcile` | 把软件 cancel count 与唯一目标 sample 的 DUT cancel count 做一次比较 | `service_cancel_reconcile()` | observed count 只对账，不再次恢复 free count |
| `active map` | 用 ROB/LQ/SQ key 反查当前 active UID 的映射 | `uid_by_active_rob`、`uid_by_lq`、`uid_by_sq` | deq preflight 必须命中唯一 active owner |
| `runtime drain` | terminal 前缀完成后，对所有 raw/event/issue/map/recovery 队列和控制位的统一收敛判定 | `runtime_drain_complete()` | cancel snapshot 未消费时禁止置 global stop |
| `global stop` | 主 service 确认 transaction 完成且 runtime drain 后发布的全局退出条件 | `global_stop_requested` | 各主动 sequence 只读该位并在安全 idle 后退出 |
| `deferred FIFO` | 已完成semantic转换、等待full-raw owner成功应用的持久队列 | `memblock_sync_pkg::deferred_raw_ctrl_q` | resync mismatch不pop队首，下一service tick重试 |

### 1.2 覆盖范围与排除项

本 review 覆盖以下 LSQ MMIO/status 子 plan 实现：

- `pendingPtr/pendingst/pendingMMIOld/scommit` 的 ROB head sideband 与 normal/fault 分流。
- V2 `sqDeq` count-only、LQ/SQ deq 原子预检、mapping/free count/terminal 收口。
- `sqDeq`、`lqCancelCnt/sqCancelCnt` 的编译期宽度及 ctrl raw/agent/monitor 链。
- redirect reservation token、anchor/snapshot、software cancel 与 DUT cancel 对账。
- lsqcommit driver active idle level 保持、主 service 调度和 global-stop runtime drain。
- real cancel directed sequence、responder 自然退出和相关 package/filelist 注册。

以下内容不纳入本 review 的功能正确性结论：

- pending-MMIO 专项拥有的 MMIO raw producer、ROB value-only resolver、canonical tag 和 provenance 修复。
- int writeback、IQ feedback、CSR、L2TLB、DCache/L2 sideband 等其他 V2 子 plan。
- RM、scoreboard、coverage、MMIO 地址/响应正确性。
- 当前并行 worker 对源码或文档的后续修改；本 review 只反映生成时读取到的工作区快照。

### 1.3 修改前、修改原因、修改后与正确性总览

| 功能 | 修改前逻辑 | 修改原因 | 修改后逻辑 | 正确性检查 |
|---|---|---|---|---|
| ROB head sideband | `pendingPtr` 取 commit batch tail，head 和 cursor 无独立 owner | V2 需要持续的 ROB head sideband；tail/key+1 会误驱 manual/wrap key | cursor 从 status 重建 modeled head；表尾发布已提交 watermark | active-map 暂未命中只清类型 sideband，不清 pointer |
| normal/fault | fault 可混入 normal batch并被 cursor 提前跨过 | fault token 不是 DUT normal commit | normal batch只收连续 normal candidate；fault head单独建 token | fault token 与真实 deq 任意顺序，完整 terminal tuple 后 rebase |
| `scommit`/`sqDeq` | 两个事件可能被当成同拍同数量或共同推进 SQ pointer | 方向、生产阶段、延迟和计数单位均不同 | `scommit`只数normal scalar ROB store分类，即普通STU store和STU CBO；`sqDeq`只释放真实SQ entry | 不建立相等断言；统一helper验证LOAD/STORE/CBO为0/1/1；仅最终`rob_commit && lsq_deq`收口 |
| V2 SQ deq | 旧逻辑假定 SQ pointer存在 | V2 顶层只有 `sqDeq` count | 从软件 SQ head预检连续 owner；profile capability选择分支 | count 超过 `EnsbufferWidth` fatal；同 raw LQ/SQ 原子提交 |
| cancel | 软件 free-count 回退与 DUT output没有逐 epoch 对账 | held output 无 valid，简单 nonzero/value-change 会漏记或重复 | 每拍 snapshot，redirect anchor 定目标 sample，software count 直接对比 observed | observed 不调用 `cancel_lq/sq()`，避免双重恢复 |
| active idle | no-item/gap 进入 configured idle，可能破坏 level | `pending*` 必须持续，`scommit/flushSb` 必须单拍 | `drive_active_idle()` 保持 cache level并清 pulse | reset/configured idle 仍保留原 `drv_mode` 路径 |
| global stop | 只看 transaction terminal 或局部 cancel 状态 | raw/event/map/recovery 尚未 drain 时会假通过或让 responder丢尾部事件 | 统一 `runtime_drain_complete()` 后才置 stop | predicate 不扫描主表，只读 queue/map size和控制位 |
| deferred ctrl | automatic列表逐项调用void wrapper，resync warning也会丢raw | warning不等于应用成功，且栈帧返回会销毁失败项 | 持久FIFO按队首success消费，失败保留并计入runtime drain | 后续raw不越过；strict仍fatal |
| software-only owner | normal/fault smoke各建私有commit handler | 单测绕过真实singleton cursor/head合同 | 两个body均get singleton并按场景reset私有状态 | 不直接reset公共status/map/LSQ pointer |

### 1.4 端到端调用关系

| 顺序 | 函数/task | 抽象职责 | 主要输出或副作用 |
|---:|---|---|---|
| 1 | ctrl/redirect monitor `mon_data()` | 采样 `sqDeq`、cancel held level和 redirect anchor | 写 semantic raw 或 timing sideband queue，不写 status |
| 2 | `service_monitor_once()` | 每个 service tick 的唯一调度点 | 前后 drain timing sideband，中间处理 semantic batch和 redirect |
| 3 | `apply_deferred_ctrl_updates_batch()` | 把本拍raw追加到持久FIFO并按队首success消费 | 失败项保留，runtime drain继续pending |
| 4 | `apply_raw_ctrl_deq()` | 对同一 raw 联合预检 LQ/SQ deq并返回success | 成功后释放 mapping和 free count |
| 5 | `request_redirect_flush()` | 为一次 redirect 创建 record并冻结 issue | 建立 active record、redirect drive和 flush epoch |
| 6 | `apply_redirect_flush_range()` | 唯一扫描 redirect 命中的 active窗口 | 累计 software cancel、清旧实例、回退 admission上界 |
| 7 | `apply_pending_lsq_cancels()` | 按 record一次性恢复软件 LQ/SQ资源 | 调 `cancel_lq/sq()` 并标记 `software_applied` |
| 8 | `service_cancel_reconcile()` | 在精确 target sample比较 DUT observed count | 只写 observed/match统计，不写 free count |
| 9 | `build_lsqcommit_xaction()` | 构建当前 ROB head level和本拍 commit pulse | 输出 `pending*`、`scommit`、fault/normal选择结果 |
| 10 | `mark_rob_commit_batch()` / fault helper | transaction被 driver采样后提交软件状态 | 更新 `rob_commit`、cursor、fault token、watermark |
| 11 | `request_global_stop_if_done()` | terminal前缀和全部 runtime状态收敛后置 stop | 其它 sequence随后安全退出 |

## 2. 编译期宽度与 Ctrl Raw 链

### 2.1 结构参数与派生宽度

**抽象功能描述：** compile profile 保存 V2 `EnsbufferWidth`、LSQ容量和 redirect/cancel采样延迟的唯一结构事实；运行期 sequence 只消费派生宽度和延迟，不建立同义 plus 参数。

源码位置：`mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh`，SQ deq/cancel 参数组。

```systemverilog
`ifndef MEMBLOCK_DUT_ENSBUFFER_WIDTH
    `define MEMBLOCK_DUT_ENSBUFFER_WIDTH 2
`endif
`define MEMBLOCK_SQ_DEQ_COUNT_W ($clog2(`MEMBLOCK_DUT_ENSBUFFER_WIDTH + 1))
`define MEMBLOCK_LQ_CANCEL_COUNT_W ($clog2(`MEMBLOCK_DUT_LQ_SIZE + 1))
`define MEMBLOCK_SQ_CANCEL_COUNT_W ($clog2(`MEMBLOCK_DUT_SQ_SIZE + 1))

`ifndef MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY
    `define MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY 1
`endif
`ifndef MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY
    `define MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY 2
`endif
`ifndef MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET
    `define MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET 1
`endif
`define MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY \
    (`MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY + `MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET)
`define MEMBLOCK_CANCEL_RECORD_MAX_DEPTH \
    (`MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY + 2)
`define MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH \
    (2 * `MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY + 8)
```

中文伪代码：

1. 本段定义 V2 SQ 每拍最多物理出队两个 entry，并从结构上限派生 `sqDeq` packed width。
2. LQ/SQ cancel count分别从各自容量派生，禁止复用 SQ index width或固定 `[6:0]/[5:0]`。
3. redirect anchor 到 LSQ采样、DUT cancel更新和 monitor观察偏移按编译期相加；派生深度只用于有界 record/snapshot队列。
4. 本段不读取运行期参数，也不推进任何 queue、pointer、free count或 terminal。

修改前固定 `[1:0]`、`[6:0]`、`[5:0]` 形成第二权威；修改后 raw/interface/xaction/monitor 和一致性检查共同消费上述宏。合法 `sqDeq` 仍需额外检查 `count <= MEMBLOCK_DUT_ENSBUFFER_WIDTH`，因为 packed width还能编码 3。

### 2.2 Raw 类型与 timing sideband 分离

**抽象功能描述：** `dispatch_raw_ctrl_t` 保存有语义事件的完整 ctrl sample；cancel snapshot与 redirect anchor使用独立 timing queue，避免 held level被 semantic event gate漏采或重复解释。

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`，raw 类型定义。

```systemverilog
typedef struct {
    bit               valid;
    bit [3:0]         lq_deq;
    bit [`MEMBLOCK_SQ_DEQ_COUNT_W-1:0] sq_deq;
    bit               lq_deq_ptr_flag;
    bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] lq_deq_ptr_value;
    bit               sq_deq_ptr_valid;
    bit               sq_deq_ptr_flag;
    bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] sq_deq_ptr_value;
    bit [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0] load_mmio_valid;
    bit [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0][`MEMBLOCK_DUT_ROB_VALUE_W-1:0]
                      load_mmio_rob_value;
    bit               store_mmio_valid;
    bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] store_mmio_rob_value;
    int unsigned      mmio_flush_epoch;
    bit               memory_violation_valid;
    bit               memory_violation_rob_valid;
    bit               memory_violation_rob_flag;
    bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] memory_violation_rob_value;
    bit               memory_violation_ftq_flag;
    bit [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] memory_violation_ftq_value;
    bit [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] memory_violation_ftq_offset;
    bit               memory_violation_is_rvc;
    bit [49:0]        memory_violation_target;
    bit               memory_violation_level;
    bit               sb_is_empty;
    longint unsigned  cycle;
} dispatch_raw_ctrl_t;

typedef struct {
    bit [`MEMBLOCK_LQ_CANCEL_COUNT_W-1:0] lq_cancel_count;
    bit [`MEMBLOCK_SQ_CANCEL_COUNT_W-1:0] sq_cancel_count;
    longint unsigned                      sample_seq;
    longint unsigned                      cycle;
} dispatch_raw_cancel_snapshot_t;

typedef struct {
    bit                                  valid;
    bit                                  level;
    bit                                  rob_flag;
    bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] rob_value;
    longint unsigned                     sample_seq;
    longint unsigned                     cycle;
} dispatch_raw_redirect_anchor_t;
```

中文伪代码：

1. 本段让 semantic ctrl raw携带 LQ/SQ deq及 capability-valid pointer；该 raw仍可同时携带 memoryViolation、SB状态和其它专项的 MMIO事实。
2. cancel held level不加入 semantic raw，而是每拍保存 count、sample序号和时间；redirect anchor只保存实际采样的 level/ROB key与sample序号。
3. adapter分别消费 semantic raw和 timing sideband；snapshot/anchor本身不调用 release、cancel、commit或 terminal API。

### 2.3 Ctrl monitor 的 cancel snapshot producer

**抽象功能描述：** ctrl monitor 每个 post-reset可见 sample都发布一份 held cancel snapshot；它只做宽度/XZ/容量检查和事实入队，不计算 software cancel，也不更新 free count。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv`，`mon_data()` 的 cancel相关完整分支。

```systemverilog
begin
    memblock_sync_pkg::dispatch_raw_cancel_snapshot_t cancel_snapshot;

    if (io_mem_to_ooo_lqCancelCnt > `MEMBLOCK_DUT_LQ_SIZE ||
        io_mem_to_ooo_sqCancelCnt > `MEMBLOCK_DUT_SQ_SIZE) begin
        `uvm_fatal("CTRL_MONITOR",
                   $sformatf("DUT cancel count exceeds capacity lq=%0d/%0d sq=%0d/%0d",
                             io_mem_to_ooo_lqCancelCnt, `MEMBLOCK_DUT_LQ_SIZE,
                             io_mem_to_ooo_sqCancelCnt, `MEMBLOCK_DUT_SQ_SIZE))
    end

    cancel_snapshot.lq_cancel_count = io_mem_to_ooo_lqCancelCnt;
    cancel_snapshot.sq_cancel_count = io_mem_to_ooo_sqCancelCnt;
    cancel_snapshot.sample_seq = memblock_sync_pkg::get_dut_sample_seq($time);
    cancel_snapshot.cycle = $time;
    memblock_sync_pkg::push_raw_cancel_snapshot(cancel_snapshot);
end
```

中文伪代码：

1. 本分支在每个有效 monitor sample记录 DUT held cancel level，不依赖 count非零或 semantic ctrl事件。
2. 先拒绝超过LQ/SQ容量的值，再复制两个 count；调用 `get_dut_sample_seq()` 让同一采样时刻共享一个单调序号。
3. `push_raw_cancel_snapshot()` 只在 monitor capture开启时把snapshot压入有界queue；后续主service drain和reconcile才决定目标拍，当前分支不写软件状态。

## 3. ROB Head Sideband 与 Commit/Fault 分流

### 3.1 `rebase_framework_head_from_commit_cursor()`

**抽象功能描述：** commit owner把连续已终态前缀后的 cursor重新绑定为 modeled ROB head；函数只维护 handler私有 cursor/head，不驱接口，也不处理 fault waiting中的重定位。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`。

```systemverilog
function void rebase_framework_head_from_commit_cursor();
    status_transaction status;

    ensure_handles();
    if (fault_head_waiting) begin
        `uvm_fatal("LSQ_COMMIT", "head rebase is not allowed while fault token is waiting")
    end
    advance_commit_cursor_past_done();
    if (commit_cursor_uid > data.main_trans_num) begin
        `uvm_fatal("LSQ_COMMIT", "commit cursor moved beyond main table")
    end
    if (commit_cursor_uid == data.main_trans_num) begin
        modeled_head_valid = 1'b0;
        modeled_rob_deq_ptr = '{default:'0};
        return;
    end
    status = data.get_status(commit_cursor_uid);
    modeled_rob_deq_ptr = status.get_rob_key();
    modeled_head_valid = 1'b1;
endfunction:rebase_framework_head_from_commit_cursor
```

中文伪代码：

1. 本函数把软件 commit cursor和当前 modeled ROB head同步到同一个权威 status，不负责接口驱动或实际 commit。
2. fault token等待期间禁止 rebase；否则先调用 `advance_commit_cursor_past_done()` 只跨过连续 `terminal_done` UID。
3. cursor越界立即fatal；恰好到表尾时清 head-valid并返回；尚有UID时直接读取该UID完整ROB key并置valid。
4. `advance_commit_cursor_past_done()` 只推进游标，不修改status；本函数不做 ROB key加一算术。

### 3.2 `clear_lsqcommit_xaction()`

**抽象功能描述：** transaction初始化阶段发布 modeled head或最终 committed watermark，并把类型sideband和单拍pulse清零；active-map lookup不参与 pointer选择。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`。

```systemverilog
function void clear_lsqcommit_xaction(input lsqcommit_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal("LSQ_COMMIT", "clear_lsqcommit_xaction got null transaction")
    end
    ensure_modeled_rob_deq_ptr_initialized();
    if (modeled_head_valid) begin
        tr.io_ooo_to_mem_lsqio_pendingPtr_flag  = modeled_rob_deq_ptr.flag;
        tr.io_ooo_to_mem_lsqio_pendingPtr_value = modeled_rob_deq_ptr.value;
    end else if (committed_watermark_publishable()) begin
        tr.io_ooo_to_mem_lsqio_pendingPtr_flag  = committed_rob_watermark.flag;
        tr.io_ooo_to_mem_lsqio_pendingPtr_value = committed_rob_watermark.value;
    end else begin
        tr.io_ooo_to_mem_lsqio_pendingPtr_flag  = 1'b0;
        tr.io_ooo_to_mem_lsqio_pendingPtr_value = '0;
    end
    tr.io_ooo_to_mem_lsqio_pendingst        = 1'b0;
    tr.io_ooo_to_mem_lsqio_pendingMMIOld    = 1'b0;
    tr.io_ooo_to_mem_lsqio_scommit          = '0;
    tr.io_ooo_to_mem_flushSb                = 1'b0;
endfunction:clear_lsqcommit_xaction
```

中文伪代码：

1. 本函数为每拍LSQ commit transaction建立中性基线，同时保证 `pendingPtr` 的level语义连续。
2. 先检查transaction非空并确保modeled head初始化；有head时无条件发布完整head key。
3. 没有head但所有UID已提交到表尾且watermark有效时发布最后已提交tail；否则发布零key。
4. 无论pointer来源为何，都先清 `pendingst/pendingMMIOld` 和 `scommit/flushSb`；build阶段只为合法当前head与本拍事件重新置位。

### 3.3 `build_lsqcommit_xaction()`

**抽象功能描述：** 每个lsqcommit sequence周期解析当前head，互斥选择normal batch或fault head，并构造level sideband与本拍scalar ROB store commit count；这里的scalar ROB store对应V2 `CommitType.STORE && !vls`，包含普通STU store和STU CBO。函数不直接修改status或cursor。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`。

```systemverilog
function void build_lsqcommit_xaction(output lsqcommit_agent_agent_xaction tr,
                                      output memblock_uid_t commit_uids[$],
                                      output bit has_commit,
                                      output bit has_fault_head,
                                      output memblock_uid_t fault_uid);
    memblock_uid_t head_uid;
    bit has_head;

    ensure_handles();
    ensure_modeled_rob_deq_ptr_initialized();
    sync_modeled_head_after_fault_terminal();
    has_head = resolve_sideband_head_uid(head_uid);
    select_rob_commit_batch(commit_uids);
    has_commit = commit_uids.size() != 0;
    has_fault_head = 1'b0;
    fault_uid = 0;
    if (!has_commit) begin
        has_fault_head = select_fault_head_candidate(fault_uid);
    end
    tr = lsqcommit_agent_agent_xaction::type_id::create("lsqcommit_dispatch_tr");
    if (tr == null) begin
        `uvm_fatal("LSQ_COMMIT", "failed to create lsqcommit xaction")
    end
    clear_lsqcommit_xaction(tr);
    if (has_head && !has_fault_head && !fault_head_waiting) begin
        memblock_op_behavior_t head_behavior;

        head_behavior = lsq_ctrl_model::derive_op_behavior(data.get_main_transaction(head_uid));
        tr.io_ooo_to_mem_lsqio_pendingst =
            memblock_op_behavior_util::is_scalar_rob_store_commit(head_behavior);
        tr.io_ooo_to_mem_lsqio_pendingMMIOld =
            head_behavior.commit_is_load && data.uid_is_mmio_load(head_uid);
    end
    foreach (commit_uids[idx]) begin
        memblock_op_behavior_t behavior;

        behavior = lsq_ctrl_model::derive_op_behavior(data.get_main_transaction(commit_uids[idx]));
        if (memblock_op_behavior_util::is_scalar_rob_store_commit(behavior)) begin
            tr.io_ooo_to_mem_lsqio_scommit++;
        end
    end
endfunction:build_lsqcommit_xaction
```

中文伪代码：

1. 本函数生成一拍ROB-head sideband和commit pulse，不在driver采样前提前提交软件状态。
2. 先同步已完成fault head并解析当前active head；再选择连续normal candidates。normal batch为空时才尝试选择fault head，保证两条路径互斥。
3. 创建transaction并调用 `clear_lsqcommit_xaction()` 发布pointer基线；当前head真实active且不是fault路径时，`is_scalar_rob_store_commit()`决定 `pendingst`，MMIO tag query决定 `pendingMMIOld`。
4. 遍历normal batch，仅对V2 scalar ROB store分类递增 `scommit`；该分类对应`CommitType.STORE && !vls`，普通STU store和STU CBO计入，load/atomic/fault不计入。
5. `derive_op_behavior()` 只派生操作类别，`uid_is_mmio_load()` 只查询状态标签；两者均不推进commit/deq/terminal。

### 3.4 `mark_rob_commit_batch()`

**抽象功能描述：** transaction经driver采样后，commit owner原子验证并应用整个normal batch，保存tail watermark，再把cursor rebase到下一权威head；函数不释放LQ/SQ资源。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`。

```systemverilog
function void mark_rob_commit_batch(input memblock_uid_t uids[$]);
    memblock_rob_key_t batch_tail_rob_key;
    memblock_rob_key_t previous_rob_key;

    if (uids.size() == 0) begin
        return;
    end
    ensure_modeled_rob_deq_ptr_initialized();
    if (fault_head_waiting || uids[0] != commit_cursor_uid ||
        data.get_status(uids[0]).get_rob_key() != modeled_rob_deq_ptr) begin
        `uvm_fatal("LSQ_COMMIT", "normal commit batch does not start at the modeled ROB head")
    end
    foreach (uids[idx]) begin
        memblock_rob_key_t current_rob_key;

        current_rob_key = data.get_status(uids[idx]).get_rob_key();
        rob_order_util::check_rob_key(current_rob_key, "mark_rob_commit_batch");
        if (uids[idx] != commit_cursor_uid + idx ||
            !uid_is_normal_commit_candidate(uids[idx]) ||
            uid_is_fault_terminal_candidate(uids[idx])) begin
            `uvm_fatal("LSQ_COMMIT",
                       $sformatf("normal commit batch preflight failed idx=%0d uid=%0d cursor=%0d",
                                 idx, uids[idx], commit_cursor_uid))
        end
        if (idx != 0 && !rob_order_util::rob_is_after(current_rob_key, previous_rob_key)) begin
            `uvm_fatal("LSQ_COMMIT",
                       $sformatf("normal commit batch ROB keys are not monotonic idx=%0d prev=%0d/%0d current=%0d/%0d",
                                 idx, previous_rob_key.flag, previous_rob_key.value,
                                 current_rob_key.flag, current_rob_key.value))
        end
        previous_rob_key = current_rob_key;
    end
    foreach (uids[idx]) begin
        if (!mark_rob_commit_uid(uids[idx])) begin
            `uvm_fatal("LSQ_COMMIT", $sformatf("normal commit uid=%0d was not applied", uids[idx]))
        end
    end
    batch_tail_rob_key = data.get_status(uids[uids.size() - 1]).get_rob_key();
    committed_rob_watermark = batch_tail_rob_key;
    committed_rob_watermark_valid = 1'b1;
    commit_cursor_uid = uids[uids.size() - 1] + 1;
    rebase_framework_head_from_commit_cursor();
endfunction:mark_rob_commit_batch
```

中文伪代码：

1. 本函数在已驱动normal batch后一次性更新 `rob_commit` 与commit cursor，不承担SQ物理出队。
2. 空batch直接返回；非空batch必须从modeled head开始，且当前不能有fault token等待。
3. 第一轮逐UID验证连续UID、normal candidate、非fault和严格递增ROB顺序；任一失败时尚未修改batch状态。
4. 第二轮调用 `mark_rob_commit_uid()` 设置每个UID的 `rob_commit`，无LSQ mapping的UID可直接置 `lsq_deq`，并尝试终态收口。
5. 全批成功后从最后UID status读取完整ROB key保存watermark，cursor推进到tail后一UID，再由rebase从status获取下一head；不执行ROB key算术。

### 3.5 Fault token 与 terminal rebase

**抽象功能描述：** fault helper只接受当前modeled head并冻结head；后续真实deq或先到deq均可触发终态，只有fault UID的动态实例和完整terminal tuple一致时才推进cursor。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`，`sync_modeled_head_after_fault_terminal()`。

```systemverilog
function bit sync_modeled_head_after_fault_terminal();
    status_transaction status;

    ensure_modeled_rob_deq_ptr_initialized();
    if (!fault_head_waiting) begin
        if (commit_cursor_uid < data.main_trans_num &&
            data.get_status(commit_cursor_uid).terminal_done) begin
            rebase_framework_head_from_commit_cursor();
        end
        return 1'b0;
    end
    status = data.get_status(fault_head_uid);
    if (status.dynamic_epoch != fault_head_dynamic_epoch ||
        status.flushed || status.issue_killed ||
        !status.rob_commit) begin
        fault_head_waiting = 1'b0;
        modeled_head_valid = 1'b0;
        rebase_framework_head_from_commit_cursor();
        return 1'b0;
    end
    if (!status.terminal_done || !status.lsq_deq || status.active || status.success ||
        !status.fault || status.active_lq_mapped || status.active_sq_mapped) begin
        return 1'b0;
    end
    commit_cursor_uid = fault_head_uid + 1;
    fault_head_waiting = 1'b0;
    fault_head_uid = 0;
    fault_head_dynamic_epoch = 0;
    modeled_head_valid = 1'b0;
    rebase_framework_head_from_commit_cursor();
    return 1'b1;
endfunction:sync_modeled_head_after_fault_terminal
```

中文伪代码：

1. 本函数把fault token与最终fault terminal状态同步，不规定token和真实deq谁先发生。
2. 没有waiting token时，仅在当前cursor UID已terminal时执行普通rebase，然后返回未完成fault同步。
3. waiting token所属动态实例被redirect替换、被kill或token位消失时，清旧token并在同一cursor重新建立head，等待reissue。
4. 当前实例尚未同时满足 `terminal_done、lsq_deq、非active、fault、无mapping且success=0` 时保持head不动。
5. tuple完整后把cursor推进到fault UID后一项，清token字段并rebase；本函数不制造LQ/SQ deq。

## 4. `scommit` 与真实 LQ/SQ Deq 解耦

### 4.1 SQ count-only preflight

**抽象功能描述：** V2 SQ deq owner从软件SQ head开始验证 `count` 个连续active owner，生成待提交UID列表；preflight失败前不修改pointer、free count、map或terminal。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`，`preflight_dut_sq_deq_from_start()`。

```systemverilog
function bit preflight_dut_sq_deq_from_start(input int unsigned count,
                                             input memblock_sq_key_t start_key,
                                             input bit ptr_is_next,
                                             output memblock_uid_t deq_uids[$]);
    ensure_handles();
    deq_uids.delete();
    if (count == 0) begin
        return 1'b1;
    end
    if (count > MEMBLOCK_DUT_ENSBUFFER_WIDTH) begin
        `uvm_fatal("LSQ_COMMIT",
                   $sformatf("sqDeq count=%0d exceeds EnsbufferWidth=%0d",
                             count, MEMBLOCK_DUT_ENSBUFFER_WIDTH))
    end
    if (start_key != lsq_ctrl.sq_deq_ptr) begin
        report_deq_mismatch($sformatf("DUT sqDeq start flag=%0d value=%0d mismatches software SQ head flag=%0d value=%0d count=%0d",
                                      start_key.flag, start_key.value,
                                      lsq_ctrl.sq_deq_ptr.flag, lsq_ctrl.sq_deq_ptr.value,
                                      count));
        return 1'b0;
    end
    for (int unsigned idx = 0; idx < count; idx++) begin
        memblock_sq_key_t key;
        memblock_uid_t uid;
        status_transaction status;
        bit uid_seen;

        key = lsq_ctrl_model::advance_sq_key(start_key, idx);
        if (data.lookup_active_uid_by_sq(key, uid)) begin
            status = data.get_status(uid);
            uid_seen = 1'b0;
            foreach (deq_uids[seen_idx]) begin
                if (deq_uids[seen_idx] == uid) begin
                    uid_seen = 1'b1;
                end
            end
            if (uid_seen || !status.active || !status.active_sq_mapped ||
                status.sqIdx_flag != key.flag || status.sqIdx_value != key.value) begin
                report_deq_mismatch($sformatf("DUT sqDeq owner mismatch uid=%0d key=%0d/%0d active=%0d mapped=%0d status_key=%0d/%0d duplicate=%0d",
                                              uid, key.flag, key.value,
                                              status.active, status.active_sq_mapped,
                                              status.sqIdx_flag, status.sqIdx_value,
                                              uid_seen));
                deq_uids.delete();
                return 1'b0;
            end
            deq_uids.push_back(uid);
        end else begin
            report_deq_mismatch($sformatf("stale DUT sqDeq count=%0d key flag=%0d value=%0d has no active uid",
                                          count, key.flag, key.value));
            deq_uids.delete();
            return 1'b0;
        end
    end
    return 1'b1;
endfunction:preflight_dut_sq_deq_from_start
```

中文伪代码：

1. 本函数为一次SQ deq准备完整owner列表，不执行实际release。
2. 先清输出列表；零count直接成功；超过 `EnsbufferWidth` fatal；起点与软件SQ head不一致时按配置warning/fatal并返回失败。
3. 按count从start key连续推进，每个key必须在SQ active map命中；命中后还要验证UID未重复、status active、mapping有效且status key一致。
4. 任一owner失败时清空临时UID列表并返回失败；全部成功时返回列表。
5. `advance_sq_key()` 只计算环形key，`lookup_active_uid_by_sq()` 只查map，`report_deq_mismatch()` 决定严格或resync失败策略；均未更新LSQ资源。

### 4.2 Full ctrl raw 联合预检与原子提交

**抽象功能描述：** 同一ctrl raw先更新SB观察值，再联合预检LQ/SQ；两侧全部成功后才依次提交release并返回成功。resync mismatch返回失败，由上层保留队首。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`，`apply_raw_ctrl_deq()`。

```systemverilog
function bit apply_raw_ctrl_deq(input memblock_sync_pkg::dispatch_raw_ctrl_t raw);
    memblock_uid_t lq_uids[$];
    memblock_uid_t sq_uids[$];
    memblock_lq_key_t lq_ptr;
    memblock_sq_key_t sq_ptr;

    ensure_handles();
    data.update_sb_is_empty(raw.sb_is_empty);
    if (raw.sq_deq == 0 && raw.sq_deq_ptr_valid) begin
        `uvm_fatal("LSQ_COMMIT", "sqDeq pointer is valid while sqDeq count is zero")
    end
    if (!MEMBLOCK_DUT_HAS_SQ_DEQ_PTR && raw.sq_deq_ptr_valid) begin
        `uvm_fatal("LSQ_COMMIT", "V2 count-only SQ deq raw unexpectedly carries a pointer")
    end
    if (MEMBLOCK_DUT_HAS_SQ_DEQ_PTR && raw.sq_deq != 0 && !raw.sq_deq_ptr_valid) begin
        `uvm_fatal("LSQ_COMMIT", "pointer-capable SQ deq raw is missing its pointer")
    end

    lq_ptr.flag = raw.lq_deq_ptr_flag;
    lq_ptr.value = raw.lq_deq_ptr_value;
    sq_ptr.flag = raw.sq_deq_ptr_flag;
    sq_ptr.value = raw.sq_deq_ptr_value;

    if (!preflight_dut_lq_deq(raw.lq_deq, lq_ptr, 1'b1, lq_uids)) begin
        return 1'b0;
    end
    if (MEMBLOCK_DUT_HAS_SQ_DEQ_PTR) begin
        if (!preflight_dut_sq_deq(raw.sq_deq, sq_ptr, 1'b1, sq_uids)) begin
            return 1'b0;
        end
    end else begin
        if (!preflight_dut_sq_deq_count_only(raw.sq_deq, sq_uids)) begin
            return 1'b0;
        end
    end
    commit_dut_lq_deq(raw.lq_deq, lq_uids);
    commit_dut_sq_deq(raw.sq_deq, sq_uids);
    foreach (lq_uids[idx]) begin
        data.try_retire_committed_uid(lq_uids[idx]);
    end
    foreach (sq_uids[idx]) begin
        data.try_retire_committed_uid(sq_uids[idx]);
    end
    sync_modeled_head_after_fault_terminal();
    return 1'b1;
endfunction:apply_raw_ctrl_deq
```

中文伪代码：

1. 本函数是ctrl raw中真实LQ/SQ deq的唯一应用入口，不消费 `scommit`，也不把cancel snapshot当deq。
2. 先更新 `sbIsEmpty`；随后检查pointer-valid与profile capability组合，V2出现SQ pointer立即fatal。
3. 组装LQ/SQ key，先完整预检LQ；再按capability选择SQ pointer或count-only预检。任一侧失败都返回0，尚未更新任何deq资源。
4. 两侧都成功后分别调用commit helper推进对应deq pointer/free count并删除mapping；再对受影响UID调用 `try_retire_committed_uid()`。
5. 最后同步可能因本次deq完成的fault head并返回1；normal commit状态不会由本函数补造。

### 4.2.1 Deferred full raw 持久 FIFO

**抽象功能描述：** adapter在semantic batch返回后，把本拍完整raw追加到全局持久FIFO，并只从队首尝试
应用。该函数负责“成功后pop、失败保留”，不解析LQ/SQ owner，也不重复转换memoryViolation event。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，
`apply_deferred_ctrl_updates_batch()`。

```systemverilog
function void apply_deferred_ctrl_updates_batch(
    ref memblock_sync_pkg::dispatch_raw_ctrl_t deferred_ctrl[$]);
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;
    memblock_sync_pkg::dispatch_raw_ctrl_t applied_raw;

    foreach (deferred_ctrl[idx]) begin
        memblock_sync_pkg::push_deferred_raw_ctrl(deferred_ctrl[idx]);
    end
    deferred_ctrl.delete();
    while (memblock_sync_pkg::peek_deferred_raw_ctrl(raw)) begin
        if (!apply_raw_ctrl_deq(raw)) begin
            break;
        end
        if (!memblock_sync_pkg::pop_deferred_raw_ctrl(applied_raw)) begin
            `uvm_fatal("DISP_MON_BATCH", "deferred ctrl apply succeeded but queue pop failed")
        end
    end
endfunction:apply_deferred_ctrl_updates_batch
```

中文伪代码：

1. 本函数承接已完成semantic conversion的本拍临时列表，避免在resync失败后由栈帧销毁raw。
2. 按原FIFO顺序把全部本拍raw追加到`deferred_raw_ctrl_q`，随后清空临时列表；该过程不重复生成
   memoryViolation event。
3. 查看持久FIFO队首并调用adapter full-raw wrapper；wrapper先做MMIO normalization，再把未拆分raw交给
   singleton commit handler。
4. owner返回0时立即break，保留当前队首和全部后续raw；owner返回1时才pop。成功后却无法pop属于内部
   队列一致性错误，直接fatal。
5. `raw_monitor_queue_size()`统计该持久FIFO，所以失败raw未重试成功前`runtime_drain_complete()`返回0。

### 4.3 `try_retire_committed_uid()` 的最终合取条件

**抽象功能描述：** terminal owner只在当前active UID已经有commit token且所有LSQ mapping释放后收口；redirect命中的实例留给统一scan，normal与fault使用不同terminal结果。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
function void try_retire_committed_uid(input memblock_uid_t uid);
    status_transaction status;

    status = get_status(uid);
    if (!status.active || !status.rob_commit) begin
        return;
    end
    if (status.active_lq_mapped || status.active_sq_mapped) begin
        return;
    end
    if (active_redirect.valid &&
        rob_order_util::rob_need_flush(status.get_rob_key(), active_redirect)) begin
        return;
    end
    if (status.replay_pending || status.redirect_pending || status.flushed ||
        status.issue_killed) begin
        return;
    end
    if (status.fault || status.exception_pending ||
        status.load_fault || status.sta_fault || status.std_fault) begin
        consume_fault_retire(uid);
        return;
    end
    if (!status.pass || !required_targets_done(uid)) begin
        return;
    end
    set_status_field(uid, MEMBLOCK_STATUS_SUCCESS, 1'b1);
    set_status_field(uid, MEMBLOCK_STATUS_TERMINAL_DONE, 1'b1);
    retire_active_uid(uid);
endfunction:try_retire_committed_uid
```

中文伪代码：

1. 本函数把独立到达的ROB commit与真实LSQ deq最终汇合，不要求两者同拍或固定先后。
2. UID非active、尚无commit token或仍有LQ/SQ mapping时直接等待；当前redirect会覆盖该UID时也等待统一redirect scan记账。
3. replay/redirect/flushed/killed中间态不允许terminal；fault类状态调用 `consume_fault_retire()` 生成 `success=0,terminal_done=1` 并清active。
4. normal项还必须 `pass` 且全部required target完成，随后通过公共setter置success与terminal并调用 `retire_active_uid()` 删除active map。
5. 本函数不读 `scommit/sqDeq` 数值，只读二者各自owner已经写入的状态结果。

## 5. LSQCommit Sequence 与 Driver 时序

### 5.1 `send_lsqcommit_cycle()`

**抽象功能描述：** lsqcommit sequence每拍构造并发送一个transaction，driver sample返回后才应用normal/fault软件状态；该task还产生轻量activity标志和terminal idle握手。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv`。

```systemverilog
task memblock_lsqcommit_dispatch_base_sequence::send_lsqcommit_cycle(
    input int unsigned cycle_idx,
    output bit has_progress,
    output bit terminal_idle_published);
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t commit_uids[$];
    memblock_flushsb_req_t flushsb_req;
    bit has_commit;
    bit has_fault_head;
    memblock_uid_t fault_uid;
    bit has_flushsb_progress;

    has_commit = 1'b0;
    has_fault_head = 1'b0;
    fault_uid = 0;
    has_flushsb_progress = 1'b0;
    has_progress = 1'b0;
    terminal_idle_published = 1'b0;
    data.warn_flushsb_timeout_if_needed(seq_csr_common::get_flushsb_timeout());
    if (data.issue_blocked_by_global_flush()) begin
        tr = lsqcommit_agent_agent_xaction::type_id::create(
            $sformatf("lsqcommit_dispatch_idle_tr_%0d", cycle_idx));
        commit_handler.clear_lsqcommit_xaction(tr);
        start_item(tr);
        finish_item(tr);
        has_progress = data.flushsb_request_pending();
        return;
    end
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    tr.set_name($sformatf("lsqcommit_dispatch_tr_%0d", cycle_idx));
    if (data.try_pop_flushsb_request(flushsb_req)) begin
        tr.io_ooo_to_mem_flushSb = 1'b1;
        data.mark_flushsb_driven(flushsb_req,
                                 memblock_sync_pkg::get_dispatch_service_cycle());
        has_flushsb_progress = 1'b1;
    end

    start_item(tr);
    finish_item(tr);

    if (has_commit) begin
        commit_handler.mark_rob_commit_batch(commit_uids);
    end else if (has_fault_head) begin
        commit_handler.mark_fault_rob_commit_uid(fault_uid);
    end
    has_progress = has_commit || has_fault_head ||
                   has_flushsb_progress || data.flushsb_busy();
    terminal_idle_published = data.is_global_stop_requested() &&
                              commit_handler.commit_cursor_uid == data.main_trans_num &&
                              commit_handler.modeled_rob_deq_ptr_initialized &&
                              !commit_handler.modeled_head_valid &&
                              !commit_handler.fault_head_waiting &&
                              !data.flushsb_request_pending() &&
                              !data.cancel_reconcile_pending() &&
                              !data.redirect_sample_anchor_pending() &&
                              !data.cancel_snapshot_buffer_pending() &&
                              !tr.io_ooo_to_mem_lsqio_pendingMMIOld &&
                              !tr.io_ooo_to_mem_lsqio_pendingst &&
                              tr.io_ooo_to_mem_lsqio_scommit == '0 &&
                              !tr.io_ooo_to_mem_flushSb;
endtask:send_lsqcommit_cycle
```

中文伪代码：

1. 本task发送一拍head sideband、commit/flush pulse，并在driver完成采样后提交软件状态。
2. 初始化输出并检查flushSb timeout；全局flush期间只发送保持pointer的idle transaction，不选择commit/fault。
3. 正常路径调用build helper互斥选择normal batch或fault head；若有flushSb request则同拍置pulse并登记driven状态。
4. `start_item/finish_item` 返回后，normal路径调用batch commit，fault路径调用fault token helper，确保软件commit不早于接口采样。
5. `has_progress` 只用于idle warning，保留已有 `flushsb_busy()` 粗粒度activity语义；它不修改状态。
6. terminal idle允许 `pendingPtr` 保持watermark，只要求类型sideband、pulse和cancel/flush队列收敛。

### 5.2 Active idle level保持

**抽象功能描述：** driver在main phase没有item或处于pre/post gap时，重驱最近一次有效head level，只清pulse；reset/configured idle继续走原 `drv_mode`。

源码位置：`mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv`，`drive_active_idle()`。

```systemverilog
task lsqcommit_agent_agent_driver::drive_active_idle();
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingMMIOld <=
        cached_sideband_valid ? cached_pending_mmio_ld : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingst <=
        cached_sideband_valid ? cached_pending_st : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <=
        cached_sideband_valid ? cached_pending_ptr_flag : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <=
        cached_sideband_valid ? cached_pending_ptr_value : '0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_scommit <= '0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= '0;
endtask:drive_active_idle
```

中文伪代码：

1. 本task只处理active main-phase气泡，防止没有新item时丢失ROB head level。
2. cache有效时重驱最近transaction的 `pendingPtr/pendingst/pendingMMIOld`；尚无cache时驱零。
3. 无条件清 `scommit` 和 `flushSb`，避免把上一拍commit或flush重复发送。
4. cache由 `send_pkt()` 更新、reset清除；本task不读取 `drv_mode`，也不请求或完成sequence item。

## 6. Redirect/Cancel Reconciliation

### 6.1 Reservation 动态实例 token

**抽象功能描述：** LSQ enqueue owner在一次真实launch完成allocation后创建带单调epoch的token；下一driver sample边界用同一token把reservation转成DUT可见。该机制不替代原batch flush epoch，也不直接计算cancel count。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，`begin_lsq_reservation_launch()`。

```systemverilog
function int unsigned begin_lsq_reservation_launch(input memblock_uid_t uid);
    status_transaction status;

    check_uid(uid, "begin_lsq_reservation_launch");
    status = get_status(uid);
    if (!status.active || (!status.active_lq_mapped && !status.active_sq_mapped)) begin
        `uvm_fatal("LSQ_RESERVATION",
                   $sformatf("uid=%0d launch has no active LSQ mapping", uid))
    end
    if (status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_NONE) begin
        `uvm_fatal("LSQ_RESERVATION",
                   $sformatf("uid=%0d reservation already exists state=%0d epoch=%0d",
                             uid, status.lsq_reservation_state,
                             status.lsq_reservation_launch_epoch))
    end
    status.lsq_reservation_launch_epoch++;
    if (status.lsq_reservation_launch_epoch == 0) begin
        `uvm_fatal("LSQ_RESERVATION", "reservation epoch wrapped")
    end
    status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE;
    status.lsq_reservation_sample_valid = 1'b0;
    status.lsq_reservation_sample_seq = 0;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    return status.lsq_reservation_launch_epoch;
endfunction:begin_lsq_reservation_launch
```

中文伪代码：

1. 本函数为已经真实launch并完成software allocation的UID建立动态实例token，不负责开放issue。
2. 先验证UID active且存在LQ或SQ mapping；已有reservation状态说明重复launch，立即fatal。
3. 单调增加launch epoch并拒绝wrap，把状态置为等待下一sample，清旧sample valid/sequence并记录service cycle。
4. 返回launch epoch给LSQ enqueue sequence保存；该返回值只用于后续实例匹配，不改变pass/fail/terminal。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，`mark_lsq_reservation_sampled()`。

```systemverilog
function void mark_lsq_reservation_sampled(input memblock_uid_t uid,
                                           input int unsigned launch_epoch,
                                           input longint unsigned sample_seq);
    status_transaction status;

    check_uid(uid, "mark_lsq_reservation_sampled");
    status = get_status(uid);
    if (launch_epoch == 0 || sample_seq == 0) begin
        `uvm_fatal("LSQ_RESERVATION",
                   $sformatf("uid=%0d invalid sample token epoch=%0d sample_seq=%0d",
                             uid, launch_epoch, sample_seq))
    end
    if (status.lsq_reservation_launch_epoch != launch_epoch ||
        status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE) begin
        `uvm_fatal("LSQ_RESERVATION",
                   $sformatf("uid=%0d sample token mismatch expected epoch=%0d/state=%0d got epoch=%0d/state=%0d",
                             uid, status.lsq_reservation_launch_epoch,
                             status.lsq_reservation_state, launch_epoch,
                             MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE))
    end
    status.lsq_reservation_sample_seq = sample_seq;
    status.lsq_reservation_sample_valid = 1'b1;
    status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
endfunction:mark_lsq_reservation_sampled
```

中文伪代码：

1. 本函数在下一driver sample边界确认特定launch实例已经DUT可见，不决定该batch是否继续开放issue。
2. 拒绝零epoch/零sample；再要求status中的当前launch epoch和等待sample状态与token完全一致，防止旧callback命中新实例。
3. 匹配后保存sample序号、置sample valid并把状态改为 `DUT_VISIBLE`。
4. 外层 `complete_v2_pending_sample()` 随后仍按原batch flush epoch决定调用 `complete_admission()` 或丢弃issue开放；真实sample事实不会被该flush判断撤销。

### 6.2 Redirect record 与 software cancel 计数

**抽象功能描述：** `request_redirect_flush()` 为每次framework redirect创建唯一record并冻结issue；record在monitor anchor到达后承载software count和DUT observed count两个独立进度。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
function void request_redirect_flush(input memblock_redirect_payload_t redirect);
    if (!redirect.valid) begin
        `uvm_fatal("COMMON_DATA", "request_redirect_flush requires valid redirect")
    end
    if (active_redirect.valid || active_cancel_record_id_valid) begin
        `uvm_fatal("COMMON_DATA", "request_redirect_flush called while another redirect is active")
    end
    if (cancel_record_q.size() >= MEMBLOCK_CANCEL_RECORD_MAX_DEPTH) begin
        `uvm_fatal("LSQ_CANCEL", "cancel record FIFO is full before redirect allocation")
    end
    redirect_phase = MEMBLOCK_REDIRECT_PHASE_DETECTED;
    flush_in_progress = 1'b1;
    memblock_sync_pkg::dispatch_flush_in_progress = 1'b1;
    memblock_sync_pkg::dispatch_flush_epoch++;
    begin
        memblock_lsq_cancel_record_t record;

        record = '{default:'0};
        next_cancel_record_id++;
        if (next_cancel_record_id == 0) begin
            `uvm_fatal("LSQ_CANCEL", "cancel record id wrapped")
        end
        record.valid = 1'b1;
        record.redirect_epoch = memblock_sync_pkg::dispatch_flush_epoch;
        record.cancel_record_id = next_cancel_record_id;
        record.redirect = redirect;
        record.redirect_service_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        cancel_record_q.push_back(record);
        active_cancel_record_id = record.cancel_record_id;
        active_cancel_record_id_valid = 1'b1;
        check_cancel_record_capacity();
    end
    issue_freeze_ack = 1'b1;
    active_redirect = redirect;
    redirect_phase = MEMBLOCK_REDIRECT_PHASE_FREEZE_REQUESTED;
    redirect_freeze_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
endfunction:request_redirect_flush
```

中文伪代码：

1. 本函数是一次redirect cancel生命周期的record创建者，不执行active-window扫描或资源回退。
2. 拒绝invalid payload、并行active redirect和满FIFO；随后进入detected phase，置flush并增加framework epoch。
3. 创建全零record，生成非零唯一ID，保存epoch、payload和service cycle，压入FIFO并登记active record ID。
4. 最后冻结issue、保存active redirect并进入freeze-requested phase；monitor之后按payload FIFO绑定anchor。
5. `check_cancel_record_capacity()` 只检查有界深度；当前函数不写software/observed count。

**抽象功能描述：** `note_lsq_cancel_for_uid()` 在统一redirect scan删除mapping之前，根据当前DUT-visible reservation为目标record累计一次software LQ/SQ cancel；同一count既是资源回退量，也是DUT compare期望值。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
function void note_lsq_cancel_for_uid(input memblock_uid_t uid,
                                      input int unsigned redirect_epoch);
    status_transaction status;
    main_control_transaction main_tr;
    int record_idx;

    check_uid(uid, "note_lsq_cancel_for_uid");
    record_idx = find_cancel_record_index(redirect_epoch);
    if (record_idx < 0) begin
        `uvm_fatal("LSQ_CANCEL",
                   $sformatf("uid=%0d has no cancel record for redirect epoch=%0d",
                             uid, redirect_epoch))
    end
    status = get_status(uid);
    if (!active_cancel_record_id_valid ||
        cancel_record_q[record_idx].cancel_record_id != active_cancel_record_id) begin
        `uvm_fatal("LSQ_CANCEL",
                   $sformatf("uid=%0d redirect epoch=%0d does not own active cancel record",
                             uid, redirect_epoch))
    end
    if (!cancel_record_q[record_idx].redirect_anchor_valid ||
        cancel_record_q[record_idx].software_count_finalized) begin
        `uvm_fatal("LSQ_CANCEL",
                   $sformatf("uid=%0d cancel record epoch=%0d is not open for scan",
                             uid, redirect_epoch))
    end
    if (status.lsq_cancel_accounted_epoch == redirect_epoch) begin
        `uvm_fatal("LSQ_CANCEL",
                   $sformatf("uid=%0d counted twice for redirect epoch=%0d",
                             uid, redirect_epoch))
    end
    if (!status.active_lq_mapped && !status.active_sq_mapped) begin
        status.lsq_cancel_accounted_epoch = redirect_epoch;
        status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_CANCEL_ACCOUNTED;
        check_cancel_pending_aggregate();
        return;
    end
    main_tr = get_main_transaction(uid);
    if (main_tr.numLsElem != 1) begin
        `uvm_fatal("LSQ_CANCEL",
                   $sformatf("uid=%0d scalar cancel requires numLsElem=1, got %0d",
                             uid, main_tr.numLsElem))
    end
    begin
        if (status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE ||
            !status.lsq_reservation_sample_valid) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("uid=%0d mapped cancel lacks DUT-visible reservation state=%0d valid=%0d",
                                 uid, status.lsq_reservation_state,
                                 status.lsq_reservation_sample_valid))
        end
        if (status.lsq_reservation_sample_seq >
            cancel_record_q[record_idx].redirect_lsq_sample_seq) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("uid=%0d reservation sample=%0d is later than redirect LSQ cutoff=%0d",
                                 uid, status.lsq_reservation_sample_seq,
                                 cancel_record_q[record_idx].redirect_lsq_sample_seq))
        end
    end
    if (status.active_lq_mapped) begin
        cancel_record_q[record_idx].software_cancel_lq_count += main_tr.numLsElem;
        pending_lq_cancel_count += main_tr.numLsElem;
        if (cancel_record_q[record_idx].software_cancel_lq_count > MEMBLOCK_LQ_SIZE) begin
            `uvm_fatal("LSQ_CANCEL", "software LQ cancel count exceeds LQ capacity")
        end
    end
    if (status.active_sq_mapped) begin
        cancel_record_q[record_idx].software_cancel_sq_count += main_tr.numLsElem;
        pending_sq_cancel_count += main_tr.numLsElem;
        if (cancel_record_q[record_idx].software_cancel_sq_count > MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CANCEL", "software SQ cancel count exceeds SQ capacity")
        end
    end
    status.lsq_cancel_accounted_epoch = redirect_epoch;
    status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_CANCEL_ACCOUNTED;
    check_cancel_pending_aggregate();
endfunction:note_lsq_cancel_for_uid
```

中文伪代码：

1. 本函数在redirect active-window scan中为一个UID登记一次software cancel，必须在mapping被删除前调用。
2. 先按epoch找到record，并要求它就是当前active、已绑定anchor、尚未finalize且该UID没有重复记账。
3. UID没有LSQ mapping时只标记已核算并返回，software count保持零；有mapping时要求scalar `numLsElem=1`。
4. mapped UID必须拥有DUT-visible reservation，且sample不晚于该redirect的LSQ cutoff；否则无法证明DUT应计cancel，立即fatal。
5. 分别按active LQ/SQ mapping增加record count与pending aggregate，并检查容量；最后记录accounted epoch和状态。
6. `check_cancel_pending_aggregate()` 复算所有未apply record之和，只做一致性检查，不恢复资源。

### 6.3 Anchor 绑定和精确目标 sample

**抽象功能描述：** redirect monitor只发布DUT实际采样的输入投影，不能把它反灌成新的recovery event或第二次request redirect。

源码位置：`mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv`，`mon_data()` 的anchor分支。

```systemverilog
if (this.vif.rst_n === 1'b1 &&
    memblock_sync_pkg::reset_backend_done === 1'b1 &&
    io_redirect_valid === 1'b1) begin
    memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor;

    anchor.valid = 1'b1;
    anchor.level = io_redirect_bits_level;
    anchor.rob_flag = io_redirect_bits_robIdx_flag;
    anchor.rob_value = io_redirect_bits_robIdx_value;
    anchor.sample_seq = memblock_sync_pkg::get_dut_sample_seq($time);
    anchor.cycle = $time;
    memblock_sync_pkg::push_raw_redirect_anchor(anchor);
end
```

中文伪代码：

1. 本分支仅在reset完成且redirect valid被monitor真实采到时创建anchor事实。
2. 复制level与完整ROB key，取得该clocking sample的统一序号并保存时间。
3. `push_raw_redirect_anchor()` 只向有界timing queue入队；本monitor不调用recovery handler、不写active redirect，也不改变status。

**抽象功能描述：** `bind_redirect_anchors_to_cancel_records()` 按FIFO把实际anchor绑定到最早未锚定record，并从编译期延迟计算LSQ scan、DUT update、monitor compare和deadline序号。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
function void bind_redirect_anchors_to_cancel_records();
    while (redirect_anchor_history_q.size() != 0) begin
        int record_idx;
        memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor;

        record_idx = find_oldest_unanchored_cancel_record_index();
        if (record_idx < 0) begin
            `uvm_fatal("LSQ_CANCEL", "redirect anchor has no unanchored framework record")
        end
        anchor = redirect_anchor_history_q.pop_front();
        if (anchor.level != cancel_record_q[record_idx].redirect.level ||
            anchor.rob_flag != cancel_record_q[record_idx].redirect.rob_key.flag ||
            anchor.rob_value != cancel_record_q[record_idx].redirect.rob_key.value) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("redirect anchor FIFO mismatch record=%0d expected level/rob=%0d/%0d/%0d observed=%0d/%0d/%0d",
                                 cancel_record_q[record_idx].cancel_record_id,
                                 cancel_record_q[record_idx].redirect.level,
                                 cancel_record_q[record_idx].redirect.rob_key.flag,
                                 cancel_record_q[record_idx].redirect.rob_key.value,
                                 anchor.level, anchor.rob_flag, anchor.rob_value))
        end
        if (record_idx > 0 &&
            cancel_record_q[record_idx - 1].valid &&
            cancel_record_q[record_idx - 1].redirect_anchor_valid &&
            anchor.sample_seq <= cancel_record_q[record_idx - 1].redirect_sample_seq) begin
            `uvm_fatal("LSQ_CANCEL", "redirect anchor sample sequence does not preserve FIFO order")
        end
        cancel_record_q[record_idx].redirect_anchor_valid = 1'b1;
        cancel_record_q[record_idx].redirect_sample_seq = anchor.sample_seq;
        cancel_record_q[record_idx].redirect_lsq_sample_seq =
            anchor.sample_seq + MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY;
        cancel_record_q[record_idx].dut_cancel_update_sample_seq =
            anchor.sample_seq + MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY;
        cancel_record_q[record_idx].compare_snapshot_sample_seq =
            anchor.sample_seq + MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY;
        cancel_record_q[record_idx].deadline_sample_seq =
            cancel_record_q[record_idx].compare_snapshot_sample_seq + 1;
    end
endfunction:bind_redirect_anchors_to_cancel_records
```

中文伪代码：

1. 本函数把monitor事实和framework record建立一一时序关系，不扫描active UID或执行cancel。
2. 只要anchor队列非空，就取最早未锚定record；没有record、payload不一致或sample顺序倒退均fatal。
3. 绑定后保存anchor sample；按profile常量计算内部LSQ cutoff、DUT cancel更新拍、monitor可观察target拍和下一拍deadline。
4. 后续redirect scan读取LSQ cutoff，reconcile读取compare target；`dut_cancel_update_sample_seq`只作诊断，不直接消费snapshot。

### 6.4 `service_cancel_reconcile()`

**抽象功能描述：** 主service每tick唯一调用该函数，先绑定anchor，再按精确sample把software count与DUT held level比较；observed进度不写LSQ资源，完成record只在software apply与observed均完成后FIFO删除。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
function void service_cancel_reconcile();
    bind_redirect_anchors_to_cancel_records();
    foreach (cancel_record_q[idx]) begin
        if (cancel_record_q[idx].valid &&
            cancel_record_q[idx].redirect_drive_done_valid &&
            !cancel_record_q[idx].redirect_anchor_valid &&
            memblock_sync_pkg::get_dispatch_service_cycle() >
                cancel_record_q[idx].anchor_deadline_service_cycle) begin
            `uvm_fatal("LSQ_CANCEL_RECONCILE", "redirect record missed monitor anchor deadline")
        end
    end

    while (cancel_snapshot_history_q.size() != 0) begin
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        int record_idx;

        snapshot = cancel_snapshot_history_q[0];
        record_idx = find_oldest_observation_pending_record_index();
        if (record_idx < 0) begin
            if (cancel_record_q.size() != 0) begin
                break;
            end
            check_cancel_baseline_snapshot(snapshot);
            void'(cancel_snapshot_history_q.pop_front());
            continue;
        end
        if (snapshot.sample_seq <
            cancel_record_q[record_idx].compare_snapshot_sample_seq) begin
            check_cancel_baseline_snapshot(snapshot);
            void'(cancel_snapshot_history_q.pop_front());
            continue;
        end
        if (snapshot.sample_seq >
            cancel_record_q[record_idx].compare_snapshot_sample_seq) begin
            `uvm_fatal("LSQ_CANCEL_RECONCILE", "missing exact cancel target snapshot")
        end
        if (!cancel_record_q[record_idx].software_count_finalized) begin
            break;
        end
        if (snapshot.lq_cancel_count !=
                cancel_record_q[record_idx].software_cancel_lq_count ||
            snapshot.sq_cancel_count !=
                cancel_record_q[record_idx].software_cancel_sq_count) begin
            `uvm_fatal("LSQ_CANCEL_RECONCILE", "software/DUT cancel count mismatch")
        end
        cancel_record_q[record_idx].observed_cancel_lq_count = snapshot.lq_cancel_count;
        cancel_record_q[record_idx].observed_cancel_sq_count = snapshot.sq_cancel_count;
        cancel_record_q[record_idx].observed_valid = 1'b1;
        cancel_reconcile_match_count++;
        if (snapshot.lq_cancel_count != 0) begin
            cancel_reconcile_lq_nonzero_match_count++;
        end
        if (snapshot.sq_cancel_count != 0) begin
            cancel_reconcile_sq_nonzero_match_count++;
        end
        cancel_held_baseline_valid = 1'b1;
        cancel_held_lq_count = snapshot.lq_cancel_count;
        cancel_held_sq_count = snapshot.sq_cancel_count;
        void'(cancel_snapshot_history_q.pop_front());
    end

    begin
        int record_idx;

        record_idx = find_oldest_observation_pending_record_index();
        if (record_idx >= 0 &&
            cancel_record_q[record_idx].software_count_finalized &&
            latest_drained_cancel_sample_seq >
                cancel_record_q[record_idx].deadline_sample_seq) begin
            `uvm_fatal("LSQ_CANCEL_RECONCILE", "cancel compare deadline expired")
        end
    end
    cleanup_completed_cancel_records();
endfunction:service_cancel_reconcile
```

中文伪代码：

1. 本函数执行每个redirect epoch的一次性observed对账，不负责software resource rollback。
2. 先绑定所有已到anchor并检查drive完成后仍未锚定的record deadline。
3. 查看snapshot队首：如果没有已锚定待观察record但record FIFO非空，说明anchor尚未到，保留snapshot并停止；完全无record时才把它作为held baseline消费。
4. snapshot早于target时必须等于baseline并弹出；晚于target说明精确拍丢失，fatal；正好target但software scan尚未finalize时保留等待。
5. target就绪后比较LQ/SQ software count与DUT snapshot；不相等fatal，相等则写observed字段、match统计和新的held baseline，再弹出snapshot。
6. 最后检查目标deadline并调用cleanup；cleanup只删除 `software_applied && observed_valid` 的连续FIFO头，不调用 `cancel_lq/sq()`。

### 6.5 唯一 redirect scan 与一次性软件回退

**抽象功能描述：** `apply_redirect_flush_range()` 只在anchor、内部LSQ sample和ctrl snapshot drain都到位后扫描active admission窗口，登记cancel并清旧动态实例；它不消费DUT observed count。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
function void apply_redirect_flush_range(input memblock_redirect_payload_t redirect);
    memblock_uid_t begin_uid;
    memblock_uid_t end_uid;
    memblock_uid_t oldest_flushed_uid;
    bit found_flushed;
    int record_idx;

    if (!redirect.valid) begin
        `uvm_fatal("COMMON_DATA", "apply_redirect_flush_range requires valid redirect")
    end
    if (!active_cancel_record_id_valid) begin
        `uvm_fatal("LSQ_CANCEL", "redirect flush scan has no active cancel record")
    end
    record_idx = find_cancel_record_index_by_id(active_cancel_record_id);
    if (record_idx < 0 ||
        !redirect_payload_equal(redirect, cancel_record_q[record_idx].redirect) ||
        !cancel_record_q[record_idx].redirect_anchor_valid ||
        memblock_sync_pkg::peek_latest_dut_sample_seq() <
            cancel_record_q[record_idx].redirect_lsq_sample_seq ||
        latest_drained_cancel_sample_seq <
            cancel_record_q[record_idx].redirect_lsq_sample_seq) begin
        `uvm_fatal("LSQ_CANCEL", "redirect flush scan started before anchored LSQ sample boundary")
    end
    advance_terminal_done_uid();
    begin_uid = get_active_scan_begin_uid();
    end_uid = get_active_scan_end_uid();
    found_flushed = 1'b0;

    for (memblock_uid_t uid = begin_uid; uid < end_uid; uid++) begin
        status_transaction status;
        memblock_rob_key_t rob_key;

        status = get_status(uid);
        if (status.terminal_done || (!status.active && !status.writeback && !status.pass)) begin
            continue;
        end
        rob_key = status.get_rob_key();
        if (rob_order_util::rob_need_flush(rob_key, redirect)) begin
            if (!found_flushed || uid < oldest_flushed_uid) begin
                oldest_flushed_uid = uid;
                found_flushed = 1'b1;
            end
            prepare_uid_for_redirect_reissue(uid, redirect);
        end
    end
    if (found_flushed) begin
        rollback_max_enqueued_uid(oldest_flushed_uid);
    end
    if (cancel_record_q[record_idx].software_cancel_lq_count > MEMBLOCK_LQ_SIZE ||
        cancel_record_q[record_idx].software_cancel_sq_count > MEMBLOCK_SQ_SIZE) begin
        `uvm_fatal("LSQ_CANCEL", "finalized software cancel count exceeds LSQ capacity")
    end
    cancel_record_q[record_idx].active_scan_done = 1'b1;
    cancel_record_q[record_idx].software_count_finalized = 1'b1;
    cancel_record_q[record_idx].state_flush_applied_service_cycle =
        memblock_sync_pkg::get_dispatch_service_cycle();
    active_cancel_record_id_valid = 1'b0;
    active_cancel_record_id = 0;
    check_cancel_pending_aggregate();
endfunction:apply_redirect_flush_range
```

中文伪代码：

1. 本函数是redirect active-window状态清理与software cancel累计的唯一scan owner。
2. 先验证payload、active record、anchor、当前DUT sample和已drain ctrl sample均跨过内部LSQ cutoff；时序未到直接fatal。
3. 只扫描 `terminal_done_uid` 到 `max_enqueued_uid` 的active窗口；终态或从未active/无进度项跳过，ROB顺序命中的UID调用 `prepare_uid_for_redirect_reissue()`。
4. prepare helper先调用 `note_lsq_cancel_for_uid()`，再删除queue/map、清dispatch结果、递增dynamic epoch并标记flushed/reissue pending。
5. 如有命中，admission上界回退到最老flushed UID之前；随后检查count容量、finalize record并清active record ID。
6. 本函数不读取DUT observed count，也不调用 `lsq_ctrl.cancel_lq/sq()`。

**抽象功能描述：** LSQ enqueue sequence按record FIFO把finalized software count一次性应用到软件资源模型；observed count只用于对账，不会再次触发cancel。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv`，`apply_pending_lsq_cancels()`。

```systemverilog
function void memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels();
    ensure_helpers();
    foreach (data.cancel_record_q[idx]) begin
        int unsigned lq_count;
        int unsigned sq_count;
        int unsigned redirect_epoch;

        if (!data.cancel_record_q[idx].valid ||
            data.cancel_record_q[idx].software_applied) begin
            continue;
        end
        if (!data.cancel_record_q[idx].software_count_finalized) begin
            break;
        end
        lq_count = data.cancel_record_q[idx].software_cancel_lq_count;
        sq_count = data.cancel_record_q[idx].software_cancel_sq_count;
        redirect_epoch = data.cancel_record_q[idx].redirect_epoch;
        if (lq_count != 0) begin
            lsq_ctrl.cancel_lq(lq_count);
            if (data.pending_lq_cancel_count < lq_count) begin
                `uvm_fatal(get_type_name(), "pending LQ cancel aggregate underflow")
            end
            data.pending_lq_cancel_count -= lq_count;
        end
        if (sq_count != 0) begin
            lsq_ctrl.cancel_sq(sq_count);
            if (data.pending_sq_cancel_count < sq_count) begin
                `uvm_fatal(get_type_name(), "pending SQ cancel aggregate underflow")
            end
            data.pending_sq_cancel_count -= sq_count;
        end
        data.mark_cancel_record_applied(redirect_epoch);
    end
    data.check_cancel_pending_aggregate();
endfunction:apply_pending_lsq_cancels
```

中文伪代码：

1. 本函数是software cancel对LQ/SQ pointer和free count的唯一应用入口，不消费monitor snapshot。
2. 按record FIFO跳过invalid/已apply项；遇到首个未finalize项停止，不能越过epoch顺序。
3. 读取该record的software LQ/SQ count；非零时分别调用 `cancel_lq/cancel_sq()` 恢复资源，并从pending aggregate扣除，扣除前检查下溢。
4. 两侧应用后调用 `mark_cancel_record_applied()` 置独立software进度；末尾复算aggregate一致性。
5. observed mismatch或match都不会再次进入本函数，因此不会双重推进free count。

### 6.6 主 service 的唯一时序调度

**抽象功能描述：** `service_monitor_once()` 是timing sideband collector和reconcile的唯一scheduler；同tick可前后搬运两次事实，但只在redirect处理后执行一次语义对账。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`。

```systemverilog
task memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once();
    memblock_sync_pkg::tick_dispatch_service_cycle();
    collect_runtime_context_events();
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
    end
    monitor_adapter.drain_lsq_timing_sidebands();
    collect_monitor_event_batch();
    exception_redirect_replay_task();
    monitor_adapter.drain_lsq_timing_sidebands();
    monitor_adapter.service_lsq_timing_reconcile();
endtask:service_monitor_once
```

中文伪代码：

1. 本task是每个negedge service tick的统一monitor与recovery调度点。
2. 先增加service cycle并收集CSR/SFence等runtime context；确保adapter存在后第一次搬运cancel snapshot与redirect anchor。
3. 再收集semantic monitor batch并执行exception/redirect handler；该阶段可能刚完成active redirect scan和software count finalize。
4. 第二次搬运处理期间到达的timing事实，最后只调用一次reconcile。
5. `drain_lsq_timing_sidebands()` 只把package queue搬到data本地queue；`service_lsq_timing_reconcile()` 才比较record，不路由issue或请求global stop。

## 7. Global Stop Runtime Drain

### 7.1 `runtime_drain_complete()`

**抽象功能描述：** transaction terminal前缀完成后，该predicate统一检查所有运行期producer、queue、active map与recovery控制是否收敛；它只读取O(1) size/count/bit和compile-bound record，不扫描主表。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
function bit runtime_drain_complete();
    return memblock_sync_pkg::raw_monitor_queue_size() == 0 &&
           exception_event_q.size() == 0 &&
           load_issue_q.size() == 0 &&
           sta_issue_q.size() == 0 &&
           std_issue_q.size() == 0 &&
           uid_by_active_rob.num() == 0 &&
           uid_by_lq.num() == 0 &&
           uid_by_sq.num() == 0 &&
           !has_pending_redirect_drive() &&
           !flush_in_progress &&
           !active_redirect.valid &&
           redirect_phase == MEMBLOCK_REDIRECT_PHASE_IDLE &&
           !issue_freeze_ack &&
           !memblock_sync_pkg::dispatch_flush_in_progress &&
           ptw_wait_replay_q.size() == 0 &&
           !flushsb_request_pending() &&
           !cancel_reconcile_pending() &&
           !has_pending_lsq_cancel_apply() &&
           pending_lq_cancel_count == 0 &&
           pending_sq_cancel_count == 0 &&
           !redirect_sample_anchor_pending() &&
           !cancel_snapshot_buffer_pending() &&
           memblock_sync_pkg::lsq_timing_sideband_queue_size() == 0;
endfunction:runtime_drain_complete
```

中文伪代码：

1. 本函数判断主动flow能否安全发布global stop，不修改任何queue或状态。
2. 首先要求package raw（包含已转换但待重试的`deferred_raw_ctrl_q`）、exception event、三类issue queue
   全部为空，并要求ROB/LQ/SQ active map全部释放。
3. 再要求redirect drive/active/phase/freeze、PTW replay与flushSb生命周期均idle。
4. 最后要求cancel record、待software apply aggregate、本地anchor/snapshot和package timing queue全部清空。
5. 任一条件不满足返回0；全部收敛才返回1。函数不读取 `has_progress`，也不以busy level伪造状态推进。

### 7.2 `request_global_stop_if_done()`

**抽象功能描述：** 主sequence每tick只在连续terminal前缀覆盖全表且runtime drain完成时置global stop；它不负责强制停止子sequence或清理未收敛状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
function void request_global_stop_if_done();
    if (transaction_done() && runtime_drain_complete()) begin
        global_stop_requested = 1'b1;
    end
endfunction:request_global_stop_if_done
```

中文伪代码：

1. 本函数是 `global_stop_requested` 的正常完成写入口。
2. 先调用 `transaction_done()` 推进并检查连续 `terminal_done` 前缀；未覆盖主表时保持stop为0。
3. terminal完成后再调用 `runtime_drain_complete()`；任何异步事实、map或recovery状态未收敛时仍保持0。
4. 两个条件同时成立才置stop；各responder和agent sequence随后自行发送安全idle并退出。

## 8. Real Cancel Directed 场景

### 8.1 Directed main table

**抽象功能描述：** manual main sequence只构造一个older anchor load与两个younger load/store victim；它复用公共真实admission/issue/writeback/commit/deq flow，不直接写reservation、cancel或terminal状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_cancel_reconcile_sequence.sv`，`build_directed_mixed_main_table()`。

```systemverilog
task memblock_main_dispatch_cancel_reconcile_sequence::build_directed_mixed_main_table();
    main_control_transaction anchor_load;
    main_control_transaction victim_load;
    main_control_transaction victim_store;

    clear_manual_main_table();
    anchor_load = make_directed_transaction("cancel_anchor_load",
                                            MEMBLOCK_OP_CLASS_INT_LOAD,
                                            0,
                                            64'h0000_0000_8000_1000);
    anchor_load.delay = 0;
    set_manual_main_transaction(0, anchor_load);

    victim_load = make_directed_transaction("cancel_victim_load",
                                            MEMBLOCK_OP_CLASS_INT_LOAD,
                                            1,
                                            64'h0000_0000_8000_2000);
    victim_load.delay = 32;
    set_manual_main_transaction(1, victim_load);

    victim_store = make_directed_transaction("cancel_victim_store",
                                             MEMBLOCK_OP_CLASS_STORE,
                                             2,
                                             64'h0000_0000_8000_3000);
    victim_store.delay = 32;
    set_manual_main_transaction(2, victim_store);
    import_manual_main_table();
endtask:build_directed_mixed_main_table
```

中文伪代码：

1. 本task建立三UID deterministic main table，用于产生同时非零LQ/SQ cancel。
2. 先清manual表；UID0为立即可issue的older load，提供flush-after redirect anchor。
3. UID1 load与UID2 store使用较大issue delay，让它们完成真实LSQ sample后仍停留在可被redirect取消的窗口。
4. 最后调用 `import_manual_main_table()` 建立权威UID/status和ROB顺序；本task不直接activate、allocate、flush或terminal。

### 8.2 Redirect injection barrier

**抽象功能描述：** virtual sequence等待两个victim都拥有DUT-visible reservation且尚未issue/writeback/deq，随后通过公共request/drive queue注入覆盖两者的真实redirect；失败边界直接fatal，不退化成零cancel场景。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_cancel_reconcile_vseq.sv`，`drive_directed_redirect_when_ready()`。

```systemverilog
task memblock_dispatch_real_cancel_reconcile_vseq::drive_directed_redirect_when_ready();
    int unsigned wait_cycles;

    ensure_service_vif();
    wait_cycles = 0;
    forever begin
        status_transaction anchor_status;
        status_transaction victim_load_status;
        status_transaction victim_store_status;
        memblock_redirect_payload_t redirect;

        @(negedge service_vif.clk);
        if (service_vif.rst_n !== 1'b1 ||
            memblock_sync_pkg::reset_backend_done !== 1'b1) begin
            continue;
        end
        wait_cycles++;
        if (wait_cycles > 256) begin
            `uvm_fatal(get_type_name(),
                       "timeout waiting for uid1/uid2 DUT-visible LSQ reservations")
        end
        if (data.is_global_stop_requested()) begin
            `uvm_fatal(get_type_name(), "global stop arrived before directed redirect injection")
        end
        if (!data.main_table_ready) begin
            continue;
        end
        if (data.main_trans_num != 3) begin
            `uvm_fatal(get_type_name(), "cancel reconcile main table must contain 3 entries")
        end

        anchor_status = data.get_status(0);
        victim_load_status = data.get_status(1);
        victim_store_status = data.get_status(2);
        if (victim_load_status.load_dispatched ||
            victim_load_status.load_writeback || victim_load_status.writeback ||
            victim_load_status.lsq_deq || victim_load_status.terminal_done) begin
            `uvm_fatal(get_type_name(), "uid1 load victim progressed before redirect injection")
        end
        if (victim_store_status.sta_dispatched || victim_store_status.std_dispatched ||
            victim_store_status.sta_writeback || victim_store_status.std_writeback ||
            victim_store_status.writeback || victim_store_status.lsq_deq ||
            victim_store_status.terminal_done) begin
            `uvm_fatal(get_type_name(), "uid2 store victim progressed before redirect injection")
        end
        if (!(victim_load_status.active_lq_mapped &&
              victim_load_status.lsq_reservation_sample_valid &&
              victim_load_status.lsq_reservation_state == MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE &&
              victim_store_status.active_sq_mapped &&
              victim_store_status.lsq_reservation_sample_valid &&
              victim_store_status.lsq_reservation_state == MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE)) begin
            continue;
        end
        if (data.active_redirect.valid || data.has_pending_redirect_drive() ||
            data.flush_in_progress ||
            data.redirect_phase != MEMBLOCK_REDIRECT_PHASE_IDLE) begin
            `uvm_fatal(get_type_name(), "redirect state was not idle before directed injection")
        end

        redirect = '{default:'0};
        redirect.valid = 1'b1;
        redirect.flush_itself = 1'b0;
        redirect.level = 1'b0;
        redirect.rob_key = anchor_status.get_rob_key();
        if (!rob_order_util::rob_need_flush(victim_load_status.get_rob_key(), redirect) ||
            !rob_order_util::rob_need_flush(victim_store_status.get_rob_key(), redirect)) begin
            `uvm_fatal(get_type_name(), "directed redirect does not cover both younger victims")
        end
        data.request_redirect_flush(redirect);
        data.push_redirect_drive(redirect);
        redirect_injected = 1'b1;
        return;
    end
endtask:drive_directed_redirect_when_ready
```

中文伪代码：

1. 本task在真实DUT flow中选择确定性redirect注入时点，不直接修改victim status或cancel count。
2. 每个negedge等待reset/main table；超过256拍、提前global stop或表项数错误均fatal。
3. 读取三个status；victim一旦已经issue/writeback/deq/terminal就失败，避免把场景误报为非零cancel覆盖。
4. 只有load victim有active LQ mapping、store victim有active SQ mapping且两者reservation都为DUT-visible时继续。
5. 要求当前没有其它redirect；构造flush-after UID0的payload，并用ROB顺序helper证明UID1/UID2都会被覆盖。
6. 调用 `request_redirect_flush()` 创建framework record/freeze，再调用 `push_redirect_drive()` 让真实redirect responder驱接口；置场景标志后返回。

directed vseq还要求LQ非零match、SQ非零match和总match计数均非零；DCache、SBuffer和redirect responder在global stop且各自无inflight时先发送安全idle再自然退出。父类real-smoke在core flow返回后还必须等待后台responder fork返回，才允许清active。

## 9. IMPLEMENTATION_DELTA 完整清单

以下逐项覆盖关联Plan的“执行中补充/修正（IMPLEMENTATION_DELTA）”全部十项，未省略中间项：

| 序号 | Plan 中的 IMPLEMENTATION_DELTA | 当前实现 | Review 判断 |
|---:|---|---|---|
| 1 | 最后 normal commit batch 后的 `pendingPtr` watermark | `mark_rob_commit_batch()` 保存tail完整ROB key；modeled head无效且cursor到表尾时 `clear_lsqcommit_xaction()` 持续发布watermark | 已实现；只影响DUT ROB比较门槛，不写head类型、pointer、free count或terminal |
| 2 | active idle 与 configured idle路径分离 | `main_phase()` 的no-item/pre-gap/post-gap调用 `drive_active_idle()`；reset仍调用 `drive_idle(cfg.drv_mode)` | 已实现；level保持、pulse清零 |
| 3 | 统一runtime drain后再请求global stop | 新增 `runtime_drain_complete()`；`request_global_stop_if_done()` 与 `end_test_check()` 复用 | 已实现；最终当前工作区回归待跑 |
| 4 | 未锚定cancel record保留snapshot | `service_cancel_reconcile()` 在record FIFO非空但无anchored pending record时break，不弹队首snapshot | 已实现；exact target和deadline策略保留 |
| 5 | full ctrl raw的SQ capability最终分支 | full raw先联合LQ/SQ preflight；V2显式调用count-only SQ preflight，全部成功后才commit两侧 | 已实现；`MEMBLOCK_DUT_HAS_SQ_DEQ_PTR` 不参与count width |
| 6 | directed文件范围与实际实现对齐 | 当前工作区已经新增real cancel main sequence、vseq、cfg并注册package/filelist；计划已明确撤销笼统排除并限定文件范围 | 已通过计划中的IMPLEMENTATION_DELTA统一范围；不再作为实现与计划冲突 |
| 7 | real-smoke responder完成握手 | 父类real-smoke在core flow返回后执行`wait fork`，确认后台DCache/SBuffer/redirect responder返回后才清active；cancel子类继续使用自己的等待握手 | 已补齐收尾时序，避免responder错过最后stop sample |
| 8 | deferred ctrl resync队首保留 | handler/adapter返回success；本拍raw转入`deferred_raw_ctrl_q`，success才pop，队列计入runtime drain | 已实现；warning不再被当成成功，后续raw不越过失败队首 |
| 9 | software-only smoke复用singleton owner | normal/fault body均取得`lsq_commit_handler::get()`并reset私有runtime；helper fallback不再factory create | 已实现；公共status/map/LSQ pointer不由该reset清理 |
| 10 | CBO的scalar ROB store sideband分类显式化 | `is_scalar_rob_store_commit()`统一驱动`pendingst/scommit`；real cancel directed要求LOAD/STORE/CBO分类为0/1/1 | 已对齐V2 `CommitType.STORE && !vls`；只验证分类，不扩张为完整CBO flow支持 |

上述十项均已在当前源码或关联计划中找到直接实现证据；第六项的历史范围冲突已由计划中的
`IMPLEMENTATION_DELTA`明确收敛，不再列为未解决项。

## 10. 实现与 Plan 不一致项

### 10.1 Directed 文件范围已收敛

| 必填项 | Review 结论 |
|---|---|
| Plan 原有逻辑 | 正文要求真实 cancel directed main sequence、vseq、cfg，并在执行中补充条目明确了三个新增文件和两个注册文件的范围。 |
| 当前源码逻辑 | 当前工作区已新增并注册 `memblock_main_dispatch_cancel_reconcile_sequence`、`memblock_dispatch_real_cancel_reconcile_vseq` 和 `tc_dispatch_real_cancel_reconcile_smoke.cfg`。 |
| 处理结果 | 计划中的 `IMPLEMENTATION_DELTA` 已撤销笼统“不修改 directed 文件”的早期限制，明确以实际新增文件为本轮范围；源码与当前计划一致，本项不再是 blocker。 |

**抽象功能描述：** `start_core_dispatch_flow()` 并发启动真实LSQ enqueue、issue、commit、L2TLB、main service和directed redirect barrier，并在结束时检查cancel match覆盖；它证明directed flow不是只存在于建议文字中。

```systemverilog
task memblock_dispatch_real_cancel_reconcile_vseq::start_core_dispatch_flow();
    memblock_lsqenq_dispatch_base_sequence lsqenq_seq;
    memblock_issue_dispatch_base_sequence issue_seq;
    memblock_lsqcommit_dispatch_base_sequence lsqcommit_seq;
    memblock_l2tlb_base_sequence l2tlb_seq;
    memblock_main_dispatch_cancel_reconcile_sequence main_seq;

    fork
        `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
        `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
        `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
        `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
        `uvm_do_on(main_seq, p_sequencer)
        drive_directed_redirect_when_ready();
    join

    if (!redirect_injected) begin
        `uvm_fatal(get_type_name(), "directed redirect was not injected")
    end
    if (data.cancel_reconcile_match_count == 0 ||
        data.cancel_reconcile_lq_nonzero_match_count == 0 ||
        data.cancel_reconcile_sq_nonzero_match_count == 0) begin
        `uvm_fatal(get_type_name(), "cancel reconcile coverage missing")
    end
endtask:start_core_dispatch_flow
```

中文伪代码：

1. 本task实现Plan正文要求的真实DUT cancel directed flow，而不是software-only账本测试。
2. 用virtual sequencer并发启动五个业务sequence和redirect barrier；`join` 等待全部自然结束。
3. 结束后要求redirect确实注入，并要求总match、LQ非零match和SQ非零match都至少一次；缺任一项fatal。
4. 当前Plan已通过IMPLEMENTATION_DELTA把该源码和cfg纳入精确写范围，源码与计划一致。

### 10.2 Real-smoke 后台 responder 完成握手

**抽象功能描述：** `memblock_dispatch_real_smoke_vseq::body()` 先并发启动长期responder和有限core flow，
再等待后台fork自然返回；该task只维护场景active窗口，不改写responder自己的inflight或global-stop状态。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`，task：`body()`。

```systemverilog
fork : background_responder_fork
    start_background_responders();
join_none

start_core_dispatch_flow();
wait fork;

memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
```

中文伪代码：

1. 后台fork启动DCache、SBuffer和redirect responder；`join_none`允许core flow同时执行。
2. `start_core_dispatch_flow()`同步等待LSQ enqueue、issue、commit、L2TLB和main service完成并返回。
3. core flow返回后执行`wait fork`，等待当前body创建的后台responder task完成；该task内部的三个responder
   只有在global stop且无inflight后才自然返回。
4. 全部后台task返回后才清`dispatch_real_smoke_active`；本逻辑不使用`disable fork`，也不伪造responder完成。

修改前core flow返回后立即清active，后台responder可能尚未观察到最后stop sample；修改后场景完成包含明确
的后台完成握手，消除依赖进程调度先后的退出竞争。

### 10.3 CBO 的 scalar ROB store sideband 分类

| 必填项 | Review 结论 |
|---|---|
| Plan 原有逻辑 | `pendingst/scommit`只描述normal scalar store，但“scalar store”没有说明是否包含非vector STU CBO。 |
| 初版源码逻辑 | 直接读取`behavior.commit_is_store`，普通STU store和STU CBO都会置1；功能方向符合V2 ROB，但没有白名单或专项可观察检查。 |
| 修改原因 | V2 `Rob.scala`按`commitType == STORE && !vls`生成`scommit`，并按head `commitType == STORE`生成`pendingst`；CBO解码为非vector STU，不能按`kind==STORE`排除。 |
| 当前源码逻辑 | 公共`is_scalar_rob_store_commit()`只接受`commit_is_store=1`且kind为STORE/CBO；`pendingst/scommit`统一调用；real cancel directed检查LOAD/STORE/CBO为0/1/1。 |
| 处理结果 | 已作为Plan第10项`IMPLEMENTATION_DELTA`登记；不改变CBO默认关闭边界，也不宣称完整CBO flow通过。 |

```systemverilog
static function bit is_scalar_rob_store_commit(memblock_op_behavior_t behavior);
    return behavior.commit_is_store &&
           !behavior.is_atomic &&
           (behavior.kind == MEMBLOCK_OP_BEHAVIOR_STORE ||
            behavior.kind == MEMBLOCK_OP_BEHAVIOR_CBO);
endfunction
```

中文伪代码：

1. LOAD behavior必须分类为0，普通STORE和CBO behavior必须分类为1；其它结果fatal。
2. `build_lsqcommit_xaction()`只用该helper派生head `pendingst`并累计normal batch `scommit`，避免两处规则漂移。
3. directed CBO probe只构造transaction并派生behavior，不进入main table、不激活UID、不驱DUT、不写status。
4. 本检查覆盖ROB sideband分类，不覆盖CBO enqueue、issue、writeback、DCache response、commit/deq闭环。

除上述已登记的执行中补充外，未发现当前LSQ status核心行为与Plan最终逻辑方案不一致。最后一轮独立
review已核对当前工作区、Plan、flow和最终日志并给出`FINAL PASS`，Plan已归档到`plan/do`。

## 11. Plan 未说明但 Coding 落实的细节

### 11.1 LSQ commit/deq handler singleton 防止双owner

| 必填项 | Review 结论 |
|---|---|
| 细节功能 | 为有状态 `lsq_commit_handler` 增加 `get()`，base sequence和adapter fallback共享同一cursor/head/fault token。 |
| 为什么Plan未覆盖 | Plan要求“唯一owner”，但主体章节没有完整规定factory fallback必须如何保证同一实例。 |
| 在本特性中的作用 | 避免monitor adapter和lsqcommit sequence各自维护独立modeled head，导致deq后fault rebase或watermark分叉。 |
| 是否需要回写Plan | 建议终审时在owner合同中保留singleton要求；当前Plan的Coding差异表已部分提到，但正文可进一步明确。 |

**抽象功能描述：** `lsq_commit_handler::get()` 返回全环境共享的有状态handler；只在首次访问时通过UVM factory创建一次，不推进任何运行期状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`。

```systemverilog
static function lsq_commit_handler get();
    if (m_inst == null) begin
        m_inst = lsq_commit_handler::type_id::create("lsq_commit_handler_singleton");
    end
    return m_inst;
endfunction:get
```

中文伪代码：

1. 本函数只负责LSQ commit/deq生命周期owner的实例唯一性。
2. 首次调用时通过UVM factory创建handler并保存静态句柄；后续调用直接返回同一句柄。
3. adapter fallback、dispatch base和lsqcommit sequence因此共享cursor、modeled head、fault token和watermark；函数本身不reset或推进这些状态。

#### 11.1.1 Software-only normal/fault smoke 的场景 reset

**抽象功能描述：** 两个software-only场景在各自body开始时复用真实flow的singleton，并只清handler私有
cursor/head状态；它们随后仍通过公开commit/deq API推进公共状态，不建立测试专用owner。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv`，
`body()`。

```systemverilog
commit_handler = lsq_commit_handler::get();
commit_handler.bind_lsq_ctrl(lsq_ctrl);
commit_handler.reset_lsqcommit_runtime_state();
build_directed_main_table();
```

中文伪代码：

1. normal smoke先取得全局唯一commit handler，并绑定由base sequence初始化的公共LSQ模型。
2. 调用公开reset只清singleton私有commit cursor、modeled head、watermark和fault token，不清status、active map
   或LQ/SQ pointer。
3. reset返回后才建立本场景main/status并执行admission、issue、writeback、commit和deq，避免前一场景私有
   游标残留影响当前单测。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv`，
`body()`。

```systemverilog
commit_handler = lsq_commit_handler::get();
commit_handler.bind_lsq_ctrl(lsq_ctrl);
commit_handler.reset_lsqcommit_runtime_state();
build_directed_main_table();
```

中文伪代码：

1. fault smoke重写了父类body，因此在自己的入口执行同一singleton bind/reset合同。
2. reset范围与normal smoke相同，只清handler私有生命周期，不把fault场景需要的公共状态直接写成终态。
3. 后续fault token、LQ deq、normal younger commit和SQ deq均由该singleton公开API推进；
   `commit_and_deq_lsq()`句柄为空时也只调用`get()`，不再factory create私有实例。

### 11.2 Pending aggregate drift自检

| 必填项 | Review 结论 |
|---|---|
| 细节功能 | `check_cancel_pending_aggregate()` 每次note/apply/cleanup后复算所有未apply record的software count。 |
| 为什么Plan未覆盖 | Plan描述了per-record与aggregate职责，但没有逐个列出每次更新后的防漂移assertion。 |
| 在本特性中的作用 | 防止record count、pending aggregate和一次性resource rollback三者因漏减或重复减失配。 |
| 是否需要回写Plan | 可只保留在implementation review；属于防御性一致性检查，不改变主flow。 |

**抽象功能描述：** 该helper只复算并比较cancel aggregate，不修改record、pointer、free count或observed状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，`check_cancel_pending_aggregate()`。

```systemverilog
function void check_cancel_pending_aggregate();
    int unsigned expected_lq;
    int unsigned expected_sq;

    expected_lq = 0;
    expected_sq = 0;
    foreach (cancel_record_q[idx]) begin
        if (cancel_record_q[idx].valid &&
            !cancel_record_q[idx].software_applied) begin
            expected_lq += cancel_record_q[idx].software_cancel_lq_count;
            expected_sq += cancel_record_q[idx].software_cancel_sq_count;
        end
    end
    if (pending_lq_cancel_count != expected_lq ||
        pending_sq_cancel_count != expected_sq) begin
        `uvm_fatal("LSQ_CANCEL", "pending cancel aggregate drift")
    end
endfunction:check_cancel_pending_aggregate
```

中文伪代码：

1. 本函数验证pending aggregate仍等于所有尚未software apply的record count之和。
2. 从零开始遍历有界record FIFO，只累计valid且未apply项的LQ/SQ software count。
3. 复算值与保存的pending aggregate任一不同就fatal；相等时直接返回。
4. 本helper不调用 `cancel_lq/sq()`、不弹record，也不改变DUT observed进度。

未发现其它需要单列的Plan外功能逻辑；MMIO provenance、canonical tag和adapter staging属于独立pending-MMIO plan，不计入本章。

## 12. 验证结果与待验证项

### 12.1 当前已有事实

| 检查 | 当前事实 | 能证明的范围 |
|---|---|---|
| `git diff --check` | 2026-07-23源码修复后执行通过；文档终审前再次执行 | 当前目标diff无空白错误 |
| 最终VCS/KDB compile | `v2_lsq_mmio_cbo_final_20260723/log/vcs_compile_rtl.log`；最终 KDB 摘要为`0 error(s), 0 warning(s)`，完整 transcript 另有一条工具自身的`LCA_FEATURES_ENABLED` usage warning | success返回值、持久FIFO API和singleton smoke修改均通过真实package/filelist编译；不把工具usage warning误记为零warning transcript |
| 默认real smoke | `v2_lsq_mmio_cbo_final_20260723/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=666666_rtl_.log`，`TEST_PASS`、error/fatal均0 | normal load的head、commit、LQ deq、runtime drain和responder退出闭环 |
| real cancel reconcile | `v2_lsq_mmio_cbo_final_20260723/log/tc=basicTest_ts=memblock_dispatch_real_cancel_reconcile_vseq_cfg=tc_dispatch_real_cancel_reconcile_smoke_seed=666666_rtl_.log`，`TEST_PASS`、error/fatal均0 | 真实redirect、LQ/SQ非零cancel match、reissue、normal commit、LQ/SQ deq和自然退出闭环 |
| pending-MMIO directed | `v2_lsq_mmio_cbo_final_20260723/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=666666_rtl_.log`，`TEST_PASS`、error/fatal均0 | 相邻tag/provenance、fault head、owner reset与global-stop raw drain未被破坏；caught fatal=1是精确预期场景 |
| legacy normal soft testcase | 场景body启动前被既有int-WB monitor `STD0 valid is X/Z`终止 | 不能作为singleton smoke通过或失败证据；该monitor配置问题不由本plan修改 |

### 12.2 剩余验证边界

- 本轮没有新增“先制造resync mismatch、后补齐owner并观察同一raw重试成功”的专用testcase；该路径已由
  显式success返回、持久queue head和runtime-size静态闭环，strict/default真实回归通过。后续压力专项可补
  瞬态resync directed，但不阻塞V2默认严格模式验收。
- `tc_sanity + default.cfg`不会建立本plan的main table，运行只会等待，不是本plan有效回归组合；已终止且
  不作为通过或失败证据。有效真实场景统一使用`basicTest + 目标vseq + 对应cfg`。
- legacy software-only testcase的输出monitor X/Z配置是既有测试入口问题；本轮只修其sequence owner逻辑，
  不扩张为testcase/env cfg重构。

最终工作区的编译、默认real smoke、real cancel和pending-MMIO相邻专项均已通过；剩余项是明确边界，不是
当前LSQ MMIO/status功能blocker。

## 13. 源码覆盖与非本次修改的逻辑分析

### 13.1 本次review覆盖的源码文件

| 类别 | 文件 | 覆盖内容 |
|---|---|---|
| 编译结构 | `mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh` | Ensbuffer、deq/cancel宽度、cancel时序和队列深度 |
| 公共类型/同步 | `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`、`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv` | reservation/cancel record、raw/snapshot/anchor、sample sequence与queue |
| 状态/模型 | `mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv`、`lsq_ctrl_model.sv` | reservation字段、真实deq/cancel资源写者边界 |
| 核心owner | `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`、`common_data_transaction.sv` | modeled head、commit/fault、deq、cancel reconcile、runtime drain |
| adapter/recovery | `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`、`exception_redirect_replay_handler.sv`、`memblock_dispatch_base_sequence.sv` | singleton owner、timing drain、redirect scan readiness |
| ctrl/redirect agent | `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_interface.sv`、`io_mem_to_ooo_ctrl_agent_agent_xaction.sv`、`io_mem_to_ooo_ctrl_agent_agent_monitor.sv`、`mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv` | 参数化字段/XZ/约束、snapshot和anchor producer |
| LSQ驱动 | `mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv`、`mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv`、`memblock_lsqenq_dispatch_base_sequence.sv` | active idle、head transaction、reservation sample与cancel apply |
| 主service | `mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv` | timing collector/reconcile单点调度和stop请求 |
| directed与退出 | `mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_cancel_reconcile_sequence.sv`、`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_cancel_reconcile_vseq.sv`、`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`、`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`、`mem_ut/ver/ut/memblock/seq/base_seq/memblock_redirect_dispatch_base_sequence.sv` | real cancel场景、`uvm_do_on`启动和responder自然退出 |
| 注册/配置/软测 | `mem_ut/ver/ut/memblock/seq/seq_pkg.sv`、`seq/seq.f`、`seq/plus_cfg/tc_dispatch_real_cancel_reconcile_smoke.cfg`、两个dispatch soft smoke | include顺序、directed preset和normal/fault API更新 |
| 参数检查 | `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv` | 派生宽度和cancel observe latency一致性检查 |

### 13.2 `git status --short` 中非本次逻辑

本review生成时工作区存在多名worker并行改动。以下内容不纳入本LSQ status功能正确性结论：

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| 项目规则与入口 | `AGENTS.md`、`AI_DOC/project_management/*.md` | 非本次源码逻辑 | 文档规则由主agent/其他worker维护，本任务禁止修改 |
| Flow/Plan文档 | `AI_DOC/mem_ut_flow_doc/*.md`、两个execution plan | 另行同步/review | 属于plan执行文档同步；本任务只创建implementation review |
| Review归档搬迁 | `review_doc/undo`删除与`review_doc/do`新增 | 非本次逻辑 | 其他已完成子plan的文档归档，不得回滚 |
| pending-MMIO专项 | ctrl agent中的MMIO accessor/raw字段、`status_transaction`的MMIO tag/provenance、`common_data_transaction` resolver、adapter MMIO staging、pending-MMIO soft sequence/vseq、`memblock_op_behavior_util.sv` | 独立review | 由pending-MMIO plan及provenance worker拥有；共享文件中只review本plan的deq/cancel/head部分 |
| 其它V2适配 | int-WB、IQ feedback、CSR或其它agent相关已提交/并行修改 | 独立子plan | 不属于LSQ MMIO/status owner边界 |
| 仿真产物 | `mem_ut/ver/ut/memblock/sim/**` 日志/编译目录 | 验证证据，不是源码修改 | 只按第12章引用日志事实，不纳入git源码diff |

当前未发现无法归类的工作区修改。终审仍需重新执行 `git status --short`，因为provenance worker可能在本review创建后继续更新共享源码。

## 14. Plan 对齐检查与 Review 结论

### 14.1 Plan 对齐结论

- 问题一至六的核心状态逻辑、编译期宽度、driver level保持、cancel对账与runtime drain均已找到实现落点。
- 九项 `IMPLEMENTATION_DELTA` 已完整逐项核对。
- directed文件范围已由计划中的IMPLEMENTATION_DELTA明确收敛；父类real-smoke收尾等待也已补入计划和源码，当前没有未登记的范围冲突。
- pending-MMIO provenance变化属于相邻plan；修复后directed已经与本plan共用最终编译产物复验通过。
- 最后一轮独立review已核对当前工作区、Plan、flow和最终日志，未发现blocker；Plan已归档到`plan/do`。

### 14.2 最终结论

**最后一轮独立终审：`FINAL PASS`。**

当前源码、Plan和flow已覆盖modeled head、normal/fault、V2 count-only deq、cancel reconcile、active idle、
runtime drain、deferred raw success/FIFO语义和singleton soft owner。最终VCS compile、real cancel、
pending-MMIO和default real smoke均已通过；未发现仍需coding的功能blocker。独立review已重新核对当前
工作区、源码、文档和最终日志并给出`FINAL PASS`，Plan已移动到`plan/do`并进入专项提交。
