# MemBlock V2 LSQ Admission Flow

本文描述当前 `mem_ut` 测试框架把公共主表中的 scalar load/store 驱动到 V2 LSQ enqueue 接口，建立
软件 LQ/SQ allocation，并在 DUT sample 边界后开放 issue route 的真实调用链。

权威源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

## 1. Flow 边界

### 1.1 术语与抽象功能说明

| 英文术语 | 当前 flow 中的中文含义 | 代码对象/状态落点 | 示例 |
|---|---|---|---|
| `candidate` | 本拍按 uid 顺序预览、尚未修改公共状态的 LSQ 入队候选 | `collect_lsq_candidates()` 的五组局部 queue | load/store 数量或 free count 达到上限时停止收集 |
| `launch` | driver 已在当前 clocking 边界把 active request 写到 VIF | `memblock_dispatch_request_launched` | launch 前遇到 redirect 时只发 idle，不建立 allocation |
| `reservation token` | 一次真实 launch 对应的动态实例凭证，绑定 `uid` 和单调 `launch_epoch` | `memblock_lsq_reservation_token_t`、`status.lsq_reservation_launch_epoch` | 同一 uid redirect 后重新入队会得到新的 epoch |
| `DUT sample sequence` | 多个 monitor/sequence 对同一仿真采样时刻共享的单调序号 | `memblock_sync_pkg::get_dut_sample_seq($time)`、`status.lsq_reservation_sample_seq` | 下一 driver 边界把上一批 token 标成 `DUT_VISIBLE` |
| `pending sample` | 已 launch、已分配软件 LQ/SQ，但尚未由下一边界记录 DUT 可见性的单深度 batch | `pending_sample_valid`、`pending_sample_tokens` | C0 launch batch A，C1 才完成 A 的 sample |
| `cancel record` | 单个 redirect epoch 的软件回退量和 DUT cancel 观察值对账记录 | `common_data_transaction::cancel_record_q` | software count 只回退一次，observed count 只核对一次 |
| `owner` | 对某类状态拥有唯一修改权的对象 | allocation owner 为 `lsq_ctrl_model::commit_allocate()`；cancel owner 为逐 epoch record 的软件应用路径 | DUT observed cancel 不拥有 pointer/free count 回退权 |

LSQ admission 只负责以下行为：

1. 从 `main_table_by_uid` 的 next-admit uid开始顺序选择 candidate。
2. 为 scalar load/store预测 LQ/SQ key并构造 V2 enqueue request。
3. driver launch 成功后立即预留软件 LSQ 资源。
4. 经过下一 driver clocking 边界后设置 `issue_ready`，进入 LOAD/STA/STD issue queue。
5. redirect 时由逐 epoch `cancel_record_q` 记录软件 cancel，LSQ enqueue sequence 每个 record 只回退一次 reservation。

本 flow 不修改主表生成、issue fire、writeback、ROB commit、LSQ deq、pass/fail 或 terminal owner。
本轮不支持 vector LS/`issueVldu`、multi-element chunk、enqueue 前 directed exception、issue hold、压力模式
和 boundary vseq。

V2 顶层只有 enqueue request input，没有 LSQ enqueue `canAccept/response`。因此当前 flow 不调用
`wait_lsq_can_accept()`、`sample_lsqenq_resp()` 或 `commit_allocate_with_resp()`。

## 2. 函数调用 Flow 图

```mermaid
flowchart TD
    A[memblock_lsqenq_dispatch_base_sequence::body] --> B[configure_from_plus]
    B --> C[ensure_helpers]
    C --> D[wait_for_main_table]
    D --> E[drive_lsqenq_loop]
    E --> F{global_stop?}
    F -->|yes, pending sample| G[send_idle_lsqenq_boundary]
    G --> H[complete_v2_pending_sample]
    H --> Z[退出 LSQ enqueue loop]
    F -->|yes, no pending| Z
    F -->|no| I[send_lsqenq_cycle]
    I --> J[apply_pending_lsq_cancels]
    J --> K{pending sample 且下一 uid 为 non-LSQ?}
    K -->|yes| L[send idle boundary]
    L --> M[complete previous sample]
    M --> N[admit_non_lsq_if_ready]
    K -->|no| O[admit_non_lsq_if_ready]
    O -->|已处理| E
    O -->|不是 non-LSQ| P[collect_lsq_candidates]
    P --> Q{candidate 非空?}
    Q -->|no| R[发送全零 idle item]
    Q -->|yes| S[clear_lsqenq_xaction]
    S --> T[assign_lsqenq_slot / set_req_fields]
    T --> U[start_item / finish_item]
    R --> U
    U --> V[driver clock-first launch]
    V --> W[complete_v2_pending_sample: 完成上一批]
    W --> X{当前 item 已 launch?}
    X -->|yes| Y[confirm_lsq_candidates: 预留当前批]
    X -->|redirect abort/idle| E
    Y --> E
```

### 函数调用 Flow 图整体文字伪代码

```text
1. 初始化阶段：
   body 初始化 seq_csr_common，读取 enable/no-progress 配置，取得公共 data、LSQ 软件镜像和 issue scheduler；
   sequence 关闭时直接返回，开启时等待主表 ready。

2. 每拍入口：
   drive_lsqenq_loop 先检查 global stop；若最后一批仍 pending，先发送 idle item提供真实 sample 边界；
   正常拍调用 send_lsqenq_cycle，并根据 launch/sample/non-LSQ 是否推进维护 no-progress warning计数。

3. redirect cancel：
   send_lsqenq_cycle 首先按 FIFO 扫描已完成 software count 的 `cancel_record_q`；
   每个 record 的 software count 只用于一次 pointer/free-count 回退并置 `software_applied`；
   DUT observed cancel 只在 reconcile 路径核对，不会再次调用 `cancel_lq/cancel_sq`。

4. non-LSQ 边界：
   若上一 LSQ batch pending而下一 uid 为 non-LSQ，先发送全零 idle并完成上一批 sample；
   随后沿用原 non-LSQ admission，不让零时间路径伪装成 driver sample边界。

5. LSQ candidate：
   collect_lsq_candidates 每拍采样一次总slot上限；随机返回0时不读取uid、pointer、free count、主表、
   状态表或map，并生成idle item；此前仍允许读取global-flush gate；
   非零时只预览连续 uid，分别限制 load element不超过6、store element不超过4，并受实际free count限制。

6. launch 与 reservation token：
   driver 在 clocking 边界先让 DUT采样上一拍，再取得当前item并检查flush epoch；
   合法active item只发送一次并立即item_done；sequence先完成上一批sample，再为当前launch批建立 allocation；
   每个已 launch uid 调用 `begin_lsq_reservation_launch()` 生成 `{uid, launch_epoch}` token。

7. sample 与 issue route：
   pending batch在下一driver边界统一取得 DUT sample sequence，并先把所有 token 标为 `DUT_VISIBLE`；
   只有保存的flush epoch仍有效时才调用complete_admission，设置issue_ready并写入LOAD/STA/STD issue queue；
   redirect覆盖的已 launch实例保留“确实被DUT采样”的事实，但不开放旧实例issue，后续由cancel record回退资源。
```

## 3. 关键状态和 owner

| 状态/对象 | 含义 | 写者 | 清理/后续读者 |
|---|---|---|---|
| `dispatch_progress.max_enqueued_uid` | 连续完成 admission 的高水位 | `set_status_field(ENQ)` 间接推进 | redirect rollback、next-admit 查询 |
| `status.active/enq` | uid 已建立当前动态实例和 admission reservation | `lsq_ctrl_model::commit_allocate()` | redirect/reissue、commit/terminal flow |
| LQ/SQ active map | DUT key 到 uid 的运行期映射 | `common_data_transaction::activate_uid()` | monitor event反查、deq、redirect cancel |
| `lq/sq_enq_ptr`、free count | 软件 LSQ allocation 镜像 | `commit_allocate()` | candidate preview、deq release、redirect cancel |
| `pending_sample_tokens` | 当前已 launch/已预留、尚未经过下一 sample 边界的一批 `{uid, launch_epoch}` | `confirm_lsq_candidates()` | `complete_v2_pending_sample()` |
| `status.lsq_reservation_state` | 当前动态实例处于未建 token、等待 sample、DUT 可见或已计入 cancel | `begin_lsq_reservation_launch()`、`mark_lsq_reservation_sampled()`、`note_lsq_cancel_for_uid()` | redirect scan 只统计 `DUT_VISIBLE` mapping |
| `status.lsq_reservation_sample_seq` | 该动态实例真实跨过的 DUT sample 序号 | `mark_lsq_reservation_sampled()` | 与 redirect 的 LSQ cutoff 比较 |
| `status.issue_ready` 和 issue queue | uid 可以进入后续 issue flow | `prepare_issue_route_for_uid()` | issue sequence/scheduler |
| `cancel_record_q` | 每个 redirect epoch 独立的软件 cancel、资源回退和 DUT observed 对账生命周期 | `request_redirect_flush()` 创建，redirect scan 完成 software count | `apply_pending_lsq_cancels()` 只应用 software count；reconcile 只比较 observed count |

`pending_sample_*` 是 sequence 局部过程态，不是第二套 active map。token 记录真实 launch/sample 事实，
但不拥有 pointer/free count 回退；资源回退权只属于对应 redirect epoch 的 software cancel 应用。

## 4. Sequence 初始化与主循环

### 4.1 `body()`

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，task：`body()`。

抽象功能描述：该 task 是 LSQ admission入口，负责配置、依赖获取和主表同步；它不拥有candidate分配、
driver launch或cancel回退。

```systemverilog
seq_csr_common::init();
configure_from_plus();
if (!enable) begin
    return;
end
ensure_helpers();
wait_for_main_table();
drive_lsqenq_loop();
```

中文伪代码：

```text
初始化公共参数快照；
读取LSQ sequence enable和公共no-progress warning阈值；
若关闭则保持driver安全idle并返回，不回退到随机父类sequence；
取得公共状态、LSQ软件镜像、issue scheduler和monitor adapter；
等待主表ready后进入常驻admission循环。
```

### 4.2 `drive_lsqenq_loop()`

源码位置：同上，task：`drive_lsqenq_loop()`。

抽象功能描述：该 task 按拍调用 admission，并确保 global stop前的最后一个 pending batch经过sample
边界；no-progress计数只用于诊断，不是正常退出条件。

```systemverilog
if (data.is_global_stop_requested()) begin
    if (pending_sample_valid) begin
        send_idle_lsqenq_boundary(cycle_idx, "global_stop trailing sample", has_progress);
    end
    break;
end
send_lsqenq_cycle(cycle_idx, has_progress);
```

中文伪代码：

```text
每轮先检查公共global stop；
若停止且仍有pending batch，发送全零idle item，让driver经过一个真实clocking边界并结算上一批；
pending清空后退出；
正常轮调用send_lsqenq_cycle；
有launch/sample/non-LSQ推进时清no-progress计数，否则累计并按阈值warning，但warning不是正常退出条件。
```

## 5. 每拍 admission 调度

### 5.1 `send_lsqenq_cycle()`

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，task：`send_lsqenq_cycle()`。

抽象功能描述：该 task 是每拍 LSQ admission 调度入口，先应用已经定案的逐 epoch software cancel，
再处理 non-LSQ 或 LSQ candidate；driver 返回后结算上一批 sample，并为当前真实 launch 建立新 token。

```systemverilog
apply_pending_lsq_cancels();
if (!collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)) begin
    send_idle_lsqenq_boundary(cycle_idx, "no LSQ candidate", has_progress);
    return;
end
start_item(tr);
finish_item(tr);
complete_v2_pending_sample(has_progress);
confirm_lsq_candidates(tr, uids, trs, behaviors, lq_keys, sq_keys, has_progress);
```

中文伪代码：

```text
先按 `cancel_record_q` FIFO 应用已定案且尚未回退的 software count，使新candidate看到正确pointer/free count；
若pending batch后面紧接non-LSQ，先发送idle完成pending sample，再执行原non-LSQ admission并返回；
否则先尝试直接处理non-LSQ；成功时不创建LSQ item；
调用collect_lsq_candidates只读预览本拍连续scalar LS；
没有candidate时发送全零idle，该边界仍可能完成上一批sample；
有candidate时清空xaction并逐slot构造request；
finish_item返回后先结算上一批pending，再按driver返回的launch结果预留当前批；
has_progress只做OR汇总，sample进展不会被当前launch结果覆盖。
```

### 5.2 `apply_pending_lsq_cancels()`

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，function：`apply_pending_lsq_cancels()`。

抽象功能描述：该函数是软件 LSQ pointer/free-count 的唯一 redirect 回退入口。它按 redirect record
顺序消费已完成 active scan 的 software count，并把该 record 标成已应用；DUT observed count 不经过
本函数，也不能触发第二次回退。

```systemverilog
foreach (data.cancel_record_q[idx]) begin
    if (!data.cancel_record_q[idx].valid ||
        data.cancel_record_q[idx].software_applied) continue;
    if (!data.cancel_record_q[idx].software_count_finalized) break;
    lq_count = data.cancel_record_q[idx].software_cancel_lq_count;
    sq_count = data.cancel_record_q[idx].software_cancel_sq_count;
    if (lq_count != 0) lsq_ctrl.cancel_lq(lq_count);
    if (sq_count != 0) lsq_ctrl.cancel_sq(sq_count);
    data.mark_cancel_record_applied(data.cancel_record_q[idx].redirect_epoch);
end
data.check_cancel_pending_aggregate();
```

中文伪代码：

```text
按 FIFO 遍历 cancel_record_q；
无效或已 software_applied 的 record 跳过；遇到最老的未 finalize record立即停止，不能越过epoch乱序回退；
读取该 record 的 software_cancel_lq_count/software_cancel_sq_count；
非零 LQ count 调用 cancel_lq，非零 SQ count 调用 cancel_sq，各自只恢复一次软件pointer/free count；
同步扣减仅用于一致性校验的公共镜像计数，发现下溢立即fatal；
调用 mark_cancel_record_applied，把当前epoch标为software_applied；
最后调用 check_cancel_pending_aggregate，确认镜像值与尚未应用record之和一致；
DUT observed count由每个dispatch service tick唯一入口service_lsq_timing_reconcile调用
service_cancel_reconcile比较，绝不回到本函数再次释放资源。
```

## 6. Candidate 生成

### 6.1 `collect_lsq_candidates()`

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，function：`collect_lsq_candidates()`。

抽象功能描述：该函数处于每拍高频路径，从next-admit uid开始只预览本拍连续候选并返回局部key；
范围上限为编译期6个slot，不修改公共pointer、free count、map或状态表。

```systemverilog
max_enq = seq_csr_common::get_enq_per_cycle();
if (max_enq == 0) begin
    return 1'b0;
end
while (uids.size() < max_enq) begin
    tentative_load = load_elem_count + (behavior.uses_lq ? behavior.num_ls_elem : 0);
    tentative_store = store_elem_count + (behavior.uses_sq ? behavior.num_ls_elem : 0);
    if (tentative_load > MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH ||
        tentative_store > MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH ||
        tentative_load > lq_free_tmp || tentative_store > sq_free_tmp) begin
        break;
    end
    // 保存candidate并只推进局部pointer。
end
```

中文伪代码：

```text
清空所有输出queue，保证无上一拍残留；
flush阻塞时在读取next uid和LSQ资源前返回；
本拍只采样一次get_enq_per_cycle；返回0时不读取或修改uid/pointer/free count；
复制真实软件pointer/free count到局部变量；
从next-admit uid开始按顺序预览，遇主表末尾、已有状态或non-LSQ立即停止，不跳过老uid；
derive_op_behavior推导LQ/SQ占用；当前scalar路径要求num_ls_elem=1，否则fatal；
分别累计load和store element，超过编译期6/4或实际free count时截断本批；
合法candidate保存预测key并只推进局部pointer；真实状态等driver launch后由commit_allocate更新。
```

该实现不要求 `base_lq_free>=6` 或 `base_sq_free>=4`，也不执行 `tentative+6/4 reserve`。6/4只表示
本拍端口上限，不能让软件模型永久保留队尾空项。

## 7. Request 字段构造

### 7.1 `set_req_fields()`

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`，function：`set_req_fields()`。

抽象功能描述：该函数是单个slot request qualifier/payload的唯一写者，把一个已验证candidate或idle
合同编码到指定V2物理slot；入口检查发生在`case (slot)`的任何字段写入之前。

```systemverilog
if (tr == null) begin
    `uvm_fatal(get_type_name(), "set_req_fields got null xaction")
end
if (slot >= MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM) begin
    `uvm_fatal(get_type_name(), "set_req_fields slot exceeds compile-time slot count")
end
default_behavior = lsq_ctrl_model::make_default_behavior();
if (valid) begin
    if (main_tr == null) begin
        `uvm_fatal(get_type_name(), "active LSQ slot got null main transaction")
    end
    if (behavior.num_ls_elem != memblock_num_ls_elem_t'(1) ||
        main_tr.numLsElem != memblock_num_ls_elem_t'(1) ||
        !(behavior.need_alloc inside {2'b01, 2'b10}) ||
        (behavior.need_alloc == 2'b01 && (!behavior.uses_lq || behavior.uses_sq)) ||
        (behavior.need_alloc == 2'b10 && (behavior.uses_lq || !behavior.uses_sq))) begin
        `uvm_fatal(get_type_name(), "slot violates scalar LSQ behavior")
    end
    dut_futype = encode_and_fit_dut_futype(main_tr.fuType, context);
    rob_key = main_tr.get_rob_key();
    uop_idx = '0;
    num_ls_elem = behavior.num_ls_elem;
    fu_op_type = main_tr.fuOpType;
    last_uop = 1'b1;
end else begin
    if (main_tr != null || behavior != default_behavior ||
        lq_key.flag || lq_key.value != '0 ||
        sq_key.flag || sq_key.value != '0) begin
        `uvm_fatal(get_type_name(), "idle slot requires null main transaction, default behavior, and zero keys")
    end
    // 只为当前slot生成全零局部payload。
end
case (slot)
    // 只写tr中当前slot对应的req qualifier/payload。
endcase
```

中文伪代码：

```text
先检查tr非null且slot小于编译期slot数；不满足时在任何slot字段写入前fatal；
active slot检查main_tr非null；
检查main_tr.numLsElem和behavior.num_ls_elem都为1；
检查load严格为need_alloc=01、uses_lq=1、uses_sq=0，store严格为need_alloc=10、uses_lq=0、uses_sq=1；
active slot接收collect_lsq_candidates保存的两组candidate preview key；driver在首次写VIF前检查key value范围，
confirm_lsq_candidates在公共reservation前检查behavior实际使用的key仍与preview一致，失败均先fatal；
调用FuType helper生成当前V2可无损表示的one-hot编码；
从main transaction取得ROB key，复制candidate预测的LQ/SQ key和fuOpType；
固定uopIdx=0、lastUop=1，并把exceptionVec/trigger/flushPipe清零；
idle slot要求main_tr为null、behavior完整等于make_default_behavior、LQ/SQ key的flag和value全部为0；
idle合同不满足时也在当前slot字段写入前fatal；
合同通过后只写tr的当前xaction slot，不修改其它slot，也不修改主表、状态表、map、pending-sample、pointer或free count。
```

`clear_lsqenq_xaction()` 和 `assign_lsqenq_slot()` 都调用该 setter，避免 active构造和idle清理维护两套字段
列表。xaction constraint和driver pre-drive validation采用相同scalar合同。

## 8. Driver clock-first streaming

### 8.1 `lsqenq_agent_agent_driver::main_phase()`

源码位置：`agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`。

抽象功能描述：driver每个clocking边界最多取得一个item，先让DUT采样上一轮VIF值，再判断当前item是
真实launch还是redirect前abort；当前item写入后立即item_done，不在本item内等待下一拍。

```systemverilog
@this.vif.drv_mp.drv_cb;
req = null;
seq_item_port.try_next_item(req);
if (req != null) begin
    if (req.pre_pkt_gap != 0 || req.post_pkt_gap != 0) begin
        `uvm_fatal(get_type_name(), "V2 LSQ streaming requires pre/post gap 0")
    end
    req.memblock_dispatch_request_launched = 1'b0;
    req.memblock_dispatch_aborted_by_redirect = 1'b0;
    active_request = has_active_request(req);
    if (active_request &&
        (memblock_sync_pkg::dispatch_flush_in_progress ||
         memblock_sync_pkg::dispatch_flush_epoch != req.memblock_dispatch_flush_epoch)) begin
        req.memblock_dispatch_aborted_by_redirect = 1'b1;
        this.drive_idle(this.cfg.drv_mode);
    end else begin
        this.send_pkt(req);
        req.memblock_dispatch_request_launched = active_request;
    end
    seq_item_port.item_done();
end else begin
    drive_idle(cfg.drv_mode);
end
```

中文伪代码：

```text
先等待driver clocking边界；该边界让DUT采样上一轮保留在VIF上的值；
把req清空后非阻塞取得当前item，防止复用旧句柄；
无item时驱动全零idle；
有item时要求pre/post gap为0，并清launch/abort metadata；
active item若flush正在进行或epoch过期，则只驱动idle并标记launch前abort；
其余item先由validate_v2_scalar_item检查全部slot，再调用send_pkt一次写VIF；
active item标记request_launched，idle item保持0；
立即item_done，让sequence可以在同一边界完成上一批sample并构造下一批。
```

`build_phase()` 要求 active driver 的 `drv_mode==DRV_0`。`validate_v2_scalar_item()` 在任何VIF赋值前检查：

- inactive slot的 `needAlloc`、qualifier和全部payload都为0。
- active load/store的 `needAlloc` 与V2 LDU/STU FuType匹配。
- active LQ的`fuOpType`只允许普通load `0..6`和software prefetch `8/9/10`；active SQ只允许普通store `0..3`。
- 六个slot中active load总数不超过compile load width 6，active store总数不超过compile store width 4。
- active key value不越过ROB/LQ/SQ真实size。
- `uopIdx=0`、`numLsElem=1`、`lastUop=1`、exception/trigger/flushPipe为0。

xaction用两组宏值表唯一维护load/prefetch/store合法opcode；逐slot constraint直接`inside`值表，driver
helper读取同一值表，避免自定义函数调用阻断VCS对随机`fuOpType`的反向求解。同一6/4合同存在于
`c_v2_batch_enqueue_width`；`v2_streaming_gap_cons`还把继承的pre/post gap都收紧为0。因此通用default
sequence既不会生成CBO/其它非法opcode、5/6个store或非零gap；
driver复核覆盖关闭约束、手工赋值和其它directed producer。dispatch主路径仍在candidate阶段结合实际
free count提前截断，不依赖driver fatal做正常仲裁。

## 9. Reservation 和 sample completion

### 9.1 `confirm_lsq_candidates()`

源码位置：`memblock_lsqenq_dispatch_base_sequence.sv`。

抽象功能描述：该函数把 driver 已确认 launch 的 candidate 原子写入软件 allocation/map，并为每个 uid
创建动态实例 token；它不开放 issue，也不根据函数调用时恰好出现的后续 flush 撤销已经发生的 launch。

```systemverilog
if (!tr.memblock_dispatch_request_launched) begin
    // 只允许redirect abort或epoch失效。
    return;
end
foreach (uids[idx]) begin
    memblock_lsq_reservation_token_t token;
    lsq_ctrl.preview_allocate(behaviors[idx], expected_lq_key, expected_sq_key);
    // 只比较behavior实际使用的key。
    lsq_ctrl.commit_allocate(uids[idx], behaviors[idx], trs[idx]);
    token.valid = 1'b1;
    token.uid = uids[idx];
    token.launch_epoch = data.begin_lsq_reservation_launch(uids[idx]);
    pending_sample_tokens.push_back(token);
end
pending_sample_flush_epoch = tr.memblock_dispatch_flush_epoch;
pending_sample_valid = 1'b1;
```

中文伪代码：

```text
driver未launch时，只允许该item已标记redirect abort、当前flush有效或epoch失效；否则fatal暴露driver合同错误；
launch和abort同时为1时fatal；
检查五组candidate queue非空且等长，并要求上一批pending已经完成；
逐uid重新preview当前真实pointer，只比较load使用的LQ key或store使用的SQ key；
预测漂移时在任何状态修改前fatal；
调用唯一allocation owner commit_allocate建立active/enq/map并推进pointer/free count；
调用begin_lsq_reservation_launch递增当前uid的launch_epoch，并把状态置为LAUNCHED_PENDING_SAMPLE；
把{uid, launch_epoch}保存到单深度pending batch，等待下一driver边界记录sample事实并决定是否开放issue；
一旦request_launched=1，即使函数执行时flush epoch已变化也不能丢弃allocation，否则DUT已经看到的请求会没有软件owner。
```

### 9.2 `complete_v2_pending_sample()`

抽象功能描述：该函数在下一 driver 边界后为上一批 token 记录统一 DUT sample sequence；随后仅用
flush epoch决定是否开放旧实例 issue。sample事实和issue开放是两个独立动作。

```systemverilog
if (!pending_sample_valid) return;
sample_seq = memblock_sync_pkg::get_dut_sample_seq($time);
foreach (pending_sample_tokens[idx]) begin
    data.mark_lsq_reservation_sampled(pending_sample_tokens[idx].uid,
                                      pending_sample_tokens[idx].launch_epoch,
                                      sample_seq);
end
if (!admission_blocked_by_flush() &&
    pending_sample_flush_epoch == memblock_sync_pkg::dispatch_flush_epoch) begin
    foreach (pending_sample_tokens[idx]) begin
        complete_admission(pending_sample_tokens[idx].uid);
    end
end
clear_v2_pending_sample();
```

中文伪代码：

```text
没有pending batch时直接返回；
用当前仿真采样时刻取得统一DUT sample sequence；
逐token调用mark_lsq_reservation_sampled，并同时校验uid的launch_epoch仍匹配，把状态变为DUT_VISIBLE；
比较pending_sample_flush_epoch与当前memblock_sync_pkg::dispatch_flush_epoch；
两者相等且global flush未阻塞时，逐uid调用complete_admission；
complete_admission先drain CSR runtime event，再由issue scheduler检查active/enq并设置issue_ready、写对应issue queue；
保存epoch与当前epoch不等或global flush阻塞时，不调用complete_admission、不开放旧实例issue；
该路径仍保留DUT_VISIBLE/sample_seq，供redirect scan判断该mapping应计入哪一个epoch的software cancel；
逐epochcancel record负责回退reservation；
无论正常或redirect路径都清本地pending queue和metadata，避免重复完成。
```

## 10. Redirect 场景

### 10.1 launch 前 redirect

```text
driver在当前边界发现flush或epoch失效
-> request_launched=0、aborted_by_redirect=1
-> drive_idle
-> confirm不建立reservation
-> next-admit uid保持不变，恢复后重试。
```

### 10.2 launch 后、sample 前 redirect

```text
当前batch已建立active/enq/map reservation
-> 下一边界complete_v2_pending_sample先记录reservation sample_seq和DUT_VISIBLE
-> 发现epoch失效，不开放旧实例issue
-> redirect scan把满足sample cutoff的mapping写入当前epoch cancel record的software count
-> LSQ sequence下一轮按该record调用apply_pending_lsq_cancels，只回退一次pointer/free count
-> DUT cancel snapshot与同一record对账，但不再次回退资源
-> rollback后的uid重新进入candidate。
```

### 10.3 sample/issue-ready 后 redirect

继续使用原全局 redirect/reissue flow。本专项不增加本地5-cycle guard、第二套flush状态或第二个cancel owner。

## 11. 参数行为

| 参数/宏 | 类型 | 当前语义 |
|---|---|---|
| `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM` | compile | V2物理slot数6 |
| `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH` | compile | 本拍load element上限6 |
| `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH` | compile | 本拍store element上限4 |
| `MEMBLOCK_ENQ_PER_CYCLE` | runtime | 固定模式本拍总slot目标，范围1..6 |
| `MEMBLOCK_ENQ_PER_CYCLE_RAND_EN` | runtime | 开启ZERO/MIDDLE/MAX随机模式 |
| `*_ZERO_WEIGHT` | runtime | 选择主动idle拍的类别总权重 |
| `*_MIDDLE_WEIGHT` | runtime | 选择1..MAX-1的类别总权重，-1为AUTO |
| `*_MAX_WEIGHT` | runtime | 选择MAX的类别总权重 |
| `MEMBLOCK_LSQENQ_READY_TIMEOUT` | runtime兼容入口 | 公共参数层仍解析并检查非负；V2 sequence不读getter、不等待ready，且不做零值warning/clamp |

随机模式允许配置非零 `ZERO_WEIGHT`，也允许 `ZERO/MIDDLE/MAX=1/0/0` 这种idle-only配置；随机模式下只有
三类权重全0才会在参数初始化时fail-fast。随机返回0不算新的admission progress；如果上一批pending，
idle边界完成上一批sample仍算progress。zero-only不会消费uid，也不会伪造terminal；非空主表使用该配置时
不会自行请求global stop，只适用于testcase/UVM phase提供外部结束条件的idle场景。

## 12. 状态变化

### 12.1 scalar load

```text
main_table uid(load)
-> collect_lsq_candidates预测LQ key
-> driver launch V2 enqueue request
-> commit_allocate建立active/enq和LQ map，扣减LQ free count
-> 下一driver边界complete_v2_pending_sample
-> prepare_issue_route_for_uid设置issue_ready并进入LOAD issue queue
-> 后续load issue/writeback/ROB commit/LQ deq/terminal由各自flow推进。
```

### 12.2 scalar store

```text
main_table uid(store)
-> collect_lsq_candidates预测SQ key，单拍最多收集4个store element
-> driver launch V2 enqueue request
-> commit_allocate建立active/enq和SQ map，扣减SQ free count
-> 下一driver边界complete_v2_pending_sample
-> prepare_issue_route_for_uid设置issue_ready并分别进入STA和STD issue queue
-> 后续store writeback/ROB commit/SQ deq/terminal由各自flow推进。
```

### 12.3 随机零入队

```text
get_enq_per_cycle返回0
-> collect在读取uid/pointer/free前返回空candidate
-> sequence发送全零idle item
-> 若有上一批pending则完成其sample；否则本拍无admission progress
-> 下一拍重新随机，不消费或跳过next uid。
```

## 13. 端到端行为总结

```text
正常LSQ admission：
  main table
  -> collect_lsq_candidates
  -> set_req_fields
  -> driver clock-first launch
  -> confirm_lsq_candidates / commit_allocate
  -> pending_sample
  -> complete_v2_pending_sample
  -> prepare_issue_route_for_uid
  -> LOAD/STA/STD issue queue

launch前redirect：
  candidate
  -> driver epoch check
  -> drive_idle + aborted_by_redirect
  -> no reservation
  -> same uid retry

launch后redirect：
  reservation token
  -> complete_v2_pending_sample比较pending_sample_flush_epoch与当前dispatch_flush_epoch
  -> mark_lsq_reservation_sampled记录DUT sample sequence和DUT_VISIBLE
  -> 保存epoch与当前epoch不等，不调用complete_admission
  -> no issue route
  -> 当前redirect epoch的cancel record统计software cancel
  -> apply_pending_lsq_cancels
  -> software count只执行一次pointer/free rollback
  -> DUT observed count只对账，不再次rollback
  -> uid reissue

末批drain：
  global_stop + pending_sample
  -> trailing idle item
  -> complete or discard pending by epoch
  -> clear pending state
  -> sequence exit
```

端到端文字伪代码：

```text
正常路径先从连续uid前缀生成不超过V2 6-load/4-store能力的request；xaction约束保证通用随机路径遵守
同一上限，driver在写VIF前再对所有producer复核。driver只launch一次，sequence立即预留资源并生成
`{uid, launch_epoch}` token，使下一批看到正确pointer。下一driver边界给上一批写入统一sample sequence，
token转为DUT可见后才把uid放入issue queue。

redirect路径按发生时点分层：launch前不建状态；launch后必须保留allocation、mapping和sample事实，
使逐epochcancel record能准确统计DUT已看到的LQ/SQ entry。epoch失效批不开放issue；software count只回退
一次，DUT observed count只负责核对。

随机idle和末批trailing idle都只提供一个全零driver边界。前者不消费next uid，后者保证最后一批不会
停在已预留未route状态；二者都不修改pass/fail/terminal语义。
```
