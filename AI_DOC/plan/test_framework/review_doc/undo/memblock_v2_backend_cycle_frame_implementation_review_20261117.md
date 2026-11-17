# V2 后端周期帧与异常地址采样实现评审草稿

创建日期：2026-11-17。状态：实现评审更新稿；当前源码独立review未发现功能blocker，真实运输、生产helper、正常Store与Store PF专项通过。主代理仍在检查波形及最终验收边界，本稿不自行归档plan。

对应方案：[正式plan](../../plan/do/memblock_v2_backend_cycle_frame_formal_plan_20261117.md)。本轮只评审第一项源码与同步flow，不包含第二项特殊指令生成、第三项RM/PMM实现，也不修改RTL。

## 1. 术语与抽象功能说明

| 术语 | 本流程中的含义 | 代码落点 | 示例 |
|---|---|---|---|
| UID、ROB key | 主表逻辑编号、完整ROB环形编号 | `status_transaction`及主表 | UID11重发时仍是11，ROB归属需独立核对 |
| dynamic epoch、issue epoch、replay sequence | 动态实例代次、目标发射代次、重试编号 | `dynamic_epoch/load_issue_epoch/sta_issue_epoch/replay_seq` | 旧发射结果不能归属新重试 |
| reset epoch、sample | 复位代次、全DUT公共物理采样编号 | `memblock_sync_pkg` | 相同sample数字也不能跨reset使用 |
| batch、semantic batch | 正常提交UID列表、同拍WB/IQ/违例语义事件组 | `commit_uids`、`events` | 一组正常Store可形成非零scommit |
| frame、projection | 一次完整后端发布对象、其中实际端口值 | `memblock_backend_cycle_frame_t/transport.head/tail` | rollback主帧与下一拍尾帧同item预装 |
| reservation、ready、COMMITTED | 未生效的准备预占、两driver已经持有item、最终确认未来边沿 | `backend_redirect_reserved/backend_release_by_id` | 准备时不递增flush epoch |
| E、S、S+2 | driver输出边沿、DUT实际输入采样、普通LSQ异常地址目标拍 | `edge_sample/input_sample/target_sample` | E11输出，S12采样，14采异常地址 |
| sampled-applied | 两路实际输入匹配后业务状态已记账 | `frame.applied` | 准备与finish_item都不开放RM比较 |
| owner、active map | 唯一恢复拥有者、ROB/LQ/SQ反查活跃UID的索引 | `active_redirect`及`uid_by_active_rob`等 | 恢复继续使用原cancel record |
| cursor、watermark、prefix | 提交游标、生产完成水位、连续终态进度 | `commit_cursor_uid/backend_*_watermark/terminal_done_uid` | 不每拍扫描历史主表 |
| fault head、fault token、terminal | 故障ROB队首、故障恢复私有记录、指令最终收口 | `fault_head_waiting`及终态状态 | fault的rob_commit并非正常退休 |
| rollback、reissue、cancel | 访存违例冲刷、重发旧UID、取消旧执行实例 | 原redirect恢复路径 | 违例指令无需成为fault head |
| sideband、level、pulse | 随附控制信号、保持值、单拍脉冲 | 类别/pending与scommit/flushSb | 清fault token不撤销异常类别保持值 |
| raw、bounded history、FIFO | 原始采样、固定容量历史、按序队列 | `backend_*_by_sample`、运输队列 | 同值和零地址仍连续记录 |
| LSQ、LQ、SQ、STA、STD、WB、IQ | 访存队列、读/写队列、Store地址/数据目标、写回、发射反馈 | 原agent与handler | Store fault保持STA/STD真实WB屏障 |
| AF、PF、GPF、AMO、CMO、VLS | 访问/页/Guest页错误、原子、缓存块操作、向量访存 | `exception_vec/op_class` | AMO类别沿既有load-like规则 |
| flushSb、CSR | StoreBuffer排空请求、访问控制上下文 | 原flush请求、`issue_csr` | flushSb不随异常自动置1 |

抽象功能职责：原WB路径记录访问结果，scheduler协调真实后端输入边界，原提交与恢复路径完成状态和资源收口；monitor只保存事实。`backend_cycle_mode=0`的software-only及旧入口保持原兼容路径。

## 2. 修改目标与覆盖文件

旧真实路径由LSQ sequence构造并发送item，finish_item后才尝试mark正常/fault提交；redirect由另一sequence独立发送。两侧缺共同实际采样边界，已发scommit可能在事后资格检查被软件取消；故障地址亦缺连续样本的精确归属。

新路径保持两个agent和单写者，以一个在途frame共同准备，双ready后批准未来E/S，真实输入匹配后一次应用原提交状态，原redirect owner继续负责取消恢复。普通LSQ地址按冻结fault身份在S+2采集；访存违例主尾连续发布。

| 文件组 | 本轮改动及职责 |
|---|---|
| `seq/base_seq_help/memblock_backend_cycle_scheduler.sv` | 新有界scheduler；候选、共同发布、实际输入确认、来源槽、flushSb服务与reset |
| `seq/base_seq_help/common_data_transaction.sv` | frame完整业务结构、两运输队列、准备预占、故障地址等待/历史、恢复及排空门槛 |
| `common/memblock_common/src/memblock_sync_pkg.sv` | agent可见基础运输、双侧回执、输入与地址64拍历史 |
| `agent/lsqcommit_agent_agent/src/*{driver,monitor,xaction}.sv` | 原LSQ单写者持有主尾item，独立flushSb合成，实际输入连续采样 |
| `agent/redirect_agent_agent/src/*{driver,monitor,xaction}.sv` | 原redirect单写者对应运输与采样，保留valid anchor |
| `agent/io_mem_to_ooo_ctrl_agent_agent/src/*monitor.sv` | 连续采样VA/GPA/VS标记，保持共享sample锚点 |
| `seq/base_seq_help/lsq_commit_handler.sv` | 资格检查前移、sample apply纯更新、类别保持值、故障收尾地址门槛 |
| `seq/base_seq_help/exception_redirect_replay_handler.sv` | 原仲裁保存reservation，原恢复增加sampled-applied门槛 |
| `seq/base_seq_help/memblock_dispatch_base_sequence.sv` | WB语义→frame apply→deferred ctrl顺序 |
| `seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv` | 原唯一service调用scheduler、拓扑检查与reset |
| `seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv`、`memblock_redirect_dispatch_base_sequence.sv` | 周期模式仅运输共同item，旧兼容模式保留 |
| `seq/seq.f`、`seq/seq_pkg.sv` | 增加scheduler编译入口；没有新增并发service |

## 3. 字段、状态与生命周期检查

| 对象 | 设置者 / 读取者 / 清理者 | 正确性约束 |
|---|---|---|
| `backend_frame` | scheduler创建，两个worker运输，实际sample消费者应用，双finished后释放 | 最多一个在途完整单元 |
| `backend_release_by_id` | prepare建立；driver写ready/driven/finished；scheduler批准或取消 | 基础身份匹配，不能把ready当实际采样 |
| `backend_redirect_reserved` | 原handler获选时设置，COMMITTED或reset清除 | 阻止年轻越界，不提前拥有cancel record |
| `backend_fault_prepared` | scheduler选合法fault head；apply/reset/失效清除 | Store双真实WB满足才建立 |
| `commit_uids/keys/epochs/commit_instances` | build冻结；最终批准与rollback重建核对；apply一次应用 | 连续UID、ROB/dynamic及load/STA/STD issue epoch和replay均属同一冻结实例 |
| `issue_epoch/replay_seq/exception_vec` | fault build冻结，最终资格核对，地址及后续RM读取 | 同dynamic重试仍不得借旧frame身份 |
| `reserved_older_decision/rob_source_slot` | 取消未发normal保存，最终E/S绑定、消费或reset清理 | 旧已采样batch不可再用，运输等待不追认过去决定 |
| `backend_*_by_sample` | monitor逐拍写，scheduler精确sample读，有界删除 | 目标保护水位覆盖前报错，不读latest替代 |
| `backend_fault_address_by_uid/backend_address_wait_q` | fault apply建立，S+2采集，reset取消 | UID映射保留冻结动态身份与整组地址，零值合法 |
| `backend_redirect_sampled_applied` | COMMITTED清、实际apply置、reset清 | 原恢复只在真实输入记账后放行 |
| `latched_is_store_exception` | 实际apply更新；driver缓存；reset清除 | 清fault token不清最近类别level |
| `backend_flushsb_requested/driven` | 原请求服务与LSQ driver往返 | 准备等待仍每拍服务，不因fault自动触发 |

## 4. 同拍状态次序

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`。

**抽象功能描述：** 原dispatch调用本函数，在资源释放前处理WB和已实际采样后端帧，保证批准提交不会被同拍deq提前取消。

真实逻辑摘要：

```systemverilog
monitor_batch_handler.process_monitor_event_batch(events);
    if (memblock_sync_pkg::backend_cycle_mode)
        memblock_backend_cycle_scheduler::get().service_backend_cycle_samples();
    // 中文注释：semantic batch 已完成 redirect-first/feedback 处理后，才释放本拍
    // ctrl/deq 对应的 LQ/SQ mapping；失败的队首 raw 保留到后续 service tick 重试。
    monitor_adapter.apply_deferred_ctrl_updates_batch(deferred_ctrl);
```

**中文伪代码：**

```text
本逻辑在原collect批次内接入实际后端输入。先由原batch handler完成当前WB/违例选择及状态更新。
周期模式随后让scheduler对同sample两路输入做完整匹配，成功才更新已批准rob_commit。
最后原adapter应用此前暂存的ctrl deq/cancel，释放资源仍走原唯一生命周期。
没有新建并发线程，也没有为了新帧丢弃真实WB。
```

修改前：WB语义后直接deferred ctrl；正常/fault记账由各自sequence发送后处理。修改后：实际frame apply位于两者之间。正确性检查：同S older commit与deq共享同一旧owner机会，WB仍先于记账。

## 5. 最终资格及不可撤销应用

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`。

**抽象功能描述：** 最终资格函数只读核对冻结对象；实际apply只能执行已经批准并由monitor确认的状态更新。二者分工使已发scommit不再事后取消记账。

真实逻辑摘要：

```systemverilog
if (frame.has_fault && (!uid_is_fault_terminal_candidate(frame.uid) ||
                data.get_status(frame.uid).replay_seq != frame.replay_seq ||
                (data.is_scalar_store_uid(frame.uid) ? data.get_status(frame.uid).sta_issue_epoch :
                    data.get_status(frame.uid).load_issue_epoch) != frame.issue_epoch ||
                data.get_status(frame.uid).exception_vec != frame.exception_vec)) return 0;
```

**中文伪代码：**

```text
本段核对fault准备仍属于同一真实访问。原fault candidate资格必须继续成立，replay sequence、目标issue epoch与exception向量必须与frame冻结值一致。
任一变化返回false，由scheduler取消尚未发布的整帧准备；不会发scommit或开放RM。
uid_is_fault_terminal_candidate内部继续要求当前活跃故障和Store双真实WB等原资格，未重建另一套fault判定。
```

修改前：finish_item后mark函数可遇flush或资格变化而拒绝；修改后：COMMITTED前资格失效取消，实际apply不重选。正常列表同时核对cursor/ROB/dynamic、load/STA/STD issue epoch和replay、原normal资格及redirect冲刷范围。独立review提出的fault身份与apply幂等问题已修，最新compile_backend12及专项使用该源码。

## 6. 共同发布及原owner激活

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_backend_cycle_scheduler.sv`。

**抽象功能描述：** scheduler在两侧持有完整item后选择未来边沿；COMMITTED才调用原redirect owner入口，保持原取消与恢复合同。

真实逻辑摘要：

```systemverilog
if (frame.has_redirect) begin
                data.request_redirect_flush(frame.redirect_event.redirect);
                data.backend_redirect_sampled_applied = 0;
                data.backend_redirect_reserved = 0;
                if (frame.redirect_event.source == MEMBLOCK_WB_EVENT_SOURCE_REDIRECT) begin
                    data.mark_redirect_intent_accepted(frame.redirect_event.redirect_intent_id);
                    if (auto_fault_intent_id == frame.redirect_event.redirect_intent_id) begin
                        void'(data.consume_redirect_intent_accepted(auto_fault_intent_id));
                        auto_fault_intent_id = 0;
                    end
                end
            end
            frame.approved = 1;
```

**中文伪代码：**

```text
本逻辑负责从运输准备转到最终发布。前置ready/资格/同物理sample检查通过后，带redirect帧调用request_redirect_flush创建原cancel record和flush epoch。
清除准备reservation并标记本帧恢复尚未sampled-applied；原软件intent按既有source合同登记accepted，auto故障intent消费后清本地编号。
然后approved并绑定E/S；未带redirect的正常/control不创建恢复owner。
原request入口仍是唯一active owner分配者，reset/更老请求只能在最终批准前取消准备。
```

修改前：原handler获选即分配owner并分别发队列。修改后：handler只保存获选reservation，双侧最终批准再分配。正确性检查：准备不递增flush epoch，不制造部分驱动。

## 7. 主尾item连续驱动

源码位置：`mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv`。

**抽象功能描述：** LSQ driver保留共同运输单元，按最终E输出主帧并在紧邻边沿直接输出尾帧；redirect driver采用相同合同但只写redirect端口。

真实逻辑摘要：

```systemverilog
tail = main_driven && tr.backend_cycle.has_tail;
        if (release_state.committed && edge_sample == release_state.edge_sample + (tail ? 1 : 0)) begin
            projection = tail ? tr.backend_cycle.tail : tr.backend_cycle.head;
            pkt.io_ooo_to_mem_lsqio_pendingPtr_flag = projection.pending_flag;
            pkt.io_ooo_to_mem_lsqio_pendingPtr_value = projection.pending_value;
            pkt.io_ooo_to_mem_lsqio_pendingst = projection.pending_st;
            pkt.io_ooo_to_mem_lsqio_pendingMMIOld = projection.pending_mmio;
            pkt.io_ooo_to_mem_lsqio_scommit = projection.scommit;
            pkt.io_ooo_to_mem_isStoreException = projection.store_exception;
            this.send_pkt(pkt);
            memblock_sync_pkg::acknowledge_backend_drive(tr.backend_cycle.frame_id, 0, tail, edge_sample);
```

**中文伪代码：**

```text
本段在每个drv_cb边沿判断当前是主投影还是预装tail。
只有release已committed且边沿等于E或E+1才取相应projection；依次设置原pending指针、pending标志、scommit和类别，再通过原send_pkt写LSQ端口。
acknowledge_backend_drive检查frame身份与边沿并保存本侧回执；有tail时保持item，下一拍无需重新ready。
等待期间drive_active_idle仍驱动独立flushSb；reset/取消输出安全idle并退出；错过已批准边沿立即fatal。
```

修改前：两个sequence各自运输，rollback下一拍控制需要重新调度；修改后：主尾同item固定相邻。正确性检查：不能见redirect就将R合法older提交全清，但R+1必须零pending/提交。

## 8. 实际输入与地址事实

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_backend_cycle_scheduler.sv`。

**抽象功能描述：** 实际输入消费者从两路有界历史中确认指定S，匹配后才业务apply；地址消费者独立等待冻结fault的S+2。

真实逻辑摘要：

```systemverilog
actual = memblock_sync_pkg::backend_lsq_input_by_sample[sample_seq].value |
                 memblock_sync_pkg::backend_redirect_input_by_sample[sample_seq].value;
        expected = is_tail ? frame.transport.tail : frame.transport.head;
        if (actual !== expected ||
            memblock_sync_pkg::backend_lsq_input_by_sample[sample_seq].reset_epoch != frame.transport.reset_epoch ||
            memblock_sync_pkg::backend_redirect_input_by_sample[sample_seq].reset_epoch != frame.transport.reset_epoch) begin
            `uvm_fatal("BACKEND_FRAME", $sformatf("backend input mismatch frame=%0d tail=%0d sample=%0d actual=%h expected=%h", frame.transport.frame_id, is_tail, sample_seq, actual, expected))
        end
        if (!frame.applied) begin
            commits.apply_sampled_backend_cycle(frame);
```

**中文伪代码：**

```text
先等两路输入watermark到S；到齐却缺历史则fatal，不用latest填补。
两侧projection互斥字段合成actual，与主或尾expected及reset epoch逐项严格比较；不一致fatal且不更新业务状态。
主样本通过才调用apply_sampled_backend_cycle；其approved/applied门槛保证一次提交，不在发送后重新资格选择。
该函数入口先处理地址等待队列；地址raw在ctrl monitor连续发布，普通LSQ目标固定S+2并核对reset，再保存整组输出。
地址队列只包含待完成对象，历史64拍，保护未消费目标；同值和零地址仍是完整样本。
```

修改前：无独立带sample的整组异常地址记录。修改后：身份来自fault帧，raw仅提供事实。正确性检查：WB sample不被误作地址sample，资源回收后仍可读取冻结frame。

## 9. 原恢复与fault收尾门槛

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv`。

**抽象功能描述：** 原恢复handler新增真实输入已应用条件，继续复用原drive/anchor/cancel和冲刷范围，不要求memoryViolation成为fault head。

真实逻辑摘要：

```systemverilog
if (data.backend_redirect_recovery_eligible() &&
            data.redirect_drive_done_for(redirect) &&
            data.cancel_redirect_scan_ready(redirect)) begin
            if (!data.apply_redirect_flush(redirect)) begin
                return;
            end
```

**中文伪代码：**

```text
本逻辑只在原active redirect存在时推进恢复。
先检查本帧实际输入业务应用门槛，再检查原driver完成和cancel扫描证据；齐全后沿原apply_redirect_flush执行范围处理，不满足继续等待。
fault最后收口额外等待backend_fault_address_ready，地址采集完成后才推进fault cursor并清token；RM比较通过不属于恢复条件。
类别保持值不随token清除撤销，新fault或reset覆盖它。
memoryViolation只走原rollback范围和reissue，不建立fault终态token。
```

正确性检查：新门槛不替代原cancel evidence；恢复放行和地址完成分别负责不同阶段，不能把S+2数据归属延迟变成RM驱动的恢复门禁。

## 10. 已核验的编译与仿真证据

以下由主代理在`backend_transport_20261117`编译/运行，本评审读取日志核验。最新有效日志目录为`/nfs/home/lixiangrui/.tmp/memblock_virtualization_impl_20261117`；旧compile_backend5及checked系列仅属开发记录，不作为当前最终源码证据。波形由主代理继续检查，本稿不宣称波形核验完成。

| 最新日志 | 已观察结果 | 实测边界 |
|---|---|---|
| `compile_backend12.log` | VCS成功，KDB结尾0 error/0 warning | 对应当前backend_transport构建 |
| `run_backend_transport.log` | 真实两个driver与输入monitor，PASS checks=81，UVM_ERROR=0、UVM_FATAL=0 | 验证共同未来E、S=E+1、紧邻tail、prepared取消、同值独立frame及退出清理；没有LSQ入队，scommit/pending始终为0 |
| `run_backend_helpers_final.log` | 生产helper各项PASS，UVM_ERROR=0、UVM_FATAL=0 | 检查rollback合法older前缀/tail、normal旧replay拒绝、fault issue/replay拒绝、apply幂等、精确S+2/零VA/非叶归属及reset清理 |
| `run_backend_normal_final.log` | 两条真实正常Store exception=0，各RM compare PASS，UVM_ERROR=0、UVM_FATAL=0 | 有真实入队与正常scommit路径，不代表违例端到端 |
| `run_backend_final_storepf_wave.log` | 真实Store PF exception=0x008000，RM compare PASS，UVM_ERROR=0、UVM_FATAL=0 | 生成故障波形，具体边沿由主代理继续核对 |

当前证据组合分别覆盖业务函数、双driver实际运输、真实普通访存，不将运输自测等同真实访存违例。真实rollback带非零older scommit、准备/主尾间reset及flushSb等待窗口若未另提供结果，仍属于未实测边界；生产helper的reset取消只证明软件生命周期清理，不能替代物理reset。标量misalign、故障地址同值连续实例亦需按最终验收矩阵审视。全basicTest一轮回归安排在三个方案完成后开展，当前未完成。

## 11. 性能、排空及非本次改动

新增高频路径只处理一个在途帧、有限提交宽度、当前事件组和固定64拍历史；没有每拍全主表扫描，没有新增max_cycles成功退出。地址等待只遍历未完成队列；原terminal前缀与runtime drain纳入新frame/queue/reservation/地址责任。

本工作树截至评审含第一项上述源码、两份flow、两个专项vseq及helper和对应CSV；原本地保存提交已在父分支完成，不与本项混评。外置RTL、第二项虚拟化生成、第三项RM/PMM源码不由本评审宣称完成。无push、无plan移动、无commit由本agent执行。

## 12. Plan 对齐检查

| 正式方案约束 | 当前实现判断 |
|---|---|
| 原唯一service、两个agent单写者 | 已实现 |
| 准备不激活owner，双ready后最终批准 | 已实现；真实运输prepared取消通过，更老业务intent取代按独立源码review闭环 |
| WB语义→实际frame apply→deferred ctrl | 已接入 |
| Store故障双真实WB与原例外 | 复用原资格 |
| 实际sample后正常/control/fault单次记账 | 已接入；apply幂等与冻结normal/fault代次拒绝由生产helper验证 |
| fault类别/head/redirect同S | 真实fault运行通过，输入消费者匹配；波形边沿核查仍由主代理完成 |
| rollback R合法older、R+1预装尾拍 | 合法older/幂等由生产helper验证，连续tail由真实运输验证；尚不等同违例端到端 |
| flushSb独立持续服务 | 代码已接入，压力与准备窗口专项待证据 |
| LSQ S+2、标量misalign仍必需、范围外独立来源 | 精确S+2由helper验证，标量misalign保留LSQ规则；AMO独立，segment当前双入口拒绝而不可达 |
| 记录及恢复解耦、required地址排空 | 已实现 |
| 第三项终态前RM参考快照 | 本项保留明确接入点，第三项尚未实施，不提前宣称完成 |

## 13. 实现与 Plan 不一致项

### 13.1 帧类型及来源槽布局的实施差异

Plan原有逻辑：完整frame拟放dispatch_types，前级槽含UID列表、head/control及LSQ投影。当前源码：完整class定义在common_data_transaction.sv之前，slot保存基础来源身份与投影；列表/ROB/dynamic保存在frame，未绑定older候选另用reserved_older_decision。原因是避免提前类型依赖和重复复制完整候选。该布局精简已写入正式plan第1项`IMPLEMENTATION_DELTA`，保留全部语义且独立源码review未发现blocker；生产helper与真实运输分别提供业务和时序证据，本稿不擅自重写执行前方案。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_backend_cycle_scheduler.sv`，前级来源字段。

**抽象功能描述：** 来源槽保存已确定物理周期的投影与一次消费身份；未绑定older候选由相邻frame对象单独保留。

```systemverilog
    memblock_backend_cycle_frame_t reserved_older_decision;
    typedef struct {
        bit valid, consumed;
        longint unsigned source_sample, reset_epoch, reservation_id;
        memblock_sync_pkg::backend_cycle_projection_t projection;
    } memblock_rob_decision_slot_t;
```

**中文伪代码：**

```text
未最终确认的合法older候选由reserved_older_decision引用保存，不把它称为过去物理拍已发布决定。
来源槽只存基础身份和投影，最终未来E/S确定后绑定来源sample并标consumed，防止同一批重复运输。
候选UID、ROB/dynamic及逐目标issue/replay仍由原frame提供最终资格核验；idle保持投影清零scommit。
该字段布局不新建决策owner；生产helper证明older前缀及一次应用，真实运输证明未来边沿/连续tail和准备取消，仍不能仅靠来源编号赋值自证或宣称真实违例端到端完成。
```

### 13.2 独立地址来源的可达范围

Plan要求AMO/segment独立来源保留既有恢复并标范围外。当前代码明确识别AMO为ADDRESS_ATOMIC，其余非LSQ非AMO为UNKNOWN并fatal，虽已定义ADDRESS_SEGMENT。当前主表`validate_main_table_entry()`明确拒绝VLDU/VSTU/VSEGLDU/VSEGSTU，公共`derive_op_behavior()`入口再次拒绝vector LS，故segment不能通过合法生产路径进入scheduler。该边界已静态确认并记入正式plan第2项`IMPLEMENTATION_DELTA`，不要求为本项新增segment生成/恢复。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_backend_cycle_scheduler.sv`，`build_cycle_frame()`。

**抽象功能描述：** 本分支为故障帧标明地址观测来源，决定是否需要进入普通LSQ地址等待；不能用未知来源构造可通过的记录。

```systemverilog
            frame.address_source = frame.address_required ? memblock_backend_cycle_frame_t::ADDRESS_LSQ :
                (data.is_atomic_uid(frame.uid) ? memblock_backend_cycle_frame_t::ADDRESS_ATOMIC :
                 memblock_backend_cycle_frame_t::ADDRESS_UNKNOWN);
            if (frame.address_required) frame.address_status = memblock_backend_cycle_frame_t::WAITING;
            if (!frame.address_required && !data.is_atomic_uid(frame.uid)) begin
                `uvm_fatal("BACKEND_ADDRESS", "fault route has no defined address source")
            end
```

**中文伪代码：**

```text
本段根据前面已确定的普通LSQ必需位选择来源；普通标量使用ADDRESS_LSQ并进入WAITING。
其他来源中，已识别原子使用ADDRESS_ATOMIC，当前不建立新增LSQ等待。
剩余范围外来源标UNKNOWN，随后在非原子的条件下立即fatal，不能以缺记录默许pass。
segment已由主表和公共行为入口拒绝，因此ADDRESS_SEGMENT当前是不可达预留；未来开放向量功能时需独立补来源合同，不能套用普通LSQ S+2。
```

### 13.3 地址对象与正常批次实例布局

正式plan补充的第2项`IMPLEMENTATION_DELTA`明确复用`backend_fault_address_by_uid`保存已批准frame，取代另建同义完整地址事件。frame已有全部UID/ROB/执行代次、CSR、S/target、来源/状态和raw组；释放运输指针不删除独立按UID引用，reset明确取消等待。该项无需第二个owner或公共消费通道。

正式plan第3项`IMPLEMENTATION_DELTA`补入正常批次严格执行身份。虽然同逻辑UID重试完整完成后仍能合法提交，主代理选择让未发旧准备对象不能承接新attempt，失配在输出前取消重建。这一更严格合同不会撤销已经输出的scommit。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`，`check_backend_cycle_commit_eligibility()`。

**抽象功能描述：** 最终正常资格核验同时确认逻辑指令和冻结的最终执行实例，只有同一实例仍然完整正常完成才能批准发布。

```systemverilog
            if (uid != commit_cursor_uid + idx || !uid_is_normal_commit_candidate(uid) ||
                data.get_status(uid).get_rob_key() != frame.commit_keys[idx] ||
                data.get_status(uid).dynamic_epoch != frame.commit_epochs[idx] ||
                data.get_status(uid).load_issue_epoch != frame.commit_instances[idx].load_issue ||
                data.get_status(uid).sta_issue_epoch != frame.commit_instances[idx].sta_issue ||
                data.get_status(uid).std_issue_epoch != frame.commit_instances[idx].std_issue ||
                data.get_status(uid).replay_seq != frame.commit_instances[idx].replay_seq) return 0;
```

**中文伪代码：**

```text
本段逐项核对冻结正常列表。UID必须位于当前cursor的连续位置，原normal完成资格必须保持。
完整ROB和dynamic不一致即拒绝；任一load/STA/STD issue epoch或replay sequence变化也拒绝。
返回false只取消尚未COMMITTED的准备对象，原scheduler重新建帧；不修改已发送提交或写参考数据。
rollback重建合法older前缀使用相同实例字段并复制匹配项，防止复用旧准备承接新重试。
生产helper主动改变normal replay，确认该真实资格入口拒绝旧frame。
```

## 14. Plan 未说明但 Coding 落实的细节

- `check_topology()`通过config_db检查两个agent的sequencer/driver/monitor开启，缺能力fail-fast；这是实际共同运输的必要前提，不新增运行参数。
- 输入monitor使用原shared sample anchor；双方watermark和采样时间在最终批准前交叉核对，未扩展L2专用producer mask。
- 基础运输历史固定64拍，用最早未消费sample保护；容量不足fatal，避免新功能变成无限历史或静默覆盖。
- 清fault token保留最近类别level，reset driver清缓存；与正常idle保持行为一致。
- 本轮未新增每拍全表debug检查，不引入额外成功超时退出。

## 15. 新增专项入口、helper与CSV覆盖

| 入口 / 文件 | 实际职责与调用关系 | 证据 / 限制 |
|---|---|---|
| `memblock_backend_cycle_selftest_vseq` | 创建`soft_test_memblock_backend_cycle_sequence`并在virtual sequencer执行 | 生产函数软件自测，不驱动真实LSQ指令 |
| `soft_test_memblock_backend_cycle_sequence::body()` | 复用原soft主表/admit/issue/WB构造，再调用真实建帧、最终资格、apply和地址消费者 | normal/fault代次拒绝、older前缀、尾投影、幂等、S+2和reset清理 |
| `memblock_backend_transport_selftest_vseq` | 专属拥有两个真实sequencer，创建LSQ/redirect薄sequence投递同一transport | checks=81；实际driver/monitor运行，无LSQ入队、scommit=0 |
| `memblock_backend_transport_lsq_sequence/redirect_sequence::body()` | 各建一个backend_sync item，通过真实agent发给原driver | 不随机化覆盖transport，无第二业务scheduler |
| `AI_DOC/seq/backend_cycle_soft_framework_testcases_20261117.csv` | 独立记录生产helper软件自测 | 与真实DUT清单分开 |
| `AI_DOC/seq/backend_cycle_real_dut_testcases_20261117.csv` | 记录双ready主尾、同值、prepared取消、terminal idle | 执行状态由主代理按最新运输日志同步，不混作真实访存违例 |

### 15.1 `run_transport()`真实运输验证

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_backend_transport_selftest_vseq.sv`。

**抽象功能描述：** 该task将同一完整主尾对象交给真实两agent并检查输入历史，验证运输合同；它不生成LSQ入队或业务违例。

```systemverilog
                check(!release_state.committed && release_state.driven == 0,
                      "two-ready must precede committed drive");
                ready_sample = memblock_sync_pkg::peek_current_dut_global_sample();
                for (int unsigned offset = 1; offset <= 3; offset++)
                    check_sample(ready_sample + offset, previous, "prepared_keeps_previous_inputs");
```

**中文伪代码：**

```text
本分支在两真实driver都声明ready后确认尚未committed且没有drive回执。
连续检查随后三个实际sample的两路输入仍为previous，证明准备阶段无端口副作用。
check_sample等待真实monitor历史并核对sample/reset及逐字段投影，不伪造raw。
之后取消分支只标canceled并等待自然finished；发布分支固定未来E，检查E旧值、S=E+1主值、下一sample尾值。
所有分支最终确认两侧finished并删除release槽，等待watchdog仅报失败，不作为成功退出。
```

### 15.2 生产helper的正常执行身份拒绝

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_backend_cycle_sequence.sv`，`body()`。

**抽象功能描述：** 软件自测使用真实build和最终资格函数确认旧正常frame不能承接新replay实例，不复制资格算法。

```systemverilog
        data.get_status(0).replay_seq++;
        if (commit_handler.check_backend_cycle_commit_eligibility(normal_frame))
            `uvm_fatal("BACKEND_SELFTEST", "stale normal replay identity was accepted")
        data.get_status(0).replay_seq--;
```

**中文伪代码：**

```text
原soft场景建立完整正常Load/Store并调用生产build_cycle_frame冻结列表。
仅改变当前normal UID的replay sequence，生产最终资格必须拒绝旧frame，否则自测fatal。
恢复原值后继续验证rollback older前缀和幂等apply，不留下人为身份污染。
该检查证明严格实例合同的入口有效，不将手工改数值声称为DUT已真实触发replay。
```

## 16. 当前评审结论

共同发布、实际采样记账和普通LSQ地址链已实现；fault与normal冻结实例、apply幂等及segment边界已闭环。当前独立源码review未发现功能blocker，最新真实运输、生产helper、正常Store和Store PF均通过。主代理继续完成波形及最终验收边界确认；本稿不把运输等同真实违例端到端，不自行移动plan或宣称三项全验收。


## 主代理最终实测补充

2026-11-17，`run_backend_flushsb_final.log`（seed6111744）、`run_backend_base_final.log`（seed6111745）均 TEST_PASS、UVM_ERROR/FATAL=0。Store PF波形读取在eda01完成，本地reader未正常打开文件，未以本地空输出作为证据。

真实端口 `top_tb.U_MEMBLOCK.io_ooo_to_mem_isStoreException` 和 `io_redirect_valid` 在1030.850ns同边沿E置1；redirect于1035.600ns清零，类别继续保持。输入S是1035.600ns边沿前采样，`io_mem_to_ooo_lsqio_vaddr` 在1040.350ns边沿后变为0x8e711a80，因此1045.100ns边沿前S+2可取到本次地址。force连接后的top_tb同名reg未反映DUT实际值，故只以U_MEMBLOCK端口作时序证据。GPA在此S1 PF中含未知高位，第一项如实保存四态值，不把它作为独立expected或正常GPA检查通过。

本项coding、文档、必要编译/基础与专项验证及独立源码review已完成，无未关闭源码blocker，允许第一项plan归档并独立提交。未实测的交错（真实违例带非零older Store、主尾间reset、逐窗口flushSb、标量misalign端到端）保留为后续统一回归和扩展验证的明确边界，不宣称已覆盖。全basicTest一轮回归仍在三项完成后执行。
