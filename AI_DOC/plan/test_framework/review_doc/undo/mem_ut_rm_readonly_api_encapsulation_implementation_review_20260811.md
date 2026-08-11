# mem_ut RM 只读 API 封装实现评审

关联执行计划：

- 执行输入：AI_DOC/plan/test_framework/plan/undo/mem_ut_rm_readonly_api_encapsulation_plan_20260809.md
- 归档目标：AI_DOC/plan/test_framework/plan/do/mem_ut_rm_readonly_api_encapsulation_plan_20260809.md

评审日期：2026-08-11

评审结论：在用户限定的 basicTest 单 DCache responder 拓扑下，测试框架已经提供一个唯一的 memblock_rm_readonly_api 只读 class。API 只复制值型结果，不实现 RM/checker，也不改变 DUT 协议、shared-memory batch 时序、主表懒分配或 responder 主循环。三个实现阶段均已独立提交，且本轮指定仿真通过。

## 1. 术语与抽象功能说明

| 英文术语 | 本文中的中文含义 | 对应代码对象 | 使用场景/例子 |
|---|---|---|---|
| RM | 后续 reference model，只是本 API 的读取方；本次不实现它 | memblock_rm.sv（本次未修改） | RM 通过 singleton 取得 API 句柄 |
| value view | 与框架内部对象完全分离的标量/packed struct 副本 | *_view_t typedef | 修改返回副本不会改变主表 |
| backing map | DUT 第一次真实 memory-facing read 懒建立的初始化内存映射 | main_mem | 只读初始化数据，不含 DUT 写入 |
| overlay | 已提交的 DCache writeback 或 Uncache store 覆盖层 | write_overlay_mem、write_overlay_byte_valid | overlay miss 不回退 backing |
| overlay batch | 已收到但要到下一正常 sample 才提交的写事件集合 | dcache_write_batch、uncache_write_batch | API 不触发 batch 提交 |
| observer | 在既有动作完成后被动记录事实的旁路状态 | mem_access_base_sequence static 状态 | 不选择、不延迟、不重排原动作 |
| owner/publisher | 唯一可以发布 DCache aggregate 的 responder 实例 | claim_dcache_observer_owner() | 第二实例 claim 失败并停止发布 |
| resident line | cached_line_by_addr 中 alias_valid=1 的协议驻留 cache line | DCache sequence 私有 map | 不等同于 clean/dirty 或 payload 完整 |
| C-data assembly | 将两拍 C-channel data beat 收齐的过程 | c_assembly_* 字段 | 首拍即阻塞 drain |
| fragment | 一条 64 B line 的低或高 32 B writeback event | dcache_fragment_*_bytes | 两半均提交后才确认整行 |
| corrupt byte mask | C response 被判定不可信的逐 byte 标记 | write_overlay_corrupt_byte_mask | 查询命中返回保护性 corrupt=1 |
| aggregate snapshot | owner 一次发布的 DCache 状态值型副本 | dcache_aggregate_snapshot | API 不读 live counter |
| generation | DCache owner 生命周期版本；reset 首次失效时递增 | dcache_owner_generation | 区分旧/新 observer 快照 |
| drain | DCache 当前没有驻留、未完成 C-data/写回观察且无 corrupt 的门槛 | dcache_drain_complete | 不是 DCACHE_L2_FLUSH_DONE 的别名 |
| ready | overlay 可被 RM 安全读取的统一门槛 | dcache_overlay_read_ready | 只有 valid=1 且 ready=1 才允许 RM 自行读取 |

抽象功能说明：memblock_rm_readonly_api 是测试框架状态到未来 RM 的唯一只读 façade。它探测已经存在的 owner、表项和 memory map，复制成独立 value view；DCache observer 只在既有 map/assembly/batch 动作完成点记录状态，并将结果作为一个快照发布。API 不拥有第二套模型，也不负责 RM 的读取时机或比较算法。

## 2. 实施范围、提交和边界

本次只 stage 与该 plan 直接相关的三个文件/文档集合；工作区中其他 L2TLB、fence、flow 和工具产物均保留原样。

| 阶段 | commit | 内容 |
|---|---|---|
| 阶段一 | 3f03801d4e — memblock: add RM readonly API value views | 新增唯一 API class、dispatch/TLB/内存 value view、非创建查询和 seq_pkg.sv 收录 |
| 阶段二 | 0b8d0a541b — memblock: observe readonly overlay writeback state | 增加 overlay corrupt byte mask、DCache/Uncache 被动 observer、fragment 事实和 readiness 聚合 |
| 阶段三 | 54e718d53b — memblock: harden readonly observer lifecycle | 增加 framework context、lifecycle 门控、64 B corrupt 计数收敛、reset generation、触及 line 的局部 observer 检查，并同步 plan |

明确不在范围内：memblock_rm.sv、RM 句柄获取、RM 调用、checker/scoreboard、比较算法、DUT/RTL、DCache/Uncache 协议时序、read_shared_mem_for_dut() 语义、主表 reset 逻辑、batch 提交边界和新 testcase。

## 3. 阶段一：唯一只读 API class 与 value view

### 3.1 singleton 和非创建 owner 探测

抽象功能描述：get() 只负责创建 API class 自身并返回统一句柄；try_get_common_data() 只探测已有 dispatch owner。它们不创建主表、memory owner 或 table entry，也不把查询 miss 转成仿真终止。

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv，函数 get()、try_get_common_data()。

~~~systemverilog
function memblock_rm_readonly_api memblock_rm_readonly_api::get();
    if (m_inst == null) begin
        m_inst = new("memblock_rm_readonly_api_singleton");
    end
    return m_inst;
endfunction:get

function bit memblock_rm_readonly_api::try_get_common_data(output common_data_transaction data);
    data = common_data_transaction::m_inst;
    return data != null;
endfunction:try_get_common_data
~~~

中文伪代码：先检查 API class 自己的静态句柄，只有句柄为空时创建一个 API 对象；随后查询只读取 common_data_transaction::m_inst，不调用会创建 singleton 的 get()。owner 不存在时由上层统一报 UVM_ERROR 并返回无效 view；API getter 本身不推进任何框架状态。

关键正确性检查：在 API 源码中没有 common_data_transaction::get()、ensure_main_line()、get_or_create 或 fatal 型表项 getter；因此 RM 首次拿句柄不会清空/分配主表，miss 也不会把 UVM_ERROR 升级成 UVM_FATAL。

### 3.2 dispatch、status 和索引反查 view

抽象功能描述：按 UID、ROB/LQ/SQ key 或 issue membership 复制已经存在的 dispatch 事实。查询只读取已有数组/关联表，返回 main_transaction_view_t、status_view_t、issue_membership_view_t 或 UID 标量，不返回原始 UVM object、queue 或 map。

源码位置：memblock_rm_readonly_api.sv，read_main_transaction_for_rm()、read_status_for_rm()、read_issue_membership_for_rm() 及 read_uid_by_*_for_rm()。

~~~systemverilog
if (uid >= data.main_trans_num || data.status_by_uid.size() <= uid) begin
    return report_query_miss("status", $sformatf("uid=%0d is outside the initialized table", uid));
end
if (data.status_by_uid[uid] == null) begin
    return report_query_miss("status", $sformatf("uid=%0d has no status entry", uid));
end
copy_status(data.status_by_uid[uid], view);
return 1'b1;
~~~

中文伪代码：先确认 dispatch owner 存在，再检查 UID 位于已初始化范围且 status 表项非空；任一条件不满足就统一记录查询 miss 并保持 valid=0。表项存在时把字段逐项复制到 API 自己的 packed struct，最后成功返回；调用方修改该 struct 不会回写 status object。

### 3.3 TLB 与 UID-TLB 纯值快照

抽象功能描述：把 TLB/UID-TLB 中 RM 需要的标量状态复制到专用 view；payload、CSR snapshot、queue 等内部对象只转成独立标量或 valid 标志，不暴露 live handle。

源码位置：memblock_rm_readonly_api.sv，copy_tlb_entry()、copy_uid_tlb_record()、read_tlb_entry_for_rm()、read_uid_tlb_for_rm()。

~~~systemverilog
view.valid                  = 1'b1;
view.lookup_key             = source.lookup_key;
view.entry_generation       = source.entry_generation;
view.s1_resolved_ppn        = source.s1_resolved_ppn;
view.s2_resolved_ppn        = source.s2_resolved_ppn;
view.fault                  = source.has_effective_fault();
~~~

中文伪代码：先探测 key 或 UID 对应记录是否存在；存在后只拷贝 lookup key、代次、stage、PPN、fault 和时间等值字段。UID-TLB 的 payload 只转换为 payload_valid，CSR snapshot 只取 update_seq，不把对象指针带出 API。

### 3.4 framework context view

抽象功能描述：为未来 RM 提供当前 dispatch/shared-memory 生命周期的标量上下文，例如主表就绪、flush、redirect、replay pending 和 sample；它用于关联日志/读取时机，不驱动任何 sequence。

源码位置：memblock_rm_readonly_api.sv，read_framework_context_for_rm()。

~~~systemverilog
view.main_table_ready                 = data.main_table_ready;
view.main_trans_num                   = data.main_trans_num;
view.global_stop_requested            = data.global_stop_requested;
view.reset_backend_done               = memblock_sync_pkg::reset_backend_done;
view.ptw_wait_replay_count            = data.ptw_wait_replay_q.size();
view.current_dut_sample_seq           = memblock_sync_pkg::peek_current_dut_global_sample();
~~~

中文伪代码：若 dispatch owner 不存在，返回无效上下文并报错；否则复制主表、停止、reset、flush、redirect、issue epoch、replay 数和当前 sample 等标量。读取 queue 的 size() 只得到瞬时计数，不消费 queue，也不改变 replay 状态。

## 4. 阶段一/三：backing 与 committed overlay 只读查询

### 4.1 统一内存 map 查询 helper

抽象功能描述：read_memory_map() 是 API class 内部的只读实现，按 byte mask 探测已经存在的 backing 或 overlay。它区分初始化 backing、已提交 overlay 和 corrupt 旁路，不承担 DUT 读写或懒分配。

源码位置：memblock_rm_readonly_api.sv，read_memory_map()。

~~~systemverilog
if (!mem_access_base_sequence::is_shared_memory_lifecycle_initialized()) begin
    return report_query_miss(overlay ? "committed_overlay" : "initialized_backing",
                             "shared-memory lifecycle is not initialized");
end
if (mem_access_base_sequence::write_overlay_corrupt_byte_mask.exists(line_addr) &&
    mem_access_base_sequence::write_overlay_corrupt_byte_mask[line_addr][byte_offset]) begin
    corrupt_hit = 1'b1;
    view.corrupt_byte_mask[i] = 1'b1;
end
~~~

中文伪代码：先检查 shared-memory lifecycle 已由既有 owner 初始化；未初始化立即报错，不创建 backing/overlay line。对每个请求 byte，backing 查询只读 main_mem，overlay 查询先看 corrupt mask，再看 byte-valid overlay；corrupt 命中返回保护性结果，普通 overlay miss 不回退主内存而报错。

### 4.2 对外返回语义

| 查询 | 正常命中 | corrupt 命中 | miss |
|---|---|---|---|
| read_initialized_backing_for_rm() | valid=1, corrupt=0, data_valid=1 | 不读取 corrupt overlay，backing 只按存在性返回 | UVM_ERROR + valid=0 |
| read_committed_overlay_for_rm() | valid=1, corrupt=0, data_valid=1 | valid=1, corrupt=1, data_valid=0，附带 corrupt byte mask | UVM_ERROR + valid=0 |

这两个 API 都不调用 read_shared_mem_for_dut()、main_mem_access_task()、ensure_main_line() 或 ensure_write_overlay_line()，不会因 RM 查询改变 lazy map、overlay、batch 或时间边界。overlay 查询依赖 shared-memory lifecycle，不依赖 DCache owner 是否仍发布；因此 owner 退出后已提交 overlay 仍可读，而 DCache readiness 会单独返回 invalid。

## 5. 阶段二/三：DCache 被动 observer 与 aggregate

### 5.0 shared-memory lifecycle 清理

抽象功能描述：shared-memory lifecycle 初始化/清理同时清除 observer 的历史快照和 fragment 旁路，但不触碰 DCache sequence 自己的 cached_line_by_addr；API 在 lifecycle 无效时拒绝读取。

源码位置：mem_base_sequence.sv，clear_shared_memory_state()、initialize_shared_memory_state()、clear_dcache_observer_state()。

~~~systemverilog
main_mem.delete();
write_overlay_mem.delete();
write_overlay_byte_valid.delete();
write_overlay_corrupt_byte_mask.delete();
dcache_write_batch.delete();
uncache_write_batch.delete();
clear_dcache_observer_state();
~~

中文伪代码：生命周期开始时先清除旧 backing、overlay、corrupt mask 和未提交 batch，再清除 aggregate、owner claim 和 fragment observer；随后配置新的 memory range 并置 lifecycle initialized。清理动作不调用 DCache map 的删除接口，因此不会替主体 responder 决定协议状态。

### 5.1 shared-memory owner 和单 publisher claim

抽象功能描述：所有跨 sequence 的 observer 状态都放在 mem_access_base_sequence 的 static shared-memory owner 中；DCache sequence 只保留自己的 cached_line_by_addr，通过 claim 成为唯一 aggregate publisher。重复 claim 不清理旧状态、不覆盖快照。

源码位置：mem_base_sequence.sv，claim_dcache_observer_owner()、release_dcache_observer_owner()。

~~~systemverilog
if (!shared_mem_lifecycle_initialized) begin
    return 1'b0;
end
if (dcache_owner_claimed) begin
    return 1'b0;
end
dcache_owner_claimed    = 1'b1;
dcache_owner_generation++;
~~~

中文伪代码：claim 先确认 shared-memory lifecycle 已初始化，再确认没有现有 publisher；任一失败都不发布 aggregate。成功时只递增 owner generation、清零快照输入并标记 owner 身份。release 只使 aggregate published/owner_valid/ready 失效，不清除已经提交的 overlay/corrupt mask。

### 5.2 resident count 的事件驱动更新

抽象功能描述：在既有 map 写入/删除动作完成后，依据旧、新 alias_valid 的转换更新驻留计数；整体清表走专用旁路通知。observer 不替换 map 写入口，也不在每拍扫描 cached_line_by_addr。

源码位置：mem_base_sequence.sv，observe_dcache_line_transition()、observe_dcache_map_cleared()。

~~~systemverilog
if (!old_alias_valid && new_alias_valid) begin
    dcache_aggregate_snapshot.resident_line_count++;
end
else if (old_alias_valid && !new_alias_valid) begin
    if (dcache_aggregate_snapshot.resident_line_count == 0) begin
        dcache_aggregate_snapshot.observer_ready = 1'b0;
    end
    else begin
        dcache_aggregate_snapshot.resident_line_count--;
    end
end
~~~

中文伪代码：GrantAck 建立 active line 时记录 0->1 并加一；Grant wait、Probe(toN)、Release/ReleaseData 删除或失效 line 时记录 1->0 并减一；重复删除或下溢只把 observer 标成不可用。reset/整体清表通知把 resident 和未完成 fragment 清零，但不会回写 DCache map。

### 5.3 C-data assembly 和 corrupt byte mask

抽象功能描述：从 C response 首拍开始阻塞 drain，完整收齐后在既有路径判定 corrupt。corrupt 数据不伪造 overlay，而是把对应 64 B line 的 64 个 byte 标坏；该状态同时维护 1 KiB API 查询 mask 和 64 B line 粒度计数 map。

源码位置：mem_base_sequence.sv，observe_dcache_c_assembly_start()、observe_dcache_c_assembly_complete()、observe_dcache_corrupt_line()。

~~~systemverilog
dcache_line = line_addr[47:6];
if (!dcache_corrupt_byte_mask_by_line.exists(dcache_line)) begin
    dcache_corrupt_byte_mask_by_line[dcache_line] = 64'hffff_ffff_ffff_ffff;
    dcache_aggregate_snapshot.observed_corrupt_line_count++;
end
write_overlay_corrupt_byte_mask[line_addr[47:10]] |= line_mask;
~~~

中文伪代码：C-data 首拍设置 c_assembly_pending，所以 resident 已为零时仍不会发布 drain；assembly 完成后清除该过程态。若既有 C response 的 corrupt 位为高，observer 以 64 B DCache line 为 key 置全 1 mask，并把同一范围映射到 1 KiB overlay mask；重复观察同一 line 不重复增加 line 计数。

### 5.4 DCache fragment pending/commit

抽象功能描述：镜像既有 DCache writeback event 的入队和提交事实，确认同一 64 B line 的低、高 32 B fragment 都已经由原 overlay commit 路径观察到后，才清除整行 corrupt mask。observer 不创建 ticket、不改变 event 顺序。

源码位置：mem_base_sequence.sv，observe_dcache_write_enqueued()、observe_dcache_write_committed()。

~~~systemverilog
if (!dcache_fragment_pending_bytes.exists(dcache_line)) begin
    dcache_fragment_pending_bytes[dcache_line] = '0;
    dcache_fragment_committed_bytes[dcache_line] = '0;
    dcache_incomplete_fragment_line_count++;
end
dcache_fragment_pending_bytes[dcache_line] |= event_bytes;
~~~

中文伪代码：DCache event 入队时按地址把实际 byte 记入 pending mask；一条 line 第一次出现时增加未完成 line 计数。提交时只收集本次 event 触及的 line，更新 committed mask；若该 line 没有 pending 记录、commit mask 与 pending mask 不一致或计数下溢，observer 变为 unavailable。只有两张 mask 都覆盖完整 64 B 时才减未完成计数、清除对应 corrupt 旁路并删除 fragment 记录。

本轮阶段三把原来对全部 dcache_fragment_committed_bytes 的遍历改成 touched_line_q 局部检查；这保持事件驱动语义并避免大量 pending line 时每个 commit 都做全表扫描。

### 5.4.1 batch 入队与提交挂接点

抽象功能描述：在既有 shared-memory 写事件入队和提交完成点追加 observer 通知，使 observer 看到的顺序与 overlay 真正可见的顺序一致；通知本身不改变 batch 内容或提交边界。

源码位置：mem_base_sequence.sv，shared_mem_access_task()、commit_shared_mem_write_batch()。

~~~systemverilog
SHARED_MEM_WRITE_DCACHE: begin
    dcache_write_batch.push_back(write_event);
    observe_dcache_write_enqueued(write_event);
end
foreach (dcache_write_batch[i]) begin
    apply_shared_mem_write(dcache_write_batch[i]);
    observe_dcache_write_committed(dcache_write_batch[i]);
end
~~

中文伪代码：DCache store 真实完成后先按原逻辑进入 dcache_write_batch，再记录 pending；下一正常 sample 的既有 commit helper 先调用 apply_shared_mem_write 使 overlay 数据可见，随后记录该 event 已提交。Uncache 仍按既有顺序在 DCache batch 后提交并清理实际覆盖的 corrupt byte；API 不调用这个 helper。

### 5.5 Uncache 部分覆盖恢复

抽象功能描述：在既有 Uncache batch 已实际提交后，只清除该 store 的 byte mask 覆盖范围；只有一条 64 B corrupt mask 全部清零时才减少 corrupt line 计数。Uncache 不参与 DCache resident count。

源码位置：mem_base_sequence.sv，observe_uncache_write_committed()。

~~~systemverilog
if (dcache_corrupt_byte_mask_by_line.exists(dcache_line)) begin
    dcache_corrupt_byte_mask_by_line[dcache_line][dcache_byte_offset] = 1'b0;
    if (dcache_corrupt_byte_mask_by_line[dcache_line] == '0) begin
        dcache_corrupt_byte_mask_by_line.delete(dcache_line);
        dcache_aggregate_snapshot.observed_corrupt_line_count--;
    end
end
~~~

中文伪代码：逐个处理已经 commit 的 Uncache byte；该 byte 若位于 corrupt 范围就清除对应 bit。部分覆盖仍保持 corrupt，完整覆盖才删除 64 B map entry 并减少 aggregate 计数；随后统一重新计算 readiness。不会因为 Uncache 写入而伪造 DCache line 已被 Probe 清空。

## 6. readiness、reset 和 drain 生命周期

### 6.1 原子 aggregate snapshot 与统一 readiness

抽象功能描述：publish_dcache_aggregate_snapshot() 把 resident、pending、assembly、corrupt、owner 和 observer readiness 组合成一个值型快照；dcache_overlay_read_ready 从同一快照推导，避免 RM 自己拼接 live 字段。

源码位置：mem_base_sequence.sv，publish_dcache_aggregate_snapshot()。

~~~systemverilog
new_drain_complete =
    dcache_aggregate_snapshot.owner_valid &&
    dcache_aggregate_snapshot.observer_ready &&
    (dcache_aggregate_snapshot.resident_line_count == 0) &&
    (dcache_aggregate_snapshot.pending_writeback_count == 0) &&
    (dcache_incomplete_fragment_line_count == 0) &&
    !dcache_aggregate_snapshot.c_assembly_pending &&
    (dcache_aggregate_snapshot.observed_corrupt_line_count == 0);
dcache_aggregate_snapshot.dcache_overlay_read_ready =
    dcache_aggregate_snapshot.published &&
    dcache_aggregate_snapshot.dcache_drain_complete;
~~~

中文伪代码：先在当前 shared owner 中计算“无驻留、无 pending、无未完成 fragment、无 assembly、无 corrupt 且 observer 正常”的 drain 条件，再写入 snapshot。只有 snapshot 已发布且 drain 成功时才置 readiness；任何新 Acquire、C-data、batch、corrupt 或 observer 不一致都会使 ready 为零或使 API 返回 invalid。

### 6.2 drain transition metadata

抽象功能描述：只记录 drain 从未完成到完成的边沿时间，供诊断关联；它不声称是最近 overlay 写入时间。

源码位置：mem_base_sequence.sv，同一 publish_dcache_aggregate_snapshot() 中的 drain_epoch、drain_transition_sample 和 drain_transition_time 更新。

~~~systemverilog
if (dcache_aggregate_snapshot.published && new_drain_complete && !was_drain_complete) begin
    dcache_aggregate_snapshot.drain_epoch++;
    dcache_aggregate_snapshot.drain_transition_sample = shared_mem_sample_time;
    dcache_aggregate_snapshot.drain_transition_time   = $time;
end
~~~

中文伪代码：只有已发布快照从非 drain 变为 drain 时递增诊断代次并记录 sample/time；DONE 保持或 drain 保持期间不重复记录。clean toN 也可以触发该边沿，所以这些字段不能被解释为 overlay 最近提交时间。

### 6.3 reset 和 owner 退出

抽象功能描述：reset 期间使 DCache aggregate 暂不可读，reset 解除后的首个正常 sample 重新发布基线；不强制提交当前 sample。owner 退出只失效 aggregate，shared-memory lifecycle 仍管理已经提交的 backing/overlay。

源码位置：mem_base_sequence.sv，DCache responder body() 的 reset 分支以及 invalidate_dcache_runtime_observer()。

~~~systemverilog
if (reset_active) begin
    clear_runtime_state(1'b1);
    invalidate_dcache_runtime_observer();
    send_dcache_xaction(cycle_xact);
    service_cycle++;
    continue;
end
if (!mem_access_base_sequence::dcache_aggregate_snapshot.published) begin
    publish_dcache_owner_baseline();
end
~~~

中文伪代码：每个 reset 采样先按既有顺序结算上一正常 sample，再清理 DCache 私有 in-flight 状态并使 aggregate invalid；第一次从 published 进入 invalid 时递增 generation，reset 保持期间不重复递增。reset 解除后，主循环在处理新握手前发布基线。该 observer 操作不调用 batch commit helper、不改变 protocol ready/valid。

### 6.4 readiness 对外 API

抽象功能描述：get_dcache_overlay_readiness_for_rm() 是 RM 判断 overlay 读取时机的唯一 DCache 门槛；它只复制 {valid, ready}，不暴露 resident map 或内部计数。

源码位置：memblock_rm_readonly_api.sv，get_dcache_overlay_readiness_for_rm()。

~~~systemverilog
if (!mem_access_base_sequence::peek_dcache_aggregate_snapshot(snapshot)) begin
    return report_query_miss("dcache_overlay_readiness",
                             "DCache owner or observer snapshot is not published");
end
view.valid = 1'b1;
view.ready = snapshot.dcache_overlay_read_ready;
return 1'b1;
~~~

中文伪代码：若 owner 未 claim、snapshot 未发布或 observer 不可用，报一次 UVM_ERROR 并返回 valid=0；若 snapshot 有效但 DCache 尚未 drain，返回 valid=1、ready=0，不把正常等待误报成 miss。RM 只有在 valid=1 且 ready=1 时才自行调用 overlay 查询。

## 7. 只读性和错误语义检查

所有 API 返回的 view 都在函数入口清零并逐字段复制；没有 ref、inout、queue、associative array 或 live UVM object handle。统一错误行为如下：

1. owner/table/map/byte 不存在时产生一次 UVM_ERROR；
2. 输出保持 valid=0，不会用零值伪造数据；
3. 不调用创建型 getter、ensure_*、DUT memory-facing task 或 fatal getter；
4. corrupt 命中是保护性成功（valid=1、corrupt=1、data_valid=0），不是 miss；
5. API class 的 singleton 创建是唯一允许的创建动作。

静态检查已执行：

~~~text
rg -n "common_data_transaction::get\(|ensure_main_line\(|read_shared_mem_for_dut\(|get_or_create|uvm_fatal" memblock_rm_readonly_api.sv
~~~

中文伪代码：在 API 源码中逐项搜索创建型或 fatal 型入口；搜索结果为空，说明 API 不会把只读查询变成状态创建或仿真终止。read_memory_map() 的 lifecycle、exists 和 byte-valid 检查均在任何数组读取之前完成。

## 8. 与 Plan 的对齐

### 8.1 实现与 Plan 不一致项

整体架构与执行前 plan 一致：只有一个 API class；RM、checker、scoreboard 未修改；observer 是旁路；协议和 batch 顺序未改。

以下是 coding 中通过 IMPLEMENTATION_DELTA 明确记录的实现补充，不能倒推为原始 plan 已经写明：

| 项目 | 原始 plan 的抽象要求 | 当前实现 | 原因 |
|---|---|---|---|
| framework context | 只要求提供已有上下文的值型读取 | 新增 framework_context_view_t 和 read_framework_context_for_rm() | 把 RM 需要的生命周期标量统一收口，避免暴露 queue |
| 64 B corrupt 计数 | 要求 byte-granular corrupt 查询和 line 级 drain 计数 | 新增 dcache_corrupt_byte_mask_by_line，与 1 KiB API mask 并存 | 同一 1 KiB backing line 内多条 DCache line 时，单一 1 KiB map 无法准确维护 line count |
| reset 发布边界 | 要求 owner/runtime reset 管理 published/generation | reset 分支显式 invalidate，解除后首个正常 sample baseline；首次失效才递增 generation | 防止初始 reset 或 reset 保持期间产生过期/重复快照 |
| fragment observer 复杂度 | 只要求被动观察 fragment commit | 只检查当前 commit event 触及的 line，不遍历全部 fragment map | 保持事件驱动，不在高频路径引入全表扫描 |

上述补充只改变 observer 旁路状态，没有改变主体协议或 memory-facing 语义。

### 8.2 Plan 未说明但 Coding 落实的细节

- mem_access_base_sequence static 字段是 shared-memory owner 的实际存储点；common_data_transaction 不保存第二份 DCache aggregate。
- dcache_aggregate_snapshot 由同一个 helper 推导 dcache_drain_complete 和 dcache_overlay_read_ready，API 只复制它的结果。
- fragment 完成检查使用按 byte 的 64-bit mask，允许未来 Uncache 部分覆盖只清实际 byte；本次没有新增 ticket、version 或取消机制。
- observer 不一致（pending 下溢、未登记 commit、fragment map 缺失）只置 observer_ready=0，不回写 DCache 私有 map、batch 或主体 response。

## 9. 验证结果

### 9.1 静态与编译检查

- git diff --check：本任务三个代码/plan 文件无空白错误。
- VCS 增量编译重新解析 seq_pkg、mem_base_sequence.sv 和 memblock_rm_readonly_api.sv，通过 elaboration/link。
- 只读入口搜索未发现 common_data_transaction::get()、ensure_main_line()、read_shared_mem_for_dut()、get_or_create 或 uvm_fatal。

### 9.2 basicTest 既有拓扑仿真

| 场景 | 命令关键参数 | 结果 |
|---|---|---|
| 基础 virtual sequence | tc=basicTest ts=virtual_base_sequence mode=rm_api_stage3 partcmp_op=off wave=off | TEST_PASS；UVM_ERROR=0；UVM_FATAL=0 |
| real dispatch smoke | tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=rm_api_real_stage3 cfg=tc_dispatch_real_smoke partcmp_op=off wave=off | TEST_PASS；UVM_ERROR=0；UVM_FATAL=0 |
| store/writeback smoke | tc=tc_dispatch_real_store_wb_smoke ts=virtual_base_sequence mode=rm_api_store_stage3 cfg=tc_dispatch_real_store_wb_smoke partcmp_op=off wave=off | TEST CASE PASSED；UVM_ERROR=0；UVM_FATAL=0 |

这三个测试均未加入 RM 调用，符合“只封装 class、不实现 RM”的范围；它们验证的是 API/observer 加入后既有主流程结果和协议驱动仍可收敛。

### 9.3 环境问题记录

首次尝试 base_fun 时远端 VCS 报 VFS_SDB_ERROR，指出历史 tdc.sdb 数据库损坏；只清理本轮生成缓存后分区/elaboration 可继续。为避免远端遗留 base_fun partcomp 进程和缓存互相影响，本轮最终验证使用独立 mode、partcmp_op=off、wave=off，没有杀掉无关进程，也没有把该环境问题归因于本次代码。

## 10. 非本次修改的逻辑分析

当前 git status --short 中下列内容不是本 plan 的修改，本次没有 stage、回滚或解释其功能正确性：

| 类别 | 代表路径 | 处理方式 |
|---|---|---|
| L2TLB/RTL/fence 文档和源码 | AI_DOC/analysis/**、L2TLB plan/review、L2tlb_agent_agent_*、fence_agent_agent_monitor.sv | 保留用户现有修改，另行 review |
| 其他 dispatch/filelist | memblock_main_dispatch_auto_build_main_table_base_sequence.sv、seq.f、tc.f、tc_pkg.sv | 不混入本任务 commit |
| 其他 soft-test 与计划 | soft_test_l2tlb_range_lookup_*、其他 AI_DOC/plan/** | 不混入本任务 |
| 工具和远端产物 | .humanize/**、mem_ut/ver/ut/memblock/sim/.eda_remote/ | 仅保留工作区原状 |

历史风险评审文档 mem_ut_rm_readonly_api_dcache_residency_plan_risk_review_20260810.md 是 coding 前的风险记录，本 review 以当前三个 commit 和本执行 plan 为准；它没有被本次 commit 改写。

## 11. 剩余边界和风险

- 本次只承诺 basicTest 既有单 DCache owner 拓扑；第二个 responder claim 会报错并停止发布，未承诺多 DCache 实例共享 count。
- 仿真没有让 RM 真正调用 API，因此没有声称 checker 比较结果已验证；后续接入 RM 时必须单独覆盖 backing/overlay miss、corrupt 命中、reset 后 invalid 和两 fragment 完成边界。
- aggregate 只在 owner published 时有效；owner 退出后 readiness 返回 invalid，但 shared-memory lifecycle 仍允许读取已提交 overlay/backing，这是两个不同有效性域。
- observer 输入若缺失或自检不一致会返回 unavailable/UVM_ERROR，不会通过强制提交、懒分配或修复主体逻辑来“制造”可读结果。

这些是当前 plan 明确的范围边界，不构成本次 basicTest 执行 blocker；若未来要支持并发 owner、跨 testcase lifecycle 或 stale event/version 处理，应另建测试框架主体 plan。

## 12. 最终结论

实现满足本 plan 的核心目标：测试框架只增加一个统一的 RM 只读 API class，并以值型 view 暴露 dispatch、TLB、UID-TLB、初始化 backing、已提交 overlay、framework context 和 DCache readiness。DCache/Uncache 的旁路 observer 记录 resident、assembly、fragment、corrupt 和 drain 事实，但没有改写主体协议、sample 可见性或响应时序。三个阶段 commit 均已完成，文档已同步，指定 basicTest 验证通过；下一步仅需将 plan 按规则从 undo 归档到 do。
