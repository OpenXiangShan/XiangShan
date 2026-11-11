# CMO与统一响应错误RM集成实现复审（2026-11-11）

## 1. 术语与抽象功能说明及范围

| 英文术语 | 当前文档中的中文含义 | 代码落点 | 使用场景/示例 |
|---|---|---|---|
| RM / core | UVM实际适配层与独立纯值参考核心 | memblock_rm、rm_ls_model_t | adapter传入期望与actual，core唯一提交 |
| owner / UID / epoch / ROB / SQ / LQ | 动态指令及队列身份，按真实采样冻结 | program item、status与WB record | ROB复用后新epoch不能匹配旧STA0 |
| STA0 / LDA / WB | 第0路Store写回、Load写回及实际写回事实 | dut_writeback_observer | STA0没有replayInst字段 |
| token / source / generation / beat / sample / watermark | 单响应身份、实际总线来源编号及其占用代次、单次传输、全局采样与完整观察边界 | response history、path_observed_sample | 内部D消费15后预期通知17 |
| raw / denied / corrupt / AF / HE | 响应原始拒绝/损坏位、访问异常与hardwareError | independent response、expected_exception | 单响应denied优先于corrupt |
| CSR / state6 / latch | 顶层错误控制输入、原子真实门控状态及锁存 | cache_error_control_by_sample、atomic fact | 原子state6使用两拍前顶层输入 |
| CMO / CBO / zero / nonzero | 四类最终缓存维护操作；zero写零，其余无数据效果 | rm_ls_decode_cmo_final_op、cmo_op | clean/flush/inval按READ检查权限 |
| cacheable / Uncache / NC / MMIO | 缓存路径、非缓存接口、后台普通非缓存及同步强序路径 | uncache_path、uncache_nc_path | NC提交不等响应，MMIO等STA0 |
| PBMT / PMA / PMP / PF / GPF | 页内存属性、物理属性/权限、页异常和客户页异常 | 翻译与PMA/PMP只读上下文 | PBMT-NC覆盖正常PMA缓存属性 |
| overlay / entry / SC shadow | RM已提交字节覆盖、8B效果条目和后续写覆盖失败SC义务 | expected_store_byte、expected_store_entries | zero分段通过既有底座保留三项副作用 |
| redirect / flushPipe / terminal | 框架恢复、真实WB属性与合法完成终态 | status、STA0、completion actual | nonzero正常可有flushPipe=1，RM不生成恢复 |
| FIFO / observer / pending / poll | 顺序采样队列、被动观察器、缺当前事实等待及定期查询 | cmo_path_fifo、observer_pending_evidence | sample未齐待下一poll，确定缺失不能当CLEAN |
| provenance / audit / late normal | 完整动态来源、持续终态审计、fault后的唯一normal尾拍 | cmo_terminal_by_rob、audit_cmo_sta0 | fault后normal必须匹配SQ、epoch、issue/replay/flush来源 |
| Prefetch / GrantData / mock | 软件/硬件预取、真实带数据响应及软件测试替身 | query_prefetch、validate_prefetch_grant | 硬件预取逐beat核对，不创建UID |
| bootstrap / topology / thin testcase | 场景初始化、控制worker拓扑与直接调用真实功能的薄测试入口 | basicTest、定向vseq、soft testcase | 专项测试复用生产比较，不放宽生产gate |

关联三个方案：`AI_DOC/plan/test_framework/plan/do/memblock_l2_grantdata_first_line_weighted_error_plan_20261110.md`、`AI_DOC/plan/rm_plan/do/memblock_nonvector_d_error_path_classification_plan_20261110.md`、`AI_DOC/plan/rm_plan/do/memblock_cmo_blackbox_rm_coding_plan_20261103.md`。基线b9c65cf260，框架阶段提交7e334444f1。本文覆盖root对memblock_rm.sv、WB observer、sync CSR输入历史、CSR monitor和thin testcase入口的实际集成；请求/数据底座、DCache来源和Uncache详细实现分别见 `memblock_cmo_core_request_implementation_review_20261111.md`、`memblock_cmo_path_dcache_error_implementation_review_20261111.md`、`memblock_uncache_error_implementation_review_20261111.md`；首次行历史另见 `AI_DOC/plan/test_framework/review_doc/undo/memblock_l2_line_history_implementation_review_20261111.md`。

## 2. 修改前后、字段与调用关系

修改前CMO因scalar size为0被归OTHER；Load/原子以永久PA错误摘要预测异常，原子未读取独立enable；Uncache通知未进入独立比较。当前CMO在分类前置，权限命令和Store外显类别分离；实际STA0、请求、响应与终态交由唯一core提交。DCache错误只从具体实际消费来源推导，Uncache后台服务保持独立，不阻塞NC程序提交。

新增RM对象cmo_requests/dcache_errors/uncache_errors在build初始化，唯一FIFO由env创建并绑定，poll服务每拍只消费新增批次。path_observed_sample为完整历史水位；observer_pending_evidence仅等待尚未齐备的证据，256次poll限制缺失完成的无界等待。程序正常/合法fault推进后释放DCache完成fact，保留请求终态审计以捕获迟到违法请求。

sync_pkg新增逐sample控制输入值记录，CSR monitor是唯一发布者；reset清当前代记录。记录包含有效、enable、reset epoch和sample。RM不解析payload偏移或读取最新控制冒充历史。成功终态登记时仅按单ROB键补审已保存的尾拍，后续按实际新增事件审计；正常路径按队首、owner键及固定物理端口处理。

## 3. 适配层核心实现

### 3.1 同拍被动事实服务

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_service_path_facts()`。

抽象功能描述：唯一消费内部观察FIFO并向三个职责对象发布相同批次，使RM查询建立在已完成的采样上。

```systemverilog
function void memblock_rm::observer_service_path_facts();
    cmo_path_agent_xaction tr;
    while(cmo_path_fifo!=null && cmo_path_fifo.try_peek(tr)) begin
        // 同拍producer均发布完后才建立跨monitor关联，结果不依赖UVM task先后。
        if(!tr.reset_active && tr.sample_seq>=memblock_sync_pkg::dut_sample_seq) break;
        if(!cmo_path_fifo.try_get(tr)) break;
        dcache_errors.observe_batch(tr);
        uncache_errors.observe_batch(tr);
        cmo_requests.observe_batch(tr);
        path_observed_sample=tr.sample_seq;
    end
    uncache_errors.check_until(path_observed_sample);
endfunction
```

中文伪代码：只查看队首；本拍尚未完成时等待下一sample；按序取出已完整批次，分别推进DCache来源、Uncache请求通知和CMO请求；最后按消费水位比较后台通知。该服务不产生redirect、不推进框架owner。

### 3.2 CMO响应错误统一入口

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_apply_cmo_d_error_expectation()`。

抽象功能描述：按独立路径取得响应错误，cacheable zero保持生产端正常，nonzero必须绑定确切CBOAck owner。

```systemverilog
function bit memblock_rm::observer_apply_cmo_d_error_expectation(
    rm_ls_program_item_t item, longint unsigned access_sample);
    mem_access_base_sequence::l2_response_history_t response;
    bit delivered;
    string detail;
    if(item.cmo_pre_access_exception!=0) return 1;
    if(item.cmo_path==RM_CMO_PATH_UNCACHE_ZERO) begin
        if(!uncache_errors.validate_cmo_zero(item,access_sample,detail)) begin
            observer_pending_evidence=uncache_errors.observation_pending();
            ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,detail); return 0;
        end
        return 1;
    end
    if(item.cmo_path==RM_CMO_PATH_CACHEABLE_ZERO) return 1;
    if(!mem_access_base_sequence::query_l2_response_history(item.cmo_request_token,
        48'(item.pa_by_byte[0]),access_sample,2'b01,response,delivered) ||
       !response.valid || !delivered || !response.full_complete ||
       !response.owner.valid || response.owner.uid!=item.uid ||
       response.owner.dynamic_epoch!=item.dynamic_epoch ||
       response.owner.rob_flag!=item.rob.flag || response.owner.rob_value!=item.rob.value[7:0] ||
       response.owner.sq_flag!=item.sq_flag || response.owner.sq_value!=item.sq_value) begin
        ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,"CMO缺少owner匹配的实际CBOAck响应"); return 0;
    end
    if(response.beat0.denied) item.expected_exception[7]=1;
    else if(response.beat0.corrupt) item.expected_exception[19]=1;
    return 1;
endfunction
```

中文伪代码：前置异常直接保留；Uncache zero调用唯一分段错误与效果验证；cacheable zero没有L2响应错误；nonzero以请求token、物理行、sample及完整owner查询实际完成beat，denied先映射Store AF，仅corrupt映射hardwareError。

### 3.3 CMO最终STA0采集

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_capture_cmo_actual()`。

抽象功能描述：将独立请求与响应证据同真实最终STA0和合法normal/fault终态关联，再传入纯值core。

```systemverilog
function bit memblock_rm::observer_capture_cmo_actual(
    memblock_rm_readonly_api::main_transaction_view_t main_view,
    rm_ls_program_item_t item);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::dut_writeback_view_t wb;
    memblock_rm_readonly_api::status_view_t status_v;
    rm_cmo_completion_actual_t actual;
    string detail;
    bit duplicate;
    ro=memblock_rm_readonly_api::get();
    if(!ro.read_dut_cmo_writeback_for_rm(item.uid,
        '{flag:main_view.rob_idx_flag,value:main_view.rob_idx_value},
        item.dynamic_epoch,wb,duplicate)) begin
        if(duplicate) begin
            ls_model.set_error(RM_LS_ERR_CMO_DUPLICATE_COMPLETION,"CMO真实STA0重复"); return 0;
        end
        ls_model.set_error(RM_LS_ERR_CMO_ACTUAL_NOT_READY,
            $sformatf("uid=%0d 缺少当前owner/epoch唯一最终STA0",item.uid));
        observer_pending_evidence=1; return 0;
    end
    // STA接口没有replayInst字段；最终性由实际STA0与owner/终态证明。
    if(!ro.read_status_for_rm(item.uid,status_v) || !wb.exception_valid ||
       !wb.flush_pipe_valid) begin
        ls_model.set_error(RM_LS_ERR_CMO_ACTUAL_NOT_READY,"CMO STA0最终属性不完整"); return 0;
    end
    if(item.cmo_path==RM_CMO_PATH_UNCACHE_ZERO && item.cmo_pre_access_exception==0) begin
        if(!cmo_requests.validate_no_requests(item,wb.sample_seq,detail)) begin
            observer_pending_evidence=cmo_requests.observation_pending();
            ls_model.set_error(RM_LS_ERR_CMO_REQUEST_MISMATCH,detail); return 0;
        end
        if(!observer_apply_cmo_d_error_expectation(item,wb.sample_seq)) return 0;
    end else begin
        if(!uncache_errors.validate_no_cmo_requests(item,wb.sample_seq,detail)) begin
            observer_pending_evidence=uncache_errors.observation_pending();
            ls_model.set_error(RM_LS_ERR_CMO_REQUEST_MISMATCH,detail); return 0;
        end
        if(!cmo_requests.validate(item,wb.sample_seq,detail)) begin
            observer_pending_evidence=cmo_requests.observation_pending();
            ls_model.set_error(RM_LS_ERR_CMO_REQUEST_MISMATCH,detail); return 0;
        end
        if(!observer_apply_cmo_d_error_expectation(item,wb.sample_seq)) return 0;
    end
    actual=new(); actual.valid=1; actual.uid=item.uid; actual.rob=item.rob;
    actual.sq_flag=item.sq_flag; actual.sq_value=item.sq_value;
    actual.dynamic_epoch=item.dynamic_epoch; actual.fu_op_type=item.op;
    actual.source_sta0=wb.source_kind==2 && wb.lane==0;
    actual.is_final=1; actual.flush_pipe=wb.flush_pipe;
    actual.exception_vec=wb.exception_vec; actual.sample_seq=wb.sample_seq;
    actual.normal_pass=status_v.sta_pass && !status_v.sta_fault;
    actual.fault_terminal=status_v.sta_fault && status_v.terminal_done;
    return ls_model.push_cmo_actual(actual);
endfunction
```

中文伪代码：读取当前UID、epoch、ROB及flush代际唯一STA0，重复立即失败；检查实际异常和flush属性。按路径排除其它错误请求集合并验证本路径请求，统一计算响应错误。复制STA0/owner/终态为值型actual并由core保存；不等待或消费redirect。STA接口没有replay字段，不能把该字段valid为0误当缺失最终写回。

### 3.4 原子错误门控

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_apply_atomic_d_error_expectation()`。

抽象功能描述：根据真实最终原子消费的独立两项raw和对应CSR输入预测异常。

```systemverilog
function bit memblock_rm::observer_apply_atomic_d_error_expectation(
    rm_ls_program_item_t item, longint unsigned access_sample
);
    bit [1:0] raw;
    bit proven;
    longint unsigned control_sample;
    string detail;
    memblock_sync_pkg::memblock_cache_error_control_t control;
    if(item==null || item.kind!=RM_LS_KIND_ATOMIC || item.expected_exception!=0) return 1;
    if(!item.all_normal_cacheable || item.uncache_path || access_sample==0) begin
        ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,"原子错误消费路径缺失"); return 0;
    end
    if(!dcache_errors.query(item.uid,item.dynamic_epoch,
        {item.rob.flag,item.rob.value[7:0]},8'b0,access_sample,1,
        raw,control_sample,proven,detail)) begin
        observer_pending_evidence=1; return 0;
    end
    if(!proven || !memblock_sync_pkg::read_cache_error_control(control_sample,control)) begin
        ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,
            $sformatf("uid=%0d 原子独立raw或CSR证据缺失：%s",item.uid,detail)); return 0;
    end
    item.expected_dcache_denied=raw[1]; item.expected_dcache_corrupt=raw[0];
    if(control.enable) begin
        if(raw[1]) item.expected_exception[item.atomic_kind==RM_ATOMIC_LR ? 5 : 7]=1;
        else if(raw[0]) item.expected_exception[19]=1;
    end
    return 1;
endfunction
```

中文伪代码：翻译或权限已异常则不附加无关下游错误。读取独立最终消费及门控所用顶层输入sample，缺证据明确失败。原始位始终检查；enable为1时denied按LR或写类映射5/7，否则仅corrupt映射19；enable为0保留正常数据和写效果。内部state6采样相对于顶层CSR有两拍延迟，query提供已扣除延迟的输入sample。

### 3.4a 软件预取错误抑制

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_check_prefetch_d_error_suppression()`。

抽象功能描述：以实际LDA及被动软件预取资格比较同步异常抑制，后台GrantData另行独立校验。

```systemverilog
function bit memblock_rm::observer_check_prefetch_d_error_suppression(rm_ls_program_item_t item);
    memblock_rm_readonly_api::dut_writeback_view_t wb;
    memblock_rob_key_t rob_key;
    memblock_rm_readonly_api ro;
    bit proven;
    string detail;
    ro=memblock_rm_readonly_api::get();
    rob_key='{flag:item.rob.flag,value:item.rob.value};
    if(!ro.read_dut_load_writeback_for_rm(rob_key,wb)) begin
        observer_pending_evidence=1;
        ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,"软件预取缺少当前ROB的真实LDA终态");return 0;
    end
    if(!wb.valid || !wb.rob_valid || !wb.rob_flag_valid || !wb.exception_valid ||
        wb.source_kind!=1 || wb.rob_flag!=item.rob.flag || wb.rob_value!=item.rob.value ||
        wb.replay_inst || wb.flush_pipe || wb.sample_seq==0) begin
        ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,"软件预取LDA终态身份或属性不匹配");return 0;
    end
    if(!dcache_errors.query_prefetch(item.uid,item.dynamic_epoch,
        {item.rob.flag,item.rob.value[7:0]},{item.lq_flag,item.lq_value},wb.sample_seq,proven,detail)) begin
        observer_pending_evidence=1;return 0;
    end
    if(!proven || !rm_dcache_error_observer::prefetch_suppression_matches(wb.exception_vec)) begin
        ls_model.set_error(RM_LS_ERR_EXCEPTION_MISMATCH,
            $sformatf("软件预取响应错误抑制失败 uid=%0d sample=%0d exception=0x%h detail=%s",
                item.uid,wb.sample_seq,wb.exception_vec,detail));return 0;
    end
    `uvm_info("RM_PREFETCH_COMPARE",$sformatf("uid=%0d epoch=%0d sample=%0d 响应错误同步抑制PASS，无数据效果比较",
        item.uid,item.dynamic_epoch,wb.sample_seq),UVM_LOW)
    return 1;
endfunction
```

中文伪代码：要求当前ROB的真实LDA且无replay/flush，按UID、epoch、ROB、LQ和sample查询prefetch终态资格；位5和19必须被抑制，缺来源在水位齐备后失败；不读取普通Load backing或生成硬件预取UID。

### 3.4b 完成后的增量重复审计

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_process_current_commit()`。

抽象功能描述：纯值core成功后注册当前终态，使游标离开该UID后仍能发现重复STA0。

```systemverilog
    if(compare_result && item.kind==RM_LS_KIND_CMO &&
       !ro.mark_dut_cmo_completion_consumed_for_rm(item.uid,
            '{flag:item.rob.flag,value:MEMBLOCK_ROB_VALUE_W'(item.rob.value)},item.dynamic_epoch))
        `uvm_error("RM_CMO_DUPLICATE_COMPLETION","CMO提交后终态审计注册失败")
    if(ls_model.current_uid!=uid_before) begin
        dcache_errors.release_completed_owner(item.uid,item.dynamic_epoch);
        if(item.kind==RM_LS_KIND_CMO && item.cmo_pre_access_exception==0 &&
           item.cmo_path!=RM_CMO_PATH_UNCACHE_ZERO)
            cmo_requests.mark_consumed(item);
    end

```

中文伪代码：只对成功CMO登记一次终态owner；WB observer按ROB键补审已捕获尾拍并检查未来STA0；fault后完整来源匹配的一次normal尾拍保持既有允许边界，重复fault或第二normal报错。随后释放完成DCache事实并标记请求已消费，不产生框架控制。

### 3.5 Load独立错误贡献

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_apply_load_d_error_expectation()`。

抽象功能描述：保持Load正常数据与错误输入资格独立，由被动消费来源得到两个预期异常位。

```systemverilog
    begin
        bit expected_af, expected_he, proven;
        longint unsigned control_sample;
        string detail;
        if (!dcache_errors.query_load_exception_bits(
                item.uid,item.dynamic_epoch,{item.rob.flag,item.rob.value[7:0]},
                {item.lq_flag,item.lq_value},access_sample,
                expected_af,expected_he,control_sample,proven,detail)) begin
            observer_pending_evidence=1; return 0;
        end
        if (!proven) begin
            ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,
                $sformatf("uid=%0d DCache消费来源缺失：%s",item.uid,detail)); return 0;
        end
        // Load各独立输入分别denied优先后合并，允许不同来源的AF与HE同时存在。
        item.expected_dcache_denied=expected_af;
        item.expected_dcache_corrupt=expected_he;
        item.expected_exception[5] |= expected_af;
        item.expected_exception[19] |= expected_he;
    end
    return 1'b1;

```

中文伪代码：按完整owner及实际WB上界查询；水位尚未覆盖则延迟，已覆盖但来源缺失则失败；不同有效来源各自denied优先后合并AF与HE，允许一项denied和另一项corrupt同时贡献两个异常，不用永久PA摘要判断。Uncache Load仍使用独立完成历史。

### 3.6 跨页首段错误

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_try_apply_first_split_data_error()`。

抽象功能描述：先证明框架已观察到真实首段，再用独立来源证明该段终止后页翻译。

```systemverilog
function bit memblock_rm::observer_try_apply_first_split_data_error(
    rm_ls_program_item_t item,
    int unsigned prefix_bytes,
    output bit deferred
);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::pma_pmp_af_view_for_rm_t byte_view;
    memblock_rm_readonly_api::dut_writeback_view_t load_view;
    bit expected_af, expected_he, proven;
    longint unsigned control_sample;
    string detail;
    memblock_first_split_access_context_t context_view;
    memblock_rob_key_t rob_key;

    deferred = 1'b0;
    if (prefix_bytes == 0 || prefix_bytes >= item.size_bytes ||
        !seq_csr_common::get_pma_pmp_model_en()) return 1'b0;
    ro = memblock_rm_readonly_api::get();
    for (int unsigned idx = 0; idx < prefix_bytes; idx++) begin
        if (!item.pa_valid_mask[idx] ||
            item.pa_by_byte[idx][47:6] != item.pa_by_byte[0][47:6] ||
            !ro.read_pma_pmp_af_for_rm(item.uid, item.dynamic_epoch, 1'b1,
                                      item.pa_by_byte[idx][47:0], 1, PMA_PMP_CMD_READ, byte_view) ||
            !byte_view.valid || !byte_view.translation_eligible || !byte_view.af_decided ||
            !byte_view.normal_cacheable || byte_view.ld_access_fault) return 1'b0;
    end
    rob_key = '{flag: item.rob.flag, value: item.rob.value};
    if (!ro.read_dut_load_writeback_for_rm(rob_key, load_view) ||
        !load_view.valid || !load_view.rob_valid || !load_view.rob_flag_valid ||
        load_view.rob_flag !== rob_key.flag || load_view.rob_value !== rob_key.value ||
        load_view.source_kind != 2'd1 || load_view.sample_seq == 0 ||
        load_view.replay_inst || load_view.flush_pipe) return 1'b0;
    if (!ro.try_read_first_split_access_context_for_rm(
            item.uid, item.dynamic_epoch, rob_key, item.computed_vaddr,
            load_view.sample_seq, context_view, deferred)) return 1'b0;
    if (!dcache_errors.query_first_split_error(
            item.uid, item.dynamic_epoch, {item.rob.flag,item.rob.value[7:0]},
            {item.lq_flag,item.lq_value},item.pa_by_byte[0],prefix_bytes,
            context_view.data_sample_seq,context_view.response_sample_seq,
            load_view.sample_seq,expected_af, expected_he, control_sample, proven, detail)) begin
        deferred=1; return 0;
    end
    if (!proven || (!expected_af && !expected_he)) return 0;
    item.expected_dcache_denied=expected_af;
    item.expected_dcache_corrupt=expected_he;
    item.expected_exception[5] |= expected_af;
    item.expected_exception[19] |= expected_he;
    `uvm_info("RM_LS_FIRST_SPLIT_DATA_ERROR",
              $sformatf("uid=%0d epoch=%0d first-page consumed AF=%0d HE=%0d before later translation",
                        item.uid,item.dynamic_epoch,expected_af,expected_he),UVM_LOW)
    return 1'b1;
endfunction
```

中文伪代码：确认首页有效PA及正常cache权限；取得同ROB真实WB与首段结构上下文；通过首段专用错误fact核对UID/epoch/ROB/LQ、PA覆盖、prefix字节数、实际data/response采样和WB上界；仅实际错误可提前停止第二页期望构造。不得按行历史授权异常。

### 3.7 CMO分类与地址

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_build_commit_item_with_observation()`。

抽象功能描述：在size零提前返回和普通Store数据构造前识别最终CMO并拒绝矛盾类型。

```systemverilog
    item.dynamic_epoch = status_view.dynamic_epoch;
    item.sq_flag = main_view.sq_idx_flag;
    item.sq_value = main_view.sq_idx_value;
    cmo_access = main_view.fu_type == MEMBLOCK_FUTYPE_STU &&
                 rm_ls_decode_cmo_final_op(main_view.fu_op_type) != RM_CMO_OP_NONE;
    if((main_view.op_class==MEMBLOCK_OP_CLASS_CBO) != cmo_access) begin
        ls_model.set_error(RM_LS_ERR_CMO_CLASSIFICATION,
            $sformatf("uid=%0d CBO op_class/fu_type/final op不一致",item.uid)); return 0;
    end
    if (cmo_access) begin
        item.cmo_op = rm_ls_decode_cmo_final_op(main_view.fu_op_type);
        void'(rm_ls_assign_cmo_data_semantics(item));
    end
    atomic_access = main_view.fu_type == MEMBLOCK_FUTYPE_MOU &&
                    memblock_op_behavior_util::is_amo_fuoptype(main_view.fu_op_type);
    // CMO只翻译请求地址；1B仅作为复用翻译循环的局部几何，返回前恢复CMO的0宽度。
    item.size_bytes = cmo_access ? 1 : atomic_access ? observer_atomic_size(main_view.fu_op_type) :
                                      observer_scalar_size(main_view.fu_op_type);
    item.computed_vaddr = atomic_access ? main_view.src_0 :
                                          observer_compute_vaddr(main_view.src_0,
                                                                 main_view.imm);

```

中文伪代码：冻结SQ和动态epoch；STU与最终CBO编码必须同CBO op_class一致；调用统一语义helper。复用翻译循环时仅临时使用1B地址几何，CMO最终size恢复0；地址仍用src0加符号扩展imm，不读取无意义STD数据。

### 3.8 权限命令和异常类别

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_build_commit_item_with_observation()`。

抽象功能描述：zero检查WRITE，nonzero检查READ，全部CMO外显Store类异常。

```systemverilog
    store_access = (cmo_access && item.cmo_permission_write) ||
                   item.kind == RM_LS_KIND_STORE ||
                   (item.kind == RM_LS_KIND_ATOMIC &&
                    item.atomic_kind != RM_ATOMIC_LR);
    exception_store_access = cmo_access || store_access;
    misaligned = (item.computed_vaddr & (item.size_bytes - 1)) != 0;
    if (item.kind == RM_LS_KIND_ATOMIC) begin
        pma_pmp_cmd = item.atomic_kind == RM_ATOMIC_LR ?
                      PMA_PMP_CMD_ATOM_READ : PMA_PMP_CMD_ATOM_WRITE;
    end else begin
        pma_pmp_cmd = store_access ? PMA_PMP_CMD_WRITE : PMA_PMP_CMD_READ;
    end

```

中文伪代码：计算permission访问布尔量时仅zero按写，另设exception_store_access将所有CMO归Store外显；PMA/PMP及页权限使用前者，最终异常位使用后者，不能从异常名反推权限命令。

### 3.9 前置异常和路径冻结

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_build_commit_item_with_observation()`。

抽象功能描述：从独立翻译/PMA/PBMT结果确定预期路径和无请求异常边界。

```systemverilog
    if (cmo_access) begin
        // 前置错误独立于CMO响应；它要求没有下游请求与数据效果。
        if (all_bytes_pma_mmio && item.expected_exception == '0)
            item.expected_exception[7] = 1'b1;
        item.cmo_pre_access_exception = item.expected_exception;
        item.cmo_path = item.cmo_op != RM_CMO_OP_ZERO ? RM_CMO_PATH_DCACHE_CMO :
                        item.uncache_path ? RM_CMO_PATH_UNCACHE_ZERO :
                                            RM_CMO_PATH_CACHEABLE_ZERO;
        item.size_bytes = 0;
        if(!observer_trace_cmo_emitted.exists({32'(item.uid),32'(item.dynamic_epoch)})) begin
        observer_trace_cmo_emitted[{32'(item.uid),32'(item.dynamic_epoch)}]=1;
        `uvm_info("RM_CMO_TRANSLATION",
                  $sformatf("uid=%0d op=%0d permission=%s path=%0d exception=0x%0h",
                            item.uid, item.cmo_op,
                            item.cmo_permission_write ? "WRITE" : "READ",
                            item.cmo_path, item.expected_exception), UVM_LOW)
        end
    end

```

中文伪代码：PMA实际MMIO使CMO前置StoreAF；不能单凭PBMTIO添该AF。nonzero固定DCache CMO路径，zero按独立属性选cacheable或Uncache。冻结前置异常并恢复CMO零size；后续请求只能匹配该预期路径。

### 3.10 最终审计

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`check_phase()`。

抽象功能描述：终场检查程序游标、剩余请求和后台通知，无遗漏不能被正常结束掩盖。

```systemverilog
function void memblock_rm::check_phase(uvm_phase phase);
    super.check_phase(phase);
    observer_service_path_facts();
    uncache_errors.final_check(path_observed_sample);
    void'(cmo_requests.check_drained());
    if(ls_model.configured && ls_model.current_uid!=observer_main_count)
        `uvm_error("RM_PROGRAM_NOT_DRAINED",
            $sformatf("RM仍有未比较指令 current=%0d count=%0d",ls_model.current_uid,observer_main_count))
    if(observer_check_store_pending)
        `uvm_error("RM_STORE_NOT_DRAINED","最终Store-table比较尚未完成")
endfunction
```

中文伪代码：先消费完整事实批次并执行Uncache最终通知检查和CMO请求drain；核对RM已比较全部主表UID以及没有待完成最终Store-table检查。只终场检查，不额外发流量。


### 3.11 同步MMIO与NC的根接入分流

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv:1571`，函数/落点：`observer_capture_commit_actual()`。

抽象功能描述：在程序提交比较前选择软件预取、CMO或同步MMIO的真实完成校验；NC继续原架构提交路径，后台通知由独立服务负责。

```systemverilog
    if(main_view.fu_type==MEMBLOCK_FUTYPE_LDU &&
        memblock_op_behavior_util::is_prefetch_fuoptype(main_view.fu_op_type))
        return observer_check_prefetch_d_error_suppression(item);
    if (item.kind == RM_LS_KIND_CMO)
        return observer_capture_cmo_actual(main_view,item);
    if (item.kind == RM_LS_KIND_STORE && item.uncache_path &&
        !item.uncache_nc_path && item.expected_exception==0) begin
        memblock_rm_readonly_api::dut_writeback_view_t store_view;
        bit duplicate;
        string detail;
        if (!ro.read_dut_cmo_writeback_for_rm(item.uid,
            '{flag:main_view.rob_idx_flag,value:main_view.rob_idx_value},
            item.dynamic_epoch,store_view,duplicate)) begin
            if(duplicate) begin
                ls_model.set_error(RM_LS_ERR_EXCEPTION_MISMATCH,"MMIO Store最终STA0重复"); return 0;
            end
            ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,
                $sformatf("uid=%0d MMIO Store缺少当前owner/epoch唯一最终STA0",item.uid));
            observer_pending_evidence=1; return 0;
        end
        if(!uncache_errors.apply_store_expectation(item,store_view.sample_seq,detail)) begin
            observer_pending_evidence=uncache_errors.observation_pending();
            ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,detail); return 0;
        end
        if(store_view.exception_vec !== item.expected_exception) begin
            ls_model.set_error(RM_LS_ERR_EXCEPTION_MISMATCH,"MMIO Store STA0异常与独立响应不匹配"); return 0;
        end
    end
```

中文伪代码：本段先分发软件预取到LDA抑制检查，再分发CMO到唯一STA0/请求/响应入口。只有普通Store且独立判为Uncache、非NC、无前置异常时，要求owner匹配的唯一STA0；重复失败，尚未观察到则设置pending。apply_store_expectation验证真实SQ/TL请求、raw与后端写接受，返回失败时仅沿其水位状态决定等待；成功后将独立异常与STA0严格比较。NC不进入该同步分支，后续core保留普通已提交Store overlay；前置异常也不产生伪Uncache响应义务。


### 3.12 独立路径属性与NC分类冻结

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv:1364`，函数/落点：`observer_build_commit_item_with_observation()`。

抽象功能描述：从翻译/PBMT/PMA结果固定cacheable、Uncache及NC分类，为同步异常和后台Store分工提供独立依据。

```systemverilog
    item.translation_valid = 1'b1;
    item.all_pma_mmio = all_bytes_pma_mmio;
    if (!seq_csr_common::get_pma_pmp_model_en())
        all_bytes_normal_cacheable = all_bytes_pma_pbmt;
    // PMA normal-cacheable只描述底层物理区域；PBMT-NC/IO会覆盖该属性并把
    // 实际访问路由到Uncache。这里冻结最终有效路径，避免合法NC Load同时
    // 被标成Uncache和normal-cacheable。
    item.all_normal_cacheable = all_bytes_pma_pbmt &&
                                all_bytes_normal_cacheable;
    // effective PBMT非零的合法Load由Uncache处理；PBMT=PMA时则依据逐byte
    // PMA分类在PMA-MMIO与normal cacheable之间选择。
    item.uncache_path = all_bytes_pma_mmio || !all_bytes_pma_pbmt;
    item.uncache_nc_path = first_effective_pbmt_valid && first_effective_pbmt==2'd1;
```

中文伪代码：完成PA几何检查后标记翻译有效，保存全部字节PMA-MMIO结论。PMA模型关闭时用已有PBMT结果收敛normal-cacheable；正常cacheable要求PBMT仍为PMA且底层物理区域可缓存。PMA-MMIO或有效PBMT非PMA进入Uncache；首个有效PBMT为NC时冻结uncache_nc_path。该值不从异常向量、TL Put或最终响应倒推，随后同步Store分流和NC正常core提交读取同一份值。


### 3.13 CMO合法fault的程序提交资格

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv:2112`，函数/落点：`observer_process_current_commit()`。

抽象功能描述：允许合法fault CMO在框架terminal成立后完成RM比较，避免把恢复中的状态误当normal完成或遗漏fault。

```systemverilog
    observer_pending_evidence=0;
    if (!ro.read_status_for_rm(ls_model.current_uid, status_view) ||
        !status_view.valid) return 1'b0;
    if (!ro.read_main_transaction_for_rm(ls_model.current_uid, main_view) || !main_view.valid)
        return 1'b0;
    if (!status_view.rob_commit &&
        !(main_view.fu_type==MEMBLOCK_FUTYPE_STU &&
          rm_ls_decode_cmo_final_op(main_view.fu_op_type)!=RM_CMO_OP_NONE &&
          status_view.terminal_done && status_view.sta_fault)) return 0;
    // fault的rob_commit可早于合法terminal；终态未成立时不提交CMO快照。
    if(main_view.fu_type==MEMBLOCK_FUTYPE_STU &&
       rm_ls_decode_cmo_final_op(main_view.fu_op_type)!=RM_CMO_OP_NONE &&
       status_view.sta_fault && !status_view.terminal_done) return 0;
```

中文伪代码：每次检查当前UID先清本次pending；status或main尚不可读则等待。普通指令仍要求rob_commit；CMO仅在STU与最终CBO编码匹配、terminal_done且sta_fault时允许无rob_commit的合法fault入口。若fault已见但terminal未成立，即使rob_commit较早置位仍返回等待。这里只读取状态，不发送redirect或构造新epoch。


### 3.14 CMO恢复元数据与普通指令边界

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv:2157`，函数/落点：`observer_process_current_commit()`。

抽象功能描述：保留普通指令的stale恢复状态拒绝，同时让CMO交给专用owner/STA/terminal契约验证。

```systemverilog
    if (!(main_view.fu_type==MEMBLOCK_FUTYPE_STU &&
          rm_ls_decode_cmo_final_op(main_view.fu_op_type)!=RM_CMO_OP_NONE) &&
        (status_view.replay_pending || status_view.redirect_pending ||
         status_view.issue_killed || status_view.flushed)) begin
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMMIT",
                       $sformatf("uid %0d commit status still carries stale replay/flush state",
                                 main_view.uid))
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end
```

中文伪代码：非CMO若仍带replay_pending、redirect_pending、issue_killed或flushed，记录一次RM_COMMIT错误并置全局阻塞，不推进程序游标。CMO不复用这组普通Load限制；其实际请求和STA0、前置异常与合法normal/fault终态仍由前后专用检查决定，不能仅因CMO跳过该条件就视为成功。


### 3.15 尚未齐备事实的有界等待

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv:2196`，函数/落点：`observer_process_current_commit()`。

抽象功能描述：为同拍跨monitor事实提供有限等待；完整水位下的确定缺证据立即失败，防止缺失写回或请求被永久轮询掩盖。

```systemverilog
    missing_dut_load_writeback = 1'b0;
    if (!observer_capture_commit_actual(main_view, item,
                                        missing_dut_load_writeback)) begin
        if(observer_pending_evidence && ++observer_evidence_wait_count<256) return 0;
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end

    observer_evidence_wait_count=0;
```

中文伪代码：调用实际完成采集失败时，只有callee明确设置pending才递增等待计数并在256次以内返回。未设置pending或达到边界则报告当前RM错误，冻结mismatch，阻止继续把后续UID当通过。采集成功后清证据等待计数，再进入既有原子WB等待及core比较；callee的observation_pending只代表水位未覆盖，不能用于容忍owner/token错配。


### 3.16 根轮询先推进事实再比较指令

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv:2510`，函数/落点：`observer_poll_process()`。

抽象功能描述：把唯一被动观察服务和架构提交比较按固定顺序串联，后台事实不依赖当前UID是否推进。

```systemverilog
task memblock_rm::observer_poll_process();
    forever begin
        observer_service_path_facts();
        void'(observer_process_current_commit());
        #1ns;
    end
endtask:observer_poll_process
```

中文伪代码：每轮先调用observer_service_path_facts，将新增完整批次分发三个observer并处理通知；再调用observer_process_current_commit比较当前已完成指令；最后保持原1ns轮询周期。即使程序UID等待其它证据或已比较结束，事实服务仍在下一轮继续，NC后台响应不会阻塞架构提交。


### 3.17 Uncache通知的唯一实际事件消费者

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv:2839`，函数/落点：`main_process() 的other_ctrl分支`。

抽象功能描述：把monitor逐拍真实Uncache通知和空拍水位交给独立比较器，避免将其送入普通数据scoreboard。

```systemverilog
        while(1)begin
            this.other_ctrl_agent_mon_item_port.get(other_ctrl_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("other_ctrl_agent_mon_item_port get as %s",other_ctrl_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(other_ctrl_agent_tr_out, other_ctrl_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("other_ctrl_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            // 此端口仅发布Uncache错误事实；沿唯一consumer交给独立checker。
            uncache_errors.observe_notification(other_ctrl_agent_tr_in);
            //this.rm_item_act_port.write(other_ctrl_agent_tr_out);
        end
```

中文伪代码：该并行分支阻塞读取唯一other_ctrl FIFO，每份事务交给uncache_errors.observe_notification。该helper只冻结valid/bits/sample/reset并推进实际通知水位，真正比较仍由事实服务check_until协调双方完整边界。原普通memblock_common打包及rm_item_exp_port发布被替换，不能让同一通知既当后台事件又当普通指令数据。

### 4.1 STA0唯一动态实例查询

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv`，函数：`read_cmo_sta0_by_rob()`。

抽象功能描述：以真实采样冻结的owner和索引隔离ROB环绕、epoch、flush以及重复完成。

```systemverilog
    function bit read_cmo_sta0_by_rob(input memblock_rob_key_t rob_key,
                                    input int unsigned flush_epoch,
                                    input memblock_uid_t uid,
                                    input int unsigned dynamic_epoch,
                                    output writeback_record_t record,
                                    output bit duplicate);
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        int unsigned match_count;
        record='0; duplicate=0; match_count=0; key={rob_key.flag,rob_key.value};
        if(!sta0_history_by_rob.exists(key)) return 0;
        foreach(sta0_history_by_rob[key][i]) begin
            if(sta0_history_by_rob[key][i].owner_valid &&
               sta0_history_by_rob[key][i].owner_uid==uid &&
               sta0_history_by_rob[key][i].owner_dynamic_epoch==dynamic_epoch &&
               sta0_history_by_rob[key][i].sample_flush_epoch==flush_epoch) begin
                record=sta0_history_by_rob[key][i]; match_count++;
            end
        end
        duplicate=match_count>1;
        return match_count==1;
    endfunction
```

中文伪代码：在单ROB键的STA0历史中检查采样时冻结UID/动态epoch和flush代际，恰好一条才可用，两条以上明确duplicate；不在所有Load/Store写回全集搜索，不过滤接口不存在的replayInst字段。成功提交后以ROB键注册终态增量审计，保留有完整来源的一次fault后normal尾拍，其余重复立即失败。

### 4.2 写回采样锚定

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv`，函数：`run()`。

抽象功能描述：在同一clocking快照的sample锚点建立后复制实际写回和owner。

```systemverilog
    task run();
        longint unsigned edge_sample;
        if (!is_bound()) begin
            `uvm_error("RM_DUT_WB_PROBE", "integer-WB monitor VIF is not bound")
            return;
        end
        if (running) begin
            return;
        end
        running = 1'b1;
        forever begin
            @(vif.mon_cb);
            if (!memblock_sync_pkg::reset_backend_done ||
                memblock_sync_pkg::l2tlb_reset_active()) begin
                if (previous_reset_backend_done) begin
                    clear_history();
                end
                previous_reset_backend_done = 1'b0;
                continue;
            end
            memblock_sync_pkg::wait_for_l2tlb_sample_anchor($time,edge_sample);
            previous_reset_backend_done = 1'b1;
            sample_and_enqueue(RM_WB_SOURCE_LDA, 0);
            sample_and_enqueue(RM_WB_SOURCE_LDA, 1);
            sample_and_enqueue(RM_WB_SOURCE_LDA, 2);
            sample_and_enqueue(RM_WB_SOURCE_STA, 0);
            sample_and_enqueue(RM_WB_SOURCE_STA, 1);
            sample_and_enqueue(RM_WB_SOURCE_STD, 0);
            sample_and_enqueue(RM_WB_SOURCE_STD, 1);
        end
    endtask
```

中文伪代码：没有VIF或重复启动直接退出；复位清历史且不等待不存在的CSRsample；正常边沿只等待本拍NBA/delta锚点，再固定数量采集3路LDA、2路STA、2路STD。每条STA0按ROB索引，同时保留原历史供旧Load/原子查询。


### 4.3 实际WB采样时冻结动态owner

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv:420`，函数/落点：`sample_and_enqueue()`。

抽象功能描述：在真实STA0到来当拍保留UID/动态epoch及按ROB索引历史，使后续查询与增量审计不受ROB复用影响。

```systemverilog
    function void sample_and_enqueue(input bit [1:0] source_kind,
                                     input int unsigned lane);
        writeback_record_t record;
        common_data_transaction owner_data;
        memblock_rob_map_key_t owner_key;
        memblock_uid_t owner_uid;
        if (sample_lane(source_kind, lane, record)) begin
            // 采样当拍冻结动态owner，ROB环绕或同flush epoch复用不能冒充旧STA0。
            owner_data=common_data_transaction::m_inst;
            owner_key={record.rob_flag,record.rob_value};
            if(record.rob_valid && record.rob_flag_valid && owner_data!=null &&
               owner_data.uid_by_active_rob.exists(owner_key)) begin
                owner_uid=owner_data.uid_by_active_rob[owner_key];
                if(owner_data.is_valid_uid(owner_uid) && owner_data.status_by_uid[owner_uid]!=null) begin
                    record.owner_valid=1; record.owner_uid=owner_uid;
                    record.owner_dynamic_epoch=owner_data.status_by_uid[owner_uid].dynamic_epoch;
                end
            end
            audit_cmo_sta0(record);
            writeback_history_q.push_back(record);
            if (record.source_kind == RM_WB_SOURCE_STA && record.lane == 0 &&
                record.rob_valid && record.rob_flag_valid)
                sta0_history_by_rob[{record.rob_flag, record.rob_value}].push_back(record);
            writeback_count++;
        end
    endfunction:sample_and_enqueue
```

中文伪代码：sample_lane从固定VIF lane复制实际写回；仅ROB字段有效时探测当前active ROB单key，若UID/status合法则冻结owner_uid与owner_dynamic_epoch。随后先交audit_cmo_sta0检查是否是已消费CMO的迟到完成，再保留原完整历史；真实STA0另入单ROB索引，最后增加writeback_count。原Load/原子查询仍可沿原历史运行；冻结owner不修改框架映射。


### 4.4 成功消费后注册终态并补审已到尾拍

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv:326`，函数/落点：`mark_cmo_completion_consumed()`。

抽象功能描述：在core唯一成功提交后记录CMO终态身份，使程序游标离开后仍持续发现重复完成；登记时补查此前已采到的尾拍。

```systemverilog
    function bit mark_cmo_completion_consumed(input writeback_record_t completion);
        common_data_transaction data;
        main_control_transaction main_tr;
        status_transaction status_v;
        cmo_terminal_audit_t terminal_v;
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        int unsigned instance_flush;
        data=common_data_transaction::m_inst;
        if(!completion.valid || !completion.owner_valid || completion.source_kind!=RM_WB_SOURCE_STA ||
           completion.lane!=0 || !completion.exception_valid || !completion.rob_flag_valid || data==null ||
           !data.is_valid_uid(completion.owner_uid)) return 0;
        status_v=data.get_status(completion.owner_uid); main_tr=data.get_main_transaction(completion.owner_uid);
        if(status_v==null || main_tr==null || main_tr.op_class!=MEMBLOCK_OP_CLASS_CBO ||
           status_v.dynamic_epoch!=completion.owner_dynamic_epoch ||
           status_v.get_rob_key()!=memblock_rob_key_t'{flag:completion.rob_flag,value:completion.rob_value}) return 0;
        if(!status_v.get_target_instance_flush_epoch(MEMBLOCK_ISSUE_TARGET_STA,instance_flush)) begin
            if(!status_v.active_instance_flush_epoch_valid) return 0;
            instance_flush=status_v.active_instance_flush_epoch;
        end
        key={completion.rob_flag,completion.rob_value};
        if(cmo_terminal_by_rob.exists(key) &&
           cmo_terminal_by_rob[key].completion.owner_uid==completion.owner_uid &&
           cmo_terminal_by_rob[key].completion.owner_dynamic_epoch==completion.owner_dynamic_epoch) return 0;
        terminal_v='0; terminal_v.valid=1; terminal_v.completion=completion;
        terminal_v.sq='{flag:status_v.sqIdx_flag,value:status_v.sqIdx_value}; terminal_v.sta_issue_epoch=status_v.sta_issue_epoch;
        terminal_v.replay_seq=status_v.replay_seq; terminal_v.instance_flush_epoch=instance_flush;
        terminal_v.fu_op_type=main_tr.fuOpType;
        cmo_terminal_by_rob[key]=terminal_v;
        // 注册只发生一次：补查该ROB在RM排到此UID之前已采到的尾拍，避免程序cursor较慢漏审。
        if(sta0_history_by_rob.exists(key)) begin
            foreach(sta0_history_by_rob[key][i])
                if(sta0_history_by_rob[key][i].sample_seq>completion.sample_seq)
                    audit_cmo_sta0(sta0_history_by_rob[key][i]);
        end
        return 1;
    endfunction
```

中文伪代码：先要求有效owner、STA0、异常属性、合法UID和同动态epoch/ROB的CBO status；取得STA实例flush代际，缺失时仅允许既有active代际。相同ROB同UID/epoch已经登记则失败，避免重复消费。冻结completion、SQ、sta_issue_epoch、replay_seq、实例flush及最终op，再注册到单ROB键。最后仅扫描该ROB已保存且sample晚于原完成的尾拍，逐条调用audit_cmo_sta0；后续采样也复用同一函数持续审计，不重新执行CMO或写数据。


### 4.5 ROB复用、迟到重复及fault后唯一normal尾拍

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv:365`，函数/落点：`audit_cmo_sta0()`。

抽象功能描述：对已消费CMO之后的STA0执行动态身份审计，允许框架已有契约中的一次合法fault后normal尾拍，拒绝额外架构完成。

```systemverilog
    function void audit_cmo_sta0(input writeback_record_t record);
        common_data_transaction data;
        cmo_terminal_audit_t terminal_v;
        status_transaction status_v;
        main_control_transaction main_tr;
        memblock_uid_t live_uid;
        memblock_rob_key_t rob_key;
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        int unsigned instance_flush;
        if(!record.valid || record.source_kind!=RM_WB_SOURCE_STA || record.lane!=0 ||
           !record.rob_flag_valid) return;
        key={record.rob_flag,record.rob_value};
        if(!cmo_terminal_by_rob.exists(key)) return;
        terminal_v=cmo_terminal_by_rob[key];
        if(record.sample_seq<=terminal_v.completion.sample_seq) return;
        data=common_data_transaction::m_inst;
        rob_key='{flag:record.rob_flag,value:record.rob_value};
        if(record.owner_valid && (record.owner_uid!=terminal_v.completion.owner_uid ||
           record.owner_dynamic_epoch!=terminal_v.completion.owner_dynamic_epoch)) begin
            cmo_terminal_by_rob.delete(key); return;
        end
        if(data!=null && data.lookup_active_uid_by_rob(rob_key,live_uid)) begin
            status_v=data.get_status(live_uid);
            if(live_uid!=terminal_v.completion.owner_uid || status_v==null ||
               status_v.dynamic_epoch!=terminal_v.completion.owner_dynamic_epoch) begin
                // 真实ROB复用已明确新owner；新动态实例优先，不误判旧终态重复。
                cmo_terminal_by_rob.delete(key); return;
            end
        end
        if(terminal_v.completion.exception_vec!=0 && record.exception_valid &&
           record.exception_vec===24'b0 && !terminal_v.late_normal_seen && data!=null &&
           !data.lookup_active_uid_by_rob(rob_key,live_uid) &&
           data.is_valid_uid(terminal_v.completion.owner_uid)) begin
            status_v=data.get_status(terminal_v.completion.owner_uid);
            main_tr=data.get_main_transaction(terminal_v.completion.owner_uid);
            if(status_v!=null && main_tr!=null && !status_v.active && status_v.terminal_done &&
               status_v.fault && status_v.sta_fault && status_v.sta_writeback &&
               status_v.dynamic_epoch==terminal_v.completion.owner_dynamic_epoch &&
               status_v.get_rob_key()==rob_key && status_v.sqIdx_flag==terminal_v.sq.flag &&
               status_v.sqIdx_value==terminal_v.sq.value &&
               status_v.sta_issue_epoch==terminal_v.sta_issue_epoch && status_v.replay_seq==terminal_v.replay_seq &&
               status_v.get_target_instance_flush_epoch(MEMBLOCK_ISSUE_TARGET_STA,instance_flush) &&
               instance_flush==terminal_v.instance_flush_epoch && main_tr.op_class==MEMBLOCK_OP_CLASS_CBO &&
               main_tr.fuOpType==terminal_v.fu_op_type && status_v.exception_vec!=0 &&
               record.sample_flush_epoch>=terminal_v.completion.sample_flush_epoch &&
               record.sample_flush_epoch==memblock_sync_pkg::dispatch_flush_epoch) begin
                terminal_v.late_normal_seen=1; cmo_terminal_by_rob[key]=terminal_v; return;
            end
        end
        report_cmo_audit_error($sformatf(
            "已消费CMO收到非法重复STA0 uid=%0d epoch=%0d rob=%0d/%0d terminal=%0d duplicate=%0d exception=0x%0h",
            terminal_v.completion.owner_uid,terminal_v.completion.owner_dynamic_epoch,
            record.rob_flag,record.rob_value,terminal_v.completion.sample_seq,record.sample_seq,record.exception_vec));
    endfunction
```

中文伪代码：仅处理有效STA0和已有终态的ROB，原completion及更早sample直接忽略。若实际记录明确属于新UID/epoch，或active ROB映射已指向新动态实例，则删除旧审计键让真实复用生效。其余尾拍仅在原completion有异常、当前异常严格为0、尚无normal尾拍、旧owner已不active时尝试合法尾拍校验：status必须terminal_done、fault/sta_fault/sta_writeback，UID/epoch/ROB/SQ、issue epoch、replay序号、STA实例flush与最终op全部匹配，仍有非零terminal异常，尾拍flush不早于原completion且等于当前dispatch_flush_epoch。全部通过仅置late_normal_seen；重复fault、第二normal或任意来源缺失均交report_cmo_audit_error计数并报RM_CMO_LATE_COMPLETION。该校验不推进UID、不接纳新数据效果。


### 4.6 审计错误的实际报告出口

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_dut_writeback_observer.sv:320`，函数/落点：`report_cmo_audit_error()`。

抽象功能描述：把持续审计检测到的重复完成转成正常UVM失败，同时为soft负向检查保留可复核计数。

```systemverilog
    virtual function void report_cmo_audit_error(input string detail);
        cmo_late_error_count++;
        `uvm_error("RM_CMO_LATE_COMPLETION",detail)
    endfunction
```

中文伪代码：每次被audit调用先增加cmo_late_error_count，再发RM_CMO_LATE_COMPLETION UVM_ERROR。该函数是virtual，soft可以定向截取期望错误；生产默认仍真实报错，不通过丢弃尾拍或清历史伪造成功。


### 4.7 只读门面连接成功CMO与持续审计

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv:697`，函数/落点：`mark_dut_cmo_completion_consumed_for_rm()`。

抽象功能描述：让根RM用同一只读STA选择结果登记终态，不直接持有或修改框架owner。

```systemverilog
function bit memblock_rm_readonly_api::mark_dut_cmo_completion_consumed_for_rm(input memblock_uid_t uid,
    input memblock_rob_key_t key,input int unsigned dynamic_epoch);
    dut_writeback_view_t completion;
    bit duplicate;
    if(!read_dut_cmo_writeback_for_rm(uid,key,dynamic_epoch,completion,duplicate)) return 0;
    return m_dut_writeback_observer.mark_cmo_completion_consumed(completion);
endfunction
```

中文伪代码：先调用read_dut_cmo_writeback_for_rm重新取得同UID/epoch/ROB与实例flush的唯一STA0，缺失或重复返回失败；成功把值型completion交给WB observer的mark_cmo_completion_consumed。根RM只在core compare_result成功且kind为CMO时调用本入口，失败报错；不会在失败提交后登记或生成恢复。read_dut_cmo_writeback_for_rm的status/实例选择源码和控制流见Uncache专项review第3.5节。


### 4.8 独立CSR输入历史的复位

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:27`，函数/落点：`reset_cache_error_control()`。

抽象功能描述：在CSR monitor确认复位边界时清除旧代控制输入，使原子与通知查询不能复用上次reset的门控。

```systemverilog
    function void reset_cache_error_control();
        cache_error_control_by_sample.delete();
    endfunction
```

中文伪代码：CSR monitor唯一正常调用者在begin_l2tlb_runtime_reset之前执行本函数，删除按sample索引的控制历史。soft fixture可在自身隔离状态初始化时调用同接口；该函数不改全局sample、不创造epoch或额外DUT信号。


### 4.9 每sample唯一错误控制输入发布

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:31`，函数/落点：`publish_cache_error_control()`。

抽象功能描述：保存monitor实际采到的cache_error_enable和reset身份，提供独立历史答案，防止RM取latest或反读最终异常推门控。

```systemverilog
    function void publish_cache_error_control(input longint unsigned sample_seq,
                                              input int unsigned reset_epoch,
                                              input logic enable);
        memblock_cache_error_control_t record;
        if (sample_seq == 0 || $isunknown(enable)) begin
            `uvm_error("RM_CACHE_ERROR_CONTROL", "cache_error_enable缺少有效采样身份或含X/Z")
            return;
        end
        record = '{valid:1'b1, enable:enable, reset_epoch:reset_epoch,
                   sample_seq:sample_seq};
        if (cache_error_control_by_sample.exists(sample_seq) &&
            cache_error_control_by_sample[sample_seq] !== record) begin
            `uvm_error("RM_CACHE_ERROR_CONTROL", "同一sample错误控制输入被重复改写")
            return;
        end
        cache_error_control_by_sample[sample_seq] = record;
    endfunction
```

中文伪代码：sample为0或enable含X/Z立即报错并返回；否则构造valid、enable、reset_epoch、sample_seq完整记录。该sample已有不同记录则报重复改写错误且保留原值，同值重复允许；最后以sample单key发布。原子按真实state6减两拍查询，Uncache通知按内部D消费sample查询，二者不互相覆盖来源。


### 4.10 错误控制输入的值型查询

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:49`，函数/落点：`read_cache_error_control()`。

抽象功能描述：按精确历史sample返回独立控制快照，缺记录明确失败，不回退到当前输入。

```systemverilog
    function bit read_cache_error_control(input longint unsigned sample_seq,
                                          output memblock_cache_error_control_t record);
        record = '0;
        if (!cache_error_control_by_sample.exists(sample_seq)) return 0;
        record = cache_error_control_by_sample[sample_seq];
        return record.valid;
    endfunction
```

中文伪代码：输出先清零；若sample键不存在直接返回0。存在则复制该记录并返回valid。只读查询无状态副作用，不补默认enable，不扫描全部CSR历史，不由RM发布或创建记录。


### 4.11 CSR monitor确认复位时清控制历史

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv:343`，函数/落点：`csr_ctrl_agent_agent_monitor::mon_data()`。

抽象功能描述：将错误控制历史清理绑定到现有唯一L2TLB/DUT采样复位发布者，避免其它monitor自行判断代际。

```systemverilog
            memblock_sync_pkg::reset_cache_error_control();
            memblock_sync_pkg::begin_l2tlb_runtime_reset();
```

中文伪代码：monitor进入既有复位分支时先清错误控制历史，再调用begin_l2tlb_runtime_reset建立公共复位边界，随后原流程取得reset_epoch并初始化raw CSR状态。该顺序确保新代sample发布前旧记录不可查询。


### 4.12 同边沿先发布sample再发布控制输入

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv:369`，函数/落点：`csr_ctrl_agent_agent_monitor::mon_data()`。

抽象功能描述：为所有内部consumer提供同一真实边沿的sample及错误控制值，延迟观察仍能读取精确历史。

```systemverilog
            current_sample_seq = memblock_sync_pkg::advance_dut_global_sample($time);
            memblock_sync_pkg::publish_cache_error_control(
                current_sample_seq, memblock_sync_pkg::get_l2tlb_current_reset_epoch(),
                io_ooo_to_mem_csrCtrl_cache_error_enable);
```

中文伪代码：正常边沿沿既有唯一advance_dut_global_sample取得current_sample_seq，随即将当前采样enable与公共reset_epoch发布到历史；之后继续原raw CSR snapshot构造。WB、memory responder、cmo_path与通知monitor等待该边沿锚点，不各自递增sample；CSR记录不存在时查询失败，不能猜输入或放宽时间容差。


### 4.13 唯一被动agent、FIFO及RM配置交接

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_env.sv:193`，函数/落点：`memblock_env::build_phase()`。

抽象功能描述：创建一个内部被动观察agent及唯一采样FIFO，使CMO请求、DCache错误来源和Uncache共用相同事实流。

```systemverilog
    cmo_path_agent_mon2rm_fifo = new("cmo_path_agent_mon2rm_fifo",this);
    cmo_path_cfg = cmo_path_agent_cfg::type_id::create("cmo_path_cfg");
    uvm_config_db#(cmo_path_agent_cfg)::set(this,"u_cmo_path_agent","cfg",cmo_path_cfg);
    this.u_cmo_path_agent = cmo_path_agent::type_id::create("u_cmo_path_agent",this);
    // 通过现有RM实例config入口交接FIFO，避免新增竞争消费者或公共错误历史副本。
    uvm_config_db#(uvm_tlm_analysis_fifo#(cmo_path_agent_xaction))::set(
        this,"rm","cmo_path_fifo",cmo_path_agent_mon2rm_fifo);
```

中文伪代码：env构造FIFO和默认被动cfg，给u_cmo_path_agent配置并例化该agent；通过现有rm实例的config_db入口交接同一个FIFO句柄。RM build初始化三个observer及水位/等待字段并必须取得该句柄，否则fatal。没有第二消费者、独立响应历史副本或主动driver。


### 4.14 被动事实的唯一分析端口连接

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_env.sv:213`，函数/落点：`memblock_env::connect_phase()`。

抽象功能描述：将agent monitor的完整逐拍事实发布到RM拥有的FIFO，固定采样入口和消费责任。

```systemverilog
    this.u_cmo_path_agent.mon_item_port.connect(this.cmo_path_agent_mon2rm_fifo.analysis_export);
```

中文伪代码：connect_phase只把monitor analysis端口连到已创建FIFO的analysis_export。root adapter从同一FIFO取出批次并依次分发职责对象；各observer不直接争抢FIFO。对应cfg/tb.f登记subagent filelist、memblock_env_pkg导入agent包、top_tb包含cmo_path_agent_connect.sv，均为组件可见性与接线登记，不改变被动观察语义。


### 4.15 Prefetch后台逐beat证明及覆盖日志

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv:268`，函数/落点：`validate_prefetch_grant()`。

抽象功能描述：核对硬件或软件预取实际产生的独立GrantData传输；不依赖UID、软件完成或实际顶层异常作为答案。

```systemverilog
    function bit validate_prefetch_grant(input transport_t source,
        input cmo_path_probe_t event_item,input longint unsigned sample);
        mem_access_base_sequence::l2_response_history_t response;
        bit available;
        bit [1:0] required_beats,expected_raw;
        longint unsigned expected_sample;
        if(!source.valid || event_item.op!=9'd5) return 0;
        // 底座要求非零beat mask；首次01只取record元信息，不以available决定当前beat。
        if(!mem_access_base_sequence::query_l2_response_history(source.token,
            mem_access_base_sequence::mem_addr_t'(source.line_addr),sample,2'b01,response,available) ||
            !response.valid) begin
            return 0;
        end
        if(response.req_source!=MEMBLOCK_REQ_L1_PREFETCH) return 1;
        required_beats=source.fired_beats==0 ? 2'b01 : 2'b10;
        if(source.fired_beats>1 || response.kind!=2 ||
            response.source_generation!=source.generation || 32'(response.source)!=event_item.id ||
            !mem_access_base_sequence::query_l2_response_history(source.token,
                mem_access_base_sequence::mem_addr_t'(source.line_addr),sample,required_beats,response,available) ||
            !available) begin
            return 0;
        end
        expected_raw=source.fired_beats==0 ? {response.beat0.denied,response.beat0.corrupt} :
                                            {response.beat1.denied,response.beat1.corrupt};
        expected_sample=source.fired_beats==0 ? response.beat0.sample : response.beat1.sample;
        if(expected_sample!=sample || expected_raw!=event_item.raw) return 0;
        `uvm_info("RM_PREFETCH_GRANT",$sformatf(
            "token=%0d source=%0d generation=%0d beat=%0d sample=%0d raw=%0b 独立预取响应比较PASS",
            source.token,event_item.id,source.generation,source.fired_beats,sample,expected_raw),UVM_HIGH)
        return 1;
    endfunction
```

中文伪代码：只接受有效transport和真实GrantData。首次以底座允许的非零01 mask读取record元信息，检查record存在但不使用available推断当前beat；普通非L1预取响应返回成功交其它consumer检查。L1预取按fired_beats选择01/10，验证kind、generation、source以及该beat在当前sample可用；随后独立raw及精确beat sample必须等于实际事件。成功发UVM_HIGH的RM_PREFETCH_GRANT，携带token/source/generation/beat/sample/raw，用于真实错误覆盖证明；日志不改变判定。00 mask在生产API和mock均被拒绝，soft不能用更宽松替身掩盖契约错误。


## 5. 测试入口与验证

新增thin软件测试直接调用真实core、请求与错误observer；生产翻译测试复用已有UID冻结TLB/PMP上下文并调用env.rm生产入口。Uncache终场test只对明确Uncache writer复用原最终字节比较，不放宽生产cacheable门槛。真实CBO builder复用原dispatch/redirect、CSR控制及CHECK_STORE服务；bootstrap和basicTest能力白名单适配见专项复审。

实际验证于2026-11-11在eda01执行，分支`mem_ut_uvm_v2`，mode=`cmo_plans_20261111`，seed=`666666`。原`base_fun`分区编译数据库出现旧产物问题，使用独立mode重新构建；没有删除既有数据库。最新统一VCS编译成功。基础与真实dispatch smoke按规则入口执行，mode替换不改变激励配置。

| 验证内容 | 实际证据与结果 |
|---|---|
| 首次行选择/Store排除/逐beat历史 | `memblock_l2_line_history_directed_vseq`通过，UVM_ERROR/FATAL为0 |
| CMO纯值语义/唯一效果/SC覆盖 | `soft_test_tc_rm_cmo_core`共920检查通过 |
| 实际请求与提交后重复完成 | `soft_test_tc_rm_cmo_request`共39检查通过 |
| 生产翻译与权限入口 | `soft_test_tc_rm_cmo_translation`共92检查通过，包括TLB PF/AF/GPF/GAF及PMP/PMA |
| DCache来源/首beat/旁路/原子寄存/CSR/split/预取 | `soft_test_tc_rm_dcache_error`通过，直接调用真实observer及生产只读API |
| Uncache请求/分段/通知/代际 | `soft_test_tc_rm_uncache_error`共103检查通过 |
| 旧provenance/Uncache ledger软件入口 | 两者均通过；provenance精确捕获预期7fatal/2error，最终UVM_ERROR/FATAL为0 |
| 基础与真实dispatch | `basicTest/virtual_base_sequence`及`memblock_dispatch_real_smoke_vseq + cfg=tc_dispatch_real_smoke`均0 error/0 fatal |
| 四种cacheable CMO | zero/clean/flush/inval四个preset均完成请求/最终属性比较及CHECK_STORE；zero最终11项entry |
| Uncache zero正常与部分写 | 正常8段、最终11项；denied25实际首错第2段，最终5项；denied100实际首段错，最终4项；均通知与真实overlay比较通过 |
| Uncache Load | PBMT NC下denied25实际UID4位5、corrupt100实际Load位19；owner历史与正常zero/最终数据比较均通过 |
| MMIO Store响应denied | 实际PA0x38000dc0经Uncache；独立请求/写接受、STA/terminal位7及通知比较通过 |
| cacheable Load | AUTO/Bare命中PA0x87f00300，CLEAN数据/denied位5/corrupt位19分别匹配 |
| AMO门控与LR/SC/CAS数据 | AUTO gate0下denied保留正常AMO数据与最终写；DISABLED静态CSR gate1下AMO denied位7/corrupt位19，LR denied位5/SC denied位7匹配；LR/SC成功、SC失败无写、128位CAS结果及Store-table均通过 |
| 软件预取真实错误 | 强制PrefetchR，两个真实GrantData beat分别核对token1、source2、generation1、sample38/39；denied raw11、corrupt raw01；LDA位5/19同步抑制通过 |
| nonzero CMO真实错误 | isolated三项DISABLED场景，denied/corrupt均确认first_kind=CBOAck且状态为目标错误、实际fault分别0x80/0x80000，与RM一致；两条年轻Load通过；0 error/0 fatal |
| 原含年轻Store的CMO fault恢复 | CMO异常比较通过，后续SQ重入与flushSb不空，定点停止，**BLOCKED，整例未通过** |

复现主入口：从`mem_ut/ver/ut/memblock/sim`执行`make eda_compile tc=basicTest ts=virtual_base_sequence mode=cmo_plans_20261111`；编译后执行`make eda_batch_run tc=<test> ts=<vseq> cfg=<preset> mode=cmo_plans_20261111`。isolated复用`tc_dispatch_real_cbo_cold_denied/corrupt`，追加`+MEMBLOCK_MAIN_TRANS_NUM=3 +MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=0`。软件PrefetchR/Load/atomic的精确runtime_plusargs及逐场景结果见对应`AI_DOC/seq/*_20261111.csv`，不能仅凭权重推断实际覆盖。

完整阻塞证据见[故障恢复专项分析](../../../analysis/testcase_flow/memblock_cmo_cboack_fault_sq_reentry_flush_stall_20261111.md)。已证实SQ重入、身份/地址混用和指针越过；真实后端pendingst契约及框架撤销延迟的责任仍待独立复核，未修改RTL或正式登记已确认RTL Buglist。isolated不含writer/CHECK_STORE，不能替代完整排空验收。

验证边界：真实Uncache zero末段首次denied尚未单独复现，0/2/3/8段及末段错误效果由纯值core/103项observer软测覆盖；动态CSR连续切换、全部异常/跨页组合及NC/MMIO所有随机组合没有开展大规模多seed回归。已有明确支持和测试边界保持原plan，不把未运行组合登记通过。

## 6. 实现与Plan不一致项

执行中调整均在原plan的IMPLEMENTATION_DELTA保留来源：真实Valid接口替代不存在的ready；directD真实offer可先于fire但MSHR/refill仍要求已送达；原子CSR在真实state6采样且对应顶层两拍前输入；不同Load来源分别映射后合并；真实首页/normal split消费；生产者与WB同拍sample锚点；STA接口没有replayInst而不追加假字段。保持计划的独立期望、精确身份和不产生恢复动作原则。每项对应原代码与中文伪码见上述章节及专项复审。

## 7. Plan未说明但Coding落实的细节

1. 同拍跨producer处理延后一完整sample，只为了确定发布边界，不是阻止RM先行的调度器。
2. STA0新增按ROB索引与采样时冻结UID/epoch，避免ROB环绕时旧完成被重复匹配；不改变原Load/原子历史选择。
3. CBO op_class、STU与最终编码矛盾明确失败，避免共享数值编码误识别原子。
4. 终场游标/通知/请求drain与Uncache测试层最终表使漏检不被phase结束隐藏。
5. 软件预取实际LDA的同步异常抑制和后台GrantData逐beat事实比较分开；WB observer在CMO成功提交后继续按ROB键审计迟到重复完成。

## 8. 非本次修改的逻辑分析

当前工作树其它修改均属同一三plan任务，但由专项复审分别覆盖：mem_base/readonly与接口是行历史和Uncache事实；完整subagent/connect是DCache来源；rm_ls_core及CBO测试builder是CMO语义；参数/CSV/flow同步单列于对应review。未修改RTL、构建工具或无关业务代码。文档基线已按用户要求单独本地提交，不混入本次实现。


## 9. 根适配 Diff 覆盖核对

本轮逐项核对根适配修改。原有14个源码块逐字匹配当前文件；新增正文补齐WB持续审计、CSR发布与根Store/通知接入。相关路径分类如下；表中源码路径均相对 `mem_ut/ver/ut/memblock`：

| 修改文件或对象 | 本review覆盖点 | 其它专项覆盖/职责边界 |
|---|---|---|
| `env/src/memblock_rm/memblock_rm.sv` | build对象/FIFO、事实服务、翻译与权限、CMO/Load/原子/Prefetch/同步Store入口、fault gate、有界等待、完成后审计、通知consumer和check_phase | core数据效果见CMO核心专项，内部消费来源见DCache/Uncache专项 |
| `seq/base_seq_help/memblock_rm_dut_writeback_observer.sv` | 同拍sample锚点、采样冻结owner、单ROB历史、唯一STA0、终态登记与持续审计、复位清历史 | 原有Load/原子选择算法保持本次diff以外逻辑 |
| `seq/base_seq_help/memblock_rm_readonly_api.sv` | CMO成功登记审计门面与STA0选择调用链 | STA0实例flush及首段context、Uncache response/write-event查询详见Uncache专项 |
| `common/memblock_common/src/memblock_sync_pkg.sv`、`agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv` | 独立错误控制输入的字段/复位/发布/读取、唯一sample发布者与同边沿控制历史 | 不建立BEU或CSR寄存器模型 |
| `env/src/memblock_env.sv`、`env/memblock_env_pkg.sv`、`cfg/tb.f`、`tb/top_tb.sv` | agent/FIFO/cfg创建及唯一分析端口连接、包/filelist/connect可见性 | 内部全部probe语义见DCache来源和CMO请求专项 |
| `tc/src/soft_test/soft_test_tc_rm_cmo_core.sv`、`soft_test_tc_rm_cmo_translation.sv`、`tc/tc_pkg.sv` | 真实core、request、Uncache helper、生产翻译入口的薄封装及package登记 | 测试函数内部行为见各专项review，执行状态由§5与对应CSV统一维护 |
| `tc/src/tc_rm_cmo_uncache_final.sv` | 已明确Uncache writer和responder完成后复用原最终字节比较 | 完整源码、边界及真实测试入口见Uncache专项第9节 |
| `seq/base_seq/memblock_main_dispatch_cbo_directed_sequence.sv`、新增冷CMO builder、对应vseq及basicTest能力白名单、`seq_pkg.sv`/`seq.f`/preset | 定向真实场景只扩既有可用入口及控制拓扑，冷目标让CBOAck成为首次行选择 | builder/CSR bootstrap/CHECK_STORE服务及实际配置见CMO核心请求专项与真实DUT CSV |

字段生命周期补充：三个observer/FIFO/水位在RM build初始化；observer_pending_evidence逐次当前UID检查清零，由等待水位的callee设置；证据等待计数在成功采集清零，确定缺证据或达到上限后报错；owner完成fact在程序成功推进后释放。WB终态键在注册时冻结完整provenance，后续ROB明确复用删除旧键，复位清理；普通cacheable Store和cacheable zero仍不通过响应错误阻止已提交overlay写入。

核对结论：未发现根源码功能遗漏；本文已为本次新增主接入和关键子函数提供源码与中文控制流，未将其它专项已分析的数据底座复制为第二套实现。当前验证状态继续由主agent在第5节最后更新，本节不声明全部真实DUT场景通过。

## 10. 最终收口与独立复查

三个方案功能代码、真实monitor/RM接入、必要检查与文档已完成。实现执行中差异均以IMPLEMENTATION_DELTA保留；三名subagent的代码由root对照diff和真实日志复核，原子寄存拍、Uncache内部消费拍、Prefetch非零mask契约和迟到重复STA均完成修正与独立复查。不存在已知未实现的三方案功能入口；完整故障恢复排空仍有上述已复现集成阻塞。

本次归档表示编码与必要验证完成，不表示全部真实DUT组合验收通过。原Store+CHECK_STORE故障场景保留BLOCKED，后续须先核对pendingst接口契约和完成责任分类，再按实际责任授权修复并回到原seed复测。
