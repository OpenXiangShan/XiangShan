//=========================================================
//File name    : memblock_rm.sv
//Author       : OpenAI_Codex
//Module name  : memblock_rm
//Discribution : memblock_rm : reference model
//Date         : 2026-04-12
//=========================================================
`ifndef MEMBLOCK_RM__SV
`define MEMBLOCK_RM__SV

// 纯值型核心不读取或写入验证环境 owner；UVM adapter 只向它传递值型快照。
`include "rm_ls_core.sv"
`include "rm_cmo_request_observer.sv"
`include "rm_dcache_error_observer.sv"
`include "rm_uncache_error_observer.sv"
`include "soft_test_rm_cmo_core.sv"
`include "soft_test_rm_uncache_error_observer.sv"
`include "soft_test_rm_cmo_request.sv"


class memblock_rm  extends tcnt_rm_base #(.seq_item_t(memblock_common_xaction));

    //virtual tc_if vif;
    memblock_env_cfg cfg;
    // 中文注释：RM 自有的 expected 状态 owner。框架只通过只读 value view
    // 提供事实；RM 不持有、修改或推进任一框架 owner。
    rm_ls_model_t ls_model;
    // 中文注释：仅保存已消费数据的异常实例；地址到齐后删除，不重新执行commit。
    rm_ls_program_item_t observer_pending_address[$];
    rm_cmo_request_observer_t cmo_requests;
    rm_dcache_error_observer dcache_errors;
    rm_uncache_error_observer uncache_errors;
    uvm_tlm_analysis_fifo#(cmo_path_agent_xaction) cmo_path_fifo;
    longint unsigned path_observed_sample;
    bit observer_pending_evidence;
    int unsigned observer_evidence_wait_count;

    int unsigned observer_main_count;
    bit          observer_check_store_pending;
    memblock_uid_t observer_check_store_uid;
    rm_ls_rob_key_t observer_check_store_rob;
    bit          observer_check_store_wait_logged;
    bit          observer_mismatch_reported;
    // 中文注释：只为当前提交UID等待首段被动证据；成功/普通失败清0，256次poll后明确报错。
    int unsigned observer_first_split_wait_count;
    int unsigned observer_atomic_wb_wait_count;
    bit          observer_trace_main_emitted[int unsigned];
    bit          observer_trace_cmo_emitted[bit [63:0]];
    bit          observer_trace_translation_emitted[int unsigned];
    bit          observer_trace_translation_path_emitted[int unsigned];
    bit          observer_trace_dut_load_emitted[int unsigned];
    longint unsigned observer_atomic_first_issue_sample[int unsigned];
    int unsigned observer_atomic_issue_dynamic_epoch[int unsigned];
    bit [3:0] observer_atomic_std_checked_mask[int unsigned];
    bit [1:0] observer_atomic_sta_pdest_valid[int unsigned];
    bit [15:0] observer_atomic_sta_pdest_flat[int unsigned];

    //aa_test_reg_model		reg_model;

    uvm_blocking_get_port #(backendToTopBypass_agent_agent_xaction) backendToTopBypass_agent_mon_item_port;
    uvm_blocking_get_port #(fence_agent_agent_xaction) fence_agent_mon_item_port;
    uvm_blocking_get_port #(csr_ctrl_agent_agent_xaction) csr_ctrl_agent_mon_item_port;
    uvm_blocking_get_port #(lsqcommit_agent_agent_xaction) lsqcommit_agent_mon_item_port;
    uvm_blocking_get_port #(lsqenq_agent_agent_xaction) lsqenq_agent_mon_item_port;
    uvm_blocking_get_port #(lintsissue_agent_agent_xaction) lintsissue_agent_mon_item_port;
    uvm_blocking_get_port #(vecissue_agent_agent_xaction) vecissue_agent_mon_item_port;
    uvm_blocking_get_port #(redirect_agent_agent_xaction) redirect_agent_mon_item_port;
    uvm_blocking_get_port #(sbuffer_agent_agent_xaction) sbuffer_agent_mon_item_port;
    uvm_blocking_get_port #(dcache_agent_agent_xaction) dcache_agent_mon_item_port;
    uvm_blocking_get_port #(int_sink_agent_agent_xaction) int_sink_agent_mon_item_port;
    uvm_blocking_get_port #(L2tlb_agent_agent_xaction) L2tlb_agent_mon_item_port;
    uvm_blocking_get_port #(itlb_agent_agent_xaction) itlb_agent_mon_item_port;
    uvm_blocking_get_port #(prefetch_agent_agent_xaction) prefetch_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_ctrl_agent_agent_xaction) io_mem_to_ooo_ctrl_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_int_wb_agent_agent_xaction) io_mem_to_ooo_int_wb_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_vec_wb_agent_agent_xaction) io_mem_to_ooo_vec_wb_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_wakeup_agent_agent_xaction) io_mem_to_ooo_wakeup_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_iq_feedback_agent_agent_xaction) io_mem_to_ooo_iq_feedback_agent_mon_item_port;
    uvm_blocking_get_port #(other_ctrl_agent_agent_xaction) other_ctrl_agent_mon_item_port;

    `uvm_component_utils(memblock_rm)

    extern         function      new(string name , uvm_component parent);
    extern         function void build_phase(uvm_phase phase);
    extern function void check_phase(uvm_phase phase);
    extern virtual task run_phase(uvm_phase phase);
    extern virtual task main_phase(uvm_phase phase);
    extern virtual task main_process();
    extern virtual task observer_poll_process();
    extern function void observer_service_path_facts();
    extern function bit observer_capture_cmo_actual(
        memblock_rm_readonly_api::main_transaction_view_t main_view,
        rm_ls_program_item_t item);
    extern function bit observer_apply_cmo_d_error_expectation(
        rm_ls_program_item_t item, longint unsigned access_sample);

    extern function void observer_service_exception_addresses(bit final_check=0);
    extern function void observer_check_pmm_arch_reference(rm_ls_program_item_t item);
    extern function void observer_compute_guest_fault_address(
        rm_ls_program_item_t item,
        memblock_rm_readonly_api::tlb_request_context_view_t context_view,
        memblock_rm_readonly_api::tlb_entry_view_t entry_view,
        bit [63:0] request_va);
    extern function bit [63:0] observer_extend_exception_va(
        bit [63:0] va,
        memblock_rm_readonly_api::tlb_request_context_view_t context_view);
    extern function int unsigned observer_scalar_size(bit [8:0] fu_op_type);
    extern function int unsigned observer_atomic_size(bit [8:0] fu_op_type);
    extern function rm_atomic_kind_e observer_atomic_kind(bit [8:0] fu_op_type);
    extern function bit [63:0] observer_compute_vaddr(bit [63:0] src_0,
                                                      bit [63:0] imm);
    extern function bit observer_stage_permission_fault(
        memblock_rm_readonly_api::tlb_entry_view_t entry,
        memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
        bit store_access,
        bit stage_one,
        bit hlvx = 0
    );
    // 中文注释：按当前 UID 冻结的 CSR 判断 raw PBMT 是否需要附加 PF/GPF。
    // 返回的 force bit 只属于当前 RM item，不回写 readonly entry 或公共 TLB 表。
    extern function bit observer_eval_pbmt_fault_overlay(
        memblock_rm_readonly_api::tlb_entry_view_t entry,
        memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
        bit s1_active,
        bit s2_active,
        output bit force_s1_pf,
        output bit force_s2_gpf
    );
    // 中文注释：按 V2 TLB response mux 选择当前访问真正可见的 PBMT。该 helper
    // 只消费已经命中的 readonly entry 与 UID 冻结上下文，不访问 live runtime CSR。
    extern function bit observer_get_effective_pbmt(
        memblock_rm_readonly_api::tlb_entry_view_t entry,
        memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
        bit s1_active,
        bit s2_active,
        output bit [1:0] effective_pbmt
    );
    // 中文注释：V2 硬件非对齐处理对 PMA C=0/1 均适用；仅 PBMT-NC/MMIO
    // 路径保留 address-misaligned expectation。
    extern function bit observer_should_expect_addr_misaligned(
        bit misaligned,
        bit store_access,
        memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
        bit all_bytes_pma_pbmt
    );
    extern function bit observer_build_commit_item(
        memblock_rm_readonly_api::main_transaction_view_t main_view,
        memblock_rm_readonly_api::status_view_t status_view,
        output rm_ls_program_item_t item
    );
    extern function bit observer_build_commit_item_with_observation(
        memblock_rm_readonly_api::main_transaction_view_t main_view,
        memblock_rm_readonly_api::status_view_t status_view,
        output rm_ls_program_item_t item,
        output bit deferred
    );
    extern function bit observer_try_apply_first_split_data_error(
        rm_ls_program_item_t item,
        int unsigned prefix_bytes,
        output bit deferred
    );
    extern function bit observer_capture_commit_actual(
        memblock_rm_readonly_api::main_transaction_view_t main_view,
        rm_ls_program_item_t item,
        output bit missing_dut_load_writeback
    );
    extern function bit observer_apply_load_d_error_expectation(
        rm_ls_program_item_t item,
        longint unsigned access_sample
    );
    extern function bit observer_check_prefetch_d_error_suppression(rm_ls_program_item_t item);
    extern function bit observer_capture_load_backing(rm_ls_program_item_t item);
    extern function bit observer_capture_atomic_backing(rm_ls_program_item_t item);
    extern function bit observer_apply_atomic_d_error_expectation(
        rm_ls_program_item_t item,
        longint unsigned access_sample
    );
    extern static function bit observer_atomic_wb_wait_pending(
        ref int unsigned wait_count,
        input bit missing_writeback,
        input rm_ls_kind_e item_kind
    );
    extern static function bit observer_atomic_std_sample_matches(
        input memblock_rm_readonly_api::main_transaction_view_t main_view,
        input bit [3:0] required_std_mask,
        input int unsigned pipe,
        input bit [63:0] std_data,
        input bit [8:0] std_fuop,
        input bit [MEMBLOCK_ROB_VALUE_W-1:0] std_rob_value,
        output int unsigned slot
    );
    extern function void observer_check_atomic_issue_monitor(
        lintsissue_agent_agent_xaction monitor_tr
    );
    extern function string observer_kind_name(rm_ls_kind_e kind);
    extern function string observer_store_size_name(int unsigned size_bytes);
    extern function void observer_trace_commit_result(
        input rm_ls_program_item_t item,
        input memblock_rm_readonly_api::status_view_t status_view,
        input bit compare_result
    );
    extern function bit observer_process_current_commit();
    extern function bit observer_check_final_store(
        input memblock_uid_t check_store_uid,
        input rm_ls_rob_key_t check_store_rob,
        output bit complete
    );
endclass

function memblock_rm::new(string name , uvm_component parent);
    super.new(name, parent);
endfunction

function void memblock_rm::build_phase(uvm_phase phase);
    super.build_phase(phase);
    this.ls_model = new();
    cmo_requests = new();
    dcache_errors = new();
    uncache_errors = new("uncache_errors");
    path_observed_sample = 0;
    observer_pending_evidence = 0;
    observer_evidence_wait_count = 0;
    if (!uvm_config_db#(uvm_tlm_analysis_fifo#(cmo_path_agent_xaction))::get(
            this,"","cmo_path_fifo",cmo_path_fifo))
        `uvm_fatal("RM_PATH_FIFO", "RM缺少唯一cmo_path被动采样FIFO")

    this.observer_main_count = 0;
    this.observer_check_store_pending = 1'b0;
    this.observer_check_store_uid = '0;
    this.observer_check_store_rob = '{default:'0};
    this.observer_check_store_wait_logged = 1'b0;
    this.observer_mismatch_reported = 1'b0;
    this.observer_first_split_wait_count = 0;
    this.observer_atomic_wb_wait_count = 0;
    this.observer_trace_main_emitted.delete();
    this.observer_trace_cmo_emitted.delete();
    this.observer_trace_translation_emitted.delete();
    this.observer_trace_translation_path_emitted.delete();
    this.observer_trace_dut_load_emitted.delete();
    this.observer_atomic_first_issue_sample.delete();
    this.observer_atomic_issue_dynamic_epoch.delete();
    this.observer_atomic_std_checked_mask.delete();
    this.observer_atomic_sta_pdest_valid.delete();
    this.observer_atomic_sta_pdest_flat.delete();
    begin
        memblock_rm_observer_cache observer;
        if (!uvm_config_db#(memblock_rm_observer_cache)::get(this, "", "rm_observer", observer)) begin
            observer = new("rm_observer_default_empty");
        end
        memblock_rm_readonly_api::bind_observer(observer);
    end
    begin
        virtual io_mem_to_ooo_int_wb_agent_agent_interface full_vif;
        virtual io_mem_to_ooo_int_wb_agent_agent_interface.mon_mp mon_vif;
        if (!uvm_config_db#(virtual io_mem_to_ooo_int_wb_agent_agent_interface)::get(
                null,
                "uvm_test_top.env.u_io_mem_to_ooo_int_wb_agent_agent*",
                "vif",
                full_vif)) begin
            `uvm_error("RM_DUT_WB_PROBE",
                       "failed to get integer-WB VIF from the existing agent config DB path")
        end else begin
            mon_vif = full_vif;
            memblock_rm_readonly_api::bind_dut_writeback_observer_vif_for_rm(mon_vif);
        end
    end
    //if(!uvm_config_db#(virtual tc_if)::get(this, "", "vif", vif)) begin
    //    `uvm_fatal(get_type_name(),$sformatf("virtual interface must be set for vif(tc_if)!!!"))
    //end
    if(!uvm_config_db#(memblock_env_cfg)::get(this,"","cfg",this.cfg)) begin
        `uvm_fatal(get_type_name(),$sformatf("build_phase: env cfg is not set!!!"));
    end else begin
        `uvm_info(get_type_name(),$sformatf("build_phase: get_cfg !!!"),UVM_DEBUG);
    end

    `uvm_info("RM_LS_CAPABILITY",
              "scalar Load/Store core enabled; Load actual comes from DUT WB history, Store expected comes from Main_table plus translation, and each Main_table check_store marker triggers a Store-table comparison through read_store_final_for_rm",
              UVM_LOW);

    this.backendToTopBypass_agent_mon_item_port = new($sformatf("backendToTopBypass_agent_mon_item_port"), this);
    this.fence_agent_mon_item_port = new($sformatf("fence_agent_mon_item_port"), this);
    this.csr_ctrl_agent_mon_item_port = new($sformatf("csr_ctrl_agent_mon_item_port"), this);
    this.lsqcommit_agent_mon_item_port = new($sformatf("lsqcommit_agent_mon_item_port"), this);
    this.lsqenq_agent_mon_item_port = new($sformatf("lsqenq_agent_mon_item_port"), this);
    this.lintsissue_agent_mon_item_port = new($sformatf("lintsissue_agent_mon_item_port"), this);
    this.vecissue_agent_mon_item_port = new($sformatf("vecissue_agent_mon_item_port"), this);
    this.redirect_agent_mon_item_port = new($sformatf("redirect_agent_mon_item_port"), this);
    this.sbuffer_agent_mon_item_port = new($sformatf("sbuffer_agent_mon_item_port"), this);
    this.dcache_agent_mon_item_port = new($sformatf("dcache_agent_mon_item_port"), this);
    this.int_sink_agent_mon_item_port = new($sformatf("int_sink_agent_mon_item_port"), this);
    this.L2tlb_agent_mon_item_port = new($sformatf("L2tlb_agent_mon_item_port"), this);
    this.itlb_agent_mon_item_port = new($sformatf("itlb_agent_mon_item_port"), this);
    this.prefetch_agent_mon_item_port = new($sformatf("prefetch_agent_mon_item_port"), this);
    this.io_mem_to_ooo_ctrl_agent_mon_item_port = new($sformatf("io_mem_to_ooo_ctrl_agent_mon_item_port"), this);
    this.io_mem_to_ooo_int_wb_agent_mon_item_port = new($sformatf("io_mem_to_ooo_int_wb_agent_mon_item_port"), this);
    this.io_mem_to_ooo_vec_wb_agent_mon_item_port = new($sformatf("io_mem_to_ooo_vec_wb_agent_mon_item_port"), this);
    this.io_mem_to_ooo_wakeup_agent_mon_item_port = new($sformatf("io_mem_to_ooo_wakeup_agent_mon_item_port"), this);
    this.io_mem_to_ooo_iq_feedback_agent_mon_item_port = new($sformatf("io_mem_to_ooo_iq_feedback_agent_mon_item_port"), this);
    this.other_ctrl_agent_mon_item_port = new($sformatf("other_ctrl_agent_mon_item_port"), this);

endfunction

task memblock_rm::run_phase(uvm_phase phase);
    super.run_phase(phase);
    memblock_rm_readonly_api::get().run_dut_writeback_observer_for_rm();
endtask:run_phase

task memblock_rm::main_phase(uvm_phase phase);
    super.main_phase(phase);
    fork
        this.observer_poll_process();
    join_none
    this.main_process();
endtask

// 抽象职责：唯一消费被动批次，先建立传输/错误事实再推进RM；不取走框架恢复事件。
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

// 抽象职责：统一处理nonzero CMO响应错误；cacheablezero正常，不查永久PA错误摘要。
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

// 抽象职责：冻结最终STA0与当前CMO请求/异常证据；normal/fault均交给core唯一提交。
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

// 抽象职责：终场只检查仍未消费的请求/通知及程序游标，不产生额外流量。
function void memblock_rm::check_phase(uvm_phase phase);
    super.check_phase(phase);
    observer_service_path_facts();
    observer_service_exception_addresses(1);
    uncache_errors.final_check(path_observed_sample);
    void'(cmo_requests.check_drained());
    if(ls_model.configured && ls_model.current_uid!=observer_main_count)
        `uvm_error("RM_PROGRAM_NOT_DRAINED",
            $sformatf("RM仍有未比较指令 current=%0d count=%0d",ls_model.current_uid,observer_main_count))
    if(observer_check_store_pending)
        `uvm_error("RM_STORE_NOT_DRAINED","最终Store-table比较尚未完成")
endfunction

// 抽象职责：按issue时独立冻结的masked映射核对架构地址、权限和内存类型。
// 不改原实现几何及数据期望，不查询当前映射；参考不足保留MISSING责任。
function void memblock_rm::observer_check_pmm_arch_reference(rm_ls_program_item_t item);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::tlb_request_context_view_t context_v;
    memblock_rm_readonly_api::tlb_entry_view_t reference;
    memblock_rm_readonly_api::pma_pmp_af_view_for_rm_t pma_view;
    bit [63:0] masked_va, reference_pa, ignored_pa, first_pa;
    bit [51:0] masked_vpn, loaded_vpn;
    bit [23:0] reference_exception, implementation_exception;
    bit [1:0] first_pbmt;
    bit force_s1,force_s2,store_access,reference_mmio,reference_cacheable;
    bit reference_uncache,model_enabled,first_split_data_stop,reference_loaded;
    bit all_pbmt_pma,first_mmio,first_pma_fault,misaligned;
    bit byte_access_fault,byte_mmio;
    int unsigned checked_bytes,pa_count;
    bit pma_fault,reference_alignment_af,pa_mismatch;
    rm_pmm_reference_result_e range_result;
    pma_pmp_cmd_e cmd;
    if(item==null || !item.pmm_address_semantic_mismatch) return;
    ro=memblock_rm_readonly_api::get();
    store_access=item.kind==RM_LS_KIND_STORE;
    implementation_exception=item.expected_exception;
    misaligned=(item.effective_fullva & (64'(item.size_bytes)-1))!=0;
    first_split_data_stop=!store_access &&
        (item.expected_dcache_denied || item.expected_dcache_corrupt) &&
        item.pa_valid_mask!=(16'hffff>>(16-item.size_bytes));
    checked_bytes=item.size_bytes;
    if(first_split_data_stop) begin
        // 首页独立D错误终止访问，只核对实际消费的PA前缀。
        checked_bytes=0;
        while(checked_bytes<item.size_bytes && item.pa_valid_mask[checked_bytes]) checked_bytes++;
        if(checked_bytes==0) begin item.pmm_reference_result=RM_PMM_REFERENCE_MISSING;return;end
        if(item.expected_dcache_denied) implementation_exception[5]=0;
    end
    cmd=store_access ? PMA_PMP_CMD_WRITE : item.hlvx ? PMA_PMP_CMD_READ_EXEC : PMA_PMP_CMD_READ;
    model_enabled=seq_csr_common::get_pma_pmp_model_en();
    reference_exception=0;reference_cacheable=1;reference_mmio=model_enabled;
    reference_uncache=0;all_pbmt_pma=1;first_pbmt=0;first_pa=0;
    pma_fault=0;first_mmio=0;first_pma_fault=0;pa_mismatch=0;reference_loaded=0;
    reference_alignment_af=0;pa_count=0;
    item.pmm_implementation_pa=item.pa_by_byte[0];
    for(int unsigned i=0;i<checked_bytes;i++) begin
        masked_va=item.effective_fullva+64'(i);masked_vpn=masked_va[63:12];
        if(!reference_loaded || loaded_vpn!=masked_vpn) begin
            if(!ro.read_pmm_arch_reference_for_rm(item.uid,item.dynamic_epoch,
                '{flag:item.rob.flag,value:MEMBLOCK_ROB_VALUE_W'(item.rob.value)},
                masked_vpn,context_v,reference)) begin
                // 首参考缺失时保留原payload真正覆盖两地址时已有的证明；
                // 已消费前页后缺后页则本次完整证明必然不足。
                if(reference_loaded) item.pmm_reference_result=RM_PMM_REFERENCE_MISSING;
                return;
            end
            item.pmm_reference_result=RM_PMM_REFERENCE_MISSING;
            if(!context_v.valid || !reference.valid || context_v.s2xlate!=2 ||
               reference.s1_stage_active || !reference.s2_stage_active ||
               reference.s2_translation_mode_at_build!=9) return;
            reference_loaded=1;loaded_vpn=masked_vpn;
            if(!observer_eval_pbmt_fault_overlay(reference,context_v,0,1,force_s1,force_s2)) return;
            if(reference.pma_af || reference.fault_effective_s2_gaf)
                reference_exception[store_access?7:5]=1;
            else if(reference.fault_effective_s2_gpf || force_s2 ||
                observer_stage_permission_fault(reference,context_v,store_access,0,item.hlvx))
                reference_exception[store_access?23:21]=1;
            // Load后段NC在LoadUnit中不被翻译异常屏蔽，mabuf先处理nc再处理异常。
            // IO的mmio则由LoadUnit在异常时清零，不能把IO+GPF也覆盖为misalign。
            if(i>0 && !store_access && misaligned && reference.s2_entry_pbmt==1) begin
                reference_exception=0;reference_exception[4]=1;break;
            end
            // 首个参考fault终止，不要求后续masked页或正常PPN/PMA。
            if(reference_exception!=0) break;
            if(!context_v.request_translation_valid) return;
        end
        range_result=rm_ls_check_pmm_stage2_reference(masked_vpn,masked_vpn,
            reference.lookup_key.vpn,reference.s2_level,reference.s2_pte_n,
            reference.s2_entry_ppn_raw,masked_va[11:0],ignored_pa,reference_pa);
        if(range_result==RM_PMM_REFERENCE_MISSING) return;
        pa_count++;
        if(i==0) begin
            first_pa=reference_pa;item.pmm_architectural_pa=reference_pa;
            first_pbmt=reference.s2_entry_pbmt;
        end
        all_pbmt_pma &= reference.s2_entry_pbmt==0;
        reference_uncache |= reference.s2_entry_pbmt!=0;
        if(model_enabled) begin
            if(!ro.read_pma_pmp_af_for_rm(item.uid,item.dynamic_epoch,1,
                reference_pa[47:0],1,cmd,pma_view) || !pma_view.valid ||
                !pma_view.translation_eligible || !pma_view.af_decided) return;
            byte_access_fault=store_access?pma_view.st_access_fault:
                (pma_view.ld_access_fault || (item.hlvx && pma_view.instr_access_fault));
            byte_mmio=reference.s2_entry_pbmt==0 && !pma_view.normal_cacheable &&
                !byte_access_fault && !pma_view.st_access_fault;
            // 无完整size查询时也不能丢失首byte或后页已独立证明的AF。
            pma_fault |= byte_access_fault;
            if(i==0) begin
                first_pma_fault=byte_access_fault;first_mmio=byte_mmio;
            end
            // 后页真实PMA-MMIO且无权限异常时，mabuf同样委托软件处理非对齐。
            if(i>0 && !store_access && misaligned && byte_mmio && !pma_fault) begin
                reference_exception=0;reference_exception[4]=1;break;
            end
            reference_cacheable &= pma_view.normal_cacheable &&
                !(store_access?pma_view.st_access_fault:
                  (pma_view.ld_access_fault || (item.hlvx && pma_view.instr_access_fault)));
            reference_mmio &= reference.s2_entry_pbmt==0 && !pma_view.normal_cacheable &&
                !pma_view.st_access_fault && !(store_access?pma_view.st_access_fault:
                  (pma_view.ld_access_fault || (item.hlvx && pma_view.instr_access_fault)));
        end
        if(item.pa_valid_mask[i] && item.pa_by_byte[i]!=reference_pa) pa_mismatch=1;
        // 与原生产路径一致：首页立即fault/不支持非对齐时不消费未访问的后页。
        if(i==0 && (item.effective_fullva & (64'(item.size_bytes)-1))!=0 &&
           (first_pma_fault || first_mmio ||
            observer_should_expect_addr_misaligned(1,store_access,context_v,all_pbmt_pma))) begin
            pma_fault=first_pma_fault;break;
        end
    end
    if(!reference_loaded) return;
    reference_cacheable &= all_pbmt_pma;
    reference_uncache |= reference_mmio;
    if(reference_exception==0) begin
        if(model_enabled && pa_count==item.size_bytes && !first_split_data_stop &&
           !first_pma_fault && !first_mmio) begin
            if(!ro.read_pma_pmp_af_for_rm(item.uid,item.dynamic_epoch,1,
                first_pa[47:0],item.size_bytes,cmd,pma_view) || !pma_view.valid ||
                !pma_view.translation_eligible || !pma_view.af_decided) return;
            pma_fault |= store_access?pma_view.st_access_fault:
                (pma_view.ld_access_fault || (item.hlvx && pma_view.instr_access_fault));
        end
        if((item.effective_fullva & (64'(item.size_bytes)-1))!=0) begin
            reference_alignment_af=first_pbmt==2 || (first_pbmt==0 && first_mmio);
            if(!reference_alignment_af && (!pma_fault || (!store_access && first_pbmt==1)) &&
               observer_should_expect_addr_misaligned(1,store_access,context_v,all_pbmt_pma))
                reference_exception[store_access?6:4]=1;
        end
        if(pma_fault || reference_alignment_af) reference_exception[store_access?7:5]=1;
    end
    // 下游D错误沿原证据链检查；仅比较本阶段独立可判的cause和成功路径。
    if((implementation_exception & 24'hA0A0F0)!=(reference_exception & 24'hA0A0F0))
        item.pmm_reference_result=RM_PMM_REFERENCE_MISMATCH;
    else if(reference_exception==0 && (pa_mismatch || first_pbmt!=item.translation_pbmt ||
        reference_cacheable!=item.all_normal_cacheable || reference_uncache!=item.uncache_path))
        item.pmm_reference_result=RM_PMM_REFERENCE_MISMATCH;
    else item.pmm_reference_result=RM_PMM_REFERENCE_MATCH;
endfunction

// 抽象职责：按顶层异常地址扩展规则重建非预异常VA，不消费DUT地址。
function bit [63:0] memblock_rm::observer_extend_exception_va(
    bit [63:0] va, memblock_rm_readonly_api::tlb_request_context_view_t context_view);
    bit guest;
    bit [3:0] s1mode;
    guest=context_view.priv_virt || context_view.is_hypervisor_inst;
    s1mode=guest ? context_view.vsatp_mode : context_view.satp_mode;
    if(!guest && context_view.priv_dmode==3) return {16'b0,va[47:0]};
    if(s1mode==8) return {{25{va[38]}},va[38:0]};
    if(s1mode==9) return {{16{va[47]}},va[47:0]};
    if(guest && context_view.hgatp_mode==8) return {23'b0,va[40:0]};
    if(guest && context_view.hgatp_mode==9) return {14'b0,va[49:0]};
    return {16'b0,va[47:0]};
endfunction

// 抽象职责：从原始S1 sector解析Guest故障地址及隐式VS页表标记。
function void memblock_rm::observer_compute_guest_fault_address(
    rm_ls_program_item_t item,
    memblock_rm_readonly_api::tlb_request_context_view_t context_view,
    memblock_rm_readonly_api::tlb_entry_view_t entry_view,
    bit [63:0] request_va);
    bit leaf, fake, nonleaf;
    bit [63:0] cross_va;
    bit [51:0] vpn;
    bit [43:0] canonical_ppn, gvpn;
    bit [1:0] vpn_idx;
    bit [11:0] offset;
    cross_va=item.effective_fullva[12]!=request_va[12] ? request_va : item.effective_fullva;
    item.expected_gpaddr_valid=1;
    item.expected_vs_nonleaf=0;
    if(context_view.s2xlate==2) begin item.expected_gpaddr=cross_va;return;end
    if(!entry_view.valid || !entry_view.s1_stage_active) begin
        item.expected_gpaddr_valid=0;return;
    end
    leaf=entry_view.s1_pte_v && (entry_view.s1_pte_r || entry_view.s1_pte_w || entry_view.s1_pte_x);
    fake=!entry_view.s1_pte_v && !entry_view.fault_effective_s1_pf && !entry_view.fault_effective_s1_af;
    nonleaf=entry_view.s1_pte_v && !(entry_view.s1_pte_r || entry_view.s1_pte_w || entry_view.s1_pte_x) &&
        !entry_view.fault_effective_s1_pf && !entry_view.fault_effective_s1_af;
    vpn=request_va[63:12];
    canonical_ppn={entry_view.s1_entry_ppn_raw,entry_view.s1_ppn_low[vpn[2:0]]};
    gvpn=canonical_ppn;
    if(!nonleaf) begin
        case(entry_view.s1_level)
            3:gvpn={canonical_ppn[43:27],vpn[26:0]};
            2:gvpn={canonical_ppn[43:18],vpn[17:0]};
            1:gvpn={canonical_ppn[43:9],vpn[8:0]};
            0:if(entry_view.s1_pte_n) gvpn={canonical_ppn[43:4],vpn[3:0]};
        endcase
    end
    vpn_idx=fake ? (context_view.vsatp_mode==8 ? 2'd2 : 2'd3) : entry_view.s1_level-2'd1;
    offset=leaf ? cross_va[11:0] : 12'(((cross_va>>(12+9*int'(vpn_idx))) & 64'h1ff)<<3);
    item.expected_gpaddr={8'b0,gvpn,offset};
    item.expected_vs_nonleaf=fake || nonleaf;
endfunction

// 抽象职责：按冻结身份消费已提交异常的地址责任；数据commit只发生一次。
function void memblock_rm::observer_service_exception_addresses(bit final_check);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::backend_exception_address_view_t actual;
    rm_ls_program_item_t item;
    bit ready, identity_ok, mismatch, guest_fault;
    ro=memblock_rm_readonly_api::get();
    // 只推进已到齐的责任前缀；队首等待立即停止，不重复扫描历史或整个待核队列。
    for(int index=0;index<observer_pending_address.size();) begin
        item=observer_pending_address[index];
        ready=ro.read_backend_exception_address_for_rm(item.uid,item.dynamic_epoch,
            '{flag:item.rob.flag,value:MEMBLOCK_ROB_VALUE_W'(item.rob.value)},actual);
        if(!ready || actual.pending || (!actual.captured && !actual.missing && !actual.canceled)) begin
            if(!final_check) break;
            `uvm_error("RM_EXCEPTION_ADDRESS_MISSING",$sformatf("uid=%0d epoch=%0d数据已消费但地址责任未完成",item.uid,item.dynamic_epoch))
            observer_pending_address.delete(index);continue;
        end
        // reset或已覆盖的异常允许取消地址责任；取消必须仍属于同一冻结实例。
        if(actual.canceled && actual.valid && actual.required && !actual.captured && !actual.missing &&
           actual.cancel_reason inside {int'(memblock_backend_cycle_frame_t::CANCELLED_RESET),
                                        int'(memblock_backend_cycle_frame_t::CANCELLED_COVERED)} &&
           (!item.address_inputs_valid || (actual.reset_epoch==item.reset_epoch &&
             actual.issue_epoch==item.issue_epoch && actual.replay_seq==item.replay_seq))) begin
            `uvm_info("RM_EXCEPTION_ADDRESS_CANCELLED",$sformatf("uid=%0d frame=%0d合法取消地址责任 reason=%0d",item.uid,actual.frame_id,actual.cancel_reason),UVM_LOW)
            observer_pending_address.delete(index);continue;
        end
        identity_ok=actual.valid && actual.required && actual.captured && !actual.missing && !actual.canceled &&
            actual.sample_seq==actual.target_sample && actual.sample_seq==actual.input_sample+2;
        if(item.address_inputs_valid) identity_ok &= actual.reset_epoch==item.reset_epoch &&
            actual.issue_epoch==item.issue_epoch && actual.replay_seq==item.replay_seq;
        guest_fault=item.expected_exception[21] || item.expected_exception[23];
        mismatch=!identity_ok || !item.expected_address_valid || actual.vaddr !== item.expected_vaddr;
        if(guest_fault) mismatch |= !item.expected_gpaddr_valid ||
            actual.gpaddr !== item.expected_gpaddr || actual.is_for_vs_non_leaf !== item.expected_vs_nonleaf;
        if(mismatch)
            `uvm_error("RM_EXCEPTION_ADDRESS_MISMATCH",$sformatf("uid=%0d epoch=%0d frame=%0d identity=%0d VA actual=%h expected=%h GPA actual=%h expected=%h valid=%0d VSnonleaf actual=%b expected=%b",item.uid,item.dynamic_epoch,actual.frame_id,identity_ok,actual.vaddr,item.expected_vaddr,actual.gpaddr,item.expected_gpaddr,item.expected_gpaddr_valid,actual.is_for_vs_non_leaf,item.expected_vs_nonleaf))
        else
            `uvm_info("RM_EXCEPTION_ADDRESS",$sformatf("uid=%0d frame=%0d地址责任PASS sample=%0d",item.uid,actual.frame_id,actual.sample_seq),UVM_LOW)
        observer_pending_address.delete(index);
    end
endfunction

function int unsigned memblock_rm::observer_scalar_size(bit [8:0] fu_op_type);
    case (fu_op_type)
        MEMBLOCK_LSUOP_LB,
        MEMBLOCK_LSUOP_LBU,
        MEMBLOCK_LSUOP_SB, MEMBLOCK_LSUOP_HLVB, MEMBLOCK_LSUOP_HLVBU: return 1;
        MEMBLOCK_LSUOP_LH,
        MEMBLOCK_LSUOP_LHU,
        MEMBLOCK_LSUOP_SH, MEMBLOCK_LSUOP_HLVH, MEMBLOCK_LSUOP_HLVHU,
        MEMBLOCK_LSUOP_HLVXHU: return 2;
        MEMBLOCK_LSUOP_LW,
        MEMBLOCK_LSUOP_LWU,
        MEMBLOCK_LSUOP_SW, MEMBLOCK_LSUOP_HLVW, MEMBLOCK_LSUOP_HLVWU,
        MEMBLOCK_LSUOP_HLVXWU: return 4;
        MEMBLOCK_LSUOP_LD,
        MEMBLOCK_LSUOP_SD, MEMBLOCK_LSUOP_HLVD: return 8;
        default: return 0;
    endcase
endfunction:observer_scalar_size

function int unsigned memblock_rm::observer_atomic_size(bit [8:0] fu_op_type);
    case (fu_op_type)
        MEMBLOCK_LSUOP_LR_W,
        MEMBLOCK_LSUOP_SC_W,
        MEMBLOCK_LSUOP_AMOSWAP_W,
        MEMBLOCK_LSUOP_AMOADD_W,
        MEMBLOCK_LSUOP_AMOXOR_W,
        MEMBLOCK_LSUOP_AMOAND_W,
        MEMBLOCK_LSUOP_AMOOR_W,
        MEMBLOCK_LSUOP_AMOMIN_W,
        MEMBLOCK_LSUOP_AMOMAX_W,
        MEMBLOCK_LSUOP_AMOMINU_W,
        MEMBLOCK_LSUOP_AMOMAXU_W,
        MEMBLOCK_LSUOP_AMOCAS_W: return 4;
        MEMBLOCK_LSUOP_LR_D,
        MEMBLOCK_LSUOP_SC_D,
        MEMBLOCK_LSUOP_AMOSWAP_D,
        MEMBLOCK_LSUOP_AMOADD_D,
        MEMBLOCK_LSUOP_AMOXOR_D,
        MEMBLOCK_LSUOP_AMOAND_D,
        MEMBLOCK_LSUOP_AMOOR_D,
        MEMBLOCK_LSUOP_AMOMIN_D,
        MEMBLOCK_LSUOP_AMOMAX_D,
        MEMBLOCK_LSUOP_AMOMINU_D,
        MEMBLOCK_LSUOP_AMOMAXU_D,
        MEMBLOCK_LSUOP_AMOCAS_D: return 8;
        MEMBLOCK_LSUOP_AMOCAS_Q: return 16;
        default: return 0;
    endcase
endfunction:observer_atomic_size

function rm_atomic_kind_e memblock_rm::observer_atomic_kind(bit [8:0] fu_op_type);
    if (fu_op_type inside {MEMBLOCK_LSUOP_LR_W, MEMBLOCK_LSUOP_LR_D})
        return RM_ATOMIC_LR;
    if (fu_op_type inside {MEMBLOCK_LSUOP_SC_W, MEMBLOCK_LSUOP_SC_D})
        return RM_ATOMIC_SC;
    if (fu_op_type inside {MEMBLOCK_LSUOP_AMOCAS_W, MEMBLOCK_LSUOP_AMOCAS_D,
                           MEMBLOCK_LSUOP_AMOCAS_Q}) return RM_ATOMIC_CAS;
    if (memblock_op_behavior_util::is_amo_fuoptype(fu_op_type))
        return RM_ATOMIC_AMO;
    return RM_ATOMIC_NONE;
endfunction:observer_atomic_kind

function bit [63:0] memblock_rm::observer_compute_vaddr(bit [63:0] src_0,
                                                        bit [63:0] imm);
    bit [63:0] signed_imm;
    signed_imm = {{52{imm[11]}}, imm[11:0]};
    return src_0 + signed_imm;
endfunction:observer_compute_vaddr

function bit memblock_rm::observer_stage_permission_fault(
    memblock_rm_readonly_api::tlb_entry_view_t entry,
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
    bit store_access,
    bit stage_one,
    bit hlvx
);
    bit pte_r;
    bit pte_w;
    bit pte_x;
    bit pte_u;
    bit pte_a;
    bit pte_d;
    bit pte_v;
    bit mxr;
    bit sum;
    bit [1:0] privilege;
    bit operation_allowed;
    bit privilege_allowed;

    if (stage_one) begin
        if (!entry.s1_stage_active) return 1'b0;
        pte_r = entry.s1_pte_r; pte_w = entry.s1_pte_w;
        pte_x = entry.s1_pte_x; pte_u = entry.s1_pte_u;
        pte_a = entry.s1_pte_a; pte_d = entry.s1_pte_d;
        pte_v = entry.s1_pte_v;
        // 宿主 S1 使用 HS MXR，VS-stage 使用 HS MXR 与 VS MXR 的逻辑或。
        mxr = tlb_context.s2xlate == 2'd0 ? tlb_context.priv_mxr :
              (tlb_context.priv_mxr || tlb_context.priv_vmxr);
        sum = tlb_context.s2xlate == 0 ? tlb_context.priv_sum : tlb_context.priv_vsum;
        privilege = tlb_context.is_hypervisor_inst ? {1'b0,tlb_context.priv_spvp} : tlb_context.priv_dmode;
    end else begin
        if (!entry.s2_stage_active) return 1'b0;
        pte_r = entry.s2_pte_r; pte_w = entry.s2_pte_w;
        pte_x = entry.s2_pte_x; pte_u = entry.s2_pte_u;
        pte_a = entry.s2_pte_a; pte_d = entry.s2_pte_d;
        // 最终 G-stage 的显式读权限仅使用 HS MXR。
        pte_v = 1'b1; mxr = tlb_context.priv_mxr; sum = 1'b1;
        privilege = 2'd3;
    end
    operation_allowed = store_access ? (pte_w && pte_a && pte_d) :
                                       ((hlvx ? pte_x : (pte_r || (pte_x && mxr))) && pte_a);
    privilege_allowed = 1'b1;
    if (stage_one) begin
        if (privilege == 2'd0) privilege_allowed = pte_u;
        else if (privilege == 2'd1) privilege_allowed = !pte_u || sum;
    end
    // fake/nonleaf为G-for-VS隐式访问载荷，由G-stage决定，不制造S1 PF。
    if(stage_one && entry.s2_stage_active &&
       (tlb_context.priv_virt || tlb_context.is_hypervisor_inst) &&
       !entry.fault_effective_s1_pf && !entry.fault_effective_s1_af &&
       ((!pte_v) || !(pte_r || pte_w || pte_x))) return 0;
    return !pte_v || (stage_one && pte_w && !pte_r) || !operation_allowed || !privilege_allowed;
endfunction:observer_stage_permission_fault

// 中文注释：RM 在每个已命中的 TLB entry 上重建 PBMT/PBMTE 的 response-visible
// fault 语义。输入均来自 UID 冻结 context 和 readonly entry；本函数不读取
// runtime current CSR，也不修改 entry，只输出本次访问的局部 force 标记。
function bit memblock_rm::observer_eval_pbmt_fault_overlay(
    memblock_rm_readonly_api::tlb_entry_view_t entry,
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
    bit s1_active,
    bit s2_active,
    output bit force_s1_pf,
    output bit force_s2_gpf
);
    return memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, s1_active, s2_active,
        force_s1_pf, force_s2_gpf);
endfunction:observer_eval_pbmt_fault_overlay

// 中文注释：V2 TLB.pbmt_check() 在 allStage 中优先非零 S1 PBMT，否则使用
// S2 PBMT。这里重建同一选择，仅用于 RM 的地址异常建模，不把 raw stage
// payload 直接当成最终 memory type。
function bit memblock_rm::observer_get_effective_pbmt(
    memblock_rm_readonly_api::tlb_entry_view_t entry,
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
    bit s1_active,
    bit s2_active,
    output bit [1:0] effective_pbmt
);
    effective_pbmt = 2'd0;
    if (!entry.valid || !tlb_context.valid ||
        entry.s1_stage_active != s1_active ||
        entry.s2_stage_active != s2_active ||
        entry.s2xlate != tlb_context.s2xlate) begin
        return 1'b0;
    end
    case (tlb_context.s2xlate)
        2'd0,
        2'd1: begin
            if (!s1_active || s2_active) return 1'b0;
            effective_pbmt = entry.s1_entry_pbmt;
        end
        2'd2: begin
            if (s1_active || !s2_active) return 1'b0;
            effective_pbmt = entry.s2_entry_pbmt;
        end
        2'd3: begin
            if (!s1_active || !s2_active) return 1'b0;
            effective_pbmt = entry.s1_entry_pbmt != 2'd0 ?
                             entry.s1_entry_pbmt : entry.s2_entry_pbmt;
        end
        default: return 1'b0;
    endcase
    return effective_pbmt != 2'b11;
endfunction:observer_get_effective_pbmt

function bit memblock_rm::observer_should_expect_addr_misaligned(
    bit misaligned,
    bit store_access,
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
    bit all_bytes_pma_pbmt
);
    bit hd_misalign_enabled;

    if (!misaligned) return 1'b0;
    hd_misalign_enabled = store_access ? tlb_context.hd_misalign_st_enable :
                                         tlb_context.hd_misalign_ld_enable;
    // V2 hardware handles misaligned accesses for both PMA C=1 and C=0
    // when the PMA/PMP permissions allow the access.  C=0 alone does not
    // imply an address-misaligned exception; only PBMT-NC or actual MMIO
    // paths do so in the DUT.  Access faults are already reflected in
    // expected_exception before this helper is called.
    if (hd_misalign_enabled && all_bytes_pma_pbmt) begin
        return 1'b0;
    end
    return 1'b1;
endfunction:observer_should_expect_addr_misaligned

// 中文注释：RM独立证明已翻译首页的普通cache访问，在查询未执行高页前预测数据异常。
// DUT仅提供同ROB执行身份和写回时间上界，错误值来自独立D.fire历史。
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
                                      item.pa_by_byte[idx][47:0], 1, item.hlvx ? PMA_PMP_CMD_READ_EXEC : PMA_PMP_CMD_READ, byte_view) ||
            !byte_view.valid || !byte_view.translation_eligible || !byte_view.af_decided ||
            !byte_view.normal_cacheable || byte_view.ld_access_fault ||
            (item.hlvx && byte_view.instr_access_fault)) return 1'b0;
    end
    rob_key = '{flag: item.rob.flag, value: item.rob.value};
    if (!ro.read_dut_load_instance_for_rm(item.uid,rob_key,item.dynamic_epoch,load_view) ||
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
endfunction:observer_try_apply_first_split_data_error

function bit memblock_rm::observer_build_commit_item(
    memblock_rm_readonly_api::main_transaction_view_t main_view,
    memblock_rm_readonly_api::status_view_t status_view,
    output rm_ls_program_item_t item
);
    bit deferred;
    return observer_build_commit_item_with_observation(main_view, status_view, item, deferred);
endfunction:observer_build_commit_item

function bit memblock_rm::observer_build_commit_item_with_observation(
    memblock_rm_readonly_api::main_transaction_view_t main_view,
    memblock_rm_readonly_api::status_view_t status_view,
    output rm_ls_program_item_t item,
    output bit deferred
);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context;
    memblock_rm_readonly_api::tlb_entry_view_t entry;
    memblock_rm_readonly_api::tlb_entry_view_t response_entry;
    memblock_rm_readonly_api::tlb_response_provenance_view_t response_provenance;
    memblock_rm_readonly_api::later_split_load_response_view_t
        later_split_load_response;
    memblock_rm_readonly_api::pma_pmp_af_view_for_rm_t pma_pmp_view;
    pma_pmp_cmd_e pma_pmp_cmd;
    bit translation_access_fault;
    bit pma_pmp_access_fault;
    bit stage_one_fault;
    bit stage_two_fault;
    bit entry_stage_one_fault;
    bit entry_stage_two_fault;
    bit entry_translation_fault;
    bit entry_target_translation_fault;
    bit first_byte_translation_fault;
    bit later_split_translation_fault;
    bit later_split_response_proven;
    bit later_split_response_is_nc;
    bit later_split_l2_completion_proven;
    bit later_split_fault_stage_one;
    bit later_split_fault_stage_two;
    bit later_split_fault_s1_af_or_s2_gaf;
    bit later_split_uncache_override;
    bit response_stage_one_fault;
    bit response_stage_two_fault;
    bit response_target_translation_fault;
    bit [1:0] response_effective_pbmt;
    bit dut_stops_after_first_page;
    bit first_byte_is_pma_mmio;
    bit first_byte_pma_known;
    bit first_byte_pma_fault;
    bit pbmt_force_s1_pf;
    bit pbmt_force_s2_gpf;
    bit [1:0] effective_pbmt;
    bit [1:0] first_effective_pbmt;
    bit first_effective_pbmt_valid;
    bit misaligned;
    bit misaligned_maps_to_access_fault;
    bit all_bytes_pma_pbmt;
    bit all_bytes_pma_mmio;
    bit all_bytes_normal_cacheable;
    memblock_rm_readonly_api::pma_pmp_af_view_for_rm_t byte_pma_pmp_view;
    bit store_access;
    bit atomic_access;
    bit cmo_access;
    bit exception_store_access;
    bit s1_active;
    bit s2_active;
    bit bare_identity;
    bit fault_segment_selected;
    bit [63:0] byte_va;
    bit [63:0] fault_request_va;
    memblock_rm_readonly_api::tlb_entry_view_t fault_entry;
    bit [51:0] first_vpn;
    bit [43:0] final_ppn;
    bit [3:0] expected_s1_mode;
    int unsigned bad_byte_index;
    bit [15:0] required_pa_mask;
    rm_ls_error_e translation_path_error;

    deferred = 1'b0;
    item = new(main_view.uid);
    pma_pmp_view = '{default:'0};
    item.valid = 1'b1;
    item.uid = main_view.uid;
    item.op = main_view.fu_op_type;
    item.main_vaddr = main_view.vaddr;
    item.rob.flag = main_view.rob_idx_flag;
    item.rob.value = main_view.rob_idx_value;
    item.lq_flag = main_view.lq_idx_flag;
    item.lq_value = main_view.lq_idx_value;
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
    item.hyperinst=memblock_op_behavior_util::is_guest_memory(main_view.fu_type,main_view.fu_op_type);
    item.hlvx=memblock_op_behavior_util::is_hlvx(main_view.fu_type,main_view.fu_op_type);
    atomic_access = main_view.fu_type == MEMBLOCK_FUTYPE_MOU &&
                    memblock_op_behavior_util::is_amo_fuoptype(main_view.fu_op_type);
    // CMO只翻译请求地址；1B仅作为复用翻译循环的局部几何，返回前恢复CMO的0宽度。
    item.size_bytes = cmo_access ? 1 : atomic_access ? observer_atomic_size(main_view.fu_op_type) :
                                      observer_scalar_size(main_view.fu_op_type);
    item.computed_vaddr = (atomic_access || item.hyperinst) ? main_view.src_0 :
                                          observer_compute_vaddr(main_view.src_0,
                                                                 main_view.imm);
    item.is_signed = main_view.fu_type == MEMBLOCK_FUTYPE_LDU &&
                    (main_view.fu_op_type == MEMBLOCK_LSUOP_LB ||
                     main_view.fu_op_type == MEMBLOCK_LSUOP_LH ||
                     main_view.fu_op_type == MEMBLOCK_LSUOP_LW ||
                     main_view.fu_op_type == MEMBLOCK_LSUOP_LD ||
                     (item.hyperinst && main_view.fu_op_type inside {
                         MEMBLOCK_LSUOP_HLVB,MEMBLOCK_LSUOP_HLVH,MEMBLOCK_LSUOP_HLVW,MEMBLOCK_LSUOP_HLVD}));
    item.fp_wen = main_view.op_class == MEMBLOCK_OP_CLASS_FP_LOAD;
    if (cmo_access) begin
        item.kind = RM_LS_KIND_CMO;
    end else if (atomic_access) begin
        item.kind = RM_LS_KIND_ATOMIC;
        item.atomic_kind = observer_atomic_kind(main_view.fu_op_type);
    end else if (main_view.fu_type == MEMBLOCK_FUTYPE_LDU &&
        memblock_op_behavior_util::is_load_fuoptype(main_view.fu_op_type)) begin
        item.kind = RM_LS_KIND_LOAD;
    end else if (main_view.fu_type == MEMBLOCK_FUTYPE_STU &&
                 memblock_op_behavior_util::is_store_fuoptype(main_view.fu_op_type)) begin
        item.kind = RM_LS_KIND_STORE;
    end else begin
        item.kind = RM_LS_KIND_OTHER;
    end
    if (item.kind == RM_LS_KIND_OTHER || item.size_bytes == 0) begin
        item.translation_valid = 1'b1;
        if (!observer_trace_translation_emitted.exists(item.uid)) begin
            `uvm_info("RM_LS_TRACE_TRANSLATION",
                      $sformatf("node=TRANSLATION uid=%0d rob=%0d/%0d kind=%s path=NOT_REQUIRED va=0x%0h pa_mask=0x0 pbmt_force_s1_pf=%0d pbmt_force_s2_gpf=%0d expected_exception=0x%0h",
                                item.uid, item.rob.flag, item.rob.value,
                                observer_kind_name(item.kind), item.computed_vaddr,
                                item.expected_pbmt_forced_s1_pf,
                                item.expected_pbmt_forced_s2_gpf,
                                item.expected_exception),
                      UVM_LOW)
            observer_trace_translation_emitted[item.uid] = 1'b1;
        end
        return 1'b1;
    end
    if(item.hyperinst && main_view.imm!=0) begin
        ls_model.set_error(RM_LS_ERR_MAIN_VADDR_MISMATCH,"特殊访存要求imm=0、地址独立取src_0"); return 0;
    end
    if (item.kind == RM_LS_KIND_STORE) begin
        if(item.hyperinst && !main_view.atomic_std_valid_mask[0]) begin
            ls_model.set_error(RM_LS_ERR_ATOMIC_OPERAND_NOT_READY,"HSV缺少冻结STD数据入口");return 0;
        end
        item.store_data = main_view.atomic_std_valid_mask[0] ?
                          main_view.atomic_std_data_flat[63:0] : main_view.src_0;
        item.store_byte_mask = 16'hffff >> (16 - item.size_bytes);
    end else if (item.kind == RM_LS_KIND_ATOMIC) begin
        bit [3:0] required_std_mask;
        required_std_mask = memblock_op_behavior_util::atomic_required_std_mask(
            main_view.fu_op_type);
        item.atomic_std_valid_mask = main_view.atomic_std_valid_mask;
        if ((item.atomic_std_valid_mask & required_std_mask) != required_std_mask ||
            item.atomic_std_valid_mask != required_std_mask) begin
            ls_model.set_error(RM_LS_ERR_ATOMIC_OPERAND_NOT_READY,
                               $sformatf("uid %0d atomic frozen STD mask expected=0x%0h actual=0x%0h",
                                         item.uid, required_std_mask,
                                         item.atomic_std_valid_mask));
            return 1'b0;
        end
        foreach (item.atomic_std_data[slot]) begin
            item.atomic_std_data[slot] =
                main_view.atomic_std_data_flat[(slot * 64) +: 64];
        end
        item.atomic_operand[63:0] = item.atomic_std_data[0];
        item.atomic_compare_value[63:0] = item.atomic_std_data[0];
        item.atomic_new_value[63:0] = item.atomic_std_data[1];
        if (item.size_bytes == 16) begin
            item.atomic_compare_value[127:64] = item.atomic_std_data[2];
            item.atomic_new_value[127:64] = item.atomic_std_data[3];
        end
        item.expected_atomic_wb_count = item.size_bytes == 16 ? 2 : 1;
        item.atomic_first_issue_sample =
            observer_atomic_first_issue_sample.exists(item.uid) ?
            observer_atomic_first_issue_sample[item.uid] : 0;
        item.atomic_expected_pdest_valid =
            observer_atomic_sta_pdest_valid.exists(item.uid) ?
            observer_atomic_sta_pdest_valid[item.uid] : '0;
        if (observer_atomic_sta_pdest_flat.exists(item.uid)) begin
            item.atomic_expected_pdest[0] =
                observer_atomic_sta_pdest_flat[item.uid][7:0];
            item.atomic_expected_pdest[1] =
                observer_atomic_sta_pdest_flat[item.uid][15:8];
        end
        if (!observer_atomic_std_checked_mask.exists(item.uid) ||
            (observer_atomic_std_checked_mask[item.uid] & required_std_mask) !=
                required_std_mask ||
            !item.atomic_expected_pdest_valid[0] ||
            (item.size_bytes == 16 && !item.atomic_expected_pdest_valid[1])) begin
            ls_model.set_error(
                RM_LS_ERR_ATOMIC_OPERAND_NOT_READY,
                $sformatf("uid %0d atomic passive issue evidence incomplete std=0x%0h required=0x%0h pdest_valid=0x%0h",
                          item.uid,
                          observer_atomic_std_checked_mask.exists(item.uid) ?
                              observer_atomic_std_checked_mask[item.uid] : '0,
                          required_std_mask,
                          item.atomic_expected_pdest_valid));
            return 1'b0;
        end
        if (main_view.imm != 0 || item.main_vaddr != main_view.src_0) begin
            ls_model.set_error(RM_LS_ERR_MAIN_VADDR_MISMATCH,
                               $sformatf("uid %0d atomic address contract requires imm=0 and vaddr=src0",
                                         item.uid));
            return 1'b0;
        end
    end
    if (item.computed_vaddr != item.main_vaddr) begin
        ls_model.set_error(RM_LS_ERR_MAIN_VADDR_MISMATCH,
                           $sformatf("uid %0d computed VA 0x%0h main VA 0x%0h",
                                     item.uid, item.computed_vaddr, item.main_vaddr));
        return 1'b0;
    end

    ro = memblock_rm_readonly_api::get();
    if (!ro.read_tlb_request_context_for_rm(item.uid,
        (item.kind inside {RM_LS_KIND_LOAD,RM_LS_KIND_STORE}) ? {14'b0,item.computed_vaddr[49:12]} : item.computed_vaddr[63:12], tlb_context) ||
        !tlb_context.valid) begin
        ls_model.set_error(RM_LS_ERR_TLB_CONTEXT_NOT_READY,
                           $sformatf("uid %0d first VPN frozen request context unavailable",
                                     item.uid));
        return 1'b0;
    end
    if (!rm_ls_decode_translation_path(tlb_context.s2xlate,
                                       tlb_context.is_hypervisor_inst,
                                       tlb_context.priv_virt,
                                       tlb_context.priv_dmode,
                                       tlb_context.satp_mode,
                                       tlb_context.vsatp_mode,
                                       tlb_context.hgatp_mode,
                                       s1_active, s2_active, bare_identity,
                                       translation_path_error)) begin
        ls_model.set_error(translation_path_error,
                           $sformatf("uid %0d frozen TLB context inconsistent/unsupported s2xlate=%0d hypervisor=%0d virt=%0d satp=%0d vsatp=%0d hgatp=%0d csr_seq=%0d",
                                     item.uid, tlb_context.s2xlate,
                                     tlb_context.is_hypervisor_inst,
                                     tlb_context.priv_virt, tlb_context.satp_mode,
                                     tlb_context.vsatp_mode, tlb_context.hgatp_mode,
                                     tlb_context.csr_update_seq));
        return 1'b0;
    end
    if(item.kind inside {RM_LS_KIND_LOAD,RM_LS_KIND_STORE}) begin
        item.address_inputs_valid=tlb_context.address_inputs_valid;
        item.request_vaddr=tlb_context.address_inputs_valid ? {14'b0,tlb_context.request_vaddr[49:0]} : item.computed_vaddr;
        item.reset_epoch=tlb_context.reset_epoch;item.issue_epoch=tlb_context.issue_epoch;
        item.replay_seq=tlb_context.replay_seq;item.access_sample=tlb_context.issue_sample;
        item.selected_pmm=rm_ls_select_pmm(item.hlvx,item.hyperinst,
            tlb_context.priv_virt || item.hyperinst,tlb_context.priv_spvp,
            tlb_context.priv_dmode,tlb_context.priv_imode,tlb_context.priv_mxr,tlb_context.priv_vmxr,
            tlb_context.pmm_mseccfg,tlb_context.pmm_menvcfg,tlb_context.pmm_henvcfg,
            tlb_context.pmm_hstatus,tlb_context.pmm_senvcfg);
        if(!rm_ls_compute_address_precheck(
            tlb_context.address_inputs_valid ? tlb_context.request_fullva : item.computed_vaddr,
            item.selected_pmm,item.hyperinst ? {1'b0,tlb_context.priv_spvp} : tlb_context.priv_dmode,
            tlb_context.s2xlate,tlb_context.priv_virt || item.hyperinst,
            tlb_context.address_inputs_valid ? tlb_context.request_valid : 1'b1,
            tlb_context.address_inputs_valid ? tlb_context.checkfullva : 1'b1,
            tlb_context.satp_mode,tlb_context.vsatp_mode,tlb_context.hgatp_mode,
            atomic_access ? 1 : item.size_bytes,item.effective_fullva,item.fault_report_va,
            item.preaf,item.prepf,item.pregpf)) begin
            ls_model.set_error(RM_LS_ERR_PMM_ENCODING,"PMM=01为非法UT控制输入");return 0;
        end
        if(item.address_inputs_valid && (tlb_context.raw_address!=item.computed_vaddr ||
            tlb_context.is_hypervisor_inst!=item.hyperinst)) begin
            ls_model.set_error(RM_LS_ERR_TLB_ENTRY_INCONSISTENT,"冻结访问输入与主表原操作数或特殊访存身份冲突");return 0;
        end
        item.expected_address_valid=1;
        item.expected_vaddr=item.fault_report_va;
        if(|(item.preaf|item.prepf|item.pregpf)) begin
            store_access=item.kind==RM_LS_KIND_STORE || cmo_access ||
                (atomic_access && item.atomic_kind!=RM_ATOMIC_LR);
            item.expected_exception[store_access?7:5]=|item.preaf;
            item.expected_exception[store_access?15:13]=|item.prepf;
            item.expected_exception[store_access?23:21]=|item.pregpf;
            item.expected_gpaddr_valid=|item.pregpf;item.expected_gpaddr=item.fault_report_va;
            item.expected_vs_nonleaf=0;item.translation_valid=1;return 1;
        end
        // 原始vaddr可能仍含被PMM忽略的only-S2有效位；保留实现比较并独立标识语义差异。
        if(item.selected_pmm==3 && tlb_context.s2xlate==2 && tlb_context.hgatp_mode==9 &&
           item.request_vaddr[49:48]!=item.effective_fullva[49:48])
        begin
            item.pmm_address_semantic_mismatch=1;
            item.pmm_reference_result=RM_PMM_REFERENCE_MISSING;
        end
        item.expected_vaddr=observer_extend_exception_va(item.fault_report_va,tlb_context);
    end else begin
        // AMO/CMO保留已有地址及生命周期合同，本新增标量快照不替换它们的来源。
        item.request_vaddr=item.computed_vaddr;
        item.effective_fullva=item.computed_vaddr;item.fault_report_va=item.computed_vaddr;
    end
    fault_request_va=item.request_vaddr;fault_entry='0;fault_segment_selected=0;
    if (!observer_trace_translation_path_emitted.exists(item.uid)) begin
        `uvm_info("RM_LS_TRANSLATION",
                  $sformatf("uid=%0d translation_path=%s satp=%0d vsatp=%0d hgatp=%0d s2xlate=%0d csr_seq=%0d",
                            item.uid, bare_identity ? "BARE_IDENTITY" : "PAGED_ENTRY",
                            tlb_context.satp_mode, tlb_context.vsatp_mode,
                            tlb_context.hgatp_mode, tlb_context.s2xlate,
                            tlb_context.csr_update_seq),
                  UVM_LOW)
        observer_trace_translation_path_emitted[item.uid] = 1'b1;
    end
    item.translation_key = tlb_context.request_key;
    if (!bare_identity) begin
        if (!ro.resolve_tlb_entry_for_rm(item.uid, item.request_vaddr[63:12],
                                         tlb_context, entry)) begin
            ls_model.set_error(RM_LS_ERR_TLB_ENTRY_NOT_READY,
                               $sformatf("uid %0d first VPN completed UID translation unavailable",
                                         item.uid));
            return 1'b0;
        end
        item.translation_key = tlb_context.entry_key;
    end
    if(item.pmm_address_semantic_mismatch && entry.valid && entry.s2_stage_active) begin
        // 同一冻结payload覆盖两个VPN时权限及PBMT同源；仅独立重算PPN/offset。
        // 覆盖不足时保留MISSING，不能用提交时current表补出一个预期别名。
        item.pmm_reference_result=rm_ls_check_pmm_stage2_reference(
            item.request_vaddr[63:12],item.effective_fullva[63:12],entry.lookup_key.vpn,
            entry.s2_level,entry.s2_pte_n,entry.s2_entry_ppn_raw,item.effective_fullva[11:0],
            item.pmm_implementation_pa,item.pmm_architectural_pa);
    end
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
        pma_pmp_cmd = store_access ? PMA_PMP_CMD_WRITE : item.hlvx ? PMA_PMP_CMD_READ_EXEC : PMA_PMP_CMD_READ;
    end
    first_byte_is_pma_mmio = 1'b0;
    first_byte_pma_known = 1'b0;
    first_byte_pma_fault = 1'b0;
    first_effective_pbmt = 2'd0;
    first_effective_pbmt_valid = 1'b0;
    misaligned_maps_to_access_fault = 1'b0;
    all_bytes_pma_pbmt = 1'b1;
    all_bytes_pma_mmio = 1'b0;
    all_bytes_normal_cacheable = 1'b0;
    // 中文注释：以下三类异常只来自冻结 TLB context/entry；main_view 的
    // tlb_af/tlb_pf/tlb_gpf/pma_af/denied/corrupt 均禁止作为 RM 真源。
    translation_access_fault = 1'b0;
    pma_pmp_access_fault = 1'b0;
    stage_one_fault = 1'b0;
    stage_two_fault = 1'b0;
    first_byte_translation_fault = 1'b0;
    later_split_translation_fault = 1'b0;
    later_split_response_proven = 1'b0;
    later_split_response_is_nc = 1'b0;
    later_split_fault_stage_one = 1'b0;
    later_split_fault_stage_two = 1'b0;
    later_split_fault_s1_af_or_s2_gaf = 1'b0;
    later_split_uncache_override = 1'b0;
    first_vpn = item.request_vaddr[63:12];
    if (bare_identity) begin
        if (!item.set_bare_identity_geometry(bad_byte_index)) begin
            ls_model.set_error(RM_LS_ERR_BARE_PA_OUT_OF_RANGE,
                               $sformatf("uid %0d Bare identity byte %0d VA 0x%0h exceeds 48-bit PA domain or wraps",
                                         item.uid, bad_byte_index,
                                         item.computed_vaddr + bad_byte_index));
            return 1'b0;
        end
    end else begin
        expected_s1_mode = tlb_context.s2xlate == 2'd0 ?
                           tlb_context.satp_mode : tlb_context.vsatp_mode;
        // 地址未对齐异常在首个地址字节被确认后，DUT不会为后续页发起翻译
        // 请求。RM仍需处理首页字节，但不能把未被DUT访问的第二页当成缺失
        // 的测试框架翻译状态。
        dut_stops_after_first_page = 1'b0;
        for (int unsigned byte_index = 0; byte_index < item.size_bytes; byte_index++) begin
            byte_va = item.request_vaddr + byte_index;
            if(item.address_inputs_valid) byte_va={14'b0,byte_va[49:0]};
            if (byte_va[63:12] != first_vpn && item.kind == RM_LS_KIND_LOAD &&
                misaligned && tlb_context.hd_misalign_ld_enable && all_bytes_pma_pbmt &&
                !translation_access_fault && !stage_one_fault && !stage_two_fault &&
                !pma_pmp_access_fault && !first_byte_is_pma_mmio) begin
                if (observer_try_apply_first_split_data_error(item, byte_index, deferred)) break;
                if (deferred) return 1'b0;
            end
            if (!ro.resolve_tlb_entry_for_rm(item.uid, byte_va[63:12],
                                             tlb_context, entry)) begin
                ls_model.set_error(RM_LS_ERR_TLB_ENTRY_NOT_READY,
                                   $sformatf("uid %0d byte %0d UID/cross-page translation unavailable",
                                             item.uid, byte_index));
                return 1'b0;
            end
            if (!entry.valid || entry.lookup_key != tlb_context.entry_key ||
                entry.s2xlate != tlb_context.s2xlate ||
                entry.s1_stage_active != s1_active ||
                entry.s2_stage_active != s2_active ||
                (s1_active && entry.s1_translation_mode_at_build != expected_s1_mode) ||
                (s2_active && entry.s2_translation_mode_at_build != tlb_context.hgatp_mode)) begin
                ls_model.set_error(RM_LS_ERR_TLB_ENTRY_INCONSISTENT,
                                   $sformatf("uid %0d byte %0d TLB entry does not match frozen context",
                                             item.uid, byte_index));
                return 1'b0;
            end
            if (!observer_eval_pbmt_fault_overlay(
                    entry, tlb_context, s1_active, s2_active,
                    pbmt_force_s1_pf, pbmt_force_s2_gpf)) begin
                ls_model.set_error(
                    RM_LS_ERR_TLB_ENTRY_INCONSISTENT,
                    $sformatf("uid %0d byte %0d TLB PBMT payload/context is inconsistent s2xlate=%0d s1_pbmt=%0d s2_pbmt=%0d mPBMTE=%0d hPBMTE=%0d",
                              item.uid, byte_index, tlb_context.s2xlate,
                              entry.s1_entry_pbmt, entry.s2_entry_pbmt,
                              tlb_context.m_pbmt_en, tlb_context.h_pbmt_en));
                return 1'b0;
            end
            if (!observer_get_effective_pbmt(
                    entry, tlb_context, s1_active, s2_active,
                    effective_pbmt)) begin
                ls_model.set_error(
                    RM_LS_ERR_TLB_ENTRY_INCONSISTENT,
                    $sformatf("uid %0d byte %0d cannot select response-visible PBMT s2xlate=%0d",
                              item.uid, byte_index, tlb_context.s2xlate));
                return 1'b0;
            end
            all_bytes_pma_pbmt &= effective_pbmt == 2'd0;
            if (byte_index == 0) begin
                // Preserve the exact first-byte type for the pipeline's
                // immediate NC/IO misalign cause. The whole-access boolean is
                // still needed when a later split page becomes NC/IO.
                first_effective_pbmt = effective_pbmt;
                first_effective_pbmt_valid = 1'b1;
                misaligned_maps_to_access_fault =
                    misaligned && effective_pbmt == 2'd2;
                dut_stops_after_first_page =
                    (item.kind == RM_LS_KIND_ATOMIC && misaligned) ||
                    observer_should_expect_addr_misaligned(
                        misaligned, store_access, tlb_context,
                        all_bytes_pma_pbmt);
            end
            item.expected_pbmt_forced_s1_pf |= pbmt_force_s1_pf;
            item.expected_pbmt_forced_s2_gpf |= pbmt_force_s2_gpf;
            if (byte_index == 0) item.translation_key = tlb_context.entry_key;
            translation_access_fault |= entry.pma_af ||
                                        entry.fault_effective_s1_af ||
                                        entry.fault_effective_s2_gaf;
            entry_stage_one_fault = entry.fault_effective_s1_pf ||
                                    ((!entry.fault_effective_s1_pf &&
                                      !entry.fault_effective_s1_af) &&
                                     observer_stage_permission_fault(entry, tlb_context,
                                                                     store_access, 1'b1, item.hlvx)) ||
                                    pbmt_force_s1_pf;
            entry_stage_two_fault = entry.fault_effective_s2_gpf ||
                                    ((!entry.fault_effective_s2_gpf &&
                                      !entry.fault_effective_s2_gaf) &&
                                     observer_stage_permission_fault(entry, tlb_context,
                                                                     store_access, 1'b0, item.hlvx)) ||
                                    pbmt_force_s2_gpf;
            // 翻译AF优先于PF，S1 PF优先于G-stage fault。
            if(entry.fault_effective_s1_af || entry.fault_effective_s2_gaf || entry.pma_af) begin
                entry_stage_one_fault=0;entry_stage_two_fault=0;
            end else if(entry_stage_one_fault) entry_stage_two_fault=0;
            entry_target_translation_fault =
                entry.fault_effective_s1_af ||
                entry.fault_effective_s2_gaf ||
                entry_stage_one_fault ||
                entry_stage_two_fault;
            if (byte_index == 0 && entry_target_translation_fault) begin
                first_byte_translation_fault = 1'b1;
            end
            if (byte_index > 0 && byte_va[63:12] != first_vpn &&
                !first_byte_translation_fault &&
                entry_target_translation_fault) begin
                later_split_translation_fault = 1'b1;
                later_split_fault_stage_one = entry_stage_one_fault;
                later_split_fault_stage_two = entry_stage_two_fault;
                later_split_fault_s1_af_or_s2_gaf =
                    entry.fault_effective_s1_af ||
                    entry.fault_effective_s2_gaf;
                response_provenance = '{default:'0};
                response_entry = '{default:'0};
                response_effective_pbmt = 2'd0;
                response_stage_one_fault = 1'b0;
                response_stage_two_fault = 1'b0;
                response_target_translation_fault = 1'b0;
                later_split_load_response = '{default:'0};
                later_split_l2_completion_proven = 1'b0;
                if (ro.try_read_later_split_load_response_for_rm(
                        item.uid, item.dynamic_epoch,
                        '{flag: main_view.rob_idx_flag,
                          value: main_view.rob_idx_value},
                        byte_va[63:12], later_split_load_response) &&
                    later_split_load_response.valid) begin
                    later_split_response_proven = 1'b1;
                    later_split_response_is_nc =
                        later_split_load_response.nc ||
                        later_split_load_response.mmio;
                end
                if (ro.try_read_completed_later_split_response_for_rm(
                        item.uid, item.dynamic_epoch,
                        '{flag: main_view.rob_idx_flag,
                          value: main_view.rob_idx_value},
                        byte_va[63:12], tlb_context.s2xlate,
                        response_provenance, response_entry) &&
                    response_provenance.valid && response_entry.valid &&
                    response_provenance.entry_generation ==
                        entry.entry_generation &&
                    observer_get_effective_pbmt(
                        response_entry, tlb_context, s1_active, s2_active,
                        response_effective_pbmt)) begin
                    response_stage_one_fault =
                        response_entry.fault_effective_s1_pf ||
                        ((!response_entry.fault_effective_s1_pf &&
                          !response_entry.fault_effective_s1_af) &&
                         observer_stage_permission_fault(
                             response_entry, tlb_context,
                             store_access, 1'b1, item.hlvx));
                    response_stage_two_fault =
                        response_entry.fault_effective_s2_gpf ||
                        ((!response_entry.fault_effective_s2_gpf &&
                          !response_entry.fault_effective_s2_gaf) &&
                         observer_stage_permission_fault(
                             response_entry, tlb_context,
                             store_access, 1'b0, item.hlvx));
                    response_target_translation_fault =
                        response_entry.fault_effective_s1_af ||
                        response_entry.fault_effective_s2_gaf ||
                        response_stage_one_fault ||
                        response_stage_two_fault;
                    later_split_l2_completion_proven =
                        response_target_translation_fault &&
                        response_provenance.request_key.vpn == byte_va[63:12] &&
                        response_provenance.request_key.s2xlate == tlb_context.s2xlate;
                    // L2 completion is retained as translation provenance.
                    // The architectural mabuf priority is decided only by the
                    // actual splitLoadResp NC/MMIO bits above, which also cover
                    // a resident TLB hit with no new L2 request.
                end
            end
            stage_one_fault |= entry_stage_one_fault;
            stage_two_fault |= entry_stage_two_fault;
            if(!fault_segment_selected && (entry_stage_one_fault || entry_stage_two_fault ||
                entry.fault_effective_s1_af || entry.fault_effective_s2_gaf || entry.pma_af)) begin
                fault_segment_selected=1;fault_entry=entry;fault_request_va=byte_va;
                if(byte_va[63:12]!=first_vpn) item.expected_vaddr=observer_extend_exception_va({byte_va[63:12],12'b0},tlb_context);
            end
            if (!entry.fault && !entry.pma_af &&
                !pbmt_force_s1_pf && !pbmt_force_s2_gpf &&
                tlb_context.request_translation_valid) begin
                if (entry.s2_stage_active) final_ppn = tlb_context.request_s2_resolved_ppn;
                else if (entry.s1_stage_active) final_ppn = tlb_context.request_s1_resolved_ppn;
                else begin
                    ls_model.set_error(RM_LS_ERR_TLB_ENTRY_INCONSISTENT,
                                       $sformatf("uid %0d byte %0d active path has no active TLB stage",
                                                 item.uid, byte_index));
                    return 1'b0;
                end
                item.pa_by_byte[byte_index] = {final_ppn, byte_va[11:0]};
                item.pa_valid_mask[byte_index] = 1'b1;
            end
            if (byte_index == 0 && misaligned &&
                seq_csr_common::get_pma_pmp_model_en() &&
                !stage_one_fault && !stage_two_fault &&
                !translation_access_fault && item.pa_valid_mask[0]) begin
                byte_pma_pmp_view = '{default:'0};
                if (!ro.read_pma_pmp_af_for_rm(
                        item.uid,
                        item.dynamic_epoch,
                        1'b1,
                        item.pa_by_byte[0][47:0],
                        1,
                        pma_pmp_cmd,
                        byte_pma_pmp_view)) begin
                    ls_model.set_error(
                        RM_LS_ERR_TRANSLATION_NOT_READY,
                        $sformatf("uid %0d first-byte PMA/PMP view unavailable PA=0x%0h epoch=%0d",
                                  item.uid, item.pa_by_byte[0], item.dynamic_epoch));
                    return 1'b0;
                end
                first_byte_pma_known = byte_pma_pmp_view.valid &&
                                       byte_pma_pmp_view.translation_eligible &&
                                       byte_pma_pmp_view.af_decided;
                first_byte_pma_fault = first_byte_pma_known &&
                    (store_access ? byte_pma_pmp_view.st_access_fault :
                                    (byte_pma_pmp_view.ld_access_fault || (item.hlvx && byte_pma_pmp_view.instr_access_fault)));
                first_byte_is_pma_mmio = first_byte_pma_known &&
                    effective_pbmt == 2'd0 &&
                    !byte_pma_pmp_view.normal_cacheable &&
                    !first_byte_pma_fault &&
                    !byte_pma_pmp_view.st_access_fault;
                if (first_byte_pma_fault) begin
                    pma_pmp_access_fault = 1'b1;
                end
                if (first_byte_is_pma_mmio) begin
                    misaligned_maps_to_access_fault = 1'b1;
                end
            end
            // A translation fault terminates the architectural access.  The DUT
            // does not issue later-page TLB requests after this point, so asking
            // the RM to resolve remaining bytes would create a false query miss
            // for a cross-page access (for example, a faulting LW at page+0xffd).
            entry_translation_fault = entry.pma_af ||
                                      entry.fault_effective_s1_pf ||
                                      entry.fault_effective_s1_af ||
                                      entry.fault_effective_s2_gpf ||
                                      entry.fault_effective_s2_gaf ||
                                      entry_stage_one_fault ||
                                      entry_stage_two_fault;
            if (entry_translation_fault) begin
                break;
            end
            if (byte_index == 0 &&
                (dut_stops_after_first_page || first_byte_pma_fault ||
                 first_byte_is_pma_mmio)) begin
                break;
            end
        end
    end
    required_pa_mask = 16'hffff >> (16 - item.size_bytes);
    // 中文注释：PMA/PMP 只在翻译成功且所有访问字节都有 PA 时查询。翻译
    // fault 后 DUT 的后续 PMP/PMA response 不具备架构确定性，不能从模型补写 AF。
    if (!translation_access_fault && !stage_one_fault && !stage_two_fault &&
        item.pa_valid_mask == required_pa_mask &&
        seq_csr_common::get_pma_pmp_model_en()) begin
        if (!ro.read_pma_pmp_af_for_rm(
                item.uid,
                item.dynamic_epoch,
                1'b1,
                item.pa_by_byte[0][47:0],
                item.size_bytes,
                pma_pmp_cmd,
                pma_pmp_view)) begin
            ls_model.set_error(
                RM_LS_ERR_TRANSLATION_NOT_READY,
                $sformatf("uid %0d PMA/PMP AF view unavailable at PA 0x%0h epoch=%0d",
                          item.uid, item.pa_by_byte[0], item.dynamic_epoch));
            return 1'b0;
        end
        item.expected_pmp_fault = store_access ? pma_pmp_view.pmp_st_fault :
                                                   (pma_pmp_view.pmp_ld_fault || (item.hlvx && pma_pmp_view.pmp_instr_fault));
        item.expected_pma_fault = store_access ? pma_pmp_view.pma_st_fault :
                                                   (pma_pmp_view.pma_ld_fault || (item.hlvx && pma_pmp_view.pma_instr_fault));
        item.expected_keyid_fault = pma_pmp_view.keyid_fault;
        item.expected_pma_atomic_fault = pma_pmp_view.pma_atomic_fault;
        item.expected_pma_cache_path_fault = pma_pmp_view.pma_cache_path_fault;
        pma_pmp_access_fault = store_access ? pma_pmp_view.st_access_fault :
                                               (pma_pmp_view.ld_access_fault || (item.hlvx && pma_pmp_view.instr_access_fault));
        // 只有 model 开启、完整尺寸 view 已决且当前 command 无基础 AF 时，
        // 才允许逐 byte 证明整笔 PMA MMIO。READ command 下 st_access_fault
        // 固定为 0，不代表额外要求 Load 具备写权限。
        all_bytes_pma_mmio = all_bytes_pma_pbmt &&
                             pma_pmp_view.valid &&
                             pma_pmp_view.translation_eligible &&
                             pma_pmp_view.af_decided &&
                             !(store_access ? pma_pmp_view.st_access_fault :
                                              (pma_pmp_view.ld_access_fault || (item.hlvx && pma_pmp_view.instr_access_fault))) &&
                             !pma_pmp_view.st_access_fault;
        // PMA/PMP 的完整尺寸查询继续服务既有 Access-Fault 期望；非对齐放宽
        // 还必须证明每一个实际 PA byte 都属于普通可缓存区域。单字节查询
        // 避免只看首地址而跨过 PMA/PMP 边界。
        all_bytes_normal_cacheable = 1'b1;
        for (int unsigned byte_index = 0; byte_index < item.size_bytes; byte_index++) begin
            byte_pma_pmp_view = '{default:'0};
            if (!ro.read_pma_pmp_af_for_rm(
                    item.uid,
                    item.dynamic_epoch,
                    1'b1,
                    item.pa_by_byte[byte_index][47:0],
                    1,
                    pma_pmp_cmd,
                    byte_pma_pmp_view)) begin
                ls_model.set_error(
                    RM_LS_ERR_TRANSLATION_NOT_READY,
                    $sformatf("uid %0d PMA/PMP byte view unavailable byte=%0d PA=0x%0h epoch=%0d",
                              item.uid, byte_index, item.pa_by_byte[byte_index],
                              item.dynamic_epoch));
                return 1'b0;
            end
            if (!byte_pma_pmp_view.valid ||
                !byte_pma_pmp_view.translation_eligible ||
                !byte_pma_pmp_view.af_decided ||
                !byte_pma_pmp_view.normal_cacheable ||
                (store_access ? byte_pma_pmp_view.st_access_fault :
                                (byte_pma_pmp_view.ld_access_fault || (item.hlvx && byte_pma_pmp_view.instr_access_fault)))) begin
                all_bytes_normal_cacheable = 1'b0;
            end
            if (!byte_pma_pmp_view.valid ||
                !byte_pma_pmp_view.translation_eligible ||
                !byte_pma_pmp_view.af_decided ||
                byte_pma_pmp_view.normal_cacheable ||
                (store_access ? byte_pma_pmp_view.st_access_fault :
                                (byte_pma_pmp_view.ld_access_fault || (item.hlvx && byte_pma_pmp_view.instr_access_fault))) ||
                byte_pma_pmp_view.st_access_fault) begin
                all_bytes_pma_mmio = 1'b0;
            end
        end
        if (misaligned && all_bytes_pma_pbmt && all_bytes_pma_mmio) begin
            misaligned_maps_to_access_fault = 1'b1;
        end
        // 当前 V2 smoke 的 U 态默认 PMP 会在基础权限阶段结束；若未来 profile
        // 允许访问且 PMA C=0，需要独立 cache observer 提供 fact 后再补 cache-path AF。
        // 此处不把 C=0/MMIO 分类或未知 fact 猜测成异常，保留模型诊断字段即可。
    end
    // 中文注释：TLB translation AF/PF/GPF 会使 PMA/PMP response 不可信，并
    // 抑制 address-misaligned；S2 PMA/PMP AF 则是翻译成功后的独立异常。
    // LoadUnit 允许它与 PBMT-NC misaligned 共存，StoreUnit 用 AF 抑制
    // storeAddrMisaligned。两类 AF 不能共用一个互斥门控。
    later_split_uncache_override =
        item.kind == RM_LS_KIND_LOAD &&
        misaligned &&
        !first_byte_translation_fault &&
        later_split_translation_fault &&
        later_split_response_proven &&
        later_split_response_is_nc &&
        (!later_split_load_response.l2_request_observed ||
         (later_split_l2_completion_proven &&
          response_provenance.response_token ==
              later_split_load_response.response_token &&
          response_provenance.response_sample_seq >
              later_split_load_response.l2_request_sample_seq));
    if (later_split_uncache_override) begin
        item.expected_exception = '0;
        item.expected_exception[4] = 1'b1;
        `uvm_info("RM_LS_LATER_SPLIT_UNCACHE",
                  $sformatf("uid=%0d va=0x%0h later_s1=%0d later_s2=%0d later_af=%0d token=%0d response_sample=%0d generation=%0d pbmt=%0d expected_exception=0x%0h",
                            item.uid, item.computed_vaddr,
                            later_split_fault_stage_one,
                            later_split_fault_stage_two,
                            later_split_fault_s1_af_or_s2_gaf,
                            response_provenance.response_token,
                            later_split_load_response.sample_seq,
                            response_provenance.entry_generation,
                            response_effective_pbmt,
                            item.expected_exception),
                  UVM_LOW)
    end else begin
        if (translation_access_fault)
            item.expected_exception[exception_store_access ? 7 : 5] = 1'b1;
        if (stage_one_fault) item.expected_exception[exception_store_access ? 15 : 13] = 1'b1;
        if (stage_two_fault) item.expected_exception[exception_store_access ? 23 : 21] = 1'b1;
        if (!translation_access_fault && !stage_one_fault && !stage_two_fault) begin
            if (pma_pmp_access_fault || misaligned_maps_to_access_fault) begin
                item.expected_exception[exception_store_access ? 7 : 5] = 1'b1;
            end
            if (!misaligned_maps_to_access_fault &&
                (!pma_pmp_access_fault ||
                 (!store_access && first_effective_pbmt_valid &&
                  first_effective_pbmt == 2'd1)) &&
                observer_should_expect_addr_misaligned(
                    misaligned, store_access, tlb_context,
                    all_bytes_pma_pbmt)) begin
                item.expected_exception[exception_store_access ? 6 : 4] = 1'b1;
            end
        end
    end
    if (item.kind == RM_LS_KIND_ATOMIC) begin
        bit [23:0] atomic_exception;
        atomic_exception = '0;
        if (translation_access_fault)
            atomic_exception[store_access ? 7 : 5] = 1'b1;
        if (stage_one_fault)
            atomic_exception[store_access ? 15 : 13] = 1'b1;
        if (stage_two_fault)
            atomic_exception[store_access ? 23 : 21] = 1'b1;
        if (!translation_access_fault && !stage_one_fault && !stage_two_fault) begin
            if (pma_pmp_access_fault || item.expected_pma_atomic_fault ||
                !all_bytes_pma_pbmt || all_bytes_pma_mmio ||
                first_byte_is_pma_mmio || misaligned_maps_to_access_fault) begin
                atomic_exception[store_access ? 7 : 5] = 1'b1;
            end else if (misaligned) begin
                atomic_exception[store_access ? 6 : 4] = 1'b1;
            end else if (seq_csr_common::get_pma_pmp_model_en() &&
                         !all_bytes_normal_cacheable) begin
                atomic_exception[store_access ? 7 : 5] = 1'b1;
            end
        end
        item.expected_exception = atomic_exception;
    end
    if (item.expected_exception == '0 &&
        item.pa_valid_mask != required_pa_mask) begin
        ls_model.set_error(RM_LS_ERR_TRANSLATION_NOT_READY,
                           $sformatf("uid %0d normal access has incomplete PA geometry mask=0x%0h",
                                     item.uid, item.pa_valid_mask));
        return 1'b0;
    end
    if(stage_two_fault && !translation_access_fault && !stage_one_fault)
        observer_compute_guest_fault_address(item,tlb_context,fault_entry,fault_request_va);
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
    item.translation_pbmt=first_effective_pbmt;
    observer_check_pmm_arch_reference(item);
    if (!observer_trace_translation_emitted.exists(item.uid)) begin
        `uvm_info("RM_LS_TRACE_TRANSLATION",
                  $sformatf("node=TRANSLATION uid=%0d rob=%0d/%0d kind=%s path=%s va=0x%0h first_pa=0x%0h last_pa=0x%0h pa_mask=0x%0h s2xlate=%0d mPBMTE=%0d hPBMTE=%0d hdMisalign=%0d allPbmtPma=%0d allPmaMmio=%0d allNormalCacheable=%0d pbmt_force_s1_pf=%0d pbmt_force_s2_gpf=%0d expected_exception=0x%0h",
                            item.uid, item.rob.flag, item.rob.value,
                            observer_kind_name(item.kind),
                            bare_identity ? "BARE_IDENTITY" : "PAGED_ENTRY",
                            item.computed_vaddr, item.pa_by_byte[0],
                            item.pa_by_byte[item.size_bytes - 1], item.pa_valid_mask,
                            tlb_context.s2xlate, tlb_context.m_pbmt_en,
                            tlb_context.h_pbmt_en,
                            store_access ? tlb_context.hd_misalign_st_enable :
                                           tlb_context.hd_misalign_ld_enable,
                            all_bytes_pma_pbmt,
                            all_bytes_pma_mmio,
                            all_bytes_normal_cacheable,
                            item.expected_pbmt_forced_s1_pf,
                            item.expected_pbmt_forced_s2_gpf,
                            item.expected_exception),
                  UVM_LOW)
        observer_trace_translation_emitted[item.uid] = 1'b1;
    end
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
    return 1'b1;
endfunction:observer_build_commit_item_with_observation

function bit memblock_rm::observer_apply_load_d_error_expectation(
    rm_ls_program_item_t item,
    longint unsigned access_sample
);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::l2_d_error_view_t error_view;
    memblock_rm_readonly_api::uncache_d_error_view_t uncache_error_view;
    memblock_rm_readonly_api::mem_addr_t line_addr;
    bit first_byte_on_line;

    if (item == null || item.kind != RM_LS_KIND_LOAD) begin
        return 1'b1;
    end
    item.access_sample = access_sample;
    if (access_sample == 0) begin
        ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,
                           $sformatf("uid %0d DUT Load writeback has no global sample provenance",
                                     item.uid));
        return 1'b0;
    end
    // A translation/PMA exception prevents a normal cache-line data access.
    // Do not attach a sticky error from an unrelated resident line.
    if (item.expected_exception != '0) begin
        return 1'b1;
    end
    ro = memblock_rm_readonly_api::get();
    uncache_error_view = '{default:'0};
    if (item.uncache_path) begin
        if (item.all_normal_cacheable) begin
            ls_model.set_error(
                RM_LS_ERR_D_ERROR_NOT_READY,
                $sformatf("uid %0d has contradictory Uncache/cacheable classification",
                          item.uid));
            return 1'b0;
        end
        if (item.pa_valid_mask == '0 ||
            !ro.read_uncache_d_error_for_rm(
                item.uid, item.dynamic_epoch,
                item.rob.flag, item.rob.value,
                item.lq_flag, item.lq_value,
                memblock_rm_readonly_api::mem_addr_t'(item.pa_by_byte[0][47:0]),
                item.size_bytes, access_sample, uncache_error_view) ||
            !uncache_error_view.valid || !uncache_error_view.found) begin
            ls_model.set_error(
                RM_LS_ERR_D_ERROR_NOT_READY,
                $sformatf("uid %0d Uncache D-error ledger unavailable epoch=%0d rob=%0d/%0d lq=%0d/%0d pa=0x%0h sample=%0d not_ready=%0d identity_mismatch=%0d address_mismatch=%0d",
                          item.uid, item.dynamic_epoch,
                          item.rob.flag, item.rob.value,
                          item.lq_flag, item.lq_value,
                          item.pa_by_byte[0], access_sample,
                          uncache_error_view.not_ready,
                          uncache_error_view.identity_mismatch,
                          uncache_error_view.address_mismatch));
            return 1'b0;
        end
        item.expected_uncache_denied = uncache_error_view.denied;
        item.expected_uncache_corrupt = uncache_error_view.corrupt &&
                                          !uncache_error_view.denied;
        if (item.expected_uncache_denied) item.expected_exception[5] = 1'b1;
        if (item.expected_uncache_corrupt) item.expected_exception[19] = 1'b1;
        `uvm_info("RM_LS_D_ERROR",
                  $sformatf("uid=%0d path=UNCACHE epoch=%0d rob=%0d/%0d lq=%0d/%0d pa=0x%0h sample=%0d source=%0d generation=%0d A/D=%0d/%0d denied=%0d corrupt=%0d expected_exception=0x%0h",
                            item.uid, item.dynamic_epoch,
                            item.rob.flag, item.rob.value,
                            item.lq_flag, item.lq_value,
                            item.pa_by_byte[0], access_sample,
                            uncache_error_view.source,
                            uncache_error_view.generation,
                            uncache_error_view.a_sample,
                            uncache_error_view.d_sample,
                            uncache_error_view.denied,
                            uncache_error_view.corrupt,
                            item.expected_exception),
                  UVM_LOW)
        return 1'b1;
    end
    if (!item.all_normal_cacheable) begin
        ls_model.set_error(
            RM_LS_ERR_D_ERROR_NOT_READY,
            $sformatf("uid %0d Load path classification incomplete allPmaMmio=%0d normalCacheable=%0d uncache=%0d",
                      item.uid, item.all_pma_mmio,
                      item.all_normal_cacheable, item.uncache_path));
        return 1'b0;
    end
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
endfunction:observer_apply_load_d_error_expectation

// 抽象职责：软件预取只比较真实LDA身份与响应错误抑制，不构造返回数据或后台请求。
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

function bit memblock_rm::observer_capture_commit_actual(
    memblock_rm_readonly_api::main_transaction_view_t main_view,
    rm_ls_program_item_t item,
    output bit missing_dut_load_writeback
);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::dut_writeback_view_t load_view;
    rm_ls_load_actual_t load_actual;
    memblock_rob_key_t load_rob_key;
    bit found_dut_load_writeback;

    missing_dut_load_writeback = 1'b0;
    ro = memblock_rm_readonly_api::get();
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
    if (item.kind == RM_LS_KIND_LOAD) begin
        load_rob_key.flag = main_view.rob_idx_flag;
        load_rob_key.value = main_view.rob_idx_value;
        found_dut_load_writeback =
            ro.read_dut_load_instance_for_rm(item.uid,load_rob_key,item.dynamic_epoch,load_view);

        // 中文注释：status 只描述框架终态，不能替代同 ROB 的 DUT LDA 写回证据。
        // 调用者记录 UVM_ERROR 后消费该 uid，继续比较后续独立 uid。
        if (!found_dut_load_writeback) begin
            missing_dut_load_writeback = 1'b1;
            return 1'b1;
        end

        if (!load_view.valid ||
            !load_view.rob_valid || !load_view.rob_flag_valid ||
            // 中文注释：异常 load 的 writeback 合法地不携带返回数据；只有无异常
            // writeback 才必须具备 data_valid，异常向量仍必须由监视器完整采集。
            (!load_view.data_valid && load_view.exception_vec === '0) ||
            !load_view.exception_valid ||
            load_view.source_kind != 2'd1 ||
            load_view.rob_flag !== load_rob_key.flag ||
            load_view.rob_value !== load_rob_key.value ||
            load_view.replay_inst || load_view.flush_pipe) begin
            ls_model.set_error(RM_LS_ERR_LOAD_ACTUAL_NOT_READY,
                               $sformatf("uid %0d committed ROB %0d/%0d DUT Load writeback is unavailable",
                                         item.uid, load_rob_key.flag, load_rob_key.value));
            return 1'b0;
        end
        load_actual = new(); load_actual.valid = 1'b1; load_actual.uid = item.uid;
        load_actual.rob.flag = load_view.rob_flag;
        load_actual.rob.value = load_view.rob_value;
        load_actual.data_valid = (load_view.exception_vec === '0);
        load_actual.data = load_view.data;
        load_actual.exception_vec = load_view.exception_vec;
        load_actual.cycle = load_view.sample_cycle;
        if (!observer_apply_load_d_error_expectation(item,
                                                     load_view.sample_seq)) begin
            return 1'b0;
        end
        if (!observer_trace_dut_load_emitted.exists(item.uid)) begin
            `uvm_info("RM_LS_TRACE_DUT_LOAD",
                      $sformatf("node=DUT_LOAD_WB uid=%0d rob=%0d/%0d source=LDA lane=%0d sample_seq=%0d sample_cycle=%0d DUT(raw_data_valid=%0d comparable_data_valid=%0d data=0x%016h exception=0x%06h replay=%0d flush=%0d)",
                                item.uid, load_actual.rob.flag, load_actual.rob.value,
                                load_view.lane, load_view.sample_seq,
                                load_view.sample_cycle,
                                load_view.data_valid, load_actual.data_valid,
                                load_actual.data, load_actual.exception_vec,
                                load_view.replay_inst, load_view.flush_pipe),
                      UVM_LOW)
            observer_trace_dut_load_emitted[item.uid] = 1'b1;
        end
        return ls_model.push_load_actual(load_actual);
    end
    if (item.kind == RM_LS_KIND_ATOMIC) begin
        memblock_rm_readonly_api::dut_writeback_view_q_t atomic_views;
        rm_atomic_actual_t atomic_actual;
        longint unsigned minimum_sample;

        load_rob_key.flag = main_view.rob_idx_flag;
        load_rob_key.value = main_view.rob_idx_value;
        minimum_sample = item.atomic_first_issue_sample;
        if (minimum_sample == 0 ||
            !ro.read_dut_atomic_writebacks_for_rm(
                item.uid, load_rob_key, item.dynamic_epoch,
                item.expected_atomic_wb_count, minimum_sample, atomic_views)) begin
            missing_dut_load_writeback = 1'b1;
            return 1'b1;
        end
        if (atomic_views.size() != item.expected_atomic_wb_count) begin
            ls_model.set_error(
                RM_LS_ERR_ATOMIC_WB_COUNT_MISMATCH,
                $sformatf("uid %0d expected exactly %0d atomic LDA0 records, found %0d",
                          item.uid, item.expected_atomic_wb_count,
                          atomic_views.size()));
            return 1'b0;
        end
        atomic_actual = new();
        atomic_actual.valid = 1'b1;
        atomic_actual.uid = item.uid;
        atomic_actual.rob = item.rob;
        atomic_actual.dynamic_epoch = item.dynamic_epoch;
        atomic_actual.wb_count = atomic_views.size();
        foreach (atomic_views[wb_index]) begin
            atomic_actual.data_valid[wb_index] = atomic_views[wb_index].data_valid;
            atomic_actual.data[wb_index] = atomic_views[wb_index].data;
            atomic_actual.exception_vec[wb_index] =
                atomic_views[wb_index].exception_vec;
            atomic_actual.pdest_valid[wb_index] =
                atomic_views[wb_index].pdest_valid;
            atomic_actual.pdest[wb_index] = atomic_views[wb_index].pdest;
            atomic_actual.rf_wen_valid[wb_index] =
                atomic_views[wb_index].rf_wen_valid;
            atomic_actual.rf_wen[wb_index] = atomic_views[wb_index].rf_wen;
            atomic_actual.sample_seq[wb_index] = atomic_views[wb_index].sample_seq;
            atomic_actual.sample_cycle[wb_index] = atomic_views[wb_index].sample_cycle;
        end
        if (!observer_apply_atomic_d_error_expectation(
                item, atomic_actual.sample_seq[atomic_actual.wb_count - 1])) begin
            return 1'b0;
        end
        return ls_model.push_atomic_actual(atomic_actual);
    end
    return 1'b1;
endfunction:observer_capture_commit_actual

function bit memblock_rm::observer_capture_load_backing(rm_ls_program_item_t item);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::mem_line_mask_t byte_mask;
    memblock_rm_readonly_api::memory_read_view_t memory_view;

    if (item.kind != RM_LS_KIND_LOAD || item.expected_exception != '0) return 1'b1;
    ro = memblock_rm_readonly_api::get();
    for (int unsigned byte_index = 0; byte_index < item.size_bytes; byte_index++) begin
        if (ls_model.expected_store_byte_exists(item.pa_by_byte[byte_index])) continue;
        byte_mask = '0; byte_mask[0] = 1'b1;
        if (!ro.read_initialized_backing_for_rm(item.pa_by_byte[byte_index],
                                                byte_mask, memory_view) ||
            !memory_view.valid || !memory_view.data_valid || memory_view.corrupt ||
            !memory_view.byte_valid[0]) begin
            ls_model.set_error(RM_LS_ERR_ORIGINAL_L2_NOT_READY,
                               $sformatf("uid %0d backing PA 0x%0h unavailable",
                                         item.uid, item.pa_by_byte[byte_index]));
            return 1'b0;
        end
        void'(ls_model.set_backing_byte(item.pa_by_byte[byte_index],
                                        memory_view.data[7:0]));
    end
    return 1'b1;
endfunction:observer_capture_load_backing

function bit memblock_rm::observer_capture_atomic_backing(rm_ls_program_item_t item);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::mem_line_mask_t byte_mask;
    memblock_rm_readonly_api::memory_read_view_t memory_view;

    if (item.kind != RM_LS_KIND_ATOMIC || item.expected_exception != '0 ||
        item.atomic_kind == RM_ATOMIC_SC)
        return 1'b1;
    ro = memblock_rm_readonly_api::get();
    for (int unsigned byte_index = 0; byte_index < item.size_bytes; byte_index++) begin
        if (ls_model.expected_store_byte_exists(item.pa_by_byte[byte_index])) continue;
        byte_mask = '0; byte_mask[0] = 1'b1;
        if (!ro.read_initialized_backing_for_rm(item.pa_by_byte[byte_index],
                                                byte_mask, memory_view) ||
            !memory_view.valid || !memory_view.data_valid || memory_view.corrupt ||
            !memory_view.byte_valid[0]) begin
            ls_model.set_error(RM_LS_ERR_ORIGINAL_L2_NOT_READY,
                               $sformatf("uid %0d atomic backing PA 0x%0h unavailable",
                                         item.uid, item.pa_by_byte[byte_index]));
            return 1'b0;
        end
        void'(ls_model.set_backing_byte(item.pa_by_byte[byte_index],
                                        memory_view.data[7:0]));
    end
    return 1'b1;
endfunction:observer_capture_atomic_backing

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
endfunction:observer_apply_atomic_d_error_expectation

function bit memblock_rm::observer_atomic_wb_wait_pending(
    ref int unsigned wait_count,
    input bit missing_writeback,
    input rm_ls_kind_e item_kind
);
    if (missing_writeback && item_kind == RM_LS_KIND_ATOMIC &&
        wait_count < 256) begin
        wait_count++;
        return 1'b1;
    end
    wait_count = 0;
    return 1'b0;
endfunction:observer_atomic_wb_wait_pending

function bit memblock_rm::observer_atomic_std_sample_matches(
    input memblock_rm_readonly_api::main_transaction_view_t main_view,
    input bit [3:0] required_std_mask,
    input int unsigned pipe,
    input bit [63:0] std_data,
    input bit [8:0] std_fuop,
    input bit [MEMBLOCK_ROB_VALUE_W-1:0] std_rob_value,
    output int unsigned slot
);
    slot = std_fuop[8:6];
    return slot < 4 && pipe == 0 &&
           std_fuop[5:0] == main_view.fu_op_type[5:0] &&
           std_rob_value == main_view.rob_idx_value &&
           required_std_mask[slot] &&
           main_view.atomic_std_valid_mask[slot] &&
           std_data === main_view.atomic_std_data_flat[(slot * 64) +: 64];
endfunction:observer_atomic_std_sample_matches

function void memblock_rm::observer_check_atomic_issue_monitor(
    lintsissue_agent_agent_xaction monitor_tr
);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::main_transaction_view_t main_view;
    memblock_rm_readonly_api::status_view_t status_view;
    memblock_uid_t owner_uid;

    if (monitor_tr == null || !monitor_tr.memblock_monitor_atomic_owner_valid) return;
    ro = memblock_rm_readonly_api::get();
    owner_uid = monitor_tr.memblock_monitor_atomic_owner_uid;
    if (!ro.read_main_transaction_for_rm(owner_uid, main_view) ||
        !ro.read_status_for_rm(owner_uid, status_view) ||
        !main_view.valid || !status_view.valid) begin
        `uvm_error("RM_ATOMIC_ISSUE_OWNER",
                   $sformatf("sample owner uid=%0d is unavailable", owner_uid))
        return;
    end
    if (status_view.dynamic_epoch !=
        monitor_tr.memblock_monitor_atomic_owner_dynamic_epoch) begin
        if (status_view.dynamic_epoch >
            monitor_tr.memblock_monitor_atomic_owner_dynamic_epoch) return;
        `uvm_error("RM_ATOMIC_ISSUE_OWNER",
                   $sformatf("uid=%0d sample dynamic epoch=%0d current=%0d",
                             owner_uid,
                             monitor_tr.memblock_monitor_atomic_owner_dynamic_epoch,
                             status_view.dynamic_epoch))
        return;
    end
    if (observer_atomic_issue_dynamic_epoch.exists(owner_uid) &&
        observer_atomic_issue_dynamic_epoch[owner_uid] != status_view.dynamic_epoch) begin
        observer_atomic_std_checked_mask[owner_uid] = '0;
        observer_atomic_sta_pdest_valid[owner_uid] = '0;
        observer_atomic_sta_pdest_flat[owner_uid] = '0;
        observer_atomic_first_issue_sample[owner_uid] = 0;
    end
    observer_atomic_issue_dynamic_epoch[owner_uid] = status_view.dynamic_epoch;
    for (int unsigned pipe = 0; pipe < 2; pipe++) begin
        bit std_valid;
        bit std_ready;
        bit [63:0] std_data;
        bit [8:0] std_fuop;
        bit [MEMBLOCK_ROB_VALUE_W-1:0] std_rob_value;
        int unsigned slot;

        if (pipe == 0) begin
            std_valid = monitor_tr.io_ooo_to_mem_issueStd_0_valid;
            std_ready = monitor_tr.io_ooo_to_mem_issueStd_0_ready;
            std_data = monitor_tr.io_ooo_to_mem_issueStd_0_bits_src_0;
            std_fuop = monitor_tr.io_ooo_to_mem_issueStd_0_bits_uop_fuOpType;
            std_rob_value = monitor_tr.io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value;
        end else begin
            std_valid = monitor_tr.io_ooo_to_mem_issueStd_1_valid;
            std_ready = monitor_tr.io_ooo_to_mem_issueStd_1_ready;
            std_data = monitor_tr.io_ooo_to_mem_issueStd_1_bits_src_0;
            std_fuop = monitor_tr.io_ooo_to_mem_issueStd_1_bits_uop_fuOpType;
            std_rob_value = monitor_tr.io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value;
        end
        if (!(std_valid && std_ready)) continue;
        if (monitor_tr.memblock_monitor_flush_epoch <
            monitor_tr.memblock_monitor_atomic_owner_activation_flush_epoch) continue;
        if (monitor_tr.memblock_monitor_flush_epoch !=
            monitor_tr.memblock_monitor_atomic_owner_activation_flush_epoch) begin
            `uvm_error("RM_ATOMIC_STD_INPUT",
                       $sformatf("uid=%0d epoch=%0d STD sample flush epoch=%0d active=%0d",
                                 owner_uid, status_view.dynamic_epoch,
                                 monitor_tr.memblock_monitor_flush_epoch,
                                 monitor_tr.memblock_monitor_atomic_owner_activation_flush_epoch))
            continue;
        end
        if (!observer_atomic_std_sample_matches(
                main_view,
                monitor_tr.memblock_monitor_atomic_owner_required_std_mask,
                pipe, std_data, std_fuop, std_rob_value, slot)) begin
            `uvm_error("RM_ATOMIC_STD_INPUT",
                       $sformatf("uid=%0d epoch=%0d pipe=%0d slot=%0d actual(op=0x%0h rob=%0d data=0x%0h) expected(op=0x%0h rob=%0d data=0x%0h required=0x%0h)",
                                 owner_uid, status_view.dynamic_epoch, pipe, slot,
                                 std_fuop, std_rob_value, std_data,
                                 main_view.fu_op_type, main_view.rob_idx_value,
                                 slot < 4 ? main_view.atomic_std_data_flat[(slot * 64) +: 64] : '0,
                                 monitor_tr.memblock_monitor_atomic_owner_required_std_mask))
        end else begin
            observer_atomic_std_checked_mask[owner_uid][slot] = 1'b1;
            if (!observer_atomic_first_issue_sample.exists(owner_uid) ||
                observer_atomic_first_issue_sample[owner_uid] == 0)
                observer_atomic_first_issue_sample[owner_uid] =
                    monitor_tr.memblock_monitor_sample_seq;
        end
    end
    for (int unsigned pipe = 0; pipe < 2; pipe++) begin
        bit sta_valid;
        bit sta_ready;
        bit [8:0] sta_fuop;
        bit sta_rob_flag;
        bit [MEMBLOCK_ROB_VALUE_W-1:0] sta_rob_value;
        bit [7:0] sta_pdest;
        int unsigned slot;

        if (pipe == 0) begin
            sta_valid = monitor_tr.io_ooo_to_mem_issueSta_0_valid;
            sta_ready = monitor_tr.io_ooo_to_mem_issueSta_0_ready;
            sta_fuop = monitor_tr.io_ooo_to_mem_issueSta_0_bits_uop_fuOpType;
            sta_rob_flag = monitor_tr.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag;
            sta_rob_value = monitor_tr.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value;
            sta_pdest = monitor_tr.io_ooo_to_mem_issueSta_0_bits_uop_pdest;
        end else begin
            sta_valid = monitor_tr.io_ooo_to_mem_issueSta_1_valid;
            sta_ready = monitor_tr.io_ooo_to_mem_issueSta_1_ready;
            sta_fuop = monitor_tr.io_ooo_to_mem_issueSta_1_bits_uop_fuOpType;
            sta_rob_flag = monitor_tr.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag;
            sta_rob_value = monitor_tr.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value;
            sta_pdest = monitor_tr.io_ooo_to_mem_issueSta_1_bits_uop_pdest;
        end
        if (!(sta_valid && sta_ready)) continue;
        if (monitor_tr.memblock_monitor_flush_epoch <
            monitor_tr.memblock_monitor_atomic_owner_activation_flush_epoch) continue;
        if (monitor_tr.memblock_monitor_flush_epoch !=
            monitor_tr.memblock_monitor_atomic_owner_activation_flush_epoch) begin
            `uvm_error("RM_ATOMIC_STA_INPUT",
                       $sformatf("uid=%0d epoch=%0d STA sample flush epoch=%0d active=%0d",
                                 owner_uid, status_view.dynamic_epoch,
                                 monitor_tr.memblock_monitor_flush_epoch,
                                 monitor_tr.memblock_monitor_atomic_owner_activation_flush_epoch))
            continue;
        end
        slot = sta_fuop[8:6];
        if (slot >= 4 || pipe != 0 || sta_fuop[5:0] != main_view.fu_op_type[5:0] ||
            sta_rob_flag != main_view.rob_idx_flag ||
            sta_rob_value != main_view.rob_idx_value ||
            !monitor_tr.memblock_monitor_atomic_owner_required_sta_mask[slot]) begin
            `uvm_error("RM_ATOMIC_STA_INPUT",
                       $sformatf("uid=%0d epoch=%0d pipe=%0d slot=%0d STA identity mismatch",
                                 owner_uid, status_view.dynamic_epoch, pipe, slot))
        end else begin
            if (slot == 0) begin
                observer_atomic_sta_pdest_valid[owner_uid][0] = 1'b1;
                observer_atomic_sta_pdest_flat[owner_uid][7:0] = sta_pdest;
            end else if (slot == 2) begin
                observer_atomic_sta_pdest_valid[owner_uid][1] = 1'b1;
                observer_atomic_sta_pdest_flat[owner_uid][15:8] = sta_pdest;
            end
            if (!observer_atomic_first_issue_sample.exists(owner_uid) ||
                observer_atomic_first_issue_sample[owner_uid] == 0)
                observer_atomic_first_issue_sample[owner_uid] =
                    monitor_tr.memblock_monitor_sample_seq;
        end
    end
endfunction:observer_check_atomic_issue_monitor

function string memblock_rm::observer_kind_name(rm_ls_kind_e kind);
    case (kind)
        RM_LS_KIND_LOAD:  return "LOAD";
        RM_LS_KIND_STORE: return "STORE";
        RM_LS_KIND_ATOMIC: return "ATOMIC";
        RM_LS_KIND_CMO: return "CMO";
        default:          return "CONTROL_OR_OTHER";
    endcase
endfunction:observer_kind_name

function string memblock_rm::observer_store_size_name(int unsigned size_bytes);
    case (size_bytes)
        1: return "BYTE";
        2: return "HALF";
        4: return "WORD";
        8: return "DOUBLE";
        16: return "QUAD";
        default: return "INVALID";
    endcase
endfunction:observer_store_size_name

function void memblock_rm::observer_trace_commit_result(
    input rm_ls_program_item_t item,
    input memblock_rm_readonly_api::status_view_t status_view,
    input bit compare_result
);
    rm_ls_history_record_t rec;
    int unsigned history_index;

    if (item.kind == RM_LS_KIND_LOAD && ls_model.history.size() != 0) begin
        history_index = ls_model.history.size() - 1;
        rec = ls_model.history[history_index];
        if (rec.uid == item.uid) begin
            `uvm_info("RM_LS_TRACE_COMPARE",
                      $sformatf("node=COMPARE uid=%0d rob=%0d/%0d kind=LOAD va=0x%0h first_pa=0x%0h EXPECTED(data=0x%016h exception=0x%06h) DUT(data=0x%016h exception=0x%06h status_exception=0x%06h) result=%s detail=%s",
                                item.uid, item.rob.flag, item.rob.value,
                                rec.vaddr, rec.first_pa, rec.expected_data,
                                rec.expected_exception, rec.actual_data,
                                rec.actual_exception, status_view.exception_vec,
                                compare_result ? "PASS" : "FAIL", rec.detail),
                      UVM_LOW)
            return;
        end
    end
    if (item.kind == RM_LS_KIND_ATOMIC && ls_model.history.size() != 0) begin
        history_index = ls_model.history.size() - 1;
        rec = ls_model.history[history_index];
        if (rec.uid == item.uid) begin
            `uvm_info("RM_LS_TRACE_COMPARE",
                      $sformatf("node=COMPARE uid=%0d rob=%0d/%0d kind=ATOMIC atomic_kind=%0d size=%0d va=0x%0h first_pa=0x%0h wb_count=%0d EXPECTED(data_lo=0x%016h exception=0x%06h) DUT(data_lo=0x%016h exception=0x%06h status_exception=0x%06h) result=%s detail=%s",
                                item.uid, item.rob.flag, item.rob.value,
                                item.atomic_kind, item.size_bytes,
                                rec.vaddr, rec.first_pa,
                                item.expected_atomic_wb_count,
                                rec.expected_data, rec.expected_exception,
                                rec.actual_data, rec.actual_exception,
                                status_view.exception_vec,
                                compare_result ? "PASS" : "FAIL", rec.detail),
                      UVM_LOW)
            return;
        end
    end
    if (item.kind == RM_LS_KIND_STORE) begin
        `uvm_info("RM_LS_TRACE_COMPARE",
                  $sformatf("node=COMMIT uid=%0d rob=%0d/%0d kind=STORE va=0x%0h first_pa=0x%0h size=%0d EXPECTED(data=0x%016h mask=0x%02h exception=0x%06h) DUT(status_exception=0x%06h store_data=DEFERRED_TO_CHECK_STORE) result=%s",
                            item.uid, item.rob.flag, item.rob.value,
                            item.computed_vaddr, item.pa_by_byte[0], item.size_bytes,
                            item.store_data, item.store_byte_mask,
                            item.expected_exception, status_view.exception_vec,
                            compare_result ? "PASS" : "FAIL"),
                  UVM_LOW)
        if (compare_result && item.expected_exception == '0)
            `uvm_info("RM_LS_TRACE_STORE_ENTRY",
                      $sformatf("node=STORE_CACHE uid=%0d rob=%0d/%0d size=%s(%0dB) first_pa=0x%0h last_pa=0x%0h mask=0x%0h EXPECTED(data=0x%0h exception=0x%0h) source=MAIN_TABLE",
                                item.uid, item.rob.flag, item.rob.value,
                                observer_store_size_name(item.size_bytes), item.size_bytes,
                                item.pa_by_byte[0], item.pa_by_byte[item.size_bytes - 1],
                                item.store_byte_mask, item.store_data,
                                item.expected_exception),
                      UVM_LOW)
        return;
    end
    `uvm_info("RM_LS_TRACE_COMPARE",
              $sformatf("node=COMMIT uid=%0d rob=%0d/%0d kind=CONTROL_OR_OTHER EXPECTED(exception=0x%06h) DUT(status_exception=0x%06h) result=%s",
                        item.uid, item.rob.flag, item.rob.value,
                        item.expected_exception, status_view.exception_vec,
                        compare_result ? "PASS" : "FAIL"),
              UVM_LOW)
endfunction:observer_trace_commit_result

function bit memblock_rm::observer_process_current_commit();
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::framework_context_view_t framework_view;
    memblock_rm_readonly_api::status_view_t status_view;
    memblock_rm_readonly_api::main_transaction_view_t main_view;
    rm_ls_program_item_t item;
    rm_ls_rob_key_t commit_rob;
    int unsigned uid_before;
    bit compare_result;
    bit check_complete;
    bit check_result;
    bit missing_dut_load_writeback;
    bit deferred;
    bit exception_precheck_ready;
    bit exception_match;
    bit any_exception;

    ro = memblock_rm_readonly_api::get();
    observer_service_exception_addresses();
    if (!ro.framework_ready_for_rm()) return 1'b0;
    if (!ro.read_framework_context_for_rm(framework_view) || !framework_view.valid ||
        !framework_view.main_table_ready || framework_view.main_trans_num == 0) return 1'b0;
    if (!ls_model.configured) begin
        if (!ls_model.configure(framework_view.main_trans_num)) return 1'b0;
        observer_main_count = framework_view.main_trans_num;
        observer_check_store_pending = 1'b0;
        observer_check_store_uid = '0;
        observer_check_store_rob = '{default:'0};
        observer_check_store_wait_logged = 1'b0;
        observer_mismatch_reported = 1'b0;
        observer_first_split_wait_count = 0;
        observer_atomic_wb_wait_count = 0;
        observer_trace_main_emitted.delete();
        observer_trace_translation_emitted.delete();
        observer_trace_dut_load_emitted.delete();
        `uvm_info("RM_LS_TRACE_INIT",
                  $sformatf("node=INIT main_trans_num=%0d current_uid=%0d",
                            observer_main_count, ls_model.current_uid),
                  UVM_LOW)
    end else if (observer_main_count != framework_view.main_trans_num) begin
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMMIT", "main transaction count changed after RM initialization")
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end
    if (observer_check_store_pending) begin
        check_result = observer_check_final_store(observer_check_store_uid,
                                                  observer_check_store_rob,
                                                  check_complete);
        if (check_complete) begin
            observer_check_store_pending = 1'b0;
        end
        return check_complete && check_result;
    end
    if (ls_model.current_uid >= observer_main_count) return 1'b1;
    if (observer_mismatch_reported) return 1'b0;
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
    if (main_view.uid != ls_model.current_uid || status_view.uid != ls_model.current_uid) begin
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMMIT", "main/status UID does not match RM program cursor")
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end
    if (main_view.rob_idx_flag != status_view.rob_idx_flag ||
        main_view.rob_idx_value != status_view.rob_idx_value) begin
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMMIT",
                       $sformatf("uid %0d main ROB %0d/%0d status ROB %0d/%0d",
                                 main_view.uid, main_view.rob_idx_flag,
                                 main_view.rob_idx_value, status_view.rob_idx_flag,
                                 status_view.rob_idx_value))
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end
    if (!observer_trace_main_emitted.exists(main_view.uid)) begin
        `uvm_info("RM_LS_TRACE_MAIN",
                  $sformatf("node=MAIN_TABLE uid=%0d rob=%0d/%0d op_class=%0d fp_wen=%0d check_store=%0d fu_type=0x%0h fu_op=0x%0h src_0=0x%016h imm=0x%016h main_va=0x%016h status(rob_commit=%0d terminal=%0d exception=0x%06h)",
                            main_view.uid, main_view.rob_idx_flag,
                            main_view.rob_idx_value, main_view.op_class,
                            main_view.op_class == MEMBLOCK_OP_CLASS_FP_LOAD,
                            main_view.check_store, main_view.fu_type,
                            main_view.fu_op_type, main_view.src_0, main_view.imm,
                            main_view.vaddr, status_view.rob_commit,
                            status_view.terminal_done, status_view.exception_vec),
                  UVM_LOW)
        observer_trace_main_emitted[main_view.uid] = 1'b1;
    end
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
    if (!observer_build_commit_item_with_observation(main_view, status_view, item, deferred)) begin
        if (deferred) begin
            observer_first_split_wait_count++;
            if (observer_first_split_wait_count < 256) return 1'b0;
            if (!observer_mismatch_reported) begin
                `uvm_error("RM_LS_FIRST_SPLIT_NOT_SETTLED",
                           $sformatf("uid=%0d split observation did not settle after 256 polls", main_view.uid))
                observer_mismatch_reported = 1'b1;
            end
            return 1'b0;
        end
        observer_first_split_wait_count = 0;
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end
    observer_first_split_wait_count = 0;
    if (!ls_model.add_item(item)) begin
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end

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
    if (observer_atomic_wb_wait_pending(observer_atomic_wb_wait_count,
                                        missing_dut_load_writeback,
                                        item.kind)) begin
        return 1'b0;
    end
    if (missing_dut_load_writeback) begin
        // 中文注释：该 Load 没有写内存效果，且本 uid 已不再进行数据比较；不能因
        // backing 尚未可读而阻断后续 uid。后续正常 Load 会按自身地址懒采集 backing。
        commit_rob.flag = status_view.rob_idx_flag;
        commit_rob.value = status_view.rob_idx_value;
        if (!ls_model.consume_current_commit_error(
                commit_rob,
                item.kind == RM_LS_KIND_ATOMIC ? RM_LS_ERR_ATOMIC_WB_NOT_READY :
                                                 RM_LS_ERR_LOAD_ACTUAL_NOT_READY,
                $sformatf("RM committed %s exists but DUT writebackLda was not found: uid=%0d robIdx=%0d/%0d expected_exception=0x%06h status_exception=0x%06h",
                          observer_kind_name(item.kind),
                          item.uid,
                          commit_rob.flag,
                          commit_rob.value,
                          item.expected_exception,
                          status_view.exception_vec))) begin
            if (!observer_mismatch_reported) begin
                `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
                observer_mismatch_reported = 1'b1;
            end
            return 1'b0;
        end
        `uvm_error("RM_LS_LDA_MISSING", ls_model.last_error_text)
        // consume_current_commit_error 已推进 current_uid；不要置位全局阻塞标志。
        observer_mismatch_reported = 1'b0;
        return 1'b0;
    end

    // 先只读比较 RM/LDA/status exception。只有三方匹配且全零时才读取
    // backing；mismatch 的 error/history/current_uid 仍统一由 commit 处理。
    if (item.kind == RM_LS_KIND_LOAD) begin
        exception_precheck_ready = ls_model.precheck_load_exception(
            item, status_view.exception_vec, exception_match, any_exception);
        if (exception_precheck_ready && exception_match && !any_exception) begin
            if (!observer_capture_load_backing(item)) begin
                if (!observer_mismatch_reported) begin
                    `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
                    observer_mismatch_reported = 1'b1;
                end
                return 1'b0;
            end
        end
    end else if (item.kind == RM_LS_KIND_ATOMIC &&
                 ls_model.pending_atomic_actual.exists(item.uid)) begin
        rm_atomic_actual_t atomic_actual;
        bit atomic_exception_match;
        atomic_actual = ls_model.pending_atomic_actual[item.uid];
        atomic_exception_match = status_view.exception_vec === item.expected_exception;
        for (int unsigned wb_index = 0;
             wb_index < atomic_actual.wb_count; wb_index++) begin
            atomic_exception_match &=
                atomic_actual.exception_vec[wb_index] === item.expected_exception;
        end
        if (atomic_exception_match && item.expected_exception == '0 &&
            !observer_capture_atomic_backing(item)) begin
            if (!observer_mismatch_reported) begin
                `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
                observer_mismatch_reported = 1'b1;
            end
            return 1'b0;
        end
    end

    commit_rob.flag = status_view.rob_idx_flag;
    commit_rob.value = status_view.rob_idx_value;
    uid_before = ls_model.current_uid;
    compare_result = ls_model.commit(commit_rob, status_view.exception_vec);
    if (!compare_result && ls_model.current_uid == uid_before) begin
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end
    if(compare_result && item.kind==RM_LS_KIND_CMO &&
       !ro.mark_dut_cmo_completion_consumed_for_rm(item.uid,
            '{flag:item.rob.flag,value:MEMBLOCK_ROB_VALUE_W'(item.rob.value)},item.dynamic_epoch))
        `uvm_error("RM_CMO_DUPLICATE_COMPLETION","CMO提交后终态审计注册失败")
    if(ls_model.current_uid!=uid_before) begin
        if((item.kind inside {RM_LS_KIND_LOAD,RM_LS_KIND_STORE}) &&
           (item.expected_exception!=0 || status_view.exception_vec!=0))
            observer_pending_address.push_back(item);
        if(item.pmm_address_semantic_mismatch) begin
            if(item.pmm_reference_result==RM_PMM_REFERENCE_MISSING)
                `uvm_error("PMM_ADDRESS_REFERENCE_MISSING",$sformatf("uid=%0d冻结G-stage参考范围不足以证明masked VPN；request=%h effective=%h，不将key差异定性为RTL错误",item.uid,item.request_vaddr,item.effective_fullva))
            else if(item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH)
                `uvm_error("PMM_ADDRESS_SEMANTIC_MISMATCH",$sformatf("uid=%0d独立冻结范围证明架构PA=%h，实际请求参考PA=%h",item.uid,item.pmm_architectural_pa,item.pmm_implementation_pa))
            else
                `uvm_info("PMM_ADDRESS_SEMANTIC_MATCH",$sformatf("uid=%0d不同VPN在冻结范围内的架构地址与实现结果相同",item.uid),UVM_LOW)
            if(item.pmm_reference_result!=RM_PMM_REFERENCE_MATCH) begin
                // 数据游标已单次推进；独立语义责任失败不能再打印本条整体PASS。
                compare_result=0;
                ls_model.set_error(item.pmm_reference_result==RM_PMM_REFERENCE_MISSING ?
                    RM_LS_ERR_PMM_ADDRESS_REFERENCE_MISSING : RM_LS_ERR_PMM_ADDRESS_SEMANTIC_MISMATCH,
                    $sformatf("uid=%0d PMM独立架构地址责任未通过 result=%0d",item.uid,item.pmm_reference_result));
            end
        end
        observer_service_exception_addresses();
        dcache_errors.release_completed_owner(item.uid,item.dynamic_epoch);
        if(item.kind==RM_LS_KIND_CMO && item.cmo_pre_access_exception==0 &&
           item.cmo_path!=RM_CMO_PATH_UNCACHE_ZERO)
            cmo_requests.mark_consumed(item);
    end
    observer_trace_commit_result(item, status_view, compare_result);
    if (!compare_result) `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
    else `uvm_info("RM_LS_COMPARE",
                   $sformatf("uid %0d committed ROB/data/exception compare PASS", uid_before),
                   UVM_LOW)
    if (compare_result && main_view.check_store) begin
        observer_check_store_pending = 1'b1;
        observer_check_store_uid = uid_before;
        observer_check_store_rob = commit_rob;
        observer_check_store_wait_logged = 1'b0;
        `uvm_info("RM_LS_FINAL_STORE",
                  $sformatf("check_store marker accepted uid=%0d ROB=%0d/%0d; schedule Store-table comparison",
                            uid_before, commit_rob.flag, commit_rob.value),
                  UVM_LOW)
    end
    observer_mismatch_reported = 1'b0;
    return compare_result;
endfunction:observer_process_current_commit

function bit memblock_rm::observer_check_final_store(
    input memblock_uid_t check_store_uid,
    input rm_ls_rob_key_t check_store_rob,
    output bit complete
);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::dcache_overlay_readiness_view_t ready_view;
    memblock_rm_observer_cache::store_final_entry_view_t store_view;
    rm_ls_store_cache_entry_t store_entry;
    bit [127:0] expected_data;
    bit [127:0] actual_data;
    bit [15:0] actual_valid_mask;
    bit [15:0] actual_corrupt_mask;
    longint unsigned observed_cycle;
    int unsigned store_entry_count;
    int unsigned checked_store_count;
    int unsigned obligation_count;
    int unsigned checked_obligation_count;
    bit pass;
    bit entry_pass;
    string entry_reason;

    complete = 1'b0;
    if (!ls_model.configured) return 1'b0;
    ro = memblock_rm_readonly_api::get();
    if (!ro.get_dcache_overlay_readiness_for_rm(ready_view) ||
        !ready_view.valid || !ready_view.ready) begin
        if (!observer_check_store_wait_logged) begin
            `uvm_info("RM_LS_TRACE_CHECK_STORE",
                      $sformatf("node=CHECK_STORE_WAIT uid=%0d rob=%0d/%0d readiness_valid=%0d readiness_ready=%0d",
                                check_store_uid, check_store_rob.flag,
                                check_store_rob.value, ready_view.valid, ready_view.ready),
                      UVM_LOW)
            observer_check_store_wait_logged = 1'b1;
        end
        return 1'b0;
    end
    complete = 1'b1;
    pass = 1'b1;
    checked_store_count = 0;
    checked_obligation_count = 0;
    store_entry_count = ls_model.expected_store_entry_count();
    `uvm_info("RM_LS_TRACE_CHECK_STORE",
              $sformatf("node=CHECK_STORE_START uid=%0d rob=%0d/%0d readiness_valid=%0d readiness_ready=%0d expected_store_count=%0d",
                        check_store_uid, check_store_rob.flag, check_store_rob.value,
                        ready_view.valid, ready_view.ready, store_entry_count),
              UVM_LOW)
    for (int unsigned store_index = 0; store_index < store_entry_count; store_index++) begin
        if (!ls_model.read_expected_store_entry(store_index, store_entry)) begin
            ls_model.set_error(RM_LS_ERR_OVERLAY_NOT_READY,
                               $sformatf("Store cache entry %0d is unavailable", store_index));
            `uvm_error("RM_LS_FINAL_STORE", ls_model.last_error_text)
            pass = 1'b0;
            continue;
        end
        checked_store_count++;
        actual_data = '0;
        actual_valid_mask = '0;
        actual_corrupt_mask = '0;
        observed_cycle = 0;
        for (int unsigned byte_index = 0;
             byte_index < store_entry.size_bytes; byte_index++) begin
            store_view = '{default:'0};
            if (ro.read_store_final_for_rm(store_entry.pa_by_byte[byte_index], store_view) &&
                store_view.valid &&
                store_view.pa == store_entry.pa_by_byte[byte_index]) begin
                actual_data[(byte_index * 8) +: 8] = store_view.value;
                actual_valid_mask[byte_index] = store_view.byte_valid;
                actual_corrupt_mask[byte_index] = store_view.corrupt;
                if (store_view.observed_cycle > observed_cycle)
                    observed_cycle = store_view.observed_cycle;
            end
        end
        entry_pass = ls_model.compare_overlay_store_entry(store_entry,
                                                          actual_valid_mask,
                                                          actual_corrupt_mask,
                                                          actual_data,
                                                          expected_data);
        entry_reason = entry_pass ? "" :
                       $sformatf(" reason=%s", ls_model.last_error_text);
        `uvm_info("RM_LS_TRACE_STORE_COMPARE",
                  $sformatf("node=STORE_COMPARE check_uid=%0d check_rob=%0d/%0d effect_uid=%0d effect_rob=%0d/%0d source=%0d size=%s(%0dB) first_pa=0x%0h last_pa=0x%0h mask=0x%0h EXPECTED(data=0x%0h) DUT(data=0x%0h valid_mask=0x%0h corrupt_mask=0x%0h observed_cycle=%0d) result=%s%s",
                            check_store_uid, check_store_rob.flag,
                            check_store_rob.value, store_entry.uid,
                            store_entry.rob.flag, store_entry.rob.value,
                            store_entry.source_kind,
                            observer_store_size_name(store_entry.size_bytes),
                            store_entry.size_bytes, store_entry.pa_by_byte[0],
                            store_entry.pa_by_byte[store_entry.size_bytes - 1],
                            store_entry.byte_mask, expected_data, actual_data,
                            actual_valid_mask, actual_corrupt_mask, observed_cycle,
                            entry_pass ? "PASS" : "FAIL",
                            entry_reason),
                  UVM_LOW)
        if (!entry_pass) begin
            `uvm_error("RM_LS_FINAL_STORE", ls_model.last_error_text)
            pass = 1'b0;
        end
    end
    obligation_count = ls_model.sc_no_write_obligation_count();
    for (int unsigned obligation_index = 0;
         obligation_index < obligation_count; obligation_index++) begin
        rm_sc_no_write_obligation_t obligation;
        bit [15:0] required_unshadowed_mask;
        bit obligation_pass;

        if (!ls_model.read_sc_no_write_obligation(obligation_index, obligation)) begin
            ls_model.set_error(RM_LS_ERR_SC_FAILURE_MEMORY_EFFECT,
                               $sformatf("SC no-write obligation %0d unavailable",
                                         obligation_index));
            `uvm_error("RM_LS_FINAL_STORE", ls_model.last_error_text)
            pass = 1'b0;
            continue;
        end
        checked_obligation_count++;
        obligation_pass = 1'b1;
        required_unshadowed_mask = obligation.byte_mask & ~obligation.shadowed_mask;
        for (int unsigned byte_index = 0;
             byte_index < obligation.size_bytes; byte_index++) begin
            byte unsigned expected_byte;
            byte unsigned backing_byte;
            bit expected_valid;
            bit backing_valid;
            bit before_valid;
            bit byte_pass;

            if (!required_unshadowed_mask[byte_index]) continue;
            byte_pass = 1'b1;
            expected_valid = ls_model.read_expected_memory_byte(
                obligation.pa_by_byte[byte_index], expected_byte);
            backing_valid = ls_model.backing_valid.exists(
                                obligation.pa_by_byte[byte_index]) &&
                            ls_model.backing_valid[obligation.pa_by_byte[byte_index]];
            if (backing_valid)
                backing_byte = ls_model.backing_byte[obligation.pa_by_byte[byte_index]];
            before_valid = obligation.expected_before_valid_mask[byte_index];
            if (before_valid &&
                (!expected_valid ||
                 expected_byte !== obligation.expected_before_value[(byte_index * 8) +: 8])) begin
                byte_pass = 1'b0;
            end
            store_view = '{default:'0};
            if (ro.read_store_final_for_rm(obligation.pa_by_byte[byte_index],
                                           store_view) && store_view.valid) begin
                if (!store_view.byte_valid || store_view.corrupt ||
                    !expected_valid || store_view.value !== expected_byte) begin
                    byte_pass = 1'b0;
                end
            end else if (expected_valid != backing_valid ||
                         (expected_valid && expected_byte !== backing_byte)) begin
                // Both absent is a valid no-write observation: an SC that failed
                // before touching DCache need not initialize a backing line.
                byte_pass = 1'b0;
            end
            if (byte_pass) obligation.checked_mask[byte_index] = 1'b1;
            else obligation_pass = 1'b0;
        end
        obligation.resolved =
            (obligation.checked_mask & required_unshadowed_mask) ==
            required_unshadowed_mask;
        obligation.evidence_kind = RM_SC_NOWRITE_FINAL_CHECK_STORE;
        `uvm_info("RM_LS_TRACE_SC_NOWRITE",
                  $sformatf("uid=%0d rob=%0d/%0d mask=0x%0h shadowed=0x%0h checked=0x%0h resolved=%0d fully_verified=%0d result=%s",
                            obligation.uid, obligation.rob.flag,
                            obligation.rob.value, obligation.byte_mask,
                            obligation.shadowed_mask, obligation.checked_mask,
                            obligation.resolved,
                            obligation.resolved && obligation.shadowed_mask == '0,
                            obligation_pass && obligation.resolved ? "PASS" : "FAIL"),
                  UVM_LOW)
        if (!obligation_pass || !obligation.resolved) begin
            ls_model.set_error(RM_LS_ERR_SC_FAILURE_MEMORY_EFFECT,
                               $sformatf("SC failure uid %0d no-write obligation unresolved shadowed=0x%0h checked=0x%0h",
                                         obligation.uid, obligation.shadowed_mask,
                                         obligation.checked_mask));
            `uvm_error("RM_LS_FINAL_STORE", ls_model.last_error_text)
            pass = 1'b0;
        end
    end
    `uvm_info("RM_LS_TRACE_CHECK_STORE",
              $sformatf("node=CHECK_STORE_DONE uid=%0d rob=%0d/%0d checked_effects=%0d checked_sc_no_write=%0d result=%s",
                        check_store_uid, check_store_rob.flag,
                        check_store_rob.value, checked_store_count,
                        checked_obligation_count,
                        pass ? "PASS" : "FAIL"),
              UVM_LOW)
    if (pass)
        `uvm_info("RM_LS_FINAL_STORE",
                  $sformatf("check_store uid=%0d ROB=%0d/%0d Store-table compare PASS",
                            check_store_uid, check_store_rob.flag, check_store_rob.value),
                  UVM_LOW)
    return pass;
endfunction:observer_check_final_store

task memblock_rm::observer_poll_process();
    forever begin
        observer_service_path_facts();
        void'(observer_process_current_commit());
        #1ns;
    end
endtask:observer_poll_process

task memblock_rm::main_process();

    backendToTopBypass_agent_agent_xaction  backendToTopBypass_agent_tr_in;
    memblock_common_xaction  backendToTopBypass_agent_tr_out;

    fence_agent_agent_xaction  fence_agent_tr_in;
    memblock_common_xaction  fence_agent_tr_out;

    csr_ctrl_agent_agent_xaction  csr_ctrl_agent_tr_in;
    memblock_common_xaction  csr_ctrl_agent_tr_out;

    lsqcommit_agent_agent_xaction  lsqcommit_agent_tr_in;
    memblock_common_xaction  lsqcommit_agent_tr_out;

    lsqenq_agent_agent_xaction  lsqenq_agent_tr_in;
    memblock_common_xaction  lsqenq_agent_tr_out;

    lintsissue_agent_agent_xaction  lintsissue_agent_tr_in;

    vecissue_agent_agent_xaction  vecissue_agent_tr_in;
    memblock_common_xaction  vecissue_agent_tr_out;

    redirect_agent_agent_xaction  redirect_agent_tr_in;
    memblock_common_xaction  redirect_agent_tr_out;

    sbuffer_agent_agent_xaction  sbuffer_agent_tr_in;
    memblock_common_xaction  sbuffer_agent_tr_out;

    dcache_agent_agent_xaction  dcache_agent_tr_in;
    memblock_common_xaction  dcache_agent_tr_out;

    int_sink_agent_agent_xaction  int_sink_agent_tr_in;
    memblock_common_xaction  int_sink_agent_tr_out;

    L2tlb_agent_agent_xaction  L2tlb_agent_tr_in;
    memblock_common_xaction  L2tlb_agent_tr_out;

    itlb_agent_agent_xaction  itlb_agent_tr_in;
    memblock_common_xaction  itlb_agent_tr_out;

    prefetch_agent_agent_xaction  prefetch_agent_tr_in;
    memblock_common_xaction  prefetch_agent_tr_out;

    io_mem_to_ooo_ctrl_agent_agent_xaction  io_mem_to_ooo_ctrl_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_ctrl_agent_tr_out;

    io_mem_to_ooo_int_wb_agent_agent_xaction  io_mem_to_ooo_int_wb_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_int_wb_agent_tr_out;

    io_mem_to_ooo_vec_wb_agent_agent_xaction  io_mem_to_ooo_vec_wb_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_vec_wb_agent_tr_out;

    io_mem_to_ooo_wakeup_agent_agent_xaction  io_mem_to_ooo_wakeup_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_wakeup_agent_tr_out;

    io_mem_to_ooo_iq_feedback_agent_agent_xaction  io_mem_to_ooo_iq_feedback_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_iq_feedback_agent_tr_out;

    other_ctrl_agent_agent_xaction  other_ctrl_agent_tr_in;
    memblock_common_xaction  other_ctrl_agent_tr_out;

    fork

        while(1)begin
            this.backendToTopBypass_agent_mon_item_port.get(backendToTopBypass_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("backendToTopBypass_agent_mon_item_port get as %s",backendToTopBypass_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(backendToTopBypass_agent_tr_out, backendToTopBypass_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("backendToTopBypass_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            backendToTopBypass_agent_tr_out = memblock_common_xaction::type_id::create("backendToTopBypass_agent_tr_out");
            backendToTopBypass_agent_tr_out.channel_id = backendToTopBypass_agent_tr_in.channel_id;
            backendToTopBypass_agent_tr_out.pack_backendToTopBypass_agent(backendToTopBypass_agent_tr_in);

            this.rm_item_exp_port.write(backendToTopBypass_agent_tr_out);
            //this.rm_item_act_port.write(backendToTopBypass_agent_tr_out);
        end

        while(1)begin
            this.fence_agent_mon_item_port.get(fence_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("fence_agent_mon_item_port get as %s",fence_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(fence_agent_tr_out, fence_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("fence_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            fence_agent_tr_out = memblock_common_xaction::type_id::create("fence_agent_tr_out");
            fence_agent_tr_out.channel_id = fence_agent_tr_in.channel_id;
            fence_agent_tr_out.pack_fence_agent(fence_agent_tr_in);

            this.rm_item_exp_port.write(fence_agent_tr_out);
            //this.rm_item_act_port.write(fence_agent_tr_out);
        end

        while(1)begin
            this.csr_ctrl_agent_mon_item_port.get(csr_ctrl_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("csr_ctrl_agent_mon_item_port get as %s",csr_ctrl_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(csr_ctrl_agent_tr_out, csr_ctrl_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("csr_ctrl_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            csr_ctrl_agent_tr_out = memblock_common_xaction::type_id::create("csr_ctrl_agent_tr_out");
            csr_ctrl_agent_tr_out.channel_id = csr_ctrl_agent_tr_in.channel_id;
            csr_ctrl_agent_tr_out.pack_csr_ctrl_agent(csr_ctrl_agent_tr_in);

            this.rm_item_exp_port.write(csr_ctrl_agent_tr_out);
            //this.rm_item_act_port.write(csr_ctrl_agent_tr_out);
        end

        while(1)begin
            this.lsqcommit_agent_mon_item_port.get(lsqcommit_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("lsqcommit_agent_mon_item_port get as %s",lsqcommit_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(lsqcommit_agent_tr_out, lsqcommit_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("lsqcommit_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            lsqcommit_agent_tr_out = memblock_common_xaction::type_id::create("lsqcommit_agent_tr_out");
            lsqcommit_agent_tr_out.channel_id = lsqcommit_agent_tr_in.channel_id;
            lsqcommit_agent_tr_out.pack_lsqcommit_agent(lsqcommit_agent_tr_in);

            this.rm_item_exp_port.write(lsqcommit_agent_tr_out);
            //this.rm_item_act_port.write(lsqcommit_agent_tr_out);
        end

        while(1)begin
            this.lsqenq_agent_mon_item_port.get(lsqenq_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("lsqenq_agent_mon_item_port get as %s",lsqenq_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(lsqenq_agent_tr_out, lsqenq_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("lsqenq_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            lsqenq_agent_tr_out = memblock_common_xaction::type_id::create("lsqenq_agent_tr_out");
            lsqenq_agent_tr_out.channel_id = lsqenq_agent_tr_in.channel_id;
            lsqenq_agent_tr_out.pack_lsqenq_agent(lsqenq_agent_tr_in);

            this.rm_item_exp_port.write(lsqenq_agent_tr_out);
            //this.rm_item_act_port.write(lsqenq_agent_tr_out);
        end

        while(1)begin
            this.lintsissue_agent_mon_item_port.get(lintsissue_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("lintsissue_agent_mon_item_port get as %s",lintsissue_agent_tr_in.psdisplay()),UVM_DEBUG)
            observer_check_atomic_issue_monitor(lintsissue_agent_tr_in);
        end

        while(1)begin
            this.vecissue_agent_mon_item_port.get(vecissue_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("vecissue_agent_mon_item_port get as %s",vecissue_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(vecissue_agent_tr_out, vecissue_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("vecissue_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            vecissue_agent_tr_out = memblock_common_xaction::type_id::create("vecissue_agent_tr_out");
            vecissue_agent_tr_out.channel_id = vecissue_agent_tr_in.channel_id;
            vecissue_agent_tr_out.pack_vecissue_agent(vecissue_agent_tr_in);

            this.rm_item_exp_port.write(vecissue_agent_tr_out);
            //this.rm_item_act_port.write(vecissue_agent_tr_out);
        end

        while(1)begin
            this.redirect_agent_mon_item_port.get(redirect_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("redirect_agent_mon_item_port get as %s",redirect_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(redirect_agent_tr_out, redirect_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("redirect_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            redirect_agent_tr_out = memblock_common_xaction::type_id::create("redirect_agent_tr_out");
            redirect_agent_tr_out.channel_id = redirect_agent_tr_in.channel_id;
            redirect_agent_tr_out.pack_redirect_agent(redirect_agent_tr_in);

            this.rm_item_exp_port.write(redirect_agent_tr_out);
            //this.rm_item_act_port.write(redirect_agent_tr_out);
        end

        while(1)begin
            this.sbuffer_agent_mon_item_port.get(sbuffer_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("sbuffer_agent_mon_item_port get as %s",sbuffer_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(sbuffer_agent_tr_out, sbuffer_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("sbuffer_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            sbuffer_agent_tr_out = memblock_common_xaction::type_id::create("sbuffer_agent_tr_out");
            sbuffer_agent_tr_out.channel_id = sbuffer_agent_tr_in.channel_id;
            sbuffer_agent_tr_out.pack_sbuffer_agent(sbuffer_agent_tr_in);

            this.rm_item_exp_port.write(sbuffer_agent_tr_out);
            //this.rm_item_act_port.write(sbuffer_agent_tr_out);
        end

        while(1)begin
            this.dcache_agent_mon_item_port.get(dcache_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("dcache_agent_mon_item_port get as %s",dcache_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(dcache_agent_tr_out, dcache_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("dcache_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            dcache_agent_tr_out = memblock_common_xaction::type_id::create("dcache_agent_tr_out");
            dcache_agent_tr_out.channel_id = dcache_agent_tr_in.channel_id;
            dcache_agent_tr_out.pack_dcache_agent(dcache_agent_tr_in);

            this.rm_item_exp_port.write(dcache_agent_tr_out);
            //this.rm_item_act_port.write(dcache_agent_tr_out);
        end

        while(1)begin
            this.int_sink_agent_mon_item_port.get(int_sink_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("int_sink_agent_mon_item_port get as %s",int_sink_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(int_sink_agent_tr_out, int_sink_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("int_sink_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            int_sink_agent_tr_out = memblock_common_xaction::type_id::create("int_sink_agent_tr_out");
            int_sink_agent_tr_out.channel_id = int_sink_agent_tr_in.channel_id;
            int_sink_agent_tr_out.pack_int_sink_agent(int_sink_agent_tr_in);

            this.rm_item_exp_port.write(int_sink_agent_tr_out);
            //this.rm_item_act_port.write(int_sink_agent_tr_out);
        end

        while(1)begin
            this.L2tlb_agent_mon_item_port.get(L2tlb_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("L2tlb_agent_mon_item_port get as %s",L2tlb_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(L2tlb_agent_tr_out, L2tlb_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("L2tlb_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            L2tlb_agent_tr_out = memblock_common_xaction::type_id::create("L2tlb_agent_tr_out");
            L2tlb_agent_tr_out.channel_id = L2tlb_agent_tr_in.channel_id;
            L2tlb_agent_tr_out.pack_L2tlb_agent(L2tlb_agent_tr_in);

            this.rm_item_exp_port.write(L2tlb_agent_tr_out);
            //this.rm_item_act_port.write(L2tlb_agent_tr_out);
        end

        while(1)begin
            this.itlb_agent_mon_item_port.get(itlb_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("itlb_agent_mon_item_port get as %s",itlb_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(itlb_agent_tr_out, itlb_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("itlb_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            itlb_agent_tr_out = memblock_common_xaction::type_id::create("itlb_agent_tr_out");
            itlb_agent_tr_out.channel_id = itlb_agent_tr_in.channel_id;
            itlb_agent_tr_out.pack_itlb_agent(itlb_agent_tr_in);

            this.rm_item_exp_port.write(itlb_agent_tr_out);
            //this.rm_item_act_port.write(itlb_agent_tr_out);
        end

        while(1)begin
            this.prefetch_agent_mon_item_port.get(prefetch_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("prefetch_agent_mon_item_port get as %s",prefetch_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(prefetch_agent_tr_out, prefetch_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("prefetch_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            prefetch_agent_tr_out = memblock_common_xaction::type_id::create("prefetch_agent_tr_out");
            prefetch_agent_tr_out.channel_id = prefetch_agent_tr_in.channel_id;
            prefetch_agent_tr_out.pack_prefetch_agent(prefetch_agent_tr_in);

            this.rm_item_exp_port.write(prefetch_agent_tr_out);
            //this.rm_item_act_port.write(prefetch_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_ctrl_agent_mon_item_port.get(io_mem_to_ooo_ctrl_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_ctrl_agent_mon_item_port get as %s",io_mem_to_ooo_ctrl_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_ctrl_agent_tr_out, io_mem_to_ooo_ctrl_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_ctrl_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_ctrl_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_ctrl_agent_tr_out");
            io_mem_to_ooo_ctrl_agent_tr_out.channel_id = io_mem_to_ooo_ctrl_agent_tr_in.channel_id;
            io_mem_to_ooo_ctrl_agent_tr_out.pack_io_mem_to_ooo_ctrl_agent(io_mem_to_ooo_ctrl_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_ctrl_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_ctrl_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_int_wb_agent_mon_item_port.get(io_mem_to_ooo_int_wb_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_int_wb_agent_mon_item_port get as %s",io_mem_to_ooo_int_wb_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_int_wb_agent_tr_out, io_mem_to_ooo_int_wb_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_int_wb_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_int_wb_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_int_wb_agent_tr_out");
            io_mem_to_ooo_int_wb_agent_tr_out.channel_id = io_mem_to_ooo_int_wb_agent_tr_in.channel_id;
            io_mem_to_ooo_int_wb_agent_tr_out.pack_io_mem_to_ooo_int_wb_agent(io_mem_to_ooo_int_wb_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_int_wb_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_int_wb_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_vec_wb_agent_mon_item_port.get(io_mem_to_ooo_vec_wb_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_vec_wb_agent_mon_item_port get as %s",io_mem_to_ooo_vec_wb_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_vec_wb_agent_tr_out, io_mem_to_ooo_vec_wb_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_vec_wb_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_vec_wb_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_vec_wb_agent_tr_out");
            io_mem_to_ooo_vec_wb_agent_tr_out.channel_id = io_mem_to_ooo_vec_wb_agent_tr_in.channel_id;
            io_mem_to_ooo_vec_wb_agent_tr_out.pack_io_mem_to_ooo_vec_wb_agent(io_mem_to_ooo_vec_wb_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_vec_wb_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_vec_wb_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_wakeup_agent_mon_item_port.get(io_mem_to_ooo_wakeup_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_wakeup_agent_mon_item_port get as %s",io_mem_to_ooo_wakeup_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_wakeup_agent_tr_out, io_mem_to_ooo_wakeup_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_wakeup_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_wakeup_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_wakeup_agent_tr_out");
            io_mem_to_ooo_wakeup_agent_tr_out.channel_id = io_mem_to_ooo_wakeup_agent_tr_in.channel_id;
            io_mem_to_ooo_wakeup_agent_tr_out.pack_io_mem_to_ooo_wakeup_agent(io_mem_to_ooo_wakeup_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_wakeup_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_wakeup_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_iq_feedback_agent_mon_item_port.get(io_mem_to_ooo_iq_feedback_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_iq_feedback_agent_mon_item_port get as %s",io_mem_to_ooo_iq_feedback_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_iq_feedback_agent_tr_out, io_mem_to_ooo_iq_feedback_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_iq_feedback_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_iq_feedback_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_iq_feedback_agent_tr_out");
            io_mem_to_ooo_iq_feedback_agent_tr_out.channel_id = io_mem_to_ooo_iq_feedback_agent_tr_in.channel_id;
            io_mem_to_ooo_iq_feedback_agent_tr_out.pack_io_mem_to_ooo_iq_feedback_agent(io_mem_to_ooo_iq_feedback_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_iq_feedback_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_iq_feedback_agent_tr_out);
        end

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

    join_none
endtask

`endif
