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

class memblock_rm  extends tcnt_rm_base #(.seq_item_t(memblock_common_xaction));

    //virtual tc_if vif;
    memblock_env_cfg cfg;
    // 中文注释：RM 自有的 expected 状态 owner。框架只通过只读 value view
    // 提供事实；RM 不持有、修改或推进任一框架 owner。
    rm_ls_model_t ls_model;
    int unsigned observer_main_count;
    bit          observer_check_store_pending;
    memblock_uid_t observer_check_store_uid;
    rm_ls_rob_key_t observer_check_store_rob;
    bit          observer_check_store_wait_logged;
    bit          observer_mismatch_reported;
    bit          observer_trace_main_emitted[int unsigned];
    bit          observer_trace_translation_emitted[int unsigned];
    bit          observer_trace_dut_load_emitted[int unsigned];

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
    extern virtual task run_phase(uvm_phase phase);
    extern virtual task main_phase(uvm_phase phase);
    extern virtual task main_process();
    extern virtual task observer_poll_process();
    extern function int unsigned observer_scalar_size(bit [8:0] fu_op_type);
    extern function bit [63:0] observer_compute_vaddr(bit [63:0] src_0,
                                                      bit [63:0] imm);
    extern function bit observer_stage_permission_fault(
        memblock_rm_readonly_api::tlb_entry_view_t entry,
        memblock_rm_readonly_api::tlb_request_context_view_t tlb_context,
        bit store_access,
        bit stage_one
    );
    extern function bit observer_build_commit_item(
        memblock_rm_readonly_api::main_transaction_view_t main_view,
        memblock_rm_readonly_api::status_view_t status_view,
        output rm_ls_program_item_t item
    );
    extern function bit observer_capture_commit_actual(
        memblock_rm_readonly_api::main_transaction_view_t main_view,
        memblock_rm_readonly_api::status_view_t status_view,
        rm_ls_program_item_t item
    );
    extern function bit observer_capture_load_backing(rm_ls_program_item_t item);
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
    this.observer_main_count = 0;
    this.observer_check_store_pending = 1'b0;
    this.observer_check_store_uid = '0;
    this.observer_check_store_rob = '{default:'0};
    this.observer_check_store_wait_logged = 1'b0;
    this.observer_mismatch_reported = 1'b0;
    this.observer_trace_main_emitted.delete();
    this.observer_trace_translation_emitted.delete();
    this.observer_trace_dut_load_emitted.delete();
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

function int unsigned memblock_rm::observer_scalar_size(bit [8:0] fu_op_type);
    case (fu_op_type)
        MEMBLOCK_LSUOP_LB,
        MEMBLOCK_LSUOP_LBU,
        MEMBLOCK_LSUOP_SB: return 1;
        MEMBLOCK_LSUOP_LH,
        MEMBLOCK_LSUOP_LHU,
        MEMBLOCK_LSUOP_SH: return 2;
        MEMBLOCK_LSUOP_LW,
        MEMBLOCK_LSUOP_LWU,
        MEMBLOCK_LSUOP_SW: return 4;
        MEMBLOCK_LSUOP_LD,
        MEMBLOCK_LSUOP_SD: return 8;
        default: return 0;
    endcase
endfunction:observer_scalar_size

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
    bit stage_one
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
        mxr = tlb_context.s2xlate == 0 ? tlb_context.priv_mxr : tlb_context.priv_vmxr;
        sum = tlb_context.s2xlate == 0 ? tlb_context.priv_sum : tlb_context.priv_vsum;
        privilege = tlb_context.priv_dmode;
    end else begin
        if (!entry.s2_stage_active) return 1'b0;
        pte_r = entry.s2_pte_r; pte_w = entry.s2_pte_w;
        pte_x = entry.s2_pte_x; pte_u = entry.s2_pte_u;
        pte_a = entry.s2_pte_a; pte_d = entry.s2_pte_d;
        pte_v = 1'b1; mxr = tlb_context.priv_vmxr; sum = 1'b1;
        privilege = 2'd3;
    end
    operation_allowed = store_access ? (pte_w && pte_a && pte_d) :
                                       ((pte_r || (pte_x && mxr)) && pte_a);
    privilege_allowed = 1'b1;
    if (stage_one) begin
        if (privilege == 2'd0) privilege_allowed = pte_u;
        else if (privilege == 2'd1) privilege_allowed = !pte_u || sum;
    end
    return !pte_v || (pte_w && !pte_r) || !operation_allowed || !privilege_allowed;
endfunction:observer_stage_permission_fault

function bit memblock_rm::observer_build_commit_item(
    memblock_rm_readonly_api::main_transaction_view_t main_view,
    memblock_rm_readonly_api::status_view_t status_view,
    output rm_ls_program_item_t item
);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context;
    memblock_rm_readonly_api::tlb_entry_view_t entry;
    bit access_fault;
    bit stage_one_fault;
    bit stage_two_fault;
    bit misaligned;
    bit store_access;
    bit s1_active;
    bit s2_active;
    bit bare_identity;
    bit [63:0] byte_va;
    bit [43:0] final_ppn;
    bit [3:0] expected_s1_mode;
    int unsigned bad_byte_index;
    rm_ls_error_e translation_path_error;

    item = new(main_view.uid);
    item.valid = 1'b1;
    item.uid = main_view.uid;
    item.op = main_view.fu_op_type;
    item.main_vaddr = main_view.vaddr;
    item.computed_vaddr = observer_compute_vaddr(main_view.src_0, main_view.imm);
    item.rob.flag = main_view.rob_idx_flag;
    item.rob.value = main_view.rob_idx_value;
    item.size_bytes = observer_scalar_size(main_view.fu_op_type);
    item.is_signed = main_view.fu_op_type == MEMBLOCK_LSUOP_LB ||
                     main_view.fu_op_type == MEMBLOCK_LSUOP_LH ||
                     main_view.fu_op_type == MEMBLOCK_LSUOP_LW ||
                     main_view.fu_op_type == MEMBLOCK_LSUOP_LD;
    if (main_view.fu_type == MEMBLOCK_FUTYPE_LDU &&
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
                      $sformatf("node=TRANSLATION uid=%0d rob=%0d/%0d kind=%s path=NOT_REQUIRED va=0x%0h pa_mask=0x0 expected_exception=0x%0h",
                                item.uid, item.rob.flag, item.rob.value,
                                observer_kind_name(item.kind), item.computed_vaddr,
                                item.expected_exception),
                      UVM_LOW)
            observer_trace_translation_emitted[item.uid] = 1'b1;
        end
        return 1'b1;
    end
    if (item.kind == RM_LS_KIND_STORE) begin
        item.store_data = main_view.src_0;
        item.store_byte_mask = 8'hff >> (8 - item.size_bytes);
    end
    if (item.computed_vaddr != item.main_vaddr) begin
        ls_model.set_error(RM_LS_ERR_MAIN_VADDR_MISMATCH,
                           $sformatf("uid %0d computed VA 0x%0h main VA 0x%0h",
                                     item.uid, item.computed_vaddr, item.main_vaddr));
        return 1'b0;
    end

    ro = memblock_rm_readonly_api::get();
    if (!ro.read_tlb_request_context_for_rm(item.uid, item.computed_vaddr[63:12], tlb_context) ||
        !tlb_context.valid) begin
        ls_model.set_error(RM_LS_ERR_TLB_CONTEXT_NOT_READY,
                           $sformatf("uid %0d first VPN frozen request context unavailable",
                                     item.uid));
        return 1'b0;
    end
    if (!rm_ls_decode_translation_path(tlb_context.s2xlate,
                                       tlb_context.is_hypervisor_inst,
                                       tlb_context.priv_virt,
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
    `uvm_info("RM_LS_TRANSLATION",
              $sformatf("uid=%0d translation_path=%s satp=%0d vsatp=%0d hgatp=%0d s2xlate=%0d csr_seq=%0d",
                        item.uid, bare_identity ? "BARE_IDENTITY" : "PAGED_ENTRY",
                        tlb_context.satp_mode, tlb_context.vsatp_mode,
                        tlb_context.hgatp_mode, tlb_context.s2xlate,
                        tlb_context.csr_update_seq),
              UVM_LOW)
    item.translation_key = tlb_context.request_key;
    if (!bare_identity) begin
        if (!ro.resolve_tlb_entry_key_for_rm(item.uid, item.computed_vaddr[63:12],
                                             tlb_context)) begin
            ls_model.set_error(RM_LS_ERR_TLB_ENTRY_NOT_READY,
                               $sformatf("uid %0d first VPN active translation exact/range entry unavailable",
                                         item.uid));
            return 1'b0;
        end
        item.translation_key = tlb_context.entry_key;
    end
    store_access = item.kind == RM_LS_KIND_STORE;
    misaligned = (item.computed_vaddr & (item.size_bytes - 1)) != 0;
    if (misaligned) begin
        item.expected_exception[store_access ? 6 : 4] = 1'b1;
        item.translation_valid = 1'b1;
        return 1'b1;
    end

    access_fault = main_view.tlb_af || main_view.pma_af ||
                   main_view.denied || main_view.corrupt;
    stage_one_fault = main_view.tlb_pf;
    stage_two_fault = main_view.tlb_gpf;
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
        for (int unsigned byte_index = 0; byte_index < item.size_bytes; byte_index++) begin
            byte_va = item.computed_vaddr + byte_index;
            if (!ro.resolve_tlb_entry_key_for_rm(item.uid, byte_va[63:12], tlb_context)) begin
                ls_model.set_error(RM_LS_ERR_TLB_ENTRY_NOT_READY,
                                   $sformatf("uid %0d byte %0d active translation exact/range entry unavailable",
                                             item.uid, byte_index));
                return 1'b0;
            end
            if (!ro.read_tlb_entry_for_rm(tlb_context.entry_key, entry)) begin
                ls_model.set_error(RM_LS_ERR_TLB_ENTRY_NOT_READY,
                                   $sformatf("uid %0d byte %0d canonical TLB entry unavailable",
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
            if (byte_index == 0) item.translation_key = tlb_context.entry_key;
            access_fault |= entry.pma_af || entry.fault_effective_s1_af ||
                            entry.fault_effective_s2_gaf;
            stage_one_fault |= entry.fault_effective_s1_pf ||
                               observer_stage_permission_fault(entry, tlb_context,
                                                               store_access, 1'b1);
            stage_two_fault |= entry.fault_effective_s2_gpf ||
                               observer_stage_permission_fault(entry, tlb_context,
                                                               store_access, 1'b0);
            if (!entry.fault && !entry.pma_af && tlb_context.request_translation_valid) begin
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
        end
    end
    if (access_fault) item.expected_exception[store_access ? 7 : 5] = 1'b1;
    else if (stage_one_fault) item.expected_exception[store_access ? 15 : 13] = 1'b1;
    else if (stage_two_fault) item.expected_exception[store_access ? 23 : 21] = 1'b1;
    else if (item.pa_valid_mask != (8'hff >> (8 - item.size_bytes))) begin
        ls_model.set_error(RM_LS_ERR_TRANSLATION_NOT_READY,
                           $sformatf("uid %0d normal access has incomplete PA geometry mask=0x%0h",
                                     item.uid, item.pa_valid_mask));
        return 1'b0;
    end
    item.translation_valid = 1'b1;
    if (!observer_trace_translation_emitted.exists(item.uid)) begin
        `uvm_info("RM_LS_TRACE_TRANSLATION",
                  $sformatf("node=TRANSLATION uid=%0d rob=%0d/%0d kind=%s path=%s va=0x%0h first_pa=0x%0h last_pa=0x%0h pa_mask=0x%0h expected_exception=0x%0h",
                            item.uid, item.rob.flag, item.rob.value,
                            observer_kind_name(item.kind),
                            bare_identity ? "BARE_IDENTITY" : "PAGED_ENTRY",
                            item.computed_vaddr, item.pa_by_byte[0],
                            item.pa_by_byte[item.size_bytes - 1], item.pa_valid_mask,
                            item.expected_exception),
                  UVM_LOW)
        observer_trace_translation_emitted[item.uid] = 1'b1;
    end
    return 1'b1;
endfunction:observer_build_commit_item

function bit memblock_rm::observer_capture_commit_actual(
    memblock_rm_readonly_api::main_transaction_view_t main_view,
    memblock_rm_readonly_api::status_view_t status_view,
    rm_ls_program_item_t item
);
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::dut_writeback_view_t load_view;
    rm_ls_load_actual_t load_actual;
    memblock_rob_key_t load_rob_key;

    ro = memblock_rm_readonly_api::get();
    if (item.kind == RM_LS_KIND_LOAD) begin
        load_rob_key.flag = main_view.rob_idx_flag;
        load_rob_key.value = main_view.rob_idx_value;
        if (!ro.read_dut_load_writeback_for_rm(load_rob_key, load_view) ||
            !load_view.valid ||
            !load_view.rob_valid || !load_view.rob_flag_valid ||
            !load_view.data_valid || !load_view.exception_valid ||
            load_view.source_kind != 2'd1 ||
            load_view.rob_flag != load_rob_key.flag ||
            load_view.rob_value != load_rob_key.value ||
            load_view.replay_inst || load_view.flush_pipe) begin
            ls_model.set_error(RM_LS_ERR_LOAD_ACTUAL_NOT_READY,
                               $sformatf("uid %0d committed ROB %0d/%0d DUT Load writeback is unavailable",
                                         item.uid, load_rob_key.flag, load_rob_key.value));
            return 1'b0;
        end
        load_actual = new(); load_actual.valid = 1'b1; load_actual.uid = item.uid;
        load_actual.rob.flag = load_view.rob_flag;
        load_actual.rob.value = load_view.rob_value;
        load_actual.data_valid = (load_view.exception_vec == '0);
        load_actual.data = load_view.data;
        load_actual.exception_vec = load_view.exception_vec;
        load_actual.cycle = load_view.sample_cycle;
        if (!observer_trace_dut_load_emitted.exists(item.uid)) begin
            `uvm_info("RM_LS_TRACE_DUT_LOAD",
                      $sformatf("node=DUT_LOAD_WB uid=%0d rob=%0d/%0d source=LDA lane=%0d sample_cycle=%0d DUT(raw_data_valid=%0d comparable_data_valid=%0d data=0x%016h exception=0x%06h replay=%0d flush=%0d)",
                                item.uid, load_actual.rob.flag, load_actual.rob.value,
                                load_view.lane, load_view.sample_cycle,
                                load_view.data_valid, load_actual.data_valid,
                                load_actual.data, load_actual.exception_vec,
                                load_view.replay_inst, load_view.flush_pipe),
                      UVM_LOW)
            observer_trace_dut_load_emitted[item.uid] = 1'b1;
        end
        return ls_model.push_load_actual(load_actual);
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

function string memblock_rm::observer_kind_name(rm_ls_kind_e kind);
    case (kind)
        RM_LS_KIND_LOAD:  return "LOAD";
        RM_LS_KIND_STORE: return "STORE";
        default:          return "CONTROL_OR_OTHER";
    endcase
endfunction:observer_kind_name

function string memblock_rm::observer_store_size_name(int unsigned size_bytes);
    case (size_bytes)
        1: return "BYTE";
        2: return "HALF";
        4: return "WORD";
        8: return "DOUBLE";
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

    ro = memblock_rm_readonly_api::get();
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
    if (!ro.read_status_for_rm(ls_model.current_uid, status_view) ||
        !status_view.valid || !status_view.rob_commit) return 1'b0;
    if (!ro.read_main_transaction_for_rm(ls_model.current_uid, main_view) || !main_view.valid)
        return 1'b0;
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
                  $sformatf("node=MAIN_TABLE uid=%0d rob=%0d/%0d op_class=%0d check_store=%0d fu_type=0x%0h fu_op=0x%0h src_0=0x%016h imm=0x%016h main_va=0x%016h status(rob_commit=%0d terminal=%0d exception=0x%06h)",
                            main_view.uid, main_view.rob_idx_flag,
                            main_view.rob_idx_value, main_view.op_class,
                            main_view.check_store, main_view.fu_type,
                            main_view.fu_op_type, main_view.src_0, main_view.imm,
                            main_view.vaddr, status_view.rob_commit,
                            status_view.terminal_done, status_view.exception_vec),
                  UVM_LOW)
        observer_trace_main_emitted[main_view.uid] = 1'b1;
    end
    if (status_view.replay_pending || status_view.redirect_pending ||
        status_view.issue_killed || status_view.flushed) begin
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMMIT",
                       $sformatf("uid %0d commit status still carries stale replay/flush state",
                                 main_view.uid))
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
    end
    if (!observer_build_commit_item(main_view, status_view, item) ||
        !ls_model.add_item(item) ||
        !observer_capture_commit_actual(main_view, status_view, item) ||
        !observer_capture_load_backing(item)) begin
        if (!observer_mismatch_reported) begin
            `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
            observer_mismatch_reported = 1'b1;
        end
        return 1'b0;
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
    bit [63:0] expected_data;
    bit [63:0] actual_data;
    bit [7:0] actual_valid_mask;
    bit [7:0] actual_corrupt_mask;
    longint unsigned observed_cycle;
    int unsigned store_entry_count;
    int unsigned checked_store_count;
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
                  $sformatf("node=STORE_COMPARE check_uid=%0d check_rob=%0d/%0d store_uid=%0d store_rob=%0d/%0d size=%s(%0dB) first_pa=0x%0h last_pa=0x%0h mask=0x%0h EXPECTED(data=0x%0h) DUT(data=0x%0h valid_mask=0x%0h corrupt_mask=0x%0h observed_cycle=%0d) result=%s%s",
                            check_store_uid, check_store_rob.flag,
                            check_store_rob.value, store_entry.uid,
                            store_entry.rob.flag, store_entry.rob.value,
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
    `uvm_info("RM_LS_TRACE_CHECK_STORE",
              $sformatf("node=CHECK_STORE_DONE uid=%0d rob=%0d/%0d checked_stores=%0d result=%s",
                        check_store_uid, check_store_rob.flag,
                        check_store_rob.value, checked_store_count,
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
    memblock_common_xaction  lintsissue_agent_tr_out;

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
            //if(!$cast(lintsissue_agent_tr_out, lintsissue_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("lintsissue_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            lintsissue_agent_tr_out = memblock_common_xaction::type_id::create("lintsissue_agent_tr_out");
            lintsissue_agent_tr_out.channel_id = lintsissue_agent_tr_in.channel_id;
            lintsissue_agent_tr_out.pack_lintsissue_agent(lintsissue_agent_tr_in);

            this.rm_item_exp_port.write(lintsissue_agent_tr_out);
            //this.rm_item_act_port.write(lintsissue_agent_tr_out);
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
            other_ctrl_agent_tr_out = memblock_common_xaction::type_id::create("other_ctrl_agent_tr_out");
            other_ctrl_agent_tr_out.channel_id = other_ctrl_agent_tr_in.channel_id;
            other_ctrl_agent_tr_out.pack_other_ctrl_agent(other_ctrl_agent_tr_in);

            this.rm_item_exp_port.write(other_ctrl_agent_tr_out);
            //this.rm_item_act_port.write(other_ctrl_agent_tr_out);
        end

    join_none
endtask

`endif
