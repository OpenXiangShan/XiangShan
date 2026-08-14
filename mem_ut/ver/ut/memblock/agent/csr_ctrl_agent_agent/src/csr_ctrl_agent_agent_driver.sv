//=========================================================
//File name    : csr_ctrl_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : csr_ctrl_agent_agent_driver
//Discribution : csr_ctrl_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef CSR_CTRL_AGENT_AGENT_DRIVER__SV
`define CSR_CTRL_AGENT_AGENT_DRIVER__SV

class csr_ctrl_agent_agent_driver  extends tcnt_driver_base#(virtual csr_ctrl_agent_agent_interface,csr_ctrl_agent_agent_cfg,csr_ctrl_agent_agent_xaction);

    `uvm_component_utils(csr_ctrl_agent_agent_driver)

    // 中文注释：L2 flush 高电平的唯一持有者。ASSERT item 的 send_pkt() 建立完整
    // CSR baseline；无 item 的 idle 周期仍使用该 baseline 驱动 high。RELEASE item
    // 只有 owner 完全匹配时才能清除，避免旧 token 拉低新一代请求。
    bit                              l2_flush_level_hold_valid;
    csr_ctrl_agent_agent_xaction     l2_flush_level_hold_tr;
    int unsigned                     l2_flush_level_hold_uid;
    int unsigned                     l2_flush_level_hold_dynamic_epoch;
    int unsigned                     l2_flush_level_hold_action_generation;
    int unsigned                     l2_flush_level_hold_kind_code;
    int unsigned                     l2_flush_level_hold_control_reset_epoch;

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(csr_ctrl_agent_agent_xaction tr);
    extern task drive_pkt_fields(csr_ctrl_agent_agent_xaction tr);
    extern task drive_l2_flush_level_hold();
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
    extern function bit control_l2_flush_metadata_complete(
        input csr_ctrl_agent_agent_xaction tr
    );
    extern function bit control_l2_flush_owner_matches_hold(
        input csr_ctrl_agent_agent_xaction tr
    );
    extern function void capture_l2_flush_level_hold(
        input csr_ctrl_agent_agent_xaction tr
    );
    extern function void clear_l2_flush_level_hold(input string reason);
    extern function bit service_control_runtime_reset_request();
endclass:csr_ctrl_agent_agent_driver

function csr_ctrl_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
    l2_flush_level_hold_valid = 1'b0;
    l2_flush_level_hold_tr = null;
    l2_flush_level_hold_uid = 0;
    l2_flush_level_hold_dynamic_epoch = 0;
    l2_flush_level_hold_action_generation = 0;
    l2_flush_level_hold_kind_code = 0;
    l2_flush_level_hold_control_reset_epoch = 0;
endfunction:new

function void csr_ctrl_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task csr_ctrl_agent_agent_driver::reset_phase(uvm_phase phase);

    clear_l2_flush_level_hold("reset_phase");
    super.reset_phase(phase);
    phase.raise_objection(this);

    repeat(2) begin
        @this.vif.drv_mp.drv_cb;
        this.drive_idle(this.cfg.drv_mode);
    end
    wait(vif.rst_n == 1'b1);
    wait(memblock_sync_pkg::reset_backend_done == 1'b1);
    repeat(20) begin
        @this.vif.drv_mp.drv_cb;
        this.drive_idle(this.cfg.drv_mode);
    end

    phase.drop_objection(this);
endtask:reset_phase

task csr_ctrl_agent_agent_driver::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //while(1) begin
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            if (service_control_runtime_reset_request()) begin
                @this.vif.drv_mp.drv_cb;
                this.drive_idle(this.cfg.drv_mode);
                continue;
            end
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                repeat(req.pre_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                @this.vif.drv_mp.drv_cb;
                this.send_pkt(req);
                repeat(req.post_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                seq_item_port.item_done();
            end
            else begin
                @this.vif.drv_mp.drv_cb;
                this.drive_idle(this.cfg.drv_mode);
            end
        end
    end
    else if (this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            @this.vif.drv_mp.drv_cb;
            `uvm_fatal(get_type_name(), $sformatf("sqr_sw==OFF & drv_sw==ON, please give a driver send task!"))
            //send task
        end
    end
endtask:main_phase

// 抽象职责：CSR driver 作为 control reset 四路 ack 的唯一 driver 写者，先清除私有
// L2 flush hold 再确认当前 epoch。返回 1 时本拍只允许驱动 idle，不能交付旧 token。
function bit csr_ctrl_agent_agent_driver::service_control_runtime_reset_request();
    int unsigned control_reset_epoch;

    if (!memblock_sync_pkg::control_csr_driver_reset_ack_needed(control_reset_epoch)) begin
        return 1'b0;
    end
    clear_l2_flush_level_hold("control_runtime_reset_request");
    memblock_sync_pkg::ack_control_csr_driver_reset(control_reset_epoch);
    return 1'b1;
endfunction:service_control_runtime_reset_request

// 抽象职责：校验一个非 DUT L2 flush metadata 是否足以唯一标识 driver hold。
// 它只判断 item 自身格式，不读取或修改 hold；ASSERT/RELEASE 的时序和 owner 对比
// 仍由 send_pkt() 负责。
function bit csr_ctrl_agent_agent_driver::control_l2_flush_metadata_complete(
    input csr_ctrl_agent_agent_xaction tr
);
    return tr != null && tr.control_l2_flush_metadata_valid &&
           tr.control_l2_flush_baseline_valid &&
           tr.control_l2_flush_control_reset_epoch != 0 &&
           (tr.control_l2_flush_action_kind ==
                csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_ASSERT ||
            tr.control_l2_flush_action_kind ==
                csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_RELEASE);
endfunction:control_l2_flush_metadata_complete

// 抽象职责：只比较 RELEASE item 与当前 driver 私有 hold 的 primitive owner 字段。
// agent 层不引用 seq 层 owner typedef，避免 package 反向依赖。
function bit csr_ctrl_agent_agent_driver::control_l2_flush_owner_matches_hold(
    input csr_ctrl_agent_agent_xaction tr
);
    return l2_flush_level_hold_valid && tr != null &&
           tr.control_l2_flush_owner_uid == l2_flush_level_hold_uid &&
           tr.control_l2_flush_owner_dynamic_epoch ==
               l2_flush_level_hold_dynamic_epoch &&
           tr.control_l2_flush_owner_action_generation ==
               l2_flush_level_hold_action_generation &&
           tr.control_l2_flush_owner_kind_code == l2_flush_level_hold_kind_code &&
           tr.control_l2_flush_control_reset_epoch ==
               l2_flush_level_hold_control_reset_epoch;
endfunction:control_l2_flush_owner_matches_hold

// 抽象职责：在 ASSERT 已经交付到 DUT interface 后深拷贝完整 CSR item，建立唯一
// high-level hold。后续 idle 只复用该固定 baseline，不创建连续 high item。
function void csr_ctrl_agent_agent_driver::capture_l2_flush_level_hold(
    input csr_ctrl_agent_agent_xaction tr
);
    if (l2_flush_level_hold_valid || tr == null ||
        !control_l2_flush_metadata_complete(tr) ||
        tr.control_l2_flush_action_kind !=
            csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_ASSERT ||
        tr.io_ooo_to_mem_csrCtrl_flush_l2_enable != 1'b1) begin
        `uvm_fatal(get_type_name(), "invalid L2 flush ASSERT while establishing driver hold")
    end
    l2_flush_level_hold_tr = csr_ctrl_agent_agent_xaction::type_id::create(
        $sformatf("l2_flush_hold_uid_%0d_gen_%0d", tr.control_l2_flush_owner_uid,
                  tr.control_l2_flush_owner_action_generation));
    if (l2_flush_level_hold_tr == null) begin
        `uvm_fatal(get_type_name(), "failed to allocate L2 flush driver hold baseline")
    end
    l2_flush_level_hold_tr.copy(tr);
    l2_flush_level_hold_tr.io_ooo_to_mem_csrCtrl_flush_l2_enable = 1'b1;
    l2_flush_level_hold_valid = 1'b1;
    l2_flush_level_hold_uid = tr.control_l2_flush_owner_uid;
    l2_flush_level_hold_dynamic_epoch = tr.control_l2_flush_owner_dynamic_epoch;
    l2_flush_level_hold_action_generation = tr.control_l2_flush_owner_action_generation;
    l2_flush_level_hold_kind_code = tr.control_l2_flush_owner_kind_code;
    l2_flush_level_hold_control_reset_epoch =
        tr.control_l2_flush_control_reset_epoch;
endfunction:capture_l2_flush_level_hold

// 抽象职责：在合法 RELEASE 或物理 reset 边界清除 driver 私有 hold。正常 RELEASE
// 先由 send_pkt() 完成 owner 校验；该 helper 本身不接受任意 sequence 直接调用。
function void csr_ctrl_agent_agent_driver::clear_l2_flush_level_hold(input string reason);
    l2_flush_level_hold_valid = 1'b0;
    l2_flush_level_hold_tr = null;
    l2_flush_level_hold_uid = 0;
    l2_flush_level_hold_dynamic_epoch = 0;
    l2_flush_level_hold_action_generation = 0;
    l2_flush_level_hold_kind_code = 0;
    l2_flush_level_hold_control_reset_epoch = 0;
endfunction:clear_l2_flush_level_hold

// 抽象职责：在 CSR driver 没有 sequence item 时保持同一 ASSERT 的完整 CSR baseline。
// 它刻意绕过 send_pkt() 的 metadata 状态迁移，防止每个 idle sample 重复建立 hold。
task csr_ctrl_agent_agent_driver::drive_l2_flush_level_hold();
    if (!l2_flush_level_hold_valid || l2_flush_level_hold_tr == null) begin
        `uvm_fatal(get_type_name(), "L2 flush hold is valid without a baseline xaction")
    end
    drive_pkt_fields(l2_flush_level_hold_tr);
endtask:drive_l2_flush_level_hold

// 抽象职责：在真实 item 边界处理 ASSERT/RELEASE 的 owner 生命周期，然后驱动完整
// CSR payload。普通 item 不能与 high hold 并发，避免两条 producer 同时维护 level。
task csr_ctrl_agent_agent_driver::send_pkt(csr_ctrl_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "cannot drive a null CSR item")
    end
    if (!tr.control_l2_flush_metadata_valid) begin
        if (tr.control_l2_flush_action_kind !=
                csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_NONE ||
            tr.control_l2_flush_baseline_valid) begin
            `uvm_fatal(get_type_name(), "CSR item has partial L2 flush metadata")
        end
        if (l2_flush_level_hold_valid) begin
            `uvm_fatal(get_type_name(), "ordinary CSR item arrived while L2 flush hold is active")
        end
        drive_pkt_fields(tr);
        return;
    end
    if (!control_l2_flush_metadata_complete(tr)) begin
        `uvm_fatal(get_type_name(), "CSR item has incomplete L2 flush metadata")
    end
    case (tr.control_l2_flush_action_kind)
        csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_ASSERT: begin
            if (l2_flush_level_hold_valid ||
                tr.io_ooo_to_mem_csrCtrl_flush_l2_enable != 1'b1) begin
                `uvm_fatal(get_type_name(), "invalid L2 flush ASSERT ownership or level")
            end
            drive_pkt_fields(tr);
            capture_l2_flush_level_hold(tr);
        end
        csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_RELEASE: begin
            if (!control_l2_flush_owner_matches_hold(tr) ||
                tr.io_ooo_to_mem_csrCtrl_flush_l2_enable != 1'b0) begin
                `uvm_fatal(get_type_name(), "L2 flush RELEASE does not match the active driver hold")
            end
            drive_pkt_fields(tr);
            clear_l2_flush_level_hold("matching_release");
        end
        default:
            `uvm_fatal(get_type_name(), "unknown L2 flush action metadata")
    endcase
endtask:send_pkt

task csr_ctrl_agent_agent_driver::drive_pkt_fields(csr_ctrl_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= tr.io_ooo_to_mem_tlbCsr_satp_mode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= tr.io_ooo_to_mem_tlbCsr_satp_asid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= tr.io_ooo_to_mem_tlbCsr_satp_ppn;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= tr.io_ooo_to_mem_tlbCsr_satp_changed;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= tr.io_ooo_to_mem_tlbCsr_vsatp_mode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= tr.io_ooo_to_mem_tlbCsr_vsatp_asid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= tr.io_ooo_to_mem_tlbCsr_vsatp_ppn;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= tr.io_ooo_to_mem_tlbCsr_vsatp_changed;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= tr.io_ooo_to_mem_tlbCsr_hgatp_mode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= tr.io_ooo_to_mem_tlbCsr_hgatp_vmid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= tr.io_ooo_to_mem_tlbCsr_hgatp_ppn;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= tr.io_ooo_to_mem_tlbCsr_hgatp_changed;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= tr.io_ooo_to_mem_tlbCsr_mbmc_BME;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= tr.io_ooo_to_mem_tlbCsr_mbmc_CMODE;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= tr.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= tr.io_ooo_to_mem_tlbCsr_mbmc_BMA;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= tr.io_ooo_to_mem_tlbCsr_priv_mxr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= tr.io_ooo_to_mem_tlbCsr_priv_sum;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= tr.io_ooo_to_mem_tlbCsr_priv_vmxr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= tr.io_ooo_to_mem_tlbCsr_priv_vsum;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= tr.io_ooo_to_mem_tlbCsr_priv_virt;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= tr.io_ooo_to_mem_tlbCsr_priv_virt_changed;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= tr.io_ooo_to_mem_tlbCsr_priv_spvp;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= tr.io_ooo_to_mem_tlbCsr_priv_imode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= tr.io_ooo_to_mem_tlbCsr_priv_dmode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= tr.io_ooo_to_mem_tlbCsr_mPBMTE;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= tr.io_ooo_to_mem_tlbCsr_hPBMTE;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= tr.io_ooo_to_mem_tlbCsr_pmm_mseccfg;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= tr.io_ooo_to_mem_tlbCsr_pmm_menvcfg;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= tr.io_ooo_to_mem_tlbCsr_pmm_henvcfg;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= tr.io_ooo_to_mem_tlbCsr_pmm_hstatus;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= tr.io_ooo_to_mem_tlbCsr_pmm_senvcfg;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= tr.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= tr.io_ooo_to_mem_csrCtrl_sbuffer_timeout;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= tr.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= tr.io_ooo_to_mem_csrCtrl_cache_error_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= tr.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= tr.io_ooo_to_mem_csrCtrl_power_down_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= tr.io_ooo_to_mem_csrCtrl_flush_l2_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= tr.io_ooo_to_mem_csrCtrl_fsIsOff;

    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable <= tr.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode <= tr.io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable;
    vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode <= tr.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode;
    vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= tr.io_ooo_to_mem_tlbCsr_priv_debug;

endtask:drive_pkt_fields

task csr_ctrl_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if (l2_flush_level_hold_valid) begin
        drive_l2_flush_level_hold();
        return;
    end

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_debugMode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= 1'b1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= 1'b1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= memblock_sync_pkg::dispatch_real_smoke_active ? 2'd3 : '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= memblock_sync_pkg::dispatch_real_smoke_active ? 2'd3 : '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_debug <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_asid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_satp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_asid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_vsatp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_mode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_vmid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_ppn <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hgatp_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BME <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_CMODE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mbmc_BMA <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_mxr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_sum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vmxr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_vsum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_virt_changed <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_spvp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_imode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_priv_dmode <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_mPBMTE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_hPBMTE <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_mseccfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_menvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_henvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_hstatus <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_tlbCsr_pmm_senvcfg <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1I_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_sbuffer_timeout <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_cache_error_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_power_down_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_flush_l2_enable <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_tEnableVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_frontend_trigger_triggerCanRaiseBpExp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_csrCtrl_fsIsOff <= '0;

    end

endtask:drive_idle

`endif
