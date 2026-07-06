//=========================================================
//File name    : memblock_sync_pkg.sv
//Author       : OpenAI_Codex
//Module name  : memblock_sync_pkg
//Discribution : shared sync state for memblock tb/uvm
//Date         : 2026-04-14
//=========================================================
`ifndef MEMBLOCK_SYNC_PKG__SV
`define MEMBLOCK_SYNC_PKG__SV

package memblock_sync_pkg;
    bit reset_backend_done = 1'b0;
    bit dispatch_flush_in_progress = 1'b0;
    bit dispatch_monitor_capture_en = 1'b0;
    bit l2tlb_responder_active = 1'b0;
    bit dispatch_real_smoke_active = 1'b0;
    bit dispatch_flushsb_waiting_empty = 1'b0;
    int unsigned dispatch_flush_epoch = 0;
    longint unsigned dispatch_service_cycle = 0;

    typedef struct {
        bit               valid;
        int unsigned      port_id;
        bit               rob_valid;
        bit               rob_flag;
        bit [8:0]         rob_value;
        bit               lq_valid;
        bit               lq_flag;
        bit [6:0]         lq_value;
        bit               sq_valid;
        bit               sq_flag;
        bit [5:0]         sq_value;
        bit [23:0]        exception_vec;
        longint unsigned  cycle;
    } dispatch_raw_int_wb_t;

    typedef struct {
        bit               valid;
        int unsigned      port_id;
        bit               is_sta;
        bit               is_std;
        bit               rob_valid;
        bit               rob_flag;
        bit [8:0]         rob_value;
        bit               lq_valid;
        bit               lq_flag;
        bit [6:0]         lq_value;
        bit               sq_valid;
        bit               sq_flag;
        bit [5:0]         sq_value;
        bit               hit;
        bit               flush_state;
        bit [3:0]         source_type;
        bit               vector_feedback;
        longint unsigned  cycle;
    } dispatch_raw_iq_feedback_t;

    typedef struct {
        bit               valid;
        bit [3:0]         lq_deq;
        bit [1:0]         sq_deq;
        bit               lq_deq_ptr_flag;
        bit [6:0]         lq_deq_ptr_value;
        bit               sq_deq_ptr_flag;
        bit [5:0]         sq_deq_ptr_value;
        bit               memory_violation_valid;
        bit               memory_violation_rob_valid;
        bit               memory_violation_rob_flag;
        bit [8:0]         memory_violation_rob_value;
        bit [49:0]        memory_violation_target;
        bit               memory_violation_level;
        bit               sb_is_empty;
        longint unsigned  cycle;
    } dispatch_raw_ctrl_t;

    typedef struct {
        bit               valid;
        bit [3:0]         satp_mode;
        bit [15:0]        satp_asid;
        bit [43:0]        satp_ppn;
        bit               satp_changed;
        bit [3:0]         vsatp_mode;
        bit [15:0]        vsatp_asid;
        bit [43:0]        vsatp_ppn;
        bit               vsatp_changed;
        bit [3:0]         hgatp_mode;
        bit [15:0]        hgatp_vmid;
        bit [43:0]        hgatp_ppn;
        bit               hgatp_changed;
        bit               priv_mxr;
        bit               priv_sum;
        bit               priv_vmxr;
        bit               priv_vsum;
        bit               priv_virt;
        bit               priv_virt_changed;
        bit               priv_spvp;
        bit [1:0]         priv_imode;
        bit [1:0]         priv_dmode;
        bit               m_pbmt_en;
        bit               h_pbmt_en;
        longint unsigned  cycle;
    } dispatch_raw_csr_t;

    typedef struct {
        bit               valid;
        bit               rs1;
        bit               rs2;
        bit [49:0]        addr;
        bit [15:0]        id;
        bit               hv;
        bit               hg;
        longint unsigned  cycle;
    } dispatch_raw_sfence_t;

    dispatch_raw_int_wb_t      raw_int_wb_q[$];
    dispatch_raw_iq_feedback_t raw_iq_feedback_q[$];
    dispatch_raw_ctrl_t        raw_ctrl_q[$];
    dispatch_raw_sfence_t      raw_sfence_q[$];
    dispatch_raw_csr_t         latest_raw_csr;
    bit                        latest_raw_csr_valid;
    int unsigned               latest_raw_csr_seq;

    function dispatch_raw_int_wb_t make_empty_raw_int_wb();
        dispatch_raw_int_wb_t item;
        item.valid         = 1'b0;
        item.port_id       = 0;
        item.rob_valid     = 1'b0;
        item.rob_flag      = 1'b0;
        item.rob_value     = '0;
        item.lq_valid      = 1'b0;
        item.lq_flag       = 1'b0;
        item.lq_value      = '0;
        item.sq_valid      = 1'b0;
        item.sq_flag       = 1'b0;
        item.sq_value      = '0;
        item.exception_vec = '0;
        item.cycle         = 0;
        return item;
    endfunction:make_empty_raw_int_wb

    function dispatch_raw_iq_feedback_t make_empty_raw_iq_feedback();
        dispatch_raw_iq_feedback_t item;
        item.valid           = 1'b0;
        item.port_id         = 0;
        item.is_sta          = 1'b0;
        item.is_std          = 1'b0;
        item.rob_valid       = 1'b0;
        item.rob_flag        = 1'b0;
        item.rob_value       = '0;
        item.lq_valid        = 1'b0;
        item.lq_flag         = 1'b0;
        item.lq_value        = '0;
        item.sq_valid        = 1'b0;
        item.sq_flag         = 1'b0;
        item.sq_value        = '0;
        item.hit             = 1'b0;
        item.flush_state     = 1'b0;
        item.source_type     = '0;
        item.vector_feedback = 1'b0;
        item.cycle           = 0;
        return item;
    endfunction:make_empty_raw_iq_feedback

    function dispatch_raw_ctrl_t make_empty_raw_ctrl();
        dispatch_raw_ctrl_t item;
        item.valid                      = 1'b0;
        item.lq_deq                     = '0;
        item.sq_deq                     = '0;
        item.lq_deq_ptr_flag            = 1'b0;
        item.lq_deq_ptr_value           = '0;
        item.sq_deq_ptr_flag            = 1'b0;
        item.sq_deq_ptr_value           = '0;
        item.memory_violation_valid     = 1'b0;
        item.memory_violation_rob_valid = 1'b0;
        item.memory_violation_rob_flag  = 1'b0;
        item.memory_violation_rob_value = '0;
        item.memory_violation_target    = '0;
        item.memory_violation_level     = 1'b0;
        item.sb_is_empty                 = 1'b0;
        item.cycle                      = 0;
        return item;
    endfunction:make_empty_raw_ctrl

    function dispatch_raw_csr_t make_empty_raw_csr();
        dispatch_raw_csr_t item;
        item.valid             = 1'b0;
        item.satp_mode         = '0;
        item.satp_asid         = '0;
        item.satp_ppn          = '0;
        item.satp_changed      = 1'b0;
        item.vsatp_mode        = '0;
        item.vsatp_asid        = '0;
        item.vsatp_ppn         = '0;
        item.vsatp_changed     = 1'b0;
        item.hgatp_mode        = '0;
        item.hgatp_vmid        = '0;
        item.hgatp_ppn         = '0;
        item.hgatp_changed     = 1'b0;
        item.priv_mxr          = 1'b0;
        item.priv_sum          = 1'b0;
        item.priv_vmxr         = 1'b0;
        item.priv_vsum         = 1'b0;
        item.priv_virt         = 1'b0;
        item.priv_virt_changed = 1'b0;
        item.priv_spvp         = 1'b0;
        item.priv_imode        = '0;
        item.priv_dmode        = '0;
        item.m_pbmt_en         = 1'b0;
        item.h_pbmt_en         = 1'b0;
        item.cycle             = 0;
        return item;
    endfunction:make_empty_raw_csr

    function dispatch_raw_sfence_t make_empty_raw_sfence();
        dispatch_raw_sfence_t item;
        item.valid = 1'b0;
        item.rs1   = 1'b0;
        item.rs2   = 1'b0;
        item.addr  = '0;
        item.id    = '0;
        item.hv    = 1'b0;
        item.hg    = 1'b0;
        item.cycle = 0;
        return item;
    endfunction:make_empty_raw_sfence

    function bit raw_csr_payload_changed(input dispatch_raw_csr_t prev,
                                         input dispatch_raw_csr_t cur);
        return
            prev.satp_mode         != cur.satp_mode         ||
            prev.satp_asid         != cur.satp_asid         ||
            prev.satp_ppn          != cur.satp_ppn          ||
            prev.vsatp_mode        != cur.vsatp_mode        ||
            prev.vsatp_asid        != cur.vsatp_asid        ||
            prev.vsatp_ppn         != cur.vsatp_ppn         ||
            prev.hgatp_mode        != cur.hgatp_mode        ||
            prev.hgatp_vmid        != cur.hgatp_vmid        ||
            prev.hgatp_ppn         != cur.hgatp_ppn         ||
            prev.priv_mxr          != cur.priv_mxr          ||
            prev.priv_sum          != cur.priv_sum          ||
            prev.priv_vmxr         != cur.priv_vmxr         ||
            prev.priv_vsum         != cur.priv_vsum         ||
            prev.priv_virt         != cur.priv_virt         ||
            prev.priv_spvp         != cur.priv_spvp         ||
            prev.priv_imode        != cur.priv_imode        ||
            prev.priv_dmode        != cur.priv_dmode        ||
            prev.m_pbmt_en         != cur.m_pbmt_en         ||
            prev.h_pbmt_en         != cur.h_pbmt_en         ||
            (cur.satp_changed      && !prev.satp_changed)   ||
            (cur.vsatp_changed     && !prev.vsatp_changed)  ||
            (cur.hgatp_changed     && !prev.hgatp_changed)  ||
            (cur.priv_virt_changed && !prev.priv_virt_changed);
    endfunction:raw_csr_payload_changed

    function void push_raw_int_wb(input dispatch_raw_int_wb_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            raw_int_wb_q.push_back(item);
        end
    endfunction:push_raw_int_wb

    function bit pop_raw_int_wb(output dispatch_raw_int_wb_t item);
        if (raw_int_wb_q.size() == 0) begin
            item = make_empty_raw_int_wb();
            return 1'b0;
        end
        item = raw_int_wb_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_int_wb

    function void push_raw_iq_feedback(input dispatch_raw_iq_feedback_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            raw_iq_feedback_q.push_back(item);
        end
    endfunction:push_raw_iq_feedback

    function bit pop_raw_iq_feedback(output dispatch_raw_iq_feedback_t item);
        if (raw_iq_feedback_q.size() == 0) begin
            item = make_empty_raw_iq_feedback();
            return 1'b0;
        end
        item = raw_iq_feedback_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_iq_feedback

    function void push_raw_ctrl(input dispatch_raw_ctrl_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            raw_ctrl_q.push_back(item);
        end
    endfunction:push_raw_ctrl

    function bit pop_raw_ctrl(output dispatch_raw_ctrl_t item);
        if (raw_ctrl_q.size() == 0) begin
            item = make_empty_raw_ctrl();
            return 1'b0;
        end
        item = raw_ctrl_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_ctrl

    function void push_raw_csr(input dispatch_raw_csr_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            latest_raw_csr = item;
            latest_raw_csr_valid = 1'b1;
            latest_raw_csr_seq++;
        end
    endfunction:push_raw_csr

    function bit get_latest_raw_csr(output dispatch_raw_csr_t item,
                                    output int unsigned seq);
        seq = latest_raw_csr_seq;
        if (!latest_raw_csr_valid) begin
            item = make_empty_raw_csr();
            return 1'b0;
        end
        item = latest_raw_csr;
        return 1'b1;
    endfunction:get_latest_raw_csr

    function void push_raw_sfence(input dispatch_raw_sfence_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            raw_sfence_q.push_back(item);
        end
    endfunction:push_raw_sfence

    function bit pop_raw_sfence(output dispatch_raw_sfence_t item);
        if (raw_sfence_q.size() == 0) begin
            item = make_empty_raw_sfence();
            return 1'b0;
        end
        item = raw_sfence_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_sfence

    function void clear_raw_monitor_queues();
        raw_int_wb_q.delete();
        raw_iq_feedback_q.delete();
        raw_ctrl_q.delete();
        raw_sfence_q.delete();
        latest_raw_csr = make_empty_raw_csr();
        latest_raw_csr_valid = 1'b0;
        latest_raw_csr_seq = 0;
        dispatch_service_cycle = 0;
    endfunction:clear_raw_monitor_queues

    function void tick_dispatch_service_cycle();
        dispatch_service_cycle++;
    endfunction:tick_dispatch_service_cycle

    function longint unsigned get_dispatch_service_cycle();
        return dispatch_service_cycle;
    endfunction:get_dispatch_service_cycle

    function int unsigned raw_monitor_queue_size();
        return raw_int_wb_q.size() +
               raw_iq_feedback_q.size() +
               raw_ctrl_q.size() +
               raw_sfence_q.size();
    endfunction:raw_monitor_queue_size
endpackage

`endif
