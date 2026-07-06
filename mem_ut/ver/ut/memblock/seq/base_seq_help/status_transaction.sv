//=========================================================
//File name    : status_transaction.sv
//Author       : OpenAI_Codex
//Module name  : status_transaction
//Discribution : dispatch framework per-uid runtime status
//Date         : 2026-05-18
//=========================================================
`ifndef STATUS_TRANSACTION__SV
`define STATUS_TRANSACTION__SV

class status_transaction extends uvm_object;

    memblock_uid_t uid;

    bit active;
    bit enq;
    bit issue_ready;
    bit tlb_mapped;
    bit queued_load;
    bit queued_sta;
    bit queued_std;
    bit load_dispatched;
    bit sta_dispatched;
    bit std_dispatched;
    bit writeback;
    bit pass;
    bit fault;
    bit load_writeback;
    bit sta_writeback;
    bit std_writeback;
    // 中文注释：各 target 的 IssueQueue feedback success 状态。
    // 置位：IQ feedback hit 或 issue-accept 兼容路径经 mark_issue_feedback_success() 通过 epoch/replay 检查后置位。
    // 清零：status reset、redirect/replay 清理对应 target 发射结果时清零。为1只说明 issue response finalSuccess，不代表真实 writeback/pass。
    bit load_issue_feedback_success;
    bit sta_issue_feedback_success;
    bit std_issue_feedback_success;
    bit load_pass;
    bit sta_pass;
    bit std_pass;
    bit load_fault;
    bit sta_fault;
    bit std_fault;
    bit exception_pending;
    bit replay_pending;
    bit replay_target_load;
    bit replay_target_sta;
    bit replay_target_std;
    bit redirect_pending;
    bit flushed;
    bit rob_commit;
    bit lsq_deq;
    bit success;
    bit terminal_done;
    bit active_lq_mapped;
    bit active_sq_mapped;

    bit                 robIdx_flag;
    bit [8:0]           robIdx_value;
    bit                 lqIdx_flag;
    bit [6:0]           lqIdx_value;
    bit                 sqIdx_flag;
    bit [5:0]           sqIdx_value;
    int unsigned        load_issue_epoch;
    int unsigned        sta_issue_epoch;
    int unsigned        std_issue_epoch;
    // 同一uid被redirect reissue后产生新动态实例；递增后可区分旧实例事件。
    int unsigned        dynamic_epoch;
    int unsigned        replay_seq;
    bit                 issue_killed;
    bit [23:0]          exception_vec;
    bit [63:0]          exception_vaddr;
    bit [63:0]          exception_gpaddr;
    longint unsigned    last_event_cycle;

    `uvm_object_utils(status_transaction)

    function new(string name = "status_transaction");
        super.new(name);
        reset(0);
    endfunction:new

    function void reset(input memblock_uid_t uid_i);
        uid               = uid_i;
        active            = 1'b0;
        enq               = 1'b0;
        issue_ready       = 1'b0;
        tlb_mapped        = 1'b0;
        queued_load       = 1'b0;
        queued_sta        = 1'b0;
        queued_std        = 1'b0;
        load_dispatched   = 1'b0;
        sta_dispatched    = 1'b0;
        std_dispatched    = 1'b0;
        writeback         = 1'b0;
        pass              = 1'b0;
        fault             = 1'b0;
        load_writeback    = 1'b0;
        sta_writeback     = 1'b0;
        std_writeback     = 1'b0;
        load_issue_feedback_success = 1'b0;
        sta_issue_feedback_success = 1'b0;
        std_issue_feedback_success = 1'b0;
        load_pass         = 1'b0;
        sta_pass          = 1'b0;
        std_pass          = 1'b0;
        load_fault        = 1'b0;
        sta_fault         = 1'b0;
        std_fault         = 1'b0;
        exception_pending = 1'b0;
        replay_pending    = 1'b0;
        replay_target_load = 1'b0;
        replay_target_sta = 1'b0;
        replay_target_std = 1'b0;
        redirect_pending  = 1'b0;
        flushed           = 1'b0;
        rob_commit        = 1'b0;
        lsq_deq           = 1'b0;
        success           = 1'b0;
        terminal_done     = 1'b0;
        active_lq_mapped  = 1'b0;
        active_sq_mapped  = 1'b0;
        robIdx_flag       = 1'b0;
        robIdx_value      = '0;
        lqIdx_flag        = 1'b0;
        lqIdx_value       = '0;
        sqIdx_flag        = 1'b0;
        sqIdx_value       = '0;
        load_issue_epoch  = 0;
        sta_issue_epoch   = 0;
        std_issue_epoch   = 0;
        dynamic_epoch     = 0;
        replay_seq        = 0;
        issue_killed      = 1'b0;
        exception_vec     = '0;
        exception_vaddr   = '0;
        exception_gpaddr  = '0;
        last_event_cycle  = 0;
    endfunction:reset

    function void snapshot_from_main(input main_control_transaction tr);
        if (tr == null) begin
            `uvm_fatal("STATUS_TR", "snapshot_from_main got null transaction")
        end
        uid          = tr.uid;
        robIdx_flag  = tr.robIdx_flag;
        robIdx_value = tr.robIdx_value;
        lqIdx_flag   = tr.lqIdx_flag;
        lqIdx_value  = tr.lqIdx_value;
        sqIdx_flag   = tr.sqIdx_flag;
        sqIdx_value  = tr.sqIdx_value;
    endfunction:snapshot_from_main

    function memblock_rob_key_t get_rob_key();
        memblock_rob_key_t key;
        key.flag  = robIdx_flag;
        key.value = robIdx_value;
        return key;
    endfunction:get_rob_key

    function int unsigned get_target_issue_epoch(input memblock_issue_target_e target);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return load_issue_epoch;
            MEMBLOCK_ISSUE_TARGET_STA:  return sta_issue_epoch;
            MEMBLOCK_ISSUE_TARGET_STD:  return std_issue_epoch;
            default: begin
                `uvm_fatal("STATUS_TR", $sformatf("get_target_issue_epoch got target=%0d", target))
            end
        endcase
        return 0;
    endfunction:get_target_issue_epoch

    function void set_target_issue_epoch(input memblock_issue_target_e target,
                                         input int unsigned issue_epoch_i);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: load_issue_epoch = issue_epoch_i;
            MEMBLOCK_ISSUE_TARGET_STA:  sta_issue_epoch  = issue_epoch_i;
            MEMBLOCK_ISSUE_TARGET_STD:  std_issue_epoch  = issue_epoch_i;
            default: begin
                `uvm_fatal("STATUS_TR", $sformatf("set_target_issue_epoch got target=%0d", target))
            end
        endcase
    endfunction:set_target_issue_epoch

endclass:status_transaction

`endif
