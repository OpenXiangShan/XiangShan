`ifndef MEMBLOCK_DTLB_ALLSTAGE_MXR_LIVE_VSEQ_SV
`define MEMBLOCK_DTLB_ALLSTAGE_MXR_LIVE_VSEQ_SV

// 用真实 AllStage LD 请求、表项和 GPA 续查检查动态 HS/VS MXR。
// 本地回包只消费真实 DTLB 请求；预期独立计算，以免 RM 的 MXR 偏差污染 DUT 判断。
class memblock_dtlb_allstage_mxr_live_vseq extends memblock_dtlb_0401_0402_vseq;
    `uvm_object_utils(memblock_dtlb_allstage_mxr_live_vseq)

    localparam bit [49:0] VA = 50'h80000128;
    localparam bit [47:0] GPA = 48'h81230128;
    localparam bit [47:0] PA = 48'h90000128;
    localparam int KEEP_STORAGE = 0;
    localparam int CHANGE_BEFORE_RETURN = 1;
    localparam int CHANGE_AFTER_RETURN = 2;
    localparam int CHANGE_BEFORE_BYPASS = 3;

    // 每个窗口记录真实查询、普通回填和 GPA 回包。窗口结束时必须已消费所有 token。
    int unsigned windows, samples, ordinary_returns, gpa_returns;
    int unsigned pf_samples, gpf_samples, allowed_samples, bypass_samples, reused_gpa_samples;
    // 待返回 token 的请求种类，仅在 token_valid 为 1 时有效。
    bit pending_get_gpa;

    function new(string name = "memblock_dtlb_allstage_mxr_live_vseq");
        super.new(name);
    endfunction

    // 仅清除 HDL 信号实际位宽之外的位，保留真实的 X/Z。
    task expect_signal(input string label_text, input string path, input int width, input uvm_hdl_data_t expected);
        uvm_hdl_data_t observed;
        observed = read_hdl(path, width);
        if (observed !== expected)
            `uvm_error("DTLB_MXR_SIGNAL", $sformatf("%s %s expected=%h observed=%h",
                                                    label_text, path, expected[63:0], observed[63:0]))
    endtask

    // 预期由测试指定的 CSR 值和叶项权限得出。VS 拒绝时优先报告 VS 页错误。
    function automatic logic [5:0] expected_fault(input bit vs_execute_only,
                                                  input bit g_execute_only,
                                                  input bit [1:0] mxr_pair);
        if (vs_execute_only && mxr_pair == 2'b00) return 6'h01;
        if (g_execute_only && !mxr_pair[1]) return 6'h04;
        return 6'h00;
    endfunction

    // 只改变 MXR 和 VMXR，在新查询发出前等待两个寄存级并核对有效值。
    task set_mxr(input bit [1:0] mxr_pair);
        string unit_path;
        unit_path = unit_name(0);
        drive({unit_path, ".io_csr_priv_mxr"}, mxr_pair[1]);
        drive({unit_path, ".io_csr_priv_vmxr"}, mxr_pair[0]);
        cycles(3);
        expect_signal("HS_MXR", {unit_path, "._csr_delay_io_out_priv_mxr"}, 1, mxr_pair[1]);
        expect_signal("VS_MXR", {unit_path, "._csr_delay_io_out_priv_vmxr"}, 1, mxr_pair[0]);
        expect_signal("NO_SFENCE", {unit_path, "._sfence_delay_io_out_valid"}, 1, 0);
        expect_signal("NO_REDIRECT", {unit_path, ".io_redirect_valid"}, 1, 0);
    endtask

    // 回包携带合法原始叶权限。VA 与 GPA 不同，用实际 getGpa 凭据返回最终 GPA。
    task stage_return(input bit vs_execute_only, input bit g_execute_only, input bit get_gpa);
        string unit_path;
        unit_path = unit_name(0);
        if (!token_valid || pending_get_gpa != get_gpa || token_stage != 3 || token_vpn != VA[49:12])
            `uvm_fatal("DTLB_MXR_TOKEN", "missing request or mismatched VPN, stage, getGpa")
        // VS 的 U=0，G 的 U=1。两个叶项均 A=1、W=0，R-only 为 09，X-only 为 0c。
        return_fields(0, VA, PA, vs_execute_only ? 6'h0c : 6'h09, 0,
                      g_execute_only ? 5'h0c : 5'h09, 0);
        drive({unit_path, ".io_ptw_resp_bits_getGpa"}, get_gpa);
        drive({unit_path, ".io_ptw_resp_bits_s1_entry_ppn"}, GPA[47:15]);
        drive({unit_path, ".io_ptw_resp_bits_s2_entry_tag"}, GPA[47:12]);
        for (int sector = 0; sector < 8; sector++) begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d", unit_path, sector), sector == int'(VA[14:12]));
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d", unit_path, sector), sector == int'(VA[14:12]));
            drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d", unit_path, sector), GPA[14:12]);
        end
        drive({unit_path, ".io_ptw_resp_valid"}, 1);
        #1ps;
        expect_signal("REFILL_WRITE", {unit_path, ".entries.io_w_valid"}, 1, !get_gpa);
    endtask

    task finish_return(input bit get_gpa);
        drive({unit_name(0), ".io_ptw_resp_valid"}, 0);
        drive({unit_name(0), ".io_ptw_resp_bits_getGpa"}, 0);
        token_valid = 0;
        if (get_gpa) gpa_returns++;
        else ordinary_returns++;
    endtask

    // 观察真实 LD 响应。中间 miss 只检查续查请求，最终响应才核对异常或 PA。
    task sample_ld(input string label_text, input int port_id, input bit [8:0] rob,
                   input bit [1:0] mxr_pair, input logic [5:0] fault,
                   input bit miss, input bit bypass, input bit capture_ptw, input bit get_gpa);
        string unit_path, response, ptw_req;
        logic [5:0] actual;
        unit_path = unit_name(0);
        response = $sformatf("%s.io_requestor_%0d_resp", unit_path, port_id);
        ptw_req = $sformatf("%s.io_ptw_req_%0d", unit_path, port_id);
        expect_signal(label_text, {response, "_valid"}, 1, 1);
        expect_signal(label_text, {indexed(unit_path, "req_out", port_id), "_vaddr"}, 50, VA);
        expect_signal(label_text, {indexed(unit_path, "req_out", port_id), "_debug_robIdx_value"}, 8, rob[7:0]);
        expect_signal(label_text, {response, "_bits_miss"}, 1, miss);
        expect_signal(label_text, {unit_path, ".readResult_p_hit_last_REG", port_id == 0 ? "" : $sformatf("_%0d", port_id)}, 1, bypass);
        if (!bypass)
            expect_signal(label_text, $sformatf("%s._entries_io_r_resp_%0d_bits_hit", unit_path, port_id),
                     1, !miss || get_gpa);
        expect_signal(label_text, {ptw_req, "_valid"}, 1, capture_ptw);
        if (capture_ptw) begin
            if (token_valid || read_hdl({ptw_req, "_valid"}) !== uvm_hdl_data_t'(1) ||
                read_hdl({ptw_req, "_bits_vpn"}, 38) !== uvm_hdl_data_t'(VA[49:12]) ||
                read_hdl({ptw_req, "_bits_s2xlate"}, 2) !== uvm_hdl_data_t'(3) ||
                read_hdl({ptw_req, "_bits_getGpa"}, 1) !== uvm_hdl_data_t'(get_gpa))
                `uvm_fatal("DTLB_MXR_TOKEN", $sformatf("%s unexpected or missing PTW request", label_text))
            token_valid = 1;
            token_group = 0;
            token_vpn = VA[49:12];
            token_stage = 3;
            pending_get_gpa = get_gpa;
        end
        actual = actual_exception(response);
        if (!miss) begin
            samples++;
            if (bypass) bypass_samples++;
            case (fault)
                6'h00: allowed_samples++;
                6'h01: pf_samples++;
                6'h04: gpf_samples++;
            endcase
            if (actual !== fault)
                `uvm_error("DTLB_MXR_FAULT", $sformatf("%s HS/VS=%b got=%h expected=%h",
                                                       label_text, mxr_pair, actual, fault))
            if (fault == 0) expect_signal(label_text, {response, "_bits_paddr_0"}, 48, PA);
            if (fault == 6'h04) expect_signal(label_text, {response, "_bits_gpaddr_0"}, 64, GPA);
        end
        `uvm_info("DTLB_MXR_SAMPLE", $sformatf("%s port=%0d HS/VS=%b miss=%b bypass=%b getGpa=%b expected=%h actual=%h",
                                               label_text, port_id, mxr_pair, miss, bypass, get_gpa, fault, actual), UVM_LOW)
    endtask

    // 观察对应查询返回。普通回包旁路和 GPA 回包使用各自从真实输出捕获的 token。
    task issue_ld(input string label_text, input int port_id, input bit [8:0] rob,
                  input bit [1:0] mxr_pair, input logic [5:0] fault,
                  input bit miss = 0, input bit bypass = 0,
                  input bit capture_ptw = 0, input bit get_gpa = 0);
        string request;
        request = $sformatf("%s.io_requestor_%0d_req", unit_name(0), port_id);
        drive({request, "_bits_debug_robIdx_flag"}, rob[8]);
        drive({request, "_bits_debug_robIdx_value"}, rob[7:0]);
        drive({unit_name(0), ".io_robPendingPtr_value"}, rob[7:0]);
        target_request(0, port_id, VA, 0, 0, 0);
        @(posedge clock_vif.clk);
        @(negedge clock_vif.clk);
        drive({request, "_valid"}, 0);
        if (bypass) finish_return(0);
        @(posedge clock_vif.clk);
        sample_ld(label_text, port_id, rob, mxr_pair, fault, miss, bypass, capture_ptw, get_gpa);
        @(negedge clock_vif.clk);
    endtask

    // 当 storage 命中但 G 权限失败，必须完成真实 getGpa 续查并重查至 GPF 终态。
    task check_cached(input string label_text, input int port_id, input bit [8:0] rob,
                      input bit vs_execute_only, input bit g_execute_only, input bit [1:0] mxr_pair,
                      ref bit saved_gpa);
        logic [5:0] fault;
        fault = expected_fault(vs_execute_only, g_execute_only, mxr_pair);
        if (fault == 6'h04) begin
            if (saved_gpa) begin
                // 页和翻译上下文持续不变，后续 GPF 使用本窗口已取得的 GPA。
                reused_gpa_samples++;
            end else begin
                issue_ld({label_text, "/gpa_request"}, port_id, rob, mxr_pair, fault, 1, 0, 1, 1);
                expect_signal(label_text, {unit_name(0), ".need_gpa"}, 1, 1);
                expect_signal(label_text, {unit_name(0), ".need_gpa_vpn"}, 38, VA[49:12]);
                expect_signal(label_text, {unit_name(0), ".need_gpa_s2xlate"}, 2, 3);
                expect_signal(label_text, {unit_name(0), ".need_gpa_robidx_value"}, 8, rob[7:0]);
                stage_return(vs_execute_only, g_execute_only, 1);
                cycles(1);
                finish_return(1);
                expect_signal(label_text, {unit_name(0), ".resp_gpa_refill"}, 1, 1);
                saved_gpa = 1;
                cycles(2);
            end
        end
        issue_ld(label_text, port_id, rob, mxr_pair, fault);
        expect_signal(label_text, {unit_name(0), ".need_gpa"}, 1, 0);
    endtask

    // 同一映射既验证四个状态，也遍历只改变一位的全部八条有向转换。
    task run_window(input string label_text, input int port_id, input bit vs_execute_only,
                    input bit g_execute_only, input int timing_case, input bit [1:0] old_mxr,
                    input bit [1:0] new_mxr);
        bit [1:0] transitions[9];
        bit [1:0] mxr_pair;
        bit [8:0] rob;
        // 窗口初始没有 GPA，第一次续查回包后，同一 VPN 可复用已取得的 GPA。
        bit saved_gpa;
        saved_gpa = 0;
        transitions = '{2'b00, 2'b01, 2'b11, 2'b10, 2'b00, 2'b10, 2'b11, 2'b01, 2'b00};
        configure_context(0, 3);
        drive({unit_name(0), ".io_robPendingPtr_flag"}, 0);
        drive({unit_name(0), ".io_csr_priv_vsum"}, 0);
        set_mxr(old_mxr);
        rob = 9'h012;
        issue_ld({label_text, "/cold"}, port_id, rob, old_mxr, 0, 1, 0, 1, 0);
        cycles(2);
        if (timing_case == CHANGE_BEFORE_RETURN || timing_case == CHANGE_BEFORE_BYPASS)
            set_mxr(new_mxr);
        stage_return(vs_execute_only, g_execute_only, 0);
        if (timing_case == CHANGE_BEFORE_BYPASS) begin
            // 旧 miss 的走表完成和新 CSR 下的独立查询同拍，检查匹配回包旁路。
            issue_ld({label_text, "/bypass"}, port_id, rob + 1, new_mxr,
                     expected_fault(vs_execute_only, g_execute_only, new_mxr), 0, 1);
        end else begin
            cycles(1);
            finish_return(0);
        end
        cycles(3);
        if (timing_case == CHANGE_AFTER_RETURN) set_mxr(new_mxr);

        if (timing_case == KEEP_STORAGE) begin
            foreach (transitions[step]) begin
                mxr_pair = transitions[step];
                set_mxr(mxr_pair);
                check_cached($sformatf("%s/step%0d", label_text, step), port_id, rob + 9'(step + 1),
                             vs_execute_only, g_execute_only, mxr_pair, saved_gpa);
            end
        end else begin
            check_cached({label_text, "/first"}, port_id, rob + 2, vs_execute_only, g_execute_only, new_mxr, saved_gpa);
            set_mxr(old_mxr);
            check_cached({label_text, "/reverse"}, port_id, rob + 3, vs_execute_only, g_execute_only, old_mxr, saved_gpa);
        end
        if (token_valid)
            `uvm_fatal("DTLB_MXR_TOKEN", "window ended with an outstanding request")
        windows++;
        `uvm_info("DTLB_MXR_WINDOW", $sformatf("%s port=%0d VS_Xonly=%b G_Xonly=%b timing=%0d",
                                               label_text, port_id, vs_execute_only, g_execute_only, timing_case), UVM_LOW)
    endtask

    virtual task body();
        require_virtual_sqr();
        seq_csr_common::init();
        if (!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(
                null, "uvm_test_top.env.u_lintsissue_agent_agent", "vif", clock_vif))
            `uvm_fatal("DTLB_MXR_CLOCK", "missing real issue clock")
        if (seq_csr_common::get_l2tlb_seq_en() || memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)
            `uvm_fatal("DTLB_MXR_OWNER", "local fixture requires formal responder disabled")
        wait (memblock_sync_pkg::reset_backend_done === 1'b1);
        cycles(48);
        `include "dtlb_0501_0504_input_defaults.svh"
        drive({DUT, ".inner_dtlbRepeater.reset"}, 1);
        initialize_lookup_ports();
        for (int port_id = 0; port_id < 2; port_id++) begin
            run_window("hot_read_read", port_id, 0, 0, KEEP_STORAGE, 0, 0);
            run_window("hot_xonly_read", port_id, 1, 0, KEEP_STORAGE, 0, 0);
            run_window("hot_read_xonly", port_id, 0, 1, KEEP_STORAGE, 0, 0);
            run_window("hot_xonly_xonly", port_id, 1, 1, KEEP_STORAGE, 0, 0);
            run_window("cold_before_return_vmxr_on", port_id, 1, 0, CHANGE_BEFORE_RETURN, 2'b00, 2'b01);
            run_window("cold_after_return_mxr_on", port_id, 1, 1, CHANGE_AFTER_RETURN, 2'b00, 2'b10);
            run_window("cold_bypass_vmxr_off", port_id, 1, 0, CHANGE_BEFORE_BYPASS, 2'b01, 2'b00);
            run_window("cold_bypass_mxr_off", port_id, 0, 1, CHANGE_BEFORE_BYPASS, 2'b11, 2'b01);
        end
        if (windows != 16 || ordinary_returns != 16 || samples != 92 || bypass_samples != 4 ||
            gpa_returns != 6 || reused_gpa_samples != 10 ||
            pf_samples != 20 || gpf_samples != 18 || allowed_samples != 54 ||
            initialization_checks != 8)
            `uvm_error("DTLB_MXR_INCOMPLETE", "window, return, sample or result counts differ from the directed matrix")
        cleanup();
        `uvm_info("DTLB_MXR_COMPLETE", $sformatf("windows=%0d samples=%0d normal_returns=%0d gpa_returns=%0d reused_gpa=%0d bypass=%0d pass=%0d pf=%0d gpf=%0d initialization=%0d",
                                                 windows, samples, ordinary_returns, gpa_returns, reused_gpa_samples,
                                                 bypass_samples, allowed_samples, pf_samples, gpf_samples, initialization_checks), UVM_LOW)
    endtask
endclass
`endif
