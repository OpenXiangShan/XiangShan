`ifndef MEMBLOCK_DTLB_0101_SOURCE_VSEQ__SV
`define MEMBLOCK_DTLB_0101_SOURCE_VSEQ__SV

// 抽象职责：定向验证真实来源模块到 DTLB 一拍响应的请求级合同。
// 不启动 scalar dispatch，不生成虚假事务终态，不驱动 DUT 响应或覆盖模型。
class memblock_dtlb_0101_source_vseq extends virtual_base_sequence;
    `uvm_object_utils(memblock_dtlb_0101_source_vseq)
    // 只保存本次场景实际 force 的输入路径；cleanup 逐项 release 后清空。
    string driven_paths[$];
    // 真实时钟由已存在的 issue interface 提供，不建立另一个采样时钟。
    virtual lintsissue_agent_agent_interface clock_vif;
    // 本轮通过严格 request/response 校验的次数，结束时必须非零。
    int unsigned checked_responses;
    localparam string DUT = "top_tb.U_MEMBLOCK";
    localparam string ISSUE = "top_tb.u_memblock__lintsissue_agent_if";

    function new(string name = "memblock_dtlb_0101_source_vseq");
        super.new(name);
        checked_responses = 0;
    endfunction
    virtual function string scenario(); return "l2_query"; endfunction

    function uvm_hdl_data_t read_hdl(input string path, input int unsigned width = 1);
        uvm_hdl_data_t value;
        if (!uvm_hdl_read(path, value))
            `uvm_fatal("DTLB0101_HDL", $sformatf("cannot read %s", path))
        // 此 VCS DPI 只填写实际信号对应的 32-bit word；高位可能保留 X。
        // 严格保留真实位宽内的四态值，仅清除不属于该信号的扩展位。
        for (int i = width; i < $bits(uvm_hdl_data_t); i++) value[i] = 0;
        return value;
    endfunction

    // 运行期只检索本场景几十个已驱动输入；与主表事务数量无关。
    task drive(input string path, input uvm_hdl_data_t value);
        bit known;
        known = 0;
        foreach (driven_paths[i]) if (driven_paths[i] == path) known = 1;
        if (!uvm_hdl_force(path, value))
            `uvm_fatal("DTLB0101_HDL", $sformatf("cannot drive %s", path))
        if (!known) driven_paths.push_back(path);
    endtask

    task cycles(input int unsigned count);
        repeat (count) @(posedge clock_vif.clk);
        @(negedge clock_vif.clk);
    endtask

    // 抽象职责：对一个真实 DTLB 请求验证下一采样拍的命令、地址和来源。
    // wait_limit 仅作失败诊断；正常返回以真实请求及响应核对完成为条件。
    task check_response(input string tlb, input int unsigned port_id,
                        input int unsigned expected_cmd, input int expected_source,
                        input bit [49:0] expected_addr);
        string req_path, resp_path, prefix;
        uvm_hdl_data_t req_cmd, req_addr, resp_addr, out_cmd, resp_valid, miss;
        uvm_hdl_data_t atomic_sel, segment_sel, rob;
        bit found;
        prefix = {DUT, ".", tlb};
        req_path = $sformatf("%s.io_requestor_%0d_req_valid", prefix, port_id);
        resp_path = $sformatf("%s.io_requestor_%0d_resp_valid", prefix, port_id);
        found = 0;
        rob = 0;
        for (int attempt = 0; attempt < 256; attempt++) begin
            @(posedge clock_vif.clk);
            if (read_hdl(req_path) === uvm_hdl_data_t'(1)) begin
                found = 1;
                break;
            end
        end
        if (!found) begin
            `uvm_error("DTLB0101_NO_REQUEST", $sformatf("scenario=%s no live request at %s", scenario(), req_path))
            return;
        end
        req_cmd = read_hdl($sformatf("%s.io_requestor_%0d_req_bits_cmd", prefix, port_id), 3);
        req_addr = read_hdl($sformatf("%s.io_requestor_%0d_req_bits_vaddr", prefix, port_id), 50);
        if (read_hdl($sformatf("%s.io_requestor_%0d_req_bits_kill", prefix, port_id)) !== uvm_hdl_data_t'(0))
            `uvm_error("DTLB0101_KILL", "target request is killed or unknown")
        if (req_addr !== uvm_hdl_data_t'(expected_addr))
            `uvm_error("DTLB0101_VADDR", $sformatf("expected va=%0h actual=%0h", expected_addr, req_addr))
        if (req_cmd !== uvm_hdl_data_t'(expected_cmd))
            `uvm_error("DTLB0101_CMD", $sformatf("expected cmd=%0d actual=%0h", expected_cmd, req_cmd))
        if (expected_source >= 0) begin
            atomic_sel = read_hdl({DUT, ".inner_state"}, 2);
            segment_sel = read_hdl({DUT, ".inner_dtlb_ld_tlb_ld_io_requestor_0_req_bits_REG"});
            if ((expected_source == 1 && !(atomic_sel === uvm_hdl_data_t'(1) || atomic_sel === uvm_hdl_data_t'(2))) ||
                (expected_source == 2 && (atomic_sel !== uvm_hdl_data_t'(0) || segment_sel !== uvm_hdl_data_t'(1))) ||
                (expected_source == 0 && (atomic_sel !== uvm_hdl_data_t'(0) || segment_sel !== uvm_hdl_data_t'(0))))
                `uvm_error("DTLB0101_SOURCE", $sformatf("source=%0d atomic=%0h segment=%0h", expected_source, atomic_sel, segment_sel))
            rob = read_hdl($sformatf("%s.io_requestor_%0d_req_bits_debug_robIdx_value", prefix, port_id), 8);
        end
        @(posedge clock_vif.clk);
        resp_valid = read_hdl(resp_path);
        out_cmd = read_hdl($sformatf("%s.req_out_%0d_cmd", prefix, port_id), 3);
        resp_addr = read_hdl($sformatf("%s.req_out_%0d_vaddr", prefix, port_id), 50);
        if (expected_source >= 0 &&
            read_hdl($sformatf("%s.req_out_%0d_debug_robIdx_value", prefix, port_id), 8) !== rob)
            `uvm_error("DTLB0101_ROB", "LD0 ROB identity changed across request register")
        miss = read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_miss", prefix, port_id));
        if (resp_valid !== uvm_hdl_data_t'(1) || out_cmd !== req_cmd ||
            resp_addr !== req_addr || miss !== uvm_hdl_data_t'(0)) begin
            `uvm_error("DTLB0101_RESPONSE", $sformatf("%s port=%0d resp=%0h cmd=%0h/%0h va=%0h/%0h miss=%0h",
                tlb, port_id, resp_valid, out_cmd, req_cmd, resp_addr, req_addr, miss))
        end else begin
            checked_responses++;
            `uvm_info("DTLB0101_CHECK", $sformatf("scenario=%s port=%0d cmd=%0d va=%0h source=%0d rob=%0h checked=%0d",
                scenario(), port_id, expected_cmd, req_addr, expected_source, rob, checked_responses), UVM_LOW)
        end
        // 覆盖模型在相同 posedge 采样；下一下降沿才撤销激励或清理。
        @(negedge clock_vif.clk);
    endtask

    task drive_load0();
        drive({ISSUE, ".io_ooo_to_mem_issueLda_0_bits_src_0"}, 'h80001000);
        drive({ISSUE, ".io_ooo_to_mem_issueLda_0_bits_uop_fuOpType"}, MEMBLOCK_LSUOP_LD);
        drive({ISSUE, ".io_ooo_to_mem_issueLda_0_bits_uop_imm"}, 0);
        drive({ISSUE, ".io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value"}, 0);
        drive({ISSUE, ".io_ooo_to_mem_issueLda_0_valid"}, 1);
        check_response("inner_dtlb_ld_tlb_ld", 0, 0, 0, 50'h80001000);
        drive({ISSUE, ".io_ooo_to_mem_issueLda_0_valid"}, 0);
    endtask

    task drive_atomic(input bit lr);
        drive({ISSUE, ".io_ooo_to_mem_issueSta_0_bits_src_0"}, 'h80002000);
        drive({ISSUE, ".io_ooo_to_mem_issueSta_0_bits_uop_fuType"}, 35'(1 << 17));
        drive({ISSUE, ".io_ooo_to_mem_issueSta_0_bits_uop_fuOpType"}, lr ? MEMBLOCK_LSUOP_LR_D : MEMBLOCK_LSUOP_AMOADD_D);
        drive({ISSUE, ".io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value"}, 0);
        drive({ISSUE, ".io_ooo_to_mem_issueSta_0_bits_uop_rfWen"}, 1);
        drive({ISSUE, ".io_ooo_to_mem_issueSta_0_valid"}, 1);
        cycles(1);
        drive({ISSUE, ".io_ooo_to_mem_issueSta_0_valid"}, 0);
        check_response("inner_dtlb_ld_tlb_ld", 0, lr ? 4 : 5, 1, 50'h80002000);
    endtask

    task drive_segment();
        string unit_path;
        unit_path = {DUT, ".inner_vSegmentUnit"};
        // 当前 vector driver 拒绝有效发射；仅在本专项驱动真实 segment 模块输入。
        // VL=1、NF=1、EEW=SEW=8、VM=1，保留真实 LD0 额外寄存和选路。
        drive({unit_path, ".io_in_bits_uop_fuType"}, 35'h200000000);
        // VlduType.vle=01_00_00000，使用合法的向量 unit-stride load 编码。
        drive({unit_path, ".io_in_bits_uop_fuOpType"}, 9'h080);
        drive({unit_path, ".io_in_bits_uop_vpu_nf"}, 1);
        drive({unit_path, ".io_in_bits_uop_vpu_vsew"}, 0);
        drive({unit_path, ".io_in_bits_uop_vpu_veew"}, 0);
        drive({unit_path, ".io_in_bits_uop_vpu_vlmul"}, 0);
        drive({unit_path, ".io_in_bits_uop_vpu_vm"}, 1);
        drive({unit_path, ".io_in_bits_uop_vpu_vstart"}, 0);
        drive({unit_path, ".io_in_bits_uop_vpu_vuopIdx"}, 0);
        drive({unit_path, ".io_in_bits_uop_vpu_lastUop"}, 0);
        drive({unit_path, ".io_in_bits_uop_vecWen"}, 1);
        drive({unit_path, ".io_in_bits_uop_v0Wen"}, 0);
        drive({unit_path, ".io_in_bits_uop_vlWen"}, 0);
        drive({unit_path, ".io_in_bits_src_0"}, 'h80003000);
        drive({unit_path, ".io_in_bits_src_1"}, 1);
        drive({unit_path, ".io_in_bits_src_2"}, 0);
        drive({unit_path, ".io_in_bits_src_3"}, '1);
        drive({unit_path, ".io_in_bits_src_4"}, 1);
        drive({unit_path, ".io_in_bits_uop_robIdx_value"}, 0);
        drive({unit_path, ".io_in_valid"}, 1);
        cycles(1);
        // NF=1 表示两段，提供两个连续微操作；第二笔才是 lastUop。
        // 首笔建立指令上下文，次笔为两个字段提供完整队列和掩码输入。
        drive({unit_path, ".io_in_bits_uop_vpu_vuopIdx"}, 1);
        drive({unit_path, ".io_in_bits_uop_vpu_lastUop"}, 1);
        cycles(1);
        drive({unit_path, ".io_in_valid"}, 0);
        check_response("inner_dtlb_ld_tlb_ld", 0, 0, 2, 50'h80003000);
    endtask

    task drive_prefetch(input bit sms);
        string unit_path;
        if (sms) begin
            unit_path = {DUT, ".inner_prefetcherOpt.pf_filter"};
            drive({unit_path, ".io_gen_req_bits_region_tag"}, 'h123);
            drive({unit_path, ".io_gen_req_bits_region_addr"}, 'h200010);
            drive({unit_path, ".io_gen_req_bits_region_bits"}, 'h000f);
            drive({unit_path, ".io_gen_req_bits_paddr_valid"}, 0);
            drive({unit_path, ".io_gen_req_bits_decr_mode"}, 0);
            drive({unit_path, ".io_gen_req_valid"}, 1);
            cycles(1);
            drive({unit_path, ".io_gen_req_valid"}, 0);
            check_response("inner_dtlb_prefetch_tlb_prefetch", 0, 0, -1, 50'h80004000);
        end else begin
            unit_path = {DUT, ".inner_l1PrefetcherOpt.pf_queue_filter"};
            drive({unit_path, ".io_enable"}, 1);
            drive({unit_path, ".io_l1_prefetch_req_bits_region"}, 'h200020);
            drive({unit_path, ".io_l1_prefetch_req_bits_bit_vec"}, 'h000f);
            drive({unit_path, ".io_l1_prefetch_req_bits_source_value"}, 3);
            drive({unit_path, ".io_l1_prefetch_req_valid"}, 1);
            cycles(1);
            drive({unit_path, ".io_l1_prefetch_req_valid"}, 0);
            check_response("inner_dtlb_ld_tlb_ld", 3, 0, -1, 50'h80008000);
        end
    endtask

    task drive_l2_query(input bit write_cmd);
        drive("top_tb.io_l2_tlb_req_req_bits_vaddr", write_cmd ? 'h80005000 : 'h80004000);
        drive("top_tb.io_l2_tlb_req_req_bits_cmd", write_cmd);
        drive("top_tb.io_l2_tlb_req_req_bits_kill", 0);
        drive("top_tb.io_l2_tlb_req_req_bits_isPrefetch", 1);
        drive("top_tb.io_l2_tlb_req_req_bits_no_translate", 0);
        drive("top_tb.io_l2_tlb_req_req_valid", 1);
        check_response("inner_dtlb_prefetch_tlb_prefetch", 1, write_cmd, -1,
                       write_cmd ? 50'h80005000 : 50'h80004000);
        drive("top_tb.io_l2_tlb_req_req_valid", 0);
        cycles(2);
    endtask

    // 抽象职责：请求级专项完成后通过整个 DUT 的 reset 取消未完成下游操作，
    // 再释放本例全部 force；该取消不冒充指令提交、DCache 或 vector WB 通过。
    task cleanup();
        drive({DUT, ".reset"}, 1);
        cycles(32);
        foreach (driven_paths[i]) begin
            if (!uvm_hdl_release(driven_paths[i]))
                `uvm_error("DTLB0101_RELEASE", driven_paths[i])
        end
        driven_paths.delete();
        cycles(32);
    endtask

    virtual task body();
        require_virtual_sqr();
        seq_csr_common::init();
        if (!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,
            "uvm_test_top.env.u_lintsissue_agent_agent", "vif", clock_vif))
            `uvm_fatal("DTLB0101_VIF", "cannot obtain existing issue clock interface")
        wait(memblock_sync_pkg::reset_backend_done === 1'b1);
        cycles(48);
        `uvm_info("DTLB0101_SCOPE", "request-level RTL source/DTLB check; no full instruction retirement claim", UVM_LOW)
        // 稳定控制输入通过已有接口设置；不 force 内部 CSR 寄存器或覆盖采样输入。
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_satp_mode", 0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_vsatp_mode", 0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_hgatp_mode", 0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_virt", 0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_dmode", 3);
        cycles(8);
        if (read_hdl({DUT, ".inner_dtlb_ld_tlb_ld.io_csr_satp_mode"}, 4) !== uvm_hdl_data_t'(0))
            `uvm_fatal("DTLB0101_CONTEXT", "expected stable Bare CSR context")
        case (scenario())
            "l2_query": begin drive_l2_query(0); drive_l2_query(1); end
            "atomic_lr": begin drive_load0(); drive_atomic(1); end
            "atomic_amo": begin drive_load0(); drive_atomic(0); end
            "segment": begin drive_load0(); drive_segment(); end
            "stream": drive_prefetch(0);
            "sms": drive_prefetch(1);
            default: `uvm_fatal("DTLB0101_SCENARIO", scenario())
        endcase
        cleanup();
        if (checked_responses != ((scenario() == "stream" || scenario() == "sms") ? 1 : 2))
            `uvm_error("DTLB0101_EMPTY", "required source response checks are incomplete")
        `uvm_info("DTLB0101_COMPLETE", $sformatf("scenario=%s checked=%0d", scenario(), checked_responses), UVM_LOW)
    endtask
endclass

`define DTLB0101_CLASS(CLASS, SCENARIO) \
class CLASS extends memblock_dtlb_0101_source_vseq; \
    `uvm_object_utils(CLASS) \
    function new(string name = `"CLASS`"); super.new(name); endfunction \
    virtual function string scenario(); return SCENARIO; endfunction \
endclass
`DTLB0101_CLASS(memblock_dtlb_0101_l2_query_vseq, "l2_query")
`DTLB0101_CLASS(memblock_dtlb_0101_atomic_lr_vseq, "atomic_lr")
`DTLB0101_CLASS(memblock_dtlb_0101_atomic_amo_vseq, "atomic_amo")
`DTLB0101_CLASS(memblock_dtlb_0101_segment_vseq, "segment")
`DTLB0101_CLASS(memblock_dtlb_0101_stream_vseq, "stream")
`DTLB0101_CLASS(memblock_dtlb_0101_sms_vseq, "sms")
`undef DTLB0101_CLASS
`endif
