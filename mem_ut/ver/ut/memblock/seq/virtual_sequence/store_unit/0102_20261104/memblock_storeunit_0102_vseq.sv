`ifndef MEMBLOCK_STOREUNIT_0102_VSEQ__SV
`define MEMBLOCK_STOREUNIT_0102_VSEQ__SV

// 抽象职责：由basicTest运行的StoreUnit声明输入边界专项；按预先安排的请求节拍，
// 独立验证两条真实STA流水的身份、地址、反馈和完成。DTLB返回是请求约束的输入夹具；
// 本例不启动主表和L2TLB responder，不声明已验证全局翻译、SQ提交或完整指令退休。
class memblock_storeunit_0102_vseq extends virtual_base_sequence;
    `uvm_object_utils(memblock_storeunit_0102_vseq)

    // 每条记录只保存一个已安排请求的预期；history[p][a]表示p口a拍前接受的请求。
    // miss请求仍保留用于两拍慢反馈，但不应产生S2/S3/普通写回；窗口开始全部清零。
    typedef struct packed {
        bit valid;
        bit miss;
        bit [8:0] rob;
        bit [6:0] sq;
        bit [63:0] va;
        bit [47:0] pa;
    } request_t;
    request_t history[2][5];
    // 只登记真实声明input的force。map使每拍更新为定长查询，结束按queue释放；
    // 本例上界为两实例的声明input总数，不扫描公共主表或历史事务。
    bit driven[string];
    string driven_paths[$];
    virtual lintsissue_agent_agent_interface clock_vif;
    // 由真实检查成功后累加；构造/复位时清零，结束必须与场景计划严格相符。
    int unsigned accepted[2], completed[2], misses[2], replacements[2];
    localparam string DUT = "top_tb.U_MEMBLOCK";

    function new(string name="memblock_storeunit_0102_vseq"); super.new(name); endfunction
    virtual function string scenario(); return "single"; endfunction

    function string unit_path(input int p);
        return $sformatf("%s.inner_StoreUnit_%0d",DUT,p);
    endfunction
    function uvm_hdl_data_t read_signal(input string path,input int width=1);
        uvm_hdl_data_t value;
        if(!uvm_hdl_read(path,value)) `uvm_fatal("STU0102_HDL",path)
        // VCS DPI只写信号实际word；保留真实位宽内四态，清除扩展位。
        for(int i=width;i<$bits(uvm_hdl_data_t);i++) value[i]=0;
        return value;
    endfunction
    task drive_input(input string path,input uvm_hdl_data_t value);
        if(!uvm_hdl_force(path,value)) `uvm_fatal("STU0102_DRIVE",path)
        if(!driven.exists(path))begin driven[path]=1;driven_paths.push_back(path);end
    endtask
    task cycles(input int count);
        repeat(count) @(posedge clock_vif.clk);
        @(negedge clock_vif.clk);
    endtask
    // 抽象职责：比较某个真实输入/输出或只读流水寄存器与当前拍独立预期；
    // 四态不一致直接报ERROR，不通过改期望、force输出或读模型计数完成检查。
    task check_signal(input string path,input int width,input uvm_hdl_data_t expected);
        uvm_hdl_data_t got;
        got=read_signal(path,width);
        if(got!==expected) `uvm_error("STU0102_COMPARE",$sformatf("scenario=%s path=%s expected=%0h actual=%0h",scenario(),path,expected,got))
    endtask

    // 抽象职责：建立本次独占的模块输入夹具；所有input由当前RTL声明生成，
    // reset使DUT及只读覆盖模型自然清空历史，空闲周期使控制输入稳定。
    task initialize_fixture();
        string u;
        foreach(history[p,a]) history[p][a]='0;
        for(int p=0;p<2;p++)begin
            accepted[p]=0;completed[p]=0;misses[p]=0;replacements[p]=0;
            u=unit_path(p);
            `include "storeunit_0102_input_defaults.svh"
            drive_input({u,".reset"},1);
        end
        cycles(8);
        for(int p=0;p<2;p++) drive_input({unit_path(p),".reset"},0);
        cycles(4);
    endtask

    // 抽象职责：在下降沿为一个端口准备本拍标量输入及上一拍请求的翻译响应。
    // 响应只针对history[p][1]；miss/地址/身份均来自本例请求记录，不由DUT输出反推。
    task prepare_cycle(input int p,input request_t current);
        string u;request_t previous;
        u=unit_path(p);previous=history[p][1];
        drive_input({u,".io_stin_valid"},current.valid);
        drive_input({u,".io_stin_bits_uop_fuOpType"},MEMBLOCK_LSUOP_SD);
        drive_input({u,".io_stin_bits_src_0"},current.va);
        drive_input({u,".io_stin_bits_uop_imm"},0);
        drive_input({u,".io_stin_bits_uop_robIdx_flag"},current.rob[8]);
        drive_input({u,".io_stin_bits_uop_robIdx_value"},current.rob[7:0]);
        drive_input({u,".io_stin_bits_uop_sqIdx_flag"},current.sq[6]);
        drive_input({u,".io_stin_bits_uop_sqIdx_value"},current.sq[5:0]);
        drive_input({u,".io_tlb_resp_valid"},previous.valid);
        drive_input({u,".io_tlb_resp_bits_miss"},previous.miss);
        drive_input({u,".io_tlb_resp_bits_fullva"},previous.va);
        drive_input({u,".io_tlb_resp_bits_gpaddr_0"},previous.va);
        drive_input({u,".io_tlb_resp_bits_paddr_0"},previous.pa);
    endtask

    // 抽象职责：在上升沿NBA更新前逐拍检查S0接受、S1交接、S2补充信息、
    // 慢反馈、S3及第4拍普通写回。固定2口×4拍窗口，不等待输出再定义采样起点。
    task check_cycle(input int p,input int tick,input request_t current);
        string u;request_t h1,h2,h3,h4;bit v2,v3,v4;
        int exception_bits[6]='{3,6,7,15,19,23};
        u=unit_path(p);h1=history[p][1];h2=history[p][2];h3=history[p][3];h4=history[p][4];
        v2=h2.valid&&!h2.miss;v3=h3.valid&&!h3.miss;v4=h4.valid&&!h4.miss;
        check_signal({u,".io_stin_ready"},1,uvm_hdl_data_t'(current.valid));
        check_signal({u,".io_tlb_req_valid"},1,uvm_hdl_data_t'(current.valid));
        check_signal({u,".io_st_mask_out_valid"},1,uvm_hdl_data_t'(current.valid));
        if(current.valid)begin
            check_signal({u,".io_tlb_req_bits_vaddr"},50,uvm_hdl_data_t'(current.va));
            check_signal({u,".io_tlb_req_bits_cmd"},3,1);
            check_signal({u,".io_st_mask_out_bits_mask"},16,16'hff);
            accepted[p]++;
            if(h1.valid)replacements[p]++;
        end
        check_signal({u,".s1_valid"},1,uvm_hdl_data_t'(h1.valid));
        check_signal({u,".io_lsq_valid"},1,uvm_hdl_data_t'(h1.valid));
        if(h1.valid)begin
            check_signal({u,".io_lsq_bits_uop_robIdx_flag"},1,uvm_hdl_data_t'(h1.rob[8]));
            check_signal({u,".io_lsq_bits_uop_robIdx_value"},8,uvm_hdl_data_t'(h1.rob[7:0]));
            check_signal({u,".io_lsq_bits_uop_sqIdx_flag"},1,uvm_hdl_data_t'(h1.sq[6]));
            check_signal({u,".io_lsq_bits_uop_sqIdx_value"},6,uvm_hdl_data_t'(h1.sq[5:0]));
            check_signal({u,".io_lsq_bits_vaddr"},50,uvm_hdl_data_t'(h1.va));
            check_signal({u,".io_lsq_bits_paddr"},48,uvm_hdl_data_t'(h1.pa));
            check_signal({u,".io_lsq_bits_miss"},1,uvm_hdl_data_t'(h1.miss));
            check_signal({u,".s1_kill"},1,uvm_hdl_data_t'(h1.miss));
        end
        check_signal({u,".s2_valid"},1,uvm_hdl_data_t'(v2));
        if(v2)begin
            check_signal({u,".io_lsq_replenish_uop_robIdx_value"},8,uvm_hdl_data_t'(h2.rob[7:0]));
            check_signal({u,".io_lsq_replenish_fullva"},64,uvm_hdl_data_t'(h2.va));
            check_signal({u,".io_lsq_replenish_hasException"},1,0);
        end
        check_signal({u,".io_feedback_slow_valid"},1,uvm_hdl_data_t'(h2.valid));
        if(h2.valid)begin
            check_signal({u,".io_feedback_slow_bits_hit"},1,uvm_hdl_data_t'(!h2.miss));
            check_signal({u,".io_feedback_slow_bits_sqIdx_flag"},1,uvm_hdl_data_t'(h2.sq[6]));
            check_signal({u,".io_feedback_slow_bits_sqIdx_value"},6,uvm_hdl_data_t'(h2.sq[5:0]));
            if(h2.miss)misses[p]++;
        end
        check_signal({u,".s3_valid"},1,uvm_hdl_data_t'(v3));
        check_signal({u,".io_stout_valid"},1,uvm_hdl_data_t'(v4));
        check_signal({u,".io_vecstout_valid"},1,0);
        if(v4)begin
            check_signal({u,".io_stout_bits_uop_robIdx_flag"},1,uvm_hdl_data_t'(h4.rob[8]));
            check_signal({u,".io_stout_bits_uop_robIdx_value"},8,uvm_hdl_data_t'(h4.rob[7:0]));
            check_signal({u,".io_stout_bits_uop_trigger"},4,15);
            check_signal({u,".io_stout_bits_debug_isMMIO"},1,0);
            foreach(exception_bits[i])
                check_signal($sformatf("%s.io_stout_bits_uop_exceptionVec_%0d",u,exception_bits[i]),1,0);
            completed[p]++;
        end
        `uvm_info("STU0102_CYCLE",$sformatf("scenario=%s pipe=%0d tick=%0d in=%0d s1=%0d s2=%0d s3=%0d wb=%0d rob=%0h miss=%0d",scenario(),p,tick,current.valid,h1.valid,v2,v3,v4,h4.rob,h1.miss),UVM_LOW)
    endtask

    // 抽象职责：按所选有效位模式驱动两条真实流水，并在请求消耗到第4拍后完成。
    // 固定尾部6拍用于观测已知四级延迟及空泡，无无限等待或公共主表扫描。
    task run_pattern();
        bit [4:0] pattern;int length,token,seed_offset;request_t current[2];
        case(scenario())
            "single":begin pattern=5'b00001;length=1;end
            "adjacent","replace","miss_replace":begin pattern=5'b00011;length=2;end
            "bubble1":begin pattern=5'b00101;length=3;end
            "bubble2":begin pattern=5'b01001;length=4;end
            "stream5":begin pattern=5'b11111;length=5;end
            default:`uvm_fatal("STU0102_SCENARIO",scenario())
        endcase
        token=0;seed_offset=$urandom_range(0,15)*16;
        for(int tick=0;tick<length+6;tick++)begin
            for(int p=0;p<2;p++)begin
                current[p]='0;
                if(tick<length && pattern[tick])begin
                    current[p].valid=1;current[p].miss=scenario()=="miss_replace"&&token==0;
                    current[p].rob=9'(32+p*32+token);current[p].sq=7'(8+p*8+token);
                    current[p].va=64'h80000000+64'(p*4096+seed_offset+token*16);
                    current[p].pa=48'h40000000+48'(p*4096+seed_offset+token*16);
                end
                prepare_cycle(p,current[p]);
            end
            @(posedge clock_vif.clk);
            for(int p=0;p<2;p++)check_cycle(p,tick,current[p]);
            for(int p=0;p<2;p++)begin
                for(int a=4;a>1;a--)history[p][a]=history[p][a-1];
                history[p][1]=current[p];
            end
            if(tick<length&&pattern[tick])token++;
            @(negedge clock_vif.clk);
        end
        for(int p=0;p<2;p++)begin
            if(accepted[p]!=token || completed[p]!=token-(scenario()=="miss_replace"?1:0) ||
               misses[p]!=(scenario()=="miss_replace"?1:0))
                `uvm_error("STU0102_ACCOUNTING",$sformatf("pipe=%0d accepted=%0d wb=%0d miss=%0d expected=%0d",p,accepted[p],completed[p],misses[p],token))
            if((scenario()=="replace"||scenario()=="miss_replace")&&replacements[p]!=1)
                `uvm_error("STU0102_REPLACE","missing adjacent accepted replacement")
        end
    endtask

    // 抽象职责：请求和结果检查结束后，以整体reset取消下游仅模块级事务，
    // 再释放本例实际force的输入；不把清理当作SQ提交或整条指令成功。
    task cleanup_fixture();
        drive_input({DUT,".reset"},1);cycles(32);
        foreach(driven_paths[i])if(!uvm_hdl_release(driven_paths[i]))`uvm_error("STU0102_RELEASE",driven_paths[i])
        driven_paths.delete();driven.delete();cycles(32);
    endtask
    virtual task body();
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))
            `uvm_fatal("STU0102_CLOCK","missing existing clock interface")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)
            `uvm_fatal("STU0102_OWNER","module fixture conflicts with global responder")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        initialize_fixture();run_pattern();cleanup_fixture();
        `uvm_info("STU0102_COMPLETE",$sformatf("scenario=%s accepted=%0d/%0d wb=%0d/%0d miss=%0d/%0d replacement=%0d/%0d",scenario(),accepted[0],accepted[1],completed[0],completed[1],misses[0],misses[1],replacements[0],replacements[1]),UVM_LOW)
    endtask
endclass

`define STU0102_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_storeunit_0102_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`STU0102_CLASS(memblock_storeunit_0102_single_vseq,"single")
`STU0102_CLASS(memblock_storeunit_0102_adjacent_vseq,"adjacent")
`STU0102_CLASS(memblock_storeunit_0102_bubble1_vseq,"bubble1")
`STU0102_CLASS(memblock_storeunit_0102_bubble2_vseq,"bubble2")
`STU0102_CLASS(memblock_storeunit_0102_stream5_vseq,"stream5")
`STU0102_CLASS(memblock_storeunit_0102_replace_vseq,"replace")
`STU0102_CLASS(memblock_storeunit_0102_miss_replace_vseq,"miss_replace")
`undef STU0102_CLASS
`endif
