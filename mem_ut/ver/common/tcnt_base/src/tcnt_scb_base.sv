`ifndef TCNT_SCB_BASE__SV
`define TCNT_SCB_BASE__SV

class tcnt_scb_base #(type seq_item_t=tcnt_data_base) extends uvm_scoreboard;

    uvm_blocking_get_port #(seq_item_t) exp_port;
    uvm_blocking_get_port #(seq_item_t) act_port;

    `uvm_component_param_utils(tcnt_scb_base #(seq_item_t))

    //scoreboard configuration
    tcnt_dec_base::scb_mode_sel_e pre_mode_sel;
    tcnt_dec_base::scb_mode_sel_e mode_sel;
    bit ophan_error_en;
        //for compare in turn : while allowed the DUT drop & ophan_error_en is 1, it means the TC should send the sequence which DUT would not drop at the last
        //for compare out turn : the ohpan_error_en should be 1 while allowed the DUT drop
    int div_num;

    bit scb_hadbeen_enable;

    seq_item_t expect_queue[int][$];
    seq_item_t actual_queue[int][$];

    //scoreboard statistics
    longint total_pkt_num[int];
    longint match_pkt_num[int];
    longint mismatch_pkt_num[int];
    longint drop_pkt_num[int];

    extern         function      new(string name , uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task run_phase(uvm_phase phase);
    extern virtual function void report();
    extern virtual task scb_compare();
    extern virtual task DoCompare__InOrder_RmMustFast_WithoutDrop();
    extern virtual task DoCompare__InOrder_RmMustFast_WithDrop();
    extern virtual task DoCompare__InOrder_DutMaybeFast_WithoutDrop();
    extern virtual task DoCompare__InOrder_DutMaybeFast_WithDrop();
    extern virtual task DoCompare__InOrder_DutMustFast_WithoutDrop();
    extern virtual task DoCompare__InOrder_DutMustFast_WithDrop();
    extern virtual task DoCompare__OutOrder();
    extern virtual task DoComapre__Disable();
endclass
function tcnt_scb_base::new(string name , uvm_component parent);
    super.new(name, parent);
    this.pre_mode_sel = tcnt_dec_base::InOrder_RmMustFast_WithoutDrop;
    this.mode_sel = tcnt_dec_base::InOrder_RmMustFast_WithoutDrop;
    this.ophan_error_en = 1'b1;
    this.div_num = 1;
    this.scb_hadbeen_enable = 1'b0;
endfunction
function void tcnt_scb_base::build_phase(uvm_phase phase);
    super.build_phase(phase);
    this.exp_port = new("exp_port", this);
    this.act_port = new("act_port", this);
endfunction
task tcnt_scb_base::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.scb_compare();
endtask
task tcnt_scb_base::scb_compare();
    while(1) begin
        this.pre_mode_sel = this.mode_sel;
        if(this.mode_sel!=tcnt_dec_base::Disable) begin
            `uvm_info(get_type_name(),$sformatf("%s is enabled with mode %p",get_name(),this.mode_sel),UVM_NONE)
            this.scb_hadbeen_enable = 1'b1;
        end
        else begin
            `uvm_info(get_type_name(),$sformatf("%s is disabled",get_name()),UVM_NONE)
        end
        case(this.mode_sel)
            tcnt_dec_base::InOrder_RmMustFast_WithoutDrop : begin
                this.DoCompare__InOrder_RmMustFast_WithoutDrop();
            end
            tcnt_dec_base::InOrder_RmMustFast_WithDrop : begin
                this.DoCompare__InOrder_RmMustFast_WithDrop();
            end
            tcnt_dec_base::InOrder_DutMaybeFast_WithoutDrop : begin
                this.DoCompare__InOrder_DutMaybeFast_WithoutDrop();
            end
            tcnt_dec_base::InOrder_DutMaybeFast_WithDrop : begin
                this.DoCompare__InOrder_DutMaybeFast_WithDrop();
            end
            tcnt_dec_base::InOrder_DutMustFast_WithoutDrop : begin
                this.DoCompare__InOrder_DutMustFast_WithoutDrop();
            end
            tcnt_dec_base::InOrder_DutMustFast_WithDrop : begin
                this.DoCompare__InOrder_DutMustFast_WithDrop();
            end
            tcnt_dec_base::OutOrder_DutMustFast_WithoutDrop,tcnt_dec_base::OutOrder_DutMustFast_WithDrop : begin
                this.DoCompare__OutOrder();
            end
            tcnt_dec_base::Disable : begin
                this.DoComapre__Disable();
            end
        endcase
    end
endtask
task tcnt_scb_base::DoCompare__InOrder_RmMustFast_WithoutDrop();
    seq_item_t get_expect, get_actual, tmp_tran;
    bit result;
    fork
        while(1) begin
            exp_port.get(get_expect);
            if(get_expect==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_RmMustFast_WithoutDrop had get expect NULL")
            end
            expect_queue[get_expect.channel_id].push_back(get_expect);
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
        while(1) begin
            act_port.get(get_actual);
            if(get_actual==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_RmMustFast_WithoutDrop had get actual NULL")
            end
            if(this.total_pkt_num[get_actual.channel_id]==0) begin
                `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id),$sformatf("getting the first DUT packet to compare"),UVM_NONE)
            end
            this.total_pkt_num[get_actual.channel_id]++;
            if(expect_queue[get_actual.channel_id].size() > 0) begin
                tmp_tran = expect_queue[get_actual.channel_id].pop_front();
                result = get_actual.compare(tmp_tran);
                if(result) begin 
                    if(this.total_pkt_num[get_actual.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_actual.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",tmp_tran.psdisplay());
                        //$display("the actual pkt is\n%s",get_actual.psdisplay());
                    end
                    this.match_pkt_num[get_actual.channel_id]++;
                end
                else begin
                    `uvm_error($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare FAILED of %0d packet",this.total_pkt_num[get_actual.channel_id]));
                    $display("the expect pkt is\n%s",tmp_tran.psdisplay());
                    $display("the actual pkt is\n%s",get_actual.psdisplay());
                    this.mismatch_pkt_num[get_actual.channel_id]++;
                end
            end
            else begin
                `uvm_error($sformatf("CHANNEL(%0d)",get_actual.channel_id), "Received from DUT, while Expect Queue is empty");
                $display("the unexpected pkt is\n%s",get_actual.psdisplay());
            end 
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
    join
endtask
task tcnt_scb_base::DoCompare__InOrder_RmMustFast_WithDrop();
    seq_item_t get_expect, get_actual, tmp_tran;
    bit result;
    fork
        while(1) begin
            exp_port.get(get_expect);
            if(get_expect==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_RmMustFast_WithDrop had get expect NULL")
            end
            expect_queue[get_expect.channel_id].push_back(get_expect);
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
        while(1) begin
            act_port.get(get_actual);
            if(get_actual==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_RmMustFast_WithDrop had get actual NULL")
            end
            if(this.total_pkt_num[get_actual.channel_id]==0) begin
                `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id),"getting the first DUT packet to compare",UVM_NONE)
            end
            this.total_pkt_num[get_actual.channel_id]++;
            if(expect_queue[get_actual.channel_id].size() > 0) begin
                int match_index=0;
                foreach(expect_queue[get_actual.channel_id][i]) begin
                    result = get_actual.compare(expect_queue[get_actual.channel_id][i]);
                    if(result) begin
                        tmp_tran = expect_queue[get_actual.channel_id][i];
                        match_index = i;
                        break;
                    end
                end
                if(result) begin 
                    if(this.total_pkt_num[get_actual.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_actual.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",tmp_tran.psdisplay());
                        //$display("the actual pkt is\n%s",get_actual.psdisplay());
                    end
                    this.match_pkt_num[get_actual.channel_id]++;
                    for(int i=0; i<match_index+1; i++) begin//delete
                        void'(expect_queue[get_actual.channel_id].pop_front());
                        this.drop_pkt_num[get_actual.channel_id] += (i<match_index) ? 1 : 0;
                    end
                end
                else begin
                    tmp_tran = expect_queue[get_actual.channel_id].pop_front();
                    `uvm_error($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare FAILED of %0d packet",this.total_pkt_num[get_actual.channel_id]));
                    $display("the first expect pkt in expect_queue is\n%s",tmp_tran.psdisplay());
                    $display("the actual pkt is\n%s",get_actual.psdisplay());
                    this.mismatch_pkt_num[get_actual.channel_id]++;
                end
            end
            else begin
                `uvm_error($sformatf("CHANNEL(%0d)",get_actual.channel_id), "Received from DUT, while Expect Queue is empty");
                $display("the unexpected pkt is\n%s",get_actual.psdisplay());
            end 
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
    join
endtask
task tcnt_scb_base::DoCompare__InOrder_DutMaybeFast_WithoutDrop();
    seq_item_t get_expect, get_actual, tmp_tran;
    bit result;
    fork
        while(1) begin
            exp_port.get(get_expect);
            if(get_expect==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_DutMaybeFast_WithoutDrop had get expect NULL")
            end
            if(actual_queue[get_expect.channel_id].size() > 0) begin
                tmp_tran = actual_queue[get_expect.channel_id].pop_front();
                result = get_expect.compare(tmp_tran);
                if(result) begin 
                    if(this.total_pkt_num[get_expect.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_expect.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_expect.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",get_expect.psdisplay());
                        //$display("the actual pkt is\n%s",tmp_tran.psdisplay());
                    end
                    this.match_pkt_num[get_expect.channel_id]++;
                end
                else begin
                    `uvm_error($sformatf("CHANNEL(%0d)",get_expect.channel_id), $sformatf("Compare FAILED of %0d packet",this.total_pkt_num[get_expect.channel_id]));
                    $display("the expect pkt is\n%s",get_expect.psdisplay());
                    $display("the actual pkt is\n%s",tmp_tran.psdisplay());
                    this.mismatch_pkt_num[get_expect.channel_id]++;
                end
            end
            else begin
                expect_queue[get_expect.channel_id].push_back(get_expect);
            end
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
        while(1) begin
            act_port.get(get_actual);
            if(get_actual==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_DutMaybeFast_WithoutDrop had get actual NULL")
            end
            if(this.total_pkt_num[get_actual.channel_id]==0) begin
                `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id),"getting the first DUT packet to compare",UVM_NONE)
            end
            this.total_pkt_num[get_actual.channel_id]++;
            if(expect_queue[get_actual.channel_id].size() > 0) begin
                tmp_tran = expect_queue[get_actual.channel_id].pop_front();
                result = get_actual.compare(tmp_tran);
                if(result) begin 
                    if(this.total_pkt_num[get_actual.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_actual.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",tmp_tran.psdisplay());
                        //$display("the actual pkt is\n%s",get_actual.psdisplay());
                    end
                    this.match_pkt_num[get_actual.channel_id]++;
                end
                else begin
                    `uvm_error($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare FAILED of %0d packet",this.total_pkt_num[get_actual.channel_id]));
                    $display("the expect pkt is\n%s",tmp_tran.psdisplay());
                    $display("the actual pkt is\n%s",get_actual.psdisplay());
                    this.mismatch_pkt_num[get_actual.channel_id]++;
                end
            end
            else begin
                actual_queue[get_actual.channel_id].push_back(get_actual);
            end 
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
    join
endtask
task tcnt_scb_base::DoCompare__InOrder_DutMaybeFast_WithDrop();
    seq_item_t get_expect, get_actual, tmp_tran;
    bit result;
    fork
        while(1) begin
            exp_port.get(get_expect);
            if(get_expect==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_DutMaybeFast_WithDrop had get expect NULL")
            end
            if(actual_queue[get_expect.channel_id].size() > 0) begin
                result = get_expect.compare(actual_queue[get_expect.channel_id][0]);
                if(result) begin 
                    tmp_tran = actual_queue[get_expect.channel_id].pop_front();
                    if(this.total_pkt_num[get_expect.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_expect.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_expect.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",get_expect.psdisplay());
                        //$display("the actual pkt is\n%s",tmp_tran.psdisplay());
                    end
                    this.match_pkt_num[get_expect.channel_id]++;
                end
                else begin
                    this.drop_pkt_num[get_actual.channel_id] += 1;
                end
            end
            else begin
                expect_queue[get_expect.channel_id].push_back(get_expect);
            end
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
        while(1) begin
            act_port.get(get_actual);
            if(get_actual==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_DutMaybeFast_WithDrop had get actual NULL")
            end
            if(this.total_pkt_num[get_actual.channel_id]==0) begin
                `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id),"getting the first DUT packet to compare",UVM_NONE)
            end
            this.total_pkt_num[get_actual.channel_id]++;
            if(expect_queue[get_actual.channel_id].size() > 0) begin
                int match_index=0;
                foreach(expect_queue[get_actual.channel_id][i]) begin
                    result = get_actual.compare(expect_queue[get_actual.channel_id][i]);
                    if(result) begin
                        tmp_tran = expect_queue[get_actual.channel_id][i];
                        match_index = i;
                        break;
                    end
                end
                if(result) begin 
                    if(this.total_pkt_num[get_actual.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_actual.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",tmp_tran.psdisplay());
                        //$display("the actual pkt is\n%s",get_actual.psdisplay());
                    end
                    this.match_pkt_num[get_actual.channel_id]++;
                    for(int i=0; i<match_index+1; i++) begin//delete
                        void'(expect_queue[get_actual.channel_id].pop_front());
                        this.drop_pkt_num[get_actual.channel_id] += (i<match_index) ? 1 : 0;
                    end
                end
                else begin
                    tmp_tran = expect_queue[get_actual.channel_id].pop_front();
                    actual_queue[get_actual.channel_id].push_back(get_actual);
                    this.drop_pkt_num[get_actual.channel_id] += 1;
                end
            end
            else begin
                actual_queue[get_actual.channel_id].push_back(get_actual);
            end 
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
    join
endtask
task tcnt_scb_base::DoCompare__InOrder_DutMustFast_WithoutDrop();
    seq_item_t get_expect, get_actual, tmp_tran;
    bit result;
    fork
        while(1) begin
            exp_port.get(get_expect);
            if(get_expect==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_DutMustFast_WithoutDrop had get expect NULL")
            end
            if(actual_queue[get_expect.channel_id].size() > 0) begin
                tmp_tran = actual_queue[get_expect.channel_id].pop_front();
                result = tmp_tran.compare(get_expect);
                if(result) begin 
                    if(this.total_pkt_num[get_expect.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_expect.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_expect.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",get_expect.psdisplay());
                        //$display("the actual pkt is\n%s",tmp_tran.psdisplay());
                    end
                    this.match_pkt_num[get_expect.channel_id]++;
                end
                else begin
                    `uvm_error($sformatf("CHANNEL(%0d)",get_expect.channel_id), $sformatf("Compare FAILED of %0d packet",this.total_pkt_num[get_expect.channel_id]));
                    $display("the expect pkt is\n%s",get_expect.psdisplay());
                    $display("the actual pkt is\n%s",tmp_tran.psdisplay());
                    this.mismatch_pkt_num[get_expect.channel_id]++;
                end
            end
            else begin
                `uvm_error($sformatf("CHANNEL(%0d)",get_expect.channel_id), "Received from RM, while Actual Queue is empty");
                $display("the unexpected pkt is\n%s",get_expect.psdisplay());
            end
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
        while(1) begin
            act_port.get(get_actual);
            if(get_actual==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_DutMustFast_WithoutDrop had get actual NULL")
            end
            if(this.total_pkt_num[get_actual.channel_id]==0) begin
                `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id),"getting the first DUT packet to compare",UVM_NONE)
            end
            this.total_pkt_num[get_actual.channel_id]++;
            actual_queue[get_actual.channel_id].push_back(get_actual);
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
    join
endtask
task tcnt_scb_base::DoCompare__InOrder_DutMustFast_WithDrop();
    seq_item_t get_expect, get_actual, tmp_tran;
    bit result;
    fork
        while(1) begin
            exp_port.get(get_expect);
            if(get_expect==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_DutMustFast_WithDrop had get expect NULL")
            end
            if(actual_queue[get_expect.channel_id].size() > 0) begin
                tmp_tran = actual_queue[get_expect.channel_id].pop_front();
                result = tmp_tran.compare(get_expect);
                if(result) begin 
                    if(this.total_pkt_num[get_expect.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_expect.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_expect.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",get_expect.psdisplay());
                        //$display("the actual pkt is\n%s",tmp_tran.psdisplay());
                    end
                    this.match_pkt_num[get_expect.channel_id]++;
                end
                else begin
                    `uvm_error($sformatf("CHANNEL(%0d)",get_expect.channel_id), $sformatf("Compare FAILED of %0d packet",this.total_pkt_num[get_expect.channel_id]));
                    $display("the expect pkt is\n%s",get_expect.psdisplay());
                    $display("the actual pkt is\n%s",tmp_tran.psdisplay());
                    this.mismatch_pkt_num[get_expect.channel_id]++;
                end
            end
            else begin
                //drop
                this.drop_pkt_num[get_actual.channel_id] += 1;
            end 
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
        while(1) begin
            act_port.get(get_actual);
            if(get_actual==null) begin
                `uvm_fatal(get_name,"DoCompare__InOrder_DutMustFast_WithDrop had get actual NULL")
            end
            if(this.total_pkt_num[get_actual.channel_id]==0) begin
                `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id),"getting the first DUT packet to compare",UVM_NONE)
            end
            this.total_pkt_num[get_actual.channel_id]++;
            actual_queue[get_actual.channel_id].push_back(get_actual);
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
    join
endtask
task tcnt_scb_base::DoCompare__OutOrder();
    seq_item_t get_expect, get_actual, tmp_tran;
    bit result;
    fork
        while(1) begin
            exp_port.get(get_expect);
            if(get_expect==null) begin
                `uvm_fatal(get_name,"DoCompare__OutOrder had get expect NULL")
            end
            expect_queue[get_expect.channel_id].push_back(get_expect);
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
        while(1) begin
            act_port.get(get_actual);
            if(get_actual==null) begin
                `uvm_fatal(get_name,"DoCompare__OutOrder had get actual NULL")
            end
            if(this.total_pkt_num[get_actual.channel_id]==0) begin
                `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id),$sformatf("getting the first DUT packet to compare"),UVM_NONE)
            end
            this.total_pkt_num[get_actual.channel_id]++;
            if(expect_queue[get_actual.channel_id].size() > 0) begin
                int match_index=0;
                foreach(expect_queue[get_actual.channel_id][i]) begin
                    result = get_actual.compare(expect_queue[get_actual.channel_id][i]);
                    if(result) begin
                        tmp_tran = expect_queue[get_actual.channel_id][i];
                        match_index = i;
                        break;
                    end
                end
                if(result) begin 
                    if(this.total_pkt_num[get_actual.channel_id]%div_num==0) begin
                        `uvm_info($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare SUCCESSFULLY of %0d packet",this.total_pkt_num[get_actual.channel_id]), UVM_FULL);
                        //$display("the expect pkt is\n%s",tmp_tran.psdisplay());
                        //$display("the actual pkt is\n%s",get_actual.psdisplay());
                    end
                    expect_queue[get_actual.channel_id].delete(match_index);
                    this.match_pkt_num[get_actual.channel_id]++;
                end
                else begin
                    `uvm_error($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Compare FAILED of %0d packet while there is no expect packet matched",this.total_pkt_num[get_actual.channel_id]));
                    $display("the actual pkt is\n%s",get_actual.psdisplay());
                    this.mismatch_pkt_num[get_actual.channel_id]++;
                end
            end
            else begin
                `uvm_error($sformatf("CHANNEL(%0d)",get_actual.channel_id), $sformatf("Received from DUT, while Expect Queue is empty"));
                $display("the unexpected pkt is\n%s",get_actual.psdisplay());
            end 
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
    join
endtask
task tcnt_scb_base::DoComapre__Disable();
    seq_item_t get_expect, get_actual, tmp_tran;
    bit result;
    fork
        while(1) begin
            exp_port.get(get_expect);
            get_expect=null;
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
        while(1) begin
            act_port.get(get_actual);
            get_actual=null;
            if(this.pre_mode_sel!=this.mode_sel) begin
                break; 
            end
        end
    join
endtask
function void tcnt_scb_base::report();
    if(this.scb_hadbeen_enable==1'b0) begin
        return;
    end
    foreach(total_pkt_num[i]) begin
        `uvm_info($sformatf("CHANNEL(%0d)",i),$sformatf("had get DUT packet num is %0d, the match packet num is %0d and mismatch packet num is %0d, dropped packet num is %0d",this.total_pkt_num[i],this.match_pkt_num[i],this.mismatch_pkt_num[i],this.drop_pkt_num[i]),UVM_NONE)
    end
    if(this.mode_sel==tcnt_dec_base::OutOrder_DutMustFast_WithDrop) begin
        this.ophan_error_en = 1'b1;
    end
    if(this.ophan_error_en==1'b0) begin
        return;
    end
    foreach(this.expect_queue[i]) begin
        if(this.expect_queue[i].size() != 0) begin
            `uvm_error($sformatf("CHANNEL(%0d)",i),$sformatf("Expect Queue is not empty(%0d packet orphaned), each Expect pkt is :",this.expect_queue[i].size()))
            foreach(this.expect_queue[i][j]) begin
                `uvm_info($sformatf("CHANNEL(%0d)",i),$sformatf("Orphan pkt[%0d][%0d] is \n%s",i,j,this.expect_queue[i][j].psdisplay()),UVM_NONE)
            end
        end
    end
    foreach(this.actual_queue[i]) begin
        if(this.actual_queue[i].size() != 0) begin
            `uvm_error($sformatf("CHANNEL(%0d)",i),$sformatf("Actual Queue is not empty(%0d packet orphaned), each actual pkt is :",this.actual_queue[i].size()))
            foreach(this.actual_queue[i][j]) begin
                `uvm_info($sformatf("CHANNEL(%0d)",i),$sformatf("Orphan pkt[%0d][%0d] is \n%s",i,j,this.actual_queue[i][j].psdisplay()),UVM_NONE)
            end
        end
    end
endfunction

`endif

