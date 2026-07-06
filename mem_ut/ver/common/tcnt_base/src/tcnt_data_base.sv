`ifndef TCNT_DATA_BASE__SV
`define TCNT_DATA_BASE__SV

class tcnt_data_base extends uvm_sequence_item;
    realtime start;
    realtime finish;
    rand bit [7:0] pload_q[$];
    int channel_id ;
    rand int pre_pkt_gap ;
    rand int post_pkt_gap ;
    longint pkt_index;
    
    constraint pre_pkt_gap_cons{
        pre_pkt_gap == 0 ;
    }
    constraint post_pkt_gap_cons{
        post_pkt_gap inside {[0:50]} ;
    }

    extern function new(string name="tcnt_data_base");
    extern function void pack();
    extern function void unpack();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(tcnt_data_base)
        `uvm_field_real(start, UVM_ALL_ON)
        `uvm_field_real(finish, UVM_ALL_ON)
        `uvm_field_queue_int(pload_q, UVM_ALL_ON)
        `uvm_field_int(channel_id, UVM_ALL_ON)
        `uvm_field_int(pre_pkt_gap, UVM_ALL_ON)
        `uvm_field_int(post_pkt_gap, UVM_ALL_ON)
        `uvm_field_int(pkt_index, UVM_ALL_ON)
    `uvm_object_utils_end
endclass

function tcnt_data_base::new(string name="tcnt_data_base");
    super.new(name);
    this.start  = $realtime;
    this.finish = $realtime;
endfunction:new
function void tcnt_data_base::pack();
    //super.pack();
endfunction:pack
function void tcnt_data_base::unpack();
    //super.unpack();
endfunction:unpack
function string tcnt_data_base::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d]",prefix,this.pkt_index);
    foreach(this.pload_q[i]) begin
        pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    end
    return pkt_str;
endfunction:psdisplay
function bit tcnt_data_base::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    tcnt_data_base rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs,is not a tcnt_data_base or its extend"))
    end
    super_result = super.compare(rhs_,comparer);
    //if(super_result==0) begin
    //    super_result = 1;
    //    foreach(this.pload_q[i]) begin
    //        if(this.pload_q[i]!=rhs_.pload_q[i]) begin
    //            super_result = 0;
    //            `uvm_info(get_type_name(),$sformatf("compare fail for this.pload[%0d]=0x%2h while the rhs_.pload[%0d]=0x%2h",i,this.pload_q[i],i,rhs_.pload_q[i]),UVM_NONE)
    //        end
    //    end
    //end
    //return super_result;
endfunction:compare

`endif

