module iommu_acd_hpm_tlbqueue #(
    parameter   TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter type          PTW_ACK_TYPE                = iommu_acd_pkg::PTW_ACK_TYPE,
    parameter   SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    
    input  logic                                        ptw_ack_valid_i,
    input  PTW_ACK_TYPE                                 ptw_ack_i,
    input  logic                                        entry_info_process_id_valid_i[0:TLB_QUEUE_DEPTH-1],
    input  logic [19:0]                                 entry_info_process_id_i      [0:TLB_QUEUE_DEPTH-1],
    input  logic [23:0]                                 entry_info_device_id_i       [0:TLB_QUEUE_DEPTH-1],
    
    output iommu_acd_pkg::RISCV_HPMEVT_TYPE             riscv_hpmevt_intf_o,
    
    input  logic                                        spare_in
//}}}
);
//{{{ Declare
    iommu_acd_pkg::RISCV_HPMEVT_TYPE                    evt;
//}}}

//{{{ Main Code
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            evt.valid   <= 1'b0;
            evt.did     <= 'd0;
            evt.pv      <= 'd0;
            evt.pid     <= 'd0;
            evt.gv      <= 'd0;
            evt.gscid   <= 'd0;
            evt.pscv    <= 'd0;
            evt.pscid   <= 'd0;
            evt.eventid <= iommu_acd_pkg::HPMEVENTID_DONOTCOUNT;
        end
        else begin
            if(ptw_ack_valid_i & (ptw_ack_i.idx[TLB_QIDX_WIDTH]==1'b0)) begin
                evt.valid   <= 1'b1;
                evt.did     <= entry_info_device_id_i[ptw_ack_i.idx[TLB_QIDX_WIDTH-1:0]];
                evt.pv      <= entry_info_process_id_valid_i[ptw_ack_i.idx[TLB_QIDX_WIDTH-1:0]];
                evt.pid     <= entry_info_process_id_i[ptw_ack_i.idx[TLB_QIDX_WIDTH-1:0]];
                evt.gv      <= (ptw_ack_i.S2MODE=='d0) ? 1'b0 : 1'b1;
                evt.gscid   <= ptw_ack_i.GSCID;
                evt.pscv    <= (ptw_ack_i.PDTV=='d0) ? 1'b0 : 1'b1;
                evt.pscid   <= ptw_ack_i.PSCID;
                evt.eventid <= iommu_acd_pkg::HPMEVENTID_TLB_MISS;
            end
            else begin
                evt.valid   <= 1'b0;
            end
        end
    end
    assign riscv_hpmevt_intf_o.valid   = evt.valid  ;
    assign riscv_hpmevt_intf_o.did     = evt.did    ;
    assign riscv_hpmevt_intf_o.pv      = evt.pv     ;
    assign riscv_hpmevt_intf_o.pid     = evt.pid    ;
    assign riscv_hpmevt_intf_o.gv      = evt.gv     ;
    assign riscv_hpmevt_intf_o.gscid   = evt.gscid  ;
    assign riscv_hpmevt_intf_o.pscv    = evt.pscv   ;
    assign riscv_hpmevt_intf_o.pscid   = evt.pscid  ;
    assign riscv_hpmevt_intf_o.eventid = evt.eventid;
//}}}
endmodule
