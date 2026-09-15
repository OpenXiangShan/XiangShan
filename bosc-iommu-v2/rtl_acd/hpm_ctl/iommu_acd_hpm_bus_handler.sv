module iommu_acd_hpm_bus_handler(
//{{{ IO
    input  logic                    clk,
    input  logic                    rstn,
    //
    input  logic                    slv_awvalid_i,
    input  logic                    slv_awready_i,
    input  logic                    slv_arvalid_i,
    input  logic                    slv_arready_i,
    //
    input  logic                    mst_awvalid_i,
    input  logic                    mst_awready_i,
    input  logic                    mst_arvalid_i,
    input  logic                    mst_arready_i,
    //
    input  logic [3:0]              hpm_cnt_inhibit_i,
    output logic [63:0]             hpm_cnt_o[3:0],
    //
    input  logic                    spare_in
//}}}
);
//{{{ Declare
    logic [63:0]                    slv_aw_cnt, slv_ar_cnt, mst_aw_cnt, mst_ar_cnt;
//}}}

//{{{ Main Code
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_aw_cnt  <= 1'b0;
    else begin
        if(~hpm_cnt_inhibit_i[0] & slv_awvalid_i & slv_awready_i)
            slv_aw_cnt <= slv_aw_cnt + 'd1;
    end
end
assign hpm_cnt_o[0] = slv_aw_cnt;

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_ar_cnt  <= 1'b0;
    else begin
        if(~hpm_cnt_inhibit_i[1] & slv_arvalid_i & slv_arready_i)
            slv_ar_cnt <= slv_ar_cnt + 'd1;
    end
end
assign hpm_cnt_o[1] = slv_ar_cnt;

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_aw_cnt  <= 1'b0;
    else begin
        if(~hpm_cnt_inhibit_i[2] & mst_awvalid_i & mst_awready_i)
            mst_aw_cnt <= mst_aw_cnt + 'd1;
    end
end
assign hpm_cnt_o[2] = mst_aw_cnt;

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_ar_cnt  <= 1'b0;
    else begin
        if(~hpm_cnt_inhibit_i[3] & mst_arvalid_i & mst_arready_i)
            mst_ar_cnt <= mst_ar_cnt + 'd1;
    end
end
assign hpm_cnt_o[3] = mst_ar_cnt;


//}}}


endmodule



module iommu_acd_riscv_hpm_bus_handler #(
    parameter type          TRANSLATE_REQ_TYPE          = iommu_acd_pkg::TRANSLATE_REQ_TYPE
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    
    input  logic                                        translate_req_valid_i,
    input  logic                                        translate_req_ready_i,
    input  TRANSLATE_REQ_TYPE                           translate_req_i,
    
    output iommu_acd_pkg::RISCV_HPMEVT_TYPE             riscv_hpmevt_intf_o,
    
    input  logic                                        spare_in
//}}}
);
//{{{ Declare
    iommu_acd_pkg::RISCV_HPMEVT_TYPE evt;
//}}}

//{{{ MainCode
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
            if(translate_req_valid_i & translate_req_ready_i) begin
                evt.valid   <= 1'b1;
                evt.did     <= translate_req_i.device_id;
                evt.pv      <= translate_req_i.process_id_valid;
                evt.pid     <= translate_req_i.process_id;
                evt.gv      <= 'd0;
                evt.gscid   <= 'd0;
                evt.pscv    <= 'd0;
                evt.pscid   <= 'd0;
                evt.eventid <= translate_req_i.is_translated ? iommu_acd_pkg::HPMEVENTID_TRANSLATED_REQUEST : iommu_acd_pkg::HPMEVENTID_UNTRANSLATED_REQUEST;
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
