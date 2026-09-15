module iommu_acd_int import iommu_acd_pkg::*; #(
    parameter   SPARE_PARAM     = 0
)(
//{{{ IO
    input  logic                            clk,
    input  logic                            rstn,
    // CFG
    input  logic [31:1]                     iocountovf_pos_i,
    input  logic [1:0]                      ecc_err_pos_i,
    // C2T_IF
    output logic                            msg_nvalid_o,
    input  logic                            msg_nready_i,
    output MSG_INT_TYPE                     msg_ndata_o,
    output logic                            msg_nlast_o,
    //
    input  logic                            spare_in
//}}}
);
//=== Declare === {{{
    localparam S_IDLE = 1'b0;
    localparam S_SEND = 1'b1;
    logic [59:0]                            interrupt_tmp_status;
    //logic [59:0]                            interrupt_send_req;

    logic                                   cs,ns;
//}}}

//=== Main Code === {{{
genvar i;
generate
    assign interrupt_tmp_status[0] = 1'b0;
    //assign interrupt_send_req[0]   = 1'b0;
    for(i=1; i<32; i++) begin : interrupt_tmp_status_gen_for_hpmovf
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                interrupt_tmp_status[i] <= 1'b0;
            else begin
                if(iocountovf_pos_i[i])
                    interrupt_tmp_status[i] <= 1'b1;
                else if(msg_nvalid_o & msg_nready_i & msg_ndata_o[4+i]==1'b1)
                    interrupt_tmp_status[i] <= 1'b0;
            end
        end
        //assign interrupt_send_req[i] = iocountovf_pos_i[i];
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            interrupt_tmp_status[32] <= 1'b0;
        else begin
            if(ecc_err_pos_i[0])
                interrupt_tmp_status[32] <= 1'b1;
            else if(msg_nvalid_o & msg_nready_i & msg_ndata_o[4+32]==1'b1)
                interrupt_tmp_status[32] <= 1'b0;
        end
    end
    //assign interrupt_send_req[32] = ecc_err_pos_i[0];

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            interrupt_tmp_status[33] <= 1'b0;
        else begin
            if(ecc_err_pos_i[1])
                interrupt_tmp_status[33] <= 1'b1;
            else if(msg_nvalid_o & msg_nready_i & msg_ndata_o[4+33]==1'b1)
                interrupt_tmp_status[33] <= 1'b0;
        end
    end
    //assign interrupt_send_req[33] = ecc_err_pos_i[1];

    for(i=34; i<60; i++) begin
        assign interrupt_tmp_status[i] = 1'b0;
        //assign interrupt_send_req[i]   = 1'b0;
    end
endgenerate

//    always@(posedge clk or negedge rstn) begin
//        if(~rstn)
//            msg_nvalid_o <= 1'b0;
//        else begin
//            if(msg_nvalid_o & ~msg_nready_i)
//                msg_nvalid_o <= 1'b1;
//            else if(|interrupt_send_req)
//                msg_nvalid_o <= 1'b1;
//            else
//                msg_nvalid_o <= 1'b0;
//        end
//    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end

    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE  : begin
            if(|interrupt_tmp_status)
                ns = S_SEND;
            else
                ns = S_IDLE;
        end
        S_SEND  :
            if(msg_nready_i)
                ns = S_IDLE;
            else
                ns = S_SEND;
        default : ns = cs;
        endcase
    end

//    assign msg_ndata_o = {interrupt_tmp_status, MSGCODE_INTERRUPT};
//    assign msg_nlast_o = 1'b1;
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            msg_nvalid_o    <= 1'b0;
            msg_ndata_o     <= 'd0;
        end
        else begin
            if(ns == S_SEND) begin
                msg_nvalid_o    <= 1'b1;
                msg_ndata_o     <= {interrupt_tmp_status, iommu_acd_pkg::MSGCODE_INTERRUPT};
            end
            else if(ns == S_IDLE) begin
                msg_nvalid_o    <= 1'b0;
            end
        end
    end
    assign msg_nlast_o  = 1'b1;

//}}}
endmodule
