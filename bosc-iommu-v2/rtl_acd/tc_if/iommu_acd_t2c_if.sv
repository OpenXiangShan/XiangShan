


module iommu_acd_t2c_if import iommu_acd_pkg::*; #(
    parameter   CTIF_DATA_WIDTH     = 64,
    parameter   CTIF_STRB_WIDTH     = CTIF_DATA_WIDTH/8,
    parameter   CTIF_ID_WIDTH       = 4,
    parameter   CTIF_DEST_WIDTH     = 4,
    parameter   CTIF_USER_WIDTH     = 1,
    parameter   SPARE_PARAM         = 0
)(
//{{{ IO
    input  logic                                clk,
    input  logic                                rstn,
    // T2C
    input  logic                                t2c_tvalid_i,
    output logic                                t2c_tready_o,
    input  logic [CTIF_DATA_WIDTH-1:0]          t2c_tdata_i,
    input  logic [CTIF_STRB_WIDTH-1:0]          t2c_tstrb_i,
    input  logic [CTIF_STRB_WIDTH-1:0]          t2c_tkeep_i,
    input  logic                                t2c_tlast_i,
    input  logic [CTIF_ID_WIDTH-1:0]            t2c_tid_i,
    input  logic [CTIF_DEST_WIDTH-1:0]          t2c_tdest_i,
    input  logic [CTIF_USER_WIDTH-1:0]          t2c_tuser_i,
    // PTW
    output logic                                msg_pvalid_o,
    input  logic                                msg_pready_i,
    output logic [63:0]                         msg_pwdata_o,
    // INV
    output logic                                msg_ivalid_o,
    input  logic                                msg_iready_i,
    output logic [63:0]                         msg_iwdata_o,
    // FAULT
    output logic                                msg_fvalid_o,
    input  logic                                msg_fready_i,
    output MSG_FAULT_ACK_TYPE                   msg_fwdata_o,
    // CFG
    output logic                                msg_cvalid_o,
    input  logic                                msg_cready_i,
    output MSG_CFG_ACCESS_TYPE                  msg_cwdata_o,
    // DBG
    output logic                                msg_gvalid_o,
    input  logic                                msg_gready_i,
    output logic [63:0]                         msg_gwdata_o,
    //
    input  logic                                tc_if_is_connected_i,
    output logic                                msg_connect_ack_got_o,
    //
    input  logic                                spare_in         
//}}}
);
//=== Declare {{{
    localparam S_IDLE                           = 4'b0000;
    localparam S_WAIT_LAST                      = 4'b0001;
    localparam S_WAIT_FLAST                     = 4'b1000;
    localparam S_WAIT_ILAST                     = 4'b0100;
    localparam S_WAIT_PLAST                     = 4'b0010;
    localparam S_WAIT_RLAST                     = 4'b0110;
    localparam S_WAIT_GLAST                     = 4'b1111;
    logic [3:0]                                 cs, ns;

    logic [3:0]                                 msg_code;
    logic [3:0]                                 msg_opcode;
    logic                                       connected;
    logic                                       msg_connect_ack_got;
//}}}

//=== MainCode {{{
    assign msg_code = t2c_tdata_i[3:0];
    assign msg_opcode = t2c_tdata_i[7:4];
    assign connected = tc_if_is_connected_i;

//=== FSM Stage1 {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
//}}}

//=== FSM Stage2 {{{
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            if(connected) begin
                if(t2c_tvalid_i) begin
                    case(msg_code)
                    MSGCODE_CFG_ACCESS: begin
                        if(~t2c_tready_o)
                            ns = S_WAIT_RLAST;
                        else if(~t2c_tlast_i)
                            ns = S_WAIT_RLAST;
                        else
                            ns = S_IDLE;
                    end
                    MSGCODE_FAULT_ACK: begin
                        if(~t2c_tready_o)
                            ns = S_WAIT_FLAST;
                        else if(~t2c_tlast_i)
                            ns = S_WAIT_FLAST;
                        else
                            ns = S_IDLE;
                    end
                    MSGCODE_INV_REQ, MSGCODE_FENCE_REQ: begin
                        if(~t2c_tready_o)
                            ns = S_WAIT_ILAST;
                        else if(~t2c_tlast_i)
                            ns = S_WAIT_ILAST;
                        else
                            ns = S_IDLE;
                    end
                    MSGCODE_PTW_ACK: begin
                        if(~t2c_tready_o)
                            ns = S_WAIT_PLAST;
                        else if(~t2c_tlast_i)
                            ns = S_WAIT_PLAST;
                        else
                            ns = S_IDLE;
                    end
                    MSGCODE_DBG: begin
                        if(~t2c_tready_o)
                            ns = S_WAIT_GLAST;
                        else if(~t2c_tlast_i)
                            ns = S_WAIT_GLAST;
                        else
                            ns = S_IDLE;
                    end
                    default: ns = S_WAIT_LAST;
                    endcase
                end
                else
                    ns = S_IDLE;
            end
            else begin
                ns = S_IDLE;
            end
        end
        S_WAIT_FLAST, S_WAIT_ILAST, S_WAIT_PLAST, S_WAIT_RLAST, S_WAIT_LAST, S_WAIT_GLAST: begin
            if(t2c_tvalid_i & t2c_tready_o & t2c_tlast_i)
                ns = S_IDLE;
            else
                ns = cs;
        end
        default: ns = S_IDLE;
        endcase
    end
//}}}

//=== msg demux {{{
    assign msg_pvalid_o = (cs==S_WAIT_PLAST | ns==S_WAIT_PLAST | (cs==S_IDLE & msg_code==MSGCODE_PTW_ACK))                                  ? t2c_tvalid_i : 1'b0;
    assign msg_ivalid_o = (cs==S_WAIT_ILAST | ns==S_WAIT_ILAST | (cs==S_IDLE & (msg_code==MSGCODE_INV_REQ | msg_code==MSGCODE_FENCE_REQ)))  ? t2c_tvalid_i : 1'b0;
    assign msg_fvalid_o = (cs==S_WAIT_FLAST | ns==S_WAIT_FLAST | (cs==S_IDLE & msg_code==MSGCODE_FAULT_ACK))                                ? t2c_tvalid_i : 1'b0;
    assign msg_cvalid_o = (cs==S_WAIT_RLAST | ns==S_WAIT_RLAST | (cs==S_IDLE & msg_code==MSGCODE_CFG_ACCESS))                               ? t2c_tvalid_i : 1'b0;
    assign msg_gvalid_o = (cs==S_WAIT_GLAST | ns==S_WAIT_GLAST | (cs==S_IDLE & msg_code==MSGCODE_DBG))                                      ? t2c_tvalid_i : 1'b0;

    assign msg_pwdata_o = (cs==S_WAIT_PLAST | ns==S_WAIT_PLAST | (cs==S_IDLE & msg_code==MSGCODE_PTW_ACK))                                  ? t2c_tdata_i : 'd0;
    assign msg_iwdata_o = (cs==S_WAIT_ILAST | ns==S_WAIT_ILAST | (cs==S_IDLE & (msg_code==MSGCODE_INV_REQ | msg_code==MSGCODE_FENCE_REQ)))  ? t2c_tdata_i : 'd0;
    assign msg_fwdata_o = (cs==S_WAIT_FLAST | ns==S_WAIT_FLAST | (cs==S_IDLE & msg_code==MSGCODE_FAULT_ACK))                                ? t2c_tdata_i : 'd0;
    assign msg_cwdata_o = (cs==S_WAIT_RLAST | ns==S_WAIT_RLAST | (cs==S_IDLE & msg_code==MSGCODE_CFG_ACCESS))                               ? t2c_tdata_i : 'd0;
    assign msg_gwdata_o = (cs==S_WAIT_GLAST | ns==S_WAIT_GLAST | (cs==S_IDLE & msg_code==MSGCODE_DBG))                                      ? t2c_tdata_i : 'd0;

// timing loop as FM's ns use t2c_tready_o
//    assign t2c_tready_o = (cs==S_WAIT_PLAST | ns==S_WAIT_PLAST | (cs==S_IDLE & msg_code==MSGCODE_PTW_ACK))                                  ? msg_pready_i : 
//                          (cs==S_WAIT_ILAST | ns==S_WAIT_ILAST | (cs==S_IDLE & (msg_code==MSGCODE_INV_REQ | msg_code==MSGCODE_FENCE_REQ)))  ? msg_iready_i : 
//                          (cs==S_WAIT_FLAST | ns==S_WAIT_FLAST | (cs==S_IDLE & msg_code==MSGCODE_FAULT_ACK))                                ? msg_fready_i : 
//                          (cs==S_WAIT_RLAST | ns==S_WAIT_RLAST | (cs==S_IDLE & msg_code==MSGCODE_CFG_ACCESS))                               ? msg_cready_i : 1'b1;
assign t2c_tready_o = cs==S_IDLE        ?   (
                                              msg_code==MSGCODE_PTW_ACK                                 ? msg_pready_i :
                                             (msg_code==MSGCODE_INV_REQ | msg_code==MSGCODE_FENCE_REQ)  ? msg_iready_i :
                                              msg_code==MSGCODE_FAULT_ACK                               ? msg_fready_i :
                                              msg_code==MSGCODE_CFG_ACCESS                              ? msg_cready_i :
                                              msg_code==MSGCODE_DBG                                     ? msg_gready_i :
                                              1'b1
                                            )            :
                      cs==S_WAIT_PLAST  ?   msg_pready_i :
                      cs==S_WAIT_ILAST  ?   msg_iready_i :
                      cs==S_WAIT_FLAST  ?   msg_fready_i :
                      cs==S_WAIT_RLAST  ?   msg_cready_i :
                      cs==S_WAIT_GLAST  ?   msg_gready_i :
                      cs==S_WAIT_LAST   ?   1'b1         :
                                            1'b1         ;

//}}}

    assign msg_connect_ack_got = ~connected & t2c_tvalid_i & t2c_tready_o & t2c_tlast_i & (msg_code==MSGCODE_CONNECT_ACK) & (msg_opcode == 4'd1);
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            msg_connect_ack_got_o <= 1'b0;
        else begin
            if(msg_connect_ack_got)
                msg_connect_ack_got_o <= 1'b1;
        end
    end

//}}}


endmodule
