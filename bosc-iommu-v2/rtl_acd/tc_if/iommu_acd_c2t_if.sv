module iommu_acd_c2t_if import iommu_acd_pkg::*; #(
    parameter   INV_IDX_WIDTH               = 4,                    // should not bigger than 4
    parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH,
    parameter   FAULT_TOKEN_WIDTH           = 5,                    // should not bigger than 12
    parameter   FAULT_INFLY_NUM             = 2**FAULT_TOKEN_WIDTH,
    parameter   PTW_IDX_WIDTH               = 8,                    // should not bigger than 12
    parameter   PTW_INFLY_NUM               = 2**PTW_IDX_WIDTH,
    parameter   CTIF_DATA_WIDTH             = 64,
    parameter   CTIF_STRB_WIDTH             = CTIF_DATA_WIDTH/8,
    parameter   CTIF_ID_WIDTH               = 4,
    parameter   CTIF_DEST_WIDTH             = 4,
    parameter   CTIF_USER_WIDTH             = 1,
    parameter   SPARE_PARAM                 = 0
)(                                                      
//{{{ IO                                                
    input  logic                                        clk,
    input  logic                                        rstn,
    // C2T                                              
    output logic                                        c2t_tvalid_o,
    input  logic                                        c2t_tready_i,
    output logic [CTIF_DATA_WIDTH-1:0]                  c2t_tdata_o,
    output logic [CTIF_STRB_WIDTH-1:0]                  c2t_tstrb_o, //
    output logic [CTIF_STRB_WIDTH-1:0]                  c2t_tkeep_o, //
    output logic                                        c2t_tlast_o,
    output logic [CTIF_ID_WIDTH-1:0]                    c2t_tid_o, //
    output logic [CTIF_DEST_WIDTH-1:0]                  c2t_tdest_o, //
    output logic [CTIF_USER_WIDTH-1:0]                  c2t_tuser_o, //
    // PTW                                              
    input  logic                                        msg_pvalid_i,
    output logic                                        msg_pready_o,
    input  logic [63:0]                                 msg_pdata_i,
    input  logic                                        msg_plast_i,
    // INV                                              
    input  logic                                        msg_ivalid_i,
    output logic                                        msg_iready_o,
    input  MSG_INV_ACK_TYPE                             msg_idata_i,
    input  logic                                        msg_ilast_i,
    // FAULT                                            
    input  logic                                        msg_fvalid_i,
    output logic                                        msg_fready_o,
    input  logic [63:0]                                 msg_fdata_i,
    input  logic                                        msg_flast_i,
    // CFG                                              
    input  logic                                        msg_rvalid_i,
    output logic                                        msg_rready_o,
    input  MSG_CFG_ACK_TYPE                             msg_rdata_i,
    input  logic                                        msg_rlast_i,
    // INTERRUPT
    input  logic                                        msg_nvalid_i,
    output logic                                        msg_nready_o,
    input  MSG_INT_TYPE                                 msg_ndata_i,
    input  logic                                        msg_nlast_i,
    // DBG
    input  logic                                        msg_gvalid_i,
    output logic                                        msg_gready_o,
    input  MSG_DBG_ACK_TYPE                             msg_gdata_i,
    input  logic                                        msg_glast_i,
    //                                                  
    input  logic [CTIF_ID_WIDTH-1:0]                    acd_tid_i,
    input  logic [CTIF_DEST_WIDTH-1:0]                  acd_tdest_i,
    input  logic                                        msg_connect_ack_got_i,
    output logic                                        tc_if_is_connected_o,
    //                                                  
    input  logic                                        spare_in         
//}}}
);
//=== Declare {{{
    localparam S_IDLE                                   = 4'b0000;
    localparam S_INIT                                   = 4'b0001;
    localparam S_WAIT_FLAST                             = 4'b1000;
    localparam S_WAIT_ILAST                             = 4'b0100;
    localparam S_WAIT_PLAST                             = 4'b0010;
    localparam S_WAIT_RLAST                             = 4'b0110;
    localparam S_WAIT_NLAST                             = 4'b1110;
    localparam S_CONNECT                                = 4'b0101;
    localparam S_WAIT_GLAST                             = 4'b1111;
    logic [3:0]                                         cs, ns;
                                                        
    logic                                               connected;
    logic [3:0]                                         init_cnt;
//}}}

//=== MainCode {{{
    assign c2t_tstrb_o = {CTIF_STRB_WIDTH{1'b1}};
    assign c2t_tkeep_o = c2t_tstrb_o;
    assign c2t_tid_o   = acd_tid_i;
    assign c2t_tdest_o = acd_tdest_i;
    assign c2t_tuser_o = 'd0;

//=== FSM Stage1 {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_INIT;
        else
            cs <= ns;
    end
//}}}

//=== FSM Stage2 {{{
    always@(*) begin
        ns = cs;
        case(cs)
        S_INIT: begin
            if(init_cnt=='hf)
                ns = S_IDLE;
            else
                ns = S_INIT;
        end
        S_IDLE: begin
            if(connected) begin //{{{
                if(msg_nvalid_i) begin
                    if(c2t_tready_i) begin
                        if(c2t_tready_i)
                            ns = S_IDLE;
                        else 
                            ns = S_WAIT_NLAST;
                    end
                    else begin
                        ns = S_WAIT_NLAST;
                    end
                end
                else if(msg_fvalid_i) begin
                    if(c2t_tready_i) begin
                        if(msg_flast_i)
                            ns = S_IDLE;
                        else
                            ns = S_WAIT_FLAST;
                    end
                    else begin
                        ns = S_WAIT_FLAST;
                    end
                end
                else if(msg_ivalid_i) begin
                    if(c2t_tready_i) begin
                        if(msg_ilast_i)
                            ns = S_IDLE;
                        else
                            ns = S_WAIT_ILAST;
                    end
                    else begin
                        ns = S_WAIT_ILAST;
                    end
                end
                else if(msg_pvalid_i) begin
                    if(c2t_tready_i) begin
                        if(msg_plast_i)
                            ns = S_IDLE;
                        else
                            ns = S_WAIT_PLAST;
                    end
                    else begin
                        ns = S_WAIT_PLAST;
                    end
                end
                else if(msg_rvalid_i) begin
                    if(c2t_tready_i) begin
                        if(msg_rlast_i)
                            ns = S_IDLE;
                        else
                            ns = S_WAIT_RLAST;
                    end
                    else begin
                        ns = S_WAIT_RLAST;
                    end
                end
                else if(msg_gvalid_i) begin
                    if(c2t_tready_i) begin
                        if(msg_glast_i)
                            ns = S_IDLE;
                        else
                            ns = S_WAIT_GLAST;
                    end
                end
                else begin
                    ns = S_IDLE;
                end
            end //}}}
            else begin
                if(c2t_tready_i)
                    ns = S_CONNECT;
                else
                    ns = S_IDLE;
            end
        end
        S_CONNECT: begin
            if(msg_connect_ack_got_i)
                ns = S_IDLE;
            else
                ns = S_CONNECT;
        end
        S_WAIT_FLAST: begin
            if(msg_fvalid_i & c2t_tready_i & msg_flast_i)
                ns = S_IDLE;
            else
                ns = S_WAIT_FLAST;
        end
        S_WAIT_ILAST: begin
            if(msg_ivalid_i & c2t_tready_i & msg_ilast_i)
                ns = S_IDLE;
            else
                ns = S_WAIT_ILAST;
        end
        S_WAIT_PLAST: begin
            if(msg_pvalid_i & c2t_tready_i & msg_plast_i)
                ns = S_IDLE;
            else
                ns = S_WAIT_PLAST;
        end
        S_WAIT_RLAST: begin
            if(msg_rvalid_i & c2t_tready_i & msg_rlast_i)
                ns = S_IDLE;
            else
                ns = S_WAIT_RLAST;
        end
        S_WAIT_NLAST: begin
            if(msg_nvalid_i & c2t_tready_i & msg_nlast_i)
                ns = S_IDLE;
            else
                ns = S_WAIT_NLAST;
        end
        S_WAIT_GLAST: begin
            if(msg_gvalid_i & c2t_tready_i & msg_glast_i)
                ns = S_IDLE;
            else
                ns = S_WAIT_GLAST;
        end
        default: ns = S_IDLE;
        endcase
    end
//}}}

//=== FSM Stage3 {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            init_cnt <= 'd0;
        else if(cs==S_INIT)
            init_cnt <= init_cnt + 'd1;
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            connected <= 1'b0;
        else if(msg_connect_ack_got_i & (cs==S_CONNECT))
            connected <= 1'b1;
    end
    
    assign tc_if_is_connected_o = connected;

    assign c2t_tvalid_o = (cs==S_IDLE & ~connected)                                           ? 1'b1         :
                          (cs==S_WAIT_NLAST | ns==S_WAIT_NLAST | (cs==S_IDLE & msg_nvalid_i)) ? msg_nvalid_i :
                          (cs==S_WAIT_FLAST | ns==S_WAIT_FLAST | (cs==S_IDLE & msg_fvalid_i)) ? msg_fvalid_i :
                          (cs==S_WAIT_ILAST | ns==S_WAIT_ILAST | (cs==S_IDLE & msg_ivalid_i)) ? msg_ivalid_i :
                          (cs==S_WAIT_PLAST | ns==S_WAIT_PLAST | (cs==S_IDLE & msg_pvalid_i)) ? msg_pvalid_i :
                          (cs==S_WAIT_RLAST | ns==S_WAIT_RLAST | (cs==S_IDLE & msg_rvalid_i)) ? msg_rvalid_i :
                          (cs==S_WAIT_GLAST | ns==S_WAIT_GLAST | (cs==S_IDLE & msg_gvalid_i)) ? msg_gvalid_i :
                          1'b0;

    assign c2t_tdata_o =  (cs==S_IDLE & ~connected)                                           ? {12'(PTW_INFLY_NUM-1), 12'(FAULT_INFLY_NUM-1), 8'b0, 4'(INV_INFLY_NUM-1), 19'd0, 1'b0, 4'd1, MSGCODE_CONNECT_REQ} :
                          (cs==S_WAIT_NLAST | ns==S_WAIT_NLAST | (cs==S_IDLE & msg_nvalid_i)) ? msg_ndata_i :
                          (cs==S_WAIT_FLAST | ns==S_WAIT_FLAST | (cs==S_IDLE & msg_fvalid_i)) ? msg_fdata_i :
                          (cs==S_WAIT_ILAST | ns==S_WAIT_ILAST | (cs==S_IDLE & msg_ivalid_i)) ? msg_idata_i :
                          (cs==S_WAIT_PLAST | ns==S_WAIT_PLAST | (cs==S_IDLE & msg_pvalid_i)) ? msg_pdata_i :
                          (cs==S_WAIT_RLAST | ns==S_WAIT_RLAST | (cs==S_IDLE & msg_rvalid_i)) ? msg_rdata_i :
                          (cs==S_WAIT_GLAST | ns==S_WAIT_GLAST | (cs==S_IDLE & msg_gvalid_i)) ? msg_gdata_i :
                          'd0;

    assign c2t_tlast_o =  (cs==S_IDLE & ~connected)                                           ? 1'b1        :
                          (cs==S_WAIT_NLAST | ns==S_WAIT_NLAST | (cs==S_IDLE & msg_nvalid_i)) ? msg_nlast_i :
                          (cs==S_WAIT_FLAST | ns==S_WAIT_FLAST | (cs==S_IDLE & msg_fvalid_i)) ? msg_flast_i :
                          (cs==S_WAIT_ILAST | ns==S_WAIT_ILAST | (cs==S_IDLE & msg_ivalid_i)) ? msg_ilast_i :
                          (cs==S_WAIT_PLAST | ns==S_WAIT_PLAST | (cs==S_IDLE & msg_pvalid_i)) ? msg_plast_i :
                          (cs==S_WAIT_RLAST | ns==S_WAIT_RLAST | (cs==S_IDLE & msg_rvalid_i)) ? msg_rlast_i :
                          (cs==S_WAIT_GLAST | ns==S_WAIT_GLAST | (cs==S_IDLE & msg_gvalid_i)) ? msg_glast_i :
                          1'b0;

    assign msg_nready_o = (cs==S_WAIT_NLAST | ns==S_WAIT_NLAST | (cs==S_IDLE & connected &  msg_nvalid_i                                                                                )) ? c2t_tready_i : 1'b0;
    assign msg_fready_o = (cs==S_WAIT_FLAST | ns==S_WAIT_FLAST | (cs==S_IDLE & connected & ~msg_nvalid_i &  msg_fvalid_i                                                                )) ? c2t_tready_i : 1'b0;
    assign msg_iready_o = (cs==S_WAIT_ILAST | ns==S_WAIT_ILAST | (cs==S_IDLE & connected & ~msg_nvalid_i & ~msg_fvalid_i &  msg_ivalid_i                                                )) ? c2t_tready_i : 1'b0;
    assign msg_pready_o = (cs==S_WAIT_PLAST | ns==S_WAIT_PLAST | (cs==S_IDLE & connected & ~msg_nvalid_i & ~msg_fvalid_i & ~msg_ivalid_i &  msg_pvalid_i                                )) ? c2t_tready_i : 1'b0;
    assign msg_rready_o = (cs==S_WAIT_RLAST | ns==S_WAIT_RLAST | (cs==S_IDLE & connected & ~msg_nvalid_i & ~msg_fvalid_i & ~msg_ivalid_i & ~msg_pvalid_i &  msg_rvalid_i                )) ? c2t_tready_i : 1'b0;
    assign msg_gready_o = (cs==S_WAIT_GLAST | ns==S_WAIT_GLAST | (cs==S_IDLE & connected & ~msg_nvalid_i & ~msg_fvalid_i & ~msg_ivalid_i & ~msg_pvalid_i & ~msg_rvalid_i & msg_gvalid_i )) ? c2t_tready_i : 1'b0;

//}}}

//}}}
endmodule
