///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_t2c.sv
//Version       :   1.0
//Author        :   yangyy
//Description   :
//      xxx     :
//      xxxx    :
//      xx      :
//Increase: 1.1
//
//          1.2
///*********************************************//


module iommu_atd_t2c
    (
    input logic                     iommu_clk,
    input logic                     iommu_rstn,

    //cfg -> acd
    input                           iommu_tr_req_en_i,//rw1c
    input   [51:0]                  iommu_tr_req_iova_i,
    input   [63:0]                  iommu_tr_req_ctl_i,

    input                           iommu_acd_cfg_wvalid_i,
    input   [63:0]                  iommu_acd_cfg_i,


    //flush/invalidation: cq -> acd
    input                           cq_wfifo_wen_i,
    input   [127:0]                 cq_wfifo_wdata_i,
    output                          cq_wfifo_full_o,

    //ptw_ack/ptw_result:ptw -> acd
    input                           ptw_wfifo_wen_i,
    input   [319:0]                 ptw_wfifo_wdata_i,
    output                          ptw_wfifo_full_o,

    input                           atd_t2c_connected_i,
    //inv_ack/fence_ack//used to check timeout
    input                           atd_t2c_cq_ack_i,

    //acd_fault_rpt -> fault_ack
    //from c2t//pulse signal//used to send ack to acd
    input                           atd_fq_ack_valid_i,
    output                          atd_t2c_cfg_end_o,
    output                          atd_t2c_dbg_end_o,
    input                           atd_t2c_cfg_ack_i,
    input                           atd_t2c_dbg_ack_i,
    
    //output to acd
    output                          atd_t2c_tvalid_o,
    input                           atd_t2c_tready_i,
    output   [63:0]                 atd_t2c_tdata_o,
    output   [7:0]                  atd_t2c_tstrb_o,
    output   [7:0]                  atd_t2c_tkeep_o,
    output                          atd_t2c_tlast_o,
    output   [3:0]                  atd_t2c_tid_o,
    output   [3:0]                  atd_t2c_tdest_o,
    output                          atd_t2c_tuser_o
);

   // FSM states
    enum logic [3:0] {
    IDLE        ,
    T2C_CQ_RD ,
    T2C_PTW_RD,
    T2C_CFG_RD,
    T2C_CQ_LAST ,
    T2C_PTW_LAST,
    T2C_CFG_LAST,
    T2C_DBG_LAST,
    T2C_DBG_ACK,
    T2C_FQ_LAST
    } t2c_cs,t2c_ns;

    logic                       cfg_wfifo_wen;
    logic   [63:0]              cfg_wfifo_din;
    logic                       cfg_wfifo_full;
    logic                       cfg_wfifo_empty;
    logic                       cfg_wfifo_ren;
    logic   [63:0]              cfg_wfifo_dout;

    //input from cq and then to acd
//  logic                       cq_wfifo_full;
    logic                       cq_wfifo_empty;
    logic                       cq_wfifo_ren;
    logic   [127:0]             cq_wfifo_dout;
    logic   [3:0]               cq_msg_code;
    logic   [1:0]               cq_func;

    logic                       ptw_wfifo_empty;
    logic                       ptw_wfifo_ren;
    logic   [319:0]             ptw_wfifo_dout;

    logic   [127:0]             cq_wfifo_rdata;
    logic   [255:0]             ptw_wfifo_rdata;

    logic                       iommu_tr_req_en;
    logic   [1:0]               t2c_dbg_cnt;
    logic                       atd_fq_ack_valid;

    logic                       t2c_cq_end;
    logic   [1:0]               t2c_ptw_cnt;
    logic                       atd_tc_connected;
    logic                       atd_t2c_tvalid;
    logic                       atd_t2c_tlast;
    logic   [63:0]              atd_t2c_tdata;
    logic   [5:0]               cq_req_idx;

////////************************************************************************///////
//cq invalid signals to acd
////////************************************************************************///////
atd_sync_fifo
    #(
    .FIFO_WID       (128),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u1_t2c_cq_wfifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (cq_wfifo_wen_i),
    .ren                (cq_wfifo_ren),
    .din                (cq_wfifo_wdata_i),
    .dout               (cq_wfifo_dout),
    .full               (cq_wfifo_full_o),
    .empty              (cq_wfifo_empty)
   );

////////***********************************************************************///////
//ptw result signals to acd
////////***********************************************************************///////
atd_sync_fifo
    #(
    .FIFO_WID       (320),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u2_t2c_ptw_wfifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (ptw_wfifo_wen_i),
    .ren                (ptw_wfifo_ren),
    .din                (ptw_wfifo_wdata_i),
    .dout               (ptw_wfifo_dout),
    .full               (ptw_wfifo_full_o),
    .empty              (ptw_wfifo_empty)
   );



////////***********************************************************************///////
//debug
////////***********************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_tr_req_en <= 1'b0;
    else if (iommu_tr_req_en_i)
        iommu_tr_req_en <= 1'b1;
    else if (t2c_cs == T2C_DBG_LAST)
        iommu_tr_req_en <= 1'b0;
    else
        iommu_tr_req_en <= iommu_tr_req_en;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        t2c_dbg_cnt <= 2'd0;
    else if ((t2c_cs == T2C_DBG_LAST) && atd_t2c_tready_i)
        t2c_dbg_cnt <= t2c_dbg_cnt + 1'b1;
    else if (t2c_cs != T2C_DBG_LAST)
        t2c_dbg_cnt <= 2'd0;
    else
        t2c_dbg_cnt <= t2c_dbg_cnt;
end

////////**********************************************************************///////
//gen fq ack to acd
////////**********************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_fq_ack_valid <= 1'b0;
    else if (atd_fq_ack_valid_i)
        atd_fq_ack_valid <= 1'b1;
    else if (t2c_cs == T2C_FQ_LAST)
        atd_fq_ack_valid <= 1'b0;
    else
        atd_fq_ack_valid <= atd_fq_ack_valid;
end


////////**********************************************************************///////
//atd to acd fsm
////////**********************************************************************///////
assign cq_wfifo_ren = (t2c_cs == T2C_CQ_RD);
assign ptw_wfifo_ren = (t2c_cs == T2C_PTW_RD);

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_wfifo_rdata <= 128'd0;
    else if (cq_wfifo_ren)
        cq_wfifo_rdata <= cq_wfifo_dout;
    else
        cq_wfifo_rdata <= cq_wfifo_rdata;
end

assign cq_msg_code = (cq_wfifo_ren && (cq_wfifo_dout[6:0] == 7'd2)) ? 4'd5 : 4'd4;
assign cq_func = (cq_wfifo_ren && (cq_wfifo_dout[9:7] == 3'd2)) ? 2'd1 : 2'd0;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_wfifo_rdata <= 256'd0;
    else if (ptw_wfifo_ren)
        ptw_wfifo_rdata <= ptw_wfifo_dout[319:64];
    else
        ptw_wfifo_rdata <= ptw_wfifo_rdata;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        t2c_cs <= IDLE;
    else
        t2c_cs <= t2c_ns;
end


always@(*) begin
    case (t2c_cs)
        IDLE:
            if ((!atd_t2c_connected_i) || (atd_t2c_connected_i && !atd_tc_connected))
                t2c_ns = IDLE;
            else if (!cq_wfifo_empty && atd_t2c_tready_i)
                t2c_ns = T2C_CQ_RD;
            else if (!ptw_wfifo_empty && atd_t2c_tready_i)
                t2c_ns = T2C_PTW_RD;
            else if (iommu_acd_cfg_wvalid_i && atd_t2c_tready_i)
                t2c_ns = T2C_CFG_RD;
            else if (iommu_tr_req_en && atd_t2c_tready_i)
                t2c_ns = T2C_DBG_LAST;
            else if (atd_fq_ack_valid)
                t2c_ns = T2C_FQ_LAST;
            else
                t2c_ns = IDLE;
        T2C_CQ_RD:
            t2c_ns = T2C_CQ_LAST;
        T2C_PTW_RD:
            t2c_ns = T2C_PTW_LAST;
        T2C_CFG_RD:
            t2c_ns = T2C_CFG_LAST;
        T2C_CQ_LAST:
            if ((atd_t2c_tready_i && t2c_cq_end) || !atd_t2c_connected_i)
                t2c_ns = IDLE;
            else
                t2c_ns = T2C_CQ_LAST;
        T2C_PTW_LAST:
            if ((atd_t2c_tready_i && (t2c_ptw_cnt == 2'd3)) || !atd_t2c_connected_i)
                t2c_ns = IDLE;
            else
                t2c_ns = T2C_PTW_LAST;
        T2C_CFG_LAST:
            if (atd_t2c_cfg_ack_i)
                t2c_ns = IDLE;
            else
                t2c_ns = T2C_CFG_LAST;
        T2C_DBG_LAST:
            if ((atd_t2c_tready_i && (t2c_dbg_cnt == 2'd2)) || !atd_t2c_connected_i)
                t2c_ns = T2C_DBG_ACK;
            else
                t2c_ns = T2C_DBG_LAST;
        T2C_DBG_ACK:
            if (atd_t2c_dbg_ack_i)
                t2c_ns = IDLE;
            else
                t2c_ns = T2C_DBG_ACK;
        T2C_FQ_LAST:
                t2c_ns = IDLE;
        default:
            t2c_ns = IDLE;
    endcase
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        t2c_cq_end <= 1'b0;
    else if ((t2c_cs == T2C_CQ_LAST) && atd_t2c_tready_i)
        t2c_cq_end <= ~ t2c_cq_end;
    else if (t2c_cs == IDLE)
        t2c_cq_end <= 1'b0;
    else
        t2c_cq_end <= t2c_cq_end;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        t2c_ptw_cnt <= 2'd0;
    else if ((t2c_cs == T2C_PTW_LAST) && atd_t2c_tready_i)
        t2c_ptw_cnt <= t2c_ptw_cnt + 1'b1;
    else if (t2c_cs == IDLE)
        t2c_ptw_cnt <= 2'd0;
    else
        t2c_ptw_cnt <= t2c_ptw_cnt;
end

assign atd_t2c_cfg_end_o = (t2c_cs == T2C_CFG_LAST) && atd_t2c_tready_i && atd_t2c_cfg_ack_i;
assign atd_t2c_dbg_end_o = (t2c_cs == T2C_DBG_ACK) && atd_t2c_dbg_ack_i;

assign atd_t2c_tstrb_o = 8'hff;
assign atd_t2c_tkeep_o = 8'hff;
assign atd_t2c_tid_o = 4'd0;
assign atd_t2c_tdest_o = 4'd0;
assign atd_t2c_tuser_o = 1'b0;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_tc_connected <= 1'b0;
    else
        atd_tc_connected <= atd_t2c_connected_i;
end

always@(*) begin
    if (!iommu_rstn)
        atd_t2c_tvalid = 1'b0;
    else if (!atd_t2c_connected_i)
        atd_t2c_tvalid = 1'b0;
    else if (atd_t2c_connected_i && !atd_tc_connected)//rising_edge
        atd_t2c_tvalid = 1'b1;
    else if ((t2c_cs == T2C_CQ_RD) || (t2c_cs == T2C_CQ_LAST))
        atd_t2c_tvalid = 1'b1;
    else if ((t2c_cs == T2C_PTW_RD) || (t2c_cs == T2C_PTW_LAST))
        atd_t2c_tvalid = 1'b1;
    else if (t2c_cs == T2C_DBG_LAST)
        atd_t2c_tvalid = 1'b1;
    else if (t2c_cs == T2C_CFG_RD)
        atd_t2c_tvalid = 1'b1;
    else if (t2c_cs == T2C_FQ_LAST)
        atd_t2c_tvalid = 1'b1;
    else
        atd_t2c_tvalid = 1'b0;
end


assign atd_t2c_tvalid_o = atd_t2c_tvalid;
assign atd_t2c_tlast_o = atd_t2c_tlast;
assign atd_t2c_tdata_o = atd_t2c_tdata;


always@(*) begin
    if (!iommu_rstn)
        atd_t2c_tlast = 1'b0;
    else if (!atd_t2c_connected_i)
        atd_t2c_tlast = 1'b0;
    else if (atd_t2c_connected_i && !atd_tc_connected)//rising_edge
        atd_t2c_tlast = 1'b1;
    else if ((t2c_cs == T2C_CQ_LAST) && t2c_cq_end)
        atd_t2c_tlast = 1'b1;
    else if ((t2c_cs == T2C_PTW_LAST) && (t2c_ptw_cnt == 2'd3))
        atd_t2c_tlast = 1'b1;
    else if ((t2c_cs == T2C_DBG_LAST) && (t2c_dbg_cnt == 2'd2))
        atd_t2c_tlast = 1'b1;
    else if (t2c_cs == T2C_CFG_RD)
        atd_t2c_tlast = 1'b1;
    else if (t2c_cs == T2C_FQ_LAST)
        atd_t2c_tlast = 1'b1;
    else
        atd_t2c_tlast = 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_req_idx <= 6'd0;
    else if (!atd_t2c_connected_i)
        cq_req_idx <= 6'd0;
    else if (atd_t2c_connected_i && atd_t2c_cq_ack_i)
        cq_req_idx <= cq_req_idx - 1'b1;
    else if (atd_t2c_connected_i && cq_wfifo_ren)
        cq_req_idx <= cq_req_idx + 1'b1;
    else
        cq_req_idx <= cq_req_idx;
end


always@(*) begin
    if (!iommu_rstn)
        atd_t2c_tdata = 64'd0;
    else if (!atd_t2c_connected_i)
        atd_t2c_tdata = 64'd0;
    else if (atd_t2c_connected_i && !atd_tc_connected)//rising_edge
        atd_t2c_tdata = {12'd1,12'd1,32'd0,4'd1,4'd0};
    else if (t2c_cs == T2C_CQ_RD)
        atd_t2c_tdata = {52'd0,cq_func,cq_req_idx,cq_msg_code};
    else if ((t2c_cs == T2C_CQ_LAST) && t2c_cq_end)
        atd_t2c_tdata = cq_wfifo_rdata[127:64];
    else if (t2c_cs == T2C_CQ_LAST)
        atd_t2c_tdata = cq_wfifo_rdata[63:0];
    else if (t2c_cs == T2C_PTW_RD)
        atd_t2c_tdata = ptw_wfifo_dout[63:0];
    else if (t2c_cs == T2C_PTW_LAST)
        case (t2c_ptw_cnt)
            2'd0:
                atd_t2c_tdata = ptw_wfifo_rdata[63:0];
            2'd1:
                atd_t2c_tdata = ptw_wfifo_rdata[127:64];
            2'd2:
                atd_t2c_tdata = ptw_wfifo_rdata[191:128];
            2'd3:
                atd_t2c_tdata = ptw_wfifo_rdata[255:192];
            default:
                atd_t2c_tdata = 64'd0;
        endcase
    else if (t2c_cs == T2C_DBG_LAST)
        case (t2c_dbg_cnt)
            2'd0:
                atd_t2c_tdata = {60'd0,4'hf};
            2'd1:
                atd_t2c_tdata = {iommu_tr_req_iova_i,12'd0};
            2'd2:
                atd_t2c_tdata = iommu_tr_req_ctl_i;
            default:
                atd_t2c_tdata = 64'd0;
        endcase
    else if (t2c_cs == T2C_CFG_RD)
        atd_t2c_tdata = iommu_acd_cfg_i;
    else if (t2c_cs == T2C_FQ_LAST)
        atd_t2c_tdata = {60'd0,4'd8};
    else
        atd_t2c_tdata = 64'd0;
end



endmodule



