///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_t2r.sv
//Version       :   2.0
//Author        :   yangyy
//Description   :
//      xxx     :
//      xxxx    :
//      xx      :
//Increase: 1.1
//
//          1.2
///*********************************************//

module iommu_atd_t2r
    (
    input                           iommu_clk,
    input                           iommu_rstn,

    //DTI_ATS_CONDIS_REQ
    input                           atd_t2r_connected_i,
    input   [7:0]                   atd_t2r_tok_trans_req_i,

    //DTI_ATS_TRANS_RESP
    //ats_ptw_result:ptw -> pcie-rc
    input                           ats_resp_wen_i,
    input   [113:0]                 ats_resp_wdata_i,
    output                          ats_resp_full_o,

    //DTI_ATS_TRANS_FAULT
    input                           ats_fault_wen_i,
    input   [15:0]                  ats_fault_wdata_i,
    output                          ats_fault_full_o,

    //DTI_ATS_INV_REQ
    //flush/invalidation: cq -> pcie-rc -> atc
    input                           ats_inv_wen_i,
    input   [127:0]                 ats_inv_wdata_i,
    output                          ats_inv_full_o,

    //DTI_ATS_INV_ACK
    //pulse signal,calc timeout
    input                           ats_inv_ack_i,

    //DTI_ATS_SYNC_REQ//from cq
    input                           ats_sync_req_i,
    //pulse signal,calc timeout//from r2t
    input                           ats_sync_ack_i,

    //DTI_ATS_PAGE_RESP
    //ats_pq_result:pq -> pcie-rc
    input                           pri_resp_wen_i,
    input   [79:0]                  pri_resp_wdata_i,
    output                          pri_resp_full_o,

    //DTI_ATS_PAGE_ACK//pulse
    //DTI_ATS_PAGE_RESPACK//pulse
    input                           ats_page_ack_i,
    input                           ats_page_respack_i,

    //output to pcie-rc
    output                          atd_t2r_tvalid_o,
    input                           atd_t2r_tready_i,
    output   [63:0]                 atd_t2r_tdata_o,
    output   [7:0]                  atd_t2r_tstrb_o,
    output   [7:0]                  atd_t2r_tkeep_o,
    output                          atd_t2r_tlast_o,
    output   [3:0]                  atd_t2r_tid_o
);

    localparam  logic   [3:0]   DTI_ATSv1   = 4'b0000;
    localparam  logic   [3:0]   DTI_ATSv2   = 4'b0001;
    localparam  logic   [3:0]   DTI_ATSv3   = 4'b0010;
    localparam  logic   [3:0]   DTI_ATSv1_OAS   = 4'b0000;//4GB

    localparam  logic   [3:0]   CONDIS_ACK  = 4'b0000;
    localparam  logic   [3:0]   TRANS_FAULT = 4'b0001;
    localparam  logic   [3:0]   TRANS_RESP  = 4'b0010;
    localparam  logic   [3:0]   INV_REQ     = 4'b1100;
    localparam  logic   [3:0]   SYNC_REQ    = 4'b1101;
    localparam  logic   [3:0]   PAGE_ACK    = 4'b1000;
    localparam  logic   [3:0]   PAGE_RESP   = 4'b1001;


   // FSM states
    enum logic [3:0] {
    IDLE        ,
    T2R_INV_RD,
    T2R_FAULT_RD,
    T2R_ATS_RD,
    T2R_PRI_RD,
    T2R_INV_LAST,
    T2R_FAULT_LAST,
    T2R_ATS_LAST ,
    T2R_PRI_LAST,
    T2R_SYNC_LAST,
    T2R_PG_ACK_LAST
    } t2r_cs,t2r_ns;


    logic                       ats_fault_empty;
    logic                       ats_fault_ren;
    logic   [15:0]              ats_fault_dout;
    //input from cq and then to acd
    logic                       ats_inv_full;
    logic                       ats_inv_empty;
    logic                       ats_inv_ren;
    logic   [127:0]             ats_inv_dout;

    logic                       pri_resp_full;
    logic                       pri_resp_empty;
    logic                       pri_resp_wen;
    logic   [79:0]              pri_resp_wdata;
    logic                       pri_resp_ren;
    logic   [79:0]              pri_resp_dout;

    logic                       resp_pv;
    logic   [19:0]              resp_pid;
    logic   [23:0]              resp_did;
    logic   [8:0]               resp_prg_index;

    logic                       ats_resp_empty;
    logic                       ats_resp_ren;
    logic   [113:0]             ats_resp_dout;
    logic                       ats_inv_wen;
    logic   [127:0]             ats_inv_wdata;
    logic   [127:0]             ats_inv_rdata;
//  logic   [255:0]             ats_resp_rdata;

    logic                       t2r_inv_end;
    logic                       t2r_ats_end;
    logic                       atd_t2r_connected;
    logic                       atd_t2r_tvalid;
    logic                       atd_t2r_tlast;
    logic   [63:0]              atd_t2r_tdata;
    logic   [7:0]               inv_req_idx;
    logic                       ats_sync_req_valid;
    logic                       ats_page_ack_valid;

////////************************************************************************///////
//cq invalid signals to acd
////////************************************************************************///////
atd_sync_fifo
    #(
    .FIFO_WID       (114),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u0_t2r_ats_resp_wfifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (ats_resp_wen_i),
    .ren                (ats_resp_ren),
    .din                (ats_resp_wdata_i),
    .dout               (ats_resp_dout),
    .full               (ats_resp_full_o),
    .empty              (ats_resp_empty)
   );

atd_sync_fifo
    #(
    .FIFO_WID       (16),
    .FIFO_DEPTH_WID (3),
    .FIFO_DEPTH     (8),
    .REG_OUT        (0)
    )
u1_t2r_ats_fault_wfifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (ats_fault_wen_i),
    .ren                (ats_fault_ren),
    .din                (ats_fault_wdata_i),
    .dout               (ats_fault_dout),
    .full               (ats_fault_full_o),
    .empty              (ats_fault_empty)
   );

////////************************************************************************///////
//ats invalid signals to acd
////////************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_inv_wen <= 1'b0;
    else if (ats_inv_wen_i && (ats_inv_wdata_i[9:7] == 3'd0) && (ats_inv_wdata_i[6:0] == 7'd4))
        ats_inv_wen <= 1'b1;
    else
        ats_inv_wen <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_inv_wdata <= 128'd0;
    else
        ats_inv_wdata <= ats_inv_wdata_i;
end

atd_sync_fifo
    #(
    .FIFO_WID       (128),
    .FIFO_DEPTH_WID (3),
    .FIFO_DEPTH     (8),
    .REG_OUT        (0)
    )
u1_t2r_ats_inv_wfifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (ats_inv_wen),
    .ren                (ats_inv_ren),
    .din                (ats_inv_wdata),
    .dout               (ats_inv_dout),
    .full               (ats_inv_full),
    .empty              (ats_inv_empty)
   );


assign ats_inv_full_o = ats_inv_full || pri_resp_full;
////////***********************************************************************///////
//pri_resp signals to acd
////////***********************************************************************///////
//maybe two ways signal are not the same time coming

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pri_resp_wen <= 1'b0;
    else if (ats_inv_wen_i && (ats_inv_wdata_i[9:7] == 3'd1) && (ats_inv_wdata_i[6:0] == 7'd4))
        pri_resp_wen <= 1'b1;
    else if (pri_resp_wen_i)
        pri_resp_wen <= 1'b1;
    else
        pri_resp_wen <= 1'b0;
end


assign resp_pv = ats_inv_wdata_i[32];
assign resp_pid = ats_inv_wdata_i[31:12];
assign resp_did = ats_inv_wdata_i[63:40];
assign resp_prg_index = ats_inv_wdata_i[104:96];

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pri_resp_wdata <= 80'd0;
    else if (ats_inv_wen_i && (ats_inv_wdata_i[9:7] == 3'd1) && (ats_inv_wdata_i[6:0] == 7'd4))
        pri_resp_wdata <= {7'd0,resp_prg_index,8'd0,resp_did,resp_pid,resp_pv,7'd0,4'd9};
    else if (pri_resp_wen_i)
        pri_resp_wdata <= pri_resp_wdata_i;
end

assign pri_resp_full_o = pri_resp_full || ats_inv_wen_i;


atd_sync_fifo
    #(
    .FIFO_WID       (80),
    .FIFO_DEPTH_WID (3),
    .FIFO_DEPTH     (8),
    .REG_OUT        (0)
    )
u1_t2r_pri_resp_wfifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (pri_resp_wen),
    .ren                (pri_resp_ren),
    .din                (pri_resp_wdata),
    .dout               (pri_resp_dout),
    .full               (pri_resp_full),
    .empty              (pri_resp_empty)
   );


////////***********************************************************************///////
//
////////***********************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_sync_req_valid <= 1'b0;
    else if (ats_sync_req_i)
        ats_sync_req_valid <= 1'b1;
    else if (t2r_cs == T2R_SYNC_LAST)
        ats_sync_req_valid <= 1'b0;
    else
        ats_sync_req_valid <= ats_sync_req_valid;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_page_ack_valid <= 1'b0;
    else if (ats_page_ack_i)
        ats_page_ack_valid <= 1'b1;
    else if (t2r_cs == T2R_PG_ACK_LAST)
        ats_page_ack_valid <= 1'b0;
    else
        ats_page_ack_valid <= ats_page_ack_valid;
end

////////**********************************************************************************************************///////
//atd to acd fsm
////////**********************************************************************************************************///////
assign ats_fault_ren = (t2r_cs == T2R_FAULT_RD);
assign ats_inv_ren = (t2r_cs == T2R_INV_RD);
assign ats_resp_ren = (t2r_cs == T2R_ATS_RD);
assign pri_resp_ren = (t2r_cs == T2R_PRI_RD);


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_inv_rdata <= 128'd0;
    else if (ats_inv_ren)
        ats_inv_rdata <= ats_inv_dout;
    else
        ats_inv_rdata <= ats_inv_rdata;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        t2r_cs <= IDLE;
    else
        t2r_cs <= t2r_ns;
end


always@(*) begin
    case (t2r_cs)
        IDLE:
            if ((!atd_t2r_connected_i) || (atd_t2r_connected_i && !atd_t2r_connected))
                t2r_ns = IDLE;
            else if (!ats_inv_empty && atd_t2r_tready_i)
                t2r_ns = T2R_INV_RD;
            else if (!ats_fault_empty && atd_t2r_tready_i)
                t2r_ns = T2R_FAULT_RD;
            else if (!ats_resp_empty && atd_t2r_tready_i)
                t2r_ns = T2R_ATS_RD;
            else if (!pri_resp_empty && atd_t2r_tready_i)
                t2r_ns = T2R_PRI_RD;
            else if (ats_sync_req_valid && atd_t2r_tready_i)
                t2r_ns = T2R_SYNC_LAST;
            else if (ats_page_ack_valid && atd_t2r_tready_i)
                t2r_ns = T2R_PG_ACK_LAST;
            else
                t2r_ns = IDLE;
        T2R_INV_RD:
            t2r_ns = T2R_INV_LAST;
        T2R_FAULT_RD:
            t2r_ns = T2R_FAULT_LAST;
        T2R_ATS_RD:
            t2r_ns = T2R_ATS_LAST;
        T2R_PRI_RD:
            t2r_ns = T2R_PRI_LAST;
        T2R_INV_LAST:
            if ((atd_t2r_tready_i && t2r_inv_end) || !atd_t2r_connected_i)
                t2r_ns = IDLE;
            else
                t2r_ns = T2R_INV_LAST;
        T2R_FAULT_LAST,T2R_SYNC_LAST,T2R_PG_ACK_LAST:
                t2r_ns = IDLE;
        T2R_ATS_LAST:
            if ((atd_t2r_tready_i && t2r_ats_end) || !atd_t2r_connected_i)
                t2r_ns = IDLE;
            else
                t2r_ns = T2R_ATS_LAST;
        T2R_PRI_LAST:
            if (atd_t2r_tready_i || !atd_t2r_connected_i)
                t2r_ns = IDLE;
            else
                t2r_ns = T2R_PRI_LAST;
        default:
            t2r_ns = IDLE;
    endcase
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        t2r_inv_end <= 1'b0;
    else if ((t2r_cs == T2R_INV_LAST) && atd_t2r_tready_i)
        t2r_inv_end <= ~ t2r_inv_end;
    else if (t2r_cs == IDLE)
        t2r_inv_end <= 1'b0;
    else
        t2r_inv_end <= t2r_inv_end;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        t2r_ats_end <= 1'b0;
    else if ((t2r_cs == T2R_ATS_LAST) && atd_t2r_tready_i)
        t2r_ats_end <= ~ t2r_ats_end;
    else if (t2r_cs != T2R_ATS_LAST)
        t2r_ats_end <= 1'b0;
    else
        t2r_ats_end <= t2r_ats_end;
end


assign atd_t2r_tstrb_o = 8'hff;
assign atd_t2r_tkeep_o = 8'hff;
assign atd_t2r_tid_o = 4'd0;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_t2r_connected <= 1'b0;
    else
        atd_t2r_connected <= atd_t2r_connected_i;
end

always@(*) begin
    if (!iommu_rstn)
        atd_t2r_tvalid = 1'b0;
    else if (!atd_t2r_connected_i)
        atd_t2r_tvalid = 1'b0;
    else if (atd_t2r_connected_i && !atd_t2r_connected)//rising_edge
        atd_t2r_tvalid = 1'b1;
    else if (t2r_cs == T2R_FAULT_RD)
        atd_t2r_tvalid = 1'b1;
    else if (t2r_cs == T2R_INV_RD)
        atd_t2r_tvalid = 1'b1;
    else if ((t2r_cs == T2R_INV_RD) || (t2r_cs == T2R_INV_LAST))
        atd_t2r_tvalid = 1'b1;
    else if ((t2r_cs == T2R_ATS_RD) || (t2r_cs == T2R_ATS_LAST))
        atd_t2r_tvalid = 1'b1;
    else if ((t2r_cs == T2R_PRI_RD) || (t2r_cs == T2R_PRI_LAST))
        atd_t2r_tvalid = 1'b1;
    else if ((t2r_cs == T2R_SYNC_LAST) || (t2r_cs == T2R_PG_ACK_LAST))
        atd_t2r_tvalid = 1'b1;
    else
        atd_t2r_tvalid = 1'b0;
end


assign atd_t2r_tvalid_o = atd_t2r_tvalid;
assign atd_t2r_tlast_o = atd_t2r_tlast;
assign atd_t2r_tdata_o = atd_t2r_tdata;


always@(*) begin
    if (!iommu_rstn)
        atd_t2r_tlast = 1'b0;
    else if (!atd_t2r_connected_i)
        atd_t2r_tlast = 1'b0;
    else if (atd_t2r_connected_i && !atd_t2r_connected)//rising_edge
        atd_t2r_tlast = 1'b1;
    else if (t2r_cs == T2R_FAULT_RD)
        atd_t2r_tlast = 1'b1;
    else if ((t2r_cs == T2R_INV_LAST) && t2r_inv_end)
        atd_t2r_tlast = 1'b1;
    else if ((t2r_cs == T2R_ATS_LAST) && t2r_ats_end)
        atd_t2r_tlast = 1'b1;
    else if ((t2r_cs == T2R_PRI_LAST) && atd_t2r_tready_i)
        atd_t2r_tlast = 1'b1;
    else if ((t2r_cs == T2R_SYNC_LAST) || (t2r_cs == T2R_PG_ACK_LAST))
        atd_t2r_tlast = 1'b1;
    else
        atd_t2r_tlast = 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        inv_req_idx <= 8'd0;
    else if (!atd_t2r_connected_i)
        inv_req_idx <= 8'd0;
    else if (atd_t2r_connected_i)
        inv_req_idx <= inv_req_idx - 1'b1;
    else if (atd_t2r_connected_i && ats_inv_ren)
        inv_req_idx <= inv_req_idx + 1'b1;
    else
        inv_req_idx <= inv_req_idx;
end


always@(*) begin
    if (!iommu_rstn)
        atd_t2r_tdata = 64'd0;
    else if (!atd_t2r_connected_i)
        atd_t2r_tdata = 64'd0;
    else if (atd_t2r_connected_i && !atd_t2r_connected)//rising_edge
        atd_t2r_tdata = {32'd0,7'd0,DTI_ATSv1_OAS,1'b1,atd_t2r_tok_trans_req_i,DTI_ATSv1,4'd1,4'd0};
    else if (t2r_cs == T2R_FAULT_RD)
        atd_t2r_tdata = {48'd0,ats_fault_dout};
    else if (t2r_cs == T2R_INV_RD)
        atd_t2r_tdata = {52'd0,inv_req_idx,4'b1100};
    else if ((t2r_cs == T2R_INV_LAST) && t2r_inv_end)
        atd_t2r_tdata = ats_inv_rdata[127:64];
    else if (t2r_cs == T2R_INV_LAST)
        atd_t2r_tdata = ats_inv_rdata[63:0];
    else if (t2r_cs == T2R_ATS_RD)
        atd_t2r_tdata = {46'd0,ats_resp_dout[17:0]};
    else if ((t2r_cs == T2R_ATS_LAST) && !t2r_ats_end)
        atd_t2r_tdata = ats_resp_dout[81:18];
    else if ((t2r_cs == T2R_ATS_LAST) && t2r_ats_end)
        atd_t2r_tdata = {32'd0,ats_resp_dout[113:82]};
    else if (t2r_cs == T2R_PRI_RD)
        atd_t2r_tdata = pri_resp_dout[63:0];
    else if (t2r_cs == T2R_PRI_LAST)
        atd_t2r_tdata = {48'd0,pri_resp_dout[79:64]};
    else if (t2r_cs == T2R_SYNC_LAST)
        atd_t2r_tdata = {60'd0,SYNC_REQ};
    else if (t2r_cs == T2R_PG_ACK_LAST)
        atd_t2r_tdata = {60'd0,PAGE_ACK};
    else
        atd_t2r_tdata = 64'd0;
end



endmodule



