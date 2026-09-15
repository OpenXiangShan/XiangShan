///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_r2t.sv
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


module iommu_atd_r2t
    (
    input                           iommu_clk,
    input                           iommu_rstn,

    //input from pcie-rc dti-ats
    input                           atd_r2t_rvalid_i,
    output                          atd_r2t_rready_o,
    input   [63:0]                  atd_r2t_rdata_i,
    input   [7:0]                   atd_r2t_rstrb_i,
    input   [7:0]                   atd_r2t_rkeep_i,
    input                           atd_r2t_rlast_i,
    input   [3:0]                   atd_r2t_rid_i,

    //DTI_ATS_CONDIS_REQ
    output                          atd_r2t_connected_o,
    output  [7:0]                   atd_r2t_tok_trans_req_o,

    //DTI_ATS_TRANS_REQ
    //pcie-rc:ats_req to cdw
    output                          ats_req_wen_o,
    output  [191:0]                 ats_req_wdata_o,
    input                           ats_req_full_i,

    //DTI_ATS_PAGE_REQ
    //pcie-rc:page_req to pq
    output                          pri_req_wen_o,
    output  [127:0]                 pri_req_wdata_o,
    input                           pri_req_full_i,

    //DTI_ATS_INV_ACK//pulse signal
    output                          ats_inv_ack_o,

    //DTI_ATS_SYNC_ACK//pulse signal
    output                          ats_sync_ack_o,

    //DTI_ATS_PAGE_RESPACK
    output                          ats_page_respack_o
);

    localparam  logic   [3:0]   DTI_ATSv1   = 4'b0000;
    localparam  logic   [3:0]   DTI_ATSv2   = 4'b0001;
    localparam  logic   [3:0]   DTI_ATSv3   = 4'b0010;

    localparam  logic   [3:0]   CONDIS_REQ  = 4'b0000;
    localparam  logic   [3:0]   TRANS_REQ   = 4'b0010;
    localparam  logic   [3:0]   INV_ACK     = 4'b1100;
    localparam  logic   [3:0]   INV_COMP    = 4'b1100;
    localparam  logic   [3:0]   SYNC_ACK    = 4'B1101;
    localparam  logic   [3:0]   PAGE_REQ    = 4'b1000;
    localparam  logic   [3:0]   PAGE_RESPACK= 4'b1001;


    // FSM states
    enum logic [2:0] {
    IDLE            ,
    R2T_WAIT_TRANS_LAST,
    R2T_WAIT_PAGE_LAST,
    R2T_WAIT_ILAST  ,
    R2T_WAIT_SLAST  ,
    R2T_WAIT_PLAST
    } r2t_cs,r2t_ns;


    logic   [3:0]                   m_msg_type;
    logic                           state;
    logic                           protocol;
    logic   [3:0]                   vertion;
    logic   [7:0]                   tok_trans_req;
    logic                           no_trans;
    logic                           atd_r2t_connected;
    logic   [7:0]                   atd_r2t_tok_trans_req;
    logic                           atd_r2t_inv_ack;
    logic                           atd_r2t_sync_ack;
    logic                           atd_r2t_page_respack;

    logic                           atd_r2t_rready;

    logic   [1:0]                   ats_req_cnt;
    logic                           ats_req_wen;
    logic   [63:0]                  ats_req_data_d0;
    logic   [63:0]                  ats_req_data_d1;
    logic   [191:0]                 ats_req_wdata;

    logic                           pri_req_wen;
    logic   [63:0]                  pri_req_data;
    logic   [127:0]                 pri_req_wdata;


////////**********************************************************************************************************///////
//gen signals
////////**********************************************************************************************************///////
assign atd_r2t_connected_o = atd_r2t_connected;
assign atd_r2t_tok_trans_req_o = atd_r2t_tok_trans_req;

assign ats_inv_ack_o = atd_r2t_inv_ack;
assign ats_sync_ack_o = atd_r2t_sync_ack && atd_r2t_connected;
assign ats_page_respack_o = atd_r2t_page_respack;

assign m_msg_type = atd_r2t_rdata_i[3:0];
assign state = atd_r2t_rdata_i[4];
assign protocol = atd_r2t_rdata_i[5];
assign vertion = atd_r2t_rdata_i[11:8];
assign tok_trans_req = atd_r2t_rdata_i[19:12];
assign no_trans = atd_r2t_rdata_i[24];


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_r2t_connected <= 1'b0;
    else if ((r2t_cs == IDLE) && atd_r2t_rvalid_i && atd_r2t_rlast_i && (m_msg_type == CONDIS_REQ) && (protocol == 1'b1) && (state == 1'b1))
        atd_r2t_connected <= 1'b1;
    else if ((r2t_cs == IDLE) && atd_r2t_rvalid_i && atd_r2t_rlast_i && (m_msg_type == CONDIS_REQ) && (protocol == 1'b1) && (state == 1'b0))
        atd_r2t_connected <= 1'b0;
    else
        atd_r2t_connected <= atd_r2t_connected;
end

//it is used to fill condis_ack
//only when !no_trans || DTI_ATSv1,trans_req is cnt,else rsv.
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_r2t_tok_trans_req <= 8'd0;
    else if (!atd_r2t_connected)
        atd_r2t_tok_trans_req <= 8'd0;
    else if (atd_r2t_connected && (m_msg_type == CONDIS_REQ) && (!no_trans || (vertion == DTI_ATSv1)))
        atd_r2t_tok_trans_req <= tok_trans_req + 1;
    else
        atd_r2t_tok_trans_req <= atd_r2t_tok_trans_req;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        r2t_cs <= IDLE;
    else
        r2t_cs <= r2t_ns;
end


always@(*) begin
    case (r2t_cs)
        IDLE:
            if (atd_r2t_rvalid_i && atd_r2t_connected)
                case (m_msg_type)
                    TRANS_REQ:
                        if (!ats_req_full_i && !atd_r2t_rlast_i)
                            r2t_ns = R2T_WAIT_TRANS_LAST;
                        else
                            r2t_ns = IDLE;
                    PAGE_REQ:
                        if (!pri_req_full_i && !atd_r2t_rlast_i)
                            r2t_ns = R2T_WAIT_PAGE_LAST;
                        else
                            r2t_ns = IDLE;
                    INV_ACK:
                        if (!atd_r2t_rready || !atd_r2t_rlast_i)
                            r2t_ns = R2T_WAIT_ILAST;
                        else
                            r2t_ns = IDLE;
                    SYNC_ACK:
                        if (!atd_r2t_rready || !atd_r2t_rlast_i)
                            r2t_ns = R2T_WAIT_SLAST;
                        else
                            r2t_ns = IDLE;
                    PAGE_RESPACK:
                        if (!atd_r2t_rready || !atd_r2t_rlast_i)
                            r2t_ns = R2T_WAIT_PLAST;
                        else
                            r2t_ns = IDLE;
                    default:
                        r2t_ns = IDLE;
                endcase
            else
                r2t_ns = IDLE;
        R2T_WAIT_TRANS_LAST,R2T_WAIT_PAGE_LAST,R2T_WAIT_ILAST,R2T_WAIT_SLAST,R2T_WAIT_PLAST:
            if (atd_r2t_rvalid_i && atd_r2t_rready && atd_r2t_rlast_i)
                r2t_ns = IDLE;
            else
                r2t_ns = r2t_cs;
        default:
            r2t_ns = IDLE;
    endcase
end


assign atd_r2t_rready_o = atd_r2t_rready;

always@(*) begin
    if (!iommu_rstn)
        atd_r2t_rready = 1'b0;
    else if ((r2t_cs == IDLE) && atd_r2t_rvalid_i && atd_r2t_rlast_i && (m_msg_type == CONDIS_REQ))
        atd_r2t_rready = 1'b1;
    else if (((r2t_cs == IDLE) && (m_msg_type == TRANS_REQ) && atd_r2t_rvalid_i && !ats_req_full_i) || ((r2t_cs == R2T_WAIT_TRANS_LAST) && atd_r2t_rvalid_i && atd_r2t_rlast_i))
        atd_r2t_rready = 1'b1;
    else if (((r2t_cs == IDLE) && (m_msg_type == PAGE_REQ) && atd_r2t_rvalid_i && !pri_req_full_i) || ((r2t_cs == R2T_WAIT_PAGE_LAST) && atd_r2t_rvalid_i && atd_r2t_rlast_i))
        atd_r2t_rready = 1'b1;
    else if ((r2t_cs == IDLE) && (m_msg_type == INV_ACK) && atd_r2t_rvalid_i && atd_r2t_rlast_i)
        atd_r2t_rready = 1'b1;
    else if ((r2t_cs == IDLE) && (m_msg_type == SYNC_ACK) && atd_r2t_rvalid_i && atd_r2t_rlast_i)
        atd_r2t_rready = 1'b1;
    else if ((r2t_cs == IDLE) && (m_msg_type == PAGE_RESPACK) && atd_r2t_rvalid_i && atd_r2t_rlast_i)
        atd_r2t_rready = 1'b1;
    else
        atd_r2t_rready = 1'b0;
end

////////**********************************************************************************************************///////
//get trans_req to cdw module
////////**********************************************************************************************************///////
assign ats_req_wen_o = ats_req_wen;
assign ats_req_wdata_o = ats_req_wdata;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_req_wen <= 1'b0;
    else if ((r2t_cs == R2T_WAIT_TRANS_LAST) && atd_r2t_rvalid_i && atd_r2t_rready && atd_r2t_rlast_i)
        ats_req_wen <= 1'b1;
    else
        ats_req_wen <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_req_cnt <= 2'd0;
    else if ((r2t_cs == IDLE) && (m_msg_type == TRANS_REQ) && atd_r2t_rvalid_i && atd_r2t_rready && !atd_r2t_rlast_i)
        ats_req_cnt <= 2'd0;
    else if ((r2t_cs == IDLE) && (m_msg_type == TRANS_REQ) && atd_r2t_rvalid_i && atd_r2t_rready)
        ats_req_cnt <= ats_req_cnt + 1;
    else
        ats_req_cnt <= ats_req_cnt;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_req_data_d0 <= 64'd0;
    else if ((r2t_cs == IDLE) && (m_msg_type == TRANS_REQ) && atd_r2t_rvalid_i && atd_r2t_rready && (ats_req_cnt == 2'd0))
        ats_req_data_d0 <= atd_r2t_rdata_i;
    else
        ats_req_data_d0 <= ats_req_data_d0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_req_data_d1 <= 64'd0;
    else if ((r2t_cs == IDLE) && (m_msg_type == TRANS_REQ) && atd_r2t_rvalid_i && atd_r2t_rready && (ats_req_cnt == 2'd1))
        ats_req_data_d1 <= atd_r2t_rdata_i;
    else
        ats_req_data_d1 <= ats_req_data_d1;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_req_wdata <= 192'd0;
    else if ((r2t_cs == R2T_WAIT_TRANS_LAST) && atd_r2t_rvalid_i && atd_r2t_rready && atd_r2t_rlast_i)
        ats_req_wdata <= {atd_r2t_rdata_i,ats_req_data_d1,ats_req_data_d0};
    else
        ats_req_wdata <= 192'd0;
end


////////**********************************************************************************************************///////
//get pri_req to cdw module
////////**********************************************************************************************************///////
assign pri_req_wen_o = pri_req_wen;
assign pri_req_wdata_o = pri_req_wdata;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pri_req_wen <= 1'b0;
    else if ((r2t_cs == R2T_WAIT_PAGE_LAST) && atd_r2t_rvalid_i && atd_r2t_rready && atd_r2t_rlast_i)
        pri_req_wen <= 1'b1;
    else
        pri_req_wen <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pri_req_data <= 64'd0;
    else if ((r2t_cs == IDLE) && (m_msg_type == PAGE_REQ) && atd_r2t_rvalid_i && atd_r2t_rready && !atd_r2t_rlast_i)
        pri_req_data <= atd_r2t_rdata_i;
    else
        pri_req_data <= pri_req_data;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pri_req_wdata <= 128'd0;
    else if ((r2t_cs == R2T_WAIT_PAGE_LAST) && atd_r2t_rvalid_i && atd_r2t_rready && atd_r2t_rlast_i)
        pri_req_wdata <= {atd_r2t_rdata_i,pri_req_data};
    else
        pri_req_wdata <= 128'd0;
end


////////**********************************************************************************************************///////
//get inv_ack
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_r2t_inv_ack <= 1'b0;
    else if (atd_r2t_rvalid_i && atd_r2t_rlast_i && (m_msg_type == INV_ACK))
        atd_r2t_inv_ack <= 1'b1;
    else
        atd_r2t_inv_ack <= 1'b0;
end

////////**********************************************************************************************************///////
//get sync_ack
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_r2t_sync_ack <= 1'b0;
    else if (atd_r2t_rvalid_i && atd_r2t_rlast_i && (m_msg_type == SYNC_ACK))
        atd_r2t_sync_ack <= 1'b1;
    else
        atd_r2t_sync_ack <= 1'b0;
end

////////**********************************************************************************************************///////
//get page_respack
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_r2t_page_respack <= 1'b0;
    else if (atd_r2t_rvalid_i && atd_r2t_rlast_i && (m_msg_type == PAGE_RESPACK))
        atd_r2t_page_respack <= 1'b1;
    else
        atd_r2t_page_respack <= 1'b0;
end




endmodule



