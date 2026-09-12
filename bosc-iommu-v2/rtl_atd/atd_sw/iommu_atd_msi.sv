///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   rv_iommu_msi.sv
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

module iommu_atd_msi (
    input                           iommu_clk,
    input                           iommu_rstn,

    //reg cfg
    input   [15:0]                  iommu_icvec_i,
    // MSI config table
    input   [55:0]                  iommu_msi_addr_i[15:0],//56
    input   [31:0]                  iommu_msi_data_i[15:0],//32
    input   [15:0]                  iommu_msi_mask_i,

    // Interrupt pending bits
    input   [3:0]                   msi_intp_i,
    output  [3:0]                   ipsr_hw2sw_o,

    //from cq
    input                           cq_iofence_wvalid_i,
    input [63:0]                    cq_iofence_waddr_i,
    input [31:0]                    cq_iofence_wdata_i,
    output                          cq_iofence_wready_o,

    //from ptw
    input                           up_ad_wen_i,
    input  [7:0]                    up_ad_wdata_i,
    input  [52:0]                   up_ad_waddr_i,

    output [5:0]                    msi_awatop_o,
    //msi
    output                          msi_awvalid_o,
    input                           msi_awready_i,
    output  [3:0]                   msi_awid_o,
    output  [55:0]                  msi_awaddr_o,
    output  [7:0]                   msi_awlen_o,
    output  [2:0]                   msi_awsize_o,
    output  [3:0]                   msi_awcache_o,
    output                          msi_wvalid_o,
    input                           msi_wready_i,
    output                          msi_wlast_o,
    output  [31:0]                  msi_wstrb_o,
    output  [255:0]                 msi_wdata_o,

    input                           msi_bvalid_i,
    output                          msi_bready_o,
    input [3:0]                     msi_bid_i,
    input [1:0]                     msi_bresp_i
);


    localparam  logic   [1:0]   RESP_OKAY           = 2'b00;
    localparam  logic   [3:0]   IOFENCE_ID          = 4'd2;
    localparam  logic   [3:0]   UP_A_ID             = 4'd3;
    localparam  logic   [3:0]   CQ_MSI_ID           = 4'd7;
    localparam  logic   [3:0]   FQ_MSI_ID           = 4'd8;
    localparam  logic   [3:0]   PM_MSI_ID           = 4'd9;
    localparam  logic   [3:0]   PQ_MSI_ID           = 4'd10;

//  localparam  logic   [11:0]  MSI_ST_ACCESS_FAULT = 12'd273;

    logic                           atd_iofence_wen;
    logic                           atd_iofence_ren;
    logic [87:0]                    atd_iofence_din;
    logic [87:0]                    atd_iofence_dout;
    logic                           atd_iofence_full;
    logic                           atd_iofence_empty;
    logic                           atd_msi_iofence_en;
    logic [55:0]                    cq_iofence_waddr;
    logic [31:0]                    cq_iofence_wdata;

    logic                           msi_ren;
    logic                           msi_mask;
    logic [31:0]                    msi_data;
    logic [55:0]                    msi_addr;
    logic [3:0]                     msi_awid;
    logic [3:0]                     msi_awid_q;
    logic [2:0]                     msi_awsize;
    logic [2:0]                     msi_awsize_q;
    logic [5:0]                     msi_awatop;
    logic [5:0]                     msi_awatop_q;
    logic [3:0]                     msi_awcache;
    logic [3:0]                     msi_awcache_q;

    logic [3:0]                     msi_req_id;
    logic [55:0]                    msi_awaddr;
    logic [55:0]                    msi_awaddr_q;

    logic [255:0]                   msi_wdata;
    logic [255:0]                   msi_wdata_q;
    logic [31:0]                    msi_wstrb;
    logic [31:0]                    msi_wstrb_q;

    logic [3:0]                     msi_intp;
    logic                           cq_msi_flag;
    logic                           cq_cfg_ren;
    logic                           cq_msi_mask;
    logic                           cq_pending_en;
    logic                           fq_msi_flag;
    logic                           fq_cfg_ren;
    logic                           fq_msi_mask;
    logic                           fq_pending_en;
    logic                           pm_msi_flag;
    logic                           pm_cfg_ren;
    logic                           pm_msi_mask;
    logic                           pm_pending_en;
    logic                           pq_msi_flag;
    logic                           pq_cfg_ren;
    logic                           pq_msi_mask;
    logic                           pq_pending_en;

    logic                           up_ad_wfifo_ren;
    logic                           up_ad_en;
    logic [60:0]                    up_ad_wfifo_dout;
    logic                           up_ad_wfifo_full;
    logic                           up_ad_wfifo_empty;

    logic                           ipsr_hw2sw_cq;
    logic                           ipsr_hw2sw_fq;
    logic                           ipsr_hw2sw_pm;
    logic                           ipsr_hw2sw_pq;


    // FSM states
    enum logic [3:0] {
    IDLE    ,
    GET_CQ_CFG,
    GET_FQ_CFG,
    GET_PM_CFG,
    GET_PQ_CFG,
    GET_IOFENCE_CFG,
    GET_UP_AD,
    MSI_DELAY,
    JUDGE_MASK,
    AW_REQ  ,
    W_DATA  ,
    B_RESP  ,
    ERROR
    } msi_cs,msi_ns;


// Default values
// AXI parameters
// AW
/* verilator lint_off WIDTH */
assign msi_awid_o   = msi_awid;
//assign msi_awaddr_o = atd_msi_iofence_en ? {cq_iofence_waddr[55:5],5'd0} : {msi_addr[55:5], 5'd0};
assign msi_awaddr_o = msi_awaddr;
assign msi_awlen_o  = 8'd0;         // MSI writes 32-bytes wide/only one transfer
assign msi_awsize_o = msi_awsize;
assign msi_awvalid_o = (msi_cs == AW_REQ);
assign msi_awatop_o = msi_awatop;
assign msi_awcache_o = msi_awcache;
// W
assign msi_wdata_o  = msi_wdata;

assign msi_wstrb_o  = msi_wstrb;
assign msi_wlast_o = (msi_cs == W_DATA);

// Send data through W channel
assign msi_wvalid_o = (msi_cs == W_DATA);

// B
assign msi_bready_o = ((msi_cs == B_RESP) && msi_bvalid_i);

//assign msi_write_error_o = (msi_cs == ERROR);

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_req_id <= 4'd0;
    else if (msi_cs == GET_CQ_CFG)
        msi_req_id <= CQ_MSI_ID;
    else if (msi_cs == GET_FQ_CFG)
        msi_req_id <= FQ_MSI_ID;
    else if (msi_cs == GET_PM_CFG)
        msi_req_id <= PM_MSI_ID;
    else if (msi_cs == GET_PQ_CFG)
        msi_req_id <= PQ_MSI_ID;
    else
        msi_req_id <= msi_req_id;
end


always@(*) begin
    if (!iommu_rstn)
        msi_awid = 4'h0;
    else if (up_ad_en)
        msi_awid = UP_A_ID;
    else if (atd_msi_iofence_en)
        msi_awid = IOFENCE_ID;
    else if (msi_ren)
        msi_awid = msi_req_id;
    else
        msi_awid = msi_awid_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_awid_q <= 4'h0;
    else
        msi_awid_q <= msi_awid;
end

always@(*) begin
    if (!iommu_rstn)
        msi_awsize = 3'h0;
    else if (up_ad_en)
        msi_awsize = 3'h0;
    else if (atd_msi_iofence_en)
        msi_awsize = 3'h2;
    else if (msi_ren)
        msi_awsize = 3'h2;
    else
        msi_awsize = msi_awsize_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_awsize_q <= 3'h0;
    else
        msi_awsize_q <= msi_awsize;
end

always@(*) begin
    if (!iommu_rstn)
        msi_awatop = 6'h00;
    else if (up_ad_en)
        msi_awatop = 6'h13;
    else if (atd_msi_iofence_en)
        msi_awatop = 6'h00;
    else if (msi_ren)
        msi_awatop = 6'h00;
    else
        msi_awatop = msi_awatop_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_awatop_q <= 6'h00;
    else
        msi_awatop_q <= msi_awatop;
end


always@(*) begin
    if (!iommu_rstn)
        msi_awcache = 4'h0;
    else if (up_ad_en)
        msi_awcache = 4'hf;
    else if (atd_msi_iofence_en)
        msi_awcache = 4'h0;
    else if (msi_ren)
        msi_awcache = 4'h0;
    else
        msi_awcache = msi_awcache_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_awcache_q <= 4'h0;
    else
        msi_awcache_q <= msi_awcache;
end


always@(*) begin
    if (!iommu_rstn)
        msi_awaddr = 56'd0;
    else if (up_ad_en)
        msi_awaddr = {up_ad_wfifo_dout[52:0],3'd0};
    else if (atd_msi_iofence_en)
        msi_awaddr = {cq_iofence_waddr[55:3],3'd0};
    else if (msi_ren)
        msi_awaddr = {msi_addr[55:3],3'd0};
    else
        msi_awaddr = msi_awaddr_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_awaddr_q <= 56'd0;
    else
        msi_awaddr_q <= msi_awaddr;
end

always@(*) begin
    if (!iommu_rstn)
        msi_wdata = 256'd0;
    else if (msi_ren)
        case (msi_addr[4:2])
            3'd0:
                msi_wdata = {224'd0,msi_data};
            3'd1:
                msi_wdata = {192'd0,msi_data,32'd0};
            3'd2:
                msi_wdata = {160'd0,msi_data,64'd0};
            3'd3:
                msi_wdata = {128'd0,msi_data,96'd0};
            3'd4:
                msi_wdata = {96'd0,msi_data,128'd0};
            3'd5:
                msi_wdata = {64'd0,msi_data,160'd0};
            3'd6:
                msi_wdata = {32'd0,msi_data,192'd0};
            3'd7:
                msi_wdata = {msi_data,224'd0};
            default:
                msi_wdata = msi_wdata_q;
        endcase
    else if (atd_iofence_ren)
        case (cq_iofence_waddr[4:2])
            3'd0:
                msi_wdata = {224'd0,cq_iofence_wdata};
            3'd1:
                msi_wdata = {192'd0,cq_iofence_wdata,32'd0};
            3'd2:
                msi_wdata = {160'd0,cq_iofence_wdata,64'd0};
            3'd3:
                msi_wdata = {128'd0,cq_iofence_wdata,96'd0};
            3'd4:
                msi_wdata = {96'd0,cq_iofence_wdata,128'd0};
            3'd5:
                msi_wdata = {64'd0,cq_iofence_wdata,160'd0};
            3'd6:
                msi_wdata = {32'd0,cq_iofence_wdata,192'd0};
            3'd7:
                msi_wdata = {cq_iofence_wdata,224'd0};
            default:
                msi_wdata = msi_wdata_q;
        endcase
    else if (up_ad_wfifo_ren)
        case (up_ad_wfifo_dout[1:0])
            2'd0:
                msi_wdata = {192'd0,56'd0,up_ad_wfifo_dout[60:53]};
            2'd1:
                msi_wdata = {128'd0,56'd0,up_ad_wfifo_dout[60:53],64'd0};
            2'd2:
                msi_wdata = {64'd0,56'd0,up_ad_wfifo_dout[60:53],128'd0};
            2'd3:
                msi_wdata = {56'd0,up_ad_wfifo_dout[60:53],192'd0};
            default:
                msi_wdata = msi_wdata_q;
        endcase
    else
        msi_wdata = msi_wdata_q;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_wdata_q  <= 256'd0;
    else
        msi_wdata_q <= msi_wdata;
end


always@(*) begin
    if (!iommu_rstn)
        msi_wstrb = 32'd0;
    else if (msi_ren)
        case (msi_addr[4:2])
            3'd0:
                msi_wstrb = {28'd0,4'hf};
            3'd1:
                msi_wstrb = {24'd0,4'hf,4'd0};
            3'd2:
                msi_wstrb = {20'd0,4'hf,8'd0};
            3'd3:
                msi_wstrb = {16'd0,4'hf,12'd0};
            3'd4:
                msi_wstrb = {12'd0,4'hf,16'd0};
            3'd5:
                msi_wstrb = {8'd0,4'hf,20'd0};
            3'd6:
                msi_wstrb = {4'd0,4'hf,24'd0};
            3'd7:
                msi_wstrb = {4'hf,28'd0};
            default:
                msi_wstrb = msi_wstrb_q;
        endcase
    else if (atd_iofence_ren)
        case (cq_iofence_waddr[4:2])
            3'd0:
                msi_wstrb = {28'd0,4'hf};
            3'd1:
                msi_wstrb = {24'd0,4'hf,4'd0};
            3'd2:
                msi_wstrb = {20'd0,4'hf,8'd0};
            3'd3:
                msi_wstrb = {16'd0,4'hf,12'd0};
            3'd4:
                msi_wstrb = {12'd0,4'hf,16'd0};
            3'd5:
                msi_wstrb = {8'd0,4'hf,20'd0};
            3'd6:
                msi_wstrb = {4'd0,4'hf,24'd0};
            3'd7:
                msi_wstrb = {4'hf,28'd0};
            default:
                msi_wstrb = msi_wstrb_q;
        endcase
    else if (up_ad_wfifo_ren)
        case (up_ad_wfifo_dout[1:0])
            2'd0:
                msi_wstrb = {24'd0,7'd0,1'b1};
            2'd1:
                msi_wstrb = {16'd0,7'd0,1'b1,8'd0};
            2'd2:
                msi_wstrb = {8'd0,7'd0,1'b1,16'd0};
            2'd3:
                msi_wstrb = {7'd0,1'b1,24'd0};
            default:
                msi_wstrb = msi_wstrb_q;
        endcase
    else
        msi_wstrb = msi_wstrb_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_wstrb_q  <= 32'd0;
    else
        msi_wstrb_q <= msi_wstrb;
end

assign cq_iofence_wready_o = !atd_iofence_full;
assign atd_iofence_wen = cq_iofence_wvalid_i && !atd_iofence_full;
assign atd_iofence_din = {cq_iofence_waddr_i[55:0],cq_iofence_wdata_i};
assign cq_iofence_waddr = atd_iofence_dout[87:32];
assign cq_iofence_wdata = atd_iofence_dout[31:0];


atd_sync_fifo
    #(
    .FIFO_WID       (88),
    .FIFO_DEPTH_WID (3),
    .FIFO_DEPTH     (8),
    .REG_OUT        (0)
    )
u0_atd_iofence_wfifo(
    .clk            (iommu_clk),
    .rstn           (iommu_rstn),
    .wen            (atd_iofence_wen),
    .ren            (atd_iofence_ren),
    .din            (atd_iofence_din),
    .dout           (atd_iofence_dout),
    .full           (atd_iofence_full),
    .empty          (atd_iofence_empty)
   );


atd_sync_fifo
    #(
    .FIFO_WID       (61),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u1_atd_ad_rfifo(
    .clk            (iommu_clk),
    .rstn           (iommu_rstn),
    .wen            (up_ad_wen_i),
    .ren            (up_ad_wfifo_ren),
    .din            ({up_ad_wdata_i,up_ad_waddr_i}),//8+53
    .dout           (up_ad_wfifo_dout),
    .full           (up_ad_wfifo_full),
    .empty          (up_ad_wfifo_empty)
   );


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_intp <= 4'd0;
    else
        msi_intp <= msi_intp_i;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_msi_flag <= 1'b0;
    else if (!msi_intp[0] && msi_intp_i[0])
        cq_msi_flag <= 1'b1;
    else if ((msi_intp[0] && !msi_intp_i[0]) || (cq_msi_flag && (msi_cs == B_RESP) &&
            msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == CQ_MSI_ID)))
        cq_msi_flag <= 1'b0;
    else
        cq_msi_flag <= cq_msi_flag;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_msi_flag <= 1'b0;
    else if (!msi_intp[1] && msi_intp_i[1])
        fq_msi_flag <= 1'b1;
    else if ((msi_intp[1] && !msi_intp_i[1]) || (fq_msi_flag && (msi_cs == B_RESP) &&
            msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == FQ_MSI_ID)))
        fq_msi_flag <= 1'b0;
    else
        fq_msi_flag <= fq_msi_flag;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pm_msi_flag <= 1'b0;
    else if (!msi_intp[2] && msi_intp_i[2])
        pm_msi_flag <= 1'b1;
    else if ((msi_intp[2] && !msi_intp_i[2]) || (pm_msi_flag && (msi_cs == B_RESP) &&
             msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == PM_MSI_ID)))
        pm_msi_flag <= 1'b0;
    else
        pm_msi_flag <= pm_msi_flag;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_msi_flag <= 1'b0;
    else if (!msi_intp[3] && msi_intp_i[3])
        pq_msi_flag <= 1'b1;
    else if ((msi_intp[3] && !msi_intp_i[3]) || (pq_msi_flag && (msi_cs == B_RESP) &&
            msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == PQ_MSI_ID)))
        pq_msi_flag <= 1'b0;
    else
        pq_msi_flag <= pq_msi_flag;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_cs <= IDLE;
    else
        msi_cs <= msi_ns;
end

always@(*) begin
    if (!iommu_rstn) begin
        msi_ns = IDLE;
    end
    case (msi_cs)
        IDLE:
            if (cq_msi_flag && cq_pending_en && (!cq_msi_mask))
                msi_ns = GET_CQ_CFG;
            else if (fq_msi_flag && fq_pending_en && (!fq_msi_mask))
                msi_ns = GET_FQ_CFG;
            else if (pm_msi_flag && pm_pending_en && (!pm_msi_mask))
                msi_ns = GET_PM_CFG;
            else if (pq_msi_flag && pq_pending_en && (!pq_msi_mask))
                msi_ns = GET_PQ_CFG;
            else if (!atd_iofence_empty)
                msi_ns = GET_IOFENCE_CFG;
            else if (!up_ad_wfifo_empty)
                msi_ns = GET_UP_AD;
            else if (cq_msi_flag && (!cq_cfg_ren || !fq_cfg_ren || !pm_cfg_ren))
                msi_ns = GET_CQ_CFG;
            else if (fq_msi_flag && (!fq_cfg_ren || !pm_cfg_ren || !pq_cfg_ren))
                msi_ns = GET_FQ_CFG;
            else if (pm_msi_flag && (!pm_cfg_ren || !pq_cfg_ren || !cq_cfg_ren))
                msi_ns = GET_PM_CFG;
            else if (pq_msi_flag && (!pq_cfg_ren || !cq_cfg_ren || !fq_cfg_ren))
                msi_ns = GET_PQ_CFG;
            else
                msi_ns = IDLE;
        GET_CQ_CFG:
            msi_ns = JUDGE_MASK;
        GET_FQ_CFG:
            msi_ns = JUDGE_MASK;
        GET_PM_CFG:
            msi_ns = JUDGE_MASK;
        GET_PQ_CFG:
            msi_ns = JUDGE_MASK;
        GET_IOFENCE_CFG:
            msi_ns = AW_REQ;
        GET_UP_AD:
            msi_ns = AW_REQ;
        JUDGE_MASK:
            if (msi_mask)
                msi_ns = IDLE;
            else
                msi_ns = AW_REQ;
        AW_REQ:
            if (msi_awready_i)
                msi_ns = W_DATA;
            else
                msi_ns = AW_REQ;
        W_DATA:
            if (msi_wready_i)
                msi_ns = B_RESP;
            else
                msi_ns = W_DATA;
        B_RESP:
            if (msi_bvalid_i && (msi_bresp_i != RESP_OKAY))
                msi_ns = ERROR;
            else if (msi_bvalid_i && (msi_bresp_i == RESP_OKAY))
                msi_ns = IDLE;
            else
                msi_ns = B_RESP;
        ERROR:
//          if (!error_vector) begin
                msi_ns = IDLE;
//          end
        default:
             msi_ns = IDLE;
    endcase
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_ren <= 1'b0;
    else if ((msi_cs == GET_CQ_CFG) || (msi_cs == GET_FQ_CFG) || (msi_cs == GET_PM_CFG) || (msi_cs == GET_PQ_CFG))
        msi_ren <= 1'b1;
    else
        msi_ren <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_addr <= 56'd0;
    else
        if (msi_cs == GET_CQ_CFG)
            msi_addr <= iommu_msi_addr_i[iommu_icvec_i[3:0]];
        else if (msi_cs == GET_FQ_CFG)
            msi_addr <= iommu_msi_addr_i[iommu_icvec_i[7:4]];
        else if (msi_cs == GET_PM_CFG)
            msi_addr <= iommu_msi_addr_i[iommu_icvec_i[11:8]];
        else if (msi_cs == GET_PQ_CFG)
            msi_addr <= iommu_msi_addr_i[iommu_icvec_i[15:12]];
        else
            msi_addr <= msi_addr;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_data <= 32'd0;
    else
        if (msi_cs == GET_CQ_CFG)
            msi_data <= iommu_msi_data_i[iommu_icvec_i[3:0]];
        else if (msi_cs == GET_FQ_CFG)
            msi_data <= iommu_msi_data_i[iommu_icvec_i[7:4]];
        else if (msi_cs == GET_PM_CFG)
            msi_data <= iommu_msi_data_i[iommu_icvec_i[11:8]];
        else if (msi_cs == GET_PQ_CFG)
            msi_data <= iommu_msi_data_i[iommu_icvec_i[15:12]];
        else
            msi_data <= msi_data;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_mask <= 1'b0;
    else
        if (msi_cs == GET_CQ_CFG)
            msi_mask <= iommu_msi_mask_i[iommu_icvec_i[3:0]];
        else if (msi_cs == GET_FQ_CFG)
            msi_mask <= iommu_msi_mask_i[iommu_icvec_i[7:4]];
        else if (msi_cs == GET_PM_CFG)
            msi_mask <= iommu_msi_mask_i[iommu_icvec_i[11:8]];
        else if (msi_cs == GET_PQ_CFG)
            msi_mask <= iommu_msi_mask_i[iommu_icvec_i[15:12]];
        else
            msi_mask <= msi_mask;
end


////////**********************************************************************************///////
//iofence
////////**********************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_iofence_ren <= 1'b0;
    else if (msi_cs == GET_IOFENCE_CFG)
        atd_iofence_ren <= 1'b1;
    else
        atd_iofence_ren <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_msi_iofence_en <= 1'b0;
    else if (msi_cs == GET_IOFENCE_CFG)
        atd_msi_iofence_en <= 1'b1;
    else if ((msi_cs == AW_REQ) && msi_awready_i)
        atd_msi_iofence_en <= 1'b0;
    else
        atd_msi_iofence_en <= atd_msi_iofence_en;
end


////////**********************************************************************************///////
//update a/d bit
////////**********************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_ad_wfifo_ren <= 1'b0;
    else if (msi_cs == GET_UP_AD)
        up_ad_wfifo_ren <= 1'b1;
    else
        up_ad_wfifo_ren <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_ad_en <= 1'b0;
    else if (msi_cs == GET_UP_AD)
        up_ad_en <= 1'b1;
    else if ((msi_cs == AW_REQ) && msi_awready_i)
        up_ad_en <= 1'b0;
    else
        up_ad_en <= up_ad_en;
end

////////**********************************************************************************///////
//msi
////////**********************************************************************************///////
//cq cfg mask and pending
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_cfg_ren <= 1'b0;
    else if ((msi_cs == AW_REQ) || (msi_cs == IDLE))
        cq_cfg_ren <= 1'b0;
    else if (msi_cs == GET_CQ_CFG)
        cq_cfg_ren <= 1'b1;
    else
        cq_cfg_ren <= cq_cfg_ren;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_msi_mask <= 1'b0;
    else if ((msi_cs == AW_REQ) && cq_cfg_ren)
        cq_msi_mask <= msi_mask;
    else
        cq_msi_mask <= cq_msi_mask;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_pending_en <= 1'b0;
    else if (cq_pending_en && (msi_cs == B_RESP) && msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == CQ_MSI_ID))
        cq_pending_en <= 1'b0;
    else if ((msi_cs == AW_REQ) && cq_cfg_ren && msi_mask)
        cq_pending_en <= 1'b1;
    else
        cq_pending_en <= cq_pending_en;
end


//fq cfg mask and pending
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_cfg_ren <= 1'b0;
    else if ((msi_cs == AW_REQ) || (msi_cs == IDLE))
        fq_cfg_ren <= 1'b0;
    else if (msi_cs == GET_FQ_CFG)
        fq_cfg_ren <= 1'b1;
    else
        fq_cfg_ren <= fq_cfg_ren;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_msi_mask <= 1'b0;
    else if ((msi_cs == AW_REQ) && fq_cfg_ren)
        fq_msi_mask <= msi_mask;
    else
        fq_msi_mask <= fq_msi_mask;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_pending_en <= 1'b0;
    else if (fq_pending_en && (msi_cs == B_RESP) && msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == FQ_MSI_ID))
        fq_pending_en <= 1'b0;
    else if ((msi_cs == AW_REQ) && fq_cfg_ren && msi_mask)
        fq_pending_en <= 1'b1;
    else
        fq_pending_en <= fq_pending_en;
end

//pm cfg mask and pending
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pm_cfg_ren <= 1'b0;
    else if ((msi_cs == AW_REQ) || (msi_cs == IDLE))
        pm_cfg_ren <= 1'b0;
    else if (msi_cs == GET_PM_CFG)
        pm_cfg_ren <= 1'b1;
    else
        pm_cfg_ren <= pm_cfg_ren;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pm_msi_mask <= 1'b0;
    else if ((msi_cs == AW_REQ) && pm_cfg_ren)
        pm_msi_mask <= msi_mask;
    else
        pm_msi_mask <= pm_msi_mask;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pm_pending_en <= 1'b0;
    else if (pm_pending_en && (msi_cs == B_RESP) && msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == PM_MSI_ID))
        pm_pending_en <= 1'b0;
    else if ((msi_cs == AW_REQ) && pm_cfg_ren && msi_mask)
        pm_pending_en <= 1'b1;
    else
        pm_pending_en <= pm_pending_en;
end


//pq cfg mask and pending
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_cfg_ren <= 1'b0;
    else if ((msi_cs == AW_REQ) || (msi_cs == IDLE))
        pq_cfg_ren <= 1'b0;
    else if (msi_cs == GET_PM_CFG)
        pq_cfg_ren <= 1'b1;
    else
        pq_cfg_ren <= pq_cfg_ren;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_msi_mask <= 1'b0;
    else if ((msi_cs == AW_REQ) && pq_cfg_ren)
        pq_msi_mask <= msi_mask;
    else
        pq_msi_mask <= pq_msi_mask;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_pending_en <= 1'b0;
    else if (pq_pending_en && (msi_cs == B_RESP) && msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == PQ_MSI_ID))
        pq_pending_en <= 1'b0;
    else if ((msi_cs == AW_REQ) && pq_cfg_ren && msi_mask)
        pq_pending_en <= 1'b1;
    else
        pq_pending_en <= pq_pending_en;
end



////////**********************************************************************************///////
//generate hw2sw_reg_ipsr
////////**********************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ipsr_hw2sw_cq <= 1'b0;
    else if (msi_intp[0] && !msi_intp_i[0])
        ipsr_hw2sw_cq <= 1'b0;
    else if (cq_msi_flag && (msi_cs == B_RESP) && msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == CQ_MSI_ID))
        ipsr_hw2sw_cq <= 1'b1;
    else
        ipsr_hw2sw_cq <= ipsr_hw2sw_cq;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ipsr_hw2sw_fq <= 1'b0;
    else if (msi_intp[1] && !msi_intp_i[1])
        ipsr_hw2sw_fq <= 1'b0;
    else if (fq_msi_flag && (msi_cs == B_RESP) && msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == FQ_MSI_ID))
        ipsr_hw2sw_fq <= 1'b1;
    else
        ipsr_hw2sw_fq <= ipsr_hw2sw_fq;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ipsr_hw2sw_pm <= 1'b0;
    else if (msi_intp[2] && !msi_intp_i[2])
        ipsr_hw2sw_pm <= 1'b0;
    else if (pm_msi_flag && (msi_cs == B_RESP) && msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == PM_MSI_ID))
        ipsr_hw2sw_pm <= 1'b1;
    else
        ipsr_hw2sw_pm <= ipsr_hw2sw_pm;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ipsr_hw2sw_pq <= 1'b0;
    else if (msi_intp[3] && !msi_intp_i[3])
        ipsr_hw2sw_pq <= 1'b0;
    else if (pq_msi_flag && (msi_cs == B_RESP) && msi_bvalid_i && (msi_bresp_i == RESP_OKAY) && (msi_bid_i == PQ_MSI_ID))
        ipsr_hw2sw_pq <= 1'b1;
    else
        ipsr_hw2sw_pq <= ipsr_hw2sw_pq;
end

assign ipsr_hw2sw_o = {ipsr_hw2sw_pq,ipsr_hw2sw_pm,ipsr_hw2sw_fq,ipsr_hw2sw_cq};


endmodule
