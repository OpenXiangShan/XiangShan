///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_tc_apb.v
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
module iommu_tc_apb
(
    input                           iommu_clk,
    input                           iommu_rstn,

    input                           iommu_penable_i,
    input                           iommu_apb_wen_i,
    input  [11:0]                   iommu_apb_addr_i,
    input  [31:0]                   iommu_apb_wdata_i,
    input                           iommu_apb_ren_i,

    output [31:0]                   iommu_apb_rdata_o,
    output                          iommu_apb_ready_o,
    output                          iommu_apb_slverr_o,

    input  [30:0]                   atd_hpm_rvalid_i,
    input  [31:0]                   atd_iocntovf_i,
    input  [63:0]                   atd_hpmctr_i[30:0],
    input  [63:0]                   atd_hpmevt_i[30:0],

    input  [63:0]                   iommu_tr_resp_i,//RO
    input                           iommu_tr_req_busy_i,
    output                          iommu_tr_req_en_o,
    output [51:0]                   iommu_tr_req_iova_o,
    output [63:0]                   iommu_tr_req_ctl_o,

    input                           iommu_acd_cfg_ack_i,
    input  [31:0]                   iommu_acd_cfg_rdata_i,
    input  [30:0]                   iommu_acd_iocntovf_i,
    output                          iommu_acd_cfg_wvalid_o,
    output [63:0]                   iommu_acd_cfg_o
    );


//***************************************************************//
//signal defination
//***************************************************************//
    // FSM states
    enum logic [2:0] {
    IDLE        ,
    TC_APB_WR   ,
    TC_APB_RD   ,
    TC_APB_WRDATA,
    TC_APB_RDATA,
    TC_APB_DELAY
    } tc_apb_cs,tc_apb_ns;


    logic                   iommu_apb_ready;
    logic                   iommu_apb_wen;
    logic                   iommu_apb_ren;
    logic [31:0]            iommu_apb_rdata;
    
    logic                   iommu_tr_req_en;
    logic [51:0]            iommu_tr_req_iova;
    logic [63:0]            iommu_tr_req_ctl;
    logic                   iommu_tr_req_busy;

    logic                   iommu_acd_cfg_wvalid;
    logic [63:0]            iommu_acd_cfg;


assign iommu_apb_rdata_o = iommu_apb_rdata;
assign iommu_apb_ready_o = iommu_apb_ready;
assign iommu_apb_slverr_o = iommu_apb_wen || iommu_apb_ren;


assign iommu_apb_wen = iommu_apb_wen_i && iommu_penable_i && (
                            ((iommu_apb_addr_i >= 12'h068) && (iommu_apb_addr_i <= 12'h254)) ||
                            (iommu_apb_addr_i == 12'h008) || 
                            (iommu_apb_addr_i == 12'h010) || 
                            (iommu_apb_addr_i == 12'h054) ||
                            (iommu_apb_addr_i == 12'h05c) ||
                            (iommu_apb_addr_i == 12'h258) ||
                            (iommu_apb_addr_i == 12'h25c) ||
                            (iommu_apb_addr_i == 12'h260) ||
                            (iommu_apb_addr_i == 12'h264) || 
                            (iommu_apb_addr_i == 12'h280) || 
                            (iommu_apb_addr_i == 12'h290) || 
                            (iommu_apb_addr_i == 12'h298));

assign iommu_apb_ren = iommu_apb_ren_i && iommu_penable_i && (
                            ((iommu_apb_addr_i >= 12'h068) && (iommu_apb_addr_i <= 12'h254)) ||
                            (iommu_apb_addr_i == 12'h008) ||
                            (iommu_apb_addr_i == 12'h010) ||
                            (iommu_apb_addr_i == 12'h058) ||
                            (iommu_apb_addr_i == 12'h05c) ||
                            (iommu_apb_addr_i == 12'h258) ||
                            (iommu_apb_addr_i == 12'h25c) ||
                            (iommu_apb_addr_i == 12'h260) ||
                            (iommu_apb_addr_i == 12'h264) ||
                            (iommu_apb_addr_i == 12'h268) ||
                            (iommu_apb_addr_i == 12'h26c) ||
                            (iommu_apb_addr_i == 12'h280) || 
                            (iommu_apb_addr_i == 12'h290) ||  
                            (iommu_apb_addr_i == 12'h298));

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        tc_apb_cs <= IDLE;
    else
        tc_apb_cs <= tc_apb_ns;
end


always@(*) begin
    case (tc_apb_cs)
        IDLE:
            if (iommu_apb_wen)            
                tc_apb_ns = TC_APB_WR;
            else if (iommu_apb_ren)
                tc_apb_ns = TC_APB_RD;
            else
                tc_apb_ns = IDLE;
        TC_APB_WR:
            if (iommu_acd_cfg_ack_i)
                tc_apb_ns = TC_APB_WRDATA;
            else
                tc_apb_ns = TC_APB_WR;
        TC_APB_RD:
            if (iommu_acd_cfg_ack_i)
                tc_apb_ns = TC_APB_RDATA;
            else
                tc_apb_ns = TC_APB_RD;
        TC_APB_WRDATA:
            tc_apb_ns = TC_APB_DELAY;
        TC_APB_RDATA:
            tc_apb_ns = TC_APB_DELAY;
        TC_APB_DELAY:
            tc_apb_ns = IDLE;
        default:
            tc_apb_ns = IDLE;
    endcase
end


assign  iommu_acd_cfg_wvalid_o = iommu_acd_cfg_wvalid;
assign  iommu_acd_cfg_o = iommu_acd_cfg;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) 
        iommu_acd_cfg_wvalid <= 1'b0;
    else if ((tc_apb_cs == IDLE) && (iommu_apb_wen || iommu_apb_ren))
        iommu_acd_cfg_wvalid <= 1'b1;
    else if (iommu_acd_cfg_ack_i && ((tc_apb_cs == TC_APB_WR) || (tc_apb_cs == TC_APB_RD)))
        iommu_acd_cfg_wvalid <= 1'b0;
    else
        iommu_acd_cfg_wvalid <= iommu_acd_cfg_wvalid;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) 
        iommu_acd_cfg <= 64'd0;
    else if ((tc_apb_cs == IDLE) && iommu_apb_wen) 
        iommu_acd_cfg <= {iommu_apb_wdata_i,19'd0,iommu_apb_addr_i[9:2],1'b1,4'd6};
    else if ((tc_apb_cs == IDLE) && iommu_apb_ren) 
        iommu_acd_cfg <= {51'd0,iommu_apb_addr_i[9:2],1'b0,4'd6};
    else if (iommu_acd_cfg_ack_i && ((tc_apb_cs == TC_APB_WR) || (tc_apb_cs == TC_APB_RD)))
        iommu_acd_cfg <= 64'd0;
    else
        iommu_acd_cfg <= iommu_acd_cfg;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_apb_ready <= 1'b0;
    else if (iommu_acd_cfg_ack_i && ((tc_apb_cs == TC_APB_WR) || (tc_apb_cs == TC_APB_RD)))
        iommu_apb_ready <= 1'b1;
    else
        iommu_apb_ready <= 1'b0;
end

//when dbg_busy is low,sw starts to read dbg_resp
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_apb_rdata <= 32'd0;
    else if (iommu_apb_ren && (iommu_apb_addr_i == 12'h258))  
        iommu_apb_rdata <= {iommu_tr_req_iova[19:0],12'd0};
    else if (iommu_apb_ren && (iommu_apb_addr_i == 12'h25c))  
        iommu_apb_rdata <= iommu_tr_req_iova[51:20];
    else if (iommu_apb_ren && (iommu_apb_addr_i == 12'h260))  
        iommu_apb_rdata <= {iommu_tr_req_ctl[31:1],iommu_tr_req_busy_i};
    else if (iommu_apb_ren && (iommu_apb_addr_i == 12'h264))  
        iommu_apb_rdata <= iommu_tr_req_ctl[63:32];
    else if (iommu_apb_ren && (iommu_apb_addr_i == 12'h268))
        iommu_apb_rdata <= iommu_tr_resp_i[31:0];
    else if (iommu_apb_ren && (iommu_apb_addr_i == 12'h26c))
        iommu_apb_rdata <= iommu_tr_resp_i[63:32];
    else if (iommu_apb_ren && iommu_acd_cfg_ack_i && (tc_apb_cs == TC_APB_RD))
        case (iommu_apb_addr_i)
            12'h008,12'h010,12'h05c,12'h280,12'h290,12'h298 : iommu_apb_rdata <= iommu_acd_cfg_rdata_i;
            12'h058 : iommu_apb_rdata <= {atd_iocntovf_i[31:1] | iommu_acd_iocntovf_i,atd_iocntovf_i[0]};
            12'h068 : iommu_apb_rdata <= atd_hpm_rvalid_i[0]  ? atd_hpmctr_i[0][31:0]   : iommu_acd_cfg_rdata_i;
            12'h070 : iommu_apb_rdata <= atd_hpm_rvalid_i[1]  ? atd_hpmctr_i[1][31:0]   : iommu_acd_cfg_rdata_i;
            12'h078 : iommu_apb_rdata <= atd_hpm_rvalid_i[2]  ? atd_hpmctr_i[2][31:0]   : iommu_acd_cfg_rdata_i;
            12'h080 : iommu_apb_rdata <= atd_hpm_rvalid_i[3]  ? atd_hpmctr_i[3][31:0]   : iommu_acd_cfg_rdata_i;
            12'h088 : iommu_apb_rdata <= atd_hpm_rvalid_i[4]  ? atd_hpmctr_i[4][31:0]   : iommu_acd_cfg_rdata_i;
            12'h090 : iommu_apb_rdata <= atd_hpm_rvalid_i[5]  ? atd_hpmctr_i[5][31:0]   : iommu_acd_cfg_rdata_i;
            12'h098 : iommu_apb_rdata <= atd_hpm_rvalid_i[6]  ? atd_hpmctr_i[6][31:0]   : iommu_acd_cfg_rdata_i;
            12'h0a0 : iommu_apb_rdata <= atd_hpm_rvalid_i[7]  ? atd_hpmctr_i[7][31:0]   : iommu_acd_cfg_rdata_i;
            12'h0a8 : iommu_apb_rdata <= atd_hpm_rvalid_i[8]  ? atd_hpmctr_i[8][31:0]   : iommu_acd_cfg_rdata_i;
            12'h0b0 : iommu_apb_rdata <= atd_hpm_rvalid_i[9]  ? atd_hpmctr_i[9][31:0]   : iommu_acd_cfg_rdata_i;
            12'h0b8 : iommu_apb_rdata <= atd_hpm_rvalid_i[10] ? atd_hpmctr_i[10][31:0]  : iommu_acd_cfg_rdata_i;
            12'h0c0 : iommu_apb_rdata <= atd_hpm_rvalid_i[11] ? atd_hpmctr_i[11][31:0]  : iommu_acd_cfg_rdata_i;
            12'h0c8 : iommu_apb_rdata <= atd_hpm_rvalid_i[12] ? atd_hpmctr_i[12][31:0]  : iommu_acd_cfg_rdata_i;
            12'h0d0 : iommu_apb_rdata <= atd_hpm_rvalid_i[13] ? atd_hpmctr_i[13][31:0]  : iommu_acd_cfg_rdata_i;
            12'h0d8 : iommu_apb_rdata <= atd_hpm_rvalid_i[14] ? atd_hpmctr_i[14][31:0]  : iommu_acd_cfg_rdata_i;
            12'h0e0 : iommu_apb_rdata <= atd_hpm_rvalid_i[15] ? atd_hpmctr_i[15][31:0]  : iommu_acd_cfg_rdata_i;
            12'h0e8 : iommu_apb_rdata <= atd_hpm_rvalid_i[16] ? atd_hpmctr_i[16][31:0]  : iommu_acd_cfg_rdata_i;
            12'h0f0 : iommu_apb_rdata <= atd_hpm_rvalid_i[17] ? atd_hpmctr_i[17][31:0]  : iommu_acd_cfg_rdata_i;
            12'h0f8 : iommu_apb_rdata <= atd_hpm_rvalid_i[18] ? atd_hpmctr_i[18][31:0]  : iommu_acd_cfg_rdata_i;
            12'h100 : iommu_apb_rdata <= atd_hpm_rvalid_i[19] ? atd_hpmctr_i[19][31:0]  : iommu_acd_cfg_rdata_i;
            12'h108 : iommu_apb_rdata <= atd_hpm_rvalid_i[20] ? atd_hpmctr_i[20][31:0]  : iommu_acd_cfg_rdata_i;
            12'h110 : iommu_apb_rdata <= atd_hpm_rvalid_i[21] ? atd_hpmctr_i[21][31:0]  : iommu_acd_cfg_rdata_i;
            12'h118 : iommu_apb_rdata <= atd_hpm_rvalid_i[22] ? atd_hpmctr_i[22][31:0]  : iommu_acd_cfg_rdata_i;
            12'h120 : iommu_apb_rdata <= atd_hpm_rvalid_i[23] ? atd_hpmctr_i[23][31:0]  : iommu_acd_cfg_rdata_i;
            12'h128 : iommu_apb_rdata <= atd_hpm_rvalid_i[24] ? atd_hpmctr_i[24][31:0]  : iommu_acd_cfg_rdata_i;
            12'h130 : iommu_apb_rdata <= atd_hpm_rvalid_i[25] ? atd_hpmctr_i[25][31:0]  : iommu_acd_cfg_rdata_i;
            12'h138 : iommu_apb_rdata <= atd_hpm_rvalid_i[26] ? atd_hpmctr_i[26][31:0]  : iommu_acd_cfg_rdata_i;
            12'h140 : iommu_apb_rdata <= atd_hpm_rvalid_i[27] ? atd_hpmctr_i[27][31:0]  : iommu_acd_cfg_rdata_i;
            12'h148 : iommu_apb_rdata <= atd_hpm_rvalid_i[28] ? atd_hpmctr_i[28][31:0]  : iommu_acd_cfg_rdata_i;
            12'h150 : iommu_apb_rdata <= atd_hpm_rvalid_i[29] ? atd_hpmctr_i[29][31:0]  : iommu_acd_cfg_rdata_i;
            12'h158 : iommu_apb_rdata <= atd_hpm_rvalid_i[30] ? atd_hpmctr_i[30][31:0]  : iommu_acd_cfg_rdata_i;
            12'h06c : iommu_apb_rdata <= atd_hpm_rvalid_i[0]  ? atd_hpmctr_i[0][63:32]  : iommu_acd_cfg_rdata_i;
            12'h074 : iommu_apb_rdata <= atd_hpm_rvalid_i[1]  ? atd_hpmctr_i[1][63:32]  : iommu_acd_cfg_rdata_i;
            12'h07c : iommu_apb_rdata <= atd_hpm_rvalid_i[2]  ? atd_hpmctr_i[2][63:32]  : iommu_acd_cfg_rdata_i;
            12'h084 : iommu_apb_rdata <= atd_hpm_rvalid_i[3]  ? atd_hpmctr_i[3][63:32]  : iommu_acd_cfg_rdata_i;
            12'h08c : iommu_apb_rdata <= atd_hpm_rvalid_i[4]  ? atd_hpmctr_i[4][63:32]  : iommu_acd_cfg_rdata_i;
            12'h094 : iommu_apb_rdata <= atd_hpm_rvalid_i[5]  ? atd_hpmctr_i[5][63:32]  : iommu_acd_cfg_rdata_i;
            12'h09c : iommu_apb_rdata <= atd_hpm_rvalid_i[6]  ? atd_hpmctr_i[6][63:32]  : iommu_acd_cfg_rdata_i;
            12'h0a4 : iommu_apb_rdata <= atd_hpm_rvalid_i[7]  ? atd_hpmctr_i[7][63:32]  : iommu_acd_cfg_rdata_i;
            12'h0ac : iommu_apb_rdata <= atd_hpm_rvalid_i[8]  ? atd_hpmctr_i[8][63:32]  : iommu_acd_cfg_rdata_i;
            12'h0b4 : iommu_apb_rdata <= atd_hpm_rvalid_i[9]  ? atd_hpmctr_i[9][63:32]  : iommu_acd_cfg_rdata_i;
            12'h0bc : iommu_apb_rdata <= atd_hpm_rvalid_i[10] ? atd_hpmctr_i[10][63:32] : iommu_acd_cfg_rdata_i;
            12'h0c4 : iommu_apb_rdata <= atd_hpm_rvalid_i[11] ? atd_hpmctr_i[11][63:32] : iommu_acd_cfg_rdata_i;
            12'h0cc : iommu_apb_rdata <= atd_hpm_rvalid_i[12] ? atd_hpmctr_i[12][63:32] : iommu_acd_cfg_rdata_i;
            12'h0d4 : iommu_apb_rdata <= atd_hpm_rvalid_i[13] ? atd_hpmctr_i[13][63:32] : iommu_acd_cfg_rdata_i;
            12'h0dc : iommu_apb_rdata <= atd_hpm_rvalid_i[14] ? atd_hpmctr_i[14][63:32] : iommu_acd_cfg_rdata_i;
            12'h0e4 : iommu_apb_rdata <= atd_hpm_rvalid_i[15] ? atd_hpmctr_i[15][63:32] : iommu_acd_cfg_rdata_i;
            12'h0ec : iommu_apb_rdata <= atd_hpm_rvalid_i[16] ? atd_hpmctr_i[16][63:32] : iommu_acd_cfg_rdata_i;
            12'h0f4 : iommu_apb_rdata <= atd_hpm_rvalid_i[17] ? atd_hpmctr_i[17][63:32] : iommu_acd_cfg_rdata_i;
            12'h0fc : iommu_apb_rdata <= atd_hpm_rvalid_i[18] ? atd_hpmctr_i[18][63:32] : iommu_acd_cfg_rdata_i;
            12'h104 : iommu_apb_rdata <= atd_hpm_rvalid_i[19] ? atd_hpmctr_i[19][63:32] : iommu_acd_cfg_rdata_i;
            12'h10c : iommu_apb_rdata <= atd_hpm_rvalid_i[20] ? atd_hpmctr_i[20][63:32] : iommu_acd_cfg_rdata_i;
            12'h114 : iommu_apb_rdata <= atd_hpm_rvalid_i[21] ? atd_hpmctr_i[21][63:32] : iommu_acd_cfg_rdata_i;
            12'h11c : iommu_apb_rdata <= atd_hpm_rvalid_i[22] ? atd_hpmctr_i[22][63:32] : iommu_acd_cfg_rdata_i;
            12'h124 : iommu_apb_rdata <= atd_hpm_rvalid_i[23] ? atd_hpmctr_i[23][63:32] : iommu_acd_cfg_rdata_i;
            12'h12c : iommu_apb_rdata <= atd_hpm_rvalid_i[24] ? atd_hpmctr_i[24][63:32] : iommu_acd_cfg_rdata_i;
            12'h134 : iommu_apb_rdata <= atd_hpm_rvalid_i[25] ? atd_hpmctr_i[25][63:32] : iommu_acd_cfg_rdata_i;
            12'h13c : iommu_apb_rdata <= atd_hpm_rvalid_i[26] ? atd_hpmctr_i[26][63:32] : iommu_acd_cfg_rdata_i;
            12'h144 : iommu_apb_rdata <= atd_hpm_rvalid_i[27] ? atd_hpmctr_i[27][63:32] : iommu_acd_cfg_rdata_i;
            12'h14c : iommu_apb_rdata <= atd_hpm_rvalid_i[28] ? atd_hpmctr_i[28][63:32] : iommu_acd_cfg_rdata_i;
            12'h154 : iommu_apb_rdata <= atd_hpm_rvalid_i[29] ? atd_hpmctr_i[29][63:32] : iommu_acd_cfg_rdata_i;
            12'h15c : iommu_apb_rdata <= atd_hpm_rvalid_i[30] ? atd_hpmctr_i[30][63:32] : iommu_acd_cfg_rdata_i;
            12'h160 : iommu_apb_rdata <= atd_hpm_rvalid_i[0]  ? atd_hpmevt_i[0][31:0]   : iommu_acd_cfg_rdata_i;
            12'h168 : iommu_apb_rdata <= atd_hpm_rvalid_i[1]  ? atd_hpmevt_i[1][31:0]   : iommu_acd_cfg_rdata_i;
            12'h170 : iommu_apb_rdata <= atd_hpm_rvalid_i[2]  ? atd_hpmevt_i[2][31:0]   : iommu_acd_cfg_rdata_i;
            12'h178 : iommu_apb_rdata <= atd_hpm_rvalid_i[3]  ? atd_hpmevt_i[3][31:0]   : iommu_acd_cfg_rdata_i;
            12'h180 : iommu_apb_rdata <= atd_hpm_rvalid_i[4]  ? atd_hpmevt_i[4][31:0]   : iommu_acd_cfg_rdata_i;
            12'h188 : iommu_apb_rdata <= atd_hpm_rvalid_i[5]  ? atd_hpmevt_i[5][31:0]   : iommu_acd_cfg_rdata_i;
            12'h190 : iommu_apb_rdata <= atd_hpm_rvalid_i[6]  ? atd_hpmevt_i[6][31:0]   : iommu_acd_cfg_rdata_i;
            12'h198 : iommu_apb_rdata <= atd_hpm_rvalid_i[7]  ? atd_hpmevt_i[7][31:0]   : iommu_acd_cfg_rdata_i;
            12'h1a0 : iommu_apb_rdata <= atd_hpm_rvalid_i[8]  ? atd_hpmevt_i[8][31:0]   : iommu_acd_cfg_rdata_i;
            12'h1a8 : iommu_apb_rdata <= atd_hpm_rvalid_i[9]  ? atd_hpmevt_i[9][31:0]   : iommu_acd_cfg_rdata_i;
            12'h1b0 : iommu_apb_rdata <= atd_hpm_rvalid_i[10] ? atd_hpmevt_i[10][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1b8 : iommu_apb_rdata <= atd_hpm_rvalid_i[11] ? atd_hpmevt_i[11][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1c0 : iommu_apb_rdata <= atd_hpm_rvalid_i[12] ? atd_hpmevt_i[12][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1c8 : iommu_apb_rdata <= atd_hpm_rvalid_i[13] ? atd_hpmevt_i[13][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1d0 : iommu_apb_rdata <= atd_hpm_rvalid_i[14] ? atd_hpmevt_i[14][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1d8 : iommu_apb_rdata <= atd_hpm_rvalid_i[15] ? atd_hpmevt_i[15][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1e0 : iommu_apb_rdata <= atd_hpm_rvalid_i[16] ? atd_hpmevt_i[16][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1e8 : iommu_apb_rdata <= atd_hpm_rvalid_i[17] ? atd_hpmevt_i[17][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1f0 : iommu_apb_rdata <= atd_hpm_rvalid_i[18] ? atd_hpmevt_i[18][31:0]  : iommu_acd_cfg_rdata_i;
            12'h1f8 : iommu_apb_rdata <= atd_hpm_rvalid_i[19] ? atd_hpmevt_i[19][31:0]  : iommu_acd_cfg_rdata_i;
            12'h200 : iommu_apb_rdata <= atd_hpm_rvalid_i[20] ? atd_hpmevt_i[20][31:0]  : iommu_acd_cfg_rdata_i;
            12'h208 : iommu_apb_rdata <= atd_hpm_rvalid_i[21] ? atd_hpmevt_i[21][31:0]  : iommu_acd_cfg_rdata_i;
            12'h210 : iommu_apb_rdata <= atd_hpm_rvalid_i[22] ? atd_hpmevt_i[22][31:0]  : iommu_acd_cfg_rdata_i;
            12'h218 : iommu_apb_rdata <= atd_hpm_rvalid_i[23] ? atd_hpmevt_i[23][31:0]  : iommu_acd_cfg_rdata_i;
            12'h220 : iommu_apb_rdata <= atd_hpm_rvalid_i[24] ? atd_hpmevt_i[24][31:0]  : iommu_acd_cfg_rdata_i;
            12'h228 : iommu_apb_rdata <= atd_hpm_rvalid_i[25] ? atd_hpmevt_i[25][31:0]  : iommu_acd_cfg_rdata_i;
            12'h230 : iommu_apb_rdata <= atd_hpm_rvalid_i[26] ? atd_hpmevt_i[26][31:0]  : iommu_acd_cfg_rdata_i;
            12'h238 : iommu_apb_rdata <= atd_hpm_rvalid_i[27] ? atd_hpmevt_i[27][31:0]  : iommu_acd_cfg_rdata_i;
            12'h240 : iommu_apb_rdata <= atd_hpm_rvalid_i[28] ? atd_hpmevt_i[28][31:0]  : iommu_acd_cfg_rdata_i;
            12'h248 : iommu_apb_rdata <= atd_hpm_rvalid_i[29] ? atd_hpmevt_i[29][31:0]  : iommu_acd_cfg_rdata_i;
            12'h250 : iommu_apb_rdata <= atd_hpm_rvalid_i[30] ? atd_hpmevt_i[30][31:0]  : iommu_acd_cfg_rdata_i;
            12'h164 : iommu_apb_rdata <= atd_hpm_rvalid_i[0]  ? atd_hpmevt_i[0][63:32]  : iommu_acd_cfg_rdata_i;
            12'h16c : iommu_apb_rdata <= atd_hpm_rvalid_i[1]  ? atd_hpmevt_i[1][63:32]  : iommu_acd_cfg_rdata_i;
            12'h174 : iommu_apb_rdata <= atd_hpm_rvalid_i[2]  ? atd_hpmevt_i[2][63:32]  : iommu_acd_cfg_rdata_i;
            12'h17c : iommu_apb_rdata <= atd_hpm_rvalid_i[3]  ? atd_hpmevt_i[3][63:32]  : iommu_acd_cfg_rdata_i;
            12'h184 : iommu_apb_rdata <= atd_hpm_rvalid_i[4]  ? atd_hpmevt_i[4][63:32]  : iommu_acd_cfg_rdata_i;
            12'h18c : iommu_apb_rdata <= atd_hpm_rvalid_i[5]  ? atd_hpmevt_i[5][63:32]  : iommu_acd_cfg_rdata_i;
            12'h194 : iommu_apb_rdata <= atd_hpm_rvalid_i[6]  ? atd_hpmevt_i[6][63:32]  : iommu_acd_cfg_rdata_i;
            12'h19c : iommu_apb_rdata <= atd_hpm_rvalid_i[7]  ? atd_hpmevt_i[7][63:32]  : iommu_acd_cfg_rdata_i;
            12'h1a4 : iommu_apb_rdata <= atd_hpm_rvalid_i[8]  ? atd_hpmevt_i[8][63:32]  : iommu_acd_cfg_rdata_i;
            12'h1ac : iommu_apb_rdata <= atd_hpm_rvalid_i[9]  ? atd_hpmevt_i[9][63:32]  : iommu_acd_cfg_rdata_i;
            12'h1b4 : iommu_apb_rdata <= atd_hpm_rvalid_i[10] ? atd_hpmevt_i[10][63:32] : iommu_acd_cfg_rdata_i;
            12'h1bc : iommu_apb_rdata <= atd_hpm_rvalid_i[11] ? atd_hpmevt_i[11][63:32] : iommu_acd_cfg_rdata_i;
            12'h1c4 : iommu_apb_rdata <= atd_hpm_rvalid_i[12] ? atd_hpmevt_i[12][63:32] : iommu_acd_cfg_rdata_i;
            12'h1cc : iommu_apb_rdata <= atd_hpm_rvalid_i[13] ? atd_hpmevt_i[13][63:32] : iommu_acd_cfg_rdata_i;
            12'h1d4 : iommu_apb_rdata <= atd_hpm_rvalid_i[14] ? atd_hpmevt_i[14][63:32] : iommu_acd_cfg_rdata_i;
            12'h1dc : iommu_apb_rdata <= atd_hpm_rvalid_i[15] ? atd_hpmevt_i[15][63:32] : iommu_acd_cfg_rdata_i;
            12'h1e4 : iommu_apb_rdata <= atd_hpm_rvalid_i[16] ? atd_hpmevt_i[16][63:32] : iommu_acd_cfg_rdata_i;
            12'h1ec : iommu_apb_rdata <= atd_hpm_rvalid_i[17] ? atd_hpmevt_i[17][63:32] : iommu_acd_cfg_rdata_i;
            12'h1f4 : iommu_apb_rdata <= atd_hpm_rvalid_i[18] ? atd_hpmevt_i[18][63:32] : iommu_acd_cfg_rdata_i;
            12'h1fc : iommu_apb_rdata <= atd_hpm_rvalid_i[19] ? atd_hpmevt_i[19][63:32] : iommu_acd_cfg_rdata_i;
            12'h204 : iommu_apb_rdata <= atd_hpm_rvalid_i[20] ? atd_hpmevt_i[20][63:32] : iommu_acd_cfg_rdata_i;
            12'h20c : iommu_apb_rdata <= atd_hpm_rvalid_i[21] ? atd_hpmevt_i[21][63:32] : iommu_acd_cfg_rdata_i;
            12'h214 : iommu_apb_rdata <= atd_hpm_rvalid_i[22] ? atd_hpmevt_i[22][63:32] : iommu_acd_cfg_rdata_i;
            12'h21c : iommu_apb_rdata <= atd_hpm_rvalid_i[23] ? atd_hpmevt_i[23][63:32] : iommu_acd_cfg_rdata_i;
            12'h224 : iommu_apb_rdata <= atd_hpm_rvalid_i[24] ? atd_hpmevt_i[24][63:32] : iommu_acd_cfg_rdata_i;
            12'h22c : iommu_apb_rdata <= atd_hpm_rvalid_i[25] ? atd_hpmevt_i[25][63:32] : iommu_acd_cfg_rdata_i;
            12'h234 : iommu_apb_rdata <= atd_hpm_rvalid_i[26] ? atd_hpmevt_i[26][63:32] : iommu_acd_cfg_rdata_i;
            12'h23c : iommu_apb_rdata <= atd_hpm_rvalid_i[27] ? atd_hpmevt_i[27][63:32] : iommu_acd_cfg_rdata_i;
            12'h244 : iommu_apb_rdata <= atd_hpm_rvalid_i[28] ? atd_hpmevt_i[28][63:32] : iommu_acd_cfg_rdata_i;
            12'h24c : iommu_apb_rdata <= atd_hpm_rvalid_i[29] ? atd_hpmevt_i[29][63:32] : iommu_acd_cfg_rdata_i;
            12'h254 : iommu_apb_rdata <= atd_hpm_rvalid_i[30] ? atd_hpmevt_i[30][63:32] : iommu_acd_cfg_rdata_i;
            default: iommu_apb_rdata <= 32'd0;
        endcase
    else
        iommu_apb_rdata <= 32'd0;
end

////////**********************************************************************************************************///////
//dbg signals
////////**********************************************************************************************************///////
assign iommu_tr_req_en_o = iommu_tr_req_en;
assign iommu_tr_req_iova_o = iommu_tr_req_iova;
assign iommu_tr_req_ctl_o = iommu_tr_req_ctl;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_tr_req_iova <= 52'd0;
    else if (iommu_apb_wen_i && (iommu_apb_addr_i == 12'h258))
        iommu_tr_req_iova <= {32'd0,iommu_apb_wdata_i[31:12]};
    else if (iommu_apb_wen_i && (iommu_apb_addr_i == 12'h25c))
        iommu_tr_req_iova <= {iommu_apb_wdata_i,iommu_tr_req_iova[19:0]};
    else
        iommu_tr_req_iova <= iommu_tr_req_iova;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_tr_req_busy <= 1'b0;
    else
        iommu_tr_req_busy <= iommu_tr_req_busy_i;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_tr_req_ctl <= 64'd0;
    else if (iommu_tr_req_busy && !iommu_tr_req_busy_i)
        iommu_tr_req_ctl <= 64'd0;
    else if (iommu_apb_wen_i && (iommu_apb_addr_i == 12'h260))
        iommu_tr_req_ctl <= {32'd0,iommu_apb_wdata_i};
    else if (iommu_apb_wen_i && (iommu_apb_addr_i == 12'h264))
        iommu_tr_req_ctl <= {iommu_apb_wdata_i,iommu_tr_req_ctl[31:0]};
    else
        iommu_tr_req_ctl <= iommu_tr_req_ctl;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_tr_req_en <= 1'b0;
    else if (iommu_apb_wen_i && (iommu_apb_addr_i == 12'h264))
        iommu_tr_req_en <= 1'b1;
    else
        iommu_tr_req_en <= 1'b0;
end


endmodule
