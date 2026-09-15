///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_c2t.sv
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


module iommu_atd_c2t
    (
    input logic                     iommu_clk,
    input logic                     iommu_rstn,

    //input from acd
    input                           atd_c2t_rvalid_i,
    output                          atd_c2t_rready_o,
    input   [63:0]                  atd_c2t_rdata_i,
    input   [7:0]                   atd_c2t_rstrb_i,
    input   [7:0]                   atd_c2t_rkeep_i,
    input                           atd_c2t_rlast_i,
    input   [3:0]                   atd_c2t_rid_i,
    input   [3:0]                   atd_c2t_rdest_i,
    input                           atd_c2t_ruser_i,

    output                          atd_c2t_connected_o,
    output                          atd_c2t_cq_ack_o,
    //acd:ptw_req output to cdw
    output                          cdw_rfifo_wen_o,
    output  [127:0]                 cdw_rfifo_wdata_o,
    input                           cdw_rfifo_full_i,

    //cq iofence ack
//    input                         cq_fence_valid_i,
    output                          atd_c2t_fence_ready_o,
    output                          atd_c2t_dvm_ready_o,

    //acd_fault -> fq
    output                          acd_fq_valid_o,
    output  [255:0]                 acd_fq_record_o,
    input                           acd_fq_ready_i,

    //acd_fault_rpt -> fault_ack
    output                          atd_fq_ack_valid_o,
    input                           atd_c2t_dbg_end_i,
    output                          atd_c2t_dbg_ack_o,

    input                           atd_c2t_cfg_end_i,
    output                          atd_c2t_cfg_ack_o,
    output  [31:0]                  atd_c2t_cfg_rdata_o,
    output  [30:0]                  atd_c2t_iocntovf_o,
    input                           atd_pmip_clr_i,
    
    //acd_reg -> apb
    input                           iommu_tr_req_go_i,
    output                          iommu_tr_req_busy_o,
    output  [63:0]                  iommu_tr_resp_o
);


    localparam  logic   [3:0]   PTW_REQ     = 4'd2;
    localparam  logic   [3:0]   INV_ACK     = 4'd4;
    localparam  logic   [3:0]   FENCE_ACK   = 4'd5;
    localparam  logic   [3:0]   CFG_ACK     = 4'd6;
    localparam  logic   [3:0]   FAULT_RPT   = 4'd8;
    localparam  logic   [3:0]   INT_RPT     = 4'd14;
    localparam  logic   [3:0]   DBG_RPT     = 4'd15;

    // FSM states
    enum logic [2:0] {
    IDLE            ,
    C2T_WAIT_PLAST  ,
    C2T_WAIT_FLAST  ,
    C2T_WAIT_DLAST  ,
    C2T_END
    } c2t_cs,c2t_ns;


    logic   [3:0]                   msg_code;
    logic                           atd_c2t_connected;
    logic                           atd_c2t_cq_ack;
    logic                           atd_c2t_fence_ready;
    logic                           atd_c2t_dvm_ready;

    logic                           cdw_rfifo_wen;
    logic   [63:0]                  cdw_rfifo_data;
    logic   [127:0]                 cdw_rfifo_wdata;
    logic                           atd_c2t_rready;

    logic                           acd_fq_valid;
    logic   [1:0]                   acd_fq_record_cnt;
    logic   [255:0]                 acd_fq_record;

    logic                           atd_c2t_dbg_ack;
    logic                           iommu_tr_req_go;
    logic                           iommu_tr_req_busy;
    logic   [63:0]                  iommu_tr_resp;
    logic                           atd_c2t_cfg_ack;
    logic   [31:0]                  atd_c2t_cfg_rdata;
    logic   [30:0]                  atd_c2t_iocntovf;

////////**********************************************************************************************************///////
//gen signals
////////**********************************************************************************************************///////
assign atd_c2t_connected_o = atd_c2t_connected;
assign atd_c2t_cq_ack_o = atd_c2t_cq_ack;
assign atd_c2t_fence_ready_o = atd_c2t_fence_ready && atd_c2t_connected;
assign atd_c2t_dvm_ready_o = atd_c2t_dvm_ready && atd_c2t_connected;

assign msg_code = atd_c2t_rdata_i[3:0];


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_c2t_connected <= 1'b0;
//    else if (atd_c2t_rvalid_i && atd_c2t_rlast_i && (msg_code == 4'd0) && (atd_c2t_rdata_i[7:4] == 4'd1))
    else if ((c2t_cs == IDLE) && atd_c2t_rvalid_i && atd_c2t_rlast_i && (atd_c2t_rdata_i[3:0] == 4'd0) && (atd_c2t_rdata_i[7:4] == 4'd1))
        atd_c2t_connected <= 1'b1;
//  else if (atd_c2t_rvalid_i && atd_c2t_rlast_i && (msg_code == 4'd0) && (atd_c2t_rdata_i[7:4] == 4'd0))
    else if ((c2t_cs == IDLE) && atd_c2t_rvalid_i && atd_c2t_rlast_i && (atd_c2t_rdata_i[3:0] == 4'd0) && (atd_c2t_rdata_i[7:4] == 4'd0))
        atd_c2t_connected <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_c2t_cq_ack <= 1'b0;
    else if (atd_c2t_rvalid_i && atd_c2t_rlast_i && ((msg_code == 4'd4) || (msg_code == 4'd5)))
        atd_c2t_cq_ack <= 1'b1;
    else
        atd_c2t_cq_ack <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_c2t_fence_ready <= 1'b0;
    else if (atd_c2t_rvalid_i && atd_c2t_rlast_i && (atd_c2t_rdata_i[11:10] == 2'd0) && (msg_code == 4'd5))
        atd_c2t_fence_ready <= 1'b1;
    else
        atd_c2t_fence_ready <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_c2t_dvm_ready <= 1'b0;
    else if (atd_c2t_rvalid_i && atd_c2t_rlast_i && (atd_c2t_rdata_i[11:10] == 2'd1) && (msg_code == 4'd5))
        atd_c2t_dvm_ready <= 1'b1;
    else
        atd_c2t_dvm_ready <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        c2t_cs <= IDLE;
    else
        c2t_cs <= c2t_ns;
end


always@(*) begin
    case (c2t_cs)
        IDLE:
            if (atd_c2t_rvalid_i && atd_c2t_connected)
                case (msg_code)
                    PTW_REQ://two doublewords
                        if (!cdw_rfifo_full_i && !atd_c2t_rlast_i)
                            c2t_ns = C2T_WAIT_PLAST;
//                      else if (!atd_c2t_rlast_i)
//                          c2t_ns = C2T_WAIT_PLAST;
                        else
                            c2t_ns = IDLE;
                    INV_ACK,FENCE_ACK://one dw
                        if (atd_c2t_rready)
                            c2t_ns = C2T_END;
                        else
                            c2t_ns = IDLE;
                    CFG_ACK://one dw
                        if (atd_c2t_rready)
                            c2t_ns = C2T_END;
                        else
                            c2t_ns = IDLE;
                    FAULT_RPT://five dws
                        if (acd_fq_ready_i && !atd_c2t_rlast_i)
                            c2t_ns = C2T_WAIT_FLAST;
                        else if (!atd_c2t_rlast_i)
                            c2t_ns = C2T_WAIT_FLAST;
                        else
                            c2t_ns = IDLE;
                    INT_RPT://one dw
                        if (atd_c2t_rready)
                            c2t_ns = C2T_END;
                        else
                            c2t_ns = IDLE;
                    DBG_RPT://two dws
                        if (atd_c2t_rready)
                            c2t_ns = C2T_WAIT_DLAST;
                        else
                            c2t_ns = IDLE;
                    default:
                        c2t_ns = IDLE;
                endcase
            else
                c2t_ns = IDLE;
        C2T_END:
                c2t_ns = IDLE;
        C2T_WAIT_PLAST,C2T_WAIT_FLAST,C2T_WAIT_DLAST:
            if (atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
                c2t_ns = IDLE;
            else
                c2t_ns = c2t_cs;
        default:
            c2t_ns = IDLE;
    endcase
end


assign atd_c2t_rready_o = atd_c2t_rready;

always@(*) begin
    if (!iommu_rstn)
        atd_c2t_rready = 1'b0;
    else if ((c2t_cs == IDLE) && atd_c2t_rvalid_i && atd_c2t_rlast_i && (msg_code == 4'd0))
        atd_c2t_rready = 1'b1;
    else if (((c2t_cs == IDLE) && (msg_code == PTW_REQ) && atd_c2t_rvalid_i && !cdw_rfifo_full_i) || ((c2t_cs == C2T_WAIT_PLAST) && atd_c2t_rvalid_i && atd_c2t_rlast_i))
        atd_c2t_rready = 1'b1;
    else if ((c2t_cs == IDLE) && (msg_code == CFG_ACK) && atd_c2t_rvalid_i && atd_c2t_rlast_i)
        atd_c2t_rready = 1'b1;
    else if ((c2t_cs == IDLE) && (msg_code == INV_ACK) && atd_c2t_rvalid_i && atd_c2t_rlast_i)
        atd_c2t_rready = 1'b1;
    else if ((c2t_cs == IDLE) && (msg_code == FENCE_ACK) && atd_c2t_rvalid_i && atd_c2t_rlast_i)
        atd_c2t_rready = 1'b1;
    else if (((c2t_cs == IDLE) && (msg_code == FAULT_RPT) && atd_c2t_rvalid_i && !acd_fq_ready_i) || ((c2t_cs == C2T_WAIT_FLAST) && atd_c2t_rvalid_i))
        atd_c2t_rready = 1'b1;
    else if ((c2t_cs == IDLE) && (msg_code == INT_RPT) && atd_c2t_rvalid_i && atd_c2t_rlast_i)
        atd_c2t_rready = 1'b1;
    else if (((c2t_cs == IDLE) && (msg_code == DBG_RPT) && atd_c2t_rvalid_i) || ((c2t_cs == C2T_WAIT_DLAST) && atd_c2t_rvalid_i && atd_c2t_rlast_i))
        atd_c2t_rready = 1'b1;
    else
        atd_c2t_rready = 1'b0;
end

////////**********************************************************************************************************///////
//get ptw_req to cdw module
////////**********************************************************************************************************///////
assign cdw_rfifo_wen_o = cdw_rfifo_wen;
assign cdw_rfifo_wdata_o = cdw_rfifo_wdata;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_rfifo_wen <= 1'b0;
    else if ((c2t_cs == C2T_WAIT_PLAST) && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        cdw_rfifo_wen <= 1'b1;
    else
        cdw_rfifo_wen <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_rfifo_data <= 64'd0;
    else if ((c2t_cs == IDLE) && (msg_code == PTW_REQ) && atd_c2t_rvalid_i && atd_c2t_rready && !atd_c2t_rlast_i)
        cdw_rfifo_data <= atd_c2t_rdata_i;
    else
        cdw_rfifo_data <= cdw_rfifo_data;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_rfifo_wdata <= 128'd0;
    else if ((c2t_cs == C2T_WAIT_PLAST) && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        cdw_rfifo_wdata <= {atd_c2t_rdata_i,cdw_rfifo_data} ;
    else
        cdw_rfifo_wdata <= 128'd0;
end

////////**********************************************************************************************************///////
//get fault_req to fq module
////////**********************************************************************************************************///////
assign acd_fq_valid_o = acd_fq_valid;
assign acd_fq_record_o = acd_fq_record;
assign atd_fq_ack_valid_o = acd_fq_valid;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
       acd_fq_valid  <= 1'b0;
    else if ((c2t_cs == C2T_WAIT_FLAST) && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        acd_fq_valid <= 1'b1;
    else
        acd_fq_valid <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        acd_fq_record_cnt <= 2'd0;
    else if (c2t_cs == IDLE)
        acd_fq_record_cnt <= 2'd0;
    else if ((c2t_cs == C2T_WAIT_FLAST) && atd_c2t_rvalid_i && atd_c2t_rready)
        acd_fq_record_cnt <= acd_fq_record_cnt + 1;
    else
        acd_fq_record_cnt <= acd_fq_record_cnt;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        acd_fq_record <= 256'd0;
    else if (atd_c2t_rvalid_i && atd_c2t_rready)
        case (acd_fq_record_cnt)
            2'd0:
                acd_fq_record <= {acd_fq_record[64*4-1:64*1],atd_c2t_rdata_i};
            2'd1:
                acd_fq_record <= {acd_fq_record[64*4-1:64*2],atd_c2t_rdata_i,acd_fq_record[64*1-1:64*0]};
            2'd2:
                acd_fq_record <= {acd_fq_record[64*4-1:64*3],atd_c2t_rdata_i,acd_fq_record[64*2-1:64*0]};
            2'd3:
                acd_fq_record <= {atd_c2t_rdata_i,acd_fq_record[64*3-1:64*0]};
            default:
                acd_fq_record <= 256'd0;
        endcase
    else
        acd_fq_record <= acd_fq_record;
end


////////**********************************************************************************************************///////
//get read register to apb module
////////**********************************************************************************************************///////
//assign atd_c2t_dbg_ack_o = atd_c2t_dbg_ack;//acd no ack
assign atd_c2t_dbg_ack_o = 1'b1;
assign iommu_tr_req_busy_o = iommu_tr_req_busy;
assign iommu_tr_resp_o = iommu_tr_resp;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_c2t_dbg_ack <= 1'b0;
    else if ((c2t_cs == IDLE) && (msg_code == DBG_RPT) && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        atd_c2t_dbg_ack <= 1'b1;
    else if ((c2t_cs == C2T_WAIT_DLAST) && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        atd_c2t_dbg_ack <= 1'b1;
    else if (atd_c2t_dbg_end_i)
        atd_c2t_dbg_ack <= 1'b0;
    else
        atd_c2t_dbg_ack <= atd_c2t_dbg_ack;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_tr_req_go <= 1'b0;
    else
        iommu_tr_req_go <= iommu_tr_req_go_i;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_tr_req_busy <= 1'b0;
    else if (!iommu_tr_req_go && iommu_tr_req_go_i)
        iommu_tr_req_busy <= 1'b1;
    else if (c2t_cs == C2T_WAIT_DLAST && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        iommu_tr_req_busy <= 1'b0;
    else
        iommu_tr_req_busy <= iommu_tr_req_busy;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_tr_resp <= 64'd0;
    else if (c2t_cs == C2T_WAIT_DLAST && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        iommu_tr_resp <= atd_c2t_rdata_i;
    else
        iommu_tr_resp <= iommu_tr_resp;
end


assign atd_c2t_cfg_ack_o = atd_c2t_cfg_ack;
assign atd_c2t_cfg_rdata_o = atd_c2t_cfg_rdata;
assign atd_c2t_iocntovf_o = atd_c2t_iocntovf;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_c2t_cfg_ack <= 1'b0;
    else if ((c2t_cs == IDLE) && (msg_code == CFG_ACK) && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        atd_c2t_cfg_ack <= 1'b1;
    else if (atd_c2t_cfg_end_i)
        atd_c2t_cfg_ack <= 1'b0;
    else
        atd_c2t_cfg_ack <= atd_c2t_cfg_ack;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_c2t_cfg_rdata <= 32'd0;
    else if ((c2t_cs == IDLE) && (msg_code == CFG_ACK) && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        atd_c2t_cfg_rdata <= atd_c2t_rdata_i[63:32];
    else
        atd_c2t_cfg_rdata <= atd_c2t_cfg_rdata;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        atd_c2t_iocntovf <= 31'd0;
    else if (atd_pmip_clr_i)
        atd_c2t_iocntovf <= 31'd0;
    else if ((c2t_cs == IDLE) && (msg_code == INT_RPT) && atd_c2t_rvalid_i && atd_c2t_rready && atd_c2t_rlast_i)
        atd_c2t_iocntovf <= atd_c2t_rdata_i[35:5];
    else
        atd_c2t_iocntovf <= atd_c2t_iocntovf;
end


endmodule



