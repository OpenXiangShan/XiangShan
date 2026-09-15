///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_cdw.sv
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

module iommu_atd_cdw #(
//    parameter RISCV_VLEN          = 6'd39,//39
    parameter RISCV_PLEN            = 6'd56
) (
    input logic                     iommu_clk,
    input logic                     iommu_rstn,

    // from regmap
    input   [43:0]                  ddtp_ppn_i,
    input   [3:0]                   ddtp_mode_i,
    input   [63:0]                  iommu_caps_i,
    input                           fctl_be_i,
    input                           fctl_gxl_i,

    //from input req
    input                           cdw_rfifo_wen_i,
    input   [127:0]                 cdw_rfifo_din_i,
    output                          cdw_rfifo_full_o,

    input                           ats_req_wen_i,
    input   [191:0]                 ats_req_wdata_i,
    output                          ats_req_full_o,

    input                           pri_req_wen_i,
    input   [127:0]                 pri_req_wdata_i,
    output                          pri_req_full_o,

    output                          lookup_dc_valid_o,
    input                           lookup_dc_ready_i,
    output  [23:0]                  lookup_dc_did_o,

    //between cdw and acd , cdw and ddtc
//  input                           ddtc_access_i,//from acd
    input                           ddtc_hit_valid_i,
    input                           ddtc_hit_i,//from ddtc
    input  [1:0]                    ddtc_hit_lvl_i,
    input  [511:0]                  ddtc_hit_content_i,

//  output                          update_dc_o,
    output                          up_dc_valid_o,
    input                           up_dc_ready_i,
    output                          up_dc_leaf_o,
    output  [1:0]                   up_dc_lvl_o,
    output  [23:0]                  up_dc_did_o,
    output  [511:0]                 up_dc_content_o,

//between cdw and pdtc
    output                          lookup_pc_valid_o,
    input                           lookup_pc_ready_i,
    output  [19:0]                  lookup_pc_pid_o,
    output  [23:0]                  lookup_pc_did_o,

//  output                          pdtc_access_o,
    input                           pdtc_hit_valid_i,
    input                           pdtc_hit_i,
    input  [1:0]                    pdtc_hit_lvl_i,
    input  [127:0]                  pdtc_hit_content_i,

//  output                          update_pc_o,
    output                          up_pc_valid_o,
    input                           up_pc_ready_i,
    output                          up_pc_leaf_o,
    output  [1:0]                   up_pc_lvl_o,
    output  [19:0]                  up_pc_pid_o,
    output  [23:0]                  up_pc_did_o,
    output  [127:0]                 up_pc_content_o,

    //between cdw and ptw
    input                           cdw_flush_i,
    input                           implicit_done_i,
    input   [43:0]                  implicit_ppn_i,

    output                          ptw_rfifo_wen_o,
    output  [464:0]                 ptw_rfifo_din_o,
    input                           ptw_rfifo_full_i,

    //to FQ
    output                          cdw_fq_valid_o,
    output  [255:0]                 cdw_fq_record_o,
    input                           cdw_fq_ready_i,

    //output to t2r and then to pcie-rc
    output                          pri_resp_wen_o,
    output  [79:0]                  pri_resp_wdata_o,
    input                           pri_resp_full_i,

    //to pq
    output                          pq_pri_wen_o,
    output  [127:0]                 pq_pri_wdata_o,
    input                           pq_pri_full_i,

    // to HPM
    output                          ddt_walk_o,
    output                          pdt_walk_o,
    output  [23:0]                  cdw_did_o,
    output                          cdw_pv_o,
    output  [19:0]                  cdw_pid_o,

    output                          cdw_arvalid_o,
    input                           cdw_arready_i,
    output  [3:0]                   cdw_arid_o,
    output  [RISCV_PLEN-1:0]        cdw_araddr_o,
    output  [7:0]                   cdw_arlen_o,
    input                           cdw_rvalid_i,
    output                          cdw_rready_o,
    input                           cdw_rlast_i,
    input   [3:0]                   cdw_rid_i,
    input   [1:0]                   cdw_rresp_i,
    input   [255:0]                 cdw_rdata_i
);



//  Extended IOMMU fault cases
    localparam  logic   [1:0]   BURST_INCR                  = 2'b01;
    localparam  logic   [1:0]   RESP_OKAY                   = 2'b00;
    localparam  logic   [11:0]  ALL_INB_TRANSACTIONS_DISALLOWED = 12'd256;// IOMMU off / ATS requested and not supported
//  localparam  logic   [11:0]  DDT_ENTRY_LD_ACCESS_FAULT   = 12'd257;  // PMP/PMA fault when accessing 'ddtp' or 'DC'
    localparam  logic   [11:0]  DDT_ENTRY_INVALID           = 12'd258;
    localparam  logic   [11:0]  DDT_ENTRY_MISCONFIGURED     = 12'd259;
    localparam  logic   [11:0]  TRANS_TYPE_DISALLOWED       = 12'd260;
//  localparam  logic   [11:0]  PDT_ENTRY_LD_ACCESS_FAULT   = 12'd265;  // PMP/PMA fault when accessing 'PC'
    localparam  logic   [11:0]  PDT_ENTRY_INVALID           = 12'd266;
    localparam  logic   [11:0]  PDT_ENTRY_MISCONFIGURED     = 12'd267;
    localparam  logic   [11:0]  DDT_DATA_CORRUPTION         = 12'd268;
    localparam  logic   [11:0]  PDT_DATA_CORRUPTION         = 12'd269;

    // FSM states
    enum logic [4:0] {
    IDLE            ,
    CDW_PRI_REQ     ,
    CDW_ATS_REQ     ,
    CDW_ACD_REQ     ,
    CDW_LOOKUP_DELAY,
    CDW_LOOKUP_REQ  ,
    CDW_LOOKUP_ACK  ,
    DC_L2_REQ       ,
    DC_L2_ENTRY     ,
    DC_L1_REQ       ,
    DC_L1_ENTRY     ,
    DC_L0_REQ       ,
    DC_L0_ENTRY     ,
    DC_UPDATE_ACK   ,
    PC_L2_REQ       ,
    PC_L2_ENTRY     ,
    PC_L1_REQ       ,
    PC_L1_ENTRY     ,
    PC_L0_REQ       ,
    PC_L0_ENTRY     ,
    PC_UPDATE_ACK   ,
    GUEST_TR        ,
    GUEST_DONE      ,
    CDW_END         ,
    ERROR
    } cdw_cs,cdw_ns;

    //------------------------
    //#  Context Fields
    //------------------------

    // MSI Address Pattern
    typedef struct packed {
        logic [11:0]    rsv;
        logic [52-1:0]  pattern;
    } msi_addr_pattern_t;

    // MSI Address Mask
    typedef struct packed {
        logic [11:0]    rsv;
        logic [52-1:0]  mask;
    } msi_addr_mask_t;

    // MSI Page Table Pointer
    typedef struct packed {
        logic [3:0]     mode;
        logic [15:0]    rsv;
        logic [43:0]    ppn;
    } msiptp_t;

    // First Stage Context
    typedef struct packed {
        logic [3:0]     mode;
        logic [15:0]    rsv;
        logic [43:0]    ppn;
    } fsc_t;

    // Translation Attributes for Device Context
    typedef struct packed {
        logic [31:0] rsv_2;
        logic [19:0] pscid;
        logic [11:0] rsv_1;
    } dc_ta_t;

    // Translation Attributes for Process Context
    typedef struct packed {
        logic [31:0]    rsv_2;
        logic [19:0]    pscid;
        logic [8:0]     rsv_1;
        logic           sum;
        logic           ens;
        logic           v;
    } pc_ta_t;

    // IO Hypervisor Guest Address Translation and Protection
    typedef struct packed {
        logic [3:0]     mode;
        logic [15:0]    gscid;
        logic [43:0]    ppn;
    } iohgatp_t;

    // Translation Control
   typedef struct packed {
        logic [31:0]    rsv_2;
        logic [7:0]     custom;
        logic [11:0]    rsv_1;
        logic           sxl;
        logic           sbe;
        logic           dpe;
        logic           sade;
        logic           gade;
        logic           prpr;
        logic           pdtv;
        logic           dtf;
        logic           t2gpa;
        logic           en_pri;
        logic           en_ats;
        logic           v;
   } tc_t;

   // Non-leaf DDT/PDT entry (64-bits)
    typedef struct packed {
        logic [9:0]     rsv_2;
        logic [43:0]    ppn;
        logic [8:0]     rsv_1;
        logic           v;
    } entry_t;


    logic                   cdw_arvalid;
    logic                   cdw_rready;

    logic [255:0]           cdw_rdata;
    logic [511:0]           cdw_entry;

    logic                   cdw_rfifo_empty;
    logic                   cdw_rfifo_ren;
    logic [127:0]           cdw_rfifo_rdata;

    logic                   ats_req_empty;
    logic                   ats_req_ren;
    logic [191:0]           ats_req_rdata;
    logic                   pri_req_empty;
    logic                   pri_req_ren;
    logic [127:0]           pri_req_rdata;

    logic [127:0]           cdw_req_data;
    logic                   cdw_req_en;
    logic                   cdw_req_en_d;
    logic [1:0]             cdw_req_mode;

    logic [7:0]             ptw_req_idx;
    logic                   req_pv;
    logic [19:0]            req_pid;
    logic [23:0]            req_did;
    logic [5:0]             trans_type;
    logic                   priv_lvl;

      // To determine if request is translated or untranslated
    logic                   is_translated;
    logic                   is_rx;
    logic                   is_store;
    logic                   cdw_ats_req;

    logic                   ddtc_hit_valid;
    logic                   ddtc_hit;
    logic                   pdtc_hit_valid;
    logic                   pdtc_hit;

    logic                   lookup_dc_valid;
    logic                   lookup_pc_valid;

//    logic                   cdw_ddtc_end;
//    logic                   cdw_pdtc_end1;
//    logic                   cdw_pdtc_end2;

    logic [511:0]           dc_content;
    logic [127:0]           pc_content;
    logic [511:0]           dc_content_q;
    logic [127:0]           pc_content_q;

    logic                   cdw_implicit_access;
    logic                   cdw_s1_en;
    logic                   cdw_s2_en;
    logic [3:0]             pdtp_mode;
    logic [3:0]             iosatp_mode;
//  logic                   cdw_pc_sum;
    logic [51:0]            req_iova;
    logic [43:0]            cdw_gppn;
    logic [1:0]             cdw_gppn_mode;
    logic [43:0]            cdw_iohgatp_ppn;
    logic [43:0]            cdw_iosatp_ppn;
    logic [15:0]            cdw_gscid;
    logic [19:0]            cdw_pscid;

//  logic                   pri_prg_index8bit;
    logic                   pri_last;
    logic                   ats_req_InD;
    logic                   cdw_is_error;
    logic [151:0]           msi_para;
    logic [19:0]            ptw_acd_para;
    logic [60:0]            ptw_trans_para;
    logic                   ptw_rfifo_wen;
    logic [464:0]           ptw_rfifo_din;

    logic                   ptw_is_bare;

    logic [2:0]             cdw_did_sel;
    logic [2:0]             cdw_did_sel_q;
    logic [63:0]            dc_entry;
    logic [2:0]             cdw_pid_sel;
    logic [2:0]             cdw_pid_sel_q;
    logic [63:0]            pc_entry;

    logic                   ddt_walk;
    logic                   pdt_walk;
    logic                   up_dc_valid;
//  logic                   up_dc_valid_d;
    logic [511:0]           up_dc_content;

    logic                   up_pc_valid;
    logic [127:0]           up_pc_content;
    // Cast read port to corresponding data structure
    msi_addr_pattern_t      msi_addr_pattern;
    msi_addr_mask_t         msi_addr_mask;
    msiptp_t                msiptp;
    tc_t                    dc_tc;
    iohgatp_t               dc_iohgatp;
    dc_ta_t                 dc_ta;
    fsc_t                   dc_fsc;
    pc_ta_t                 pc_ta;
    fsc_t                   pc_fsc;
    entry_t                 entry;
    logic [63:0]            entry_q;
    logic [1:0]             quest_tr_mode;
    logic                   cdw_implicit_done;
    logic [43:0]            cdw_implicit_ppn;
    // Physical pointer to access memory bus
    logic [RISCV_PLEN-1:0]  cdw_pptr_q;

    // Enable MSI DC fields configuration checks
    logic                   en_msi_check;
    logic   [11:0]          cdw_cause_code;
    logic                   pid_wide_error;
    // Signal MSI field config error to main FSM
    logic                   msi_check_error;
    logic                   cdw_error_flag;
    logic                   cdw_error_flag_dtf;
    logic                   cdw_fq_valid;

    logic [1:0]             pri_resp;
    logic                   pri_resp_en;
    logic                   pri_resp_wen;
    logic [79:0]            pri_resp_wdata;

    logic                   pq_pri_en;
    logic                   pq_pri_wen;
    logic [127:0]           pq_pri_wdata;

    logic                   sv39;
    logic                   sv48;
    logic                   sv57;
    logic                   svpbmt;
    logic                   sv39x4;
    logic                   sv48x4;
    logic                   sv57x4;
    logic                   amo_mrif;
    logic                   msi_flat;
    logic                   msi_mrif;
    logic                   amo_hwad;
    logic                   ats;
    logic                   t2gpa;
    logic                   hpm;
    logic                   dbg;
    logic                   pd8;
    logic                   pd17;
    logic                   pd20;


// Output values
// AXI parameters
// AR
assign cdw_arid_o = 4'b0001;
assign cdw_araddr_o = {cdw_pptr_q[RISCV_PLEN-1:6],6'd0};                       // Physical address to access
assign cdw_arlen_o = 8'd1;

assign cdw_arvalid_o = cdw_arvalid;
assign cdw_rready_o = cdw_rready;

always@(*) begin
    if ((cdw_cs == DC_L2_REQ) || (cdw_cs == DC_L1_REQ) || (cdw_cs == DC_L0_REQ) ||
        (cdw_cs == PC_L2_REQ) || (cdw_cs == PC_L1_REQ) || (cdw_cs == PC_L0_REQ))
        cdw_arvalid = 1'b1;
    else
        cdw_arvalid = 1'b0;
end

// R
always@(*) begin
    if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY) || (cdw_cs == DC_L0_ENTRY) ||
         (cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY) || (cdw_cs == PC_L0_ENTRY)) && cdw_rvalid_i)
        cdw_rready = 1'b1;
    else
        cdw_rready = 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_rdata <= 256'd0;
    else if (cdw_rvalid_i && !cdw_rlast_i)
        cdw_rdata <= cdw_rdata_i;
end

assign cdw_entry = {cdw_rdata_i,cdw_rdata};


atd_sync_fifo
    #(
    .FIFO_WID       (128),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
//  .FIFO_DEPTH_WID (2),
//    .FIFO_DEPTH   (4),
    .REG_OUT        (0)
    )
u0_cdw_sync_fifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (cdw_rfifo_wen_i),
    .ren                (cdw_rfifo_ren),
    .din                (cdw_rfifo_din_i),
    .dout               (cdw_rfifo_rdata),
    .full               (cdw_rfifo_full_o),
    .empty              (cdw_rfifo_empty)
   );

atd_sync_fifo
    #(
    .FIFO_WID       (192),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
//  .FIFO_DEPTH_WID (2),
//    .FIFO_DEPTH   (4),
    .REG_OUT        (0)
    )
u1_ats_req_fifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (ats_req_wen_i),
    .ren                (ats_req_ren),
    .din                (ats_req_wdata_i),
    .dout               (ats_req_rdata),
    .full               (ats_req_full_o),
    .empty              (ats_req_empty)
   );

atd_sync_fifo
    #(
    .FIFO_WID       (128),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
//  .FIFO_DEPTH_WID (2),
//    .FIFO_DEPTH   (4),
    .REG_OUT        (0)
    )
u2_pri_req_fifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (pri_req_wen_i),
    .ren                (pri_req_ren),
    .din                (pri_req_wdata_i),
    .dout               (pri_req_rdata),
    .full               (pri_req_full_o),
    .empty              (pri_req_empty)
   );


 //fifo out signals
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pri_req_ren <= 1'b0;
    else if (cdw_cs == CDW_PRI_REQ)
        pri_req_ren <= 1'b1;
    else
        pri_req_ren <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_req_ren <= 1'b0;
    else if (cdw_cs == CDW_ATS_REQ)
        ats_req_ren <= 1'b1;
    else
        ats_req_ren <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_rfifo_ren <= 1'b0;
    else if (cdw_cs == CDW_ACD_REQ)
        cdw_rfifo_ren <= 1'b1;
    else
        cdw_rfifo_ren <= 1'b0;
end

assign cdw_req_en = pri_req_ren || ats_req_ren || cdw_rfifo_ren;

//pri_last=pri_req_rdata[10];
//priv=pri_req_rdata[6];
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_req_data <= 'd0;
    else if (pri_req_ren)
        cdw_req_data <= {pri_req_rdata[127:76],1'd0,pri_req_rdata[10:7],pri_req_rdata[6],6'd1,ats_req_rdata[55:32],pri_req_rdata[31:12],pri_req_rdata[11],6'd0,pri_req_rdata[72:64],pri_req_rdata[3:0]};
    else if (ats_req_ren)//52+12+24+20+4+16
        cdw_req_data <= {ats_req_rdata[159:108],12'd1,ats_req_rdata[55:32],ats_req_rdata[95:76],ats_req_rdata[21],3'd0,ats_req_rdata[19:8],ats_req_rdata[3:0]};
    else if (cdw_rfifo_ren)
        cdw_req_data <= cdw_rfifo_rdata;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_req_en_d <= 1'b0;
    else
        cdw_req_en_d <= cdw_req_en;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_req_mode <= 2'd0;
    else if (pri_req_ren)
        cdw_req_mode <= 2'd2;
    else if (ats_req_ren)
        cdw_req_mode <= 2'd1;
    else if (cdw_rfifo_ren)
        cdw_req_mode <= 2'd0;
    else
        cdw_req_mode <= cdw_req_mode;
end

assign ptw_req_idx = cdw_req_data[11:4];
//assign pri_prg_index8bit = cdw_req_data[12];
assign ats_req_InD = cdw_req_data[18] && cdw_req_data[16];
assign req_pv = cdw_req_data[19];
assign req_pid = cdw_req_data[39:20];
assign req_did = cdw_req_data[63:40];
assign trans_type = cdw_req_data[69:64];
assign priv_lvl = cdw_req_data[70];
assign pri_last = cdw_req_data[74];
assign req_iova = cdw_req_data[127:76];


//translate type
assign is_translated = (!trans_type[3] && trans_type[2]);
assign is_store = ((&trans_type[1:0] == 1'b1) && (!trans_type[3]));
assign is_rx = (!trans_type[3] && !trans_type[1] && trans_type[0]);
assign cdw_ats_req = (cdw_req_mode == 2'd1);



////////**********************************************************************************************************///////
//regiter input signals
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) begin
        ddtc_hit_valid <= 1'b0;
        ddtc_hit <= 1'b0;
        pdtc_hit_valid <= 1'b0;
        pdtc_hit <= 1'b0;
    end else begin
        ddtc_hit_valid <= ddtc_hit_valid_i;
        ddtc_hit <=  ddtc_hit_i;
        pdtc_hit_valid <= pdtc_hit_valid_i;
        pdtc_hit <=  pdtc_hit_i;
    end
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_cs <= IDLE;
    else
        cdw_cs <= cdw_ns;
end


//assign cdw_ddtc_end = ddtc_hit_valid_i && ddtc_hit_i && (dc_tc.pdtv == 1'b0);
//assign cdw_pdtc_end1 = pdtc_hit_valid_i && pdtc_hit_i && (dc_fsc.mode != 4'd0);
//assign cdw_pdtc_end2 = pdtc_hit_valid_i && pdtc_hit_i && (dc_fsc.mode == 4'd0) && (dc_iohgatp.mode != 4'd0);


always@(*) begin
    case (cdw_cs)
        IDLE:
            if (!pri_req_empty)
                cdw_ns = CDW_PRI_REQ;
            else if(!ats_req_empty)
                cdw_ns = CDW_ATS_REQ;
            else if(!cdw_rfifo_empty)
                cdw_ns = CDW_ACD_REQ;
            else
                cdw_ns = IDLE;
        CDW_PRI_REQ,CDW_ATS_REQ,CDW_ACD_REQ:
            cdw_ns = CDW_LOOKUP_DELAY;
        CDW_LOOKUP_DELAY:
            cdw_ns = CDW_LOOKUP_REQ;
        CDW_LOOKUP_REQ:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (lookup_dc_valid && lookup_dc_ready_i)
                cdw_ns = CDW_LOOKUP_ACK;
            else if (lookup_pc_valid && lookup_pc_ready_i)
                cdw_ns = CDW_LOOKUP_ACK;
            else
                cdw_ns = CDW_LOOKUP_REQ;
        CDW_LOOKUP_ACK:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (ddtc_hit_valid_i && ddtc_hit_i)
                if ((ddtc_hit_lvl_i == 2'd2) && (ddtp_mode_i == 4'd4))
                    cdw_ns = DC_L2_ENTRY;
                else if ((ddtc_hit_lvl_i == 2'd1) && ((ddtp_mode_i == 4'd4) || (ddtp_mode_i == 4'd3)))
                    cdw_ns = DC_L1_ENTRY;
                else if ((ddtc_hit_lvl_i == 2'd0) && dc_tc.pdtv && (req_pv || dc_tc.dpe))
                    cdw_ns = CDW_LOOKUP_REQ;
                else if ((ddtc_hit_lvl_i == 2'd0) && !dc_tc.pdtv)
                    cdw_ns = CDW_END;
                else
                    cdw_ns = CDW_LOOKUP_ACK;
            else if (ddtc_hit_valid_i && !ddtc_hit_i)
                if (ddtp_mode_i == 4'd4)
                    cdw_ns = DC_L2_REQ;
                else if (ddtp_mode_i == 4'd3)
                    cdw_ns = DC_L1_REQ;
                else if (ddtp_mode_i == 4'd2)
                    cdw_ns = DC_L0_REQ;
                else
                    cdw_ns = ERROR;//judged by acd
            else if (pdtc_hit_valid_i && pdtc_hit_i)
                if ((pdtc_hit_lvl_i == 2'd2) && (dc_fsc.mode == 4'd3))// PD20
                    cdw_ns = PC_L2_ENTRY;
                else if ((pdtc_hit_lvl_i == 2'd1) && ((dc_fsc.mode == 4'd3) || (dc_fsc.mode == 4'd2)))// PD17
                    cdw_ns = PC_L1_ENTRY;
                else if (pdtc_hit_lvl_i == 2'd0)// PD8
                    cdw_ns = CDW_END;
                else
                    cdw_ns = CDW_LOOKUP_ACK;
            else if (pdtc_hit_valid_i && !pdtc_hit_i)
                if (dc_fsc.mode == 4'd3)// PD20
                    cdw_ns = PC_L2_REQ;
                else if (dc_fsc.mode == 4'd2)// PD17
                    cdw_ns = PC_L1_REQ;
                else if (dc_fsc.mode == 4'd1)// PD8
                    cdw_ns = PC_L0_REQ;
                else
                    cdw_ns = CDW_LOOKUP_ACK;
//          else if ((up_dc_valid || up_dc_valid_d)  && dc_tc.pdtv)
//              cdw_ns = CDW_LOOKUP_REQ;
            else
                cdw_ns = CDW_LOOKUP_ACK;
        DC_L2_REQ:
            if (cdw_arready_i)
                cdw_ns = DC_L2_ENTRY;
            else
                cdw_ns = DC_L2_REQ;
        DC_L2_ENTRY:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if ((cdw_rvalid_i && cdw_rlast_i) || (ddtc_hit_valid && ddtc_hit))
                cdw_ns = DC_L1_REQ;
            else
                cdw_ns = DC_L2_ENTRY;
        DC_L1_REQ:
            if (cdw_arready_i)
                cdw_ns = DC_L1_ENTRY;
            else
                cdw_ns = DC_L1_REQ;
        DC_L1_ENTRY:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if ((cdw_rvalid_i && cdw_rlast_i) || (ddtc_hit_valid && ddtc_hit))
                cdw_ns = DC_L0_REQ;
            else
                cdw_ns = DC_L1_ENTRY;
        DC_L0_REQ:
            if (cdw_arready_i)
                cdw_ns = DC_L0_ENTRY;
            else
                cdw_ns = DC_L0_REQ;
        DC_L0_ENTRY:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (cdw_rvalid_i && cdw_rlast_i && (dc_iohgatp.mode != 'd0) && dc_tc.pdtv)
                cdw_ns  = GUEST_TR;
            else if (cdw_rvalid_i && cdw_rlast_i)
                cdw_ns  = DC_UPDATE_ACK;
            else
                cdw_ns = DC_L0_ENTRY;
        DC_UPDATE_ACK:
            if (up_dc_ready_i && dc_tc.pdtv)
//              cdw_ns  = CDW_LOOKUP_ACK;
                cdw_ns  = CDW_LOOKUP_REQ;
            else if (up_dc_ready_i)
                cdw_ns  = CDW_END;
            else
                cdw_ns = DC_UPDATE_ACK;
        PC_L2_REQ:
            if (cdw_arready_i)
                cdw_ns = PC_L2_ENTRY;
            else
                cdw_ns = PC_L2_REQ;
        PC_L2_ENTRY:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (cdw_rvalid_i && cdw_rlast_i && (dc_iohgatp.mode != 'd0) && dc_tc.pdtv)
                cdw_ns = GUEST_TR;
            else if (cdw_rvalid_i && cdw_rlast_i)
                cdw_ns = PC_L1_REQ;
            else
                cdw_ns = PC_L2_ENTRY;
        PC_L1_REQ:
            if (cdw_arready_i)
                cdw_ns = PC_L1_ENTRY;
            else
                cdw_ns = PC_L1_REQ;
        PC_L1_ENTRY:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (cdw_rvalid_i && cdw_rlast_i && (dc_iohgatp.mode != 'd0) && dc_tc.pdtv)
                cdw_ns = GUEST_TR;
            else if (cdw_rvalid_i && cdw_rlast_i)
                cdw_ns = PC_L0_REQ;
            else
                cdw_ns = PC_L1_ENTRY;
        PC_L0_REQ:
            if (cdw_arready_i)
                cdw_ns = PC_L0_ENTRY;
            else
                cdw_ns = PC_L0_REQ;
        PC_L0_ENTRY:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (cdw_rvalid_i && cdw_rlast_i)
                cdw_ns = PC_UPDATE_ACK;
            else
                cdw_ns = PC_L0_ENTRY;
        PC_UPDATE_ACK:
            if (up_pc_ready_i)
                cdw_ns  = CDW_END;
            else
                cdw_ns = PC_UPDATE_ACK;
        GUEST_TR:
            if (cdw_flush_i)
                cdw_ns = IDLE;
            else if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (!ptw_rfifo_full_i)
                cdw_ns = GUEST_DONE;
            else
                cdw_ns = GUEST_TR;
        GUEST_DONE:
            if (cdw_flush_i)
                cdw_ns = IDLE;
            else if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (implicit_done_i && dc_tc.pdtv && (quest_tr_mode == 2'b01))//update ddtp
                cdw_ns = CDW_LOOKUP_REQ;
            else if (implicit_done_i && (quest_tr_mode == 2'b10))
                cdw_ns = PC_L1_REQ;
            else if (implicit_done_i && (quest_tr_mode == 2'b11))
                cdw_ns = PC_L0_REQ;
            else
                cdw_ns = GUEST_DONE;
        CDW_END:
            if (cdw_error_flag)
                cdw_ns = ERROR;
            else if (!ptw_rfifo_full_i)
                cdw_ns = IDLE;
            else
                cdw_ns  = CDW_END;
        ERROR:
            // Check whether we have to wait for AXI transmission to end?????
            if (!ptw_rfifo_full_i || cdw_fq_ready_i)
                cdw_ns = IDLE;
            else
                cdw_ns = ERROR;
        default:
            cdw_ns = IDLE;
    endcase
end

assign lookup_dc_valid_o = lookup_dc_valid;
assign lookup_pc_valid_o = lookup_pc_valid;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        lookup_dc_valid <= 1'b0;
    else if (cdw_req_en)
        lookup_dc_valid <= 1'b0;
    else if (cdw_req_en_d && !cdw_error_flag)
        lookup_dc_valid <= 1'b1;
    else if ((cdw_cs == CDW_LOOKUP_REQ) && lookup_dc_ready_i)
        lookup_dc_valid <= 1'b0;
end

assign lookup_dc_did_o = req_did;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        lookup_pc_valid <= 1'b0;
    else if (cdw_req_en)
        lookup_pc_valid <= 1'b0;
    else if (((cdw_cs == CDW_LOOKUP_ACK) && ddtc_hit_valid_i && ddtc_hit_i && dc_tc.pdtv) || (up_dc_valid && dc_tc.pdtv))
//  else if (((cdw_cs == CDW_LOOKUP_ACK) && ddtc_hit_valid_i && ddtc_hit_i && dc_tc.pdtv) || ((up_dc_valid || up_dc_valid_d) && dc_tc.pdtv))
        lookup_pc_valid <= 1'b1;
    else if ((cdw_cs == CDW_LOOKUP_REQ) && lookup_pc_ready_i)
        lookup_pc_valid <= 1'b0;
end

assign lookup_pc_pid_o = req_pid;
assign lookup_pc_did_o = req_did;

////////**********************************************************************************************************///////
//cdw fsm control signals and gen addr
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        quest_tr_mode <= 2'd0;
    else if (cdw_error_flag || cdw_req_en)
        quest_tr_mode <= 2'd0;
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (dc_iohgatp.mode != 'd0) && dc_tc.pdtv)
        quest_tr_mode <= 2'd1;
    else if ((cdw_cs == PC_L2_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (dc_iohgatp.mode != 'd0))
        quest_tr_mode <= 2'd2;
    else if ((cdw_cs == PC_L1_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (dc_iohgatp.mode != 'd0))
        quest_tr_mode <= 2'd3;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_implicit_done <= 1'b0;
    else if (cdw_error_flag || cdw_req_en)
        cdw_implicit_done <= 1'b0;
    else if ((cdw_cs == GUEST_DONE) && (quest_tr_mode == 2'b01) && implicit_done_i)
        cdw_implicit_done <= 1'b1;
    else if ((cdw_cs == CDW_LOOKUP_ACK) && pdtc_hit_valid_i)
        cdw_implicit_done <= 1'b0;
    else
        cdw_implicit_done <= cdw_implicit_done;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_implicit_ppn <= 44'd0;
    else if (cdw_error_flag || cdw_req_en)
        cdw_implicit_ppn <= 44'd0;
    else if ((cdw_cs == GUEST_DONE) && (quest_tr_mode == 2'b01) && implicit_done_i)
        cdw_implicit_ppn <= implicit_ppn_i;
    else
        cdw_implicit_ppn <= cdw_implicit_ppn;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_pptr_q <= 'd0;
    else begin
    case (cdw_cs)
        CDW_LOOKUP_ACK:
            if (ddtc_hit_valid_i && !ddtc_hit_i)
                if (ddtp_mode_i == 4'd4)// 56 = 44 + 9 + 3
                    cdw_pptr_q <= {ddtp_ppn_i,req_did[23:15], 3'b0};
                else if (ddtp_mode_i == 4'd3)
                    cdw_pptr_q <= {ddtp_ppn_i, req_did[14:6], 3'b0};
                else if (ddtp_mode_i == 4'd2)
                    cdw_pptr_q <= {ddtp_ppn_i, req_did[5:0], 6'b0};
                else
                    cdw_pptr_q <= cdw_pptr_q;
            else if (pdtc_hit_valid_i && !pdtc_hit_i)
                if (dc_fsc.mode == 4'd3)// PD20
                    cdw_pptr_q <= {(cdw_implicit_done ? cdw_implicit_ppn : dc_fsc.ppn), 6'b0, req_pid[19:17], 3'b0};
                else if (dc_fsc.mode == 4'd2)// PD17
                    cdw_pptr_q <= {(cdw_implicit_done ? cdw_implicit_ppn : dc_fsc.ppn), req_pid[16:8], 3'b0};
                else if (dc_fsc.mode == 4'd1)// PD8
                    cdw_pptr_q <= {(cdw_implicit_done ? cdw_implicit_ppn : dc_fsc.ppn), req_pid[7:0], 4'b0};
                else
                    cdw_pptr_q <= cdw_pptr_q;
            else
                cdw_pptr_q <= cdw_pptr_q;
        DC_L2_ENTRY:
            if (cdw_error_flag)
                cdw_pptr_q <= 'd0;
            else if ((cdw_rvalid_i && cdw_rlast_i) || (ddtc_hit_valid && !ddtc_hit))
                cdw_pptr_q <= {entry.ppn, req_did[14:6], 3'b0};
            else
                cdw_pptr_q <= cdw_pptr_q;
        DC_L1_ENTRY:
            if (cdw_error_flag)
                cdw_pptr_q <= 'd0;
            else if ((cdw_rvalid_i && cdw_rlast_i) || (ddtc_hit_valid && !ddtc_hit))
                cdw_pptr_q <= {entry.ppn, req_did[5:0], 6'b0};
            else
                cdw_pptr_q <= cdw_pptr_q;
        PC_L2_ENTRY:
            if (cdw_error_flag)
                cdw_pptr_q <= 'd0;
            else if (((cdw_rvalid_i && cdw_rlast_i) || (pdtc_hit_valid && !pdtc_hit))  && !cdw_s2_en)
                cdw_pptr_q <= {entry.ppn, req_pid[16:8], 3'b0};
            else
                cdw_pptr_q <= cdw_pptr_q;
        PC_L1_ENTRY:
            if (cdw_error_flag)
                cdw_pptr_q <= 'd0;
            else if (((cdw_rvalid_i && cdw_rlast_i) || (pdtc_hit_valid && !pdtc_hit))  && !cdw_s2_en)
                cdw_pptr_q <= {entry.ppn, req_pid[7:0], 4'b0};
            else
                cdw_pptr_q <= cdw_pptr_q;
        GUEST_DONE:
            if (cdw_error_flag)
                cdw_pptr_q <= 'd0;
            else if (implicit_done_i && (quest_tr_mode == 2'b10))
                cdw_pptr_q <= {implicit_ppn_i, req_pid[16:8], 3'b0};
            else if (implicit_done_i && (quest_tr_mode == 2'b11))
                cdw_pptr_q <= {implicit_ppn_i, req_pid[7:0], 4'b0};
            else
                cdw_pptr_q <= cdw_pptr_q;
        default:
            cdw_pptr_q <= cdw_pptr_q;
    endcase
    end
end

////////**********************************************************************************************************///////
//get implicit access signals
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_implicit_access <= 1'b0;
    else if ((cdw_cs == GUEST_TR) && !ptw_rfifo_full_i)
        cdw_implicit_access <= 1'b1;
    else
        cdw_implicit_access <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_is_bare <= 1'b0;
    else if (((cdw_cs == CDW_END) || (cdw_cs == GUEST_TR)) && !cdw_error_flag && !cdw_flush_i)
        if ((is_translated && !dc_tc.t2gpa) || ((dc_iohgatp.mode == 4'd0) && ((dc_tc.pdtv && pc_fsc.mode == 4'd0) || (!dc_tc.pdtv && (dc_fsc.mode == 4'd0)))))
            ptw_is_bare <= 1'b1;
        else
            ptw_is_bare <= 1'b0;
    else
        ptw_is_bare <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_gppn <= 44'd0;
    else if (cdw_req_en)
        cdw_gppn <= 44'd0;
    else if ((cdw_cs == DC_L0_ENTRY) && ((cdw_rvalid_i && cdw_rlast_i) || (ddtc_hit_valid && ddtc_hit)))
        cdw_gppn <= dc_fsc.ppn;
    else if (((cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY)) && ((cdw_rvalid_i && cdw_rlast_i) || (pdtc_hit_valid && pdtc_hit)) && (dc_iohgatp.mode != 'd0))
        cdw_gppn <= entry.ppn;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_gppn_mode <= 2'd0;
    else if (cdw_req_en)
        cdw_gppn_mode <= 2'd0;
    else if ((cdw_cs == DC_L0_ENTRY) && ((cdw_rvalid_i && cdw_rlast_i) || (ddtc_hit_valid && ddtc_hit)) && (dc_iohgatp.mode != 'd0))
        if (dc_tc.pdtv && (dc_fsc.mode == 4'd3))
            cdw_gppn_mode <= 2'd2;
        else if (dc_tc.pdtv && (dc_fsc.mode == 4'd2))
            cdw_gppn_mode <= 2'd1;
        else if (dc_tc.pdtv && (dc_fsc.mode == 4'd1))
            cdw_gppn_mode <= 2'd0;
        else
            cdw_gppn_mode <= cdw_gppn_mode;
    else if ((cdw_cs == PC_L2_ENTRY) && ((cdw_rvalid_i && cdw_rlast_i) || (pdtc_hit_valid && pdtc_hit)) && (dc_iohgatp.mode != 'd0))
        cdw_gppn_mode <= 2'd1;
    else if ((cdw_cs == PC_L1_ENTRY) && ((cdw_rvalid_i && cdw_rlast_i) || (pdtc_hit_valid && pdtc_hit)) && (dc_iohgatp.mode != 'd0))
        cdw_gppn_mode <= 2'd0;
    else
        cdw_gppn_mode <= cdw_gppn_mode;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_iohgatp_ppn <= 44'd0;
    else if ((is_translated && dc_tc.t2gpa) || (dc_iohgatp.mode != 4'd0))
        cdw_iohgatp_ppn <= dc_iohgatp.ppn;
    else
        cdw_iohgatp_ppn <= cdw_iohgatp_ppn;
end

//hit content
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_iosatp_ppn <= 44'd0;
    else if (is_translated)
        cdw_iosatp_ppn <= 44'd0;
    else if (!dc_tc.pdtv)
        cdw_iosatp_ppn <= dc_fsc.ppn;
    else
        cdw_iosatp_ppn <= pc_fsc.ppn;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_gscid <= 'd0;
    else if ((is_translated && dc_tc.t2gpa) || (dc_iohgatp.mode != 4'd0))
        cdw_gscid <= dc_iohgatp.gscid;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_pscid <= 'd0;
    else if (is_translated)
        cdw_pscid <= 'd0;
    else if (!dc_tc.pdtv)
        cdw_pscid <= dc_ta.pscid;
    else
        cdw_pscid <= pc_ta.pscid;
end


assign cdw_s1_en = dc_tc.pdtv ? (pc_fsc.mode != 4'd0) : (dc_fsc.mode != 4'd0);
assign cdw_s2_en = ((is_translated && dc_tc.t2gpa) || (dc_iohgatp.mode != 4'd0));


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pdtp_mode <= 'd0;
    else if (cdw_req_en)
        pdtp_mode <= 'd0;
    else if (dc_tc.pdtv)
        pdtp_mode <= dc_fsc.mode;
    else
        pdtp_mode <= 'd0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iosatp_mode <= 'd0;
    else if (cdw_req_en)
        iosatp_mode <= 'd0;
    else if (dc_tc.pdtv)
        iosatp_mode <= pc_fsc.mode;
    else
        iosatp_mode <= dc_fsc.mode;
end

////////**********************************************************************************************************///////
//gen cdw output signal to ptw_fifo
////////**********************************************************************************************************///////
assign ptw_rfifo_wen_o = ptw_rfifo_wen;
assign ptw_rfifo_din_o = ptw_rfifo_din;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_rfifo_wen <= 1'b0;
    else if ((cdw_req_mode == 2'd0) && (cdw_cs == ERROR) && (!ptw_rfifo_full_i))
        ptw_rfifo_wen <= 1'b1;
    else if ((cdw_req_mode == 2'd0) && ((cdw_cs == CDW_END) || (cdw_cs == GUEST_TR)) && !ptw_rfifo_full_i && !cdw_error_flag && !cdw_flush_i)
        ptw_rfifo_wen <= 1'b1;
    else
        ptw_rfifo_wen <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_is_error <= 1'b0;
    else if ((cdw_cs == ERROR) && (!ptw_rfifo_full_i))
        cdw_is_error <= 1'b1;
    else
        cdw_is_error <= 1'b0;
end

//152=4+44+52+52
assign msi_para = {msiptp.mode,msiptp.ppn,msi_addr_mask.mask,msi_addr_pattern.pattern};
//61=7+24+20+1+8+1
assign ptw_trans_para = {dc_tc.sade && amo_hwad,dc_tc.gade && amo_hwad,ats_req_InD,cdw_req_mode,dc_tc.prpr,dc_tc.en_pri,req_did,req_pid,req_pv,ptw_req_idx,cdw_is_error};
//20bit//cdw_gppn_mode and pdtp_mode[4:3] are multiplexing when pdtp_mode[4:3] is no use.
assign ptw_acd_para = {cdw_gppn_mode,pdtp_mode[1:0],dc_iohgatp.mode,iosatp_mode,pc_ta.sum,pc_ta.ens,dc_tc.sxl,dc_tc.dpe,dc_tc.pdtv,dc_tc.dtf,dc_tc.t2gpa,dc_tc.en_ats};
//assign cdw_pc_sum = pc_ta.sum;
//313=61+20+232
//465=152+313
assign ptw_rfifo_din = {msi_para,ptw_trans_para,ptw_acd_para,cdw_implicit_access,ptw_is_bare,cdw_s1_en,cdw_s2_en,pc_ta.sum,priv_lvl,trans_type,req_iova,cdw_gppn,cdw_iohgatp_ppn,cdw_iosatp_ppn,cdw_gscid,cdw_pscid};


////////**********************************************************************************************************///////
//get dc and pc entry of non-leaf
////////**********************************************************************************************************///////
always@(*) begin
    if (ddtc_hit_valid_i && ddtc_hit_i && (ddtc_hit_lvl_i == 2'd1) && ((ddtp_mode_i == 4'd4) || (ddtp_mode_i == 4'd3)))
        cdw_did_sel = req_did[8:6];
    else if (ddtc_hit_valid_i && ddtc_hit_i && (ddtc_hit_lvl_i == 2'd2) && (ddtp_mode_i == 4'd4))
        cdw_did_sel = req_did[17:15];
    else if (cdw_rvalid_i && cdw_rlast_i && (cdw_cs == DC_L1_ENTRY))
        cdw_did_sel = req_did[8:6];
    else if (cdw_rvalid_i && cdw_rlast_i && (cdw_cs == DC_L2_ENTRY))
        cdw_did_sel = req_did[17:15];
    else
        cdw_did_sel = cdw_did_sel_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_did_sel_q <= 3'd0;
    else
        cdw_did_sel_q <= cdw_did_sel;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dc_entry <= 'd0;
    else if (cdw_req_en)
        dc_entry <= 'd0;
    else if ((cdw_cs == CDW_LOOKUP_ACK) && ddtc_hit_valid_i && ddtc_hit_i)
        case (cdw_did_sel)
            3'd0:
                dc_entry <= ddtc_hit_content_i[64*1-1:0];
            3'd1:
                dc_entry <= ddtc_hit_content_i[64*2-1:64*1];
            3'd2:
                dc_entry <= ddtc_hit_content_i[64*3-1:64*2];
            3'd3:
                dc_entry <= ddtc_hit_content_i[64*4-1:64*3];
            3'd4:
                dc_entry <= ddtc_hit_content_i[64*5-1:64*4];
            3'd5:
                dc_entry <= ddtc_hit_content_i[64*6-1:64*5];
            3'd6:
                dc_entry <= ddtc_hit_content_i[64*7-1:64*6];
            3'd7:
                dc_entry <= ddtc_hit_content_i[64*8-1:64*7];
            default:
                dc_entry <= 'd0;
        endcase
end


always@(*) begin
    if (pdtc_hit_valid_i && pdtc_hit_i && (pdtc_hit_lvl_i == 2'd1) && ((dc_fsc.mode == 4'd3) || (dc_fsc.mode == 4'd2)))
        cdw_pid_sel = req_pid[10:8];
    else if (pdtc_hit_valid_i && pdtc_hit_i && (pdtc_hit_lvl_i == 2'd2) && (dc_fsc.mode == 4'd3))
        cdw_pid_sel = req_pid[19:17];
    else if (cdw_rvalid_i && cdw_rlast_i && (cdw_cs == PC_L1_ENTRY))
        cdw_pid_sel = req_pid[10:8];
    else if (cdw_rvalid_i && cdw_rlast_i && (cdw_cs == PC_L2_ENTRY))
        cdw_pid_sel = req_pid[19:17];
    else
        cdw_pid_sel = cdw_pid_sel_q;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_pid_sel_q <= 3'd0;
    else
        cdw_pid_sel_q <= cdw_pid_sel;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pc_entry <= 'd0;
    else if (cdw_req_en)
        pc_entry <= 'd0;
    else if ((cdw_cs == CDW_LOOKUP_ACK) && pdtc_hit_valid_i && pdtc_hit_i && cdw_pid_sel[0])
        pc_entry <= pdtc_hit_content_i[63:0];
    else if ((cdw_cs == CDW_LOOKUP_ACK) && pdtc_hit_valid_i && pdtc_hit_i)
        pc_entry <= pdtc_hit_content_i[127:64];
    else
        pc_entry <= pc_entry;
end


always@(*) begin
    if (cdw_req_en)
        entry = 'd0;
    else if (ddtc_hit_valid && ddtc_hit)
        entry = entry_t'(dc_entry);
    else if (pdtc_hit_valid && pdtc_hit)
        entry = entry_t'(pc_entry);
    else if (((cdw_cs == DC_L1_ENTRY) || (cdw_cs == DC_L2_ENTRY)) && cdw_rvalid_i && cdw_rlast_i)
        case (cdw_did_sel)
            3'd0:
                entry = entry_t'(cdw_entry[64*1-1:0]);
            3'd1:
                entry = entry_t'(cdw_entry[64*2-1:64*1]);
            3'd2:
                entry = entry_t'(cdw_entry[64*3-1:64*2]);
            3'd3:
                entry = entry_t'(cdw_entry[64*4-1:64*3]);
            3'd4:
                entry = entry_t'(cdw_entry[64*5-1:64*4]);
            3'd5:
                entry = entry_t'(cdw_entry[64*6-1:64*5]);
            3'd6:
                entry = entry_t'(cdw_entry[64*7-1:64*6]);
            3'd7:
                entry = entry_t'(cdw_entry[64*8-1:64*7]);
            default:
                entry = 'd0;
        endcase
    else if (((cdw_cs == PC_L1_ENTRY) || (cdw_cs == PC_L2_ENTRY)) && cdw_rvalid_i && cdw_rlast_i)
        case (cdw_pid_sel)
            3'd0:
                entry = entry_t'(cdw_entry[64*1-1:0]);
            3'd1:
                entry = entry_t'(cdw_entry[64*2-1:64*1]);
            3'd2:
                entry = entry_t'(cdw_entry[64*3-1:64*2]);
            3'd3:
                entry = entry_t'(cdw_entry[64*4-1:64*3]);
            3'd4:
                entry = entry_t'(cdw_entry[64*5-1:64*4]);
            3'd5:
                entry = entry_t'(cdw_entry[64*6-1:64*5]);
            3'd6:
                entry = entry_t'(cdw_entry[64*7-1:64*6]);
            3'd7:
                entry = entry_t'(cdw_entry[64*8-1:64*7]);
            default:
                entry = 'd0;
        endcase
    else
        entry = entry_q;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        entry_q <= 64'd0;
    else
        entry_q <= entry;
end


////////**********************************************************************************************************///////
//get dc and pc content
////////**********************************************************************************************************///////
always@(*) begin
    if (cdw_req_en)
        dc_content = 512'd0;
    else if ((cdw_cs == CDW_LOOKUP_ACK) && ddtc_hit_valid_i && ddtc_hit_i && (ddtc_hit_lvl_i == 2'd0))
        dc_content = ddtc_hit_content_i;
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i)
        dc_content = cdw_entry;
    else if ((cdw_cs == GUEST_DONE) && implicit_done_i && (quest_tr_mode == 2'b01))//pdtp
        dc_content = {dc_content_q[511:236],implicit_ppn_i,dc_content_q[191:0]};
    else
        dc_content = dc_content_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dc_content_q <= 512'd0;
    else
        dc_content_q <= dc_content;
end


always@(*) begin
    if (cdw_req_en)
        pc_content = 128'd0;
    else if ((cdw_cs == CDW_LOOKUP_ACK) && pdtc_hit_valid_i && pdtc_hit_i && (pdtc_hit_lvl_i == 2'd0))
        pc_content = pdtc_hit_content_i;
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i)
        case (cdw_pptr_q[5:4])
            2'd0:
                pc_content = cdw_entry[127:0];
            2'd1:
                pc_content = cdw_entry[255:128];
            2'd2:
                pc_content = cdw_entry[383:256];
            2'd3:
                pc_content = cdw_entry[511:384];
            default:
                pc_content = 128'd0;
        endcase
    else
        pc_content = pc_content_q;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pc_content_q <= 128'd0;
    else
        pc_content_q <= pc_content;
end


////////**********************************************************************************************************///////
//get local dc and pc content
////////**********************************************************************************************************///////
assign dc_tc = tc_t'(dc_content[63:0]);
assign dc_iohgatp = iohgatp_t'(dc_content[127:64]);
assign dc_ta = dc_ta_t'(dc_content[191:128]);
assign dc_fsc = fsc_t'(dc_content[255:192]);
assign msiptp = msiptp_t'(dc_content[319:256]);
assign msi_addr_mask = msi_addr_mask_t'(dc_content[383:320]);
assign msi_addr_pattern = msi_addr_pattern_t'(dc_content[447:384]);
assign pc_ta = pc_ta_t'(pc_content[63:0]);
assign pc_fsc = fsc_t'(pc_content[127:64]);


////////**********************************************************************************************************///////
//update dc and pc content
////////**********************************************************************************************************///////
assign up_dc_valid_o = up_dc_valid;
assign up_dc_leaf_o = 1'b1;
assign up_dc_lvl_o = 2'd0;
assign up_dc_did_o = req_did;
assign up_dc_content_o = up_dc_content;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_dc_valid <= 1'b0;
    else if (cdw_flush_i || cdw_error_flag || (up_dc_valid && up_dc_ready_i))
        up_dc_valid <= 1'b0;
    else if ((cdw_rvalid_i && cdw_rlast_i && (cdw_cs == DC_L0_ENTRY) && (!dc_tc.pdtv || (dc_tc.pdtv && !cdw_s2_en))) ||
                ((cdw_cs == GUEST_DONE) && (quest_tr_mode == 2'b01) && implicit_done_i))
        up_dc_valid <= 1'b1;
    else
        up_dc_valid <= up_dc_valid;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_dc_content <= 512'd0;
    else if (cdw_flush_i || cdw_error_flag || (up_dc_valid && up_dc_ready_i))
        up_dc_content <= 512'd0;
    else if ((cdw_rvalid_i && cdw_rlast_i && (cdw_cs == DC_L0_ENTRY) && (!dc_tc.pdtv || (dc_tc.pdtv && !cdw_s2_en))) ||
                ((cdw_cs == GUEST_DONE) && (quest_tr_mode == 2'b01) && implicit_done_i))
        up_dc_content <= dc_content;
    else
        up_dc_content <= up_dc_content;
end


assign up_pc_valid_o = up_pc_valid;
assign up_pc_leaf_o = 1'b1;
assign up_pc_lvl_o = 2'd0;
assign up_pc_pid_o = req_pid;
assign up_pc_did_o = req_did;
assign up_pc_content_o = up_pc_content;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_pc_valid <= 1'b0;
    else if (cdw_flush_i || cdw_error_flag || (up_pc_valid && up_pc_ready_i))
        up_pc_valid <= 1'b0;
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i)
        up_pc_valid <= 1'b1;
    else
        up_pc_valid <= up_pc_valid;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_pc_content <= 128'd0;
    else if (up_pc_valid && up_pc_ready_i)
        up_pc_content <= 128'd0;
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i)
        up_pc_content <= pc_content;
end


assign ddt_walk_o = ddt_walk;
assign pdt_walk_o = pdt_walk;
assign cdw_did_o = req_did;
assign cdw_pv_o = dc_tc.pdtv && (req_pv || dc_tc.dpe);
assign cdw_pid_o = req_pid;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ddt_walk <= 1'b0;
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY) || (cdw_cs == DC_L0_ENTRY)) && cdw_rvalid_i && cdw_rlast_i)
        ddt_walk <= 1'b1;
    else
        ddt_walk <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pdt_walk <= 1'b0;
    else if (((cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY) || (cdw_cs == PC_L0_ENTRY)) && cdw_rvalid_i && cdw_rlast_i)
        pdt_walk <= 1'b1;
    else
        pdt_walk <= 1'b0;
end


////////**********************************************************************************************************///////
//gen cdw error
////////**********************************************************************************************************///////
// CDW walking
assign sv39 = iommu_caps_i[9];
assign sv48 = iommu_caps_i[10];
assign sv57 = iommu_caps_i[11];
assign svpbmt = iommu_caps_i[15];
assign sv39x4 = iommu_caps_i[17];
assign sv48x4 = iommu_caps_i[18];
assign sv57x4 = iommu_caps_i[19];
assign amo_mrif = iommu_caps_i[21];
assign msi_flat = iommu_caps_i[22];
assign msi_mrif = iommu_caps_i[23];
assign amo_hwad = iommu_caps_i[24];
assign ats = iommu_caps_i[25];
assign t2gpa = iommu_caps_i[26];
assign hpm = iommu_caps_i[30];
assign dbg = iommu_caps_i[31];
assign pd8 = iommu_caps_i[38];
assign pd17 = iommu_caps_i[39];
assign pd20 = iommu_caps_i[40];

//these signals are protected by iommu-reg
//!t2gpa && dc_tc.t2gpa
//!amo_hwad && (dc_tc.sade || dc_tc.gade)
//!sv39x4 && dc_iohgatp.mode == 4'd8
//!sv48x4 && dc_iohgatp.mode == 4'd9
//dc_tc.pdtv && !pd20 && dc_fsc.mode == 4'd3
//dc_tc.pdtv && !pd17 && dc_fsc.mode == 4'd2
//dc_tc.pdtv && !pd8 && dc_fsc.mode == 4'd1
//!dc_tc.pdtv && !dc_tc.sxl && !sv39 && dc_fsc.mode == 4'd8
//!dc_tc.pdtv && !dc_tc.sxl && !sv48 && dc_fsc.mode == 4'd9
//dc_rsv[511:448] is not judged because it is not used
//!dc_tc.sxl && !sv39 && pc_fsc.mode == 4'd8
//!dc_tc.sxl && !sv48 && pc_fsc.mode == 4'd9


assign pid_wide_error = req_pv && dc_tc.pdtv && ((dc_fsc.mode == 4'd1 && |req_pid[19:8]) ||
                                   (dc_fsc.mode == 4'd2 && |req_pid[19:17]));

always@(*) begin
    //*****INB_DISALLOWED****//
    if (cdw_req_en_d && (ddtp_mode_i == 4'd0))
        cdw_error_flag = 1'b1;
//  else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (req_pv || dc_tc.dpe) && dc_tc.pdtv && |dc_fsc.mode[3:2])
//      cdw_error_flag = 1'b1;
    //*****DDT_DATA_CORRUPTION****//
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY) || (cdw_cs == DC_L0_ENTRY)) && cdw_rvalid_i && cdw_rlast_i && (cdw_rresp_i != RESP_OKAY))
        cdw_error_flag = 1'b1;
    //*****PDT_DATA_CORRUPTION****//
    else if (((cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY) || (cdw_cs == PC_L0_ENTRY))  && cdw_rvalid_i && cdw_rlast_i && (cdw_rresp_i != RESP_OKAY))
        cdw_error_flag = 1'b1;
    //*****DDT_ENTRY_INVALID****//
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY)) && cdw_rvalid_i && cdw_rlast_i && !entry.v)
        cdw_error_flag = 1'b1;
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (!dc_tc.v))
        cdw_error_flag = 1'b1;
    //*****DDT_ENTRY_MISCONFIGURED****//
    //dc non-leaf miscfg
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY)) &&
                        (cdw_rvalid_i && cdw_rlast_i && (|entry.rsv_1 || |entry.rsv_2)))
        cdw_error_flag = 1'b1;
    //dc_tc_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (|dc_tc.rsv_1 || |dc_tc.rsv_2) ||
                        (!dc_tc.en_ats && (dc_tc.t2gpa || dc_tc.en_pri)) ||
                        (!dc_tc.en_pri && dc_tc.prpr) ||
                        (!dc_tc.pdtv && dc_tc.dpe) ||
                        (fctl_be_i != dc_tc.sbe) ||
                        dc_tc.sxl ||
                        fctl_gxl_i))
        cdw_error_flag = 1'b1;
    //dc_iohgatp_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (dc_tc.t2gpa && dc_iohgatp.mode == 4'd0) ||
                        (!(dc_iohgatp.mode inside {4'd0, 4'd8, 4'd9, 4'd10})) ||
                        (!fctl_gxl_i && !sv57x4 && dc_iohgatp.mode == 4'd10) ||
                        (|dc_iohgatp.mode && |dc_iohgatp.ppn[1:0])))
        cdw_error_flag = 1'b1;
    //dc_ta_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (|dc_ta.rsv_1 || |dc_ta.rsv_2))
        cdw_error_flag = 1'b1;
    //dc_fsc_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (dc_tc.pdtv && |dc_fsc.mode[3:2]) ||
                        (!dc_tc.pdtv && !dc_tc.sxl && (!(dc_fsc.mode inside {4'd0, 4'd8, 4'd9, 4'd10}) ||(!sv57 && dc_fsc.mode == 4'd10))) ||
                        |dc_fsc.rsv))
        cdw_error_flag = 1'b1;
    //dc_msi_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i &&
                        (|(msiptp.mode & 4'b1110) || |msiptp.rsv || |msi_addr_mask.rsv || |msi_addr_pattern.rsv ||
                        (dc_iohgatp.mode == 4'd0 && msiptp.mode != 4'd0)))
        cdw_error_flag = 1'b1;
    //*****TRANS_TYPE_DISALLOWED****//
    //spec_step.2
    else if (cdw_req_en_d && !(ddtp_mode_i inside {4'd2,4'd3,4'd4}))
        cdw_error_flag = 1'b1;
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (req_pv || dc_tc.dpe) && dc_tc.pdtv && |dc_fsc.mode[3:2])
        cdw_error_flag = 1'b1;
    //spec_step.5
    else if (cdw_req_en_d && ((ddtp_mode_i == 4'd2 && |req_did[23:6]) || (ddtp_mode_i == 4'd3 && |req_did[23:15])))
        cdw_error_flag = 1'b1;
    //spec_step.7,dc is from main-memory
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                            ((is_translated || cdw_ats_req) && !dc_tc.en_ats) ||
                            (req_pv && !dc_tc.pdtv) || pid_wide_error))
        cdw_error_flag = 1'b1;
    //spec_step.7,dc is from DDTC in atd_cache
    else if ((cdw_cs == CDW_LOOKUP_ACK) && ddtc_hit_valid_i && ddtc_hit_i && (
                                ((is_translated || cdw_ats_req) && !dc_tc.en_ats) ||
                                (req_pv && !dc_tc.pdtv) || pid_wide_error))
        cdw_error_flag = 1'b1;
    //spec_step.15,pc is from main-memory
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && priv_lvl && !pc_ta.ens)
        cdw_error_flag = 1'b1;
    //spec_step.15,pc is from PDTC in atd_cache
    else if ((cdw_cs == CDW_LOOKUP_ACK) && pdtc_hit_valid_i && pdtc_hit_i && priv_lvl && !pc_ta.ens)
        cdw_error_flag = 1'b1;
    //*****PDT_ENTRY_INVALID****//
    else if (((cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY)) && cdw_rvalid_i && cdw_rlast_i && !entry.v)
        cdw_error_flag = 1'b1;
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && !pc_ta.v)
        cdw_error_flag = 1'b1;
    //*****PDT_ENTRY_MISCONFIGURED****//
    //pc non-leaf miscfg
    else if (((cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY)) &&
                        (cdw_rvalid_i && cdw_rlast_i && (|entry.rsv_1 || |entry.rsv_2)))
        cdw_error_flag = 1'b1;
    //pc_ta_miscfg
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (|pc_ta.rsv_1 || |pc_ta.rsv_2))
        cdw_error_flag = 1'b1;
    //pc_fsc_miscfg
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i &&
                        (|pc_fsc.rsv || !(pc_fsc.mode inside {4'd0, 4'd8, 4'd9, 4'd10})))
        cdw_error_flag = 1'b1;
    else
        cdw_error_flag = 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_cause_code <= 'd0;
    else if (cdw_req_en_d && (ddtp_mode_i == 4'd0))
        cdw_cause_code <= ALL_INB_TRANSACTIONS_DISALLOWED;
//  else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (req_pv || dc_tc.dpe) && dc_tc.pdtv && |dc_fsc.mode[3:2])
//      cdw_cause_code <= ALL_INB_TRANSACTIONS_DISALLOWED;
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY) || (cdw_cs == DC_L0_ENTRY)) && cdw_rvalid_i && cdw_rlast_i && (cdw_rresp_i != RESP_OKAY))
        cdw_cause_code <= DDT_DATA_CORRUPTION;
    else if (((cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY) || (cdw_cs == PC_L0_ENTRY))  && cdw_rvalid_i && cdw_rlast_i && (cdw_rresp_i != RESP_OKAY))
        cdw_cause_code <= PDT_DATA_CORRUPTION;
    //*****DDT_ENTRY_INVALID****//
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY)) && cdw_rvalid_i && cdw_rlast_i && !entry.v)
        cdw_cause_code <= DDT_ENTRY_INVALID;
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (!dc_tc.v))
        cdw_cause_code <= DDT_ENTRY_INVALID;
    //*****DDT_ENTRY_MISCONFIGURED****//
    //dc non-leaf miscfg
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY)) &&
                        (cdw_rvalid_i && cdw_rlast_i && (|entry.rsv_1 || |entry.rsv_2)))
        cdw_cause_code <= DDT_ENTRY_MISCONFIGURED;
    //dc_tc_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (|dc_tc.rsv_1 || |dc_tc.rsv_2) ||
                        (!dc_tc.en_ats && (dc_tc.t2gpa || dc_tc.en_pri)) ||
                        (!dc_tc.en_pri && dc_tc.prpr) ||
                        (!dc_tc.pdtv && dc_tc.dpe) ||
                        (fctl_be_i != dc_tc.sbe) ||
                        dc_tc.sxl ||
                        fctl_gxl_i))
        cdw_cause_code <= DDT_ENTRY_MISCONFIGURED;
    //dc_iohgatp_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (dc_tc.t2gpa && dc_iohgatp.mode == 4'd0) ||
                        (!(dc_iohgatp.mode inside {4'd0, 4'd8, 4'd9, 4'd10})) ||
                        (!fctl_gxl_i && !sv57x4 && dc_iohgatp.mode == 4'd10) ||
                        (|dc_iohgatp.mode && |dc_iohgatp.ppn[1:0])))
        cdw_cause_code <= DDT_ENTRY_MISCONFIGURED;
    //dc_ta_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (|dc_ta.rsv_1 || |dc_ta.rsv_2))
        cdw_cause_code <= DDT_ENTRY_MISCONFIGURED;
    //dc_fsc_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (dc_tc.pdtv && |dc_fsc.mode[3:2]) ||
                        (!dc_tc.pdtv && !dc_tc.sxl && (!(dc_fsc.mode inside {4'd0, 4'd8, 4'd9, 4'd10}) ||(!sv57 && dc_fsc.mode == 4'd10))) ||
                        |dc_fsc.rsv))
        cdw_cause_code <= DDT_ENTRY_MISCONFIGURED;
    //dc_msi_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i &&
                        (|(msiptp.mode & 4'b1110) || |msiptp.rsv || |msi_addr_mask.rsv || |msi_addr_pattern.rsv ||
                        (dc_iohgatp.mode == 4'd0 && msiptp.mode != 4'd0)))
        cdw_cause_code <= DDT_ENTRY_MISCONFIGURED;
    //*****TRANS_TYPE_DISALLOWED****//
    //spec_step.2
    else if (cdw_req_en_d && !(ddtp_mode_i inside {4'd2,4'd3,4'd4}))
        cdw_cause_code <= ALL_INB_TRANSACTIONS_DISALLOWED;
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (req_pv || dc_tc.dpe) && dc_tc.pdtv && |dc_fsc.mode[3:2])
        cdw_cause_code <= TRANS_TYPE_DISALLOWED;
    //spec_step.5
    else if (cdw_req_en_d && ((ddtp_mode_i == 4'd2 && |req_did[23:6]) || (ddtp_mode_i == 4'd3 && |req_did[23:15])))
        cdw_cause_code <= TRANS_TYPE_DISALLOWED;
    //spec_step.7,dc is from main-memory
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                            ((is_translated || cdw_ats_req) && !dc_tc.en_ats) ||
                            (req_pv && !dc_tc.pdtv) || pid_wide_error))
        cdw_cause_code <= TRANS_TYPE_DISALLOWED;
    //spec_step.7,dc is from DDTC in atd_cache
    else if ((cdw_cs == CDW_LOOKUP_ACK) && ddtc_hit_valid_i && ddtc_hit_i && (
                                ((is_translated || cdw_ats_req) && !dc_tc.en_ats) ||
                                (req_pv && !dc_tc.pdtv) || pid_wide_error))
        cdw_cause_code <= TRANS_TYPE_DISALLOWED;
    //spec_step.15,pc is from main-memory
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && priv_lvl && !pc_ta.ens)
         cdw_cause_code <= TRANS_TYPE_DISALLOWED;
    //spec_step.15,pc is from PDTC in atd_cache
    else if ((cdw_cs == CDW_LOOKUP_ACK) && pdtc_hit_valid_i && pdtc_hit_i && priv_lvl && !pc_ta.ens)
         cdw_cause_code <= TRANS_TYPE_DISALLOWED;
    //*****PDT_ENTRY_INVALID****//
    else if (((cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY)) && cdw_rvalid_i && cdw_rlast_i && !entry.v)
        cdw_cause_code <= PDT_ENTRY_INVALID;
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && !pc_ta.v)
        cdw_cause_code <= PDT_ENTRY_INVALID;
    //*****PDT_ENTRY_MISCONFIGURED****//
    //pc non-leaf miscfg
    else if (((cdw_cs == PC_L2_ENTRY) || (cdw_cs == PC_L1_ENTRY)) &&
                        (cdw_rvalid_i && cdw_rlast_i && (|entry.rsv_1 || |entry.rsv_2)))
        cdw_cause_code <= PDT_ENTRY_MISCONFIGURED;
    //pc_ta_miscfg
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (|pc_ta.rsv_1 || |pc_ta.rsv_2))
        cdw_cause_code <= PDT_ENTRY_MISCONFIGURED;
    //pc_fsc_miscfg
    else if ((cdw_cs == PC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i &&
                        (|pc_fsc.rsv || !(pc_fsc.mode inside {4'd0, 4'd8, 4'd9, 4'd10})))
        cdw_cause_code <= PDT_ENTRY_MISCONFIGURED;
    else
        cdw_cause_code <= 'd0;
end


assign cdw_fq_valid_o = cdw_fq_valid;
assign cdw_fq_record_o = {64'd0,req_iova,12'd0,64'd0,req_did,trans_type,priv_lvl,req_pv,req_pid,cdw_cause_code};


always@(*) begin
    //*****INB_DISALLOWED****//
    if (cdw_req_en_d && (ddtp_mode_i == 4'd0))
        cdw_error_flag_dtf = 1'b1;
//  else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (req_pv || dc_tc.dpe) && dc_tc.pdtv && |dc_fsc.mode[3:2])
//      cdw_error_flag_dtf = 1'b1;
    //*****DDT_DATA_CORRUPTION****//
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY) || (cdw_cs == DC_L0_ENTRY)) && cdw_rvalid_i && cdw_rlast_i && (cdw_rresp_i != RESP_OKAY))
        cdw_error_flag_dtf = 1'b1;
    //*****DDT_ENTRY_INVALID****//
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY)) && cdw_rvalid_i && cdw_rlast_i && !entry.v)
        cdw_error_flag_dtf = 1'b1;
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (!dc_tc.v))
        cdw_error_flag_dtf = 1'b1;
    //*****DDT_ENTRY_MISCONFIGURED****//
    //dc non-leaf miscfg
    else if (((cdw_cs == DC_L2_ENTRY) || (cdw_cs == DC_L1_ENTRY)) &&
                        (cdw_rvalid_i && cdw_rlast_i && (|entry.rsv_1 || |entry.rsv_2)))
        cdw_error_flag_dtf = 1'b1;
    //dc_tc_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (|dc_tc.rsv_1 || |dc_tc.rsv_2) ||
                        (!dc_tc.en_ats && (dc_tc.t2gpa || dc_tc.en_pri)) ||
                        (!dc_tc.en_pri && dc_tc.prpr) ||
                        (!dc_tc.pdtv && dc_tc.dpe) ||
                        (fctl_be_i != dc_tc.sbe) ||
                        dc_tc.sxl ||
                        fctl_gxl_i))
        cdw_error_flag_dtf = 1'b1;
    //dc_iohgatp_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (dc_tc.t2gpa && dc_iohgatp.mode == 4'd0) ||
                        (!(dc_iohgatp.mode inside {4'd0, 4'd8, 4'd9, 4'd10})) ||
                        (!fctl_gxl_i && !sv57x4 && dc_iohgatp.mode == 4'd10) ||
                        (|dc_iohgatp.mode && |dc_iohgatp.ppn[1:0])))
        cdw_error_flag_dtf = 1'b1;
    //dc_ta_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (|dc_ta.rsv_1 || |dc_ta.rsv_2))
        cdw_error_flag_dtf = 1'b1;
    //dc_fsc_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (
                        (dc_tc.pdtv && |dc_fsc.mode[3:2]) ||
                        (!dc_tc.pdtv && !dc_tc.sxl && (!(dc_fsc.mode inside {4'd0, 4'd8, 4'd9, 4'd10}) ||(!sv57 && dc_fsc.mode == 4'd10))) ||
                        |dc_fsc.rsv))
        cdw_error_flag_dtf = 1'b1;
    //dc_msi_miscfg
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i &&
                        (|(msiptp.mode & 4'b1110) || |msiptp.rsv || |msi_addr_mask.rsv || |msi_addr_pattern.rsv ||
                        (dc_iohgatp.mode == 4'd0 && msiptp.mode != 4'd0)))
        cdw_error_flag_dtf = 1'b1;
    //*****TRANS_TYPE_DISALLOWED****//
    //spec_step.2
    else if (cdw_req_en_d && !(ddtp_mode_i inside {4'd2,4'd3,4'd4}))
        cdw_error_flag_dtf = 1'b1;
    else if ((cdw_cs == DC_L0_ENTRY) && cdw_rvalid_i && cdw_rlast_i && (req_pv || dc_tc.dpe) && dc_tc.pdtv && |dc_fsc.mode[3:2])
        cdw_error_flag_dtf = 1'b1;
    //spec_step.5
    else if (cdw_req_en_d && ((ddtp_mode_i == 4'd2 && |req_did[23:6]) || (ddtp_mode_i == 4'd3 && |req_did[23:15])))
        cdw_error_flag_dtf = 1'b1;
    //spec_step.7,dc is from DDTC in atd_cache
    else if ((cdw_cs == CDW_LOOKUP_ACK) && ddtc_hit_valid_i && ddtc_hit_i && (
                                ((is_translated || cdw_ats_req) && !dc_tc.en_ats) ||
                                (req_pv && !dc_tc.pdtv) || pid_wide_error))
        cdw_error_flag_dtf = 1'b1;
    else
        cdw_error_flag_dtf = 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_fq_valid <= 1'b0;
    else if (dc_tc.dtf)
        cdw_fq_valid <= cdw_error_flag_dtf && cdw_fq_ready_i;
    else
        cdw_fq_valid <= cdw_error_flag && cdw_fq_ready_i;
end


////////**********************************************************************************************************///////
//gen ats_pri resp output signal to atd_t2r module
////////**********************************************************************************************************///////
assign pri_resp_wen_o = pri_resp_wen;
assign pri_resp_wdata_o = pri_resp_wdata;

assign pri_resp_en = dc_tc.en_ats && dc_tc.en_pri && dc_tc.prpr && pri_last;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pri_resp_wen <= 1'b0;
    else if ((cdw_req_mode == 2'd2) && (cdw_cs == ERROR) && (!pri_resp_full_i) && pri_resp_en)
        pri_resp_wen <= 1'b1;
    else if ((cdw_req_mode == 2'd2) && (cdw_cs == CDW_END) && !pri_resp_full_i && !cdw_error_flag && pri_resp_en)
        pri_resp_wen <= 1'b1;
    else
        pri_resp_wen <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pri_resp <= 2'd0;
    else if ((cdw_req_mode == 2'd2) && (!dc_tc.en_ats  || !dc_tc.en_pri))
        pri_resp <= 2'd1;
    else if ((cdw_req_mode == 2'd2) && (cdw_cs == CDW_END) && cdw_is_error)
        pri_resp <= 2'd0;
    else if ((cdw_req_mode == 2'd2) && (cdw_cs == CDW_END))
        pri_resp <= 2'd2;
    else if (cdw_error_flag)
        pri_resp <= 2'd0;
end


assign pri_resp_wdata = {2'd0,pri_resp,3'd0,cdw_req_data[12:4],8'd0,req_did,req_pid,req_pv,7'd0,4'd9};


////////**********************************************************************************************************///////
//gen ats_pri_req output signal to pq module
////////**********************************************************************************************************///////
assign pq_pri_wen_o = pq_pri_wen;
assign pq_pri_wdata_o = pq_pri_wdata;

assign pq_pri_en = dc_tc.en_ats && dc_tc.en_pri;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_pri_wen <= 1'b0;
    else if ((cdw_req_mode == 2'd2) && (cdw_cs == CDW_END) && !pri_resp_full_i && !cdw_error_flag && pq_pri_en)
        pq_pri_wen <= 1'b1;
    else
        pq_pri_wen <= 1'b0;
end


assign pq_pri_wdata = {req_iova,cdw_req_data[12:4],cdw_req_data[74:72],req_did,5'd0,cdw_req_data[71:70],req_pv,req_pid,12'd0};


endmodule


