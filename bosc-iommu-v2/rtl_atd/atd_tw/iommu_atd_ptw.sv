///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_ptw.sv
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


module iommu_atd_ptw #(
    parameter  RISCV_GLEN   = 50,//sv48x4
    parameter  RISCV_PLEN   = 56
) (
    input                           iommu_clk,
    input                           iommu_rstn,

//between cdw and ptw
    input                           ptw_rfifo_wen_i,
    input   [464:0]                 ptw_rfifo_din_i,
    output                          ptw_rfifo_full_o,
//    //between ptw and cdw
    output                          cdw_flush_o,
    output                          cdw_implicit_done_o,
    output  [43:0]                  cdw_implicit_ppn_o,

//between ptw and atd-cache
    output                          lookup_s1ptc_valid_o,
    input                           lookup_s1ptc_ready_i,
    output  [2:0]                   lookup_s1ptc_lvl_o,
    output                          lookup_s1ptc_leaf_o,
    output                          lookup_s1ptc_gv_o,
    output  [51:0]                  lookup_s1ptc_va_o,
    output  [19:0]                  lookup_s1ptc_pscid_o,
    output  [15:0]                  lookup_s1ptc_gscid_o,

    input                           s1ptc_hit_valid_i,
    input                           s1ptc_hit_i,
    input  [2:0]                    s1ptc_hit_lvl_i,
    input  [511:0]                  s1ptc_hit_content_i,

    output                          lookup_s2ptc_valid_o,
    input                           lookup_s2ptc_ready_i,
    output  [2:0]                   lookup_s2ptc_lvl_o,
    output                          lookup_s2ptc_leaf_o,
    output  [15:0]                  lookup_s2ptc_gscid_o,
    output  [51:0]                  lookup_s2ptc_gpa_o,

    input                           s2ptc_hit_valid_i,
    input                           s2ptc_hit_i,
    input  [2:0]                    s2ptc_hit_lvl_i,
    input  [511:0]                  s2ptc_hit_content_i,

    output                          up_s1ptc_valid_o,
    input                           up_s1ptc_ready_i,
    output  [2:0]                   up_s1ptc_lvl_o,
    output                          up_s1ptc_leaf_o,
    output                          up_s1ptc_gv_o,
    output  [63:0]                  up_s1ptc_svnapot_o,
    output  [511:0]                 up_s1ptc_pte_o,
    output  [51:0]                  up_s1ptc_va_o,
    output                          up_s1ptc_sxl_o,
    output  [19:0]                  up_s1ptc_pscid_o,
    output  [15:0]                  up_s1ptc_gscid_o,

    output                          up_s2ptc_valid_o,
    input                           up_s2ptc_ready_i,
    output  [2:0]                   up_s2ptc_lvl_o,
    output                          up_s2ptc_leaf_o,
    output  [63:0]                  up_s2ptc_svnapot_o,
    output  [511:0]                 up_s2ptc_pte_o,
    output  [51:0]                  up_s2ptc_gpa_o,
    output  [15:0]                  up_s2ptc_gscid_o,

    //to FQ
    output                          ptw_fq_valid_o,
    output  [255:0]                 ptw_fq_record_o,
    input                           ptw_fq_ready_i,

    //output to t2c and then to acd
    output                          ptw_wfifo_wen_o,
    output  [319:0]                 ptw_wfifo_wdata_o,
    input                           ptw_wfifo_full_i,

    //to ats_t2r
    //remove-5bit-reserved
    output                          ats_fault_valid_o,
    output  [15:0]                  ats_fault_record_o,
    input                           ats_fault_ready_i,

    //output to t2r and then to pcie-rc
    //remove-46bit-reserved
    output                          ats_resp_wen_o,
    output  [113:0]                 ats_resp_wdata_o,
    input                           ats_resp_full_i,

    //between ptw and HPM
    output                          ptw_s1_walk_o,
    output                          ptw_s2_walk_o,
    output  [15:0]                  ptw_gscid_o,
    output  [19:0]                  ptw_pscid_o,

    //to msi
    output                          up_ad_wen_o,
    output  [7:0]                   up_ad_wdata_o,
    output  [52:0]                  up_ad_waddr_o,

    output                          ptw_awvalid_o,
    input                           ptw_awready_i,
    output  [3:0]                   ptw_awid_o,
    output  [55:0]                  ptw_awaddr_o,
    output  [7:0]                   ptw_awlen_o,
    output                          ptw_wvalid_o,
    input                           ptw_wready_i,
    output                          ptw_wlast_o,
    output  [31:0]                  ptw_wstrb_o,
    output  [255:0]                 ptw_wdata_o,

    input                           ptw_bvalid_i,
    output                          ptw_bready_o,
    input [3:0]                     ptw_bid_i,
    input [1:0]                     ptw_bresp_i,

    output                          ptw_arvalid_o,
    input                           ptw_arready_i,
    output  [3:0]                   ptw_arid_o,
    output  [RISCV_PLEN-1:0]        ptw_araddr_o,
    output  [7:0]                   ptw_arlen_o,
    input                           ptw_rvalid_i,
    output                          ptw_rready_o,
    input                           ptw_rlast_i,
    input   [3:0]                   ptw_rid_i,
    input   [1:0]                   ptw_rresp_i,
    input   [255:0]                 ptw_rdata_i
);

//  localparam logic            BURST_FIXED                 = 2'b00;
//  localparam logic    [1:0]   BURST_INCR                  = 2'b01;
    localparam logic    [1:0]   RESP_OKAY                   = 2'b00;
    localparam logic    [3:0]   UP_D1_ID                    = 4'd5;
    localparam logic    [3:0]   UP_D2_ID                    = 4'd6;

    localparam logic    [11:0]  INSTR_ACCESS_FAULT          = 12'd1;
    localparam logic    [11:0]  INSTR_PAGE_FAULT            = 12'd12; //instruction page fault
    localparam logic    [11:0]  LOAD_PAGE_FAULT             = 12'd13; // Load/read page fault
    localparam logic    [11:0]  STORE_PAGE_FAULT            = 12'd15; // Store/write/AMO page fault
    localparam logic    [11:0]  INSTR_GUEST_PAGE_FAULT      = 12'd20; //instruction guest-page fault
    localparam logic    [11:0]  LOAD_GUEST_PAGE_FAULT       = 12'd21; // Load/read guest-page fault
    localparam logic    [11:0]  STORE_GUEST_PAGE_FAULT      = 12'd23; // Store/write/AMO guest-page fault
//  localparam  logic   [11:0]  MSI_PTE_LD_ACCESS_FAULT     = 12'd261;  // PMP/PMA checkn fault when accessing MSI PTE
    localparam logic    [11:0]  MSI_PTE_INVALID             = 12'd262;
    localparam logic    [11:0]  MSI_PTE_MISCONFIGURED       = 12'd263;
//    localparam logic [11:0] MRIF_ACCESS_FAULT             = 12'd264;//acd
    localparam logic    [11:0]  MSI_PT_DATA_CORRUPTION      = 12'd270;
//    localparam logic [11:0] MSI_MRIF_DATA_CORRUPTION      = 12'271;//acd
//    localparam logic [11:0] INTERN_DATAPATH_FAULT         = 12'272;//acd
//    localparam logic [11:0] MSI_ST_ACCESS_FAULT           = 12'273;//msi
    localparam logic    [11:0]  PT_DATA_CORRUPTION          = 12'd274;

    //PTW FSM states
    enum logic [4:0] {
    IDLE            ,
    PTW_LOOKUP_DELAY,
    PTW_LOOKUP_REQ  ,
    PTW_LOOKUP_ACK  ,
    PTW_MSI_DELAY   ,
    S1_L3_REQ       ,
    S1_L3_PTE       ,
    S1_L2_REQ       ,
    S1_L2_PTE       ,
    S1_L1_REQ       ,
    S1_L1_PTE       ,
    S1_L0_REQ       ,
    S1_L0_PTE       ,
    S2_L3_REQ       ,
    S2_L3_PTE       ,
    S2_L2_REQ       ,
    S2_L2_PTE       ,
    S2_L1_REQ       ,
    S2_L1_PTE       ,
    S2_L0_REQ       ,
    S2_L0_PTE       ,
    AW_REQ_S1       ,
    AW_REQ          ,
    W_DATA          ,
    B_RESP          ,
    PTW_END     ,
    ERROR
    } ptw_cs,ptw_ns;

    // ----------------------
    // Virtual Memory
    // ----------------------
    // memory management, pte for sv39/sv48
    typedef struct packed {
        logic                   n;
        logic [1:0]             pbmt;
        logic [6:0]             rsv;
        logic [44-1:0]          ppn; // PPN length for
        logic [1:0]             rsv_1;
        logic                   d;
        logic                   a;
        logic                   g;
        logic                   u;
        logic                   x;
        logic                   w;
        logic                   r;
        logic                   v;
    } pte_t;


    typedef struct packed {
        logic [2:0]             rsv_4;
        logic                   nid2;
        logic [5:0]             rsv_3;
        logic [43:0]            nppn;
        logic [9:0]             nid1;

        logic                   c;
        logic [8:0]             rsv_2; // PPN length for
        logic [46:0]            ppn;
        logic [3:0]             rsv_1;
        logic [1:0]             m;
        logic                   v;
    } msi_pte_t;

    pte_t                       pte;
    msi_pte_t                   msi_pte;
    logic [127:0]               msi_pte_q;
    logic [10:0]                msi_pte_nid;

    logic                       ptw_rfifo_ren;
    logic                       ptw_rfifo_ren_d;
    logic [464:0]               ptw_rfifo_rdata;
    logic [464:0]               ptw_rfifo_dout;
    logic                       ptw_rfifo_empty;

    logic [3:0]                 msiptp_mode;
    logic [43:0]                msiptp_ppn;
    logic [51:0]                msi_addr_mask;
    logic [51:0]                msi_addr_pattern;
    logic                       ptw_req_sade;
    logic                       ptw_req_gade;
    logic                       ats_req_InD;
    logic [1:0]                 ptw_req_mode;
    logic                       ptw_ats_prpr;
    logic                       ptw_en_pri;
    logic [23:0]                ptw_req_did;
    logic [19:0]                ptw_req_pid;
    logic                       ptw_req_pv;
    logic [7:0]                 ptw_req_idx;
    logic                       cdw_is_error;
    logic                       cdw_is_error_d;
    logic                       ptw_is_bare_pre;
    logic [1:0]                 ptw_gppn_mode;
    logic                       ptw_implicit_access;
    logic                       ptw_is_bare;
    logic                       ptw_s1_en;
    logic                       ptw_s2_en;
    logic                       ptw_pc_sum;
//  logic                       ptw_pc_ens;
    logic                       ptw_priv_lvl;
    logic [5:0]                 ptw_trans_type;
    logic [51:0]                ptw_req_iova;
    logic [43:0]                ptw_gppn;
    logic [43:0]                ptw_iohgatp_ppn;
    logic [43:0]                ptw_iosatp_ppn;
    logic [15:0]                ptw_gscid;
    logic [19:0]                ptw_pscid;

    logic                       msi_en;
    logic                       iova_is_msi;
    logic                       msi_iova_match;
    logic                       msi_gpa_match;
    logic                       msi_gpa_match_q;
    logic                       gpa_is_msi;
    logic [51:0]                msi_extract_addr;
    logic [51:0]                msi_extract_addr_q;
    logic [51:0]                msi_gpa;

    logic                       ptw_arvalid;
    logic [255:0]               ptw_rdata;
    logic [511:0]               ptw_pte;

    logic                       s1ptc_hit_valid;
    logic                       s1ptc_hit;
    logic                       s2ptc_hit_valid;
    logic                       s2ptc_hit;

    logic                       lookup_s1ptc_valid;
    logic                       lookup_s2ptc_valid;
    logic                       s2_last_flag;
    logic [2:0]                 s1_lvl_cnt;

    logic                       cdw_flush;
    logic                       cdw_implicit_done;
    logic [43:0]                cdw_implicit_ppn;

    // SV48x4 defines a 50 bit GPA for second stage
    logic [RISCV_GLEN-1:0]      ptw_gpa_n;
    logic [RISCV_GLEN-1:0]      ptw_gpa_q;
    logic [RISCV_PLEN-1:0]      ptw_pptr_q;
    logic [RISCV_GLEN-1:0]      ptw_gpa_iotval2;

    logic [2:0]                 ptw_iova_sel;
    logic [2:0]                 ptw_gppn_sel;
    logic [63:0]                s1ptc_pte;
    logic [63:0]                s2ptc_pte;

    logic [63:0]                pte_q;
    logic [2:0]                 ptw_iova_sel_q;
    logic [2:0]                 ptw_gppn_sel_q;

    logic                       up_s1ptc_valid;
    logic                       up_s1ptc_d;
    logic [2:0]                 up_s1ptc_lvl;
    logic [511:0]               up_s1ptc_pte;

    logic                       up_s2ptc_valid;
    logic                       up_s2ptc_d;
    logic [2:0]                 up_s2ptc_lvl;
    logic [511:0]               up_s2ptc_pte;

    // To determine if transaction is a store
    logic                       req_is_store;
    logic                       req_is_rx;
    logic                       req_is_load;
    logic [51:0]                ptw_spaddr;
    logic                       s1_ppn_rsv_error;
    logic                       s2_ppn_rsv_error;
    logic                       ext_error;
    logic                       s1_permit_error;
    logic                       s2_permit_error;
    logic                       ptw_iova_error;
    logic                       ptw_error_flag;
    logic                       ptw_error_flag_d;
//  logic                       msi_match_error;
    logic                       ptw_other_error;
    logic                       guest_fq;
    logic                       iotval2_bit0;
    logic                       iotval2_bit1;
    logic                       ptw_fq_valid;
    logic [11:0]                ptw_cause_code;

    logic                       ptw_s1_walk_a;
    logic                       ptw_s2_walk_a;
    logic                       ptw_s1_walk_d;
    logic                       ptw_s2_walk_d;
    logic                       ptw_s1_walk;
    logic                       ptw_s2_walk;

    logic                       svnapot_en_s1;
    logic                       svnapot_en_s2;
    logic                       svnapot_en_s12;
    logic                       svnapot_en;
    logic                       mrif_en;
    logic [43:0]                mrif_nppn;
    logic [17:0]                acd_mix_para1;
    logic [19:0]                acd_mix_para2;
    logic [19:0]                acd_para;
    logic [1:0]                 op_code;
    logic [1:0]                 s1_size;
    logic [1:0]                 s2_size;
    logic [8:0]                 s1_perm;
    logic [8:0]                 s2_perm;
    logic                       ptw_wfifo_wen;
    logic [319:0]               ptw_wfifo_din;

    //ats_req signals
    logic [2:0]                 ama;
    logic [3:0]                 trans_rng;
    logic                       bypass;
    logic                       untranslated;
    logic                       allow_x;
    logic                       allow_w;
    logic                       allow_r;
    logic                       ats_resp_wen;
    logic [113:0]               ats_resp_wdata;

    //remove-5bit-reserved
    logic                       ats_fault_valid;
    logic [15:0]                ats_fault_record;
    logic [1:0]                 ats_fault_type;

    logic                       up_a_en;
    logic                       up_d_en;
    logic                       up_d_en_q;
    logic [7:0]                 up_ad_pte;
    logic [52:0]                up_ad_addr;

    logic  [3:0]                ptw_awid;
    logic  [31:0]               ptw_wstrb;
    logic  [255:0]              ptw_wdata;
    logic  [3:0]                ptw_awid_q;
    logic  [31:0]               ptw_wstrb_q;
    logic  [255:0]              ptw_wdata_q;
////////**********************************************************************************************************///////
//axi4 interface siganls
////////**********************************************************************************************************///////
// AR
assign ptw_arid_o       = 4'b0000;
assign ptw_araddr_o     = {ptw_pptr_q[RISCV_PLEN-1:6],6'd0};     // 56bit,Physical address to access
assign ptw_arlen_o    = 8'b1;                 // 1 beat per burst only
assign ptw_arvalid_o = ptw_arvalid;


always@(*) begin
    if ((ptw_cs == S1_L3_REQ) || (ptw_cs == S1_L2_REQ) || (ptw_cs == S1_L1_REQ) || (ptw_cs == S1_L0_REQ) ||
        (ptw_cs == S2_L3_REQ) || (ptw_cs == S2_L2_REQ) || (ptw_cs == S2_L1_REQ) || (ptw_cs == S2_L0_REQ))
        ptw_arvalid = 1'b1;
    else
        ptw_arvalid = 1'b0;
end

// R
assign ptw_rready_o = 1'b1;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_rdata <= 256'd0;
    else if (ptw_rvalid_i && !ptw_rlast_i)
        ptw_rdata <= ptw_rdata_i;
    else
        ptw_rdata <= ptw_rdata;
end

//assign pte = pte_t'(ptw_rdata_i);
assign ptw_pte = {ptw_rdata_i,ptw_rdata};


////////**********************************************************************************************************///////
//buffer and gen signals from cdw module
////////**********************************************************************************************************///////
atd_sync_fifo
    #(
    .FIFO_WID       (465),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u_ptw_rfifo_sync(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (ptw_rfifo_wen_i),
    .ren                (ptw_rfifo_ren),
    .din                (ptw_rfifo_din_i),
    .dout               (ptw_rfifo_rdata),
    .full               (ptw_rfifo_full_o),
    .empty              (ptw_rfifo_empty)
   );


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_rfifo_ren <= 1'b0;
    else if ((ptw_cs == IDLE) && !ptw_rfifo_empty)
        ptw_rfifo_ren <= 1'b1;
    else
        ptw_rfifo_ren <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_rfifo_dout <= 'd0;
    else if (ptw_rfifo_ren)
        ptw_rfifo_dout <= ptw_rfifo_rdata;
    else
        ptw_rfifo_dout <= ptw_rfifo_dout;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_rfifo_ren_d <= 1'b0;
    else
        ptw_rfifo_ren_d <= ptw_rfifo_ren;
end


assign msiptp_mode = ptw_rfifo_dout[464:461];
assign msiptp_ppn = ptw_rfifo_dout[460:417];
assign msi_addr_mask = ptw_rfifo_dout[416:365];
assign msi_addr_pattern = ptw_rfifo_dout[364:313];
assign ptw_req_sade = ptw_rfifo_dout[312];
assign ptw_req_gade = ptw_rfifo_dout[311];
assign ats_req_InD = ptw_rfifo_dout[310];
assign ptw_req_mode = ptw_rfifo_dout[309:308];
assign ptw_ats_prpr = ptw_rfifo_dout[307];
assign ptw_en_pri = ptw_rfifo_dout[306];
//assign ptw_pc_ens = ptw_rfifo_dout[238];
assign ptw_t2gpa = ptw_rfifo_dout[233];
assign ptw_en_ats =  ptw_rfifo_dout[232];
assign ptw_req_did = ptw_rfifo_dout[305:282];
assign ptw_req_pid = ptw_rfifo_dout[281:262];
assign ptw_req_pv = ptw_rfifo_dout[261];
assign ptw_req_idx = ptw_rfifo_dout[260:253];
assign cdw_is_error = ptw_rfifo_rdata[252] && ptw_rfifo_ren;
assign ptw_is_bare_pre = ptw_rfifo_rdata[230] && ptw_rfifo_ren;
assign ptw_gppn_mode = ptw_rfifo_dout[251:250];
assign acd_para = {2'd0,ptw_rfifo_dout[249:232]};
assign ptw_implicit_access = ptw_rfifo_dout[231];
assign ptw_is_bare = ptw_rfifo_dout[230];
assign ptw_s1_en = ptw_rfifo_dout[229];
assign ptw_s2_en = ptw_rfifo_dout[228];
assign ptw_pc_sum = ptw_rfifo_dout[227];
assign ptw_priv_lvl = ptw_rfifo_dout[226];
assign ptw_trans_type = ptw_rfifo_dout[225:220];
assign ptw_req_iova = ptw_rfifo_dout[219:168];
assign ptw_gppn = ptw_rfifo_dout[167:124];
assign ptw_iohgatp_ppn = ptw_rfifo_dout[123:80];
assign ptw_iosatp_ppn = ptw_rfifo_dout[79:36];
assign ptw_gscid = ptw_rfifo_dout[35:20];
assign ptw_pscid = ptw_rfifo_dout[19:0];


assign msi_en = msiptp_mode != 4'd0;
assign msi_iova_match = ((ptw_req_iova & ~msi_addr_mask) == (msi_addr_pattern & ~msi_addr_mask));
assign iova_is_msi = (!ptw_s1_en && msi_en && req_is_store && !ptw_implicit_access && msi_iova_match);

////////**********************************************************************************************************///////
//regiter input signals
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) begin
        s1ptc_hit_valid <= 1'b0;
        s1ptc_hit <= 1'b0;
        s2ptc_hit_valid <= 1'b0;
        s2ptc_hit <= 1'b0;
    end else begin
        s1ptc_hit_valid <= s1ptc_hit_valid_i;
        s1ptc_hit <= s1ptc_hit_i;
        s2ptc_hit_valid <= s2ptc_hit_valid_i;
        s2ptc_hit <= s2ptc_hit_i;
    end
end

////////**********************************************************************************************************///////
//ptw module main fsm
////////**********************************************************************************************************///////

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_cs <= IDLE;
    else
        ptw_cs <= ptw_ns;
end


always@(*) begin
    case (ptw_cs)
        IDLE:
            if (!ptw_rfifo_empty)
                ptw_ns = PTW_LOOKUP_DELAY;
            else
                ptw_ns = IDLE;
        PTW_LOOKUP_DELAY:
            if (ptw_is_bare_pre || ((ptw_req_mode == 2'd0) && cdw_is_error))
                ptw_ns = PTW_END;
            else if (((ptw_req_mode == 2'd2) && !ptw_en_pri) || ((ptw_req_mode == 2'd1) && !ptw_en_ats) || ptw_error_flag)
                ptw_ns = ERROR;
            else
                ptw_ns = PTW_LOOKUP_REQ;
        PTW_LOOKUP_REQ:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (ptw_rfifo_ren_d && iova_is_msi && !ptw_s1_en)
                ptw_ns = PTW_MSI_DELAY;//access msi_pte_req,iova is gpa
            else if ((lookup_s1ptc_valid && lookup_s1ptc_ready_i) || (lookup_s2ptc_valid && lookup_s2ptc_ready_i))
                ptw_ns = PTW_LOOKUP_ACK;
            else
                ptw_ns = PTW_LOOKUP_REQ;
        PTW_LOOKUP_ACK:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (s1ptc_hit_valid_i && s1ptc_hit_i)
                if (msi_gpa_match && !req_is_store)
                    ptw_ns = ERROR;
                else if (gpa_is_msi)
                    ptw_ns = PTW_MSI_DELAY;//access msi_pte_req
                else if (ptw_s2_en)
                    ptw_ns = PTW_LOOKUP_REQ;
                else if (s1ptc_hit_lvl_i == 3'd3)
                    ptw_ns = S1_L3_PTE;
                else if (s1ptc_hit_lvl_i == 3'd2)
                    ptw_ns = S1_L2_PTE;
                else if (s1ptc_hit_lvl_i == 3'd1)
                    ptw_ns = S1_L1_PTE;
                else if (s1ptc_hit_lvl_i == 3'd0)
                    ptw_ns = PTW_END;
                else
                    ptw_ns = PTW_LOOKUP_ACK;
            else if (s2ptc_hit_valid_i && s2ptc_hit_i)
                if (s2ptc_hit_lvl_i == 3'd3)
                    ptw_ns = S2_L3_PTE;
                else if (s2ptc_hit_lvl_i == 3'd2)
                    ptw_ns = S2_L2_PTE;
                else if (s2ptc_hit_lvl_i == 3'd1)
                    ptw_ns = S2_L1_PTE;
                else if (s2ptc_hit_lvl_i == 3'd0)
                    ptw_ns = S2_L0_PTE;
                else
                    ptw_ns = PTW_LOOKUP_ACK;
            else if (s2ptc_hit_valid_i && !s2ptc_hit_i)
                if (acd_para[15:12] == 4'd9)//sv48x4
                    ptw_ns = S2_L3_REQ;
                else if (acd_para[15:12] == 4'd8)//sv39x4
                    ptw_ns = S2_L2_REQ;
                else
                    ptw_ns = PTW_LOOKUP_ACK;
            else if (s1ptc_hit_valid_i && !s1ptc_hit_i)
                if (ptw_s2_en)
                    ptw_ns = PTW_LOOKUP_REQ;
                else if (acd_para[11:8] == 4'd9)//sv48
                    ptw_ns = S1_L3_REQ;
                else if (acd_para[11:8] == 4'd8)//sv39
                    ptw_ns = S1_L2_REQ;
                else
                    ptw_ns = PTW_LOOKUP_ACK;
            else
                ptw_ns = PTW_LOOKUP_ACK;
        PTW_MSI_DELAY:
            ptw_ns = S2_L1_PTE;//access msi_pte_req            
        S1_L3_REQ:
            if (ptw_arready_i)
                ptw_ns = S1_L3_PTE;
            else
                ptw_ns = S1_L3_REQ;
        S1_L3_PTE:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && (pte.r || pte.x) && !ptw_s2_en)
                if (up_d_en)
                    ptw_ns = AW_REQ;
                else
                    ptw_ns = PTW_END;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && !ptw_s2_en)
                ptw_ns = S1_L2_REQ;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en)
                if (up_d_en)
                    ptw_ns = AW_REQ_S1;
                else
                    ptw_ns = PTW_LOOKUP_REQ;
            else
                ptw_ns = S1_L3_PTE;
        S1_L2_REQ:
            if (ptw_arready_i)
                ptw_ns = S1_L2_PTE;
            else
                ptw_ns = S1_L2_REQ;
        S1_L2_PTE:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && (pte.r || pte.x) && !ptw_s2_en)
                if (up_d_en)
                    ptw_ns = AW_REQ;
                else
                    ptw_ns = PTW_END;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && !ptw_s2_en)
                ptw_ns = S1_L1_REQ;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en)
                if (up_d_en)
                    ptw_ns = AW_REQ_S1;
                else
                    ptw_ns = PTW_LOOKUP_REQ;
            else
                ptw_ns = S1_L2_PTE;
        S1_L1_REQ:
            if (ptw_arready_i)
                ptw_ns = S1_L1_PTE;
            else
                ptw_ns = S1_L1_REQ;
        S1_L1_PTE:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && (pte.r || pte.x) && !ptw_s2_en)
                if (up_d_en)
                    ptw_ns = AW_REQ;
                else
                    ptw_ns = PTW_END;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && !ptw_s2_en)
                ptw_ns = S1_L0_REQ;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en)
                if (up_d_en)
                    ptw_ns = AW_REQ_S1;
                else
                    ptw_ns = PTW_LOOKUP_REQ;
            else
                ptw_ns = S1_L1_PTE;
        S1_L0_REQ:
            if (ptw_arready_i)
                ptw_ns = S1_L0_PTE;
            else
                ptw_ns = S1_L0_REQ;
        S1_L0_PTE:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && msi_gpa_match && !req_is_store)
                ptw_ns = ERROR;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && gpa_is_msi)
                ptw_ns = S2_L1_PTE;//access msi_pte_req
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && !ptw_s2_en)
                if (up_d_en)
                    ptw_ns = AW_REQ;
                else
                    ptw_ns = PTW_END;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en)
                if (up_d_en)
                    ptw_ns = AW_REQ_S1;
                else
                    ptw_ns = PTW_LOOKUP_REQ;
            else
                ptw_ns = S1_L0_PTE;
        //stage2 mode
        S2_L3_REQ:
            if (ptw_arready_i)
                ptw_ns = S2_L3_PTE;
            else
                ptw_ns = S2_L3_REQ;
        S2_L3_PTE:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && (pte.r || pte.x))
                if (s2_last_flag || (!ptw_s1_en && !ptw_implicit_access))
                    if (up_d_en)
                        ptw_ns = AW_REQ;
                    else
                        ptw_ns = PTW_END;
                else if (s1_lvl_cnt == 3'd4)
                    ptw_ns = S1_L3_REQ;
                else if (s1_lvl_cnt == 3'd3)
                    ptw_ns = S1_L2_REQ;
                else if (s1_lvl_cnt == 3'd2)
                    ptw_ns = S1_L1_REQ;
                else if (s1_lvl_cnt == 3'd1)
                    ptw_ns = S1_L0_REQ;
                else//cdw_implicit_access
                    ptw_ns = IDLE;
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit))
                ptw_ns = S2_L2_REQ;
            else
                ptw_ns = S2_L3_PTE;
        S2_L2_REQ:
            if (ptw_arready_i)
                ptw_ns = S2_L2_PTE;
            else
                ptw_ns = S2_L2_REQ;
        S2_L2_PTE:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && (pte.r || pte.x))
                if (s2_last_flag || (!ptw_s1_en && !ptw_implicit_access))
                    if (up_d_en)
                        ptw_ns = AW_REQ;
                    else
                        ptw_ns = PTW_END;
                else if (s1_lvl_cnt == 3'd4)
                    ptw_ns = S1_L3_REQ;
                else if (s1_lvl_cnt == 3'd3)
                    ptw_ns = S1_L2_REQ;
                else if (s1_lvl_cnt == 3'd2)
                    ptw_ns = S1_L1_REQ;
                else if (s1_lvl_cnt == 3'd1)
                    ptw_ns = S1_L0_REQ;
                else//cdw_implicit_access
                    ptw_ns = IDLE;
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit))
                ptw_ns = S2_L1_REQ;
            else
                ptw_ns = S2_L2_PTE;
        S2_L1_REQ:
            if (ptw_arready_i)
                ptw_ns = S2_L1_PTE;
            else
                ptw_ns = S2_L1_REQ;
        S2_L1_PTE:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && (pte.r || pte.x))
                if (s2_last_flag || (!ptw_s1_en && !ptw_implicit_access))
                    if (up_d_en)
                        ptw_ns = AW_REQ;
                    else
                        ptw_ns = PTW_END;
                else if (s1_lvl_cnt == 3'd4)
                    ptw_ns = S1_L3_REQ;
                else if (s1_lvl_cnt == 2'd3)
                    ptw_ns = S1_L2_REQ;
                else if (s1_lvl_cnt == 2'd2)
                    ptw_ns = S1_L1_REQ;
                else if (s1_lvl_cnt == 2'd1)
                    ptw_ns = S1_L0_REQ;
                else//cdw_implicit_access
                    ptw_ns = IDLE;
             else if ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit) || iova_is_msi || gpa_is_msi)
                ptw_ns = S2_L0_REQ;
            else
                ptw_ns = S2_L1_PTE;

        S2_L0_REQ:
            if (ptw_arready_i)
                ptw_ns = S2_L0_PTE;
            else
                ptw_ns = S2_L0_REQ;
        S2_L0_PTE:
            if (ptw_error_flag)
                ptw_ns = ERROR;
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit))
                if (s2_last_flag || (!ptw_s1_en && !ptw_implicit_access) || iova_is_msi || gpa_is_msi)
                    if (up_d_en)
                        ptw_ns = AW_REQ;
                    else
                        ptw_ns = PTW_END;
                else if (s1_lvl_cnt == 3'd4)
                    ptw_ns = S1_L3_REQ;
                else if (s1_lvl_cnt == 2'd3)
                    ptw_ns = S1_L2_REQ;
                else if (s1_lvl_cnt == 2'd2)
                    ptw_ns = S1_L1_REQ;
                else if (s1_lvl_cnt == 2'd1)
                    ptw_ns = S1_L0_REQ;
                else// no cdw_implicit_access
                    ptw_ns = IDLE;
            else
                ptw_ns = S2_L0_PTE;
        AW_REQ_S1:
            if (ptw_awready_i)
                ptw_ns = W_DATA;
            else
                ptw_ns = AW_REQ_S1;
        AW_REQ:
            if (ptw_awready_i)
                ptw_ns = W_DATA;
            else
                ptw_ns = AW_REQ;
        W_DATA:
            if (ptw_wready_i)
                ptw_ns = B_RESP;
            else
                ptw_ns = W_DATA;
        B_RESP:
            if (ptw_bvalid_i && (ptw_bresp_i != RESP_OKAY))
                ptw_ns = ERROR;
            else if (ptw_bvalid_i && (ptw_bid_i == UP_D1_ID))
                ptw_ns = PTW_LOOKUP_REQ;
            else if (ptw_bvalid_i && (ptw_bid_i == UP_D2_ID))
                ptw_ns = PTW_END;
            else
                ptw_ns = B_RESP;
        PTW_END://gen updata_iova
            if (((ptw_req_mode == 2'd0) && !ptw_wfifo_full_i) ||
                ((ptw_req_mode == 2'd1) && !ats_resp_full_i))
                ptw_ns = IDLE;
            else
                ptw_ns = PTW_END;
        ERROR:
            if (((ptw_req_mode == 2'd0) && !ptw_wfifo_full_i && ptw_fq_ready_i) ||
                ((ptw_req_mode == 2'd1) && !ats_fault_ready_i))
                ptw_ns = IDLE;
            else
                ptw_ns = ERROR;
        default:
             ptw_ns = IDLE;
    endcase
end

////////**********************************************************************************************************///////
//ptw lookup s1ptc and get hit result
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        lookup_s1ptc_valid <= 1'b0;
    else if (ptw_rfifo_ren || ptw_error_flag)
        lookup_s1ptc_valid <= 1'b0;
    else if ((ptw_cs == PTW_LOOKUP_REQ) && lookup_s1ptc_valid && lookup_s1ptc_ready_i)
        lookup_s1ptc_valid <= 1'b0;
    else if (ptw_rfifo_ren_d && (ptw_req_mode == 2'd2) && ptw_en_pri)
        lookup_s1ptc_valid <= 1'b1;
    else if (ptw_rfifo_ren_d && (ptw_req_mode == 2'd1) && ptw_en_ats)
        lookup_s1ptc_valid <= 1'b1;
    else if (ptw_rfifo_ren_d && (ptw_req_mode == 2'd0) && ptw_s1_en)
        lookup_s1ptc_valid <= 1'b1;
    else
        lookup_s1ptc_valid <= lookup_s1ptc_valid;
end

assign lookup_s1ptc_valid_o = lookup_s1ptc_valid;
assign lookup_s1ptc_lvl_o = 3'd0;
assign lookup_s1ptc_leaf_o = 1'b0;
assign lookup_s1ptc_gv_o = (acd_para[15:12] != 4'd0);
assign lookup_s1ptc_pscid_o = ptw_pscid;
assign lookup_s1ptc_gscid_o = ptw_gscid;
assign lookup_s1ptc_va_o = ptw_req_iova;

////////**********************************************************************************************************///////
//ptw lookup s2ptc and get hit result
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        lookup_s2ptc_valid <= 1'b0;
    else if (ptw_rfifo_ren || ptw_error_flag)
        lookup_s2ptc_valid <= 1'b0;
    else if ((ptw_cs == PTW_LOOKUP_REQ) && lookup_s2ptc_valid && lookup_s2ptc_ready_i)
        lookup_s2ptc_valid <= 1'b0;
    else if ((ptw_cs == PTW_LOOKUP_ACK) && (ptw_req_mode == 2'd2) && s1ptc_hit_valid_i && ptw_s2_en && ptw_en_pri)
        lookup_s2ptc_valid <= 1'b1;
    else if (ptw_rfifo_ren_d && (ptw_req_mode == 2'd1) && ptw_en_ats && ptw_t2gpa)
        lookup_s2ptc_valid <= 1'b1;
    else if (ptw_rfifo_ren_d && (ptw_req_mode == 2'd0) && (ptw_implicit_access || (!ptw_s1_en && ptw_s2_en && !iova_is_msi)))
        lookup_s2ptc_valid <= 1'b1;
    else if ((ptw_cs == PTW_LOOKUP_ACK) && (ptw_req_mode == 2'd0) && s1ptc_hit_valid_i && ptw_s2_en)
        lookup_s2ptc_valid <= 1'b1;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE)) &&
                    ptw_rvalid_i && ptw_rlast_i && ptw_s2_en && !up_d_en && !gpa_is_msi)
        lookup_s2ptc_valid <= 1'b1;
    else if ((ptw_cs == B_RESP) && ptw_bvalid_i && (ptw_bresp_i == RESP_OKAY) && (ptw_bid_i == UP_D1_ID) && ptw_s2_en)
        lookup_s2ptc_valid <= 1'b1;
    else
        lookup_s2ptc_valid <= lookup_s2ptc_valid;
end

assign lookup_s2ptc_valid_o = lookup_s2ptc_valid;
assign lookup_s2ptc_lvl_o = 3'd0;
assign lookup_s2ptc_leaf_o = 1'b0;
assign lookup_s2ptc_gscid_o = ptw_gscid;
assign lookup_s2ptc_gpa_o = {14'd0,ptw_gpa_q[RISCV_GLEN-1:12]};


////////**********************************************************************************************************///////
//ptw fsm control signals and gen addr
////////**********************************************************************************************************///////
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        s2_last_flag <= 1'b0;
    else if (ptw_rfifo_ren)
        s2_last_flag <= 1'b0;
    else if (ptw_rfifo_ren_d && !ptw_s1_en && ptw_s2_en && !ptw_implicit_access)
        s2_last_flag <= 1'b1;
    else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && (pte.r || pte.x) && ptw_s2_en &&
                     ((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE)))
        s2_last_flag <= 1'b1;
    else if ((ptw_cs == PTW_END) && (
            ((ptw_req_mode == 2'd0) && !ptw_wfifo_full_i) ||
            ((ptw_req_mode == 2'd1) && !ats_resp_full_i)))
        s2_last_flag <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        s1_lvl_cnt <= 3'd0;
    else if (ptw_implicit_access || ptw_rfifo_ren)
        s1_lvl_cnt <= 3'd0;
    else if (ptw_rfifo_ren_d && ptw_s1_en && ptw_s2_en && acd_para[11:8] == 4'd9)
        s1_lvl_cnt <= 3'd4;
    else if (ptw_rfifo_ren_d && ptw_s1_en && ptw_s2_en && acd_para[11:8] == 4'd8)
        s1_lvl_cnt <= 3'd3;
    else if ((ptw_cs == S1_L3_REQ) && ptw_s2_en)
        s1_lvl_cnt <= 3'd3;
    else if ((ptw_cs == S1_L2_REQ) && ptw_s2_en)
        s1_lvl_cnt <= 3'd2;
    else if ((ptw_cs == S1_L1_REQ) && ptw_s2_en)
        s1_lvl_cnt <= 3'd1;
    else if ((ptw_cs == S1_L0_REQ) && ptw_s2_en)
        s1_lvl_cnt <= 3'd0;
end


always@(*) begin
    case (ptw_cs)
        PTW_LOOKUP_REQ:
            if (ptw_rfifo_ren_d && ptw_implicit_access && (acd_para[15:12] == 4'd9))//pc+s2,sv48x4
                if (ptw_gppn_mode == 2'd2)
                    ptw_gpa_n = {ptw_gppn[RISCV_GLEN-12-1:0],6'd0,ptw_req_pid[19:17],3'd0};
                else if (ptw_gppn_mode == 2'd1)
                    ptw_gpa_n = {ptw_gppn[RISCV_GLEN-12-1:0],ptw_req_pid[16:8],3'd0};
                else
                    ptw_gpa_n = {ptw_gppn[RISCV_GLEN-12-1:0],ptw_req_pid[7:0],4'd0};
            else if (ptw_rfifo_ren_d && ptw_implicit_access && (acd_para[15:12] == 4'd8))//pc+s2,sv39x4
                if (ptw_gppn_mode == 2'd2)
                    ptw_gpa_n = {9'd0,ptw_gppn[RISCV_GLEN-12-9-1:0],6'd0,ptw_req_pid[19:17],3'd0};
                else if (ptw_gppn_mode == 2'd1)
                    ptw_gpa_n = {9'd0,ptw_gppn[RISCV_GLEN-12-9-1:0],ptw_req_pid[16:8],3'd0};
                else
                    ptw_gpa_n = {9'd0,ptw_gppn[RISCV_GLEN-12-9-1:0],ptw_req_pid[7:0],4'd0};
            else if (ptw_rfifo_ren_d && ptw_s1_en && ptw_s2_en && (acd_para[15:8] == 8'h99))//dc+s1+s2,s2_sv48x4,s1_sv48
                ptw_gpa_n = {ptw_iosatp_ppn[RISCV_GLEN-12-1:0], ptw_req_iova[35:27], 3'd0};
            else if (ptw_rfifo_ren_d && ptw_s1_en && ptw_s2_en && (acd_para[15:8] == 8'h98))//dc+s1+s2,s2_sv48x4,s1_sv39
                ptw_gpa_n = {ptw_iosatp_ppn[RISCV_GLEN-12-1:0], ptw_req_iova[26:18], 3'd0};
            else if (ptw_rfifo_ren_d && ptw_s1_en && ptw_s2_en && (acd_para[15:8] == 8'h89))//dc+s1+s2,s2_sv39x4,s1_sv48
                ptw_gpa_n = {9'd0,ptw_iosatp_ppn[RISCV_GLEN-12-9-1:0], ptw_req_iova[35:27], 3'd0};
            else if (ptw_rfifo_ren_d && ptw_s1_en && ptw_s2_en && (acd_para[15:8] == 8'h88))//dc+s1+s2,s2_sv39x4,s1_sv39
                ptw_gpa_n = {9'd0,ptw_iosatp_ppn[RISCV_GLEN-12-9-1:0], ptw_req_iova[26:18], 3'd0};
            else if (ptw_rfifo_ren_d && !ptw_s1_en && ptw_s2_en && (acd_para[15:12] == 4'd9))//dc+s2,sv48x4
                ptw_gpa_n = {ptw_req_iova[RISCV_GLEN-12-1:0],12'd0};
            else if (ptw_rfifo_ren_d && !ptw_s1_en && ptw_s2_en && (acd_para[15:12] == 4'd8))//dc+s2,sv39x4
                ptw_gpa_n = {9'd0,ptw_req_iova[RISCV_GLEN-12-9-1:0],12'd0};
            else
                ptw_gpa_n = ptw_gpa_q;
        S1_L3_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en)
                ptw_gpa_n = {pte.ppn[RISCV_GLEN-12-1:0], ptw_req_iova[26:18], 3'b0};
            else
                ptw_gpa_n = ptw_gpa_q;
        S1_L2_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en)
                ptw_gpa_n = {pte.ppn[RISCV_GLEN-12-1:0], ptw_req_iova[17:9], 3'b0};
            else
                ptw_gpa_n = ptw_gpa_q;
        S1_L1_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en)
                ptw_gpa_n = {pte.ppn[RISCV_GLEN-12-1:0], ptw_req_iova[8:0], 3'b0};
            else
                ptw_gpa_n = ptw_gpa_q;
        S1_L0_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en)
                ptw_gpa_n = {pte.ppn[RISCV_GLEN-12-1:0], 12'd0};
            else
                ptw_gpa_n = ptw_gpa_q;
        default:
             ptw_gpa_n = ptw_gpa_q;
    endcase
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_gpa_q <= 50'd0;
    else
        ptw_gpa_q <= ptw_gpa_n;
end

always@(*) begin
    if (ptw_rfifo_ren)
        msi_gpa_match = 1'b0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE)) &&
         ((s1ptc_hit_valid && s1ptc_hit) || (ptw_rvalid_i && ptw_rlast_i)) && (pte.r || pte.x) && ptw_s1_en && msi_en && !ptw_implicit_access)
        msi_gpa_match = (({8'd0,pte.ppn} & ~msi_addr_mask) == (msi_addr_pattern & ~msi_addr_mask));
    else
        msi_gpa_match = msi_gpa_match_q;
    end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_gpa_match_q <= 1'b0;
    else
        msi_gpa_match_q <= msi_gpa_match;
end

assign gpa_is_msi = msi_gpa_match && req_is_store;
assign msi_gpa = iova_is_msi ? ptw_req_iova : {8'd0,pte.ppn};


//test timing
logic [51:0]    msi_gpa_q;
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_gpa_q <= 52'd0;
    else
        msi_gpa_q <= msi_gpa;
end

integer i;
logic [5:0] j;
always @(*) begin
    msi_extract_addr = 52'd0;
    j = 6'd0;

    for (i = 0; i < 52; i = i + 1) begin
        if (msi_addr_mask[i]) begin
            msi_extract_addr[j] = msi_gpa_q[i];
            j = j + 1;
        end
    end
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_extract_addr_q <= 52'd0;
    else
        msi_extract_addr_q <= msi_extract_addr;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_pptr_q  <= 56'd0;
    else case (ptw_cs)
        PTW_LOOKUP_REQ:
            if ((ptw_s2_en || ptw_implicit_access) && (acd_para[15:12]==4'd9))//sv48x4
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-1:39], 3'b0};
            else if ((ptw_s2_en || ptw_implicit_access) && (acd_para[15:12]==4'd8))//sv39x4
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-9-1:30], 3'b0};
            else if (ptw_s1_en && !ptw_s2_en  && (acd_para[11:8]==4'd9))    //sv48
                ptw_pptr_q <= {ptw_iosatp_ppn, ptw_req_iova[35:27], 3'b0};
            else if (ptw_s1_en && !ptw_s2_en  && (acd_para[11:8]==4'd8))    //sv39
                ptw_pptr_q <= {ptw_iosatp_ppn, ptw_req_iova[26:18], 3'b0};
        S1_L3_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en && (acd_para[15:12]==4'd9))
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-1:39], 3'b0};
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en && (acd_para[15:12]==4'd8))
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-9-1:30], 3'b0};
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit))
                ptw_pptr_q <= {pte.ppn, ptw_req_iova[26:18],3'd0};
        S1_L2_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en && (acd_para[15:12]==4'd9))
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-1:39], 3'b0};
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en && (acd_para[15:12]==4'd8))
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-9-1:30], 3'b0};
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit))
                ptw_pptr_q <= {pte.ppn, ptw_req_iova[17:9],3'd0};
        S1_L1_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en && (acd_para[15:12]==4'd9))
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-1:39], 3'b0};
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en && (acd_para[15:12]==4'd8))
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-9-1:30], 3'b0};
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit))
                ptw_pptr_q <= {pte.ppn, ptw_req_iova[8:0], 3'd0};
        S1_L0_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en && (acd_para[15:12]==4'd9))
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-1:39], 3'b0};
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && ptw_s2_en && (acd_para[15:12]==4'd8))
                ptw_pptr_q <= {ptw_iohgatp_ppn[43:2], ptw_gpa_n[RISCV_GLEN-9-1:30], 3'b0};
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit))
                ptw_pptr_q <= {pte.ppn,12'd0};
        S2_L3_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && (pte.r || pte.x))
                ptw_pptr_q <=  {pte.ppn[43:27], ptw_gpa_q[38:0]};
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit))
                ptw_pptr_q <= {pte.ppn,ptw_gpa_q[38:30], 3'b0};
        S2_L2_PTE:
            if (((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && (pte.r || pte.x))
                ptw_pptr_q <=  {pte.ppn[43:18], ptw_gpa_q[29:0]};
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit))
                ptw_pptr_q <= {pte.ppn,ptw_gpa_q[29:21], 3'b0};
        S2_L1_PTE:
            if (iova_is_msi || gpa_is_msi)//access msi_pte_req
                ptw_pptr_q <= {msiptp_ppn,12'd0} | {msi_extract_addr_q,4'd0};
            else if (((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && (pte.r || pte.x))
                ptw_pptr_q <=  {pte.ppn[43:9], ptw_gpa_q[20:0]};
            else if ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit))
                ptw_pptr_q <= {pte.ppn, ptw_gpa_q[20:12], 3'b0};
        S2_L0_PTE:
            if ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) begin
                ptw_pptr_q <= {pte.ppn, ptw_gpa_q[11:0]};
            end
        ERROR:
             ptw_pptr_q <= 56'd0;
        default:
//             ptw_pptr_q <= 56'd0;
            ptw_pptr_q <= ptw_pptr_q;
    endcase
end


////////**********************************************************************************************************///////
//get s1 and s2 pte
////////**********************************************************************************************************///////
always@(*) begin
    if (ptw_rfifo_ren)
        ptw_iova_sel = 3'd0;
    else if (s1ptc_hit_valid_i && s1ptc_hit_i)
        case (s1ptc_hit_lvl_i)
            3'd0:
                ptw_iova_sel = ptw_req_iova[2:0];
            3'd1:
                ptw_iova_sel = ptw_req_iova[11:9];
            3'd2:
                ptw_iova_sel = ptw_req_iova[20:18];
            3'd3:
                ptw_iova_sel = ptw_req_iova[29:27];
            default:
                ptw_iova_sel = ptw_iova_sel_q;
        endcase
    else if (ptw_rvalid_i && ptw_rlast_i)
        case (ptw_cs)
            S1_L0_PTE:
                ptw_iova_sel = ptw_req_iova[2:0];
            S1_L1_PTE:
                ptw_iova_sel = ptw_req_iova[11:9];
            S1_L2_PTE:
                ptw_iova_sel = ptw_req_iova[20:18];
            S1_L3_PTE:
                ptw_iova_sel = ptw_req_iova[29:27];
            default:
                ptw_iova_sel = ptw_iova_sel_q;
        endcase
    else
        ptw_iova_sel = ptw_iova_sel_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_iova_sel_q <= 3'd0;
    else
        ptw_iova_sel_q <= ptw_iova_sel;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        s1ptc_pte <= 64'd0;
    else if (ptw_rfifo_ren)
        s1ptc_pte <= 64'd0;
    else if ((ptw_cs == PTW_LOOKUP_ACK) && s1ptc_hit_valid_i && s1ptc_hit_i)
        case (ptw_iova_sel)
            3'd0:
                s1ptc_pte <= s1ptc_hit_content_i[64*1-1:0];
            3'd1:
                s1ptc_pte <= s1ptc_hit_content_i[64*2-1:64*1];
            3'd2:
                s1ptc_pte <= s1ptc_hit_content_i[64*3-1:64*2];
            3'd3:
                s1ptc_pte <= s1ptc_hit_content_i[64*4-1:64*3];
            3'd4:
                s1ptc_pte <= s1ptc_hit_content_i[64*5-1:64*4];
            3'd5:
                s1ptc_pte <= s1ptc_hit_content_i[64*6-1:64*5];
            3'd6:
                s1ptc_pte <= s1ptc_hit_content_i[64*7-1:64*6];
            3'd7:
                s1ptc_pte <= s1ptc_hit_content_i[64*8-1:64*7];
            default:
                s1ptc_pte <= 'd0;
        endcase
end


always@(*) begin
    if (ptw_rfifo_ren)
        ptw_gppn_sel = 3'd0;
    else if (s2ptc_hit_valid_i && s2ptc_hit_i)
        case (s2ptc_hit_lvl_i)
            3'd0:
                ptw_gppn_sel = ptw_gpa_q[14:12];
            3'd1:
                ptw_gppn_sel = ptw_gpa_q[23:21];
            3'd2:
                ptw_gppn_sel = ptw_gpa_q[32:30];
            3'd3:
                ptw_gppn_sel = ptw_gpa_q[41:39];
            default:
                ptw_gppn_sel = ptw_gppn_sel_q;
        endcase
    else if (ptw_rvalid_i && ptw_rlast_i)
        case (ptw_cs)
            S2_L0_PTE:
                ptw_gppn_sel = ptw_gpa_q[14:12];
            S2_L1_PTE:
                ptw_gppn_sel = ptw_gpa_q[23:21];
            S2_L2_PTE:
                ptw_gppn_sel = ptw_gpa_q[32:30];
            S2_L3_PTE:
                ptw_gppn_sel = ptw_gpa_q[41:39];
            default:
                ptw_gppn_sel = ptw_gppn_sel_q;
        endcase
    else
        ptw_gppn_sel = ptw_gppn_sel_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_gppn_sel_q <= 3'd0;
    else
        ptw_gppn_sel_q <= ptw_gppn_sel;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        s2ptc_pte <= 64'd0;
    else if (ptw_rfifo_ren)
        s2ptc_pte <= 64'd0;
    else if ((ptw_cs == PTW_LOOKUP_ACK) && s2ptc_hit_valid_i && s2ptc_hit_i)
        case (ptw_gppn_sel)
            3'd0:
                s2ptc_pte <= s2ptc_hit_content_i[64*1-1:0];
            3'd1:
                s2ptc_pte <= s2ptc_hit_content_i[64*2-1:64*1];
            3'd2:
                s2ptc_pte <= s2ptc_hit_content_i[64*3-1:64*2];
            3'd3:
                s2ptc_pte <= s2ptc_hit_content_i[64*4-1:64*3];
            3'd4:
                s2ptc_pte <= s2ptc_hit_content_i[64*5-1:64*4];
            3'd5:
                s2ptc_pte <= s2ptc_hit_content_i[64*6-1:64*5];
            3'd6:
                s2ptc_pte <= s2ptc_hit_content_i[64*7-1:64*6];
            3'd7:
                s2ptc_pte <= s2ptc_hit_content_i[64*8-1:64*7];
            default:
                s2ptc_pte <= 'd0;
        endcase
end


always@(*) begin
    if (ptw_rfifo_ren)
        pte = 64'd0;
    else if (s1ptc_hit_valid && s1ptc_hit)
        pte = pte_t'(s1ptc_pte);
    else if (s2ptc_hit_valid && s2ptc_hit)
        pte = pte_t'(s2ptc_pte);
    else if (((ptw_cs == S1_L0_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L3_PTE)) && ptw_rvalid_i && ptw_rlast_i)
        case (ptw_iova_sel)
//      case (ptw_pptr_q[5:3])
            3'd0:
                pte = pte_t'(ptw_pte[64*1-1:0]);
            3'd1:
                pte = pte_t'(ptw_pte[64*2-1:64*1]);
            3'd2:
                pte = pte_t'(ptw_pte[64*3-1:64*2]);
            3'd3:
                pte = pte_t'(ptw_pte[64*4-1:64*3]);
            3'd4:
                pte = pte_t'(ptw_pte[64*5-1:64*4]);
            3'd5:
                pte = pte_t'(ptw_pte[64*6-1:64*5]);
            3'd6:
                pte = pte_t'(ptw_pte[64*7-1:64*6]);
            3'd7:
                pte = pte_t'(ptw_pte[64*8-1:64*7]);
            default:
                pte = 64'd0;
        endcase
    else if (((ptw_cs == S2_L0_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L3_PTE)) && ptw_rvalid_i && ptw_rlast_i)
        case (ptw_gppn_sel)
//      case (ptw_pptr_q[5:3])
            3'd0:
                pte = pte_t'(ptw_pte[64*1-1:0]);
            3'd1:
                pte = pte_t'(ptw_pte[64*2-1:64*1]);
            3'd2:
                pte = pte_t'(ptw_pte[64*3-1:64*2]);
            3'd3:
                pte = pte_t'(ptw_pte[64*4-1:64*3]);
            3'd4:
                pte = pte_t'(ptw_pte[64*5-1:64*4]);
            3'd5:
                pte = pte_t'(ptw_pte[64*6-1:64*5]);
            3'd6:
                pte = pte_t'(ptw_pte[64*7-1:64*6]);
            3'd7:
                pte = pte_t'(ptw_pte[64*8-1:64*7]);
            default:
                pte = 64'd0;
        endcase
    else
        pte = pte_q;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pte_q <= 64'd0;
    else
        pte_q <= pte;
end

////////**********************************************************************************************************///////
//update s1ptc and s2ptc content
////////**********************************************************************************************************///////

assign up_s1ptc_valid_o = up_s1ptc_valid;
assign up_s1ptc_lvl_o = up_s1ptc_lvl;
assign up_s1ptc_leaf_o = up_s1ptc_lvl == 3'd0;
assign up_s1ptc_gv_o = (acd_para[15:12] != 4'd0);
assign up_s1ptc_svnapot_o = 64'd0;
assign up_s1ptc_pte_o = up_s1ptc_pte;
assign up_s1ptc_va_o = ptw_req_iova;
assign up_s1ptc_sxl_o = acd_para[5];
assign up_s1ptc_pscid_o = ptw_pscid;
assign up_s1ptc_gscid_o = ptw_gscid;
assign up_s2ptc_valid_o = up_s2ptc_valid;
assign up_s2ptc_lvl_o = up_s2ptc_lvl;
assign up_s2ptc_leaf_o = (up_s2ptc_lvl == 3'd0);
assign up_s2ptc_svnapot_o = 64'd0;
assign up_s2ptc_pte_o = up_s2ptc_pte;
//assign up_s2ptc_va_o = ptw_req_iova;
assign up_s2ptc_gpa_o = {14'd0,ptw_gpa_q[RISCV_GLEN-1:12]};
assign up_s2ptc_gscid_o = ptw_gscid;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_s1ptc_lvl <= 3'd0;
    else if (ptw_rfifo_ren)
        up_s1ptc_lvl <= 3'd0;
    else if (ptw_cs == S1_L0_REQ)
        up_s1ptc_lvl <= 3'd0;
    else if (ptw_cs == S1_L1_REQ)
        up_s1ptc_lvl <= 3'd1;
    else if (ptw_cs == S1_L2_REQ)
        up_s1ptc_lvl <= 3'd2;
    else if (ptw_cs == S1_L3_REQ)
        up_s1ptc_lvl <= 3'd3;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_s2ptc_lvl <= 3'd0;
    else if (ptw_rfifo_ren || iova_is_msi || gpa_is_msi)
        up_s2ptc_lvl <= 3'd0;
    else if (ptw_cs == S2_L0_REQ)
        up_s2ptc_lvl <= 3'd0;
    else if (ptw_cs == S2_L1_REQ)
        up_s2ptc_lvl <= 3'd1;
    else if (ptw_cs == S2_L2_REQ)
        up_s2ptc_lvl <= 3'd2;
    else if (ptw_cs == S2_L3_REQ)
        up_s2ptc_lvl <= 3'd3;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_s1ptc_valid <= 1'b0;
    else if (ptw_rfifo_ren || ptw_error_flag)
        up_s1ptc_valid <= 1'b0;
    else if (up_s1ptc_valid && up_s1ptc_ready_i)
        up_s1ptc_valid <= 1'b0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE)) && ptw_rvalid_i && ptw_rlast_i && !gpa_is_msi)
        up_s1ptc_valid <= 1'b1;
end

assign up_s1ptc_d = ptw_req_sade && req_is_store;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_s1ptc_pte <= 512'd0;
    else if (ptw_rfifo_ren)
        up_s1ptc_pte <= 512'd0;
    else if (up_s1ptc_valid && up_s1ptc_ready_i)
        up_s1ptc_pte <= 512'd0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE))
                                && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i && ptw_req_sade)
        case (ptw_iova_sel)
            3'd0:
                up_s1ptc_pte <= {ptw_pte[64*8-1:64*1],ptw_pte[64*0+63:64*0+8],up_s1ptc_d,1'b1,ptw_pte[64*0+5:64*0+0]};
            3'd1:
                up_s1ptc_pte <= {ptw_pte[64*8-1:64*2],ptw_pte[64*1+63:64*1+8],up_s1ptc_d,1'b1,ptw_pte[64*1+5:64*1+0],ptw_pte[64*1-1:0]};
            3'd2:
                up_s1ptc_pte <= {ptw_pte[64*8-1:64*3],ptw_pte[64*2+63:64*2+8],up_s1ptc_d,1'b1,ptw_pte[64*2+5:64*2+0],ptw_pte[64*2-1:0]};
            3'd3:
                up_s1ptc_pte <= {ptw_pte[64*8-1:64*4],ptw_pte[64*3+63:64*3+8],up_s1ptc_d,1'b1,ptw_pte[64*3+5:64*3+0],ptw_pte[64*3-1:0]};
            3'd4:
                up_s1ptc_pte <= {ptw_pte[64*8-1:64*5],ptw_pte[64*4+63:64*4+8],up_s1ptc_d,1'b1,ptw_pte[64*4+5:64*4+0],ptw_pte[64*4-1:0]};
            3'd5:
                up_s1ptc_pte <= {ptw_pte[64*8-1:64*6],ptw_pte[64*5+63:64*5+8],up_s1ptc_d,1'b1,ptw_pte[64*5+5:64*5+0],ptw_pte[64*5-1:0]};
            3'd6:
                up_s1ptc_pte <= {ptw_pte[64*8-1:64*7],ptw_pte[64*6+63:64*6+8],up_s1ptc_d,1'b1,ptw_pte[64*6+5:64*6+0],ptw_pte[64*6-1:0]};
            3'd7:
                up_s1ptc_pte <= {ptw_pte[64*7+63:64*7+8],up_s1ptc_d,1'b1,ptw_pte[64*7+5:64*7+0],ptw_pte[64*7-1:0]};
            default:
                up_s1ptc_pte <= 512'd0;
        endcase
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE)) && ptw_rvalid_i && ptw_rlast_i)
        up_s1ptc_pte <= ptw_pte;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_s2ptc_valid <= 1'b0;
    else if (ptw_rfifo_ren || ptw_error_flag)
        up_s2ptc_valid <= 1'b0;
    else if (up_s2ptc_valid && up_s2ptc_ready_i)
        up_s2ptc_valid <= 1'b0;
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE)) &&
                ptw_rvalid_i && ptw_rlast_i && !iova_is_msi && !gpa_is_msi)
        up_s2ptc_valid <= 1'b1;
end

assign up_s2ptc_d = ptw_req_gade && req_is_store && s2_last_flag;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_s2ptc_pte <= 512'd0;
    else if (ptw_rfifo_ren)
        up_s2ptc_pte <= 512'd0;
    else if (up_s2ptc_valid && up_s2ptc_ready_i)
        up_s2ptc_pte <= 512'd0;
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE))
                                && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i && ptw_req_gade)
        case (ptw_gppn_sel)
            3'd0:
                up_s2ptc_pte <= {ptw_pte[64*8-1:64*1],ptw_pte[64*0+63:64*0+8],up_s2ptc_d,1'b1,ptw_pte[64*0+5:64*0+0]};
            3'd1:
                up_s2ptc_pte <= {ptw_pte[64*8-1:64*2],ptw_pte[64*1+63:64*1+8],up_s2ptc_d,1'b1,ptw_pte[64*1+5:64*1+0],ptw_pte[64*1-1:0]};
            3'd2:
                up_s2ptc_pte <= {ptw_pte[64*8-1:64*3],ptw_pte[64*2+63:64*2+8],up_s2ptc_d,1'b1,ptw_pte[64*2+5:64*2+0],ptw_pte[64*2-1:0]};
            3'd3:
                up_s2ptc_pte <= {ptw_pte[64*8-1:64*4],ptw_pte[64*3+63:64*3+8],up_s2ptc_d,1'b1,ptw_pte[64*3+5:64*3+0],ptw_pte[64*3-1:0]};
            3'd4:
                up_s2ptc_pte <= {ptw_pte[64*8-1:64*5],ptw_pte[64*4+63:64*4+8],up_s2ptc_d,1'b1,ptw_pte[64*4+5:64*4+0],ptw_pte[64*4-1:0]};
            3'd5:
                up_s2ptc_pte <= {ptw_pte[64*8-1:64*6],ptw_pte[64*5+63:64*5+8],up_s2ptc_d,1'b1,ptw_pte[64*5+5:64*5+0],ptw_pte[64*5-1:0]};
            3'd6:
                up_s2ptc_pte <= {ptw_pte[64*8-1:64*7],ptw_pte[64*6+63:64*6+8],up_s2ptc_d,1'b1,ptw_pte[64*6+5:64*6+0],ptw_pte[64*6-1:0]};
            3'd7:
                up_s2ptc_pte <= {ptw_pte[64*7+63:64*7+8],up_s2ptc_d,1'b1,ptw_pte[64*7+5:64*7+0],ptw_pte[64*7-1:0]};
            default:
                up_s2ptc_pte <= 512'd0;
        endcase
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE)) && ptw_rvalid_i && ptw_rlast_i)
        up_s2ptc_pte <= ptw_pte;
end


// Indicate whether this translation was triggered by a store or a load
assign req_is_store = !ptw_trans_type[3] && (ptw_trans_type[1:0] == 2'b11);
assign req_is_rx = (!ptw_trans_type[3] && !ptw_trans_type[1] && ptw_trans_type[0]);
assign req_is_load = (!ptw_trans_type[3] && ptw_trans_type[1] && !ptw_trans_type[0]);

assign cdw_flush_o = cdw_flush;
assign cdw_implicit_done_o = cdw_implicit_done;
assign cdw_implicit_ppn_o = cdw_implicit_ppn;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_flush <= 1'b0;
    else if ((ptw_rvalid_i && (ptw_rresp_i != RESP_OKAY)) || ptw_error_flag)
        cdw_flush <= ptw_implicit_access;
    else
        cdw_flush <= 1'b0;
end

// Do not update IOTLB for CDW implicit accesses
// When Stage 2 is disabled and the GPA (SPA) is an MSI address, IOTLB is not updated yet and
// MSI translation process is invoked
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_implicit_done <= 1'b0;
    else if (ptw_implicit_access && ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) &&
            ((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE)) && (pte.r || pte.x))
        cdw_implicit_done <= 1'b1;
    else
        cdw_implicit_done <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_implicit_ppn <= 44'd0;
    else if (ptw_implicit_access && ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) &&
            ((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE)) && (pte.r || pte.x))
        cdw_implicit_ppn <= pte.ppn;
    else
        cdw_implicit_ppn <= 44'd0;
end


//First-stage checks: A fault is generated if:
//- (1): W transaction and pte.w=0;
//- (2): RX transaction and pte.x=0;
//- (3): U-mode transaction and pte.u=0;
//- (4): S-mode transaction, pte.u=1 and (PC.SUM=0 or transaction is RX)

//Second-stage checks: A fault is generated if:
//- (1): W transaction and pte.w=0;
//- (2): RX transaction and pte.x=0;

assign s1_ppn_rsv_error = (acd_para[11:8] == 4'd9) ? |pte.ppn[43:38] : |pte.ppn[43:29];
assign s2_ppn_rsv_error = (acd_para[15:12] == 4'd9) ? |pte.ppn[43:38] : |pte.ppn[43:29];
assign ext_error = pte.n && (pte.ppn[3:0] != 4'b1000) || (pte.pbmt == 2'd3);
assign s1_permit_error = (ptw_req_mode == 2'd1 && !pte.r) ||
                    (req_is_store && !pte.w) || (req_is_load && !pte.r) || (req_is_rx && !pte.x);
assign s1_ens_error = (!ptw_priv_lvl && !pte.u) || (ptw_priv_lvl && pte.u && (!ptw_pc_sum || req_is_rx));

assign s2_permit_error = (ptw_req_mode == 2'd1 && !pte.r) ||
                    (s2_last_flag && req_is_store && !pte.w) || (s2_last_flag && req_is_load && !pte.r) || (s2_last_flag && req_is_rx && !pte.x);
assign ptw_iova_error = (acd_para[11:8] == 4'd8 && ptw_req_iova[26] && ptw_req_iova[51:27] != 25'h1ffffff) ||
                        (acd_para[11:8] == 4'd8 && !ptw_req_iova[26] && ptw_req_iova[51:27] != 25'h0000000) ||
                        (acd_para[11:8] == 4'd9 && ptw_req_iova[35] && ptw_req_iova[51:36] != 16'hffff) ||
                        (acd_para[11:8] == 4'd9 && !ptw_req_iova[35] && ptw_req_iova[51:36] != 16'h0000);

always@(*) begin
    if (!iommu_rstn)
        ptw_other_error = 1'b0;
    else if (ptw_rfifo_ren_d && msi_en && (ptw_req_mode == 2'd0) && msi_iova_match && !req_is_store && !cdw_is_error_d)
        ptw_other_error = 1'b1;
    //*****STORE_PAGE_FAULT or LOAD_PAGE_FAULT****//
    else if (ptw_rfifo_ren_d && (ptw_req_mode == 2'd0) && ptw_iova_error && !cdw_is_error_d)
        ptw_other_error = 1'b1;
    //*****MSI_PT_DATA_CORRUPTION*****//
    else if (ptw_rvalid_i && ptw_rlast_i && (ptw_rresp_i != RESP_OKAY))
        ptw_other_error = 1'b1;
    else if ((ptw_cs == S1_L3_PTE || ptw_cs == S1_L2_PTE || ptw_cs == S1_L1_PTE) &&
                                    ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) &&
                                    ((|pte.rsv && !pte.v || (!pte.r && pte.w) || pte.n) ||
                                    ((pte.r || pte.x) && s1_ppn_rsv_error) ||
                                    (!pte.r && !pte.x && (pte.a || pte.d || pte.u || |pte.pbmt))))
        ptw_other_error = 1'b1;
    else if (ptw_cs == S1_L0_PTE && ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) &&
                                    ((|pte.rsv || !pte.v || (!pte.r && pte.w) || (!pte.r && !pte.x && !pte.w)) ||
                                    ((pte.r || pte.x) && s1_ppn_rsv_error)))
        ptw_other_error = 1'b1;
    else if ((ptw_cs == S2_L3_PTE || ptw_cs == S2_L2_PTE || ptw_cs == S2_L1_PTE) &&
                                    ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) &&
                                    ((!pte.v || (!pte.r && pte.w) || |pte.rsv || pte.n) ||
                                    ((pte.r || pte.x) && s2_ppn_rsv_error) ||
                                    (!pte.r && !pte.x && (pte.a || pte.d || pte.u || |pte.pbmt))))
        ptw_other_error = 1'b1;
    else if (ptw_cs == S2_L0_PTE && ptw_rvalid_i && ptw_rlast_i && (iova_is_msi || gpa_is_msi) &&
                                    (!msi_pte.v || (msi_pte.m == 2'd0) || (msi_pte.m == 2'd2) ||
                                     |msi_pte.rsv_1 || |msi_pte.rsv_2 || |msi_pte.rsv_3 || |msi_pte.rsv_4 || msi_pte.c))
        ptw_other_error = 1'b1;
    else if (ptw_cs == S2_L0_PTE && ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && !iova_is_msi && !gpa_is_msi &&
                                    ((!pte.v || (!pte.r && pte.w) || (!pte.r && !pte.x && !pte.w) || |pte.rsv) ||
                                    ((pte.r || pte.x) && s2_ppn_rsv_error)))
        ptw_other_error = 1'b1;
    else
        ptw_other_error = 1'b0;
end


always@(*) begin
    if (!iommu_rstn)
        ptw_error_flag = 1'b0;
    else if ((ptw_rfifo_ren_d && ptw_is_bare) || (ptw_cs == PTW_END))
        ptw_error_flag = 1'b0;
    else if (ptw_other_error)
        ptw_error_flag = 1'b1;
    else if ((ptw_cs == S1_L3_PTE || ptw_cs == S1_L2_PTE || ptw_cs == S1_L1_PTE || ptw_cs == S1_L0_PTE) &&
                    ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) &&
                    (pte.r || pte.x) && (s1_permit_error || s1_ens_error))
        ptw_error_flag = 1'b1;
    else if ((ptw_cs == S2_L3_PTE || ptw_cs == S2_L2_PTE || ptw_cs == S2_L1_PTE || ptw_cs == S2_L0_PTE) &&
                    ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && !iova_is_msi && !gpa_is_msi &&
                    (pte.r || pte.x) && s2_last_flag && (s2_permit_error || !pte.u))
        ptw_error_flag = 1'b1;
    else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit) || (s2ptc_hit_valid && s2ptc_hit)) && ext_error)
        ptw_error_flag = 1'b1;
    else if (ptw_rvalid_i && ptw_rlast_i && (pte.r || pte.x) &&
            (((ptw_cs == S1_L3_PTE || ptw_cs == S2_L3_PTE) && |pte.ppn[26:0]) ||
            ((ptw_cs == S1_L2_PTE || ptw_cs == S2_L2_PTE) && |pte.ppn[17:0]) ||
            ((ptw_cs == S1_L1_PTE || ptw_cs == S2_L1_PTE) && |pte.ppn[8:0])))
        ptw_error_flag = 1'b1;
    else if ((ptw_cs == S1_L3_PTE || ptw_cs == S1_L2_PTE || ptw_cs == S1_L1_PTE || ptw_cs == S1_L0_PTE) &&
                    ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) &&
                    (pte.r || pte.x) && ((!ptw_req_sade && !pte.a) || (!ptw_req_sade && req_is_store && !pte.d)))
        ptw_error_flag = 1'b1;
    else if ((ptw_cs == S2_L3_PTE || ptw_cs == S2_L2_PTE || ptw_cs == S2_L1_PTE || ptw_cs == S2_L0_PTE) &&
                    ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && !iova_is_msi && !gpa_is_msi &&
                    (pte.r || pte.x) && ((!ptw_req_gade && !pte.a) ||
                    (s2_last_flag && !ptw_req_gade && req_is_store && !pte.d)))
        ptw_error_flag = 1'b1;
    else if (ptw_cs == ERROR)
        ptw_error_flag = 1'b1;
    else
        ptw_error_flag = 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_cause_code <= 12'd0;
    else if (ptw_rfifo_ren_d && msi_en && (ptw_req_mode == 2'd0) && msi_iova_match && !req_is_store && !cdw_is_error_d)
        ptw_cause_code <= INSTR_ACCESS_FAULT;
    else if (ptw_rfifo_ren_d && (ptw_req_mode == 2'd0) && ptw_iova_error && !cdw_is_error_d)
        if (req_is_rx)
            ptw_cause_code <= INSTR_PAGE_FAULT;
        else if (req_is_store)
            ptw_cause_code <= STORE_PAGE_FAULT;
        else
            ptw_cause_code <= LOAD_PAGE_FAULT;
    else if (ptw_rvalid_i && ptw_rlast_i && (iova_is_msi || gpa_is_msi) && (ptw_rresp_i != RESP_OKAY))
        ptw_cause_code <= MSI_PT_DATA_CORRUPTION;
    else if (ptw_rvalid_i && ptw_rlast_i && (ptw_rresp_i != RESP_OKAY))
        ptw_cause_code <= PT_DATA_CORRUPTION;
    else if ((ptw_cs == S2_L0_PTE) && (iova_is_msi || gpa_is_msi) && !msi_pte.v)
        ptw_cause_code <= MSI_PTE_INVALID;
    else if ((ptw_cs == S2_L0_PTE) && (iova_is_msi || gpa_is_msi) && ptw_error_flag)
        ptw_cause_code <= MSI_PTE_MISCONFIGURED;
    else if (ptw_error_flag && ((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE)))
        if (req_is_rx)
            ptw_cause_code <= INSTR_GUEST_PAGE_FAULT;
        else if (req_is_store)
            ptw_cause_code <= STORE_GUEST_PAGE_FAULT;
        else
            ptw_cause_code <= LOAD_GUEST_PAGE_FAULT;
    else if (ptw_error_flag && ((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE)))
        if (req_is_rx)
            ptw_cause_code <= INSTR_PAGE_FAULT;
        else if (req_is_store)
            ptw_cause_code <= STORE_PAGE_FAULT;
        else
            ptw_cause_code <= LOAD_PAGE_FAULT;
    else if ((ptw_cs == ERROR) && msi_gpa_match && !req_is_store)
        ptw_cause_code <= INSTR_ACCESS_FAULT;
    else if (ptw_cs == ERROR)
        ptw_cause_code <= ptw_cause_code;
    else
        ptw_cause_code <= 12'd0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_gpa_iotval2 <= 50'd0;
    else if (ptw_rvalid_i && ptw_rlast_i && (ptw_rresp_i != RESP_OKAY))
        ptw_gpa_iotval2 <= 50'd0;
    else if ((ptw_cs == S2_L0_PTE) && (iova_is_msi || gpa_is_msi) && ptw_error_flag)
        ptw_gpa_iotval2 <= 50'd0;
    else if (ptw_error_flag && ((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE)))
        ptw_gpa_iotval2 <= ptw_gpa_n;
    else
        ptw_gpa_iotval2 <= 50'd0;
end

assign guest_fq = (ptw_cause_code == INSTR_GUEST_PAGE_FAULT) || (ptw_cause_code == LOAD_GUEST_PAGE_FAULT) || (ptw_cause_code == STORE_GUEST_PAGE_FAULT);
assign iotval2_bit0 = guest_fq && (ptw_implicit_access || ptw_s1_en) && !s2_last_flag;
assign iotval2_bit1 = guest_fq && ptw_s1_en && ptw_req_sade && !s2_last_flag;

assign ptw_fq_valid_o = ptw_fq_valid;
//64_3 = 14+50-2+1+1
//64_2 = iova[51:0]+12
//64_1 = rsv
//64_0 = 24+6+1+1+20+12
assign ptw_fq_record_o = {14'd0,ptw_gpa_iotval2[49:2],iotval2_bit1,iotval2_bit0,ptw_req_iova,12'd0,64'd0,ptw_req_did,ptw_trans_type,ptw_priv_lvl,ptw_req_pv,ptw_req_pid,ptw_cause_code};

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_fq_valid <= 1'b0;
    else if ((ptw_cs == ERROR) && !ptw_error_flag_d && msi_gpa_match && !req_is_store && ptw_fq_ready_i)
        ptw_fq_valid <= !acd_para[2];
    else if ((ptw_req_mode == 2'd0) && (ptw_cs == ERROR))
        ptw_fq_valid <= 1'b0;
    else if ((ptw_req_mode == 2'd0) && ptw_error_flag && ptw_fq_ready_i)
        ptw_fq_valid <= !acd_para[2];
    else
        ptw_fq_valid <= 1'b0;
end

// HPM event indicators
assign ptw_s1_walk_o = ptw_s1_walk;
assign ptw_s2_walk_o = ptw_s2_walk;
assign ptw_gscid_o = ptw_gscid;
assign ptw_pscid_o = ptw_pscid;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_s1_walk_a <= 1'b0;
    else if (ptw_error_flag)
        ptw_s1_walk_a <= 1'b0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i)
        ptw_s1_walk_a <= ptw_req_sade && !pte.a;
    else
        ptw_s1_walk_a <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_s2_walk_a <= 1'b0;
    else if (ptw_error_flag)
        ptw_s2_walk_a <= 1'b0;
    else if ((ptw_cs == S2_L0_PTE) && ptw_rvalid_i && ptw_rlast_i && (iova_is_msi || gpa_is_msi))
        ptw_s2_walk_a <= 1'b0;
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i)
        ptw_s2_walk_a <= ptw_req_gade && !pte.a;
    else
        ptw_s2_walk_a <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_s1_walk_d <= 1'b0;
    else if (ptw_error_flag)
        ptw_s1_walk_d <= 1'b0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i)
        ptw_s1_walk_d <= ptw_req_sade && !pte.d && pte.w && req_is_store;
    else
        ptw_s1_walk_d <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_s2_walk_d <= 1'b0;
    else if (ptw_error_flag)
        ptw_s2_walk_d <= 1'b0;
    else if ((ptw_cs == S2_L0_PTE) && ptw_rvalid_i && ptw_rlast_i && (iova_is_msi || gpa_is_msi))
        ptw_s2_walk_d <= 1'b0;
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE))
                    && (pte.r || pte.x) && s2_last_flag && ptw_rvalid_i && ptw_rlast_i)
        ptw_s2_walk_d <= ptw_req_gade && !pte.d && pte.w && req_is_store;
    else
        ptw_s2_walk_d <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_s1_walk <= 1'b0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE)) && ptw_rvalid_i && ptw_rlast_i)
        ptw_s1_walk <= 1'b1;
    else if ((ptw_cs == B_RESP) && ptw_bvalid_i && (ptw_bid_i == UP_D1_ID))
        ptw_s1_walk <= 1'b1;
    else
    	ptw_s1_walk <= ptw_s1_walk_a || ptw_s1_walk_d;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_s2_walk <= 1'b0;
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE)) && !iova_is_msi && !gpa_is_msi && ptw_rvalid_i && ptw_rlast_i)
        ptw_s2_walk <= 1'b1;
    else if ((ptw_cs == B_RESP) && ptw_bvalid_i && (ptw_bid_i == UP_D2_ID))
        ptw_s2_walk <= 1'b1;
    else
        ptw_s2_walk <= ptw_s2_walk_a || ptw_s2_walk_d;
end


////////**********************************************************************************************************///////
//gen ptw output signal to atd_tc module
////////**********************************************************************************************************///////
assign ptw_wfifo_wen_o = ptw_wfifo_wen;
assign ptw_wfifo_wdata_o = ptw_wfifo_din;
assign svnapot_en_s1 = ptw_s1_en && !ptw_s2_en && s1_perm[8];
assign svnapot_en_s2 = !ptw_s1_en && ptw_s2_en && s2_perm[8];
assign svnapot_en_s12 = ptw_s1_en && s1_perm[8] && ptw_s2_en && s2_perm[8];
assign svnapot_en = svnapot_en_s1 || svnapot_en_s2 || svnapot_en_s12;
assign mrif_en = msi_pte.m == 2'd1;
assign mrif_nppn = mrif_en ? msi_pte.nppn :  44'd0;

//18bit
assign acd_mix_para1 = {s2_size,s1_size,op_code,ptw_req_idx,4'd2};
//20bit
assign acd_mix_para2 = {svnapot_en,s2_perm[4:0],1'b0,s1_perm[4:0],s1_perm[7:6],s2_perm[7:6],s1_perm[5],s2_perm[5],ptw_req_sade,ptw_req_gade};
//320bit
assign msi_pte_nid = mrif_en ? {msi_pte.nid2,msi_pte.nid1} : 11'd0;
assign ptw_wfifo_din = {64'd0,mrif_nppn,8'd0,mrif_en,msi_pte_nid,ptw_spaddr,acd_mix_para2,ptw_gscid,ptw_pscid,acd_para,8'd0,ptw_gpa_q[RISCV_GLEN-1:12],acd_mix_para1};

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cdw_is_error_d <= 1'b0;
    else
        cdw_is_error_d <= cdw_is_error;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        op_code <= 2'd0;
    else if ((ptw_cs == PTW_END) && cdw_is_error_d)
        op_code <= 2'd2;
    else if ((ptw_rfifo_ren_d && ptw_is_bare) || (ptw_cs == PTW_END))
        op_code <= 2'd0;
    else if (ptw_other_error)
        op_code <= 2'd2;
    else if ((ptw_cs == S1_L3_PTE || ptw_cs == S1_L2_PTE || ptw_cs == S1_L1_PTE || ptw_cs == S1_L0_PTE) &&
                    ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) &&
                    (pte.r || pte.x) && (s1_permit_error || s1_ens_error))
        op_code <= 2'd1;
    else if ((ptw_cs == S2_L3_PTE || ptw_cs == S2_L2_PTE || ptw_cs == S2_L1_PTE || ptw_cs == S2_L0_PTE) &&
                    ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && !iova_is_msi && !gpa_is_msi &&
                    (pte.r || pte.x) && s2_last_flag && (s2_permit_error || !pte.u))
        op_code <= 2'd1;
    else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit) || (s2ptc_hit_valid && s2ptc_hit)) && ext_error)
        op_code <= 2'd2;
    else if ((ptw_rvalid_i && ptw_rlast_i) && (pte.r || pte.x) &&
            (((ptw_cs == S1_L3_PTE || ptw_cs == S2_L3_PTE) && |pte.ppn[26:0]) ||
            ((ptw_cs == S1_L2_PTE || ptw_cs == S2_L2_PTE) && |pte.ppn[17:0]) ||
            ((ptw_cs == S1_L1_PTE || ptw_cs == S2_L1_PTE) && |pte.ppn[8:0])))
        op_code <= 2'd2;
    else if ((ptw_cs == S1_L3_PTE || ptw_cs == S1_L2_PTE || ptw_cs == S1_L1_PTE || ptw_cs == S1_L0_PTE) &&
                    ((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) &&
                    (pte.r || pte.x) && ((!ptw_req_sade && !pte.a) || (!ptw_req_sade && req_is_store && !pte.d)))
        op_code <= 2'd2;
    else if ((ptw_cs == S2_L3_PTE || ptw_cs == S2_L2_PTE || ptw_cs == S2_L1_PTE || ptw_cs == S2_L0_PTE) &&
                    ((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && !iova_is_msi && !gpa_is_msi &&
                    (pte.r || pte.x) && ((!ptw_req_gade && !pte.a) ||
                    (s2_last_flag && !ptw_req_gade && req_is_store && !pte.d)))
            op_code <= 2'd2;
    else if ((ptw_cs == ERROR) && msi_gpa_match && !req_is_store)
        op_code <= 2'd2;
    else
        op_code <= op_code;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_spaddr <= 52'd0;
    else if (ptw_rfifo_ren)
        ptw_spaddr <= 52'd0;
    else if (ptw_rfifo_ren_d && ptw_is_bare)
        ptw_spaddr <= ptw_req_iova;
    else if (ptw_rvalid_i && ptw_rlast_i && (iova_is_msi || gpa_is_msi))
        if (msi_pte.m == 2'd3)
            ptw_spaddr <= {8'd0,msi_pte.ppn[46:3]};
        else if (msi_pte.m == 2'd1)
            ptw_spaddr <= {5'd0,msi_pte.ppn};
        else
            ptw_spaddr <= 52'd0;
    else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && (pte.r || pte.x) && !ptw_s2_en)
        if (ptw_cs == S1_L3_PTE)
            ptw_spaddr <= {8'd0,pte.ppn[43:27],ptw_req_iova[26:0]};
        else if (ptw_cs == S1_L2_PTE)
            ptw_spaddr <= {8'd0,pte.ppn[43:18],ptw_req_iova[17:0]};
        else if (ptw_cs == S1_L1_PTE)
            ptw_spaddr <= {8'd0,pte.ppn[43:9],ptw_req_iova[8:0]};
        else if (ptw_cs == S1_L0_PTE)
            ptw_spaddr <= {8'd0,pte.ppn};
        else if (ptw_cs == PTW_END)
            ptw_spaddr <= {8'd0,pte.ppn};
        else
            ptw_spaddr <= ptw_spaddr;
    else if (((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && (pte.r || pte.x) && ptw_s2_en)
        if (ptw_cs == S2_L3_PTE)
            ptw_spaddr <= {8'd0,pte.ppn[43:27],ptw_gpa_q[38:12]};
        else if (ptw_cs == S2_L2_PTE)
            ptw_spaddr <= {8'd0,pte.ppn[43:18],ptw_gpa_q[29:12]};
        else if (ptw_cs == S2_L1_PTE)
            ptw_spaddr <= {8'd0,pte.ppn[43:9],ptw_gpa_q[20:12]};
        else if ((ptw_cs == S2_L0_PTE) && !iova_is_msi && !gpa_is_msi)
            ptw_spaddr <= {8'd0,pte.ppn};
        else
            ptw_spaddr <= ptw_spaddr;
    else
        ptw_spaddr <= ptw_spaddr;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        s1_size <= 2'd0;
    else if (ptw_rfifo_ren)
        s1_size <= 2'd0;
    else if (((ptw_rvalid_i && ptw_rlast_i) || (s1ptc_hit_valid && s1ptc_hit)) && (pte.r || pte.x) && !ptw_s2_en)
        if (ptw_cs == S1_L3_PTE)
            s1_size <= 2'd3;
        else if (ptw_cs == S1_L2_PTE)
            s1_size <= 2'd2;
        else if (ptw_cs == S1_L1_PTE)
            s1_size <= 2'd1;
        else if (ptw_cs == S1_L0_PTE)
            s1_size <= 2'd0;
        else
            s1_size <= s1_size;
    else
        s1_size <= s1_size;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        s2_size <= 2'd0;
    else if (ptw_rfifo_ren_d)
        s2_size <= 2'd0;
    else if (((ptw_rvalid_i && ptw_rlast_i) || (s2ptc_hit_valid && s2ptc_hit)) && (pte.r || pte.x) && ptw_s2_en)
        if (ptw_cs == S2_L3_PTE)
            s2_size <= 2'd3;
        else if (ptw_cs == S2_L2_PTE)
            s2_size <= 2'd2;
        else if (ptw_cs == S2_L1_PTE)
            s2_size <= 2'd1;
        else if (ptw_cs == S2_L0_PTE)
            s2_size <= 2'd0;
        else
            s2_size <= s2_size;
    else
        s2_size <= s2_size;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_error_flag_d <= 1'b0;
    else
        ptw_error_flag_d <= ptw_error_flag;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_wfifo_wen <= 1'b0;
    else if ((ptw_req_mode == 2'd0) && !ptw_error_flag_d && ptw_error_flag)
        ptw_wfifo_wen <= 1'b1;
    else if ((ptw_req_mode == 2'd0) && (ptw_cs == PTW_END) && !ptw_wfifo_full_i)
        ptw_wfifo_wen <= 1'b1;
    else
        ptw_wfifo_wen <= 1'b0;
end

//the lastest pte is leaf-pte
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        s1_perm <= 9'd0;
    else if (ptw_rfifo_ren)
        s1_perm <= 9'd0;
    else if (s1ptc_hit_valid && s1ptc_hit)
        s1_perm <= {s1ptc_pte[63:61],s1ptc_pte[7],s1ptc_pte[5:1]};
    else if (((ptw_cs == S1_L0_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L3_PTE)) && ptw_rvalid_i && ptw_rlast_i)
        case (ptw_iova_sel)
            3'd0:
                s1_perm <= {ptw_pte[64*0+63:64*0+61],ptw_pte[64*0+7],ptw_pte[64*0+5:64*0+1]};
            3'd1:
                s1_perm <= {ptw_pte[64*1+63:64*1+61],ptw_pte[64*1+7],ptw_pte[64*1+5:64*1+1]};
            3'd2:
                s1_perm <= {ptw_pte[64*2+63:64*2+61],ptw_pte[64*2+7],ptw_pte[64*2+5:64*2+1]};
            3'd3:
                s1_perm <= {ptw_pte[64*3+63:64*3+61],ptw_pte[64*3+7],ptw_pte[64*3+5:64*3+1]};
            3'd4:
                s1_perm <= {ptw_pte[64*4+63:64*4+61],ptw_pte[64*4+7],ptw_pte[64*4+5:64*4+1]};
            3'd5:
                s1_perm <= {ptw_pte[64*5+63:64*5+61],ptw_pte[64*5+7],ptw_pte[64*5+5:64*5+1]};
            3'd6:
                s1_perm <= {ptw_pte[64*6+63:64*6+61],ptw_pte[64*6+7],ptw_pte[64*6+5:64*6+1]};
            3'd7:
                s1_perm <= {ptw_pte[64*7+63:64*7+61],ptw_pte[64*7+7],ptw_pte[64*7+5:64*7+1]};
            default:
                s1_perm <= 9'd0;
        endcase
end

//the lastest pte is leaf-pte
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        s2_perm <= 9'd0;
    else if (ptw_rfifo_ren)
        s2_perm <= 9'd0;
    else if ((ptw_cs == S2_L0_PTE) && ptw_rvalid_i && ptw_rlast_i && (iova_is_msi || gpa_is_msi))
        s2_perm <= 9'd0;
    else if (s2ptc_hit_valid && s2ptc_hit)
        s2_perm <= {s2ptc_pte[63:61],s2ptc_pte[7],s2ptc_pte[5:1]};
    else if (((ptw_cs == S2_L0_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L3_PTE)) && ptw_rvalid_i && ptw_rlast_i)
        case (ptw_gppn_sel)
            3'd0:
                s2_perm <= {ptw_pte[64*0+63:64*0+61],ptw_pte[64*0+7],ptw_pte[64*0+5:64*0+1]};
            3'd1:
                s2_perm <= {ptw_pte[64*1+63:64*1+61],ptw_pte[64*1+7],ptw_pte[64*1+5:64*1+1]};
            3'd2:
                s2_perm <= {ptw_pte[64*2+63:64*2+61],ptw_pte[64*2+7],ptw_pte[64*2+5:64*2+1]};
            3'd3:
                s2_perm <= {ptw_pte[64*3+63:64*3+61],ptw_pte[64*3+7],ptw_pte[64*3+5:64*3+1]};
            3'd4:
                s2_perm <= {ptw_pte[64*4+63:64*4+61],ptw_pte[64*4+7],ptw_pte[64*4+5:64*4+1]};
            3'd5:
                s2_perm <= {ptw_pte[64*5+63:64*5+61],ptw_pte[64*5+7],ptw_pte[64*5+5:64*5+1]};
            3'd6:
                s2_perm <= {ptw_pte[64*6+63:64*6+61],ptw_pte[64*6+7],ptw_pte[64*6+5:64*6+1]};
            3'd7:
                s2_perm <= {ptw_pte[64*7+63:64*7+61],ptw_pte[64*7+7],ptw_pte[64*7+5:64*7+1]};
            default:
                s2_perm <= 9'd0;
        endcase
end

//get msi_pte
always@(*) begin
    if (ptw_rfifo_ren)
        msi_pte = 128'd0;
    else if ((ptw_cs == S2_L0_PTE) && (iova_is_msi || gpa_is_msi) && ptw_rvalid_i && ptw_rlast_i)
        case (ptw_pptr_q[5:4])
            2'd0:
                msi_pte = msi_pte_t'(ptw_pte[128*1-1:0]);
            2'd1:
                msi_pte = msi_pte_t'(ptw_pte[128*2-1:128*1]);
            2'd2:
                msi_pte = msi_pte_t'(ptw_pte[128*3-1:128*2]);
            2'd3:
                msi_pte = msi_pte_t'(ptw_pte[128*4-1:128*3]);
            default:
                msi_pte = 128'd0;
        endcase
    else
        msi_pte = msi_pte_q;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        msi_pte_q <= 128'd0;
    else
        msi_pte_q <= msi_pte;
end

////////**********************************************************************************************************///////
//gen ats output signal to atd_t2r module
////////**********************************************************************************************************///////
assign ats_resp_wen_o = ats_resp_wen;
assign ats_resp_wdata_o = ats_resp_wdata;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_resp_wen <= 1'b0;
    else if ((ptw_req_mode == 2'd1) && !ptw_error_flag_d && ptw_error_flag)
        ats_resp_wen <= 1'b1;
    else if ((ptw_req_mode == 2'd1) && (ptw_cs == PTW_END) && !ats_resp_full_i)
        ats_resp_wen <= 1'b1;
    else
        ats_resp_wen <= 1'b0;
end


always@(*) begin
    if (!iommu_rstn)
        allow_x = 1'b0;
    else if (ptw_rfifo_ren)
        allow_x = 1'b0;
    else if ((ptw_req_mode == 2'd1) && ptw_en_ats && ptw_s1_en && !ptw_s2_en)
        allow_x = s1_perm[2] && s1_perm[0] && ats_req_InD;
    else if ((ptw_req_mode == 2'd1) && ptw_en_ats && ptw_s2_en)
        allow_x = s2_perm[2] && s2_perm[0] && ats_req_InD;
    else
        allow_x = 1'b0;
end

always@(*) begin
    if (!iommu_rstn)
        allow_w = 1'b0;
    else if (ptw_rfifo_ren)
        allow_w = 1'b0;
    else if ((ptw_req_mode == 2'd1) && ptw_en_ats && ptw_s1_en && !ptw_s2_en)
        allow_w = s1_perm[1];
    else if ((ptw_req_mode == 2'd1) && ptw_en_ats && ptw_s2_en)
        allow_w = s2_perm[1];
    else
        allow_w = 1'b0;
end

always@(*) begin
    if (!iommu_rstn)
        allow_r = 1'b0;
    else if (ptw_rfifo_ren)
        allow_r = 1'b0;
    else if ((ptw_req_mode == 2'd1) && ptw_en_ats && ptw_s1_en && !ptw_s2_en)
        allow_r = !s1_perm[1] || s1_perm[0];
    else if ((ptw_req_mode == 2'd1) && ptw_en_ats && ptw_s2_en)
        allow_r = !s2_perm[1] || s2_perm[0];
    else
        allow_r = 1'b0;
end

assign ama = 3'd0;
assign trans_rng = 4'd0;
assign bypass = ptw_is_bare;
assign untranslated = (ptw_req_mode == 2'd1) && (op_code == 2'd0);
//114=160-46rsv
assign ats_resp_wdata = {ptw_spaddr,13'd0,ama,8'd0,trans_rng,13'd0,allow_x,allow_w,allow_r,bypass,4'b0,untranslated,ptw_req_idx,4'd2};
////////**********************************************************************************************************///////
//gen ats fault output signal to atd_t2r module
////////**********************************************************************************************************///////
assign ats_fault_valid_o = ats_fault_valid;
assign ats_fault_record_o = ats_fault_record;
//double check?
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_fault_valid <= 1'b0;
    else if (ptw_rfifo_ren)
        ats_fault_valid <= 1'b0;
    else if ((ptw_req_mode == 2'd1) && (ptw_cs == ERROR))
        ats_fault_valid <= 1'b1;
    else if ((ptw_req_mode == 2'd1) && ptw_error_flag && ats_fault_ready_i)
        ats_fault_valid <= 1'b1;
    else
        ats_fault_valid <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ats_fault_type <= 2'd0;
    else if (ptw_rfifo_ren)
        ats_fault_type <= 2'd0;
    else if ((ptw_req_mode == 2'd1) && !ptw_en_ats)
        ats_fault_type <= 2'd2;
    else if ((ptw_req_mode == 2'd1) && ptw_error_flag)
        ats_fault_type <= 2'd1;
end


assign ats_fault_record = {2'd0,ats_fault_type,ptw_req_idx,4'd2};

////////**********************************************************************************************************///////
//amo update A/D bit to memory
////////**********************************************************************************************************///////
assign up_ad_wen_o = up_a_en && !up_d_en_q;
assign up_ad_wdata_o = up_ad_pte;
assign up_ad_waddr_o = up_ad_addr;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_a_en <= 1'b0;
    else if (ptw_error_flag)
        up_a_en <= 1'b0;
    else if ((ptw_cs == S2_L0_PTE) && ptw_rvalid_i && ptw_rlast_i && (iova_is_msi || gpa_is_msi))
        up_a_en <= 1'b0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i)
        up_a_en <= ptw_req_sade && !pte.a;
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i)
        up_a_en <= ptw_req_gade && !pte.a;
    else
        up_a_en <= 1'b0;
end


always@(*) begin
    if (!iommu_rstn)
        up_d_en = 1'b0;
    else if (ptw_error_flag)
        up_d_en = 1'b0;
    else if ((ptw_cs == S2_L0_PTE) && ptw_rvalid_i && ptw_rlast_i && (iova_is_msi || gpa_is_msi))
        up_d_en = 1'b0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i)
        up_d_en = ptw_req_sade && !pte.d && pte.w && req_is_store;
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE))
                    && (pte.r || pte.x) && s2_last_flag && ptw_rvalid_i && ptw_rlast_i)
        up_d_en = ptw_req_gade && !pte.d && pte.w && req_is_store;
    else
        up_d_en = 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_d_en_q <= 1'b0;
    else
        up_d_en_q <= up_d_en;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_ad_pte <= 8'd0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i && ptw_req_sade && !pte.d && pte.w && req_is_store)
        up_ad_pte <= {1'b1,1'b1,6'd0};//update a and d bit
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE))
                    && (pte.r || pte.x) && s2_last_flag && ptw_rvalid_i && ptw_rlast_i && ptw_req_gade && !pte.d && pte.w && req_is_store)
        up_ad_pte <= {1'b1,1'b1,6'd0};//update a and d bit
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i && ptw_req_sade && !pte.a)
        up_ad_pte <= {1'b0,1'b1,6'd0};//update a bit
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i && ptw_req_gade && !pte.a)
        up_ad_pte <= {1'b0,1'b1,6'd0};//update a bit
    else
        up_ad_pte <= up_ad_pte;
end

//byte addr
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        up_ad_addr <= 53'd0;
    else if (((ptw_cs == S1_L3_PTE) || (ptw_cs == S1_L2_PTE) || (ptw_cs == S1_L1_PTE) || (ptw_cs == S1_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i && ptw_req_sade)
        up_ad_addr <= ptw_pptr_q[RISCV_PLEN-1:3];//a or d bit addr
    else if (((ptw_cs == S2_L3_PTE) || (ptw_cs == S2_L2_PTE) || (ptw_cs == S2_L1_PTE) || (ptw_cs == S2_L0_PTE))
                    && (pte.r || pte.x) && ptw_rvalid_i && ptw_rlast_i && ptw_req_gade)
        up_ad_addr <= ptw_pptr_q[RISCV_PLEN-1:3];//a or d bit addr
    else
        up_ad_addr <= up_ad_addr;
end

// AXI parameters
// AW
/* verilator lint_off WIDTH */
assign ptw_awid_o   = ptw_awid;
assign ptw_awaddr_o = {up_ad_addr,3'd0};
assign ptw_awlen_o  = 8'd0;         // MSI writes 32-bytes wide/only one transfer
assign ptw_awvalid_o = (ptw_cs == AW_REQ_S1) || (ptw_cs == AW_REQ);
// W
assign ptw_wdata_o  = ptw_wdata;
assign ptw_wstrb_o  = ptw_wstrb;
assign ptw_wlast_o = (ptw_cs == W_DATA);

// Send data through W channel
assign ptw_wvalid_o = (ptw_cs == W_DATA);

// B
assign ptw_bready_o = ((ptw_cs == B_RESP) && ptw_bvalid_i);

always@(*) begin
    if (!iommu_rstn)
        ptw_awid = 4'h0;
    else if (ptw_cs == AW_REQ_S1)
        ptw_awid = UP_D1_ID;
    else if (ptw_cs == AW_REQ)
        ptw_awid = UP_D2_ID;
    else
        ptw_awid = ptw_awid_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_awid_q <= 4'h0;
    else
        ptw_awid_q <= ptw_awid;
end


always@(*) begin
    if (!iommu_rstn)
        ptw_wdata = 256'd0;
    else if (ptw_cs == W_DATA)
        case (up_ad_addr[1:0])
            2'd0:
                ptw_wdata = {192'd0,56'd0,up_ad_pte};
            2'd1:
                ptw_wdata = {128'd0,56'd0,up_ad_pte,64'd0};
            2'd2:
                ptw_wdata = {64'd0,56'd0,up_ad_pte,128'd0};
            2'd3:
                ptw_wdata = {56'd0,up_ad_pte,192'd0};
            default: ptw_wdata = ptw_wdata_q;
        endcase
    else
        ptw_wdata = ptw_wdata_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_wdata_q  <= 256'd0;
    else
        ptw_wdata_q <= ptw_wdata;
end


always@(*) begin
    if (!iommu_rstn)
        ptw_wstrb = 32'd0;
    else if (ptw_cs == W_DATA)
        case (up_ad_addr[1:0])
            2'd0:
                ptw_wstrb = {24'd0,7'd0,1'b1};
            2'd1:
                ptw_wstrb = {16'd0,7'd0,1'b1,8'd0};
            2'd2:
                ptw_wstrb = {8'd0,7'd0,1'b1,16'd0};
            2'd3:
                ptw_wstrb = {7'd0,1'b1,24'd0};
            default: ptw_wstrb = ptw_wstrb_q;
        endcase
    else
        ptw_wstrb = ptw_wstrb_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        ptw_wstrb_q  <= 32'd0;
    else
        ptw_wstrb_q <= ptw_wstrb;
end

endmodule
