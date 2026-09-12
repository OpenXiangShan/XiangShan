///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_tw.sv
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

module iommu_atd_tw #(
    parameter  RISCV_GLEN                      = 6'd50,//sv48
    parameter  RISCV_PLEN                      = 6'd56,
    parameter  INV_IDX_WIDTH                   ,
    parameter  DDTC_INV_IDX_WIDTH              ,
    parameter  DDTC_TLB_QIDX_WIDTH             ,
    parameter  DDTC_MICRO_TLB_IDX_WIDTH        ,
    parameter  DDTC_CABIN_LKP_IDX_WIDTH        ,
    parameter  DDTC_CABIN_UPD_IDX_WIDTH        ,
    parameter  DDTC_CABIN_INV_IDX_WIDTH        ,
    parameter  DDTC_BANK_L0_IDX_WIDTH          ,
    parameter  DDTC_BANK_L1_IDX_WIDTH          ,
    parameter  DDTC_BANK_L2_IDX_WIDTH          ,
    parameter  DDTC_BANK_L0_SET_IDX_WIDTH      ,
    parameter  DDTC_BANK_L1_SET_IDX_WIDTH      ,
    parameter  DDTC_BANK_L2_SET_IDX_WIDTH      ,
    parameter  DDTC_BANK_L0_WAY_IDX_WIDTH      ,
    parameter  DDTC_BANK_L1_WAY_IDX_WIDTH      ,
    parameter  DDTC_BANK_L2_WAY_IDX_WIDTH      ,
    parameter  PDTC_TLB_QIDX_WIDTH             ,
    parameter  PDTC_MICRO_TLB_IDX_WIDTH        ,
    parameter  PDTC_CABIN_LKP_IDX_WIDTH        ,
    parameter  PDTC_CABIN_UPD_IDX_WIDTH        ,
    parameter  PDTC_CABIN_INV_IDX_WIDTH        ,
    parameter  PDTC_BANK_L0_IDX_WIDTH          ,
    parameter  PDTC_BANK_L1_IDX_WIDTH          ,
    parameter  PDTC_BANK_L2_IDX_WIDTH          ,
    parameter  PDTC_BANK_L0_SET_IDX_WIDTH      ,
    parameter  PDTC_BANK_L1_SET_IDX_WIDTH      ,
    parameter  PDTC_BANK_L2_SET_IDX_WIDTH      ,
    parameter  PDTC_BANK_L0_WAY_IDX_WIDTH      ,
    parameter  PDTC_BANK_L1_WAY_IDX_WIDTH      ,
    parameter  PDTC_BANK_L2_WAY_IDX_WIDTH      ,
    parameter  S1PTC_TLB_QIDX_WIDTH            ,
    parameter  S1PTC_MICRO_TLB_IDX_WIDTH       ,
    parameter  S1PTC_CABIN_LKP_IDX_WIDTH       ,
    parameter  S1PTC_CABIN_UPD_IDX_WIDTH       ,
    parameter  S1PTC_CABIN_INV_IDX_WIDTH       ,
    parameter  S1PTC_BANK_L0_IDX_WIDTH         ,
    parameter  S1PTC_BANK_L1_IDX_WIDTH         ,
    parameter  S1PTC_BANK_L2_IDX_WIDTH         ,
    parameter  S1PTC_BANK_L3_IDX_WIDTH         ,
    parameter  S1PTC_BANK_L4_IDX_WIDTH         ,
    parameter  S1PTC_BANK_L0_SET_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L1_SET_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L2_SET_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L3_SET_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L4_SET_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L0_WAY_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L1_WAY_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L2_WAY_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L3_WAY_IDX_WIDTH     ,
    parameter  S1PTC_BANK_L4_WAY_IDX_WIDTH     ,
    parameter  S2PTC_TLB_QIDX_WIDTH            ,
    parameter  S2PTC_MICRO_TLB_IDX_WIDTH       ,
    parameter  S2PTC_CABIN_LKP_IDX_WIDTH       ,
    parameter  S2PTC_CABIN_UPD_IDX_WIDTH       ,
    parameter  S2PTC_CABIN_INV_IDX_WIDTH       ,
    parameter  S2PTC_BANK_L0_IDX_WIDTH         ,
    parameter  S2PTC_BANK_L1_IDX_WIDTH         ,
    parameter  S2PTC_BANK_L2_IDX_WIDTH         ,
    parameter  S2PTC_BANK_L3_IDX_WIDTH         ,
    parameter  S2PTC_BANK_L4_IDX_WIDTH         ,
    parameter  S2PTC_BANK_L0_SET_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L1_SET_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L2_SET_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L3_SET_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L4_SET_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L0_WAY_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L1_WAY_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L2_WAY_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L3_WAY_IDX_WIDTH     ,
    parameter  S2PTC_BANK_L4_WAY_IDX_WIDTH
) (
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

//    output                          atd_ipsr_pmip_o,
    output  [30:0]                  atd_hpm_rvalid_o,
    output  [31:0]                  atd_iocntovf_o,
    output  [63:0]                  atd_hpmctr_o[30:0],
    output  [63:0]                  atd_hpmevt_o[30:0],
    output                          atd_pmip_clr_o,
    //between atd_tw and atd_tc
    input                           cdw_rfifo_wen_i,
    input   [127:0]                 cdw_rfifo_wdata_i,
    output                          cdw_rfifo_full_o,

    input                           ats_req_wen_i,
    input   [191:0]                 ats_req_wdata_i,
    output                          ats_req_full_o,

    input                           pri_req_wen_i,
    input   [127:0]                 pri_req_wdata_i,
    output                          pri_req_full_o,

    output                          ptw_wfifo_wen_o,
    output  [319:0]                 ptw_wfifo_wdata_o,
    input                           ptw_wfifo_full_i,

    //flush/invalidation: cq -> atd-cache
    input                           cq_wfifo_wen_i,
    input   [127:0]                 cq_wfifo_wdata_i,
    output                          cq_wfifo_full_o,
    input                           cq_fence_valid_i,
    output                          cq_fence_ready_o,

    //to FQ
    output                          cdw_fq_valid_o,
    output  [255:0]                 cdw_fq_record_o,
    input                           cdw_fq_ready_i,
    output                          ptw_fq_valid_o,
    output  [255:0]                 ptw_fq_record_o,
    input                           ptw_fq_ready_i,

    //output to t2r and then to pcie-rc
    output                          pri_resp_wen_o,
    output  [79:0]                  pri_resp_wdata_o,
    input                           pri_resp_full_i,

    //to pq
    output                          pq_pri_wen_o,
    output  [127:0]                 pq_pri_wdata_o,
    input                           pq_pri_full_i,

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
    //to msi
    output                          up_ad_wen_o,
    output  [7:0]                   up_ad_wdata_o,
    output  [52:0]                  up_ad_waddr_o,

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
    input   [255:0]                 cdw_rdata_i,

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

    logic   [43:0]                  ddtp_ppn;
    logic   [3:0]                   ddtp_mode;
    logic   [63:0]                  iommu_caps;
    logic   [11:0]                  iommu_fctl;

    //between cdw and HPM
    logic                           ddt_walk;
    logic                           pdt_walk;
    logic   [23:0]                  cdw_did;
    logic                           cdw_pv;
    logic   [19:0]                  cdw_pid;
    logic                           ptw_s1_walk;
    logic                           ptw_s2_walk;
    logic   [15:0]                  ptw_gscid;
    logic   [19:0]                  ptw_pscid;
    logic   [30:0]                  atd_iocntinh;
    logic   [63:0]                  atd_hpmevt[30:0];
    logic   [30:0]                  atd_hpm_rvalid;
    logic                           atd_hpmcnt_cy;
    logic   [30:0]                  atd_iocntovf;
    logic   [63:0]                  atd_hpmctr[30:0];
    logic   [63:0]                  atd_hpmctr_r[30:0];
    logic                           atd_pmip_clr;

//between ptw and cdw
    logic                           cdw_flush;
    logic                           cdw_implicit_done;
    logic   [43:0]                  cdw_implicit_ppn;

//between cdw and ptw
    logic                           ptw_rfifo_wen;
    logic   [464:0]                 ptw_rfifo_din;
    logic                           ptw_rfifo_full;

    logic                           lookup_dc_valid;
    logic                           lookup_dc_ready;
    logic  [23:0]                   lookup_dc_did;
    logic                           ddtc_hit_valid;
    logic                           ddtc_hit;//from ddtc
    logic  [1:0]                    ddtc_hit_lvl;
    logic  [511:0]                  ddtc_hit_content;
    logic                           up_dc_valid;
    logic                           up_dc_ready;
    logic                           up_dc_leaf;
    logic  [1:0]                    up_dc_lvl;
    logic  [23:0]                   up_dc_did;
    logic  [511:0]                  up_dc_content;

    logic                           lookup_pc_valid;
    logic                           lookup_pc_ready;
    logic  [19:0]                   lookup_pc_pid;
    logic  [23:0]                   lookup_pc_did;
    logic                           pdtc_hit_valid;
    logic                           pdtc_hit;
    logic  [1:0]                    pdtc_hit_lvl;
    logic  [127:0]                  pdtc_hit_content;
    logic                           up_pc_valid;
    logic                           up_pc_ready;
    logic                           up_pc_leaf;
    logic  [1:0]                    up_pc_lvl;
    logic  [19:0]                   up_pc_pid;
    logic  [23:0]                   up_pc_did;
    logic  [127:0]                  up_pc_content;

    //between ptw and atd-cache
    logic                           lookup_s1ptc_valid;
    logic                           lookup_s1ptc_ready;
    logic  [2:0]                    lookup_s1ptc_lvl;
    logic                           lookup_s1ptc_leaf;
    logic                           lookup_s1ptc_gv;
    logic  [51:0]                   lookup_s1ptc_va;
    logic  [19:0]                   lookup_s1ptc_pscid;
    logic  [15:0]                   lookup_s1ptc_gscid;
    logic                           s1ptc_hit_valid;
    logic                           s1ptc_hit;//from ddtc
    logic  [2:0]                    s1ptc_hit_lvl;
    logic  [511:0]                  s1ptc_hit_content;

    logic                           lookup_s2ptc_valid;
    logic                           lookup_s2ptc_ready;
    logic  [2:0]                    lookup_s2ptc_lvl;
    logic                           lookup_s2ptc_leaf;
    logic  [15:0]                   lookup_s2ptc_gscid;
    logic  [51:0]                   lookup_s2ptc_gpa;
    logic                           s2ptc_hit_valid;
    logic                           s2ptc_hit;//from ddtc
    logic [2:0]                     s2ptc_hit_lvl;
    logic [511:0]                   s2ptc_hit_content;

    logic                           up_s1ptc_valid;
    logic                           up_s1ptc_ready;
    logic  [2:0]                    up_s1ptc_lvl;
    logic                           up_s1ptc_leaf;
    logic                           up_s2ptc_gv;
    logic  [63:0]                   up_s1ptc_svnapot;
    logic  [511:0]                  up_s1ptc_pte;
    logic  [51:0]                   up_s1ptc_va;
    logic                           up_s1ptc_sxl;
    logic  [19:0]                   up_s1ptc_pscid;
    logic  [15:0]                   up_s1ptc_gscid;
    logic                           up_s2ptc_valid;
    logic                           up_s2ptc_ready;
    logic  [2:0]                    up_s2ptc_lvl;
    logic                           up_s2ptc_leaf;
    logic  [63:0]                   up_s2ptc_svnapot;
    logic  [511:0]                  up_s2ptc_pte;
    logic  [51:0]                   up_s2ptc_gpa;
    logic  [15:0]                   up_s2ptc_gscid;

    logic [3:0]                     inv_req_valid;
    logic [3:0]                     inv_req_ready;
    logic [INV_IDX_WIDTH-1:0]       inv_req_idx;
    logic [1:0]                     inv_req_type;
    logic                           inv_req_dv_gv;
    logic [23:0]                    inv_req_did_gscid;
    logic                           inv_req_pscv;
    logic [19:0]                    inv_req_pid_pscid;
    logic                           inv_req_av;
    logic [63:12]                   inv_req_addr;
    logic [3:0]                     inv_ack_valid;
    logic [3:0] [INV_IDX_WIDTH-1:0] inv_ack_idx;

assign atd_hpm_rvalid_o = atd_hpm_rvalid; 
assign atd_hpmctr_o = atd_hpmctr;
assign atd_hpmevt_o = atd_hpmevt;
assign atd_iocntovf_o = {atd_iocntovf,atd_hpmcnt_cy};
assign atd_pmip_clr_o = atd_pmip_clr;

iommu_tw_apb u0_iommu_tw_apb
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),
    
    .iommu_penable_i                (iommu_penable_i),
    .iommu_apb_wen_i                (iommu_apb_wen_i),
    .iommu_apb_addr_i               (iommu_apb_addr_i),
    .iommu_apb_wdata_i              (iommu_apb_wdata_i),
    .iommu_apb_ren_i                (iommu_apb_ren_i),

    .iommu_apb_rdata_o              (iommu_apb_rdata_o),
    .iommu_apb_ready_o              (iommu_apb_ready_o),
    .iommu_apb_slverr_o             (iommu_apb_slverr_o),

    .iommu_caps_o                   (iommu_caps),
    .iommu_ddtp_mode_o              (ddtp_mode),
    .iommu_ddtp_ppn_o               (ddtp_ppn),
    .iommu_fctl_o                   (iommu_fctl),

//    .iommu_ipsr_pmip_o              (atd_ipsr_pmip_o),
    .iommu_ipsr_pmip_clr_o          (atd_pmip_clr),
//    .iommu_iocntovf_i               (atd_iocntovf),
    .iommu_hpmcnt_cy_o              (atd_hpmcnt_cy),
    .iommu_iocntinh_o               (atd_iocntinh),
//    .iommu_hpmctr_i                 (atd_hpmctr),
    .iommu_hpmctr_o                 (atd_hpmctr_r),
    .iommu_hpmevt_o                 (atd_hpmevt)
    );


genvar i;
generate
for (i = 0;i < 31; i++) begin : gen_atd_hpm
iommu_atd_hpm u1_iommu_atd_hpm (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .ddt_walk_i                     (ddt_walk),
    .pdt_walk_i                     (pdt_walk),
    .cdw_did_i                      (cdw_did),
    .cdw_pv_i                       (cdw_pv),
    .cdw_pid_i                      (cdw_pid),
    .ptw_s1_walk_i                  (ptw_s1_walk),
    .ptw_s2_walk_i                  (ptw_s2_walk),
    .ptw_gscid_i                    (ptw_gscid),
    .ptw_pscid_i                    (ptw_pscid),

    .atd_pmip_clr_i                 (atd_pmip_clr),
    .atd_iocntinh_i                 (atd_iocntinh[i]),
    .atd_hpmctr_i                   (atd_hpmctr_r[i]),
    .atd_hpmevt_i                   (atd_hpmevt[i]),
    
    .atd_hpm_rvalid_o               (atd_hpm_rvalid[i]),
    .atd_iocntovf_o                 (atd_iocntovf[i]),
    .atd_hpmctr_o                   (atd_hpmctr[i])
);
end
endgenerate


iommu_atd_cdw
    #(
//    .RISCV_VLEN                       (RISCV_VLEN),//39
    .RISCV_PLEN                     (RISCV_PLEN)
)
u2_iommu_atd_cdw
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    // from regmap
    .ddtp_ppn_i                     (ddtp_ppn),
    .ddtp_mode_i                    (ddtp_mode),
    .iommu_caps_i                   (iommu_caps),
    .fctl_be_i                      (iommu_fctl[0]),
    .fctl_gxl_i                     (iommu_fctl[2]),

    //from input req
    .cdw_rfifo_wen_i                (cdw_rfifo_wen_i),
    .cdw_rfifo_din_i                (cdw_rfifo_wdata_i),
    .cdw_rfifo_full_o               (cdw_rfifo_full_o),

    .ats_req_wen_i                  (ats_req_wen_i),
    .ats_req_wdata_i                (ats_req_wdata_i),
    .ats_req_full_o                 (ats_req_full_o),

    .pri_req_wen_i                  (pri_req_wen_i),
    .pri_req_wdata_i                (pri_req_wdata_i),
    .pri_req_full_o                 (pri_req_full_o),

    .lookup_dc_valid_o              (lookup_dc_valid),
    .lookup_dc_ready_i              (lookup_dc_ready),
    .lookup_dc_did_o                (lookup_dc_did),
    .ddtc_hit_valid_i               (ddtc_hit_valid),
    .ddtc_hit_i                     (ddtc_hit),
    .ddtc_hit_lvl_i                 (ddtc_hit_lvl),
    .ddtc_hit_content_i             (ddtc_hit_content),
    .up_dc_valid_o                  (up_dc_valid),
    .up_dc_ready_i                  (up_dc_ready),
    .up_dc_leaf_o                   (up_dc_leaf),
    .up_dc_lvl_o                    (up_dc_lvl),
    .up_dc_did_o                    (up_dc_did),
    .up_dc_content_o                (up_dc_content),

    .lookup_pc_valid_o              (lookup_pc_valid),
    .lookup_pc_ready_i              (lookup_pc_ready),
    .lookup_pc_pid_o                (lookup_pc_pid),
    .lookup_pc_did_o                (lookup_pc_did),
    .pdtc_hit_valid_i               (pdtc_hit_valid),
    .pdtc_hit_i                     (pdtc_hit),
    .pdtc_hit_lvl_i                 (pdtc_hit_lvl),
    .pdtc_hit_content_i             (pdtc_hit_content),
    .up_pc_valid_o                  (up_pc_valid),
    .up_pc_ready_i                  (up_pc_ready),
    .up_pc_leaf_o                   (up_pc_leaf),
    .up_pc_lvl_o                    (up_pc_lvl),
    .up_pc_pid_o                    (up_pc_pid),
    .up_pc_did_o                    (up_pc_did),
    .up_pc_content_o                (up_pc_content),

    //between cdw and ptw
    .cdw_flush_i                    (cdw_flush),
    .implicit_done_i                (cdw_implicit_done),
    .implicit_ppn_i                 (cdw_implicit_ppn),

    .ptw_rfifo_wen_o                (ptw_rfifo_wen),
    .ptw_rfifo_din_o                (ptw_rfifo_din),
    .ptw_rfifo_full_i               (ptw_rfifo_full),

    //to FQ
    .cdw_fq_valid_o                 (cdw_fq_valid_o),
    .cdw_fq_record_o                (cdw_fq_record_o),
    .cdw_fq_ready_i                 (cdw_fq_ready_i),

    //output to t2r and then to pcie-rc
    .pri_resp_wen_o                 (pri_resp_wen_o),
    .pri_resp_wdata_o               (pri_resp_wdata_o),
    .pri_resp_full_i                (pri_resp_full_i),
    .pq_pri_wen_o                   (pq_pri_wen_o),
    .pq_pri_wdata_o                 (pq_pri_wdata_o),
    .pq_pri_full_i                  (pq_pri_full_i),

    // to HPM
    .ddt_walk_o                     (ddt_walk),
    .pdt_walk_o                     (pdt_walk),
    .cdw_did_o                      (cdw_did),
    .cdw_pv_o                       (cdw_pv),
    .cdw_pid_o                      (cdw_pid),

    .cdw_arvalid_o                  (cdw_arvalid_o),
    .cdw_arready_i                  (cdw_arready_i),
    .cdw_arid_o                     (cdw_arid_o),
    .cdw_araddr_o                   (cdw_araddr_o),
    .cdw_arlen_o                    (cdw_arlen_o),
    .cdw_rvalid_i                   (cdw_rvalid_i),
    .cdw_rready_o                   (cdw_rready_o),
    .cdw_rlast_i                    (cdw_rlast_i),
    .cdw_rid_i                      (cdw_rid_i),
    .cdw_rresp_i                    (cdw_rresp_i),
    .cdw_rdata_i                    (cdw_rdata_i)
);


iommu_atd_ptw
    #(
    .RISCV_GLEN                     (RISCV_GLEN),
    .RISCV_PLEN                     (RISCV_PLEN)
)
u3_iommu_atd_ptw
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

//between cdw and ptw
    .ptw_rfifo_wen_i                (ptw_rfifo_wen),
    .ptw_rfifo_din_i                (ptw_rfifo_din),
    .ptw_rfifo_full_o               (ptw_rfifo_full),
//    //between ptw and cdw
    .cdw_flush_o                    (cdw_flush),
    .cdw_implicit_done_o            (cdw_implicit_done),
    .cdw_implicit_ppn_o             (cdw_implicit_ppn),

//between ptw and atd-cache
    .lookup_s1ptc_valid_o           (lookup_s1ptc_valid),
    .lookup_s1ptc_ready_i           (lookup_s1ptc_ready),
    .lookup_s1ptc_lvl_o             (lookup_s1ptc_lvl),
    .lookup_s1ptc_leaf_o            (lookup_s1ptc_leaf),
    .lookup_s1ptc_gv_o              (lookup_s1ptc_gv),
    .lookup_s1ptc_va_o              (lookup_s1ptc_va),
    .lookup_s1ptc_pscid_o           (lookup_s1ptc_pscid),
    .lookup_s1ptc_gscid_o           (lookup_s1ptc_gscid),

    .s1ptc_hit_valid_i              (s1ptc_hit_valid),
    .s1ptc_hit_i                    (s1ptc_hit),
    .s1ptc_hit_lvl_i                (s1ptc_hit_lvl),
    .s1ptc_hit_content_i            (s1ptc_hit_content),

    .lookup_s2ptc_valid_o           (lookup_s2ptc_valid),
    .lookup_s2ptc_ready_i           (lookup_s2ptc_ready),
    .lookup_s2ptc_lvl_o             (lookup_s2ptc_lvl),
    .lookup_s2ptc_leaf_o            (lookup_s2ptc_leaf),
//  .lookup_s2ptc_gv_o              (lookup_s2ptc_gv),
    .lookup_s2ptc_gscid_o           (lookup_s2ptc_gscid),
    .lookup_s2ptc_gpa_o             (lookup_s2ptc_gpa),

    .s2ptc_hit_valid_i              (s2ptc_hit_valid),
    .s2ptc_hit_i                    (s2ptc_hit),
    .s2ptc_hit_lvl_i                (s2ptc_hit_lvl),
    .s2ptc_hit_content_i            (s2ptc_hit_content),

    .up_s1ptc_valid_o               (up_s1ptc_valid),
    .up_s1ptc_ready_i               (up_s1ptc_ready),
    .up_s1ptc_lvl_o                 (up_s1ptc_lvl),
    .up_s1ptc_leaf_o                (up_s1ptc_leaf),
    .up_s1ptc_gv_o                  (up_s1ptc_gv),
    .up_s1ptc_svnapot_o             (up_s1ptc_svnapot),
    .up_s1ptc_pte_o                 (up_s1ptc_pte),
    .up_s1ptc_va_o                  (up_s1ptc_va),
    .up_s1ptc_sxl_o                 (up_s1ptc_sxl),
    .up_s1ptc_pscid_o               (up_s1ptc_pscid),
    .up_s1ptc_gscid_o               (up_s1ptc_gscid),

    .up_s2ptc_valid_o               (up_s2ptc_valid),
    .up_s2ptc_ready_i               (up_s2ptc_ready),
    .up_s2ptc_lvl_o                 (up_s2ptc_lvl),
    .up_s2ptc_leaf_o                (up_s2ptc_leaf),
    .up_s2ptc_svnapot_o             (up_s2ptc_svnapot),
    .up_s2ptc_pte_o                 (up_s2ptc_pte),
    .up_s2ptc_gpa_o                 (up_s2ptc_gpa),
//  .up_s2ptc_gv_o                  (up_s2ptc_gv),
    .up_s2ptc_gscid_o               (up_s2ptc_gscid),

    .ptw_fq_valid_o                 (ptw_fq_valid_o),
    .ptw_fq_record_o                (ptw_fq_record_o),
    .ptw_fq_ready_i                 (ptw_fq_ready_i),

    //input from ptw and then to acd
    .ptw_wfifo_wen_o                (ptw_wfifo_wen_o),
    .ptw_wfifo_wdata_o              (ptw_wfifo_wdata_o),
    .ptw_wfifo_full_i               (ptw_wfifo_full_i),

    //to ats_t2r
    .ats_fault_valid_o              (ats_fault_valid_o),
    .ats_fault_record_o             (ats_fault_record_o),
    .ats_fault_ready_i              (ats_fault_ready_i),
    .ats_resp_wen_o                 (ats_resp_wen_o),
    .ats_resp_wdata_o               (ats_resp_wdata_o),
    .ats_resp_full_i                (ats_resp_full_i),

    //between ptw and HPM
    .ptw_s1_walk_o                  (ptw_s1_walk),
    .ptw_s2_walk_o                  (ptw_s2_walk),
    .ptw_gscid_o                    (ptw_gscid),
    .ptw_pscid_o                    (ptw_pscid),
    //to msi
    .up_ad_wen_o                    (up_ad_wen_o),
    .up_ad_wdata_o                  (up_ad_wdata_o),
    .up_ad_waddr_o                  (up_ad_waddr_o),

    .ptw_awvalid_o                  (ptw_awvalid_o),
    .ptw_awready_i                  (ptw_awready_i),
    .ptw_awid_o                     (ptw_awid_o),
    .ptw_awaddr_o                   (ptw_awaddr_o),
    .ptw_awlen_o                    (ptw_awlen_o),
    .ptw_wvalid_o                   (ptw_wvalid_o),
    .ptw_wready_i                   (ptw_wready_i),
    .ptw_wlast_o                    (ptw_wlast_o),
    .ptw_wstrb_o                    (ptw_wstrb_o),
    .ptw_wdata_o                    (ptw_wdata_o),
    .ptw_bvalid_i                   (ptw_bvalid_i),
    .ptw_bready_o                   (ptw_bready_o),
    .ptw_bid_i                      (ptw_bid_i),
    .ptw_bresp_i                    (ptw_bresp_i),

    .ptw_arvalid_o                  (ptw_arvalid_o),
    .ptw_arready_i                  (ptw_arready_i),
    .ptw_arid_o                     (ptw_arid_o),
    .ptw_araddr_o                   (ptw_araddr_o),
    .ptw_arlen_o                    (ptw_arlen_o),
    .ptw_rvalid_i                   (ptw_rvalid_i),
    .ptw_rready_o                   (ptw_rready_o),
    .ptw_rlast_i                    (ptw_rlast_i),
    .ptw_rid_i                      (ptw_rid_i),
    .ptw_rresp_i                    (ptw_rresp_i),
    .ptw_rdata_i                    (ptw_rdata_i)
);


iommu_atd_inv_top
    #(
    .FIFO_IDX_WIDTH             (INV_IDX_WIDTH  ), // = 5, //iommu_atd_cache_pkg::INV_IDX_WIDTH,
    .FIFO_DEPTH                 (2**INV_IDX_WIDTH),
    .SPARE_PARAM                (1'b0           )  // = 0
    )
u4_iommu_atd_inv_top
    (
    .clk                            (iommu_clk          ),
    .rstn                           (iommu_rstn         ),
    .invalid_req_valid_i            (cq_wfifo_wen_i     ),
    .invalid_req_ready_o            (cq_wfifo_full_o    ),
    .invalid_req_i                  (cq_wfifo_wdata_i   ),
    .invalid_req_fence_i            (cq_fence_valid_i   ),

    .inv_req_valid_o                (inv_req_valid      ),
    .inv_req_ready_i                (inv_req_ready      ),
    .inv_req_idx_o                  (inv_req_idx        ),
    .inv_req_itype_o                (inv_req_type       ),        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    .inv_req_dv_gv_o                (inv_req_dv_gv      ),
    .inv_req_did_gscid_o            (inv_req_did_gscid  ),
    .inv_req_pscv_o                 (inv_req_pscv       ),
    .inv_req_pid_pscid_o            (inv_req_pid_pscid  ),
    .inv_req_av_o                   (inv_req_av         ),
    .inv_req_addr_o                 (inv_req_addr       ),
    .inv_ack_valid_i                (inv_ack_valid      ),
    .inv_ack_idx_i                  (inv_ack_idx        ),
    .cmd_fifo_empty_o               (cq_fence_ready_o   ),
    .spare_in                       (1'b0               )
    );


iommu_atd_ddtc_wrap
    #(
    .INV_IDX_WIDTH              (INV_IDX_WIDTH              ), // = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    .TLB_QIDX_WIDTH             (DDTC_TLB_QIDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH       ,
    .MICRO_TLB_IDX_WIDTH        (DDTC_MICRO_TLB_IDX_WIDTH   ), // = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH  ,
    .CABIN_LKP_IDX_WIDTH        (DDTC_CABIN_LKP_IDX_WIDTH   ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH  ,
    .CABIN_UPD_IDX_WIDTH        (DDTC_CABIN_UPD_IDX_WIDTH   ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH  ,
    .CABIN_INV_IDX_WIDTH        (DDTC_CABIN_INV_IDX_WIDTH   ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH  ,
    .BANK_L0_IDX_WIDTH          (DDTC_BANK_L0_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH    ,
    .BANK_L1_IDX_WIDTH          (DDTC_BANK_L1_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH    ,
    .BANK_L2_IDX_WIDTH          (DDTC_BANK_L2_IDX_WIDTH     ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH    ,
    .BANK_L0_SET_IDX_WIDTH      (DDTC_BANK_L0_SET_IDX_WIDTH ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    .BANK_L1_SET_IDX_WIDTH      (DDTC_BANK_L1_SET_IDX_WIDTH ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    .BANK_L2_SET_IDX_WIDTH      (DDTC_BANK_L2_SET_IDX_WIDTH ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    .BANK_L0_WAY_IDX_WIDTH      (DDTC_BANK_L0_WAY_IDX_WIDTH ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    .BANK_L1_WAY_IDX_WIDTH      (DDTC_BANK_L1_WAY_IDX_WIDTH ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    .BANK_L2_WAY_IDX_WIDTH      (DDTC_BANK_L2_WAY_IDX_WIDTH ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    .SPARE_PARAM                (1'b0                       )  // = 0
)
u5_iommu_atd_ddtc_wrap
    (
    .clk                            (iommu_clk              ),
    .rstn                           (iommu_rstn             ),
    .invalid_req_valid_i            (inv_req_valid[0]       ),
    .invalid_req_ready_o            (inv_req_ready[0]       ),
    .invalid_req_idx_i              (inv_req_idx            ),
    .invalid_req_type_i             (inv_req_type           ),      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    .invalid_req_dv_gv_i            (inv_req_dv_gv          ),
    .invalid_req_did_gscid_i        (inv_req_did_gscid      ),
    .invalid_req_pscv_i             (inv_req_pscv           ),
    .invalid_req_pid_pscid_i        (inv_req_pid_pscid      ),
    .invalid_req_av_i               (inv_req_av             ),
    .invalid_req_addr_i             (inv_req_addr           ),
    .invalid_ack_valid_o            (inv_ack_valid[0]       ),
    .invalid_ack_idx_o              (inv_ack_idx[0]         ),

    .lookup_req_valid_i             (lookup_dc_valid        ),
    .lookup_req_ready_o             (lookup_dc_ready        ),
    .lookup_req_idx_i               ({DDTC_TLB_QIDX_WIDTH{1'b0}}                ),
    .lookup_req_device_id_i         (lookup_dc_did          ),
    .lookup_ack_valid_o             (ddtc_hit_valid         ),
    .lookup_ack_hit_o               (ddtc_hit               ),
    .lookup_ack_idx_o               (                       ),
    .lookup_ack_prefetched_o        (                       ),
    .lookup_ack_lvl_o               (ddtc_hit_lvl           ),
    .lookup_ack_o                   (ddtc_hit_content       ),
    .update_req_valid_i             (up_dc_valid            ),
    .update_req_ready_o             (up_dc_ready            ),
    .update_req_idx_i               ({DDTC_TLB_QIDX_WIDTH{1'b0}}                    ),
    .update_req_lvl_i               (up_dc_lvl              ),
    .update_req_prefetched_i        (1'b0                   ),
    .update_req_i                   (up_dc_content          ),
    .update_req_device_id_i         (up_dc_did              ),
    .update_ack_valid_o             (                       ),
    .update_ack_idx_o               (                       ),
    .multi_hit_check_i              (1'b1                   ),
    .multi_hit_fault_o              (                       ),
    .ecc_err_o                      (                       ),
    .spare_in                       (1'b0                   )
    );

iommu_atd_pdtc_wrap
    #(
    .INV_IDX_WIDTH              (INV_IDX_WIDTH             ), // = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    .TLB_QIDX_WIDTH             (PDTC_TLB_QIDX_WIDTH       ), // = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH       ,
    .MICRO_TLB_IDX_WIDTH        (PDTC_MICRO_TLB_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH  ,
    .CABIN_LKP_IDX_WIDTH        (PDTC_CABIN_LKP_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH  ,
    .CABIN_UPD_IDX_WIDTH        (PDTC_CABIN_UPD_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH  ,
    .CABIN_INV_IDX_WIDTH        (PDTC_CABIN_INV_IDX_WIDTH  ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH  ,
    .BANK_L0_IDX_WIDTH          (PDTC_BANK_L0_IDX_WIDTH    ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH    ,
    .BANK_L1_IDX_WIDTH          (PDTC_BANK_L1_IDX_WIDTH    ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH    ,
    .BANK_L2_IDX_WIDTH          (PDTC_BANK_L2_IDX_WIDTH    ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH    ,
    .BANK_L0_SET_IDX_WIDTH      (PDTC_BANK_L0_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    .BANK_L1_SET_IDX_WIDTH      (PDTC_BANK_L1_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    .BANK_L2_SET_IDX_WIDTH      (PDTC_BANK_L2_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    .BANK_L0_WAY_IDX_WIDTH      (PDTC_BANK_L0_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    .BANK_L1_WAY_IDX_WIDTH      (PDTC_BANK_L1_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    .BANK_L2_WAY_IDX_WIDTH      (PDTC_BANK_L2_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    .SPARE_PARAM                (1'b0                      )  // = 0
    )
u6_iommu_atd_pdtc_wrap
    (
    .clk                            (iommu_clk          ),
    .rstn                           (iommu_rstn         ),
    .invalid_req_valid_i            (inv_req_valid[1]   ),
    .invalid_req_ready_o            (inv_req_ready[1]   ),
    .invalid_req_idx_i              (inv_req_idx        ),
    .invalid_req_type_i             (inv_req_type       ),         // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    .invalid_req_dv_gv_i            (inv_req_dv_gv      ),
    .invalid_req_did_gscid_i        (inv_req_did_gscid  ),
    .invalid_req_pscv_i             (inv_req_pscv       ),
    .invalid_req_pid_pscid_i        (inv_req_pid_pscid  ),
    .invalid_req_av_i               (inv_req_av         ),
    .invalid_req_addr_i             (inv_req_addr       ),
    .invalid_ack_valid_o            (inv_ack_valid[1]   ),
    .invalid_ack_idx_o              (inv_ack_idx[1]     ),

    .lookup_req_valid_i             (lookup_pc_valid    ),
    .lookup_req_ready_o             (lookup_pc_ready    ),
    .lookup_req_idx_i               ({PDTC_TLB_QIDX_WIDTH{1'b0}}                ),
    .lookup_req_device_id_i         (lookup_pc_did      ),
    .lookup_req_process_id_i        (lookup_pc_pid      ),
    .lookup_ack_valid_o             (pdtc_hit_valid     ),
    .lookup_ack_hit_o               (pdtc_hit           ),
    .lookup_ack_idx_o               (                   ),
    .lookup_ack_prefetched_o        (                   ),
    .lookup_ack_lvl_o               (pdtc_hit_lvl       ),
    .lookup_ack_o                   (pdtc_hit_content   ),
    .update_req_valid_i             (up_pc_valid        ),
    .update_req_ready_o             (up_pc_ready        ),
    .update_req_idx_i               ({PDTC_TLB_QIDX_WIDTH{1'b0}}                ),
    .update_req_lvl_i               (up_pc_lvl          ),
    .update_req_prefetched_i        (1'b0               ),
    .update_req_i                   (up_pc_content      ),
    .update_req_device_id_i         (up_pc_did          ),
    .update_req_process_id_i        (up_pc_pid          ),
    .update_ack_valid_o             (                   ),
    .update_ack_idx_o               (                   ),
    .multi_hit_check_i              (1'b0               ),
    .multi_hit_fault_o              (                   ),
    .ecc_err_o                      (                       ),
    .spare_in                       (1'b0               )
    );


iommu_atd_s1ptc_wrap #(
    .INV_IDX_WIDTH              (INV_IDX_WIDTH              ), // = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    .TLB_QIDX_WIDTH             (S1PTC_TLB_QIDX_WIDTH       ), // = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    .MICRO_TLB_IDX_WIDTH        (S1PTC_MICRO_TLB_IDX_WIDTH  ), // = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    .CABIN_LKP_IDX_WIDTH        (S1PTC_CABIN_LKP_IDX_WIDTH  ), // = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    .CABIN_UPD_IDX_WIDTH        (S1PTC_CABIN_UPD_IDX_WIDTH  ), // = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    .CABIN_INV_IDX_WIDTH        (S1PTC_CABIN_INV_IDX_WIDTH  ), // = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH,
    .BANK_L0_IDX_WIDTH          (S1PTC_BANK_L0_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    .BANK_L1_IDX_WIDTH          (S1PTC_BANK_L1_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    .BANK_L2_IDX_WIDTH          (S1PTC_BANK_L2_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    .BANK_L3_IDX_WIDTH          (S1PTC_BANK_L3_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    .BANK_L4_IDX_WIDTH          (S1PTC_BANK_L4_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    .BANK_L0_SET_IDX_WIDTH      (S1PTC_BANK_L0_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    .BANK_L1_SET_IDX_WIDTH      (S1PTC_BANK_L1_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    .BANK_L2_SET_IDX_WIDTH      (S1PTC_BANK_L2_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    .BANK_L3_SET_IDX_WIDTH      (S1PTC_BANK_L3_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    .BANK_L4_SET_IDX_WIDTH      (S1PTC_BANK_L4_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    .BANK_L0_WAY_IDX_WIDTH      (S1PTC_BANK_L0_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    .BANK_L1_WAY_IDX_WIDTH      (S1PTC_BANK_L1_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    .BANK_L2_WAY_IDX_WIDTH      (S1PTC_BANK_L2_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    .BANK_L3_WAY_IDX_WIDTH      (S1PTC_BANK_L3_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    .BANK_L4_WAY_IDX_WIDTH      (S1PTC_BANK_L4_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH,
    .SPARE_PARAM                (1'b0                       )  // = 0
    )
u7_iommu_atd_s1ptc_wrap
    (
    .clk                            (iommu_clk          ),
    .rstn                           (iommu_rstn         ),
    .invalid_req_valid_i            (inv_req_valid[2]   ),
    .invalid_req_ready_o            (inv_req_ready[2]   ),
    .invalid_req_idx_i              (inv_req_idx        ),
    .invalid_req_type_i             (inv_req_type       ),        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    .invalid_req_dv_gv_i            (inv_req_dv_gv      ),
    .invalid_req_did_gscid_i        (inv_req_did_gscid  ),
    .invalid_req_pscv_i             (inv_req_pscv       ),
    .invalid_req_pid_pscid_i        (inv_req_pid_pscid  ),
    .invalid_req_av_i               (inv_req_av         ),
    .invalid_req_addr_i             (inv_req_addr       ),
    .invalid_ack_valid_o            (inv_ack_valid[2]   ),
    .invalid_ack_idx_o              (inv_ack_idx[2]     ),

    .lookup_req_valid_i             (lookup_s1ptc_valid ),
    .lookup_req_ready_o             (lookup_s1ptc_ready ),
    .lookup_req_idx_i               ({S1PTC_TLB_QIDX_WIDTH{1'b0}} ),
    .lookup_req_gv_i                (lookup_s1ptc_gv    ),
    .lookup_req_gscid_i             ( lookup_s1ptc_gscid),
    .lookup_req_pscid_i             (lookup_s1ptc_pscid ),
    .lookup_req_va_i                ( lookup_s1ptc_va   ),
    .lookup_ack_valid_o             (s1ptc_hit_valid    ),
    .lookup_ack_idx_o               (                   ),
    .lookup_ack_hit_o               (s1ptc_hit          ),
    .lookup_ack_lvl_o               (s1ptc_hit_lvl      ),
    .lookup_ack_prefetched_o        (                   ),
    .lookup_ack_o                   (s1ptc_hit_content  ),
    .lookup_ack_svnapot_o           (                   ),
    .update_req_valid_i             (up_s1ptc_valid     ),
    .update_req_ready_o             (up_s1ptc_ready     ),
    .update_req_idx_i               ({S1PTC_TLB_QIDX_WIDTH{1'b0}}               ),
    .update_req_lvl_i               (up_s1ptc_lvl       ),
    .update_req_prefetched_i        (1'b0               ),
    .update_req_i                   (up_s1ptc_pte       ),
    .update_req_svnapot_i           (up_s1ptc_svnapot   ),
    .update_req_gv_i                (up_s1ptc_gv    ),
    .update_req_pscid_i             (up_s1ptc_pscid     ),
    .update_req_gscid_i             (up_s1ptc_gscid     ),
    .update_req_va_i                (up_s1ptc_va        ),
    .update_req_sxl_i               (up_s1ptc_sxl       ),
    .update_ack_valid_o             (                   ),
    .update_ack_idx_o               (                   ),
    .multi_hit_check_i              (1'b1               ),
    .multi_hit_fault_o              (                   ),
    .ecc_err_o                      (                   ),
    .spare_in                       (1'b0               )
    );


iommu_atd_s2ptc_wrap
    #(
    .INV_IDX_WIDTH              (INV_IDX_WIDTH              ), // = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    .TLB_QIDX_WIDTH             (S2PTC_TLB_QIDX_WIDTH       ), // = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    .MICRO_TLB_IDX_WIDTH        (S2PTC_MICRO_TLB_IDX_WIDTH  ), // = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    .CABIN_LKP_IDX_WIDTH        (S2PTC_CABIN_LKP_IDX_WIDTH  ), // = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    .CABIN_UPD_IDX_WIDTH        (S2PTC_CABIN_UPD_IDX_WIDTH  ), // = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    .CABIN_INV_IDX_WIDTH        (S2PTC_CABIN_INV_IDX_WIDTH  ), // = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH,
    .BANK_L0_IDX_WIDTH          (S2PTC_BANK_L0_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    .BANK_L1_IDX_WIDTH          (S2PTC_BANK_L1_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    .BANK_L2_IDX_WIDTH          (S2PTC_BANK_L2_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    .BANK_L3_IDX_WIDTH          (S2PTC_BANK_L3_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    .BANK_L4_IDX_WIDTH          (S2PTC_BANK_L4_IDX_WIDTH    ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    .BANK_L0_SET_IDX_WIDTH      (S2PTC_BANK_L0_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    .BANK_L1_SET_IDX_WIDTH      (S2PTC_BANK_L1_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    .BANK_L2_SET_IDX_WIDTH      (S2PTC_BANK_L2_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    .BANK_L3_SET_IDX_WIDTH      (S2PTC_BANK_L3_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    .BANK_L4_SET_IDX_WIDTH      (S2PTC_BANK_L4_SET_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    .BANK_L0_WAY_IDX_WIDTH      (S2PTC_BANK_L0_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    .BANK_L1_WAY_IDX_WIDTH      (S2PTC_BANK_L1_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    .BANK_L2_WAY_IDX_WIDTH      (S2PTC_BANK_L2_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    .BANK_L3_WAY_IDX_WIDTH      (S2PTC_BANK_L3_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    .BANK_L4_WAY_IDX_WIDTH      (S2PTC_BANK_L4_WAY_IDX_WIDTH), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH,
    .SPARE_PARAM                (1'b0                       )  // = 0
    )
u8_iommu_atd_s2ptc_wrap
    (
    .clk                            (iommu_clk          ),
    .rstn                           (iommu_rstn         ),
    .invalid_req_valid_i            (inv_req_valid[3]   ),
    .invalid_req_ready_o            (inv_req_ready[3]   ),
    .invalid_req_idx_i              (inv_req_idx        ),
    .invalid_req_type_i             (inv_req_type       ),        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    .invalid_req_dv_gv_i            (inv_req_dv_gv      ),
    .invalid_req_did_gscid_i        (inv_req_did_gscid  ),
    .invalid_req_pscv_i             (inv_req_pscv       ),
    .invalid_req_pid_pscid_i        (inv_req_pid_pscid  ),
    .invalid_req_av_i               (inv_req_av         ),
    .invalid_req_addr_i             (inv_req_addr       ),
    .invalid_ack_valid_o            (inv_ack_valid[3]   ),
    .invalid_ack_idx_o              (inv_ack_idx[3]     ),

    .lookup_req_valid_i             (lookup_s2ptc_valid ),
    .lookup_req_ready_o             (lookup_s2ptc_ready ),
    .lookup_req_idx_i               ({S1PTC_TLB_QIDX_WIDTH{1'b0}}               ),
    .lookup_req_gscid_i             (lookup_s2ptc_gscid ),
    .lookup_req_gpa_i               (lookup_s2ptc_gpa ),
    .lookup_ack_valid_o             (s2ptc_hit_valid    ),
    .lookup_ack_idx_o               (                   ),
    .lookup_ack_hit_o               (s2ptc_hit          ),
    .lookup_ack_lvl_o               (s2ptc_hit_lvl      ),
    .lookup_ack_prefetched_o        (                   ),
    .lookup_ack_o                   (s2ptc_hit_content  ),
    .lookup_ack_svnapot_o           (                   ),

    .update_req_valid_i             (up_s2ptc_valid     ),
    .update_req_ready_o             (up_s2ptc_ready     ),
    .update_req_idx_i               ({S1PTC_TLB_QIDX_WIDTH{1'b0}}               ),
    .update_req_lvl_i               (up_s2ptc_lvl       ),
    .update_req_prefetched_i        (1'b0               ),
    .update_req_i                   (up_s2ptc_pte       ),
    .update_req_svnapot_i           (up_s2ptc_svnapot   ),
    .update_req_gscid_i             (up_s2ptc_gscid     ),
    .update_req_gpa_i               (up_s2ptc_gpa       ),
    .update_ack_valid_o             (                   ),
    .update_ack_idx_o               (                   ),
    .csr_fctl_gxl_i                 (iommu_fctl[2]      ),
    .multi_hit_check_i              (1'b0               ),
    .multi_hit_fault_o              (                   ),
    .ecc_err_o                      (                   ),
    .spare_in                       (1'b0               )
    );

endmodule



