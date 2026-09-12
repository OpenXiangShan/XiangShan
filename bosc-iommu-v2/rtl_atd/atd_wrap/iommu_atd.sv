///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd.sv
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
//`timescale 1ns/1ps
module iommu_atd #(
//    parameter RISCV_VLEN          = 6'd39,//39
    parameter RISCV_GLEN            = 6'd50,//sv48x4
    parameter RISCV_PLEN            = 6'd56,
    parameter  INV_IDX_WIDTH                    = iommu_atd_cache_pkg::INV_IDX_WIDTH ,
    parameter  DDTC_INV_IDX_WIDTH               = iommu_atd_cache_pkg::INV_IDX_WIDTH        ,
    parameter  DDTC_TLB_QIDX_WIDTH              = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH       ,
    parameter  DDTC_MICRO_TLB_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH  ,
    parameter  DDTC_CABIN_LKP_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH  ,
    parameter  DDTC_CABIN_UPD_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH  ,
    parameter  DDTC_CABIN_INV_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH  ,
    parameter  DDTC_BANK_L0_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH    ,
    parameter  DDTC_BANK_L1_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH    ,
    parameter  DDTC_BANK_L2_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH    ,
    parameter  DDTC_BANK_L0_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    parameter  DDTC_BANK_L1_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    parameter  DDTC_BANK_L2_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    parameter  DDTC_BANK_L0_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    parameter  DDTC_BANK_L1_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    parameter  DDTC_BANK_L2_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    parameter  PDTC_TLB_QIDX_WIDTH              = iommu_atd_cache_pkg::DDTC_TLB_QIDX_WIDTH       ,
    parameter  PDTC_MICRO_TLB_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_MICRO_TLB_IDX_WIDTH  ,
    parameter  PDTC_CABIN_LKP_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH  ,
    parameter  PDTC_CABIN_UPD_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH  ,
    parameter  PDTC_CABIN_INV_IDX_WIDTH         = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH  ,
    parameter  PDTC_BANK_L0_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH    ,
    parameter  PDTC_BANK_L1_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH    ,
    parameter  PDTC_BANK_L2_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH    ,
    parameter  PDTC_BANK_L0_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    parameter  PDTC_BANK_L1_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    parameter  PDTC_BANK_L2_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    parameter  PDTC_BANK_L0_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_WAY_IDX_WIDTH,
    parameter  PDTC_BANK_L1_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_WAY_IDX_WIDTH,
    parameter  PDTC_BANK_L2_WAY_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_WAY_IDX_WIDTH,
    parameter  S1PTC_TLB_QIDX_WIDTH             = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    parameter  S1PTC_MICRO_TLB_IDX_WIDTH        = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    parameter  S1PTC_CABIN_LKP_IDX_WIDTH        = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    parameter  S1PTC_CABIN_UPD_IDX_WIDTH        = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    parameter  S1PTC_CABIN_INV_IDX_WIDTH        = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH,
    parameter  S1PTC_BANK_L0_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    parameter  S1PTC_BANK_L1_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    parameter  S1PTC_BANK_L2_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    parameter  S1PTC_BANK_L3_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    parameter  S1PTC_BANK_L4_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    parameter  S1PTC_BANK_L0_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    parameter  S1PTC_BANK_L1_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    parameter  S1PTC_BANK_L2_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    parameter  S1PTC_BANK_L3_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    parameter  S1PTC_BANK_L4_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    parameter  S1PTC_BANK_L0_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    parameter  S1PTC_BANK_L1_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    parameter  S1PTC_BANK_L2_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    parameter  S1PTC_BANK_L3_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    parameter  S1PTC_BANK_L4_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH,
    parameter  S2PTC_TLB_QIDX_WIDTH             = iommu_atd_cache_pkg::S2PTC_TLB_QIDX_WIDTH,
    parameter  S2PTC_MICRO_TLB_IDX_WIDTH        = iommu_atd_cache_pkg::S2PTC_MICRO_TLB_IDX_WIDTH,
    parameter  S2PTC_CABIN_LKP_IDX_WIDTH        = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    parameter  S2PTC_CABIN_UPD_IDX_WIDTH        = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
    parameter  S2PTC_CABIN_INV_IDX_WIDTH        = iommu_atd_cache_pkg::S2PTC_CABIN_INV_IDX_WIDTH,
    parameter  S2PTC_BANK_L0_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    parameter  S2PTC_BANK_L1_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    parameter  S2PTC_BANK_L2_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    parameter  S2PTC_BANK_L3_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    parameter  S2PTC_BANK_L4_IDX_WIDTH          = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    parameter  S2PTC_BANK_L0_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    parameter  S2PTC_BANK_L1_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    parameter  S2PTC_BANK_L2_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    parameter  S2PTC_BANK_L3_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    parameter  S2PTC_BANK_L4_SET_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    parameter  S2PTC_BANK_L0_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L0_WAY_IDX_WIDTH,
    parameter  S2PTC_BANK_L1_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L1_WAY_IDX_WIDTH,
    parameter  S2PTC_BANK_L2_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L2_WAY_IDX_WIDTH,
    parameter  S2PTC_BANK_L3_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L3_WAY_IDX_WIDTH,
    parameter  S2PTC_BANK_L4_WAY_IDX_WIDTH      = iommu_atd_cache_pkg::S2PTC_BANK_L4_WAY_IDX_WIDTH
) (
    input                           iommu_clk,
    input                           iommu_rstn,

    input                           iommu_penable_i,
    input                           iommu_psel_i,
    input                           iommu_pwrite_i,
    input   [31:0]                  iommu_paddr_i,
    input   [31:0]                  iommu_pwdata_i,

    output  [31:0]                  iommu_prdata_o,
    output                          iommu_pready_o,
    output                          iommu_pslverr_o,

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

    //output to acd
    output                          atd_t2c_tvalid_o,
    input                           atd_t2c_tready_i,
    output   [63:0]                 atd_t2c_tdata_o,
    output   [7:0]                  atd_t2c_tstrb_o,
    output   [7:0]                  atd_t2c_tkeep_o,
    output                          atd_t2c_tlast_o,
    output   [3:0]                  atd_t2c_tid_o,
    output   [3:0]                  atd_t2c_tdest_o,
    output                          atd_t2c_tuser_o,

    input                           atd_r2t_rvalid_i,
    output                          atd_r2t_rready_o,
    input   [63:0]                  atd_r2t_rdata_i,
    input   [7:0]                   atd_r2t_rstrb_i,
    input   [7:0]                   atd_r2t_rkeep_i,
    input                           atd_r2t_rlast_i,
    input   [3:0]                   atd_r2t_rid_i,

    output                          atd_t2r_tvalid_o,
    input                           atd_t2r_tready_i,
    output   [63:0]                 atd_t2r_tdata_o,
    output   [7:0]                  atd_t2r_tstrb_o,
    output   [7:0]                  atd_t2r_tkeep_o,
    output                          atd_t2r_tlast_o,
    output   [3:0]                  atd_t2r_tid_o,

    //ace5-liteDVM
    input                           atd_ace5_acvalid,
    output                          atd_ace5_acready,
    input  [52-1:0]                 atd_ace5_acaddr,
    input  [3:0]                    atd_ace5_acvmidext,
    input  [3:0]                    atd_ace5_acsnoop,
    input  [2:0]                    atd_ace5_acprot,
    output                          atd_ace5_crvalid,
    input                           atd_ace5_crready,
    output  [4:0]                   atd_ace5_crresp,
    output  [1:0]                   atd_ace5_ardomain,
    output  [3:0]                   atd_ace5_arsnoop,
    output  [1:0]                   atd_ace5_arbar,
    output  [5:0]                   atd_ace5_awatop,
    output  [1:0]                   atd_ace5_awdomain,

    //pte_axi4
    output                          atd_ace5_arvalid,
    input                           atd_ace5_arready,
    output [5:0]                    atd_ace5_arid,
    output [RISCV_PLEN-1:0]         atd_ace5_araddr,
    output [7:0]                    atd_ace5_arlen,
    output [2:0]                    atd_ace5_arsize,
    output [1:0]                    atd_ace5_arburst,
    output                          atd_ace5_arlock,
    output [3:0]                    atd_ace5_arcache,
    output [2:0]                    atd_ace5_arprot,
    output [3:0]                    atd_ace5_arqos,
    input                           atd_ace5_rvalid,
    output                          atd_ace5_rready,
    input                           atd_ace5_rlast,
    input  [5:0]                    atd_ace5_rid,
    input  [1:0]                    atd_ace5_rresp,
    input  [255:0]                  atd_ace5_rdata,
    output                          atd_ace5_awvalid,
    input                           atd_ace5_awready,
    output [5:0]                    atd_ace5_awid,
    output [RISCV_PLEN-1:0]         atd_ace5_awaddr,
    output [7:0]                    atd_ace5_awlen,
    output [2:0]                    atd_ace5_awsize,
    output [1:0]                    atd_ace5_awburst,
    output                          atd_ace5_awlock,
    output [3:0]                    atd_ace5_awcache,
    output [2:0]                    atd_ace5_awprot,
    output [3:0]                    atd_ace5_awqos,
    output                          atd_ace5_wvalid,
    input                           atd_ace5_wready,
    output                          atd_ace5_wlast,
    output [31:0]                   atd_ace5_wstrb,
    output [255:0]                  atd_ace5_wdata,
    input                           atd_ace5_bvalid,
    output                          atd_ace5_bready,
    input  [5:0]                    atd_ace5_bid,
    input  [1:0]                    atd_ace5_bresp
);

    logic  [31:0]                   tc_prdata;
    logic                           tc_pready;
    logic                           tc_pslverr;
    logic  [31:0]                   tw_prdata;
    logic                           tw_pready;
    logic                           tw_pslverr;
    logic  [31:0]                   sw_prdata;
    logic                           sw_pready;
    logic                           sw_pslverr;

    logic                           atd_ipsr_pmip;
    logic  [30:0]                   atd_hpm_rvalid;
    logic  [31:0]                   atd_iocntovf;
    logic  [63:0]                   atd_hpmctr[30:0];
    logic  [63:0]                   atd_hpmevt[30:0];
    logic                           atd_pmip_clr;
    
    //cdw
    logic                           cdw_arvalid;
    logic                           cdw_arready;
    logic [3:0]                     cdw_arid;
    logic [RISCV_PLEN-1:0]          cdw_araddr;
    logic [7:0]                     cdw_arlen;
    logic                           cdw_rvalid;
    logic                           cdw_rready;
    logic                           cdw_rlast;
    logic [3:0]                     cdw_rid;
    logic [1:0]                     cdw_rresp;
    logic [255:0]                   cdw_rdata;

    logic                           ptw_arvalid;
    logic                           ptw_arready;
    logic [3:0]                     ptw_arid;
    logic [RISCV_PLEN-1:0]          ptw_araddr;
    logic [7:0]                     ptw_arlen;
    logic                           ptw_rvalid;
    logic                           ptw_rready;
    logic                           ptw_rlast;
    logic [3:0]                     ptw_rid;
    logic [1:0]                     ptw_rresp;
    logic [255:0]                   ptw_rdata;

    logic                           cq_acvalid;
    logic                           cq_acready;
    logic [52-1:0]                  cq_acaddr;
    logic [3:0]                     cq_acvmidext;
    logic [3:0]                     cq_acsnoop;
    logic [2:0]                     cq_acprot;
    logic                           cq_crvalid;
    logic                           cq_crready;
    logic [4:0]                     cq_crresp;
    logic [1:0]                     cq_ardomain;
    logic [3:0]                     cq_arsnoop;
    logic [1:0]                     cq_arbar;

    logic                           cq_arvalid;
    logic                           cq_arready;
    logic [3:0]                     cq_arid;
    logic [RISCV_PLEN-1:0]          cq_araddr;
    logic [7:0]                     cq_arlen;
    logic                           cq_rvalid;
    logic                           cq_rready;
    logic                           cq_rlast;
    logic [3:0]                     cq_rid;
    logic [1:0]                     cq_rresp;
    logic [255:0]                   cq_rdata;

    logic                           fq_awvalid;
    logic                           fq_awready;
    logic [3:0]                     fq_awid;
    logic [RISCV_PLEN-1:0]          fq_awaddr;
    logic [7:0]                     fq_awlen;
    logic                           fq_wvalid;
    logic                           fq_wready;
    logic                           fq_wlast;
    logic [31:0]                    fq_wstrb;
    logic [255:0]                   fq_wdata;
    logic                           fq_bvalid;
    logic                           fq_bready;
    logic [3:0]                     fq_bid;
    logic [1:0]                     fq_bresp;

    logic                           pq_awvalid;
    logic                           pq_awready;
    logic [3:0]                     pq_awid;
    logic [RISCV_PLEN-1:0]          pq_awaddr;
    logic [7:0]                     pq_awlen;
    logic                           pq_wvalid;
    logic                           pq_wready;
    logic                           pq_wlast;
    logic [31:0]                    pq_wstrb;
    logic [255:0]                   pq_wdata;
    logic                           pq_bvalid;
    logic                           pq_bready;
    logic [3:0]                     pq_bid;
    logic [1:0]                     pq_bresp;

    logic [5:0]                     msi_awatop;
    logic                           msi_awvalid;
    logic                           msi_awready;
    logic [3:0]                     msi_awid;
    logic [RISCV_PLEN-1:0]          msi_awaddr;
    logic [7:0]                     msi_awlen;
    logic [2:0]                     msi_awsize;
    logic [3:0]                     msi_awcache;
    logic                           msi_wvalid;
    logic                           msi_wready;
    logic                           msi_wlast;
    logic [31:0]                    msi_wstrb;
    logic [255:0]                   msi_wdata;
    logic                           msi_bvalid;
    logic                           msi_bready;
    logic [3:0]                     msi_bid;
    logic [1:0]                     msi_bresp;

    logic                           ptw_awvalid;
    logic                           ptw_awready;
    logic [3:0]                     ptw_awid;
    logic [RISCV_PLEN-1:0]          ptw_awaddr;
    logic [7:0]                     ptw_awlen;
    logic                           ptw_wvalid;
    logic                           ptw_wready;
    logic                           ptw_wlast;
    logic [31:0]                    ptw_wstrb;
    logic [255:0]                   ptw_wdata;
    logic                           ptw_bvalid;
    logic                           ptw_bready;
    logic [3:0]                     ptw_bid;
    logic [1:0]                     ptw_bresp;

    logic                           pri_resp_wen;
    logic [79:0]                    pri_resp_wdata;
    logic                           pri_resp_full;
    logic                           pq_pri_wen;
    logic [127:0]                   pq_pri_wdata;
    logic                           pq_pri_full;
    logic                           ats_fault_valid;
    logic [15:0]                    ats_fault_record;
    logic                           ats_fault_ready;
    logic                           ats_resp_wen;
    logic [113:0]                   ats_resp_wdata;
    logic                           ats_resp_full;

    logic                           cdw_rfifo_wen;
    logic [127:0]                   cdw_rfifo_wdata;
    logic                           cdw_rfifo_full;

    logic                           ats_req_wen;
    logic [191:0]                   ats_req_wdata;
    logic                           ats_req_full;

    logic                           pri_req_wen;
    logic [127:0]                   pri_req_wdata;
    logic                           pri_req_full;

    logic                           acd_fq_valid;
    logic [255:0]                   acd_fq_record;
    logic                           acd_fq_ready;
    logic                           cdw_fq_valid;
    logic [255:0]                   cdw_fq_record;
    logic                           cdw_fq_ready;
    logic                           ptw_fq_valid;
    logic [255:0]                   ptw_fq_record;
    logic                           ptw_fq_ready;
    logic                           up_ad_wen;
    logic  [7:0]                    up_ad_wdata;
    logic  [52:0]                   up_ad_waddr;
    logic                           ptw_wfifo_wen;
    logic [319:0]                   ptw_wfifo_wdata;
    logic                           ptw_wfifo_full;

    logic                           cq_atd_fifo_wen;
    logic [127:0]                   cq_atd_fifo_din;
    logic                           cq_atd_fifo_full;
    logic                           cq_atd_fence_valid;
    logic                           cq_atd_fence_ready;

    logic                           cq_acd_fifo_wen;
    logic [127:0]                   cq_acd_fifo_din;
    logic                           cq_acd_fifo_full;
    logic                           cq_acd_fence_ready;
    logic                           cq_acd_dvm_ready;
    logic                           cq_ats_fifo_full;
    logic                           cq_atc_fence_ready;

    logic                          iommu_apb_wen;
    logic [11:0]                   iommu_apb_addr;
    logic [31:0]                   iommu_apb_wdata;
    logic                          iommu_apb_ren;

    logic [31:0]                   iommu_apb_rdata;
    logic                          iommu_apb_ready;
    logic                          iommu_apb_slverr;
    logic [31:0]                   tc_apb_rdata;
    logic                          tc_apb_ready;
    logic                          tc_apb_slverr;
    logic [31:0]                   tw_apb_rdata;
    logic                          tw_apb_ready;
    logic                          tw_apb_slverr;
    logic [31:0]                   sw_apb_rdata;
    logic                          sw_apb_ready;
    logic                          sw_apb_slverr;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) begin
        iommu_apb_wen <= 1'b0;
        iommu_apb_addr <= 12'd0;
        iommu_apb_wdata <= 32'd0;
        iommu_apb_ren <= 1'b0;
    end else begin
        iommu_apb_wen <= iommu_psel_i && iommu_pwrite_i;
        iommu_apb_addr <= iommu_paddr_i[11:0];
        iommu_apb_wdata <= iommu_pwdata_i;
        iommu_apb_ren <= iommu_psel_i && (!iommu_pwrite_i);
    end
end

assign iommu_pready_o = iommu_apb_ready;
assign iommu_prdata_o = iommu_apb_rdata;
assign iommu_pslverr_o = iommu_apb_slverr;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn) begin
        iommu_apb_ready <= 1'b0;
        iommu_apb_rdata <= 32'd0;
        iommu_apb_slverr <= 1'b0;
    end else if (tc_apb_slverr) begin
        iommu_apb_ready <= tc_apb_ready;
        iommu_apb_rdata <= tc_apb_rdata;
        iommu_apb_slverr <= 1'b0;
    end else if (tw_apb_slverr) begin
        iommu_apb_ready <= tw_apb_ready;
        iommu_apb_rdata <= tw_apb_rdata;
        iommu_apb_slverr <= 1'b0;
    end else if (sw_apb_slverr) begin
        iommu_apb_ready <= sw_apb_ready;
        iommu_apb_rdata <= sw_apb_rdata;
        iommu_apb_slverr <= 1'b0;
    end else begin
        iommu_apb_ready <= iommu_psel_i && iommu_penable_i;
        iommu_apb_rdata <= 32'd0;
        iommu_apb_slverr <= iommu_psel_i && iommu_penable_i;
    end
end


iommu_atd_tc u0_iommu_atd_tc
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .iommu_penable_i                (iommu_penable_i),
    .iommu_apb_wen_i                (iommu_apb_wen),
    .iommu_apb_addr_i               (iommu_apb_addr),
    .iommu_apb_wdata_i              (iommu_apb_wdata),
    .iommu_apb_ren_i                (iommu_apb_ren),

    .iommu_apb_rdata_o              (tc_apb_rdata),
    .iommu_apb_ready_o              (tc_apb_ready),
    .iommu_apb_slverr_o             (tc_apb_slverr),

    .atd_hpm_rvalid_i               (atd_hpm_rvalid),
    .atd_iocntovf_i                 (atd_iocntovf),
    .atd_hpmctr_i                   (atd_hpmctr),
    .atd_hpmevt_i                   (atd_hpmevt),
    .atd_pmip_clr_i                 (atd_pmip_clr),
    .atd_ipsr_pmip_o                (atd_ipsr_pmip),
    ////////**********************************************************************************************************///////
    //receive acd
    ////////**********************************************************************************************************///////
    //input from acd
    .atd_c2t_rvalid_i               (atd_c2t_rvalid_i),
    .atd_c2t_rready_o               (atd_c2t_rready_o),
    .atd_c2t_rdata_i                (atd_c2t_rdata_i),
    .atd_c2t_rstrb_i                (atd_c2t_rstrb_i),
    .atd_c2t_rkeep_i                (atd_c2t_rkeep_i),
    .atd_c2t_rlast_i                (atd_c2t_rlast_i),
    .atd_c2t_rid_i                  (atd_c2t_rid_i  ),
    .atd_c2t_rdest_i                (atd_c2t_rdest_i),
    .atd_c2t_ruser_i                (atd_c2t_ruser_i),

    //acd:ptw_req output to cdw
    .cdw_rfifo_wen_o                (cdw_rfifo_wen  ),
    .cdw_rfifo_wdata_o              (cdw_rfifo_wdata),
    .cdw_rfifo_full_i               (cdw_rfifo_full ),

    //acd_fault -> cdw
    .acd_fq_valid_o                 (acd_fq_valid   ),
    .acd_fq_record_o                (acd_fq_record  ),
    .acd_fq_ready_i                 (acd_fq_ready   ),

    ////////**********************************************************************************************************///////
    //transmit to acd
    ////////**********************************************************************************************************///////                               //flush/invalidation: cq -> acd
    .cq_wfifo_wen_i                 (cq_acd_fifo_wen),
    .cq_wfifo_wdata_i               (cq_acd_fifo_din),
    .cq_wfifo_full_o                (cq_acd_fifo_full),
    .cq_fence_ready_o               (cq_acd_fence_ready),
    .cq_dvm_ready_o                 (cq_acd_dvm_ready),

    //ptw_ack/ptw_result:ptw -> acd
    .ptw_wfifo_wen_i                (ptw_wfifo_wen),
    .ptw_wfifo_wdata_i              (ptw_wfifo_wdata),
    .ptw_wfifo_full_o               (ptw_wfifo_full),

    //output to acd
    .atd_t2c_tvalid_o               (atd_t2c_tvalid_o),
    .atd_t2c_tready_i               (atd_t2c_tready_i),
    .atd_t2c_tdata_o                (atd_t2c_tdata_o),
    .atd_t2c_tstrb_o                (atd_t2c_tstrb_o),
    .atd_t2c_tkeep_o                (atd_t2c_tkeep_o),
    .atd_t2c_tlast_o                (atd_t2c_tlast_o),
    .atd_t2c_tid_o                  (atd_t2c_tid_o  ),
    .atd_t2c_tdest_o                (atd_t2c_tdest_o),
    .atd_t2c_tuser_o                (atd_t2c_tuser_o)
);


iommu_atd_tr u1_iommu_atd_tr
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .atd_r2t_rvalid_i               (atd_r2t_rvalid_i),
    .atd_r2t_rready_o               (atd_r2t_rready_o),
    .atd_r2t_rdata_i                (atd_r2t_rdata_i),
    .atd_r2t_rstrb_i                (atd_r2t_rstrb_i),
    .atd_r2t_rkeep_i                (atd_r2t_rkeep_i),
    .atd_r2t_rlast_i                (atd_r2t_rlast_i),
    .atd_r2t_rid_i                  (atd_r2t_rid_i  ),

    .ats_req_wen_o                  (ats_req_wen),
    .ats_req_wdata_o                (ats_req_wdata),
    .ats_req_full_i                 (ats_req_full),

    .pri_req_wen_o                  (pri_req_wen),
    .pri_req_wdata_o                (pri_req_wdata),
    .pri_req_full_i                 (pri_req_full),

    .ats_sync_req_i                 (cq_atd_fence_valid),
    .ats_sync_ack_o                 (cq_atc_fence_ready),

    .ats_resp_wen_i                 (ats_resp_wen),
    .ats_resp_wdata_i               (ats_resp_wdata),
    .ats_resp_full_o                (ats_resp_full),

    .ats_fault_wen_i                (ats_fault_valid),
    .ats_fault_wdata_i              (ats_fault_record),
    .ats_fault_full_o               (ats_fault_ready),

    .ats_inv_wen_i                  (cq_acd_fifo_wen),
    .ats_inv_wdata_i                (cq_acd_fifo_din),
    .ats_inv_full_o                 (cq_ats_fifo_full),

    .pri_resp_wen_i                 (pri_resp_wen),
    .pri_resp_wdata_i               (pri_resp_wdata),
    .pri_resp_full_o                (pri_resp_full),

    .atd_t2r_tvalid_o               (atd_t2r_tvalid_o   ),
    .atd_t2r_tready_i               (atd_t2r_tready_i   ),
    .atd_t2r_tdata_o                (atd_t2r_tdata_o    ),
    .atd_t2r_tstrb_o                (atd_t2r_tstrb_o    ),
    .atd_t2r_tkeep_o                (atd_t2r_tkeep_o    ),
    .atd_t2r_tlast_o                (atd_t2r_tlast_o    ),
    .atd_t2r_tid_o                  (atd_t2r_tid_o      )
);



iommu_atd_tw
    #(
    .RISCV_GLEN                     (RISCV_GLEN),//50
    .RISCV_PLEN                     (RISCV_PLEN),
    .INV_IDX_WIDTH                  (INV_IDX_WIDTH        ),
    .DDTC_INV_IDX_WIDTH             (DDTC_INV_IDX_WIDTH        ),
    .DDTC_TLB_QIDX_WIDTH            (DDTC_TLB_QIDX_WIDTH       ),
    .DDTC_MICRO_TLB_IDX_WIDTH       (DDTC_MICRO_TLB_IDX_WIDTH  ),
    .DDTC_CABIN_LKP_IDX_WIDTH       (DDTC_CABIN_LKP_IDX_WIDTH  ),
    .DDTC_CABIN_UPD_IDX_WIDTH       (DDTC_CABIN_UPD_IDX_WIDTH  ),
    .DDTC_CABIN_INV_IDX_WIDTH       (DDTC_CABIN_INV_IDX_WIDTH  ),
    .DDTC_BANK_L0_IDX_WIDTH         (DDTC_BANK_L0_IDX_WIDTH    ),
    .DDTC_BANK_L1_IDX_WIDTH         (DDTC_BANK_L1_IDX_WIDTH    ),
    .DDTC_BANK_L2_IDX_WIDTH         (DDTC_BANK_L2_IDX_WIDTH    ),
    .DDTC_BANK_L0_SET_IDX_WIDTH     (DDTC_BANK_L0_SET_IDX_WIDTH),
    .DDTC_BANK_L1_SET_IDX_WIDTH     (DDTC_BANK_L1_SET_IDX_WIDTH),
    .DDTC_BANK_L2_SET_IDX_WIDTH     (DDTC_BANK_L2_SET_IDX_WIDTH),
    .DDTC_BANK_L0_WAY_IDX_WIDTH     (DDTC_BANK_L0_WAY_IDX_WIDTH),
    .DDTC_BANK_L1_WAY_IDX_WIDTH     (DDTC_BANK_L1_WAY_IDX_WIDTH),
    .DDTC_BANK_L2_WAY_IDX_WIDTH     (DDTC_BANK_L2_WAY_IDX_WIDTH),

    .PDTC_TLB_QIDX_WIDTH            (PDTC_TLB_QIDX_WIDTH       ),
    .PDTC_MICRO_TLB_IDX_WIDTH       (PDTC_MICRO_TLB_IDX_WIDTH  ),
    .PDTC_CABIN_LKP_IDX_WIDTH       (PDTC_CABIN_LKP_IDX_WIDTH  ),
    .PDTC_CABIN_UPD_IDX_WIDTH       (PDTC_CABIN_UPD_IDX_WIDTH  ),
    .PDTC_CABIN_INV_IDX_WIDTH       (PDTC_CABIN_INV_IDX_WIDTH  ),
    .PDTC_BANK_L0_IDX_WIDTH         (PDTC_BANK_L0_IDX_WIDTH    ),
    .PDTC_BANK_L1_IDX_WIDTH         (PDTC_BANK_L1_IDX_WIDTH    ),
    .PDTC_BANK_L2_IDX_WIDTH         (PDTC_BANK_L2_IDX_WIDTH    ),
    .PDTC_BANK_L0_SET_IDX_WIDTH     (PDTC_BANK_L0_SET_IDX_WIDTH),
    .PDTC_BANK_L1_SET_IDX_WIDTH     (PDTC_BANK_L1_SET_IDX_WIDTH),
    .PDTC_BANK_L2_SET_IDX_WIDTH     (PDTC_BANK_L2_SET_IDX_WIDTH),
    .PDTC_BANK_L0_WAY_IDX_WIDTH     (PDTC_BANK_L0_WAY_IDX_WIDTH),
    .PDTC_BANK_L1_WAY_IDX_WIDTH     (PDTC_BANK_L1_WAY_IDX_WIDTH),
    .PDTC_BANK_L2_WAY_IDX_WIDTH     (PDTC_BANK_L2_WAY_IDX_WIDTH),

    .S1PTC_TLB_QIDX_WIDTH           (S1PTC_TLB_QIDX_WIDTH       ),
    .S1PTC_MICRO_TLB_IDX_WIDTH      (S1PTC_MICRO_TLB_IDX_WIDTH  ),
    .S1PTC_CABIN_LKP_IDX_WIDTH      (S1PTC_CABIN_LKP_IDX_WIDTH  ),
    .S1PTC_CABIN_UPD_IDX_WIDTH      (S1PTC_CABIN_UPD_IDX_WIDTH  ),
    .S1PTC_CABIN_INV_IDX_WIDTH      (S1PTC_CABIN_INV_IDX_WIDTH  ),
    .S1PTC_BANK_L0_IDX_WIDTH        (S1PTC_BANK_L0_IDX_WIDTH    ),
    .S1PTC_BANK_L1_IDX_WIDTH        (S1PTC_BANK_L1_IDX_WIDTH    ),
    .S1PTC_BANK_L2_IDX_WIDTH        (S1PTC_BANK_L2_IDX_WIDTH    ),
    .S1PTC_BANK_L3_IDX_WIDTH        (S1PTC_BANK_L3_IDX_WIDTH    ),
    .S1PTC_BANK_L4_IDX_WIDTH        (S1PTC_BANK_L4_IDX_WIDTH    ),
    .S1PTC_BANK_L0_SET_IDX_WIDTH    (S1PTC_BANK_L0_SET_IDX_WIDTH),
    .S1PTC_BANK_L1_SET_IDX_WIDTH    (S1PTC_BANK_L1_SET_IDX_WIDTH),
    .S1PTC_BANK_L2_SET_IDX_WIDTH    (S1PTC_BANK_L2_SET_IDX_WIDTH),
    .S1PTC_BANK_L3_SET_IDX_WIDTH    (S1PTC_BANK_L3_SET_IDX_WIDTH),
    .S1PTC_BANK_L4_SET_IDX_WIDTH    (S1PTC_BANK_L4_SET_IDX_WIDTH),
    .S1PTC_BANK_L0_WAY_IDX_WIDTH    (S1PTC_BANK_L0_WAY_IDX_WIDTH),
    .S1PTC_BANK_L1_WAY_IDX_WIDTH    (S1PTC_BANK_L1_WAY_IDX_WIDTH),
    .S1PTC_BANK_L2_WAY_IDX_WIDTH    (S1PTC_BANK_L2_WAY_IDX_WIDTH),
    .S1PTC_BANK_L3_WAY_IDX_WIDTH    (S1PTC_BANK_L3_WAY_IDX_WIDTH),
    .S1PTC_BANK_L4_WAY_IDX_WIDTH    (S1PTC_BANK_L4_WAY_IDX_WIDTH),

    .S2PTC_TLB_QIDX_WIDTH           (S2PTC_TLB_QIDX_WIDTH       ),
    .S2PTC_MICRO_TLB_IDX_WIDTH      (S2PTC_MICRO_TLB_IDX_WIDTH  ),
    .S2PTC_CABIN_LKP_IDX_WIDTH      (S2PTC_CABIN_LKP_IDX_WIDTH  ),
    .S2PTC_CABIN_UPD_IDX_WIDTH      (S2PTC_CABIN_UPD_IDX_WIDTH  ),
    .S2PTC_CABIN_INV_IDX_WIDTH      (S2PTC_CABIN_INV_IDX_WIDTH  ),
    .S2PTC_BANK_L0_IDX_WIDTH        (S2PTC_BANK_L0_IDX_WIDTH    ),
    .S2PTC_BANK_L1_IDX_WIDTH        (S2PTC_BANK_L1_IDX_WIDTH    ),
    .S2PTC_BANK_L2_IDX_WIDTH        (S2PTC_BANK_L2_IDX_WIDTH    ),
    .S2PTC_BANK_L3_IDX_WIDTH        (S2PTC_BANK_L3_IDX_WIDTH    ),
    .S2PTC_BANK_L4_IDX_WIDTH        (S2PTC_BANK_L4_IDX_WIDTH    ),
    .S2PTC_BANK_L0_SET_IDX_WIDTH    (S2PTC_BANK_L0_SET_IDX_WIDTH),
    .S2PTC_BANK_L1_SET_IDX_WIDTH    (S2PTC_BANK_L1_SET_IDX_WIDTH),
    .S2PTC_BANK_L2_SET_IDX_WIDTH    (S2PTC_BANK_L2_SET_IDX_WIDTH),
    .S2PTC_BANK_L3_SET_IDX_WIDTH    (S2PTC_BANK_L3_SET_IDX_WIDTH),
    .S2PTC_BANK_L4_SET_IDX_WIDTH    (S2PTC_BANK_L4_SET_IDX_WIDTH),
    .S2PTC_BANK_L0_WAY_IDX_WIDTH    (S2PTC_BANK_L0_WAY_IDX_WIDTH),
    .S2PTC_BANK_L1_WAY_IDX_WIDTH    (S2PTC_BANK_L1_WAY_IDX_WIDTH),
    .S2PTC_BANK_L2_WAY_IDX_WIDTH    (S2PTC_BANK_L2_WAY_IDX_WIDTH),
    .S2PTC_BANK_L3_WAY_IDX_WIDTH    (S2PTC_BANK_L3_WAY_IDX_WIDTH),
    .S2PTC_BANK_L4_WAY_IDX_WIDTH    (S2PTC_BANK_L4_WAY_IDX_WIDTH)
)
u2_iommu_atd_tw
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .iommu_penable_i                (iommu_penable_i),
    .iommu_apb_wen_i                (iommu_apb_wen),
    .iommu_apb_addr_i               (iommu_apb_addr),
    .iommu_apb_wdata_i              (iommu_apb_wdata),
    .iommu_apb_ren_i                (iommu_apb_ren),

    .iommu_apb_rdata_o              (tw_apb_rdata),
    .iommu_apb_ready_o              (tw_apb_ready),
    .iommu_apb_slverr_o             (tw_apb_slverr),

//    .atd_ipsr_pmip_o                (atd_ipsr_pmip),
    .atd_hpm_rvalid_o               (atd_hpm_rvalid),
    .atd_iocntovf_o                 (atd_iocntovf),
    .atd_hpmctr_o                   (atd_hpmctr),
    .atd_hpmevt_o                   (atd_hpmevt),
    .atd_pmip_clr_o                 (atd_pmip_clr),

    //between atd_tw and atd_tc
    .cdw_rfifo_wen_i                (cdw_rfifo_wen  ),
    .cdw_rfifo_wdata_i              (cdw_rfifo_wdata),
    .cdw_rfifo_full_o               (cdw_rfifo_full ),

    .ats_req_wen_i                  (ats_req_wen),
    .ats_req_wdata_i                (ats_req_wdata),
    .ats_req_full_o                 (ats_req_full),

    .pri_req_wen_i                  (pri_req_wen),
    .pri_req_wdata_i                (pri_req_wdata),
    .pri_req_full_o                 (pri_req_full),

    .ptw_wfifo_wen_o                (ptw_wfifo_wen  ),
    .ptw_wfifo_wdata_o              (ptw_wfifo_wdata),
    .ptw_wfifo_full_i               (ptw_wfifo_full ),

    //flush/invalidation: cq -> atd-cache
    .cq_wfifo_wen_i                 (cq_atd_fifo_wen),
    .cq_wfifo_wdata_i               (cq_atd_fifo_din),
    .cq_wfifo_full_o                (cq_atd_fifo_full),
    .cq_fence_valid_i               (cq_atd_fence_valid),
    .cq_fence_ready_o               (cq_atd_fence_ready),

    //to FQ
    .cdw_fq_valid_o                 (cdw_fq_valid),
    .cdw_fq_record_o                (cdw_fq_record),
    .cdw_fq_ready_i                 (cdw_fq_ready),
    .ptw_fq_valid_o                 (ptw_fq_valid),
    .ptw_fq_record_o                (ptw_fq_record),
    .ptw_fq_ready_i                 (ptw_fq_ready),

    //output to t2r and then to pcie-rc
    .pri_resp_wen_o                 (pri_resp_wen),
    .pri_resp_wdata_o               (pri_resp_wdata),
    .pri_resp_full_i                (pri_resp_full),

    //from cdw
    .pq_pri_wen_o                   (pq_pri_wen),
    .pq_pri_wdata_o                 (pq_pri_wdata),
    .pq_pri_full_i                  (pq_pri_full),

    .ats_fault_valid_o              (ats_fault_valid),
    .ats_fault_record_o             (ats_fault_record),
    .ats_fault_ready_i              (ats_fault_ready),

    .ats_resp_wen_o                 (ats_resp_wen),
    .ats_resp_wdata_o               (ats_resp_wdata),
    .ats_resp_full_i                (ats_resp_full),

    .up_ad_wen_o                    (up_ad_wen),
    .up_ad_wdata_o                  (up_ad_wdata),
    .up_ad_waddr_o                  (up_ad_waddr),

    .cdw_arvalid_o                  (cdw_arvalid),
    .cdw_arready_i                  (cdw_arready),
    .cdw_arid_o                     (cdw_arid),
    .cdw_araddr_o                   (cdw_araddr),
    .cdw_arlen_o                    (cdw_arlen),
    .cdw_rvalid_i                   (cdw_rvalid),
    .cdw_rready_o                   (cdw_rready),
    .cdw_rlast_i                    (cdw_rlast),
    .cdw_rid_i                      (cdw_rid),
    .cdw_rresp_i                    (cdw_rresp),
    .cdw_rdata_i                    (cdw_rdata),

    .ptw_awvalid_o                  (ptw_awvalid),
    .ptw_awready_i                  (ptw_awready),
    .ptw_awid_o                     (ptw_awid),
    .ptw_awaddr_o                   (ptw_awaddr),
    .ptw_awlen_o                    (ptw_awlen),
    .ptw_wvalid_o                   (ptw_wvalid),
    .ptw_wready_i                   (ptw_wready),
    .ptw_wlast_o                    (ptw_wlast),
    .ptw_wstrb_o                    (ptw_wstrb),
    .ptw_wdata_o                    (ptw_wdata),
    .ptw_bvalid_i                   (ptw_bvalid),
    .ptw_bready_o                   (ptw_bready),
    .ptw_bid_i                      (ptw_bid),
    .ptw_bresp_i                    (ptw_bresp),

    .ptw_arvalid_o                  (ptw_arvalid),
    .ptw_arready_i                  (ptw_arready),
    .ptw_arid_o                     (ptw_arid),
    .ptw_araddr_o                   (ptw_araddr),
    .ptw_arlen_o                    (ptw_arlen),
    .ptw_rvalid_i                   (ptw_rvalid),
    .ptw_rready_o                   (ptw_rready),
    .ptw_rlast_i                    (ptw_rlast),
    .ptw_rid_i                      (ptw_rid),
    .ptw_rresp_i                    (ptw_rresp),
    .ptw_rdata_i                    (ptw_rdata)
);


iommu_atd_sw
#(
    .RISCV_GLEN                     (RISCV_GLEN),//50
    .RISCV_PLEN                     (RISCV_PLEN)
)
u3_iommu_atd_sw
(
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .iommu_penable_i                (iommu_penable_i),
    .iommu_apb_wen_i                (iommu_apb_wen),
    .iommu_apb_addr_i               (iommu_apb_addr),
    .iommu_apb_wdata_i              (iommu_apb_wdata),
    .iommu_apb_ren_i                (iommu_apb_ren),

    .iommu_apb_rdata_o              (sw_apb_rdata),
    .iommu_apb_ready_o              (sw_apb_ready),
    .iommu_apb_slverr_o             (sw_apb_slverr),

    .atd_ipsr_pmip_i                (atd_ipsr_pmip),
    //cq to atd-cache
    .cq_atd_fifo_wen_o              (cq_atd_fifo_wen),
    .cq_atd_fifo_din_o              (cq_atd_fifo_din),
    .cq_atd_fifo_full_i             (cq_atd_fifo_full),
    .cq_atd_fence_valid_o           (cq_atd_fence_valid),
    .cq_atd_fence_ready_i           (cq_atd_fence_ready),

    //cq to atd_tc
    .cq_acd_fifo_wen_o              (cq_acd_fifo_wen),
    .cq_acd_fifo_din_o              (cq_acd_fifo_din),
    .cq_acd_fifo_full_i             (cq_acd_fifo_full),
    .cq_acd_fence_ready_i           (cq_acd_fence_ready),
    .cq_acd_dvm_ready_i             (cq_acd_dvm_ready),

    .cq_ats_fifo_full_i             (cq_ats_fifo_full),
    .cq_atc_fence_ready_i           (cq_atc_fence_ready),

    //from atd_tc to cq
    .fq_acd_valid_i                 (acd_fq_valid),
    .fq_acd_record_i                (acd_fq_record),
    .fq_acd_ready_o                 (acd_fq_ready),

    //from CDW
    .fq_cdw_valid_i                 (cdw_fq_valid),
    .fq_cdw_record_i                (cdw_fq_record),
    .fq_cdw_ready_o                 (cdw_fq_ready),

    //from PTW
    .fq_ptw_valid_i                 (ptw_fq_valid),
    .fq_ptw_record_i                (ptw_fq_record),
    .fq_ptw_ready_o                 (ptw_fq_ready),

    //from cdw
    .pq_pri_wen_i                   (pq_pri_wen),
    .pq_pri_wdata_i                 (pq_pri_wdata),
    .pq_pri_full_o                  (pq_pri_full),

    .up_ad_wen_i                    (up_ad_wen),
    .up_ad_wdata_i                  (up_ad_wdata),
    .up_ad_waddr_i                  (up_ad_waddr),

    //ace5-liteDVM
    .cq_acvalid_i                   (cq_acvalid),
    .cq_acready_o                   (cq_acready),
    .cq_acaddr_i                    (cq_acaddr),
    .cq_acvmidext_i                 (cq_acvmidext),
    .cq_acsnoop_i                   (cq_acsnoop),
    .cq_acprot_i                    (cq_acprot),
    .cq_crvalid_o                   (cq_crvalid),
    .cq_crready_i                   (cq_crready),
    .cq_crresp_o                    (cq_crresp),
    .cq_ardomain_o                  (cq_ardomain),
    .cq_arsnoop_o                   (cq_arsnoop),
    .cq_arbar_o                     (cq_arbar),
    //cq_ar/r
    .cq_arvalid_o                   (cq_arvalid),
    .cq_arready_i                   (cq_arready),
    .cq_arid_o                      (cq_arid),
    .cq_araddr_o                    (cq_araddr),
    .cq_arlen_o                     (cq_arlen),
    .cq_rvalid_i                    (cq_rvalid),
    .cq_rready_o                    (cq_rready),
    .cq_rlast_i                     (cq_rlast),
    .cq_rid_i                       (cq_rid),
    .cq_rresp_i                     (cq_rresp),
    .cq_rdata_i                     (cq_rdata),

    //fq_aw/w
    .fq_awvalid_o                   (fq_awvalid),
    .fq_awready_i                   (fq_awready),
    .fq_awid_o                      (fq_awid),
    .fq_awaddr_o                    (fq_awaddr),
    .fq_awlen_o                     (fq_awlen),
    .fq_wvalid_o                    (fq_wvalid),
    .fq_wready_i                    (fq_wready),
    .fq_wlast_o                     (fq_wlast),
    .fq_wstrb_o                     (fq_wstrb),
    .fq_wdata_o                     (fq_wdata),
    .fq_bvalid_i                    (fq_bvalid),
    .fq_bready_o                    (fq_bready),
    .fq_bid_i                       (fq_bid),
    .fq_bresp_i                     (fq_bresp),

    //pq_aw/w
    .pq_awvalid_o                   (pq_awvalid),
    .pq_awready_i                   (pq_awready),
    .pq_awid_o                      (pq_awid),
    .pq_awaddr_o                    (pq_awaddr),
    .pq_awlen_o                     (pq_awlen),
    .pq_wvalid_o                    (pq_wvalid),
    .pq_wready_i                    (pq_wready),
    .pq_wlast_o                     (pq_wlast),
    .pq_wstrb_o                     (pq_wstrb),
    .pq_wdata_o                     (pq_wdata),
    .pq_bvalid_i                    (pq_bvalid),
    .pq_bready_o                    (pq_bready),
    .pq_bid_i                       (pq_bid),
    .pq_bresp_i                     (pq_bresp),

    //msi_aw/w
    .msi_awatop_o                   (msi_awatop),
    .msi_awvalid_o                  (msi_awvalid),
    .msi_awready_i                  (msi_awready),
    .msi_awid_o                     (msi_awid),
    .msi_awaddr_o                   (msi_awaddr),
    .msi_awlen_o                    (msi_awlen),
    .msi_awsize_o                   (msi_awsize),
    .msi_awcache_o                  (msi_awcache),
    .msi_wvalid_o                   (msi_wvalid),
    .msi_wready_i                   (msi_wready),
    .msi_wlast_o                    (msi_wlast),
    .msi_wstrb_o                    (msi_wstrb),
    .msi_wdata_o                    (msi_wdata),
    .msi_bvalid_i                   (msi_bvalid),
    .msi_bready_o                   (msi_bready),
    .msi_bid_i                      (msi_bid),
    .msi_bresp_i                    (msi_bresp)
);


iommu_atd_ds u4_iommu_atd_ds
(
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .cdw_arvalid                    (cdw_arvalid),
    .cdw_arready                    (cdw_arready),
    .cdw_arid                       (cdw_arid),
    .cdw_araddr                     (cdw_araddr),
    .cdw_arlen                      (cdw_arlen),
    .cdw_rvalid                     (cdw_rvalid),
    .cdw_rready                     (cdw_rready),
    .cdw_rlast                      (cdw_rlast),
    .cdw_rid                        (cdw_rid),
    .cdw_rresp                      (cdw_rresp),
    .cdw_rdata                      (cdw_rdata),

    .ptw_arvalid                    (ptw_arvalid),
    .ptw_arready                    (ptw_arready),
    .ptw_arid                       (ptw_arid),
    .ptw_araddr                     (ptw_araddr),
    .ptw_arlen                      (ptw_arlen),
    .ptw_rvalid                     (ptw_rvalid),
    .ptw_rready                     (ptw_rready),
    .ptw_rlast                      (ptw_rlast),
    .ptw_rid                        (ptw_rid),
    .ptw_rresp                      (ptw_rresp),
    .ptw_rdata                      (ptw_rdata),

    //ace5-liteDVM
    .cq_acvalid                     (cq_acvalid),
    .cq_acready                     (cq_acready),
    .cq_acaddr                      (cq_acaddr),
    .cq_acvmidext                   (cq_acvmidext),
    .cq_acsnoop                     (cq_acsnoop),
    .cq_acprot                      (cq_acprot),
    .cq_crvalid                     (cq_crvalid),
    .cq_crready                     (cq_crready),
    .cq_crresp                      (cq_crresp),
    .cq_ardomain                    (cq_ardomain),
    .cq_arsnoop                     (cq_arsnoop),
    .cq_arbar                       (cq_arbar),

    .cq_arvalid                     (cq_arvalid),
    .cq_arready                     (cq_arready),
    .cq_arid                        (cq_arid),
    .cq_araddr                      (cq_araddr),
    .cq_arlen                       (cq_arlen),
    .cq_rvalid                      (cq_rvalid),
    .cq_rready                      (cq_rready),
    .cq_rlast                       (cq_rlast),
    .cq_rid                         (cq_rid),
    .cq_rresp                       (cq_rresp),
    .cq_rdata                       (cq_rdata),

    .fq_awvalid                     (fq_awvalid),
    .fq_awready                     (fq_awready),
    .fq_awid                        (fq_awid),
    .fq_awaddr                      (fq_awaddr),
    .fq_awlen                       (fq_awlen),
    .fq_wvalid                      (fq_wvalid),
    .fq_wready                      (fq_wready),
    .fq_wlast                       (fq_wlast),
    .fq_wstrb                       (fq_wstrb),
    .fq_wdata                       (fq_wdata),
    .fq_bvalid                      (fq_bvalid),
    .fq_bready                      (fq_bready),
    .fq_bid                         (fq_bid),
    .fq_bresp                       (fq_bresp),

    .pq_awvalid                     (pq_awvalid),
    .pq_awready                     (pq_awready),
    .pq_awid                        (pq_awid),
    .pq_awaddr                      (pq_awaddr),
    .pq_awlen                       (pq_awlen),
    .pq_wvalid                      (pq_wvalid),
    .pq_wready                      (pq_wready),
    .pq_wlast                       (pq_wlast),
    .pq_wstrb                       (pq_wstrb),
    .pq_wdata                       (pq_wdata),
    .pq_bvalid                      (pq_bvalid),
    .pq_bready                      (pq_bready),
    .pq_bid                         (pq_bid),
    .pq_bresp                       (pq_bresp),

    .msi_awatop                     (msi_awatop),
    .msi_awvalid                    (msi_awvalid),
    .msi_awready                    (msi_awready),
    .msi_awid                       (msi_awid),
    .msi_awaddr                     (msi_awaddr),
    .msi_awlen                      (msi_awlen),
    .msi_awsize                     (msi_awsize),
    .msi_awcache                    (msi_awcache),
    .msi_wvalid                     (msi_wvalid),
    .msi_wready                     (msi_wready),
    .msi_wlast                      (msi_wlast),
    .msi_wstrb                      (msi_wstrb),
    .msi_wdata                      (msi_wdata),
    .msi_bvalid                     (msi_bvalid),
    .msi_bready                     (msi_bready),
    .msi_bid                        (msi_bid),
    .msi_bresp                      (msi_bresp),

    .ptw_awvalid                    (ptw_awvalid),
    .ptw_awready                    (ptw_awready),
    .ptw_awid                       (ptw_awid),
    .ptw_awaddr                     (ptw_awaddr),
    .ptw_awlen                      (ptw_awlen),
    .ptw_wvalid                     (ptw_wvalid),
    .ptw_wready                     (ptw_wready),
    .ptw_wlast                      (ptw_wlast),
    .ptw_wstrb                      (ptw_wstrb),
    .ptw_wdata                      (ptw_wdata),
    .ptw_bvalid                     (ptw_bvalid),
    .ptw_bready                     (ptw_bready),
    .ptw_bid                        (ptw_bid),
    .ptw_bresp                      (ptw_bresp),

    //ace5-liteDVM
    .ds_ace5_acvalid                (atd_ace5_acvalid),
    .ds_ace5_acready                (atd_ace5_acready),
    .ds_ace5_acaddr                 (atd_ace5_acaddr),
    .ds_ace5_acvmidext              (atd_ace5_acvmidext),
    .ds_ace5_acsnoop                (atd_ace5_acsnoop),
    .ds_ace5_acprot                 (atd_ace5_acprot),
    .ds_ace5_crvalid                (atd_ace5_crvalid),
    .ds_ace5_crready                (atd_ace5_crready),
    .ds_ace5_crresp                 (atd_ace5_crresp),

    .ds_ace5_ardomain               (atd_ace5_ardomain),
    .ds_ace5_arsnoop                (atd_ace5_arsnoop),
    .ds_ace5_arbar                  (atd_ace5_arbar),
    .ds_ace5_awatop                 (atd_ace5_awatop),
    .ds_ace5_awdomain               (atd_ace5_awdomain),

    //pte_axi4
    .ds_ace5_arvalid                (atd_ace5_arvalid),
    .ds_ace5_arready                (atd_ace5_arready),
    .ds_ace5_arid                   (atd_ace5_arid),
    .ds_ace5_araddr                 (atd_ace5_araddr),
    .ds_ace5_arlen                  (atd_ace5_arlen),
    .ds_ace5_arsize                 (atd_ace5_arsize),
    .ds_ace5_arburst                (atd_ace5_arburst),
    .ds_ace5_arlock                 (atd_ace5_arlock),
    .ds_ace5_arcache                (atd_ace5_arcache),
    .ds_ace5_arprot                 (atd_ace5_arprot),
    .ds_ace5_arqos                  (atd_ace5_arqos),
    .ds_ace5_rvalid                 (atd_ace5_rvalid),
    .ds_ace5_rready                 (atd_ace5_rready),
    .ds_ace5_rlast                  (atd_ace5_rlast),
    .ds_ace5_rid                    (atd_ace5_rid),
    .ds_ace5_rresp                  (atd_ace5_rresp),
    .ds_ace5_rdata                  (atd_ace5_rdata),

    .ds_ace5_awvalid                (atd_ace5_awvalid),
    .ds_ace5_awready                (atd_ace5_awready),
    .ds_ace5_awid                   (atd_ace5_awid),
    .ds_ace5_awaddr                 (atd_ace5_awaddr),
    .ds_ace5_awlen                  (atd_ace5_awlen),
    .ds_ace5_awsize                 (atd_ace5_awsize),
    .ds_ace5_awburst                (atd_ace5_awburst),
    .ds_ace5_awlock                 (atd_ace5_awlock),
    .ds_ace5_awcache                (atd_ace5_awcache),
    .ds_ace5_awprot                 (atd_ace5_awprot),
    .ds_ace5_awqos                  (atd_ace5_awqos),
    .ds_ace5_wvalid                 (atd_ace5_wvalid),
    .ds_ace5_wready                 (atd_ace5_wready),
    .ds_ace5_wlast                  (atd_ace5_wlast),
    .ds_ace5_wstrb                  (atd_ace5_wstrb),
    .ds_ace5_wdata                  (atd_ace5_wdata),
    .ds_ace5_bvalid                 (atd_ace5_bvalid),
    .ds_ace5_bready                 (atd_ace5_bready),
    .ds_ace5_bid                    (atd_ace5_bid),
    .ds_ace5_bresp                  (atd_ace5_bresp)
);


endmodule
//# Disabled verilator_lint_on WIDTH


