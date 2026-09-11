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

module iommu_atd_sw #(
//    parameter RISCV_VLEN          = 6'd39,//39
    parameter RISCV_GLEN            = 6'd41,//39
    parameter RISCV_PLEN            = 6'd56
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

    input                           atd_ipsr_pmip_i,
    //cq to atd-cache
    output                          cq_atd_fifo_wen_o,
    output  [127:0]                 cq_atd_fifo_din_o,
    input                           cq_atd_fifo_full_i,
    output                          cq_atd_fence_valid_o,
    input                           cq_atd_fence_ready_i,

    //cq to atd_tc
    output                          cq_acd_fifo_wen_o,
    output  [127:0]                 cq_acd_fifo_din_o,
    input                           cq_acd_fifo_full_i,
    input                           cq_acd_fence_ready_i,
    input                           cq_acd_dvm_ready_i,
    //from pcie-rp r2t
    input                           cq_ats_fifo_full_i,
    input                           cq_atc_fence_ready_i,

    //from atd_tc to cq
    input                           fq_acd_valid_i,
    input   [255:0]                 fq_acd_record_i,
    output                          fq_acd_ready_o,

    //from CDW
    input                           fq_cdw_valid_i,
    input   [255:0]                 fq_cdw_record_i,
    output                          fq_cdw_ready_o,

    //from PTW
    input                           fq_ptw_valid_i,
    input   [255:0]                 fq_ptw_record_i,
    output                          fq_ptw_ready_o,

    //from cdw
    input                           pq_pri_wen_i,
    input   [127:0]                 pq_pri_wdata_i,
    output                          pq_pri_full_o,
    //update a/d bit from ptw
    input                           up_ad_wen_i,
    input  [7:0]                    up_ad_wdata_i,
    input  [52:0]                   up_ad_waddr_i,

    //ace5-liteDVM
    input                           cq_acvalid_i,
    output                          cq_acready_o,
    input  [52-1:0]                 cq_acaddr_i,
    input  [3:0]                    cq_acvmidext_i,
    input  [3:0]                    cq_acsnoop_i,
    input  [2:0]                    cq_acprot_i,
    output                          cq_crvalid_o,
    input                           cq_crready_i,
    output  [4:0]                   cq_crresp_o,
    output  [1:0]                   cq_ardomain_o,
    output  [3:0]                   cq_arsnoop_o,
    output  [1:0]                   cq_arbar_o,
    //cq_ar/r
    output                          cq_arvalid_o,
    input                           cq_arready_i,
    output  [3:0]                   cq_arid_o,
    output  [RISCV_PLEN-1:0]        cq_araddr_o,
    output  [7:0]                   cq_arlen_o,
    input                           cq_rvalid_i,
    output                          cq_rready_o,
    input                           cq_rlast_i,
    input   [3:0]                   cq_rid_i,
    input   [1:0]                   cq_rresp_i,
    input   [255:0]                 cq_rdata_i,

    //fq_aw/w
    output                          fq_awvalid_o,
    input                           fq_awready_i,
    output  [3:0]                   fq_awid_o,
    output  [RISCV_PLEN-1:0]        fq_awaddr_o,
    output  [7:0]                   fq_awlen_o,
    output                          fq_wvalid_o,
    input                           fq_wready_i,
    output                          fq_wlast_o,
    output  [31:0]                  fq_wstrb_o,
    output  [255:0]                 fq_wdata_o,

    input                           fq_bvalid_i,
    output                          fq_bready_o,
    input   [3:0]                   fq_bid_i,
    input   [1:0]                   fq_bresp_i,

    //pq_aw/w
    output                          pq_awvalid_o,
    input                           pq_awready_i,
    output  [3:0]                   pq_awid_o,
    output  [RISCV_PLEN-1:0]        pq_awaddr_o,
    output  [7:0]                   pq_awlen_o,
    output                          pq_wvalid_o,
    input                           pq_wready_i,
    output                          pq_wlast_o,
    output  [31:0]                  pq_wstrb_o,
    output  [255:0]                 pq_wdata_o,

    input                           pq_bvalid_i,
    output                          pq_bready_o,
    input   [3:0]                   pq_bid_i,
    input   [1:0]                   pq_bresp_i,

    //msi_aw/w
    output  [5:0]                   msi_awatop_o,
    output                          msi_awvalid_o,
    input                           msi_awready_i,
    output  [3:0]                   msi_awid_o,
    output  [RISCV_PLEN-1:0]        msi_awaddr_o,
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
    input   [3:0]                   msi_bid_i,
    input   [1:0]                   msi_bresp_i
);


    //reg cfg
    logic   [31:0]                  iommu_cqh;
    logic   [31:0]                  iommu_fqt;
    logic   [31:0]                  iommu_pqt;
    logic   [5:0]                   iommu_cqcsr;
    logic   [3:0]                   iommu_fqcsr;
    logic   [3:0]                   iommu_pqcsr;
    logic   [3:0]                   iommu_ipsr;

    logic   [4:0]                   iommu_cqb_size;
    logic   [43:0]                  iommu_cqb_ppn;
    logic   [31:0]                  iommu_cqt;
    logic   [4:0]                   iommu_fqb_size;
    logic   [43:0]                  iommu_fqb_ppn;
    logic   [31:0]                  iommu_fqh;
    logic   [4:0]                   iommu_pqb_size;
    logic   [43:0]                  iommu_pqb_ppn;
    logic   [31:0]                  iommu_pqh;
    logic   [5:0]                   iommu_cqcsr_r;
    logic   [3:0]                   iommu_fqcsr_r;
    logic   [3:0]                   iommu_pqcsr_r;
    logic   [3:0]                   iommu_ipsr_r;
    logic   [3:0]                   iommu_ipsr_hw2sw;
    logic                           iommu_ipsr_cip;
    logic                           iommu_ipsr_fip;
    logic                           iommu_ipsr_pip;

    logic  [15:0]                   iommu_icvec;
    logic  [15:0]                   iommu_msi_mask;
    logic  [55:0]                   iommu_msi_addr[15:0];
    logic  [31:0]                   iommu_msi_data[15:0];

    //from cq
    logic                           cq_iofence_wvalid;
    logic [63:0]                    cq_iofence_waddr;
    logic [31:0]                    cq_iofence_wdata;
    logic                           cq_iofence_wready;

assign iommu_ipsr_r = {iommu_ipsr_pip,atd_ipsr_pmip_i,iommu_ipsr_fip,iommu_ipsr_cip};

iommu_sw_apb u0_iommu_sw_apb
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

    .iommu_cqh_i                    (iommu_cqh),
    .iommu_fqt_i                    (iommu_fqt),
    .iommu_pqt_i                    (iommu_pqt),
    .iommu_cqcsr_i                  (iommu_cqcsr_r),
    .iommu_fqcsr_i                  (iommu_fqcsr_r),
    .iommu_pqcsr_i                  (iommu_pqcsr_r),
    .iommu_ipsr_i                   (iommu_ipsr_hw2sw),

    .iommu_cqb_size_o               (iommu_cqb_size     ),
    .iommu_cqb_ppn_o                (iommu_cqb_ppn      ),
    .iommu_cqt_o                    (iommu_cqt          ),
    .iommu_fqb_size_o               (iommu_fqb_size     ),
    .iommu_fqb_ppn_o                (iommu_fqb_ppn      ),
    .iommu_fqh_o                    (iommu_fqh          ),
    .iommu_pqb_size_o               (iommu_pqb_size     ),
    .iommu_pqb_ppn_o                (iommu_pqb_ppn      ),
    .iommu_pqh_o                    (iommu_pqh          ),
    .iommu_cqcsr_o                  (iommu_cqcsr        ),
    .iommu_fqcsr_o                  (iommu_fqcsr        ),
    .iommu_pqcsr_o                  (iommu_pqcsr        ),
    .iommu_ipsr_o                   (iommu_ipsr         ),
    .iommu_icvec_o                  (iommu_icvec        ),

    .iommu_msi_mask_o               (iommu_msi_mask     ),
    .iommu_msi_addr_o               (iommu_msi_addr ),
    .iommu_msi_data_o               (iommu_msi_data )
    );



iommu_atd_cq
    #(
    .RISCV_PLEN                     (RISCV_PLEN)
)
u1_iommu_atd_cq
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .iommu_cqb_ppn_i                (iommu_cqb_ppn),
    .iommu_cqb_size_i               (iommu_cqb_size),
    .iommu_cqt_i                    (iommu_cqt),

    .iommu_cqcsr_i                  (iommu_cqcsr),
    .iommu_cqcsr_o                  (iommu_cqcsr_r),
    .iommu_cip_clr_i                (iommu_ipsr[0]),
    .iommu_cip_en_o                 (iommu_ipsr_cip),
    .iommu_cqh_o                    (iommu_cqh),

    //to atd-cache
    .cq_atd_fifo_wen_o              (cq_atd_fifo_wen_o),
    .cq_atd_fifo_din_o              (cq_atd_fifo_din_o),
    .cq_atd_fifo_full_i             (cq_atd_fifo_full_i),
    .cq_atd_fence_valid_o           (cq_atd_fence_valid_o),
    .cq_atd_fence_ready_i           (cq_atd_fence_ready_i),

    //to atd_dti to acd
    .cq_acd_fifo_wen_o              (cq_acd_fifo_wen_o),
    .cq_acd_fifo_din_o              (cq_acd_fifo_din_o),
    .cq_acd_fifo_full_i             (cq_acd_fifo_full_i),
    .cq_acd_fence_ready_i           (cq_acd_fence_ready_i),
    .cq_acd_dvm_ready_i             (cq_acd_dvm_ready_i),

    .cq_ats_fifo_full_i             (cq_ats_fifo_full_i),
    .cq_atc_fence_ready_i           (cq_atc_fence_ready_i),

    //to msi
    .cq_iofence_wvalid_o            (cq_iofence_wvalid),
    .cq_iofence_waddr_o             (cq_iofence_waddr),
    .cq_iofence_wdata_o             (cq_iofence_wdata),
    .cq_iofence_wready_i            (cq_iofence_wready),

    //ace5-liteDVM
    .cq_acvalid_i                   (cq_acvalid_i),
    .cq_acready_o                   (cq_acready_o),
    .cq_acaddr_i                    (cq_acaddr_i),
    .cq_acvmidext_i                 (cq_acvmidext_i),
    .cq_acsnoop_i                   (cq_acsnoop_i),
    .cq_acprot_i                    (cq_acprot_i),
    .cq_crvalid_o                   (cq_crvalid_o),
    .cq_crready_i                   (cq_crready_i),
    .cq_crresp_o                    (cq_crresp_o),
    .cq_ardomain_o                  (cq_ardomain_o),
    .cq_arsnoop_o                   (cq_arsnoop_o),
    .cq_arbar_o                     (cq_arbar_o),

    .cq_arvalid_o                   (cq_arvalid_o),
    .cq_arready_i                   (cq_arready_i),
    .cq_arid_o                      (cq_arid_o),
    .cq_araddr_o                    (cq_araddr_o),
    .cq_arlen_o                     (cq_arlen_o),
    .cq_rvalid_i                    (cq_rvalid_i),
    .cq_rready_o                    (cq_rready_o),
    .cq_rlast_i                     (cq_rlast_i),
    .cq_rid_i                       (cq_rid_i),
    .cq_rresp_i                     (cq_rresp_i),
    .cq_rdata_i                     (cq_rdata_i)
);


iommu_atd_fq
    #(
//    .RISCV_VLEN                       (RISCV_VLEN),
    .RISCV_GLEN                     (RISCV_GLEN),
    .RISCV_PLEN                     (RISCV_PLEN)
)
u2_iommu_atd_fq
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .iommu_fqb_ppn_i                (iommu_fqb_ppn),
    .iommu_fqb_size_i               (iommu_fqb_size),
    .iommu_fqh_i                    (iommu_fqh),

    .iommu_fqcsr_i                  (iommu_fqcsr),
    .iommu_fqcsr_o                  (iommu_fqcsr_r),
    .iommu_fip_clr_i                (iommu_ipsr[1]),
    .iommu_fip_en_o                 (iommu_ipsr_fip),
    .iommu_fqt_o                    (iommu_fqt),

    //from ACD
    .fq_acd_valid_i                 (fq_acd_valid_i),
    .fq_acd_record_i                (fq_acd_record_i),
    .fq_acd_ready_o                 (fq_acd_ready_o),

    //from CDW
    .fq_cdw_valid_i                 (fq_cdw_valid_i),
    .fq_cdw_record_i                (fq_cdw_record_i),
    .fq_cdw_ready_o                 (fq_cdw_ready_o),

    //from PTW
    .fq_ptw_valid_i                 (fq_ptw_valid_i),
    .fq_ptw_record_i                (fq_ptw_record_i),
    .fq_ptw_ready_o                 (fq_ptw_ready_o),

    //fq
    .fq_awvalid_o                   (fq_awvalid_o),
    .fq_awready_i                   (fq_awready_i),
    .fq_awid_o                      (fq_awid_o),
    .fq_awaddr_o                    (fq_awaddr_o),
    .fq_awlen_o                     (fq_awlen_o),
    .fq_wvalid_o                    (fq_wvalid_o),
    .fq_wready_i                    (fq_wready_i),
    .fq_wlast_o                     (fq_wlast_o),
    .fq_wstrb_o                     (fq_wstrb_o),
    .fq_wdata_o                     (fq_wdata_o),
    .fq_bvalid_i                    (fq_bvalid_i),
    .fq_bready_o                    (fq_bready_o),
    .fq_bid_i                       (fq_bid_i),
    .fq_bresp_i                     (fq_bresp_i)
);


iommu_atd_pq
    #(
    .RISCV_PLEN                     (RISCV_PLEN)
)
u3_iommu_atd_pq
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .iommu_pqb_ppn_i                (iommu_pqb_ppn),
    .iommu_pqb_size_i               (iommu_pqb_size),
    .iommu_pqh_i                    (iommu_pqh),
    .iommu_pqcsr_i                  (iommu_pqcsr),
    .iommu_pqcsr_o                  (iommu_pqcsr_r),
    .iommu_pip_clr_i                (iommu_ipsr[3]),
    .iommu_pip_en_o                 (iommu_ipsr_pip),
    .iommu_pqt_o                    (iommu_pqt),

    //from cdw
    .pq_pri_wen_i                   (pq_pri_wen_i),
    .pq_pri_wdata_i                 (pq_pri_wdata_i),
    .pq_pri_full_o                  (pq_pri_full_o),

    //pq
    .pq_awvalid_o                   (pq_awvalid_o),
    .pq_awready_i                   (pq_awready_i),
    .pq_awid_o                      (pq_awid_o),
    .pq_awaddr_o                    (pq_awaddr_o),
    .pq_awlen_o                     (pq_awlen_o),
    .pq_wvalid_o                    (pq_wvalid_o),
    .pq_wready_i                    (pq_wready_i),
    .pq_wlast_o                     (pq_wlast_o),
    .pq_wstrb_o                     (pq_wstrb_o),
    .pq_wdata_o                     (pq_wdata_o),
    .pq_bvalid_i                    (pq_bvalid_i),
    .pq_bready_o                    (pq_bready_o),
    .pq_bid_i                       (pq_bid_i),
    .pq_bresp_i                     (pq_bresp_i)
);


iommu_atd_msi u4_iommu_atd_msi
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .iommu_icvec_i                  (iommu_icvec),
    .iommu_msi_mask_i               (iommu_msi_mask),
    .iommu_msi_addr_i               (iommu_msi_addr),
    .iommu_msi_data_i               (iommu_msi_data),

    // Interrupt pending bits
    .msi_intp_i                     (iommu_ipsr_r),
    .ipsr_hw2sw_o                   (iommu_ipsr_hw2sw),

    //from cq
    .cq_iofence_wvalid_i            (cq_iofence_wvalid),
    .cq_iofence_waddr_i             (cq_iofence_waddr),
    .cq_iofence_wdata_i             (cq_iofence_wdata),
    .cq_iofence_wready_o            (cq_iofence_wready),

    .up_ad_wen_i                    (up_ad_wen_i),
    .up_ad_wdata_i                  (up_ad_wdata_i),
    .up_ad_waddr_i                  (up_ad_waddr_i),
    //msi
    .msi_awatop_o                   (msi_awatop_o),
    .msi_awvalid_o                  (msi_awvalid_o),
    .msi_awready_i                  (msi_awready_i),
    .msi_awid_o                     (msi_awid_o),
    .msi_awaddr_o                   (msi_awaddr_o),
    .msi_awlen_o                    (msi_awlen_o),
    .msi_awsize_o                   (msi_awsize_o),
    .msi_awcache_o                  (msi_awcache_o),
    .msi_wvalid_o                   (msi_wvalid_o),
    .msi_wready_i                   (msi_wready_i),
    .msi_wlast_o                    (msi_wlast_o),
    .msi_wstrb_o                    (msi_wstrb_o),
    .msi_wdata_o                    (msi_wdata_o),
    .msi_bvalid_i                   (msi_bvalid_i),
    .msi_bready_o                   (msi_bready_o),
    .msi_bid_i                      (msi_bid_i),
    .msi_bresp_i                    (msi_bresp_i)
    );


endmodule



