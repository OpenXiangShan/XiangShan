///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_tc.sv
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


module iommu_atd_tc
    (
    input logic                     iommu_clk,
    input logic                     iommu_rstn,

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
    input                           atd_pmip_clr_i, 
    output                          atd_ipsr_pmip_o,
    ////////**********************************************************************************************************///////
    //receive acd
    ////////**********************************************************************************************************///////
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

    //acd:ptw_req output to cdw
    output                          cdw_rfifo_wen_o,
    output  [127:0]                 cdw_rfifo_wdata_o,
    input                           cdw_rfifo_full_i,

    //acd_fault -> cdw
    output                          acd_fq_valid_o,
    output  [255:0]                 acd_fq_record_o,
    input                           acd_fq_ready_i,

    ////////**********************************************************************************************************///////
    //transmit to acd
    ////////**********************************************************************************************************///////
    //flush/invalidation: cq -> acd
    input                           cq_wfifo_wen_i,
    input   [127:0]                 cq_wfifo_wdata_i,
    output                          cq_wfifo_full_o,
    output                          cq_fence_ready_o,
    output                          cq_dvm_ready_o,

    //ptw_ack/ptw_result:ptw -> acd
    input                           ptw_wfifo_wen_i,
    input   [319:0]                 ptw_wfifo_wdata_i,
    output                          ptw_wfifo_full_o,

    //output to acd
    output                          atd_t2c_tvalid_o,
    input                           atd_t2c_tready_i,
    output   [63:0]                 atd_t2c_tdata_o,
    output   [7:0]                  atd_t2c_tstrb_o,
    output   [7:0]                  atd_t2c_tkeep_o,
    output                          atd_t2c_tlast_o,
    output   [3:0]                  atd_t2c_tid_o,
    output   [3:0]                  atd_t2c_tdest_o,
    output                          atd_t2c_tuser_o
);

    logic                           iommu_tr_req_busy;
    logic   [63:0]                  iommu_tr_resp;
    logic                           iommu_tr_req_en;
    logic   [51:0]                  iommu_tr_req_iova;
    logic   [63:0]                  iommu_tr_req_ctl;

    logic   [31:0]                  iommu_acd_cfg_rdata;
    logic   [30:0]                  iommu_acd_iocntovf;             
    logic                           iommu_acd_cfg_wvalid;
    logic   [63:0]                  iommu_acd_cfg;

    logic                           atd_c2t_connected;
    logic                           atd_c2t_cq_ack;
    logic                           acd_fq_valid;
    logic                           atd_c2t_cfg_end;
    logic                           atd_c2t_dbg_end;
    logic                           atd_c2t_cfg_ack;
    logic                           atd_c2t_dbg_ack;

    assign  atd_ipsr_pmip_o = |iommu_acd_iocntovf || |atd_iocntovf_i;
////////**********************************************************************************************************///////
//module inst
////////**********************************************************************************************************///////
iommu_tc_apb u0_iommu_tc_apb
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

    .atd_hpm_rvalid_i               (atd_hpm_rvalid_i),
    .atd_iocntovf_i                 (atd_iocntovf_i),
    .atd_hpmctr_i                   (atd_hpmctr_i),
    .atd_hpmevt_i                   (atd_hpmevt_i),

    .iommu_tr_resp_i                (iommu_tr_resp),//RO
    .iommu_tr_req_busy_i            (iommu_tr_req_busy  ),
    .iommu_tr_req_en_o              (iommu_tr_req_en),
    .iommu_tr_req_iova_o            (iommu_tr_req_iova  ),
    .iommu_tr_req_ctl_o             (iommu_tr_req_ctl   ),

    .iommu_acd_cfg_ack_i            (atd_c2t_cfg_ack  ),
    .iommu_acd_cfg_rdata_i          (iommu_acd_cfg_rdata),
    .iommu_acd_iocntovf_i           (iommu_acd_iocntovf),
    .iommu_acd_cfg_wvalid_o         (iommu_acd_cfg_wvalid),
    .iommu_acd_cfg_o                (iommu_acd_cfg      )
    );


iommu_atd_c2t u1_iommu_atd_c2t
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),
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

    .atd_c2t_connected_o            (atd_c2t_connected),
    .atd_c2t_cq_ack_o               (atd_c2t_cq_ack     ),

    .cdw_rfifo_wen_o                (cdw_rfifo_wen_o    ),
    .cdw_rfifo_wdata_o              (cdw_rfifo_wdata_o  ),
    .cdw_rfifo_full_i               (cdw_rfifo_full_i   ),

    .atd_c2t_fence_ready_o          (cq_fence_ready_o   ),
    .atd_c2t_dvm_ready_o            (cq_dvm_ready_o ),

    .acd_fq_valid_o                 (acd_fq_valid_o     ),
    .acd_fq_record_o                (acd_fq_record_o    ),
    .acd_fq_ready_i                 (acd_fq_ready_i     ),
    
    .atd_fq_ack_valid_o             (acd_fq_valid       ),
    .atd_c2t_dbg_end_i              (atd_c2t_dbg_end    ),
    .atd_c2t_dbg_ack_o              (atd_c2t_dbg_ack    ),

    .atd_c2t_cfg_end_i              (atd_c2t_cfg_end    ),
    .atd_c2t_cfg_ack_o              (atd_c2t_cfg_ack    ),
    .atd_c2t_cfg_rdata_o            (iommu_acd_cfg_rdata),
    .atd_c2t_iocntovf_o             (iommu_acd_iocntovf ),
    .atd_pmip_clr_i                 (atd_pmip_clr_i),

    .iommu_tr_req_go_i              (iommu_tr_req_ctl[0]),
    .iommu_tr_req_busy_o            (iommu_tr_req_busy),
    .iommu_tr_resp_o                (iommu_tr_resp)
);

iommu_atd_t2c u2_iommu_atd_t2c
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    .iommu_tr_req_en_i              (iommu_tr_req_en    ),
    .iommu_tr_req_iova_i            (iommu_tr_req_iova  ),
    .iommu_tr_req_ctl_i             (iommu_tr_req_ctl   ),
    .iommu_acd_cfg_wvalid_i         (iommu_acd_cfg_wvalid),
    .iommu_acd_cfg_i                (iommu_acd_cfg       ),

    .cq_wfifo_wen_i                 (cq_wfifo_wen_i     ),
    .cq_wfifo_wdata_i               (cq_wfifo_wdata_i   ),
    .cq_wfifo_full_o                (cq_wfifo_full_o    ),

    .ptw_wfifo_wen_i                (ptw_wfifo_wen_i    ),
    .ptw_wfifo_wdata_i              (ptw_wfifo_wdata_i  ),
    .ptw_wfifo_full_o               (ptw_wfifo_full_o   ),

    .atd_t2c_connected_i            (atd_c2t_connected  ),
    .atd_t2c_cq_ack_i               (atd_c2t_cq_ack     ),
    .atd_fq_ack_valid_i             (acd_fq_valid       ),
    .atd_t2c_cfg_end_o              (atd_c2t_cfg_end    ),
    .atd_t2c_dbg_end_o              (atd_c2t_dbg_end    ),
    .atd_t2c_cfg_ack_i              (atd_c2t_cfg_ack    ),
    .atd_t2c_dbg_ack_i              (atd_c2t_dbg_ack    ),

    .atd_t2c_tvalid_o               (atd_t2c_tvalid_o   ),
    .atd_t2c_tready_i               (atd_t2c_tready_i   ),
    .atd_t2c_tdata_o                (atd_t2c_tdata_o    ),
    .atd_t2c_tstrb_o                (atd_t2c_tstrb_o    ),
    .atd_t2c_tkeep_o                (atd_t2c_tkeep_o    ),
    .atd_t2c_tlast_o                (atd_t2c_tlast_o    ),
    .atd_t2c_tid_o                  (atd_t2c_tid_o      ),
    .atd_t2c_tdest_o                (atd_t2c_tdest_o    ),
    .atd_t2c_tuser_o                (atd_t2c_tuser_o    )
);




endmodule



