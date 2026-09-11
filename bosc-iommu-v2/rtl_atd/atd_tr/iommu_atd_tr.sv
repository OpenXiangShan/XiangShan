///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_tr.sv
//Version       :   2.0
//Author        :   yangyy
//Description   :
//      xxx     :
//      xxxx    :
//      xx      :
//Increase: 2.1
//
//          2.2
///*********************************************//

module iommu_atd_tr
    (
    input logic                     iommu_clk,
    input logic                     iommu_rstn,

    ////////**********************************************************************************************************///////
    //receive pcie-rp
    ////////**********************************************************************************************************///////
    //input from pcie-rp
    input                           atd_r2t_rvalid_i,
    output                          atd_r2t_rready_o,
    input   [63:0]                  atd_r2t_rdata_i,
    input   [7:0]                   atd_r2t_rstrb_i,
    input   [7:0]                   atd_r2t_rkeep_i,
    input                           atd_r2t_rlast_i,
    input   [3:0]                   atd_r2t_rid_i,

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

    //DTI_ATS_SYNC_ACK//pulse signal to cq
    input                           ats_sync_req_i,
    output                          ats_sync_ack_o,

    //ats_ptw_result:ptw -> pcie-rc
    input                           ats_resp_wen_i,
    input   [113:0]                 ats_resp_wdata_i,
    output                          ats_resp_full_o,

    //DTI_ATS_TRANS_FAULT
    input                           ats_fault_wen_i,
    input   [15:0]                  ats_fault_wdata_i,
    output                          ats_fault_full_o,

    //DTI_ATS_INV_REQ
    //flush/invalidation: cq -> pcie-rc -> atc
    input                           ats_inv_wen_i,
    input   [127:0]                 ats_inv_wdata_i,
    output                          ats_inv_full_o,

    //DTI_ATS_PAGE_RESP
    //ats_pq_result:pq -> pcie-rc
    input                           pri_resp_wen_i,
    input   [79:0]                  pri_resp_wdata_i,
    output                          pri_resp_full_o,

    //output to pcie-rc
    output                          atd_t2r_tvalid_o,
    input                           atd_t2r_tready_i,
    output   [63:0]                 atd_t2r_tdata_o,
    output   [7:0]                  atd_t2r_tstrb_o,
    output   [7:0]                  atd_t2r_tkeep_o,
    output                          atd_t2r_tlast_o,
    output   [3:0]                  atd_t2r_tid_o
);

    logic                           atd_c2t_connected;
    logic    [7:0]                  atd_r2t_tok_trans_req;

    logic                           atd_fq_ack_ready;
    logic                           acd_fq_valid;
    logic                           ats_inv_ack;
    logic                           ats_sync_ack;
    logic                           ats_page_respack;
    logic                           pri_req_wen;


assign pri_req_wen_o = pri_req_wen;
assign ats_sync_ack_o = ats_sync_ack;
////////**********************************************************************************************************///////
//module inst
////////**********************************************************************************************************///////
iommu_atd_r2t u0_iommu_atd_r2t
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),
    //input from acd
    .atd_r2t_rvalid_i               (atd_r2t_rvalid_i),
    .atd_r2t_rready_o               (atd_r2t_rready_o),
    .atd_r2t_rdata_i                (atd_r2t_rdata_i),
    .atd_r2t_rstrb_i                (atd_r2t_rstrb_i),
    .atd_r2t_rkeep_i                (atd_r2t_rkeep_i),
    .atd_r2t_rlast_i                (atd_r2t_rlast_i),
    .atd_r2t_rid_i                  (atd_r2t_rid_i  ),

    .atd_r2t_connected_o            (atd_r2t_connected),
    .atd_r2t_tok_trans_req_o        (atd_r2t_tok_trans_req),

    .ats_req_wen_o                  (ats_req_wen_o  ),
    .ats_req_wdata_o                (ats_req_wdata_o),
    .ats_req_full_i                 (ats_req_full_i ),

    .pri_req_wen_o                  (pri_req_wen    ),
    .pri_req_wdata_o                (pri_req_wdata_o),
    .pri_req_full_i                 (pri_req_full_i ),

    .ats_inv_ack_o                  (ats_inv_ack),
    .ats_sync_ack_o                 (ats_sync_ack),
    .ats_page_respack_o             (ats_page_respack)
);

iommu_atd_t2r u1_iommu_atd_t2r
    (
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn),

    //DTI_ATS_CONDIS_REQ
    .atd_t2r_connected_i            (atd_r2t_connected),
    .atd_t2r_tok_trans_req_i        (atd_r2t_tok_trans_req),

    .ats_resp_wen_i                 (ats_resp_wen_i),
    .ats_resp_wdata_i               (ats_resp_wdata_i),
    .ats_resp_full_o                (ats_resp_full_o),

    .ats_fault_wen_i                (ats_fault_wen_i),
    .ats_fault_wdata_i              (ats_fault_wdata_i),
    .ats_fault_full_o               (ats_fault_full_o),

    .ats_inv_wen_i                  (ats_inv_wen_i),
    .ats_inv_wdata_i                (ats_inv_wdata_i),
    .ats_inv_full_o                 (ats_inv_full_o),

    .ats_inv_ack_i                  (ats_inv_ack),
    .ats_sync_req_i                 (ats_sync_req_i),
    .ats_sync_ack_i                 (ats_sync_ack),

    .pri_resp_wen_i                 (pri_resp_wen_i),
    .pri_resp_wdata_i               (pri_resp_wdata_i),
    .pri_resp_full_o                (pri_resp_full_o),

    //DTI_ATS_PAGE_ACK//pulse
    //DTI_ATS_PAGE_RESPACK//pulse
    .ats_page_ack_i                 (pri_req_wen),
    .ats_page_respack_i             (ats_page_respack),

    //output to pcie-rp
    .atd_t2r_tvalid_o               (atd_t2r_tvalid_o   ),
    .atd_t2r_tready_i               (atd_t2r_tready_i   ),
    .atd_t2r_tdata_o                (atd_t2r_tdata_o    ),
    .atd_t2r_tstrb_o                (atd_t2r_tstrb_o    ),
    .atd_t2r_tkeep_o                (atd_t2r_tkeep_o    ),
    .atd_t2r_tlast_o                (atd_t2r_tlast_o    ),
    .atd_t2r_tid_o                  (atd_t2r_tid_o      )
);


endmodule



