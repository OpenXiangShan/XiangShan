/////////////////////////////////////////////////////////////
// iommu_acd_cfg_ctrl_wrap
/////////////////////////////////////////////////////////////
module iommu_acd_cfg_ctrl_wrap #(
    parameter   TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter   BUS_INFLY_TOKEN_WIDTH       = iommu_acd_pkg::BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_INFLY_TOKEN_NUM         = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter type          TLBQ2INV_TYPE               = iommu_acd_pkg::tlbq2inv_t,
    parameter type          PTW_REQ_GRP_TYPE            = iommu_acd_pkg::ptw_req_grp_t,
    parameter   INV_IDX_WIDTH               = iommu_acd_pkg::INV_IDX_WIDTH,
    parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH,
    parameter   FAULT_TOKEN_WIDTH           = iommu_acd_pkg::FAULT_TOKEN_WIDTH,
    parameter   FAULT_INFLY_NUM             = 2**FAULT_TOKEN_WIDTH,
    parameter   PTW_IDX_WIDTH               = iommu_acd_pkg::PTW_IDX_WIDTH,
    parameter   PTW_INFLY_NUM               = 2**PTW_IDX_WIDTH,
    parameter   INTERNAL_INV_IDX_WIDTH      = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter type          PTW_REQ_TYPE                = iommu_acd_pkg::PTW_REQ_TYPE,
    parameter type          PTW_ACK_TYPE                = iommu_acd_pkg::PTW_ACK_TYPE,
    parameter   SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // PTW                                              
    input  logic                                        ptw_req_valid_i,
    output logic                                        ptw_req_ready_o,
    input  PTW_REQ_TYPE                                 ptw_req_i,
    output logic                                        ptw_ack_valid_o,
    output PTW_ACK_TYPE                                 ptw_ack_o,
    input  logic                                        msg_pvalid_i,
    output logic                                        msg_pready_o,
    input  logic [63:0]                                 msg_pwdata_i,
    output logic                                        msg_pvalid_o,
    input  logic                                        msg_pready_i,
    output logic [63:0]                                 msg_pdata_o,
    output logic                                        msg_plast_o,
    // INV                                              
    output logic                                        inv_tlbcache_req_valid_o,
    input  logic                                        inv_tlbcache_req_ready_i,
    output INVALID_REQ_TYPE                             inv_tlbcache_req_o,
    input  logic                                        inv_tlbcache_ack_valid_i,
    input  INVALID_REQ_TYPE                             inv_tlbcache_ack_i,
    input  logic                                        msg_ivalid_i,
    output logic                                        msg_iready_o,
    input  logic [63:0]                                 msg_iwdata_i,
    output logic                                        msg_ivalid_o,
    input  logic                                        msg_iready_i,
    output iommu_acd_pkg::MSG_INV_ACK_TYPE              msg_idata_o,
    output logic                                        msg_ilast_o,
    // Info from TLBQUEUE                               
    input  TLBQ2INV_TYPE                                qinfo2_inv_i,
    input  PTW_REQ_GRP_TYPE                             ptwreqinfo2_ptw_i,
    // FAULT                                            
    input  logic                                        fault_rpt_valid_i,
    output logic                                        fault_rpt_ready_o,
    input  iommu_acd_pkg::FAULT_RPT_TYPE                fault_rpt_i,
    input  logic                                        msg_fvalid_i,
    output logic                                        msg_fready_o,
    input  iommu_acd_pkg::MSG_FAULT_ACK_TYPE            msg_fwdata_i,
    output logic                                        msg_fvalid_o,
    input  logic                                        msg_fready_i,
    output logic [63:0]                                 msg_fdata_o,
    output logic                                        msg_flast_o,
    // CFG                                              
    output logic                                        iommu_fctl_gxl_o,
    output logic [3:0]                                  iommu_ddtp_iommu_mode_o,
    output logic                                        iommu_ipsr_pmip_clr_o,
    output logic                                        acd_fault_ctrl_multi_hit_check_en_o,
    output logic                                        acd_fault_ctrl_selfdefine_fault_rpt_en_o,
    input  logic                                        int_multi_hit_check_fail_i,
    input  logic                                        msg_cvalid_i,
    output logic                                        msg_cready_o,
    input  iommu_acd_pkg::MSG_CFG_ACCESS_TYPE           msg_cwdata_i,
    output logic                                        msg_rvalid_o,
    input  logic                                        msg_rready_i,
    output iommu_acd_pkg::MSG_CFG_ACK_TYPE              msg_rdata_o,
    output logic                                        msg_rlast_o,
//    output logic [63:12]                                tr_req_iova_vpn_o,
//    output logic [23:0]                                 tr_req_ctl_did_o,
//    output logic                                        tr_req_ctl_pv_o,
//    output logic [19:0]                                 tr_req_ctl_pid_o,
//    output logic                                        tr_req_ctl_nw_o,
//    output logic                                        tr_req_ctl_exe_o,
//    output logic                                        tr_req_ctl_priv_o,
//    output logic                                        tr_req_ctl_go_o,
//    input  logic                                        tr_req_finish_i,
//    input  logic [63:0]                                 tr_req_resp_i,
    output logic                                        iocountinh_o        [1:31],
    output logic [14:0]                                 iohpmevt_eventid_o  [1:31],
    output logic                                        iohpmevt_dmask_o    [1:31],
    output logic [19:0]                                 iohpmevt_pid_pscid_o[1:31],
    output logic [23:0]                                 iohpmevt_did_gscid_o[1:31],
    output logic                                        iohpmevt_pv_pscv_o  [1:31],
    output logic                                        iohpmevt_dv_gscv_o  [1:31],
    output logic                                        iohpmevt_idt_o      [1:31],
    output logic [63:0]                                 iohpmctr_counter_o  [1:31],
    input  logic                                        iohpmevt_of_i       [1:31],
    input  logic [63:0]                                 iohpmctr_counter_i  [1:31],
    // INTERRUPT                                        
    output logic                                        msg_nvalid_o,
    input  logic                                        msg_nready_i,
    output iommu_acd_pkg::MSG_INT_TYPE                  msg_ndata_o,
    output logic                                        msg_nlast_o,
    // Bus_handler
    input  logic [BUS_INFLY_TOKEN_NUM-1:0]              bh2inv_outstanding_list_i,
    input  logic [BUS_INFLY_TOKEN_NUM-1:0]              bh2inv_outstanding_rw_list_i,
    //                                                  
    input  logic [1:0]                                  trans_unit_ecc_err_i,
    //                                                  
    input  logic                                        spare_in
//}}}
);
//=== Declare === {{{
    logic                                               int_inv_buf_overflow;
    logic                                               int_fault_buf_overflow;
    logic                                               int_performance_buf_overflow;
    logic                                               int_inv_buf_overflow_en;
    logic                                               int_fault_buf_overflow_en;
    logic                                               int_performance_buf_overflow_en;
    logic                                               int_multi_hit_check_fail_en;

    logic [INV_INFLY_NUM-1:0]                           inv_ptwackfilter_req_valid;
    INVALID_REQ_TYPE [INV_INFLY_NUM-1:0]                inv_ptwackfilter_req;

    logic                                               ptw_ptw_ack_valid_o;
    PTW_ACK_TYPE                                        ptw_ptw_ack_o;
    PTW_REQ_TYPE                                        ptw_ptw_req_o;

    logic [31:1]                                        iocountovf_pos;

    logic [1:0]                                         cfg_ecc_err_o;
//}}}

//===
    assign int_performance_buf_overflow = 1'b0;

//=== CFG {{{
    iommu_acd_cfg #(
    /*parameter  */ .SPARE_PARAM                (0                                          ) // = 0
    ) U_cfg(
    /*input  logic                                      */  .clk                                        (clk                                        ),
    /*input  logic                                      */  .rstn                                       (rstn                                       ),
    /*output logic                                      */  .iommu_fctl_gxl_o                           (iommu_fctl_gxl_o                           ),
    /*output logic [3:0]                                */  .iommu_ddtp_iommu_mode_o                    (iommu_ddtp_iommu_mode_o                    ),
    /*output logic                                      */  .iommu_ipsr_pmip_clr_o                      (iommu_ipsr_pmip_clr_o                      ),
    /*output logic                                      */  .acd_fault_ctrl_multi_hit_check_en_o        (acd_fault_ctrl_multi_hit_check_en_o        ),
    /*output logic                                      */  .acd_fault_ctrl_selfdefine_fault_rpt_en_o   (acd_fault_ctrl_selfdefine_fault_rpt_en_o   ),
//    /*output logic [63:12]                              */  .tr_req_iova_vpn_o                          (tr_req_iova_vpn_o                          ),
//    /*output logic [23:0]                               */  .tr_req_ctl_did_o                           (tr_req_ctl_did_o                           ),
//    /*output logic                                      */  .tr_req_ctl_pv_o                            (tr_req_ctl_pv_o                            ),
//    /*output logic [19:0]                               */  .tr_req_ctl_pid_o                           (tr_req_ctl_pid_o                           ),
//    /*output logic                                      */  .tr_req_ctl_nw_o                            (tr_req_ctl_nw_o                            ),
//    /*output logic                                      */  .tr_req_ctl_exe_o                           (tr_req_ctl_exe_o                           ),
//    /*output logic                                      */  .tr_req_ctl_priv_o                          (tr_req_ctl_priv_o                          ),
//    /*output logic                                      */  .tr_req_ctl_go_o                            (tr_req_ctl_go_o                            ),
//    /*input  logic                                      */  .tr_req_finish_i                            (tr_req_finish_i                            ),
//    /*input  logic [63:0]                               */  .tr_req_resp_i                              (tr_req_resp_i                              ),
    /*output logic                                      */  .iocountinh_o                               (iocountinh_o                               ), //[1:31],
    /*output logic [14:0]                               */  .iohpmevt_eventid_o                         (iohpmevt_eventid_o                         ), //[1:31],
    /*output logic                                      */  .iohpmevt_dmask_o                           (iohpmevt_dmask_o                           ), //[1:31],
    /*output logic [19:0]                               */  .iohpmevt_pid_pscid_o                       (iohpmevt_pid_pscid_o                       ), //[1:31],
    /*output logic [23:0]                               */  .iohpmevt_did_gscid_o                       (iohpmevt_did_gscid_o                       ), //[1:31],
    /*output logic                                      */  .iohpmevt_pv_pscv_o                         (iohpmevt_pv_pscv_o                         ), //[1:31],
    /*output logic                                      */  .iohpmevt_dv_gscv_o                         (iohpmevt_dv_gscv_o                         ), //[1:31],
    /*output logic                                      */  .iohpmevt_idt_o                             (iohpmevt_idt_o                             ), //[1:31],
    /*output logic                                      */  .iohpmctr_counter_o                         (iohpmctr_counter_o                         ), //[1:31],
    /*input  logic                                      */  .iohpmevt_of_i                              (iohpmevt_of_i                              ), //[1:31],
    /*input  logic [63:0]                               */  .iohpmctr_counter_i                         (iohpmctr_counter_i                         ), //[1:31],
    /*output logic [31:1]                               */  .iocountovf_pos_o                           (iocountovf_pos                             ),
    /*input  logic                                      */  .int_multi_hit_check_fail_i                 (int_multi_hit_check_fail_i                 ),
    /*input  logic                                      */  .int_inv_buf_overflow_i                     (int_inv_buf_overflow                       ),
    /*input  logic                                      */  .int_fault_buf_overflow_i                   (int_fault_buf_overflow                     ),
    /*input  logic                                      */  .int_performance_buf_overflow_i             (int_performance_buf_overflow               ),
    /*output logic                                      */  .int_multi_hit_check_fail_en_o              (int_multi_hit_check_fail_en                ),
    /*output logic                                      */  .int_inv_buf_overflow_en_o                  (int_inv_buf_overflow_en                    ),
    /*output logic                                      */  .int_fault_buf_overflow_en_o                (int_fault_buf_overflow_en                  ),
    /*output logic                                      */  .int_performance_buf_overflow_en_o          (int_performance_buf_overflow_en            ),
    /*input  logic                                      */  .msg_valid_i                                (msg_cvalid_i                               ),
    /*output logic                                      */  .msg_ready_o                                (msg_cready_o                               ),
    /*input  logic MSG_CFG_ACCESS_TYPE                  */  .msg_wdata_i                                (msg_cwdata_i                               ),
    /*output logic                                      */  .msg_rvalid_o                               (msg_rvalid_o                               ),
    /*input  logic                                      */  .msg_rready_i                               (msg_rready_i                               ),
    /*output logic MSG_CFG_ACK_TYPE                     */  .msg_rdata_o                                (msg_rdata_o                                ),
    /*output logic                                      */  .msg_rlast_o                                (msg_rlast_o                                ),
    /*input  logic [1:0]                                */  .trans_unit_ecc_err_i                       (trans_unit_ecc_err_i                       ),
    /*output logic [1:0]                                */  .ecc_err_o                                  (cfg_ecc_err_o                              ),
    /*input  logic                                      */  .spare_in                                   (1'b0                                       ) 
    );
//}}}

//=== INV {{{
    iommu_acd_inv #(
    /*parameter  */ .INV_IDX_WIDTH              (INV_IDX_WIDTH                              ), // = iommu_acd_pkg::INV_IDX_WIDTH,
    /*parameter  */ .INTERNAL_INV_IDX_WIDTH     (INTERNAL_INV_IDX_WIDTH                     ), // = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH                      ), // = 6,
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE                           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
    /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH                             ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
    /*parameter type         */ .TLBQ2INV_TYPE              (TLBQ2INV_TYPE                              ), // = iommu_acd_pkg::tlbq2inv_t,
    /*parameter  */ .SPARE_PARAM                (1'b0                                       )  // = 0
    ) U_inv(                                                                                    
    /*input  logic                                      */  .clk                                        (clk                                        ),
    /*input  logic                                      */  .rstn                                       (rstn                                       ),
    /*output logic                                      */  .inv_tlbcache_req_valid_o                   (inv_tlbcache_req_valid_o                   ),
    /*input  logic                                      */  .inv_tlbcache_req_ready_i                   (inv_tlbcache_req_ready_i                   ),
    /*output INVALID_REQ_TYPE                           */  .inv_tlbcache_req_o                         (inv_tlbcache_req_o                         ),
    /*input  logic                                      */  .inv_tlbcache_ack_valid_i                   (inv_tlbcache_ack_valid_i                   ),
    /*input  INVALID_REQ_TYPE                           */  .inv_tlbcache_ack_i                         (inv_tlbcache_ack_i                         ),
    /*input  TLBQ2INV_TYPE                              */  . qinfo2_inv_i                              (qinfo2_inv_i                               ),
    /*output logic [INV_INFLY_NUM-1:0]                  */  .inv_ptwackfilter_req_valid_o               (inv_ptwackfilter_req_valid                 ),
    /*output INVALID_REQ_TYPE [INV_INFLY_NUM-1:0]       */  .inv_ptwackfilter_req_o                     (inv_ptwackfilter_req                       ),
    /*input  logic                                      */  .msg_valid_i                                (msg_ivalid_i                               ),
    /*output logic                                      */  .msg_ready_o                                (msg_iready_o                               ),
    /*input  logic [63:0]                               */  .msg_wdata_i                                (msg_iwdata_i                               ),
    /*output logic                                      */  .msg_ivalid_o                               (msg_ivalid_o                               ),
    /*input  logic                                      */  .msg_iready_i                               (msg_iready_i                               ),
    /*output MSG_INV_ACK_TYPE                           */  .msg_idata_o                                (msg_idata_o                                ),
    /*output logic                                      */  .msg_ilast_o                                (msg_ilast_o                                ),
    /*output logic                                      */  .inv_buf_overflow_err_o                     (int_inv_buf_overflow                       ),
    /*input  logic [BUS_INFLY_TOKEN_NUM-1:0]            */  .bh2inv_outstanding_list_i                  (bh2inv_outstanding_list_i                  ),
    /*input  logic [BUS_INFLY_TOKEN_NUM-1:0]            */  .bh2inv_outstanding_rw_list_i               (bh2inv_outstanding_rw_list_i               ),
    /*input  logic                                      */  .spare_in                                   (1'b0) 
);
//}}}

//=== PTW {{{
    iommu_acd_ptw #(
    /*parameter  */ .PTW_IDX_WIDTH              (PTW_IDX_WIDTH                              ), // = 8,
    /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH                             ), // = 3,
    /*parameter type         */ .PTW_REQ_TYPE               (PTW_REQ_TYPE                               ), // = logic,
    /*parameter type         */ .PTW_ACK_TYPE               (PTW_ACK_TYPE                               ), // = logic,
    /*parameter type         */ .PTW_REQ_GRP_TYPE           (PTW_REQ_GRP_TYPE                           ), // = iommu_acd_pkg::ptw_req_grp_t,
    /*parameter  */ .SPARE_PARAM                (0                                          ) // = 0
    ) U_ptw(                                                                                    
    /*input  logic                                      */  .clk                                        (clk                                        ),
    /*input  logic                                      */  .rstn                                       (rstn                                       ),
    /*input  logic                                      */  .ptw_req_valid_i                            (ptw_req_valid_i                            ),
    /*output logic                                      */  .ptw_req_ready_o                            (ptw_req_ready_o                            ),
    /*input  PTW_REQ_TYPE                               */  .ptw_req_i                                  (ptw_req_i                                  ),
    /*output logic                                      */  .ptw_ack_valid_o                            (ptw_ptw_ack_valid_o                        ),
    /*output PTW_ACK_TYPE                               */  .ptw_ack_o                                  (ptw_ptw_ack_o                              ),
    /*output PTW_REQ_TYPE                               */  .ptw_req_o                                  (ptw_ptw_req_o                              ),
    /*input  logic                                      */  .msg_valid_i                                (msg_pvalid_i                               ),
    /*output logic                                      */  .msg_ready_o                                (msg_pready_o                               ),
    /*input  logic [63:0]                               */  .msg_wdata_i                                (msg_pwdata_i                               ),
    /*output logic                                      */  .msg_pvalid_o                               (msg_pvalid_o                               ),
    /*input  logic                                      */  .msg_pready_i                               (msg_pready_i                               ),
    /*output logic [63:0]                               */  .msg_pdata_o                                (msg_pdata_o                                ),
    /*output logic                                      */  .msg_plast_o                                (msg_plast_o                                ),
    /*input  PTW_REQ_GRP_TYPE                           */  .ptwreqinfo2_ptw_i                          (ptwreqinfo2_ptw_i                          ),
    /*input  logic                                      */  .spare_in                                   (1'b0                                       ) 
    );
//}}}

//=== FAULT {{{
    iommu_acd_fault #(
    /*parameter  */ .FAULT_TOKEN_WIDTH          (FAULT_TOKEN_WIDTH                          ), // 3
    /*parameter  */ .SPARE_PARAM                (0                                          ) // = 0
    ) U_fault(
    /*input  logic                                      */  .clk                                        (clk                                        ),
    /*input  logic                                      */  .rstn                                       (rstn                                       ),
    /*input  logic                                      */  .fault_rpt_valid_i                          (fault_rpt_valid_i                          ),
    /*output logic                                      */  .fault_rpt_ready_o                          (fault_rpt_ready_o                          ),
    /*input  FAULT_RPT_TYPE                             */  .fault_rpt_i                                (fault_rpt_i                                ),
    /*input  logic                                      */  .msg_valid_i                                (msg_fvalid_i                               ),
    /*output logic                                      */  .msg_ready_o                                (msg_fready_o                               ),
    /*input  MSG_FAULT_ACK_TYPE                         */  .msg_wdata_i                                (msg_fwdata_i                               ),
    /*output logic                                      */  .msg_fvalid_o                               (msg_fvalid_o                               ),
    /*input  logic                                      */  .msg_fready_i                               (msg_fready_i                               ),
    /*output logic [63:0]                               */  .msg_fdata_o                                (msg_fdata_o                                ),
    /*output logic                                      */  .msg_flast_o                                (msg_flast_o                                ),
    /*output logic                                      */  .fault_buf_overflow_err_o                   (int_fault_buf_overflow                     ),
    /*input  logic                                      */  .spare_in                                   (1'b0                                       ) 
);
//}}}

//=== PTW ACK FILTER {{{
    iommu_acd_ptwackfilter #(
    /*parameter type         */ .PTW_REQ_TYPE               (PTW_REQ_TYPE                               ), // = iommu_ack_pkg::PTW_REQ_TYPE,
    /*parameter type         */ .PTW_ACK_TYPE               (PTW_ACK_TYPE                               ), // = iommu_acd_pkg::PTW_ACK_TYPE,
    /*parameter  */ .INV_IDX_WIDTH              (INV_IDX_WIDTH                              ), // = iommu_acd_pkg::INV_IDX_WIDTH,
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE                           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
    /*parameter  */ .SPARE_PARAM                (1'b0                                       )  // = 0
    ) U_paf(                                                
    /*input  logic                                      */  .clk                                        (clk                                        ),
    /*input  logic                                      */  .rstn                                       (rstn                                       ),
    /*input  logic                                      */  .ptw_ack_valid_i                            (ptw_ptw_ack_valid_o                        ),
    /*input  PTW_ACK_TYPE                               */  .ptw_ack_i                                  (ptw_ptw_ack_o                              ),
    /*input  PTW_REQ_TYPE                               */  .ptw_req_i                                  (ptw_ptw_req_o                              ),
    /*input  logic [INV_INFLY_NUM-1:0]                  */  .inv_req_valid_i                            (inv_ptwackfilter_req_valid                 ),
    /*input  INVALID_REQ_TYPE [INV_INFLY_NUM-1:0]       */  .inv_req_i                                  (inv_ptwackfilter_req                       ),
    /*output logic                                      */  .ptw_ack_valid_o                            (ptw_ack_valid_o                            ),
    /*output PTW_ACK_TYPE                               */  .ptw_ack_o                                  (ptw_ack_o                                  ),
    /*input  logic                                      */  .csr_fctl_gxl_i                             (iommu_fctl_gxl_o                           ),
    /*input  logic                                      */  .spare_in                                   (1'b0                                       ) 
    );

//}}}

//=== INTERRUPT {{{
    iommu_acd_int#(
    /*parameter  */ .SPARE_PARAM                (1'b0                                       ) // = 0
    ) U_interrupt(
    /*input  logic                                      */  .clk                                        (clk                                        ),
    /*input  logic                                      */  .rstn                                       (rstn                                       ),
    /*input  logic [31:1]                               */  .iocountovf_pos_i                           (iocountovf_pos                             ),
    /*input  logic [1:0]                                */  .ecc_err_pos_i                              (cfg_ecc_err_o                              ),
    /*output logic                                      */  .msg_nvalid_o                               (msg_nvalid_o                               ),
    /*input  logic                                      */  .msg_nready_i                               (msg_nready_i                               ),
    /*output MSG_INT_TYPE                               */  .msg_ndata_o                                (msg_ndata_o                                ),
    /*output logic                                      */  .msg_nlast_o                                (msg_nlast_o                                ),
    /*input  logic                                      */  .spare_in                                   (1'b0                                       )
    );
//}}}

endmodule
