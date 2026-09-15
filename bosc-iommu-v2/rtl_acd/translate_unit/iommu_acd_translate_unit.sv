/////////////////////////////////////////////////////////////////////////////
// iommu_acd_translate_unit
/////////////////////////////////////////////////////////////////////////////
module iommu_acd_translate_unit #(
//{{{ PARAM
    parameter   TRANS_QIDX_WIDTH            = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    parameter   TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter type          TRANSLATE_REQ_TYPE          = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE          = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    parameter type          PTW_REQ_TYPE                = iommu_acd_pkg::PTW_REQ_TYPE,
    parameter type          PTW_ACK_TYPE                = iommu_acd_pkg::PTW_ACK_TYPE,
    parameter type          FAULT_RPT_TYPE              = iommu_acd_pkg::FAULT_RPT_TYPE,
    parameter   INTERNAL_INV_IDX_WIDTH      = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter   MICRO_TLB_IDX_WIDTH         = iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    parameter   CABIN_LKP_IDX_WIDTH         = iommu_acd_pkg::CABIN_LKP_IDX_WIDTH,
    parameter   CABIN_UPD_IDX_WIDTH         = iommu_acd_pkg::CABIN_UPD_IDX_WIDTH,
    parameter   CABIN_INV_IDX_WIDTH         = iommu_acd_pkg::CABIN_INV_IDX_WIDTH,
    parameter   BANK_4K_IDX_WIDTH           = iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    parameter   BANK_2M_IDX_WIDTH           = iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    parameter   BANK_1G_IDX_WIDTH           = iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    parameter   BANK_0T_IDX_WIDTH           = iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    parameter   BANK_4K_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    parameter   BANK_2M_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    parameter   BANK_1G_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    parameter   BANK_0T_SET_IDX_WIDTH       = iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    parameter   BANK_4K_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    parameter   BANK_2M_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    parameter   BANK_1G_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    parameter   BANK_0T_WAY_IDX_WIDTH       = iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    parameter   CABIN_LKP_NUM               = 2**CABIN_LKP_IDX_WIDTH,
    parameter   CABIN_UPD_NUM               = 2**CABIN_UPD_IDX_WIDTH,
    parameter   CABIN_INV_NUM               = 2**CABIN_INV_IDX_WIDTH,
    parameter   BANK_4K_NUM                 = 2**BANK_4K_IDX_WIDTH,
    parameter   BANK_2M_NUM                 = 2**BANK_2M_IDX_WIDTH,
    parameter   BANK_1G_NUM                 = 2**BANK_1G_IDX_WIDTH,
    parameter   BANK_0T_NUM                 = 2**BANK_0T_IDX_WIDTH,
    parameter   BANK_4K_SET_NUM             = 2**BANK_4K_SET_IDX_WIDTH,
    parameter   BANK_2M_SET_NUM             = 2**BANK_2M_SET_IDX_WIDTH,
    parameter   BANK_1G_SET_NUM             = 2**BANK_1G_SET_IDX_WIDTH,
    parameter   BANK_0T_SET_NUM             = 2**BANK_0T_SET_IDX_WIDTH,
    parameter   BANK_4K_WAY_NUM             = 2**BANK_4K_WAY_IDX_WIDTH,
    parameter   BANK_2M_WAY_NUM             = 2**BANK_2M_WAY_IDX_WIDTH,
    parameter   BANK_1G_WAY_NUM             = 2**BANK_1G_WAY_IDX_WIDTH,
    parameter   BANK_0T_WAY_NUM             = 2**BANK_0T_WAY_IDX_WIDTH,
    parameter   INV_IDX_WIDTH               = iommu_acd_pkg::INV_IDX_WIDTH,
    parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH,
    parameter type          TLBQ2INV_TYPE               = iommu_acd_pkg::tlbq2inv_t,
    parameter type          PTW_REQ_GRP_TYPE            = iommu_acd_pkg::ptw_req_grp_t,
    parameter type          LOOKUP_REQ_TYPE             = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter   SPARE_PARAM                 = 0 
//}}}
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // BUS_HANDLER                                      
    input  logic                                        translate_req_valid_i,
    output logic                                        translate_req_ready_o,
    input  TRANSLATE_REQ_TYPE                           translate_req_i,
    output logic                                        translate_ack_valid_o,
    output TRANSLATE_ACK_TYPE                           translate_ack_o,
    input  logic                                        mrif_credit_grant_valid_i,
    // PTW from queue                                   
    output logic                                        ptw_req_valid_o,
    input  logic                                        ptw_req_ready_i,
    output PTW_REQ_TYPE                                 ptw_req_o,
    input  logic                                        ptw_ack_valid_i,
    input  PTW_ACK_TYPE                                 ptw_ack_i,
    // FAULT from queue                                 
    output logic                                        fault_rpt_valid_o,
    input  logic                                        fault_rpt_ready_i,
    output FAULT_RPT_TYPE                               fault_rpt_o,
    // INV to cache                                     
    input  logic                                        inv_tlbcache_req_valid_i,
    output logic                                        inv_tlbcache_req_ready_o,
    input  INVALID_REQ_TYPE                             inv_tlbcache_req_i,
    output logic                                        inv_tlbcache_ack_valid_o,
    output INVALID_REQ_TYPE                             inv_tlbcache_ack_o,
    // Info to INV                                      
    output TLBQ2INV_TYPE                                qinfo2_inv_o,
    output PTW_REQ_GRP_TYPE                             ptwreqinfo2_ptw_o,
    // CFG
    input  logic                                        csr_fctl_gxl_i,
    input  logic [3:0]                                  csr_ddtp_iommu_mode_i,
    input  logic                                        multi_hit_check_i,
    output logic                                        multi_hit_fault_o,
    output logic [1:0]                                  ecc_err_o,
    // DBG
    input  logic                                        dbg_translate_req_valid_i,
    output logic                                        dbg_translate_req_ready_o,
    input  TRANSLATE_REQ_TYPE                           dbg_translate_req_i,
    output logic                                        dbg_translate_ack_valid_o,
    output TRANSLATE_ACK_TYPE                           dbg_translate_ack_o,
    // HPM
    output iommu_acd_pkg::RISCV_HPMEVT_TYPE             riscv_hpmevt_intf_o,
`ifdef IOMMU_IDBG
    // IDBG
    input  iommu_acd_pkg::IDBG_TYPE_M                   idbg_intf_m_i[2:0], //[2:0] 0: tlb_queue 1:microTLB 2:mainTLB_RAM
    output iommu_acd_pkg::IDBG_TYPE_S                   idbg_intf_s_o[2:0],
`endif
    //                                                  
    input  logic                                        spare_in
//}}}
);
//=== Declare === {{{
    logic                                               lookup_req_valid;
    logic                                               lookup_req_ready;
    LOOKUP_REQ_TYPE                                     lookup_req;
    logic                                               lookup_ack_valid;
    LOOKUP_ACK_TYPE                                     lookup_ack;
    logic                                               update_req_valid;
    logic                                               update_req_ready;
    UPDATE_REQ_TYPE                                     update_req;
    logic                                               update_ack_valid;
    UPDATE_REQ_TYPE                                     update_ack;

    // INV
    logic                                               tlbq_invalid_req_valid_o;
    logic                                               tlbq_invalid_req_ready_i;
    INVALID_REQ_TYPE                                    tlbq_invalid_req_o;
    logic                                               tlbq_invalid_ack_valid_i;
    INVALID_REQ_TYPE                                    tlbq_invalid_ack_i;

//}}}

//=== tlb_queue === {{{
    iommu_acd_tlb_queue #(
    /*parameter  */ .TRANS_QIDX_WIDTH           (TRANS_QIDX_WIDTH           ), // = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH             ), // = iommu_ack_pkg::TLB_QIDX_WIDTH,
    /*parameter type         */ .TRANSLATE_REQ_TYPE         (TRANSLATE_REQ_TYPE         ), // = iommu_ack_pkg::TRANSLATE_REQ_TYPE,
    /*parameter type         */ .TRANSLATE_ACK_TYPE         (TRANSLATE_ACK_TYPE         ), // = iommu_ack_pkg::TRANSLATE_ACK_TYPE,
    /*parameter type         */ .PTW_REQ_TYPE               (PTW_REQ_TYPE               ), // = iommu_ack_pkg::PTW_REQ_TYPE,
    /*parameter type         */ .PTW_ACK_TYPE               (PTW_ACK_TYPE               ), // = iommu_ack_pkg::PTW_ACK_TYPE,
    /*parameter type         */ .FAULT_RPT_TYPE             (FAULT_RPT_TYPE             ), // = iommu_ack_pkg::FAULT_RPT_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = iommu_ack_pkg::lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = iommu_ack_pkg::lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = iommu_ack_pkg::update_req_t,
    /*parameter type         */ .TLBQ2INV_TYPE              (TLBQ2INV_TYPE              ), // = iommu_acd_pkg::tlbq2inv_t,
    /*parameter type         */ .PTW_REQ_GRP_TYPE           (PTW_REQ_GRP_TYPE           ), // = iommu_acd_pkg::ptw_req_grp_t,
    /*parameter  */ .INTERNAL_INV_IDX_WIDTH     (INTERNAL_INV_IDX_WIDTH     ), // = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
    /*parameter  */ .SPARE_PARAM                (1'b0                       )  // = 0
    ) U_tlb_queue(                                                                      
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .translate_req_valid_i      (translate_req_valid_i      ),
    /*output logic                                      */  .translate_req_ready_o      (translate_req_ready_o      ),
    /*input  TRANSLATE_REQ_TYPE                         */  .translate_req_i            (translate_req_i            ),
    /*output logic                                      */  .translate_ack_valid_o      (translate_ack_valid_o      ),
    /*output TRANSLATE_ACK_TYPE                         */  .translate_ack_o            (translate_ack_o            ),
    /*output logic                                      */  .ptw_req_valid_o            (ptw_req_valid_o            ),
    /*input  logic                                      */  .ptw_req_ready_i            (ptw_req_ready_i            ),
    /*output PTW_REQ_TYPE                               */  .ptw_req_o                  (ptw_req_o                  ),
    /*input  logic                                      */  .ptw_ack_valid_i            (ptw_ack_valid_i            ),
    /*input  PTW_ACK_TYPE                               */  .ptw_ack_i                  (ptw_ack_i                  ),
    /*output logic                                      */  .lookup_req_valid_o         (lookup_req_valid           ),
    /*input  logic                                      */  .lookup_req_ready_i         (lookup_req_ready           ),
    /*output LOOKUP_REQ_TYPE                            */  .lookup_req_o               (lookup_req                 ),
    /*input  logic                                      */  .lookup_ack_valid_i         (lookup_ack_valid           ),
    /*input  LOOKUP_ACK_TYPE                            */  .lookup_ack_i               (lookup_ack                 ),
    /*output logic                                      */  .update_req_valid_o         (update_req_valid           ),
    /*input  logic                                      */  .update_req_ready_i         (update_req_ready           ),
    /*output UPDATE_REQ_TYPE                            */  .update_req_o               (update_req                 ),
    /*input  logic                                      */  .update_ack_valid_i         (update_ack_valid           ),
    /*input  UPDATE_REQ_TYPE                            */  .update_ack_i               (update_ack                 ),
    /*output logic                                      */  .fault_rpt_valid_o          (fault_rpt_valid_o          ),
    /*input  logic                                      */  .fault_rpt_ready_i          (fault_rpt_ready_i          ),
    /*output FAULT_RPT_TYPE                             */  .fault_rpt_o                (fault_rpt_o                ),
    /*output TLBQ2INV_TYPE                              */  .qinfo2_inv_o               (qinfo2_inv_o               ),
    /*output PTW_REQ_GRP_TYPE                           */  .ptwreqinfo2_ptw_o          (ptwreqinfo2_ptw_o          ),
    /*input  logic                                      */  .csr_fctl_gxl_i             (csr_fctl_gxl_i             ),
    /*input  logic [3:0]                                */  .csr_ddtp_iommu_mode_i      (csr_ddtp_iommu_mode_i      ),
    /*input  logic                                      */  .dbg_translate_req_valid_i  (dbg_translate_req_valid_i  ),
    /*output logic                                      */  .dbg_translate_req_ready_o  (dbg_translate_req_ready_o  ),
    /*input  TRANSLATE_REQ_TYPE                         */  .dbg_translate_req_i        (dbg_translate_req_i        ),
    /*output logic                                      */  .dbg_translate_ack_valid_o  (dbg_translate_ack_valid_o  ),
    /*output TRANSLATE_ACK_TYPE                         */  .dbg_translate_ack_o        (dbg_translate_ack_o        ),
    /*output iommu_acd_pkg::RISCV_HPMEVT_TYPE           */  .riscv_hpmevt_intf_o        (riscv_hpmevt_intf_o        ),
`ifdef IOMMU_IDBG
    /*input  iommu_acd_pkg::IDBG_TYPE_M                 */  .idbg_intf_m_i              (idbg_intf_m_i           [0]),
    /*output iommu_acd_pkg::IDBG_TYPE_S                 */  .idbg_intf_s_o              (idbg_intf_s_o           [0]),
`endif
    /*output logic                                      */  .invalid_req_valid_o        (tlbq_invalid_req_valid_o   ),
    /*input  logic                                      */  .invalid_req_ready_i        (tlbq_invalid_req_ready_i   ),
    /*output INVALID_REQ_TYPE                           */  .invalid_req_o              (tlbq_invalid_req_o         ),
    /*input  logic                                      */  .invalid_ack_valid_i        (tlbq_invalid_ack_valid_i   ),
    /*input  INVALID_REQ_TYPE                           */  .invalid_ack_i              (tlbq_invalid_ack_i         ),
    /*input  logic                                      */  .mrif_credit_grant_valid_i  (mrif_credit_grant_valid_i  ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
    );
//}}}

//=== tlb cache === {{{
    iommu_acd_tlb_wrap #(
    /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH             ), //= iommu_acd_pkg::TLB_QIDX_WIDTH,
    /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), //= iommu_acd_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), //= iommu_acd_pkg::lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), //= iommu_acd_pkg::lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), //= iommu_acd_pkg::update_req_t,
    /*parameter  */ .MICRO_TLB_IDX_WIDTH        (MICRO_TLB_IDX_WIDTH        ), //= iommu_acd_pkg::MICRO_TLB_IDX_WIDTH,
    /*parameter  */ .CABIN_LKP_IDX_WIDTH        (CABIN_LKP_IDX_WIDTH        ), //= iommu_acd_pkg::CABIN_LKP_IDX_WIDTH,
    /*parameter  */ .CABIN_UPD_IDX_WIDTH        (CABIN_UPD_IDX_WIDTH        ), //= iommu_acd_pkg::CABIN_UPD_IDX_WIDTH,
    /*parameter  */ .CABIN_INV_IDX_WIDTH        (CABIN_INV_IDX_WIDTH        ), //= iommu_acd_pkg::CABIN_INV_IDX_WIDTH,
    /*parameter  */ .BANK_4K_IDX_WIDTH          (BANK_4K_IDX_WIDTH          ), //= iommu_acd_pkg::BANK_4K_IDX_WIDTH,
    /*parameter  */ .BANK_2M_IDX_WIDTH          (BANK_2M_IDX_WIDTH          ), //= iommu_acd_pkg::BANK_2M_IDX_WIDTH,
    /*parameter  */ .BANK_1G_IDX_WIDTH          (BANK_1G_IDX_WIDTH          ), //= iommu_acd_pkg::BANK_1G_IDX_WIDTH,
    /*parameter  */ .BANK_0T_IDX_WIDTH          (BANK_0T_IDX_WIDTH          ), //= iommu_acd_pkg::BANK_0T_IDX_WIDTH,
    /*parameter  */ .BANK_4K_SET_IDX_WIDTH      (BANK_4K_SET_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_4K_SET_IDX_WIDTH,
    /*parameter  */ .BANK_2M_SET_IDX_WIDTH      (BANK_2M_SET_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_2M_SET_IDX_WIDTH,
    /*parameter  */ .BANK_1G_SET_IDX_WIDTH      (BANK_1G_SET_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_1G_SET_IDX_WIDTH,
    /*parameter  */ .BANK_0T_SET_IDX_WIDTH      (BANK_0T_SET_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_0T_SET_IDX_WIDTH,
    /*parameter  */ .BANK_4K_WAY_IDX_WIDTH      (BANK_4K_WAY_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_4K_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_2M_WAY_IDX_WIDTH      (BANK_2M_WAY_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_2M_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_1G_WAY_IDX_WIDTH      (BANK_1G_WAY_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_1G_WAY_IDX_WIDTH,
    /*parameter  */ .BANK_0T_WAY_IDX_WIDTH      (BANK_0T_WAY_IDX_WIDTH      ), //= iommu_acd_pkg::BANK_0T_WAY_IDX_WIDTH,
    /*parameter  */ .SPARE_PARAM                (1'b0                       )  //= 0 
    ) U_tlb_cache(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .lookup_req_valid_i         (lookup_req_valid           ),
    /*output logic                                      */  .lookup_req_ready_o         (lookup_req_ready           ),
    /*input  LOOKUP_REQ_TYPE                            */  .lookup_req_i               (lookup_req                 ),
    /*output logic                                      */  .lookup_ack_valid_o         (lookup_ack_valid           ),
    /*output LOOKUP_ACK_TYPE                            */  .lookup_ack_o               (lookup_ack                 ),
    /*input  logic                                      */  .update_req_valid_i         (update_req_valid           ),
    /*output logic                                      */  .update_req_ready_o         (update_req_ready           ),
    /*input  UPDATE_REQ_TYPE                            */  .update_req_i               (update_req                 ),
    /*output logic                                      */  .update_ack_valid_o         (update_ack_valid           ),
    /*output UPDATE_REQ_TYPE                            */  .update_ack_o               (update_ack                 ),
    /*input  logic                                      */  .invalid_req_valid_i        (inv_tlbcache_req_valid_i   ),
    /*output logic                                      */  .invalid_req_ready_o        (inv_tlbcache_req_ready_o   ),
    /*input  INVALID_REQ_TYPE                           */  .invalid_req_i              (inv_tlbcache_req_i         ),
    /*output logic                                      */  .invalid_ack_valid_o        (inv_tlbcache_ack_valid_o   ),
    /*output INVALID_REQ_TYPE                           */  .invalid_ack_o              (inv_tlbcache_ack_o         ),
    /*output logic                                      */  .ram_initial_done_o         (                           ),
    /*input  logic                                      */  .csr_fctl_gxl_i             (csr_fctl_gxl_i             ),
    /*input  logic                                      */  .multi_hit_check_i          (multi_hit_check_i          ),
    /*output logic                                      */  .multi_hit_fault_o          (multi_hit_fault_o          ),
    /*output logic [1:0]                                */  .ecc_err_o                  (ecc_err_o                  ),
`ifdef IOMMU_IDBG
    /*input  iommu_acd_pkg::IDBG_TYPE_M                 */  .idbg_intf_m_i              (idbg_intf_m_i         [2:1]),  //[1:0]
    /*output iommu_acd_pkg::IDBG_TYPE_S                 */  .idbg_intf_s_o              (idbg_intf_s_o         [2:1]),  //[1:0]
`endif
    /*input  logic                                      */  .tlbq_invalid_req_valid_i   (tlbq_invalid_req_valid_o   ),
    /*output logic                                      */  .tlbq_invalid_req_ready_o   (tlbq_invalid_req_ready_i   ),
    /*input  INVALID_REQ_TYPE                           */  .tlbq_invalid_req_i         (tlbq_invalid_req_o         ),
    /*output logic                                      */  .tlbq_invalid_ack_valid_o   (tlbq_invalid_ack_valid_i   ),
    /*output INVALID_REQ_TYPE                           */  .tlbq_invalid_ack_o         (tlbq_invalid_ack_i         ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
    );

//}}}

//=== HPM === {{{
//    iommu_acd_hpm_translate_unit U_hpm(
//    /*input  logic                  */  .clk                        (clk                        ),
//    /*input  logic                  */  .rstn                       (rstn                       ),
//    /*input  logic                  */  .translate_req_valid_i      (translate_req_valid_i      ),
//    /*input  logic                  */  .translate_req_ready_i      (translate_req_ready_o      ),
//    /*input  logic                  */  .translate_ack_valid_i      (translate_ack_valid_o      ),
//    /*input  logic                  */  .translate_ack_success_i    (translate_ack_o.resp=='d0  ),
//    /*input  logic                  */  .ptw_req_valid_i            (ptw_req_valid_o            ),
//    /*input  logic                  */  .ptw_req_ready_i            (ptw_req_ready_i            ),
//    /*input  logic                  */  .ptw_ack_valid_i            (ptw_ack_valid_i            ),
//    /*input  logic                  */  .ptw_ack_success_i          (ptw_ack_i.opcode[1:0]=='d0 ),
//    /*input  logic [5:0]            */  .hpm_cnt_inhibit_i          (hpm_cnt_inhibit_i[9:4]     ),
//    /*output logic [63:0]           */  .hpm_cnt_o                  (hpm_cnt_o[9:4]             ), //[5:0],
//    /*input  logic                  */  .spare_in                   (1'b0                       )
//    );

//}}}

//=== IDBG
endmodule
