//////////////////////////////////////////////////////////////////
// iommu_acd_tlb_queue
//////////////////////////////////////////////////////////////////
module iommu_acd_tlb_queue #( //{{{
//{{{ PARAM
    parameter   TRANS_QIDX_WIDTH            = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    parameter   TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter type          TRANSLATE_REQ_TYPE          = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE          = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    parameter type          PTW_REQ_TYPE                = iommu_acd_pkg::PTW_REQ_TYPE,
    parameter type          PTW_ACK_TYPE                = iommu_acd_pkg::PTW_ACK_TYPE,
    parameter type          FAULT_RPT_TYPE              = iommu_acd_pkg::FAULT_RPT_TYPE,
    parameter type          LOOKUP_REQ_TYPE             = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter type          TLBQ2INV_TYPE               = iommu_acd_pkg::tlbq2inv_t,
    parameter type          PTW_REQ_GRP_TYPE            = iommu_acd_pkg::ptw_req_grp_t,
    parameter   INTERNAL_INV_IDX_WIDTH      = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
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
    // PTW                                              
    output logic                                        ptw_req_valid_o,
    input  logic                                        ptw_req_ready_i,
    output PTW_REQ_TYPE                                 ptw_req_o,
    input  logic                                        ptw_ack_valid_i,
    input  PTW_ACK_TYPE                                 ptw_ack_i,
    // LOOKUP                                           
    output logic                                        lookup_req_valid_o,
    input  logic                                        lookup_req_ready_i,
    output LOOKUP_REQ_TYPE                              lookup_req_o,
    input  logic                                        lookup_ack_valid_i,
    input  LOOKUP_ACK_TYPE                              lookup_ack_i,
    // UPDATE                                           
    output logic                                        update_req_valid_o,
    input  logic                                        update_req_ready_i,
    output UPDATE_REQ_TYPE                              update_req_o,
    input  logic                                        update_ack_valid_i,
    input  UPDATE_REQ_TYPE                              update_ack_i,
    // FAULT                                            
    output logic                                        fault_rpt_valid_o,
    input  logic                                        fault_rpt_ready_i,
    output FAULT_RPT_TYPE                               fault_rpt_o,
    // Info to INV                                      
    output TLBQ2INV_TYPE                                qinfo2_inv_o,
    output PTW_REQ_GRP_TYPE                             ptwreqinfo2_ptw_o,
    // CFG                                              
    input  logic                                        csr_fctl_gxl_i,
    input  logic [3:0]                                  csr_ddtp_iommu_mode_i,
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
    input  iommu_acd_pkg::IDBG_TYPE_M                   idbg_intf_m_i,
    output iommu_acd_pkg::IDBG_TYPE_S                   idbg_intf_s_o,
`endif
    // INV
    output logic                                        invalid_req_valid_o,
    input  logic                                        invalid_req_ready_i,
    output INVALID_REQ_TYPE                             invalid_req_o,
    input  logic                                        invalid_ack_valid_i,
    input  INVALID_REQ_TYPE                             invalid_ack_i,
    // CREDIT
    input  logic                                        mrif_credit_grant_valid_i,
    //                                                  
    input  logic                                        spare_in
//}}}
);
//=== Declare === {{{
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_csr_fctl_gxl_i;
    logic [3:0]                                         entry_csr_ddtp_iommu_mode_i[TLB_QUEUE_DEPTH-1:0];
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_update_i;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_translate_req_valid_i;
    TRANSLATE_REQ_TYPE                                  entry_translate_req_i[TLB_QUEUE_DEPTH-1:0];
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_translate_ack_valid_o; //
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_translate_ack_ready_i;
    TRANSLATE_ACK_TYPE [TLB_QUEUE_DEPTH-1:0]            entry_translate_ack_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_ptw_req_valid_o; //
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_ptw_req_ready_i;
    PTW_REQ_TYPE [TLB_QUEUE_DEPTH-1:0]                  entry_ptw_req_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_ptw_ack_valid_i;
    PTW_ACK_TYPE                                        entry_ptw_ack_i[TLB_QUEUE_DEPTH-1:0];
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_lookup_req_valid_o; //
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_lookup_req_ready_i;
    LOOKUP_REQ_TYPE [TLB_QUEUE_DEPTH-1:0]               entry_lookup_req_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_lookup_ack_valid_i;
    LOOKUP_ACK_TYPE                                     entry_lookup_ack_i[TLB_QUEUE_DEPTH-1:0];
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_update_req_valid_o; //
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_update_req_ready_i;
    UPDATE_REQ_TYPE [TLB_QUEUE_DEPTH-1:0]               entry_update_req_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_update_ack_valid_i;
    UPDATE_REQ_TYPE [TLB_QUEUE_DEPTH-1:0]               entry_update_ack_i;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_fault_rpt_valid_o; //
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_fault_rpt_ready_i;
    FAULT_RPT_TYPE [TLB_QUEUE_DEPTH-1:0]                entry_fault_rpt_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_valid_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_ptw_ongoing_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_wr_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_valid_i[TLB_QUEUE_DEPTH-1:0];
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_depend_bits_i[TLB_QUEUE_DEPTH-1:0];
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_depend_bit_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_relook_hint_i;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_invalid_req_valid_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_invalid_req_ready_i;
    INVALID_REQ_TYPE [TLB_QUEUE_DEPTH-1:0]              entry_invalid_req_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_invalid_ack_valid_i;
    INVALID_REQ_TYPE [TLB_QUEUE_DEPTH-1:0]              entry_invalid_ack_i;

    logic                                               dbg_entry_csr_fctl_gxl_i;
    logic [3:0]                                         dbg_entry_csr_ddtp_iommu_mode_i;
    logic                                               dbg_entry_update_i;
    logic                                               dbg_entry_translate_req_valid_i;
    TRANSLATE_REQ_TYPE                                  dbg_entry_translate_req_i;
    logic                                               dbg_entry_translate_ack_valid_o; //
    logic                                               dbg_entry_translate_ack_ready_i;
    TRANSLATE_ACK_TYPE                                  dbg_entry_translate_ack_o;
    logic                                               dbg_entry_ptw_req_valid_o; //
    logic                                               dbg_entry_ptw_req_ready_i;
    PTW_REQ_TYPE                                        dbg_entry_ptw_req_o;
    logic                                               dbg_entry_ptw_ack_valid_i;
    PTW_ACK_TYPE                                        dbg_entry_ptw_ack_i;
    logic                                               dbg_entry_lookup_req_valid_o; //
    logic                                               dbg_entry_lookup_req_ready_i;
    LOOKUP_REQ_TYPE                                     dbg_entry_lookup_req_o;
    logic                                               dbg_entry_lookup_ack_valid_i;
    LOOKUP_ACK_TYPE                                     dbg_entry_lookup_ack_i;
    logic                                               dbg_entry_update_req_valid_o; //
    logic                                               dbg_entry_update_req_ready_i;
    UPDATE_REQ_TYPE                                     dbg_entry_update_req_o;
    logic                                               dbg_entry_update_ack_valid_i;
    UPDATE_REQ_TYPE                                     dbg_entry_update_ack_i;
    logic                                               dbg_entry_fault_rpt_valid_o; //
    logic                                               dbg_entry_fault_rpt_ready_i;
    FAULT_RPT_TYPE                                      dbg_entry_fault_rpt_o;
    logic                                               dbg_entry_valid_o;
    logic                                               dbg_entry_ptw_ongoing_o;
    logic                                               dbg_entry_wr_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         dbg_entry_valid_i;
    logic [TLB_QUEUE_DEPTH-1:0]                         dbg_entry_depend_bits_i;
    logic                                               dbg_entry_depend_bit_o;
    logic                                               dbg_entry_relook_hint_i;

    logic        [1:0]                                  dbgmux_ptw_req_valid_o;
    logic        [1:0]                                  dbgmux_ptw_req_ready_i;
    PTW_REQ_TYPE [1:0]                                  dbgmux_ptw_req_o;

    logic           [1:0]                               dbgmux_lookup_req_valid_o;
    logic           [1:0]                               dbgmux_lookup_req_ready_i;
    LOOKUP_REQ_TYPE [1:0]                               dbgmux_lookup_req_o;

    logic [3:0]                                         entry_info_fsm_o             [0:TLB_QUEUE_DEPTH-1];
    logic [TRANS_QIDX_WIDTH  :0]                        entry_info_tidx_o            [0:TLB_QUEUE_DEPTH-1];
    logic                                               entry_info_priv_o            [0:TLB_QUEUE_DEPTH-1];
    logic                                               entry_info_ext_o             [0:TLB_QUEUE_DEPTH-1];
    logic                                               entry_info_wr_o              [0:TLB_QUEUE_DEPTH-1];
    logic                                               entry_info_is_translated_o   [0:TLB_QUEUE_DEPTH-1];
    logic                                               entry_info_process_id_valid_o[0:TLB_QUEUE_DEPTH-1];
    logic [19:0]                                        entry_info_process_id_o      [0:TLB_QUEUE_DEPTH-1];
    logic [23:0]                                        entry_info_device_id_o       [0:TLB_QUEUE_DEPTH-1];
    logic [63:12]                                       entry_info_va_o              [0:TLB_QUEUE_DEPTH-1];

    logic                                               dbg_entry_invalid_req_valid_o;
    logic                                               dbg_entry_invalid_req_ready_i;
    INVALID_REQ_TYPE                                    dbg_entry_invalid_req_o      ;
    logic                                               dbg_entry_invalid_ack_valid_i;
    INVALID_REQ_TYPE                                    dbg_entry_invalid_ack_i      ;

    logic                                               mrif_credit_cnt;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_mrif_credit_req_o;
    logic [TLB_QUEUE_DEPTH-1:0]                         entry_mrif_credit_ack_i;
//}}}

//=== MainCode === {{{
//=== ptw_req_arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (TLB_QUEUE_DEPTH            ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (PTW_REQ_TYPE               ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_entry_ptw_req_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_ptw_req_valid_o      ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TLB_QUEUE_DEPTH{1'b0}}    ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (entry_ptw_req_o            ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_ptw_req_ready_i      ),
    /*output logic                    */ .req_o         (dbgmux_ptw_req_valid_o  [1]),
    /*output DATA_TYPE                */ .data_o        (dbgmux_ptw_req_o        [1]),
    /*input  logic                    */ .gnt_i         (dbgmux_ptw_req_ready_i  [1]) 
    );

    assign dbgmux_ptw_req_valid_o[0] = dbg_entry_ptw_req_valid_o;
    assign dbg_entry_ptw_req_ready_i = dbgmux_ptw_req_ready_i[0];
    assign dbgmux_ptw_req_o[0]       = dbg_entry_ptw_req_o;

    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (2                          ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (PTW_REQ_TYPE               ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_ptw_req_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (dbgmux_ptw_req_valid_o     ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   (2'b0                       ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (dbgmux_ptw_req_o           ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (dbgmux_ptw_req_ready_i     ),
    /*output logic                    */ .req_o         (ptw_req_valid_o            ),
    /*output DATA_TYPE                */ .data_o        (ptw_req_o                  ),
    /*input  logic                    */ .gnt_i         (ptw_req_ready_i            ) 
    );
//}}}

//=== update_req_arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (TLB_QUEUE_DEPTH            ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (UPDATE_REQ_TYPE            ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_update_req_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_update_req_valid_o   ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TLB_QUEUE_DEPTH{1'b0}}    ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (entry_update_req_o         ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_update_req_ready_i   ),
    /*output logic                    */ .req_o         (update_req_valid_o         ),
    /*output DATA_TYPE                */ .data_o        (update_req_o               ),
    /*input  logic                    */ .gnt_i         (update_req_ready_i         ) 
    );
//}}}

//=== lookup_req_arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (TLB_QUEUE_DEPTH            ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (LOOKUP_REQ_TYPE            ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_entry_lookup_req_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_lookup_req_valid_o   ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TLB_QUEUE_DEPTH{1'b0}}    ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (entry_lookup_req_o         ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_lookup_req_ready_i   ),
    /*output logic                    */ .req_o         (dbgmux_lookup_req_valid_o[1]),
    /*output DATA_TYPE                */ .data_o        (dbgmux_lookup_req_o      [1]),
    /*input  logic                    */ .gnt_i         (dbgmux_lookup_req_ready_i[1]) 
    );

    assign dbgmux_lookup_req_valid_o[0] = dbg_entry_lookup_req_valid_o;
    assign dbg_entry_lookup_req_ready_i = dbgmux_lookup_req_ready_i[0];
    assign dbgmux_lookup_req_o[0]       = dbg_entry_lookup_req_o;

    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (2                          ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (LOOKUP_REQ_TYPE            ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_lookup_req_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (dbgmux_lookup_req_valid_o  ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   (2'b0                       ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (dbgmux_lookup_req_o        ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (dbgmux_lookup_req_ready_i  ),
    /*output logic                    */ .req_o         (lookup_req_valid_o         ),
    /*output DATA_TYPE                */ .data_o        (lookup_req_o               ),
    /*input  logic                    */ .gnt_i         (lookup_req_ready_i         ) 
    );

//}}}

//=== fault_rpt_arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (TLB_QUEUE_DEPTH            ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (FAULT_RPT_TYPE             ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_fault_rpt_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_fault_rpt_valid_o    ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TLB_QUEUE_DEPTH{1'b0}}    ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (entry_fault_rpt_o          ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_fault_rpt_ready_i    ),
    /*output logic                    */ .req_o         (fault_rpt_valid_o          ),
    /*output DATA_TYPE                */ .data_o        (fault_rpt_o                ),
    /*input  logic                    */ .gnt_i         (fault_rpt_ready_i          ) 
    );
//}}}

//=== translate_ack_arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (TLB_QUEUE_DEPTH            ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (TRANSLATE_ACK_TYPE         ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_translate_ack_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_translate_ack_valid_o),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TLB_QUEUE_DEPTH{1'b0}}    ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (entry_translate_ack_o      ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_translate_ack_ready_i),
    /*output logic                    */ .req_o         (translate_ack_valid_o      ),
    /*output DATA_TYPE                */ .data_o        (translate_ack_o            ),
    /*input  logic                    */ .gnt_i         (1'b1                       ) 
    );
//}}}

//=== {{{ ready to BUS_HANDLER
    assign translate_req_ready_o = ~(&entry_valid_o);
//}}}

//=== info to INV {{{
genvar q2v;
generate
    for(q2v=0; q2v<TLB_QUEUE_DEPTH; q2v++) begin : q2inv_info_connect_gen
        assign qinfo2_inv_o.valid[q2v] = entry_valid_o[q2v] & entry_ptw_ongoing_o[q2v];
        assign qinfo2_inv_o.wr   [q2v] = entry_wr_o[q2v];
    end
endgenerate
//}}}

//=== info to PTW {{{
genvar q2p;
generate
    for(q2p=0; q2p<TLB_QUEUE_DEPTH; q2p++) begin : q2ptw_info_connect_gen
        assign ptwreqinfo2_ptw_o.ptw_req[q2p] = entry_ptw_req_o[q2p];
    end
endgenerate
//}}}

//=== invalid_req_arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (TLB_QUEUE_DEPTH            ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (INVALID_REQ_TYPE           ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_entry_invalid_req_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_invalid_req_valid_o  ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TLB_QUEUE_DEPTH{1'b0}}    ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (entry_invalid_req_o        ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_invalid_req_ready_i  ),
    /*output logic                    */ .req_o         (invalid_req_valid_o        ),
    /*output DATA_TYPE                */ .data_o        (invalid_req_o              ),
    /*input  logic                    */ .gnt_i         (invalid_req_ready_i        ) 
    );

//}}}

//=== MRIF CREDIT {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            mrif_credit_cnt <= 'd1;
        else begin
            if(mrif_credit_grant_valid_i)
                mrif_credit_cnt <= mrif_credit_cnt + 1'b1;
            else if((entry_mrif_credit_ack_i&entry_mrif_credit_req_o) != 0)
                mrif_credit_cnt <= mrif_credit_cnt - 1'b1;
        end
    end

    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (1                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (TLB_QUEUE_DEPTH            ),   //= 2,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_mrif_credit_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_mrif_credit_req_o    ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TLB_QUEUE_DEPTH{1'b0}}    ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (TLB_QUEUE_DEPTH'(0)        ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_mrif_credit_ack_i    ),
    /*output logic                    */ .req_o         (                           ),
    /*output DATA_TYPE                */ .data_o        (                           ),
    /*input  logic                    */ .gnt_i         (mrif_credit_cnt!='d0       ) 
    );

//}}}

//=== entry inst {{{
genvar i;
generate
    for(i=0; i<TLB_QUEUE_DEPTH; i++) begin : queue_entry_gen
        assign entry_csr_fctl_gxl_i         [i] = csr_fctl_gxl_i;
        assign entry_csr_ddtp_iommu_mode_i  [i] = csr_ddtp_iommu_mode_i;

        assign entry_translate_req_valid_i  [i] = translate_req_valid_i;
        assign entry_translate_req_i        [i] = translate_req_i;

        assign entry_ptw_ack_valid_i        [i] = ptw_ack_valid_i & (ptw_ack_i.idx==(TLB_QIDX_WIDTH+1)'(i));       // PTW ack demux
        assign entry_ptw_ack_i              [i] = ptw_ack_i;

        assign entry_lookup_ack_valid_i     [i] = lookup_ack_valid_i & (lookup_ack_i.idx==(TLB_QIDX_WIDTH+1)'(i)); // LOOKUP ack demux
        assign entry_lookup_ack_i           [i] = lookup_ack_i;

        assign entry_update_ack_valid_i     [i] = update_ack_valid_i & (update_ack_i.idx[TLB_QIDX_WIDTH-1:0]==TLB_QIDX_WIDTH'(i)); // UPDATE ack demux
        assign entry_update_ack_i           [i] = update_ack_i;

        always@(*) begin                                            // dependency bit connection
            for (int unsigned j=0; j<TLB_QUEUE_DEPTH; j++) begin
                entry_depend_bits_i[i][j] = (i==j) ? 1'b0 : entry_depend_bit_o[j];
                entry_valid_i[i][j]       = (i==j) ? 1'b0 : entry_valid_o[j];
            end
        end
        assign entry_relook_hint_i          [i] = 1'b1;
        
        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(TLB_QIDX_WIDTH)) U_entry_valid_check(
            .valid_i    (entry_valid_o              ),
            .update_i   (translate_req_valid_i      ),
            .tag_i      (TLB_QIDX_WIDTH'(i)    ),
            .update_o   (entry_update_i[i]          )
        );

        assign entry_invalid_ack_valid_i    [i] = invalid_ack_valid_i & (invalid_ack_i.idx[INTERNAL_INV_IDX_WIDTH-1:0]==(INTERNAL_INV_IDX_WIDTH'(i)));
        assign entry_invalid_ack_i          [i] = invalid_ack_i;

        iommu_acd_tlb_queue_entry #(
        /*parameter  */ .TLB_QUEUE_DBG              (1'b0                       ), //= 1'b0,
        /*parameter  */ .TRANS_QIDX_WIDTH           (TRANS_QIDX_WIDTH           ), // = iommu_acd_pkg::TRANS_QIDX_WIDTH,
        /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH             ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
        /*parameter type         */ .TRANSLATE_REQ_TYPE         (TRANSLATE_REQ_TYPE         ), // = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
        /*parameter type         */ .TRANSLATE_ACK_TYPE         (TRANSLATE_ACK_TYPE         ), // = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
        /*parameter type         */ .PTW_REQ_TYPE               (PTW_REQ_TYPE               ), // = iommu_acd_pkg::PTW_REQ_TYPE,
        /*parameter type         */ .PTW_ACK_TYPE               (PTW_ACK_TYPE               ), // = iommu_acd_pkg::PTW_ACK_TYPE,
        /*parameter type         */ .FAULT_RPT_TYPE             (FAULT_RPT_TYPE             ), // = iommu_acd_pkg::FAULT_RPT_TYPE,
        /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = iommu_acd_pkg::lookup_req_t,
        /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = iommu_acd_pkg::lookup_ack_t,
        /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = logic,
        /*parameter  */ .INTERNAL_INV_IDX_WIDTH     (INTERNAL_INV_IDX_WIDTH     ), // = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
        /*parameter  */ .SPARE_PARAM                (1'b0                       )  // = 0
        ) U_entry(                                                                      
        /*input  logic                                      */  .clk                        (clk                                ),
        /*input  logic                                      */  .rstn                       (rstn                               ),
        /*input  logic [TLB_QIDX_WIDTH  :0]                 */  .idx                        ((TLB_QIDX_WIDTH+1)'(i)             ),
        /*input  logic                                      */  .update_i                   (entry_update_i                 [i] ),
        /*input  logic                                      */  .translate_req_valid_i      (entry_translate_req_valid_i    [i] ),
        /*input  TRANSLATE_REQ_TYPE                         */  .translate_req_i            (entry_translate_req_i          [i] ),
        /*output logic                                      */  .translate_ack_valid_o      (entry_translate_ack_valid_o    [i] ),
        /*input  logic                                      */  .translate_ack_ready_i      (entry_translate_ack_ready_i    [i] ),
        /*output TRANSLATE_ACK_TYPE                         */  .translate_ack_o            (entry_translate_ack_o          [i] ),
        /*output logic                                      */  .ptw_req_valid_o            (entry_ptw_req_valid_o          [i] ),
        /*input  logic                                      */  .ptw_req_ready_i            (entry_ptw_req_ready_i          [i] ),
        /*output PTW_REQ_TYPE                               */  .ptw_req_o                  (entry_ptw_req_o                [i] ),
        /*input  logic                                      */  .ptw_ack_valid_i            (entry_ptw_ack_valid_i          [i] ),
        /*input  PTW_ACK_TYPE                               */  .ptw_ack_i                  (entry_ptw_ack_i                [i] ),
        /*output logic                                      */  .lookup_req_valid_o         (entry_lookup_req_valid_o       [i] ),
        /*input  logic                                      */  .lookup_req_ready_i         (entry_lookup_req_ready_i       [i] ),
        /*output LOOKUP_REQ_TYPE                            */  .lookup_req_o               (entry_lookup_req_o             [i] ),
        /*input  logic                                      */  .lookup_ack_valid_i         (entry_lookup_ack_valid_i       [i] ),
        /*input  LOOKUP_ACK_TYPE                            */  .lookup_ack_i               (entry_lookup_ack_i             [i] ),
        /*output logic                                      */  .update_req_valid_o         (entry_update_req_valid_o       [i] ),
        /*input  logic                                      */  .update_req_ready_i         (entry_update_req_ready_i       [i] ),
        /*output UPDATE_REQ_TYPE                            */  .update_req_o               (entry_update_req_o             [i] ),
        /*input  logic                                      */  .update_ack_valid_i         (entry_update_ack_valid_i       [i] ),
        /*input  UPDATE_REQ_TYPE                            */  .update_ack_i               (entry_update_ack_i             [i] ),
        /*output logic                                      */  .fault_rpt_valid_o          (entry_fault_rpt_valid_o        [i] ),
        /*input  logic                                      */  .fault_rpt_ready_i          (entry_fault_rpt_ready_i        [i] ),
        /*output FAULT_RPT_TYPE                             */  .fault_rpt_o                (entry_fault_rpt_o              [i] ),
        /*output logic                                      */  .valid_o                    (entry_valid_o                  [i] ),
        /*output logic                                      */  .ptw_ongoing_o              (entry_ptw_ongoing_o            [i] ),
        /*output logic                                      */  .wr_o                       (entry_wr_o                     [i] ),
        /*input  logic [TLB_QUEUE_DEPTH-1:0]                */  .valid_i                    (entry_valid_i                  [i] ),
        /*input  logic [TLB_QUEUE_DEPTH-1:0]                */  .depend_bits_i              (entry_depend_bits_i            [i] ),
        /*output logic                                      */  .depend_bit_o               (entry_depend_bit_o             [i] ),
        /*input  logic                                      */  .relook_hint_i              (entry_relook_hint_i            [i] ),
        /*input  logic                                      */  .csr_fctl_gxl_i             (entry_csr_fctl_gxl_i           [i] ),
        /*input  logic [3:0]                                */  .csr_ddtp_iommu_mode_i      (entry_csr_ddtp_iommu_mode_i    [i] ),
        /*output logic [3:0]                                */  .info_fsm_o                 (entry_info_fsm_o               [i] ),
        /*output logic [TRANS_QIDX_WIDTH  :0]               */  .info_tidx_o                (entry_info_tidx_o              [i] ),
        /*output logic                                      */  .info_priv_o                (entry_info_priv_o              [i] ),
        /*output logic                                      */  .info_ext_o                 (entry_info_ext_o               [i] ),
        /*output logic                                      */  .info_wr_o                  (entry_info_wr_o                [i] ),
        /*output logic                                      */  .info_is_translated_o       (entry_info_is_translated_o     [i] ),
        /*output logic                                      */  .info_process_id_valid_o    (entry_info_process_id_valid_o  [i] ),
        /*output logic [19:0]                               */  .info_process_id_o          (entry_info_process_id_o        [i] ),
        /*output logic [23:0]                               */  .info_device_id_o           (entry_info_device_id_o         [i] ),
        /*output logic [63:12]                              */  .info_va_o                  (entry_info_va_o                [i] ),
        /*output logic                                      */  .invalid_req_valid_o        (entry_invalid_req_valid_o      [i] ),
        /*input  logic                                      */  .invalid_req_ready_i        (entry_invalid_req_ready_i      [i] ),
        /*output INVALID_REQ_TYPE                           */  .invalid_req_o              (entry_invalid_req_o            [i] ),
        /*input  logic                                      */  .invalid_ack_valid_i        (entry_invalid_ack_valid_i      [i] ),
        /*input  INVALID_REQ_TYPE                           */  .invalid_ack_i              (entry_invalid_ack_i            [i] ),
        /*output logic                                      */  .mrif_credit_req_o          (entry_mrif_credit_req_o        [i] ),
        /*input  logic                                      */  .mrif_credit_ack_i          (entry_mrif_credit_ack_i        [i] ),
        /*input  logic                                      */  .spare_in                   (1'b0                               ) 
        );

    end
endgenerate

        localparam DBG_TLB_QUEUE_IDX = {
                                        1'b1, {TLB_QIDX_WIDTH{1'b0}}
                                        };
        assign dbg_translate_req_ready_o        = ~dbg_entry_valid_o;
        assign dbg_entry_update_i               = dbg_translate_req_valid_i & dbg_translate_req_ready_o;
        assign dbg_entry_csr_fctl_gxl_i         = csr_fctl_gxl_i;
        assign dbg_entry_csr_ddtp_iommu_mode_i  = csr_ddtp_iommu_mode_i;
        assign dbg_entry_translate_req_valid_i  = dbg_translate_req_valid_i;
        assign dbg_entry_translate_req_i        = dbg_translate_req_i;
        assign dbg_entry_ptw_ack_valid_i        = ptw_ack_valid_i & (ptw_ack_i.idx==DBG_TLB_QUEUE_IDX);       // PTW ack demux
        assign dbg_entry_ptw_ack_i              = ptw_ack_i;
        assign dbg_entry_lookup_ack_valid_i     = lookup_ack_valid_i & (lookup_ack_i.idx==DBG_TLB_QUEUE_IDX); // LOOKUP ack demux
        assign dbg_entry_lookup_ack_i           = lookup_ack_i;
        assign dbg_entry_update_ack_valid_i     = 1'b1;
        assign dbg_entry_update_ack_i           = dbg_entry_update_req_o;
        assign dbg_entry_depend_bits_i          = 'd0;
        assign dbg_entry_valid_i                = 'd0;
        assign dbg_entry_relook_hint_i          = 1'b1;
        
        // translate ack
        assign dbg_translate_ack_valid_o        = dbg_entry_translate_ack_valid_o;
        assign dbg_entry_translate_ack_ready_i  = 1'b1;
        assign dbg_translate_ack_o              = dbg_entry_translate_ack_o;
        
        // nouse
        assign dbg_entry_update_req_ready_i     = 1'b1;
        assign dbg_entry_fault_rpt_ready_i      = 1'b1;

        assign dbg_entry_invalid_req_ready_i    = 1'b1;
        assign dbg_entry_invalid_ack_valid_i    = 1'b1;
        assign dbg_entry_invalid_ack_i          = 'd0;

        iommu_acd_tlb_queue_entry #(
        /*parameter  */ .TLB_QUEUE_DBG              (1'b1                       ), //= 1'b0,
        /*parameter  */ .TRANS_QIDX_WIDTH           (TRANS_QIDX_WIDTH           ), // = iommu_acd_pkg::TRANS_QIDX_WIDTH,
        /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH             ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
        /*parameter type         */ .TRANSLATE_REQ_TYPE         (TRANSLATE_REQ_TYPE         ), // = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
        /*parameter type         */ .TRANSLATE_ACK_TYPE         (TRANSLATE_ACK_TYPE         ), // = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
        /*parameter type         */ .PTW_REQ_TYPE               (PTW_REQ_TYPE               ), // = iommu_acd_pkg::PTW_REQ_TYPE,
        /*parameter type         */ .PTW_ACK_TYPE               (PTW_ACK_TYPE               ), // = iommu_acd_pkg::PTW_ACK_TYPE,
        /*parameter type         */ .FAULT_RPT_TYPE             (FAULT_RPT_TYPE             ), // = iommu_acd_pkg::FAULT_RPT_TYPE,
        /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = iommu_acd_pkg::lookup_req_t,
        /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = iommu_acd_pkg::lookup_ack_t,
        /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = logic,
        /*parameter  */ .INTERNAL_INV_IDX_WIDTH     (INTERNAL_INV_IDX_WIDTH     ), // = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
        /*parameter  */ .SPARE_PARAM                (1'b0                       )  // = 0
        ) U_dbg_entry(                                                                      
        /*input  logic                                      */  .clk                        (clk                            ),
        /*input  logic                                      */  .rstn                       (rstn                           ),
        /*input  logic [TLB_QIDX_WIDTH  :0]                 */  .idx                        (DBG_TLB_QUEUE_IDX              ),
        /*input  logic                                      */  .update_i                   (dbg_entry_update_i             ),
        /*input  logic                                      */  .translate_req_valid_i      (dbg_entry_translate_req_valid_i),
        /*input  TRANSLATE_REQ_TYPE                         */  .translate_req_i            (dbg_entry_translate_req_i      ),
        /*output logic                                      */  .translate_ack_valid_o      (dbg_entry_translate_ack_valid_o),
        /*input  logic                                      */  .translate_ack_ready_i      (dbg_entry_translate_ack_ready_i),
        /*output TRANSLATE_ACK_TYPE                         */  .translate_ack_o            (dbg_entry_translate_ack_o      ),
        /*output logic                                      */  .ptw_req_valid_o            (dbg_entry_ptw_req_valid_o      ),
        /*input  logic                                      */  .ptw_req_ready_i            (dbg_entry_ptw_req_ready_i      ),
        /*output PTW_REQ_TYPE                               */  .ptw_req_o                  (dbg_entry_ptw_req_o            ),
        /*input  logic                                      */  .ptw_ack_valid_i            (dbg_entry_ptw_ack_valid_i      ),
        /*input  PTW_ACK_TYPE                               */  .ptw_ack_i                  (dbg_entry_ptw_ack_i            ),
        /*output logic                                      */  .lookup_req_valid_o         (dbg_entry_lookup_req_valid_o   ),
        /*input  logic                                      */  .lookup_req_ready_i         (dbg_entry_lookup_req_ready_i   ),
        /*output LOOKUP_REQ_TYPE                            */  .lookup_req_o               (dbg_entry_lookup_req_o         ),
        /*input  logic                                      */  .lookup_ack_valid_i         (dbg_entry_lookup_ack_valid_i   ),
        /*input  LOOKUP_ACK_TYPE                            */  .lookup_ack_i               (dbg_entry_lookup_ack_i         ),
        /*output logic                                      */  .update_req_valid_o         (dbg_entry_update_req_valid_o   ),
        /*input  logic                                      */  .update_req_ready_i         (dbg_entry_update_req_ready_i   ),
        /*output UPDATE_REQ_TYPE                            */  .update_req_o               (dbg_entry_update_req_o         ),
        /*input  logic                                      */  .update_ack_valid_i         (dbg_entry_update_ack_valid_i   ),
        /*input  UPDATE_REQ_TYPE                            */  .update_ack_i               (dbg_entry_update_ack_i         ),
        /*output logic                                      */  .fault_rpt_valid_o          (dbg_entry_fault_rpt_valid_o    ),
        /*input  logic                                      */  .fault_rpt_ready_i          (dbg_entry_fault_rpt_ready_i    ),
        /*output FAULT_RPT_TYPE                             */  .fault_rpt_o                (dbg_entry_fault_rpt_o          ),
        /*output logic                                      */  .valid_o                    (dbg_entry_valid_o              ),
        /*output logic                                      */  .ptw_ongoing_o              (dbg_entry_ptw_ongoing_o        ),
        /*output logic                                      */  .wr_o                       (dbg_entry_wr_o                 ),
        /*input  logic [TLB_QUEUE_DEPTH-1:0]                */  .valid_i                    (dbg_entry_valid_i              ),
        /*input  logic [TLB_QUEUE_DEPTH-1:0]                */  .depend_bits_i              (dbg_entry_depend_bits_i        ),
        /*output logic                                      */  .depend_bit_o               (dbg_entry_depend_bit_o         ),
        /*input  logic                                      */  .relook_hint_i              (dbg_entry_relook_hint_i        ),
        /*input  logic                                      */  .csr_fctl_gxl_i             (dbg_entry_csr_fctl_gxl_i       ),
        /*input  logic [3:0]                                */  .csr_ddtp_iommu_mode_i      (dbg_entry_csr_ddtp_iommu_mode_i),
        /*output logic [3:0]                                */  .info_fsm_o                 (),
        /*output logic [TRANS_QIDX_WIDTH  :0]               */  .info_tidx_o                (),
        /*output logic                                      */  .info_priv_o                (),
        /*output logic                                      */  .info_ext_o                 (),
        /*output logic                                      */  .info_wr_o                  (),
        /*output logic                                      */  .info_is_translated_o       (),
        /*output logic                                      */  .info_process_id_valid_o    (),
        /*output logic [19:0]                               */  .info_process_id_o          (),
        /*output logic [23:0]                               */  .info_device_id_o           (),
        /*output logic [63:12]                              */  .info_va_o                  (),
        /*output logic                                      */  .invalid_req_valid_o        (dbg_entry_invalid_req_valid_o  ),
        /*input  logic                                      */  .invalid_req_ready_i        (dbg_entry_invalid_req_ready_i  ),
        /*output INVALID_REQ_TYPE                           */  .invalid_req_o              (dbg_entry_invalid_req_o        ),
        /*input  logic                                      */  .invalid_ack_valid_i        (dbg_entry_invalid_ack_valid_i  ),
        /*input  INVALID_REQ_TYPE                           */  .invalid_ack_i              (dbg_entry_invalid_ack_i        ),
        /*output logic                                      */  .mrif_credit_req_o          (),
        /*input  logic                                      */  .mrif_credit_ack_i          (1'b1                           ),
        /*input  logic                                      */  .spare_in                   (1'b0                           ) 
        );

//}}}

//=== hpm {{{
    iommu_acd_hpm_tlbqueue #(
    /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH                 ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
    /*parameter  */ .TLB_QUEUE_DEPTH            (TLB_QUEUE_DEPTH                ), // = 2**TLB_QIDX_WIDTH,
    /*parameter type         */ .PTW_ACK_TYPE               (PTW_ACK_TYPE                   ), // = iommu_acd_pkg::PTW_ACK_TYPE,
    /*parameter  */ .SPARE_PARAM                (                               )  // = 0
    ) U_riscv_hpm(
    /*input  logic                                      */  .clk                            (clk                            ),
    /*input  logic                                      */  .rstn                           (rstn                           ),
    /*input  logic                                      */  .ptw_ack_valid_i                (ptw_ack_valid_i                ),
    /*input  PTW_ACK_TYPE                               */  .ptw_ack_i                      (ptw_ack_i                      ),
    /*input  logic                                      */  .entry_info_process_id_valid_i  (entry_info_process_id_valid_o  ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic [19:0]                               */  .entry_info_process_id_i        (entry_info_process_id_o        ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic [23:0]                               */  .entry_info_device_id_i         (entry_info_device_id_o         ), //[0:TLB_QUEUE_DEPTH-1],
    /*output iommu_acd_pkg::RISCV_HPMEVT_TYPE           */  .riscv_hpmevt_intf_o            (riscv_hpmevt_intf_o            ),
    /*input  logic                                      */  .spare_in                       (spare_in                       )
    );
//}}}

`ifdef IOMMU_IDBG
//=== IDBG {{{
    iommu_acd_mon_unit_tlb_queue #(
    /*parameter  */ .TRANS_QIDX_WIDTH           (TRANS_QIDX_WIDTH               ), //  = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH                 ), //  = iommu_acd_pkg::TLB_QIDX_WIDTH,
    /*parameter  */ .SPARE_PARAM                (1'b0                           )  //  = 0
    ) U_idbg(
    /*input  logic                                      */  .clk                            (clk                            ),
    /*input  logic                                      */  .rstn                           (rstn                           ),
    /*input  logic                                      */  .idbg_go_i                      (idbg_intf_m_i.idbg_go          ),
    /*output logic                                      */  .idbg_busy_o                    (idbg_intf_s_o.idbg_busy        ),
    /*input  logic [7:0]                                */  .idbg_opcode_i                  (idbg_intf_m_i.idbg_opcode      ),
    /*input  logic [31:0]                               */  .idbg_dat_i                     (idbg_intf_m_i.idbg_datw        ),
    /*output logic [31:0]                               */  .idbg_dat_o                     (idbg_intf_s_o.idbg_datr        ),
    /*output logic                                      */  .idbg_datv_o                    (idbg_intf_s_o.idbg_datv        ),
    /*input  logic [TLB_QUEUE_DEPTH-1:0]                */  .entry_valid_i                  (entry_valid_o                  ),
    /*input  logic [3:0]                                */  .entry_info_fsm_i               (entry_info_fsm_o               ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic [TRANS_QIDX_WIDTH  :0]               */  .entry_info_tidx_i              (entry_info_tidx_o              ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic                                      */  .entry_info_priv_i              (entry_info_priv_o              ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic                                      */  .entry_info_ext_i               (entry_info_ext_o               ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic                                      */  .entry_info_wr_i                (entry_info_wr_o                ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic                                      */  .entry_info_is_translated_i     (entry_info_is_translated_o     ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic                                      */  .entry_info_process_id_valid_i  (entry_info_process_id_valid_o  ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic [19:0]                               */  .entry_info_process_id_i        (entry_info_process_id_o        ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic [23:0]                               */  .entry_info_device_id_i         (entry_info_device_id_o         ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic [63:12]                              */  .entry_info_va_i                (entry_info_va_o                ), //[0:TLB_QUEUE_DEPTH-1],
    /*input  logic                                      */  .param_in                       (1'b0                           )
    );
//}}}
`endif
//}}}

endmodule
//}}}



