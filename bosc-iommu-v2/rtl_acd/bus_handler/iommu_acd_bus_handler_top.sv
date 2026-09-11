//////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////
module iommu_acd_bus_handler_top #(
//{{{ PARAM
    parameter   WBUF_FIFO_TYPE              = 1'b1, // 0:FF type, 1:RAM type
    parameter   SLV_AW_REGSLICE             = 1,
    parameter   SLV_W_REGSLICE              = 1,
    parameter   SLV_AR_REGSLICE             = 1,
    parameter   SLV_R_REGSLICE              = 1,
    parameter   SLV_B_REGSLICE              = 1,
    parameter   MST_AW_REGSLICE             = 1,
    parameter   MST_W_REGSLICE              = 1,
    parameter   MST_AR_REGSLICE             = 1,
    parameter   MST_R_REGSLICE              = 1,
    parameter   MST_B_REGSLICE              = 1,
    parameter   BUS_PROPERTY_BAR            = 0,
    parameter   TRANS_QUEUE_IDX_WIDTH       = 3,
    parameter   BUS_INFLY_TOKEN_WIDTH       = 6,
    parameter   BUS_INFLY_TOKEN_NUM         = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_ADDR_WIDTH              = 64,
    parameter   BUS_DATA_WIDTH              = 128,
    parameter   BUS_SIZE_WIDTH              = 3,
    parameter   BUS_STRB_WIDTH              = BUS_DATA_WIDTH/8,
    parameter   BUS_ID_WIDTH                = 8,
    parameter   BUS_USER_WIDTH              = 8,
    parameter   BUS_LOOP_WIDTH              = 1,
    parameter type          BUS_CH_AX_TYPE              = iommu_acd_pkg::ch_ax_t,
    parameter type          BUS_CH_W_TYPE               = iommu_acd_pkg::ch_w_t,
    parameter type          BUS_CH_B_TYPE               = iommu_acd_pkg::ch_b_t,
    parameter type          BUS_CH_R_TYPE               = iommu_acd_pkg::ch_r_t,
    parameter type          TRANSLATE_REQ_TYPE          = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE          = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    parameter   SPARE_PARAM                 = 0
//}}}
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // SLV port
    input  logic                                        slv_awvalid_i            ,
    output logic                                        slv_awready_o            ,
    input  BUS_CH_AX_TYPE                               slv_awpayld_i            ,
    input  logic [23:0]                                 slv_aw_device_id_i       ,
    input  logic [19:0]                                 slv_aw_process_id_i      ,
    input  logic                                        slv_aw_process_id_valid_i,
    input  logic                                        slv_aw_is_translated_i   ,
    input  logic                                        slv_wvalid_i             ,
    output logic                                        slv_wready_o             ,
    input  BUS_CH_W_TYPE                                slv_wpayld_i             ,
    output logic                                        slv_bvalid_o             ,
    input  logic                                        slv_bready_i             ,
    output BUS_CH_B_TYPE                                slv_bpayld_o             ,
    input  logic                                        slv_arvalid_i            ,
    output logic                                        slv_arready_o            ,
    input  BUS_CH_AX_TYPE                               slv_arpayld_i            ,
    input  logic [23:0]                                 slv_ar_device_id_i       ,
    input  logic [19:0]                                 slv_ar_process_id_i      ,
    input  logic                                        slv_ar_process_id_valid_i,
    input  logic                                        slv_ar_is_translated_i   ,
    output logic                                        slv_rvalid_o             ,
    input  logic                                        slv_rready_i             ,
    output BUS_CH_R_TYPE                                slv_rpayld_o             ,
    // MST port
    output logic                                        mst_awvalid_o            ,
    input  logic                                        mst_awready_i            ,
    output BUS_CH_AX_TYPE                               mst_awpayld_o            ,
    output logic                                        mst_wvalid_o             ,
    input  logic                                        mst_wready_i             ,
    output BUS_CH_W_TYPE                                mst_wpayld_o             ,
    input  logic                                        mst_bvalid_i             ,
    output logic                                        mst_bready_o             ,
    input  BUS_CH_B_TYPE                                mst_bpayld_i             ,
    output logic                                        mst_arvalid_o            ,
    input  logic                                        mst_arready_i            ,
    output BUS_CH_AX_TYPE                               mst_arpayld_o            ,
    input  logic                                        mst_rvalid_i             ,
    output logic                                        mst_rready_o             ,
    input  BUS_CH_R_TYPE                                mst_rpayld_i             ,
    // Translate Port
    output logic                                        translate_req_valid_o,
    input  logic                                        translate_req_ready_i,
    output TRANSLATE_REQ_TYPE                           translate_req_o,
    input  logic                                        translate_ack_valid_i,
    input  TRANSLATE_ACK_TYPE                           translate_ack_i,
    output logic                                        mrif_credit_grant_valid_o,
    // INV
    output logic [BUS_INFLY_TOKEN_NUM-1:0]              bh2inv_outstanding_list_o,
    output logic [BUS_INFLY_TOKEN_NUM-1:0]              bh2inv_outstanding_rw_list_o,
    // HPM
    output iommu_acd_pkg::RISCV_HPMEVT_TYPE             riscv_hpmevt_intf_o,
`ifdef IOMMU_IDBG
    input  iommu_acd_pkg::IDBG_TYPE_M                   idbg_intf_m_i,
    output iommu_acd_pkg::IDBG_TYPE_S                   idbg_intf_s_o,
    input  logic [63:0]                                 idbg_scnt_i,
`endif
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare === {{{
    logic                                       s2w_wvalid                  ;
    logic                                       s2w_wready                  ;
    BUS_CH_W_TYPE                               s2w_wpayld                  ;
    logic                                       b2s_bvalid                  ;
    logic                                       b2s_bready                  ;
    BUS_CH_B_TYPE                               b2s_bpayld                  ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           b2s_token                   ;
    logic                                       r2s_rvalid                  ;
    logic                                       r2s_rready                  ;
    BUS_CH_R_TYPE                               r2s_rpayld                  ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           r2s_token                   ;
    logic                                       s2q_valid                   ;
    logic                                       s2q_ready                   ;
    logic                                       s2q_bc_fail                 ;
    logic                                       s2q_wr                      ;
    logic [23:0]                                s2q_device_id               ;
    logic [19:0]                                s2q_process_id              ;
    logic                                       s2q_process_id_valid        ;
    logic                                       s2q_is_translated           ;
    BUS_CH_AX_TYPE                              s2q_axpayld                 ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           s2q_token                   ;
    logic                                       s2q_w_can_enqueue_hint      ;
    logic [BUS_INFLY_TOKEN_NUM-1:0]             wdata_ready                 ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           enqueue_token               ;
    logic                                       qout_valid                  ;
    logic                                       qout_ready                  ;
    logic                                       qout_wr                     ;
    BUS_CH_AX_TYPE                              qout_axpayld                ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           qout_token                  ;
    logic                                       qout_fault                  ;
    logic                                       w2m_awvalid                 ;
    logic                                       w2m_awready                 ;
    BUS_CH_AX_TYPE                              w2m_awpayld                 ;
    logic                                       w2m_wvalid                  ;
    logic                                       w2m_wready                  ;
    BUS_CH_W_TYPE                               w2m_wpayld                  ;
    logic                                       w2m_fault                   ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           w2m_token                   ;
    logic                                       m2b_bvalid                  ;
    logic                                       m2b_bready                  ;
    BUS_CH_B_TYPE                               m2b_bpayld                  ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           m2b_token                   ;
    logic                                       r2m_arvalid                 ;
    logic                                       r2m_arready                 ;
    BUS_CH_AX_TYPE                              r2m_arpayld                 ;
    logic                                       r2m_fault                   ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           r2m_token                   ;
    logic                                       m2r_rvalid                  ;
    logic                                       m2r_rready                  ;
    BUS_CH_R_TYPE                               m2r_rpayld                  ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           m2r_token                   ;

    logic                                       qout_ready_w_int, qout_ready_r_int;

    logic                                       wbuf_dequeue_fifo_empty;

    logic                                       q2m_valid                   ;
    logic                                       q2m_ready                   ;
    logic                                       q2m_wr                      ;
    BUS_CH_AX_TYPE                              q2m_axpayld                 ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           q2m_token                   ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           q2m_pair_token              ;
    logic                                       q2m_fault                   ;

    logic                                       qout_mrif, r2m_mrif, w2m_mrif;

    logic                                       mst_r2m_arvalid             ;
    logic                                       mst_r2m_arready             ;
    BUS_CH_AX_TYPE                              mst_r2m_arpayld             ;
    logic                                       mst_r2m_fault               ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           mst_r2m_token               ;
    logic                                       mst_r2m_mrif                ;

    logic                                       mst_w2m_awvalid             ;
    logic                                       mst_w2m_awready             ;
    BUS_CH_AX_TYPE                              mst_w2m_awpayld             ;
    logic                                       mst_w2m_wvalid              ;
    logic                                       mst_w2m_wready              ;
    BUS_CH_W_TYPE                               mst_w2m_wpayld              ;
    logic                                       mst_w2m_fault               ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           mst_w2m_token               ;
    logic                                       mst_w2m_mrif                ;

    logic                                       mst_m2b_bvalid              ;
    logic                                       mst_m2b_bready              ;
    BUS_CH_B_TYPE                               mst_m2b_bpayld              ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           mst_m2b_token               ;
    logic                                       mst_m2b_mrif                ;

    logic                                       mrif_done                   ;
    logic                                       mrif_valid                  ;
    logic [55:12]                               mrif_nppn                   ;
    logic [10:0]                                mrif_nid                    ;
    logic [BUS_ID_WIDTH-1:0]                    mrif_axid                   ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]           mrif_token                  ;

//}}}

//=== Main Code === {{{
    assign qout_ready = qout_wr ? qout_ready_w_int : qout_ready_r_int;
//}}}

//=== Inst === {{{
// SLV INTF{{{
    iommu_acd_bus_handler_slv_intf #(
    /*parameter              */ .SLV_AW_REGSLICE            (SLV_AW_REGSLICE            ), // = 1,
    /*parameter              */ .SLV_W_REGSLICE             (SLV_W_REGSLICE             ), // = 1,
    /*parameter              */ .SLV_AR_REGSLICE            (SLV_AR_REGSLICE            ), // = 1,
    /*parameter              */ .SLV_R_REGSLICE             (SLV_R_REGSLICE             ), // = 1,
    /*parameter              */ .SLV_B_REGSLICE             (SLV_B_REGSLICE             ), // = 1,
    /*parameter  */ .BUS_PROPERTY_BAR           (BUS_PROPERTY_BAR           ), // = 0,
    /*parameter  */ .TRANS_QUEUE_IDX_WIDTH      (TRANS_QUEUE_IDX_WIDTH      ), // = 3,
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), // = 6,
    /*parameter  */ .BUS_ADDR_WIDTH             (BUS_ADDR_WIDTH             ), // = 64,
    /*parameter  */ .BUS_DATA_WIDTH             (BUS_DATA_WIDTH             ), // = 128,
    /*parameter  */ .BUS_ID_WIDTH               (BUS_ID_WIDTH               ), // = 8,
    /*parameter  */ .BUS_USER_WIDTH             (BUS_USER_WIDTH             ), // = 8,
    /*parameter type         */ .BUS_CH_AX_TYPE             (BUS_CH_AX_TYPE             ), // = iommu_acd_pkg::ch_ax_t,
    /*parameter type         */ .BUS_CH_W_TYPE              (BUS_CH_W_TYPE              ), // = iommu_acd_pkg::ch_w_t,
    /*parameter type         */ .BUS_CH_B_TYPE              (BUS_CH_B_TYPE              ), // = iommu_acd_pkg::ch_b_t,
    /*parameter type         */ .BUS_CH_R_TYPE              (BUS_CH_R_TYPE              ), // = iommu_acd_pkg::ch_r_t,
    /*parameter  */ .SPARE_PARAM                (1'b0                       )  // = 0
    ) U_slv_intf(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .slv_awvalid_i              (slv_awvalid_i              ),
    /*output logic                                      */  .slv_awready_o              (slv_awready_o              ),
    /*input  BUS_CH_AX_TYPE                             */  .slv_awpayld_i              (slv_awpayld_i              ),
    /*input  logic [23:0]                               */  .slv_aw_device_id_i         (slv_aw_device_id_i         ),
    /*input  logic [19:0]                               */  .slv_aw_process_id_i        (slv_aw_process_id_i        ),
    /*input  logic                                      */  .slv_aw_process_id_valid_i  (slv_aw_process_id_valid_i  ),
    /*input  logic                                      */  .slv_aw_is_translated_i     (slv_aw_is_translated_i     ),
    /*input  logic                                      */  .slv_wvalid_i               (slv_wvalid_i               ),
    /*output logic                                      */  .slv_wready_o               (slv_wready_o               ),
    /*input  BUS_CH_W_TYPE                              */  .slv_wpayld_i               (slv_wpayld_i               ),
    /*output logic                                      */  .slv_bvalid_o               (slv_bvalid_o               ),
    /*input  logic                                      */  .slv_bready_i               (slv_bready_i               ),
    /*output BUS_CH_B_TYPE                              */  .slv_bpayld_o               (slv_bpayld_o               ),
    /*input  logic                                      */  .slv_arvalid_i              (slv_arvalid_i              ),
    /*output logic                                      */  .slv_arready_o              (slv_arready_o              ),
    /*input  BUS_CH_AX_TYPE                             */  .slv_arpayld_i              (slv_arpayld_i              ),
    /*input  logic [23:0]                               */  .slv_ar_device_id_i         (slv_ar_device_id_i         ),
    /*input  logic [19:0]                               */  .slv_ar_process_id_i        (slv_ar_process_id_i        ),
    /*input  logic                                      */  .slv_ar_process_id_valid_i  (slv_ar_process_id_valid_i  ),
    /*input  logic                                      */  .slv_ar_is_translated_i     (slv_ar_is_translated_i     ),
    /*output logic                                      */  .slv_rvalid_o               (slv_rvalid_o               ),
    /*input  logic                                      */  .slv_rready_i               (slv_rready_i               ),
    /*output BUS_CH_R_TYPE                              */  .slv_rpayld_o               (slv_rpayld_o               ),
    /*output logic                                      */  .s2w_wvalid_o               (s2w_wvalid                 ),
    /*input  logic                                      */  .s2w_wready_i               (s2w_wready                 ),
    /*output BUS_CH_W_TYPE                              */  .s2w_wpayld_o               (s2w_wpayld                 ),
    /*input  logic                                      */  .b2s_bvalid_i               (b2s_bvalid                 ),
    /*output logic                                      */  .b2s_bready_o               (b2s_bready                 ),
    /*input  BUS_CH_B_TYPE                              */  .b2s_bpayld_i               (b2s_bpayld                 ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .b2s_token_i                (b2s_token                  ),
    /*input  logic                                      */  .r2s_rvalid_i               (r2s_rvalid                 ),
    /*output logic                                      */  .r2s_rready_o               (r2s_rready                 ),
    /*input  BUS_CH_R_TYPE                              */  .r2s_rpayld_i               (r2s_rpayld                 ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .r2s_token_i                (r2s_token                  ),
    /*output logic                                      */  .s2q_valid_o                (s2q_valid                  ),
    /*input  logic                                      */  .s2q_ready_i                (s2q_ready                  ),
    /*output logic                                      */  .s2q_bc_fail_o              (s2q_bc_fail                ),
    /*output logic                                      */  .s2q_wr_o                   (s2q_wr                     ),
    /*output logic [23:0]                               */  .s2q_device_id_o            (s2q_device_id              ),
    /*output logic [19:0]                               */  .s2q_process_id_o           (s2q_process_id             ),
    /*output logic                                      */  .s2q_process_id_valid_o     (s2q_process_id_valid       ),
    /*output logic                                      */  .s2q_is_translated_o        (s2q_is_translated          ),
    /*input  logic BUS_CH_AX_TYPE                       */  .s2q_axpayld_o              (s2q_axpayld                ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .s2q_token_o                (s2q_token                  ),
    /*input  logic                                      */  .s2q_w_can_enqueue_hint_i   (s2q_w_can_enqueue_hint     ),
    /*output logic [BUS_INFLY_TOKEN_NUM-1:0]            */  .bh2inv_outstanding_list_o   (bh2inv_outstanding_list_o   ),
    /*output logic [BUS_INFLY_TOKEN_NUM-1:0]            */  .bh2inv_outstanding_rw_list_o(bh2inv_outstanding_rw_list_o),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
);
//}}}

//WR-BUF {{{
    iommu_acd_bus_handler_w_buf #(
    /*parameter  */ .WFIFO_TYPE                 (WBUF_FIFO_TYPE             ), // = 1'b1, // 0:FF type, 1:RAM type
    /*parameter  */ .WFIFO_DEPTH                (32                         ),
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), //= 3,
    /*parameter  */ .BUS_ID_WIDTH               (BUS_ID_WIDTH               ), //= 8,
    /*parameter  */ .BUS_ADDR_WIDTH             (BUS_ADDR_WIDTH             ), //= 64,
    /*parameter  */ .BUS_USER_WIDTH             (BUS_USER_WIDTH             ), //= 8,
    /*parameter  */ .BUS_DATA_WIDTH             (BUS_DATA_WIDTH             ), //= 128,
    /*parameter type         */ .BUS_CH_AX_TYPE             (BUS_CH_AX_TYPE             ), // = iommu_acd_pkg::ch_ax_t,
    /*parameter type         */ .BUS_CH_W_TYPE              (BUS_CH_W_TYPE              ), // = iommu_acd_pkg::ch_w_t,
    /*parameter type         */ .BUS_CH_B_TYPE              (BUS_CH_B_TYPE              ), // = iommu_acd_pkg::ch_b_t,
    /*parameter  */ .SPARE_PARAM                (1'b0                       )  // = 0
    ) U_w_buf(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .s2q_valid_i                (s2q_valid                  ),
    /*input  logic                                      */  .s2q_ready_i                (s2q_ready                  ),
    /*input  logic                                      */  .s2q_wr_i                   (s2q_wr                     ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .s2q_token_i                (s2q_token                  ),
    /*input  logic [ 1:0]                               */  .s2q_bar_i                  (s2q_axpayld.axbar          ),
    /*input  logic                                      */  .s2w_wvalid_i               (s2w_wvalid                 ),
    /*output logic                                      */  .s2w_wready_o               (s2w_wready                 ),
    /*input  BUS_CH_W_TYPE                              */  .s2w_wpayld_i               (s2w_wpayld                 ),
    /*output logic [BUS_INFLY_TOKEN_NUM-1:0]            */  .wdata_ready_o              (wdata_ready                ),
    /*input  logic                                      */  .qout_valid_i               (qout_valid                 ),
    /*output logic                                      */  .qout_ready_o               (qout_ready_w_int           ),
    /*input  logic                                      */  .qout_wr_i                  (qout_wr                    ),
    /*input  BUS_CH_AX_TYPE                             */  .qout_axpayld_i             (qout_axpayld               ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .qout_token_i               (qout_token                 ),
    /*input  logic                                      */  .qout_fault_i               (qout_fault                 ),
    /*input  logic                                      */  .qout_mrif_i                (qout_mrif                  ),
    /*output logic                                      */  .w2m_awvalid_o              (w2m_awvalid                ),
    /*input  logic                                      */  .w2m_awready_i              (w2m_awready                ),
    /*output BUS_CH_AX_TYPE                             */  .w2m_awpayld_o              (w2m_awpayld                ),
    /*output logic                                      */  .w2m_wvalid_o               (w2m_wvalid                 ),
    /*input  logic                                      */  .w2m_wready_i               (w2m_wready                 ),
    /*output BUS_CH_W_TYPE                              */  .w2m_wpayld_o               (w2m_wpayld                 ),
    /*output logic                                      */  .w2m_token_o                (w2m_token                  ),
    /*output logic                                      */  .w2m_fault_o                (w2m_fault                  ),
    /*output logic                                      */  .w2m_mrif_o                 (w2m_mrif                   ),
    /*output logic                                      */  .wbuf_dequeue_fifo_empty_o  (wbuf_dequeue_fifo_empty    ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
);
//}}}

//MST-IF {{{
    iommu_acd_bus_handler_mst_intf #(
    /*parameter              */ .MST_AW_REGSLICE            (MST_AW_REGSLICE            ), // = 1,
    /*parameter              */ .MST_W_REGSLICE             (MST_W_REGSLICE             ), // = 1,
    /*parameter              */ .MST_AR_REGSLICE            (MST_AR_REGSLICE            ), // = 1,
    /*parameter              */ .MST_R_REGSLICE             (MST_R_REGSLICE             ), // = 1,
    /*parameter              */ .MST_B_REGSLICE             (MST_B_REGSLICE             ), // = 1,
    /*parameter  */ .BUS_PROPERTY_BAR           (BUS_PROPERTY_BAR           ), // = 0,
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), // = 6,
    /*parameter  */ .BUS_ADDR_WIDTH             (BUS_ADDR_WIDTH             ), // = 64,
    /*parameter  */ .BUS_DATA_WIDTH             (BUS_DATA_WIDTH             ), // = 128,
    /*parameter  */ .BUS_ID_WIDTH               (BUS_ID_WIDTH               ), // = 8,
    /*parameter  */ .BUS_USER_WIDTH             (BUS_USER_WIDTH             ), // = 8,
    /*parameter  */ .BUS_LOOP_WIDTH             (BUS_LOOP_WIDTH             ), // = 1,
    /*parameter type         */ .BUS_CH_AX_TYPE             (BUS_CH_AX_TYPE             ), // = iommu_acd_pkg::ch_ax_t,
    /*parameter type         */ .BUS_CH_W_TYPE              (BUS_CH_W_TYPE              ), // = iommu_acd_pkg::ch_w_t,
    /*parameter type         */ .BUS_CH_B_TYPE              (BUS_CH_B_TYPE              ), // = iommu_acd_pkg::ch_b_t,
    /*parameter type         */ .BUS_CH_R_TYPE              (BUS_CH_R_TYPE              ), // = iommu_acd_pkg::ch_r_t,
    /*parameter  */ .SPARE_PARAM                (0                          )  //= 0
    ) U_mst_intf(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*output logic                                      */  .mst_awvalid_o              (mst_awvalid_o              ),
    /*input  logic                                      */  .mst_awready_i              (mst_awready_i              ),
    /*output BUS_CH_AX_TYPE                             */  .mst_awpayld_o              (mst_awpayld_o              ),
    /*output logic                                      */  .mst_wvalid_o               (mst_wvalid_o               ),
    /*input  logic                                      */  .mst_wready_i               (mst_wready_i               ),
    /*output BUS_CH_W_TYPE                              */  .mst_wpayld_o               (mst_wpayld_o               ),
    /*input  logic                                      */  .mst_bvalid_i               (mst_bvalid_i               ),
    /*output logic                                      */  .mst_bready_o               (mst_bready_o               ),
    /*input  BUS_CH_B_TYPE                              */  .mst_bpayld_i               (mst_bpayld_i               ),
    /*output logic                                      */  .mst_arvalid_o              (mst_arvalid_o              ),
    /*input  logic                                      */  .mst_arready_i              (mst_arready_i              ),
    /*output BUS_CH_AX_TYPE                             */  .mst_arpayld_o              (mst_arpayld_o              ),
    /*input  logic                                      */  .mst_rvalid_i               (mst_rvalid_i               ),
    /*output logic                                      */  .mst_rready_o               (mst_rready_o               ),
    /*input  BUS_CH_R_TYPE                              */  .mst_rpayld_i               (mst_rpayld_i               ),
    /*input  logic                                      */  .w2m_awvalid_i              (mst_w2m_awvalid            ),
    /*output logic                                      */  .w2m_awready_o              (mst_w2m_awready            ),
    /*input  BUS_CH_AX_TYPE                             */  .w2m_awpayld_i              (mst_w2m_awpayld            ),
    /*input  logic                                      */  .w2m_wvalid_i               (mst_w2m_wvalid             ),
    /*output logic                                      */  .w2m_wready_o               (mst_w2m_wready             ),
    /*input  BUS_CH_W_TYPE                              */  .w2m_wpayld_i               (mst_w2m_wpayld             ),
    /*input  logic                                      */  .w2m_token_i                (mst_w2m_token              ),
    /*input  logic                                      */  .w2m_fault_i                (mst_w2m_fault              ),
    /*input  logic                                      */  .w2m_mrif_i                 (mst_w2m_mrif               ),
    /*output logic                                      */  .m2b_bvalid_o               (mst_m2b_bvalid             ),
    /*input  logic                                      */  .m2b_bready_i               (mst_m2b_bready             ),
    /*output BUS_CH_B_TYPE                              */  .m2b_bpayld_o               (mst_m2b_bpayld             ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .m2b_token_o                (mst_m2b_token              ),
    /*output logic                                      */  .m2b_mrif_o                 (mst_m2b_mrif               ),
    /*input  logic                                      */  .r2m_arvalid_i              (mst_r2m_arvalid            ),
    /*output logic                                      */  .r2m_arready_o              (mst_r2m_arready            ),
    /*input  BUS_CH_AX_TYPE                             */  .r2m_arpayld_i              (mst_r2m_arpayld            ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .r2m_token_i                (mst_r2m_token              ),
    /*input  logic                                      */  .r2m_fault_i                (mst_r2m_fault              ),
    /*input  logic                                      */  .r2m_mrif_i                 (mst_r2m_mrif               ),
    /*output logic                                      */  .m2r_rvalid_o               (m2r_rvalid                 ),
    /*input  logic                                      */  .m2r_rready_i               (m2r_rready                 ),
    /*output BUS_CH_R_TYPE                              */  .m2r_rpayld_o               (m2r_rpayld                 ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .m2r_token_o                (m2r_token                  ),
    /*input  logic                                      */  .q2m_valid_i                (q2m_valid                  ),
    /*output logic                                      */  .q2m_ready_o                (q2m_ready                  ),
    /*input  logic                                      */  .q2m_wr_i                   (q2m_wr                     ),
    /*input  BUS_CH_AX_TYPE                             */  .q2m_axpayld_i              (q2m_axpayld                ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .q2m_token_i                (q2m_token                  ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .q2m_pair_token_i           (q2m_pair_token             ),
    /*input  logic                                      */  .q2m_fault_i                (q2m_fault                  ),
    /*input  logic                                      */  .spare_in                   (1'b0                       )
);
//}}}

//B-BUF {{{
    iommu_acd_bus_handler_b_buf #(
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), //= 6,
    /*parameter  */ .BUS_ID_WIDTH               (BUS_ID_WIDTH               ), //= 8,
    /*parameter  */ .BUS_USER_WIDTH             (BUS_USER_WIDTH             ), //= 8,
    /*parameter type         */ .BUS_CH_B_TYPE              (BUS_CH_B_TYPE              ), // = iommu_acd_pkg::ch_b_t,
    /*parameter  */ .SPARE_PARAM                (0                          )  //= 0
    ) U_b_buf(                                                                          
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .m2b_bvalid_i               (m2b_bvalid                 ),
    /*output logic                                      */  .m2b_bready_o               (m2b_bready                 ),
    /*input  BUS_CH_B_TYPE                              */  .m2b_bpayld_i               (m2b_bpayld                 ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .m2b_token_i                (m2b_token                  ),
    /*input  logic                                      */  .s2q_valid_i                (s2q_valid                  ),
    /*input  logic                                      */  .s2q_ready_i                (s2q_ready                  ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .s2q_token_i                (s2q_token                  ),
    /*input  logic                                      */  .s2q_wr_i                   (s2q_wr                     ),
    /*input  logic                                      */  .b2s_bvalid_o               (b2s_bvalid                 ),
    /*output logic                                      */  .b2s_bready_i               (b2s_bready                 ),
    /*output BUS_CH_B_TYPE                              */  .b2s_bpayld_o               (b2s_bpayld                 ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .b2s_token_o                (b2s_token                  ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
);

//}}}

//TRANS-QUEUE {{{
    iommu_acd_bus_handler_trans_queue #(
    /*parameter  */ .BUS_PROPERTY_BAR           (BUS_PROPERTY_BAR           ), // = 0,
    /*parameter  */ .TRANS_QUEUE_IDX_WIDTH      (TRANS_QUEUE_IDX_WIDTH      ), // = 3,
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), // = 6,
    /*parameter  */ .BUS_ADDR_WIDTH             (BUS_ADDR_WIDTH             ), // = 64,
    /*parameter  */ .BUS_DATA_WIDTH             (BUS_DATA_WIDTH             ), // = 128,
    /*parameter  */ .BUS_ID_WIDTH               (BUS_ID_WIDTH               ), // = 8,
    /*parameter  */ .BUS_USER_WIDTH             (BUS_USER_WIDTH             ), // = 8,
    /*parameter type         */ .BUS_CH_AX_TYPE             (BUS_CH_AX_TYPE             ), // = iommu_acd_pkg::ch_ax_t,
    /*parameter type         */ .TRANSLATE_REQ_TYPE         (TRANSLATE_REQ_TYPE         ), // = logic,
    /*parameter type         */ .TRANSLATE_ACK_TYPE         (TRANSLATE_ACK_TYPE         ), // = logic,
    /*parameter  */ .SPARE_PARA                 (0                          )  // = 0
    ) U_trans_queue(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .s2q_valid_i                (s2q_valid                  ),
    /*output logic                                      */  .s2q_ready_o                (s2q_ready                  ),
    /*input  logic                                      */  .s2q_bc_fail_i              (s2q_bc_fail                ),
    /*input  logic                                      */  .s2q_wr_i                   (s2q_wr                     ),
    /*input  logic [23:0]                               */  .s2q_device_id_i            (s2q_device_id              ),
    /*input  logic                                      */  .s2q_process_id_valid_i     (s2q_process_id_valid       ),
    /*input  logic [19:0]                               */  .s2q_process_id_i           (s2q_process_id             ),
    /*input  logic                                      */  .s2q_is_translated_i        (s2q_is_translated          ),
    /*input  BUS_CH_AX_TYPE                             */  .s2q_axpayld_i              (s2q_axpayld                ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .s2q_token_i                (s2q_token                  ),
    /*output logic                                      */  .ptw_req_valid_o            (translate_req_valid_o      ),
    /*input  logic                                      */  .ptw_req_ready_i            (translate_req_ready_i      ),
    /*output TRANSLATE_REQ_TYPE                         */  .ptw_req_o                  (translate_req_o            ),
    /*input  logic                                      */  .ptw_ack_valid_i            (translate_ack_valid_i      ),
    /*input  TRANSLATE_ACK_TYPE                         */  .ptw_ack_i                  (translate_ack_i            ),
    /*output logic                                      */  .mrif_credit_grant_valid_o  (mrif_credit_grant_valid_o  ),
    /*input  logic [BUS_INFLY_TOKEN_NUM-1:0]            */  .wdata_ready_i              (wdata_ready                ),  // valid WDATA got stored
    /*output logic [TRANS_QUEUE_IDX_WIDTH-1:0]          */  .enqueue_qidx_o             (                           ),
    /*input  logic                                      */  .wbuf_dequeue_fifo_empty_i  (wbuf_dequeue_fifo_empty    ),
    /*output logic                                      */  .s2q_w_can_enqueue_hint_o   (s2q_w_can_enqueue_hint     ),
    /*output logic                                      */  .qout_valid_o               (qout_valid                 ),
    /*input  logic                                      */  .qout_ready_i               (qout_ready                 ),
    /*output logic                                      */  .qout_wr_o                  (qout_wr                    ),
    /*output BUS_CH_AX_TYPE                             */  .qout_axpayld_o             (qout_axpayld               ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .qout_token_o               (qout_token                 ),
    /*output logic                                      */  .qout_fault_o               (qout_fault                 ),
    /*output logic                                      */  .qout_mrif_o                (qout_mrif                  ),
    /*input  logic                                      */  .mrif_done_i                (mrif_done                  ),
    /*output logic                                      */  .mrif_valid_o               (mrif_valid                 ),
    /*output logic [55:12]                              */  .mrif_nppn_o                (mrif_nppn                  ),
    /*output logic [10:0]                               */  .mrif_nid_o                 (mrif_nid                   ),
    /*output logic [BUS_ID_WIDTH-1:0]                   */  .mrif_axid_o                (mrif_axid                  ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .mrif_token_o               (mrif_token                 ),
    /*output logic                                      */  .q2m_valid_o                (q2m_valid                  ),
    /*input  logic                                      */  .q2m_ready_i                (q2m_ready                  ),
    /*output logic                                      */  .q2m_wr_o                   (q2m_wr                     ),
    /*output BUS_CH_AX_TYPE                             */  .q2m_axpayld_o              (q2m_axpayld                ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .q2m_token_o                (q2m_token                  ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .q2m_pair_token_o           (q2m_pair_token             ),
    /*output logic                                      */  .q2m_fault_o                (q2m_fault                  ),
    /*input  logic                                      */  .spare_in                   (1'b0                       )   
);

//}}}

// R-BUF {{{
    iommu_acd_bus_handler_r_buf #(
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), //= 6,
    /*parameter  */ .BUS_ID_WIDTH               (BUS_ID_WIDTH               ), //= 8,
    /*parameter  */ .BUS_ADDR_WIDTH             (BUS_ADDR_WIDTH             ), //= 64,
    /*parameter  */ .BUS_USER_WIDTH             (BUS_USER_WIDTH             ), //= 8,
    /*parameter  */ .BUS_DATA_WIDTH             (BUS_DATA_WIDTH             ), //= 128,
    /*parameter type         */ .BUS_CH_AX_TYPE             (BUS_CH_AX_TYPE             ), // = iommu_acd_pkg::ch_ax_t,
    /*parameter type         */ .BUS_CH_R_TYPE              (BUS_CH_R_TYPE              ), // = iommu_acd_pkg::ch_r_t,
    /*parameter  */ .SPARE_PARAM                (0                          )  //= 0
    ) U_r_buf(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .s2q_valid_i                (s2q_valid                  ),
    /*input  logic                                      */  .s2q_ready_i                (s2q_ready                  ),
    /*input  logic                                      */  .s2q_wr_i                   (s2q_wr                     ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .s2q_token_i                (s2q_token                  ),
    /*output logic                                      */  .r2s_rvalid_o               (r2s_rvalid                 ),
    /*input  logic                                      */  .r2s_rready_i               (r2s_rready                 ),
    /*output BUS_CH_R_TYPE                              */  .r2s_rpayld_o               (r2s_rpayld                 ),
    /*output logic                                      */  .r2s_token_o                (r2s_token                  ),
    /*input  logic                                      */  .qout_valid_i               (qout_valid                 ),
    /*output logic                                      */  .qout_ready_o               (qout_ready_r_int           ),
    /*input  logic                                      */  .qout_wr_i                  (qout_wr                    ),
    /*input  BUS_CH_AX_TYPE                             */  .qout_axpayld_i             (qout_axpayld               ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .qout_token_i               (qout_token                 ),
    /*input  logic                                      */  .qout_fault_i               (qout_fault                 ),
    /*input  logic                                      */  .qout_mrif_i                (qout_mrif                  ),
    /*output logic                                      */  .r2m_arvalid_o              (r2m_arvalid                ),
    /*input  logic                                      */  .r2m_arready_i              (r2m_arready                ),
    /*output BUS_CH_AX_TYPE                             */  .r2m_arpayld_o              (r2m_arpayld                ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .r2m_token_o                (r2m_token                  ),
    /*output logic                                      */  .r2m_fault_o                (r2m_fault                  ),
    /*output logic                                      */  .r2m_mrif_o                 (r2m_mrif                   ),
    /*input  logic                                      */  .m2r_rvalid_i               (m2r_rvalid                 ),
    /*output logic                                      */  .m2r_rready_o               (m2r_rready                 ),
    /*input  BUS_CH_R_TYPE                              */  .m2r_rpayld_i               (m2r_rpayld                 ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .m2r_token_i                (m2r_token                  ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
);
//}}}

// HPM {{{
    iommu_acd_riscv_hpm_bus_handler #(
    /*parameter type         */ .TRANSLATE_REQ_TYPE         (TRANSLATE_REQ_TYPE         ) // = iommu_acd_pkg::TRANSLATE_REQ_TYPE
    ) U_riscv_hpm(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .translate_req_valid_i      (translate_req_valid_o      ),
    /*input  logic                                      */  .translate_req_ready_i      (translate_req_ready_i      ),
    /*input  TRANSLATE_REQ_TYPE                         */  .translate_req_i            (translate_req_o            ),
    /*output iommu_acd_pkg::RISCV_HPMEVT_TYPE           */  .riscv_hpmevt_intf_o        (riscv_hpmevt_intf_o        ),
    /*input  logic                                      */  .spare_in                   (1'b0                       )
    );

//    iommu_acd_hpm_bus_handler U_hpm(
//    /*input  logic                                      */  .clk                        (clk                        ),
//    /*input  logic                                      */  .rstn                       (rstn                       ),
//    /*input  logic                                      */  .slv_awvalid_i              (slv_awvalid_i              ),
//    /*input  logic                                      */  .slv_awready_i              (slv_awready_o              ),
//    /*input  logic                                      */  .slv_arvalid_i              (slv_arvalid_i              ),
//    /*input  logic                                      */  .slv_arready_i              (slv_arready_o              ),
//    /*input  logic                                      */  .mst_awvalid_i              (mst_awvalid_o              ),
//    /*input  logic                                      */  .mst_awready_i              (mst_awready_i              ),
//    /*input  logic                                      */  .mst_arvalid_i              (mst_arvalid_o              ),
//    /*input  logic                                      */  .mst_arready_i              (mst_arready_i              ),
//    /*input  logic [3:0]                                */  .hpm_cnt_inhibit_i          (hpm_cnt_inhibit_i          ),
//    /*output logic [63:0]                               */  .hpm_cnt_o                  (hpm_cnt_o                  ), //[3:0],
//    /*input  logic                                      */  .spare_in                   (1'b0                       )
//    );

//}}}

// MRIF {{{
    iommu_acd_bus_handler_mrif_agent #(
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), // = 6,
    /*parameter  */ .BUS_ID_WIDTH               (BUS_ID_WIDTH               ), // = 8,
    /*parameter  */ .BUS_ADDR_WIDTH             (64                         ), // = 64,
    /*parameter  */ .BUS_USER_WIDTH             (BUS_USER_WIDTH             ), // = 8,
    /*parameter  */ .BUS_DATA_WIDTH             (BUS_DATA_WIDTH             ), // = 128,
    /*parameter  */ .BUS_SIZE_WIDTH             (BUS_SIZE_WIDTH             ), // = 3,
    /*parameter  */ .BUS_STRB_WIDTH             (BUS_STRB_WIDTH             ), // = BUS_DATA_WIDTH/8,
    /*parameter type         */ .BUS_CH_AX_TYPE             (BUS_CH_AX_TYPE             ), // = iommu_acd_pkg::ch_ax_t,
    /*parameter type         */ .BUS_CH_W_TYPE              (BUS_CH_W_TYPE              ), // = iommu_acd_pkg::ch_w_t,
    /*parameter type         */ .BUS_CH_B_TYPE              (BUS_CH_B_TYPE              ), // = iommu_acd_pkg::ch_b_t,
    /*parameter  */ .SPARE_PARAM                (0                          )  // = 0
    ) U_mrif_agent(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*output logic                                      */  .mrif_done_o                (mrif_done                  ),
    /*input  logic                                      */  .mrif_valid_i               (mrif_valid                 ),
    /*input  logic [55:12]                              */  .mrif_nppn_i                (mrif_nppn                  ),
    /*input  logic [10:0]                               */  .mrif_nid_i                 (mrif_nid                   ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .mrif_axid_i                (mrif_axid                  ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .mrif_token_i               (mrif_token                 ),
    /*input  logic                                      */  .r2m_arvalid_i              (r2m_arvalid                ),
    /*output logic                                      */  .r2m_arready_o              (r2m_arready                ),
    /*input  BUS_CH_AX_TYPE                             */  .r2m_arpayld_i              (r2m_arpayld                ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .r2m_token_i                (r2m_token                  ),
    /*input  logic                                      */  .r2m_fault_i                (r2m_fault                  ),
    /*input  logic                                      */  .r2m_mrif_i                 (r2m_mrif                   ),
    /*output logic                                      */  .r2m_arvalid_o              (mst_r2m_arvalid            ),
    /*input  logic                                      */  .r2m_arready_i              (mst_r2m_arready            ),
    /*output BUS_CH_AX_TYPE                             */  .r2m_arpayld_o              (mst_r2m_arpayld            ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .r2m_token_o                (mst_r2m_token              ),
    /*output logic                                      */  .r2m_fault_o                (mst_r2m_fault              ),
    /*output logic                                      */  .r2m_mrif_o                 (mst_r2m_mrif               ),
    /*input  logic                                      */  .w2m_awvalid_i              (w2m_awvalid                ),
    /*output logic                                      */  .w2m_awready_o              (w2m_awready                ),
    /*input  BUS_CH_AX_TYPE                             */  .w2m_awpayld_i              (w2m_awpayld                ),
    /*input  logic                                      */  .w2m_wvalid_i               (w2m_wvalid                 ),
    /*output logic                                      */  .w2m_wready_o               (w2m_wready                 ),
    /*input  BUS_CH_W_TYPE                              */  .w2m_wpayld_i               (w2m_wpayld                 ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .w2m_token_i                (w2m_token                  ),
    /*input  logic                                      */  .w2m_fault_i                (w2m_fault                  ),
    /*input  logic                                      */  .w2m_mrif_i                 (w2m_mrif                   ),
    /*output logic                                      */  .w2m_awvalid_o              (mst_w2m_awvalid            ),
    /*input  logic                                      */  .w2m_awready_i              (mst_w2m_awready            ),
    /*output BUS_CH_AX_TYPE                             */  .w2m_awpayld_o              (mst_w2m_awpayld            ),
    /*output logic                                      */  .w2m_wvalid_o               (mst_w2m_wvalid             ),
    /*input  logic                                      */  .w2m_wready_i               (mst_w2m_wready             ),
    /*output BUS_CH_W_TYPE                              */  .w2m_wpayld_o               (mst_w2m_wpayld             ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .w2m_token_o                (mst_w2m_token              ),
    /*output logic                                      */  .w2m_fault_o                (mst_w2m_fault              ),
    /*output logic                                      */  .w2m_mrif_o                 (mst_w2m_mrif               ),
    /*input  logic                                      */  .m2b_bvalid_i               (mst_m2b_bvalid             ),
    /*output logic                                      */  .m2b_bready_o               (mst_m2b_bready             ),
    /*input  BUS_CH_B_TYPE                              */  .m2b_bpayld_i               (mst_m2b_bpayld             ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .m2b_token_i                (mst_m2b_token              ),
    /*input  logic                                      */  .m2b_mrif_i                 (mst_m2b_mrif               ),
    /*output logic                                      */  .m2b_bvalid_o               (m2b_bvalid                 ),
    /*input  logic                                      */  .m2b_bready_i               (m2b_bready                 ),
    /*output BUS_CH_B_TYPE                              */  .m2b_bpayld_o               (m2b_bpayld                 ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .m2b_token_o                (m2b_token                  ),
    /*output logic                                      */  .m2b_mrif_o                 (m2b_mrif                   ),
    /*input  logic                                      */  .spare_in                   (1'b0                       )
);
//}}}

//}}}

`ifdef IOMMU_IDBG
    iommu_acd_mon_unit_translate_intf #(
    /*parameter  */ .RAM_IDX_WIDTH              (`IOMMU_IDBG_TRANSLATEINTF_RAM_IDX_WIDTH), // = 6,
    /*parameter  */ .SPARE_PARAM                (1'b0                                   )  // = 0
    ) U_idbg(
    /*input  logic                                      */  .clk                                    (clk                                            ),
    /*input  logic                                      */  .rstn                                   (rstn                                           ),
    /*input  logic                                      */  .idbg_go_i                              (idbg_intf_m_i.idbg_go                          ),
    /*output logic                                      */  .idbg_busy_o                            (idbg_intf_s_o.idbg_busy                        ),
    /*input  logic [7:0]                                */  .idbg_opcode_i                          (idbg_intf_m_i.idbg_opcode                      ),
    /*input  logic [31:0]                               */  .idbg_dat_i                             (idbg_intf_m_i.idbg_datw                        ),
    /*output logic [31:0]                               */  .idbg_dat_o                             (idbg_intf_s_o.idbg_datr                        ),
    /*output logic                                      */  .idbg_datv_o                            (idbg_intf_s_o.idbg_datv                        ),
    /*input  logic [63:0]                               */  .idbg_timer_i                           (idbg_scnt_i                                    ),
    /*input  logic                                      */  .translate_req_valid_i                  (translate_req_valid_o & translate_req_ready_i  ),
    /*input  TRANSLATE_REQ_TYPE                         */  .translate_req_i                        (translate_req_o                                ),
    /*input  logic                                      */  .translate_ack_valid_i                  (translate_ack_valid_i                          ),
    /*input  TRANSLATE_ACK_TYPE                         */  .translate_ack_i                        (translate_ack_i                                ),
    /*input  logic                                      */  .spare_in                               (1'b0                                           )
    );
`endif

//  //=== SIMULATION INFO {{{
//  `ifdef IOMMU_SIMULATION
//      always@(negedge clk) begin
//      // input info
//          if(slv_awvalid_i & slv_awready_o) begin
//              $display("[IOMMU_SIM] NEW_SLV_AW_INPUT_GOT. %t", $time);
//              $display("%m %s %0d", `__FILE__, `__LINE__);
//              $display("AWID     AWADDR           AWLEN    AWSIZE  AWBURST  AWLOCK  AWCACHE  AWPORT  AWREGION  AWUSER   AWQOS  DID    PID   PIDV  TRANSED ");
//              $display("%08h,%16h,%08h,%07h,%08h,%07h,%08b,%07b,%09b,%08h,%06b,%06h,%05h,%05d,%08d",
//                          slv_awid_i      ,
//                          slv_awaddr_i    ,
//                          slv_awlen_i     ,
//                          slv_awsize_i    ,
//                          slv_awburst_i   ,
//                          slv_awlock_i    ,
//                          slv_awcache_i   ,
//                          slv_awprot_i    ,
//                          slv_awregion_i  ,
//                          slv_awuser_i    ,
//                          slv_awqos_i     ,
//                          slv_aw_device_id_i,
//                          slv_aw_process_id_i,
//                          slv_aw_process_id_valid_i,
//                          slv_aw_is_translated_i
//              );
//          end
//          if(slv_bvalid_o & slv_bready_i) begin
//              $display("[IOMMU_SIM] NEW_SLV_B_RESP_SENT. %t", $time);
//              $display("BID      BRESP   BUSER");
//              $display("%08h,%7h,%08h,",
//                          slv_bid_o       ,
//                          slv_bresp_o     ,
//                          slv_buser_o     
//              );
//          end
//          if(slv_arvalid_i & slv_arready_o) begin
//              $display("[IOMMU_SIM] NEW_SLV_AW_INPUT_GOT. %t", $time);
//              $display("AWID     AWADDR           AWLEN    AWSIZE  AWBURST  AWLOCK  AWCACHE  AWPORT  AWREGION  AWUSER   AWQOS  DID    PID   PIDV  TRANSED ");
//              $display("%08h,%16h,%08h,%07h,%08h,%07h,%08b,%07b,%09b,%08h,%06b,%06h,%05h,%05d,%08d",
//                          slv_awid_i      ,
//                          slv_awaddr_i    ,
//                          slv_awlen_i     ,
//                          slv_awsize_i    ,
//                          slv_awburst_i   ,
//                          slv_awlock_i    ,
//                          slv_awcache_i   ,
//                          slv_awprot_i    ,
//                          slv_awregion_i  ,
//                          slv_awuser_i    ,
//                          slv_awqos_i     ,
//                          slv_aw_device_id_i,
//                          slv_aw_process_id_i,
//                          slv_aw_process_id_valid_i,
//                          slv_aw_is_translated_i
//              );
//          end
//          if(mst_awvalid_o & mst_awready_i) begin
//              $display("[IOMMU_SIM] NEW_MST_AW_OUTPUT_SENT. %t", $time);
//              $display("AWID     AWADDR           AWLEN    AWSIZE  AWBURST  AWLOCK  AWCACHE  AWPORT  AWREGION  AWUSER   AWQOS");
//              $display("%08h,%16h,%08h,%07h,%08h,%07h,%08b,%07b,%09b,%08h,%06b",
//                          mst_awid_o      ,
//                          mst_awaddr_o    ,
//                          mst_awlen_o     ,
//                          mst_awsize_o    ,
//                          mst_awburst_o   ,
//                          mst_awlock_o    ,
//                          mst_awcache_o   ,
//                          mst_awprot_o    ,
//                          mst_awregion_o  ,
//                          mst_awuser_o    ,
//                          mst_awqos_o     
//              );
//          end
//          if(mst_bvalid_i & mst_bready_o) begin
//              $display("[IOMMU_SIM] NEW_MST_B_RESP_GOT. %t", $time);
//              $display("BID      BRESP   BUSER");
//              $display("%08h,%7h,%08h,",
//                          mst_bid_i       ,
//                          mst_bresp_i     ,
//                          mst_buser_i     
//              );
//          end
//          if(mst_arvalid_o & mst_arready_i) begin
//              $display("[IOMMU_SIM] NEW_MST_AR_OUTPUT_SENT. %t", $time);
//              $display("ARID     ARADDR           ARLEN    ARSIZE  ARBURST  ARLOCK  ARCACHE  ARPORT  ARREGION  ARUSER   ARQOS");
//              $display("%08h,%16h,%08h,%07h,%08h,%07h,%08b,%07b,%09b,%08h,%06b",
//                          mst_arid_o      ,
//                          mst_araddr_o    ,
//                          mst_arlen_o     ,
//                          mst_arsize_o    ,
//                          mst_arburst_o   ,
//                          mst_arlock_o    ,
//                          mst_arcache_o   ,
//                          mst_arprot_o    ,
//                          mst_arregion_o  ,
//                          mst_aruser_o    ,
//                          mst_arqos_o     
//              );
//          end
//          if(ptw_req_valid_o & ptw_req_ready_i) begin
//              $display("[IOMMU_SIM] NEW_TRANS-REQ_SENT. %t", $time);
//              $display("TQIDX    DID    PID   PIDV  TRANSED VPN              P  X  W/R");
//              $display("%08h,%06h,%05h,%05d,%07d,%16h,%2b,%2b,%2b",
//                          ptw_req_o.idx,
//                          ptw_req_o.device_id,
//                          ptw_req_o.process_id,
//                          ptw_req_o.process_id_valid,
//                          ptw_req_o.is_translated,
//                          ptw_req_o.va,
//                          ptw_req_o.priv,
//                          ptw_req_o.ext,
//                          ptw_req_o.wr
//              );
//          end
//          if(ptw_ack_valid_i) begin
//              $display("[IOMMU_SIM] NEW_TRANS-ACK_GOT. %t", $time);
//              $display("TQIDX    RESP  PA");
//              $display("%08h,%5b,%16h",
//                          ptw_ack_i.idx,
//                          ptw_ack_i.resp,
//                          ptw_ack_i.pa
//              );
//          end
//      end
//  `endif
//  //}}}
endmodule










