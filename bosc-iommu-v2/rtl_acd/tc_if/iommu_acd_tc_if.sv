

module iommu_acd_tc_if import iommu_acd_pkg::*; #(
    parameter   INV_IDX_WIDTH               = 4,                    // should not bigger than 4
    parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH,
    parameter   FAULT_TOKEN_WIDTH           = 5,                    // should not bigger than 12
    parameter   FAULT_INFLY_NUM             = 2**FAULT_TOKEN_WIDTH,
    parameter   PTW_IDX_WIDTH               = 8,                    // should not bigger than 12
    parameter   PTW_INFLY_NUM               = 2**PTW_IDX_WIDTH,
    parameter   CTIF_DATA_WIDTH             = 64,
    parameter   CTIF_STRB_WIDTH             = CTIF_DATA_WIDTH/8,
    parameter   CTIF_ID_WIDTH               = 4,
    parameter   CTIF_DEST_WIDTH             = 4,
    parameter   CTIF_USER_WIDTH             = 1,
    parameter   SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // C2T                                              
    output logic                                        c2t_tvalid_o,
    input  logic                                        c2t_tready_i,
    output logic [CTIF_DATA_WIDTH-1:0]                  c2t_tdata_o,
    output logic [CTIF_STRB_WIDTH-1:0]                  c2t_tstrb_o,
    output logic [CTIF_STRB_WIDTH-1:0]                  c2t_tkeep_o,
    output logic                                        c2t_tlast_o,
    output logic [CTIF_ID_WIDTH-1:0]                    c2t_tid_o,
    output logic [CTIF_DEST_WIDTH-1:0]                  c2t_tdest_o,
    output logic [CTIF_USER_WIDTH-1:0]                  c2t_tuser_o,
    // T2C                                              
    input  logic                                        t2c_tvalid_i,
    output logic                                        t2c_tready_o,
    input  logic [CTIF_DATA_WIDTH-1:0]                  t2c_tdata_i,
    input  logic [CTIF_STRB_WIDTH-1:0]                  t2c_tstrb_i,
    input  logic [CTIF_STRB_WIDTH-1:0]                  t2c_tkeep_i,
    input  logic                                        t2c_tlast_i,
    input  logic [CTIF_ID_WIDTH-1:0]                    t2c_tid_i,
    input  logic [CTIF_DEST_WIDTH-1:0]                  t2c_tdest_i,
    input  logic [CTIF_USER_WIDTH-1:0]                  t2c_tuser_i,
    // PTW                                              
    output logic                                        msg_pvalid_o,
    input  logic                                        msg_pready_i,
    output logic [63:0]                                 msg_pwdata_o,
    input  logic                                        msg_pvalid_i,
    output logic                                        msg_pready_o,
    input  logic [63:0]                                 msg_pdata_i,
    input  logic                                        msg_plast_i,
    // INV                                              
    output logic                                        msg_ivalid_o,
    input  logic                                        msg_iready_i,
    output logic [63:0]                                 msg_iwdata_o,
    input  logic                                        msg_ivalid_i,
    output logic                                        msg_iready_o,
    input  MSG_INV_ACK_TYPE                             msg_idata_i,
    input  logic                                        msg_ilast_i,
    // FAULT                                            
    output logic                                        msg_fvalid_o,
    input  logic                                        msg_fready_i,
    output MSG_FAULT_ACK_TYPE                           msg_fwdata_o,
    input  logic                                        msg_fvalid_i,
    output logic                                        msg_fready_o,
    input  logic [63:0]                                 msg_fdata_i,
    input  logic                                        msg_flast_i,
    // CFG                                              
    output logic                                        msg_cvalid_o,
    input  logic                                        msg_cready_i,
    output MSG_CFG_ACCESS_TYPE                          msg_cwdata_o,
    input  logic                                        msg_rvalid_i,
    output logic                                        msg_rready_o,
    input  MSG_CFG_ACK_TYPE                             msg_rdata_i,
    input  logic                                        msg_rlast_i,
    // INTERRUPT
    input  logic                                        msg_nvalid_i,
    output logic                                        msg_nready_o,
    input  MSG_INT_TYPE                                 msg_ndata_i,
    input  logic                                        msg_nlast_i,
    // DBG
    output logic                                        msg_gvalid_o,
    input  logic                                        msg_gready_i,
    output logic [63:0]                                 msg_gwdata_o,
    input  logic                                        msg_gvalid_i,
    output logic                                        msg_gready_o,
    input  MSG_DBG_ACK_TYPE                             msg_gdata_i,
    input  logic                                        msg_glast_i,
    //                                                  
    input  logic [CTIF_ID_WIDTH-1:0]                    acd_tid_i,
    input  logic [CTIF_DEST_WIDTH-1:0]                  acd_tdest_i,
    //                                                  
    input  logic                                        spare_in         
//}}}
);
//=== Declare {{{
    typedef struct packed {
    logic [CTIF_DATA_WIDTH-1:0]                  t2c_tdata_i ;
    logic [CTIF_STRB_WIDTH-1:0]                  t2c_tstrb_i ;
    logic [CTIF_STRB_WIDTH-1:0]                  t2c_tkeep_i ;
    logic                                        t2c_tlast_i ;
    logic [CTIF_ID_WIDTH-1:0]                    t2c_tid_i   ;
    logic [CTIF_DEST_WIDTH-1:0]                  t2c_tdest_i ;
    logic [CTIF_USER_WIDTH-1:0]                  t2c_tuser_i ;
    } t2c_t;

    typedef struct packed {
    logic [CTIF_DATA_WIDTH-1:0]                  c2t_tdata_o ;
    logic [CTIF_STRB_WIDTH-1:0]                  c2t_tstrb_o ;
    logic [CTIF_STRB_WIDTH-1:0]                  c2t_tkeep_o ;
    logic                                        c2t_tlast_o ;
    logic [CTIF_ID_WIDTH-1:0]                    c2t_tid_o   ;
    logic [CTIF_DEST_WIDTH-1:0]                  c2t_tdest_o ;
    logic [CTIF_USER_WIDTH-1:0]                  c2t_tuser_o ;
    } c2t_t;


    logic                                               msg_connect_ack_got;
    logic                                               tc_if_is_connected ;
//}}}


//=== C2T IF {{{
    c2t_t   c2t_a4s_dest, c2t_a4s_src;
    logic   c2t_a4s_src_valid, c2t_a4s_dest_valid;
    logic   c2t_a4s_src_ready, c2t_a4s_dest_ready;
    assign c2t_tvalid_o= c2t_a4s_dest_valid;
    assign c2t_a4s_dest_ready= c2t_tready_i;
    assign c2t_tdata_o = c2t_a4s_dest.c2t_tdata_o;
    assign c2t_tstrb_o = c2t_a4s_dest.c2t_tstrb_o;
    assign c2t_tkeep_o = c2t_a4s_dest.c2t_tkeep_o;
    assign c2t_tlast_o = c2t_a4s_dest.c2t_tlast_o;
    assign c2t_tid_o   = c2t_a4s_dest.c2t_tid_o  ;
    assign c2t_tdest_o = c2t_a4s_dest.c2t_tdest_o;
    assign c2t_tuser_o = c2t_a4s_dest.c2t_tuser_o;
    
    iommu_acd_bus_handler_regslice #(
    /*parameter  */ .PAYLD_WIDTH                ($bits(c2t_a4s_dest)        )  // = 8
    ) U_c2t_regslice(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .valid_src                  (c2t_a4s_src_valid          ),
    /*output logic                                      */  .ready_src                  (c2t_a4s_src_ready          ),
    /*input  logic [PAYLD_WIDTH-1:0]                    */  .payload_src                (c2t_a4s_src                ),
    /*output logic                                      */  .valid_dst                  (c2t_a4s_dest_valid         ),
    /*input  logic                                      */  .ready_dst                  (c2t_a4s_dest_ready         ),
    /*output logic [PAYLD_WIDTH-1:0]                    */  .payload_dst                (c2t_a4s_dest               )
    );


    iommu_acd_c2t_if #(
    /*parameter  */ .INV_IDX_WIDTH              (INV_IDX_WIDTH              ), // = 4,
    /*parameter  */ .FAULT_TOKEN_WIDTH          (FAULT_TOKEN_WIDTH          ), // = 5,
    /*parameter  */ .PTW_IDX_WIDTH              (PTW_IDX_WIDTH              ), // = 8,
    /*parameter  */ .CTIF_DATA_WIDTH            (64                         ), // = 64,
    /*parameter  */ .CTIF_ID_WIDTH              (CTIF_ID_WIDTH              ), // = 4,
    /*parameter  */ .CTIF_DEST_WIDTH            (CTIF_DEST_WIDTH            ), // = 4,
    /*parameter  */ .CTIF_USER_WIDTH            (1                          ), // = 1,
    /*parameter  */ .SPARE_PARAM                (0                          )  // = 0
    ) U_c2t(                                                                            
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*output logic                                      */  .c2t_tvalid_o               (c2t_a4s_src_valid          ), //(c2t_tvalid_o       ),
    /*input  logic                                      */  .c2t_tready_i               (c2t_a4s_src_ready          ), //(c2t_tready_i       ),
    /*output logic [CTIF_DATA_WIDTH-1:0]                */  .c2t_tdata_o                (c2t_a4s_src.c2t_tdata_o    ), //(c2t_tdata_o        ),
    /*output logic [CTIF_STRB_WIDTH-1:0]                */  .c2t_tstrb_o                (c2t_a4s_src.c2t_tstrb_o    ), //(c2t_tstrb_o        ), //
    /*output logic [CTIF_STRB_WIDTH-1:0]                */  .c2t_tkeep_o                (c2t_a4s_src.c2t_tkeep_o    ), //(c2t_tkeep_o        ), //
    /*output logic                                      */  .c2t_tlast_o                (c2t_a4s_src.c2t_tlast_o    ), //(c2t_tlast_o        ),
    /*output logic [CTIF_ID_WIDTH-1:0]                  */  .c2t_tid_o                  (c2t_a4s_src.c2t_tid_o      ), //(c2t_tid_o          ), //
    /*output logic [CTIF_DEST_WIDTH-1:0]                */  .c2t_tdest_o                (c2t_a4s_src.c2t_tdest_o    ), //(c2t_tdest_o        ), //
    /*output logic [CTIF_USER_WIDTH-1:0]                */  .c2t_tuser_o                (c2t_a4s_src.c2t_tuser_o    ), //(c2t_tuser_o        ), //
    /*input  logic                                      */  .msg_pvalid_i               (msg_pvalid_i               ),
    /*output logic                                      */  .msg_pready_o               (msg_pready_o               ),
    /*input  logic [63:0]                               */  .msg_pdata_i                (msg_pdata_i                ),
    /*input  logic                                      */  .msg_plast_i                (msg_plast_i                ),
    /*input  logic                                      */  .msg_ivalid_i               (msg_ivalid_i               ),
    /*output logic                                      */  .msg_iready_o               (msg_iready_o               ),
    /*input  MSG_INV_ACK_TYPE                           */  .msg_idata_i                (msg_idata_i                ),
    /*input  logic                                      */  .msg_ilast_i                (msg_ilast_i                ),
    /*input  logic                                      */  .msg_fvalid_i               (msg_fvalid_i               ),
    /*output logic                                      */  .msg_fready_o               (msg_fready_o               ),
    /*input  logic [63:0]                               */  .msg_fdata_i                (msg_fdata_i                ),
    /*input  logic                                      */  .msg_flast_i                (msg_flast_i                ),
    /*input  logic                                      */  .msg_rvalid_i               (msg_rvalid_i               ),
    /*output logic                                      */  .msg_rready_o               (msg_rready_o               ),
    /*input  MSG_CFG_ACK_TYPE                           */  .msg_rdata_i                (msg_rdata_i                ),
    /*input  logic                                      */  .msg_rlast_i                (msg_rlast_i                ),
    /*input  logic                                      */  .msg_nvalid_i               (msg_nvalid_i               ),
    /*output logic                                      */  .msg_nready_o               (msg_nready_o               ),
    /*input  MSG_INT_TYPE                               */  .msg_ndata_i                (msg_ndata_i                ),
    /*input  logic                                      */  .msg_nlast_i                (msg_nlast_i                ),
    /*input  logic                                      */  .msg_gvalid_i               (msg_gvalid_i               ),
    /*output logic                                      */  .msg_gready_o               (msg_gready_o               ),
    /*input  MSG_DBG_ACK_TYPE                           */  .msg_gdata_i                (msg_gdata_i                ),
    /*input  logic                                      */  .msg_glast_i                (msg_glast_i                ),
    /*input  logic [CTIF_ID_WIDTH-1:0]                  */  .acd_tid_i                  (acd_tid_i                  ),
    /*input  logic [CTIF_DEST_WIDTH-1:0]                */  .acd_tdest_i                (acd_tdest_i                ),
    /*input  logic                                      */  .msg_connect_ack_got_i      (msg_connect_ack_got        ),
    /*output logic                                      */  .tc_if_is_connected_o       (tc_if_is_connected         ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
    );
//}}}

//=== T2C IF {{{
    t2c_t  t2c_a4s_dest, t2c_a4s_src;
    logic  t2c_a4s_src_valid, t2c_a4s_dest_valid;
    logic  t2c_a4s_src_ready, t2c_a4s_dest_ready;
    assign t2c_a4s_src_valid = t2c_tvalid_i;
    assign t2c_tready_o = t2c_a4s_src_ready;
    assign t2c_a4s_src.t2c_tdata_i = t2c_tdata_i;
    assign t2c_a4s_src.t2c_tstrb_i = t2c_tstrb_i;
    assign t2c_a4s_src.t2c_tkeep_i = t2c_tkeep_i;
    assign t2c_a4s_src.t2c_tlast_i = t2c_tlast_i;
    assign t2c_a4s_src.t2c_tid_i   = t2c_tid_i  ;
    assign t2c_a4s_src.t2c_tdest_i = t2c_tdest_i;
    assign t2c_a4s_src.t2c_tuser_i = t2c_tuser_i;
    
    iommu_acd_bus_handler_regslice #(
    /*parameter  */ .PAYLD_WIDTH                ($bits(t2c_a4s_dest)        )  // = 8
    ) U_t2c_regslice(
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .valid_src                  (t2c_a4s_src_valid          ),
    /*output logic                                      */  .ready_src                  (t2c_a4s_src_ready          ),
    /*input  logic [PAYLD_WIDTH-1:0]                    */  .payload_src                (t2c_a4s_src                ),
    /*output logic                                      */  .valid_dst                  (t2c_a4s_dest_valid         ),
    /*input  logic                                      */  .ready_dst                  (t2c_a4s_dest_ready         ),
    /*output logic [PAYLD_WIDTH-1:0]                    */  .payload_dst                (t2c_a4s_dest               )
    );


    iommu_acd_t2c_if #(
    /*parameter  */ .CTIF_DATA_WIDTH            (64                         ), // = 64,
    /*parameter  */ .CTIF_ID_WIDTH              (CTIF_ID_WIDTH              ), //= 4,
    /*parameter  */ .CTIF_DEST_WIDTH            (CTIF_DEST_WIDTH            ), //= 4,
    /*parameter  */ .CTIF_USER_WIDTH            (1                          ), //= 1,
    /*parameter  */ .SPARE_PARAM                (0                          )  //= 0
    ) U_t2c(                                                                            
    /*input  logic                                      */  .clk                        (clk                        ),
    /*input  logic                                      */  .rstn                       (rstn                       ),
    /*input  logic                                      */  .t2c_tvalid_i               (t2c_a4s_dest_valid         ), //(t2c_tvalid_i       ),
    /*output logic                                      */  .t2c_tready_o               (t2c_a4s_dest_ready         ), //(t2c_tready_o       ),
    /*input  logic [CTIF_DATA_WIDTH-1:0]                */  .t2c_tdata_i                (t2c_a4s_dest.t2c_tdata_i   ), //(t2c_tdata_i        ),
    /*input  logic [CTIF_STRB_WIDTH-1:0]                */  .t2c_tstrb_i                (t2c_a4s_dest.t2c_tstrb_i   ), //(t2c_tstrb_i        ),
    /*input  logic [CTIF_STRB_WIDTH-1:0]                */  .t2c_tkeep_i                (t2c_a4s_dest.t2c_tkeep_i   ), //(t2c_tkeep_i        ),
    /*input  logic                                      */  .t2c_tlast_i                (t2c_a4s_dest.t2c_tlast_i   ), //(t2c_tlast_i        ),
    /*input  logic [CTIF_ID_WIDTH-1:0]                  */  .t2c_tid_i                  (t2c_a4s_dest.t2c_tid_i     ), //(t2c_tid_i          ),
    /*input  logic [CTIF_DEST_WIDTH-1:0]                */  .t2c_tdest_i                (t2c_a4s_dest.t2c_tdest_i   ), //(t2c_tdest_i        ),
    /*input  logic [CTIF_USER_WIDTH-1:0]                */  .t2c_tuser_i                (t2c_a4s_dest.t2c_tuser_i   ), //(t2c_tuser_i        ),
    /*output logic                                      */  .msg_pvalid_o               (msg_pvalid_o               ),
    /*input  logic                                      */  .msg_pready_i               (msg_pready_i               ),
    /*output logic [63:0]                               */  .msg_pwdata_o               (msg_pwdata_o               ),
    /*output logic                                      */  .msg_ivalid_o               (msg_ivalid_o               ),
    /*input  logic                                      */  .msg_iready_i               (msg_iready_i               ),
    /*output logic [63:0]                               */  .msg_iwdata_o               (msg_iwdata_o               ),
    /*output logic                                      */  .msg_fvalid_o               (msg_fvalid_o               ),
    /*input  logic                                      */  .msg_fready_i               (msg_fready_i               ),
    /*input  MSG_FAULT_ACK_TYPE                         */  .msg_fwdata_o               (msg_fwdata_o               ),
    /*output logic                                      */  .msg_cvalid_o               (msg_cvalid_o               ),
    /*input  logic                                      */  .msg_cready_i               (msg_cready_i               ),
    /*input  MSG_CFG_ACCESS_TYPE                        */  .msg_cwdata_o               (msg_cwdata_o               ),
    /*output logic                                      */  .msg_gvalid_o               (msg_gvalid_o               ),
    /*input  logic                                      */  .msg_gready_i               (msg_gready_i               ),
    /*output logic [63:0]                               */  .msg_gwdata_o               (msg_gwdata_o               ),
    /*input  logic                                      */  .tc_if_is_connected_i       (tc_if_is_connected         ),
    /*output logic                                      */  .msg_connect_ack_got_o      (msg_connect_ack_got        ),
    /*input  logic                                      */  .spare_in                   (1'b0                       ) 
    );
//}}}


endmodule
