//////////////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_slv_intf
//      1. AXI SLV interface
//      2. parametered insertion regslice for each channel
//      3. AW/AR arbter for TRANS_QUEUE input, AR has higher priorit
//
// use to connect outside AXI MST
//
/////////////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_slv_intf import iommu_acd_pkg::*; #( //{{{
//{{{ PARAMETER
    parameter SLV_AW_REGSLICE                       = 1,
    parameter SLV_W_REGSLICE                        = 1,
    parameter SLV_AR_REGSLICE                       = 1,
    parameter SLV_R_REGSLICE                        = 1,
    parameter SLV_B_REGSLICE                        = 1,
    parameter BUS_PROPERTY_BAR                      = 0,
    parameter  TRANS_QUEUE_IDX_WIDTH    = 3,
    parameter  BUS_INFLY_TOKEN_WIDTH    = 6,
    parameter  BUS_INFLY_TOKEN_NUM      = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter  BUS_ADDR_WIDTH           = 64,
    parameter  BUS_DATA_WIDTH           = 128,
    parameter  BUS_SIZE_WIDTH           = 3,
    parameter  BUS_STRB_WIDTH           = BUS_DATA_WIDTH/8,
    parameter  BUS_ID_WIDTH             = 8,
    parameter  BUS_USER_WIDTH           = 8,
    parameter type         BUS_CH_AX_TYPE           = iommu_acd_pkg::ch_ax_t,
    parameter type         BUS_CH_W_TYPE            = iommu_acd_pkg::ch_w_t,
    parameter type         BUS_CH_B_TYPE            = iommu_acd_pkg::ch_b_t,
    parameter type         BUS_CH_R_TYPE            = iommu_acd_pkg::ch_r_t,
    parameter  SPARE_PARAM              = 0
//}}}
)(
//{{{ IO
    input logic                                     clk                         ,
    input logic                                     rstn                        ,
    // SLV Intf
    input  logic                                    slv_awvalid_i               ,
    output logic                                    slv_awready_o               ,
    input  BUS_CH_AX_TYPE                           slv_awpayld_i               ,
    input  logic [23:0]                             slv_aw_device_id_i          ,
    input  logic [19:0]                             slv_aw_process_id_i         ,
    input  logic                                    slv_aw_process_id_valid_i   ,
    input  logic                                    slv_aw_is_translated_i      ,
    input  logic                                    slv_wvalid_i                ,
    output logic                                    slv_wready_o                ,
    input  BUS_CH_W_TYPE                            slv_wpayld_i                ,
    output logic                                    slv_bvalid_o                ,
    input  logic                                    slv_bready_i                ,
    output BUS_CH_B_TYPE                            slv_bpayld_o                ,
    input  logic                                    slv_arvalid_i               ,
    output logic                                    slv_arready_o               ,
    input  BUS_CH_AX_TYPE                           slv_arpayld_i               ,
    input  logic [23:0]                             slv_ar_device_id_i          ,
    input  logic [19:0]                             slv_ar_process_id_i         ,
    input  logic                                    slv_ar_process_id_valid_i   ,
    input  logic                                    slv_ar_is_translated_i      ,
    output logic                                    slv_rvalid_o                ,
    input  logic                                    slv_rready_i                ,
    output BUS_CH_R_TYPE                            slv_rpayld_o                ,
    // MST Intf
    output logic                                    s2w_wvalid_o                ,
    input  logic                                    s2w_wready_i                ,
    output BUS_CH_W_TYPE                            s2w_wpayld_o                ,
    input  logic                                    b2s_bvalid_i                ,
    output logic                                    b2s_bready_o                ,
    input  BUS_CH_B_TYPE                            b2s_bpayld_i                ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        b2s_token_i                 ,
    input  logic                                    r2s_rvalid_i                ,
    output logic                                    r2s_rready_o                ,
    input  BUS_CH_R_TYPE                            r2s_rpayld_i                ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        r2s_token_i                 ,
    // SLV_IF to TRANS_QUEUE
    output logic                                    s2q_valid_o                 ,
    input  logic                                    s2q_ready_i                 ,
    output logic                                    s2q_bc_fail_o               ,
    output logic                                    s2q_wr_o                    ,
    output logic [23:0]                             s2q_device_id_o             ,
    output logic [19:0]                             s2q_process_id_o            ,
    output logic                                    s2q_process_id_valid_o      ,
    output logic                                    s2q_is_translated_o         ,
    output BUS_CH_AX_TYPE                           s2q_axpayld_o               ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        s2q_token_o                 ,   // token output to TRANS_QUEUE, each ongoing transaction has a dedicated token
    input  logic                                    s2q_w_can_enqueue_hint_i    ,   // indicates that AW can output even AR exists, as TRANS_QUEUE may full for R but not for W
    //
    output logic [BUS_INFLY_TOKEN_NUM-1:0]          bh2inv_outstanding_list_o   ,   // indicates the outstanding transaction, each bit correspond to a token
    output logic [BUS_INFLY_TOKEN_NUM-1:0]          bh2inv_outstanding_rw_list_o,   // indicates the outstanding transaction type 0:READ, 1:WRITE

    input  logic                                    spare_in                     
//}}}
);
//=== Declare === {{{
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token, nxt_token;                       // transaction token
    //logic [BUS_INFLY_TOKEN_WIDTH-1:0] token_num;
    logic [BUS_INFLY_TOKEN_NUM-1:0]     token_pool, nxt_token_pool;             // token occpupied flag, each bit map to a token
    logic [BUS_INFLY_TOKEN_NUM-1:0]     token_rw_pool, nxt_token_rw_pool;       // token occpupide R/W flag, 0:READ 1:WRITE
    logic                               token_pool_empty;

    typedef struct packed {
        BUS_CH_AX_TYPE                  axpayld;
        logic [23:0]                    device_id;
        logic [19:0]                    process_id;
        logic                           process_id_valid;
        logic                           is_translated;
    } ch_ax_t;
    typedef struct packed {
        BUS_CH_W_TYPE                   wpayld;
    } ch_w_t;
    typedef struct packed {
        BUS_CH_B_TYPE                   bpayld;
    } ch_b_t;
    typedef struct packed {
        BUS_CH_R_TYPE                   rpayld;
    } ch_r_t;

    logic [1:0]                         out_arb_valid;
    logic [1:0]                         out_arb_gnt, out_arb_mask;
    logic                               mst_arvalid, mst_awvalid;
    
    typedef struct packed {
        ch_ax_t                         axi_info;
        logic                           wr;
    } arb_t;
    arb_t [1:0]                         out_arb_data_i;
    arb_t                               out_arb_data_o;

    logic [1:0]                         arb_prior;
    logic [1:0]                         is_bar;

//}}}

//=== Main Code === {{{
//SLV AW REGSLCIE {{{
    ch_ax_t slv_aw_regslice_in, slv_aw_regslice_out;
    logic   slv_aw_regslice_in_valid, slv_aw_regslice_in_ready;
    logic   slv_aw_regslice_out_valid, slv_aw_regslice_out_ready;

generate
    if(SLV_AW_REGSLICE) begin : aw_regslice_gen
        assign slv_aw_regslice_in_valid             = slv_awvalid_i;
        assign slv_awready_o                        = slv_aw_regslice_in_ready;
        assign slv_aw_regslice_in.axpayld           = slv_awpayld_i;
        assign slv_aw_regslice_in.device_id         = slv_aw_device_id_i;
        assign slv_aw_regslice_in.process_id        = slv_aw_process_id_i;
        assign slv_aw_regslice_in.process_id_valid  = slv_aw_process_id_valid_i;
        assign slv_aw_regslice_in.is_translated     = slv_aw_is_translated_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(ch_ax_t))
        ) U_slv_aw_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (slv_aw_regslice_in_valid),
            .ready_src   (slv_aw_regslice_in_ready),
            .payload_src (slv_aw_regslice_in),
            .ready_dst   (slv_aw_regslice_out_ready),
            .valid_dst   (slv_aw_regslice_out_valid),
            .payload_dst (slv_aw_regslice_out)
        );
    end
    else begin : aw_noregslice_gen
        assign slv_aw_regslice_out_valid    = slv_awvalid_i;
        assign slv_awready_o                = slv_aw_regslice_out_ready;
        assign slv_aw_regslice_out.axpayld           = slv_awpayld_i;
        assign slv_aw_regslice_out.device_id         = slv_aw_device_id_i;
        assign slv_aw_regslice_out.process_id        = slv_aw_process_id_i;
        assign slv_aw_regslice_out.process_id_valid  = slv_aw_process_id_valid_i;
        assign slv_aw_regslice_out.is_translated     = slv_aw_is_translated_i;
    end
endgenerate

    //assign mst_awid_o     = slv_aw_regslice_out.awid    ;
    //assign mst_awaddr_o   = slv_aw_regslice_out.awaddr  ;
    //assign mst_awlen_o    = slv_aw_regslice_out.awlen   ;
    //assign mst_awsize_o   = slv_aw_regslice_out.awsize  ;
    //assign mst_awburst_o  = slv_aw_regslice_out.awburst ;
    //assign mst_awlock_o   = slv_aw_regslice_out.awlock  ;
    //assign mst_awcache_o  = slv_aw_regslice_out.awcache ;
    //assign mst_awprot_o   = slv_aw_regslice_out.awprot  ;
    //assign mst_awregion_o = slv_aw_regslice_out.awregion;
    //assign mst_awuser_o   = slv_aw_regslice_out.awuser  ;
    //assign mst_awqos_o    = slv_aw_regslice_out.awqos   ;
    //assign mst_awvalid_o  = out_arb_mask[1];
    //assign mst_awvalid    = slv_aw_regslice_out_valid ;
    assign slv_aw_regslice_out_ready = out_arb_gnt[1];//mst_awready_i  ;

//}}}

//SLV W REGSLCIE {{{
    ch_w_t slv_w_regslice_in, slv_w_regslice_out;
    logic   slv_w_regslice_in_valid, slv_w_regslice_in_ready;
    logic   slv_w_regslice_out_valid, slv_w_regslice_out_ready;

generate
    if(SLV_W_REGSLICE) begin : w_regslice_gen
        assign slv_w_regslice_in_valid  = slv_wvalid_i;
        assign slv_wready_o             = slv_w_regslice_in_ready;
        assign slv_w_regslice_in.wpayld = slv_wpayld_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(ch_w_t))
        ) U_slv_w_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (slv_w_regslice_in_valid),
            .ready_src   (slv_w_regslice_in_ready),
            .payload_src (slv_w_regslice_in),
            .ready_dst   (slv_w_regslice_out_ready),
            .valid_dst   (slv_w_regslice_out_valid),
            .payload_dst (slv_w_regslice_out)
        );
    end
    else begin : w_noregslice_gen
        assign slv_w_regslice_out_valid = slv_wvalid_i;
        assign slv_wready_o             = slv_w_regslice_out_ready;
        assign slv_w_regslice_out.wpayld= slv_wpayld_i;
    end
endgenerate

    assign s2w_wvalid_o             = slv_w_regslice_out_valid;
    assign s2w_wpayld_o             = slv_w_regslice_out.wpayld;
    assign slv_w_regslice_out_ready = s2w_wready_i  ;

//}}}

//SLV B REGSLCIE {{{
    ch_b_t  slv_b_regslice_in, slv_b_regslice_out;
    logic   slv_b_regslice_in_valid, slv_b_regslice_in_ready;
    logic   slv_b_regslice_out_valid, slv_b_regslice_out_ready;

generate
    if(SLV_B_REGSLICE) begin : b_regslice_gen
        assign slv_b_regslice_in_valid = b2s_bvalid_i;
        assign b2s_bready_o            = slv_b_regslice_in_ready;
        assign slv_b_regslice_in.bpayld= b2s_bpayld_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(ch_b_t))
        ) U_slv_b_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (slv_b_regslice_in_valid),
            .ready_src   (slv_b_regslice_in_ready),
            .payload_src (slv_b_regslice_in),
            .ready_dst   (slv_b_regslice_out_ready),
            .valid_dst   (slv_b_regslice_out_valid),
            .payload_dst (slv_b_regslice_out)
        );
    end
    else begin : b_noregslice_gen
        assign slv_b_regslice_out_valid = b2s_bvalid_i;
        assign b2s_bready_o             = slv_b_regslice_out_ready;
        assign slv_b_regslice_out.bpayld= b2s_bpayld_i;
    end
endgenerate

    assign slv_bpayld_o             = slv_b_regslice_out.bpayld;
    assign slv_bvalid_o             = slv_b_regslice_out_valid;
    assign slv_b_regslice_out_ready = slv_bready_i  ;

//}}}

//SLV AR REGSLCIE {{{
    ch_ax_t slv_ar_regslice_in, slv_ar_regslice_out;
    logic   slv_ar_regslice_in_valid, slv_ar_regslice_in_ready;
    logic   slv_ar_regslice_out_valid, slv_ar_regslice_out_ready;

generate
    if(SLV_AR_REGSLICE) begin : ar_regslice_gen
        assign slv_ar_regslice_in_valid             = slv_arvalid_i;
        assign slv_arready_o                        = slv_ar_regslice_in_ready;
        assign slv_ar_regslice_in.axpayld           = slv_arpayld_i;
        assign slv_ar_regslice_in.device_id         = slv_ar_device_id_i;
        assign slv_ar_regslice_in.process_id        = slv_ar_process_id_i;
        assign slv_ar_regslice_in.process_id_valid  = slv_ar_process_id_valid_i;
        assign slv_ar_regslice_in.is_translated     = slv_ar_is_translated_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(ch_ax_t))
        ) U_slv_ar_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (slv_ar_regslice_in_valid),
            .ready_src   (slv_ar_regslice_in_ready),
            .payload_src (slv_ar_regslice_in),
            .ready_dst   (slv_ar_regslice_out_ready),
            .valid_dst   (slv_ar_regslice_out_valid),
            .payload_dst (slv_ar_regslice_out)
        );
    end
    else begin : ar_noregslice_gen
        assign slv_ar_regslice_out_valid            = slv_arvalid_i;
        assign slv_arready_o                        = slv_ar_regslice_out_ready;
        assign slv_ar_regslice_out.axpayld          = slv_arpayld_i;
        assign slv_ar_regslice_out.device_id        = slv_ar_device_id_i;
        assign slv_ar_regslice_out.process_id       = slv_ar_process_id_i;
        assign slv_ar_regslice_out.process_id_valid = slv_ar_process_id_valid_i;
        assign slv_ar_regslice_out.is_translated    = slv_ar_is_translated_i;
    end
endgenerate

    //assign mst_arid_o     = slv_ar_regslice_out.arid    ;
    //assign mst_araddr_o   = slv_ar_regslice_out.araddr  ;
    //assign mst_arlen_o    = slv_ar_regslice_out.arlen   ;
    //assign mst_arsize_o   = slv_ar_regslice_out.arsize  ;
    //assign mst_arburst_o  = slv_ar_regslice_out.arburst ;
    //assign mst_arlock_o   = slv_ar_regslice_out.arlock  ;
    //assign mst_arcache_o  = slv_ar_regslice_out.arcache ;
    //assign mst_arprot_o   = slv_ar_regslice_out.arprot  ;
    //assign mst_arregion_o = slv_ar_regslice_out.arregion;
    //assign mst_aruser_o   = slv_ar_regslice_out.aruser  ;
    //assign mst_arqos_o    = slv_ar_regslice_out.arqos   ;

    //assign mst_arvalid_o  = out_arb_mask[0] ;
    //assign mst_arvalid    = slv_ar_regslice_out_valid ;
    assign slv_ar_regslice_out_ready = out_arb_gnt[0];//mst_arready_i  ;

//}}}

//SLV R REGSLCIE {{{
    ch_r_t  slv_r_regslice_in,        slv_r_regslice_out;
    logic   slv_r_regslice_in_valid,  slv_r_regslice_in_ready;
    logic   slv_r_regslice_out_valid, slv_r_regslice_out_ready;

generate
    if(SLV_R_REGSLICE) begin : r_regslice_gen
        assign slv_r_regslice_in_valid  = r2s_rvalid_i;
        assign r2s_rready_o             = slv_r_regslice_in_ready;
        assign slv_r_regslice_in.rpayld = r2s_rpayld_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(ch_r_t))
        ) U_slv_r_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (slv_r_regslice_in_valid),
            .ready_src   (slv_r_regslice_in_ready),
            .payload_src (slv_r_regslice_in),
            .ready_dst   (slv_r_regslice_out_ready),
            .valid_dst   (slv_r_regslice_out_valid),
            .payload_dst (slv_r_regslice_out)
        );
    end
    else begin : r_noregslice_gen
        assign slv_r_regslice_out.rpayld= r2s_rpayld_i;
        assign slv_r_regslice_out_valid = r2s_rvalid_i;
        assign r2s_rready_o             = slv_r_regslice_out_ready;
    end
endgenerate
    assign slv_rpayld_o             = slv_r_regslice_out.rpayld;
    assign slv_rvalid_o             = slv_r_regslice_out_valid;
    assign slv_r_regslice_out_ready = slv_rready_i;

//}}}

//ARB {{{
    assign out_arb_data_i[0] = {slv_ar_regslice_out, 1'b0};
    assign out_arb_data_i[1] = {slv_aw_regslice_out, 1'b1};
    assign is_bar[0] = BUS_PROPERTY_BAR ? (slv_aw_regslice_out_valid & out_arb_data_i[1].axi_info.axpayld.axbar[0]) : 1'b0;
    assign is_bar[1] = BUS_PROPERTY_BAR ? (slv_ar_regslice_out_valid & out_arb_data_i[0].axi_info.axpayld.axbar[0]) : 1'b0;
    assign arb_prior[1] = is_bar[0] ? 1'b0:
                          is_bar[1] ? 1'b1:
                          s2q_w_can_enqueue_hint_i;
    assign arb_prior[0] = 1'b0;

    iommu_acd_bus_handler_trans_arb #(
        .ARB_TYPE             (0),
        .REQ_NUM              (2),
        .DATA_TYPE            (arb_t),
        .AXIVLDRDY            (1)
    ) U_output_arb(
        /*input  logic                    */ .clk         (clk                                                              ),
        /*input  logic                    */ .rstn        (rstn                                                             ),
        /*input  logic [REQ_NUM-1:0]      */ .req_i       ({slv_aw_regslice_out_valid, slv_ar_regslice_out_valid}           ),  // R use req[0] channel, meas has hither priority than W
//        /*input  logic [REQ_NUM-1:0]      */ .req_prior_i ({s2q_w_can_enqueue_hint_i, 1'b0}                                 ),  // if w_hint (means still empty trans_queue entry for W) and no ar transaction, drive aw prior so that W can be granted
        /*input  logic [REQ_NUM-1:0]      */ .req_prior_i (arb_prior                                                        ),  // if w_hint (means still empty trans_queue entry for W) and no ar transaction, drive aw prior so that W can be granted
        /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i      ({out_arb_data_i[1], out_arb_data_i[0]}                           ),
        /*output logic [REQ_NUM-1:0]      */ .gnt_o       (out_arb_gnt                                                      ),
        /*output logic                    */ .req_o       (s2q_valid_o                                                      ),
        /*output DATA_TYPE                */ .data_o      (out_arb_data_o                                                   ),
        /*input  logic                    */ .gnt_i       (s2q_ready_i & (~token_pool_empty)                                )   // if all token is used with ongoing transaction, reverse pressure to input AW&AR
    );

    iommu_acd_bus_handler_4k_boundary_check #(                                                                                  // check if the transaction target address cross 4K boundary, drive fault if so
        .BUS_ADDR_WIDTH        (BUS_ADDR_WIDTH),
        .BUS_SIZE_WIDTH        (BUS_SIZE_WIDTH)
    ) U_bc(
        /*input  logic                      */  .valid_i    (s2q_valid_o            ),
        /*input  logic [BUS_ADDR_WIDTH-1:0] */  .addr_i     (s2q_axpayld_o.axaddr   ),
        /*input  logic [1:0]                */  .burst_i    (s2q_axpayld_o.axburst  ),
        /*input  logic [7:0]                */  .length_i   (s2q_axpayld_o.axlen    ),
        /*input  logic [BUS_SIZE_WIDTH-1:0] */  .size_i     (s2q_axpayld_o.axsize   ),
        /*output logic                      */  .violation_o(s2q_bc_fail_o          )
    ); 
    
    assign out_arb_mask             = out_arb_gnt                               ;
    //assign s2q_wr_o                 = ~out_arb_mask[0]                  ;
    assign s2q_wr_o                 = out_arb_data_o.wr                         ;
    assign s2q_device_id_o          = out_arb_data_o.axi_info.device_id         ;
    assign s2q_process_id_o         = out_arb_data_o.axi_info.process_id        ;
    assign s2q_process_id_valid_o   = out_arb_data_o.axi_info.process_id_valid  ;
    assign s2q_is_translated_o      = out_arb_data_o.axi_info.is_translated     ;
    assign s2q_axpayld_o            = out_arb_data_o.axi_info.axpayld           ;
//    always@(posedge clk or negedge rstn) begin                                                                        // update token value for next transaction
//        if(~rstn) begin
//            token_num <= {BUS_INFLY_TOKEN_WIDTH{1'd1}};
//            token     <= {BUS_INFLY_TOKEN_WIDTH{1'd1}};
//        end
//        else begin
//            case({(s2q_valid_o & s2q_ready_i), (slv_rvalid_o & slv_rready_i), (slv_bvalid_o & slv_bready_i)})         // token_num updated, when output to trans_queue, decrease. when B or R resp output to MST outsied, increae
//            3'b100 : token_num <= token_num - 'd1;
//            3'b110 : token_num <= token_num;
//            3'b101 : token_num <= token_num;
//            3'b111 : token_num <= token_num + 'd1;
//            3'b001 : token_num <= token_num + 'd1;
//            3'b010 : token_num <= token_num + 'd1;
//            3'b011 : token_num <= token_num + 'd2;
//            default: token_num <= token_num;
//            endcase
//
//            if(s2q_valid_o & s2q_ready_i) begin                                                                       // token value is always updated circlically, no duplicated token used at the same time, controlled by token_num and xready
//                token <= token - 'd1;
//            end
//        end
//    end
    always@(*) begin
        nxt_token       = token;
        for(int unsigned kk=0; kk<BUS_INFLY_TOKEN_NUM; kk++) begin
            if(token_pool[kk] == 1'b0) begin
                nxt_token = BUS_INFLY_TOKEN_WIDTH'(kk);
            end
        end
    end
    always@(*) begin
        for(int unsigned km=0; km<BUS_INFLY_TOKEN_NUM; km++) begin
            if((token_pool[km]==1'b0) & s2q_valid_o & s2q_ready_i & (nxt_token==BUS_INFLY_TOKEN_WIDTH'(km)))
                nxt_token_pool[km] = 1'b1;
            else if((token_pool[km]==1'b1) & r2s_rvalid_i & r2s_rready_o & r2s_rpayld_i.rlast & (r2s_token_i==BUS_INFLY_TOKEN_WIDTH'(km)))
                nxt_token_pool[km] = 1'b0;
            else if((token_pool[km]==1'b1) & b2s_bvalid_i & b2s_bready_o & (b2s_token_i==BUS_INFLY_TOKEN_WIDTH'(km)))
                nxt_token_pool[km] = 1'b0;
            else
                nxt_token_pool[km] = token_pool[km];
        end
    end
    always@(*) begin
        for(int unsigned km=0; km<BUS_INFLY_TOKEN_NUM; km++) begin
            if((token_pool[km]==1'b0) & s2q_valid_o & s2q_ready_i & (nxt_token==BUS_INFLY_TOKEN_WIDTH'(km)))
                nxt_token_rw_pool[km] = s2q_wr_o ? 1'b1 : 1'b0;
            else
                nxt_token_rw_pool[km] = token_rw_pool[km];
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            token           <= {BUS_INFLY_TOKEN_WIDTH{1'b1}};
            token_pool      <= 'd0;
            token_rw_pool   <= 'd0;
        end
        else begin
            token           <= nxt_token;
            token_pool      <= nxt_token_pool;
            token_rw_pool   <= nxt_token_rw_pool;
        end
    end
    assign token_pool_empty         = &(token_pool);
    assign s2q_token_o              = nxt_token;
//}}}

// FLAG to INV {{{
    assign bh2inv_outstanding_list_o    = token_pool;
    assign bh2inv_outstanding_rw_list_o = token_rw_pool;
//}}}

//}}}

endmodule
//}}}


