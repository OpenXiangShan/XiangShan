///////////////////////////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_mst_intf
//      1. AXI MST interface
//      2. B and R sequence control
//      3. generate B/R resp for fault AW/AR, and corresponding sequence control
//      4. parametered insertion regslice for each channel
//
// use to connect outside AXI SLV 
//
// AW.VALID shoud aligned with corresponding W.VALID
// next AW never input before current W.LAST
// 
///////////////////////////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_mst_intf #( //{{{
    parameter MST_AW_REGSLICE                       = 1,
    parameter MST_W_REGSLICE                        = 1,
    parameter MST_AR_REGSLICE                       = 1,
    parameter MST_R_REGSLICE                        = 1,
    parameter MST_B_REGSLICE                        = 1,
    parameter   BUS_PROPERTY_BAR        = 0,
    parameter   BUS_INFLY_TOKEN_WIDTH   = 6,
    parameter   BUS_INFLY_TOKEN_NUM     = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_ADDR_WIDTH          = 64,
    parameter   BUS_DATA_WIDTH          = 128,
    parameter   BUS_SIZE_WIDTH          = 3,
    parameter   BUS_STRB_WIDTH          = BUS_DATA_WIDTH/8,
    parameter   BUS_ID_WIDTH            = 8,
    parameter   BUS_USER_WIDTH          = 8,
    parameter   BUS_LOOP_WIDTH          = 1,
    parameter type          BUS_CH_AX_TYPE          = iommu_acd_pkg::ch_ax_t,
    parameter type          BUS_CH_W_TYPE           = iommu_acd_pkg::ch_w_t,
    parameter type          BUS_CH_B_TYPE           = iommu_acd_pkg::ch_b_t,
    parameter type          BUS_CH_R_TYPE           = iommu_acd_pkg::ch_r_t,
    parameter   SPARE_PARAM             = 0
)(
//{{{ IO
    input  logic                                    clk             ,
    input  logic                                    rstn            ,
    // MST port
    output logic                                    mst_awvalid_o   ,
    input  logic                                    mst_awready_i   ,
    output BUS_CH_AX_TYPE                           mst_awpayld_o   ,
    output logic                                    mst_wvalid_o    ,
    input  logic                                    mst_wready_i    ,
    output BUS_CH_W_TYPE                            mst_wpayld_o    ,
    input  logic                                    mst_bvalid_i    ,
    output logic                                    mst_bready_o    ,
    input  BUS_CH_B_TYPE                            mst_bpayld_i    ,
    output logic                                    mst_arvalid_o   ,
    input  logic                                    mst_arready_i   ,
    output BUS_CH_AX_TYPE                           mst_arpayld_o   ,
    input  logic                                    mst_rvalid_i    ,
    output logic                                    mst_rready_o    ,
    input  BUS_CH_R_TYPE                            mst_rpayld_i    ,
    // W-BUF Intf
    input  logic                                    w2m_awvalid_i   ,
    output logic                                    w2m_awready_o   ,
    input  BUS_CH_AX_TYPE                           w2m_awpayld_i   ,
    input  logic                                    w2m_wvalid_i    ,
    output logic                                    w2m_wready_o    ,
    input  BUS_CH_W_TYPE                            w2m_wpayld_i    ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        w2m_token_i     ,
    input  logic                                    w2m_fault_i     ,
    input  logic                                    w2m_mrif_i      ,
    // B-BUF Intf
    output logic                                    m2b_bvalid_o    ,
    input  logic                                    m2b_bready_i    ,
    output BUS_CH_B_TYPE                            m2b_bpayld_o    ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        m2b_token_o     ,
    output logic                                    m2b_mrif_o      ,
    // R_BUF Intf
    input  logic                                    r2m_arvalid_i   ,
    output logic                                    r2m_arready_o   ,
    input  BUS_CH_AX_TYPE                           r2m_arpayld_i   ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        r2m_token_i     ,
    input  logic                                    r2m_fault_i     ,
    input  logic                                    r2m_mrif_i      ,
    output logic                                    m2r_rvalid_o    ,
    input  logic                                    m2r_rready_i    ,
    output BUS_CH_R_TYPE                            m2r_rpayld_o    ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        m2r_token_o     ,
    // QUEUE Intf
    input  logic                                    q2m_valid_i     ,
    output logic                                    q2m_ready_o     ,
    input  logic                                    q2m_wr_i        ,
    input  BUS_CH_AX_TYPE                           q2m_axpayld_i   ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        q2m_token_i     ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        q2m_pair_token_i,
    input  logic                                    q2m_fault_i     ,
    //
    input  logic                                    spare_in         
//}}}
);
//=== Declare === {{{
    //typedef struct packed {
    //    logic [BUS_USER_WIDTH-1:0]          user;
    //    logic [BUS_ID_WIDTH-1:0]            id;
    //    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token;
    //    logic                               fault;
    //} aw_token_fifo_t;
    //typedef struct packed {
    //    logic [7:0]                         len;
    //    logic [BUS_USER_WIDTH-1:0]          user;
    //    logic [BUS_ID_WIDTH-1:0]            id;
    //    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token;
    //    logic                               fault;
    //} ar_token_fifo_t;

    typedef struct packed {
        logic                               mrif;
        logic [BUS_LOOP_WIDTH-1:0]          axloop;
        logic                               idunq;
//        logic [BUS_USER_WIDTH-1:0]          user;
        logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token;
        logic                               fault;
    } aw_token_fifo_t;                                                                                  // aw_token_fifo wdata/rdata type
    typedef struct packed {
        logic                               mrif;
        logic [BUS_LOOP_WIDTH-1:0]          axloop;
        logic                               idunq;
        logic [7:0]                         len;
//        logic [BUS_USER_WIDTH-1:0]          user;
        logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token;
        logic                               fault;
    } ar_token_fifo_t;                                                                                  // ar_token_fifo wdata/rdata type

    aw_token_fifo_t aw_token_fifo_out,  aw_token_fifo_in,   aw_token_fifo_data_pop;                     // aw_token_fifo signals
    logic [BUS_ID_WIDTH-1:0]            aw_token_fifo_id_in,aw_token_fifo_id_out, aw_token_fifo_id_pop;
    logic           aw_token_fifo_push, aw_token_fifo_pop;
    logic           aw_token_fifo_full, aw_token_fifo_empty;
    logic           aw_token_fifo_out_fault;
    logic           aw_token_fifo_match_fault;                                                          // id_pop is from r_regslice but data_pop is fault and id_pop is different with current id_out
                                                                                                        // means that an older fault is stored before before current MST_R with the same ID

    ar_token_fifo_t ar_token_fifo_out,  ar_token_fifo_in,   ar_token_fifo_data_pop;                     // ar_token_fifo signals
    logic [BUS_ID_WIDTH-1:0]            ar_token_fifo_id_in,ar_token_fifo_id_out, ar_token_fifo_id_pop;
    logic           ar_token_fifo_push, ar_token_fifo_pop;
    logic           ar_token_fifo_full, ar_token_fifo_empty;
    logic           ar_token_fifo_out_fault;
    logic           ar_token_fifo_match_fault;                                                          // id_pop is from r_regslice but data_pop is fault and id_pop is different with current id_out
                                                                                                        // means that an older fault is stored before before current MST_R with the same ID

    BUS_CH_AX_TYPE  mst_aw_regslice_in,         mst_aw_regslice_out;
    logic           mst_aw_regslice_in_valid,   mst_aw_regslice_in_ready;
    logic           mst_aw_regslice_out_valid,  mst_aw_regslice_out_ready;

    BUS_CH_W_TYPE   mst_w_regslice_in,          mst_w_regslice_out;
    logic           mst_w_regslice_in_valid,    mst_w_regslice_in_ready;
    logic           mst_w_regslice_out_valid,   mst_w_regslice_out_ready;

    BUS_CH_B_TYPE   mst_b_regslice_in,          mst_b_regslice_out;
    logic           mst_b_regslice_in_valid,    mst_b_regslice_in_ready;
    logic           mst_b_regslice_out_valid,   mst_b_regslice_out_ready;

    BUS_CH_AX_TYPE  mst_ar_regslice_in,         mst_ar_regslice_out;
    logic           mst_ar_regslice_in_valid,   mst_ar_regslice_in_ready;
    logic           mst_ar_regslice_out_valid,  mst_ar_regslice_out_ready;

    BUS_CH_R_TYPE   mst_r_regslice_in,          mst_r_regslice_out;
    logic           mst_r_regslice_in_valid,    mst_r_regslice_in_ready;
    logic           mst_r_regslice_out_valid,   mst_r_regslice_out_ready;

    logic                                   fault_aw_valid, fault_aw_ready;

    logic                                   err_awvalid ;
    logic                                   err_awready ;
    BUS_CH_AX_TYPE                          err_awpayld ;
    logic                                   err_wvalid  ;
    logic                                   err_wready  ;
    BUS_CH_W_TYPE                           err_wpayld  ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       err_aw_token;
    logic                                   err_bvalid  ;
    logic                                   err_bready  ;
    BUS_CH_B_TYPE                           err_bpayld  ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       err_b_token ;
    logic                                   err_arvalid ;
    logic                                   err_arready ;
    BUS_CH_AX_TYPE                          err_arpayld ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       err_ar_token;
    logic                                   err_rvalid  ;
    logic                                   err_rready  ;
    BUS_CH_R_TYPE                           err_rpayld  ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       err_r_token ;

    logic [1:0]                             bar_out_valid, bar_out_ready;

    logic           ar_token_fifo_out_mrif, ar_token_fifo_match_mrif, err_ar_fault, err_ar_mrif;
//}}}

//=== MST AW REGSLCIE === {{{
generate
    if(MST_AW_REGSLICE) begin : aw_regslice_gen
        assign mst_aw_regslice_in_valid = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[1]) ? 1'b1 :
                                                                                  (w2m_awvalid_i & (~w2m_fault_i));
        assign mst_aw_regslice_in       = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[1]) ? q2m_axpayld_i :
                                                                                  w2m_awpayld_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(BUS_CH_AX_TYPE))
        ) U_mst_aw_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (mst_aw_regslice_in_valid),
            .ready_src   (mst_aw_regslice_in_ready),
            .payload_src (mst_aw_regslice_in),
            .ready_dst   (mst_aw_regslice_out_ready),
            .valid_dst   (mst_aw_regslice_out_valid),
            .payload_dst (mst_aw_regslice_out)
        );
    end
    else begin : aw_noregslice_gen
        assign mst_aw_regslice_out_valid= ((BUS_PROPERTY_BAR!=0) & bar_out_valid[1]) ? 1'b1 :
                                                                                  (w2m_awvalid_i & (~w2m_fault_i));
        assign mst_aw_regslice_in_ready = mst_aw_regslice_out_ready;
        assign mst_aw_regslice_out      = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[1]) ? q2m_axpayld_i :
                                                                                  w2m_awpayld_i;
    end

    assign mst_awvalid_o                = mst_aw_regslice_out_valid;
    assign mst_aw_regslice_out_ready    = mst_awready_i;
    assign mst_awpayld_o                = mst_aw_regslice_out;

endgenerate
//}}}

//=== MST W REGSLCIE === {{{
generate
    if(MST_W_REGSLICE) begin : w_regslice_gen
        assign mst_w_regslice_in_valid  = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[1]) ? 1'b0 :
                                                                                  (w2m_wvalid_i & (~w2m_fault_i));
        assign mst_w_regslice_in        = w2m_wpayld_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(BUS_CH_W_TYPE))
        ) U_mst_w_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (mst_w_regslice_in_valid),
            .ready_src   (mst_w_regslice_in_ready),
            .payload_src (mst_w_regslice_in),
            .ready_dst   (mst_w_regslice_out_ready),
            .valid_dst   (mst_w_regslice_out_valid),
            .payload_dst (mst_w_regslice_out)
        );
    end
    else begin : w_noregslice_gen
        assign mst_w_regslice_out_valid = w2m_wvalid_i & (~w2m_fault_i);
        assign mst_w_regslice_in_ready  = mst_w_regslice_out_ready;
        assign mst_w_regslice_out       = w2m_wpayld_i;
    end
endgenerate

    assign mst_wvalid_o                 = mst_w_regslice_out_valid;
    assign mst_w_regslice_out_ready     = mst_wready_i;
    assign mst_wpayld_o                 = mst_w_regslice_out;
//}}}

//=== MST B REGSLCIE === {{{
generate
    if(MST_B_REGSLICE) begin : b_regslice_gen
        assign mst_b_regslice_in_valid  = mst_bvalid_i;
        assign mst_bready_o             = mst_b_regslice_in_ready;
        assign mst_b_regslice_in        = mst_bpayld_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(BUS_CH_B_TYPE))
        ) U_mst_b_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (mst_b_regslice_in_valid),
            .ready_src   (mst_b_regslice_in_ready),
            .payload_src (mst_b_regslice_in),
            .ready_dst   (mst_b_regslice_out_ready),
            .valid_dst   (mst_b_regslice_out_valid),
            .payload_dst (mst_b_regslice_out)
        );
    end
    else begin : b_noregslice_gen
        assign mst_b_regslice_out_valid = mst_bvalid_i;
        assign mst_bready_o             = mst_b_regslice_out_ready;
        assign mst_b_regslice_out       = mst_bpayld_i;
    end
endgenerate
        assign mst_b_regslice_out_ready = (aw_token_fifo_out_fault | aw_token_fifo_match_fault)? 1'b0 : m2b_bready_i;
//}}}

//=== AW token fifo === {{{ store AW INFO:{MRIF, LOOP, IDUNQ, TOKEN, FAULT}&ID with the original sequence in the aw_token_fifo, pop with B, the B is seleced with FAULT info
    assign aw_token_fifo_push       = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[1]) ? bar_out_ready[1] :
                                      w2m_wvalid_i                          ? (w2m_wready_o & w2m_wpayld_i.wlast) : // bug_fix, 20241009, aw_token fifo push with W last
                                                                              1'b0;                                 //w2m_awvalid_i & w2m_awready_o;
    // push AW.                                                               {MRIF,        LOOP,                    IDUNQ,                  TOKEN,                                       FAULT} when w2m input handshake 
    assign aw_token_fifo_in         = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[1]) ? {1'b0,        q2m_axpayld_i.axloop,    q2m_axpayld_i.axidunq,  {q2m_wr_i ? q2m_token_i : q2m_pair_token_i}, 1'b0       } :
                                                                              {w2m_mrif_i,  w2m_awpayld_i.axloop,    w2m_awpayld_i.axidunq,  w2m_token_i,                                 w2m_fault_i} ;
    assign aw_token_fifo_id_in      = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[1]) ? q2m_axpayld_i.axid :
                                                                              w2m_awpayld_i.axid;                   // push coupled AW.ID

    assign aw_token_fifo_out_fault  = aw_token_fifo_out.fault;                                                      // indicates if the current oldest fifo output is fault

    assign aw_token_fifo_pop        = aw_token_fifo_out_fault   ? (err_bvalid & err_bready) :
                                      aw_token_fifo_match_fault ? (err_bvalid & err_bready) : (mst_b_regslice_out_valid & mst_b_regslice_out_ready);  // if the current oldest fifo out is fault, POP with slv_err R finish, otherwise POP with R regslice output handshake

    assign aw_token_fifo_id_pop     = aw_token_fifo_out_fault   ? err_bpayld.bid  :  mst_b_regslice_out.bid;        // when current oldest fifo out is fault, AW.ID is given to slv_err, after B finish, slv_err use the this as AW.ID to pop
                                                                                                                    //    otherwise, use the BID input to pop
    assign aw_token_fifo_match_fault= (aw_token_fifo_id_out != aw_token_fifo_id_pop) & (aw_token_fifo_data_pop.fault==1'b1) & mst_b_regslice_out_valid;
    // iommu_acd_bus_handler_sync_fifo #(
    // /*parameter  */     .WIDTH          ($bits(aw_token_fifo_t) ), //= 128,
    // /*parameter  */     .DEPTH          (BUS_INFLY_TOKEN_NUM    ), //= 32,
    // /*parameter  */     .SPARE_PARA     (1'b0                   )  //= 0
    // ) mst_aw_token_fifo(
    // /*input  logic              */  .clk            (clk                    ),
    // /*input  logic              */  .rstn           (rstn                   ),
    // /*input  logic              */  .push_i         (aw_token_fifo_push     ),
    // /*output logic              */  .full_o         (aw_token_fifo_full     ),
    // /*output logic              */  .afull_o        (                       ), // almost full
    // /*input  logic [WIDTH-1:0]  */  .wdata_i        (aw_token_fifo_in       ),
    // /*input  logic              */  .pop_i          (aw_token_fifo_pop      ),
    // /*output logic              */  .empty_o        (aw_token_fifo_empty    ),
    // /*output logic              */  .aempty_o       (                       ),
    // /*output logic [WIDTH-1:0]  */  .rdata_o        (aw_token_fifo_out      ),
    // /*input  logic              */  .spare_in       (1'b0                   ) 
    // );
    iommu_acd_bus_handler_id_queue #(
        /*parameter  */ .ID_WIDTH           (BUS_ID_WIDTH),
        /*parameter  */ .IDX_WIDTH          (BUS_INFLY_TOKEN_WIDTH), //= 3,
        /*parameter  */ .DATA_WIDTH         ($bits(aw_token_fifo_t)), //= 8,
        /*parameter  */ .SPARE_PARA         (0)  //= 0
    ) mst_aw_token_fifo(
        /*input  logic                            */    .clk     (clk                   ),
        /*input  logic                            */    .rstn    (rstn                  ),
        /*input  logic                            */    .push    (aw_token_fifo_push    ),
        /*input  logic [IDX_WIDTH-1:0]            */    .wid     (aw_token_fifo_id_in   ),
        /*input  logic [DATA_WIDTH-1:0]           */    .wdata   (aw_token_fifo_in      ),
        /*output logic                            */    .wfull   (aw_token_fifo_full    ),
        /*input  logic                            */    .pop     (aw_token_fifo_pop     ),
        /*input  logic [IDX_WIDTH-1:0]            */    .rid     (aw_token_fifo_id_pop  ),
        /*output logic [DATA_WIDTH-1:0]           */    .rdata   (aw_token_fifo_data_pop),
        /*output logic                            */    .rvalid  (                      ),
        /*output logic                            */    .rempty  (aw_token_fifo_empty   ),
        /*output logic [IDX_WIDTH-1:0]            */    .ido     (aw_token_fifo_id_out  ),
        /*output logic [DATA_WIDTH-1:0]           */    .datao   (aw_token_fifo_out     ),
        /*input  logic                            */    .spare_in(1'b0                  ) 
    );

    assign err_awvalid          = (aw_token_fifo_out_fault | aw_token_fifo_match_fault) & (~aw_token_fifo_empty);                                                                         // oldest AW is fault, call slv_err
    assign err_awpayld.axid     = aw_token_fifo_out_fault   ? aw_token_fifo_id_out :
                                  aw_token_fifo_match_fault ? aw_token_fifo_id_pop : 'd0;
    assign err_awpayld.axaddr   = 'd0;
    assign err_awpayld.axlen    = 'd0;
    assign err_awpayld.axsize   = 'd0;
    assign err_awpayld.axburst  = 'd0;
    assign err_awpayld.axlock   = 'd0;
    assign err_awpayld.axcache  = 'd0;
    assign err_awpayld.axprot   = 'd0;
    assign err_awpayld.axregion = 'd0;
    assign err_awpayld.axuser   = 'd0;
    assign err_awpayld.axqos    = 'd0;
    assign err_awpayld.axsnoop  = 'd0;
    assign err_awpayld.axdomain = 'd0;
    assign err_awpayld.axbar    = 'd0;
    assign err_awpayld.axidunq  = aw_token_fifo_out_fault   ? aw_token_fifo_out.idunq       :
                                  aw_token_fifo_match_fault ? aw_token_fifo_data_pop.idunq  : 'd0;
    assign err_awpayld.axloop   = aw_token_fifo_out_fault   ? aw_token_fifo_out.axloop        :
                                  aw_token_fifo_match_fault ? aw_token_fifo_data_pop.axloop   : 'd0;
    assign err_awpayld.axatop   = 'd0;

    assign err_wvalid           = (aw_token_fifo_out_fault | aw_token_fifo_match_fault) & (~aw_token_fifo_empty);
    assign err_wpayld.wdata     = 'd0;
    assign err_wpayld.wstrb     = 'd0;
    assign err_wpayld.wlast     = 'd1;
    assign err_wpayld.wuser     = 'd0;

    assign err_aw_token         = aw_token_fifo_out_fault   ? aw_token_fifo_out.token :
                                  aw_token_fifo_match_fault ? aw_token_fifo_data_pop.token : 'd0;
    assign err_bready           = aw_token_fifo_out_fault   ? m2b_bready_i :
                                  aw_token_fifo_match_fault ? m2b_bready_i : 1'b0;
//}}}

//=== MST AR REGSLCIE === {{{
generate
    if(MST_AR_REGSLICE) begin : ar_regslice_gen
        assign mst_ar_regslice_in_valid = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? 1'b1 :
                                                                                  (r2m_arvalid_i & (~r2m_fault_i) & (~r2m_mrif_i));
        assign mst_ar_regslice_in       = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? q2m_axpayld_i :
                                                                                  r2m_arpayld_i ;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(BUS_CH_AX_TYPE))
        ) U_mst_ar_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (mst_ar_regslice_in_valid),
            .ready_src   (mst_ar_regslice_in_ready),
            .payload_src (mst_ar_regslice_in),
            .ready_dst   (mst_ar_regslice_out_ready),
            .valid_dst   (mst_ar_regslice_out_valid),
            .payload_dst (mst_ar_regslice_out)
        );
    end
    else begin : ar_noregslice_gen
        assign mst_ar_regslice_out_valid= ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? 1'b1 :
                                                                                  (r2m_arvalid_i & (~r2m_fault_i) & (~r2m_mrif_i));
        assign mst_ar_regslice_in_ready = mst_ar_regslice_out_ready;
        assign mst_ar_regslice_out      = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? q2m_axpayld_i :
                                                                                  r2m_arpayld_i   ;
    end

    assign mst_arvalid_o            = mst_ar_regslice_out_valid;
    assign mst_ar_regslice_out_ready= mst_arready_i;
    assign mst_arpayld_o            = mst_ar_regslice_out;

endgenerate

//}}}

//=== MST R REGSLCIE === {{{
generate
    if(MST_R_REGSLICE) begin : r_regslice_gen
        assign mst_r_regslice_in_valid  = mst_rvalid_i;
        assign mst_rready_o             = mst_r_regslice_in_ready;
        assign mst_r_regslice_in        = mst_rpayld_i;

        iommu_acd_bus_handler_regslice #(
            .PAYLD_WIDTH     ($bits(BUS_CH_R_TYPE))
        ) U_mst_r_regslice(
            .clk         (clk),    // Clock
            .rstn        (rstn),  // Asynchronous reset active low
            .valid_src   (mst_r_regslice_in_valid),
            .ready_src   (mst_r_regslice_in_ready),
            .payload_src (mst_r_regslice_in),
            .ready_dst   (mst_r_regslice_out_ready),
            .valid_dst   (mst_r_regslice_out_valid),
            .payload_dst (mst_r_regslice_out)
        );
    end
    else begin : r_noregslice_gen
        assign mst_r_regslice_out_valid = mst_rvalid_i;
        assign mst_rready_o             = mst_r_regslice_out_ready;
        assign mst_r_regslice_out       = mst_rpayld_i;
    end
    assign mst_r_regslice_out_ready     = (ar_token_fifo_out_fault | ar_token_fifo_match_fault) ? 1'b0 : m2r_rready_i;
endgenerate
//}}}

//=== AR token fifo === {{{ store AR INFO:{LOOP, IDUNQ, LEN, USER, TOKEN, FAULT}&ID with the original sequence in the aw_token_fifo, pop with R, the R is seleced with FAULT info
    assign ar_token_fifo_push       = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? bar_out_ready[0] :
                                      r2m_arvalid_i                         ? r2m_arready_o :
                                                                              1'b0;
    // push AR.                                                               {MRIF,        LOOP,                    IDUNQ,                  LEN,                TOKEN,                                          FAULT} when r2m input handshake
    assign ar_token_fifo_in         = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? {1'b0,        q2m_axpayld_i.axloop,    q2m_axpayld_i.axidunq,  8'd0,                {q2m_wr_i ? q2m_pair_token_i : q2m_token_i},   1'b0} :
                                                                              {r2m_mrif_i,  r2m_arpayld_i.axloop,    r2m_arpayld_i.axidunq,  r2m_arpayld_i.axlen, r2m_token_i,                                   r2m_fault_i};
    assign ar_token_fifo_id_in      = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? q2m_axpayld_i.axid :
                                                                              r2m_arpayld_i.axid;   // push coupled AR.ID
    
    assign ar_token_fifo_out_fault  = ar_token_fifo_out.fault;                                      // indicates if the current oldest fifo output is fault
    assign ar_token_fifo_out_mrif   = ar_token_fifo_out.mrif;
    
    assign ar_token_fifo_pop        = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif)   ? (err_rvalid & err_rready & err_rpayld.rlast) :
                                      (ar_token_fifo_match_fault| ar_token_fifo_match_mrif) ? (err_rvalid & err_rready & err_rpayld.rlast) : (mst_r_regslice_out_valid & mst_r_regslice_out_ready & mst_r_regslice_out.rlast);
                                                                                                                                            // when current oldest fifo out is fault, AR.ID is given to slv_err, after R finish, slv_err use the this as RID to pop
    
    assign ar_token_fifo_id_pop     = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif)   ? err_rpayld.rid : mst_r_regslice_out.rid;      //     otherwise, use the RID input to pop
    
    assign ar_token_fifo_match_fault= (ar_token_fifo_id_out != ar_token_fifo_id_pop) & (ar_token_fifo_data_pop.fault==1'b1) & mst_r_regslice_out_valid;     //     only POP with RLAST
    assign ar_token_fifo_match_mrif = (ar_token_fifo_id_out != ar_token_fifo_id_pop) & (ar_token_fifo_data_pop.mrif ==1'b1) & mst_r_regslice_out_valid;
    // iommu_acd_bus_handler_sync_fifo #(
    // /*parameter  */ .WIDTH          ($bits(ar_token_fifo_t) ), //= 128,
    // /*parameter  */ .DEPTH          (BUS_INFLY_TOKEN_NUM    ), //= 32,
    // /*parameter  */ .SPARE_PARA     (1'b0                   )  //= 0
    // ) mst_ar_token_fifo(
    // /*input  logic                           */ .clk        (clk                    ),
    // /*input  logic                           */ .rstn       (rstn                   ),
    // /*input  logic                           */ .push_i     (ar_token_fifo_push     ),
    // /*output logic                           */ .full_o     (ar_token_fifo_full     ),
    // /*output logic                           */ .afull_o    (                       ), // almost full
    // /*input  logic [WIDTH-1:0]               */ .wdata_i    (ar_token_fifo_in       ),
    // /*input  logic                           */ .pop_i      (ar_token_fifo_pop      ),
    // /*output logic                           */ .empty_o    (ar_token_fifo_empty    ),
    // /*output logic                           */ .aempty_o   (                       ),
    // /*output logic [WIDTH-1:0]               */ .rdata_o    (ar_token_fifo_out      ),
    // /*input  logic                           */ .spare_in   (1'b0                   ) 
    // );
    iommu_acd_bus_handler_id_queue #(
        /*parameter  */ .ID_WIDTH           (BUS_ID_WIDTH           ),
        /*parameter  */ .IDX_WIDTH          (BUS_INFLY_TOKEN_WIDTH  ), //= 3,
        /*parameter  */ .DATA_WIDTH         ($bits(ar_token_fifo_t) ), //= 8,
        /*parameter  */ .SPARE_PARA         (0                      )  //= 0
    ) mst_ar_token_fifo(
        /*input  logic                            */    .clk     (clk                   ),
        /*input  logic                            */    .rstn    (rstn                  ),
        /*input  logic                            */    .push    (ar_token_fifo_push    ),
        /*input  logic [IDX_WIDTH-1:0]            */    .wid     (ar_token_fifo_id_in   ),
        /*input  logic [DATA_WIDTH-1:0]           */    .wdata   (ar_token_fifo_in      ),
        /*output logic                            */    .wfull   (ar_token_fifo_full    ),
        /*input  logic                            */    .pop     (ar_token_fifo_pop     ),
        /*input  logic [IDX_WIDTH-1:0]            */    .rid     (ar_token_fifo_id_pop  ),
        /*output logic [DATA_WIDTH-1:0]           */    .rdata   (ar_token_fifo_data_pop),
        /*output logic                            */    .rvalid  (                      ),
        /*output logic                            */    .rempty  (ar_token_fifo_empty   ),
        /*output logic [IDX_WIDTH-1:0]            */    .ido     (ar_token_fifo_id_out  ),
        /*output logic [DATA_WIDTH-1:0]           */    .datao   (ar_token_fifo_out     ),
        /*input  logic                            */    .spare_in(1'b0                  ) 
    );

    assign err_arvalid          = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif | ar_token_fifo_match_fault | ar_token_fifo_match_mrif) & (~ar_token_fifo_empty);     // oldest AR is fault, or mrif, call slv_err
    assign err_arpayld.axid     = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif)   ? ar_token_fifo_id_out          :
                                  (ar_token_fifo_match_fault| ar_token_fifo_match_mrif) ? ar_token_fifo_id_pop          : 'd0;
    assign err_arpayld.axaddr   = 'd0;
    assign err_arpayld.axlen    = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif)   ? ar_token_fifo_out.len         :
                                  (ar_token_fifo_match_fault| ar_token_fifo_match_mrif) ? ar_token_fifo_data_pop.len    : 'd0;
    assign err_arpayld.axsize   = 'd0;
    assign err_arpayld.axburst  = 'd0;
    assign err_arpayld.axlock   = 'd0;
    assign err_arpayld.axcache  = 'd0;
    assign err_arpayld.axprot   = 'd0;
    assign err_arpayld.axregion = 'd0;
    assign err_arpayld.axuser   = 'd0;
    assign err_arpayld.axqos    = 'd0;
    assign err_arpayld.axsnoop  = 'd0;
    assign err_arpayld.axdomain = 'd0;
    assign err_arpayld.axbar    = 'd0;
    assign err_arpayld.axidunq  = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif)   ? ar_token_fifo_out.idunq       :
                                  (ar_token_fifo_match_fault| ar_token_fifo_match_mrif) ? ar_token_fifo_data_pop.idunq  : 'd0;
    assign err_arpayld.axatop   = 'd0;
    assign err_arpayld.axloop   = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif)   ? ar_token_fifo_out.axloop        :
                                  (ar_token_fifo_match_fault| ar_token_fifo_match_mrif) ? ar_token_fifo_data_pop.axloop   : 'd0;

    assign err_ar_token         = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif)   ? ar_token_fifo_out.token       :
                                  (ar_token_fifo_match_fault| ar_token_fifo_match_mrif) ? ar_token_fifo_data_pop.token  : 'd0;
    assign err_rready           = (ar_token_fifo_out_fault  | ar_token_fifo_out_mrif)   ? m2r_rready_i                  :
                                  (ar_token_fifo_match_fault| ar_token_fifo_match_mrif) ? m2r_rready_i                  : 1'b0;
    assign err_ar_fault         = ar_token_fifo_out_fault   | ar_token_fifo_match_fault;
    assign err_ar_mrif          = ar_token_fifo_out_mrif    | ar_token_fifo_match_mrif;
//}}}

//=== slv err === {{{
    iommu_acd_bus_handler_slv_err  #(
    /*parameter  */ .BUS_INFLY_TOKEN_WIDTH  (BUS_INFLY_TOKEN_WIDTH  ), // = 6,
    /*parameter  */ .BUS_ADDR_WIDTH         (BUS_ADDR_WIDTH         ), // = 64,
    /*parameter  */ .BUS_DATA_WIDTH         (BUS_DATA_WIDTH         ), // = 128,
    /*parameter  */ .BUS_SIZE_WIDTH         (BUS_SIZE_WIDTH         ), // = 3,
    /*parameter  */ .BUS_STRB_WIDTH         (BUS_STRB_WIDTH         ), // = BUS_DATA_WIDTH/8,
    /*parameter  */ .BUS_ID_WIDTH           (BUS_ID_WIDTH           ), // = 8,
    /*parameter  */ .BUS_USER_WIDTH         (BUS_USER_WIDTH         ), // = 8,
    /*parameter type         */ .BUS_CH_AX_TYPE         (BUS_CH_AX_TYPE         ), // = iommu_acd_pkg::ch_ax_t,
    /*parameter type         */ .BUS_CH_W_TYPE          (BUS_CH_W_TYPE          ), // = iommu_acd_pkg::ch_w_t,
    /*parameter type         */ .BUS_CH_B_TYPE          (BUS_CH_B_TYPE          ), // = iommu_acd_pkg::ch_b_t,
    /*parameter type         */ .BUS_CH_R_TYPE          (BUS_CH_R_TYPE          ), // = iommu_acd_pkg::ch_r_t,
    /*parameter  */ .SPARE_PARAM            (0)  // = 0
    ) U_slv_err(
    /*input  logic                                  */  .clk                    (clk                    ),
    /*input  logic                                  */  .rstn                   (rstn                   ),
    /*input  logic                                  */  .slv_awvalid_i          (err_awvalid            ),   // AW valid input, should be align with corresponding W valid, guranteed by outside driver
    /*output logic                                  */  .slv_awready_o          (err_awready            ),
    /*input  BUS_CH_AX_TYPE                         */  .slv_awpayld_i          (err_awpayld            ),
    /*input  logic                                  */  .slv_wvalid_i           (err_wvalid             ),
    /*output logic                                  */  .slv_wready_o           (err_wready             ),
    /*input  BUS_CH_W_TYPE                          */  .slv_wpayld_i           (err_wpayld             ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]      */  .slv_aw_token_i         (err_aw_token           ),
    /*output logic                                  */  .slv_bvalid_o           (err_bvalid             ),
    /*input  logic                                  */  .slv_bready_i           (err_bready             ),
    /*output BUS_CH_B_TYPE                          */  .slv_bpayld_o           (err_bpayld             ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]      */  .slv_b_token_o          (err_b_token            ),
    /*input  logic                                  */  .slv_arvalid_i          (err_arvalid            ),
    /*output logic                                  */  .slv_arready_o          (err_arready            ),
    /*input  BUS_CH_AX_TYPE                         */  .slv_arpayld_i          (err_arpayld            ),
    /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]      */  .slv_ar_token_i         (err_ar_token           ),
    /*input  logic                                  */  .slv_ar_fault_i         (err_ar_fault           ),
    /*input  logic                                  */  .slv_ar_mrif_i          (err_ar_mrif            ),
    /*output logic                                  */  .slv_rvalid_o           (err_rvalid             ),
    /*input  logic                                  */  .slv_rready_i           (err_rready             ),
    /*output BUS_CH_R_TYPE                          */  .slv_rpayld_o           (err_rpayld             ),
    /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]      */  .slv_r_token_o          (err_r_token            ),
    /*input  logic                                  */  .spare_in               (1'b0                   )
    );
//}}}

//=== arb === {{{
//=== AW
    assign w2m_awready_o= ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? 1'b0 :
                          w2m_fault_i                           ? 1'b1 : mst_aw_regslice_in_ready; // if w2m input is fault, just push {USER, TOKEN, FAULT}&ID to aw_token_fifo, no need wait regslice READY

//=== W
    assign w2m_wready_o = ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? 1'b0 :
                          w2m_fault_i                           ? 1'b1 : mst_w_regslice_in_ready;
//=== B
    assign m2b_bvalid_o = (aw_token_fifo_out_fault | aw_token_fifo_match_fault) ? err_bvalid    : mst_b_regslice_out_valid;    // when current oldest AW.INFO is fault, select slv_err to B, otherwise select the B regslice as outside B
    assign m2b_bpayld_o = (aw_token_fifo_out_fault | aw_token_fifo_match_fault) ? err_bpayld    : mst_b_regslice_out;
    assign m2b_token_o  = (aw_token_fifo_out_fault | aw_token_fifo_match_fault) ? err_b_token   : aw_token_fifo_data_pop.token;//[BUS_INFLY_TOKEN_WIDTH+1-1:1];
    assign m2b_mrif_o   = (aw_token_fifo_out_fault | aw_token_fifo_match_fault) ? 1'b0          : aw_token_fifo_data_pop.mrif;
//=== AR
    assign r2m_arready_o= ((BUS_PROPERTY_BAR!=0) & bar_out_valid[0]) ? 1'b0 :
                          r2m_fault_i                           ? 1'b1 : mst_ar_regslice_in_ready;
//=== R
    assign m2r_rvalid_o = (ar_token_fifo_out_fault | ar_token_fifo_match_fault) ? err_rvalid    : mst_r_regslice_out_valid;    // when current oldest AR.INFO is fault, select slv_err to R, otherwise select the R regslcie as outside R
    assign m2r_rpayld_o = (ar_token_fifo_out_fault | ar_token_fifo_match_fault) ? err_rpayld    : mst_r_regslice_out;
    assign m2r_token_o  = (ar_token_fifo_out_fault | ar_token_fifo_match_fault) ? err_r_token   : ar_token_fifo_data_pop.token;//[BUS_INFLY_TOKEN_WIDTH+1-1:1];
//}}}

//=== bar ==={{{
generate
    if(BUS_PROPERTY_BAR) begin: bar_gen
        iommu_acd_mtlb_ready_mux #(
        /*parameter  */ .NUM    (2          ) // = 2
        ) U_bar_dmux(
        /*input  logic                  */  .clk        (clk            ),
        /*input  logic                  */  .rstn       (rstn           ),
        /*input  logic                  */  .valid_i    (q2m_valid_i    ),
        /*output logic                  */  .ready_o    (q2m_ready_o    ),
        /*output logic [NUM-1:0]        */  .valid_o    (bar_out_valid  ),
        /*input  logic [NUM-1:0]        */  .ready_i    (bar_out_ready  )
        );
        assign bar_out_ready[1] = ~bar_out_valid[1] ? 1'b0 : mst_aw_regslice_in_ready;
        assign bar_out_ready[0] = ~bar_out_valid[0] ? 1'b0 : mst_ar_regslice_in_ready;
    end
    else begin: nobar_gen
        assign q2m_ready_o  = 'd0;
        assign bar_out_valid= 'd0;
        assign bar_out_ready= 'd0;
    end
endgenerate
//}}}

endmodule
//}}}



