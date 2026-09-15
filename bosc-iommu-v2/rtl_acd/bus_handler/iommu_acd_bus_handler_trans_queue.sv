//////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_trans_queue
//      1. transaction enqueue and transaction-info(AW/AR) store
//      2. translation req genereration and ack processs
//      3. dequeue and AXI sequence control
//
// use to as sink for slv_intf and master of ptw
//
/////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_trans_queue #( //{{{
    parameter   BUS_PROPERTY_BAR        = 0,
    parameter   TRANS_QUEUE_IDX_WIDTH   = 3,
    parameter   TRANS_QUEUE_DEPTH       = 2**TRANS_QUEUE_IDX_WIDTH,
    parameter   BUS_INFLY_TOKEN_WIDTH   = 6,
    parameter   BUS_INFLY_TOKEN_NUM     = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_ADDR_WIDTH          = 64,
    parameter   BUS_DATA_WIDTH          = 128,
    parameter   BUS_SIZE_WIDTH          = 3,
    parameter   BUS_STRB_WIDTH          = BUS_DATA_WIDTH/8,
    parameter   BUS_ID_WIDTH            = 8,
    parameter   BUS_USER_WIDTH          = 8,
    parameter type          BUS_CH_AX_TYPE          = iommu_acd_pkg::ch_ax_t,
    parameter type          TRANSLATE_REQ_TYPE      = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE      = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    parameter   SPARE_PARA              = 0
)(
//{{{ IO
    input  logic                                    clk,
    input  logic                                    rstn,
    // TRANSACTION INPUT (AW & W) from SLV_IF
    input  logic                                    s2q_valid_i                 ,
    output logic                                    s2q_ready_o                 ,
    input  logic                                    s2q_bc_fail_i               ,
    input  logic                                    s2q_wr_i                    ,
    input  logic [23:0]                             s2q_device_id_i             ,
    input  logic                                    s2q_process_id_valid_i      ,
    input  logic [19:0]                             s2q_process_id_i            ,
    input  logic                                    s2q_is_translated_i         ,
    input  BUS_CH_AX_TYPE                           s2q_axpayld_i               ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        s2q_token_i                 ,
    // TRANSLATE INTF
    output logic                                    ptw_req_valid_o             ,
    input  logic                                    ptw_req_ready_i             ,
    output TRANSLATE_REQ_TYPE                       ptw_req_o                   ,
    input  logic                                    ptw_ack_valid_i             ,
    input  TRANSLATE_ACK_TYPE                       ptw_ack_i                   ,
    output logic                                    mrif_credit_grant_valid_o   ,
    // W_BUF
    input  logic [BUS_INFLY_TOKEN_NUM-1:0]          wdata_ready_i               ,  // valid WDATA got stored
    output logic [TRANS_QUEUE_IDX_WIDTH-1:0]        enqueue_qidx_o              ,
    input  logic                                    wbuf_dequeue_fifo_empty_i   ,
    output logic                                    s2q_w_can_enqueue_hint_o    ,
    //
    output logic                                    qout_valid_o                ,
    input  logic                                    qout_ready_i                ,
    output logic                                    qout_wr_o                   ,
    output BUS_CH_AX_TYPE                           qout_axpayld_o              ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        qout_token_o                ,
    output logic                                    qout_fault_o                ,
    output logic                                    qout_mrif_o                 ,
    input  logic                                    mrif_done_i                 ,
    output logic                                    mrif_valid_o                ,
    output logic [55:12]                            mrif_nppn_o                 ,
    output logic [10:0]                             mrif_nid_o                  ,
    output logic [BUS_ID_WIDTH-1:0]                 mrif_axid_o                 ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        mrif_token_o                ,
    //
    output logic                                    q2m_valid_o                 ,
    input  logic                                    q2m_ready_i                 ,
    output logic                                    q2m_wr_o                    ,
    output BUS_CH_AX_TYPE                           q2m_axpayld_o               ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        q2m_token_o                 ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        q2m_pair_token_o            ,
    output logic                                    q2m_fault_o                 ,
    
    input  logic                                    spare_in                     
//}}}
);
//=== Declare === {{{
    typedef logic [TRANS_QUEUE_IDX_WIDTH-1:0] entry_idx_t;
    localparam WR_ONLY_DEPTH = TRANS_QUEUE_DEPTH / 3;
    localparam RD_ONLY_DEPTH = WR_ONLY_DEPTH;
    localparam SHARE_DEPTH   = TRANS_QUEUE_DEPTH - WR_ONLY_DEPTH - RD_ONLY_DEPTH;
    
    typedef struct packed {
    logic                               mrif    ;
    logic                               fault   ;
    logic                               wr      ;
    BUS_CH_AX_TYPE                      axpayld ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token   ;
    } tqe_info_t;                                       // trans-queue-entry info type              // transaction info to store into queue_entry

    logic [TRANS_QUEUE_DEPTH-1:0]       entry_update_i;//                    [0:TRANS_QUEUE_DEPTH-1];   // entry update
    logic                               entry_trans_valid_i             [0:TRANS_QUEUE_DEPTH-1];      // input transaction enqueue valid to entry
    logic [23:0]                        entry_trans_device_id_i         [0:TRANS_QUEUE_DEPTH-1];      // input transaction enqueue device_id to entry
    logic                               entry_trans_process_id_valid_i  [0:TRANS_QUEUE_DEPTH-1];      // input transaction enqueue process_id_valid to entry
    logic [19:0]                        entry_trans_process_id_i        [0:TRANS_QUEUE_DEPTH-1];      // input transaction enqueue process_id to entry
    logic                               entry_trans_is_translated_i     [0:TRANS_QUEUE_DEPTH-1];      // input transaction enqueue is translated flag to entry
    tqe_info_t                          entry_trans_info_i              [0:TRANS_QUEUE_DEPTH-1];      // input transaction enqueue AXI Info to entry
    logic                               entry_trans_bc_fail_i           [0:TRANS_QUEUE_DEPTH-1];      // input transaction enqueue 4K bounndary check fail flag to entry
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_depend_bits_i             [0:TRANS_QUEUE_DEPTH-1];      // AXI sequence dependency flag arrary to entry
    logic                               entry_depend_bit_o              [0:TRANS_QUEUE_DEPTH-1];      // AXI sequence dependency flag to other entry
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_valid_i                   [0:TRANS_QUEUE_DEPTH-1];      // Other entry valid flag
    logic                               entry_wdata_ready_i             [0:TRANS_QUEUE_DEPTH-1];      // AXI W.DATA ready flag to entry
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_valid_o                   ;                             // entry valid falg to other entry
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_ptw_valid_o               ;                             // PTW req valid
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_ptw_ready_i               ;                             // PTW req ready
    TRANSLATE_REQ_TYPE [TRANS_QUEUE_DEPTH-1:0]  entry_ptw_req_o     ;                             // PTW req payload
    logic                               entry_ptw_ack_valid_i           [0:TRANS_QUEUE_DEPTH-1];      // PTW ack valid to entry
    TRANSLATE_ACK_TYPE                  entry_ptw_ack_i                 [0:TRANS_QUEUE_DEPTH-1];      // PTW ack payload
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_output_valid_o            ;                             // output transaction dequeue valid
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_output_ready_i            ;                             // output transaction dequeue ready
    tqe_info_t                          entry_output_cont_o             [0:TRANS_QUEUE_DEPTH-1];      // output transaction dequeue AXI Info
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_output_fault_o            ;                             // output transaction dequeue FAULT flag
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_output_wr_o               ;                             // output transaction dequeue W/R flag
    logic [BUS_ADDR_WIDTH-1:0]          entry_outptu_pa_o               [0:TRANS_QUEUE_DEPTH-1];      // output transaction dequeue PA
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   entry_token_o                   [0:TRANS_QUEUE_DEPTH-1];      // output transaction dequeue TOKEN

    logic                               wr_only_ready, rd_only_ready, share_ready;                      // indicates if any avaiable entry for W, W or R, R transaction
    logic [WR_ONLY_DEPTH-1:0]           share_update_valid_low_bits;

    logic                               bar_ready;
    logic [TRANS_QUEUE_IDX_WIDTH+1:0]   bar_w_cnt, bar_r_cnt;

    logic [TRANS_QUEUE_DEPTH-1:0]       bar_entry_update_i;
    tqe_info_t                          bar_entry_trans_info_i          [0:TRANS_QUEUE_DEPTH-1];
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   bar_entry_pair_token_i          [0:TRANS_QUEUE_DEPTH-1];
    logic [TRANS_QUEUE_DEPTH-1:0]       bar_entry_qentry_valid_i        [0:TRANS_QUEUE_DEPTH-1];
    logic                               bar_entry_wbuf_dequeue_fifo_empty_i [0:TRANS_QUEUE_DEPTH-1];
    logic [TRANS_QUEUE_DEPTH-1:0]       bar_entry_valid_o;
    logic [TRANS_QUEUE_DEPTH-1:0]       bar_entry_output_valid_o;
    logic [TRANS_QUEUE_DEPTH-1:0]       bar_entry_output_ready_i;
    tqe_info_t                          bar_entry_output_cont_o         [0:TRANS_QUEUE_DEPTH-1];
    logic                               bar_entry_output_wait_o         [0:TRANS_QUEUE_DEPTH-1];
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   bar_entry_output_pair_token_o   [0:TRANS_QUEUE_DEPTH-1];

    logic [TRANS_QUEUE_IDX_WIDTH-1:0]   bar_entry_wr_ptr, bar_entry_rd_ptr;

    logic                               bar_pair_token_fifo_push, bar_pair_token_fifo_pop;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   bar_pair_token_fifo_in;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   bar_pair_token_fifo_out;

    logic                               bar_entry_output_valid_int, bar_entry_output_ready_int;
    tqe_info_t                          bar_entry_output_cont_int;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]   bar_entry_output_pair_token_int;

    logic                               bar_output_wait;

    logic                               bus_acc_arb_ready, bus_acc_arb_valid;

    logic                               bar_pair_got;

    logic                               mrif_barrier_axid_valid;
    logic [BUS_ID_WIDTH-1:0]            mrif_barrier_axid;
    logic [TRANS_QUEUE_DEPTH-1:0]       entry_output_mrif_o;
    logic [BUS_ID_WIDTH-1:0]            entry_axid_o                    [0:TRANS_QUEUE_DEPTH-1];
//}}}

//=== Main Code === {{{
//=== QUEUE_ENTRY instantiate {{{
genvar i;
generate
    for(i=0; i<TRANS_QUEUE_DEPTH; i++) begin: queue_entry_gen                                       // queue entry instantiate and connection
        assign entry_trans_valid_i           [i] = s2q_valid_i;                                     // entry input directly form input transaction
        assign entry_trans_device_id_i       [i] = s2q_device_id_i;
        assign entry_trans_process_id_valid_i[i] = s2q_process_id_valid_i;
        assign entry_trans_process_id_i      [i] = s2q_process_id_i;
        assign entry_trans_is_translated_i   [i] = s2q_is_translated_i;
        assign entry_trans_info_i            [i] = {
                                                    1'b0            ,
                                                    1'b0            ,
                                                    s2q_wr_i        ,
                                                    s2q_axpayld_i   ,
                                                    s2q_token_i     
                                                    };
        assign entry_trans_bc_fail_i         [i] = s2q_bc_fail_i;

        always@(*) begin                                                                            // dependency bit connection
            for (int unsigned j=0; j<TRANS_QUEUE_DEPTH; j++) begin
                entry_depend_bits_i[i][j] = (i==j) ? 1'b0 : entry_depend_bit_o[j];
                entry_valid_i[i][j]       = (i==j) ? 1'b0 : entry_valid_o[j];
            end
        end

        assign entry_wdata_ready_i           [i] = wdata_ready_i[entry_token_o[i]];                 // AXI W.DATA ready flag 

        //assign entry_ptw_req_ready_i=;
        assign entry_ptw_ack_valid_i         [i] = ptw_ack_valid_i & (ptw_ack_i.idx == i);          // PTW ack demux
        //assign entry_output_ready_i=;
        assign entry_ptw_ack_i               [i] = ptw_ack_i;

        iommu_acd_bus_handler_trans_queue_entry #(
        /*parameter  */ .IDX_WIDTH                  (TRANS_QUEUE_IDX_WIDTH      ), // = 8             ,
        /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), // = 6             ,
        /*parameter  */ .BUS_ADDR_WIDTH             (BUS_ADDR_WIDTH             ), // = 64            ,
        /*parameter  */ .BUS_ID_WIDTH               (BUS_ID_WIDTH               ), // = 8             ,
        /*parameter type         */ .TQE_INFO_TYPE              (tqe_info_t                 ), // = iommu_acd_pkg::tqe_info_t,
        /*parameter type         */ .TRANSLATE_REQ_TYPE         (TRANSLATE_REQ_TYPE         ), // = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
        /*parameter type         */ .TRANSLATE_ACK_TYPE         (TRANSLATE_ACK_TYPE         )  // = iommu_acd_pkg::TRANSLATE_ACK_TYPE 
        ) U_entry(
        /*input  logic                                      */  .clk                        (clk                                ),
        /*input  logic                                      */  .rstn                       (rstn                               ),
        /*input  logic [IDX_WIDTH-1:0]                      */  .idx                        (TRANS_QUEUE_IDX_WIDTH'(i)          ),
        /*input  logic                                      */  .update_i                   (entry_update_i                  [i]),
        /*input  logic                                      */  .trans_valid_i              (entry_trans_valid_i             [i]),
        /*input  logic [23:0]                               */  .trans_device_id_i          (entry_trans_device_id_i         [i]),
        /*input  logic                                      */  .trans_process_id_valid_i   (entry_trans_process_id_valid_i  [i]),
        /*input  logic [19:0]]                              */  .trans_process_id_i         (entry_trans_process_id_i        [i]),
        /*input  logic                                      */  .trans_is_translated_i      (entry_trans_is_translated_i     [i]),
        /*input  TQE_INFO_TYPE                              */  .trans_info_i               (entry_trans_info_i              [i]),
        /*input  logic                                      */  .trans_bc_fail_i            (entry_trans_bc_fail_i           [i]),
        /*input  logic [IDX_VECTOR_WIDTH-1:0]               */  .depend_bits_i              (entry_depend_bits_i             [i]),
        /*output logic                                      */  .depend_bit_o               (entry_depend_bit_o              [i]),
        /*input  logic [IDX_VECTOR_WIDTH-1:0]               */  .valid_i                    (entry_valid_i                   [i]),
        /*input  logic                                      */  .wdata_ready_i              (entry_wdata_ready_i             [i]),
        /*output logic                                      */  .valid_o                    (entry_valid_o                   [i]),
        /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]          */  .token_o                    (entry_token_o                   [i]),
        /*output logic [BUS_ID_WIDTH-1:0]                   */  .axid_o                     (entry_axid_o                    [i]),
        /*output logic                                      */  .ptw_valid_o                (entry_ptw_valid_o               [i]),
        /*input  logic                                      */  .ptw_ready_i                (entry_ptw_ready_i               [i]),
        /*output TRANSLATE_REQ_TYPE                         */  .ptw_req_o                  (entry_ptw_req_o                 [i]),
        /*input  logic                                      */  .ptw_ack_valid_i            (entry_ptw_ack_valid_i           [i]),
        /*input  TRANSLATE_ACK_TYPE                         */  .ptw_ack_i                  (entry_ptw_ack_i                 [i]),
        /*output logic                                      */  .output_valid_o             (entry_output_valid_o            [i]),
        /*input  logic                                      */  .output_ready_i             (entry_output_ready_i            [i]),
        /*output TQE_INFO_TYPE                              */  .output_cont_o              (entry_output_cont_o             [i]),
        /*output logic                                      */  .output_fault_o             (entry_output_fault_o            [i]),
        /*output logic                                      */  .output_wr_o                (entry_output_wr_o               [i]),
        /*output logic                                      */  .output_mrif_o              (entry_output_mrif_o             [i]),
        /*input  logic                                      */  .mrif_barrier_axid_valid_i  (mrif_barrier_axid_valid            ),
        /*input  logic [BUS_ID_WIDTH-1:0]                   */  .mrif_barrier_axid_i        (mrif_barrier_axid                  ),
        /*                                                  */  .spare_in(1'b0)
        );
    end
endgenerate

//}}}

//=== ENTRY UPDATE DEMUX {{{                                                                                    // enqueue control
    //assign s2q_ready_o = wr_only_ready | rd_only_ready | share_ready;
    assign wr_only_ready= ~(&entry_valid_o[(WR_ONLY_DEPTH-1)              :0]);                                 // WR_ONLY entries all occupied check
    assign share_ready  = ~(&entry_valid_o[(WR_ONLY_DEPTH+SHARE_DEPTH-1)  :WR_ONLY_DEPTH]);                     // SHARE entries all occupied check
    assign rd_only_ready= ~(&entry_valid_o[(TRANS_QUEUE_DEPTH-1)          :(TRANS_QUEUE_DEPTH-RD_ONLY_DEPTH)]); // RD_ONLY entries all occupied check
    assign s2q_ready_o  = ((BUS_PROPERTY_BAR!=0) & s2q_axpayld_i.axbar[0]) ?  bar_ready :
                          s2q_wr_i                                    ?  (wr_only_ready | share_ready) :
                                                                         (rd_only_ready | share_ready);         // ready to slv_intf
    assign s2q_w_can_enqueue_hint_o = wr_only_ready & (~share_ready) & (~rd_only_ready);                        // arb hint to slv_intf, indicates that W transaction can enqueue
    assign share_update_valid_low_bits = s2q_wr_i ? entry_valid_o[(WR_ONLY_DEPTH-1):0] : 
                                                    entry_valid_o[(TRANS_QUEUE_DEPTH-1):(TRANS_QUEUE_DEPTH-RD_ONLY_DEPTH)];

//genvar j,k,l;
genvar g;
generate
    for(g = 0; g < TRANS_QUEUE_DEPTH; g++) begin : entry_update_gen
        if(g < WR_ONLY_DEPTH) begin : wr_only_entry_update                                                      // if input transaction is WR, and WR_ONLY entries not all occupied, store in WR_ONLY entries
    //for(j = 0; j < WR_ONLY_DEPTH; j++) begin
        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(TRANS_QUEUE_IDX_WIDTH)) U_entry_valid_check(
            .valid_i    (entry_valid_o              ),
            .update_i   (s2q_valid_i & s2q_wr_i & (~s2q_axpayld_i.axbar[0] | BUS_PROPERTY_BAR==0)   ),
            .tag_i      (TRANS_QUEUE_IDX_WIDTH'(g)  ),
            .update_o   (entry_update_i[g]          )
        );
        end
        else if(g < (WR_ONLY_DEPTH+SHARE_DEPTH)) begin : share_entry_update                                     // if WR_ONLY and RD_ONLY entries all occupied, use SHARE entries
    //end
    //for(k = WR_ONLY_DEPTH; k < (WR_ONLY_DEPTH+SHARE_DEPTH); j++) begin
         iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(TRANS_QUEUE_IDX_WIDTH)) U_entry_valid_check(
            .valid_i    ({entry_valid_o[(TRANS_QUEUE_DEPTH-1):WR_ONLY_DEPTH],share_update_valid_low_bits}   ),
            .update_i   (s2q_valid_i & (~s2q_axpayld_i.axbar[0] | BUS_PROPERTY_BAR==0)              ),
            .tag_i      (TRANS_QUEUE_IDX_WIDTH'(g)  ),
            .update_o   (entry_update_i[g]          )
        );
        end
        else begin : rd_only_entry_update                                                                       // if input transaction is RD, and RD_ONLY entries not all occupied, store in RD_ONLY entries
    //end
    //for(l = (WR_ONLY_DEPTH + SHARE_DEPTH); l < TRANS_QUEUE_DEPTH; j++) begin
        logic [WR_ONLY_DEPTH+SHARE_DEPTH-1:0] non_rdonly_depth_all_1;
        assign non_rdonly_depth_all_1 = {(WR_ONLY_DEPTH+SHARE_DEPTH){1'b1}};
        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(TRANS_QUEUE_IDX_WIDTH)) U_entry_valid_check(
            .valid_i    ({entry_valid_o[(TRANS_QUEUE_DEPTH-1):(TRANS_QUEUE_DEPTH-RD_ONLY_DEPTH)], non_rdonly_depth_all_1}  ),
            .update_i   (s2q_valid_i & (~s2q_wr_i) & (~s2q_axpayld_i.axbar[0] | BUS_PROPERTY_BAR==0)),
            .tag_i      (TRANS_QUEUE_IDX_WIDTH'(g)                  ),
            .update_o   (entry_update_i[g]                          )
        );
        end
    end
    //end
endgenerate

    always@(*) begin
        enqueue_qidx_o = 'd0;
        for(int unsigned kl = 0; kl < TRANS_QUEUE_DEPTH; kl++) begin
            if(entry_update_i[kl] == 1'b0)
                enqueue_qidx_o = kl;
        end
    end
//}}}

//=== BUS ACC ARB {{{                                                                                                       // dequeue arbiter
    tqe_info_t [TRANS_QUEUE_DEPTH-1:0]  acc_arb_data_i;
    tqe_info_t                          acc_arb_data_o;
    always@(*) begin
        for(int unsigned mk=0; mk<TRANS_QUEUE_DEPTH; mk++) begin
            //acc_arb_data_i[($bits(tqe_info_t)*(mk+1)-1):($bits(tqe_info_t)*mk)] = entry_output_cont_o[mk];
            //acc_arb_data_i = {acc_arb_data_i[$bits(tqe_info_t)*(TRANS_QUEUE_DEPTH-1)-1:0], entry_output_cont_o[mk]};
// combine entry's output_fault_o with output_cont_o to arb input, 20241111
//            acc_arb_data_i[mk] = entry_output_cont_o[mk];
            acc_arb_data_i[mk].mrif     = entry_output_mrif_o[mk];
            acc_arb_data_i[mk].fault    = entry_output_fault_o[mk];
            acc_arb_data_i[mk].wr       = entry_output_cont_o[mk].wr    ;
            acc_arb_data_i[mk].axpayld  = entry_output_cont_o[mk].axpayld;
            acc_arb_data_i[mk].token    = entry_output_cont_o[mk].token ;
        end
    end

    assign bus_acc_arb_ready = (bar_output_wait & (BUS_PROPERTY_BAR!=0)) ? 1'b0 : qout_ready_i;
    assign qout_valid_o      = (bar_output_wait & (BUS_PROPERTY_BAR!=0)) ? 1'b0 : bus_acc_arb_valid;

    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                          ),   //= 0,
    /*parameter     */ .REQ_NUM             (TRANS_QUEUE_DEPTH          ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (tqe_info_t                 ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                          )    //= 1
    ) U_bus_acc_arb(
    /*input  logic                    */ .clk           (clk                        ),
    /*input  logic                    */ .rstn          (rstn                       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_output_valid_o       ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TRANS_QUEUE_DEPTH{1'b0}}  ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (acc_arb_data_i             ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_output_ready_i       ),
    /*output logic                    */ .req_o         (bus_acc_arb_valid          ),
    /*output DATA_TYPE                */ .data_o        (acc_arb_data_o             ),
    /*input  logic                    */ .gnt_i         (bus_acc_arb_ready          ) 
    );

// combine entry's output_fault_o with ouput_cont_o to arb_input ,so qout_fault_o just extracted from arb output, 20241111
//    assign qout_fault_o = |(entry_output_fault_o & entry_output_ready_i);
    assign qout_fault_o     = acc_arb_data_o.fault ;
    //always@(*) begin
    //    qout_wr_o     = 'd0;
    //    for(int unsigned mn = 0; mn < TRANS_QUEUE_DEPTH; mn++) begin
    //        if(entry_output_ready_i[mn] == 1'b1) begin
    //            qout_wr_o     = entry_output_wr_o[mn];
    //        end
    //    end
    //end
    assign qout_wr_o        = acc_arb_data_o.wr     ;
    assign qout_axpayld_o   = acc_arb_data_o.axpayld;
    assign qout_token_o     = acc_arb_data_o.token  ;
    assign qout_mrif_o      = acc_arb_data_o.mrif   ;
//}}}

//=== PTW OUT ARB {{{                                                                           // PTW req output arbiter
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0),   //= 0,
    /*parameter     */ .REQ_NUM             (TRANS_QUEUE_DEPTH),   //= 2,
    /*parameter type            */ .DATA_TYPE           (TRANSLATE_REQ_TYPE),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1)    //= 1
    ) U_ptw_req_arb(
    /*input  logic                    */ .clk           (clk),
    /*input  logic                    */ .rstn          (rstn),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_ptw_valid_o),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({TRANS_QUEUE_DEPTH{1'b0}}),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (entry_ptw_req_o),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_ptw_ready_i),
    /*output logic                    */ .req_o         (ptw_req_valid_o),
    /*output DATA_TYPE                */ .data_o        (ptw_req_o),
    /*input  logic                    */ .gnt_i         (ptw_req_ready_i) 
    );
//}}}

//=== BAR_ENTRY UPDATE DEMUX {{{
genvar bar;
generate
    if(BUS_PROPERTY_BAR) begin: bar_gen
        assign bar_ready = ~(&bar_entry_valid_o);
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                bar_w_cnt <= 'd0;
            else begin
                if(bar_pair_got & ~s2q_wr_i)
                    bar_w_cnt <= bar_w_cnt - 'd1;
                else if(~bar_pair_got & s2q_valid_i & s2q_axpayld_i.axbar[0] & s2q_wr_i & bar_ready)
                    bar_w_cnt <= bar_w_cnt + 'd1;
            end
        end
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                bar_r_cnt <= 'd0;
            else begin
                if(bar_pair_got &  s2q_wr_i)
                    bar_r_cnt <= bar_r_cnt - 'd1;
                else if(~bar_pair_got & s2q_valid_i & s2q_axpayld_i.axbar[0] & ~s2q_wr_i & bar_ready)
                    bar_r_cnt <= bar_r_cnt + 'd1;
            end
        end
    
        assign bar_pair_got = s2q_valid_i & s2q_axpayld_i.axbar[0] & bar_ready &
                              ((s2q_wr_i & |bar_r_cnt) | (~s2q_wr_i & |bar_w_cnt));
    
        assign bar_pair_token_fifo_push = s2q_valid_i & s2q_axpayld_i.axbar[0] & bar_ready & ~bar_pair_got;
        assign bar_pair_token_fifo_in   = s2q_token_i;
        assign bar_pair_token_fifo_pop  = bar_pair_got;
    
        iommu_acd_bus_handler_sync_fifo #(
        /*parameter  */ .FIFOTYPE       (0                      ), // = 0,    // 0:FF 1:RAM
        /*parameter  */ .WIDTH          (BUS_INFLY_TOKEN_WIDTH  ), // = 128,
        /*parameter  */ .DEPTH          (2*TRANS_QUEUE_DEPTH    ), // = 32,
        /*parameter  */ .SPARE_PARA     (0                      )  // = 0
        ) U_bar_pair_token_fifo(
        /*input  logic                          */  .clk                    (clk                        ),
        /*input  logic                          */  .rstn                   (rstn                       ),
        /*input  logic                          */  .push_i                 (bar_pair_token_fifo_push   ),
        /*output logic                          */  .full_o                 (),
        /*output logic                          */  .afull_o                (), // almost full
        /*input  logic [WIDTH-1:0]              */  .wdata_i                (bar_pair_token_fifo_in     ),
        /*input  logic                          */  .pop_i                  (bar_pair_token_fifo_pop    ),
        /*output logic                          */  .empty_o                (),
        /*output logic                          */  .aempty_o               (),
        /*output logic [WIDTH-1:0]              */  .rdata_o                (bar_pair_token_fifo_out    ),
        /*input  logic                          */  .spare_in               () 
        );
    
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                bar_entry_wr_ptr <= 'd0;
            else begin
                if(bar_pair_got)
                    bar_entry_wr_ptr <= bar_entry_wr_ptr + 'd1;
            end
        end
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                bar_entry_rd_ptr <= 'd0;
            else begin
                if(bar_entry_output_valid_int & bar_entry_output_ready_int)
                    bar_entry_rd_ptr <= bar_entry_rd_ptr + 'd1;
            end
        end
    
        always@(*) begin
            bar_entry_output_valid_int      = bar_entry_output_valid_o[bar_entry_rd_ptr];
            bar_entry_output_cont_int       = bar_entry_output_cont_o[bar_entry_rd_ptr];
            bar_output_wait                 = bar_entry_output_wait_o[bar_entry_rd_ptr];
            bar_entry_output_pair_token_int = bar_entry_output_pair_token_o[bar_entry_rd_ptr];
        end
    
        assign q2m_valid_o      = bar_entry_output_valid_int;
        assign bar_entry_output_ready_int = q2m_ready_i;
        assign q2m_wr_o         = bar_entry_output_cont_int.wr;
        assign q2m_axpayld_o    = bar_entry_output_cont_int.axpayld;
        assign q2m_token_o      = bar_entry_output_cont_int.token;
        assign q2m_pair_token_o = bar_entry_output_pair_token_int;
        assign q2m_fault_o      = 1'b0;
    
        for(bar=0; bar<TRANS_QUEUE_DEPTH; bar++) begin : bar_entry_gen
            assign bar_entry_update_i[bar]      = bar_pair_got & (bar_entry_wr_ptr==bar);
            assign bar_entry_trans_info_i[bar]  = {
                                                    1'b0            ,
                                                    1'b0            ,
                                                    s2q_wr_i        ,
                                                    s2q_axpayld_i   ,
                                                    s2q_token_i     
                                                    };
            assign bar_entry_pair_token_i[bar]  = bar_pair_token_fifo_out;
            always@(*) begin
                for(int unsigned eqval=0; eqval<TRANS_QUEUE_DEPTH; eqval++) begin
                    bar_entry_qentry_valid_i[bar][eqval] = entry_valid_o[eqval];
                end
            end
            assign bar_entry_wbuf_dequeue_fifo_empty_i[bar]=wbuf_dequeue_fifo_empty_i;
            assign bar_entry_output_ready_i[bar]= bar_entry_output_ready_int & (bar_entry_rd_ptr==TRANS_QUEUE_IDX_WIDTH'(bar));
        
            iommu_acd_bus_handler_bar_queue_entry #(
            /*parameter  */ .IDX_WIDTH              (TRANS_QUEUE_IDX_WIDTH      ), // = 8                             ,
            /*parameter  */ .BUS_ADDR_WIDTH         (BUS_ADDR_WIDTH             ), // = 64                            ,
            /*parameter  */ .BUS_ID_WIDTH           (BUS_ID_WIDTH               ), // = 8                             ,
            /*parameter  */ .BUS_INFLY_TOKEN_WIDTH  (BUS_INFLY_TOKEN_WIDTH      ), // = 6                             ,
            /*parameter type         */ .TQE_INFO_TYPE          (tqe_info_t                 )  // = iommu_acd_pkg::tqe_info_t     ,
            ) U_bar_entry(
            /*input  logic                                  */  .clk                        (clk                                    ),
            /*input  logic                                  */  .rstn                       (rstn                                   ),
            /*input  logic                                  */  .update_i                   (bar_entry_update_i                     [bar]),
            /*input  TQE_INFO_TYPE                          */  .trans_info_i               (bar_entry_trans_info_i                 [bar]),
            /*input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]      */  .pair_token_i               (bar_entry_pair_token_i                 [bar]),
            /*input  logic [IDX_VECTOR_WIDTH-1:0]           */  .qentry_valid_i             (bar_entry_qentry_valid_i               [bar]),
            /*input  logic                                  */  .wbuf_dequeue_fifo_empty_i  (bar_entry_wbuf_dequeue_fifo_empty_i    [bar]),
            /*output logic                                  */  .valid_o                    (bar_entry_valid_o                      [bar]),
            /*output logic                                  */  .output_valid_o             (bar_entry_output_valid_o               [bar]),
            /*input  logic                                  */  .output_ready_i             (bar_entry_output_ready_i               [bar]),
            /*output TQE_INFO_TYPE                          */  .output_cont_o              (bar_entry_output_cont_o                [bar]),
            /*output logic                                  */  .output_wait_o              (bar_entry_output_wait_o                [bar]),
            /*output logic [BUS_INFLY_TOKEN_WIDTH-1:0]      */  .output_pair_token_o        (bar_entry_output_pair_token_o          [bar]),
            /*input  logic                                  */  .spare_in                   (1'b0                                   )
            );
        end
    end
    else begin: nobar_gen
        assign bar_ready                        = 'd0;
        assign bar_w_cnt                        = 'd0;
        assign bar_r_cnt                        = 'd0;
        assign bar_pair_got                     = 'd0;
        assign bar_pair_token_fifo_push         = 'd0;
        assign bar_pair_token_fifo_in           = 'd0;
        assign bar_pair_token_fifo_pop          = 'd0;
        assign bar_pair_token_fifo_out          = 'd0;
        assign bar_entry_wr_ptr                 = 'd0;
        assign bar_entry_rd_ptr                 = 'd0;
        assign bar_entry_output_valid_int       = 'd0;
        assign bar_entry_output_cont_int        = 'd0;
        assign bar_output_wait                  = 'd0;
        assign bar_entry_output_pair_token_int  = 'd0;
        assign q2m_valid_o                      = 'd0;
        assign bar_entry_output_ready_int       = 'd0;
        assign q2m_wr_o                         = 'd0;
        assign q2m_axpayld_o                    = 'd0;
        assign q2m_token_o                      = 'd0;
        assign q2m_pair_token_o                 = 'd0;
        assign q2m_fault_o                      = 1'b0;
        genvar bar;
        for(bar=0; bar<TRANS_QUEUE_DEPTH; bar++) begin : bar_entry_gen
            assign bar_entry_update_i[bar]                  = 'd0;
            assign bar_entry_trans_info_i[bar]              = 'd0;
            assign bar_entry_pair_token_i[bar]              = 'd0;
            assign bar_entry_qentry_valid_i[bar]            = 'd0;
            assign bar_entry_wbuf_dequeue_fifo_empty_i[bar] = 'd0;
            assign bar_entry_output_ready_i[bar]            = 'd0;
            assign bar_entry_valid_o[bar]                   = 'd0;
            assign bar_entry_output_valid_o[bar]            = 'd0;
            assign bar_entry_output_cont_o[bar]             = 'd0;
            assign bar_entry_output_wait_o[bar]             = 'd0;
            assign bar_entry_output_pair_token_o[bar]       = 'd0;
        end
    end

endgenerate
//}}}

//=== MRIF {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            mrif_barrier_axid_valid <= 1'b0;
            mrif_barrier_axid       <= 'd0;
        end
        else begin
            if(mrif_done_i) begin
                mrif_barrier_axid_valid <= 1'b0;
            end
            else if(qout_valid_o & qout_ready_i & qout_mrif_o) begin
                mrif_barrier_axid_valid <= 1'b1;
                mrif_barrier_axid       <= qout_axpayld_o.axid;
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            mrif_credit_grant_valid_o <= 1'b0;
        else begin
            mrif_credit_grant_valid_o <= 1'b0;
            if(mrif_barrier_axid_valid & mrif_done_i)
                mrif_credit_grant_valid_o <= 1'b1;
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            mrif_valid_o<= 1'b0;
            mrif_nppn_o <= 'd0;
            mrif_nid_o  <= 'd0;
            mrif_axid_o <= 'd0;
            mrif_token_o<= 'd0;
        end
        else begin
            mrif_valid_o <= 1'b0;
            if(ptw_ack_valid_i & ptw_ack_i.mrif & entry_output_wr_o[ptw_ack_i.idx[TRANS_QUEUE_IDX_WIDTH-1:0]]) begin
                mrif_valid_o<= 1'b1;
                mrif_nppn_o <= ptw_ack_i.nppn;
                mrif_nid_o  <= ptw_ack_i.nid;
                mrif_axid_o <= entry_axid_o[ptw_ack_i.idx[TRANS_QUEUE_IDX_WIDTH-1:0]];
                mrif_token_o<= entry_token_o[ptw_ack_i.idx[TRANS_QUEUE_IDX_WIDTH-1:0]];
            end
        end
    end
//}}}

//}}}

endmodule
//}}}



