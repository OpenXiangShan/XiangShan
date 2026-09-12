module iommu_acd_ptw #(
    parameter type          PTW_REQ_TYPE                = iommu_acd_pkg::PTW_REQ_TYPE,
    parameter type          PTW_ACK_TYPE                = iommu_acd_pkg::PTW_ACK_TYPE,
    parameter               PTW_IDX_WIDTH               = iommu_acd_pkg::PTW_IDX_WIDTH,
    parameter               PTW_INFLY_NUM               = 2**PTW_IDX_WIDTH,
    parameter               TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter               TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter type          PTW_REQ_GRP_TYPE            = iommu_acd_pkg::ptw_req_grp_t,
    parameter               SPARE_PARAM                 = 0
)(
//==={{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // TLB_QUEUE                                        
    input  logic                                        ptw_req_valid_i,
    output logic                                        ptw_req_ready_o,
    input  PTW_REQ_TYPE                                 ptw_req_i,
    output logic                                        ptw_ack_valid_o,
    output PTW_ACK_TYPE                                 ptw_ack_o,
    output PTW_REQ_TYPE                                 ptw_req_o,  // original ptw_req out for invalid filter
    // T2C IF                                           
    input  logic                                        msg_valid_i,
    output logic                                        msg_ready_o,
    input  logic [63:0]                                 msg_wdata_i,
    // C2T IF                                           
    output logic                                        msg_pvalid_o,
    input  logic                                        msg_pready_i,
    output logic [63:0]                                 msg_pdata_o,
    output logic                                        msg_plast_o,
    // INFO from TLBQUEUE
    input  PTW_REQ_GRP_TYPE                             ptwreqinfo2_ptw_i,
    //                                                  
    input  logic                                        spare_in            
//}}}
);
//=== Declare {{{
    logic [3:0]                                         ocnt;
    logic                                               fifo_push, fifo_pop;
    logic                                               fifo_full, fifo_empty;
    PTW_REQ_TYPE                                        fifo_in,   fifo_out;
    iommu_acd_pkg::MSG_PTW_REQ_TYPE                     msg_ptw_req;
    logic                                               ptw_ack_got;
    logic [3:0]                                         incnt;
    logic [PTW_IDX_WIDTH:0 ]                            token;
                                                        
    logic [7:0]                                         ack_idx;
    logic [2:0]                                         ack_opcode;
    logic [1:0]                                         ack_s1size;
    logic [1:0]                                         ack_s2size;
    logic [57:12]                                       ack_gpa;
    logic [7:0]                                         ack_dc_pc;
    logic [3:0]                                         ack_s1mode;
    logic [3:0]                                         ack_s2mode;
    logic [3:0]                                         ack_pdtmode;
    logic [19:0]                                        ack_pscid;
    logic [15:0]                                        ack_gscid;
    logic [4:0]                                         ack_s1perm;
    logic [4:0]                                         ack_s2perm;
    logic                                               ack_n;
    logic [63:12]                                       ack_pa;
//    logic [15:0]                                        ack_s1avec;
//    logic [15:0]                                        ack_s1dvec;
//    logic [15:0]                                        ack_s2avec;
//    logic [15:0]                                        ack_s2dvec;
//    logic [31:0]                                        ack_s1pbmtvec;
//    logic [31:0]                                        ack_s2pbmtvec;
    logic                                               ack_s1_d;
    logic                                               ack_s2_d;
    logic                                               ack_sade;
    logic                                               ack_gade;
    logic [1:0]                                         ack_s1_pbmt;
    logic [1:0]                                         ack_s2_pbmt;
    logic                                               ack_mrif;
    logic [43:0]                                        ack_nppn;
    logic [10:0]                                        ack_nid;
    //logic [55:9]                                        ack_mrif_addr;
    PTW_ACK_TYPE                                        ptw_ack;
//}}}

//=== MainCode {{{
//=== out fifo {{{
    assign fifo_push= ptw_req_valid_i & ~fifo_full;
    assign ptw_req_ready_o = ~fifo_full;
    assign fifo_in  = ptw_req_i;

    assign fifo_pop = (msg_pvalid_o & msg_pready_i & ocnt=='d1);

    iommu_acd_bus_handler_sync_fifo #(
    /*parameter */ .WIDTH          ($bits(PTW_REQ_TYPE)    ), // = 128,
    /*parameter */ .DEPTH          (1                      ), // = 32,
    /*parameter */ .SPARE_PARA     (0                      )  // = 0
    ) U_fifo(
    /*input  logic                           */ .clk                    (clk                    ),
    /*input  logic                           */ .rstn                   (rstn                   ),
    /*input  logic                           */ .push_i                 (fifo_push              ),
    /*output logic                           */ .full_o                 (fifo_full              ),
    /*output logic                           */ .afull_o                (                       ), // almost full
    /*input  logic [WIDTH-1:0]               */ .wdata_i                (fifo_in                ),
    /*input  logic                           */ .pop_i                  (fifo_pop               ),
    /*output logic                           */ .empty_o                (fifo_empty             ),
    /*output logic                           */ .aempty_o               (                       ),
    /*output logic [WIDTH-1:0]               */ .rdata_o                (fifo_out               ),
    /*input  logic                           */ .spare_in               (1'b0                   ) 
    );
//}}}

//=== PTW msg out {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            ocnt <= 'd0;
        else begin
            if(msg_pvalid_o & msg_pready_i) begin
                if(fifo_pop)
                    ocnt <= 'd0;
                else
                    ocnt <= ocnt + 'd1;
            end
        end
    end

    assign msg_ptw_req.msg_code = iommu_acd_pkg::MSGCODE_PTW_REQ;
generate
    if(8>TLB_QIDX_WIDTH+1) begin
        assign msg_ptw_req.idx  = {(7-TLB_QIDX_WIDTH)'(0), fifo_out.idx};
    end
    else begin
        assign msg_ptw_req.idx  = fifo_out.idx[7:0];
    end
endgenerate
    assign msg_ptw_req.proto    = 1'b0;
    assign msg_ptw_req.reserved0= 'd0;
    assign msg_ptw_req.pv       = fifo_out.process_id_valid;
    assign msg_ptw_req.process_id= fifo_out.process_id;
    assign msg_ptw_req.device_id= fifo_out.device_id;
    assign msg_ptw_req.va       = fifo_out.va;
    assign msg_ptw_req.attr[5:0]= (~fifo_out.is_translated & ~fifo_out.wr & fifo_out.ext) ? iommu_acd_pkg::FTTYP_UNTRANSLATED_READ_FOR_EXECUTE_TRANSACTION :
                                  (~fifo_out.is_translated & ~fifo_out.wr)                ? iommu_acd_pkg::FTTYP_UNTRANSLATED_READ_TRANSACTION :
                                  (~fifo_out.is_translated &  fifo_out.wr)                ? iommu_acd_pkg::FTTYP_UNTRANSLATED_WRITE_AMO_TRANSACTION :
                                  ( fifo_out.is_translated & ~fifo_out.wr & fifo_out.ext) ? iommu_acd_pkg::FTTYP_TRANSLATED_READ_FOR_EXECUTE_TRANSACTION :
                                  ( fifo_out.is_translated & ~fifo_out.wr)                ? iommu_acd_pkg::FTTYP_TRANSLATED_READ_TRANSACTION :
                                  ( fifo_out.is_translated &  fifo_out.wr)                ? iommu_acd_pkg::FTTYP_TRANSLATED_WRITE_AMO_TRANSACTION : 
                                                                                            6'd0;
    //assign msg_ptw_req.attr[0]  = ~fifo_out.wr;
    //assign msg_ptw_req.attr[1]  =  fifo_out.wr;
    //assign msg_ptw_req.attr[2]  = fifo_out.ext;
    //assign msg_ptw_req.attr[3]  = fifo_out.priv;
    //assign msg_ptw_req.attr[4]  = fifo_out.is_translated;
    assign msg_ptw_req.attr[6]  = fifo_out.priv;
    assign msg_ptw_req.attr[11:7]= 'd0;

    assign msg_pvalid_o = ~fifo_empty & (token > 'd0);
    assign msg_pdata_o  = (ocnt=='d0) ? {msg_ptw_req.device_id, msg_ptw_req.process_id, msg_ptw_req.pv, msg_ptw_req.reserved0, msg_ptw_req.proto, msg_ptw_req.idx, msg_ptw_req.msg_code} :
                          (ocnt=='d1) ? {msg_ptw_req.va, msg_ptw_req.attr} : 'd0;

    assign msg_plast_o  = (ocnt=='d1);

//}}}

//=== incnt {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            incnt <= 'd0;
        else begin
            if(ptw_ack_got)
                incnt <= 'd0;
            else if(msg_valid_i & msg_ready_o)
                incnt <= incnt + 'd1;
        end
    end

    assign msg_ready_o = 1'b1;
    assign ptw_ack_got = msg_valid_i & msg_ready_o & (incnt=='d4);
//}}}

//=== ptw_ack to tlb_queue {{{
    assign ack_idx      = msg_wdata_i[11:4];
    assign ack_opcode   = {1'b0, msg_wdata_i[13:12]};
    assign ack_s1size   = msg_wdata_i[15:14];
    assign ack_s2size   = msg_wdata_i[17:16];
    assign ack_gpa      = msg_wdata_i[63:18];
    assign ack_dc_pc    = msg_wdata_i[7:0];
    assign ack_s1mode   = msg_wdata_i[11:8];
    assign ack_s2mode   = msg_wdata_i[15:12];
    assign ack_pdtmode  = msg_wdata_i[19:16];
    assign ack_pscid    = msg_wdata_i[39:20];
    assign ack_gscid    = msg_wdata_i[55:40];
    assign ack_s1perm   = msg_wdata_i[4:0];
    assign ack_s2perm   = msg_wdata_i[10:6];
    assign ack_n        = msg_wdata_i[11];
    assign ack_pa       = msg_wdata_i[63:12];
//    assign ack_s1avec   = msg_wdata_i[15:0];
//    assign ack_s1dvec   = msg_wdata_i[31:16];
//    assign ack_s2avec   = msg_wdata_i[47:32];
//    assign ack_s2dvec   = msg_wdata_i[63:48];
//    assign ack_s1pbmtvec= msg_wdata_i[31:0];
//    assign ack_s2pbmtvec= msg_wdata_i[63:32];
    assign ack_s1_d     = msg_wdata_i[59];
    assign ack_s2_d     = msg_wdata_i[58];
    assign ack_sade     = msg_wdata_i[57];
    assign ack_gade     = msg_wdata_i[56];
    assign ack_s1_pbmt  = msg_wdata_i[63:62];
    assign ack_s2_pbmt  = msg_wdata_i[61:60];
    assign ack_mrif     = msg_wdata_i[11];
    assign ack_nid      = msg_wdata_i[10:0];
    assign ack_nppn     = msg_wdata_i[63:20];
    //assign ack_mrif_addr= msg_wdata_i[58:12];

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            ptw_ack         <= 'd0;
        end
        else begin
            if(msg_valid_i & msg_ready_o) begin
                if(incnt=='d0) begin
                    ptw_ack.idx     <= ack_idx[TLB_QIDX_WIDTH:0];
                    ptw_ack.opcode  <= ack_opcode;
                    ptw_ack.GPPN    <= {4'b0, ack_gpa};
                    ptw_ack.S1SIZE  <= ack_s1size;
                    ptw_ack.S2SIZE  <= ack_s2size;
                end
                else if(incnt=='d1) begin
                    ptw_ack.ENATS   <= ack_dc_pc[0];
                    ptw_ack.T2GPA   <= ack_dc_pc[1];
                    ptw_ack.DTF     <= ack_dc_pc[2];
                    ptw_ack.PDTV    <= ack_dc_pc[3];
                    ptw_ack.DPE     <= ack_dc_pc[4];
                    ptw_ack.SXL     <= ack_dc_pc[5];
                    ptw_ack.ENS     <= ack_dc_pc[6];
                    ptw_ack.SUM     <= ack_dc_pc[7];
                    ptw_ack.S1MODE  <= ack_s1mode;
                    ptw_ack.S2MODE  <= ack_s2mode;
                    ptw_ack.PDTMODE <= ack_pdtmode;
                    ptw_ack.GSCID   <= ack_gscid;
                    ptw_ack.PSCID   <= ack_pscid;
                    ptw_ack.S1_D    <= ack_s1_d;
                    ptw_ack.S2_D    <= ack_s2_d;
                    ptw_ack.SADE    <= ack_sade;
                    ptw_ack.GADE    <= ack_gade;
                    ptw_ack.PBMT    <= (ack_s1_pbmt!='d0 & ack_s1_pbmt!='d3) ? ack_s1_pbmt :
                                       (ack_s2_pbmt!='d0 & ack_s2_pbmt!='d3) ? ack_s2_pbmt : 2'b00;
                end
                else if(incnt=='d2) begin
                    ptw_ack.N       <= ack_n;
                    ptw_ack.S1_PERM <= ack_s1perm;        // G U X W R
                    ptw_ack.S2_PERM <= ack_s2perm;        // G U X W R
                    ptw_ack.PPN     <= ack_pa;
                end
                else if(incnt=='d3) begin
                    ptw_ack.MRIF    <= ack_mrif;
                    if(ack_mrif) begin
                        //ptw_ack.PPN[58:12] <= ack_mrif_addr;
                        ptw_ack.GPPN[55:12]<= ack_nppn;
                        ptw_ack.PPN[63:59] <= ack_nid[4:0];
                        ptw_ack.GPPN[61:56]<= ack_nid[10:5];
                    end
                end
//                else if(incnt=='d3) begin
//                    ptw_ack.S1_PERM_D <= ack_s1dvec;
//                    ptw_ack.S1_PERM_A <= ack_s1avec;
//                    ptw_ack.S2_PERM_D <= ack_s2dvec;
//                    ptw_ack.S2_PERM_A <= ack_s2avec;
//                end
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            ptw_ack_valid_o <= 1'b0;
            ptw_ack_o       <= 'd0;
        end
        else if(ptw_ack_got) begin
            ptw_ack_valid_o <= 1'b1;
            ptw_ack_o       <= ptw_ack;
        end
        else
            ptw_ack_valid_o <= 1'b0;
    end
//}}}

//=== TOKEN CTRL {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            token <= PTW_INFLY_NUM;
        else begin
            case({ptw_ack_got, fifo_pop})
            2'b00: token <= token;
            2'b10: token <= token+'d1;
            2'b01: token <= token-'d1;
            2'b11: token <= token;
            default:token<= token;
            endcase
        end
    end
//}}}

//=== ptw_req inf store and indexed out {{{
// use ptwreqinfo from TLB_QUEUE, no local store anymore, 20241218
//    iommu_acd_bus_handler_idx_fifo #( //{{{
//    /*parameter */ .DEPTH          (PTW_INFLY_NUM          ), // = 32,
//    /*parameter */ .WIDTH          ($bits(PTW_REQ_TYPE)    ), // = 64,
//    /*parameter */ .IDX_WIDTH      (TLB_QIDX_WIDTH         ), // = 3,
//    /*parameter */ .SPARE_PARA     (1'b0                   )  // = 0
//    ) U_ptw_req(
//    /*input  logic                          */  .clk                    (clk                    ),
//    /*input  logic                          */  .rstn                   (rstn                   ),
//    /*input  logic                          */  .push_i                 (fifo_pop               ),
//    /*output logic                          */  .full_o                 (                       ),
//    /*output logic                          */  .afull_o                (                       ),
//    /*input  logic [WIDTH-1:0]              */  .wdata_i                (fifo_out               ),
//    /*input  logic [IDX_WIDTH-1:0]          */  .widx_i                 (fifo_out.idx           ),
//    /*input  logic                          */  .wlast_i                (1'b1                   ),
//    /*input  logic                          */  .pop_i                  (ptw_ack_valid_o        ),
//    /*output logic                          */  .empty_o                (                       ),
//    /*output logic                          */  .aempty_o               (                       ),
//    /*output logic [WIDTH-1:0]              */  .rdata_o                (ptw_req_o              ),
//    /*input  logic [IDX_WIDTH-1:0]          */  .ridx_i                 (ptw_ack.idx            ),
//    /*output logic [IDX_NUM-1:0]            */  .data_ready_o           (                       ),
//    /*input  logic                          */  .spare_in               (1'b0                   ) 
//    );
always@(*) begin
    ptw_req_o = ptwreqinfo2_ptw_i.ptw_req[ptw_ack.idx[TLB_QIDX_WIDTH-1:0]];
end

//}}}

//}}}

endmodule
