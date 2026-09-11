/////////////////////////////////////////////////////////////////
// iommu_acd_inv_top
/////////////////////////////////////////////////////////////////
module iommu_atd_inv_top #( //{{{
    parameter  FIFO_IDX_WIDTH              = 5, //iommu_atd_cache_pkg::INV_IDX_WIDTH,
    parameter  FIFO_DEPTH                  = 2**FIFO_IDX_WIDTH,
    parameter  SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // CMD in
    input  logic                                        invalid_req_valid_i,
    output logic                                        invalid_req_ready_o,
    input  logic [127:0]                                invalid_req_i,
    input  logic                                        invalid_req_fence_i,
    // INV to DDTC/PDTC/S1PTC/S2PTC                     
    output logic [3:0]                                  inv_req_valid_o,
    input  logic [3:0]                                  inv_req_ready_i,
    output logic [FIFO_IDX_WIDTH-1:0]                   inv_req_idx_o,
    output logic [1:0]                                  inv_req_itype_o,        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    output logic                                        inv_req_dv_gv_o,
    output logic [23:0]                                 inv_req_did_gscid_o,
    output logic                                        inv_req_pscv_o,
    output logic [19:0]                                 inv_req_pid_pscid_o,
    output logic                                        inv_req_av_o,
    output logic [63:12]                                inv_req_addr_o,
    input  logic [3:0]                                  inv_ack_valid_i,
    input  logic [3:0] [FIFO_IDX_WIDTH-1:0]             inv_ack_idx_i,
    // empty flag out
    output logic                                        cmd_fifo_empty_o,
    //                                                  
    input  logic                                        spare_in
//}}}                                                   
);
//=== Declare {{{
    typedef struct packed {
        logic [FIFO_IDX_WIDTH-1:0]                      idx;        // invalid cmd idx
        logic [1:0]                                     itype;      // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
        logic                                           dv_gv;
        logic [23:0]                                    did_gscid;
        logic                                           pscv;
        logic [19:0]                                    pid_pscid;
        logic                                           av;
        logic [63:12]                                   addr;
    } INVALID_REQ_TYPE;

    logic [1:0]                                         invalid_req_type_i;        // type. 00:INVAL_DDT 01:INV_PDT 10:VMA 11:GVMA
    logic                                               invalid_req_dv_gv_i;
    logic [23:0]                                        invalid_req_did_gscid_i;
    logic                                               invalid_req_pscv_i;
    logic [19:0]                                        invalid_req_pid_pscid_i;
    logic                                               invalid_req_av_i;
    logic [63:12]                                       invalid_req_addr_i;

    logic [6:0]                                         opcode;
    logic [9:7]                                         func3;

    logic            [FIFO_DEPTH-1:0]                   entry_update_i                 ;
    logic            [FIFO_DEPTH-1:0]                   entry_inv_req_valid_i          ;
    INVALID_REQ_TYPE [FIFO_DEPTH-1:0]                   entry_inv_req_i                ;
    logic            [FIFO_DEPTH-1:0]                   entry_inv_req_is_fence_i       ;
    logic            [FIFO_DEPTH-1:0]                   entry_inv_tlbcache_req_valid_o ;
    logic            [FIFO_DEPTH-1:0]                   entry_inv_tlbcache_req_ready_i ;
    INVALID_REQ_TYPE [FIFO_DEPTH-1:0]                   entry_inv_tlbcache_req_o       ;
    logic [3:0]                                         entry_inv_tlbcache_ack_valid_i[FIFO_DEPTH-1:0] ;
    logic            [FIFO_DEPTH-1:0]                   entry_valid_o                  ;
    logic            [FIFO_DEPTH-1:0]                   entry_valid_i       [FIFO_DEPTH-1:0];
    logic            [FIFO_DEPTH-1:0]                   entry_depend_bits_i [FIFO_DEPTH-1:0];
    logic            [FIFO_DEPTH-1:0]                   entry_depend_bit_o             ;

    logic                                               inv_req_valid_muxed;
    INVALID_REQ_TYPE                                    inv_req_muxed;
    logic                                               inv_req_ready_muxed;

//}}}

//=== MainCode {{{
    assign opcode = invalid_req_i[6:0];
    assign func3  = invalid_req_i[9:7];

    assign invalid_req_type_i       = (opcode=='h1) ? ( (func3=='h0) ? 2'b10 : 2'b11) :           // should now have other comb input
                                      (opcode=='h3) ? ( (func3=='h0) ? 2'b00 : 2'b01) :
                                      2'b00;
    assign invalid_req_dv_gv_i      = invalid_req_i[33];
    assign invalid_req_did_gscid_i  = invalid_req_type_i[1] ? {8'b0, invalid_req_i[59:44]} : invalid_req_i[63:40];
    assign invalid_req_pscv_i       = invalid_req_i[32];
    assign invalid_req_pid_pscid_i  = invalid_req_i[31:12];
    assign invalid_req_av_i         = invalid_req_i[10];
    assign invalid_req_addr_i       = invalid_req_i[125:74];

//=== {{{ ready and empty out
    assign invalid_req_ready_o      = ~(&entry_valid_o);
    assign cmd_fifo_empty_o         = ~(|entry_valid_o);
//}}}

//=== {{{ inv_req_out arb
    iommu_acd_bus_handler_trans_arb #(
    /*parameter    */ .ARB_TYPE            (1                              ),   //= 0,
    /*parameter    */ .REQ_NUM             (FIFO_DEPTH                     ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (INVALID_REQ_TYPE               ),   //= logic,
    /*parameter    */ .AXIVLDRDY           (1                              )    //= 1
    ) U_inv_req_arb(
    /*input  logic                    */ .clk           (clk                            ),
    /*input  logic                    */ .rstn          (rstn                           ),
    /*input  logic [REQ_NUM-1:0]      */ .req_i         (entry_inv_tlbcache_req_valid_o ),
    /*input  logic [REQ_NUM-1:0]      */ .req_prior_i   ({FIFO_DEPTH{1'b0}}             ),
    /*input  DATA_TYPE [REQ_NUM-1:0]  */ .data_i        (entry_inv_tlbcache_req_o       ),
    /*output logic [REQ_NUM-1:0]      */ .gnt_o         (entry_inv_tlbcache_req_ready_i ),
    /*output logic                    */ .req_o         (inv_req_valid_muxed            ),
    /*output DATA_TYPE                */ .data_o        (inv_req_muxed                  ),
    /*input  logic                    */ .gnt_i         (inv_req_ready_muxed            ) 
    );
//}}}

//=== {{{ inv_req out 1to4 demux    
    iommu_acd_mtlb_ready_mux #(
    /*parameter */ .NUM    (4              )  // = 2
    ) U_inv_req_out(
    /*input  logic                  */  .clk            (clk                            ),
    /*input  logic                  */  .rstn           (rstn                           ),
    /*input  logic                  */  .valid_i        (inv_req_valid_muxed            ),
    /*output logic                  */  .ready_o        (inv_req_ready_muxed            ),
    /*output logic [NUM-1:0]        */  .valid_o        (inv_req_valid_o                ),
    /*input  logic [NUM-1:0]        */  .ready_i        (inv_req_ready_i                ) 
    );

    assign inv_req_idx_o        = inv_req_muxed.idx;
    assign inv_req_itype_o      = inv_req_muxed.itype;
    assign inv_req_dv_gv_o      = inv_req_muxed.dv_gv;
    assign inv_req_did_gscid_o  = inv_req_muxed.did_gscid;
    assign inv_req_pscv_o       = inv_req_muxed.pscv;
    assign inv_req_pid_pscid_o  = inv_req_muxed.pid_pscid;
    assign inv_req_av_o         = inv_req_muxed.av;
    assign inv_req_addr_o       = inv_req_muxed.addr;
//}}}


//}}}



//=== Inst {{{
genvar i;
generate
    for(i=0; i<FIFO_DEPTH; i++) begin : entry_inst_gen
        assign entry_inv_req_valid_i[i] = invalid_req_valid_i;
        assign entry_inv_req_i[i]       = {
                                            FIFO_IDX_WIDTH'(i)     ,
                                            invalid_req_type_i     ,
                                            invalid_req_dv_gv_i    ,
                                            invalid_req_did_gscid_i,
                                            invalid_req_pscv_i     ,
                                            invalid_req_pid_pscid_i,
                                            invalid_req_av_i       ,
                                            invalid_req_addr_i     
                                          };
        assign entry_inv_req_is_fence_i[i] = invalid_req_fence_i;
        always@(*) begin
            for(int unsigned ii=0; ii<FIFO_DEPTH; ii++) begin
                entry_depend_bits_i[i][ii] = (ii==i) ? 1'b0 : entry_depend_bit_o[ii];
                entry_valid_i      [i][ii] = (ii==i) ? 1'b0 : entry_valid_o[ii];
            end
        end
        assign entry_inv_tlbcache_ack_valid_i[i][0] = inv_ack_valid_i[0] & (inv_ack_idx_i[0]==FIFO_IDX_WIDTH'(i));
        assign entry_inv_tlbcache_ack_valid_i[i][1] = inv_ack_valid_i[1] & (inv_ack_idx_i[1]==FIFO_IDX_WIDTH'(i));
        assign entry_inv_tlbcache_ack_valid_i[i][2] = inv_ack_valid_i[2] & (inv_ack_idx_i[2]==FIFO_IDX_WIDTH'(i));
        assign entry_inv_tlbcache_ack_valid_i[i][3] = inv_ack_valid_i[3] & (inv_ack_idx_i[3]==FIFO_IDX_WIDTH'(i));
        
        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(FIFO_IDX_WIDTH)) U_entry_valid_check(
            .valid_i    (entry_valid_o              ),
            .update_i   (invalid_req_valid_i        ),
            .tag_i      (FIFO_IDX_WIDTH'(i)         ),
            .update_o   (entry_update_i[i]          )
        );
        
        iommu_atd_inv_entry #(
        /*parameter */ .FIFO_IDX_WIDTH             (FIFO_IDX_WIDTH             ), // = 4,
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = logic,
        /*parameter */ .SPARE_PARAM                (1'b0                       )  // = 0
        ) U_entry(                                                      
        /*input  logic                                      */  .clk                        (clk                                ),
        /*input  logic                                      */  .rstn                       (rstn                               ),
        /*input  logic                                      */  .update_i                   (entry_update_i                  [i]),
        /*input  logic                                      */  .inv_req_valid_i            (entry_inv_req_valid_i           [i]),
        /*input  INVALID_REQ_TYPE                           */  .inv_req_i                  (entry_inv_req_i                 [i]),
        /*input  logic                                      */  .inv_req_is_fence_i         (entry_inv_req_is_fence_i        [i]),
        /*output logic                                      */  .inv_tlbcache_req_valid_o   (entry_inv_tlbcache_req_valid_o  [i]),
        /*input  logic                                      */  .inv_tlbcache_req_ready_i   (entry_inv_tlbcache_req_ready_i  [i]),
        /*output INVALID_REQ_TYPE                           */  .inv_tlbcache_req_o         (entry_inv_tlbcache_req_o        [i]),
        /*input  logic [3:0]                                */  .inv_tlbcache_ack_valid_i   (entry_inv_tlbcache_ack_valid_i  [i]),
        /*output logic                                      */  .valid_o                    (entry_valid_o                   [i]),
        /*input  logic [INV_INFLY_NUM-1:0]                  */  .valid_i                    (entry_valid_i                   [i]),
        /*input  logic [INV_INFLY_NUM-1:0]                  */  .depend_bits_i              (entry_depend_bits_i             [i]),
        /*output logic                                      */  .depend_bit_o               (entry_depend_bit_o              [i]),
        /*input  logic                                      */  .spare_in                   (1'b0                               ) 
        );
    end
endgenerate

//}}}


endmodule
//}}}



/////////////////////////////////////////////////////////////////
// iommu_atd_inv_entry
/////////////////////////////////////////////////////////////////
module iommu_atd_inv_entry #( //{{{
    parameter  FIFO_IDX_WIDTH              = 4,
    parameter  INV_INFLY_NUM               = 2**FIFO_IDX_WIDTH,
    parameter type          INVALID_REQ_TYPE            = logic,
    parameter  SPARE_PARAM                 = 0
)(                                                      
//{{{ IO                                                
    input  logic                                        clk                     ,
    input  logic                                        rstn                    ,
    // UPDATE IF                                           
    input  logic                                        update_i                ,
    input  logic                                        inv_req_valid_i         ,
    input  INVALID_REQ_TYPE                             inv_req_i               ,
    input  logic                                        inv_req_is_fence_i      ,
    // TLB_CACHE                                        
    output logic                                        inv_tlbcache_req_valid_o,
    input  logic                                        inv_tlbcache_req_ready_i,
    output INVALID_REQ_TYPE                             inv_tlbcache_req_o      ,
    input  logic [3:0]                                  inv_tlbcache_ack_valid_i,
    //                                                  
    output logic                                        valid_o                 ,
    input  logic [INV_INFLY_NUM-1:0]                    valid_i                 ,
    input  logic [INV_INFLY_NUM-1:0]                    depend_bits_i           ,
    output logic                                        depend_bit_o            ,
    //                                                  
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    localparam S_IDLE                                   = 3'b000;
    localparam S_WAIT_CACHE                             = 3'b001;
    localparam S_WAIT_FENCE                             = 3'b101;
    localparam S_WAIT_ACK                               = 3'b110;
                                                        
    logic [2:0]                                         cs, ns;
                                                        
    logic                                               inv_req_is_fence_store;
                                                        
    logic [INV_INFLY_NUM-1:0]                           depend_status, depend_bits;

    logic [3:0]                                         inv_ack_got_flag;
//}}}

//=== MainCode {{{

//=== FSM Stage1 {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
//}}}

//=== FSM Stage2 {{{
    always@(*) begin
        case(cs)
        S_IDLE      : begin
            if(update_i) begin
                if(inv_req_is_fence_i)
                    ns = S_WAIT_FENCE;
                else
                    ns = S_WAIT_CACHE;
            end
            else
                ns = S_IDLE;
        end
        S_WAIT_CACHE: begin
            if(inv_tlbcache_req_ready_i)
                ns = S_WAIT_ACK;
            else
                ns = S_WAIT_CACHE;
        end
        S_WAIT_ACK: begin
            if(inv_ack_got_flag==4'b1111)
                ns = S_IDLE;
            else
                ns = S_WAIT_ACK;
        end
        S_WAIT_FENCE: begin
            if(depend_status == 'd0)
                ns = S_IDLE;
            else
                ns = S_WAIT_FENCE;
        end
        default: ns = S_IDLE;
        endcase
    end
//}}}

//=== FSM Stage3 {{{
//=== valid_o ===
    assign valid_o = (cs != S_IDLE);

//=== depend_bits {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            depend_bits <= 'b0;
        else if((ns != S_IDLE) && (cs == S_IDLE))
            depend_bits <= depend_bits_i;
        else begin
            for(int unsigned kk=0; kk<INV_INFLY_NUM; kk++) begin
                if(depend_bits[kk] & (~valid_i[kk]))
                    depend_bits[kk] <= 1'b0;
            end
        end
    end
    //assign depend_status = depend_bits & valid_i;
    assign depend_status = depend_bits;

//=== depend_bit_o
    assign depend_bit_o = inv_req_valid_i & valid_o & inv_req_is_fence_i;
//}}}

//=== REQ out
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_tlbcache_req_valid_o<= 1'b0;
            inv_tlbcache_req_o      <= 'd0;
        end
        else begin
            if(inv_tlbcache_req_valid_o & ~inv_tlbcache_req_ready_i)
                inv_tlbcache_req_valid_o <= 1'b1;
            else begin
                if(cs==S_IDLE & ns==S_WAIT_CACHE) begin
                    inv_tlbcache_req_valid_o<= 1'b1;
                    inv_tlbcache_req_o      <= inv_req_i;
                end
                else if(cs==S_WAIT_CACHE & ns==S_WAIT_ACK) begin
                    inv_tlbcache_req_valid_o<= 1'b0;
                end
            end
        end
    end

//=== ack_got_flag
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_ack_got_flag <= 4'b0000;
        end
        else begin
            if(cs!=S_WAIT_CACHE & ns==S_WAIT_CACHE)
                inv_ack_got_flag <= 4'b0000;
            else begin
                for(int unsigned a=0; a<4; a++) begin
                    if(inv_tlbcache_ack_valid_i[a])
                        inv_ack_got_flag[a] <= 1'b1;
                end
            end
        end
    end

//}}}

//}}}

endmodule
//}}}




