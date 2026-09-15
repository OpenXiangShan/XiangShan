// ////////////////////////////////////////////////////////////////////
// // iommu_acd_bus_handler_idx_queue
// ////////////////////////////////////////////////////////////////////
// module iommu_acd_bus_handler_idx_queue #( //{{{
//     parameter  IDX_WIDTH        = 3,
//     parameter  DEPTH            = 2**IDX_WIDTH,
//     parameter  SPARE_PARA       = 0
// )(
//     input  logic                    clk                 ,
//     input  logic                    rstn                ,
// 
//     input  logic                    push                ,
//     input  logic [IDX_WIDTH-1:0]    widx                ,
//     output logic                    wfull               ,
// 
//     input  logic [DEPTH-1:0]        pop                 ,
//     input  logic [IDX_WIDTH-1:0]    ridx[DEPTH-1:0]     ,
//     output logic                    rempty              ,
// 
//     output logic [IDX_WIDTH-1:0]    idx                 ,
// 
//     input  logic                    spare_in             
// );
// //=== Declare === {{{
//     typedef logic [IDX_WIDTH-1:0]   buf_idx_t;
//     buf_idx_t [DEPTH-1:0]           fifo;
//     buf_idx_t                       push_idx;
//     buf_idx_t                       push_ptr;
//     logic                           push_en;
// 
//     typedef struct packed {
//         buf_idx_t idx;
//         logic     v;
//     } idxfifo_t;
//     
//     idxfifo_t [DEPTH-1:0]           idxfifo;
//     idxfifo_t [DEPTH-1:0]           idxfifo_n;
// 
//     logic [DEPTH-1:0]               pop_idx_match_list;
//     logic [IDX_WIDTH-1:0]           pop_idx_match_cnt_list[DEPTH-1:0];
// //}}}
// 
// //=== Main Code === {{{
//     assign rempty = 'd0;
//     assign wfull  = 'd0;
//     assign idx = idxfifo[0].v ? idxfifo[0].idx : 'b0;
// 
//     always@(*) begin
//         push_idx = 'd0;
//         push_ptr = 'd0;
//         push_en  = 'd0;
//         for(int unsigned i=0; i<DEPTH; i++) begin
//             if(push) begin
//                 if(idxfifo[DEPTH-1-i].v==1'b0) begin
//                     push_idx = widx;
//                     push_ptr = 7-i;
//                     push_en  = 1'b1;
//                 end
//             end
//         end
//     end
// 
//     always@(*) begin
//         if(push_ptr=='d0 && push_en=='d1) begin
//             idxfifo_n[0] = {push_idx, 1'b1};
//         end
//         else if(pop_idx_match_list[0]=='b1) begin
//             idxfifo_n[0] = {idxfifo[0].idx, 1'b0};
//         end
//         else begin
//             idxfifo_n[0] = idxfifo[0];
//         end
//     
//         for(int unsigned k=1; k<DEPTH; k++) begin
//             logic [DEPTH-1:0]               pop_idx_match_list_sr;
//             buf_idx_t                       tmp_ptr;
//             pop_idx_match_list_sr = pop_idx_match_list >> (8-k);
//             tmp_ptr = k - pop_idx_match_cnt_list[k];
//             if(k==push_ptr && push_en==1'b1)        // fresh push to this cell
//                 idxfifo_n[tmp_ptr] = {push_idx, 1'b1};
//             else
//                 idxfifo_n[tmp_ptr] = idxfifo[k];
//         end
//     end
// 
//     always@(posedge clk or negedge rstn)begin
//         if(~rstn) begin
//             for(int unsigned kn=0; kn<DEPTH; kn++) begin
//                 idxfifo[kn] <= {{IDX_WIDTH{1'b0}}, 1'b0};
//             end
//         end
//         else begin
//             idxfifo <= idxfifo_n;
//         end
//     end
// 
// genvar ki;
// generate
//     for(ki=0; ki<DEPTH; ki++) begin
//         pop_idx_match     U_pop_idx_match    (pop, ridx,          idxfifo[ki].idx,   pop_idx_match_list[ki]);
//         pop_idx_match_cnt U_pop_idx_match_cnt(pop_idx_match_list, ki[IDX_WIDTH-1:0], pop_idx_match_cnt_list[ki]);
//     end
// endgenerate
// //}}}
// 
// endmodule
// //}}}

///////////////////////////////////////////////////
module pop_idx_match #( //{{{
    parameter  IDX_WIDTH        = 3,
    parameter  DEPTH            = 2**IDX_WIDTH,
    parameter  SPARE_PARA       = 0
)(
    input  logic [DEPTH-1:0]                pop,
    input  logic [IDX_WIDTH-1:0]            ridx[DEPTH-1:0],
    input  logic [IDX_WIDTH-1:0]            idx,
    output logic                            match
);
    always@(*) begin
        match = 1'b0;
        for(int unsigned i=0; i<DEPTH; i++)begin
            if(ridx[i]==idx && pop[i]==1'b1)
                match = 1'b1;
        end
    end
endmodule
//}}}

///////////////////////////////////////////////////
module pop_idx_match_cnt #( //{{{
    parameter  IDX_WIDTH        = 3,
    parameter  DEPTH            = 2**IDX_WIDTH,
    parameter  SPARE_PARA       = 0
)(
    input  logic [DEPTH-1:0]                pop_idx_match_list,
    input  logic [IDX_WIDTH-1:0]            idx,
    output logic [IDX_WIDTH-1:0]            cnt
);
    logic [DEPTH-1:0] pop_idx_match_list_masked;
    always@(*)begin
        for(int unsigned i=0; i<DEPTH; i++) begin
            if(i>=idx)
                pop_idx_match_list_masked[i] = 1'b0;
            else
                pop_idx_match_list_masked[i] = pop_idx_match_list[i];
        end
    end
    
    logic [IDX_WIDTH-1:0] cntq[DEPTH-1:0];
    assign cntq[0] = pop_idx_match_list_masked[0] ? 3'd1 : 3'd0;
    always@(*) begin
        for(int unsigned k=1; k<DEPTH; k++)begin
            cntq[k] = pop_idx_match_list_masked[k] ? (cntq[k-1] + 3'd1) : cntq[k-1];
        end
    end
    assign cnt = cntq[7];

endmodule
//}}}



////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_sync_fifo
// synchrous fifo
////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_sync_fifo #( //{{{
    parameter  FIFOTYPE         = 0,    // 0:FF 1:RAM
    parameter  WIDTH            = 128,
    parameter  DEPTH            = 32,

    parameter  SPARE_PARA       = 0
)(
    input  logic                            clk         ,
    input  logic                            rstn        ,

    input  logic                            push_i      ,
    output logic                            full_o      ,
    output logic                            afull_o     , // almost full
    input  logic [WIDTH-1:0]                wdata_i     ,

    input  logic                            pop_i       ,
    output logic                            empty_o     ,
    output logic                            aempty_o    ,
    output logic [WIDTH-1:0]                rdata_o     ,

    input  logic                            spare_in     
);
//=== Declare === {{{
    localparam PTR_WIDTH       = $clog2(DEPTH);
    logic [WIDTH-1:0]                       fifo [DEPTH-1:0];
    logic [PTR_WIDTH:0]                     wptr, rptr;
    logic                                   empty_int;
    logic                                   wr, rd, ram_rd, ram_rd_ff;
    logic [WIDTH-1:0]                       ram_rdata_o;
    logic [WIDTH-1:0]                       ram_rdata_f1, ram_rdata_f2;
    logic                                   ram_rdata_v1, ram_rdata_v2;
    logic                                   ram_rdata_wsel,ram_rdata_rsel;

//}}}

//=== Main code === {{{
generate
    if(DEPTH=='d1) begin : depth_1
        assign full_o  = (wptr!=rptr);
        assign afull_o = 1'b1;
        assign empty_o = (wptr == rptr);
        assign aempty_o= 1'b1;
    end
    else begin
        if(FIFOTYPE=='d0) begin : ff_type
            assign full_o  = ((wptr[PTR_WIDTH-1:0] == rptr[PTR_WIDTH-1:0]) & (wptr[PTR_WIDTH] != rptr[PTR_WIDTH]));
            assign afull_o = (((wptr[PTR_WIDTH-1:0] + (PTR_WIDTH-1)'('d2)) == rptr[PTR_WIDTH-1:0]) & (wptr[PTR_WIDTH] != rptr[PTR_WIDTH]));
            assign empty_o = (wptr == rptr);
            assign aempty_o= (((wptr[PTR_WIDTH-1:0] - (PTR_WIDTH-1)'('d2)) == rptr[PTR_WIDTH-1:0]) & (wptr[PTR_WIDTH] == rptr[PTR_WIDTH]));
            assign empty_int= 1'b0;
        end
        else begin : dpram_type
            assign full_o   = ((wptr[PTR_WIDTH-1:0] == rptr[PTR_WIDTH-1:0]) & (wptr[PTR_WIDTH] != rptr[PTR_WIDTH]));
            assign afull_o  = (((wptr[PTR_WIDTH-1:0] + (PTR_WIDTH-1)'('d2)) == rptr[PTR_WIDTH-1:0]) & (wptr[PTR_WIDTH] != rptr[PTR_WIDTH]));
            assign empty_int= (wptr == rptr);
            assign empty_o  = ~(ram_rdata_v1 | ram_rdata_v2);
            assign aempty_o = ~(ram_rdata_v1 & ram_rdata_v2);
        end
    end
endgenerate

    assign wr       = push_i & ~full_o;
    assign rd       = pop_i & ~empty_o;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            wptr <= 'd0;
        else if(wr)
            wptr <= wptr + 'd1;
    end

generate
    if(FIFOTYPE=='d0) begin : rptr_gen_ff_type
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                rptr <= 'd0;
            else if(rd)
                rptr <= rptr + 'd1;
        end
        assign ram_rd = 1'b0;
        assign ram_rd_ff = 1'b0;
        assign ram_rdata_rsel = 1'b0;
        assign ram_rdata_wsel = 1'b0;
        assign ram_rdata_v1 = 'd0;
        assign ram_rdata_v2 = 'd0;
        assign ram_rdata_f1 = 'd0;
        assign ram_rdata_f2 = 'd0;
    end
    else begin : rptr_gen_dpram_type
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                rptr <= 'd0;
            else begin
                if(ram_rd)
                    rptr <= rptr + 'd1;
            end
        end
    
        assign ram_rd = ~empty_int &
                        (
                            ram_rd_ff ? (~ram_rdata_v1 & ~ram_rdata_v2) :
                                        (~ram_rdata_v1 | ~ram_rdata_v2)
                         );
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                ram_rd_ff <= 'd0;
            else
                ram_rd_ff <= ram_rd;
        end
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                ram_rdata_wsel<= 'd0;
            else begin
                if(ram_rd_ff)
                    ram_rdata_wsel <= ~ram_rdata_wsel;
            end
        end
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                ram_rdata_rsel <= 'd0;
            else begin
                if(rd)
                    ram_rdata_rsel <= ~ram_rdata_rsel;
            end
        end
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                ram_rdata_f1 <= 'd0;
                ram_rdata_v1 <= 'd0;
            end
            else begin
                if(rd & ram_rdata_rsel=='d0) begin
                    ram_rdata_v1 <= 1'b0;
                end
                else if(ram_rd_ff & ram_rdata_wsel=='d0) begin
                    ram_rdata_v1 <= 1'b1;
                    ram_rdata_f1 <= ram_rdata_o;
                end
            end
        end
    
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                ram_rdata_f2 <= 'd0;
                ram_rdata_v2 <= 'd0;
            end
            else begin
                if(rd & ram_rdata_rsel=='d1) begin
                    ram_rdata_v2 <= 1'b0;
                end
                else if(ram_rd_ff & ram_rdata_wsel=='d1) begin
                    ram_rdata_v2 <= 1'b1;
                    ram_rdata_f2 <= ram_rdata_o;
                end
            end
        end
    
    end
endgenerate

generate
    if(DEPTH=='d1) begin : fifo_depth_1
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                for(int unsigned k = 0; k < DEPTH; k++) begin
                    fifo[k] <= 'd0;
                end
            end
            else begin
                if(push_i & (~full_o)) begin
                    fifo[0] <= wdata_i;
                end
            end
        end
        assign rdata_o = fifo[0];
    end
    else begin
        if(FIFOTYPE==0) begin : fifo_gen_ff_type
            always@(posedge clk or negedge rstn) begin
                if(~rstn) begin
                    for(int unsigned k = 0; k < DEPTH; k++) begin
                        fifo[k] <= 'd0;
                    end
                end
                else begin
                    if(push_i & (~full_o)) begin
                        fifo[wptr[PTR_WIDTH-1:0]] <= wdata_i;
                    end
                end
            end
            assign rdata_o = fifo[rptr[PTR_WIDTH-1:0]];
        end
        else begin : fifo_gen_dpram_type
            assign rdata_o = ram_rdata_rsel ? ram_rdata_f2 : ram_rdata_f1;
    
            iommu_acd_wbuf_ram_wrap #(
            /*parameter int  unsigned */ .AWIDTH    (PTR_WIDTH      ), // = 4,
            /*parameter int  unsigned */ .DWIDTH    (WIDTH          )  // = 128
            ) U_fdata(
            /*input  logic                      */  .CLK            (clk                    ),
            /*input  logic                      */  .WEA            (~wr                    ),
            /*input  logic                      */  .MEA            (~wr                    ),
            /*input  logic [AWIDTH-1:0]         */  .ADDRA          (wptr[PTR_WIDTH-1:0]    ),
            /*input  logic [DWIDTH-1:0]         */  .DINA           (wdata_i                ),
            /*input  logic                      */  .MEB            (~ram_rd                ),
            /*input  logic [AWIDTH-1:0]         */  .ADDRB          (rptr[PTR_WIDTH-1:0]    ),
            /*output logic [DWIDTH-1:0]         */  .DOUTB          (ram_rdata_o            ) 
            );
        end
    end
endgenerate
//}}}

endmodule //}}}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// iommu_acd_bus_hander_idx_fifo
// fifo that supports 
//     1. stored data targeted with a idx
//     2. pop data with idx matching
//     3. timely data ready flag indicates if data stored with idx==N with data_ready_o[N]
//
// used for W.DATA buffer and sequence control 
//
// all input wdata should coupled with a widx_i
// wdata coupled with same widx should input continuously, no id interleave support
//
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_idx_fifo #( //{{{
    parameter  FIFOTYPE         = 1'b0, // 0:FF 1:RAM
    parameter  DEPTH            = 32,
    parameter  WIDTH            = 64,
    parameter  IDX_WIDTH        = 3,
    parameter  IDX_NUM          = 2**IDX_WIDTH,

    parameter  SPARE_PARA       = 0
)(
    input  logic                            clk             ,
    input  logic                            rstn            ,

    input  logic                            push_i          ,
    output logic                            full_o          ,
    output logic                            afull_o         ,
    input  logic [WIDTH-1:0]                wdata_i         ,
    input  logic [IDX_WIDTH-1:0]            widx_i          ,
    input  logic                            wlast_i         ,

    input  logic                            pop_i           ,
    output logic                            empty_o         ,
    output logic                            aempty_o        ,
    output logic [WIDTH-1:0]                rdata_o         ,
    input  logic [IDX_WIDTH-1:0]            ridx_i          ,

    output logic [IDX_NUM-1:0]              data_ready_o    ,

    input  logic                            spare_in         
);
//=== Declare === {{{
    localparam PTR_WIDTH                    = $clog2(DEPTH);

    typedef struct packed {
        logic [PTR_WIDTH-1:0]               ptr;    // linked-list-pointer to the next data element
        logic                               s;      // the start of the data sqeuence,(the first data)
        logic [IDX_WIDTH-1:0]               fidx;
        logic                               v;
        logic [WIDTH-1:0]                   fdata;
    } fifo_t;

    fifo_t [DEPTH-1:0] fifo;

    logic [PTR_WIDTH-1:0]                   wptr, wnum, nxt_wptr, rptr, nxt_rptr;
    logic                                   write, read;

    logic                                   newstart;   // indicates that a new wdata_in sequence start

    logic                                   got_start_element;
    logic [DEPTH-1:0]                       got_start_element_mux;
    logic [PTR_WIDTH-1:0]                   got_start_ptr;

    logic [WIDTH-1:0]                       rdata_mux;
    logic                                   read_ff;
    logic [WIDTH-1:0]                       rdata_mux_ff;

//}}}

//=== data_ready_o === {{{
    always@(*) begin
        data_ready_o = 'd0;
        for(int unsigned v = 0; v < IDX_NUM; v++) begin
            for(int unsigned w = 0; w < DEPTH; w++) begin
                if((fifo[w].fidx == v) & (fifo[w].v == 1'b1))
                    data_ready_o[v] = 1'b1;
            end
        end
    end
//}}}

//=== fifo cnt === {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            wnum <= 'd0;
        else begin
            case({write, read})
            2'b00: wnum <= wnum;
            2'b01: wnum <= wnum-1;
            2'b10: wnum <= wnum+1;
            2'b11: wnum <= wnum;
            default:wnum <= wnum;
            endcase
        end
    end

    assign full_o   = (wnum == DEPTH-'d1);
    assign empty_o  = (wnum == 'd0);
    assign afull_o  = (wnum == DEPTH-'d2);
    assign aempty_o = (wnum == 'd2);
    assign write    = push_i & (~full_o);
    assign read     = pop_i  & (~empty_o);
//}}}

//=== WR === {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
//            for(int unsigned u=0; u<DEPTH; u++) begin
//                fifo[u] <= 'd0;
//            end
            wptr <= 'd0;
        end
        else begin
            if(write) begin
// fifo dirver in multiple always block, unsynthesizable, 20241107
//                fifo[wptr].fdata    <= wdata_i;
//                fifo[wptr].fidx     <= widx_i;
//                fifo[wptr].v        <= 1'b1;
                wptr                <= nxt_wptr;
            end
        end
    end

    logic [DEPTH-1:0]       tmp_v_list0;
    logic [PTR_WIDTH:0]     q_add_wptr;
    logic [PTR_WIDTH-1:0]   tmp_v_list0_idx;
    always@(*) begin
        for(int unsigned q=0; q<DEPTH; q++) begin
            q_add_wptr      = q + wptr;
            tmp_v_list0_idx = PTR_WIDTH'(q_add_wptr);
            tmp_v_list0[q]  = fifo[tmp_v_list0_idx].v;
        end
    end
    always@(*) begin
        nxt_wptr = wptr;
        for(int unsigned Q=DEPTH-1; Q>0; Q--) begin
            if(tmp_v_list0[Q] == 1'b0)
                nxt_wptr = PTR_WIDTH'(Q + wptr);
        end
    end
//}}}

//=== fifo === {{{
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        newstart <= 1'b1;
    else begin
        if(write) begin
            if(newstart & ~wlast_i)
                newstart <= 1'b0;
            else if(~newstart & wlast_i)
                newstart <= 1'b1;
        end
    end
end

genvar i;
generate
    if(FIFOTYPE==1'b0) begin : ff_type_gen
        for(i=0; i<DEPTH; i++) begin : fifo_gen
            always@(posedge clk or negedge rstn) begin
                if(~rstn)
                    fifo[i] <= 'd0;
                else begin
                    if(read & nxt_rptr==i) begin
                        fifo[i].v <= 1'b0;
                    end
                    else if(write & wptr==i) begin
                        fifo[i].fdata   <= wdata_i;
                        fifo[i].fidx    <= widx_i;
                        fifo[i].v       <= 1'b1;
                        fifo[i].s       <= newstart;
                        fifo[i].ptr     <= nxt_wptr;
                    end
                end
            end
        end
    end
    else begin : ram_type_gen
        for(i=0; i<DEPTH; i++) begin : fifo_gen
            always@(posedge clk or negedge rstn) begin
                if(~rstn)
                    fifo[i] <= 'd0;
                else begin
                    fifo[i].fdata <= 'd0;
                    if(read & nxt_rptr==i) begin
                        fifo[i].v <= 1'b0;
                    end
                    else if(write & wptr==i) begin
//                        fifo[i].fdata   <= wdata_i;
                        fifo[i].fidx    <= widx_i;
                        fifo[i].v       <= 1'b1;
                        fifo[i].s       <= newstart;
                        fifo[i].ptr     <= nxt_wptr;
                    end
                end
            end
        end

        iommu_acd_wbuf_ram_wrap #(
        /*parameter int  unsigned */ .AWIDTH    (PTR_WIDTH      ), // = 4,
        /*parameter int  unsigned */ .DWIDTH    (WIDTH          )  // = 128
        ) U_fdata(
        /*input  logic                      */  .CLK            (clk            ),
        /*input  logic                      */  .WEA            (~write         ),
        /*input  logic                      */  .MEA            (~write         ),
        /*input  logic [AWIDTH-1:0]         */  .ADDRA          (wptr           ),
        /*input  logic [DWIDTH-1:0]         */  .DINA           (wdata_i        ),
        /*input  logic                      */  .MEB            (~read          ),
        /*input  logic [AWIDTH-1:0]         */  .ADDRB          (nxt_rptr       ),
        /*output logic [DWIDTH-1:0]         */  .DOUTB          (rdata_mux      ) 
        );

    end
endgenerate
//}}}

//=== RD === {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            rptr            <= 'd0;
        else if(read) begin
// bug fix, RD use link-list-pointer for data selection by now, 20241112
//            rptr            <= nxt_rptr;
            rptr <= fifo[nxt_rptr].ptr;
// fifo dirver in multiple always block, unsynthesizable, 20241107
//            fifo[nxt_rptr].v<= 1'b0;
        end
    end

// bug fix, RD use link-list-pointer for data selection by now, 20241112
//    logic [DEPTH:0]       tmp_rmatch_list;
//    logic [PTR_WIDTH-1:0] tmp_rmatch_ptr;
//    always@(*) begin
//        tmp_rmatch_list = 'd0;
//        tmp_rmatch_ptr  = rptr;
//        for(int unsigned p=0; p<DEPTH; p++) begin
//            //tmp_rmatch_list[p] = fifo[p + rptr].v & (fifo[p + rptr].fidx == ridx_i);
//            tmp_rmatch_ptr = PTR_WIDTH'(p+rptr);
//            //if(fifo[p + rptr].v & (fifo[p + rptr].fidx == ridx_i))
//            if(fifo[tmp_rmatch_ptr].v & (fifo[tmp_rmatch_ptr].fidx == ridx_i))
//                tmp_rmatch_list[p] = 1'b1;
//            else
//                tmp_rmatch_list[p] = 1'b0;
//        end
//    end
//    logic [DEPTH:0]         tmp_rmatch_list_sr;
//    assign tmp_rmatch_list_sr = {tmp_rmatch_list[DEPTH-1:0], 1'b0};
//    always@(*) begin
//        nxt_rptr = 'd0;
//        for(int unsigned P=DEPTH; P>0; P--) begin
//            if((tmp_rmatch_list[P-1] == 1'b1) && (tmp_rmatch_list_sr[P-1] == 1'b0))
//                nxt_rptr = PTR_WIDTH'(P-1 + rptr);
//        end
//    end

genvar gsp;
generate
//    always@(*) begin
//        got_start_element = 1'b0;
//        got_start_ptr     = 'd0;
//        for(int unsigned p=0; p<DEPTH; p++) begin
//            if(fifo[p].v & fifo[p].s & fifo[p].fidx==ridx_i) begin
//                got_start_element = 1'b1;
//                got_start_ptr     = PTR_WIDTH'(p);
//            end
//        end
//    end
    for(gsp=0; gsp<DEPTH; gsp++) begin : got_start_gen
        assign got_start_element_mux[gsp] = (fifo[gsp].v & fifo[gsp].s & fifo[gsp].fidx==ridx_i);
    end
endgenerate
    assign got_start_element = |got_start_element_mux;
    always@(*) begin
        got_start_ptr = 'd0;
        for(int unsigned i=0; i<DEPTH; i++) begin
            if(got_start_element_mux[i]) begin
                got_start_ptr = PTR_WIDTH'(i);
            end
        end
    end

    assign nxt_rptr = got_start_element ? got_start_ptr : rptr;

generate
    if(FIFOTYPE==1'b0) begin : ff_type_rdata_o_gen
        assign rdata_mux_ff = 'd0;
        assign rdata_mux = fifo[nxt_rptr].fdata;
        assign rdata_o   = rdata_mux;
    end
    else begin : ram_type_rdata_o_gen
        always@(posedge clk or negedge rstn) begin
            if(!rstn) begin
                read_ff     <= 1'b0;
                rdata_mux_ff<= 'd0;
            end
            else begin
                read_ff     <= read;

                if(read_ff)
                    rdata_mux_ff<= rdata_mux;
            end
        end
        assign rdata_o   = read_ff ? rdata_mux : rdata_mux_ff;
    end
endgenerate
//}}}
endmodule
//}}}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_id_queue
// a id&data couple fifo, which supports id targeted pop at any fifo location
// data with same id will always poped with the original input sequence
// use for AXI B or R different ID out-of-order handler
// 
// all input data (wdata) has a coupled id (wid) and stored with the original input sqeuence
// 
// idxfifo  [0]   |   idxfifo[1]  |   idxfifo[2]  |   ....    |   idxfifo[DEPTH-1]
// .id      id0         id1             id2             id...
// .dat     data0       data1           data2           data...
// .v       1           1               1               1
//
// always outputs idxfifo[0]'s id and dat, indicates the oldest id&data
// 
// can pop data in any idxfifo[?] with id matchin
// but only output the oldest match idxfifo[?].dat
// for example, if id1 == id2 == id3 == ID, and rid == ID, idxfifo[1].dat will output at rdata
// 
// always keep the original input sequence in the queue, no bubble between idxfifo[?]
// as the example upper, after the pop, the stored idxfifos change as
//
// idxfifo  [0]   |   idxfifo[1]  |   idxfifo[2]  |   ....    |   idxfifo[DEPTH-1]
// .id      id0         id2             id3             id...
// .dat     data0       data2           data3           data...
// .v       1           1               1               1
//
// the id2&dat2 will be moved forward, so no bubble in idxfifo[1], all following idxfifo[?] also moved forward
// 
// when a new id&data inputs, always stored in the tail idxfifo[?]
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_id_queue #( //{{{
    parameter  ID_WIDTH         = 8,                // ID width
    parameter  IDX_WIDTH        = 3,                // queue depth idx width
    parameter  DEPTH            = 2**IDX_WIDTH,     // queue depth
    parameter  DATA_WIDTH       = 8,                // DATA width
    parameter  SPARE_PARA       = 0                 // NO USE
)(
    input  logic                            clk     ,
    input  logic                            rstn    ,

    input  logic                            push    ,           // pulse to push wid&wdata couple in, wid&wdata can only stored when push==1 and wfull==0, so if wfull==0, outside dirver should maintain the wid&wdata, otherwise will be dropped
    input  logic [ID_WIDTH-1:0]             wid     ,           //
    input  logic [DATA_WIDTH-1:0]           wdata   ,
    output logic                            wfull   ,           // indicates if all queue unit are occupied by a wid&wdata couple

    input  logic                            pop     ,           // pulse to pop a wdata that the coupled wid matched with rid, 
    input  logic [ID_WIDTH-1:0]             rid     ,
    output logic [DATA_WIDTH-1:0]           rdata   ,
    output logic                            rvalid  ,           // synced with rid (not pop), indicates if any valid wdata stored in the queue that coupled wid matched with rid
    output logic                            rempty  ,           // indicates if any wid&wdata stored in the queue

    output logic [ID_WIDTH-1:0]             ido     ,           // the oldest id output
    output logic [DATA_WIDTH-1:0]           datao   ,           // the oldest data output

    input  logic                            spare_in 
);
//=== Declare === {{{
    logic                           dopush, dopop;
    logic [DATA_WIDTH-1:0]          push_dat;
    logic [ID_WIDTH-1:0]            push_id;
    logic [IDX_WIDTH-1:0]           push_ptr;
    logic                           push_en;

    typedef struct packed {
        logic                       v;
        logic [ID_WIDTH-1:0]        id;
        logic [DATA_WIDTH-1:0]      dat;
    } idxfifo_t;

    idxfifo_t [DEPTH-1:0]           idxfifo;                    // store unit array
    idxfifo_t [DEPTH-1:0]           idxfifo_n;                  // 

    logic [IDX_WIDTH-1:0]           pop_idx_match_position;
//}}}
    assign dopush   = push & (~wfull);                          // real store if push and non full
    assign dopop    = pop & (~rempty) & rvalid;                 // real pop if pop and non empty and do has a matching couple

    assign wfull    = idxfifo[DEPTH-1].v ? 1'b1 : 1'b0;
    assign rempty   = idxfifo[0].v ? 1'b0 : 1'b1;
    assign rdata    = idxfifo[pop_idx_match_position].dat;

    assign ido      = idxfifo[0].id;
    assign datao    = idxfifo[0].dat;

    always@(*) begin                                                                    // always find the valid matching unit with rid
        pop_idx_match_position  = 'd0;
        rvalid                  = 'd0;
        for(int unsigned lk=0; lk<DEPTH; lk++) begin
            if((idxfifo[DEPTH-1-lk].v==1'b1) & (idxfifo[DEPTH-1-lk].id==rid)) begin
                pop_idx_match_position  = IDX_WIDTH'(DEPTH-1-lk);
                rvalid                  = 'b1;
            end
        end
    end

    always@(*) begin                                                                    // always find the tail unit for wid&wdata store
        push_id  = 'd0;
        push_ptr = 'd0;
        push_en  = 'd0;
        push_dat = 'd0;
        for(int unsigned i=0; i<DEPTH; i++) begin
            if(dopush & (idxfifo[DEPTH-1-i].v==1'b0)) begin
                    push_id  = wid;
                    push_ptr = IDX_WIDTH'(DEPTH-1-i);
                    push_en  = 1'b1;
                    push_dat = wdata;
            end
        end
    end

    always@(*) begin                                                                    // idxfifo update
        for(int unsigned i=0; i<DEPTH; i++) begin
            if(~dopop) begin                                                            // NO-POP
                if(i==push_ptr && push_en==1'b1) begin                                  // but has a push in this cycle, just store the wid&wdat in the tail unit
                    idxfifo_n[i].id  = push_id;
                    idxfifo_n[i].dat = push_dat;
                    idxfifo_n[i].v   = 1'b1;
                end
                else                                                                    // other non-tail unit just keep its content
                    idxfifo_n[i]     = idxfifo[i];
            end
            else begin                                                                  // POP
                if(i<pop_idx_match_position)                                            // for unit that in front of matching unit, just keep its content
                    idxfifo_n[i] = idxfifo[i];
                else if(i>=pop_idx_match_position && i<DEPTH-1) begin                   // for unit that not in front of matching unit, move forward on step
                    if(i+1==push_ptr && push_en==1'b1) begin                            // if idxfifo[N] is current tail, should stored one step forward in idxfifo[N-1] as a POP is ongoing, otherwise a bubble generates
                        idxfifo_n[i].id  = push_id;
                        idxfifo_n[i].dat = push_dat;
                        idxfifo_n[i].v   = 1'b1;
                    end
                    else
                        idxfifo_n[i]     = idxfifo[i+1];                                // for the unit that in front of the last but one, keep its content
                end
                else begin                                                              // idxfifo[N-1] always be invalided when POP
                    idxfifo_n[i].id      = 'd0;
                    idxfifo_n[i].dat     = 'd0;
                    idxfifo_n[i].v       = 'b0;
                end
            end
        end
    end

    always@(posedge clk or negedge rstn)begin                                           // update idxfifo contents
        if(~rstn) begin
            for(int unsigned kn=0; kn<DEPTH; kn++) begin
                idxfifo[kn] <= {{IDX_WIDTH{1'b0}}, 1'b0};
            end
        end
        else begin
            idxfifo <= idxfifo_n;
        end
    end


endmodule
//}}}



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//     
//    module iommu_acd_bus_handler_sync_fifo_dpramtype #( //{{{
//        parameter  WIDTH            = 128,
//        parameter  DEPTH            = 32,   // should not be 1
//    
//        parameter  SPARE_PARA       = 0
//    )(
//        input  logic                            clk         ,
//        input  logic                            rstn        ,
//    
//        input  logic                            push_i      ,
//        output logic                            full_o      ,
//        output logic                            afull_o     , // almost full
//        input  logic [WIDTH-1:0]                wdata_i     ,
//    
//        input  logic                            pop_i       ,
//        output logic                            empty_o     ,
//        output logic                            aempty_o    ,
//        output logic [WIDTH-1:0]                rdata_o     ,
//    
//        input  logic                            spare_in     
//    );
//    //=== Declare === {{{
//        localparam PTR_WIDTH       = $clog2(DEPTH);
//        logic [PTR_WIDTH:0]                     wptr, wptr_ff, rptr;
//        logic                                   empty_int;
//        logic                                   wr, rd, ram_rd, ram_rd_ff;
//        logic [WIDTH-1:0]                       ram_rdata_o;
//        logic [WIDTH-1:0]                       ram_rdata_f1, ram_rdata_f2;
//        logic                                   ram_rdata_v1, ram_rdata_v2;
//        logic                                   ram_rdata_wsel,ram_rdata_rsel;
//    
//    //}}}
//    
//    //=== Main code === {{{
//        assign full_o   = ((wptr[PTR_WIDTH-1:0] == rptr[PTR_WIDTH-1:0]) & (wptr[PTR_WIDTH] != rptr[PTR_WIDTH]));
//        assign afull_o  = (((wptr[PTR_WIDTH-1:0] + (PTR_WIDTH-1)'('d2)) == rptr[PTR_WIDTH-1:0]) & (wptr[PTR_WIDTH] != rptr[PTR_WIDTH]));
//    //    assign empty_int= (wptr_ff == rptr);
//        assign empty_int= (wptr == rptr);
//    
//        assign empty_o  = ~(ram_rdata_v1 | ram_rdata_v2);
//        assign aempty_o = ~(ram_rdata_v1 & ram_rdata_v2);
//    
//        assign wr       = push_i & ~full_o;
//        assign rd       = pop_i & ~empty_o;
//    
//        always@(posedge clk or negedge rstn) begin
//            if(~rstn) begin
//                wptr    <= 'd0;
//                wptr_ff <= 'd0;
//            end
//            else begin
//                wptr_ff <= wptr;
//                if(wr)
//                    wptr<= wptr + 'd1;
//            end
//        end
//    
//        always@(posedge clk or negedge rstn) begin
//            if(~rstn)
//                rptr <= 'd0;
//            else begin
//                if(ram_rd)
//                    rptr <= rptr + 'd1;
//            end
//        end
//    
//        assign ram_rd = ~empty_int &
//                        (
//                            ram_rd_ff ? (~ram_rdata_v1 & ~ram_rdata_v2) :
//                                        (~ram_rdata_v1 | ~ram_rdata_v2)
//                         );
//        always@(posedge clk or negedge rstn) begin
//            if(~rstn)
//                ram_rd_ff <= 'd0;
//            else
//                ram_rd_ff <= ram_rd;
//        end
//    
//        always@(posedge clk or negedge rstn) begin
//            if(~rstn)
//                ram_rdata_wsel<= 'd0;
//            else begin
//                if(ram_rd_ff)
//                    ram_rdata_wsel <= ~ram_rdata_wsel;
//            end
//        end
//    
//        always@(posedge clk or negedge rstn) begin
//            if(~rstn)
//                ram_rdata_rsel <= 'd0;
//            else begin
//                if(rd)
//                    ram_rdata_rsel <= ~ram_rdata_rsel;
//            end
//        end
//    
//        always@(posedge clk or negedge rstn) begin
//            if(~rstn) begin
//                ram_rdata_f1 <= 'd0;
//                ram_rdata_v1 <= 'd0;
//            end
//            else begin
//                if(rd & ram_rdata_rsel=='d0) begin
//                    ram_rdata_v1 <= 1'b0;
//                end
//                else if(ram_rd_ff & ram_rdata_wsel=='d0) begin
//                    ram_rdata_v1 <= 1'b1;
//                    ram_rdata_f1 <= ram_rdata_o;
//                end
//            end
//        end
//    
//        always@(posedge clk or negedge rstn) begin
//            if(~rstn) begin
//                ram_rdata_f2 <= 'd0;
//                ram_rdata_v2 <= 'd0;
//            end
//            else begin
//                if(rd & ram_rdata_rsel=='d1) begin
//                    ram_rdata_v2 <= 1'b0;
//                end
//                else if(ram_rd_ff & ram_rdata_wsel=='d1) begin
//                    ram_rdata_v2 <= 1'b1;
//                    ram_rdata_f2 <= ram_rdata_o;
//                end
//            end
//        end
//    
//        assign rdata_o = ram_rdata_rsel ? ram_rdata_f2 : ram_rdata_f1;
//    
//        iommu_acd_sdpram_model #(
//        /*parameter int  unsigned */ .AWIDTH    (PTR_WIDTH      ), // = 4,
//        /*parameter int  unsigned */ .DWIDTH    (WIDTH          )  // = 128
//        ) U_fdata(
//        /*input  logic                      */  .clk            (clk                    ),
//        /*input  logic                      */  .wea            (~wr                    ),
//        /*input  logic                      */  .mea            (~wr                    ),
//        /*input  logic [AWIDTH-1:0]         */  .addra          (wptr[PTR_WIDTH-1:0]    ),
//        /*input  logic [DWIDTH-1:0]         */  .dina           (wdata_i                ),
//        /*input  logic                      */  .meb            (~ram_rd                ),
//        /*input  logic [AWIDTH-1:0]         */  .addrb          (rptr[PTR_WIDTH-1:0]    ),
//        /*output logic [DWIDTH-1:0]         */  .doutb          (ram_rdata_o            ) 
//        );
//    //}}}
//    
//    endmodule //}}}

