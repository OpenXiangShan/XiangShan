//------------------------------------------------------------------------------
  /*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : fifo
    File-history: 
       (1) ZHAO Huatao created in 20240425: Build response FIFO for response sub-router, pulling or pushing response flits;
       (2) jinpeize do fix en-queue logic:when en-queuqing at fifo is full,fifo_cnt should not decrease in 2024/5/13:;
       (3) Shao jinhua change to data channel fifo in 20240517;
       (4) jin peize do merge 4 channels in 2024/5/20;
       (5) pass random regression tests 70W+ rounds up to 2024/5/29;
       (6) zhoushize do timing optimization in 2024/10/29;

  */
//------------------------------------------------------------------------------

`include "common_defines.sv"
`include "router_define.sv"

module fifo #(
    parameter CLOCK_ENABLE      = 0,
    parameter FIFO_WIDTH        = 2,   // fifo data width
    parameter FIFO_DEPTH        = 2      // fifo buffer depth
) (
    clock,
    rstn,

    push,
    pop,
    din,
    cg_en,
    DFT_SE,

    fifo_cnt,
    full,
    nempty,
    dout
);

//*** PARAMETER ****************************************************************

//*** INPUT/OUTPUT *************************************************************

    input  wire                             clock;
    input  wire                             rstn;

    input  wire                             push;
    input  wire                             pop;
    input  wire [FIFO_WIDTH       -1 :0]    din;
    input  wire                             cg_en;

    input  wire                             DFT_SE;
    output wire [FIFO_DEPTH       -1 :0]    fifo_cnt;
    output wire                             full;
    output wire                             nempty;
    output reg  [FIFO_WIDTH       -1 :0]    dout;

//*** WIRE/REG *****************************************************************

    reg         [FIFO_WIDTH       -1 :0]    fifo_buffer_q[FIFO_DEPTH-1 :0];
    wire        [FIFO_DEPTH       -1 :0]    fifo_cnt_ns;
    reg         [FIFO_DEPTH       -1 :0]    fifo_cnt_q;
    wire                                    fifo_empty_ns;
    reg                                     fifo_nempty_q;
    wire                                    fifo_full_ns;
    reg                                     fifo_full_q;
    wire        [FIFO_DEPTH       -1 :0]    rd_ptr_ns;
    reg         [FIFO_DEPTH       -1 :0]    rd_ptr_q;
    wire        [FIFO_DEPTH       -1 :0]    wr_ptr_ns;
    reg         [FIFO_DEPTH       -1 :0]    wr_ptr_q;
    wire        [FIFO_DEPTH       -1 :0]    wr_en;
    wire                                    update_state;
    wire                                    cg;
    wire                                    ck;

    genvar                                  i;


//*** MAIN BODY ****************************************************************

    assign wr_ptr_ns[FIFO_DEPTH-1:0] = {wr_ptr_q[FIFO_DEPTH-2:0],wr_ptr_q[FIFO_DEPTH-1]}; //default shift left
  
    assign rd_ptr_ns[FIFO_DEPTH-1:0] = {rd_ptr_q[FIFO_DEPTH-2:0],rd_ptr_q[FIFO_DEPTH-1]}; //default shift left
  
    assign update_state = push ^ pop;
  
    assign fifo_empty_ns =  (fifo_cnt_q[1]            ^ fifo_cnt_q[0])            & ~push; //one flit left and pop
    assign fifo_full_ns  =  (fifo_cnt_q[FIFO_DEPTH-1] ^ fifo_cnt_q[FIFO_DEPTH-2]) & ~pop; //one slot left and push
  
    assign full  = fifo_full_q;
    assign nempty = fifo_nempty_q;
    assign fifo_cnt = fifo_cnt_q;
  
    assign fifo_cnt_ns[FIFO_DEPTH-1:0] = (push == 1'b1)
                                          ? {fifo_cnt_q[FIFO_DEPTH-2:0],1'b1}    //push and not pop
                                          : {1'b0,fifo_cnt_q[FIFO_DEPTH-1:1]};   //pop  and not push

    clock_gate U_CLK_GATE_FIFO(
        .clk        (clock  ),
        .se         (DFT_SE ),
        .clk_en     (cg_en  ),
        .cg         (cg     )
    );

    assign ck = (CLOCK_ENABLE==1) ? cg : clock;
            
    generate
        for( i = 0;i < FIFO_DEPTH;i = i + 1 ) begin: DFF_FIFO_ENTRIES
            assign wr_en[i] = wr_ptr_q[i] & push;

            always @(posedge ck) begin
                if (wr_en[i])
                    fifo_buffer_q[i] <= #`NOC_DELAY din;
            end

        end
    endgenerate
  
    always @(*) begin : OP_RD_DOUT
        dout[FIFO_WIDTH-1:0] = {FIFO_WIDTH{1'b0}};
        for( integer j = 0;j < FIFO_DEPTH;j = j + 1 ) begin
            //if( rd_ptr_q[j] == 1'b1)
            //    dout[FIFO_WIDTH-1:0] = fifo_buffer_q[j][FIFO_WIDTH-1:0];
            dout[FIFO_WIDTH-1:0] = dout[FIFO_WIDTH-1:0] | (fifo_buffer_q[j][FIFO_WIDTH-1:0] & {FIFO_WIDTH{rd_ptr_q[j]}});
        end
    end
  
    always @(posedge clock or negedge rstn) begin : DFF_WR_PTR_Q
        if( rstn == 1'b0 )
            wr_ptr_q[FIFO_DEPTH-1:0] <= #`NOC_DELAY {{(FIFO_DEPTH-1){1'b0}},1'b1};
        else if( push == 1'b1 )
            wr_ptr_q[FIFO_DEPTH-1:0] <= #`NOC_DELAY wr_ptr_ns[FIFO_DEPTH-1:0];
    end
  
    always @(posedge clock or negedge rstn) begin : DFF_RD_PTR_Q
        if( rstn == 1'b0 )
            rd_ptr_q[FIFO_DEPTH-1:0] <= #`NOC_DELAY {{(FIFO_DEPTH-1){1'b0}},1'b1};
        else if( pop == 1'b1 )
            rd_ptr_q[FIFO_DEPTH-1:0] <= #`NOC_DELAY rd_ptr_ns[FIFO_DEPTH-1:0];
    end
  
    always @(posedge clock or negedge rstn) begin : DFF_FIFO_CNT_Q
        if( rstn == 1'b0 )
            fifo_cnt_q[FIFO_DEPTH-1:0] <= #`NOC_DELAY {FIFO_DEPTH{1'b0}};
        else if( update_state == 1'b1)
            fifo_cnt_q[FIFO_DEPTH-1:0] <= #`NOC_DELAY fifo_cnt_ns[FIFO_DEPTH-1:0];
    end
  
    always @(posedge clock or negedge rstn) begin : DFF_FIFO_EMPTY_Q
        if( rstn == 1'b0 )
            fifo_nempty_q <= #`NOC_DELAY 1'b0;
        else if( update_state == 1'b1 )
            fifo_nempty_q <= #`NOC_DELAY ~fifo_empty_ns;
    end
  
    always @(posedge clock or negedge rstn) begin : DFF_FIFO_FULL_Q
        if( rstn == 1'b0 )
            fifo_full_q <= #`NOC_DELAY 1'b0;
        else if( update_state == 1'b1 )
            fifo_full_q <= #`NOC_DELAY fifo_full_ns;
    end
  

//*** DEBUG ********************************************************************
    `ifdef SVA_ON

    assert_no_pop_empty:
    assert property(
        @(posedge clock) disable iff(!rstn) (~nempty |-> ~pop)
    )
    else 
        $fatal("fifo pop nempty")
    ;
    
    assert_no_push_full:
    assert property(
        @(posedge clock) disable iff(!rstn) (full |-> ~push)
    )
    else 
        $fatal("fifo push full")
    ;

    final begin 
        assert (~nempty)
            else $fatal("fifo is not empty");
    end
    
    `endif

endmodule
