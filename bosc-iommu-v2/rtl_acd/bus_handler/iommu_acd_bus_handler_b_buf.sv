module iommu_acd_bus_handler_b_buf import iommu_acd_pkg::*; #(
    parameter   BUS_INFLY_TOKEN_WIDTH   = 6,
    parameter   BUS_INFLY_TOKEN_NUM     = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_ID_WIDTH            = 8,
    parameter   BUS_USER_WIDTH          = 8,
    parameter type          BUS_CH_B_TYPE           = iommu_acd_pkg::ch_b_t,
    parameter   SPARE_PARAM             = 0
)(
    input  logic                                    clk             ,
    input  logic                                    rstn            ,
    // MST Intf
    input  logic                                    m2b_bvalid_i    ,
    output logic                                    m2b_bready_o    ,
    input  BUS_CH_B_TYPE                            m2b_bpayld_i    ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        m2b_token_i     ,
    // SLV Intf
    input  logic                                    s2q_valid_i     ,
    input  logic                                    s2q_ready_i     ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        s2q_token_i     ,
    input  logic                                    s2q_wr_i        ,
    output logic                                    b2s_bvalid_o    ,
    input  logic                                    b2s_bready_i    ,
    output BUS_CH_B_TYPE                            b2s_bpayld_o    ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        b2s_token_o     ,

    input  logic                                    spare_in        
);
//=== Declare === {{{
//gpf    typedef struct packed {
//gpf        logic [BUS_ID_WIDTH-1:0]            bid;
//gpf        logic [1:0]                         bresp;
//gpf        logic [BUS_USER_WIDTH-1:0]          buser;
//gpf        logic                               bvalid;
//gpf        logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token;
//gpf    } bfifo_t;
//gpf
//gpf    bfifo_t [BUS_INFLY_TOKEN_NUM-1:0]       bfifo;
//gpf
//gpf    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       bfifo_wptr, bfifo_rptr, bfifo_wr_num;
//gpf    logic                                   bfifo_wr, bfifo_rd;
//}}}

//gpf  
//gpf //=== Main code === {{{
//gpf     assign bfifo_wr = s2q_valid_i & s2q_ready_i & s2q_wr_i;
//gpf     assign bfifo_rd = b2s_bvalid_o & b2s_bready_i;
//gpf 
//gpf     always@(posedge clk or negedge rstn) begin
//gpf         if(~rstn)
//gpf             bfifo_wr_num <= 'd0;
//gpf         else begin
//gpf             case({bfifo_wr, bfifo_rd})
//gpf             2'b01  :bfifo_wr_num <= bfifo_wr_num - 'd1;
//gpf             2'b10  :bfifo_wr_num <= bfifo_wr_num + 'd1;
//gpf             2'b11  :bfifo_wr_num <= bfifo_wr_num;
//gpf             2'b00  :bfifo_wr_num <= bfifo_wr_num;
//gpf             default:bfifo_wr_num <= bfifo_wr_num;
//gpf             endcase
//gpf         end
//gpf     end
//gpf 
//gpf     always@(posedge clk or negedge rstn) begin
//gpf         if(~rstn)
//gpf             bfifo_wptr <= 'd0;
//gpf         else if(bfifo_wr)
//gpf             bfifo_wptr <= bfifo_wptr + 'd1;
//gpf     end
//gpf 
//gpf     always@(posedge clk or negedge rstn) begin
//gpf         if(~rstn)
//gpf             bfifo_rptr <= 'd0;
//gpf         else if(bfifo_rd)
//gpf             bfifo_rptr <= bfifo_rptr + 'd1;
//gpf     end
//gpf 
//gpf     always@(posedge clk or negedge rstn) begin
//gpf         if(~rstn) begin
//gpf             for(int unsigned kk=0; kk<BUS_INFLY_TOKEN_NUM; kk++) begin
//gpf                 bfifo[kk] <= 'd0;
//gpf             end
//gpf         end
//gpf         else begin
//gpf             if(bfifo_wr) begin
//gpf                 bfifo[bfifo_wptr].token <= s2q_token_i;
//gpf             end
//gpf 
//gpf             if(bfifo_rd) begin
//gpf                 bfifo[bfifo_rptr].bvalid <= 1'b0;
//gpf             end
//gpf 
//gpf             if(m2b_bvalid_i & m2b_bready_o) begin
//gpf                 for(int unsigned aa=0; aa<BUS_INFLY_TOKEN_NUM; aa++) begin
//gpf                     if(bfifo[aa].token == m2b_token_i) begin
//gpf                         bfifo[aa].bvalid <= 1'b1;
//gpf                         bfifo[aa].bid    <= m2b_bid_i;
//gpf                         bfifo[aa].bresp  <= m2b_bresp_i;
//gpf                         bfifo[aa].buser  <= m2b_buser_i;
//gpf                     end
//gpf                 end
//gpf             end
//gpf         end
//gpf     end
//gpf     
//gpf     assign m2b_bready_o = 1'b1;
//gpf 
//gpf     assign b2s_bvalid_o = bfifo[bfifo_rptr].bvalid;
//gpf     assign b2s_bid_o    = bfifo[bfifo_rptr].bid;
//gpf     assign b2s_bresp_o  = bfifo[bfifo_rptr].bresp;
//gpf     assign b2s_buser_o  = bfifo[bfifo_rptr].buser;
//gpf //}}}
//gpf 

//=== B bypass === {{{
// trans_queue guarantee that AW&W keep the original sequence at MST_IF output
// so inbound B with the same ID will also keep the original sequence
    assign b2s_bvalid_o = m2b_bvalid_i;
    assign m2b_bready_o = b2s_bready_i;
    assign b2s_bpayld_o = m2b_bpayld_i;
    assign b2s_token_o  = m2b_token_i ;
//}}}
endmodule
