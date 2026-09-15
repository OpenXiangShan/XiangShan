//`timescale 1ns/1ps
// leda off
module atd_sync_fifo(
  clk,
  rstn,
  wen,
  ren,
  din,
  dout,
//  afull,
  full,
//  cnt,
//  wptr,
//  rptr,
  empty
);

  //--------------------------------------------------------------------------//
  //   Parameter Define
  //--------------------------------------------------------------------------//

  parameter                   FIFO_WID       = 32                   ;
  parameter                   FIFO_DEPTH_WID = 4                    ;
  parameter                   FIFO_DEPTH     = (1 << FIFO_DEPTH_WID);
//  parameter                   FIFO_ID        = 0;
  parameter                   REG_OUT        = 0;

  //--------------------------------------------------------------------------//
  //   Port Define
  //--------------------------------------------------------------------------//
  input                       clk;
  input                       rstn;
  input                       wen;
  input                       ren;
  input [FIFO_WID-1:0]        din;
  output[FIFO_WID-1:0]        dout;
  //output [FIFO_DEPTH_WID:0]   cnt;
  output                      full;
  //output                      afull;
  output                      empty;
  //output [FIFO_DEPTH_WID-1:0] wptr;
  //output [FIFO_DEPTH_WID-1:0] rptr;

  //--------------------------------------------------------------------------//
  //   R e g i s t e r    a n d    W i r e    D e c l a r a t i o n s
  //--------------------------------------------------------------------------//

  reg [FIFO_DEPTH_WID:0]      fifo_cnt;
  reg [FIFO_DEPTH_WID-1:0]    fifo_wptr;
  reg [FIFO_DEPTH_WID-1:0]    fifo_rptr;
  reg [FIFO_WID-1:0]          dout_r;
  reg [FIFO_WID-1:0]          fifo_mem [0:FIFO_DEPTH-1];
  wire                        full,empty;
//  integer i;
  wire                        ptr_clear= empty& ~wen & ~ren;
  //wire [FIFO_DEPTH_WID-1:0]   wptr     = fifo_wptr;
  //wire [FIFO_DEPTH_WID-1:0]   rptr     = fifo_rptr;

  //----------------------------------------------------------
  always@(posedge clk) begin
      if (wen& ~full) begin
          fifo_mem[fifo_wptr] <= din;
      end
  end

  always@(posedge clk or negedge rstn) begin
      if (!rstn)
          dout_r <= 'b0;
      else if (ren)
          dout_r <= fifo_mem[fifo_rptr];
  end

  assign  dout = REG_OUT?dout_r:fifo_mem[fifo_rptr];

  always @ (posedge clk or negedge rstn) begin
      if (!rstn)
          fifo_wptr <= {FIFO_DEPTH_WID{1'b0}};
      else if ( ptr_clear )
          fifo_wptr <= {FIFO_DEPTH_WID{1'b0}};
      else if (wen& fifo_wptr==(FIFO_DEPTH-1) )
          fifo_wptr <= {FIFO_DEPTH_WID{1'b0}};
      else if (wen& ~full)
          fifo_wptr <= fifo_wptr + 1'b1;
  end

  always @ (posedge clk or negedge rstn) begin
      if (!rstn)
          fifo_rptr <= {FIFO_DEPTH_WID{1'b0}};
      else if ( ptr_clear )
          fifo_rptr <= {FIFO_DEPTH_WID{1'b0}};
      else if (ren& fifo_rptr==(FIFO_DEPTH-1) )
          fifo_rptr <= {FIFO_DEPTH_WID{1'b0}};
      else if (ren& ~empty)
          fifo_rptr <= fifo_rptr + 1'b1;
  end

  always @ (posedge clk or negedge rstn) begin
      if (!rstn)
          fifo_cnt <= {(FIFO_DEPTH_WID+1){1'b0}};
      else if (wen&~ren&~full)
          fifo_cnt <= fifo_cnt + 1'b1;
      else if (~wen&ren&~empty)
          fifo_cnt <= fifo_cnt - 1'b1;
  end

  assign    full  =  fifo_cnt==FIFO_DEPTH;
  //assign    afull =  fifo_cnt==FIFO_DEPTH-1;
  assign    empty =  fifo_cnt==0;
//  assign    cnt   =  fifo_cnt;

//synopsys translate_off
always @ (posedge clk or negedge rstn) begin
   if(full&wen&~ren) begin
     $display("%m: at time %t ERROR: FIFO is Overflow.",$time);
     #100;
     $finish;
   end else if(empty&ren&~wen) begin
     $display("%m: at time %t ERROR: FIFO is Underflow.",$time);
     #100;
     $finish;
   end
end
//synopsys translate_on

endmodule
// leda on
