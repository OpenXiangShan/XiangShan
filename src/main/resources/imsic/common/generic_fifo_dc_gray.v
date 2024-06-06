//-----------------------------------------------------------------------------
// Title         : generic_fifo_dc_gray.v
// Project       : Generic
//-----------------------------------------------------------------------------
// File          : generic_fifo_dc_gray.v


/*

 Description
 ===========

 I/Os
 ----
 clk Read Port Clock
 clk Write Port Clock
 rst    low active, either sync. or async. master reset (see below how to select)
 re     read enable, synchronous, high active
 we     read enable, synchronous, high active
 din    Data Input
 dout   Data Output

 full   Indicates the FIFO is full (driven at the rising edge of clk)
 empty  Indicates the FIFO is empty (driven at the rising edge of clk)

 Parameters
 ----------
 The FIFO takes 2 parameters:
 dw Data bus width
 aw Address bus width (Determines the FIFO size by evaluating 2^aw)


 IMPORTANT ! writing while the FIFO is full or reading while the FIFO is
 empty will place the FIFO in an undefined state.

 */


module generic_fifo_dc_gray(/*AUTOARG*/
   // Outputs
   dout, full, empty, 
   // Inputs
   clk, rst,  din, we, re
   );

   parameter dw=32;
   parameter aw=3;
   parameter max_size = 1<<aw;

   input [dw-1:0] din;
   input          clk;
   input          rst;
   input          we;
   input          re;
   
   output [dw-1:0] dout;
   output          full; 
   output          empty;

   ////////////////////////////////////////////////////////////////////
   //
   // Local Wires
   //

   reg [aw:0]       wp_bin;
   reg [aw:0]       rp_bin;
   wire             full;
   wire             empty;

   wire [aw:0]      wp_bin_next;
   wire [aw:0]      rp_bin_next;

   

   ////////////////////////////////////////////////////////////////////
   //
   // Memory Block
   //

   generic_dpram #(.NumWords(max_size),
                   .NumBits(dw),
                   .AddrBits(aw))
   generic_dpram_inst0(
                       // Outputs
                       .rd              (dout),
                       // Inputs
                       .wrclk           (clk),
                       .rstn            (rst),
                       .waddr           (wp_bin[aw-1:0]),
                       .we              (we),
                       .wd              (din),
                       .rdclk           (clk),
                       .raddr           (rp_bin[aw-1:0]));
   

   ////////////////////////////////////////////////////////////////////
   //
   // Read/Write Pointers Logic
   //

   always @(posedge clk or negedge rst) begin
      if(!rst) begin
         wp_bin <= {aw+1{1'b0}};
      end
      else if(we) begin
         wp_bin <= wp_bin_next;
      end
   end


   assign wp_bin_next  = wp_bin + {{aw{1'b0}},1'b1};

   always @(posedge clk or negedge rst) begin
      if(!rst) begin
         rp_bin <= {aw+1{1'b0}};
      end
      else if(re) begin
         rp_bin <= rp_bin_next;
      end // else: !if(!rst)
   end // always @ (posedge clk)
   


   assign rp_bin_next  = rp_bin + {{aw{1'b0}},1'b1};


   ////////////////////////////////////////////////////////////////////
   //
   // Registered Full & Empty Flags
   //

   assign empty = (wp_bin == rp_bin) ;
   assign full  = (wp_bin[aw] == ~rp_bin[aw]) & ((wp_bin[aw-1:0] == rp_bin[aw-1:0])) ;


   ////////////////////////////////////////////////////////////////////
   //
   // Sanity Check
   //

   // synopsys translate_off
   always @(posedge clk) begin
      if(we && full) begin
         $display("%m WARNING: Writing while fifo is FULL (%t)",$time);
      end
   end
   
   always @(posedge clk) begin
      if(re && empty) begin
         $display("%m WARNING: Reading while fifo is EMPTY (%t)",$time);
      end
   end   
   // synopsys translate_on

endmodule

