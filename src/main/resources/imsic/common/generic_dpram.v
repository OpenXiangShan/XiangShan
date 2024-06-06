//-----------------------------------------------------------------------------
// Title         : generic_dpram
// Project       : Generic
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

module generic_dpram(/*AUTOARG*/
   // Outputs
   rd,
   // Inputs
   wrclk, waddr, we,rstn, wd, rdclk, raddr
   );
   

   
   parameter NumWords = 32;
   parameter AddrBits = 4;
   parameter NumBits  = 8;
   
   input wrclk;
   input rstn;
   input [AddrBits-1:0]                  waddr;
   input                                 we;
   input [NumBits-1:0]                   wd;

   input                                 rdclk;
   input [AddrBits-1:0]                  raddr;
   output [NumBits-1:0]                  rd;

   //---------------------
   //-- FPGA Inferred RAM
   //---------------------

   reg [NumBits-1:0]                     rd;
   reg [NumBits-1:0]                     mem[NumWords-1:0];

   always @(posedge wrclk)
     begin
        if (we) mem[waddr] <= wd;
     end

   always @(posedge rdclk,negedge rstn)
     begin
        if (~rstn)
            rd <= {NumBits{1'b0}};
        else
            rd <= mem[raddr];
     end


endmodule // generic_dpram
