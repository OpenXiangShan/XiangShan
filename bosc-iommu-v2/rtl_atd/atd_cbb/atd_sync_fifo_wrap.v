//`timescale 1ns/1ps

module atd_sync_fifo_wrap(
  clk,
  rstn,
  wen,
  ren,
  din,
  dout,
  full,
  empty
//  cnt,
//  wptr,
//  rptr,
);

//--------------------------------------------------------------------------//
//   Parameter Define
//--------------------------------------------------------------------------//

parameter                   USE_RS         = 0                    ;
parameter                   FIFO_TMO       = 1                    ;
parameter                   FIFO_WID       = 32                   ;
parameter                   FIFO_DEPTH_WID = 4                    ;
parameter                   FIFO_DEPTH     = (1 << FIFO_DEPTH_WID);

//--------------------------------------------------------------------------//
//   Port Define
//--------------------------------------------------------------------------//
input                       clk;
input                       rstn;
input                       wen;
input                       ren;
input [FIFO_WID-1:0]        din;
output[FIFO_WID-1:0]        dout;
output                      full;
output                      empty;
//output [FIFO_DEPTH_WID:0]   cnt;
//output [FIFO_DEPTH_WID-1:0] wptr;
//output [FIFO_DEPTH_WID-1:0] rptr;


//------------------------------------------------------------------------------
generate

if (USE_RS==1) begin :RS
  // parameter TMO:
  // 0 - pass through mode
  // 1 - forward timing mode
  // 2 - full timing mode
  // 3 - backward timing mode
  wire ready_o,valid_o;
  assign full = ~ready_o;
  assign empty= ~valid_o;

  atd_reg_slice
      #(
           .TMO       (FIFO_TMO   )
          ,.PLD_W     (FIFO_WID   )
       )
  u_rs (
          //push
          .aclk_i     ( clk),
          .aresetn_i  ( rstn),
          .valid_i    ( wen),
          .ready_o    ( ready_o),
          .payload_i  ( din),

          //pop
          .valid_o    ( valid_o),
          .ready_i    ( ren),
          .payload_o  ( dout)
   );

end else begin :FIFO

  atd_sync_fifo
        #(.FIFO_WID   (FIFO_WID)
         ,.FIFO_DEPTH_WID(FIFO_DEPTH_WID)
         ,.FIFO_DEPTH (FIFO_DEPTH)
        )
  u_sync_fifo(
           .clk       (clk),
           .rstn      (rstn),
           .wen       (wen ),
           .ren       (ren ),
           .din       (din ),
           .dout      (dout),
	   .afull     (),
           .wptr      (),
           .rptr      (),
           .cnt       (),
           .full      (full),
           .empty     (empty)
   );
  end

endgenerate


endmodule

