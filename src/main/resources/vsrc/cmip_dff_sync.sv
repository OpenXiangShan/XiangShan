/* Copyright bosc
 * author: zhaohong
 * Function: handel synchronize for 1bit signal.*/
module cmip_dff_sync #(
parameter RST_VALUE    = 1'b0,
parameter N            = 2  //min is 2.
)
(
input           clk  ,
input           rstn ,
input           din  ,
output wire     dout  
);

reg  [N-1:0]    dout_tmp;  
integer i;
always @(posedge clk or negedge rstn)
begin
    if (~rstn)
        dout_tmp[N-1:0] <= {N{1'b0}};
    else 
        dout_tmp[N-1:0] <= {dout_tmp[N-2:0],din};
end
assign dout = dout_tmp[N-1];

endmodule
