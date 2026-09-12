
module atd_sync2_rstn(
input clk,
input rst_n,
output so
);
reg si_d1, si_d2;
always@(posedge clk or negedge rst_n)
    if (!rst_n) begin
        si_d1 <= 1'b0;
        si_d2 <= 1'b0;
    end else begin
        si_d1 <= 1'b1;
        si_d2 <= si_d1;
    end
assign so = si_d2;
endmodule
