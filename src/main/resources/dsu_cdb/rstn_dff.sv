

`include "common_defines.sv"
`include "router_define.sv"
module rstn_dff (
    sync_o,
    sync_i,
    clk,
    rstn
);


    output wire   sync_o   ;

    input  wire   sync_i   ;
    input  wire   clk      ;
    input  wire   rstn     ;


    reg           sync_q0  ;
    reg           sync_q1  ;
    reg           sync_q2  ;


    always @(posedge clk or negedge rstn) begin
        if (rstn==1'b0)  begin
            sync_q0= #`NOC_DELAY 1'b0;
            sync_q1= #`NOC_DELAY 1'b0;
            sync_q2= #`NOC_DELAY 1'b0;
        end
        else begin
            sync_q0= #`NOC_DELAY 1'b1   ;
            sync_q1= #`NOC_DELAY sync_q0;
            sync_q2= #`NOC_DELAY sync_q1;
        end
    end


    assign sync_o = sync_q2;

endmodule
