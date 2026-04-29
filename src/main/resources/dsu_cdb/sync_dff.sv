

`include "common_defines.sv"
`include "router_define.sv"
module sync_dff (
    sync_o,

    sync_i,
    clk,
    rstn
);

    parameter     WIDTH = 1           ;

    output wire [WIDTH-1:0]  sync_o   ;

    input  wire [WIDTH-1:0]  sync_i   ;
    input  wire              clk      ;
    input  wire              rstn     ;


    reg         [WIDTH-1:0]  sync_q0  ;
    reg         [WIDTH-1:0]  sync_q1  ;
    reg         [WIDTH-1:0]  sync_q2  ;


    always @(posedge clk or negedge rstn) begin
        if (rstn==1'b0)  begin
            sync_q0[WIDTH-1:0] <= #`NOC_DELAY {WIDTH{1'b0}};
            sync_q1[WIDTH-1:0] <= #`NOC_DELAY {WIDTH{1'b0}};
            sync_q2[WIDTH-1:0] <= #`NOC_DELAY {WIDTH{1'b0}};
        end
        else begin
            sync_q0[WIDTH-1:0] <= #`NOC_DELAY sync_i[WIDTH-1:0] ;
            sync_q1[WIDTH-1:0] <= #`NOC_DELAY sync_q0[WIDTH-1:0];
            sync_q2[WIDTH-1:0] <= #`NOC_DELAY sync_q1[WIDTH-1:0];
        end
    end


    assign sync_o[WIDTH-1:0] = sync_q2[WIDTH-1:0];

endmodule
