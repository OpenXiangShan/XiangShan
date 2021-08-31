
import "DPI-C" function void log_write_helper
(
    input byte channel,
    input byte opcode,
    input byte param,
    input byte source,
    input byte sink,
    input longint address,
    input longint data_0,
    input longint data_1,
    input longint data_2,
    input longint data_3,
    input longint stamp,
    input string prefix
);

module TLLogWriter(
    input [7:0] channel,
    input [7:0] opcode,
    input [7:0] param,
    input [7:0] source,
    input [7:0] sink,
    input [63:0] address,
    input [63:0] data_0,
    input [63:0] data_1,
    input [63:0] data_2,
    input [63:0] data_3,
    input [63:0] stamp,
    input wen,
    input clock,
    input reset
);
    parameter string prefix;

    always @(posedge clock) begin
        if(wen && !reset) begin
            log_write_helper(
                channel, opcode, param, source, sink,
                address, data_0, data_1, data_2, data_3, stamp, prefix
            );
        end
    end

endmodule