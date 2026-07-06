`ifndef TCNT_REALTIME__SV
`define TCNT_REALTIME__SV

//======================================================================================
// timedelay
//======================================================================================
package tcnt_realtime;

    timeunit 1ns;
    timeprecision 1ps;

    task delay_ns(realtime delay);
        #delay;
    endtask
    task delay_us(int delay);
        repeat(delay) begin
            delay_ns(1_000);
        end
    endtask
    task delay_ms(int delay);
        repeat(delay) begin
            delay_ns(1_000_000);
        end
    endtask
    task delay_random(int min, int max=-1,realtime unit=1ns);
        int delay;
        if(max == -1) begin
            delay = min;
        end
        else begin
            void'(std::randomize(delay) with { delay inside {[min:max]}; });
        end
        #(delay * unit);
    endtask
    function get_timestamp(realtime unit=1ns);
        realtime timestamp;
        timestamp = $realtime / unit;
        return timestamp;
    endfunction

endpackage

import tcnt_realtime::*;

`endif
