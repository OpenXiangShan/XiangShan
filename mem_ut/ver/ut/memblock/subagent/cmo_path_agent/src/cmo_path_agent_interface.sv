`ifndef CMO_PATH_AGENT_INTERFACE_SV
`define CMO_PATH_AGENT_INTERFACE_SV
interface cmo_path_agent_interface(input logic clk, input logic rst_n);
    import cmo_path_agent_dec::*;
    // 仅DUT->observer方向；无driver clocking block，防止内部被动探针驱动DUT。
    cmo_path_probe_t probes[CMO_PATH_PROBE_NUM];
    clocking mon_cb @(posedge clk);
        default input #1step;
        input rst_n;
        input probes;
    endclocking
endinterface
`endif
