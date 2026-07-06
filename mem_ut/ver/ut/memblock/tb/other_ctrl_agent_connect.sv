//=========================================================
//File name    : other_ctrl_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : other_ctrl_agent_connect
//Discribution : other_ctrl_agent_connect : other_ctrl_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef OTHER_CTRL_AGENT_CONNECT__SV
`define OTHER_CTRL_AGENT_CONNECT__SV

`define MEMBLOCK__OTHER_CTRL_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    other_ctrl_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual other_ctrl_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.io_hartId = U_IF_NAME.io_hartId; \
        force U_IF_NAME.io_dcacheError_ecc_error_valid = RTL_PATH.io_dcacheError_ecc_error_valid; \
        force U_IF_NAME.io_dcacheError_ecc_error_bits = RTL_PATH.io_dcacheError_ecc_error_bits; \
        force U_IF_NAME.io_uncacheError_ecc_error_valid = RTL_PATH.io_uncacheError_ecc_error_valid; \
        force U_IF_NAME.io_uncacheError_ecc_error_bits = RTL_PATH.io_uncacheError_ecc_error_bits; \
        force U_IF_NAME.io_memInfo_sqFull = RTL_PATH.io_memInfo_sqFull; \
        force U_IF_NAME.io_memInfo_lqFull = RTL_PATH.io_memInfo_lqFull; \
        force U_IF_NAME.io_memInfo_dcacheMSHRFull = RTL_PATH.io_memInfo_dcacheMSHRFull; \
        force U_IF_NAME.io_inner_hartId = RTL_PATH.io_inner_hartId; \
        force U_IF_NAME.io_inner_reset_vector = RTL_PATH.io_inner_reset_vector; \
        force RTL_PATH.io_outer_reset_vector = U_IF_NAME.io_outer_reset_vector; \
        force U_IF_NAME.io_outer_cpu_wfi = RTL_PATH.io_outer_cpu_wfi; \
        force U_IF_NAME.io_outer_l2_flush_en = RTL_PATH.io_outer_l2_flush_en; \
        force U_IF_NAME.io_outer_power_down_en = RTL_PATH.io_outer_power_down_en; \
        force U_IF_NAME.io_outer_cpu_critical_error = RTL_PATH.io_outer_cpu_critical_error; \
        force U_IF_NAME.io_outer_msi_ack = RTL_PATH.io_outer_msi_ack; \
        force RTL_PATH.io_inner_beu_errors_icache_ecc_error_valid = U_IF_NAME.io_inner_beu_errors_icache_ecc_error_valid; \
        force RTL_PATH.io_inner_beu_errors_icache_ecc_error_bits = U_IF_NAME.io_inner_beu_errors_icache_ecc_error_bits; \
        force U_IF_NAME.io_outer_beu_errors_icache_ecc_error_valid = RTL_PATH.io_outer_beu_errors_icache_ecc_error_valid; \
        force U_IF_NAME.io_outer_beu_errors_icache_ecc_error_bits = RTL_PATH.io_outer_beu_errors_icache_ecc_error_bits; \
        force U_IF_NAME.io_reset_backend = tc_if.rst_n; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_hartId = RTL_PATH.io_hartId; \
        force U_IF_NAME.io_dcacheError_ecc_error_valid = RTL_PATH.io_dcacheError_ecc_error_valid; \
        force U_IF_NAME.io_dcacheError_ecc_error_bits = RTL_PATH.io_dcacheError_ecc_error_bits; \
        force U_IF_NAME.io_uncacheError_ecc_error_valid = RTL_PATH.io_uncacheError_ecc_error_valid; \
        force U_IF_NAME.io_uncacheError_ecc_error_bits = RTL_PATH.io_uncacheError_ecc_error_bits; \
        force U_IF_NAME.io_memInfo_sqFull = RTL_PATH.io_memInfo_sqFull; \
        force U_IF_NAME.io_memInfo_lqFull = RTL_PATH.io_memInfo_lqFull; \
        force U_IF_NAME.io_memInfo_dcacheMSHRFull = RTL_PATH.io_memInfo_dcacheMSHRFull; \
        force U_IF_NAME.io_inner_hartId = RTL_PATH.io_inner_hartId; \
        force U_IF_NAME.io_inner_reset_vector = RTL_PATH.io_inner_reset_vector; \
        force U_IF_NAME.io_outer_reset_vector = RTL_PATH.io_outer_reset_vector; \
        force U_IF_NAME.io_outer_cpu_wfi = RTL_PATH.io_outer_cpu_wfi; \
        force U_IF_NAME.io_outer_l2_flush_en = RTL_PATH.io_outer_l2_flush_en; \
        force U_IF_NAME.io_outer_power_down_en = RTL_PATH.io_outer_power_down_en; \
        force U_IF_NAME.io_outer_cpu_critical_error = RTL_PATH.io_outer_cpu_critical_error; \
        force U_IF_NAME.io_outer_msi_ack = RTL_PATH.io_outer_msi_ack; \
        force U_IF_NAME.io_inner_beu_errors_icache_ecc_error_valid = RTL_PATH.io_inner_beu_errors_icache_ecc_error_valid; \
        force U_IF_NAME.io_inner_beu_errors_icache_ecc_error_bits = RTL_PATH.io_inner_beu_errors_icache_ecc_error_bits; \
        force U_IF_NAME.io_outer_beu_errors_icache_ecc_error_valid = RTL_PATH.io_outer_beu_errors_icache_ecc_error_valid; \
        force U_IF_NAME.io_outer_beu_errors_icache_ecc_error_bits = RTL_PATH.io_outer_beu_errors_icache_ecc_error_bits; \
        force U_IF_NAME.io_reset_backend = tc_if.rst_n; \
    end \
    `endif

`endif
