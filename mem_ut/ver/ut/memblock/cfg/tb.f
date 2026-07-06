
-F ../../../common/tcnt_base/tcnt_base.f
+incdir+../cfg
../env/plus_pkg.sv
../common/memblock_common/src/memblock_sync_pkg.sv

-F ../agent/backendToTopBypass_agent_agent/backendToTopBypass_agent_agent.f
-F ../agent/fence_agent_agent/fence_agent_agent.f
-F ../agent/csr_ctrl_agent_agent/csr_ctrl_agent_agent.f
-F ../agent/lsqcommit_agent_agent/lsqcommit_agent_agent.f
-F ../agent/lsqenq_agent_agent/lsqenq_agent_agent.f
-F ../agent/lintsissue_agent_agent/lintsissue_agent_agent.f
-F ../agent/vecissue_agent_agent/vecissue_agent_agent.f
-F ../agent/redirect_agent_agent/redirect_agent_agent.f
-F ../agent/sbuffer_agent_agent/sbuffer_agent_agent.f
-F ../agent/dcache_agent_agent/dcache_agent_agent.f
-F ../agent/int_sink_agent_agent/int_sink_agent_agent.f
-F ../agent/L2tlb_agent_agent/L2tlb_agent_agent.f
-F ../agent/itlb_agent_agent/itlb_agent_agent.f
-F ../agent/prefetch_agent_agent/prefetch_agent_agent.f
-F ../agent/io_mem_to_ooo_ctrl_agent_agent/io_mem_to_ooo_ctrl_agent_agent.f
-F ../agent/io_mem_to_ooo_int_wb_agent_agent/io_mem_to_ooo_int_wb_agent_agent.f
-F ../agent/io_mem_to_ooo_vec_wb_agent_agent/io_mem_to_ooo_vec_wb_agent_agent.f
-F ../agent/io_mem_to_ooo_wakeup_agent_agent/io_mem_to_ooo_wakeup_agent_agent.f
-F ../agent/io_mem_to_ooo_iq_feedback_agent_agent/io_mem_to_ooo_iq_feedback_agent_agent.f
-F ../agent/other_ctrl_agent_agent/other_ctrl_agent_agent.f

-F ../common/memblock_common/memblock_common.f
-F ../seq/seq.f
-F ../env/memblock_env.f
-F ../tc/tc.f
../tb/top_tb.sv
